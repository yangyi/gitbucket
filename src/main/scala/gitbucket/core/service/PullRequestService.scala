package gitbucket.core.service

import gitbucket.core.model.{Issue, PullRequest, CommitStatus, CommitState}
import gitbucket.core.model.Profile._, profile.api._
import gitbucket.core.util.JGitUtil
import scala.concurrent.ExecutionContext
import scalaz.{Monad, OptionT}

trait PullRequestService { self: IssuesService =>
  import PullRequestService._

  def getPullRequest(owner: String, repository: String, issueId: Int)
                    (implicit e: ExecutionContext): DBIO[Option[(Issue, PullRequest)]] = (
    for {
      issue   <- OptionT.optionT[DBIO](getIssue(owner, repository, issueId.toString))
      pullreq <- OptionT.optionT[DBIO](PullRequests.filter(_.byPrimaryKey(owner, repository, issueId)).result.headOption)
    } yield issue -> pullreq
  ).run

  def updateCommitId(owner: String, repository: String, issueId: Int, commitIdTo: String, commitIdFrom: String): DBIO[Int] =
    PullRequests.filter(_.byPrimaryKey(owner, repository, issueId))
      .map(pr => pr.commitIdTo -> pr.commitIdFrom)
      .update((commitIdTo, commitIdFrom))

  def getPullRequestCountGroupByUser(closed: Boolean, owner: Option[String], repository: Option[String])
                                    (implicit e: ExecutionContext): DBIO[Seq[PullRequestCount]] =
    PullRequests
      .join(Issues).on { (t1, t2) => t1.byPrimaryKey(t2.userName, t2.repositoryName, t2.issueId) }
      .filter { case (t1, t2) =>
        (t2.closed         === closed.bind) &&
        (t1.userName       === owner.get.bind, owner.isDefined) &&
        (t1.repositoryName === repository.get.bind, repository.isDefined)
      }
      .groupBy { case (t1, t2) => t2.openedUserName }
      .map { case (userName, t) => userName -> t.length }
      .sortBy(_._2 desc)
      .result
      .map { _.map ( x => PullRequestCount(x._1, x._2) ) }

  def createPullRequest(originUserName: String, originRepositoryName: String, issueId: Int,
                        originBranch: String, requestUserName: String, requestRepositoryName: String, requestBranch: String,
                        commitIdFrom: String, commitIdTo: String): DBIO[Int] =
    PullRequests += PullRequest(
      originUserName,
      originRepositoryName,
      issueId,
      originBranch,
      requestUserName,
      requestRepositoryName,
      requestBranch,
      commitIdFrom,
      commitIdTo)

  def getPullRequestsByRequest(userName: String, repositoryName: String, branch: String, closed: Boolean): DBIO[Seq[PullRequest]] =
    PullRequests
      .join(Issues).on { (t1, t2) => t1.byPrimaryKey(t2.userName, t2.repositoryName, t2.issueId) }
      .filter { case (t1, t2) =>
        (t1.requestUserName       === userName.bind) &&
        (t1.requestRepositoryName === repositoryName.bind) &&
        (t1.requestBranch         === branch.bind) &&
        (t2.closed                === closed.bind)
      }
      .map { case (t1, t2) => t1 }
      .result

  /**
   * for repository viewer.
   * 1. find pull request from from `branch` to othre branch on same repository
   *   1. return if exists pull request to `defaultBranch`
   *   2. return if exists pull request to othre branch
   * 2. return None
   */
  def getPullRequestFromBranch(userName: String, repositoryName: String, branch: String, defaultBranch: String): DBIO[Option[(PullRequest, Issue)]] =
    PullRequests
      .join(Issues).on { (t1, t2) => t1.byPrimaryKey(t2.userName, t2.repositoryName, t2.issueId) }
      .filter { case (t1, t2) =>
        (t1.requestUserName       === userName.bind) &&
        (t1.requestRepositoryName === repositoryName.bind) &&
        (t1.requestBranch         === branch.bind) &&
        (t1.userName              === userName.bind) &&
        (t1.repositoryName        === repositoryName.bind) &&
        (t2.closed                === false.bind)
      }
      .sortBy{ case (t1, t2) => t1.branch =!= defaultBranch.bind }
      .result
      .headOption

  /**
   * Fetch pull request contents into refs/pull/${issueId}/head and update pull request table.
   */
  def updatePullRequests(owner: String, repository: String, branch: String)
                        (implicit e: ExecutionContext): DBIO[Unit] =
    getPullRequestsByRequest(owner, repository, branch, false).flatMap { pullreq =>
      DBIO.seq(pullreq.map { x =>
        Repositories.filter(_.byRepository(x.userName, x.repositoryName)).exists.result.flatMap {
          case true =>
            val (commitIdTo, commitIdFrom) = JGitUtil.updatePullRequest(
              x.userName, x.repositoryName, x.branch, x.issueId,
              x.requestUserName, x.requestRepositoryName, x.requestBranch)
            updateCommitId(x.userName, x.repositoryName, x.issueId, commitIdTo, commitIdFrom)
          case false => Monad[DBIO].point(())
        }
      }: _*)
    }

  def getPullRequestByRequestCommit(userName: String, repositoryName: String, toBranch:String, fromBranch: String, commitId: String): DBIO[Option[(PullRequest, Issue)]] = {
    if(toBranch == fromBranch){
      Monad[DBIO].point(None)
    } else {
      PullRequests
        .join(Issues).on { (t1, t2) => t1.byPrimaryKey(t2.userName, t2.repositoryName, t2.issueId) }
        .filter { case (t1, t2) =>
          (t1.userName              === userName.bind) &&
          (t1.repositoryName        === repositoryName.bind) &&
          (t1.branch                === toBranch.bind) &&
          (t1.requestUserName       === userName.bind) &&
          (t1.requestRepositoryName === repositoryName.bind) &&
          (t1.requestBranch         === fromBranch.bind) &&
          (t1.commitIdTo            === commitId.bind)
        }
        .result
        .headOption
    }
  }
}

object PullRequestService {

  val PullRequestLimit = 25

  case class PullRequestCount(userName: String, count: Int)

  case class MergeStatus(
    hasConflict: Boolean,
    commitStatues:List[CommitStatus],
    branchProtection: ProtectedBranchService.ProtectedBranchInfo,
    branchIsOutOfDate: Boolean,
    hasUpdatePermission: Boolean,
    needStatusCheck: Boolean,
    hasMergePermission: Boolean,
    commitIdTo: String){

    val statuses: List[CommitStatus] =
      commitStatues ++ (branchProtection.contexts.toSet -- commitStatues.map(_.context).toSet).map(CommitStatus.pending(branchProtection.owner, branchProtection.repository, _))
    val hasRequiredStatusProblem = needStatusCheck && branchProtection.contexts.exists(context => statuses.find(_.context == context).map(_.state) != Some(CommitState.SUCCESS))
    val hasProblem = hasRequiredStatusProblem || hasConflict || (!statuses.isEmpty && CommitState.combine(statuses.map(_.state).toSet) != CommitState.SUCCESS)
    val canUpdate = branchIsOutOfDate && !hasConflict
    val canMerge = hasMergePermission && !hasConflict && !hasRequiredStatusProblem
    lazy val commitStateSummary:(CommitState, String) = {
      val stateMap = statuses.groupBy(_.state)
      val state = CommitState.combine(stateMap.keySet)
      val summary = stateMap.map{ case (keyState, states) => states.size+" "+keyState.name }.mkString(", ")
      state -> summary
    }
    lazy val statusesAndRequired:List[(CommitStatus, Boolean)] = statuses.map{ s => s -> branchProtection.contexts.exists(_==s.context) }
    lazy val isAllSuccess = commitStateSummary._1==CommitState.SUCCESS
  }
}
