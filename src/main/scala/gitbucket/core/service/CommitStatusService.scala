package gitbucket.core.service

import gitbucket.core.model.Profile._
import profile.api._

import gitbucket.core.model.{CommitState, CommitStatus, Account}


trait CommitStatusService {
  /** insert or update */
  def createCommitStatus(userName: String, repositoryName: String, sha:String, context:String, state:CommitState,
                         targetUrl:Option[String], description:Option[String], now:java.util.Date, creator:Account): DBIO[Int] =
    CommitStatuses.filter(t => t.byCommit(userName, repositoryName, sha) && t.context===context.bind )
      .map(_.commitStatusId).result.headOption.flatMap {
      case Some(id) =>
        CommitStatuses.filter(_.byPrimaryKey(id)).map {
          t => (t.state , t.targetUrl , t.updatedDate , t.creator, t.description)
        }.update( (state, targetUrl, now, creator.userName, description) )
         .map(_ => id)
      case None =>
        (CommitStatuses returning CommitStatuses.map(_.commitStatusId)) += CommitStatus(
          userName       = userName,
          repositoryName = repositoryName,
          commitId       = sha,
          context        = context,
          state          = state,
          targetUrl      = targetUrl,
          description    = description,
          creator        = creator.userName,
          registeredDate = now,
          updatedDate    = now)
    }

  def getCommitStatus(userName: String, repositoryName: String, id: Int): DBIO[Option[CommitStatus]] =
    CommitStatuses.filter(t => t.byPrimaryKey(id) && t.byRepository(userName, repositoryName)).result.headOption

  def getCommitStatus(userName: String, repositoryName: String, sha: String, context: String): DBIO[Option[CommitStatus]] =
    CommitStatuses.filter(t => t.byCommit(userName, repositoryName, sha) && t.context===context.bind ).result.headOption

  def getCommitStatues(userName: String, repositoryName: String, sha: String): DBIO[Seq[CommitStatus]] =
    byCommitStatues(userName, repositoryName, sha).result

  def getRecentStatuesContexts(userName: String, repositoryName: String, time: java.util.Date): DBIO[Seq[String]] =
    CommitStatuses.filter(t => t.byRepository(userName, repositoryName)).filter(t => t.updatedDate > time.bind).groupBy(_.context).map(_._1).result

  def getCommitStatuesWithCreator(userName: String, repositoryName: String, sha: String): DBIO[Seq[(CommitStatus, Account)]] =
    byCommitStatues(userName, repositoryName, sha).join(Accounts)
      .filter{ case (t,a) => t.creator === a.userName }.result

  protected def byCommitStatues(userName: String, repositoryName: String, sha: String) =
    CommitStatuses.filter(t => t.byCommit(userName, repositoryName, sha) ).sortBy(_.updatedDate desc)

}