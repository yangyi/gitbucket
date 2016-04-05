package gitbucket.core.service

import gitbucket.core.model.Milestone
import gitbucket.core.model.Profile._, profile.api._

import scala.concurrent.ExecutionContext

trait MilestonesService {

  def createMilestone(owner: String, repository: String, title: String, description: Option[String],
                      dueDate: Option[java.util.Date]): DBIO[Int] =
    Milestones += Milestone(
      userName       = owner,
      repositoryName = repository,
      title          = title,
      description    = description,
      dueDate        = dueDate,
      closedDate     = None
    )

  def updateMilestone(milestone: Milestone): DBIO[Int] =
    Milestones
      .filter (t =>  t.byPrimaryKey(milestone.userName, milestone.repositoryName, milestone.milestoneId))
      .map    (t => (t.title, t.description.?, t.dueDate.?, t.closedDate.?))
      .update (milestone.title, milestone.description, milestone.dueDate, milestone.closedDate)

  def openMilestone(milestone: Milestone): DBIO[Int] =
    updateMilestone(milestone.copy(closedDate = None))

  def closeMilestone(milestone: Milestone): DBIO[Int] =
    updateMilestone(milestone.copy(closedDate = Some(currentDate)))

  def deleteMilestone(owner: String, repository: String, milestoneId: Int): DBIO[Unit] = DBIO.seq(
    Issues.filter(_.byMilestone(owner, repository, milestoneId)).map(_.milestoneId.?).update(None),
    Milestones.filter(_.byPrimaryKey(owner, repository, milestoneId)).delete
  )

  def getMilestone(owner: String, repository: String, milestoneId: Int): DBIO[Option[Milestone]] =
    Milestones.filter(_.byPrimaryKey(owner, repository, milestoneId)).result.headOption

  def getMilestonesWithIssueCount(owner: String, repository: String)
                                 (implicit e: ExecutionContext): DBIO[Seq[(Milestone, Int, Int)]] = {
    for {
      milestones <- getMilestones(owner, repository)
      q <- Issues
        .filter  { t => t.byRepository(owner, repository) && t.milestoneId.? isDefined }
        .groupBy { t => t.milestoneId -> t.closed }
        .map     { case (t1, t2) => t1._1 -> t1._2 -> t2.length }
        .result
      counts = q.toMap
    } yield milestones.map { milestone =>
      (milestone, counts.getOrElse((milestone.milestoneId, false), 0), counts.getOrElse((milestone.milestoneId, true), 0))
    }
  }

  def getMilestones(owner: String, repository: String): DBIO[Seq[Milestone]] =
    Milestones.filter(_.byRepository(owner, repository)).sortBy(_.milestoneId asc).result

}
