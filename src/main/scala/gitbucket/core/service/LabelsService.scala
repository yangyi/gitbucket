package gitbucket.core.service

import gitbucket.core.model.Label
import gitbucket.core.model.Profile._
import profile.api._

trait LabelsService {

  def getLabels(owner: String, repository: String): DBIO[Seq[Label]] =
    Labels.filter(_.byRepository(owner, repository)).sortBy(_.labelName asc).result

  def getLabel(owner: String, repository: String, labelId: Int): DBIO[Option[Label]] =
    Labels.filter(_.byPrimaryKey(owner, repository, labelId)).result.headOption

  def getLabel(owner: String, repository: String, labelName: String): DBIO[Option[Label]] =
    Labels.filter(_.byLabel(owner, repository, labelName)).result.headOption

  def createLabel(owner: String, repository: String, labelName: String, color: String): DBIO[Int] =
    Labels returning Labels.map(_.labelId) += Label(
      userName       = owner,
      repositoryName = repository,
      labelName      = labelName,
      color          = color
    )

  def updateLabel(owner: String, repository: String, labelId: Int, labelName: String, color: String)
                 : DBIO[Int] =
    Labels.filter(_.byPrimaryKey(owner, repository, labelId))
          .map(t => t.labelName -> t.color)
          .update(labelName, color)

  def deleteLabel(owner: String, repository: String, labelId: Int): DBIO[Unit] = DBIO.seq(
    IssueLabels.filter(_.byLabel(owner, repository, labelId)).delete,
    Labels.filter(_.byPrimaryKey(owner, repository, labelId)).delete
  )

}
