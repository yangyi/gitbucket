package gitbucket.core.service

import gitbucket.core.model.SshKey
import gitbucket.core.model.Profile._, profile.api._

trait SshKeyService {

  def addPublicKey(userName: String, title: String, publicKey: String): DBIO[Int] =
    SshKeys += SshKey(userName = userName, title = title, publicKey = publicKey)

  def getPublicKeys(userName: String)(implicit s: Session): DBIO[Seq[SshKey]] =
    SshKeys.filter(_.userName === userName.bind).sortBy(_.sshKeyId).result

  def deletePublicKey(userName: String, sshKeyId: Int): DBIO[Int] =
    SshKeys filter (_.byPrimaryKey(userName, sshKeyId)) delete

}
