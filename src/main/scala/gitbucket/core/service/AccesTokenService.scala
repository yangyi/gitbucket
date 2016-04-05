package gitbucket.core.service

import gitbucket.core.model.Profile._, profile.api._

import gitbucket.core.model.{Account, AccessToken}
import gitbucket.core.util.StringUtil
import slick.dbio.SuccessAction

import scala.annotation.tailrec
import scala.concurrent.ExecutionContext
import scala.util.Random


trait AccessTokenService {

  def makeAccessTokenString: String = {
    val bytes = new Array[Byte](20)
    Random.nextBytes(bytes)
    bytes.map("%02x".format(_)).mkString
  }

  def tokenToHash(token: String): String = StringUtil.sha1(token)

  /**
   * @return DBIO[(TokenId, Token)]
   */
  def generateAccessToken(userName: String, note: String)
                         (implicit e: ExecutionContext): DBIO[(Int, String)] = {

    def hash0(token: String = makeAccessTokenString): DBIO[Option[(String, String)]] = {
      val hash = tokenToHash(token)
      for {
        exists <- AccessTokens.filter(_.tokenHash === hash.bind).exists.result
      } yield
        if (exists) None else Some(hash -> token)
    }

    @tailrec
    def generate0(): DBIO[(Int, String)] = {
      hash0() match {
        case SuccessAction(Some((hash, token))) =>
          ((AccessTokens returning AccessTokens.map(_.accessTokenId)) += AccessToken(
            userName  = userName,
            note      = note,
            tokenHash = hash)) map (_ -> token)
        case SuccessAction(_) => generate0()
      }
    }

    generate0()
  }

  def getAccountByAccessToken(token: String): DBIO[Option[Account]] =
    Accounts
      .join(AccessTokens)
      .filter{ case (ac, t) => (ac.userName === t.userName) && (t.tokenHash === tokenToHash(token).bind) && (ac.removed === false.bind) }
      .map{ case (ac, t) => ac }
      .result
      .headOption

  def getAccessTokens(userName: String): DBIO[Seq[AccessToken]] =
    AccessTokens.filter(_.userName === userName.bind).sortBy(_.accessTokenId.desc).result

  def deleteAccessToken(userName: String, accessTokenId: Int): DBIO[Int] =
    AccessTokens filter (t => t.userName === userName.bind && t.accessTokenId === accessTokenId.bind) delete

}

object AccessTokenService extends AccessTokenService
