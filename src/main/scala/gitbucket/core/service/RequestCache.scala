package gitbucket.core.service

import gitbucket.core.model.{Issue, Account}
import gitbucket.core.controller.Context
import slick.jdbc.JdbcBackend.Database

import scala.concurrent.Await
import scala.concurrent.duration.Duration

/**
 * This service is used for a view helper mainly.
 *
 * It may be called many times in one request, so each method stores
 * its result into the cache which available during a request.
 */
@deprecated
trait RequestCache extends SystemSettingsService with AccountService with IssuesService {

  // TODO to enable take out from the Context
  private def db(implicit context: Context): Database = {
//    request2Session(context.request)
    ???
  }

  def getIssue(userName: String, repositoryName: String, issueId: String)(implicit context: Context): Option[Issue] = {
    context.cache(s"issue.${userName}/${repositoryName}#${issueId}"){
      Await.result(db.run(super.getIssue(userName, repositoryName, issueId)), Duration.Inf)
    }
  }

  def getAccountByUserName(userName: String)(implicit context: Context): Option[Account] = {
    context.cache(s"account.${userName}"){
      Await.result(db.run(super.getAccountByUserName(userName)), Duration.Inf)
    }
  }

  def getAccountByMailAddress(mailAddress: String)(implicit context: Context): Option[Account] = {
    context.cache(s"account.${mailAddress}"){
      Await.result(db.run(super.getAccountByMailAddress(mailAddress)), Duration.Inf)
    }
  }
}
