package gitbucket.core.servlet

import javax.servlet._
import javax.servlet.http.HttpServletRequest
import org.scalatra.ScalatraBase
import org.slf4j.LoggerFactory
import slick.dbio.DBIO
import slick.jdbc.JdbcBackend.Database
import gitbucket.core.util.Keys

import scala.concurrent.Await
import scala.concurrent.duration.Duration

/**
 * Controls the transaction with the open session in view pattern.
 */
//class TransactionFilter extends Filter {
//
//  private val logger = LoggerFactory.getLogger(classOf[TransactionFilter])
//
//  def init(config: FilterConfig) = {}
//
//  def destroy(): Unit = {}
//
//  def doFilter(req: ServletRequest, res: ServletResponse, chain: FilterChain): Unit = {
//    if(req.asInstanceOf[HttpServletRequest].getServletPath().startsWith("/assets/")){
//      // assets don't need transaction
//      chain.doFilter(req, res)
//    } else {
//      Database() withTransaction { session =>
//        // Register Scalatra error callback to rollback transaction
//        ScalatraBase.onFailure { _ =>
//          logger.debug("Rolled back transaction")
//          session.rollback()
//        }(req.asInstanceOf[HttpServletRequest])
//
//        logger.debug("begin transaction")
//        req.setAttribute(Keys.Request.DBSession, session)
//        chain.doFilter(req, res)
//        logger.debug("end transaction")
//      }
//    }
//  }
//
//}

// TODO
trait DatabaseProvider {
  implicit val db: Database = DatabaseProvider.db

  /**
   * Run the supplied Action and return the result synchronously.
   * Note that this method is blocking, waiting until return the result.
   * Be limited to use by batch (i.e. main or actor).
   */
  def withSession[A](f: => DBIO[A])(implicit db: Database): A = {
    Await.result(db.run(f), Duration.Inf)
  }

}

object DatabaseProvider {

  private lazy val db: Database = Database.forConfig("db")

  def close(): Unit = db.close

}
