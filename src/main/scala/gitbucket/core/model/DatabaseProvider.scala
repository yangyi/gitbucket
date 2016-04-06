package gitbucket.core.model

import slick.dbio._
import slick.jdbc.JdbcBackend._

import scala.concurrent.Await
import scala.concurrent.duration.Duration

// TODO
trait DatabaseProvider extends Profile {
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
