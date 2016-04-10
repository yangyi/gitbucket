package gitbucket.core.plugin

import org.eclipse.jgit.transport.{ReceivePack, ReceiveCommand}

import scala.concurrent.ExecutionContext
import slick.jdbc.JdbcBackend._

trait ReceiveHook {

  def preReceive(owner: String, repository: String, receivePack: ReceivePack, command: ReceiveCommand, pusher: String)
                (implicit ec: ExecutionContext, db: Database): Option[String] = None

  def postReceive(owner: String, repository: String, receivePack: ReceivePack, command: ReceiveCommand, pusher: String)
                 (implicit ec: ExecutionContext, db: Database): Unit = ()

}
