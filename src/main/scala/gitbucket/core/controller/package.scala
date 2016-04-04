package gitbucket.core

import gitbucket.core.servlet.DatabaseProvider
import slick.jdbc.JdbcBackend.Database

// TODO under review
package object controller {
  val db: Database = DatabaseProvider()

}
