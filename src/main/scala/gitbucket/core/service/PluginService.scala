package gitbucket.core.service

import gitbucket.core.model.Plugin
import gitbucket.core.model.Profile._, profile.api._

trait PluginService {

  def getPlugins(): DBIO[Seq[Plugin]] =
    Plugins.sortBy(_.pluginId).result

  def registerPlugin(plugin: Plugin): DBIO[Int] =
    Plugins += plugin

  def updatePlugin(plugin: Plugin): DBIO[Int] =
    Plugins.filter(_.pluginId === plugin.pluginId.bind).map(_.version).update(plugin.version)

  def deletePlugin(pluginId: String): DBIO[Int] =
    Plugins.filter(_.pluginId === pluginId.bind).delete

  def getPlugin(pluginId: String): DBIO[Option[Plugin]] =
    Plugins.filter(_.pluginId === pluginId.bind).result.headOption

}
