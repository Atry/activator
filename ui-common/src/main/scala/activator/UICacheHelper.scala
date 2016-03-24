/**
 * Copyright (C) 2016 Lightbend, Inc <http://www.lightbend.com>
 */
package activator

import activator.properties.ActivatorProperties
import activator.properties.ActivatorProperties.SCRIPT_NAME
import activator.cache._
import akka.actor.ActorRefFactory
import java.io.File
import activator.cache.RemoteTemplateRepository
import com.typesafe.config.ConfigFactory
import akka.actor.ActorSystem
import akka.actor.ActorContext
import akka.event.LoggingAdapter
import scala.concurrent.duration._

// This helper constructs the template cache in the default CLI/UI location.
object UICacheHelper {

  // this is intended to be close to "forever" since if we time
  // out we'll pretty much fail catastrophically
  private implicit val timeout = akka.util.Timeout(Duration(240, SECONDS))

  // TODO - Config or ActiavtorProperties?
  lazy val config = ConfigFactory.load()

  def log(actorFactory: ActorRefFactory) = actorFactory match {
    case system: ActorSystem => system.log
    case context: ActorContext => context.system.log
    case whatever => throw new RuntimeException(s"don't know how to get log from $whatever")
  }

  val localCache = new File(ActivatorProperties.ACTIVATOR_TEMPLATE_CACHE)

  val localSeed = Option(ActivatorProperties.ACTIVATOR_TEMPLATE_LOCAL_REPO) map (new File(_)) filter (_.isDirectory)

  def makeDefaultCache(actorFactory: ActorRefFactory): TemplateCache = {
    DefaultTemplateCache(
      actorFactory = actorFactory,
      location = localCache,
      remote = RemoteTemplateRepository(config, log(actorFactory)),
      seedRepository = localSeed)
  }

  def makeLocalOnlyCache(actorFactory: ActorRefFactory): TemplateCache = {
    DefaultTemplateCache(
      actorFactory = actorFactory,
      location = localCache,
      seedRepository = localSeed)
  }

  /** Grabs the additional script files we should clone with templates, if they are available in our environment. */
  def scriptFilesForCloning: Seq[(File, String)] = {
    def fileFor(loc: String, name: String): Option[(File, String)] = Option(loc) map (new File(_)) filter (_.exists) map (_ -> name)

    val batFile = fileFor(ActivatorProperties.ACTIVATOR_LAUNCHER_BAT("/bin/"), "bin/" + SCRIPT_NAME + ".bat")
    val jarFile = fileFor(ActivatorProperties.ACTIVATOR_LAUNCHER_JAR("libexec"), "libexec/" + ActivatorProperties.ACTIVATOR_LAUNCHER_JAR_NAME("libexec"))
    val bashFile = fileFor(ActivatorProperties.ACTIVATOR_LAUNCHER_BASH("/bin/"), "bin/" + SCRIPT_NAME)
    if (jarFile.isDefined && (batFile.isDefined || bashFile.isDefined))
      Seq(batFile, jarFile, bashFile).flatten
    else {
      val batFile = fileFor(ActivatorProperties.ACTIVATOR_LAUNCHER_BAT(""), SCRIPT_NAME + ".bat")
      val jarFile = fileFor(ActivatorProperties.ACTIVATOR_LAUNCHER_JAR(null), ActivatorProperties.ACTIVATOR_LAUNCHER_JAR_NAME(null))
      val bashFile = fileFor(ActivatorProperties.ACTIVATOR_LAUNCHER_BASH(""), SCRIPT_NAME)
      if (jarFile.isDefined && (batFile.isDefined || bashFile.isDefined))
        Seq(batFile, jarFile, bashFile).flatten
      else
        Nil
    }
  }
}
