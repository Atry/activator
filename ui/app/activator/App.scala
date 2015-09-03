/**
 * Copyright (C) 2013 Typesafe <http://typesafe.com/>
 */
package activator

import java.util.UUID
import akka.actor._
import java.util.concurrent.atomic.AtomicInteger
import activator.properties.ActivatorProperties
import java.net.URLEncoder

import akka.util.Timeout

final case class AppIdSocketId(appId: String, socketId: UUID)

class App(val id: AppIdSocketId, val config: AppConfig, val system: ActorSystem,
  val typesafeComActor: ActorRef,
  val lookupTimeout: Timeout) extends ActorWrapper {
  require(config.id == id.appId)

  val appInstance = App.nextInstanceId.getAndIncrement()
  override def toString = s"App(${config.id}@$appInstance})"
  val actorName = "app-" + URLEncoder.encode(config.id, "UTF-8") + "-" + appInstance

  val actor = system.actorOf(Props(new AppActor(config, typesafeComActor, lookupTimeout, (_, _, _) => ())),
    name = actorName)

  system.actorOf(Props(new ActorWatcher(actor, this)), "app-actor-watcher-" + appInstance)

}

object App {
  val nextInstanceId = new AtomicInteger(1)
}
