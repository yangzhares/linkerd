package io.buoyant.linkerd.protocol.h2

import com.fasterxml.jackson.annotation.JsonIgnore
import com.twitter.finagle.buoyant.H2
import com.twitter.finagle.buoyant.h2.{Request, Response, Status}
import com.twitter.finagle._
import com.twitter.logging.Logger
import com.twitter.util.Stopwatch
import io.buoyant.config.types.Port
import io.buoyant.k8s.istio.{DefaultMixerHost, DefaultMixerPort, IstioLoggerBase, MixerClient}
import io.buoyant.linkerd.LoggerInitializer
import io.buoyant.router.context.DstBoundCtx

class IstioLogger(val mixerClient: MixerClient, params: Stack.Params) extends Filter[Request, Response, Request, Response] with IstioLoggerBase {
  private[this] val log = Logger()

  def apply(req: Request, svc: Service[Request, Response]) = {
    val elapsed = Stopwatch.start()

    svc(req).respond { ret =>
      val duration = elapsed()
      val responseCode = ret.toOption.map(_.status).getOrElse(Status.InternalServerError)

      // check for valid istio path
      val istioPath = DstBoundCtx.current.flatMap { bound =>
        bound.id match {
          case path: Path if (path.elems.length == pathLength) => Some(path)
          case _ => None
        }
      }

      val _ = report(istioPath, responseCode.code, req.path, duration)
    }
  }
}

case class IstioLoggerConfig(
  mixerHost: Option[String],
  mixerPort: Option[Port]
) extends H2LoggerConfig {

  @JsonIgnore
  override def role = Stack.Role("IstioLogger")
  @JsonIgnore
  override def description = "Logs telemetry data to Istio Mixer"
  @JsonIgnore
  override def parameters = Seq()

  @JsonIgnore
  private[this] val log = Logger.get("IstioLoggerConfig")

  @JsonIgnore
  private[h2] val host = mixerHost.getOrElse(DefaultMixerHost)
  @JsonIgnore
  private[h2] val port = mixerPort.map(_.port).getOrElse(DefaultMixerPort)
  log.info("connecting to Istio Mixer at %s:%d", host, port)

  @JsonIgnore
  private[this] val mixerDst = Name.bound(Address(host, port))

  @JsonIgnore
  private[this] val mixerService = H2.client
    .withParams(H2.client.params)
    .newService(mixerDst, "istioLogger")

  @JsonIgnore
  private[h2] val client = new MixerClient(mixerService)

  @JsonIgnore
  def mk(params: Stack.Params): Filter[Request, Response, Request, Response] = {
    new IstioLogger(client, params)
  }
}

class IstioLoggerInitializer extends LoggerInitializer {
  val configClass = classOf[IstioLoggerConfig]
  override val configId = "io.l5d.k8s.istio"
}

object IstioLoggerInitializer extends IstioLoggerInitializer
