package io.buoyant.namer.consul

import com.fasterxml.jackson.annotation.JsonIgnore
import com.twitter.finagle._
import com.twitter.finagle.tracing.NullTracer
import io.buoyant.config.types.Port
import io.buoyant.consul.utils.RichConsulClient
import io.buoyant.consul.v1
import io.buoyant.consul.v1.{HealthStatus, ConsistencyMode}
import io.buoyant.namer.{NamerConfig, NamerInitializer}

/**
 * Supports namer configurations in the form:
 *
 * <pre>
 * namers:
 * - kind: io.l5d.consulcanary
 *   host: consul.site.biz
 *   port: 8600
 *   includeTag: true
 *   useHealthCheck: false
 *   setHost: true
 *   token: some-consul-acl-token
 *   consistencyMode: default
 *   failFast: false
 *   preferServiceAddress: true
 * </pre>
 */
class ConsulCanaryInitializer extends NamerInitializer {
  val configClass = classOf[ConsulCanaryConfig]
  override def configId = "io.l5d.consulcanary"
}

object ConsulCanaryInitializer extends ConsulCanaryInitializer

case class ConsulCanaryConfig(
  host: Option[String],
  port: Option[Port],
  includeTag: Option[Boolean],
  useHealthCheck: Option[Boolean],
  token: Option[String] = None,
  setHost: Option[Boolean] = None,
  consistencyMode: Option[ConsistencyMode] = None,
  failFast: Option[Boolean] = None,
  preferServiceAddress: Option[Boolean] = None
) extends NamerConfig {

  @JsonIgnore
  override def defaultPrefix: Path = Path.read("/io.l5d.consulcanary")

  private[this] def getHost = host.getOrElse("localhost")
  private[this] def getPort = port match {
    case Some(p) => p.port
    case None => 8500
  }

  /**
   * Build a Namer backed by Consul.
   */
  @JsonIgnore
  def newNamer(params: Stack.Params): Namer = {
    val service = Http.client
      .withParams(Http.client.params ++ params)
      .withLabel(prefix.show.stripPrefix("/"))
      .interceptInterrupts
      .failFast(failFast)
      .setAuthToken(token)
      .ensureHost(host, port)
      .withTracer(NullTracer)
      .newService(s"/$$/inet/$getHost/$getPort")

    val consul = useHealthCheck match {
      case Some(true) => v1.HealthApi(service, Set(HealthStatus.Passing))
      case _ => v1.CatalogApi(service)
    }
    val agent = v1.AgentApi(service)

    val stats = params[param.Stats].statsReceiver.scope(prefix.show.stripPrefix("/"))

    ConsulCanaryNamer.tagged(
      prefix, consul, agent, setHost.getOrElse(false), consistencyMode, preferServiceAddress, stats
    )
  }
}
