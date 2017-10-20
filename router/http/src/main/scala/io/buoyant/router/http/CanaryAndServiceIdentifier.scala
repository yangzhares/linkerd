package io.buoyant.router.http

import com.twitter.finagle.buoyant.Dst
import com.twitter.finagle.http.{Request, Version}
import com.twitter.finagle.{Dtab, Path}
import com.twitter.util.Future
import io.buoyant.router.RoutingFactory
import io.buoyant.router.RoutingFactory.{IdentifiedRequest, RequestIdentification, UnidentifiedRequest}

object CanaryAndServiceIdentifier {
  def mk(
    prefix: Path,
    baseDtab: () => Dtab = () => Dtab.base
  ): RoutingFactory.Identifier[Request] = CanaryAndServiceIdentifier(prefix, false, baseDtab)
}

case class CanaryAndServiceIdentifier(
  prefix: Path,
  uris: Boolean = false,
  baseDtab: () => Dtab = () => Dtab.base
) extends RoutingFactory.Identifier[Request] {

  val VALID_HEADER_VALUES = List("disabled", "enabled", "always")

  private[this] def suffix(req: Request): Path =
    if (uris) Path.read(req.path) else Path.empty

  private[this] def mkPath(path: Path): Dst.Path =
    Dst.Path(prefix ++ path, baseDtab(), Dtab.local)

  def parseCanaryOptionsHeaders(servicename: String, optionsheaders: String): String = {
    val optionsarray = optionsheaders.split(",")
    for (optionsheader <- optionsarray) {
      if (optionsheader contains '=') {
        val optionsval = optionsheader.split("=")
        if (servicename.equals(optionsval(0))) {
          if (VALID_HEADER_VALUES contains optionsval(1)) {
            return optionsval(1)
          }
        }
      }
    }
    if (!(optionsarray(0) contains '=')) {
      if (VALID_HEADER_VALUES contains optionsarray(0)) {
        return optionsarray(0)
      }
    }
    return "disabled"
  }

  def parseVersionOptionsHeaders(servicename: String, optionsheaders: String): String = {
    val optionsarray = optionsheaders.split(",")
    for (optionsheader <- optionsarray) {
      if (optionsheader contains '=') {
        val optionsval = optionsheader.split("=")
        if (servicename.equals(optionsval(0))) {
          return optionsval(1)
        }
      }
    }
    if (!(optionsarray(0) contains '=')) {
      return optionsarray(0)
    }
    return "disabled"
  }

  def apply(req: Request): Future[RequestIdentification[Request]] = req.version match {
    case Version.Http10 =>
      val dst = mkPath(Path.Utf8("1.0", req.method.toString) ++ suffix(req))
      Future.value(new IdentifiedRequest(dst, req))

    case Version.Http11 =>
      req.host match {
        case Some(host) if host.nonEmpty =>
          val servicename = host.split('.').drop(1).length match {
            case 2 => if (host.split('.')(0) contains '-') host.split('.')(0).split('-').dropRight(1).mkString("-") else host.split('.')(0)
            case _ => host.split('.')(0)
          }
          val canaryOptionsheaders = req.headerMap.getOrElse("x-cisco-spark-canary-opts", "disabled")
          val versionOptionsheaders = req.headerMap.getOrElse("x-cisco-spark-version-opts", "disabled")
          val canaryTag = parseCanaryOptionsHeaders(servicename, canaryOptionsheaders)
          val versionTag = parseVersionOptionsHeaders(servicename, versionOptionsheaders)
          val tag = if (versionTag != "disabled") versionTag else canaryTag
          //System.err.format("computed canary tag: %s\n", tag)
          val dst = mkPath(Path.Utf8("1.1", tag, servicename) ++ suffix(req))
          //System.err.format("CanaryAndServiceIdentifier: returning path: %s\n\n", dst.toString)
          Future.value(new IdentifiedRequest(dst, req))
        case _ =>
          Future.value(
            new UnidentifiedRequest(
              s"${Version.Http11} request missing hostname"
            )
          )
      }
  }

}
