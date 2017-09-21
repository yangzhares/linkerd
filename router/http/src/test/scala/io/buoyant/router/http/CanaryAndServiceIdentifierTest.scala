package io.buoyant.router.http

import com.twitter.finagle.Path
import com.twitter.finagle.buoyant.Dst
import com.twitter.finagle.http.{Method, Request, Version}
import io.buoyant.router.RoutingFactory.{IdentifiedRequest, UnidentifiedRequest}
import io.buoyant.test.{Awaits, Exceptions}
import org.scalatest.FunSuite

class CanaryAndServiceIdentifierTest extends FunSuite with Awaits with Exceptions {

  test("simple 3-segment service hostname, no canary header") {
    val identifier = CanaryAndServiceIdentifier(Path.Utf8("http"), false)
    val req = Request()
    req.uri = "/"
    req.host = "locus.koalabait.com"
    assert(
      await(identifier(req)).asInstanceOf[IdentifiedRequest[Request]].dst ==
        Dst.Path(Path.read("/http/1.1/disabled/locus"))
    )
  }

  test("simple 3-segment service hostname, simple canary header") {
    val identifier = CanaryAndServiceIdentifier(Path.Utf8("http"), false)
    val req = Request()
    req.uri = "/"
    req.host = "locus.koalabait.com"
    req.headerMap.set("x-cisco-spark-canary-opts", "enabled")
    assert(
      await(identifier(req)).asInstanceOf[IdentifiedRequest[Request]].dst ==
        Dst.Path(Path.read("/http/1.1/enabled/locus"))
    )
  }

  test("simple 4-segment service hostname, no canary header") {
    val identifier = CanaryAndServiceIdentifier(Path.Utf8("http"), false)
    val req = Request()
    req.uri = "/"
    req.host = "locus.foobar.koalabait.com"
    assert(
      await(identifier(req)).asInstanceOf[IdentifiedRequest[Request]].dst ==
        Dst.Path(Path.read("/http/1.1/disabled/locus"))
    )
  }

  test("simple 4-segment service hostname, simple canary header") {
    val identifier = CanaryAndServiceIdentifier(Path.Utf8("http"), false)
    val req = Request()
    req.uri = "/"
    req.host = "locus.foobar.koalabait.com"
    req.headerMap.set("x-cisco-spark-canary-opts", "enabled")
    assert(
      await(identifier(req)).asInstanceOf[IdentifiedRequest[Request]].dst ==
        Dst.Path(Path.read("/http/1.1/enabled/locus"))
    )
  }

  test("public service hostname, no canary header") {
    val identifier = CanaryAndServiceIdentifier(Path.Utf8("http"), false)
    val req = Request()
    req.uri = "/"
    req.host = "locus-foobar.koalabait.com"
    assert(
      await(identifier(req)).asInstanceOf[IdentifiedRequest[Request]].dst ==
        Dst.Path(Path.read("/http/1.1/disabled/locus"))
    )
  }

  test("public service hostname, simple canary header") {
    val identifier = CanaryAndServiceIdentifier(Path.Utf8("http"), false)
    val req = Request()
    req.uri = "/"
    req.host = "locus-foobar.koalabait.com"
    req.headerMap.set("x-cisco-spark-canary-opts", "enabled")
    assert(
      await(identifier(req)).asInstanceOf[IdentifiedRequest[Request]].dst ==
        Dst.Path(Path.read("/http/1.1/enabled/locus"))
    )
  }

  test("complex service hostname, no canary header") {
    val identifier = CanaryAndServiceIdentifier(Path.Utf8("http"), false)
    val req = Request()
    req.uri = "/"
    req.host = "locus-foo-bar.koalabait.com"
    assert(
      await(identifier(req)).asInstanceOf[IdentifiedRequest[Request]].dst ==
        Dst.Path(Path.read("/http/1.1/disabled/locus-foo"))
    )
  }

  test("complex service hostname, simple canary header") {
    val identifier = CanaryAndServiceIdentifier(Path.Utf8("http"), false)
    val req = Request()
    req.uri = "/"
    req.host = "locus-foo-bar.koalabait.com"
    req.headerMap.set("x-cisco-spark-canary-opts", "enabled")
    assert(
      await(identifier(req)).asInstanceOf[IdentifiedRequest[Request]].dst ==
        Dst.Path(Path.read("/http/1.1/enabled/locus-foo"))
    )
  }

  test("complex canary header, target service not specified") {
    val identifier = CanaryAndServiceIdentifier(Path.Utf8("http"), false)
    val req = Request()
    req.uri = "/"
    req.host = "locus-foo.koalabait.com"
    req.headerMap.set("x-cisco-spark-canary-opts", "disabled,conversation=always")
    assert(
      await(identifier(req)).asInstanceOf[IdentifiedRequest[Request]].dst ==
        Dst.Path(Path.read("/http/1.1/disabled/locus"))
    )
  }

  test("complex canary header, target service specified with default") {
    val identifier = CanaryAndServiceIdentifier(Path.Utf8("http"), false)
    val req = Request()
    req.uri = "/"
    req.host = "locus-foo.koalabait.com"
    req.headerMap.set("x-cisco-spark-canary-opts", "disabled,locus=always")
    assert(
      await(identifier(req)).asInstanceOf[IdentifiedRequest[Request]].dst ==
        Dst.Path(Path.read("/http/1.1/always/locus"))
    )
  }

  test("complex canary header, target service specified with no default") {
    val identifier = CanaryAndServiceIdentifier(Path.Utf8("http"), false)
    val req = Request()
    req.uri = "/"
    req.host = "locus-foo.koalabait.com"
    req.headerMap.set("x-cisco-spark-canary-opts", "locus=always")
    assert(
      await(identifier(req)).asInstanceOf[IdentifiedRequest[Request]].dst ==
        Dst.Path(Path.read("/http/1.1/always/locus"))
    )
  }

  test("complex canary header, target service not specified with no default") {
    val identifier = CanaryAndServiceIdentifier(Path.Utf8("http"), false)
    val req = Request()
    req.uri = "/"
    req.host = "locus-foo.koalabait.com"
    req.headerMap.set("x-cisco-spark-canary-opts", "conversation=always")
    assert(
      await(identifier(req)).asInstanceOf[IdentifiedRequest[Request]].dst ==
        Dst.Path(Path.read("/http/1.1/disabled/locus"))
    )
  }

  test("complex canary header, multiple services specified with default") {
    val identifier = CanaryAndServiceIdentifier(Path.Utf8("http"), false)
    val req = Request()
    req.uri = "/"
    req.host = "locus-foo.koalabait.com"
    req.headerMap.set("x-cisco-spark-canary-opts", "disabled,conversation=enabled,locus=always,raindrop=enabled")
    assert(
      await(identifier(req)).asInstanceOf[IdentifiedRequest[Request]].dst ==
        Dst.Path(Path.read("/http/1.1/always/locus"))
    )
  }

  test("complex canary header, multiple services specified with no default") {
    val identifier = CanaryAndServiceIdentifier(Path.Utf8("http"), false)
    val req = Request()
    req.uri = "/"
    req.host = "locus-foo.koalabait.com"
    req.headerMap.set("x-cisco-spark-canary-opts", "conversation=enabled,locus=always,raindrop=enabled")
    assert(
      await(identifier(req)).asInstanceOf[IdentifiedRequest[Request]].dst ==
        Dst.Path(Path.read("/http/1.1/always/locus"))
    )
  }

  test("nonsense header, single value") {
    val identifier = CanaryAndServiceIdentifier(Path.Utf8("http"), false)
    val req = Request()
    req.uri = "/"
    req.host = "locus-foo.koalabait.com"
    req.headerMap.set("x-cisco-spark-canary-opts", "aiubviyb3q4v")
    assert(
      await(identifier(req)).asInstanceOf[IdentifiedRequest[Request]].dst ==
        Dst.Path(Path.read("/http/1.1/disabled/locus"))
    )
  }

  test("nonsense header, single value, service name") {
    val identifier = CanaryAndServiceIdentifier(Path.Utf8("http"), false)
    val req = Request()
    req.uri = "/"
    req.host = "locus-foo.koalabait.com"
    req.headerMap.set("x-cisco-spark-canary-opts", "locus")
    assert(
      await(identifier(req)).asInstanceOf[IdentifiedRequest[Request]].dst ==
        Dst.Path(Path.read("/http/1.1/disabled/locus"))
    )
  }

  test("nonsense header, single value, service name = nonsense") {
    val identifier = CanaryAndServiceIdentifier(Path.Utf8("http"), false)
    val req = Request()
    req.uri = "/"
    req.host = "locus-foo.koalabait.com"
    req.headerMap.set("x-cisco-spark-canary-opts", "locus=bababbovuwb")
    assert(
      await(identifier(req)).asInstanceOf[IdentifiedRequest[Request]].dst ==
        Dst.Path(Path.read("/http/1.1/disabled/locus"))
    )
  }

  test("nonsense header, multiple values") {
    val identifier = CanaryAndServiceIdentifier(Path.Utf8("http"), false)
    val req = Request()
    req.uri = "/"
    req.host = "locus-foo.koalabait.com"
    req.headerMap.set("x-cisco-spark-canary-opts", "boiubaer,baoy34ybvn")
    assert(
      await(identifier(req)).asInstanceOf[IdentifiedRequest[Request]].dst ==
        Dst.Path(Path.read("/http/1.1/disabled/locus"))
    )
  }

  test("nonsense header, valid default") {
    val identifier = CanaryAndServiceIdentifier(Path.Utf8("http"), false)
    val req = Request()
    req.uri = "/"
    req.host = "locus-foo.koalabait.com"
    req.headerMap.set("x-cisco-spark-canary-opts", "always,boiubaer,baoy34ybvn")
    assert(
      await(identifier(req)).asInstanceOf[IdentifiedRequest[Request]].dst ==
        Dst.Path(Path.read("/http/1.1/always/locus"))
    )
  }

  test("nonsense header, valid non-default") {
    val identifier = CanaryAndServiceIdentifier(Path.Utf8("http"), false)
    val req = Request()
    req.uri = "/"
    req.host = "locus-foo.koalabait.com"
    req.headerMap.set("x-cisco-spark-canary-opts", "by7qv34ibvb,locus=enabled,baoy34ybvn")
    assert(
      await(identifier(req)).asInstanceOf[IdentifiedRequest[Request]].dst ==
        Dst.Path(Path.read("/http/1.1/enabled/locus"))
    )
  }
}
