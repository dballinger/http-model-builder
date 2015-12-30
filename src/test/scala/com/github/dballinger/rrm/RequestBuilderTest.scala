package com.github.dballinger.rrm

import com.github.tomakehurst.wiremock.WireMockServer
import org.scalatest.{BeforeAndAfterEach, Matchers, FlatSpec}

class RequestBuilderTest extends FlatSpec with MockHttpSupport with Matchers with BeforeAndAfterEach {

  import com.github.tomakehurst.wiremock.client.WireMock._
  import RequestBuilder._

  var wiremock: WireMockServer = _

  override protected def beforeEach(): Unit = {
    super.beforeEach()
    wiremock = startMockHttp()
  }

  override protected def afterEach(): Unit = {
    stopMockHttp()
    super.afterEach()
  }

  "RequestBuilder" should "respond with status" in {
    val expectedStatus = 202
    val path = "/res"
    wiremock.stubFor(
      get(urlEqualTo(path))
          .willReturn(aResponse().withStatus(expectedStatus))
    )
    val actualStatus = aRequestFor(urlForPath(path)).get().status

    actualStatus shouldBe expectedStatus
  }

  it should "GET a resource" in {
    val path = "/res"
    wiremock.stubFor(
      get(urlEqualTo(path))
          .willReturn(aResponse())
    )
    val actualStatus = aRequestFor(urlForPath(path)).get().status

    actualStatus shouldBe 200
  }

  it should "POST a resource" in {
    val path = "/res"
    wiremock.stubFor(
      post(urlEqualTo(path))
          .willReturn(aResponse())
    )
    val actualStatus = aRequestFor(urlForPath(path)).post().status

    actualStatus shouldBe 200
  }

  it should "PUT a resource" in {
    val path = "/res"
    wiremock.stubFor(
      put(urlEqualTo(path))
          .willReturn(aResponse())
    )
    val actualStatus = aRequestFor(urlForPath(path)).put().status

    actualStatus shouldBe 200
  }

  it should "DELETE a resource" in {
    val path = "/res"
    wiremock.stubFor(
      delete(urlEqualTo(path))
          .willReturn(aResponse())
    )
    val actualStatus = aRequestFor(urlForPath(path)).delete().status

    actualStatus shouldBe 200
  }

  it should "PATCH a resource" in {
    val path = "/res"
    wiremock.stubFor(
      patch(urlEqualTo(path))
          .willReturn(aResponse())
    )
    val actualStatus = aRequestFor(urlForPath(path)).patch().status

    actualStatus shouldBe 200
  }

  it should "OPTIONS a resource" in {
    val path = "/res"
    wiremock.stubFor(
      options(urlEqualTo(path))
          .willReturn(aResponse())
    )
    val actualStatus = aRequestFor(urlForPath(path)).options().status

    actualStatus shouldBe 200
  }

  it should "POST data" in {
    val path = "/res"
    val data = "data"
    wiremock.stubFor(
      post(urlEqualTo(path))
          .withRequestBody(equalTo(data))
          .willReturn(aResponse())
    )
    val actualStatus = aRequestFor(urlForPath(path))
        .withBody(data)
        .post()
        .status

    actualStatus shouldBe 200
  }

  it should "PUT data" in {
    val path = "/res"
    val data = "data"
    wiremock.stubFor(
      put(urlEqualTo(path))
          .withRequestBody(equalTo(data))
          .willReturn(aResponse())
    )
    val actualStatus = aRequestFor(urlForPath(path))
        .withBody(data)
        .put()
        .status

    actualStatus shouldBe 200
  }

  it should "PATCH data" in {
    val path = "/res"
    val data = "data"
    wiremock.stubFor(
      patch(urlEqualTo(path))
          .withRequestBody(equalTo(data))
          .willReturn(aResponse())
    )
    val actualStatus = aRequestFor(urlForPath(path))
        .withBody(data)
        .patch()
        .status

    actualStatus shouldBe 200
  }

  it should "send headers" in {
    val path = "/res"
    val h1Name = "a"
    val h1Value = "1"
    val h2Name = "b"
    val h2Value = "2"
    wiremock.stubFor(
      get(urlEqualTo(path))
          .withHeader(h1Name, equalTo(h1Value))
          .withHeader(h2Name, equalTo(h2Value))
          .willReturn(aResponse())
    )
    val actualStatus = aRequestFor(urlForPath(path))
        .withHeader(h1Name, h1Value)
        .withHeader(h2Name, h2Value)
        .get()
        .status

    actualStatus shouldBe 200
  }

  it should "send query params" in {
    val path = "/res"
    val p1Name = "a"
    val p1Value = "1"
    val p2Name = "b"
    val p2Value = "2"
    wiremock.stubFor(
      get(urlPathEqualTo(path))
          .withQueryParam(p1Name, equalTo(p1Value))
          .withQueryParam(p2Name, equalTo(p2Value))
          .willReturn(aResponse())
    )
    val actualStatus = aRequestFor(urlForPath(path))
        .withQueryParam(p1Name, p1Value)
        .withQueryParam(p2Name, p2Value)
        .get()
        .status

    actualStatus shouldBe 200
  }

  it should "not allow get, delete, options to be called after adding a body and setting a header" in {
    val req = aRequestFor(urlForPath("/whatever"))
        .withBody("")
        .withHeader("a", "1")
    "req.get()" shouldNot compile
    "req.delete()" shouldNot compile
    "req.options()" shouldNot compile
  }

  it should "not allow get, delete, options to be called after adding a body and setting a query param" in {
    val req = aRequestFor(urlForPath("/whatever"))
        .withBody("")
        .withQueryParam("a", "1")
    "req.get()" shouldNot compile
    "req.delete()" shouldNot compile
    "req.options()" shouldNot compile
  }
}

trait MockHttpSupport {

  private[this] var server: Option[WireMockServer] = None

  def startMockHttp(): WireMockServer = {
    if (server.isEmpty) {
      val s = new WireMockServer(0)
      server = Option(s)
      s.start()
      s
    } else throw new IllegalStateException("Wiremock server has already been started")
  }

  def stopMockHttp(): Unit = server foreach {
    s =>
      s.stop()
      server = None
  }

  def urlForPath(path: String): String = server.map {
    s =>
      s"http://localhost:${s.port()}$path"
  }.getOrElse(throw new IllegalStateException("Wiremock server has not been started"))
}
