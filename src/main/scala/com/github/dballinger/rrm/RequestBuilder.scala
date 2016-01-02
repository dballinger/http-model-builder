package com.github.dballinger.rrm

import java.io.ByteArrayOutputStream
import java.net.URI
import java.nio.charset.Charset

import org.apache.http.client.methods._
import org.apache.http.client.utils.URIBuilder
import org.apache.http.entity.StringEntity
import org.apache.http.impl.client.HttpClientBuilder

import scala.io.{Codec, Source}
import scala.util.Try

class DataSendingRequestBuilder(baseUrl: String, body: Option[String], headers: Map[String, String], params: Map[String, String]) {

  def withHeader(name: String, value: String): this.type = new RequestBuilder(baseUrl, body, headers + (name -> value), params).asInstanceOf[this.type]

  def withQueryParam(name: String, value: String): this.type = new RequestBuilder(baseUrl, body, headers, params + (name -> value)).asInstanceOf[this.type]

  def post(): Response = {
    val request = new HttpPost(uri)
    addBody(request)
    ex(request)
  }

  def put(): Response = {
    val request = new HttpPut(uri)
    addBody(request)
    ex(request)
  }

  def patch(): Response = {
    val request = new HttpPatch(uri)
    addBody(request)
    ex(request)
  }

  protected def ex(request: HttpUriRequest) = {
    val client = HttpClientBuilder.create().build()
    headers foreach {
      h =>
        val (name, value) = h
        request.addHeader(name, value)
    }
    val httpResponse = client.execute(request)
    val response = new Response {

      import scala.collection.JavaConverters._

      val status = httpResponse.getStatusLine.getStatusCode

      val headers = httpResponse.getAllHeaders.map {
        header =>
          (header.getName, header.getValue)
      }.toMap

      val bodyAsString = {
        val entity = httpResponse.getEntity
        val out = new ByteArrayOutputStream()
        entity.writeTo(out)
        val charset = Try {
          entity.getContentType.getElements.flatMap {
            element =>
              element.getParameters
          }.find {
            param =>
              param.getName == "charset"
          }.map {
            param =>
              Charset.forName(param.getValue)
          }
        }.toOption.flatten.getOrElse(Charset.forName("utf-8"))

        new String(out.toByteArray, charset)
      }
    }
    client.close()
    response
  }

  private def addBody(request: HttpEntityEnclosingRequestBase): Unit = {
    body foreach {
      b =>
        request.setEntity(new StringEntity(b))
    }
  }

  protected def uri: URI = params.foldLeft(new URIBuilder(baseUrl)) {
    (builder, p) =>
      val (name, value) = p
      builder.addParameter(name, value)
  }.build()
}

class RequestBuilder(baseUrl: String, body: Option[String] = None, headers: Map[String, String], params: Map[String, String]) extends DataSendingRequestBuilder(baseUrl, body, headers, params) {

  def withBody(body: String): DataSendingRequestBuilder = new DataSendingRequestBuilder(baseUrl, Some(body), headers, params)

  def get(): Response = {
    ex(new HttpGet(uri))
  }

  def delete(): Response = {
    ex(new HttpDelete(uri))
  }

  def options(): Response = {
    ex(new HttpOptions(uri))
  }

}

object RequestBuilder {
  def aRequestFor(url: String): RequestBuilder = new RequestBuilder(url, None, Map(), Map())
}

trait Response {

  def status: Int

  def headers: Map[String, String]

  def bodyAsString: String
}
