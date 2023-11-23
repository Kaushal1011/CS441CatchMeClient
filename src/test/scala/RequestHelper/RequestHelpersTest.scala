package RequestHelper
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import helpers.requestHelpers
import sttp.client3._
import io.circe._
import io.circe.generic.auto._


class RequestHelpersTest extends AnyFlatSpec with Matchers {

  implicit val backend: SttpBackend[Identity, Any] = HttpURLConnectionBackend()

  "get" should "retrieve data from a public API" in {
    val response = requestHelpers.get[Json]("https://httpbin.org/get")
    println(response)
    response.isRight shouldBe true
  }

  "get" should "return decoding error " in {
    val response = requestHelpers.get[Map[String, String]]("https://httpbin.org/get")
    println(response)
    response.isLeft shouldBe true
  }

  "post" should "send data to a public API and receive a response" in {
    val requestBody = Map("key" -> "value")
    val response = requestHelpers.post[Json, Map[String, String]]("https://httpbin.org/post", requestBody)
    println(response)
    response.isRight shouldBe true
  }

  "post" should "return decoding error " in {
    val requestBody = Map("key" -> "value")
    val response = requestHelpers.post[Map[String, String], Map[String, String]]("https://httpbin.org/post", requestBody)
    println(response)
    response.isLeft shouldBe true
  }

}
