package helpers

import sttp.client3._
import sttp.client3.circe._
import io.circe.generic.auto._
import io.circe.syntax._
import sttp.model.MediaType
import scala.util.{Try, Success, Failure}

object requestHelpers {
  // Define an implicit sttp backend. Here we are using the synchronous backend.
  implicit val backend: SttpBackend[Identity, Any] = HttpURLConnectionBackend()

  // A function to perform a GET request and return the response
  def get[T](url: String)(implicit decoder: io.circe.Decoder[T]): Either[String, T] = {
    val request = basicRequest
      .get(uri"$url")
      .response(asJson[T])

    request.send(backend) match {
      case Response(Right(body), _, _, _, _,_) => { println(body)
        Right(body)}
      case Response(Left(error), _, _, _, _,_) => { println(error.getMessage)
        Left(error.getMessage)}
    }
  }

  // A function to perform a POST request with a JSON body and return the response
  def post[T,T2](url: String, requestBody: T2)(implicit encoder: io.circe.Encoder[T2], decoder: io.circe.Decoder[T]): Either[String, T] = {
    val jsonBody = requestBody.asJson.noSpaces
    val request = basicRequest
      .post(uri"$url")
      .contentType(MediaType.ApplicationJson)
      .body(jsonBody)
      .response(asJson[T])

    request.send(backend) match {
      case Response(Right(body), _, _, _, _,_) => Right(body)
      case Response(Left(error), _, _, _, _,_) => Left(error.getMessage)
    }
  }

  // usage example
//  def main(args: Array[String]): Unit = {
//    val url = "http://localhost:8080/queryagent"
//
//    case class queryAgentRequest(val agentName: String)
//    case class ComparableNode(
//                               id: Int = -1,
//                               props: List[Int] = List.empty,
//                               childPropsHash: List[Int] = List.empty,
//                               valuableFlag: Boolean = false
//                             )
//    case class AgentData(
//                          name: String,
//                          currentLocation: ComparableNode,
//                          adjacentNodes: List[ComparableNode]
//                        )
//    val requestBody = queryAgentRequest("thief")
//    val response = post[AgentData, queryAgentRequest](url, requestBody)
//    println(response.getOrElse("Error"))
//  }
}

