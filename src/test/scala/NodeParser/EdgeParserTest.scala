package NodeParser

import helpers.{ComparableEdge, NodeDataParser}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import scala.util.Using

class EdgeParserTest extends AnyFlatSpec with Matchers {

  val repeatable: Boolean = true
  behavior of "NodeParser for Nodes"

  it should "parse an edge stringrep into a comparable edge" in {
    val edgeString = "(0, 13, 0.5666946724261208, List(), List(79, 37, 59, 14, 99, 98, 55, 50), List(-1349935424, 1077067315, 110373521, 2109098941), List(1389696985, 977685876, 944514158, 888249067, -1883113608, -667623957), true, false)"
    val edge = NodeDataParser.parseEdgeData(edgeString)
    val expectedEdge = new ComparableEdge(0, 13, 0.5666946724261208, List(), List(79, 37, 59, 14, 99, 98, 55, 50), List(-1349935424, 1077067315, 110373521, 2109098941), List(1389696985, 977685876, 944514158, 888249067, -1883113608, -667623957), true, false)

    edge.toString shouldBe expectedEdge.toString
  }

  it should "be able to parse in format string rep|string rep which is the output of the mapper job" in {
    val resourcePath = "secondjob.txt"
    Using(scala.io.Source.fromResource(resourcePath)) { source =>
      val lines = source.getLines.toList

      for ((line, lineNumber) <- lines.zipWithIndex) {
        try {
          val edge1 = NodeDataParser.parseEdgeData(line.trim)
        } catch {
          case e: Exception =>
            fail(s"Error processing line $lineNumber: ${e.getMessage}")
        }
      }
    } recover {
      case e: Exception =>
        fail(s"Error reading the file: ${e.getMessage}")
    }
  }
}
