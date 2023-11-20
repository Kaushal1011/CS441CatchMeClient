package Game

import com.google.common.graph.{MutableGraph, GraphBuilder}
import helpers.NodeDataParser
import scala.annotation.tailrec
import scala.io.Source
import scala.jdk.CollectionConverters._
import helpers.requestHelpers.get
import io.circe.generic.auto._
import scala.util.Using

case class ComparableNode(
                           id: Int = -1,
                           props: List[Int] = List.empty,
                           childPropsHash: List[Int] = List.empty,
                           valuableFlag: Boolean = false
                         )


case class ConfidenceScore(
                            id: Int = -1,
                            score: Double = 0.0
                          )

case class AgentData(
                      name:String,
                      currentLocation:ComparableNode,
                      adjacentNodes:List[ComparableNode],
                      confidenceScores: List[ConfidenceScore] = List.empty,
                      valuableDataDistance: Int = -1
                    )

case class GameState(policeLoc: Int, thiefLoc: Int, winner: String)

case class AgentDataRequest(agentName: String)

case class ActionRequest(agentName: String, moveToId: Int)


object Utilities {

  def findDistanceToNode(graph: MutableGraph[ComparableNode], startId: Int, endId: Int): Option[Int] = {
    // A helper method to find a node by id
    def findNode(graph: MutableGraph[ComparableNode], nodeId: Int): Option[ComparableNode] =
      graph.nodes().asScala.find(_.id == nodeId)

    // Tail-recursive BFS implementation
    @tailrec
    def bfs(frontier: List[(ComparableNode, Int)], visited: Set[Int]): Option[Int] = frontier match {
      case (node, dist) :: rest if node.id == endId => Some(dist)
      case (node, dist) :: rest if !visited.contains(node.id) =>
        val neighbors = graph.adjacentNodes(node).iterator().asScala
          .filterNot(n => visited.contains(n.id))
          .toList.map(n => (n, dist + 1))
        bfs(rest ++ neighbors, visited + node.id)
      case _ :: rest => bfs(rest, visited)
      case Nil => None
    }

    for {
      startNode <- findNode(graph, startId)
      endNode <- findNode(graph, endId)
      if graph.nodes().contains(startNode) && graph.nodes().contains(endNode)
    } yield bfs(List((startNode, 0)), Set.empty[Int]).getOrElse(-1)
  }


  def findDistanceToValuableData(graph: MutableGraph[ComparableNode], startId: Int): Option[Int] = {
    // A helper method to find a node by id
    def findNode(graph: MutableGraph[ComparableNode], nodeId: Int): Option[ComparableNode] =
      graph.nodes().asScala.find(_.id == nodeId)

    // Tail-recursive BFS implementation
    @tailrec
    def bfs(frontier: List[(ComparableNode, Int)], visited: Set[Int]): Option[Int] = frontier match {
      case (node, dist) :: rest if node.valuableFlag => Some(dist)
      case (node, dist) :: rest if !visited.contains(node.id) =>
        val neighbors = graph.adjacentNodes(node).iterator().asScala
          .filterNot(n => visited.contains(n.id))
          .toList.map(n => (n, dist + 1))
        bfs(rest ++ neighbors, visited + node.id)
      case _ :: rest => bfs(rest, visited)
      case Nil => None
    }

    for {
      startNode <- findNode(graph, startId)
      if graph.nodes().contains(startNode)
    } yield bfs(List((startNode, 0)), Set.empty[Int]).getOrElse(-1)
  }

  def loadGraph(graphPath: String): MutableGraph[ComparableNode] = {
    val graph = GraphBuilder.undirected().build[ComparableNode]()
    val nodeMap = scala.collection.mutable.Map[Int, ComparableNode]()

    def getNode(id: Int, props: List[Int], childrenPropsHash: List[Int], valuable: Boolean): ComparableNode = {
      nodeMap.getOrElseUpdate(id, ComparableNode(id, props, childrenPropsHash, valuable))
    }

    Using(if (graphPath.startsWith("http://") || graphPath.startsWith("https://")) {
      Source.fromURL(graphPath)
    } else {
      Source.fromFile(graphPath)
    }) { source =>
      source.getLines().foreach { line =>
        val edge = NodeDataParser.parseEdgeData(line)
        val srcNode = getNode(edge.srcId, edge.propertiesSrc, edge.children_prop_hash_destination, edge.valuableSrc)
        val dstNode = getNode(edge.dstId, edge.propertiesDst, edge.children_prop_hash_destination, edge.valuableDst)
        graph.putEdge(srcNode, dstNode)
      }
    } match {
      case scala.util.Success(_) => graph
      case scala.util.Failure(ex) =>
        println(s"Failed to load graph: ${ex.getMessage}")
        GraphBuilder.undirected().build[ComparableNode]() // Return an empty graph or handle the error as appropriate
    }
  }

  case class queryGraphRequest(val queryGraphPath: String)
  def requestAndLoadGraph(apiUrl:String): MutableGraph[ComparableNode] = {
    val graphPath:String = get[queryGraphRequest](apiUrl + "/querygraph").getOrElse(queryGraphRequest("Error")).queryGraphPath
    println("Loading graph from: " + graphPath)
    loadGraph(graphPath)
  }

}
// Example usage:
// val graph: MutableGraph[ComparableNode] = GraphBuilder.undirected().build[ComparableNode]()
// Add nodes and edges to the graph
// val distanceOpt = findDistance(graph, startId = 1, endId = 5)
// distanceOpt.foreach(distance => println(s"The distance is: $distance"))