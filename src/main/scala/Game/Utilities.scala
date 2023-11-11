package Game

import com.google.common.graph.{MutableGraph, GraphBuilder}
import helpers.NodeDataParser
import scala.annotation.tailrec
import scala.io.Source
import scala.jdk.CollectionConverters._
import helpers.requestHelpers.get
import io.circe.generic.auto._

case class ComparableNode(
                           id: Int = -1,
                           props: List[Int] = List.empty,
                           childPropsHash: List[Int] = List.empty,
                           valuableFlag: Boolean = false
                         )


case class AgentData(
                      name:String,
                      currentLocation:ComparableNode,
                      adjacentNodes:List[ComparableNode]
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
        val neighbors = graph.successors(node).iterator().asScala
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
        val neighbors = graph.successors(node).iterator().asScala
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

    println("Loading graph from: " + graphPath)

    val graph = GraphBuilder.directed().build[ComparableNode]()

    val source: Source = if (graphPath.startsWith("http://") || graphPath.startsWith("https://")) {
      Source.fromURL(graphPath)
    } else {
      Source.fromFile(graphPath)
    }

    // Load edges
    source.getLines().foreach { line =>
      val edge = NodeDataParser.parseEdgeData(line)
      val srcNode = ComparableNode(edge.srcId, edge.propertiesSrc, edge.children_prop_hash_destination, edge.valuableSrc)
      val dstNode = ComparableNode(edge.dstId, edge.propertiesDst, edge.children_prop_hash_destination, edge.valuableDst)
      // this method silently creates nodes if they don't exist
      graph.putEdge(srcNode, dstNode)
    }

    graph
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