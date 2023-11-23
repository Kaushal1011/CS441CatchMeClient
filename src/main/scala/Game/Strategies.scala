package Game
import com.google.common.graph.{GraphBuilder, MutableGraph}
import helpers.requestHelpers.post
import io.circe.generic.auto._
import Utilities.{findDistanceToNode, findDistanceToValuableData}
import scala.jdk.CollectionConverters._

// this object defined the most basic strategies for the agents
// contains the base strategy class and a simple strategy that picks a random node from the neighbours
// the chase enemy node strategy picks the node with the lowest distance to the other agent
// the chase valuable data strategy picks the node with the lowest distance to the valuable data
object Strategies {

  // base strategy class that defines the basic functions for the strategies
  class BaseStrategy (val agentName: String, val apiUrl: String) {

    def getAgentState(agentNameParam: String): AgentData = {
      val requestBody = AgentDataRequest(agentNameParam)
      val response = post[AgentData, AgentDataRequest](apiUrl + "/queryagent", requestBody)
      response.getOrElse(AgentData("Error", ComparableNode(), List.empty))
    }


    // extremely simple strategy just pick random node from neighbours
    // game may never end as nodes get stuck if query and regional graph differ
    def decideMove(): ActionRequest = {

      // pick a random node from list of neighbours
      val agentState = getAgentState(agentName)
      val neighbours = agentState.adjacentNodes

      if (neighbours.isEmpty) {
        // pick a completely random node from query graph
        ActionRequest(agentName, -1)
      }
      else{
        val moveToId = neighbours(scala.util.Random.nextInt(neighbours.length)).id
        ActionRequest(agentName, moveToId)
      }

    }

  }

  // chase the other agent by picking the node with thh lowest distance to the other agent
  class ChaseEnemyNodeStrategy(override val agentName: String, override val apiUrl: String, val queryGraph: MutableGraph[ComparableNode]) extends BaseStrategy(agentName, apiUrl){

    protected def getChaseState: AgentData = {

      val chaseAgentName = if (agentName == "police") "thief" else "police"
      getAgentState(chaseAgentName)

    }

    override def decideMove(): ActionRequest = {
      val agentState = getAgentState(agentName)
      val chaseState = getChaseState

      // find is the neighbours are more than one
      val neighboursCount  = agentState.adjacentNodes.length

      if (neighboursCount > 1){
        // for all neighbours find the distance to the chase agent and pick the one with the lowest distance
        val moveToId = agentState.adjacentNodes.map(node => (node.id, findDistanceToNode(queryGraph, node.id, chaseState.currentLocation.id))).minBy(_._2)._1
        ActionRequest(agentName, moveToId)
      }else if (neighboursCount == 1){
        // only 1 neighbour to pick
        val moveToId = agentState.adjacentNodes.head.id
        ActionRequest(agentName, moveToId)
      }else{
        // pick a completely random node from query graph
        val moveToId = queryGraph.nodes().asScala.toList(scala.util.Random.nextInt(queryGraph.nodes().asScala.toList.length)).id
        ActionRequest(agentName, moveToId)
      }

    }

  }

  class ChaseValuableDataStrategy(override val agentName: String, override val apiUrl: String, val queryGraph: MutableGraph[ComparableNode]) extends BaseStrategy(agentName, apiUrl){

    override def decideMove(): ActionRequest = {
      val agentState = getAgentState(agentName)

      // find is the neighbours are more than one
      val neighboursCount  = agentState.adjacentNodes.length

      if (neighboursCount > 1){
        // for all neighbours find the distance to the chase agent and pick the one with the lowest distance
        val moveToId = agentState.adjacentNodes.map(node => (node.id, findDistanceToValuableData(queryGraph, node.id))).minBy(_._2)._1
        ActionRequest(agentName, moveToId)
      }else if (neighboursCount == 1){
        // only 1 neighbour to pick
        val moveToId = agentState.adjacentNodes.head.id
        ActionRequest(agentName, moveToId)
      }else{
        // pick a completely random node from query graph
        val moveToId = queryGraph.nodes().asScala.toList(scala.util.Random.nextInt(queryGraph.nodes().asScala.toList.length)).id
        ActionRequest(agentName, moveToId)
      }

    }

  }


}
