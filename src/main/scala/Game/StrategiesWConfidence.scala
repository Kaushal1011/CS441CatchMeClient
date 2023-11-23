package Game

import Utilities.{findDistanceToNode, findDistanceToValuableData}
import scala.jdk.CollectionConverters._
import Game.Strategies.{BaseStrategy, ChaseEnemyNodeStrategy, ChaseValuableDataStrategy}
import com.google.common.graph.{GraphBuilder, MutableGraph}
import Game.Utilities.{findDistanceToNode, findDistanceToValuableData}
import Game.Strategies.{BaseStrategy, ChaseEnemyNodeStrategy, ChaseValuableDataStrategy}


// this object extends the basic strategies by adding a notion of confidence to the neighbours
// the strategies will focus on only neighbours with a confidence score of 1.0
object StrategiesWConfidence {

  // filter neighbours by confidence score
  // only pick from neighbours with a confidence score of 1.0
  class ConfidenceBaseStrategy(override val agentName: String, override val apiUrl: String) extends BaseStrategy(agentName, apiUrl) {

    override def decideMove(): ActionRequest = {
      val agentState = getAgentState(agentName)
      val neighbours = agentState.adjacentNodes
      val confidenceScores = agentState.confidenceScores
      decideMoveWithConfidence(agentState, confidenceScores)
    }

    private def decideMoveWithConfidence(agentState: AgentData, confidenceScores: Seq[ConfidenceScore]): ActionRequest = {
      val neighbours = agentState.adjacentNodes

      if (neighbours.isEmpty) {
        ActionRequest(agentName, -1)
      } else {
        val highConfidenceNeighbours = neighbours.zipWithIndex.filter { case (node, index) =>
          confidenceScores(index).score == 1.0
        }.map(_._1)

        val selectedNode = if (highConfidenceNeighbours.nonEmpty) {
          highConfidenceNeighbours(scala.util.Random.nextInt(highConfidenceNeighbours.length))
        } else {
          neighbours(scala.util.Random.nextInt(neighbours.length))
        }

        ActionRequest(agentName, selectedNode.id)
      }
    }
  }

  // filter neighbours by confidence score
  // only pick from neighbours with a confidence score of 1.0
  // chase the other agent by picking the node with the lowest distance to the other agent
  class ConfidenceChaseEnemyNodeStrategy(override val agentName: String, override val apiUrl: String, override val queryGraph: MutableGraph[ComparableNode]) extends ChaseEnemyNodeStrategy(agentName, apiUrl, queryGraph) {

    override def decideMove(): ActionRequest = {
      val agentState = getAgentState(agentName)
      val chaseState = getChaseState
      val confidenceScores = agentState.confidenceScores
      val neighbours = agentState.adjacentNodes

      decideMoveWithConfidence(agentState, chaseState, confidenceScores, queryGraph)

    }

    private def decideMoveWithConfidence(agentState: AgentData, chaseState: AgentData, confidenceScores: Seq[ConfidenceScore], queryGraph: MutableGraph[ComparableNode]): ActionRequest = {
      val neighbours = agentState.adjacentNodes

      if (neighbours.isEmpty) {
        ActionRequest(agentName, -1)
      } else {
        val highConfidenceNeighbours = neighbours.zipWithIndex.filter { case (node, index) =>
          confidenceScores(index).score == 1.0
        }.map(_._1)

        val relevantNeighbours = if (highConfidenceNeighbours.nonEmpty) highConfidenceNeighbours else neighbours
        val moveToId = relevantNeighbours.map(node => (node.id, findDistanceToNode(queryGraph, node.id, chaseState.currentLocation.id))).minBy(_._2)._1

        ActionRequest(agentName, moveToId)
      }
    }
  }


  // filter neighbours by confidence score
  // only pick from neighbours with a confidence score of 1.0
  // chase the valuable data by picking the node with the lowest distance to the valuable data
  class ConfidenceChaseValuableDataStrategy(override val agentName: String, override val apiUrl: String, override val queryGraph: MutableGraph[ComparableNode]) extends ChaseValuableDataStrategy(agentName, apiUrl, queryGraph) {

    override def decideMove(): ActionRequest = {
      val agentState = getAgentState(agentName)
      val confidenceScores = agentState.confidenceScores
      val neighbours = agentState.adjacentNodes

      decideMoveWithConfidence(agentState, confidenceScores, queryGraph)

    }

    private def decideMoveWithConfidence(agentState: AgentData, confidenceScores: Seq[ConfidenceScore], queryGraph: MutableGraph[ComparableNode]): ActionRequest = {
      val neighbours = agentState.adjacentNodes

      if (neighbours.isEmpty) {
        ActionRequest(agentName, -1)
      } else {
        val highConfidenceNeighbours = neighbours.zipWithIndex.filter { case (node, index) =>
          confidenceScores(index).score == 1.0
        }.map(_._1)

        val relevantNeighbours = if (highConfidenceNeighbours.nonEmpty) highConfidenceNeighbours else neighbours
        val moveToId = relevantNeighbours.map(node => (node.id, findDistanceToValuableData(queryGraph, node.id))).minBy(_._2)._1

        ActionRequest(agentName, moveToId)
      }
    }
  }

}

