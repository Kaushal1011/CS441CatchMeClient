package Game

import com.google.common.graph.{GraphBuilder, MutableGraph}
import Utilities.{findDistanceToNode, findDistanceToValuableData}

import scala.jdk.CollectionConverters._
import Game.Strategies.{BaseStrategy, ChaseEnemyNodeStrategy, ChaseValuableDataStrategy}
import com.typesafe.config.ConfigFactory

// this object extends the basic strategies by adding a notion of confidence to the neighbours and probabilistic exploration
// the strategies will focus on only neighbours with a confidence score of 1.0
// the strategies will explore randomly with a probability as defined in the config
object StrategiesProbabilisticWConfidence {

  private val exploreProbThres = ConfigFactory.load().getConfig("CatchMeClient").getConfig("strat").getDouble("confexplore")

  class ProbabilisticBaseStrategyConf(override val agentName: String, override val apiUrl: String) extends BaseStrategy(agentName, apiUrl) {

    override def decideMove(): ActionRequest = {
      val agentState = getAgentState(agentName)
      val neighbours = agentState.adjacentNodes
      val confidenceScores = agentState.confidenceScores

      if (scala.util.Random.nextDouble() < exploreProbThres) {
        decideMoveWithConfidence(agentState, confidenceScores)
      } else {
        exploreRandomly(agentName,neighbours)
      }
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

  // focus on neighbours with a confidence score of 1.0
  // chases the other agent if the probability is less than the threshold, otherwise explores randomly
  class ProbabilisticChaseEnemyNodeStrategyConf(override val agentName: String, override val apiUrl: String, override val queryGraph: MutableGraph[ComparableNode]) extends ChaseEnemyNodeStrategy(agentName, apiUrl, queryGraph) {

    override def decideMove(): ActionRequest = {
      val agentState = getAgentState(agentName)
      val chaseState = getChaseState
      val confidenceScores = agentState.confidenceScores
      val neighbours = agentState.adjacentNodes

      if (scala.util.Random.nextDouble() < exploreProbThres) {
        decideMoveWithConfidence(agentState, chaseState, confidenceScores, queryGraph)
      } else {
        exploreRandomly(agentName,neighbours)
      }
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

  // focus on neighbours with a confidence score of 1.0
  // chases the valuable data if the probability is less than the threshold, otherwise explores randomly
  class ProbabilisticChaseValuableDataStrategyConf(override val agentName: String, override val apiUrl: String, override val queryGraph: MutableGraph[ComparableNode]) extends ChaseValuableDataStrategy(agentName, apiUrl, queryGraph) {

    override def decideMove(): ActionRequest = {
      val agentState = getAgentState(agentName)
      val confidenceScores = agentState.confidenceScores
      val neighbours = agentState.adjacentNodes

      if (scala.util.Random.nextDouble() < exploreProbThres) {
        decideMoveWithConfidence(agentState, confidenceScores, queryGraph)
      } else {
        exploreRandomly(agentName,neighbours)
      }
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

  private def exploreRandomly(agentName:String,neighbours: Seq[ComparableNode]): ActionRequest = {
    val selectedNode = neighbours(scala.util.Random.nextInt(neighbours.length))
    ActionRequest(agentName, selectedNode.id)
  }
}