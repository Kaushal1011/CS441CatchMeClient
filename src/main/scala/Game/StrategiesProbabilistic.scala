package Game

import Game.Strategies.{BaseStrategy, ChaseEnemyNodeStrategy, ChaseValuableDataStrategy}
import Game.Utilities.{findDistanceToNode, findDistanceToValuableData}
import com.google.common.graph.MutableGraph
import com.typesafe.config.ConfigFactory

// this object extends the basic strategies by adding a notion of probabilistic exploration
// the strategies will explore randomly with a probability as defined in the config
// the chase enemy node strategy picks the node with the lowest distance to the other agent with a probability and explores randomly with a probability
// the chase valuable data strategy picks the node with the lowest distance to the valuable data with a probability and explores randomly with a probability
object StrategiesProbabilistic {

  // load the exploration probability from the config
  private val exploreProbThres = ConfigFactory.load().getConfig("CatchMeClient").getConfig("strat").getDouble("probexplore")


  // chases the other agent if the probability is less than the threshold, otherwise explores randomly
  class ProbabilisticChaseEnemyNodeStrategy(override val agentName: String, override val apiUrl: String, override val queryGraph: MutableGraph[ComparableNode]) extends ChaseEnemyNodeStrategy(agentName, apiUrl, queryGraph) {

    override def decideMove(): ActionRequest = {
      val agentState = getAgentState(agentName)
      val neighbours = agentState.adjacentNodes

      if (neighbours.isEmpty) {
        ActionRequest(agentName, -1)
      } else {

        if (scala.util.Random.nextDouble() < exploreProbThres) {
          // Default behaviour
          val moveToId = neighbours.map(node => (node.id, findDistanceToNode(queryGraph, node.id, getChaseState.currentLocation.id))).minBy(_._2)._1
          ActionRequest(agentName, moveToId)
        } else {
          // Exploration
          exploreRandomly(agentName,neighbours)
        }
      }
    }
  }

  // chases the valuable data if the probability is less than the threshold, otherwise explores randomly
  class ProbabilisticChaseValuableDataStrategy(override val agentName: String, override val apiUrl: String, override val queryGraph: MutableGraph[ComparableNode]) extends ChaseValuableDataStrategy(agentName, apiUrl, queryGraph) {

    override def decideMove(): ActionRequest = {
      val agentState = getAgentState(agentName)
      val neighbours = agentState.adjacentNodes

      if (neighbours.isEmpty) {
        ActionRequest(agentName, -1)
      } else {
        if (scala.util.Random.nextDouble() < exploreProbThres) {
          // Default behaviour
          val moveToId = neighbours.map(node => (node.id, findDistanceToValuableData(queryGraph, node.id))).minBy(_._2)._1
          ActionRequest(agentName, moveToId)
        } else {
          // Exploration
          exploreRandomly(agentName,neighbours)
        }
      }
    }
  }

  // Helper function for exploration behaviour
  private def exploreRandomly( agentName: String ,neighbours: Seq[ComparableNode]): ActionRequest = {
    val selectedNode = neighbours(scala.util.Random.nextInt(neighbours.length))
    ActionRequest(agentName, selectedNode.id)
  }
}

