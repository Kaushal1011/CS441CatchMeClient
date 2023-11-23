package Game

import Game.Strategies.BaseStrategy

import helpers.requestHelpers.post
import io.circe.generic.auto._

// agent class inspired by design of AI Agents / Agents in common gym environments
class Agent(val name: String, val strategy: BaseStrategy) {
  def agentState:AgentData = {
    strategy.getAgentState(name)
  }
  def decideMove(): ActionRequest = {
    strategy.decideMove()
  }

  def playMove(action:ActionRequest): GameState = {

    val response = post[GameState, ActionRequest](strategy.apiUrl + "/action", action)
    response.getOrElse(GameState(-1,-1,"Error"))

  }


}
