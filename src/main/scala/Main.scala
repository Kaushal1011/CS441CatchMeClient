import Game.Utilities.requestAndLoadGraph
import Game.Strategies.{BaseStrategy, ChaseEnemyNodeStrategy, ChaseValuableDataStrategy}
import Game.Agent
import Game.{ComparableNode, GameState}
import com.google.common.graph.MutableGraph

object Main {
  def main(args: Array[String]): Unit = {
    // Check if all required arguments are provided
    if (args.length < 4) {
      println("Usage: GameRunner <apiUrl> <policeStrategyName> <thiefStrategyName> <turnsForEachPlayer>")
      System.exit(1)
    }

    // Assign each argument to a val
    val apiUrl = args(0)
    val policeStrategyName = args(1)
    val thiefStrategyName = args(2)
    val maxMovesForGame = args(3).toInt // Assuming this is an integer

    // Call the function to start the game
    runGame(apiUrl, policeStrategyName, thiefStrategyName, maxMovesForGame)
  }


  def createStrategy(strategyName: String, agentName: String, apiUrl: String, queryGraph: MutableGraph[ComparableNode]): BaseStrategy = {
    strategyName.toLowerCase match {
      case "random" => new BaseStrategy(agentName, apiUrl)
      case "chaseenemy" => new ChaseEnemyNodeStrategy(agentName, apiUrl, queryGraph)
      case "chasevaluable" => new ChaseValuableDataStrategy(agentName, apiUrl, queryGraph)
      case "randomprob" => new BaseStrategy(agentName = agentName, apiUrl)
      case "chaseenemyprob" => new ChaseEnemyNodeStrategy(agentName, apiUrl, queryGraph)
      case "chasevaluableprob" => new ChaseValuableDataStrategy(agentName, apiUrl, queryGraph)
      case _ => new BaseStrategy(agentName, apiUrl)
    }
  }

  def checkWinner(gameState: GameState): Tuple2[Boolean, String] = {
    gameState.winner match {
      case "Police" => Tuple2(true, "police")
      case "Thief" => Tuple2(true, "thief")
      case _ => Tuple2(false, "none")
    }
  }


  def runGame(apiUrl: String,  policeStrategyName: String, thiefStrategyName: String, maxMovesForGame: Int): Unit = {
    // Implementation to start the game
    println(s"Starting game with the following settings:")
    println(s"API URL: $apiUrl")
    println(s"Police Strategy Name: $policeStrategyName")
    println(s"Thief Strategy Name: $thiefStrategyName")
    println(s"Max Moves For Game: $maxMovesForGame")

    val queryGraph = requestAndLoadGraph(apiUrl)

    // case based on police strategy name
    val policeStrategy = createStrategy(policeStrategyName, "police", apiUrl, queryGraph)

    val thiefStrategy = createStrategy(thiefStrategyName, "thief", apiUrl, queryGraph)

    val policeAgent = new Agent("police", policeStrategy)
    val thiefAgent = new Agent("thief", thiefStrategy)


    // play the game for turnForEachPlayer

    for ( i <- 1 to maxMovesForGame) {
      // play police move
      val policeMove = policeAgent.decideMove()
      println(s"Police move: ${policeMove}")
      val policeGameState = policeAgent.playMove(policeMove)

      if (policeGameState.thiefLoc == -1 && policeGameState.policeLoc == -1) {
        println(s"Error in police move: ${policeMove}")
        System.exit(0)
      }

      println(s"State after turn ${i} for police: ${policeGameState}")

      // check winner after turn
      val (isWinner, winner) = checkWinner(policeGameState)
      if (isWinner) {
        println(s"${winner} won the game in turn ${i}")
        System.exit(0)
      }


      // play thief move
      val thiefMove = thiefAgent.decideMove()
      println(s"Thief move: ${thiefMove}")
      val thiefGameState = thiefAgent.playMove(thiefMove)

      if (thiefGameState.thiefLoc == -1 && thiefGameState.policeLoc == -1) {
        println(s"Error in thief move: ${thiefMove}")
        System.exit(0)
      }

      println(s"State after turn ${i} for thief: ${thiefGameState}")

      if (isWinner) {
        println(s"${winner} won the game in turn ${i}")
        System.exit(0)
      }


    }

    println("Max turns reached. Game ends. No one wins")

    System.exit(0)



  }

}