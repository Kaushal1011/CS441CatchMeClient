import Game.Utilities.{initializeGame, requestAndLoadGraph}
import Game.Strategies.{BaseStrategy, ChaseEnemyNodeStrategy, ChaseValuableDataStrategy}
import Game.StrategiesWConfidence.{ConfidenceBaseStrategy, ConfidenceChaseEnemyNodeStrategy, ConfidenceChaseValuableDataStrategy}
import Game.StrategiesProbabilistic.{ProbabilisticChaseEnemyNodeStrategy, ProbabilisticChaseValuableDataStrategy}
import Game.StrategiesProbabilisticWConfidence.{ProbabilisticBaseStrategyConf, ProbabilisticChaseEnemyNodeStrategyConf, ProbabilisticChaseValuableDataStrategyConf}
import Game.Agent
import Game.{ComparableNode, GameState}
import com.google.common.graph.MutableGraph

import java.io.{File, PrintWriter}
import java.text.SimpleDateFormat
import java.util.Date
import org.slf4j.LoggerFactory
import com.typesafe.config.ConfigFactory

object Main {

  // logger for logging
  private val logger = LoggerFactory.getLogger(getClass)

  // main function that runs the game x number of times with the supplied strategies for y max iterations, prints the stats for strats.
  def main(args: Array[String]): Unit = {
    // Check if the minimum required arguments are provided
    if (args.length < 5) {
      println("Usage: GameRunner <apiUrl> <policeStrategyName> <thiefStrategyName> <turnsForEachPlayer> <simulationIters> [regionalGraphPath] [queryGraphPath]")
      logger.error("Usage: GameRunner <apiUrl> <policeStrategyName> <thiefStrategyName> <turnsForEachPlayer> <simulationIters> [regionalGraphPath] [queryGraphPath]")
      System.exit(1)
    }

    val apiUrl = args(0)
    val policeStrategyName = args(1)
    val thiefStrategyName = args(2)
    val maxMovesForGame = args(3).toInt
    val simIters = args(4).toInt

    val config = ConfigFactory.load()

    // load default paths from config if not provided
    val regionalGraphPath = if (args.length > 5) args(5) else config.getConfig("CatchMeClient").getConfig("client").getString("defaultRegionalPath")
    val queryGraphPath = if (args.length > 6) args(6) else config.getConfig("CatchMeClient").getConfig("client").getString("defaultQueryPath")

    // Run the game for sim iters and collect the results and print them
    val results = (1 to simIters).map(_ =>
      {
        initializeGame(apiUrl, regionalGraphPath, queryGraphPath, "ifilosemyselftonightiwillblamescala")
        runGame(apiUrl, policeStrategyName, thiefStrategyName, maxMovesForGame)
      })

    // aggregate the results
    val policeWins = results.count(_._1 == "police")
    val thiefWins = results.count(_._1 == "thief")
    val draws = results.count(_._1 == "none")
    val avgTurns = results.map(_._2).sum / simIters

    println(s"Police wins: $policeWins")
    println(s"Thief wins: $thiefWins")
    println(s"Draws: $draws")
    println(s"Average turns: $avgTurns")

    logger.info(s"Police wins: $policeWins")
    logger.info(s"Thief wins: $thiefWins")
    logger.info(s"Draws: $draws")
    logger.info(s"Average turns: $avgTurns")


  }

  // create a strategy based on the strategy name
  def createStrategy(strategyName: String, agentName: String, apiUrl: String, queryGraph: MutableGraph[ComparableNode]): BaseStrategy = {
    strategyName.toLowerCase match {
      case "random" => new BaseStrategy(agentName, apiUrl)
      case "chaseenemy" => new ChaseEnemyNodeStrategy(agentName, apiUrl, queryGraph)
      case "chasevaluable" => new ChaseValuableDataStrategy(agentName, apiUrl, queryGraph)
      case "randomprob" => new BaseStrategy(agentName = agentName, apiUrl)
      case "chaseenemyprob" => new ProbabilisticChaseEnemyNodeStrategy(agentName, apiUrl, queryGraph)
      case "chasevaluableprob" => new ProbabilisticChaseValuableDataStrategy(agentName, apiUrl, queryGraph)
      case "randomconf" => new ConfidenceBaseStrategy(agentName, apiUrl)
      case "chaseenemyconf" => new ConfidenceChaseEnemyNodeStrategy(agentName, apiUrl, queryGraph)
      case "chasevaluableconf" => new ConfidenceChaseValuableDataStrategy(agentName, apiUrl, queryGraph)
      case "randomprobconf" => new ProbabilisticBaseStrategyConf(agentName, apiUrl)
      case "chaseenemyprobconf" => new ProbabilisticChaseEnemyNodeStrategyConf(agentName, apiUrl, queryGraph)
      case "chasevaluableprobconf" => new ProbabilisticChaseValuableDataStrategyConf(agentName, apiUrl, queryGraph)
      case _ => new BaseStrategy(agentName, apiUrl)
    }
  }

  // check if there is a winner
  def checkWinner(gameState: GameState): Tuple2[Boolean, String] = {
    gameState.winner match {
      case "Police" => Tuple2(true, "police")
      case "Thief" => Tuple2(true, "thief")
      case _ => Tuple2(false, "none")
    }
  }

  // function that runs the game for maxMovesForGame turns and returns the winner and the number of turns it took
  def runGame(apiUrl: String,  policeStrategyName: String, thiefStrategyName: String, maxMovesForGame: Int): (String, Int) = {

    // Create a file with a timestamp and strategies in its name
    // this file is used to store game logs which can be visualised by visualiser
    val timestamp = new SimpleDateFormat("yyyyMMddHHmmss").format(new Date())
    val fileName = s"./runlogs/${timestamp}_runlogs_$policeStrategyName$thiefStrategyName.txt"
    val file = new File(fileName)
    val writer = new PrintWriter(file)

    // Implementation to start the game
    logger.info(s"Starting game with the following settings:")
    logger.info(s"API URL: $apiUrl")
    logger.info(s"Police Strategy Name: $policeStrategyName")
    logger.info(s"Thief Strategy Name: $thiefStrategyName")
    logger.info(s"Max Moves For Game: $maxMovesForGame")

    try {


      val queryGraph = requestAndLoadGraph(apiUrl)

      // case based on police strategy name
      val policeStrategy = createStrategy(policeStrategyName, "police", apiUrl, queryGraph)

      val thiefStrategy = createStrategy(thiefStrategyName, "thief", apiUrl, queryGraph)

      val policeAgent = new Agent("police", policeStrategy)
      val thiefAgent = new Agent("thief", thiefStrategy)


      // play the game for turnForEachPlayer
      for (i <- 1 to maxMovesForGame) {
        // play police move
        val policeMove = policeAgent.decideMove()
        logger.info(s"Police move: ${policeMove}")
        val policeGameState = policeAgent.playMove(policeMove)

        if (policeGameState.thiefLoc == -1 && policeGameState.policeLoc == -1) {
          logger.info(s"Error in police move: ${policeMove}")
          writer.close()
          return Tuple2("none", i)
        }

        logger.info(s"State after turn ${i} for police: ${policeGameState}")

        writer.println(policeGameState.policeLoc + "," + policeGameState.thiefLoc)
        writer.flush()

        // check winner after turn
        val (isWinner, winner) = checkWinner(policeGameState)
        if (isWinner) {
          logger.info(s"${winner} won the game in turn ${i}")
          writer.close()
          return Tuple2(winner, i)

        }


        // play thief move
        val thiefMove = thiefAgent.decideMove()
        logger.info(s"Thief move: ${thiefMove}")
        val thiefGameState = thiefAgent.playMove(thiefMove)

        if (thiefGameState.thiefLoc == -1 && thiefGameState.policeLoc == -1) {
          logger.info(s"Error in thief move: ${thiefMove}")
          writer.close()
          return Tuple2("none", i)
        }

        logger.info(s"State after turn ${i} for thief: ${thiefGameState}")

        writer.println(thiefGameState.policeLoc + "," + thiefGameState.thiefLoc)
        writer.flush()

        if (isWinner) {
          logger.info(s"${winner} won the game in turn ${i}")
          writer.close()
          return Tuple2(winner, i)
        }


      }

      logger.info("Max turns reached. Game ends. No one wins")
      writer.close()
      return Tuple2("none", maxMovesForGame)


    } catch {
      case e: Exception =>
        e.printStackTrace() // Log the exception
      // Handle exception if needed
    } finally {
      writer.close() // Make sure to close the file writer
    }

    Tuple2("none", maxMovesForGame)

  }

}