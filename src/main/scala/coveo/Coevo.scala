package coevo

import Types._
import Utils._

import scala.annotation.tailrec
import scala.util.Random

object Coevo extends App {

  val valueSystems = Array(
    Array(1.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0),
    Array(1.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0)
  )

  val agentValueSystems = Array(
    valueSystems(0),
    valueSystems(0),
    valueSystems(0),
    valueSystems(0),
    valueSystems(0),
    valueSystems(1),
    valueSystems(1),
    valueSystems(1),
    valueSystems(1),
    valueSystems(1)
  )

  val grouping = Seq(
    Seq(Seq(0, 1), Seq(2, 3), Seq(4, 5), Seq(6, 7)),
    Seq(Seq(0, 3), Seq(2, 1), Seq(4, 7), Seq(6, 5)),
    Seq(Seq(0, 6), Seq(2, 7), Seq(4, 1), Seq(6, 3)),
    Seq(Seq(0, 7), Seq(2, 5), Seq(4, 3), Seq(6, 1)),
    Seq(Seq(0, 2), Seq(1, 3), Seq(4, 6), Seq(5, 7)),
    Seq(Seq(0, 4), Seq(1, 5), Seq(2, 6), Seq(3, 7)),
    Seq(Seq(0, 6), Seq(1, 7), Seq(2, 4), Seq(3, 5))
  )

  val results = simulation.run(9, 1, 3, 1, 0.5, 1, 0, 0.02, agentValueSystems, Left(grouping))

  simulation.entropiesPerRound(results).zipWithIndex.foreach { epr =>
    println(s"R ${epr._2}: ${epr._1}")
  }
}

object simulation {

  def run(seed: Long,
          iPower: Double,
          memoryLength: Int,
          cont: Double,
          coord: Double,
          conform: Double,
          confirm: Double,
          mutation: Double,
          agentValueSystems: Array[ValueSystem],
          grouping: Either[Seq[Seq[Pair]], Round]
         ) = {
    val random = new Random(seed)

    val signals = (1 to agentValueSystems.size).map {
      Signal(_)
    }.toArray

    val agents = (0 to agentValueSystems.size - 1).toArray.map { i =>
      agent.build(i, signals, agentValueSystems(i), cont, coord, conform, confirm, mutation, iPower)
    }

    val (rounds, fixedGrouping) = grouping match {
      case Left(pairing) => (pairing, true)
      case Right(numberOfRounds) => (group(agents, random, numberOfRounds), false)
    }

    play(agents, rounds, fixedGrouping, memoryLength, signals, random)
  }


  case class Simulation(agents: Array[agent.Agent], institution: Institution, memory: Map[Round, Array[SignalSelection]], institutionHistory: Array[Institution])

  def play(agents: Array[agent.Agent], rounds: Seq[Seq[Pair]], fixedGrouping: Boolean, memoryLenght: Int, signals: Array[Signal], random: Random) = {

    @tailrec
    def playRound(pairs: Seq[Pair], agents: Array[agent.Agent], signalSelections: Array[SignalSelection]): Array[agent.Agent] = {
      if (pairs.isEmpty) agents
      else {
        val pair = pairs.head
        val agentID1 = pair.head
        val agentID2 = pair.last
        val signalSelectionForAgent1 = signalSelections.find(_._1 == agentID1).getOrElse(signalSelections.head)._2
        val signalSelectionForAgent2 = signalSelections.find(_._1 == agentID2).getOrElse(signalSelections.head)._2
        val agent1 = agents(agentID1)
        val agent2 = agents(agentID2)
        val newAgents = agents
          .updated(agentID1, agent.recall(agent1, signalSelectionForAgent1, signalSelectionForAgent2, memoryLenght))
          .updated(agentID2, agent.recall(agent2, signalSelectionForAgent2, signalSelectionForAgent1, memoryLenght))

        playRound(pairs.tail, newAgents, signalSelections)
      }
    }

    @tailrec
    def play0(round: Round, rounds: Seq[Seq[Pair]], simulation: Simulation): Simulation = {
      if (rounds.isEmpty) {
        simulation
      }
      else {
        val (newAgentsFromSignals, signalSelection) = produceSignals(round, simulation.agents, signals, simulation.institution, fixedGrouping, random)
        val newAgents = playRound(rounds.head, newAgentsFromSignals, signalSelection)
        val newInstitution = institutions(newAgents.map {
          _.valueSystem
        })
        play0(round + 1, rounds.tail, Simulation(newAgents, newInstitution, simulation.memory + (round -> signalSelection), simulation.institutionHistory :+ newInstitution))
      }
    }

    play0(1, rounds, Simulation(agents, Array(), Map(), Array()))
  }


  def entropiesPerRound(simulation: Simulation) = {

    val maxEnt = maxEntropy(simulation.agents.length)

    val raw = simulation.memory.map { case (r, signals) =>
      val s = signals.groupBy(_._2).view.mapValues(_.size).values.toSeq
      r -> entropy(s)
    }.toSeq.sortBy(_._1)

    //Normalization
    raw.map { case (_, e) => e / maxEnt }.toArray
  }

  def maxEntropy(numberOfAgents: Int) = entropy(Seq.fill(numberOfAgents)(1))

  def group(agents: Array[agent.Agent], rng: Random, n: Int = 100): Seq[Seq[Pair]] = {
    (1 to n).map { _ =>
      rng.shuffle(agents.map {
        _.id
      }.toSeq).grouped(2).toSeq
    }
  }

  def institutions(valueSystems: Array[ValueSystem]) = {
    valueSystems.transpose.map { vs => vs.mean }
  }

  def produceSignals(round: Round, agents: Array[agent.Agent], signals: Array[Signal], institution: Institution, fixedGrouping: Boolean, random: Random): (Array[agent.Agent], Array[SignalSelection]) = {
   if (round == 1) {
      val zipWith = {
        if (fixedGrouping) signals.toSeq
        else uniform(signals, 10, random)
      }

      (agents, agents.map {
        _.id
      } zip zipWith)
    }
    else {
      agents.map { a =>
        agent.choose(round, a, institution, agents.length, random)
      }.unzip
    }
  }

}