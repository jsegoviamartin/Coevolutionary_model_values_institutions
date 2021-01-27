package coevo

import Types._
import Utils._

import scala.annotation.tailrec
import org.apache.commons.math3.random._

object Coevo extends App {


  val agentValueSystems = (0 to 7).map { _ => Array(0.0, 0.0, 0.82, 0.18, 0.0, 0.0, 0.0, 0.0) }.toArray

  val grouping = Seq(
    Seq(Seq(0, 1), Seq(2, 3), Seq(4, 5), Seq(6, 7)),
    Seq(Seq(0, 3), Seq(2, 1), Seq(4, 7), Seq(6, 5)),
    Seq(Seq(0, 6), Seq(2, 7), Seq(4, 1), Seq(6, 3)),
    Seq(Seq(0, 7), Seq(2, 5), Seq(4, 3), Seq(6, 1)),
    Seq(Seq(0, 2), Seq(1, 3), Seq(4, 6), Seq(5, 7)),
    Seq(Seq(0, 4), Seq(1, 5), Seq(2, 6), Seq(3, 7)),
    Seq(Seq(0, 6), Seq(1, 7), Seq(2, 4), Seq(3, 5)),
    Seq(Seq(0, 6), Seq(1, 7), Seq(2, 4), Seq(3, 5))
  )

  var ugly: Seq[Seq[Double]] = Seq()

  val res = (for (s <- 1001 to 2001) yield {
    simulation.run(
      s,
      0.7,
      3,
      0.7,
      1.0,
      0.0,
      1.0,
      0.01,
      agentValueSystems,
      NoReplacement,
      Right(8),
      MeanInstitution
    )
  }).map { case (s) =>

    simulation.entropiesPerRound(s)
  }.transpose.map { a =>
    // a.toArray.mean
    median(a.toArray)
  }

  println("RES " + res)


  def median(array: Array[Double]) = array.sortWith(_ < _).drop(array.length / 2).head
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
          signalAssignment: SignalAssignment,
          grouping: Either[Seq[Seq[Pair]], Round],
          institutionType: InstitutionType
         ) = {
    val random = new Well44497a(seed)
    val shuffleRandom = new scala.util.Random(seed)

    val signals = (1 to agentValueSystems.size).map {
      Signal(_)
    }.toArray

    val agents = (0 to agentValueSystems.size - 1).toArray.map { i =>
      agent.build(i, signals, agentValueSystems(i), cont, coord, conform, confirm, mutation, iPower)
    }

    val rounds = grouping match {
      case Left(pairing) => pairing
      case Right(numberOfRounds) => group(agents, shuffleRandom, numberOfRounds)
    }

    play(agents, rounds, memoryLength, signals, random, institutionType, signalAssignment)
  }


  case class Simulation(agents: Array[agent.Agent], institution: Institution, memory: Map[Round, Array[SignalSelection]], institutionHistory: Array[Institution])

  def play(agents: Array[agent.Agent], rounds: Seq[Seq[Pair]], memoryLenght: Int, signals: Array[Signal], random: RandomGenerator, institutionType: InstitutionType, signalAssignment: SignalAssignment) = {

    @tailrec
    def playPairs(pairs: Seq[Pair], agents: Array[agent.Agent], signalSelections: Array[SignalSelection]): Array[agent.Agent] = {
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

        playPairs(pairs.tail, newAgents, signalSelections)
      }
    }

    @tailrec
    def playRounds(round: Round, rounds: Seq[Seq[Pair]], simulation: Simulation): Simulation = {
      if (rounds.isEmpty) {
        simulation
      }
      else {
        val (newAgentsFromSignals, signalSelection) = produceSignals(round, simulation.agents, signals, simulation.institution, signalAssignment, random)
        val newAgents = playPairs(rounds.head, newAgentsFromSignals, signalSelection)
        val newInstitution = institutionType match {
          case MajorityInstitution => majorityInstitutions(agentsToOrderdSignals(agents, signalSelection)) // the max is 1, others are 0
          case Relative => relaviteInstitutions(agentsToOrderdSignals(agents, signalSelection)) // eachOne is divided by the number of agents
          case MeanInstitution => meanInstitutions(newAgents.map {
            _.valueSystem
          })
        }
        // vaj majority = institutionsMaj(signalSelection)
        // val
        playRounds(round + 1, rounds.tail, Simulation(newAgents, newInstitution, simulation.memory + (round -> signalSelection), simulation.institutionHistory :+ newInstitution))
      }
    }

    playRounds(1, rounds, Simulation(agents, Array(), Map(), Array()))

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

  def group(agents: Array[agent.Agent], random: scala.util.Random, n: Int = 100): Seq[Seq[Pair]] = {
    (1 to n).map { _ =>
      random.shuffle(
        agents.map {
          _.id
        }.toSeq
      )
        .grouped(2).toSeq
    }
  }

  def agentsToOrderdSignals(agents: Array[agent.Agent], signalSelection: Array[SignalSelection]) = {
    val oo = agents.map { ag =>
      signalSelection.find { case (id, signal) => id == ag.id }.map {
        _._2
      }.getOrElse(Signal(0))
    }
    println("Ordered " + oo.toSeq)
    oo
  }

  def meanInstitutions(valueSystems: Array[ValueSystem]) = {
    valueSystems.transpose.map { vs => vs.mean }
  }

  def majorityInstitutions(signalSelectionsignals: Array[Signal]) = {
    val maxSignal = signalSelectionsignals.map {
      _.id
    }.max
    signalSelectionsignals.map { s =>
      if (s.id == maxSignal) 1.0
      else 0.0
    }
  }

  def relaviteInstitutions(signals: Array[Signal]) = {
    signals.map {
      _.id.toDouble / signals.length
    }
  }

  def produceSignals(round: Round, agents: Array[agent.Agent], signals: Array[Signal], institution: Institution, signalAssignment: SignalAssignment, random: RandomGenerator): (Array[agent.Agent], Array[SignalSelection]) = {
    if (round == 1) {
      val zipWith = {
        signalAssignment match {
          case NoReplacement => signals.toSeq
          case _ => uniform(signals, signals.length, random)
        }
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