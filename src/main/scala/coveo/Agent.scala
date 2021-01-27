package coevo

import Types._
import Utils._
import org.apache.commons.math3.random._


object agent {

  def build(id: AgentID,
            signals: Array[Signal],
            valueSystem: ValueSystem,
            contentBias: Double,
            coordinationBias: Double,
            conformityBias: Double,
            confirmationBias: Double,
            mutation: Double,
            iPower: Double
           ) =
    Agent(id, signals, Map[Signal, Int](), Map[Signal, Int](), Array[Signal](), Array[Signal](), valueSystem, contentBias, coordinationBias, conformityBias, confirmationBias, mutation, iPower)

  case class Agent(id: AgentID, signals: Array[Signal], memoryShown: SignalMemory, memoryObserved: SignalMemory, lastShown: Array[Signal], lastObserved: Array[Signal], valueSystem: ValueSystem, contentBias: Double, coordinationBias: Double, conformityBias: Double, confirmationBias: Double, mutation: Double, iPower: Double)

  def recall(agent: Agent, vShown: Signal, vObserved: Signal, memoryLength: Int) = {
    val lastShown = agent.lastShown.takeRight(memoryLength - 1).appended(vShown)
    val lastObserved = agent.lastObserved.takeRight(memoryLength - 1).appended(vObserved)

    agent.copy(
      lastShown = lastShown,
      lastObserved = lastObserved,
      memoryShown = lastShown.groupBy(identity).view.mapValues(_.size).toMap,
      memoryObserved = lastObserved.groupBy(identity).view.mapValues(_.size).toMap
    )
  }

  def withB(agent: Agent, nbShown: Int, nbObserved: Int, round: Round, idx: Int, nbAgents: Int) = {
    val coordinationBiasComplement = 1.0 - agent.coordinationBias
    if (!((nbShown == nbObserved) && (nbShown == 0))) {
      val weightedAgentBiasComplement = 0.98 * (1.0 - agent.contentBias)

      (weightedAgentBiasComplement * coordinationBiasComplement * nbShown / round) +
        (weightedAgentBiasComplement * agent.coordinationBias * nbObserved / round) +
        (0.98 * agent.contentBias * agent.valueSystem(idx)) + (agent.mutation / nbAgents)
    } else {
      (0.98 * coordinationBiasComplement * nbShown / round) +
        (0.98 * agent.coordinationBias * nbObserved / round) +
        (agent.mutation / nbAgents)
    }
  }

  def choose(round: Round, agent: Agent, institution: Institution, nbAgents: Int, random: RandomGenerator) = {
    val probs = agent.signals.zipWithIndex.map { case (signal, index) =>
      (signal, withB(agent, agent.memoryShown.getOrElse(signal, 0), agent.memoryObserved.getOrElse(signal, 0), round, index, nbAgents))
    }

  //  println( agent.id + " PROBS  " + probs.toSeq)
    val elecc = Utils.multinomial(probs, random)

    val aux: Array[Int] = agent.signals.map { s => if (s == elecc) 1 else 0 }

    val nonConformity = 1 - agent.conformityBias
    val currentChoiceBias = 1 - agent.confirmationBias
    val currentChoiceWeight = aux.map {
      _ * currentChoiceBias
    }
    val valueSystemWeight = agent.valueSystem.map {
      _ * agent.confirmationBias
    }

    val agentValuesSum = currentChoiceWeight add valueSystemWeight


    val individualWeight = agentValuesSum.map {
      _ * nonConformity
    }

    val confByIPower = agent.conformityBias * agent.iPower
    val institutionalWeight = institution.map {
      _ * confByIPower
    }

    val newValueSystem = individualWeight add institutionalWeight

    (agent.copy(valueSystem = newValueSystem), (agent.id, elecc))
  }

}