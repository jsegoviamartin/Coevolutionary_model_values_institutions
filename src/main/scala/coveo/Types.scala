package coveo

object Types {

  case class Signal(id: Int) {
    override def toString = s"S$id"
  }

  type AgentID = Int
  type ValueSystem = Array[Double]
  type Pair = Seq[AgentID]
  type SignalSelection = (AgentID, Signal)
  //type Round = Seq[Pair]
  type Round= Int
  type Institution= Array[Double]
  type SignalMemory = Map[Signal, Int]

}
