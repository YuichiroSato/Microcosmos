package jp.satoyuichiro.microcosmos.model.learning

import jp.satoyuichiro.microcosmos.model.bio.Velocity
import jp.satoyuichiro.microcosmos.model.World

object Qlearning {

  var carnivoreQ = Qvalue.init
  var herbivoreQ = Qvalue.init

  def herbivoreLearn(): Unit = {

  }

  def carnivoreLearn(): Unit = {

  }

  def herbivoreAction(subWorld: World, velocity: Velocity): Velocity = {
    Action.herbivoreAction(Qvalue.bestAction(herbivoreQ, Qvalue.toState(subWorld, velocity)), velocity)
//    Action.herbivoreAction((8 * Math.random()).toInt, velocity)
  }

  def carnivoreAction(subWorld: World, velocity: Velocity): Velocity = {
    Action.carnivoreAction(Qvalue.bestAction(carnivoreQ, Qvalue.toState(subWorld, velocity)), velocity)
//    Action.carnivorAction((8 * Math.random()).toInt, velocity)
  }
}

// (State 4 * 9 * 16) * (Action 7) = (Qvalue 4032)
case class Qvalue(val values: Map[Tuple2[State, Int], Double])

object Qvalue {

  def init: Qvalue = {
    Qvalue(State.allStates flatMap (s => makePair(s, Action.index)) map (t => t -> 100.0) toMap)
  }

  def makePair(state: State, ls: List[Int]): List[Tuple2[State, Int]] = {
    ls map (i => (state, i))
  }
  
  def toState(subWorld: World, velocity: Velocity): State = {
    State(subWorld, velocity)
  }
  
  def stateToActionValue(qvalue: Qvalue, state: State): List[Tuple2[Int, Double]] = {
    qvalue.values filter (_._1._1 == state) map (t => (t._1._2, qvalue.values.getOrElse(t._1, 0.0))) toList
  }
  
  def bestAction(qvalue: Qvalue, state: State): Int = {
    val actionValue = stateToActionValue(qvalue, state)
    if (0 < actionValue.size) {
      actionValue.maxBy(_._2)._1
    } else {
      Action.maxValue
    }
  }
}

//        |
//  ul    |   ur
//        |
//-----------------
//        |
//   bl   |   br
//        |
case class State(val ul: SubState, val ur: SubState, val bl: SubState, val br: SubState, val bs: BioState)
case class SubState(val plant: Boolean, val herbivore: Boolean, val carnivore: Boolean)
case class BioState(val ulAhead: Boolean, val urAhead: Boolean, val blAhead: Boolean, val brAhead: Boolean)

object State {

  def apply(subWorld: World, velocity: Velocity): State = {
    State(SubState.empty, SubState.empty, SubState.empty, SubState.empty, BioState.empty)
  }

  def allStates: List[State] = {
    val sall = SubState.allSubStates
    val ball = BioState.allBioState
    for(a <- sall; b <- sall; c <- sall; d <- sall; e <- ball) yield State(a, b, c, d, e)
  }
}

object SubState {

  def apply(ls: List[Boolean]): SubState = {
    if (ls.size == 3) {
      SubState(ls.head, ls.tail.head, ls.tail.tail.head)
    } else {
      empty
    }
  }
  
  def allSubStates: List[SubState] = {
    val tf = List(true, false)
    val all = for(a <- tf; b <- tf; c <- tf) yield List(a,b,c)
    all map (ls => SubState(ls))
  }
  def empty: SubState = SubState(false, false, false)
  def plant: SubState = SubState(true, false, false)
  def harbivore: SubState = SubState(false, true, false)
  def carnivore: SubState = SubState(false, false, true)
}

object BioState {
  
  def apply(ls: List[Boolean]): BioState = {
    if (ls.size == 4) {
      BioState(ls.head, ls.drop(1).head, ls.drop(2).head, ls.drop(3).head)
    } else {
      empty
    }
  }
  def allBioState: List[BioState] = {
    val tf = List(true, false)
    val all = for(a <-tf; b <- tf; c <- tf; d <- tf) yield List(a,b,c,d)
    all map (ls => BioState(ls))
  }
  def empty: BioState = BioState(false, false, false, false)
}

object Action {

  val maxValue = 7
  
  def index = (1 to maxValue).toList

  def carnivoreAction(i: Int, velocity: Velocity): Velocity = {
    i match {
      case 1 => Velocity(velocity.speed + 3.0, velocity.rotation)
      case 2 => Velocity(velocity.speed - 3.0, velocity.rotation)
      case 3 => Velocity(velocity.speed, velocity.rotation + 0.1)
      case 4 => Velocity(velocity.speed, velocity.rotation - 0.1)
      case 5 => Velocity(velocity.speed, 0.0)
      case 6 => Velocity(0.0, 0.0)
      case _ => velocity
    }
  }

  def herbivoreAction(i: Int, velocity: Velocity): Velocity = {
    i match {
      case 1 => Velocity(velocity.speed + 0.7, velocity.rotation)
      case 2 => Velocity(velocity.speed - 0.7, velocity.rotation)
      case 3 => Velocity(velocity.speed, velocity.rotation + 0.1)
      case 4 => Velocity(velocity.speed, velocity.rotation - 0.1)
      case 5 => Velocity(velocity.speed, 0.0)
      case 6 => Velocity(0.0, 0.0)
      case _ => velocity
    }
  }
}