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
    Action.herbivoreAction((8 * Math.random()).toInt, velocity)
  }

  def carnivoreAction(subWorld: World, velocity: Velocity): Velocity = {
    Action.carnivorAction((8 * Math.random()).toInt, velocity)
  }
}

// (State 4 * 9) * (Action 8) = (Qvalue 288)
case class Qvalue(val values: Map[Tuple2[State, Int], Double])

object Qvalue {

  def init: Qvalue = {
    Qvalue(State.allStates flatMap (s => makePair(s, Action.index)) map (t => t -> 100.0) toMap)
  }

  def makePair(state: State, ls: List[Int]): List[Tuple2[State, Int]] = {
    ls map (i => (state, i))
  }
}

//        |
//  ul    |   ur
//        |
//-----------------
//        |
//   bl   |   br
//        |
case class State(val ul: SubState, val ur: SubState, val bl: SubState, val br: SubState)
case class SubState(val plant: Boolean, val herbivore: Boolean, val carnivore: Boolean)

object State {

  def apply(subWorld: World): State = {
    State(SubState.empty, SubState.empty, SubState.empty, SubState.empty)
  }

  def allStates: List[State] = {
    val all = SubState.allSubStates
    for(a <- all; b <- all; c <- all; d <- all) yield State(a, b, c, d)
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

object Action {

  def index: List[Int] = List(1, 2, 3, 4, 5, 6, 7, 8)

  def carnivorAction(i: Int, velocity: Velocity): Velocity = {
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