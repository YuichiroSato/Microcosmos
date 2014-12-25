package jp.satoyuichiro.microcosmos.model.learning

import jp.satoyuichiro.microcosmos.model.bio.Velocity
import jp.satoyuichiro.microcosmos.model.World
import jp.satoyuichiro.microcosmos.model.bio.Bio
import jp.satoyuichiro.microcosmos.model.bio.Plant
import jp.satoyuichiro.microcosmos.model.bio.Herbivore
import jp.satoyuichiro.microcosmos.model.bio.Carnivore
import scala.collection.immutable.Queue
import jp.satoyuichiro.microcosmos.model.bio.Animal

import scalaz._
import Scalaz._

object Qlearning {

  def toData(animal0: Animal, animal1: Animal): (S, Int, S, Int) = {
    if (animal0.isInstanceOf[Carnivore]) {
      val hoge0 = animal0.asInstanceOf[Carnivore]
      val hoge1 = animal1.asInstanceOf[Carnivore]
      
      val state0 = S(hoge0.learningInfo.subWorld, hoge0)
      val action = hoge0.learningInfo.action
      val state1 = S(hoge1.learningInfo.subWorld, hoge1)
      val reward = hoge1.internal.life - hoge0.learningInfo.animal.internal.life
    
      (state0, action, state1, reward)
    } else {
      val hoge0 = animal0.asInstanceOf[Herbivore]
      val hoge1 = animal1.asInstanceOf[Herbivore]
      
      val state0 = S(hoge0.learningInfo.subWorld, hoge0)
      val action = hoge0.learningInfo.action
      val state1 = S(hoge1.learningInfo.subWorld, hoge1)
      val reward = hoge1.internal.life - hoge0.learningInfo.animal.internal.life
      
      (state0, action, state1, reward)
    }
  }
    
  def update(a0: Animal, a1: Animal): State[StateActionValue, Unit] = {
    val data = toData(a0, a1)
    val currentState = data._3
    for {
      avMap <- getAVMap(currentState)
      result <- branch(avMap, data)
    } yield result
  }
  
  def getOldValue(s: S, a: Int) = State[StateActionValue, Double] {
    sav => (sav, sav.SA_Value.getOrElse((s,a), StateActionValue.initValue))
  }
  
  def getAVMap(s: S) = State[StateActionValue, Map[Int, Double]] {
    sav => (sav, sav.S_AValue.getOrElse(s, Map.empty[Int, Double]))
  }
  
  def branch(avMap: Map[Int, Double], data: (S, Int, S, Int)): State[StateActionValue, Unit] = {
    val (targetState, action, currentState, reward) = data
    if (avMap.isEmpty)
      for {
        result <- State[StateActionValue, Unit] { sav => (sav.update(targetState, action, StateActionValue.initValue), ()) }
      } yield result
    else
      for {
        oldValue <- getOldValue(targetState, action)
        maxValue <- getMaxValue(avMap)
        result <- updateValue(targetState, oldValue, action, reward, maxValue)
      } yield result
  }
  
  def getMaxValue(map: Map[Int, Double]) = State[StateActionValue, Double] {
    sav => (sav, if (map.isEmpty) Double.MinValue else map.maxBy(_._2)._2)
  }
  
  def updateValue(targetState: S, oldValue: Double, action: Int, reward: Double, maxValue: Double) = State[StateActionValue, Unit] {
    sav => (sav.update(targetState, action, calculateNewValue(oldValue, reward, maxValue)), ())
  }
  
  val alpha = 0.1
  val gamma = 0.9
  
  def calculateNewValue(oldValue: Double, reward: Double, maxValue: Double): Double = oldValue + alpha * (reward + gamma * maxValue - oldValue)

  def toState(subWorld: World, animal: Animal): S = {
    S(subWorld, animal)
  }
  
  val epsilon = 0.5
  
  def learningRasio(): Unit = {
    println("carn " + CarnivoreQlearning.learningRasio)
    println("herb " + HerbivoreQlearning.learningRasio)
  }
  
}

object HerbivoreQlearning {
  
  private var stateActionValue = StateActionValue.empty
  
  def learn(herbivore0: Herbivore, herbivore1: Herbivore): Unit = {
    stateActionValue = Qlearning.update(herbivore0, herbivore1).exec(stateActionValue)
  }
  
  def action(subWorld: World, herbivore: Herbivore): Int = {
    if (Qlearning.epsilon < Math.random()) {
      (Action.maxValue * Math.random()).toInt
    } else {
      stateActionValue.getBestAction(Qlearning.toState(subWorld, herbivore))
    }
  }
  
  def getBestAction(subWorld: World, herbivore: Herbivore): Int = stateActionValue.getBestAction(Qlearning.toState(subWorld, herbivore))
  
  def getValue: StateActionValue = stateActionValue
  def setValue(sav: StateActionValue): Unit = this.stateActionValue = sav
  
  def learningRasio: String = stateActionValue.bestAction.size + " " + stateActionValue.bestAction
}

object CarnivoreQlearning {
  
  private var stateActionValue = StateActionValue.empty

  def learn(carnivore0: Carnivore, carnivore1: Carnivore): Unit = {
    stateActionValue = Qlearning.update(carnivore0, carnivore1).exec(stateActionValue)
  }
  
  def action(subWorld: World, carnivore: Carnivore): Int = {
    if (Qlearning.epsilon < Math.random()) {
      (Action.maxValue * Math.random()).toInt
    } else {
      stateActionValue.getBestAction(Qlearning.toState(subWorld, carnivore))
    }
  }

  def getBestAction(subWorld: World, carnivore: Carnivore): Int = stateActionValue.getBestAction(Qlearning.toState(subWorld, carnivore))
  
  def getValue: StateActionValue = stateActionValue
  def setValue(sav: StateActionValue): Unit = this.stateActionValue = sav

  def learningRasio: String = stateActionValue.bestAction.size + " " + stateActionValue.bestAction
}

// (Environment State 4 * 9) * (Internal State 4 * 3) * (Action 7) = (Qvalue 3024)
case class StateActionValue(SA_Value: Map[(S, Int), Double], S_AValue: Map[S, Map[Int, Double]], bestAction: Map[S, Int]) {
  
  def update(s: S, a: Int, value: Double): StateActionValue = putSA_Value((s,a), value).putS_AValue(s, a, value).putBestAction(s, a, value)
  
  def putSA_Value(sa: (S, Int), value: Double): StateActionValue = copy(SA_Value = this.SA_Value + (sa -> value))
  
  def putS_AValue(s: S, a: Int, value: Double): StateActionValue = copy(S_AValue = this.S_AValue + (s -> putAValue(s, a, value)))
  
  def putAValue(s: S, a: Int, value: Double): Map[Int, Double] = S_AValue.getOrElse(s, Map.empty[Int, Double]) + (a -> value)
  
  def putBestAction(s: S, a: Int, value: Double): StateActionValue = {
    if (!bestAction.contains(s)) return copy(bestAction = this.bestAction + (s -> a))
    
    S_AValue.get(s) match {
      case Some(map) =>
        val max = map.maxBy(_._2)._2
        if (max <= value)
          copy(bestAction = this.bestAction + (s -> a))
        else
          this
      case None => copy(bestAction = this.bestAction + (s -> a))
    }
  }
  
  def getBestAction(s: S): Int = bestAction.getOrElse(s, (Action.maxValue * Math.random()).toInt)
}

object StateActionValue {
  
  val initValue = 100.0
  
  def empty: StateActionValue = StateActionValue(Map.empty[(S, Int), Double], Map.empty[S, Map[Int, Double]], Map.empty[S, Int])
}

//        |
//  ul    |   ur
//        |
//-----------------
//        |
//   bl   |   br
//        |
case class S(ul: SubState, ur: SubState, bl: SubState, br: SubState, bs: BioState)

object S {
  
  def apply(subWorld: World, animal: Animal): S = {
    try {
      val width = subWorld.cells.size
      val height = subWorld.cells(0).size
      val w = width / 2
      val h = height / 2

      val ul = subWorldToSubState(subWorld, 0, 0, w, h)
      val ur = subWorldToSubState(subWorld, w, 0, w, h)
      val bl = subWorldToSubState(subWorld, 0, h, w, h)
      val br = subWorldToSubState(subWorld, w, h, w, h)
      
      S(ul, ur, bl, br, BioState(animal))
    } catch {
      case _: Throwable => empty
    }
  }
  
  def subWorldToSubState(subWorld: World, startx: Int, starty: Int, width: Int, height: Int): SubState = {
    var pexists = false
    var hexists = false
    var cexists = false
    for (i <- startx to startx + width - 1) {
      for (j <- starty to starty + height - 1) {
        val bios = subWorld.cells(i)(j).bios
        if (!bios.isEmpty) {
          for (bio <- bios) {
            bio match {
              case p: Plant => pexists ||= true
              case h: Herbivore => hexists ||= true
              case c: Carnivore => cexists ||= true
            }
          }
        }
      }
    }
    SubState(pexists, hexists, cexists)
  }
  
  def empty: S = S(SubState.empty, SubState.empty, SubState.empty, SubState.empty, BioState.empty)
}

case class SubState(val plant: Boolean, val herbivore: Boolean, val carnivore: Boolean)
case class BioState(val angleType: Int, val speed: Int)

object SubState {

  def apply(ls: List[Boolean]): SubState = {
    if (ls.size == 3) {
      SubState(ls.head, ls.tail.head, ls.tail.tail.head)
    } else {
      empty
    }
  }

  def make(bios: List[Bio]): SubState = {
    val p = bios.filter(_.isInstanceOf[Plant]).size > 0
    val h = bios.filter(_.isInstanceOf[Herbivore]).size > 0
    val c = bios.filter(_.isInstanceOf[Carnivore]).size > 0
    SubState(p, h, c)
  }

  def allSubStates: List[SubState] = {
    val tf = List(true, false)
    val all = for (a <- tf; b <- tf; c <- tf) yield List(a, b, c)
    all map (ls => SubState(ls))
  }

  def empty: SubState = SubState(false, false, false)
  def plant: SubState = SubState(true, false, false)
  def harbivore: SubState = SubState(false, true, false)
  def carnivore: SubState = SubState(false, false, true)
}

//        |
//    0   |    1
//        |
//-----------------
//        |
//    2   |    3
//        |
object BioState {

  val maxSpeedType = 2
  val maxAngleType = 4

  def apply(animal: Animal): BioState = {
    val angle = animal.external.coordinates.angle % (2 * Math.PI)
    val angleType = angle match {
      case _ if (angle <= Math.PI / 2) => 0
      case _ if (Math.PI / 2 < angle && angle <= Math.PI) => 1
      case _ if (Math.PI < angle && angle <= 3 * Math.PI / 2) => 2
      case _ => 3
    }
    BioState(speedType(animal.velocity.speed), angleType)
  }

  def speedType(speed: Double): Int = if (maxSpeedType < speed.abs) maxSpeedType else speed.abs.toInt

  def allBioState: List[BioState] = {
    val speedTypes = (0 to maxSpeedType).toList
    val angleTypes = (0 to maxAngleType).toList
    for (a <- angleTypes; s <- speedTypes) yield BioState(s, a)
  }

  def empty: BioState = BioState(0, 0)
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