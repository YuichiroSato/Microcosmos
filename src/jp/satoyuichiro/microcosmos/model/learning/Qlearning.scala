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
import java.io.FileOutputStream
import java.io.ObjectOutputStream
import java.io.File
import java.io.FileInputStream
import java.io.InputStream
import java.io.ObjectInputStream

object Qlearning {

  def update(sav: StateActionValue, current: S, action: Int, reward: Int, next: S): StateActionValue = {
    val avMap = getAVMap(sav, current)
    if (avMap.isEmpty)
      insertNewSA(sav, current, action)
    else {
      val oldValue = getOldValue(sav, next, action)
      val maxValue = getMaxValue(sav, avMap)
      val newValue = calculateNewValue(oldValue, reward, maxValue)
      updateValue(sav, next, action, newValue)
    }
  }
  
  def getOldValue(sav: StateActionValue, s: S, a: Int): Double = {
    sav.SA_Value.getOrElse((s,a), StateActionValue.initValue)
  }
  
  def getAVMap(sav: StateActionValue, s: S): Map[Int, Double] = {
    sav.S_AValue.getOrElse(s, Map.empty[Int, Double])
  }
  
  def insertNewSA(sav: StateActionValue, targetState: S, action: Int): StateActionValue = {
    sav.update(targetState, action, StateActionValue.initValue)
  }
  
  def getMaxValue(sav: StateActionValue, map: Map[Int, Double]): Double = {
    if (map.isEmpty) Double.MinValue else map.maxBy(_._2)._2
  }
  
  def updateValue(sav: StateActionValue, targetState: S, action: Int, newValue: Double): StateActionValue = {
    sav.update(targetState, action, newValue)
  }
  
  val alpha = 0.1
  val gamma = 0.9
  
  def calculateNewValue(oldValue: Double, reward: Double, maxValue: Double): Double = oldValue + alpha * (reward + gamma * maxValue - oldValue)

  def toState(subWorld: World, animal: Animal): S = {
    S(subWorld, animal)
  }
  
  def learningRasio(sav: StateActionValue): String = sav.bestAction.size.toString
}

class QlearningStrategy(stateActionValue: StateActionValue) extends Strategy[Carnivore]{
  
  def chooseAction(subWorld: World, carn: Carnivore): Int = {
    stateActionValue.getBestAction(Qlearning.toState(subWorld, carn))
  }
}

// (Environment State 4 * 9) * (Internal State 8 * 3) * (Action 7) = (Qvalue 6048)
case class StateActionValue(SA_Value: Map[(S, Int), Double], S_AValue: Map[S, Map[Int, Double]], bestAction: Map[S, Int]) extends Serializable {
  
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
  
    def deserialize(fileName: String): StateActionValue = {
    val file = new File(fileName + ".sav")
    
    if (file.exists()) {
      val fStream = new FileInputStream(file)
	  val oStream = new ObjectInputStream(fStream)
      try {
        oStream.readObject().asInstanceOf[StateActionValue]
      } catch {
        case e: Throwable => throw e
      } finally {
        oStream.close()
        fStream.close()
      }
    } else {
      throw new RuntimeException("no file")
    }
  }
  
  def serialize(sav: StateActionValue, fileName: String): Unit = {
    try {
      val fStream = new FileOutputStream(fileName + ".sav")
	  val oStream = new ObjectOutputStream(fStream)
	  oStream.writeObject(sav)
	  
	  fStream.close()
	  oStream.close()
    } catch {
      case _: Throwable => println("something wrong!")
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
case class S(ul: SubState, ur: SubState, bl: SubState, br: SubState, bs: BioState) {
  
  override def toString: String = {
    val sul = subStateToString(ul)
    val sur = subStateToString(ur)
    val sbl = subStateToString(bl)
    val sbr = subStateToString(br)
    sul + sur + sbl + sbr + "-" + bs.speed + bs.angleType
  }
  
  def subStateToString(subState: SubState): String = {
    var res = 0
    res += (if (subState.plant) 1 else 0)
    res += (if (subState.herbivore) 2 else 0)
    res += (if (subState.carnivore) 4 else 0)
    res.toString
  }
}

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
case class BioState(val speed: Int, val angleType: Int)

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

  def speedType(speed: Double): Int = {
    speed match {
      case s if (maxSpeedType < s) => maxSpeedType
      case s if (0 <= s) => s.toInt
      case s if (-maxSpeedType <= s) => s.toInt - 1
      case s if (s < -maxSpeedType) => -maxSpeedType - 1
    }
  }

  def allBioState: List[BioState] = {
    val speedTypes = (0 to maxSpeedType).toList
    val angleTypes = (0 to maxAngleType).toList
    for (a <- angleTypes; s <- speedTypes) yield BioState(s, a)
  }

  def empty: BioState = BioState(0, 0)
}

object Action {

  val maxValue = 6

  def index = (0 to maxValue).toList

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