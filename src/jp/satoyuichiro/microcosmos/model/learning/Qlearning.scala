package jp.satoyuichiro.microcosmos.model.learning

import jp.satoyuichiro.microcosmos.model.bio.Velocity
import jp.satoyuichiro.microcosmos.model.World
import jp.satoyuichiro.microcosmos.model.bio.Bio
import jp.satoyuichiro.microcosmos.model.bio.Plant
import jp.satoyuichiro.microcosmos.model.bio.Herbivore
import jp.satoyuichiro.microcosmos.model.bio.Carnivore
import scala.collection.immutable.Queue

object Qlearning {

  private var carnivoreQ = null: Qvalue
  private var herbivoreQ = null: Qvalue

  private var carnivoreLookUp = Map.empty[State, Int]
  private var herbivoreLookUp = Map.empty[State, Int]

  private var carnivoreInput = Queue.empty[Tuple4[State, Int, State, Int]]
  private var herbivoreInput = Queue.empty[Tuple4[State, Int, State, Int]]

  def init: Unit = {
    carnivoreQ = Qvalue.init
    herbivoreQ = Qvalue.init

    carnivoreLookUp = carnivoreQ.initLookUpTable
    herbivoreLookUp = herbivoreQ.initLookUpTable
  }

  val maxSize = 100
  
  def herbivoreLearn(herbivore0: Herbivore, herbivore1: Herbivore): Unit = {
    if (herbivoreInput.size < maxSize) {
      val state0 = State(herbivore0.learningInfo.subWorld, herbivore0.learningInfo.animal.velocity)
      val action = herbivore0.learningInfo.action
      val state1 = State(herbivore0.learningInfo.subWorld, herbivore0.learningInfo.animal.velocity)
      val reward = herbivore1.internal.life - herbivore0.learningInfo.animal.internal.life
      herbivoreInput = herbivoreInput.enqueue((state0, action, state1, reward))
    }
  }

  def carnivoreLearn(carnivore0: Carnivore, carnivore1: Carnivore): Unit = {
    if (carnivoreInput.size < maxSize) {
      val state0 = State(carnivore0.learningInfo.subWorld, carnivore0.learningInfo.animal.velocity)
      val action = carnivore0.learningInfo.action
      val state1 = State(carnivore0.learningInfo.subWorld, carnivore0.learningInfo.animal.velocity)
      val reward = carnivore1.internal.life - carnivore0.learningInfo.animal.internal.life
      carnivoreInput = carnivoreInput.enqueue((state0, action, state1, reward))
    }
  }
  
  def endOfLearning: Boolean = herbivoreInput.size >= maxSize || carnivoreInput.size >= maxSize
  
  def update(): Unit = {
    println("updating")
    val start = System.currentTimeMillis()
    for(elem <- herbivoreInput) {
      herbivoreUpdate(elem._1, elem._2, elem._3, elem._4)
    }
    for(elem <- carnivoreInput) {
      carnivoreUpdate(elem._1, elem._2, elem._3, elem._4)
    }
    println("end "+ (System.currentTimeMillis() - start))
  }

  val alpha = 0.1
  val gamma = 0.9

  def herbivoreUpdate(targetState: State, action: Int, currentState: State, reward: Int): Unit = {
    val targetQ = (targetState, action)
    val oldQ = herbivoreQ.values.getOrElse(targetQ, Qvalue.initValue)
    herbivoreQ.pickUp.get(targetState) match {
      case Some(candidateQ) if 0 < candidateQ.size =>
        val start = System.currentTimeMillis()
        val maxQ = (candidateQ maxBy (_._2))._2
        System.out.println(System.currentTimeMillis() - start)
        val newQ = oldQ + alpha * (reward + gamma * maxQ - oldQ)
        if (herbivoreLookUp.getOrElse(targetQ._1, Int.MinValue) < newQ) {
          herbivoreLookUp += targetQ._1 -> targetQ._2
        }
        herbivoreQ.upadte(targetQ, newQ)
//        println("herb " + targetQ._2 + " r " + reward +" new " + newQ + " old "+ oldQ + " max " + maxQ)
      case None => 
    }
  }

  def carnivoreUpdate(targetState: State, action: Int, currentState: State, reward: Int): Unit = {
    val targetQ = (targetState, action)
    val oldQ = carnivoreQ.values.getOrElse(targetQ, Qvalue.initValue)
    carnivoreQ.pickUp.get(targetState) match {
      case Some(candidateQ) if 0 < candidateQ.size=>
        val maxQ = (candidateQ maxBy (_._2))._2
        val newQ = oldQ + alpha * (reward + gamma * maxQ - oldQ)
        if (carnivoreLookUp.getOrElse(targetQ._1, Int.MinValue) < newQ) {
          carnivoreLookUp += targetQ._1 -> targetQ._2
        }
        carnivoreQ.upadte(targetQ, newQ)
//        println("herb " + targetQ._2 + " r " + reward +" new " + newQ + " old "+ oldQ + " max " + maxQ)
      case None => 
    }
  }
  
  val epsilon = 0.1

  def herbivoreAction(subWorld: World, velocity: Velocity): Int = {
    if (epsilon < Math.random()) {
      (Action.maxValue * Math.random()).toInt
    } else {
      herbivoreLookUp.getOrElse(Qvalue.toState(subWorld, velocity), Action.maxValue)
    }
  }

  def carnivoreAction(subWorld: World, velocity: Velocity): Int = {
    if (epsilon < Math.random()) {
      (Action.maxValue * Math.random()).toInt
    } else {
      carnivoreLookUp.getOrElse(Qvalue.toState(subWorld, velocity), Action.maxValue)
    }
  }
  
  def getCarniveorLookUp: Map[State, Int] = carnivoreLookUp
  def getHerbivoreLookUp: Map[State, Int] = herbivoreLookUp
}

// (State 4 * 9 * 16 * 6) * (Action 7) = (Qvalue 24192)
case class Qvalue(var values: Map[Tuple2[State, Int], Double], var pickUp: Map[State, Map[Int, Double]]) {

  def initLookUpTable: Map[State, Int] = {
    State.allStates map (state => state -> Action.maxValue) toMap
  }

  def upadte(target: Tuple2[State, Int], value: Double): Unit = {
    values += target -> value
    var subPickUp = pickUp.getOrElse(target._1, Map.empty[Int, Double])
    subPickUp += target._2 -> value
    pickUp += target._1 -> subPickUp
  }
}

object Qvalue {

  val initValue = 100.0

  def init: Qvalue = {
    val values = State.allStates flatMap (s => makePair(s, Action.index)) map (t => t -> initValue) toMap
    var pickUp = Map.empty[State, Map[Int, Double]]
    values foreach {
      tuple =>
        val state = tuple._1._1
        val actionValueMap = (Action.index map (i => i -> initValue)).toMap
        pickUp += state -> actionValueMap
    }
    Qvalue(values, pickUp)
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

object StateActionFunction {
  
  private var carnivoreLookUp = Map.empty[State, Int]
  private var herbivoreLookUp = Map.empty[State, Int]

  def setHerbivoreQ(map: Map[State, Int]): Unit = herbivoreLookUp = map
  def setCarnivoreQ(map: Map[State, Int]): Unit = carnivoreLookUp = map
  
  def herbivoreAction(subWorld: World, velocity: Velocity): Int = {
      herbivoreLookUp.getOrElse(Qvalue.toState(subWorld, velocity), (Action.maxValue * Math.random()).toInt)
  }

  def carnivoreAction(subWorld: World, velocity: Velocity): Int = {
      carnivoreLookUp.getOrElse(Qvalue.toState(subWorld, velocity), (Action.maxValue * Math.random()).toInt)
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
case class BioState(val ulAhead: Boolean, val urAhead: Boolean, val blAhead: Boolean, val brAhead: Boolean, val speed: Int)

object State {

  def apply(subWorld: World, velocity: Velocity): State = {
    try {
      val width = subWorld.cells.size
      val height = subWorld.cells(0).size
      val w = width / 2
      val h = height / 2

//      val ul = subWorld.getSubWorld(0, 0, w, h).getBios
//      val ur = subWorld.getSubWorld(w, 0, w, h).getBios
//      val bl = subWorld.getSubWorld(0, h, w, h).getBios
//      val br = subWorld.getSubWorld(w, h, w, h).getBios
//
//      State(SubState.make(ul), SubState.make(ur), SubState.make(bl), SubState.make(br), BioState(velocity))
      val ul = subWorldToSubState(subWorld, 0, 0, w, h)
      val ur = subWorldToSubState(subWorld, w, 0, w, h)
      val bl = subWorldToSubState(subWorld, 0, h, w, h)
      val br = subWorldToSubState(subWorld, w, h, w, h)
      
      State(ul, ur, bl, br, BioState(velocity))
    } catch {
      case _: Throwable => State(SubState.empty, SubState.empty, SubState.empty, SubState.empty, BioState.empty)
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
          bios foreach {
            bio => bio match {
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

  def allStates: List[State] = {
    val sall = SubState.allSubStates
    val ball = BioState.allBioState
    for (a <- sall; b <- sall; c <- sall; d <- sall; e <- ball) yield State(a, b, c, d, e)
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
//  ul    |   ur
//        |
//-----------------
//        |
//   bl   |   br
//        |
object BioState {

  val maxSpeedType = 5

  def apply(ls: List[Boolean]): BioState = {
    if (ls.size == 4) {
      BioState(ls.head, ls.drop(1).head, ls.drop(2).head, ls.drop(3).head, 0)
    } else {
      empty
    }
  }

  def apply(ls: List[Boolean], speedType: Int): BioState = {
    if (ls.size == 4) {
      BioState(ls.head, ls.drop(1).head, ls.drop(2).head, ls.drop(3).head, speedType)
    } else {
      empty
    }
  }

  def apply(velocity: Velocity): BioState = {
    val ul = 0 < velocity.rotation && velocity.rotation <= Math.PI / 2
    val ur = Math.PI / 2 < velocity.rotation && velocity.rotation <= Math.PI
    val bl = Math.PI < velocity.rotation && velocity.rotation <= 3 * Math.PI / 2
    val br = 3 * Math.PI / 2 < velocity.rotation && velocity.rotation <= 0
    BioState(ul, ur, bl, br, speedType(velocity.speed))
  }

  def speedType(speed: Double): Int = if (maxSpeedType < speed) maxSpeedType else speed.abs.toInt

  def allBioState: List[BioState] = {
    val tf = List(true, false)
    val speedTypes = (0 to maxSpeedType).toList
    val all = for (a <- tf; b <- tf; c <- tf; d <- tf) yield List(a, b, c, d)
    all flatMap (ls => speedTypes map (s => BioState(ls, s)))
  }

  def empty: BioState = BioState(false, false, false, false, 0)
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