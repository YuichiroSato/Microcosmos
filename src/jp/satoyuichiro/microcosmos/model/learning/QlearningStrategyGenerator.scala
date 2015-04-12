package jp.satoyuichiro.microcosmos.model.learning

import jp.satoyuichiro.microcosmos.model.bio.External
import jp.satoyuichiro.microcosmos.model.bio.Internal
import jp.satoyuichiro.microcosmos.model.bio.Velocity
import jp.satoyuichiro.microcosmos.model.bio.LearningInfo
import jp.satoyuichiro.microcosmos.model.bio.Carnivore
import jp.satoyuichiro.microcosmos.model.World
import jp.satoyuichiro.microcosmos.model.Cell
import jp.satoyuichiro.microcosmos.model.bio.Plant
import jp.satoyuichiro.microcosmos.model.bio.Herbivore
import jp.satoyuichiro.microcosmos.model.bio.Bio
import jp.satoyuichiro.microcosmos.model.bio.Coordinates
import jp.satoyuichiro.microcosmos.model.bio.Appearance

object QlearningStrategyGenerator {

  val fieldWidth = 600
  val fieldHeight = 600
  var world = LearningWorld.init(fieldWidth, fieldHeight)
  
  def main(args: Array[String]): Unit = {
    Carnivore.setStrategy(new LearningStrategy())
    val n = 10000
    val de = 1.0 / n.toDouble
    var time = 0
    for (i <- 1 to 2 * n) {
      if (i % 100 == 0) println(i + " time" + time + " learning" + Qlearning.learningRasio(Memory.getNext))
      world = world.update
      time += 1
      if (world.isEnd || 100 < time) {
        world = LearningWorld.init(fieldWidth, fieldHeight)
        val sav = Memory.getNext
        val strategy = new LearningStrategy()
        strategy.setSAV(sav)
        Carnivore.setStrategy(strategy)
        time = 0
      }
      LearningStrategy.incrimentEpsilon(de)
    }
    println(Qlearning.learningRasio(Memory.getNext))
    StateActionValue.serialize(Memory.getNext, "Qlearning10000")
  }

}

class LearningStrategy() extends Strategy[Carnivore] {
  
  var stateActionValue = StateActionValue.empty
  
  def chooseAction(subWorld: World, carn: Carnivore): Int = {
    val lcarn = carn.asInstanceOf[LearningCarnivore]
    if (lcarn.count <= 0) {
      if (Math.random() < LearningStrategy.epsilon) {
        val action = stateActionValue.getBestAction(Qlearning.toState(subWorld, carn))
        TransitionBuilder.setAction(lcarn.index, action)
        action
      } else {
        val action = Carnivore.randomAction
        TransitionBuilder.setAction(lcarn.index, action)
        action
      }
    } else {
      Carnivore.doNothing
    }
  }
  
  def setSAV(sav: StateActionValue): Unit = stateActionValue = sav
}

object LearningStrategy {

  var epsilon = 0.0
  
  def setEpsilon(e: Double): Unit = epsilon = e
  def incrimentEpsilon(d: Double): Unit = epsilon += d
  
}

case class LearningWorld(world: World) {
  
  def update: LearningWorld = evolve.interact
  
  def evolve: LearningWorld = {
    if (isValid) {
      val evolved = (world.plants map (_.evolve.asInstanceOf[Plant])) ++
        (world.carnivores map (c => LearningWorld.evolveLearningCarnivore(c.asInstanceOf[LearningCarnivore]))) ++
        (world.herbivores map (_.evolve.asInstanceOf[Herbivore]))
      new LearningWorld(World(evolved map world.applyBoundaryCondition, world.width, world.height))
    } else {
      this
    }
  }
  
  def interact: LearningWorld = {
    var w = world
    for (bio <- w.getBios) {
      if (bio.isInstanceOf[LearningCarnivore]) {
        w = LearningWorld.interactLearningCarnivore(w, bio.asInstanceOf[LearningCarnivore])
      } else {
        w = bio.interact(w)
      }
    }
    LearningWorld.toLearningWorld(World(w.getBios filter (!_.isDead) map w.applyBoundaryCondition, w.width, w.height))
  }
  
  def isValid: Boolean = world.carnivores.count(_.isInstanceOf[LearningCarnivore]) == world.carnivores.size
  
  def isEnd: Boolean = world.isEnd
}

object LearningWorld {

  def init(fieldWidth: Int, fieldHeight: Int): LearningWorld = {
    LearningWorld.toLearningWorld(World.init(fieldWidth, fieldHeight))
  }
  
  def toLearningWorld(world: World): LearningWorld = {
    var newWorld = world
    world.carnivores foreach { c => 
      if (!c.isInstanceOf[LearningCarnivore]) {
        world.updateBio(c, LearningCarnivore.fromCarnivore(c))
      }}
    new LearningWorld(newWorld)
  }
  
  def evolveLearningCarnivore(carn: LearningCarnivore): LearningCarnivore = {
    val buf = carn.internal.life
    val newCarn = LearningCarnivore(carn.evolve.asInstanceOf[Carnivore])
    TransitionBuilder.setReward(carn.index, newCarn.internal.life - buf)
    if (carn.isDead) {
      TransitionBuilder.addReward(carn.index, -1000)
    }
    newCarn
  }
  
  def interactLearningCarnivore(world: World, carn: LearningCarnivore): World = {
    val i = carn.index

    val herbN = world.herbivores.size
    val w1 = carn.eatHervibore(world)
    TransitionBuilder.addReward(i, (herbN - w1.herbivores.size) * 100)
    
    val carnN = world.carnivores.size
    val w2 = carn.giveBirthCarnivore(w1)
    if (carnN < w2.carnivores.size) {
      TransitionBuilder.addReward(i, 1000)
    }
    
    val w3 = carn.chooseAction(w2)
    
    val s = S(Carnivore.getSubWorld(world, carn), carn)
    TransitionBuilder.setInitState(i, s)
    TransitionBuilder.setNextState(i, s)
    TransitionBuilder.build(i) match {
      case Some(t) =>
        Memory.putTransition(t)
        TransitionBuilder.init(i)
        TransitionBuilder.setCurrentState(i, s)
        Memory.doLearning
      case None => 
    }
    w3
  }
}

object Memory {
  
  val maxSize = 100
  var logs = List.empty[Transition]
  var nextSAV = StateActionValue.empty
  
  def putTransition(log: Transition): Unit = {
    if (logs.size < maxSize) {
      logs = log :: logs
    } else {
      logs = logs.dropRight(1)
      logs = log :: logs
    }
  }
  
  def getTransitions: List[Transition] = logs
  
  def getRandomTransition: Transition = {
    val n = (Math.random() * logs.size).toInt
    logs.drop(n).head
  }
  
  def doLearning: Unit = {
    val t = getRandomTransition
    nextSAV = Qlearning.update(nextSAV, t.current, t.action, t.reward, t.next)
  }
  
  def setNext(sav: StateActionValue): Unit = nextSAV = sav
  
  def getNext: StateActionValue = nextSAV
}

object TransitionBuilder {
  
  var current: Map[Int, S] = Map.empty[Int, S]
  var action: Map[Int, Int] = Map.empty[Int, Int]
  var reward: Map[Int, Int] = Map.empty[Int, Int]
  var next: Map[Int, S] = Map.empty[Int, S]
  
  def init(i: Int): Unit = {
    current -= i
    action -= i
    reward -= i
    next -= i
  }
  
  def setInitState(i: Int, s: S): Unit = if (!current.keySet.contains(i)) current += Tuple2(i, s)
  
  def setCurrentState(i: Int, s: S): Unit = current += Tuple2(i, s)
  
  def setNextState(i: Int, s: S): Unit = next += Tuple2(i, s)
  
  def setReward(i: Int, r: Int): Unit = reward += Tuple2(i, r)
  
  def addReward(i: Int, r: Int): Unit = reward += Tuple2(i, reward.getOrElse(i, 0) + r)
  
  def setAction(i: Int, a: Int): Unit = action += Tuple2(i, a)
  
  def build(i: Int): Option[Transition] = {
    (current.get(i), action.get(i), reward.get(i), next.get(i)) match {
      case (Some(c), Some(a), Some(r), Some(n)) => Some(Transition(c, a, r, n))
      case _ => None
    }
  }
}

case class Transition(current: S, action: Int, reward: Int, next: S)

class LearningCarnivore(external: External, internal: Internal, velocity: Velocity, count: Int, index: Int)
  extends Carnivore(external, internal, velocity, count, index) {
  
  override val maxCount = LearningCarnivore.maxCount
  
  override def setExternal(e: External): LearningCarnivore = LearningCarnivore(copy(external = e))
  override def setExternal(c: Coordinates, a: Appearance): LearningCarnivore = LearningCarnivore(copy(external = External(c, a)))
  override def setInternal(l: Int, w: Int, m: Int): LearningCarnivore = LearningCarnivore(copy(internal = Internal(l, w, m)))
  override def setLife(l: Int): LearningCarnivore = LearningCarnivore(copy(internal = Internal(l, internal.water, internal.mineral)))
  override def setLife(f: Int => Int): LearningCarnivore = setLife(f(internal.life))
  override def setVelocity(v: Velocity): LearningCarnivore = LearningCarnivore(copy(velocity = v))
  override def decrimentCounter: LearningCarnivore = LearningCarnivore(copy(count = count - 1))
  override def repareCounter: LearningCarnivore = LearningCarnivore(copy(count = maxCount))
    
  override def toString: String = "Learning" + super.toString
}

object LearningCarnivore {
  
  val maxCount = 10
  
  def fromCarnivore(carn: Carnivore): LearningCarnivore = {
    new LearningCarnivore(carn.external, carn.internal, carn.velocity, maxCount, IndexMaker.getIndex())
  }
  
  def apply(carn: Carnivore): LearningCarnivore = {
    new LearningCarnivore(carn.external, carn.internal, carn.velocity, carn.count, carn.index)
  }
}

object IndexMaker {
  
  var counter = 0
  
  def getIndex(): Int = {
    counter += 1
    counter
  }
}