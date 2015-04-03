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
  var world = LearningWorld.toLearningWorld(World.init(fieldWidth, fieldHeight))
  
  def main(args: Array[String]): Unit = {
    Carnivore.setStrategy(new LearningStrategy())
    for (_ <- 1 to 10) {
      world = world.update
    }
    Memory.getTransitions foreach println
  }

}

class LearningStrategy() extends Strategy[Carnivore] {
  
  var stateActionValue: StateActionValue = null
  
  def chooseAction(subWorld: World, carn: Carnivore): Int = {
    val action = (Math.random() * 3).toInt//stateActionValue.getBestAction(Qlearning.toState(subWorld, carn))
    TransitionBuilder.setAction((carn.asInstanceOf[LearningCarnivore]).index, action)
    action
  }
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
    new LearningWorld(World(w.getBios filter (!_.isDead) map w.applyBoundaryCondition, w.width, w.height))
  }
  
  def isValid: Boolean = world.carnivores.count(_.isInstanceOf[LearningCarnivore]) == world.carnivores.size
}

object LearningWorld {
  
  def toLearningWorld(world: World): LearningWorld = {
    var newWorld = world
    world.carnivores foreach { c => 
      if (c.isInstanceOf[Carnivore]) {
        world.updateBio(c, LearningCarnivore(c))
      }}
    new LearningWorld(newWorld)
  }
  
  def evolveLearningCarnivore(carn: LearningCarnivore): LearningCarnivore = {
    LearningCarnivore(carn.evolve.asInstanceOf[Carnivore], carn.index)
  }
  
  def interactLearningCarnivore(world: World, carn: LearningCarnivore): World = {
    val w1 = carn.eatHervibore(world)
    val w2 = carn.giveBirthCarnivore(w1)
    val w3 = carn.chooseAction(w2)
    val i = carn.index
    val s = S(Carnivore.getSubWorld(world, carn), carn)
    TransitionBuilder.setNextState(i, s)
    TransitionBuilder.setReward(i, 0)
    TransitionBuilder.build(i) match {
      case Some(t) => Memory.putTransition(t)
      case None => 
    }
    TransitionBuilder.init(i)
    TransitionBuilder.setCurrentState(i, s)
    w3
  }
}

object Memory {
  
  var logs = List.empty[Transition]
  
  def putTransition(log: Transition): Unit = {
    logs = log :: logs
  }
  
  def getTransitions: List[Transition] = logs
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
  
  def setCurrentState(i: Int, s: S): Unit = current += Tuple2(i, s)
  
  def setNextState(i: Int, s: S): Unit = next += Tuple2(i, s)
  
  def setReward(i: Int, r: Int): Unit = reward += Tuple2(i, r)
  
  def setAction(i: Int, a: Int): Unit = action += Tuple2(i, a)
  
  def build(i: Int): Option[Transition] = {
    (current.get(i), action.get(i), reward.get(i), next.get(i)) match {
      case (Some(c), Some(a), Some(r), Some(n)) => Some(Transition(c, a, r, n))
      case _ => None
    }
  }
}

case class Transition(current: S, action: Int, reward: Int, next: S)

class LearningCarnivore(external: External, internal: Internal, velocity: Velocity, val index: Int) extends Carnivore(external, internal, velocity) {
  
  override def setExternal(e: External): LearningCarnivore = LearningCarnivore(copy(external = e), index)
  override def setExternal(c: Coordinates, a: Appearance): LearningCarnivore = LearningCarnivore(copy(external = External(c, a)), index)
  override def setInternal(l: Int, w: Int, m: Int): LearningCarnivore = LearningCarnivore(copy(internal = Internal(l, w, m)), index)
  override def setLife(l: Int): LearningCarnivore = LearningCarnivore(copy(internal = Internal(l, internal.water, internal.mineral)), index)
  override def setLife(f: Int => Int): LearningCarnivore = setLife(f(internal.life))
  override def setVelocity(v: Velocity): LearningCarnivore = LearningCarnivore(copy(velocity = v), index)
  
  override def toString: String = "Learning" + super.toString
}

object LearningCarnivore {
  
  def apply(carn: Carnivore): LearningCarnivore = {
    new LearningCarnivore(carn.external, carn.internal, carn.velocity, IndexMaker.getIndex())
  }
  
  def apply(carn: Carnivore, i: Int): LearningCarnivore = {
    new LearningCarnivore(carn.external, carn.internal, carn.velocity, i)
  }
}

object IndexMaker {
  
  var counter = 0
  
  def getIndex(): Int = {
    counter += 1
    counter
  }
}