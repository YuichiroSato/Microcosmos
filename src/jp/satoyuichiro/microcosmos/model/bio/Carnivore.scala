package jp.satoyuichiro.microcosmos.model.bio

import jp.satoyuichiro.microcosmos.model.World
import java.awt.Color
import jp.satoyuichiro.microcosmos.model.learning.Action
import jp.satoyuichiro.microcosmos.model.learning.Qlearning
import jp.satoyuichiro.microcosmos.model.learning.StateActionFunction

case class Carnivore(override val external: External, override val internal: Internal, override val velocity: Velocity, var learningInfo: LearningInfo)
  extends Animal(external, internal, velocity) {

  def evolve: Bio = setExternal(move, external.appearance).setLife(_ - 1)

  def interact(world: World): World = chooseAction(giveBirthCarnivore(eatHervibore(world)))

  val filterf = (bio: Bio) => bio.isInstanceOf[Herbivore]
  val updatef = () => setLife(_ + Carnivore.lifeUp)

  def eatHervibore(world: World): World = {
    eat(world, filterf, updatef)
  }

  val condition = () => Carnivore.giveBirthLife < internal.life
  val born = () => Carnivore(external.coordinates.x, external.coordinates.y)
  val update2 = () => setLife(_ - Carnivore.giveBirthCost)

  def giveBirthCarnivore(world: World) = {
    giveBirth(world, condition, born, update2)
  }

  def isDead: Boolean = internal.life <= 0

  def chooseAction(world: World): World = {
    if (learningInfo.count < 0) {
      if (learningInfo.learning) {
        val subWorld = world.getSubWorldAround(this, 40, 40)
        val action = Qlearning.carnivoreAction(subWorld, this)
        val nextVelocity = Action.carnivoreAction(action, velocity)
        val nextLearnInfo = LearningInfo(Carnivore.learningInterval, subWorld, this, action, true)
        val carb = new Carnivore(external, internal, nextVelocity, nextLearnInfo)
        Qlearning.carnivoreLearn(this, carb)
        world.remove(this)
        world.add(carb)
      } else {
        val subWorld = world.getSubWorldAround(this, 40, 40)
        val action = StateActionFunction.carnivoreAction(subWorld, this)
        val nextVelocity = Action.carnivoreAction(action, velocity)
        val nextLearnInfo = LearningInfo(Carnivore.learningInterval, subWorld, this, action)
        val carb = Carnivore(external, internal, nextVelocity, nextLearnInfo)
        world.remove(this)
        world.add(carb)
      }
    }
    learningInfo = learningInfo.decriment
    world
  }
  
  def setExternal(e: External): Carnivore = copy(external = e)
  def setExternal(c: Coordinates, a: Appearance): Carnivore = copy(external = External(c, a))
  def setInternal(l: Int, w: Int, m: Int): Carnivore = copy(internal = Internal(l, w, m))
  def setLife(l: Int): Carnivore = copy(internal = Internal(l, internal.water, internal.mineral))
  def setLife(f: Int => Int): Carnivore = setLife(f(internal.life))
  def setLearningTrue: Carnivore = copy(learningInfo = learningInfo.setLearningTrue)
}

object Carnivore {

  val lifeUp = 200
  val giveBirthLife = 5000
  val initLife = 500
  val giveBirthCost = 4000
  val learningInterval = 19

  def apply(x: Int, y: Int): Carnivore = {
    val coordinates = Coordinates(x, y, Math.random())
    val appearance = Appearance(12, Color.RED)
    val velocity = Velocity(5 * Math.random(), Math.random() - 0.5)
    val learningInfo = LearningInfo((learningInterval * Math.random()).toInt, World.empty, Carnivore.empty, Action.maxValue)
    Carnivore(External(coordinates, appearance), Internal(initLife, 10, 10), velocity, learningInfo)
  }
  
  def learning(x: Int, y: Int): Carnivore = apply(x, y).setLearningTrue
  
  def empty: Carnivore = {
    val external = External(Coordinates(0,0,0), Appearance(0, Color.RED))
    val internal = Internal(initLife, 0, 0)
    val velocity = Velocity(0, 0)
    val learningInfo0 = LearningInfo(learningInterval, World.empty, null, Action.maxValue)
    val animal = Carnivore(external, internal, velocity, learningInfo0)
    val learningInfo1 = LearningInfo(learningInterval, World.empty, animal, Action.maxValue)
    Carnivore(external, internal, velocity,  learningInfo1)
  }
}
