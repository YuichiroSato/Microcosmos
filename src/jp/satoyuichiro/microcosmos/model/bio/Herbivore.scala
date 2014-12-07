package jp.satoyuichiro.microcosmos.model.bio

import jp.satoyuichiro.microcosmos.model.World
import java.awt.Color
import jp.satoyuichiro.microcosmos.model.learning.Action
import jp.satoyuichiro.microcosmos.model.learning.Qlearning
import jp.satoyuichiro.microcosmos.model.learning.StateActionFunction

case class Herbivore(override val external: External, override val internal: Internal, override val velocity: Velocity, var learningInfo: LearningInfo)
  extends Animal(external, internal, velocity) {

  def evolve: Bio = setExternal(move, external.appearance).setInternal(internal.life - 1, internal.water, internal.mineral)
  
  def interact(world: World): World = chooseAction(giveBirthHerbivore(eatPlant(world)))
  
  val filterf = (bio: Bio) => bio.isInstanceOf[Plant]
  val updatef = () => setInternal(internal.life + Herbivore.lifeUp, internal.water, internal.mineral)
  
  def eatPlant(world: World): World = {
    eat(world, filterf, updatef)
  }

  val condition = () => Herbivore.giveBirthLife < internal.life
  val born = () => Herbivore(external.coordinates.x, external.coordinates.y)
  val update2 = () => setInternal(internal.life - Herbivore.giveBirthCost, internal.water, internal.mineral)
  
  def giveBirthHerbivore(world: World) = {
    giveBirth(world, condition, born, update2)
  }
  
  def isDead: Boolean = internal.life <= 0
  
  def chooseAction(world: World): World = {
    if (learningInfo.count < 0) {
      val subWorld = world.getSubWorldAround(this, 40, 40)
      val action = StateActionFunction.herbivoreAction(subWorld, this)
      val nextVelocity = Action.herbivoreAction(action, velocity)
      val nextLearningInfo = LearningInfo(Herbivore.learningInterval, subWorld, this, action)
      val herb = Herbivore(external, internal, nextVelocity, nextLearningInfo)
      world.remove(this)
      world.add(herb)
    }
    learningInfo = learningInfo.decriment
    world
  }
  
  def setExternal(e: External): Herbivore = Herbivore(e, internal, velocity, learningInfo)
  def setExternal(c: Coordinates, a: Appearance): Herbivore = Herbivore(External(c, a), internal, velocity, learningInfo)
  def setInternal(l: Int, w: Int, m: Int): Herbivore = Herbivore(external, Internal(l, w, m), velocity, learningInfo)
}

object Herbivore {
    
  val lifeUp = 200
  val giveBirthLife = 4000
  val initLife = 100
  val giveBirthCost = 2000
  val learningInterval = 19
  
  def apply(x: Int, y: Int): Herbivore = {
    val coordinates = Coordinates(x,y, Math.random)
    val appearance = Appearance(12, Color.BLUE)
    val velocity = Velocity(10 * Math.random(), Math.random() - 0.5)
    val learningInfo = LearningInfo((learningInterval * Math.random()).toInt, World.empty, Herbivore.empty, Action.maxValue)
    Herbivore(External(coordinates, appearance), Internal(initLife, 10, 10), velocity, learningInfo)
  }

  def empty: Herbivore = {
    val external = External(Coordinates(0,0,0), Appearance(0, Color.RED))
    val internal = Internal(initLife, 0, 0)
    val velocity = Velocity(0, 0)
    val learningInfo0 = LearningInfo(learningInterval, World.empty, null, Action.maxValue)
    val animal = Herbivore(external, internal, velocity, learningInfo0)
    val learningInfo1 = LearningInfo(learningInterval, World.empty, animal, Action.maxValue)
    Herbivore(external, internal, velocity,  learningInfo1)
  }
}