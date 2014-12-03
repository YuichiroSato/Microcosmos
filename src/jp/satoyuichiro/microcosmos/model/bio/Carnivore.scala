package jp.satoyuichiro.microcosmos.model.bio

import jp.satoyuichiro.microcosmos.model.World
import java.awt.Color
import jp.satoyuichiro.microcosmos.model.learning.Action
import jp.satoyuichiro.microcosmos.model.learning.Qlearning
import jp.satoyuichiro.microcosmos.model.learning.StateActionFunction

case class Carnivore(override val external: External, override val internal: Internal, override val velocity: Velocity, var learningInfo: LearningInfo)
  extends Animal(external, internal, velocity) {

  def evolve: Bio = Carnivore(External(move, external.appearance), Internal(internal.life - 1, internal.water, internal.mineral), velocity, learningInfo)

  def interact(world: World): World = chooseAction(giveBirthCarnivore(eatHervibore(world)))

  val filterf = (bio: Bio) => bio.isInstanceOf[Herbivore]
  val updatef = () => Carnivore(external, Internal(internal.life + Carnivore.lifeUp, internal.water, internal.mineral), velocity, learningInfo)

  def eatHervibore(world: World): World = {
    eat(world, filterf, updatef)
  }

  val condition = () => Carnivore.giveBirthLife < internal.life
  val born = () => Carnivore(external.coordinates.x, external.coordinates.y)
  val update2 = () => Carnivore(external, Internal(internal.life - Carnivore.giveBirthCost, internal.water, internal.mineral), velocity, learningInfo)

  def giveBirthCarnivore(world: World) = {
    giveBirth(world, condition, born, update2)
  }

  def isDead: Boolean = internal.life <= 0

  def chooseAction(world: World): World = {
    if (learningInfo.count < 0) {
      val subWorld = world.getSubWorldAround(this, 20, 20)
      val action = StateActionFunction.carnivoreAction(subWorld, velocity)
      val carb = Carnivore(external, internal, Action.carnivoreAction(action, velocity), LearningInfo(Carnivore.learningInterval, subWorld, this, action))
//      Qlearning.carnivoreLearn(this, carb)
      world.remove(this)
      world.add(carb)
    }
    learningInfo = learningInfo.decriment
    world
  }
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
