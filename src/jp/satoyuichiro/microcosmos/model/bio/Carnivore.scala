package jp.satoyuichiro.microcosmos.model.bio

import jp.satoyuichiro.microcosmos.model.World
import java.awt.Color
import jp.satoyuichiro.microcosmos.model.learning.Action
import jp.satoyuichiro.microcosmos.model.learning.Qlearning

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
      Qlearning.carnivoreLearn(this)
      val subWorld = world.getSubWorldAround(this, 40, 40)
      val carb = Carnivore(external, internal, Qlearning.carnivoreAction(subWorld, velocity), LearningInfo(Carnivore.learningInterval, subWorld, velocity))
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
  val learningInterval = 20

  def apply(x: Int, y: Int): Carnivore = {
    val coordinates = Coordinates(x, y, Math.random())
    val appearance = Appearance(12, Color.RED)
    val velocity = Velocity(5 * Math.random(), Math.random() - 0.5)
    Carnivore(External(coordinates, appearance), Internal(initLife, 10, 10), velocity, LearningInfo(learningInterval, World.empty, velocity))
  }
}
