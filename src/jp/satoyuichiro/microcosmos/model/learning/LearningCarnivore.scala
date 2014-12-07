package jp.satoyuichiro.microcosmos.model.learning

import jp.satoyuichiro.microcosmos.model.bio.Carnivore
import jp.satoyuichiro.microcosmos.model.World
import jp.satoyuichiro.microcosmos.model.bio.LearningInfo
import jp.satoyuichiro.microcosmos.model.bio.External
import jp.satoyuichiro.microcosmos.model.bio.Internal
import jp.satoyuichiro.microcosmos.model.bio.Velocity
import jp.satoyuichiro.microcosmos.model.bio.Coordinates
import jp.satoyuichiro.microcosmos.model.bio.Appearance
import java.awt.Color

class LearningCarnivore(override val external: External, override val internal: Internal, override val velocity: Velocity, var learningInfoo: LearningInfo)
extends Carnivore(external, internal, velocity, learningInfoo) {

  override def chooseAction(world: World): World = {
    if (learningInfoo.count < 0) {
      val subWorld = world.getSubWorldAround(this, 40, 40)
      val action = Qlearning.carnivoreAction(subWorld, this)
      val carb = new LearningCarnivore(external, internal, Action.carnivoreAction(action, velocity), LearningInfo(Carnivore.learningInterval, subWorld, this, action))
      Qlearning.carnivoreLearn(this, carb)
      world.remove(this)
      world.add(carb)
    }
    learningInfoo = learningInfoo.decriment
    world
  }
}

object LearningCarnivore {

  val lifeUp = 200
  val giveBirthLife = 5000
  val initLife = 500
  val giveBirthCost = 4000
  val learningInterval = 19

  def apply(x: Int, y: Int): LearningCarnivore = {
    val coordinates = Coordinates(x, y, Math.random())
    val appearance = Appearance(12, Color.RED)
    val velocity = Velocity(5 * Math.random(), Math.random() - 0.5)
    val learningInfo = LearningInfo((learningInterval * Math.random()).toInt, World.empty, Carnivore.empty, Action.maxValue)
    new LearningCarnivore(External(coordinates, appearance), Internal(initLife, 10, 10), velocity, learningInfo)
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
