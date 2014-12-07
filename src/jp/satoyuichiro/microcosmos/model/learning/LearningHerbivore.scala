package jp.satoyuichiro.microcosmos.model.learning

import jp.satoyuichiro.microcosmos.model.World
import jp.satoyuichiro.microcosmos.model.bio.LearningInfo
import jp.satoyuichiro.microcosmos.model.bio.Coordinates
import jp.satoyuichiro.microcosmos.model.bio.Appearance
import jp.satoyuichiro.microcosmos.model.bio.Velocity
import java.awt.Color
import jp.satoyuichiro.microcosmos.model.bio.External
import jp.satoyuichiro.microcosmos.model.bio.Internal
import jp.satoyuichiro.microcosmos.model.bio.Herbivore

class LearningHerbivore(override val external: External, override val internal: Internal, override val velocity: Velocity, var learningInfoo: LearningInfo)
  extends Herbivore(external, internal, velocity, learningInfoo) {

   override def chooseAction(world: World): World = {
    if (learningInfoo.count < 0) {
      val subWorld = world.getSubWorldAround(this, 40, 40)
      val action = Qlearning.herbivoreAction(subWorld, this)
      val herb = new LearningHerbivore(external, internal, Action.herbivoreAction(action, velocity), LearningInfo(Herbivore.learningInterval, subWorld, this, action))
      Qlearning.herbivoreLearn(this, herb)
      world.remove(this)
      world.add(herb)
    }
    learningInfoo = learningInfoo.decriment
    world
  }

}

object LearningHerbivore {
    
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
    new Herbivore(External(coordinates, appearance), Internal(initLife, 10, 10), velocity, learningInfo)
  }

  def empty: Herbivore = {
    val external = External(Coordinates(0,0,0), Appearance(0, Color.RED))
    val internal = Internal(initLife, 0, 0)
    val velocity = Velocity(0, 0)
    val learningInfo0 = LearningInfo(learningInterval, World.empty, null, Action.maxValue)
    val animal = new LearningHerbivore(external, internal, velocity, learningInfo0)
    val learningInfo1 = LearningInfo(learningInterval, World.empty, animal, Action.maxValue)
    new LearningHerbivore(external, internal, velocity,  learningInfo1)
  }
}