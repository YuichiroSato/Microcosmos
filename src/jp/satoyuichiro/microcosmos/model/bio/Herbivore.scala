package jp.satoyuichiro.microcosmos.model.bio

import jp.satoyuichiro.microcosmos.model.World
import java.awt.Color
import jp.satoyuichiro.microcosmos.model.learning.Action
import jp.satoyuichiro.microcosmos.model.learning.Qlearning

case class Herbivore(override val external: External, override val internal: Internal, override val velocity: Velocity, var learningInfo: LearningInfo)
  extends Animal(external, internal, velocity) {

  def evolve: Bio = Herbivore(External(move, external.appearance), Internal(internal.life - 1, internal.water, internal.mineral), velocity, learningInfo)
  
  def interact(world: World): World = chooseAction(giveBirthHerbivore(eatPlant(world)))
  
  val filterf = (bio: Bio) => bio.isInstanceOf[Plant]
  val updatef = () => Herbivore(external, Internal(internal.life + Herbivore.lifeUp, internal.water, internal.mineral), velocity, learningInfo)
  
  def eatPlant(world: World): World = {
    eat(world, filterf, updatef)
  }

  val condition = () => Herbivore.giveBirthLife < internal.life
  val born = () => Herbivore(external.coordinates.x, external.coordinates.y)
  val update2 = () => Herbivore(external, Internal(internal.life - Herbivore.giveBirthCost, internal.water, internal.mineral), velocity, learningInfo)
  
  def giveBirthHerbivore(world: World) = {
    giveBirth(world, condition, born, update2)
  }
  
  def isDead: Boolean = internal.life <= 0
  
  def chooseAction(world: World): World = {
    if (learningInfo.count < 0) {
      Qlearning.herbivoreLearn(this)
      val subWorld = world.getSubWorldAround(this, 40, 40)
      val herb = Herbivore(external, internal, Qlearning.herbivoreAction(null, velocity), LearningInfo(Herbivore.learningInterval, subWorld, velocity))
      world.remove(this)
      world.add(herb)
    }
    learningInfo = learningInfo.decriment
    world
  }
}

object Herbivore {
    
  val lifeUp = 200
  val giveBirthLife = 4000
  val initLife = 100
  val giveBirthCost = 2000
  val learningInterval = 20
  
  def apply(x: Int, y: Int): Herbivore = {
    val coordinates = Coordinates(x,y, Math.random)
    val appearance = Appearance(12, Color.BLUE)
    val velocity = Velocity(10 * Math.random(), Math.random() - 0.5)
    Herbivore(External(coordinates, appearance), Internal(initLife, 10, 10), velocity, LearningInfo(learningInterval, World.empty, velocity))
  }
}