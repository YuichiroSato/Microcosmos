package jp.satoyuichiro.microcosmos.model.bio

import jp.satoyuichiro.microcosmos.model.World
import java.awt.Color
import jp.satoyuichiro.microcosmos.model.learning.Action

case class Herbivore(override val external: External, override val internal: Internal, override val velocity: Velocity) extends Animal(external, internal, velocity) {

  def evolve: Bio = Herbivore(External(move, external.appearance), Internal(internal.life - 1, internal.water, internal.mineral), changeVelocity)
  
  def interact(world: World): World = giveBirthHerbivore(eatPlant(world))
  
  val filterf = (bio: Bio) => bio.isInstanceOf[Plant]
  val updatef = () => Herbivore(external, Internal(internal.life + Herbivore.lifeUp, internal.water, internal.mineral), velocity)
  
  def eatPlant(world: World): World = {
    eat(world, filterf, updatef)
  }

  val condition = () => Herbivore.giveBirthLife < internal.life
  val born = () => Herbivore(external.coordinates.x, external.coordinates.y)
  val update2 = () => Herbivore(external, Internal(internal.life - Herbivore.giveBirthCost, internal.water, internal.mineral), velocity)
  
  def giveBirthHerbivore(world: World) = {
    giveBirth(world, condition, born, update2)
  }
  
  def isDead: Boolean = internal.life <= 0
  
  def changeVelocity: Velocity = {
    if (Math.random() < 0.05) {
      Action.herbivoreAction((2 * Math.random()).toInt, velocity)
    }
    else if (Math.random() < 0.1) {
      Action.herbivoreAction((2 * Math.random()).toInt + 2, velocity)
    }
    else if (Math.random() < 0.1) {
      Action.herbivoreAction(5, velocity)
    }
    else
      Action.herbivoreAction(7, velocity)
  }
}

object Herbivore {
    
  val lifeUp = 200
  val giveBirthLife = 4000
  val initLife = 100
  val giveBirthCost = 2000
  
  def apply(x: Int, y: Int): Herbivore = {
    val coordinates = Coordinates(x,y, Math.random)
    val appearance = Appearance(12, Color.BLUE)
    val velocity = Velocity(10 * Math.random(), Math.random() - 0.5)
    Herbivore(External(coordinates, appearance), Internal(initLife, 10, 10), velocity)
  }
}