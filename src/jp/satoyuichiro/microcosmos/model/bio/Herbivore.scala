package jp.satoyuichiro.microcosmos.model.bio

import jp.satoyuichiro.microcosmos.model.World
import java.awt.Color

case class Herbivore(override val external: External, override val internal: Internal, override val velocity: Velocity) extends Animal(external, internal, velocity) {

  def evolve: Bio = Herbivore(External(move, external.appearance), internal, changeVelocity)
  
  def interact(world: World): World = world
  
  def isDead: Boolean = false
  
  def changeVelocity: Velocity = {
    if (Math.random() < 0.05) {
      propel(10 * Math.random() - 5, Math.random() - 0.5)
    } else if (Math.random() < 0.05) {
      Velocity(velocity.speed + 10 * Math.random() - 5, 0.0)
    } else {
      velocity
    }
  }
}

object Herbivore {
  
  def apply(x: Int, y: Int): Herbivore = {
    val coordinates = Coordinates(x,y, 0.0)
    val appearance = Appearance(12, Color.BLUE)
    val velocity = Velocity(10 * Math.random(), 0.1)
    Herbivore(External(coordinates, appearance), null, velocity)
  }
}