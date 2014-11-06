package jp.satoyuichiro.microcosmos.model.bio

import jp.satoyuichiro.microcosmos.model.World
import java.awt.Color

case class Carnivore(override val coordinates: Coordinates, override val appearance: Appearance, override val velocity: Velocity) extends Animal(coordinates, appearance, velocity) {

  def update(world: World): Bio = Carnivore(move, appearance, changeVelocity)
  
  def changeVelocity: Velocity = {
    if (Math.random() < 0.1) {
      propel(10 * Math.random() - 5,  Math.random() - 0.5)
    } else if (Math.random() < 0.05) {
      Velocity(velocity.speed + 10 * Math.random() - 5, 0.0)
    } else {
      velocity
    }
  }
}

object Carnivore {
  
  def apply(x: Int, y: Int): Carnivore = {
    val coordinates = Coordinates(x,y, 0.0)
    val appearance = Appearance(12, Color.RED)
    val velocity = Velocity(10 * Math.random(), 0.1)
    Carnivore(coordinates, appearance, velocity)
  }
}