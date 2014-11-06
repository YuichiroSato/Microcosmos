package jp.satoyuichiro.microcosmos.model.bio

import jp.satoyuichiro.microcosmos.model.World
import java.awt.Color

case class Herbivore(override val coordinates: Coordinates, override val appearance: Appearance, override val velocity: Velocity) extends Animal(coordinates, appearance, velocity) {

  def update(world: World): Bio = Herbivore(move, appearance, velocity)
  
//  def move: Animal = {
//    val newCoordinates = Coordinates((coordinates.x + velocity.vx).toInt, (coordinates.y + velocity.vy).toInt, coordinates.angle)
//    Herbivore(newCoordinates, appearance, velocity)
//  }
}

object Herbivore {
  
  def apply(x: Int, y: Int): Herbivore = {
    val coordinates = Coordinates(x,y, 0.0)
    val appearance = Appearance(10, Color.BLUE)
    val velocity = Velocity(10 * Math.random(), 0.1)
    Herbivore(coordinates, appearance, velocity)
  }
}