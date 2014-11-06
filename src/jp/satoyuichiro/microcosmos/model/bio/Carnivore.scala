package jp.satoyuichiro.microcosmos.model.bio

import jp.satoyuichiro.microcosmos.model.World
import java.awt.Color

case class Carnivore(override val coordinates: Coordinates, override val appearance: Appearance, override val velocity: Velocity) extends Animal(coordinates, appearance, velocity) {

  def update(world: World): Bio = Carnivore(move, appearance, velocity)
}

object Carnivore {
  
  def apply(x: Int, y: Int): Carnivore = {
    val coordinates = Coordinates(x,y, 0.0)
    val appearance = Appearance(12, Color.RED)
    val velocity = Velocity(10 * Math.random(), 0.1)
    Carnivore(coordinates, appearance, velocity)
  }
}