package jp.satoyuichiro.microcosmos.model.bio

abstract class Animal(override val coordinates: Coordinates, override val appearance: Appearance, val velocity: Velocity) extends Bio(coordinates, appearance){

  def move: Coordinates = {
    val x = coordinates.x + velocity.speed * Math.cos(coordinates.angle)
    val y = coordinates.y + velocity.speed * Math.sin(coordinates.angle)
    val angle = coordinates.angle + velocity.rotation
    Coordinates(x.toInt, y.toInt, angle)
  }
}

case class Velocity(val speed: Double, val rotation: Double)