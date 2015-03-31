package jp.satoyuichiro.microcosmos.model.bio

import java.awt.Color
import jp.satoyuichiro.microcosmos.model.World

import scalaz._
import Scalaz._

abstract class Bio(val external: External, val internal: Internal) {

  def evolve: Bio
  def interact(world: World): World
  def isDead: Boolean
}

case class External(val coordinates: Coordinates, val appearance: Appearance) {
  
  def setAngle(a: Double): External = copy(coordinates.setAngle(a))
}

object External {
  
  def herbivore(c: Coordinates): External = External(c, Appearance(12, Color.BLUE))
  
  val ccc = Lens.lensu[External, Coordinates]((ex, co) => ex.copy(coordinates = co), _.coordinates)
  val coordinateX = ccc >=> Coordinates.X
  val coordinateY = ccc >=> Coordinates.Y
}

case class Appearance(val size: Int, val color: Color)
case class Coordinates(val x: Int, val y: Int, val angle: Double) {
  
  def setAngle(a: Double): Coordinates = copy(angle = a)
  def distance(c: Coordinates): Double = Math.sqrt(x * c.x + y * c.y)
  def distance2(c: Coordinates): Double = x * c.x + y * c.y
}

object Coordinates {
  
  val X = Lens.lensu[Coordinates, Int]((c, x1) => c.copy(x = x1), _.x)
  val Y = Lens.lensu[Coordinates, Int]((c, y1) => c.copy(y = y1), _.y)
  val Angle = Lens.lensu[Coordinates, Double]((c, a) => c.copy(angle = a), _.angle)
}
case class Internal(val life: Int, val water: Int, val mineral: Int)