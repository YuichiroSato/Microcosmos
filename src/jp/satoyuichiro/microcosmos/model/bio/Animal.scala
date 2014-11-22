package jp.satoyuichiro.microcosmos.model.bio

abstract class Animal(override val external: External, override val internal: Internal, val velocity: Velocity) extends Bio(external, internal){

  def move: Coordinates = {
    val x = external.coordinates.x + velocity.speed * Math.cos(external.coordinates.angle)
    val y = external.coordinates.y + velocity.speed * Math.sin(external.coordinates.angle)
    val angle = external.coordinates.angle + velocity.rotation
    Coordinates(x.toInt, y.toInt, angle)
  }
  
  def propel(dv: Double, dtheta: Double): Velocity = Velocity(velocity.speed + dv, velocity.rotation + dtheta)
  
  def distance(bio: Bio): Double = {
    val dx = bio.external.coordinates.x - this.external.coordinates.x
    val dy = bio.external.coordinates.y - this.external.coordinates.y
    Math.sqrt(dx * dx + dy * dy)
  }
}

case class Velocity(val speed: Double, val rotation: Double)