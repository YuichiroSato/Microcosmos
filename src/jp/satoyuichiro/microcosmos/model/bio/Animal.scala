package jp.satoyuichiro.microcosmos.model.bio

import jp.satoyuichiro.microcosmos.model.World

import scalaz._
import Scalaz._

abstract class Animal(override val external: External, override val internal: Internal, val velocity: Velocity, val learningInfo: LearningInfo) extends Bio(external, internal){

  def move: Coordinates = {
    val x = external.coordinates.x + velocity.speed * Math.cos(external.coordinates.angle)
    val y = external.coordinates.y + velocity.speed * Math.sin(external.coordinates.angle)
    val angle = (external.coordinates.angle + velocity.rotation) % (2 * Math.PI)
    Coordinates(x.toInt, y.toInt, angle)
  }
  
  def propel(dv: Double, dtheta: Double): Velocity = Velocity(velocity.speed + dv, velocity.rotation + dtheta)
  
  def distance(bio: Bio): Double = {
    val dx = bio.external.coordinates.x - this.external.coordinates.x
    val dy = bio.external.coordinates.y - this.external.coordinates.y
    Math.sqrt(dx * dx + dy * dy)
  }
  
  def eat(world: World, filterf: Bio => Boolean, update: () => Bio): World = {
    val x = external.coordinates.x
    val y = external.coordinates.y
    val w = external.appearance.size
    val subWorld = world.getSubWorld(x - w, y - w, w * 2, w * 2)
    val bios = subWorld.getBios filter filterf
    if (0 < bios.size) {
      val dist = bios map (h => (h, distance(h)))
      val eatingTarget = (dist minBy (d => d._2))._1
      world.remove(eatingTarget)
      world.updateBio(this, update())
    } else {
      world
    }
  }
  
  def giveBirth(world: World, condition: () => Boolean, born: () => Bio, update: () => Bio): World = {
    if (condition()) {
      val bio = born()
      world.add(bio)
      world.updateBio(this, update())
    } else
      world
  }
}

case class Velocity(val speed: Double, val rotation: Double) {
  
  def speedUp(d: Double): Velocity = copy(speed = speed + d)
  def rotate(r: Double): Velocity = copy(rotation = rotation + r)
}
case class LearningInfo(val count: Int, val subWorld: World, val animal: Animal, val action: Int, val makeBorn: Int, val learning: Boolean = false) {
  
  def nextLearningInfo(c: Int, sub: World, an: Animal, ac: Int): LearningInfo = copy(count = c, subWorld = sub, animal = an, action = ac)
  def incrementMakeBorn: LearningInfo = copy(makeBorn = this.makeBorn + 1)
  def decriment: LearningInfo = copy(count = count - 1)
  def setLearningTrue: LearningInfo = copy(learning = true)
}