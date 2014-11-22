package jp.satoyuichiro.microcosmos.model.bio

import jp.satoyuichiro.microcosmos.model.World
import java.awt.Color

case class Carnivore(override val external: External, override val internal: Internal, override val velocity: Velocity) extends Animal(external, internal, velocity) {

  def evolve: Bio = Carnivore(External(move, external.appearance), Internal(internal.life - 1, internal.water, internal.mineral), changeVelocity)

  def interact(world: World): World = giveBirth(eat(world))

  def eat(world: World): World = {
    val x = external.coordinates.x
    val y = external.coordinates.y
    val w = external.appearance.size
    val subWorld = world.getSubWorld(x - w, y - w, w * 2, w * 2)
    val herbs = subWorld.getBios.filter(_.isInstanceOf[Herbivore])
    if (0 < herbs.size) {
      val dist = herbs map (h => (h, distance(h)))
      val eatingTarget = (dist minBy (d => d._2))._1.asInstanceOf[Herbivore]
      world.removeHerbivore(eatingTarget)
      world.removeCarnivore(this)
      world.addCarnivore(Carnivore(external, Internal(internal.life + Carnivore.lifeUp, internal.water, internal.mineral), velocity))
    } else
      world
  }

  def giveBirth(world: World): World = {
    if (Carnivore.giveBirthLife < internal.life) {
      val born = Carnivore(external.coordinates.x, external.coordinates.y)
      world.addCarnivore(born)
      world.removeCarnivore(this)
      world.addCarnivore(Carnivore(external, Internal(internal.life - Carnivore.giveBirthCost, internal.water, internal.mineral), velocity))
    } else
      world
  }

  def isDead: Boolean = internal.life <= 0

  def changeVelocity: Velocity = {
    if (velocity.speed < 0.5) return Velocity(2 + 8 * Math.random(), velocity.rotation)
    if (Math.random() < 0.1) {
      propel(Math.random(), Math.random() - 0.5)
    } else if (Math.random() < 0.005) {
      Velocity(2 + Math.random() - 0.5, Math.random() - 0.5)
    } else if (Math.random() < 0.5) {
      Velocity(velocity.speed, 0.0)
    } else {
      velocity
    }
  }
}

object Carnivore {

  val lifeUp = 300
  val giveBirthLife = 10000
  val initLife = 500
  val giveBirthCost = 9500

  def apply(x: Int, y: Int): Carnivore = {
    val coordinates = Coordinates(x, y, Math.random())
    val appearance = Appearance(12, Color.RED)
    val velocity = Velocity(5 * Math.random(), Math.random() - 0.5)
    Carnivore(External(coordinates, appearance), Internal(initLife, 10, 10), velocity)
  }
}