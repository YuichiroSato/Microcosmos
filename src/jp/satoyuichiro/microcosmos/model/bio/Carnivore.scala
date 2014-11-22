package jp.satoyuichiro.microcosmos.model.bio

import jp.satoyuichiro.microcosmos.model.World
import java.awt.Color

case class Carnivore(override val external: External, override val internal: Internal, override val velocity: Velocity) extends Animal(external, internal, velocity) {

  def evolve: Bio = Carnivore(External(move, external.appearance), Internal(internal.life - 1, internal.water, internal.mineral), changeVelocity)
  
  def interact(world: World): World = {
    val x = external.coordinates.x
    val y = external.coordinates.y
    val w = external.appearance.size
    val subWorld = world.getSubWorld(x - w, y - w, w * 2, w * 2)
    val herbs = subWorld.getBios.filter(_.isInstanceOf[Herbivore])
    if (0 < herbs.size) {
      val dist = herbs map (h => (h, distance(h)))
      val eatingTarget = (dist minBy(d => d._2))._1.asInstanceOf[Herbivore]
      world.removeHerbivore(eatingTarget)
      world.removeCarnivore(this)
      world.addCarnivore(Carnivore(external, Internal(internal.life + 50, internal.water, internal.mineral), velocity))
    }
    else 
      world
  }
 
  def isDead: Boolean = internal.life <= 0
  
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
    Carnivore(External(coordinates, appearance), Internal(100, 10, 10), velocity)
  }
}