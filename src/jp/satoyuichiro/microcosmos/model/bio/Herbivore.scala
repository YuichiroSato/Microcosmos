package jp.satoyuichiro.microcosmos.model.bio

import jp.satoyuichiro.microcosmos.model.World
import java.awt.Color

case class Herbivore(override val external: External, override val internal: Internal, override val velocity: Velocity) extends Animal(external, internal, velocity) {

  def evolve: Bio = Herbivore(External(move, external.appearance), Internal(internal.life - 1, internal.water, internal.mineral), changeVelocity)
  
  def interact(world: World): World = {
    val x = external.coordinates.x
    val y = external.coordinates.y
    val w = external.appearance.size 
    val subWorld = world.getSubWorld(x - w, y - w, w * 2, w * 2)
    val plants = subWorld.getBios.filter(_.isInstanceOf[Plant])
    if (0 < plants.size) {
      val dist = plants map (h => (h, distance(h)))
      val eatingTarget = (dist minBy(d => d._2))._1.asInstanceOf[Plant]
      world.removePlant(eatingTarget)
    }
    else 
      world
  }
  
  def isDead: Boolean = internal.life <= 0
  
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
    Herbivore(External(coordinates, appearance), Internal(100, 10, 10), velocity)
  }
}