package jp.satoyuichiro.microcosmos.model.bio

import jp.satoyuichiro.microcosmos.model.World
import java.awt.Color

case class Herbivore(override val external: External, override val internal: Internal, override val velocity: Velocity) extends Animal(external, internal, velocity) {

  def evolve: Bio = Herbivore(External(move, external.appearance), Internal(internal.life - 1, internal.water, internal.mineral), changeVelocity)
  
  def interact(world: World): World = giveBirth(eat(world))
  
  def eat(world: World): World = {
    val x = external.coordinates.x
    val y = external.coordinates.y
    val w = external.appearance.size 
    val subWorld = world.getSubWorld(x - w, y - w, w * 2, w * 2)
    val plants = subWorld.getBios.filter(_.isInstanceOf[Plant])
    if (0 < plants.size) {
      val dist = plants map (h => (h, distance(h)))
      val eatingTarget = (dist minBy(d => d._2))._1.asInstanceOf[Plant]
      world.removePlant(eatingTarget)
      world.removeHerbivore(this)
      world.addHerbivore(Herbivore(external, Internal(internal.life + Herbivore.lifeUp, internal.water, internal.mineral), velocity))
    }
    else 
      world
  }
  
  def giveBirth(world: World): World  = {
    if (Herbivore.giveBirthLife < internal.life) {
      val born = Herbivore(external.coordinates.x, external.coordinates.y)
      world.addHerbivore(born)
      world.removeHerbivore(this)
      world.addHerbivore(Herbivore(external, Internal(internal.life - Herbivore.giveBirthCost, internal.water, internal.mineral), velocity))
    }
    else
      world
  }
  
  def isDead: Boolean = internal.life <= 0
  
  def changeVelocity: Velocity = {
    if (velocity.speed < 0.5) return Velocity(2 + 8 * Math.random(), velocity.rotation)
    if (Math.random() < 0.1) {
      propel(Math.random() - 0.5, 0.0)
    } else if (Math.random() < 0.05) {
      Velocity(2 * Math.random() - 0.7, Math.random() - 0.5)
    } else if (Math.random() < 0.5) {
      Velocity(velocity.speed, 0.0)
    } else {
      velocity
    }
  }
}

object Herbivore {
    
  val lifeUp = 500
  val giveBirthLife = 7000
  val initLife = 100
  val giveBirthCost = 2000
  
  def apply(x: Int, y: Int): Herbivore = {
    val coordinates = Coordinates(x,y, Math.random)
    val appearance = Appearance(12, Color.BLUE)
    val velocity = Velocity(10 * Math.random(), Math.random() - 0.5)
    Herbivore(External(coordinates, appearance), Internal(initLife, 10, 10), velocity)
  }
}