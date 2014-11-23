package jp.satoyuichiro.microcosmos.model.bio

import jp.satoyuichiro.microcosmos.model.World
import java.awt.Color

case class Carnivore(override val external: External, override val internal: Internal, override val velocity: Velocity) extends Animal(external, internal, velocity) {

  def evolve: Bio = Carnivore(External(move, external.appearance), Internal(internal.life - 1, internal.water, internal.mineral), changeVelocity)

  def interact(world: World): World = giveBirthCarnivore(eatHervibore(world))

  val filterf = (bio: Bio) => bio.isInstanceOf[Herbivore]
  val updatef = () => Carnivore(external, Internal(internal.life + Carnivore.lifeUp, internal.water, internal.mineral), velocity)
  
  def eatHervibore(world: World): World = {
    eat(world, filterf, updatef)
  }

  val condition = () => Carnivore.giveBirthLife < internal.life
  val born = () => Carnivore(external.coordinates.x, external.coordinates.y)
  val update2 = () => Carnivore(external, Internal(internal.life - Carnivore.giveBirthCost, internal.water, internal.mineral), velocity)
  
  def giveBirthCarnivore(world: World) = {
    giveBirth(world, condition, born, update2)
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