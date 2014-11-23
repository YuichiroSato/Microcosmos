package jp.satoyuichiro.microcosmos.model.bio

import jp.satoyuichiro.microcosmos.model.World
import java.awt.Color
import jp.satoyuichiro.microcosmos.model.Cell

case class Plant(override val external: External, override val internal: Internal) extends Bio(external, internal) {

  def evolve: Bio = this
  
  def interact(world: World): World = {
    if (Math.random() < 0.01) {
      if (Math.random() < 0.2) {
        val x = (external.coordinates.x + 200 * Math.random()).toInt 
        val y = (external.coordinates.y + 200 * Math.random()).toInt
        giveBirthPlant(world, x, y)
      }
      else {
        val x = (external.coordinates.x + 50 * Math.random() - 25).toInt 
        val y = (external.coordinates.y + 50 * Math.random() - 25).toInt
        giveBirthPlant(world, x, y)
      }
    }
    else {
      world
    }
  }
  
  def giveBirthPlant(world: World, x: Int, y: Int): World = {
    val xy = world.boundaryCondition(Coordinates(x,y,0.0))
    world.add(Plant(xy.x, xy.y))
  }
  
  def isDead: Boolean = internal.life <= 0
  
}

object Plant {
  
  def apply(x: Int, y: Int): Plant = {
    new Plant(External(Coordinates(x,y, 0.0), Appearance(10, Color.GREEN)), Internal(100, 10, 10))
  }
  
}

 