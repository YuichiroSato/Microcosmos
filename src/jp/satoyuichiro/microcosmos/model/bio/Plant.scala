package jp.satoyuichiro.microcosmos.model.bio

import jp.satoyuichiro.microcosmos.model.World
import java.awt.Color
import jp.satoyuichiro.microcosmos.model.Cell

case class Plant(override val external: External, override val internal: Internal) extends Bio(external, internal) {

  def evolve: Bio = this
  
  def interact(world: World): World = {
    if (Math.random() < 0.001) {
      var cells = world.cells
      val x = (external.coordinates.x + 30 * Math.random() - 15).toInt 
      val y = (external.coordinates.y + 30 * Math.random() - 15).toInt
      val xy = world.boundaryCondition(Coordinates(x,y,0.0))
      val plant = Plant(xy.x, xy.y)
//      val addedBios = Plant(xy.x, xy.y) :: cells(xy.x)(xy.y).bios
//      cells(xy.x)(xy.y) = Cell(cells(xy.x)(xy.y).materials, addedBios)
//      World(cells, world.width, world.height)
      world.addPlant(plant)
    }
    else {
      world
    }
  }
  
  def isDead: Boolean = internal.life <= 0
  
}

object Plant {
  
  def apply(x: Int, y: Int): Plant = {
    new Plant(External(Coordinates(x,y, 0.0), Appearance(10, Color.GREEN)), Internal(100, 10, 10))
  }
  
}

 