package jp.satoyuichiro.microcosmos.model

import jp.satoyuichiro.microcosmos.model.material.Water
import jp.satoyuichiro.microcosmos.model.material.Mineral
import jp.satoyuichiro.microcosmos.model.bio.Bio
import jp.satoyuichiro.microcosmos.model.bio.Plant
import jp.satoyuichiro.microcosmos.model.bio.Carnivore
import jp.satoyuichiro.microcosmos.model.bio.Herbivore
import jp.satoyuichiro.microcosmos.model.bio.Coordinates
import jp.satoyuichiro.microcosmos.model.bio.Appearance
import java.awt.Color

case class World(val cells: Array[Array[Cell]]) {

  def update: World = {
    val x = cells.size
    val y = cells.head.size
    var newCells = Array.fill(x)(Array.fill(y)(null:Cell))
    for (i <- 0 to x - 1) {
      for (j <- 0 to y - 1) {
        newCells(i)(j) = cells(i)(j).update
      }
    }
    World(newCells)
  }
}

object World {
  
  def init(x: Int, y: Int): World = {
    var cells = Array.fill(x)(Array.fill(y)(null:Cell))
    for (i <- 0 to x - 1) {
      for (j <- 0 to y - 1) {
        cells(i)(j) = Cell.init(i, j)
      }
    }
    World(cells)
  }
}

case class Cell(val materials: Tuple2[Water, Mineral], val bios: List[Bio]) {
  
  def update: Cell = {
    Cell(materials, bios map (_.update))
  }
}

object Cell {
  
  def init(x: Int, y: Int): Cell = {
    val bios = Math.random match {
      case i if i < 0.001 => List(new Plant(Coordinates(x,y, 0.0), Appearance(10, Color.GREEN)))
      case i if 0.001 < i && i < 0.0012 => List(new Carnivore(Coordinates(x,y,0.0), Appearance(10, Color.RED)))
      case i if 0.0012 < i && i < 0.0017 => List(new Herbivore(Coordinates(x,y,0.0), Appearance(10, Color.BLUE)))
      case _ => List.empty[Bio]
    }
    Cell((new Water(), new Mineral()), bios)
  }
}