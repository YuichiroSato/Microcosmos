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

case class World(val cells: Array[Array[Cell]], val width: Int, val height: Int) {

  def update: World = {
    val bios = getBios map (_.update(this))
    World(bios, width, height)
  }
  
  def getBios: List[Bio] = {
    cells.toList flatMap (_.toList flatMap (_.bios))
  }
}

object World {
  
  def apply(bios: List[Bio], width: Int, height: Int): World = {
    val materials = (new Water(), new Mineral())
    var cells = Array.fill(width)(Array.fill(height)(null:Cell))
    for (i <- 0 to width - 1) {
      for (j <- 0 to height - 1) {
        cells(i)(j) = Cell(materials, bios filter(bio => bio.coordinates.x == i && bio.coordinates.y == j))
      }
    }
    World(cells, width, height)
  }
  
  def init(width: Int, height: Int): World = {
    var cells = Array.fill(width)(Array.fill(height)(null:Cell))
    for (i <- 0 to width - 1) {
      for (j <- 0 to height - 1) {
        cells(i)(j) = Cell.init(i, j)
      }
    }
    World(cells, width, height)
  }
}

case class Cell(val materials: Tuple2[Water, Mineral], val bios: List[Bio]) {
  
}

object Cell {
  
  def init(x: Int, y: Int): Cell = {
    val bios = Math.random match {
      case i if i < 0.001 => List(new Plant(Coordinates(x,y, 0.0), Appearance(10, Color.GREEN)))
      case i if 0.001 < i && i < 0.0012 => List(Carnivore(x,y))
      case i if 0.0012 < i && i < 0.0017 => List(Herbivore(x,y))
      case _ => List.empty[Bio]
    }
    Cell((new Water(), new Mineral()), bios)
  }
}