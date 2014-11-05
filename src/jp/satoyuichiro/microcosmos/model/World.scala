package jp.satoyuichiro.microcosmos.model

import jp.satoyuichiro.microcosmos.model.material.Water
import jp.satoyuichiro.microcosmos.model.material.Mineral
import jp.satoyuichiro.microcosmos.model.bio.Bio
import jp.satoyuichiro.microcosmos.model.bio.Plant
import jp.satoyuichiro.microcosmos.model.bio.Carnivore
import jp.satoyuichiro.microcosmos.model.bio.Herbivore

case class World(val cells: Array[Array[Cell]]) {

  def update: World = {
    World(cells)
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

case class Cell(val materials: Tuple2[Water, Mineral], val bios: List[Bio])

object Cell {
  
  def init(x: Int, y: Int): Cell = {
    val bios = Math.random match {
      case i if i < 0.001 => List(new Plant(x,y))
      case i if 0.001 < i && i < 0.0012 => List(new Carnivore(x,y))
      case i if 0.0012 < i && i < 0.0017 => List(new Herbivore(x,y))
      case _ => List.empty[Bio]
    }
    Cell((new Water(), new Mineral()), bios)
  }
}