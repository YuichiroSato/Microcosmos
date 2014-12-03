package jp.satoyuichiro.microcosmos.model.learning

import jp.satoyuichiro.microcosmos.model.World
import jp.satoyuichiro.microcosmos.model.Cell
import jp.satoyuichiro.microcosmos.model.bio.Plant
import jp.satoyuichiro.microcosmos.model.bio.Bio
import jp.satoyuichiro.microcosmos.model.material.Water
import jp.satoyuichiro.microcosmos.model.material.Mineral

class LearningWorld(cells: Array[Array[Cell]], plants: List[Plant], carnivores: List[LearningCarnivore], herbivores: List[LearningHerbivore],
    override val width: Int, override val height: Int) extends World(cells, plants, carnivores, herbivores, width, height) {

}

object LearningWorld {
  
//  def apply(bios: List[Bio], width: Int, height: Int): World = {
//    val materials = (new Water(), new Mineral())
//    var cells = Array.fill(width)(Array.fill(height)(Cell.empty))
//    var plants = List.empty[Plant]
//    var carns = List.empty[LearningCarnivore]
//    var herbs = List.empty[LearningHerbivore]
//    bios foreach {
//      bio =>
//        val x = bio.external.coordinates.x
//        val y = bio.external.coordinates.y
//        if (-1 < x && -1 < y && x < width && y < height) {
//          cells(x)(y) = Cell(materials, bio :: cells(x)(y).bios)
//          bio match {
//            case plant: Plant => plants ::= plant
//            case herb: LearningHerbivore => herbs ::= herb
//            case carn: LearningCarnivore => carns ::= carn
//          }
//        }
//    }
//    new LearningWorld(cells, plants, carns, herbs, width, height)
//  }

  def init(width: Int, height: Int): LearningWorld = {
    var cells = Array.fill(width)(Array.fill(height)(Cell.empty))
    for (i <- 0 to width - 1) {
      for (j <- 0 to height - 1) {
        cells(i)(j) = Cellinit(i, j)
      }
    }
    val tem = new LearningWorld(cells, List.empty[Plant], List.empty[LearningCarnivore], List.empty[LearningHerbivore], width, height)
    val bios = tem.getBios
    val plants = bios filter (_.isInstanceOf[Plant]) map (_.asInstanceOf[Plant])
    val carns = bios filter (_.isInstanceOf[LearningCarnivore]) map (_.asInstanceOf[LearningCarnivore])
    val herbs = bios filter (_.isInstanceOf[LearningHerbivore]) map (_.asInstanceOf[LearningHerbivore])
    new LearningWorld(tem.cells, plants, carns, herbs, width, height)
  }
  
  def Cellinit(x: Int, y: Int): Cell = {
    val bios = Math.random match {
      case i if i < 0.0003 => List(Plant(x, y))
      case i if 0.0003 < i && i < 0.00031 => List(LearningCarnivore(x, y))
      case i if 0.00031 < i && i < 0.00033 => List(LearningHerbivore(x, y))
      case _ => List.empty[Bio]
    }
    Cell((new Water(), new Mineral()), bios)
  }

}