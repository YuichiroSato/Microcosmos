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
import jp.satoyuichiro.microcosmos.model.bio.External

case class World(var cells: Array[Array[Cell]], var plants: List[Plant], var carnivores: List[Carnivore], var herbivores: List[Herbivore], val width: Int, val height: Int) {

  def update: World = {
    val evolved = (plants map (_.evolve.asInstanceOf[Plant])) ++ (carnivores map (_.evolve.asInstanceOf[Carnivore])) ++ (herbivores map (_.evolve.asInstanceOf[Herbivore]))
    var world = World(evolved map applyBoundaryCondition, width, height)
    world.getBios foreach {
      bio => world = bio.interact(world)
    }
    World(world.getBios filter (!_.isDead) map applyBoundaryCondition, width, height)
  }
  
  def isEnd: Boolean = plants.size == 0 || carnivores.size == 0 || herbivores.size == 0

  def getBios: List[Bio] = {
    cells.toList flatMap (_.toList flatMap (_.bios))
  }

  def applyBoundaryCondition(bio: Bio): Bio = {
    bio match {
      case plant: Plant => plant
      case carn: Carnivore => Carnivore(External(boundaryCondition(carn.external.coordinates), carn.external.appearance), carn.internal, carn.velocity, carn.learningInfo)
      case herb: Herbivore => Herbivore(External(boundaryCondition(herb.external.coordinates), herb.external.appearance), herb.internal, herb.velocity, herb.learningInfo)
    }
  }

  def boundaryCondition(coordinates: Coordinates): Coordinates = {
    var x = coordinates.x
    var y = coordinates.y

    if (x < 0) x += width - 1
    if (y < 0) y += height - 1
    if (width <= x) x -= width + 1
    if (height <= y) y -= height + 1

    Coordinates(x, y, coordinates.angle)
  }

  def add(bio: Bio): World = {
    val x = bio.external.coordinates.x
    val y = bio.external.coordinates.y
    val xy = boundaryCondition(Coordinates(x, y, 0.0))
    if (0 <= x && 0 <= y && x < width && y < height) {
      cells(xy.x)(xy.y) = Cell(cells(xy.x)(xy.y).materials, bio :: cells(xy.x)(xy.y).bios)
      append(bio)
    }
    this
  }
  
  def append(bio: Bio): Unit = {
    bio match {
      case plant: Plant => plants ::= plant
      case herb: Herbivore => herbivores ::= herb
      case carn: Carnivore => carnivores ::= carn
    }
  }

  def remove(bio: Bio): World = {
    val x = bio.external.coordinates.x
    val y = bio.external.coordinates.y
    if (0 <= x && 0 <= y && x < width && y < height) {
      cells(x)(y) = cells(x)(y).remove(bio)
      deppend(bio)
    }
    this
  }
  
  def deppend(bio: Bio): Unit = {
    bio match {
      case plant: Plant => plants = removeABio(plant, plants)
      case herb: Herbivore => herbivores = removeABio(herb, herbivores)
      case carn: Carnivore => carnivores = removeABio(carn, carnivores)
    }
  }
  
  def removeABio[A <: Bio](bio: A, bios: List[A]): List[A] = {
    val i = bios.indexOf(bio)
    bios.take(i) ++ bios.drop(i + 1)
  }
  
  def getSubWorld(x: Int, y: Int, w: Int, h: Int): World = {
    var subCells = Array.fill(w)(Array.fill(h)(Cell.empty))
    for (i <- 0 to w - 1) {
      for (j <- 0 to h - 1) {
        if (0 <= x && 0 <= y && x + i < width && y + j < height) subCells(i)(j) = cells(x + i)(y + j)
      }
    }
    World(subCells, List.empty[Plant], List.empty[Carnivore], List.empty[Herbivore], w, h)
  }
  
  def getSubWorldAround(bio: Bio, w: Int, h: Int): World = {
    val x = bio.external.coordinates.x
    val y = bio.external.coordinates.y
    getSubWorld(x, y, w, h)
  }
}

object World {

  def apply(bios: List[Bio], width: Int, height: Int): World = {
    val materials = (new Water(), new Mineral())
    var cells = Array.fill(width)(Array.fill(height)(Cell.empty))
    var plants = List.empty[Plant]
    var carns = List.empty[Carnivore]
    var herbs = List.empty[Herbivore]
    bios foreach {
      bio =>
        val x = bio.external.coordinates.x
        val y = bio.external.coordinates.y
        if (-1 < x && -1 < y && x < width && y < height) {
          cells(x)(y) = Cell(materials, bio :: cells(x)(y).bios)
          bio match {
            case plant: Plant => plants ::= plant
            case herb: Herbivore => herbs ::= herb
            case carn: Carnivore => carns ::= carn
          }
        }
    }
    World(cells, plants, carns, herbs, width, height)
  }

  def init(width: Int, height: Int): World = {
    var cells = Array.fill(width)(Array.fill(height)(Cell.empty))
    for (i <- 0 to width - 1) {
      for (j <- 0 to height - 1) {
        cells(i)(j) = Cell.init(i, j)
      }
    }
    val tem = World(cells, List.empty[Plant], List.empty[Carnivore], List.empty[Herbivore], width, height)
    val bios = tem.getBios
    val plants = bios filter (_.isInstanceOf[Plant]) map (_.asInstanceOf[Plant])
    val carns = bios filter (_.isInstanceOf[Carnivore]) map (_.asInstanceOf[Carnivore])
    val herbs = bios filter (_.isInstanceOf[Herbivore]) map (_.asInstanceOf[Herbivore])
    World(tem.cells, plants, carns, herbs, width, height)
  }
  
  def empty: World = {
    World(List.empty[Bio],1,1)
  }
}

case class Cell(val materials: Tuple2[Water, Mineral], val bios: List[Bio]) {

  def remove(bio: Bio): Cell = Cell(materials, bios filter (_ != bio))
}

object Cell {

  def init(x: Int, y: Int): Cell = {
    val bios = Math.random match {
      case i if i < 0.0003 => List(Plant(x, y))
      case i if 0.0003 < i && i < 0.00031 => List(Carnivore(x, y))
      case i if 0.00031 < i && i < 0.00033 => List(Herbivore(x, y))
      case _ => List.empty[Bio]
    }
    Cell((new Water(), new Mineral()), bios)
  }

  def empty: Cell = {
    Cell((new Water(), new Mineral()), List.empty[Bio])
  }
}