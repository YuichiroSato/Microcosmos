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

case class World(var cells: Array[Array[Cell]], var plants: List[Plant], var carnivores: List[Carnivore], var herbivores: List[Herbivore],
    val width: Int, val height: Int) {

  def update: World = evolve.interact
  
  def evolve: World = {
    val evolved = (plants map (_.evolve.asInstanceOf[Plant])) ++
      (carnivores map (_.evolve.asInstanceOf[Carnivore])) ++
      (herbivores map (_.evolve.asInstanceOf[Herbivore]))
    World(evolved map applyBoundaryCondition, width, height)
  }
  
  def interact: World = {
    var world = this
    for (bio <- getBios) {
      world = bio.interact(world)
    }
    World(world.getBios filter (!_.isDead) map applyBoundaryCondition, width, height)
  }
  
  def isEnd: Boolean = plants.size == 0 || carnivores.size == 0 || herbivores.size == 0

  def getBios: List[Bio] = cells.toList flatMap (_.toList flatMap (_.bios))

  def applyBoundaryCondition(bio: Bio): Bio = {
    bio match {
      case plant: Plant => plant
      case carn: Carnivore => carn.setExternal(boundaryCondition(carn.external.coordinates), carn.external.appearance)
      case herb: Herbivore => herb.setExternal(boundaryCondition(herb.external.coordinates), herb.external.appearance)
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
  
  def updateBio(from: Bio, to: Bio): World = if (plants.contains(from) || carnivores.contains(from) || herbivores.contains(from)) remove(from).add(to) else this
  
  def deppend(bio: Bio): Unit = {
    bio match {
      case plant: Plant => plants = World.removeABio(plant, plants)
      case herb: Herbivore => herbivores = World.removeABio(herb, herbivores)
      case carn: Carnivore => carnivores = World.removeABio(carn, carnivores)
    }
  }
  
  def getSubWorld(x: Int, y: Int, w: Int, h: Int): World = {
    var subCells = Array.fill(w)(Array.fill(h)(Cell.empty))
    var subPlants = List.empty[Plant]
    var subCarnivores = List.empty[Carnivore]
    var subHerbivores = List.empty[Herbivore]
    for (i <- 0 to w - 1) {
      for (j <- 0 to h - 1) {
        val xi = x + i
        val yi = y + j
        if (0 <= xi && 0 <= yi && xi < width && yi < height) {
          subCells(i)(j) = cells(xi)(yi)
          cells(xi)(yi).bios foreach {
            bio => bio match {
              case p: Plant => subPlants ::= p
              case c: Carnivore => subCarnivores ::= c
              case h: Herbivore => subHerbivores ::= h
            }
          }
        }
      }
    }
    World(subCells, subPlants, subCarnivores, subHerbivores, w, h)
  }
  
  def getSubWorldAround(bio: Bio, w: Int, h: Int): World = {
    val startx = bio.external.coordinates.x - (w.toDouble / 2.0).floor.toInt
    val starty = bio.external.coordinates.y - (h.toDouble / 2.0).floor.toInt
    val subWorld = World(this.getBios, this.width, this.height)
    subWorld.remove(bio).getSubWorld(startx, starty, w, h)
  }
  
  def getCell(x: Int, y: Int): Option[Cell] = {
    if (-1 < x && x < cells.length && -1 < y && y < cells(0).length)
    	Some(cells(x)(y))
    else
      None
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
    val plants = bios collect { case p: Plant => p }
    val carns = bios collect { case c: Carnivore => c }
    val herbs = bios collect { case h: Herbivore => h }
    World(tem.cells, plants, carns, herbs, width, height)
  }
  
  val learnInit = (bio: Bio) => bio match {
    case p: Plant => p
    case h: Herbivore => h.setLearningTrue
    case c: Carnivore => c.setLearningTrue
  }
  
  def initLearning(width: Int, height: Int): World = {
    val world = init(width, height)
    World(world.getBios map learnInit, width, height)
  }
  
  def empty: World = {
    World(List.empty[Bio],1,1)
  }
  
  def empty(width: Int, height: Int): World = {
    World(List.empty[Bio], width, height)
  }
  
  def removeABio[A <: Bio](bio: A, bios: List[A]): List[A] = {
    val i = bios.indexOf(bio)
    bios.take(i) ++ bios.drop(i + 1)
  }
  
  def boundaryCondition(coordinates: Coordinates, width: Int, height: Int): Coordinates = {
    var x = coordinates.x
    var y = coordinates.y

    if (x < 0) x += width - 1
    if (y < 0) y += height - 1
    if (width <= x) x -= width + 1
    if (height <= y) y -= height + 1

    Coordinates(x, y, coordinates.angle)
  } 
}

case class Cell(val materials: Tuple2[Water, Mineral], val bios: List[Bio]) {

  def remove(bio: Bio): Cell = Cell(materials, World.removeABio(bio, bios))
}

object Cell {

  val plant = 0.0003
  val carnivore = 0.00001
  val herbivore = 0.00002
  val total = plant + carnivore + herbivore
  
  def init(x: Int, y: Int): Cell = {
    val bios = Math.random() match {
      case i if i < plant => List(Plant(x, y))
      case i if plant <= i && i < plant + carnivore => List(Carnivore(x, y))
      case i if plant + carnivore <= i && i < total => List(Herbivore(x, y))
      case _ => List.empty[Bio]
    }
    Cell((new Water(), new Mineral()), bios)
  }

  def empty: Cell = {
    Cell((new Water(), new Mineral()), List.empty[Bio])
  }
}