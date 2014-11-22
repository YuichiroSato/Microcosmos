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

case class World(var cells: Array[Array[Cell]], val plants: List[Plant], val carnivores: List[Carnivore], val herbivores: List[Herbivore], val width: Int, val height: Int) {

  def update: World = {
    val evolved = (plants map (_.evolve.asInstanceOf[Plant])) ++ (carnivores map (_.evolve.asInstanceOf[Carnivore])) ++ (herbivores map (_.evolve.asInstanceOf[Herbivore]))
	var world = World(evolved map applyBoundaryCondition, width, height)
    world.getBios foreach {
      bio => world = bio.interact(world)
    }
    World(world.getBios filter (!_.isDead) map applyBoundaryCondition, width, height)
  }
  
  def getBios: List[Bio] = {
    cells.toList flatMap (_.toList flatMap (_.bios))
  }
  
  def applyBoundaryCondition(bio: Bio): Bio = {
    bio match {
      case plant: Plant => plant
      case carn: Carnivore => Carnivore(External(boundaryCondition(carn.external.coordinates), carn.external.appearance), carn.internal, carn.velocity)
      case herb: Herbivore => Herbivore(External(boundaryCondition(herb.external.coordinates), herb.external.appearance), herb.internal, herb.velocity)
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
  
  def addPlant(plant: Plant): World = {
    val x = plant.external.coordinates.x
    val y = plant.external.coordinates.y
    val xy = boundaryCondition(Coordinates(x,y, 0.0))
    cells(xy.x)(xy.y) = Cell(cells(xy.x)(xy.y).materials, plant :: cells(xy.x)(xy.y).bios)
    World(cells, plant :: plants, carnivores, herbivores, width, height)
  }
  
  def addHerbivore(herb: Herbivore): World = {
    val x = herb.external.coordinates.x
    val y = herb.external.coordinates.y
    val xy = boundaryCondition(Coordinates(x,y, 0.0))
    cells(xy.x)(xy.y) = Cell(cells(xy.x)(xy.y).materials, herb :: cells(xy.x)(xy.y).bios)
    World(cells, plants, carnivores, herb :: herbivores, width, height)
  }
  
  def addCarnivore(carn: Carnivore): World = {
    val x = carn.external.coordinates.x
    val y = carn.external.coordinates.y
    val xy = boundaryCondition(Coordinates(x,y, 0.0))
    cells(xy.x)(xy.y) = Cell(cells(xy.x)(xy.y).materials, carn :: cells(xy.x)(xy.y).bios)
    World(cells, plants, carn :: carnivores, herbivores, width, height)
  }
  
  def getSubWorld(x:Int, y:Int, w: Int, h: Int): World = {
    var subCells = Array.fill(w)(Array.fill(h)(Cell.empty))
    for (i <- 0 to w - 1) {
      for (j <- 0 to h - 1) {
        if (0 <= x && 0 <= y && x + i < width && y + j < height) subCells(i)(j) = cells(x + i)(y + j)
      }
    }
    World(subCells, List.empty[Plant], List.empty[Carnivore], List.empty[Herbivore], w, h)
  }
  
  def removePlant(plant: Plant): World = {
    val x = plant.external.coordinates.x
    val y = plant.external.coordinates.y
    cells(x)(y) = cells(x)(y).remove(plant)
    World(cells, plants filter (_ != plant), carnivores, herbivores, width, height)
  }
  
  def removeHerbivore(herb: Herbivore): World = {
    val x = herb.external.coordinates.x
    val y = herb.external.coordinates.y
    cells(x)(y) = cells(x)(y).remove(herb)
    World(cells, plants, carnivores, herbivores filter (_ != herb), width, height)
  }
  
  def removeCarnivore(carn: Carnivore): World = {
    val x = carn.external.coordinates.x
    val y = carn.external.coordinates.y
    cells(x)(y) = cells(x)(y).remove(carn)
    World(cells, plants, carnivores filter (_ != carn), herbivores, width, height)
  }

}

object World {
  
  def apply(bios: List[Bio], width: Int, height: Int): World = {
    val materials = (new Water(), new Mineral())
    var cells = Array.fill(width)(Array.fill(height)(null:Cell))
    for (i <- 0 to width - 1) {
      for (j <- 0 to height - 1) {
        cells(i)(j) = Cell(materials, bios filter(bio => bio.external.coordinates.x == i && bio.external.coordinates.y == j))
      }
    }
    val plants = bios filter (_.isInstanceOf[Plant]) map (_.asInstanceOf[Plant])
    val carns = bios filter (_.isInstanceOf[Carnivore]) map (_.asInstanceOf[Carnivore])
    val herbs = bios filter (_.isInstanceOf[Herbivore]) map (_.asInstanceOf[Herbivore])
    World(cells, plants, carns, herbs, width, height)
  }
  
  def init(width: Int, height: Int): World = {
    var cells = Array.fill(width)(Array.fill(height)(null:Cell))
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
}

case class Cell(val materials: Tuple2[Water, Mineral], val bios: List[Bio]) {
  
  def remove(bio: Bio): Cell = Cell(materials, bios filter (_ != bio))
}

object Cell {
  
  def init(x: Int, y: Int): Cell = {
    val bios = Math.random match {
      case i if i < 0.0003 => List(Plant(x,y))
      case i if 0.0003 < i && i < 0.00035 => List(Carnivore(x,y))
      case i if 0.00035 < i && i < 0.00045 => List(Herbivore(x,y))
      case _ => List.empty[Bio]
    }
    Cell((new Water(), new Mineral()), bios)
  }
  
  def empty: Cell = {
    Cell((new Water(), new Mineral()), List.empty[Bio])
  }
}