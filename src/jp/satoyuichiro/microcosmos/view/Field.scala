package jp.satoyuichiro.microcosmos.view

import javax.swing.JPanel
import java.awt.Color
import java.awt.Graphics
import jp.satoyuichiro.microcosmos.model.World
import jp.satoyuichiro.microcosmos.model.bio.Bio
import jp.satoyuichiro.microcosmos.model.Cell
import jp.satoyuichiro.microcosmos.model.material.Water
import jp.satoyuichiro.microcosmos.model.material.Mineral
import jp.satoyuichiro.microcosmos.model.bio.Plant
import jp.satoyuichiro.microcosmos.model.bio.Carnivore
import jp.satoyuichiro.microcosmos.model.bio.Herbivore
import java.awt.Polygon

class Field(width: Int, height: Int) extends JPanel {

  this.setBackground(Color.WHITE)
  
  def paintWorld(world: World, g: Graphics): Unit = {
    world.plants foreach { plant => paintPlant(plant, g) }
    world.carnivores foreach { carnivore => paintCarnivore(carnivore, g) }
    world.herbivores foreach { herbivore => paintHerbivore(herbivore, g) }
  }
  
  def paintCell(cell: Cell, g: Graphics): Unit = {
    paintMaterial(cell.materials, g)
    paintBios(cell.bios, g)
  }
  
  def paintMaterial(material: Tuple2[Water, Mineral], g: Graphics): Unit = {
    
  }
  
  def paintBios(bios: List[Bio], g: Graphics): Unit = {
    bios foreach {
      bio => bio match {
        case plant: Plant => paintPlant(plant, g)
        case carn: Carnivore => paintCarnivore(carn, g)
        case herb: Herbivore => paintHerbivore(herb, g)
      }
    }
  }
  
  def paintPlant(plant: Plant, g: Graphics): Unit = {
    g.setColor(plant.external.appearance.color)
    g.fillOval(plant.external.coordinates.x, plant.external.coordinates.y, plant.external.appearance.size, plant.external.appearance.size)
  }
  
  def paintCarnivore(carnivore: Carnivore, g: Graphics): Unit = {
    val x = carnivore.external.coordinates.x
    val y = carnivore.external.coordinates.y
    val r = carnivore.external.appearance.size
    val theta = carnivore.external.coordinates.angle
    
    val xpoints = Array(x + r * Math.cos(theta), x + r * Math.cos(theta + 2.5), x + 0.5 * r * Math.cos(theta - 3.14), x + r * Math.cos(theta - 2.5)) map (_.toInt)
    val ypoints = Array(y + r * Math.sin(theta), y + r * Math.sin(theta + 2.5), y + 0.5 * r * Math.sin(theta - 3.14), y + r * Math.sin(theta - 2.5)) map (_.toInt)
    g.setColor(carnivore.external.appearance.color)
    g.fillPolygon(xpoints, ypoints, xpoints.length)
  }
  
  def paintHerbivore(herbivore: Herbivore, g: Graphics): Unit = {
    val x = herbivore.external.coordinates.x
    val y = herbivore.external.coordinates.y
    val r = herbivore.external.appearance.size
    val theta = herbivore.external.coordinates.angle
    
    val xpoints = Array(x + r * Math.cos(theta), x + r * Math.cos(theta + 2.5), x + r * Math.cos(theta - 3.14), x + r * Math.cos(theta - 2.5)) map (_.toInt)
    val ypoints = Array(y + r * Math.sin(theta), y + r * Math.sin(theta + 2.5), y + r * Math.sin(theta - 3.14), y + r * Math.sin(theta - 2.5)) map (_.toInt)
    g.setColor(herbivore.external.appearance.color)
    g.fillPolygon(xpoints, ypoints, xpoints.length)
  }
}