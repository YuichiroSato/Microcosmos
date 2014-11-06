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

class Field(width: Int, height: Int) extends JPanel {

  this.setBackground(Color.WHITE)
  
  def paintWorld(world: World, g: Graphics): Unit = {
    world.cells foreach {
      cellarray => cellarray foreach {
        cell => paintCell(cell, g)
      }
    }
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
    g.setColor(plant.appearance.color)
    g.fillOval(plant.coordinates.x, plant.coordinates.y, plant.appearance.size, plant.appearance.size)
  }
  
  def paintCarnivore(carnivore: Carnivore, g: Graphics): Unit = {
    g.setColor(carnivore.appearance.color)
    g.fillRect(carnivore.coordinates.x, carnivore.coordinates.y, carnivore.appearance.size, carnivore.appearance.size)
  }
  
  def paintHerbivore(herbivore: Herbivore, g: Graphics): Unit = {
    g.setColor(herbivore.appearance.color)
    g.fillRect(herbivore.coordinates.x, herbivore.coordinates.y, herbivore.appearance.size, herbivore.appearance.size)
  }
}