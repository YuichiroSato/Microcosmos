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

class Field(width: Int, height: Int) extends JPanel(true) {

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
    g.setColor(Color.GREEN)
    g.fillOval(plant.x, plant.y, 10, 10)
  }
  
  def paintCarnivore(carnivore: Carnivore, g: Graphics): Unit = {
    g.setColor(Color.RED)
    g.fillRect(carnivore.x, carnivore.y, 10, 10)
  }
  
  def paintHerbivore(herbivore: Herbivore, g: Graphics): Unit = {
    g.setColor(Color.BLUE)
    g.fillRect(herbivore.x, herbivore.y, 10, 10)
  }
}