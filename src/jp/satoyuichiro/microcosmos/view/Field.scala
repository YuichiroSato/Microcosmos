package jp.satoyuichiro.microcosmos.view

import javax.swing.JPanel
import java.awt.Color
import java.awt.Graphics
import jp.satoyuichiro.microcosmos.model.World

class Field(width: Int, height: Int) extends JPanel {

  this.setBackground(Color.WHITE)
  
  def paintWorld(world: World, g: Graphics): Unit = {
    g.setColor(Color.BLACK)
    g.fillRect(world.x, world.y, 10, 10)
  }
}