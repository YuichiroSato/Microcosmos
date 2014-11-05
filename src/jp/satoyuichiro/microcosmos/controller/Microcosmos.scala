package jp.satoyuichiro.microcosmos.controller

import javax.swing.JFrame
import jp.satoyuichiro.microcosmos.view.Field
import java.awt.Graphics
import jp.satoyuichiro.microcosmos.model.World
import java.awt.Color

object Microcosmos extends JFrame with Runnable {

  val fieldWidth = 400
  val fieldHeight = 400
  val field = new Field(fieldWidth, fieldHeight)
  var world = new World(0,0)

  def main(args: Array[String]): Unit = {
    init()
    (new Thread(this)).start()
  }
  
  def init(): Unit = {
    this.setBounds(100, 100, fieldWidth, fieldHeight)
    this.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE)
    this.add(field)
    this.setVisible(true)
    this.show()
  }
  
  def run() {
    while(true) {
      world = world.update;
      this.repaint()
      Thread.sleep(100)
    }
  }
  
  override def paint(g: Graphics): Unit = {
    g.setColor(Color.WHITE)
    g.fillRect(0, 0, fieldWidth, fieldHeight)
    
    field.paintWorld(world, g)
    field.setVisible(true)
  }

}