package jp.satoyuichiro.microcosmos.controller

import javax.swing.JFrame
import jp.satoyuichiro.microcosmos.view.Field
import java.awt.Graphics
import jp.satoyuichiro.microcosmos.model.World
import java.awt.Color
import java.awt.Toolkit
import jp.satoyuichiro.microcosmos.model.learning.Qlearning

object Microcosmos extends JFrame with Runnable {

  val fieldWidth = 600
  val fieldHeight = 600
  val field = new Field(fieldWidth, fieldHeight)
  var world = World.init(fieldWidth, fieldHeight)

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
    Qlearning.init
  }
  
  def run() {
    while(true) {
      world = world.update
      render()
      Thread.sleep(25)
    }
  }
  
  def render(): Unit = {
    val bufferStrategy = this.getBufferStrategy()
    if (bufferStrategy == null) {
        this.createBufferStrategy(3)
        return
    }
    
	val buf = bufferStrategy.getDrawGraphics()
    buf.setColor(Color.WHITE)
    buf.fillRect(0, 0, fieldWidth, fieldHeight)
    
    field.paintWorld(world, buf)
    buf.dispose()
    bufferStrategy.show()
  }
}