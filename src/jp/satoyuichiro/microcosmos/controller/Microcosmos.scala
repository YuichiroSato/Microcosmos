package jp.satoyuichiro.microcosmos.controller

import javax.swing.JFrame
import jp.satoyuichiro.microcosmos.view.Field
import java.awt.Graphics
import jp.satoyuichiro.microcosmos.model.World
import java.awt.Color
import java.awt.Toolkit
import jp.satoyuichiro.microcosmos.model.learning._
import jp.satoyuichiro.microcosmos.model.learning.S
import java.io.FileInputStream
import java.io.ObjectInputStream
import scala.io.Source
import java.io.File
import jp.satoyuichiro.microcosmos.model.bio.Carnivore

object Microcosmos extends JFrame with Runnable {

  val fieldWidth = 600
  val fieldHeight = 600
  val field = new Field(fieldWidth, fieldHeight)
  var world = World.init(fieldWidth, fieldHeight)
  
  var iterationCount = 0

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
    Carnivore.setStrategy(new RandomStrategy())
  }
    
  def run() {
    val sleepTime = 25
    while(true) {
      world = world.update
      if (world.isEnd) {
        world = World.init(fieldWidth, fieldHeight)
        val map = StateActionValue.deserialize("Qlearning10000")
        Carnivore.setStrategy(new QlearningStrategy(map))
        iterationCount += 1
      }
      render()
      Thread.sleep(sleepTime)
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