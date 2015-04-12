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
  
  val qstrategy = new QlearningStrategy(StateActionValue.deserialize("data/Qlearning10000"))
  val pstrategy = new PerceptronStrategy(PerceptronStateActionValue.deserialize("data/Perceptron10000"))

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
    Carnivore.setStrategy(qstrategy)
  }
    
  def run() {
    val sleepTime = 25
    var time = 0
    while(true) {
      world = world.update
      if (world.isEnd || 1000 < time) {
        world = World.init(fieldWidth, fieldHeight)
        Carnivore.strategy match {
          case s: RandomStrategy => Carnivore.setStrategy(qstrategy)
          case s: QlearningStrategy => Carnivore.setStrategy(pstrategy)
          case s: PerceptronStrategy =>
            PerceptronStrategy.clearMemory()
            Carnivore.setStrategy(qstrategy)
          case _ =>
        }
        time = 0
        iterationCount += 1
      }
      render()
      time += 1
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