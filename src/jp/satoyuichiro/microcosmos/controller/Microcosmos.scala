package jp.satoyuichiro.microcosmos.controller

import javax.swing.JFrame
import jp.satoyuichiro.microcosmos.view.Field
import java.awt.Graphics
import jp.satoyuichiro.microcosmos.model.World
import java.awt.Color
import java.awt.Toolkit
import jp.satoyuichiro.microcosmos.model.learning.Qlearning
import jp.satoyuichiro.microcosmos.model.learning.StateActionFunction
import jp.satoyuichiro.microcosmos.model.learning.State
import java.io.FileInputStream
import java.io.ObjectInputStream

object Microcosmos extends JFrame with Runnable {

  val fieldWidth = 600
  val fieldHeight = 600
  val field = new Field(fieldWidth, fieldHeight)
  var world = World.init(fieldWidth, fieldHeight)
  
  val filePathC = "data/carnivore"
  val filePathH = "data/herbivore"
  var fileCount = 0

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
    val map = readMap(0)
    StateActionFunction.setCarnivoreQ(map._1)
    StateActionFunction.setHerbivoreQ(map._2)
  }
  
  def readMap(count: Int): Tuple2[Map[State, Int], Map[State, Int]] = {
    if (count < 0) return (Map.empty[State, Int], Map.empty[State, Int])

    try {
      val cFile = new FileInputStream(filePathC + fileCount.toString + ".txt")
	  val cStream = new ObjectInputStream(cFile)
      val cmap = cStream.readObject().asInstanceOf[Map[State, Int]]

      val hFile = new FileInputStream(filePathH + fileCount.toString + ".txt")
	  val hStream = new ObjectInputStream(hFile)
      val hmap = hStream.readObject().asInstanceOf[Map[State, Int]]
      
      cStream.close()
      hStream.close()
      cFile.close()
      hFile.close()
      
      (cmap, hmap)
    } catch {
      case _: Throwable =>
        fileCount -= 1
        readMap(fileCount)
    }
  }
  
  def run() {
    val sleepTime = 25
    while(true) {
      world = world.update
      if (world.isEnd) {
        world = World.init(fieldWidth, fieldHeight)
        fileCount += 1
        val map = readMap(fileCount)
        StateActionFunction.setCarnivoreQ(map._1)
        StateActionFunction.setHerbivoreQ(map._2)
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