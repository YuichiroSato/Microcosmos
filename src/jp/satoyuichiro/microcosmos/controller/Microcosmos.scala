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

object Microcosmos extends JFrame with Runnable {

  val fieldWidth = 600
  val fieldHeight = 600
  val field = new Field(fieldWidth, fieldHeight)
  var world = World.init(fieldWidth, fieldHeight)
  
  val filePathC = "data/carnivoreQ"
  val filePathH = "data/herbivoreQ"
  var iterationCount = 0
  var fileCount = -1

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
    CarnivoreQlearning.setValue(StateActionValue.empty)
    HerbivoreQlearning.setValue(StateActionValue.empty)
  }
  
  def readMap(count: Int): Tuple2[StateActionValue, StateActionValue] = {
    if (count < 0) return (StateActionValue.empty, StateActionValue.empty)
    
    val cFileName = filePathC + count.toString + ".txt"
    val hFileName = filePathH + count.toString + ".txt"
    val cf = new File(cFileName)
    val hf = new File(hFileName)
    
    if (cf.exists() && hf.exists()) {
      val cFile = new FileInputStream(cFileName)
	  val cStream = new ObjectInputStream(cFile)

      val hFile = new FileInputStream(hFileName)
	  val hStream = new ObjectInputStream(hFile)
    
      try {
        val cmap = cStream.readObject().asInstanceOf[StateActionValue]
        val hmap = hStream.readObject().asInstanceOf[StateActionValue]
        fileCount = count
        
        (cmap, hmap)
      } catch {
        case _: Throwable => (StateActionValue.empty, StateActionValue.empty)
      } finally {
        cStream.close()
        hStream.close()
        cFile.close()
        hFile.close()
      }
    } else {
      readMap(count - 1)
    }
  }
  
  def run() {
    val sleepTime = 25
    while(true) {
      world = world.update
      if (world.isEnd) {
        world = World.init(fieldWidth, fieldHeight)
        val map = readMap(iterationCount)
        CarnivoreQlearning.setValue(map._1)
        HerbivoreQlearning.setValue(map._2)
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