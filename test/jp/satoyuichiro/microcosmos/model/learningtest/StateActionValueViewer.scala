package jp.satoyuichiro.microcosmos.model.learningtest

import jp.satoyuichiro.microcosmos.model.learning.StateActionValue
import java.io.File
import java.io.FileInputStream
import java.io.ObjectInputStream

object StateActionValueViewer {

  val filePathc = "data/carnivoreQ"
  val filePathh = "data/herbivoreQ"
  
  def main(args: Array[String]): Unit = {
    val count = 5
    val t = read(count)
    println("carnivore")
    printSAV(t._1)
    println("\n harbivore")
    printSAV(t._2)
  }
  
  def read(count: Int): Tuple2[StateActionValue, StateActionValue] = {
    val cFileName = filePathc + count.toString + ".txt"
    val hFileName = filePathh + count.toString + ".txt"
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
      (StateActionValue.empty, StateActionValue.empty)
    }
  }

  def printSAV(sav: StateActionValue): Unit = {
//    val s_av = sav.S_AValue
//    s_av.map(_.toString).toList.sorted foreach println
    val best = sav.bestAction
    best.map(_.toString).toList.sorted foreach println
//    println(sav.toString)
  }
}