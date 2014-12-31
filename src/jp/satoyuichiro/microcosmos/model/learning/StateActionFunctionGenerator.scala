package jp.satoyuichiro.microcosmos.model.learning

import jp.satoyuichiro.microcosmos.model.World
import java.io.FileOutputStream
import java.io.ObjectOutputStream
import java.io.File
import java.io.FileInputStream
import java.io.ObjectInputStream

object StateActionFunctionGenerator {

  val filePathc = "data/carnivoreQ"
  val filePathh = "data/herbivoreQ"
  
  val fieldWidth = 600
  val fieldHeight = 600
  
  def main(args: Array[String]): Unit = {
    val n = 10000
    init()
    
    var world = World.initLearning(fieldWidth, fieldHeight)
    for (i <- 1 to n) {println(i)
      world = world.update
      if (world.isEnd) world = World.initLearning(fieldWidth, fieldHeight)
    }
    
    Qlearning.learningRasio()
    output(find())
  }
  
  def init(): Unit = {
    val sav = read(find() - 1)
    CarnivoreQlearning.setValue(sav._1)
    HerbivoreQlearning.setValue(sav._2)
    Qlearning.learningRasio()
  }
  
  def find(): Int = {
    var count = 0
    var cFileName = filePathc + count.toString + ".txt"
    var hFileName = filePathh + count.toString + ".txt"
    var cf = new File(cFileName)
    var hf = new File(hFileName)
    if (!(cf.exists() && hf.exists())) return 0
    
    while(cf.exists() && hf.exists()) {
      cFileName = filePathc + count.toString + ".txt"
      hFileName = filePathh + count.toString + ".txt"
      cf = new File(cFileName)
      hf = new File(hFileName)
      count += 1
    }
    println("count" + (count - 1))
    count - 1
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
        case _: Throwable => println("hoge")
          (StateActionValue.empty, StateActionValue.empty)
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
  
  def output(count: Int): Unit = {
    try {
      val cFile = new FileOutputStream(filePathc + count.toString + ".txt")
	  val cStream = new ObjectOutputStream(cFile)
	  cStream.writeObject(CarnivoreQlearning.getValue)

      val hFile = new FileOutputStream(filePathh + count.toString + ".txt")
	  val hStream = new ObjectOutputStream(hFile)
	  hStream.writeObject(HerbivoreQlearning.getValue)
	  
	  cStream.close()
	  cFile.close()
	  hStream.close()
	  hFile.close()
    } catch {
      case _: Throwable => println("something wrong!")
    }
  }

}