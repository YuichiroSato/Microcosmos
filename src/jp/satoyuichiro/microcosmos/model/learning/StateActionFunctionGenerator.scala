package jp.satoyuichiro.microcosmos.model.learning

import jp.satoyuichiro.microcosmos.model.World
import java.io.FileOutputStream
import java.io.ObjectOutputStream

object StateActionFunctionGenerator {

  val filePathc = "data/carnivoreQ"
  val filePathh = "data/herbivoreQ"
  
  val fieldWidth = 600
  val fieldHeight = 600
  
  def main(args: Array[String]): Unit = {
    val n = 10000
    Qlearning.init
    var world = World.initLearning(fieldWidth, fieldHeight)
    for (i <- 1 to n) {println(i)
      world = world.update
      Qlearning.update()
      if (world.isEnd) world = World.initLearning(fieldWidth, fieldHeight)
    }
    
    output(0)
  }
  
  def output(count: Int): Unit = {
    try {
      val cFile = new FileOutputStream(filePathc + count.toString + ".txt")
	  val cStream = new ObjectOutputStream(cFile)
	  cStream.writeObject(Qlearning.getCarniveorLookUp)

      val hFile = new FileOutputStream(filePathh + count.toString + ".txt")
	  val hStream = new ObjectOutputStream(hFile)
	  hStream.writeObject(Qlearning.getHerbivoreLookUp)
	  
	  cStream.close()
	  cFile.close()
	  hStream.close()
	  hFile.close()
    } catch {
      case _: Throwable => println("something wrong!")
    }
  }

}