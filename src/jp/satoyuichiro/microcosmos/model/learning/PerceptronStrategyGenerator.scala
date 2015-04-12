package jp.satoyuichiro.microcosmos.model.learning

import jp.satoyuichiro.microcosmos.model.bio.Carnivore
import jp.satoyuichiro.microcosmos.model.World
import jp.satoyuichiro.microcosmos.model.bio.Plant
import jp.satoyuichiro.microcosmos.model.bio.Herbivore
import jp.satoyuichiro.microcosmos.model.bio.External
import jp.satoyuichiro.microcosmos.model.bio.Internal
import jp.satoyuichiro.microcosmos.model.bio.Velocity
import jp.satoyuichiro.microcosmos.model.bio.Coordinates
import jp.satoyuichiro.microcosmos.model.bio.Appearance
import java.io.File
import java.io.FileInputStream
import java.io.ObjectInputStream
import java.io.FileOutputStream
import java.io.ObjectOutputStream

object PerceptronStrategyGenerator {

  val fieldWidth = 600
  val fieldHeight = 600
  var world = PerceptronLearningWorld.init(fieldWidth, fieldHeight)
  
  def main(args: Array[String]): Unit = {
    var strategy = new PerceptronLearningStrategy()
    strategy.initPerceptron()
    PerceptronMemory.setNext(strategy.getPerceptron)
    PerceptronMemory.setPrevious(strategy.getPerceptron)
    Carnivore.setStrategy(strategy)
    val n = 10000
    val de = 1.0 / n.toDouble
    var time = 0
    for (i <- 1 to 2 * n) {
      if (i % 100 == 0) println("\n" + i + " time" + time)
      world = world.update
      time += 1
      if (world.isEnd || 100 < time) {
        world = PerceptronLearningWorld.init(fieldWidth, fieldHeight)
        val psav = PerceptronMemory.getNext
        strategy = PerceptronLearningStrategy(psav)
        Carnivore.setStrategy(strategy)
        PerceptronMemory.setNext(psav)
        PerceptronMemory.setPrevious(psav)
        PerceptronStrategy.clearMemory()
        time = 0
      }
      PerceptronLearningStrategy.incrimentEpsilon(de)
    }
    PerceptronStateActionValue.serialize(Carnivore.strategy.asInstanceOf[PerceptronLearningStrategy].getPerceptron, "Perceptron10000")
  }
}

class PerceptronStrategy(psav: PerceptronStateActionValue) extends Strategy[Carnivore] {
  
  def chooseAction(subWorld: World, carn: Carnivore): Int = {
    if (carn.count <= 0) {
      PerceptronStrategy.addMemory(subWorld, carn)
      val memory = PerceptronStrategy.getMemory(carn)
      val s1 = memory.head
      val s2 = memory.tail.head
      val s3 = memory.tail.tail.head
      val s4 = memory.tail.tail.tail.head
      psav.getBestAction(s1, s2, s3, s4)
    } else {
      Carnivore.doNothing
    }
  }
}

object PerceptronStrategy {
  
  def apply(fileName: String): PerceptronStrategy = {
    new PerceptronStrategy(PerceptronStateActionValue.deserialize(fileName))
  }
  
  var memory = Map.empty[Int, List[S]]
  
  def addMemory(subWorld: World, carn: Carnivore): Unit = {
    val s = Qlearning.toState(subWorld, carn)
    val ls = memory.getOrElse(carn.index, List.fill(4)(S.empty)).take(3)
    memory += Tuple2(carn.index, s :: ls)
  }
  
  def getMemory(carn: Carnivore): List[S] = {
    memory.getOrElse(carn.index, List.fill(5)(S.empty))
  }
  
  def clearMemory(): Unit = memory = Map.empty[Int, List[S]]
}

class PerceptronLearningStrategy extends Strategy[Carnivore] {
  
  var perceptron: PerceptronStateActionValue = null
  
  def chooseAction(subWorld: World, carn: Carnivore): Int = {
//        val memory = PerceptronStrategy.getMemory(carn)
//    print(memory.head + " " + memory.tail.head + " " + memory.tail.tail.head + " " + memory.tail.tail.tail.head + " ")
//        println("i" + carn.index)
    if (carn.count <= 0) {
      PerceptronStrategy.addMemory(subWorld, carn)
      if (Math.random() < PerceptronLearningStrategy.epsilon) {
        val memory = PerceptronStrategy.getMemory(carn)
        val action = perceptron.getBestAction(memory)
        print("a" + action + " ")
        PerceptronTransitionBuilder.setAction(carn.index, action)
        action
      } else {
        val action = Carnivore.randomAction
        PerceptronTransitionBuilder.setAction(carn.index, action)
        action
      }
    } else {
      Carnivore.doNothing
    }
  }
  
  def initPerceptron(): Unit = perceptron = PerceptronStateActionValue()
  
  def setPerceptron(psav: PerceptronStateActionValue): Unit = perceptron = psav
  
  def getPerceptron: PerceptronStateActionValue = perceptron
}

object PerceptronLearningStrategy {
  
  def apply(psav: PerceptronStateActionValue): PerceptronLearningStrategy = {
    val s = new PerceptronLearningStrategy()
    s.setPerceptron(psav)
    s
  }

  var epsilon = 0.0
  
  def setEpsilon(e: Double): Unit = epsilon = e
  
  def incrimentEpsilon(d: Double): Unit = epsilon += d
  
}

case class PerceptronStateActionValue() {
  
  var perceptron = Perceptron.init
  
  def getBestAction(sls: List[S]): Int = {
    val s1 = sls.head
    val s2 = sls.tail.head
    val s3 = sls.tail.tail.head
    val s4 = sls.tail.tail.tail.head
    getBestAction(s1, s2, s3, s4)
  }
  
  def getBestAction(s1: S, s2: S, s3: S, s4: S): Int = {
    perceptron.refresh()
    perceptron.input(s1, s2, s3, s4)
    perceptron.fire()
    val out = perceptron.output
    perceptron.refresh()
    out.indexOf(out.max) + 1
  }
  
  def getMaxStateActionValue(sls: List[S]): Double = {
    val s1 = sls.head
    val s2 = sls.tail.head
    val s3 = sls.tail.tail.head
    val s4 = sls.tail.tail.tail.head
    getMaxStateActionValue(s1, s2, s3, s4)
  }
  
  def getMaxStateActionValue(s1: S, s2: S, s3: S, s4: S): Double = {
    perceptron.refresh()
    perceptron.input(s1, s2, s3, s4)
    perceptron.fire()
    perceptron.output.max
  }
  
  def setPerceptron(nn: Perceptron): Unit = perceptron = nn
  
  def getPerceptron(): Perceptron = perceptron
  
  def update(old: PerceptronStateActionValue, current: List[S], action: Int, reward: Int, next: List[S]): Unit = {
    if (current.size == 4 && next.size == 4) {
      val maxValue = old.getMaxStateActionValue(next)
      perceptron.refresh()
      val s1 = current.head
      val s2 = current.tail.head
      val s3 = current.tail.tail.head
      val s4 = current.tail.tail.tail.head
      perceptron.input(s1, s2, s3, s4)
      perceptron.fire()
      val output = perceptron.output
      val newValue = (reward.toDouble / 1000) + Qlearning.gamma * maxValue
      val traingData = output.take(action - 1) ++ List(newValue) ++ output.drop(action)
      perceptron.backPropagation(traingData)
//	      println("a"+action+" v"+newValue)
//	      println("out" + output)
//	      println("train" + traingData)
    }
  }
}

object PerceptronStateActionValue {
  
  def deserialize(fileName: String): PerceptronStateActionValue = {
    val file = new File(fileName + ".psav")
    
    if (file.exists()) {
      val fStream = new FileInputStream(file)
	  val oStream = new ObjectInputStream(fStream)
      try {
        oStream.readObject().asInstanceOf[PerceptronStateActionValue]
      } catch {
        case e: Throwable => throw e
      } finally {
        oStream.close()
        fStream.close()
      }
    } else {
      throw new RuntimeException("no file")
    }
  }
  
  def serialize(psav: PerceptronStateActionValue, fileName: String): Unit = {
    try {
      val fStream = new FileOutputStream(fileName + ".psav")
	  val oStream = new ObjectOutputStream(fStream)
	  oStream.writeObject(psav)
	  
	  fStream.close()
	  oStream.close()
    } catch {
      case _: Throwable => println("something wrong!")
    }
  }
}

case class Perceptron(nn: NeuralNetwork2D) {
 
  def input(s1: S, s2: S, s3: S, s4: S): Unit = {
    val i1 = Perceptron.SToInput(s1)
    val i2 = Perceptron.SToInput(s2)
    val i3 = Perceptron.SToInput(s3)
    val i4 = Perceptron.SToInput(s4)
    nn.input(List(i1, i2, i3, i4))
  }
  
  def fire(): Unit = {
    nn.fire()
  }
  
  def output: List[Double] = {
    nn.output.head
  }
  
  def refresh(): Unit = {
    NeuralNetwork2D.refresh(nn)
  }
  
  def backPropagation(traingData: List[Double]): Unit = {
    NeuralNetwork2D.backPropagation(nn, List(traingData))
  }
}

object Perceptron {
  
  /*
   * input layer
   * 
   *   ul      ur      bl      br     biostate
   *  o o o   o o o   o o o   o o o   o o
   *  p h c
   *  
   *  ooo ooo ooo ooo oo <- S(t_1)
   *  ooo ooo ooo ooo oo <- S(t_2)
   *  ooo ooo ooo ooo oo <- S(t_3)
   *  ooo ooo ooo ooo oo <- S(t_4)
   * 
   *  x = 14, y = 4
   *  
   *  output layer
   *  
   *  o o o o o o o
   *  1 2 3 4 5 6 7 ActionType
   *  
   *  x = 7, y = 1
   */
  
  def init: Perceptron = {
    val nn = NeuralNetwork2D((14,4), (7,2), (7,1))
    Perceptron(NeuralNetwork2D.randomize(nn))
  }
  
  def SToInput(s: S): List[Double] = {
    subStateToInput(s.ul) ++ subStateToInput(s.ur) ++ subStateToInput(s.bl) ++ subStateToInput(s.br) ++ bioStateToInput(s.bs)
  }
  
  def subStateToInput(sub: SubState): List[Double] = {
    val p = if (sub.plant) 1.0 else 0.0
    val h = if (sub.herbivore) 1.0 else 0.0
    val c = if (sub.carnivore) 1.0 else 0.0
    List(p, h, c)
  }
  
  def bioStateToInput(bs: BioState): List[Double] = {
    List(bs.speed, (bs.angleType - 2.0) / 2.0)
  }
}

object PerceptronMemory {
  
  val maxSize = 100
  var logs = List.empty[PerceptronTransition]
  var previousPSAV: PerceptronStateActionValue = null
  var nextPSAV: PerceptronStateActionValue = null
  
  def putTransition(log: PerceptronTransition): Unit = {
    if (logs.size < maxSize) {
      logs = log :: logs
    } else {
      logs = logs.dropRight(1)
      logs = log :: logs
    }
  }
  
  def getTransitions: List[PerceptronTransition] = logs
  
  def getRandomTransition: PerceptronTransition = {
    val n = (Math.random() * logs.size).toInt
    logs.drop(n).head
  }
  
  def doLearning: Unit = {
    val t = getRandomTransition
    nextPSAV.update(previousPSAV, t.current, t.action, t.reward, t.next)
  }
  
  def setNext(psav: PerceptronStateActionValue): Unit = nextPSAV = psav
  
  def setPrevious(psav: PerceptronStateActionValue): Unit = previousPSAV = psav
  
  def getNext: PerceptronStateActionValue = nextPSAV
}

object PerceptronTransitionBuilder {
  
  var current: Map[Int, List[S]] = Map.empty[Int, List[S]]
  var action: Map[Int, Int] = Map.empty[Int, Int]
  var reward: Map[Int, Int] = Map.empty[Int, Int]
  var next: Map[Int, List[S]] = Map.empty[Int, List[S]]
  
  def init(i: Int): Unit = {
    current -= i
    action -= i
    reward -= i
    next -= i
  }
  
  def setInitState(i: Int, s: List[S]): Unit = if (!current.keySet.contains(i)) current += Tuple2(i, s)
  
  def setCurrentState(i: Int, s: List[S]): Unit = current += Tuple2(i, s)
  
  def setNextState(i: Int, s: List[S]): Unit = next += Tuple2(i, s)
  
  def setReward(i: Int, r: Int): Unit = reward += Tuple2(i, r)
  
  def addReward(i: Int, r: Int): Unit = reward += Tuple2(i, reward.getOrElse(i, 0) + r)
  
  def setAction(i: Int, a: Int): Unit = action += Tuple2(i, a)
  
  def build(i: Int): Option[PerceptronTransition] = {
    (current.get(i), action.get(i), reward.get(i), next.get(i)) match {
      case (Some(c), Some(a), Some(r), Some(n)) => Some(PerceptronTransition(c, a, r, n))
      case _ => None
    }
  }
}

case class PerceptronTransition(val current: List[S], val action: Int, val reward: Int, val next: List[S])

case class PerceptronLearningWorld(world: World) {
  
  def update: PerceptronLearningWorld = evolve.interact
  
  def evolve: PerceptronLearningWorld = {
    if (isValid) {
      val evolved = (world.plants map (_.evolve.asInstanceOf[Plant])) ++
        (world.carnivores map (c => PerceptronLearningWorld.evolvePerceptronCarnivore(c.asInstanceOf[PerceptronCarnivore]))) ++
        (world.herbivores map (_.evolve.asInstanceOf[Herbivore]))
      new PerceptronLearningWorld(World(evolved map world.applyBoundaryCondition, world.width, world.height))
    } else {
      this
    }
  }
  
  def interact: PerceptronLearningWorld = {
    var w = world
    for (bio <- w.getBios) {
      if (bio.isInstanceOf[PerceptronCarnivore]) {
        w = PerceptronLearningWorld.interactPerceptronCarnivore(w, bio.asInstanceOf[PerceptronCarnivore])
      } else {
        w = bio.interact(w)
      }
    }
    PerceptronLearningWorld.toLearningWorld(World(w.getBios filter (!_.isDead) map w.applyBoundaryCondition, w.width, w.height))
  }
  
  def isValid: Boolean = world.carnivores.count(_.isInstanceOf[PerceptronCarnivore]) == world.carnivores.size
  
  def isEnd: Boolean = world.isEnd
}

object PerceptronLearningWorld {

  def init(fieldWidth: Int, fieldHeight: Int): PerceptronLearningWorld = {
    PerceptronLearningWorld.toLearningWorld(World.init(fieldWidth, fieldHeight))
  }
  
  def toLearningWorld(world: World): PerceptronLearningWorld = {
    var newWorld = world
    world.carnivores foreach { c => 
      if (!c.isInstanceOf[PerceptronCarnivore]) {
        world.updateBio(c, PerceptronCarnivore.fromCarnivore(c))
      }}
    new PerceptronLearningWorld(newWorld)
  }
  
  def evolvePerceptronCarnivore(carn: PerceptronCarnivore): PerceptronCarnivore = {
    val buf = carn.internal.life
    val newCarn = PerceptronCarnivore(carn.evolve.asInstanceOf[Carnivore])
    PerceptronTransitionBuilder.setReward(carn.index, newCarn.internal.life - buf)
    if (carn.isDead) {
      PerceptronTransitionBuilder.addReward(carn.index, -1000)
    }
    newCarn
  }
  
  def interactPerceptronCarnivore(world: World, carn: PerceptronCarnivore): World = {
    val i = carn.index

    val herbN = world.herbivores.size
    val w1 = carn.eatHervibore(world)
    PerceptronTransitionBuilder.addReward(i, (herbN - w1.herbivores.size) * 100)
    
    val carnN = world.carnivores.size
    val w2 = carn.giveBirthCarnivore(w1)
    if (carnN < w2.carnivores.size) {
      PerceptronTransitionBuilder.addReward(i, 1000)
    }
    
    val w3 = carn.chooseAction(w2)
    
    val s = PerceptronStrategy.getMemory(carn)
    PerceptronTransitionBuilder.setInitState(i, s)
    PerceptronTransitionBuilder.setNextState(i, s)
    PerceptronTransitionBuilder.build(i) match {
      case Some(t) =>
        PerceptronMemory.putTransition(t)
        PerceptronTransitionBuilder.init(i)
        PerceptronTransitionBuilder.setCurrentState(i, s)
        PerceptronMemory.doLearning
      case None =>
    }
    w3
  }
}

class PerceptronCarnivore(external: External, internal: Internal, velocity: Velocity, count: Int, index: Int)
  extends Carnivore(external, internal, velocity, count, index) {
  
  override val maxCount = LearningCarnivore.maxCount
  
  override def setExternal(e: External): PerceptronCarnivore = PerceptronCarnivore(copy(external = e))
  override def setExternal(c: Coordinates, a: Appearance): PerceptronCarnivore = PerceptronCarnivore(copy(external = External(c, a)))
  override def setInternal(l: Int, w: Int, m: Int): PerceptronCarnivore = PerceptronCarnivore(copy(internal = Internal(l, w, m)))
  override def setLife(l: Int): PerceptronCarnivore = PerceptronCarnivore(copy(internal = Internal(l, internal.water, internal.mineral)))
  override def setLife(f: Int => Int): PerceptronCarnivore = setLife(f(internal.life))
  override def setVelocity(v: Velocity): PerceptronCarnivore = PerceptronCarnivore(copy(velocity = v))
  override def decrimentCounter: PerceptronCarnivore = PerceptronCarnivore(copy(count = count - 1))
  override def repareCounter: PerceptronCarnivore = PerceptronCarnivore(copy(count = maxCount))
    
  override def toString: String = "Perceptron" + super.toString
}

object PerceptronCarnivore {
  
  val maxCount = 10
  
  def fromCarnivore(carn: Carnivore): PerceptronCarnivore = {
    new PerceptronCarnivore(carn.external, carn.internal, carn.velocity, maxCount, carn.index)
  }
  
  def apply(carn: Carnivore): PerceptronCarnivore = {
    new PerceptronCarnivore(carn.external, carn.internal, carn.velocity, carn.count, carn.index)
  }
}