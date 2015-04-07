package jp.satoyuichiro.microcosmos.model.learning

case class NeuralNetwork(layers: List[Layer]) {

  def fire(): Unit = {
    layers foreach { l => l.fire() }
  }
  
  def input(inp: List[Double]): Unit = {
    (layers.head.neurons zip inp) foreach { t => t._1.setState(t._2) } 
  }
  
  def output: List[Double] = layers.last.neurons map (_.output)
  
  def printNetwork(): Unit = {
    var i = 1
    println("Network")
    for (l <- layers) {
      print("Layer" + i + "\nNeurons ")
      val sorted = l.neurons sortBy (_.index)
      sorted foreach { n =>
        print(n.index + " ")
      }
      i += 1
      println()
    }
  }
  
  def printConnection(): Unit = {
    var i = 1
    println("Network connection")
    for (l <- layers) {
      println("Layer" + i)
      println("Neuron \\ connect to")
      for (n <- (l.neurons sortBy (_.index))) {
        print(n.index + " [")
        for (b <- n.bonds) {
          print(b.to.index + " ")
        }
        println("]")
      }
      i += 1
    }
  }
  
  def printStrength(): Unit = {
    var i = 1
    println("Network connection strength")
    for (l <- layers) {
      println("Layer" + i)
      println("Neuron \\ bias, connect to")
      for (n <- (l.neurons sortBy (_.index))) {
        print(n.index + " " + n.bias + ", [")
        for (b <- n.bonds) {
          print("(" + b.to.index + " " + b.getStrength + ")")
        }
        println("]")
      }
      i += 1
    }
  }
  
  def printState(): Unit = {
    var i = 1
    println("Network State (Neuron state)")
    for (l <- layers) {
      println("Layer" + i)
      for (n <- (l.neurons sortBy (_.index))) {
        print("(" + n.index + " " + n.state + ") ")
      }
      println()
      i += 1
    }
  }

  def printOutput(): Unit = {
    var i = 1
    println("Network output (Neuron output)")
    for (l <- layers) {
      println("Layer" + i)
      for (n <- (l.neurons sortBy (_.index))) {
        print("(" + n.index + " " + n.output + ") ")
      }
      println()
      i += 1
    }
  }
}

object NeuralNetwork {
  
  def apply(xs: Int*): NeuralNetwork = {
    val layers = xs.toList map (i => Layer(i))
    val connected = (layers zip layers.tail).map(t => Layer.connect(t._1, t._2))
    NeuralNetwork(connected ++ List(layers.last))
  }
  
  def randomize(nt: NeuralNetwork): NeuralNetwork = {
    nt.layers foreach { l =>
      l.neurons foreach { n =>
        n.randomize()
      }
    }
    nt
  }
  
  def refresh(nt: NeuralNetwork): NeuralNetwork = {
    nt.layers foreach { l =>
      l.neurons foreach { n =>
        n.refresh
      }
    }
    nt
  }
  
  def backPropagation(nt: NeuralNetwork, traingData: List[Double]): NeuralNetwork = {
    Layer.setOutputDelta(nt.layers.last, traingData)
    Layer.updateStrength(nt.layers.last)
    Layer.updateBias(nt.layers.last)
    nt.layers.reverse.tail foreach { l =>
      Layer.calculateDelta(l)
      Layer.updateStrength(l)
      Layer.updateBias(l)
    }
    nt
  }
}

case class Layer(neurons: List[Neuron]) {
 
  def fire(): Unit = {
    neurons foreach { n => n.fire() }
  }
}

object Layer {
  
  def apply(i: Int): Layer = {
    Layer((for(_ <- 1 to i) yield Neuron.empty).toList)
  }
  
  def connect(from: Layer, to: Layer): Layer = {
    from.neurons foreach { n => n.connect(to) }
    from
  }
  
  def addConnection(from: Layer, to: Layer): Layer = {
    from.neurons foreach { n => n.addConnection(to) }
    from
  }
  
  def setOutputDelta(to: Layer, traingData: List[Double]): Layer = {
    (to.neurons zip traingData) foreach {
      t => t._1.setDelta((t._1.output - t._2) * Neuron.differential(t._1.getState))
    }
    to
  }
  
  /*   to  b  
   *n-> o ----o <-b.to
   *    o \---o
   *    o    -o
   *    o  \--o
   *    
   *      bonds
   * 
   */
  
  def calculateDelta(to: Layer): Layer = {
    to.neurons foreach { n =>
      n.bonds foreach { b => 
        n.addDelta(b.to.getDelta * b.getStrength * Neuron.differential(n.getState))
      }
    }
    to
  }
  
  val alpha = 0.1
  
  def updateStrength(to: Layer): Layer = {
    to.neurons foreach { n =>
      n.bonds foreach { b =>
        b.enhance(-alpha * b.to.getDelta * n.getState)
      }
    }
    to
  }
  
  def updateBias(to: Layer): Layer = {
    to.neurons foreach { n =>
      n.addBias(-alpha * n.getDelta * (-1))
    }
    to
  }
}

case class NeuralNetwork2D(layer2Ds: List[Layer2D]) {
  
  def fire(): Unit = {
    layer2Ds foreach { l => l.fire() }
  }
  
  def input(input: List[List[Double]]): Unit = {
    (layer2Ds.head.layers zip input) foreach { t =>
      (t._1.neurons zip t._2) foreach { s =>
        s._1.setState(s._2)
      } 
    }
  }
  
  def output: List[List[Double]] = layer2Ds.last.layers map (_.neurons map (_.output))
}

object NeuralNetwork2D {

  def apply(xs: (Int,Int)*): NeuralNetwork2D = {
    val layer2Ds = xs.toList map (t => Layer2D(t))
    val connected = (layer2Ds zip layer2Ds.tail).map(t => Layer2D.connect(t._1, t._2))
    NeuralNetwork2D(connected ++ List(layer2Ds.last))
  }
  
  def randomize(nt: NeuralNetwork2D): NeuralNetwork2D = {
    nt.layer2Ds foreach { l2D =>
      l2D.layers foreach { l =>
        l.neurons foreach { n =>
          n.randomize()
        }
      }
    }
    nt
  }
  
  def refresh(nt: NeuralNetwork2D): NeuralNetwork2D = {
    nt.layer2Ds foreach { l2D =>
      l2D.layers foreach { l =>
        l.neurons foreach { n =>
          n.refresh
        }
      }
    }
    nt
  }
  
  def backPropagation(nt: NeuralNetwork2D, traingData: List[List[Double]]): NeuralNetwork2D = {
    Layer2D.setOutputDelta(nt.layer2Ds.last, traingData)
    Layer2D.updateStrength(nt.layer2Ds.last)
    Layer2D.updateBias(nt.layer2Ds.last)
    nt.layer2Ds.reverse.tail foreach { l2D =>
      Layer2D.calculateDelta(l2D)
      Layer2D.updateStrength(l2D)
      Layer2D.updateBias(l2D)
    }
    nt
  }

}

case class Layer2D(layers: List[Layer]) {
  
  def fire(): Unit = {
    layers foreach { l => l.fire() }
  }
}

object Layer2D {

  /*
   *  x = 6, y = 4
   *   |0          5
   * -----------------> x
   * 0 | o o o o o o
   *   | o o o o o o
   *   | o o o o o o
   * 3 | o o o o o o  
   *   \/
   *    y
   *  
   */
  
  def apply(x: Int, y: Int): Layer2D = {
    Layer2D(List.fill(y)(Layer(x)))
  }
  
  def apply(t: (Int, Int)): Layer2D = {
    Layer2D(t._1, t._2)
  }
  
  def connect(from: Layer2D, to: Layer2D): Layer2D = {
    from.layers foreach { l1 => 
      to.layers foreach { l2 =>
        Layer.addConnection(l1, l2)
      }
    }
    from
  }
  
  def setOutputDelta(to: Layer2D, traingData: List[List[Double]]): Layer2D = {
    (to.layers zip traingData) foreach {
      t => Layer.setOutputDelta(t._1, t._2)
    }
    to
  }
  
  def calculateDelta(to: Layer2D): Layer2D = {
    to.layers foreach { l =>
      Layer.calculateDelta(l)
    }
    to
  }
  
  def updateStrength(to: Layer2D): Layer2D = {
    to.layers foreach { l =>
      Layer.updateStrength(l)
    }
    to
  }
  
  def updateBias(to: Layer2D): Layer2D = {
    to.layers foreach { l =>
      Layer.updateBias(l)
    }
    to
  }
}

case class Neuron(var bonds: List[Bond], index: Int) {
  
  var state = 0.0
  var bias = 0.0
  var delta = 0.0
  
  def setState(s: Double): Unit = state = s
  
  def getState: Double = state
  
  def add(s: Double): Unit = state += s
  
  def output: Double = Neuron.sigmoid(state - bias)
  
  def fire(): Unit = {
    bonds foreach { b =>
      b.to.add(output * b.getStrength)
    }
  }
  
  def addBond(b: Bond): Neuron = copy(bonds = b :: this.bonds)
  
  def connect(l: Layer): Unit = bonds = l.neurons map (n => Bond(n))
  
  def addConnection(l: Layer): Unit = bonds ++= l.neurons map (n => Bond(n))
  
  def randomize(): Unit = {
    bias = Math.random()
    bonds foreach { b =>
      b.setStrength(Math.random())
    }
  }
  
  def setDelta(d: Double): Unit = delta = d
  
  def addDelta(d: Double): Unit = delta += d
  
  def getDelta: Double = delta
  
  def addBias(b: Double): Unit = bias += b
  
  def refresh(): Unit = {
    state = 0.0
    delta = 0.0
  }
}

object Neuron {
  
  def sigmoid(x: Double): Double = 1 / (1 + Math.exp(-x))
  
  def differential(x: Double): Double = sigmoid(x) * (1 - sigmoid(x))
  
  def outputLayerDelta(n: Neuron, trainingData: Double): Double = (n.output - trainingData) * differential(n.getState)
  
  def internalLayerDelta() = 0.0
  
  def apply(bonds: List[Bond]): Neuron = Neuron(bonds, IndexMaker2.getNewIndex)
  
  def empty: Neuron = Neuron(List.empty[Bond], IndexMaker2.getNewIndex)
}

case class Bond(to: Neuron) {
  
  var strength = 0.0
  
  def setStrength(s: Double): Unit = strength = s
  
  def enhance(s: Double): Unit = strength += s
  
  def getStrength: Double = strength
}

object Bond {
  
}

object IndexMaker2 {
  
  var count = 0
  
  def getNewIndex: Int = {
    count += 1
    count
  }
}