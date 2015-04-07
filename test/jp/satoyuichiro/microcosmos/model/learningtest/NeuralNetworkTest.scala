package jp.satoyuichiro.microcosmos.model.learningtest

import org.junit._
import org.junit.Assert._
import junit.framework.TestCase
import jp.satoyuichiro.microcosmos.model.learning._

class NeuralNetworkTest {

  @Test def testNeuralNetwork {
    val nt = NeuralNetwork(3,2,3)
    val l1 = nt.layers.head
    val l2 = nt.layers.tail.head
    val l3 = nt.layers.tail.tail.head
    
    assertTrue(2 == l1.neurons.head.bonds.size)
    assertTrue(3 == l2.neurons.head.bonds.size)
    assertTrue(0 == l3.neurons.head.bonds.size)
    
    assertEquals(l2.neurons, l1.neurons.head.bonds map (_.to))
    assertEquals(l3.neurons, l2.neurons.head.bonds map (_.to))
    
    val nt2 = NeuralNetwork.randomize(nt)
//    nt2.printConnection(); println
//    nt2.printStrength(); println
//    nt2.printState(); println
//    println("\ninput\n\n")
    
    val inp1 = List(1.0,1.0,0.0)
    val inp2 = List(0.0,0.5,0.0)
    val t1 = List(0.0,0.0,1.0)
    val t2 = List(1.0,0.0,0.5)
    val dif: (List[Double], List[Double]) => Double = (a, b) => ((a zip b) map (t => (t._1 - t._2) * (t._1 - t._2))).fold(0.0)(_ + _)
    
    var tmp = nt2
    for (_ <- 0 to 10000) {
      tmp = NeuralNetwork.refresh(tmp)
      tmp.input(inp1)
      tmp.fire()
      tmp = NeuralNetwork.backPropagation(tmp, t1)
    }
    val result1 = NeuralNetwork.refresh(tmp)
    result1.input(inp1)
    result1.fire()
//    result1.printOutput
    assertTrue(dif(result1.output,t1) < 0.1)
    
    for (_ <- 0 to 10000) {
      tmp = NeuralNetwork.refresh(tmp)
      tmp.input(inp2)
      tmp.fire()
      tmp = NeuralNetwork.backPropagation(tmp, t2)
    }
    val result2 = NeuralNetwork.refresh(tmp)
    result2.input(inp2)
    result2.fire()
//    result2.printOutput
    assertTrue(dif(result2.output,t2) < 0.1)
  }
  
  @Test def testLayer {
    val n1 = Neuron(List.empty[Bond], 1)
    val n2 = Neuron(List.empty[Bond], 2)
    val n3 = Neuron(List.empty[Bond], 3)
    val n4 = Neuron(List.empty[Bond], 4)
    val n5 = Neuron(List.empty[Bond], 5)
    
    val l1 = Layer(List(n1, n2, n3))
    val l2 = Layer(List(n4, n5))
    assertEquals(List.empty[Bond], l1.neurons.head.bonds)
    
    Layer.connect(l1, l2)
    assertEquals(List(Bond(n4), Bond(n5)), l1.neurons.head.bonds)
    
    l1.neurons.head.bonds.head.setStrength(0.1)
    l1.neurons.head.bonds.tail.head.setStrength(0.3)
    l1.neurons.tail.head.bonds.head.setStrength(0.1)
    l1.neurons.tail.head.bonds.tail.head.setStrength(0.1)
    l1.neurons foreach { n => n.setState(0.2) }
    
    l1.fire()
    assertTrue(0 < n4.getState)
    assertTrue(0 < n5.getState)
  }
  
  @Test def testBond {
    val n1 = Neuron(List.empty[Bond], 1)
    val n2 = Neuron(List.empty[Bond], 2)
    val n3 = Neuron(List(Bond(n1), Bond(n2)), 3)
    n3.bonds.head.setStrength(0.1)
    n3.bonds.tail.head.setStrength(-0.5)
    n3.setState(1.0)
    
    assertTrue(0 == n1.getState)
    assertTrue(0 == n2.getState)
    
    n3.fire()
    assertTrue(0 < n1.getState)
    assertTrue(0 > n2.getState)
    
    n3.setState(0.0)
    n1.setState(0.0)
    n2.setState(0.0)
    n3.fire()
    assertTrue(0.049 < n1.getState && n1.getState < 0.051)
    assertTrue(-0.251 < n2.getState && n2.getState < -0.249)
  }
}