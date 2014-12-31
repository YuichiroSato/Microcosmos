package jp.satoyuichiro.microcosmos.modeltest

import org.junit._
import org.junit.Assert._
import junit.framework.TestCase
import jp.satoyuichiro.microcosmos.model._
import jp.satoyuichiro.microcosmos.model.material._
import jp.satoyuichiro.microcosmos.model.bio._

class WorldTest extends TestCase {

  @Test def testgetSubWorld {
    val plant1 = Plant(1,1)
    val plant2 = Plant(2,2)
    val carn1 = Carnivore(6,6)
    val herb1 = Herbivore(3,9)
    val world = World(List(plant1, plant2, carn1, herb1), 10, 10)
    
    val sub1 = world.getSubWorld(0, 0, 5, 5)
    assertEquals(List(plant1, plant2), sub1.getBios)
    
    val sub2 = world.getSubWorld(5, 0, 5, 5)
    assertEquals(List.empty[Bio], sub2.getBios)
    
    val sub3 = world.getSubWorld(0, 5, 5, 5)
    assertEquals(List(herb1), sub3.getBios)
    
    val sub4 = world.getSubWorld(5, 5, 5, 5)
    assertEquals(List(carn1), sub4.getBios)
  }

  @Test def testgetSubWorldAround {
    val plant1 = Plant(3,3)
    val plant2 = Plant(2,2)
    val carn1 = Carnivore(6,6)
    val herb1 = Herbivore(3,9)
    val world = World(List(plant1, plant2, carn1, herb1), 10, 10)
    assertEquals(Set(plant2, plant1, carn1, herb1), world.getBios.toSet)
    
    val sub1 = world.getSubWorldAround(plant1, 10, 10)
    assertEquals(List(plant2), sub1.plants)
    assertEquals(List(carn1), sub1.carnivores)
    assertEquals(List(), sub1.herbivores)
    assertEquals(Set(plant2, carn1), sub1.getBios.toSet)
    assertEquals(Set(plant2, plant1, carn1, herb1).size, world.getBios.toSet.size)
    
    val sub2 = world.getSubWorldAround(plant2, 5, 5)
    assertEquals(List(plant1), sub2.plants)
    assertEquals(List(), sub2.carnivores)
    assertEquals(List(), sub2.herbivores)
    assertEquals(Set(plant1), sub2.getBios.toSet)
    
    val sub3 = world.getSubWorldAround(carn1, 40, 40)
    assertEquals(Set(plant1, plant2, herb1), sub3.getBios.toSet)
  }
}