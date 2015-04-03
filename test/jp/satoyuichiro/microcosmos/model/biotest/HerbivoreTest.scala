package jp.satoyuichiro.microcosmos.model.biotest

import org.junit._
import org.junit.Assert._
import junit.framework.TestCase
import jp.satoyuichiro.microcosmos.model._
import jp.satoyuichiro.microcosmos.model.material._
import jp.satoyuichiro.microcosmos.model.learning._
import jp.satoyuichiro.microcosmos.model.bio._

class HerbivoreTest extends TestCase {
  
  val emptyWorld = World.empty(400,400)
  
  @Test def testchooseAction {
    val herb = Herbivore(20,20)
    println(Math.toDegrees(herb.external.coordinates.angle))
    val plant = Plant(25,15)
    val plant2 = Plant(1,30)
    val world = emptyWorld.add(herb).add(plant).add(plant2)
    val world2 = herb.herdCordedStrategy(world)
    println(Math.toDegrees(world2.herbivores.head.external.coordinates.angle))
  }
  
  @Test def testrunInto {
    val external = External.herbivore(Coordinates(20,20,3.14))
    val velocity = Velocity(10, 0.3)
    val herb = new Herbivore(external, null, velocity)
    
    val c1 = Coordinates(20, 10, 0.4)
    val res1 = herb.runInto(c1)
    assertTrue(90.0 == Math.toDegrees(res1.external.coordinates.angle))
    assertTrue(10 == res1.velocity.speed)
    assertTrue(0.0 == res1.velocity.rotation)
    
    val c2 = Coordinates(15, 25, 0.4)
    val res2 = herb.runInto(c2)
    assertTrue(225 == Math.toDegrees(res2.external.coordinates.angle))
  }
  
  /*       90
   *        |
   *   135  |    45
   *        | 
   * 180    |
   * ---------------> 0.0
   *        |
   *        |
   *  -135  |   -45
   *        |
   *        -90
   */
  @Test def testgetAngle {
    val external = External.herbivore(Coordinates(20,20,0.4))
    val velocity = Velocity(10, 0.3)
    val herb = new Herbivore(external, null, velocity)
    
    val c1 = Coordinates(30,20,0.0)
    val angle1 = herb.targetAngle(c1)
    assertTrue(0.0 == Math.toDegrees(angle1))
    
    val c2 = Coordinates(20,10,0.0)
    val angle2 = herb.targetAngle(c2)
    assertTrue(90.0 == Math.toDegrees(angle2))
    
    val c3 = Coordinates(10,20,0.0)
    val angle3 = herb.targetAngle(c3)
    assertTrue(180.0 == Math.toDegrees(angle3))

    val c4 = Coordinates(20,30,0.0)
    val angle4 = herb.targetAngle(c4)
    assertTrue(-90.0 == Math.toDegrees(angle4))

    val c5 = Coordinates(25,25,0.0)
    val angle5 = herb.targetAngle(c5)
    assertTrue(-45.0 == Math.toDegrees(angle5))

    val c6 = Coordinates(25,15,0.0)
    val angle6 = herb.targetAngle(c6)
    assertTrue(45.0 == Math.toDegrees(angle6))
    
    val c7 = Coordinates(15,15,0.0)
    val angle7 = herb.targetAngle(c7)
    assertTrue(135.0 == Math.toDegrees(angle7))
    
    val c8 = Coordinates(15,25,0.0)
    val angle8 = herb.targetAngle(c8)
    assertTrue(-135.0 == Math.toDegrees(angle8))
}
  
  @Test def testfindClosestPlantCarnivore {
    val x = 40
    val y = 40
    
    val herb = Herbivore(x,y)

    val carn1 = Carnivore(x+10,y+5)
    val carn2 = Carnivore(x+10,y-10)
    val carn3 = Carnivore(x-5,y-20)
    val carn4 = Carnivore(x-5,y+5)
    
    val plant1 = Plant(x+10,y+6)
    val plant2 = Plant(x+10,y-11)
    val plant3 = Plant(x-5,y-21)
    val plant4 = Plant(x-5,y+6)
    
    val world1 = World.empty(400,400).add(herb).add(plant1).add(plant2).add(plant3).add(plant4)
    .add(carn1).add(carn2).add(carn3).add(carn4)
    val opoc1 = herb.findClosestPlantCarnivore(world1)
    assertEquals((Some(plant4), Some(carn4)), opoc1)
    
    val world2 = World.empty(400,400).add(herb).add(plant1).add(plant2).add(plant3).add(plant4)
    val opoc2 = herb.findClosestPlantCarnivore(world2)
    assertEquals((Some(plant4), None), opoc2)
    
    val world3 = World.empty(400,400).add(herb).add(plant1).add(plant2)
    val opoc3 = herb.findClosestPlantCarnivore(world3)
    assertEquals((Some(plant1), None), opoc3)
    
    val world4 = World.empty(400,400).add(herb).add(plant1).add(plant3)
    val opoc4 = herb.findClosestPlantCarnivore(world4)
    assertEquals((Some(plant1), None), opoc4)
    
    val world5 = World.empty(400,400).add(herb).add(plant2).add(plant3)
    val opoc5 = herb.findClosestPlantCarnivore(world5)
    assertEquals((Some(plant2), None), opoc5)
  }

}