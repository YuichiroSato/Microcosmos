package jp.satoyuichiro.microcosmos.model.learningtest

import org.junit._
import org.junit.Assert._
import junit.framework.TestCase
import jp.satoyuichiro.microcosmos.model._
import jp.satoyuichiro.microcosmos.model.material._
import jp.satoyuichiro.microcosmos.model.learning._
import jp.satoyuichiro.microcosmos.model.bio._

class QlearningTest extends TestCase {

  
  @Test def testState {
    val materials = (new Water(), new Mineral())
    val cell1 = Cell(materials, List.empty[Bio])
    val pcell = Cell(materials, List(Plant(0, 0)))
    val ccell = Cell(materials, List(Carnivore(0, 0)))
    val hcell = Cell(materials, List(Herbivore(0,0)))
    
    val subSubWorld1 = Array(cell1, cell1, cell1, cell1)
    val subWorld1 = World(Array(subSubWorld1, subSubWorld1, subSubWorld1, subSubWorld1), List.empty[Plant], List.empty[Carnivore], List.empty[Herbivore], 8, 8)
    val state1 = S(subWorld1, Carnivore.apply(2,2))
    println(state1)
    
    val subSubWorld2 = Array(cell1, cell1, cell1, cell1)
    val subWorld2 = World(Array(Array(cell1, cell1, pcell, cell1), subSubWorld1, subSubWorld1, subSubWorld1), List.empty[Plant], List.empty[Carnivore], List.empty[Herbivore], 8, 8)
    val state2 = S(subWorld2, Carnivore.apply(2,2))
    println(state2)
    
  }
  
  @Test def testsubWorldToSubState {
    val materials = (new Water(), new Mineral())
    val cell1 = Cell(materials, List.empty[Bio])
    val pcell = Cell(materials, List(Plant(0, 0)))
    val ccell = Cell(materials, List(Carnivore(0, 0)))
    val hcell = Cell(materials, List(Herbivore(0,0)))
    
    val subSubWorld1 = Array(cell1, cell1, cell1, cell1)
    val subSubWorld2 = Array(cell1, cell1, pcell, cell1)
    val subSubWorld3 = Array(cell1, ccell, pcell, cell1)
    val subSubWorld4 = Array(cell1, cell1, hcell, cell1)

    val subWorld1 = World(Array(subSubWorld1, subSubWorld2, subSubWorld3, subSubWorld4), List.empty[Plant], List.empty[Carnivore], List.empty[Herbivore], 8, 8)
    
    val state1 = S.subWorldToSubState(subWorld1, 0, 0, 2, 2)
    assertEquals(SubState(false, false, false), state1)
    
    val state2 = S.subWorldToSubState(subWorld1, 2, 0, 2, 2)
    assertEquals(SubState(false, false, true), state2)
    
    val state3 = S.subWorldToSubState(subWorld1, 2, 0, 2, 2)
    assertEquals(SubState(false, false, true), state3)
    
    val state4 = S.subWorldToSubState(subWorld1, 2, 2, 2, 2)
    assertEquals(SubState(true, true, false), state4)
    
    val state5 = S.subWorldToSubState(subWorld1, 0, 0, 4, 4)
    assertEquals(SubState(true, true, true), state5)
  }
  
  @Test def testBioState {
    val coordinates1 = Coordinates(0, 0, 0.1)
    val coordinates2 = Coordinates(0, 0, 1.6)
    val coordinates3 = Coordinates(0, 0, 6.2)
    val coordinates4 = Coordinates(0, 0, 6.5)
    val velocity1 = Velocity(0.1, 0.0)
    val velocity2 = Velocity(1.5, 0.0)
    val velocity3 = Velocity(2.3, 0.0)
    
    val c1 = Carnivore(0,0).copy(velocity = velocity1).setExternal(coordinates1, null)
    assertEquals(BioState(0,0), BioState(c1))
    
    val c2 = c1.copy(velocity = velocity2).setExternal(coordinates2, null)
    assertEquals(BioState(1,1), BioState(c2))
    
    val c3 = c2.copy(velocity = velocity3).setExternal(coordinates3, null)
    assertEquals(BioState(2,3), BioState(c3))
    
    val c4 = c3.setExternal(coordinates4, null)
    assertEquals(BioState(2,0), BioState(c4))
  }
  
  @Test def testStateActionValue {
    val subState1 = SubState(false, false, false)
    val subState2 = SubState(true, false, false)
    val bioState = BioState(1, 1)
    
    val s1 = S(subState1, subState1, subState1, subState1, bioState)
    val s2 = S(subState2, subState1, subState1, subState1, bioState)
    
    val sav1 = new StateActionValue(Map.empty[(S, Int), Double], Map.empty[S, Map[Int, Double]], Map.empty[S, Int])
    
    val sav2 = sav1.update(s1, 1, 10)
    assertEquals(Some(10), sav2.SA_Value.get((s1, 1)))
    assertEquals(Some(Map(1 -> 10)), sav2.S_AValue.get(s1))
    assertEquals(Some(1), sav2.bestAction.get(s1))
    
    val sav3 = sav2.update(s1, 1, 1000)
    assertEquals(Some(1000), sav3.SA_Value.get((s1, 1)))
    assertEquals(Some(Map(1 -> 1000)), sav3.S_AValue.get(s1))
    assertEquals(Some(1), sav3.bestAction.get(s1))
    
    val sav4 = sav3.update(s1, 2, 20)
    assertEquals(Some(20), sav4.SA_Value.get((s1, 2)))
    assertEquals(Some(Map(1 -> 1000, 2 -> 20)), sav4.S_AValue.get(s1))
    assertEquals(Some(1), sav4.bestAction.get(s1))
    
    val sav5 = sav4.update(s2, 7, 50)
    assertEquals(Some(50), sav5.SA_Value.get((s2, 7)))
    assertEquals(Some(Map(7 -> 50)), sav5.S_AValue.get(s2))
    assertEquals(Some(7), sav5.bestAction.get(s2))
    
    val sav6 = sav5.update(s1, 7, 100000)
    assertEquals(Some(1000), sav6.SA_Value.get((s1, 1)))
    assertEquals(Some(Map(1 -> 1000, 2 -> 20, 7 -> 100000)), sav6.S_AValue.get(s1))
    assertEquals(Some(7), sav6.bestAction.get(s1))
    
  }
}