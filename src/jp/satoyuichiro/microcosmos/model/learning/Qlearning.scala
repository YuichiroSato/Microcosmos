package jp.satoyuichiro.microcosmos.model.learning

import jp.satoyuichiro.microcosmos.model.bio.Velocity
import jp.satoyuichiro.microcosmos.model.World

object Qlearning {

  var carnivoreQ = 0
  var herbivoreQ = 0
  
  def herbivoreLearn(): Unit = {
    
  }
  
  def carnivoreLearn(): Unit = {
    
  }
  
  def herbivoreAction(subWorld: World, velocity: Velocity): Velocity = {
    Action.herbivoreAction((8 * Math.random()).toInt, velocity)
  }
  
  def carnivoreAction(subWorld: World, velocity: Velocity): Velocity = {
    Action.carnivorAction((8 * Math.random()).toInt, velocity)
  }
}

case class Qvalue

object Action {
  
  def carnivorAction(i: Int, velocity: Velocity): Velocity = {
    i match {
      case 1 => Velocity(velocity.speed + 3.0, velocity.rotation)
      case 2 => Velocity(velocity.speed - 3.0, velocity.rotation)
      case 3 => Velocity(velocity.speed, velocity.rotation + 0.1)
      case 4 => Velocity(velocity.speed, velocity.rotation - 0.1)
      case 5 => Velocity(velocity.speed, 0.0)
      case 6 => Velocity(0.0, 0.0)
      case _ => velocity
    }
  }
  
  def herbivoreAction(i: Int, velocity: Velocity): Velocity = {
    i match {
      case 1 => Velocity(velocity.speed + 0.7, velocity.rotation)
      case 2 => Velocity(velocity.speed - 0.7, velocity.rotation)
      case 3 => Velocity(velocity.speed, velocity.rotation + 0.1)
      case 4 => Velocity(velocity.speed, velocity.rotation - 0.1)
      case 5 => Velocity(velocity.speed, 0.0)
      case 6 => Velocity(0.0, 0.0)
      case _ => velocity
    }
  }
}