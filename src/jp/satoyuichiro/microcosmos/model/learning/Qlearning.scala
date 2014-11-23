package jp.satoyuichiro.microcosmos.model.learning

import jp.satoyuichiro.microcosmos.model.bio.Velocity

object Qlearning {

}

case class Qvalue

object Action {
  
  def carnivorAction(i: Int, velocity: Velocity): Velocity = {
    i match {
      case 1 => Velocity(velocity.speed + 1.0, velocity.rotation)
      case 2 => Velocity(velocity.speed - 1.0, velocity.rotation)
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