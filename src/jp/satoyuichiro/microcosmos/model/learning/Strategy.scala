package jp.satoyuichiro.microcosmos.model.learning

import jp.satoyuichiro.microcosmos.model.bio.Animal
import jp.satoyuichiro.microcosmos.model.World
import jp.satoyuichiro.microcosmos.model.bio.Carnivore

trait Strategy[A <: Animal] {

  def chooseAction(subWorld: World, a: A): Int
}

class EmptyStrategy extends Strategy[Carnivore] {
  
  def chooseAction(subWorld: World, carn: Carnivore): Int = Carnivore.doNothing
}