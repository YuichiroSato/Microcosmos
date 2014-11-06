package jp.satoyuichiro.microcosmos.model.bio

import jp.satoyuichiro.microcosmos.model.World

case class Plant(override val coordinates: Coordinates, override val appearance: Appearance) extends Bio(coordinates, appearance) {

  def update(world: World): Bio = this
}