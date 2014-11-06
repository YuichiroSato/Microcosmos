package jp.satoyuichiro.microcosmos.model.bio

import java.awt.Color
import jp.satoyuichiro.microcosmos.model.World

abstract class Bio(val coordinates: Coordinates, val appearance: Appearance) {

  def update(world: World): Bio
}

case class Appearance(val size: Int, val color: Color)
case class Coordinates(val x: Int, val y: Int, val angle: Double)