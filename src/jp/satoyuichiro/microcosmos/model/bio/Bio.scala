package jp.satoyuichiro.microcosmos.model.bio

import java.awt.Color

abstract class Bio(val coordinates: Coordinates, val appearance: Appearance) {

  def update: Bio
}

case class Appearance(val size: Int, val color: Color)
case class Coordinates(val x: Int, val y: Int, val angle: Double)