package jp.satoyuichiro.microcosmos.model.bio

import java.awt.Color
import jp.satoyuichiro.microcosmos.model.World

abstract class Bio(val external: External, val internal: Internal) {

  def evolve: Bio
  def interact(world: World): World
  def isDead: Boolean
}

case class External(val coordinates: Coordinates, val appearance: Appearance)
case class Appearance(val size: Int, val color: Color)
case class Coordinates(val x: Int, val y: Int, val angle: Double)
case class Internal(val life: Int, val water: Int, val mineral: Int)