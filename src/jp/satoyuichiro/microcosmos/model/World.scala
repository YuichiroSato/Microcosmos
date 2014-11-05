package jp.satoyuichiro.microcosmos.model

class World(val x: Int, val y: Int) {

  def update: World = {
    new World((x + 10) % 400, (y + 10) % 400)
  }
}