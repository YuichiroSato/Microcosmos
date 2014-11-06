package jp.satoyuichiro.microcosmos.model.bio

class Herbivore(override val coordinates: Coordinates, override val appearance: Appearance) extends Animal(coordinates, appearance) {

  def update: Bio = new Herbivore(coordinates, appearance)
}