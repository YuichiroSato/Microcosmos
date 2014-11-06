package jp.satoyuichiro.microcosmos.model.bio

case class Carnivore(override val coordinates: Coordinates, override val appearance: Appearance) extends Animal(coordinates, appearance) {

  def update: Bio = new Carnivore(Coordinates(coordinates.x + 10, coordinates.y, coordinates.angle), appearance)
}