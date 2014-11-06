package jp.satoyuichiro.microcosmos.model.bio

case class Plant(override val coordinates: Coordinates, override val appearance: Appearance) extends Bio(coordinates, appearance) {

  def update: Bio = this
}