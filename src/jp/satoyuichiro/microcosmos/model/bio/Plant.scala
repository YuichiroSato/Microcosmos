package jp.satoyuichiro.microcosmos.model.bio

import jp.satoyuichiro.microcosmos.model.World
import java.awt.Color
import jp.satoyuichiro.microcosmos.model.Cell

import scalaz._
import Scalaz._

case class Plant(override val external: External, override val internal: Internal) extends Bio(external, internal) {

  def update(world: World): World = Plant.update(this, world)
  
  def evolve: Bio = this
  
  def interact(world: World): World = {
    if (Math.random() < 0.001) {
      val x = external.coordinates.x
      val y = external.coordinates.y
      val w = 40
      val subWorld = world.getSubWorld(x, y, w, w)
      val plants = subWorld.getBios filter (_.isInstanceOf[Plant])
      if (5 < plants.size) {
        if (Math.random() < 0.01) {
          val x = (external.coordinates.x + 200 * Math.random()).toInt 
          val y = (external.coordinates.y + 200 * Math.random()).toInt
          giveBirthPlant(world, x, y)
        }
        world
      }
      else {
        val x = (external.coordinates.x + 50 * Math.random() - 25).toInt 
        val y = (external.coordinates.y + 50 * Math.random() - 25).toInt
        giveBirthPlant(world, x, y)
      }
    }
    else {
      world
    }
  }
  
  def giveBirthPlant(world: World, x: Int, y: Int): World = {
    val xy = world.boundaryCondition(Coordinates(x,y,0.0))
    world.add(Plant(xy.x, xy.y))
  }
  
  def isDead: Boolean = internal.life <= 0
  
}

object Plant {
  
  def apply(x: Int, y: Int): Plant = {
    new Plant(External(Coordinates(x,y, 0.0), Appearance(10, Color.GREEN)), Internal(100, 10, 10))
  }
  
  def update(plant: Plant, world: World): World = {
    if (alive(plant, world))
      doUpdate(plant).exec(world)
    else
      world
  }
    
  def doUpdate(plant: Plant): State[World, Unit] = for {
    p1 <- evolve(plant)
    p2 <- interact(p1)
    u <- set(p2.some)
  } yield u
  
  def alive(plant: Plant, world: World): Boolean = world.plants.contains(plant) 
  
  def evolve(plant: Plant) = State[World, Plant] {
    world => (world.remove(plant), plant)
  }
  
  def interact(plant: Plant): State[World, Plant] = for {
    p1 <- birthPlant(plant)
    u <- set(p1)
    p2 <- State.gets[World, Plant](_ => plant)
  } yield p2
  
  def birthPlant(plant: Plant) = State[World, Option[Plant]] {
    world =>
      if (condition1) {
        if (condition2(plant, world)) {
          (world, makePlantAround(plant, 200, world).some)
        } else
          (world, makePlantAround(plant, 50, world).some)
      } else
        (world, None)
  }
  
  def condition1(): Boolean = Math.random() < 0.001
  
  def condition2(plant: Plant, world: World): Boolean = {
    val x = plantX get plant
    val y = plantY get plant
    val w = 40
    val subWorld = world.getSubWorld(x, y, w, w)
    val plants = subWorld.getBios collect { case p: Plant => p }
    5 < plants.size
  }
  
  def makePlantAround(plant: Plant, width: Int, world: World): Plant = {
    val fix = ((_:Int) + width * Math.random() - width / 2) >>> (_.toInt)
    val xy = plantXY.get(plant) |> (fix *** fix)
    val co = world.boundaryCondition(Coordinates(xy._1, xy._2, 0.0))
    Plant(co.x, co.y)
  }
  
  def set(plant: Option[Plant]) = State[World, Unit] {
    world => plant match {
      case Some(p) => world.append(p)
      case None =>
    }
    (world, ())
  }
  
  val plantExternal = Lens.lensu[Plant, External]((plant, ex) => plant.copy(external = ex), _.external)
  val plantX = plantExternal >=> External.coordinateX
  val plantY = plantExternal >=> External.coordinateY
  val plantXY = Lens.lensu[Plant, (Int, Int)]((plant, xy) => plantX.set(plant, xy._1), plant => (plantX.get(plant), plantY.get(plant)))
}

 