package jp.satoyuichiro.microcosmos.model.bio

import jp.satoyuichiro.microcosmos.model.World
import java.awt.Color
import jp.satoyuichiro.microcosmos.model.learning.Action
import jp.satoyuichiro.microcosmos.model.learning._

import scalaz._
import Scalaz._

case class Carnivore(override val external: External, override val internal: Internal, override val velocity: Velocity, val count: Int)
  extends Animal(external, internal, velocity) {

  def evolve: Bio = setExternal(move, external.appearance).setLife(runningCost(velocity.speed) >>> existingCost)
  
  val runningCost = (speed: Double) => (life: Int) => if (0 < speed) life - speed.toInt else  life - Math.pow(speed, 2).toInt
  val existingCost = (life: Int) => life - 1

  def interact(world: World): World = chooseAction(giveBirthCarnivore(eatHervibore(world)))

  val filterf = (bio: Bio) => bio.isInstanceOf[Herbivore]
  val updatef = () => setLife(_ + Carnivore.lifeUp)

  def eatHervibore(world: World): World = {
    eat(world, filterf, updatef)
  }

  val condition = () => Carnivore.giveBirthLife < internal.life
  val born = () => Carnivore(external.coordinates.x, external.coordinates.y)
  val update2 = () => setLife(_ - Carnivore.giveBirthCost)

  def giveBirthCarnivore(world: World) = {
    giveBirth(world, condition, born, update2)
  }

  def isDead: Boolean = internal.life <= 0

  val maxCount = Carnivore.maxCount
  
  def chooseAction(world: World): World = {
    if (count < 0) {
      val nextState = Carnivore.nextState(world, this)
      if (this == nextState) {
        world
      } else {
        world.updateBio(this, nextState.repareCounter)
      }
    } else {
      world.updateBio(this, decrimentCounter)
    }
  }
  
  def setExternal(e: External): Carnivore = copy(external = e)
  def setExternal(c: Coordinates, a: Appearance): Carnivore = copy(external = External(c, a))
  def setInternal(l: Int, w: Int, m: Int): Carnivore = copy(internal = Internal(l, w, m))
  def setLife(l: Int): Carnivore = copy(internal = Internal(l, internal.water, internal.mineral))
  def setLife(f: Int => Int): Carnivore = setLife(f(internal.life))
  def setVelocity(v: Velocity): Carnivore = copy(velocity = v)
  def decrimentCounter: Carnivore = copy(count = count - 1)
  def repareCounter: Carnivore = copy(count = maxCount)
  
}

object Carnivore {

  val lifeUp = 2000
  val giveBirthLife = 5000
  val initLife = 500
  val giveBirthCost = 4000
  val learningInterval = 10
  val workingInterval = 2
  val maxCount = 5

  def apply(x: Int, y: Int): Carnivore = {
    val coordinates = Coordinates(x, y, Math.random())
    val appearance = Appearance(12, Color.RED)
    val velocity = Velocity(5 * Math.random(), Math.random() - 0.5)
    Carnivore(External(coordinates, appearance), Internal(initLife, 10, 10), velocity, maxCount)
  }
  
  def empty: Carnivore = {
    val external = External(Coordinates(0,0,0), Appearance(0, Color.RED))
    val internal = Internal(initLife, 0, 0)
    val velocity = Velocity(0, 0)
    Carnivore(external, internal, velocity, maxCount)
  }
  
  var strategy: Strategy[Carnivore] = new EmptyStrategy()
  
  def setStrategy(s: Strategy[Carnivore]): Unit = {
    strategy = s
  }
  
  val eyesight = 80
  
  def getSubWorld(world: World, carn: Carnivore): World = {
    world.getSubWorldAround(carn, eyesight, eyesight)
  }
  
  def nextState(world: World, carn: Carnivore): Carnivore = {
    val subWorld = getSubWorld(world, carn)
    val action = strategy.chooseAction(subWorld, carn)
    val velocity = Carnivore.action(action, carn.velocity)
    carn.setVelocity(velocity)
  }
  
  val doNothing = 7
  
  def randomAction: Int = {
    (Math.random() * doNothing).toInt + 1
  }
  
  def action(i: Int, velocity: Velocity): Velocity = {
    i match {
      case 1 => Velocity(velocity.speed + 3.0, velocity.rotation)
      case 2 => Velocity(velocity.speed - 3.0, velocity.rotation)
      case 3 => Velocity(velocity.speed, velocity.rotation + 0.1)
      case 4 => Velocity(velocity.speed, velocity.rotation - 0.1)
      case 5 => Velocity(velocity.speed, 0.0)
      case 6 => Velocity(0.0, 0.0)
      case _ => velocity
    }
  }
}
