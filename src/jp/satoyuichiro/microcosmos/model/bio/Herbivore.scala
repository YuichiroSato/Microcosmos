package jp.satoyuichiro.microcosmos.model.bio

import jp.satoyuichiro.microcosmos.model.World
import java.awt.Color
import jp.satoyuichiro.microcosmos.model.learning.Action
import jp.satoyuichiro.microcosmos.model.learning._
import scalaz._
import Scalaz._

case class Herbivore(override val external: External, override val internal: Internal, override val velocity: Velocity, override val learningInfo: LearningInfo)
  extends Animal(external, internal, velocity, learningInfo) {

  def evolve: Bio = setExternal(move, external.appearance).setLife(runningCost(velocity.speed) >>> existingCost)
  
  val runningCost = (speed: Double) => (life: Int) => life - speed.abs.toInt
  val existingCost = (life: Int) => life - 1

  def interact(world: World): World = chooseAction(giveBirthHerbivore(eatPlant(world)))
  
  val filterf = (bio: Bio) => bio.isInstanceOf[Plant]
  val updatef = () => setLife(_ + Herbivore.lifeUp)
  
  def eatPlant(world: World): World = {
    eat(world, filterf, updatef)
  }

  val condition = () => Herbivore.giveBirthLife < internal.life
  val born = () => Herbivore(external.coordinates.x, external.coordinates.y)
  val update2 = () => setLife(_ - Herbivore.giveBirthCost).incrementMakeBorn
  
  def giveBirthHerbivore(world: World) = {
    giveBirth(world, condition, born, update2)
  }
  
  def isDead: Boolean = internal.life <= 0
  
  def chooseAction(world: World): World = {
    if (Math.random() < 0.1) {
      herdCordedStrategy(world)
    } else {
      world
    }
  }
  
  def herdCordedStrategy(world: World) = {
    val opoc = findClosestPlantCarnivore(world)
    val herb = opoc match {
      case (Some(p: Plant), Some(c: Carnivore)) => runAway(c.external.coordinates)
      case (Some(p: Plant), None) => runInto(p.external.coordinates)
      case (None, Some(c: Carnivore)) => runAway(c.external.coordinates)
      case (None, None) => randomMove()
    }
    world.updateBio(this, herb)
  }
  
  def runAway(c: Coordinates): Herbivore = {
    val target = targetAngle(c)
    var v = this.velocity
    var e = this.external
    
    if (velocity.speed < 1.0) {
      v = v.speedUp(1.0)
    }
    if (target < 0) {
      e = e.setAngle(target + 3 * Math.PI)
    } else {
      e = e.setAngle(target + Math.PI)
    }
    this.setExternal(e).setVelocity(Velocity(v.speed, 0.0))
  }
  
  def runInto(c: Coordinates): Herbivore = {
    val target = targetAngle(c)
    var v = this.velocity
    var e = this.external
    
    if (velocity.speed < 1.0) {
      v = v.speedUp(1.0)
    }
    if (target < 0) {
      e = e.setAngle(target + 2 * Math.PI)
    } else {
      e = e.setAngle(target)
    }
    this.setExternal(e).setVelocity(Velocity(v.speed, 0.0))
  }
  
  def targetAngle(c: Coordinates): Double = {
    val dx = c.x - external.coordinates.x
    val dy = -c.y + external.coordinates.y
    Math.atan2(dy, dx)
  }
  
  def randomMove(): Herbivore = {
    (Math.random() * 20).toInt match {
	  case 0 => this.setVelocity(Velocity(0, velocity.rotation))
	  case 1 => this.setVelocity(velocity.speedUp(2.0))
	  case 2 => this.setVelocity(velocity.speedUp(-1.0))
	  case 3 => this.setVelocity(velocity.speedUp(0.5).rotate(0.1))
	  case 4 => this.setVelocity(velocity.speedUp(0.5).rotate(-0.1))
      case _ => this
    }
  }
  
  def chooseActionWithLearning(world: World): World = {
    if (learningInfo.count < 0) {
      if (learningInfo.learning) {
        val subWorld = world.getSubWorldAround(this, 80, 80)
        val action = HerbivoreQlearning.action(subWorld, this)
        val nextVelocity = Action.herbivoreAction(action, velocity)
        val nextLearningInfo = learningInfo.nextLearningInfo(Herbivore.learningInterval, subWorld, this, action)
        val herb = new Herbivore(external, internal, nextVelocity, nextLearningInfo)
        HerbivoreQlearning.learn(learningInfo.animal.asInstanceOf[Herbivore], herb)
        world.updateBio(this, herb)
      } else {
        val subWorld = world.getSubWorldAround(this, 80, 80)
        val action = HerbivoreQlearning.getBestAction(subWorld, this)
        val nextVelocity = Action.herbivoreAction(action, velocity)
        val nextLearningInfo = learningInfo.nextLearningInfo(Herbivore.workingInterval, subWorld, this, action)
        val herb = Herbivore(external, internal, nextVelocity, nextLearningInfo)
        world.updateBio(this, herb)
      }
    } else {
      world.updateBio(this, copy(learningInfo = this.learningInfo.decriment))
    }
  }
  
  val maxSearchWidth = 100
  type PCPair = (Option[Plant], Option[Carnivore])
  
  def findClosestPlantCarnivore(world: World): PCPair = {
    var res = (None: Option[Plant], None: Option[Carnivore])
    val x = external.coordinates.x
    val y = external.coordinates.y
    for (i <- (1 to maxSearchWidth)) {
      for (j <- (-i to i)) {
        res = pickUpPlantCarnivore(world, x - i, y + j, res)
        res = pickUpPlantCarnivore(world, x + i, y + j, res)
        res = pickUpPlantCarnivore(world, x + j, y - i, res)
        res = pickUpPlantCarnivore(world, x + j, y + i, res)
        res match {
          case (Some(p), Some(c)) => return res
          case _ =>
        }
      }
    }
    res
  }
  
  def pickUpPlantCarnivore(world: World, x: Int, y: Int, result: PCPair): PCPair = {
    var op = None: Option[Plant]
    var oc = None: Option[Carnivore]
    world.getCell(x, y) match {
      case Some(c) =>
        for (b <- c.bios) yield {
          if (b.isInstanceOf[Plant])
            op = Some(b.asInstanceOf[Plant])
          else if (b.isInstanceOf[Carnivore])
            oc = Some(b.asInstanceOf[Carnivore])
        }
      case None =>
    }
    result match {
      case (Some(p), Some(c)) => result
      case (Some(p), None) => (Some(p), oc)
      case (None, Some(c)) => (op, Some(c))
      case (None, None) => (op, oc)
    }
  }
  
  def setExternal(e: External): Herbivore = copy(external = e)
  def setExternal(c: Coordinates, a: Appearance): Herbivore = copy(external = External(c, a))
  def setInternal(l: Int, w: Int, m: Int): Herbivore = copy(internal = Internal(l, w, m))
  def setLife(l: Int): Herbivore = copy(internal = Internal(l, internal.water, internal.mineral))
  def setLife(f: Int => Int): Herbivore = setLife(f(internal.life))
  def setVelocity(v: Velocity): Herbivore = copy(velocity = v)
  def incrementMakeBorn(): Herbivore = copy(learningInfo = this.learningInfo.incrementMakeBorn)
  def setLearningTrue: Herbivore = copy(learningInfo = learningInfo.setLearningTrue)
}

object Herbivore {
    
  val lifeUp = 800
  val giveBirthLife = 1000
  val initLife = 100
  val giveBirthCost = 500
  val learningInterval = 10
  val workingInterval = 2
  
  def apply(x: Int, y: Int): Herbivore = {
    val coordinates = Coordinates(x,y, Math.random)
    val appearance = Appearance(12, Color.BLUE)
    val velocity = Velocity(10 * Math.random(), Math.random() - 0.5)
    val learningInfo = LearningInfo((learningInterval * Math.random()).toInt, World.empty, Herbivore.empty, Action.maxValue, 0)
    Herbivore(External(coordinates, appearance), Internal(initLife, 10, 10), velocity, learningInfo)
  }
  
  def learning(x: Int, y: Int): Herbivore = apply(x, y).setLearningTrue

  def empty: Herbivore = {
    val external = External(Coordinates(0,0,0), Appearance(0, Color.RED))
    val internal = Internal(initLife, 0, 0)
    val velocity = Velocity(0, 0)
    val learningInfo0 = LearningInfo(learningInterval, World.empty, null, Action.maxValue, 0)
    val animal = Herbivore(external, internal, velocity, learningInfo0)
    val learningInfo1 = LearningInfo(learningInterval, World.empty, animal, Action.maxValue, 0)
    Herbivore(external, internal, velocity,  learningInfo1)
  }
}