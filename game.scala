import scala.util.Random

case class Point(x: Int = 0, y: Int = 0) {
	def +(a: Point) = Point(x + a.x, y + a.y)
	def inRange(max: Point) =
		x >= 0 &&
		y >= 0 &&
		x < max.x &&
		y < max.y
	override def toString = "(" + x + "," + y + ")"
}

case class TileDirection(horizontal: Boolean, positive: Boolean) {
	def unary_- = TileDirection(horizontal, !positive)
	def asPoint = {
		val num = if (positive) 1 else -1
		Point(
			if (horizontal) num else 0,
			if (horizontal) 0 else num
		)
	}
}

object TileDirection {
	val Up = TileDirection(false, false)
	val Down = TileDirection(false, true)
	val Left = TileDirection(true, false)
	val Right = TileDirection(true, true)
}
case class Tile(a: TileDirection, b: TileDirection) {
	def takePlayer(playerFrom: TileDirection): Option[TileDirection] = // returns where the next direction to go to would be if player can go here
		if (playerFrom == -a) Some(b)
		else if (playerFrom == -b) Some(a)
		else None
	override def toString =
		if (a == TileDirection.Up && b == TileDirection.Down || a == TileDirection.Down && b == TileDirection.Up) "|"
		else if (a == TileDirection.Left && b == TileDirection.Right || a == TileDirection.Right && b == TileDirection.Left) "-"
		else if (a == TileDirection.Up && b == TileDirection.Right || a == TileDirection.Right && b == TileDirection.Up) "⌞"
		else if (a == TileDirection.Right && b == TileDirection.Down || a == TileDirection.Down && b == TileDirection.Right) "⌜"
		else if (a == TileDirection.Down && b == TileDirection.Left || a == TileDirection.Left && b == TileDirection.Down) "⌝"
		else if (a == TileDirection.Left && b == TileDirection.Up || a == TileDirection.Up && b == TileDirection.Left) "⌟"
		else "X"
}

object Tile {
	def randomTile(board: Board): Tile = {
		val tile = Tile(TileDirection(Random.nextBoolean, Random.nextBoolean), TileDirection(Random.nextBoolean, Random.nextBoolean))
		if (tile.toString != "X") tile else randomTile(board)
	}
}

class Board(size: Point, private var _watermelons: Set[Point], tileGenerator: Board => Tile) { //TODO: val needed?
	def watermelons = _watermelons
	private def watermelons_=(newVal: Set[Point]) { _watermelons = newVal }
	private var _tiles: Map[Point, Tile] = Map()
	def tiles = _tiles
	private def tiles_=(newVal: Map[Point, Tile]) { _tiles = newVal }
	private var _nextTile = tileGenerator(this)
	def nextTile = _nextTile
	private def nextTile_=(newVal: Tile) { _nextTile = newVal }
	private var _playerLoc = Point(y = -1)
	def playerLoc = _playerLoc
	private def playerLoc_=(newVal: Point) { _playerLoc = newVal }
	private var _playerFacing = TileDirection.Down
	def playerFacing = _playerFacing
	private def playerFacing_=(newVal: TileDirection) { _playerFacing = newVal }
	private var _watermelonsCollected: Int = 0
	def watermelonsCollected = _watermelonsCollected
	private def watermelonsCollected_=(newVal: Int) { _watermelonsCollected = newVal }
	def nextLoc = playerLoc + playerFacing.asPoint
	def nextFacing = for {
		nextTile <- tiles get nextLoc
		nextFacing <- nextTile.takePlayer(playerFacing)
	} yield nextFacing
	private def tryCollectWatermelon = if (watermelons contains playerLoc) {
		watermelonsCollected += 1
		watermelons = watermelons - playerLoc
	}
	def tryWalk = nextFacing match {
		case Some(dir) => {
			playerLoc = nextLoc
			playerFacing = dir
			tryCollectWatermelon
		}
		case _ => ()
	}
	private def resetPlayer = {
		playerLoc = Point(y = -1)
		playerFacing = TileDirection.Down
	}
	private def bombLocation(loc: Point) = {
		if (loc == playerLoc) resetPlayer
		tiles = tiles - loc
	}
	def bombAroundLocation(loc: Point) =
		for (x <- -1 to 1; y <- -1 to 1) bombLocation(loc + Point(x, y))
	def placeTile(loc: Point) = if (!(tiles contains loc) && loc.inRange(size)) {
		tiles = tiles + (loc -> nextTile)
		nextTile = tileGenerator(this)
	}
	override def toString = {
		var returnVal = ""
		for (y <- 0 until size.y) {
			for (x <- 0 until size.x) {
				returnVal += (tiles getOrElse (Point(x, y),  Tile(TileDirection.Up, TileDirection.Up)))
			}
			returnVal += "\n"
		}
		returnVal += "watermelon locs: "
		for (pt <- watermelons) {
			returnVal += pt + " "
		}
		returnVal += "\n"
		returnVal += "watermelons collected: " + watermelonsCollected + "\n"
		returnVal += "next tile: " + nextTile + "\n"
		returnVal += "player loc: " + playerLoc + "\n"
		returnVal += "player facing: " + playerFacing.asPoint + "\n"
		returnVal
	}
}

object Main {
	def bigLoop(board: Board): Unit = {
		println(board)
		readLine() match {
		case "tile" => {
			board.placeTile(Point(readInt(), readInt()))
			bigLoop(board)
		}
		case "bomb" => {
			board.bombAroundLocation(Point(readInt(), readInt()))
			bigLoop(board)
		}
		case "walk" => {
			board.tryWalk
			bigLoop(board)
		}
		case "exit" => ()
		case _ => {
			println("wadu")
			bigLoop(board)
		}
		}
	}
	def main(args: Array[String]) {
		val melons = Set(Point(), Point(3, 2), Point(2, 1))
		val board = new Board(Point(5, 5), melons, Tile.randomTile)
		bigLoop(board)
	}
}
