object Main {
	def bigLoop(game: Game): Unit = {
		val board = game.boards(0)
		println(game)
		readLine() match {
		case "tile" => {
			board.placeTile(Point(readInt(), readInt()))
			bigLoop(game)
		}
		case "bomb" => {
			board.bombAroundLocation(Point(readInt(), readInt()))
			bigLoop(game)
		}
		case "walk" => {
			game.walkTick
			bigLoop(game)
		}
		case "countdown" => {
			game.countdownTick
			bigLoop(game)
		}
		case "exit" => ()
		case _ => {
			println("wadu")
			bigLoop(game)
		}
		}
	}
	def main(args: Array[String]) {
		val melons = Set(Point(), Point(3, 2), Point(2, 1))
		val board = new Board(Point(5, 5), melons, Tile.randomTile, 1)
		val game = new Game(List(board))
		bigLoop(game)
	}
}
