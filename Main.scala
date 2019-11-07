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
			board.walkTick
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
		val board = new Board(Point(5, 5), melons, Tile.randomTile, 1)
		bigLoop(board)
	}
}
