class Game(val boards: List[Board]) {
	def walkTick =
		for (boardNum <- 0 to boards.size) {
			val board = boards(boardNum)
			board.walkTick
			if (board.watermelons.size == 0 && !places.contains(boardNum)) {
				places = places + (boardNum -> (
					if (places.size == 0)
						1
					else
						places.max._2 + 1
				))
			}
		}
	private var places: Map[Int, Int] = Map()
	private var countdown: Int = -1
	def isCountdown = (countdown != -1)
	def isGameOver = (countdown == 0)
	def countdownVal = countdown
	private def fillRemainingPlaces = {
		var remainingBoards: Map[Int, Int] = Map()
		for (boardNum <- 0 to boards.size)
			if (!places.contains(boardNum))
				remainingBoards = remainingBoards + (boardNum -> boards(boardNum).watermelonsCollected)
		var lastMax = -1
		while (remainingBoards.size > 0) {
			val maxVal = remainingBoards.max
			remainingBoards = remainingBoards - maxVal._2
			places = places + (maxVal._1 -> (places.max._2 + (
				if (maxVal._2 == lastMax)
					0
				else
					1
			)))
			lastMax = maxVal._2
		}
	}
	private def decrementCountdown =
		if (countdown == -1)
			countdown = 5
		else if (countdown != 0)
			countdown -= 1
		if (countdown == 0)
			fillRemainingPlaces
	def countdownTick =
		if (places.size != 0)
			decrementCountdown
	override def toString = {
		var returnVal = ""
		for (board <- boards)
			returnVal += board
		returnVal + "Countdown: " + countdownVal + "\n"
	}
}
