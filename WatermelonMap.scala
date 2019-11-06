class WatermelonMap(private var _watermelons: Set[Point]) {
	def watermelons = _watermelons
	private def watermelons_=(newVal: Set[Point]) { _watermelons = newVal }

	private var _watermelonsCollected: Int = 0
	def watermelonsCollected = _watermelonsCollected
	private def watermelonsCollected_=(newVal: Int) { _watermelonsCollected = newVal }

	def tryCollectWatermelon(playerLoc: Point) = if (watermelons contains playerLoc) {
		watermelonsCollected += 1
		watermelons = watermelons - playerLoc
	}

	override def toString = {
		var returnVal = ""
		returnVal += "watermelon locs: "
		for (pt <- watermelons) {
			returnVal += pt + " "
		}
		returnVal += "\n"
		returnVal += "watermelons collected: " + watermelonsCollected + "\n"
		returnVal
	}
}
