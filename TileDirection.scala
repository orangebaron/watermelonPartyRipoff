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
