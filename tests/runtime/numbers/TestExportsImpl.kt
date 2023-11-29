package wit.worlds

object TestExportsImpl {
    fun roundtripU8(a: UByte): UByte = a

    fun roundtripS8(a: Byte): Byte = a

    fun roundtripU16(a: UShort): UShort = a

    fun roundtripS16(a: Short): Short = a

    fun roundtripU32(a: UInt): UInt = a

    fun roundtripS32(a: Int): Int = a

    fun roundtripU64(a: ULong): ULong = a

    fun roundtripS64(a: Long): Long = a

    fun roundtripFloat32(a: Float): Float = a

    fun roundtripFloat64(a: Double): Double = a

    fun roundtripChar(a: String): String = a

    var scalar = 0u

    fun setScalar(a: UInt) {
        scalar = a
    }

    fun getScalar(): UInt = scalar
}
