package wit.worlds

import wit.worlds.TestImports.getScalar
import wit.worlds.TestImports.roundtripChar
import wit.worlds.TestImports.roundtripFloat32
import wit.worlds.TestImports.roundtripFloat64
import wit.worlds.TestImports.roundtripS16
import wit.worlds.TestImports.roundtripS32
import wit.worlds.TestImports.roundtripS32
import wit.worlds.TestImports.roundtripS64
import wit.worlds.TestImports.roundtripS8
import wit.worlds.TestImports.roundtripU16
import wit.worlds.TestImports.roundtripU32
import wit.worlds.TestImports.roundtripU64
import wit.worlds.TestImports.roundtripU8
import wit.worlds.TestImports.setScalar

object NumbersImpl {
    private fun expect(v: Boolean) {
        if (!v) {
            throw AssertionError()
        }
    }

    fun testImports() {
        expect(roundtripU8(1.toUByte()) == 1.toUByte())
        expect(roundtripU8(0.toUByte()) == 0.toUByte())
        expect(roundtripU8(0xFF.toUByte()) == 0xFF.toUByte())
        expect(roundtripS8(1.toByte()) == 1.toByte())
        expect(roundtripS8(Byte.MIN_VALUE) == Byte.MIN_VALUE)
        expect(roundtripS8(Byte.MAX_VALUE) == Byte.MAX_VALUE)
        expect(roundtripU16(1.toUShort()) == 1.toUShort())
        expect(roundtripU16(0.toUShort()) == 0.toUShort())
        expect(roundtripU16(0xFFFF.toUShort()) == 0xFFFF.toUShort())
        expect(roundtripS16(1.toShort()) == 1.toShort())
        expect(roundtripS16(Short.MIN_VALUE) == Short.MIN_VALUE)
        expect(roundtripS16(Short.MAX_VALUE) == Short.MAX_VALUE)
        expect(roundtripU32(1u) == 1u)
        expect(roundtripU32(0u) == 0u)
        expect(roundtripU32((-0x1).toUInt()) == (-0x1).toUInt())
        expect(roundtripS32(1) == 1)
        expect(roundtripS32(Int.MIN_VALUE) == Int.MIN_VALUE)
        expect(roundtripS32(Int.MAX_VALUE) == Int.MAX_VALUE)
        expect(roundtripU64(1uL) == 1uL)
        expect(roundtripU64(0uL) == 0uL)
        expect(roundtripU64((-0x1L).toULong()) == (-0x1L).toULong())
        expect(roundtripS64(1L) == 1L)
        expect(roundtripS64(Long.MIN_VALUE) == Long.MIN_VALUE)
        expect(roundtripS64(Long.MAX_VALUE) == Long.MAX_VALUE)
        expect(roundtripFloat32(1.0f) == 1.0f)
        expect(roundtripFloat32(Float.POSITIVE_INFINITY) == Float.POSITIVE_INFINITY)
        expect(roundtripFloat32(Float.NEGATIVE_INFINITY) == Float.NEGATIVE_INFINITY)
        expect(roundtripFloat32(Float.NaN).isNaN())
        expect(roundtripFloat64(1.0) == 1.0)
        expect(roundtripFloat64(Double.POSITIVE_INFINITY) == Double.POSITIVE_INFINITY)
        expect(roundtripFloat64(Double.NEGATIVE_INFINITY) == Double.NEGATIVE_INFINITY)
        expect(roundtripFloat64(Double.NaN).isNaN())
        expect(roundtripChar("a") == "a")
        expect(roundtripChar(" ") == " ")
        expect(roundtripChar("🚩") == "🚩")
        setScalar(2u)
        expect(getScalar() == 2u)
        setScalar(4u)
        expect(getScalar() == 4u)
    }

}
