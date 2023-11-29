package wit.worlds

import wit.worlds.TestImports.listMinmax16
import wit.worlds.TestImports.listMinmax32
import wit.worlds.TestImports.listMinmax64
import wit.worlds.TestImports.listMinmax8
import wit.worlds.TestImports.listMinmaxFloat
import wit.worlds.TestImports.listResult
import wit.worlds.TestImports.listResult2
import wit.worlds.TestImports.listResult3
import wit.worlds.TestImports.listRoundtrip
import wit.worlds.TestImports.stringRoundtrip
import wit.worlds.Lists.Tuple2

object ListsImpl {
    fun allocatedBytes(): UInt {
        return 0u
    }

    fun testImports() {
        run {
            val result: UByteArray = listResult()
            expect(result.size == 5)
            expect(result[0] == 1.toUByte())
            expect(result[1] == 2.toUByte())
            expect(result[2] == 3.toUByte())
            expect(result[3] == 4.toUByte())
            expect(result[4] == 5.toUByte())
        }
        run {
            val result: String = listResult2()
            expect(result == "hello!")
        }
        run {
            val result: ArrayList<String> = listResult3()
            expect(result.size == 2)
            expect(result[0] == "hello,")
            expect(result[1] == "world!")
        }
        for (s in arrayOf<String>("x", "", "hello", "hello ⚑ world")) {
            val result: String = stringRoundtrip(s)
            expect(result == s)
            val bytes: UByteArray = s.encodeToByteArray().toUByteArray()
            expect(bytes.contentEquals(listRoundtrip(bytes)))
        }
        run {
            val result: Tuple2<UByteArray, ByteArray> = listMinmax8(
                ubyteArrayOf(0.toUByte(), 0xFF.toUByte()),
                byteArrayOf(0x80.toByte(), 0x7F.toByte())
            )
            expect(result.f0.size == 2 && result.f0[0] == 0.toUByte() && result.f0[1] == 0xFF.toUByte())
            expect(result.f1.size == 2 && result.f1[0] == 0x80.toByte() && result.f1[1] == 0x7F.toByte())
        }
        run {
            val result: Tuple2<UShortArray, ShortArray> = listMinmax16(
                ushortArrayOf(0.toUShort(), 0xFFFF.toUShort()),
                shortArrayOf(0x8000.toShort(), 0x7FFF.toShort())
            )
            expect(result.f0.size == 2 && result.f0[0] == 0.toUShort() && result.f0[1] == 0xFFFF.toUShort())
            expect(
                result.f1.size == 2 && result.f1[0] == 0x8000.toShort() && result.f1[1] == 0x7FFF.toShort()
            )
        }
        run {
            val result: Tuple2<UIntArray, IntArray> =
                listMinmax32(uintArrayOf(0u, (-0x1).toUInt()), intArrayOf(-0x80000000, 0x7FFFFFFF))
            expect(result.f0.size == 2 && result.f0[0] == 0u && result.f0[1] == (-0x1).toUInt())
            expect(result.f1.size == 2 && result.f1[0] == -0x80000000 && result.f1[1] == 0x7FFFFFFF)
        }
        run {
            val result: Tuple2<ULongArray, LongArray> = listMinmax64(
                ulongArrayOf(0u, (-0x1L).toULong()), longArrayOf(
                    (0x8000000000000000uL).toLong(), 0x7FFFFFFFFFFFFFFFL
                )
            )
            expect(
                result.f0.size == 2 && result.f0[0] == 0uL && result.f0[1] == (-0x1L).toULong()
            )
            expect(
                result.f1.size == 2 && result.f1[0] == (0x8000000000000000uL).toLong() && result.f1[1] == 0x7FFFFFFFFFFFFFFFL
            )
        }
        run {
            val result: Tuple2<FloatArray, DoubleArray> = listMinmaxFloat(
                floatArrayOf(
                    -Float.MAX_VALUE,
                    Float.MAX_VALUE,
                    Float.NEGATIVE_INFINITY,
                    Float.POSITIVE_INFINITY
                ), doubleArrayOf(
                    -Double.MAX_VALUE,
                    Double.MAX_VALUE,
                    Double.NEGATIVE_INFINITY,
                    Double.POSITIVE_INFINITY
                )
            )
            expect(
                result.f0.size == 4 && result.f0[0] == -Float.MAX_VALUE && result.f0[1] == Float.MAX_VALUE && result.f0[2] == Float.NEGATIVE_INFINITY && result.f0[3] == Float.POSITIVE_INFINITY
            )
            expect(
                result.f1.size == 4 && result.f1[0] == -Double.MAX_VALUE && result.f1[1] == Double.MAX_VALUE && result.f1[2] == Double.NEGATIVE_INFINITY && result.f1[3] == Double.POSITIVE_INFINITY
            )
        }
    }

    fun expect(v: Boolean) {
        if (!v) {
            throw AssertionError()
        }
    }
}
