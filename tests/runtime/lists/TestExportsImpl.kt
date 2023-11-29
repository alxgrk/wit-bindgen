package wit.worlds

import wit.worlds.Lists.Tuple2
import wit.worlds.ListsImpl.expect

object TestExportsImpl {
    fun emptyListParam(a: UByteArray) {
        expect(a.isEmpty())
    }

    fun emptyStringParam(a: String) {
        expect(a.isEmpty())
    }

    fun emptyListResult(): UByteArray = UByteArray(0)

    fun emptyStringResult(): String = ""

    fun listParam(a: UByteArray) {
        expect(a.size == 4)
        expect(a[0].toInt() == 1)
        expect(a[1].toInt() == 2)
        expect(a[2].toInt() == 3)
        expect(a[3].toInt() == 4)
    }

    fun listParam2(a: String) {
        expect(a == "foo")
    }

    fun listParam3(a: ArrayList<String>) {
        expect(a.size == 3)
        expect(a[0] == "foo")
        expect(a[1] == "bar")
        expect(a[2] == "baz")
    }

    fun listParam4(a: ArrayList<ArrayList<String>>) {
        expect(a.size == 2)
        expect(a[0].size == 2)
        expect(a[1].size == 1)
        expect(a[0][0] == "foo")
        expect(a[0][1] == "bar")
        expect(a[1][0] == "baz")
    }

    fun listResult(): UByteArray = ubyteArrayOf(1.toUByte(), 2.toUByte(), 3.toUByte(), 4.toUByte(), 5.toUByte())

    fun listResult2(): String = "hello!"

    fun listResult3(): ArrayList<String> = arrayListOf("hello,", "world!")

    fun listRoundtrip(a: UByteArray): UByteArray = a

    fun stringRoundtrip(a: String): String = a

    fun listMinmax8(a: UByteArray, b: ByteArray): Tuple2<UByteArray, ByteArray> = Tuple2(a, b)

    fun listMinmax16(a: UShortArray, b: ShortArray): Tuple2<UShortArray, ShortArray> = Tuple2(a, b)

    fun listMinmax32(a: UIntArray, b: IntArray): Tuple2<UIntArray, IntArray> = Tuple2(a, b)

    fun listMinmax64(a: ULongArray, b: LongArray): Tuple2<ULongArray, LongArray> = Tuple2(a, b)

    fun listMinmaxFloat(a: FloatArray, b: DoubleArray): Tuple2<FloatArray, DoubleArray> = Tuple2(a, b)
}
