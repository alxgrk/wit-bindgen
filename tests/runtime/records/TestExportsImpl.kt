package wit.worlds

import wit.worlds.Records.Tuple1
import wit.worlds.Records.Tuple2
import wit.worlds.Records.Tuple4
import wit.worlds.TestExports.F1
import wit.worlds.TestExports.F2
import wit.worlds.TestExports.R1
import wit.worlds.TestExports.Flag8
import wit.worlds.TestExports.Flag16
import wit.worlds.TestExports.Flag32
import wit.worlds.TestExports.Flag64

object TestExportsImpl {
    fun multipleResults(): Tuple2<UByte, UShort> {
        return Tuple2(100.toUByte(), 200.toUShort())
    }

    fun swapTuple(tuple: Tuple2<UByte, UInt>): Tuple2<UInt, UByte> {
        return Tuple2(tuple.f1, tuple.f0)
    }

    fun roundtripFlags1(a: F1): F1 {
        return a
    }

    fun roundtripFlags2(a: F2): F2 {
        return a
    }

    fun roundtripFlags3(
        a: Flag8,
        b: Flag16,
        c: Flag32,
        d: Flag64
    ): Tuple4<Flag8, Flag16, Flag32, Flag64> {
        return Tuple4(a, b, c, d)
    }

    fun roundtripRecord1(a: R1): R1 {
        return a
    }

    fun tuple1(a: Tuple1<UByte>): Tuple1<UByte> {
        return a
    }
}
