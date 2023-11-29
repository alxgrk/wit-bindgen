package wit.worlds

import wit.worlds.Variants.Tuple3
import wit.worlds.Variants.Tuple4
import wit.worlds.Variants.Tuple6
import wit.worlds.Variants.Result

object TestExportsImpl {
    fun roundtripOption(a: Float?): UByte? {
        return a?.toInt()?.toUByte()
    }

    fun roundtripResult(a: Result<UInt, Float>): Result<Double, UByte> {
        return when (a) {
            is Result.Ok -> Result.ok(a.value.toDouble())
            is Result.Err -> Result.err(a.value.toInt().toUByte())
            else -> throw AssertionError()
        }
    }

    fun roundtripEnum(a: TestExports.E1): TestExports.E1 {
        return a
    }

    fun invertBool(a: Boolean): Boolean {
        return !a
    }

    fun variantCasts(a: Tuple6<TestExports.C1, TestExports.C2, TestExports.C3, TestExports.C4, TestExports.C5, TestExports.C6>): Tuple6<TestExports.C1, TestExports.C2, TestExports.C3, TestExports.C4, TestExports.C5, TestExports.C6> {
        return a
    }

    fun variantZeros(a: Tuple4<TestExports.Z1, TestExports.Z2, TestExports.Z3, TestExports.Z4>): Tuple4<TestExports.Z1, TestExports.Z2, TestExports.Z3, TestExports.Z4> {
        return a
    }

    fun variantTypedefs(a: UInt?, b: Boolean, c: Result<UInt, Unit>) {}
    fun variantEnums(
        a: Boolean,
        b: Result<Unit, Unit>,
        c: TestExports.MyErrno
    ): Tuple3<Boolean, Result<Unit, Unit>, TestExports.MyErrno> {
        return Tuple3(a, b, c)
    }
}
