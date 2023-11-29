package wit.worlds

import wit.worlds.TestImports.C1
import wit.worlds.TestImports.C2
import wit.worlds.TestImports.C3
import wit.worlds.TestImports.C4
import wit.worlds.TestImports.C5
import wit.worlds.TestImports.C6
import wit.worlds.TestImports.E1
import wit.worlds.TestImports.Z1
import wit.worlds.TestImports.Z2
import wit.worlds.TestImports.Z3
import wit.worlds.TestImports.Z4
import wit.worlds.TestImports.MyErrno
import wit.worlds.TestImports.invertBool
import wit.worlds.TestImports.roundtripEnum
import wit.worlds.TestImports.roundtripOption
import wit.worlds.TestImports.roundtripResult
import wit.worlds.TestImports.variantCasts
import wit.worlds.TestImports.variantEnums
import wit.worlds.TestImports.variantTypedefs
import wit.worlds.TestImports.variantZeros
import wit.worlds.Variants.Result
import wit.worlds.Variants.Tuple3
import wit.worlds.Variants.Tuple4
import wit.worlds.Variants.Tuple6

object VariantsImpl {
    fun testImports() {
        expect(roundtripOption(1.0f) == 1.toUByte())
        expect(roundtripOption(null) == null)
        expect(roundtripOption(2.0f) == 2.toUByte())
        run {
            val result: Result<Double, UByte> = roundtripResult(Result.ok(2.toUInt()))
            expect(result is Result.Ok && result.value == 2.0)
        }
        run {
            val result: Result<Double, UByte> = roundtripResult(Result.ok(4.toUInt()))
            expect(result is Result.Ok && result.value == 4.0)
        }
        run {
            val result: Result<Double, UByte> = roundtripResult(Result.err(5.3f))
            expect(result is Result.Err && result.value == 5.toUByte())
        }
        expect(roundtripEnum(E1.A) == E1.A)
        expect(roundtripEnum(E1.B) == E1.B)
        expect(invertBool(true) == false)
        expect(invertBool(false) == true)
        run {
            val result: Tuple6<C1, C2, C3, C4, C5, C6> =
                variantCasts(
                    Tuple6(
                        C1.A(1),
                        C2.A(2),
                        C3.A(3),
                        C4.A(4L),
                        C5.A(5L),
                        C6.A(6.0f)
                    )
                )
            expect(result.f0 is C1.A && result.f0.value == 1)
            expect(result.f1 is C2.A && result.f1.value == 2)
            expect(result.f2 is C3.A && result.f2.value == 3)
            expect(result.f3 is C4.A && result.f3.value == 4L)
            expect(result.f4 is C5.A && result.f4.value == 5L)
            expect(result.f5 is C6.A && result.f5.value == 6.0f)
        }
        run {
            val result: Tuple6<C1, C2, C3, C4, C5, C6> =
                variantCasts(
                    Tuple6(
                        C1.B(1L),
                        C2.B(2.0f),
                        C3.B(3.0),
                        C4.B(4.0f),
                        C5.B(5.0),
                        C6.B(6.0)
                    )
                )
            expect(result.f0 is C1.B && result.f0.value == 1L)
            expect(result.f1 is C2.B && result.f1.value == 2.0f)
            expect(result.f2 is C3.B && result.f2.value == 3.0)
            expect(result.f3 is C4.B && result.f3.value == 4.0f)
            expect(result.f4 is C5.B && result.f4.value == 5.0)
            expect(result.f5 is C6.B && result.f5.value == 6.0)
        }
        run {
            val result: Tuple4<Z1, Z2, Z3, Z4> = variantZeros(
                Tuple4(
                    Z1.A(1),
                    Z2.A(2L),
                    Z3.A(3.0f),
                    Z4.A(4.0)
                )
            )
            expect(result.f0 is Z1.A && result.f0.value == 1)
            expect(result.f1 is Z2.A && result.f1.value == 2L)
            expect(result.f2 is Z3.A && result.f2.value == 3.0f)
            expect(result.f3 is Z4.A && result.f3.value == 4.0)
        }
        run {
            val result: Tuple4<Z1, Z2, Z3, Z4> = variantZeros(
                Tuple4(
                    Z1.B,
                    Z2.B,
                    Z3.B,
                    Z4.B
                )
            )
            expect(result.f0 is Z1.B)
            expect(result.f1 is Z2.B)
            expect(result.f2 is Z3.B)
            expect(result.f3 is Z4.B)
        }
        variantTypedefs(null, false, Result.err(Unit))
        run {
            val result: Tuple3<Boolean, Result<Unit, Unit>, MyErrno> =
                variantEnums(true, Result.ok(Unit), MyErrno.SUCCESS)
            expect(result.f0 == false)
            expect(result.f1 is Result.Err)
            expect(result.f2 == MyErrno.A)
        }
    }

    private fun expect(v: Boolean) {
        if (!v) {
            throw AssertionError()
        }
    }
}
