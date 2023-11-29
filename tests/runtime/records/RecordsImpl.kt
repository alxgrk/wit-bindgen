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
import wit.worlds.TestImports.multipleResults
import wit.worlds.TestImports.roundtripFlags1
import wit.worlds.TestImports.roundtripFlags2
import wit.worlds.TestImports.roundtripFlags3
import wit.worlds.TestImports.roundtripRecord1
import wit.worlds.TestImports.swapTuple
import wit.worlds.TestImports.tuple1

object RecordsImpl {
    fun testImports() {
        run {
            val results: Tuple2<UByte, UShort> = multipleResults()
            expect(results.f0 == 4.toUByte())
            expect(results.f1 == 5.toUShort())
        }
        run {
            val results: Tuple2<UInt, UByte> = swapTuple(Tuple2(1.toUByte(), 2u))
            expect(results.f0 == 2u)
            expect(results.f1 == 1.toUByte())
        }
        expect(roundtripFlags1(TestImports.F1.A).value == F1.A.value)
        expect(roundtripFlags1(TestImports.F1(0.toUByte())).value == 0.toUByte())
        expect(roundtripFlags1(TestImports.F1.B).value == F1.B.value)
        expect(
            roundtripFlags1(TestImports.F1((TestImports.F1.A.value or TestImports.F1.B.value))).value
                    == (F1.A.value or F1.B.value)
        )
        expect(roundtripFlags2(TestImports.F2.C).value == F2.C.value)
        expect(roundtripFlags2(TestImports.F2(0.toUByte())).value == 0.toUByte())
        expect(roundtripFlags2(TestImports.F2.D).value == F2.D.value)
        expect(
            (roundtripFlags2(TestImports.F2((TestImports.F2.C.value or TestImports.F2.E.value))).value
                    == (F2.C.value or F2.E.value))
        )
        run {
            val results: Tuple4<TestImports.Flag8, TestImports.Flag16, TestImports.Flag32, TestImports.Flag64> =
                roundtripFlags3(TestImports.Flag8.B0, TestImports.Flag16.B1, TestImports.Flag32.B2, TestImports.Flag64.B3)
            expect(results.f0.value == Flag8.B0.value)
            expect(results.f1.value == Flag16.B1.value)
            expect(results.f2.value == Flag32.B2.value)
            expect(results.f3.value == Flag64.B3.value)
        }
        run {
            val result: TestImports.R1 = roundtripRecord1(TestImports.R1(8.toUByte(), TestImports.F1.A))
            expect(result.a == 8.toUByte())
            expect(result.b.value == F1.A.value)
        }
        run {
            val result: TestImports.R1 =
                roundtripRecord1(TestImports.R1(0.toUByte(), TestImports.F1((TestImports.F1.A.value or TestImports.F1.B.value))))
            expect(result.a == 0.toUByte())
            expect(result.b.value == (F1.A.value or F1.B.value))
        }
        run {
            val result: Tuple1<UByte> = tuple1(Tuple1(1.toUByte()))
            expect(result.f0 == 1.toUByte())
        }
    }

    private fun expect(v: Boolean) {
        if (!v) {
            throw AssertionError()
        }
    }
}
