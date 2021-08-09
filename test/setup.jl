using BitIntegers, Test
using Statistics: mean
using Serialization: serialize, deserialize


module TestBitIntegers

using BitIntegers, Test

BitIntegers.@define_integers 24
BitIntegers.@define_integers 200
BitIntegers.@define_integers 8  MyInt8 MyUInt8

# the following should throw, but is hard to test:
# BitIntegers.@define_integers 8 MyInt8

@testset "definitions" begin
    @test @isdefined Int24
    @test @isdefined UInt24
    @test sizeof(Int24) == sizeof(UInt24) == 3
    @test Int24  <: Signed
    @test UInt24 <: Unsigned

    @test @isdefined Int200
    @test @isdefined UInt200
    @test sizeof(Int200) == sizeof(UInt200) == 25
    @test Int200  <: Signed
    @test UInt200 <: Unsigned

    @test @isdefined MyInt8
    @test @isdefined MyUInt8
    @test sizeof(MyInt8) == sizeof(MyUInt8) == 1
    @test MyInt8  <: Signed
    @test MyUInt8 <: Unsigned
end

end # module TestBitIntegers

const BInts = Base.BitInteger_types
const XInts = (BitIntegers.BitInteger_types..., TestBitIntegers.UInt24, TestBitIntegers.Int24)
const Ints = (BInts..., XInts...)

# we don't include most Base-only type combinations:
const TypeCombos =
    [((X, Y) for X in Ints for Y in (X ∈ BInts ? XInts : Ints))...,
     (Int, Int), (Int, UInt), (UInt, Int), (UInt, UInt)]

nothing
