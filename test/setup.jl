using BitIntegers, Test
using Statistics: mean
using Serialization: serialize, deserialize

# while v1.4 and below are supported:
using BitIntegers: bitrotate

module TestBitIntegers

using BitIntegers, Test

BitIntegers.@define_integers 24
BitIntegers.@define_integers 24 I24 U24 # to test mixed operations with Int24
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

using .TestBitIntegers: Int24, UInt24, I24, U24, MyInt8, MyUInt8

const BInts = Base.BitInteger_types
const XInts = (BitIntegers.BitInteger_types..., TestBitIntegers.UInt24, TestBitIntegers.Int24)
const Ints = (BInts..., XInts...)

# we don't include most Base-only type combinations:
const TypeCombos =
    [((X, Y) for X in Ints for Y in (X ∈ BInts ? XInts : Ints))...,
     (Int, Int), (Int, UInt), (UInt, Int), (UInt, UInt)]


# initial attempt at implementing bswap for odd byte-sizes, but it's less efficient
# than the `bswap_simple` version after being optimized by llvm!
# let's keep this version around to test values
if VERSION >= v"1.6"
    @generated function bswap_odd(x::BitIntegers.XBI)
        @assert isodd(sizeof(x))
        nb = sizeof(x) * 8
        ix = "i$nb"
        iy = "i$(nb+8)"
        quote
            Base.llvmcall(($"""
                           declare $iy @llvm.bswap.$iy($iy %Val)
                           define $ix @entry($ix) {
                               %y = zext $ix %0 to $iy
                               %y2 = call $iy @llvm.bswap.$iy($iy %y)
                               %y3 = lshr $iy %y2, 8
                               %x2 = trunc $iy %y3 to $ix
                               ret $ix %x2
                           }
                           """, "entry"), $x, Tuple{$x}, x)
        end
    end
end

nothing
