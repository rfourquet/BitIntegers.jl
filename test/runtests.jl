using BitIntegers, Test


module TestBitIntegers

using BitIntegers, Test

BitIntegers.@define_integers 24
BitIntegers.@define_integers 8  MyInt8 MyUInt8

# the following should throw, but is hard to test:
# BitIntegers.@define_integers 8 MyInt8

@testset "definitions" begin
    @test @isdefined Int24
    @test @isdefined UInt24
    @test sizeof(Int24) == sizeof(UInt24) == 3
    @test Int24  <: Signed
    @test UInt24 <: Unsigned

    @test @isdefined MyInt8
    @test @isdefined MyUInt8
    @test sizeof(MyInt8) == sizeof(MyUInt8) == 1
    @test MyInt8  <: Signed
    @test MyUInt8 <: Unsigned
end

end # module TestBitIntegers


@testset "types are defined" begin
    for (T, s) = (Int256 => 256, Int512 => 512, Int1024 => 1024)
        @test sizeof(T) * 8 == s
        @test T <: Signed
        @test T <: BitIntegers.BitSigned
        @test T <: BitIntegers.BitInteger
        @test T ∈ BitIntegers.BitSigned_types
        @test T ∈ BitIntegers.BitInteger_types
    end
    for (T, s) = (UInt256 => 256, UInt512 => 512, UInt1024 => 1024)
        @test sizeof(T) * 8 == s
        @test T <: Unsigned
        @test T <: BitIntegers.BitUnsigned
        @test T <: BitIntegers.BitInteger
        @test T ∈ BitIntegers.BitUnsigned_types
        @test T ∈ BitIntegers.BitInteger_types
    end
end
