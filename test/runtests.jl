using BitIntegers, Test

const BInts = Base.BitInteger_types
const XInts = BitIntegers.BitInteger_types
const Ints = (BInts..., XInts...)

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


@testset "conversions" begin
    @testset "between types" begin
        for X in Ints
            # we don't test Base-only type combinations:
            for Y in (X ∈ BInts ? XInts : Ints)
                @test Y(42) === Y(X(42))
                if sizeof(X) * 8 >= 128 > sizeof(Y) * 8
                    x = X(UInt128(1) << 127)
                    @test_throws InexactError Y(x)
                end
                if X <: Signed && Y <: Unsigned
                    x = X(-1)
                    @test_throws InexactError Y(x)
                end
            end
        end
    end
    @testset "signedness" begin
        for X in XInts
            XU = Base.uinttype(X)
            @test XU <: Unsigned
            x = X(1)
            @test x isa X
            xu = convert(Unsigned, x)
            xs = convert(Signed, x)
            @test xu isa XU
            @test xu === Unsigned(x) === unsigned(x)
            @test xs === Signed(x) === signed(x)
            if X <: Signed
                @test sizeof(X) == sizeof(XU)
                @test X !== XU
                @test x === xs
            else
                @test X === XU
                @test x === xu
            end
            if X <: Signed
                @test_throws InexactError Unsigned(X(-1))
                @test unsigned(X(-1)) isa XU
            end
        end
    end
end
