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

const BInts = Base.BitInteger_types
const XInts = (BitIntegers.BitInteger_types..., TestBitIntegers.UInt24, TestBitIntegers.Int24)
const Ints = (BInts..., XInts...)

# we don't include most Base-only type combinations:
const TypeCombos =
    [((X, Y) for X in Ints for Y in (X ∈ BInts ? XInts : Ints))...,
     (Int, Int), (Int, UInt), (UInt, Int), (UInt, UInt)]


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


@testset "typemin, typemax" begin
    for X in XInts
        @test typemin(X) < typemax(X)
        if X <: Unsigned
            @test typemin(X) == 0
            @test count_ones(typemax(X)) == 8 * sizeof(X)
        else
            @test typemin(X) < 0 && typemax(X) > 0
            # @test typemin(X) - 1 == typemax(X)
        end
    end
end


@testset "conversions" begin
    @testset "between types" begin
        for (X, Y) in TypeCombos
            @test Y(42) === Y(X(42))
            if sizeof(X) * 8 >= 128 > sizeof(Y) * 8
                x = X(UInt128(1) << 126)
                @test_throws InexactError Y(x)
            end
            if X <: Signed && Y <: Unsigned
                x = X(-1)
                @test_throws InexactError Y(x)
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


@testset "promote_rule" begin
    for (X, Y) in TypeCombos
        T = promote_type(X, Y)
        if X.size > Y.size
            @test T === X
        elseif X.size == Y.size
            @test T === (X <: Unsigned ? X : Y)
        else
            @test T == Y
        end
    end
end


@testset "x % T" begin
    i = rand(0:typemax(Int8))
    for (X, Y) in TypeCombos
        x = X(i)
        @test x % Y isa Y
        @test x % Y == x
        @test x % Y % X === x
    end
end


@testset "comparisons" begin
    i, j = sort!(rand(0:typemax(Int8), 2))
    i == j && ((i, j) = (2, 3))
    for (X, Y) in TypeCombos
        x, y = X(i), Y(j)
        @test x < y
        @test x <= y
        @test x != y
        @test y > x
        @test y >= x
        @test x == Y(i)
        @test y == X(j)
    end
end


@testset "bit operations" begin
    i, j = rand(1:Int(typemax(Int8)), 2)
    k, l = rand(Int(typemin(Int8)):-1, 2)
    for X in XInts
        for op in (~, bswap)
            if sizeof(X) % 2 != 0 && op == bswap
                @test_throws ErrorException op(X(i))
                continue
            end
            x = op(X(i))
            @test x isa X
            @test x != X(i) # we assume sizeof(X) > 8
            @test op(x) == X(i)
        end
        for op in (count_ones, leading_zeros, trailing_zeros, leading_ones, trailing_ones)
            r = op(X(i))
            @test r isa Int
            if op ∈ (count_ones, trailing_ones, trailing_zeros, leading_ones)
                @test r === op(i)
            else # leading_zeros
                @test r == op(i) + 8 * (sizeof(X) - sizeof(i))
            end
        end
    end
    for (X, Y) in TypeCombos
        T = promote_type(X, Y)
        for n = (i, k), m = (j, l)
            if n < 0 && X <: Unsigned || m < 0 && Y <: Unsigned
                continue
            end
            x, y = X(n), Y(m)
            for op in (&, |, xor)
                z = op(x, y)
                @test z isa T
                @test signed(z) == op(n, m)
            end
            @test flipsign(x, y) isa X
            @test signed(flipsign(x, y)) == flipsign(n, m)
        end
    end
end


@testset "arithmetic operations" begin
    for (X, Y) in TypeCombos
        T = promote_type(X, Y)
        for op = (-, +, *, div, rem, mod)
            TT = op ∈ (-, +, *) ? T :
                 op === mod ?
                   (Y <: Signed   && T <: Unsigned ? typeof(signed(T(0))) :
                    Y <: Unsigned && T <: Signed ?   typeof(unsigned(T(0))) :
                    T) :
                 X <: Signed   && T <: Unsigned ? typeof(signed(T(0))) :
                 X <: Unsigned && T <: Signed ?   typeof(unsigned(T(0))) :
                 T
            @test op(X(5), Y(2)) isa TT
            @test op(X(5), Y(2)) == op(5, 2)
        end
    end
end


@testset "BigInt" begin
    r = -big(2)^2000:big(2)^2000
    b = rand(r)
    for X in XInts
        x = b % X
        @test x isa X
        if typemin(X) <= b <= typemax(X)
            @test x == b
            @test x == X(b)
        else
            @test_throws InexactError X(b)
        end
    end
end
