using BitIntegers, Test
using Statistics: mean


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


@testset "widen" begin
    M = TestBitIntegers
    @test widen(M.MyInt8)   == Int16
    @test widen(M.MyUInt8)  == UInt16
    @test widen(M.Int24)    == BigInt
    @test widen(M.UInt24)   == BigInt
    @test widen(Int256)   == Int512
    @test widen(UInt256)  == UInt512
    @test widen(Int512)   == Int1024
    @test widen(UInt512)  == UInt1024
    @test widen(Int1024)  == BigInt
    @test widen(UInt1024) == BigInt
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


@testset "ndigits0z" begin
    base = rand([-100:-2; 2:200])
    for X in XInts
        x = rand(X)
        if X <: Unsigned && base < 0
            x >>= 1 # ndigits0znb not implemented in this case
        end
        n = big(x)
        @test Base.ndigits0z(x, base) == Base.ndigits0z(n, base)
    end
end


@testset "floats" begin
    for X in XInts
        x = rand(X)
        n = big(x)
        for F in (Float16, Float32, Float64)
            f = F(n)
            @test F(x) == f
            @test promote_type(X, F) == F
            if !isinf(f)
                @test F(X(f)) == f
            end
            # the argument to X() is ±Inf in many cases:
            @test_throws InexactError X(prevfloat(F(typemin(X))))
            @test_throws InexactError X(nextfloat(F(typemax(X))))
        end
        @test AbstractFloat(x) == Float64(x)
    end
end


@testset "rand" begin
    for X in XInts
        # ranges
        a, k, b = X.(sort!(big.([x >> (sizeof(X)*4) for x in rand(X, 3)]))) # TODO: remove conversions
        k >>= rand(0:ndigits(k, base=2)-1)
        r = k < 0 ? (b:k:a) : (a:k:b)
        @test rand(r) ∈ r

        # scalars
        ispow2(sizeof(X)) || continue # cf. Issue #29053
        A = rand(X, 3000)
        for a = [A, bswap.(A)]
            for f in (leading_zeros, leading_ones, trailing_zeros, trailing_ones)
                @test 0.9 < mean(f.(a)) < 1.1
            end
            @test 8*sizeof(X)÷2 - 1 < mean(count_ones.(a)) < 8*sizeof(X)÷2 + 1
        end
    end
end

@testset "checked operations" begin
    for X in XInts
        @test Base.sub_with_overflow(typemin(X)+X(3), X(3)) == (typemin(X), false)
        @test Base.sub_with_overflow(typemin(X)+X(2), X(3)) == (typemax(X), true)
        @test Base.add_with_overflow(typemax(X)-X(3), X(3)) == (typemax(X), false)
        @test Base.add_with_overflow(typemax(X)-X(2), X(3)) == (typemin(X), true)
        if X <: Signed # unimplemented otherwise (problem with LLVM)
            @test Base.mul_with_overflow(typemax(X), X(1))  == (typemax(X), false)
            @test Base.mul_with_overflow(typemax(X), X(2))  == (-2 % X,     true)
        end

        @test Base.checked_abs(typemax(X)) == typemax(X)
        if X <: Signed
            @test_throws OverflowError Base.checked_abs(typemin(X))
        else
            @test Base.checked_abs(typemin(X)) == zero(X)
            x = rand(X)
            @test Base.checked_abs(x) == x
        end
    end
end
