include("setup.jl")

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

@testset "show" begin
    @test string(MyInt8(1)) == "1" # issue #34
    @test sprint(show, MyInt8(1)) == "1"
    @test sprint(show, MyUInt8(1)) == "0x01"
    for XX = (MyInt8, MyUInt8)
        for _=1:5
            xx = rand(XX)
            @test string(xx) == string(Int(xx))
            @test string(xx, base=16) == string(Int(xx), base=16)
        end
    end
    for XX in XInts
        xx = rand(XX)
        @test string(xx) == string(big(xx))
        @test string(xx, base=16) == string(big(xx), base=16)
    end
end

@testset "string macros" begin
    @test   int256"1" ===   Int256(1)
    @test  uint256"1" ===  UInt256(1)
    @test   int512"1" ===   Int512(1)
    @test  uint512"1" ===  UInt512(1)
    @test  int1024"1" ===  Int1024(1)
    @test uint1024"1" === UInt1024(1)
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
        if sizeof(X) > sizeof(Y)
            @test T === X
        elseif sizeof(X) == sizeof(Y)
            @test T === (X <: Unsigned ? X : Y)
        else
            @test T == Y
        end
    end
    # promote_rule follows Base rules; for types with same size/signedness:
    # - types from XBI win
    # - do not resolve for two types from XBI
    for X = (Int16, UInt16), Y = (MyInt8, MyUInt8)
        # X bigger
        @test promote_type(X, Y) == X == promote_type(Y, X)
    end
    for X = (Int16, UInt16, MyInt8, MyUInt8), Y = (Int24, UInt24)
        # Y bigger
        @test promote_type(X, Y) == Y == promote_type(Y, X)
    end
    # same size:
    @test promote_type(Int8, MyInt8) == MyInt8
    @test promote_type(Int8, MyUInt8) == MyUInt8
    @test promote_type(UInt8, MyInt8) == UInt8
    @test promote_type(UInt8, MyUInt8) == MyUInt8
    @test promote_type(Int24, I24) == BitIntegers.AbstractBitSigned # typejoin
    @test promote_type(Int24, U24) == U24
    @test promote_type(UInt24, I24) == UInt24
    @test promote_type(UInt24, U24) == BitIntegers.AbstractBitUnsigned
    # fail for two BitIntegers types
    @test_throws ErrorException U24(1) + UInt24(2) # can't resolve
end


@testset "x % T" begin
    i = rand(0:typemax(Int8))
    for (X, Y) in TypeCombos
        x = X(i)
        @test x % Y isa Y
        @test x % Y == x
        @test x % Y % X === x
    end
    for X in XInts
        x = X(i)
        @test x % X === x
        if X <: Signed
            @test x % BitIntegers.AbstractBitSigned === x
            @test x % Signed === x
        else
            @test x % BitIntegers.AbstractBitUnsigned === x
            @test x % Unsigned === x
        end
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


@testset "hash" begin
    for X in (XInts..., I32, U32, I64, U64, MyInt8, MyUInt8)
        for xx in rand(X, 4)
            hh = rand(UInt)
            @test hash(xx, hh) == hash(big(xx), hh)
        end
    end
end


@testset "bit operations" begin
    i, j = rand(1:Int(typemax(Int8)), 2)
    k, l = rand(Int(typemin(Int8)):-1, 2)
    for X in XInts
        for op in (~, bswap)
            x = op(X(i))
            @test x isa X
            @test x != X(i) # we assume sizeof(X) > 8
            @test op(x) == X(i)
        end
        # bswap specific
        if VERSION >= v"1.6"
            for y = rand(X, 20)
                x = bswap(y)
                if iseven(sizeof(x))
                    # test that default implemented matches bswap_simple
                    @test x == BitIntegers.bswap_simple(y)
                else
                    # test that default implemented (i.e. bswap_simple) matches bswap_odd
                    @test x == bswap_odd(y)
                end
            end
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
            if !(n == -128 && X == Int8 && m < 0)
                @test signed(flipsign(x, y)) == flipsign(n, m)
            end
        end
    end
    for (X, Y) in TypeCombos
        x,y = X(i),Y(j)
        @test isodd(x) == isodd(i) == !iseven(x)
        @test isodd(y) == isodd(j) == !iseven(y)
        # Test performance of isodd(x): doesn't allocate.
        @test @allocated(isodd(x) && isodd(y)) == 0
        @test @allocated(iseven(x) && iseven(y)) == 0
    end
end


@testset "shift operations" begin
    vals = [rand(Int8, 10)..., rand(Int64, 10)..., rand(Int1024, 10)...]
    sh_cts = unique(i + j for i in [0, 32, 64, 128, 200, 256, 1024] for j in -20:20)
    sh_rnd = Integer[rand(1:1024, 20)..., rand(Int256(1):Int256(1024), 20)...]
    shifts = Integer[sh_cts..., -sh_cts..., sh_rnd..., -sh_rnd...]
    for X in XInts
        for val in vals
            mask = big(1) << (8 * sizeof(X)) - 1
            x = val % X
            b = big(x)
            for s in shifts
                @test (x >> s) & mask == (b >> s) & mask
                @test (x << s) & mask == (b << s) & mask
                @test (x >>> s) & mask == ((b & mask) >>> s) & mask
            end
        end
    end
end


@testset "bit rotations" begin
    for X in XInts
        x = X(24)
        l = 8*sizeof(X)
        @test bitrotate(x, 2) == 4*x
        @test bitrotate(x, l-1) == div(x, 2)
        x = rand(X)
        for k in (0, UInt8(13), UInt32(371), Int16(-123), Int64(-1072), BigInt(-21330))
            y = @inferred bitrotate(x, k)
            @test y isa X && bitrotate(y, k) == bitrotate(x, 2*k)
            k isa Signed && @test bitrotate(y, -k) == x
        end
    end
end


function test_noalloc(op::OP, x, y) where {OP}
    @test @allocated(op(x, y)) == 0
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
            if VERSION >= v"1.11-" || sizeof(T) <= 16
                test_noalloc(op, X(5), Y(2))
            end
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

@testset "bitstring" begin
    @test bitstring(UInt256(3)) == bitstring(Int256(3)) == '0'^254 * "11"
    @test bitstring(UInt512(3) << 256) == bitstring(Int512(3) << 256) == '0'^254 * "11" * '0'^256
    let (x, y) = rand(UInt128, 2)
        u = UInt1024(x) << 512 + UInt1024(y)
        v = u + UInt1024(1) << 1023
        @test bitstring(u) == bitstring(Int1024(u)) == '0'^384 * bitstring(x) * '0'^384 * bitstring(y)
        @test bitstring(v) == bitstring(typemin(Int1024) + Int1024(u)) == '1' * '0'^383 * bitstring(x) * '0'^384 * bitstring(y)
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

@testset "read/write" begin
    function read_write_tests(io, X)
        x, y = rand(X, 2)
        @test write(io, x) == sizeof(x)
        @test write(io, y) == sizeof(y)
        seekstart(io)
        @test read(io, X) == x
        @test read(io, X) == y
        @test_throws EOFError read(io, X)
        seekstart(io)
        serialize(io, x)
        serialize(io, 3)
        serialize(io, y)
        serialize(io, true)
        seekstart(io)
        q = deserialize(io)
        @test q === x
        q = deserialize(io)
        @test q === 3
        q = deserialize(io)
        @test q === y
        q = deserialize(io)
        @test q === true
    end

    @testset "IOBuffer" begin
        for X in XInts
            read_write_tests(IOBuffer(), X)
        end
    end

    @testset "IOStream" begin
        for X in XInts
            open(tempname(), "w+") do iostream
                read_write_tests(iostream, X)
            end
        end
    end
end

@testset "rand" begin
    for X in XInts
        # ranges
        a, k, b = X.(sort!(big.([x >> (sizeof(X)*4) for x in rand(X, 3)]))) # TODO: remove conversions
        k >>= rand(0:ndigits(k, base=2)-1)
        r = k < 0 ? (b:k:a) : (a:k:b)
        @test rand(r) ∈ r
        @test rand(a:b) ∈ a:b

        # scalars
        ispow2(sizeof(X)) || VERSION >= v"1.4" || continue # cf. Issue #29053
        A = rand(X, 3000)
        for a = (ispow2(sizeof(X)) ? [A, bswap.(A)] : [A])
            for f in (leading_zeros, leading_ones, trailing_zeros, trailing_ones)
                @test 0.9 < mean(f.(a)) < 1.1
            end
            @test 8*sizeof(X)÷2 - 1 < mean(count_ones.(a)) < 8*sizeof(X)÷2 + 1
        end
    end
end

@testset "checked operations" begin
    for X in XInts
        if sizeof(X) != 3 # bug with [U]Int24, cf. Julia issue #34288
            @test Base.sub_with_overflow(typemin(X)+X(3), X(3)) == (typemin(X), false)
            @test Base.sub_with_overflow(typemin(X)+X(2), X(3)) == (typemax(X), true)
            @test Base.add_with_overflow(typemax(X)-X(3), X(3)) == (typemax(X), false)
            @test Base.add_with_overflow(typemax(X)-X(2), X(3)) == (typemin(X), true)
            if X <: Signed # unimplemented otherwise (problem with LLVM)
                @test Base.mul_with_overflow(typemax(X), X(1))  == (typemax(X), false)
                @test Base.mul_with_overflow(typemax(X), X(2))  == (-2 % X,     true)
            end
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

@testset "aqua" begin
    using Aqua
    Aqua.test_all(BitIntegers; deps_compat = false, piracies=false)
    Aqua.test_deps_compat(BitIntegers; ignore=[:Random], check_extras=false)
    # BitIntegers defines methods such as `<(::UBS, ::UBU)` which is (probably
    # benign) type piracy.
    Aqua.test_piracies(BitIntegers; treat_as_own = [Int128, Int16, Int64, Int32, Int8])

    # And test the new method for disambiguation
    @test Int256(BigFloat(3)) === Int256(3)
end
