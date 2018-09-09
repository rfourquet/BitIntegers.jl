# * BitIntegers

module BitIntegers

import Base: &, *, +, -, <, <<, <=, ==, >>, >>>, |, ~, AbstractFloat, bswap, count_ones, div,
             flipsign, leading_zeros, mod, ndigits0zpb, promote_rule, rem, trailing_zeros,
             typemax, typemin, unsigned, xor

using Base: add_int, and_int, ashr_int, bswap_int, checked_sdiv_int, checked_srem_int,
            checked_udiv_int, checked_urem_int, ctlz_int, ctpop_int, cttz_int, flipsign_int,
            lshr_int, mul_int, ndigits0z, ndigits0znb, neg_int, not_int, or_int, shl_int,
            sitofp, sle_int, slt_int, sub_int, uinttype, uitofp, ule_int, ult_int, xor_int

using Base.GMP: ispos, Limb

using Core: bitcast, check_top_bit, checked_trunc_sint, checked_trunc_uint, sext_int,
            trunc_int, zext_int


# * types definition & aliases

abstract type AbstractBitUnsigned <: Unsigned end
abstract type AbstractBitSigned   <: Signed   end


# ** @define_integers

macro define_integers(n::Int, SI=nothing, UI=nothing)
    if SI === UI === nothing
        SI = Symbol(:Int, n)
        UI = Symbol(:UInt, n)
    elseif SI === nothing || UI === nothing
        throw(ArgumentError(
            "@define_integers requires exactly 1 or 3 arguments, got 2"))
    end

    quote
        primitive type $SI <: AbstractBitSigned   $n end
        primitive type $UI <: AbstractBitUnsigned $n end

        Base.Signed(x::$(esc(UI)))   = $(esc(SI))(x)
        Base.Unsigned(x::$(esc(SI))) = $(esc(UI))(x)
        Base.uinttype(::Type{$(esc(SI))}) = $(esc(UI))
        Base.uinttype(::Type{$(esc(UI))}) = $(esc(UI))
    end
end


# ** instanciation of types

const _DEFINED_SIZES = (256, 512, 1024)

for n = _DEFINED_SIZES
    @eval begin
        @define_integers $n
        export $(Symbol(:UInt, n))
        export $(Symbol(:Int,  n))
    end
end


# ** type lists & aliases

const BitUnsigned_types = # (UInt256, ...)
    tuple((getfield(@__MODULE__, (Symbol(:UInt, n))) for n in _DEFINED_SIZES)...)

const BitSigned_types = # (Int256, ...)
    tuple((getfield(@__MODULE__, (Symbol(:Int, n))) for n in _DEFINED_SIZES)...)

const BitInteger_types = (BitSigned_types..., BitUnsigned_types...)

const BitUnsigned = Union{BitUnsigned_types...}
const BitSigned   = Union{BitSigned_types...}
const BitInteger  = Union{BitInteger_types...}


## convenient abbreviations, only for internal use

const BBS = Base.BitSigned
const BBU = Base.BitUnsigned
const BBI = Base.BitInteger

# eXtended
const XBS = AbstractBitSigned
const XBU = AbstractBitUnsigned
const XBI = Union{XBS,XBU}

# Union, Unified
const UBS = Union{BBS,XBS}
const UBU = Union{BBU,XBU}
const UBI = Union{BBI,XBI}


# ** typemin, typemax

typemin(::Type{T}) where {T<:XBU} = convert(T, 0)
typemax(::Type{T}) where {T<:XBU} = ~convert(T, 0)

typemin(::Type{T}) where {T<:XBS} = convert(T, 1) << (sizeof(T)*8-1)
typemax(::Type{T}) where {T<:XBS} = bitcast(T, typemax(uinttype(T)) >> 1)


# * conversions, promotions

# ** unsigned

unsigned(x::XBS) = reinterpret(typeof(convert(Unsigned, zero(x))), x)

# ** integers

# U -> X
(::Type{T})(x::Union{UBI,Bool}) where {T<:XBI} = convertto(T, x)::T

# X -> B
for T in Base.BitInteger_types
    @eval Core.$(Symbol(:to, T))(x::XBI) = convertto($T, x)
    @eval Base.$(Symbol(T))(x::XBI) = convertto($T, x)::$T
end

@generated function convertto(::Type{T}, x::Union{UBI,Bool}) where {T<:UBI}
    x === Bool && return :(and_int(zext_int(T, x), T(1)))
    if T <: Unsigned
        if x <: Signed
            if sizeof(x) < sizeof(T)
                :(sext_int(T, check_top_bit(x)))
            elseif sizeof(x) == sizeof(T)
                :(bitcast(T, check_top_bit(x)))
            else
                :(checked_trunc_uint(T, x))
            end
        else # x <: Unsigned
            if sizeof(x) < sizeof(T)
                :(zext_int(T, x))
            elseif sizeof(x) == sizeof(T)
                x === T ?
                    :x  :
                    :(reinterpret(T, x))
            else
                :(checked_trunc_uint(T, x))
            end
        end
    else # T <: Signed
        if x <: Signed
            if sizeof(x) < sizeof(T)
                :(sext_int(T, x))
            elseif sizeof(x) == sizeof(T)
                x === T ?
                    :x  :
                    :(reinterpret(T, x))
            else
                :(checked_trunc_sint(T, x))
            end
        else # x <: Unsigned
            if sizeof(x) < sizeof(T)
                :(zext_int(T, x))
            elseif sizeof(x) == sizeof(T)
                :(bitcast(T, check_top_bit(x)))
            else
                :(checked_trunc_sint(T, check_top_bit(x)))
            end
        end
    end
end

@generated function rem(x::Union{UBI,Bool}, ::Type{to}) where {to<:UBI}
    from = x
    to === from && return :x # this replaces Base's method for BBI
    if to.size < from.size
        :(trunc_int(to, x))
    elseif from === Bool
        :(convert(to, x))
    elseif from.size < to.size
        if from <: Signed
            :(sext_int(to, x))
        else
            :(convert(to, x))
        end
    else
        :(bitcast(to, x))
    end
end

@generated function promote_rule(::Type{X}, ::Type{Y}) where {X<:XBI,Y<:UBI}
    if X.size > Y.size
        X
    elseif X.size == Y.size
        X <: Unsigned ?
            X :
            Y
    else
        Y
    end
end

# ** BigInt

function rem(x::BigInt, ::Type{T}) where T<:XBI
    if sizeof(T) <= sizeof(Limb)
        iszero(x) ? zero(T) : flipsign(unsafe_load(x.d) % T, x.size)
    else
        u = zero(T)
        for l = 1:min(abs(x.size), cld(sizeof(T), sizeof(Limb)))
            u += (unsafe_load(x.d, l) % T) << ((sizeof(Limb)<<3)*(l-1))
        end
        flipsign(u, x.size)
    end
end

function (::Type{T})(x::BigInt) where T<:XBU
    if sizeof(T) < sizeof(Limb)
        convert(T, convert(Limb,x))
    else
        0 <= x.size <= cld(sizeof(T),sizeof(Limb)) || throw(InexactError(Symbol(string(T)), T, x))
        x % T
    end
end

function (::Type{T})(x::BigInt) where T<:XBS
    n = abs(x.size)
    if sizeof(T) < sizeof(Limb)
        SLimb = typeof(Signed(one(Limb)))
        convert(T, convert(SLimb, x))
    else
        0 <= n <= cld(sizeof(T),sizeof(Limb)) || throw(InexactError(Symbol(string(T)), T, x))
        y = x % T
        ispos(x) ⊻ (y > 0) && throw(InexactError(Symbol(string(T)), T, x)) # catch overflow
        y
    end
end

# ** floats

AbstractFloat(x::XBI) = Float64(x)

for T in (Float32, Float64)
    @eval begin
        (::Type{$T})(x::XBS) = sizeof(x) > 16 ? $T(big(x)) : sitofp($T, x)
        (::Type{$T})(x::XBU) = sizeof(x) > 16 ? $T(big(x)) : uitofp($T, x)
        promote_rule(::Type{$T}, ::Type{<:XBI}) = $T
    end
end

promote_rule(::Type{Float16}, ::Type{<:XBI}) = Float16

# TODO: avoid conversion to BigInt
(::Type{T})(x::AbstractFloat) where {T<:XBI} = T(BigInt(x))::T
# to disambiguate:
(::Type{T})(x::Float16) where {T<:XBI} = T(BigInt(x))::T


# * comparisons

(<)(x::T, y::T) where {T<:XBU} = ult_int(x, y)
(<)(x::T, y::T) where {T<:XBS} = slt_int(x, y)

(<=)(x::T, y::T) where {T<:XBU} = ule_int(x, y)
(<=)(x::T, y::T) where {T<:XBS} = sle_int(x, y)

==(x::UBS, y::UBU) = (x >= 0) & (unsigned(x) == y)
==(x::UBU, y::UBS) = (y >= 0) & (x == unsigned(y))
<( x::UBS, y::UBU) = (x <  0) | (unsigned(x) <  y)
<( x::UBU, y::UBS) = (y >= 0) & (x <  unsigned(y))
<=(x::UBS, y::UBU) = (x <  0) | (unsigned(x) <= y)
<=(x::UBU, y::UBS) = (y >= 0) & (x <= unsigned(y))


# * bit operations

(~)(x::XBI) = not_int(x)
(&)(x::T, y::T) where {T<:XBI} = and_int(x, y)
(|)(x::T, y::T) where {T<:XBI} = or_int(x, y)
xor(x::T, y::T) where {T<:XBI} = xor_int(x, y)

>>( x::UBS, y::UBU) = ashr_int(x, y)
>>( x::UBU, y::UBU) = lshr_int(x, y)
>>>(x::UBI, y::UBU) = lshr_int(x, y)
<<( x::UBI, y::UBU) = shl_int(x, y)

>>( x::UBI, y::Int) = ifelse(0 <= y, x >> unsigned(y),  x << unsigned(-y))
<<( x::UBI, y::Int) = ifelse(0 <= y, x << unsigned(y),  x >> unsigned(-y))
>>>(x::UBI, y::Int) = ifelse(0 <= y, x >>> unsigned(y), x << unsigned(-y))

count_ones(    x::XBI) = Int(ctpop_int(x))
leading_zeros( x::XBI) = Int(ctlz_int(x))
trailing_zeros(x::XBI) = Int(cttz_int(x))

function bswap(x::XBI)
    if sizeof(x) % 2 != 0
        # llvm instruction is invalid
        error("unimplemented")
    else
        bswap_int(x)
    end
end

flipsign(x::T, y::T) where {T<:XBS} = flipsign_int(x, y)

# this doesn't catch flipsign(x::BBS, y::BBS), which is more specific in Base
flipsign(x::UBS, y::UBS) = flipsign_int(promote(x, y)...) % typeof(x)


# * arithmetic operations

(-)(x::XBI)                    = neg_int(x)
(-)(x::T, y::T) where {T<:XBI} = sub_int(x, y)
(+)(x::T, y::T) where {T<:XBI} = add_int(x, y)
(*)(x::T, y::T) where {T<:XBI} = mul_int(x, y)

div(x::XBS, y::Unsigned) = flipsign(signed(div(unsigned(abs(x)), y)), x)
div(x::Unsigned, y::XBS) = unsigned(flipsign(signed(div(x, unsigned(abs(y)))), y))

rem(x::XBS, y::Unsigned) = flipsign(signed(rem(unsigned(abs(x)), y)), x)
rem(x::Unsigned, y::XBS) = rem(x, unsigned(abs(y)))

mod(x::XBS, y::Unsigned) = rem(y + unsigned(rem(x, y)), y)

# these operations fail LLVM for bigger types than UInt128
div(x::T, y::T) where {T<:XBS} = sizeof(T) > 16 ? T(div(big(x), big(y))) : checked_sdiv_int(x, y)
rem(x::T, y::T) where {T<:XBS} = sizeof(T) > 16 ? T(rem(big(x), big(y))) : checked_srem_int(x, y)
div(x::T, y::T) where {T<:XBU} = sizeof(T) > 16 ? T(div(big(x), big(y))) : checked_udiv_int(x, y)
rem(x::T, y::T) where {T<:XBU} = sizeof(T) > 16 ? T(rem(big(x), big(y))) : checked_urem_int(x, y)


# * misc

function ndigits0zpb(x::XBU, b::Int)
    # precondition: b > 1
    x == 0 && return 0
    b < 0   && return ndigits0znb(signed(x), b)
    b == 2  && return sizeof(x)<<3 - leading_zeros(x)
    b == 8  && return (sizeof(x)<<3 - leading_zeros(x) + 2) ÷ 3
    b == 16 && return sizeof(x)<<1 - leading_zeros(x)>>2
    # b == 10 && return ndigits0z(x) # TODO: implement ndigits0z(x)

    d = 0
    while x > typemax(Int)
        x = div(x,b)
        d += 1
    end
    x = div(x,b)
    d += 1

    m = 1
    while m <= x
        m *= b
        d += 1
    end
    return d
end

ndigits0zpb(x::XBS, b::Integer) = ndigits0zpb(unsigned(abs(x)), Int(b))
ndigits0zpb(x::XBU, b::Integer) = ndigits0zpb(x, Int(b))


end # module
