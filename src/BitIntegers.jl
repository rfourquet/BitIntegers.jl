# * BitIntegers

module BitIntegers

import Base: <, <=, ==, promote_rule, rem, unsigned

using Base: sle_int, slt_int, ule_int, ult_int

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


# * conversions, promotions

unsigned(x::XBS) = reinterpret(typeof(convert(Unsigned, zero(x))), x)

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


end # module
