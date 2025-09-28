module BitSizedIntegers

# export BitSized, SignedBitSized, UnsignedBitSized, @makebitsized

# module for storing bit sized integer types
module ITypes
using ..BitSizedIntegers
end

import .ITypes: ITypes
import ..AbstractBitSigned, ..AbstractBitUnsigned
#=
The idea is to round up the storage size to nearest multiple of 8, i.e. whole bytes.
The unused bits are set to zero.
=#

abstract type SignedBitSized{N} <: AbstractBitSigned end
abstract type UnsignedBitSized{N} <: AbstractBitUnsigned end
BitSized{N} = Union{SignedBitSized{N}, UnsignedBitSized{N}}

bitsizeof(::T) where T = 8sizeof(T)
bitsizeof(::Type{T}) where T = 8sizeof(T)
bitsizeof(::BitSized{N}) where N = N
bitsizeof(::Type{<:BitSized{N}}) where N = N

#=
function Base.show(io::IO, ::Type{T}) where {N, T<:BitSized{N}}
    prefix = T <: SignedBitSized ? "" : "U"
    print(prefix,"Int",N)
end
=#
include("intrinsics.jl")
import .Intrinsics: Intrinsics, bitsizeof

@generated function is_top_bit_set(x::T) where {N, T<:BitSized{N}}
    @inline
    bit = (1%T) << (N-1)
    :(x & $bit ≠ 0)
end

function check_top_bit(::Type{To}, x::xT) where {To, xT}
    @inline 
    xN = bitsizeof(xT)
    xN % 8 == 0 && return Core.check_top_bit(To, x)
    is_top_bit_set(x) && Core.throw_inexacterror(sizeof(x) === sizeof(To) ? :convert : :trunc, To, x)
    x
end

function Base.hex(x::BitSized, pad::Int, neg::Bool)
    m = cld(bitsizeof(x) - leading_zeros(x), 4)
    n = neg + max(pad, m)
    a = Base.StringMemory(n)
    i = n
    while i >= 2
        b = (x % UInt8)::UInt8
        d1, d2 = b >> 0x4, b & 0xf
        @inbounds a[i-1] = d1 + ifelse(d1 > 0x9, 0x57, 0x30)
        @inbounds a[i]   = d2 + ifelse(d2 > 0x9, 0x57, 0x30)
        x >>= 0x8
        i -= 2
    end
    if i > neg
        d = (x % UInt8)::UInt8 & 0xf
        @inbounds a[i] = d + ifelse(d > 0x9, 0x57, 0x30)
    end
    neg && (@inbounds a[1] = 0x2d) # UInt8('-')
    Base.unsafe_takestring(a)
end


end # module BitSizedIntegers
