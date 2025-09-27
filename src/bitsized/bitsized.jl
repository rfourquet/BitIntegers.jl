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

end # module BitSizedIntegers
