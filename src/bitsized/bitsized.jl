module BitSizedIntegers
# export BitSized, SignedBitSized, UnsignedBitSized, @makebitsized

# module for storing bit sized integer types
module ITypes
using ..BitSizedIntegers
end

import .ITypes: ITypes


#=
The idea is to round up the storage size to nearest multiple of 8, i.e. whole bytes.
The unused bits are set to zero.
=#

abstract type SignedBitSized{N} <: Signed end
abstract type UnsignedBitSized{N} <: Unsigned end
BitSized{N} = Union{SignedBitSized{N}, UnsignedBitSized{N}}

function Base.show(io::IO, ::Type{T}) where {N, T<:BitSized{N}}
    prefix = T <: SignedBitSized ? "" : "U"
    print(prefix,"Int",N)
end
include("intrinsics.jl")
import .Intrinsics: Intrinsics

@generated function BitSized{N}(x, ::Type{signed} = Signed) where {N, signed <: Union{Signed,Unsigned}}
    B = 8cld(N, 8)
    xN = 8sizeof(x)
    if signed <: Signed
        ext = N < xN ? :checked_trunc_sint : :sext_int
        sym = Symbol(:Int, N)
    else
        ext = N < xN ? :checked_trunc_uint : :zext_int
        sym = Symbol(:UInt, N)
    end
    ss = string(sym)
    quote
        if isdefined(ITypes, $(QuoteNode(sym))) 
            Intrinsics.$ext($sym, x)
        else
            error($ss, " not defined, run @makebitsized $N")
        end
#        Base.llvmcall($code, ITypes.$s, Tuple{Int64}, Int64(x))
    end
end

macro makebitsized(N)
    B = 8cld(N,8)
    T = Symbol(N isa Signed ? :Int : :UInt, N)
    M = @__MODULE__
    (isdefined(ITypes, T) || isdefined(Main, T)) && return :($T)
    quote
        @eval ITypes begin
            (primitive type $T <: SignedBitSized{$N} $B end)
            $T(x) = BitSized{$N}(x)
        end
        @eval $M const $T = ITypes.$T
#        setglobal!($M, $(esc(QuoteNode(T))), ITypes.$T)
        const $(esc(T)) = ITypes.$T
    end
end


#@makebitsized 5
#@makebitsized 77

@generated function Base.show(io::IO, i::BT) where {N,BT<:SignedBitSized{N}}
    B = 8sizeof(BT)
    OB = 2^(64 - leading_zeros(N))
    T = Symbol(:Int, OB)
    code = """
    %3 = trunc i$(B) %0 to i$N
    %4 = sext i$N %3 to i$OB
    ret i$OB %4
    """
    quote
        show(io, Base.llvmcall($code, $T, Tuple{BT}, i))
    end
end



@generated function Base.:+(a::T, b::T) where {N, T <: SignedBitSized{N}}
    s = 8sizeof(T)
    code = """
    %3 = trunc i$s %0 to i$N
    %4 = trunc i$s %1 to i$N
    %5 = add i$N %3, %4
    %6 = sext i$N %5 to i$s
    ret i$s %6
    """
    quote
        Base.llvmcall($code, T, Tuple{T,T}, a, b)
    end
end

@generated function Base.:-(a::T, b::T) where {N, T <: SignedBitSized{N}}
    s = 8sizeof(T)
    code = """
    %3 = trunc i$s %0 to i$N
    %4 = trunc i$s %1 to i$N
    %5 = sub i$N %3, %4
    %6 = sext i$N %5 to i$s
    ret i$s %6
    """
    quote
        Base.llvmcall($code, T, Tuple{T,T}, a, b)
    end
end

@generated function Base.:*(a::T, b::T) where {N, T <: SignedBitSized{N}}
    s = 8sizeof(T)
    code = """
    %3 = trunc i$s %0 to i$N
    %4 = trunc i$s %1 to i$N
    %5 = mul i$N %3, %4
    %6 = sext i$N %5 to i$s
    ret i$s %6
    """
    quote
        Base.llvmcall($code, T, Tuple{T,T}, a, b)
    end
end


@generated function Base.:-(a::T) where {N, T<:SignedBitSized{N}}
    s = 8sizeof(T)
    code = """
    %3 = trunc i$s %0 to i$N
    %4 = sub i$N 0, %3
    %5 = sext i$N %4 to i$s
    ret i$s %5
    """
    quote
        Base.llvmcall($code, T, Tuple{T}, a)
    end
end

#=
function Base.Int64(x::Int5)
    Base.llvmcall("""
    %3 = sext i8 %0 to i64
    ret i64 %3
    """, Int64, Tuple{Int5}, x)
end
=#
end # module BitSizedIntegers
