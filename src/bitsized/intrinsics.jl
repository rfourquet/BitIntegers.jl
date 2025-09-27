module Intrinsics
import ..BitSized, ..bitsizeof

#=
We need to make intrinsics aware of bit sized integers.
But for some of them we can use the ones from core. Those
that don't care about the unused bits. 
=#

const intrinsics = (:add_int, :and_int, :ashr_int, :bswap_int,
                    :checked_sadd_int, :checked_sdiv_int,
                    :checked_smul_int, :checked_srem_int,
                    :checked_ssub_int, :checked_uadd_int,
                    :checked_udiv_int, :checked_umul_int,
                    :checked_urem_int, :checked_usub_int, :ctlz_int,
                    :ctpop_int, :cttz_int, :flipsign_int, :lshr_int,
                    :mul_int, :ndigits0z, :ndigits0znb, :neg_int,
                    :not_int, :or_int, :shl_int, :sitofp, :sle_int,
                    :slt_int, :sub_int, :uitofp, :ule_int,
                    :ult_int, :xor_int)

onearg = (:ctlz_int, :ctpop_int, :cttz_int, :neg_int, :not_int, :bswap_int)

twoarg = (:add_int, :and_int, :ashr_int, :checked_sadd_int,
           :checked_sdiv_int, :checked_smul_int, :checked_srem_int,
           :checked_ssub_int, :checked_uadd_int, :checked_udiv_int,
           :checked_umul_int, :checked_urem_int, :checked_usub_int,
           :flipsign_int, :lshr_int, :mul_int, :ndigits0z,
           :ndigits0znb, :or_int, :shl_int, :sitofp, :sle_int,
           :slt_int, :sub_int, :uitofp, :ule_int, :ult_int, :xor_int,
           :checked_trunc_sint, :checked_trunc_uint,
           :bitcast, :sext_int, :trunc_int, :zext_int)

#=
bitcast, checked_trunc_sint, checked_trunc_uint, sext_int,
            trunc_int, zext_int
=#
for F in twoarg
    @eval $F(x,y) = Core.Intrinsics.$F(x,y)
end

for F in onearg
    @eval $F(x) = Core.Intrinsics.$F(x)
end


for F in (onearg..., twoarg...)
    @eval $F(x::BitSized...) = error("Unimplemented intrinsic ", $F)
end

function checked_trunc_sint(::Type{To}, x::From) where {To,From}
    @inline
    y = trunc_int(To, x)
    back = sext_int(From, y)
    Core.Intrinsics.eq_int(x, back) || Core.throw_inexacterror(:trunc, To, x)
    y
end

function checked_trunc_uint(::Type{To}, x::From) where {To,From}
    @inline
    y = trunc_int(To, x)
    back = zext_int(From, y)
    Core.Intrinsics.eq_int(x, back) || Core.throw_inexacterror(:trunc, To, x)
    y
end



@generated function Base.typemin(::Type{T}) where {N,T<:BitSized{N}}
    S = 8sizeof(T)
    code = """
    %s = shl i$N 1, $(N-1)
    %r = zext i$N %s to i$S
    ret i$S %r
    """
    quote
        Base.llvmcall($code, T, Tuple{})
    end
end

@generated function Base.typemax(::Type{T}) where {N, T<:BitSized{N}}
    S = 8sizeof(T)
    if T <: Signed
        code = """
        %r = zext i$(N-1) -1 to i$S
        ret i$S %r
        """
    else
        code = """
        %r = zext i$N -1 to i$S
        ret i$S %r
        """
    end
    quote
        Base.llvmcall($code, T, Tuple{})
    end
end


# arith and logic, two args
for F in (:add, :and, :mul, :or, :sub, :xor)
    ifun = Symbol(F, "_int")
    ex = quote
        @generated function $ifun(x::T,y::T) where {N, T<:BitSized{N}}
            S = 8sizeof(T)
            F = $(QuoteNode(F))
            code =     """
            %3 = trunc i$S %0 to i$N
            %4 = trunc i$S %1 to i$N
            %5 = $F i$N %3, %4
            %6 = zext i$N %5 to i$S
            ret i$S %6
            """

            quote
                Base.llvmcall($code, T, Tuple{T,T}, x, y)
            end
        end
    end
    @eval $ex
end

# arith comp
for F in (:sle, :slt, :ule, :ult)
    ifun = Symbol(F, "_int")
    ex = quote
        @generated function $ifun(x::T, y::T) where {N, T<:BitSized{N}}
            S = 8sizeof(T)
            F = $(QuoteNode(F))
            code = """
            %3 = trunc i$S %0 to i$N
            %4 = trunc i$S %1 to i$N
            %5 = icmp $F i$N %3, %4
            %6 = zext i1 %5 to i8
            ret i8 %6
            """
            quote
                Base.llvmcall($code, Bool, Tuple{T,T}, x, y)
            end
        end
    end
    @eval $ex
end


# bit counts
for F in (:ctlz, :ctpop, :cttz)
    ifun = Symbol(F, "_int")
    ex = quote
        @generated function $ifun(x::T) where {N, T<:BitSized{N}}
            S = 8sizeof(T)
            F = $(QuoteNode(F))
            sfun = string($ifun)
            code = ("""
            declare i$N @llvm.ctlz.i$N(i$N)
            define i$S @$sfun(i$S %0) {
            %x = trunc i$S %0 to i$N
            %ct = call i$N @llvm.$F.i$N(i$N %x)
            %ret = zext i$N %ct to i$S
            ret i$S %ret
            }
            """, string(sfun))
            quote
                Base.llvmcall($code, T, Tuple{T}, x)
            end
        end
    end
    @eval $ex
end

# checked arith
for F in (:sadd, :smul, :ssub, :uadd, :umul, :usub)
    ifun = Symbol("checked_",F, "_int")
    ex = quote
        @generated function $ifun(x::T, y::T) where {N, T<:BitSized{N}}
            S = 8sizeof(T)
            F = $(QuoteNode(F))
            # Tuple{Int8, Bool} is taken by llvmcall to be [2 x i8], not {i8, i8}
            rettype = S == 8 ? "[2 x i8]" : "{i$S, i8}"
            code = """
            %3 = trunc i$S %0 to i$N
            %4 = trunc i$S %1 to i$N
            %5 = call {i$N, i1} @llvm.$F.with.overflow.i$N(i$N %3, i$N %4)
            %6 = extractvalue { i$N, i1} %5, 0
            %7 = extractvalue { i$N, i1} %5, 1
            %flag = zext i1 %7 to i8
            %z = zext i$N %6 to i$S
            %9 = insertvalue $rettype zeroinitializer, i$S %z, 0
            %10 = insertvalue $rettype %9, i8 %flag, 1
            ret $rettype %10
            """
            quote
                Base.llvmcall($code, Tuple{T, Bool}, Tuple{T,T}, x, y)
            end
        end
    end
    @eval $ex
end


# checked sdiv
@generated function checked_sdiv_int(x::T, y::T) where {N, T<:BitSized{N}}
    S = 8sizeof(T)
    M = @__MODULE__
    llvmname = string(M, ".check_sdiv_int", S)
    io = IOBuffer()
    code = ("""
    declare void @ijl_throw(ptr)
    declare ptr @jl_diverror_exception()
    define i$S @$llvmname(i$S %x, i$S %y) {
      %num = trunc i$S %x to i$N
      %denom = trunc i$S %y to i$N
      %tmin = shl i$N 1, $(N-1)
      %1 = icmp ne i$N %num, %tmin
      %2 = icmp ne i$N %denom, -1
      %3 = or i1 %1, %2
      %4 = icmp ne i$N %denom, 0
      %valid = and i1 %4, %3
      br i1 %valid, label %pass, label %fail
    fail:
      %exc = load ptr, ptr @jl_diverror_exception, align 8
      call void @ijl_throw(ptr nonnull %exc)
      unreachable
    pass:
      %q = sdiv i$N %num, %denom
      %r = zext i$N %q to i$S
      ret i$S %r
    }
    """, llvmname)

    quote
        Base.llvmcall($code, T, Tuple{T,T}, x, y)
    end
end

# checked srem

@generated function checked_srem_int(x::T, y::T) where {N, T<:BitSized{N}}
    S = 8sizeof(T)
    llvmname = string(@__MODULE__, ".checked_srem_int", S)
    code = ("""
         declare void @ijl_throw(ptr)
         declare ptr @jl_diverror_exception()
         define i$S @$llvmname(i$S %x, i$S %y) {
          top:
            %num = trunc i$S %x to i$N
            %denom = trunc i$S %y to i$N
            switch i$N %denom, label %oksrem [
              i$N 0, label %fail
              i$N -1, label %after_srem
            ]
          fail:
            %exc = load ptr, ptr @jl_diverror_exception, align 8
            call void @ijl_throw(ptr nonnull %exc)
            unreachable
          oksrem:
            %rem = srem i$N %num, %denom
            br label %after_srem
          after_srem:
            %res = phi i$N [ %rem, %oksrem ], [ 0, %top]
            %ret = zext i$N %res to i$S
            ret i$S %ret
         }
    """, llvmname)
    quote
        Base.llvmcall($code, T, Tuple{T,T}, x, y)
    end
end

for F in (:udiv, :urem)
    fname = Symbol("checked_",F,"_int")
    ex = quote
        @generated function $fname(x::T, y::T) where {N, T<:BitSized{N}}
            S = 8sizeof(T)
            llvmname = string(@__MODULE__, ".",$fname, S)
            op = $(QuoteNode(F))
            definecode = ("""
              declare void @ijl_throw(ptr)
              declare ptr @jl_diverror_exception()
              define i$S @$llvmname(i$S %x, i$S %y) {
                %num = trunc i$S %x to i$N
                %denom = trunc i$S %y to i$N
                %divby0 = icmp eq i$N %denom, 0
                br i1 %divby0, label %fail, label %pass
               fail:
                %exc = load ptr, ptr @jl_diverror_exception, align 8
                call void @ijl_throw(ptr nonnull %exc)
                unreachable
               pass:
                %result = $op i$N %num, %denom
                %ret = zext i$N %result to i$S
                ret i$S %ret
              }
     """, llvmname)
            quote
                Base.llvmcall($definecode, T, Tuple{T,T}, x, y)
            end
        end
    end
    @eval $ex
end


# what's left
#=
( :ndigits0z, :ndigits0znb)


bitcast, sext_int,
            trunc_int, zext_int

    %2 = shl i$N 1, $(N-1)
    %3 = $ext i$N %2 to i$S
    ret i$S %3

=#

function trunc_int(::Type{RT}, x::T) where {RT, T}
    RT <: BitSized || T <: BitSized || return Core.Intrinsics.trunc_int(RT, x)
    _trunc_int(RT, x)
end

@generated function _trunc_int(::Type{RT}, x::T) where {RT, T}
    RN = bitsizeof(RT)
    N = bitsizeof(T)
    N == RN && return :(Core.Intrinsics.trunc_int(RT, x))
    N < RN && error("SExt: output bitsize must be > input bitsize")
    (N % 8) == (RN % 8) == 0 && return :(Core.Intrinsics.trunc_int(RT, x))
    S = 8sizeof(T)
    RS = 8sizeof(RT)
    prep = N == S ? "%x = or i$N %0, 0" : "%x = trunc i$S %0 to i$N"
    zext = RN == RS ? "%ret = or i$RN %r, 0" :
        "%ret = zext i$RN %r to i$RS" 
    code = """
         $prep
         %r = trunc i$N %x to i$RN
         $zext
         ret i$RS %ret
    """
    quote
        Base.llvmcall($code, RT, Tuple{T}, x)
    end
end

function sext_int(::Type{RT}, x::T) where {RT, T}
    RT <: BitSized || T <: BitSized || return Core.Intrinsics.sext_int(RT, x)
    _sext_int(RT, x)
end

@generated function _sext_int(::Type{RT}, x::T) where {RT, T}
    RN = bitsizeof(RT)
    N = bitsizeof(T)
    (N % 8) == (RN % 8) == 0 && return Core.Intrinsics.sext_int(RT, x)
    N == RN && return Core.Intrinsics.sext_int(RT, x)
    RN ≤ N && error("SExt: output bitsize must be > input bitsize")
    S = 8sizeof(T)
    RS = 8sizeof(RT)
    prep = S == N ? "%x = or i$S %0, 0" : "%x = trunc i$S %0 to i$N"
    ext = N == RN ? "%r.0 = or i$x %x, 0" : "%r.0 = sext i$N %x to i$RN"
    zext = RN == RS ? "%r = or i$RN %r.0, 0" : "%r = zext i$RN %r.0 to i$RS"
    ret = "ret i$RS %r"
    code = join((prep, ext, zext, ret), "\n")
    quote
        Base.llvmcall($code, RT, Tuple{T}, x)
    end
end


function zext_int(::Type{RT}, x::T) where {RT, T}
    RT <: BitSized || T <: BitSized || return Core.Intrinsics.zext_int(RT, x)
    _zext_int(RT, x)
end

@generated function _zext_int(::Type{RT}, x::T) where {RT, T}
    RN = bitsizeof(RT)
    N = bitsizeof(T)
    N % 8 == RN % 8 == 0 && return Core.Intrinsics.sext_int(RT, x)
    RN ≤ N && error("ZExt: output bitsize must be > input bitsize")
    S = 8sizeof(T)
    RS = 8sizeof(RT)
    prep = S == N ? "%x = or i$S %0, 0" : "%x = trunc i$S %0 to i$N"
    ext = N == RN ? "%r.0 = or i$x %x, 0" : "%r.0 = zext i$N %x to i$RN"
    zext = RN == RS ? "%r = or i$RN %r.0, 0" : "%r = zext i$RN %r.0 to i$RS"
    ret = "ret i$RS %r"
    code = join((prep, ext, zext, ret), "\n")

    quote
        Base.llvmcall($code, RT, Tuple{T}, x)
    end
end



bitcast(::Type{RT}, x::T) where {RT, N, T<:BitSized{N}} = reinterpret(RT, x)



for F in (:sitofp, :uitofp)
    ex = quote
        @generated function $F(::Type{FT}, x::T) where {FT<:Union{Float16,Float32,Float64}, N, T<:BitSized{N}}
            S = 8sizeof(T)
            op = $(QuoteNode(F))
            ft = FT<:Float16 ? "half" :
                FT <: Float32 ? "float" :
                "double"

            code = """
            %x = trunc i$S %0 to i$N
            %ret = $op i$N %x to $ft
            ret $ft %ret
            """
            quote
                Base.llvmcall($code, FT, Tuple{T}, x)
            end
        end
    end
    @eval $ex
end


@generated function bswap_int(x::T) where {N, T<:BitSized{N}}
    S = 8sizeof(T)
    code = """
       %x = trunc i$S %0 to i$N
       %r = call i$N @llvm.bswap.i$N(i$N %x)
       %ret = zext i$N %r to i$S
       ret i$S %ret
    """
    quote
        Base.llvmcall($code, T, Tuple{T}, x)
    end
end


@generated function shl_int(x::T, y::T) where {N, T<:BitSized{N}}
    S = 8sizeof(T)
    code = """
       %x = trunc i$S %0 to i$N
       %y = trunc i$S %1 to i$N
       %3 = shl i$N %x, %y
;       %4 = icmp ugt i$N %y, $(N-1)
;       %r = select i1 %4, i$N 0, i$N %3
       %ret = zext i$N %3 to i$S
       ret i$S %ret
    """
    quote
        Base.llvmcall($code, T, Tuple{T,T}, x, y)
    end
end

@generated function neg_int(x::T) where {N, T<:BitSized{N}}
    S = 8sizeof(T)
    code = """
       %x = trunc i$S %0 to i$N
       %r = sub i$N 0, %x
       %ret = zext i$N %r to i$S
       ret i$S %ret
    """
    quote
        Base.llvmcall($code, T, Tuple{T}, x)
    end
end

@generated function not_int(x::T) where {N, T<:BitSized{N}}
    S = 8sizeof(T)
    code = """
       %x = trunc i$S %0 to i$N
       %r = xor i$N %x, -1
       %ret = zext i$N %r to i$S
       ret i$S %ret
    """
    quote
        Base.llvmcall($code, T, Tuple{T}, x)
    end
end


# arith shift right
@generated function ashr_int(x::T, y::T) where {N, T<:BitSized{N}}
    S = 8sizeof(T)
    code = """
       %x = trunc i$S %0 to i$N
       %y = trunc i$S %1 to i$N
       %v = call i$N @llvm.umin.i$N(i$N %y, i$N $(N-1))
       %r = ashr i$N %x, %v
       %ret = zext i$N %r to i$S
       ret i$S %ret
    """
    quote
        Base.llvmcall($code, T, Tuple{T,T}, x, y)
    end
end

@generated function flipsign_int(x::T, y::T) where {N, T<:BitSized{N}}
    S = 8sizeof(T)
    code = """
       %x = trunc i$S %0 to i$N
       %y = trunc i$S %1 to i$N
       %v = ashr i$N %y, $(N-1)
       %3 = add i$N %v, %x
       %r = xor i$N %3, %v
       %ret = zext i$N %r to i$S
       ret i$S %ret
    """
    quote
        Base.llvmcall($code, T, Tuple{T,T}, x, y)
    end
end

@generated function lshr_int(x::T, y::T) where {N, T<:BitSized{N}}
    S = 8sizeof(T)
    code = """
       %x = trunc i$S %0 to i$N
       %y = trunc i$S %1 to i$N
       %s = lshr i$N %x, %y
       %t = icmp ugt i$N %y, $(N-1)
       %r = select i1 %t, i$N 0, i$N %s
       %ret = zext i$N %r to i$S
       ret i$S %ret
    """
    quote
        Base.llvmcall($code, T, Tuple{T,T}, x, y)
    end
end

end  # module Intrinsics
