# * BitIntegers

module BitIntegers


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

const BitUnsigned_types =
    tuple((getfield(@__MODULE__, (Symbol(:UInt, n))) for n in _DEFINED_SIZES)...)

const BitSigned_types =
    tuple((getfield(@__MODULE__, (Symbol(:Int, n))) for n in _DEFINED_SIZES)...)

const BitInteger_types = (BitSigned_types..., BitUnsigned_types...)

const BitUnsigned = Union{BitUnsigned_types...}
const BitSigned   = Union{BitSigned_types...}
const BitInteger  = Union{BitInteger_types...}


end # module
