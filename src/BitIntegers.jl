# * BitIntegers

module BitIntegers

# * types definition & aliases

abstract type AbstractBitUnsigned <: Unsigned end
abstract type AbstractBitSigned   <: Signed   end


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

end # module
