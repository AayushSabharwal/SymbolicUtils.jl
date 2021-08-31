export PolyForm, simplify_fractions
using Bijections
using DynamicPolynomials: PolyVar

"""
    PolyForm{T} <: Symbolic{T}

Abstracts a [MultivariatePolynomials.jl](https://juliaalgebra.github.io/MultivariatePolynomials.jl/stable/) as a SymbolicUtils expression and vice-versa.

The SymbolicUtils term interface (`istree`, `operation, and `arguments`) works on PolyForm lazily: the `operation` and `arguments` are created by converting one level of arguments into SymbolicUtils expressions. They may further contain PolyForm within them.
We use this to hold polynomials in memory while doing `simplify_fractions`.

    PolyForm{T}(x; Fs=Union{typeof(*),typeof(+),typeof(^)}, recurse=false)

Turn a Symbolic expression `x` into a polynomial and return a PolyForm that abstracts it.

`Fs` are the types of functions which should be applied if arguments are themselves
polynomialized. For example, if you only want to polynomialize the base of power
expressions, you would  leave out `typeof(^)` from the union. In this case `^`
is not called, but maintained as a `Pow` term.

`recurse` is a flag which calls `PolyForm` recursively on subexpressions. For example:

```julia
PolyForm(sin((x+y)^2))               #=> sin((x+y)^2)
PolyForm(sin((x+y)^2), recurse=true) #=> sin((x^2 + (2x)y + y^2))
```
"""
struct PolyForm{T, M} <: Symbolic{T}
    p::MP.AbstractPolynomialLike
    pvar2sym::Bijection{Any,Any}   # @polyvar x --> @sym x  etc.
    sym2term::Dict{Sym,Any}        # Symbol("sin-$hash(sin(x+y))") --> sin(x+y) => sin(PolyForm(...))
    metadata::M
end

function (::Type{PolyForm{T}})(p, d1, d2, m=nothing) where {T}
    p isa Number && return p
    p isa MP.AbstractPolynomialLike && MP.isconstant(p) && return convert(Number, p)
    PolyForm{T, typeof(m)}(p, d1, d2, m)
end

Base.hash(p::PolyForm, u::UInt64) = xor(hash(p.p, u),  trunc(UInt, 0xbabacacababacaca))
Base.isequal(x::PolyForm, y::PolyForm) = isequal(x.p, y.p)

# We use the same PVAR2SYM bijection to maintain the PolyVar <-> Sym mapping,
# When all PolyForms go out of scope in a session, we allow it to free up memory and
# start over if necessary
const PVAR2SYM = Ref(WeakRef())
const SYM2TERM = Ref(WeakRef())
clear_dicts() = (PVAR2SYM[] = WeakRef(nothing); SYM2TERM[] = WeakRef(nothing); nothing)
function get_pvar2sym()
    v = PVAR2SYM[].value
    if v === nothing
        d = Bijection{Any,Any}()
        PVAR2SYM[] = WeakRef(d)
        return d
    else
        return v
    end
end

function get_sym2term()
    v = SYM2TERM[].value
    if v === nothing
        d = Dict{Sym,Any}()
        SYM2TERM[] = WeakRef(d)
        return d
    else
        return v
    end
end

function mix_dicts(p, q)
    p.pvar2sym !== q.pvar2sym && error("pvar2sym mappings don't match for $p and $q")
    p.sym2term !== q.sym2term && error("sym2term mappings don't match for $p and $q")

    p.pvar2sym, p.sym2term
end

# forward gcd

PF = :(PolyForm{promote_symtype(/, symtype(x), symtype(y))})
@eval begin
    Base.div(x::PolyForm, y::PolyForm) = $PF(div(x.p, y.p), mix_dicts(x, y)...)
    Base.div(x::Integer, y::PolyForm)  = $PF(div(x, y.p), y.pvar2sym, y.sym2term)
    Base.div(x::PolyForm, y::Integer)  = $PF(div(x.p, y), x.pvar2sym, x.sym2term)

    Base.gcd(x::PolyForm, y::PolyForm) = $PF(_gcd(x.p, y.p), mix_dicts(x, y)...)
    Base.gcd(x::Integer, y::PolyForm)  = $PF(_gcd(x, y.p), y.pvar2sym, y.sym2term)
    Base.gcd(x::PolyForm, y::Integer)  = $PF(_gcd(x.p, y), x.pvar2sym, x.sym2term)
end

_isone(p::PolyForm) = isone(p.p)

function polyize(x, pvar2sym, sym2term, vtype, pow, Fs, recurse)
    if x isa Number
        return x
    elseif istree(x)
        if !(symtype(x) <: Number)
            error("Cannot convert $x of symtype $(symtype(x)) into a PolyForm")
        end

        op = operation(x)
        args = arguments(x)

        local_polyize(y) = polyize(y, pvar2sym, sym2term, vtype, pow, Fs, recurse)

        if typeof(+) <: Fs && op == (+)
            return sum(local_polyize, args)
        elseif typeof(*) <: Fs && op == (*)
            return prod(local_polyize, args)
        elseif typeof(^) <: Fs && op == (^) && args[2] isa Integer && args[2] > 0
            @assert length(args) == 2
            return local_polyize(args[1])^(args[2])
        else
            # create a new symbol to store this

            y = if recurse
                similarterm(x,
                            op,
                            map(a->PolyForm(a, pvar2sym, sym2term, vtype; Fs, recurse),
                                args), symtype(x))
            else
                x
            end

            name = Symbol(string(op), "_", hash(y))

            @label lookup
            sym = Sym{symtype(x)}(name)
            if haskey(sym2term, sym)
                if isequal(sym2term[sym][1], x)
                    return local_polyize(sym)
                else # hash collision
                    name = Symbol(name, "_")
                    @goto lookup
                end
            end

            sym2term[sym] = (x => y)

            return local_polyize(sym)
        end
    elseif issym(x)
        if haskey(active_inv(pvar2sym), x)
            return pvar2sym(x)
        end
        pvar = MP.similarvariable(vtype, nameof(x))
        pvar2sym[pvar] = x
        return pvar
    end
end

function PolyForm(x,
        pvar2sym=get_pvar2sym(),
        sym2term=get_sym2term(),
        vtype=DynamicPolynomials.PolyVar{true};
        Fs = Union{typeof(+), typeof(*), typeof(^)},
        recurse=false,
        metadata=metadata(x))

    # Polyize and return a PolyForm
    p = polyize(x, pvar2sym, sym2term, vtype, pow, Fs, recurse)
    PolyForm{symtype(x)}(p, pvar2sym, sym2term, metadata)
end

TermInterface.istree(x::Type{PolyForm}) = true

TermInterface.operation(x::PolyForm) = MP.nterms(x.p) == 1 ? (*) : (+)

function TermInterface.arguments(x::PolyForm{T}) where {T}

    function is_var(v)
        MP.nterms(v) == 1 &&
        isone(MP.coefficient(MP.terms(v)[1])) &&
        MP.degree(MP.monomial(v)) == 1
    end

    function get_var(v)
        # must be called only after a is_var check
        MP.variable(MP.monomial(v))
    end

    function resolve(p)
        !is_var(p) && return p
        pvar = get_var(p)
        s = x.pvar2sym[pvar]
        haskey(x.sym2term, s) ? x.sym2term[s][2] : s
    end

    if MP.nterms(x.p) == 1
        MP.isconstant(x.p) && return [convert(Number, x.p)]
        c = MP.coefficient(x.p)
        t = MP.monomial(x.p)

        if !isone(c)
            [c, (unstable_pow(resolve(v), pow)
                        for (v, pow) in MP.powers(t) if !iszero(pow))...]
        else
            [unstable_pow(resolve(v), pow)
                    for (v, pow) in MP.powers(t) if !iszero(pow)]
        end
    else
        ts = MP.terms(x.p)
        return [MP.isconstant(t) ?
                convert(Number, t) :
                (is_var(t) ?
                 resolve(t) :
                 PolyForm{T, Nothing}(t, x.pvar2sym, x.sym2term, nothing)) for t in ts]
    end
end

Base.show(io::IO, x::PolyForm) = show_term(io, x)

"""
    expand(expr)

Expand expressions by distributing multiplication over addition, e.g.,
`a*(b+c)` becomes `ab+ac`.

`expand` uses replace symbols and non-algebraic expressions by variables of type
`variable_type` to compute the distribution using a specialized sparse
multivariate polynomials implementation.
`variable_type` can be any subtype of `MultivariatePolynomials.AbstractVariable`.
"""
expand(expr) = PolyForm(expr, Fs=Union{typeof(+), typeof(*), typeof(^)}, recurse=true)


## Rational Polynomial form with Div

function polyform_factors(d::Div, pvar2sym, sym2term)
    make(xs) = map(xs) do x
        if x isa Pow && arguments(x)[2] isa Integer && arguments(x)[2] > 0
            # here we do want to recurse one level, that's why it's wrong to just
            # use Fs = Union{typeof(+), typeof(*)} here.
            Pow(PolyForm(arguments(x)[1], pvar2sym, sym2term), arguments(x)[2])
        else
            PolyForm(x, pvar2sym, sym2term)
        end
    end

    return make(numerators(d)), make(denominators(d))
end

_mul(xs...) = all(isempty, xs) ? 1 : *(Iterators.flatten(xs)...)

function simplify_fractions(d::Div)
    d.simplified && return d
    ns, ds = polyform_factors(d, get_pvar2sym(), get_sym2term())
    ns, ds = rm_gcds(ns, ds)
    if all(_isone, ds)
        return isempty(ns) ? 1 : simplify_fractions(_mul(ns))
    else
        return Div(simplify_fractions(_mul(ns)), simplify_fractions(_mul(ds)), true)
    end
end

function add_divs(x::Div, y::Div)
    x_num, x_den = polyform_factors(x, get_pvar2sym(), get_sym2term())
    y_num, y_den = polyform_factors(y, get_pvar2sym(), get_sym2term())

    Div(_mul(x_num, y_den) + _mul(x_den, y_num), _mul(x_den, y_den))
end

"""
    simplify_fractions(x)

Find `Div` nodes and simplify them by cancelling a set of factors of numerators
and denominators. It may leave some expressions in `PolyForm` format.
"""
function simplify_fractions(x)
    !has_div(x) && return x

    isdiv(x) = x isa Div

    rules = [@rule ~x::isdiv => simplify_fractions(~x)
             @acrule ~a::isdiv + ~b::isdiv => add_divs(~a,~b)]

    Fixpoint(Postwalk(Chain(rules)))(x)
end

function has_div(x)
    return x isa Div || (istree(x) && any(has_div, unsorted_arguments(x)))
end

flatten_pows(xs) = map(xs) do x
    x isa Pow ? Iterators.repeated(arguments(x)...) : (x,)
end |> Iterators.flatten |> a->collect(Any,a)

coefftype(x::PolyForm) = coefftype(x.p)
coefftype(x::MP.AbstractPolynomialLike{T}) where {T} = T
coefftype(x) = typeof(x)

const MaybeGcd = Union{PolyForm, MP.AbstractPolynomialLike, Integer}
_gcd(x::MaybeGcd, y::MaybeGcd) = (coefftype(x) <: Complex || coefftype(y) <: Complex) ? 1 : gcd(x, y)
_gcd(x, y) = 1

function rm_gcds(ns, ds)
    ns = flatten_pows(ns)
    ds = flatten_pows(ds)

    for i = 1:length(ns)
        for j = 1:length(ds)
            g = _gcd(ns[i], ds[j])
            if !_isone(g)
                ns[i] = div(ns[i], g)
                ds[j] = div(ds[j], g)
            end
        end
    end

    filter!(!_isone, ns)
    filter!(!_isone, ds)

    ns,ds
end
