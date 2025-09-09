const IDXS_SYM = :𝑖ᵢₓ
const IDXS_SYMREAL = Sym{SymReal}(IDXS_SYM; type = Vector{Int}, shape = Unknown(1))
const IDXS_SAFEREAL = Sym{SafeReal}(IDXS_SYM; type = Vector{Int}, shape = Unknown(1))
const IDXS_TREEREAL = Sym{TreeReal}(IDXS_SYM; type = Vector{Int}, shape = Unknown(1))

idxs_for_arrayop(::Type{SymReal}) = IDXS_SYMREAL
idxs_for_arrayop(::Type{SafeReal}) = IDXS_SAFEREAL
idxs_for_arrayop(::Type{TreeReal}) = IDXS_TREEREAL

macro arrayop(output_idx, expr, options...)
    rs = []
    reduce = +
    call = nothing

    extra = []
    for o in options
        if Meta.isexpr(o, :call) && o.args[1] == :in
            push!(rs, :($(o.args[2]) => $(o.args[3])))
        elseif Meta.isexpr(o, :(=)) && o.args[1] == :reduce
            reduce = o.args[2]
        elseif Meta.isexpr(o, :(=)) && o.args[1] == :term
            call = o.args[2]
        else
            push!(extra, o)
        end
    end
    @assert output_idx.head == :tuple

    oidxs = filter(x->x isa Symbol, output_idx.args)
    iidxs = find_indices(expr)
    vartype_ref = find_vartype_reference(expr)
    idxs  = union(oidxs, iidxs)
    fbody = call2term(deepcopy(expr))
    oftype(x,T) = :($x::$T)

    let_assigns = Expr(:block)
    push!(let_assigns.args, Expr(:(=), :__vartype, :($vartype($vartype_ref))))
    push!(let_assigns.args, Expr(:(=), :__idx, :($idxs_for_arrayop(__vartype))))
    for (i, idx) in enumerate(idxs)
        push!(let_assigns.args, Expr(:(=), idx, :(__idx[$i])))
    end
    push!(let_assigns.args, Expr(:(=), :__expr, fbody))
    push!(let_assigns.args, Expr(:(=), :__output_idx, :($OutIdxT{__vartype}($output_idx))))
    push!(let_assigns.args, Expr(:(=), :__ranges, :($RangesT{__vartype}($(rs...)))))
    return Expr(:let, let_assigns, quote
        $ArrayOp{__vartype}(__output_idx,
                 __expr,
                 $reduce,
                 $(call2term(call)),
                 __ranges)
    end) |> esc
end

function find_indices(expr, idxs=[])
    !(expr isa Expr) && return idxs
    if expr.head == :ref
        return append!(idxs, filter(x->x isa Symbol, expr.args[2:end]))
    elseif expr.head == :call && expr.args[1] == :getindex || expr.args[1] == getindex
        return append!(idxs, filter(x->x isa Symbol, expr.args[3:end]))
    else
        foreach(x->find_indices(x, idxs), expr.args)
        return idxs
    end
end

function find_vartype_reference(expr)
    !(expr isa Expr) && return nothing
    if expr.head == :ref
        return expr.args[1]
    elseif expr.head == :call && (expr.args[1] == :getindex || expr.args[1] === getindex)
        return expr.args[2]
    end
    for arg in expr.args
        res = find_vartype_reference(arg)
        res === nothing || return res
    end
    return nothing
end

function call2term(expr, arrs=[])
    !(expr isa Expr) && return :($unwrap($expr))
    if expr.head == :call
        if expr.args[1] == :(:)
            return expr
        end
        return Expr(:call, term, map(call2term, expr.args)...)
    elseif expr.head == :ref
        return Expr(:ref, call2term(expr.args[1]), expr.args[2:end]...)
    elseif expr.head == Symbol("'")
        return Expr(:call, term, adjoint, map(call2term, expr.args)...)
    end

    return Expr(expr.head, map(call2term, expr.args)...)
end
