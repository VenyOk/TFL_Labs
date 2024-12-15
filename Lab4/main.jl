const MAX_GROUPS = 9

# Определение структур для различных типов узлов AST
struct group_node
    id::Int
    nd
end

struct star_node
    nd
end

struct char_node
    char::Char
end

struct expr_ref_node
    id::Int
end

struct alt_node
    branch
end

struct concat_node
    nds
end

struct look_ahead_node
    nd
end

struct non_group_node
    nd
end

# Определение структуры для токенов
struct token
    type::String
    val
end

# Определение структуры для лексера
mutable struct lex
    s::String
    index::Int
    lex(str) = new(str, 1)
end

# Функция для получения текущего символа
current(L::lex) = L.index <= length(L.s) ? L.s[L.index] : nothing

# Функция для токенизации входной строки
function tokenize(L::lex)
    t = []
    while L.index <= length(L.s)
        symbol = current(L)
        if symbol == '|'
            push!(t, token("alternative", nothing))
            L.index += 1
        elseif symbol == '*'
            push!(t, token("star", nothing))
            L.index += 1
        elseif symbol == ')'
            push!(t, token("close_br", nothing))
            L.index += 1
        elseif symbol !== nothing && (symbol >= 'a' && symbol <= 'z')
            push!(t, token("char", symbol))
            L.index += 1
        elseif symbol == '('
            L.index += 1
            symbol3 = current(L)
            if symbol3 !== nothing
                if symbol3 == '?'
                    L.index += 1
                    symbol4 = current(L)
                    if symbol4 == '='
                        push!(t, token("look_ahead_open", nothing))
                        L.index += 1
                    elseif symbol4 == ':'
                        push!(t, token("non_cap_open", nothing))
                        L.index += 1
                    elseif symbol4 >= '0' && symbol4 <= '9'
                        push!(t, token("expr_ref_open", Int(symbol4)))
                        L.index += 1
                    else
                        error("Некорректный символ после '?': $symbol4")
                    end
                else
                    push!(t, token("open_br", nothing))
                end
            end
        else
            error("Неопознанный символ $symbol")
        end
    end
    return t
end

# Определение структуры для состояния парсера
mutable struct ParsingState
    index::Int
    l_ahead::Bool
    group::Int # количество групп
    t::Vector{token}
    groups::Dict
end

# Конструктор для состояния парсера
function ParsingState(;
    index=1, # Текущая позиция
    l_ahead=false, 
    group=0, # Количество групп
    t=Vector{token}(), # вектор токенов
    groups=Dict(),
)
    new(index, l_ahead, group, t, groups)
end

# Функция для получения текущего токена
cur_t(p::ParsingState) = p.index <= length(p.t) ? p.t[p.index] : nothing

# Функция для проверки и потребления текущего токена
function check_token(p::ParsingState, t=nothing)
    token = cur_t(p)
    if token === nothing
        error("Ошибка! Конец выражения")
    end
    if token.type != t
        error("Неверный тип токена! Ожидался $t")
    end
    p.index += 1
    return token
end

# Основная функция для парсинга
function parsing(p::ParsingState)
    ast = parsing_alt(p)
    if cur_t(p) !== nothing
        error("Лишние символы после корректного выражения")
    end
    refs(p, ast, Set())
    return ast
end

# Функция для парсинга альтернатив
function parsing_alt(p::ParsingState)
    br = [parsing_concat(p)]
    while cur_t(p) !== nothing && cur_t(p).type == "alternative"
        check_token(p, "alternative")
        push!(br, parsing_concat(p))
    end
    return length(br) == 1 ? br[1] : alt_node(br)
end

# Функция для парсинга конкатенаций
function parsing_concat(p::ParsingState)
    nd = []
    while cur_t(p) !== nothing && cur_t(p).type ∉ ["alternative", "close_br"]
        push!(nd, parsing_star(p))
    end
    return length(nd) == 1 ? nd[1] : concat_node(nd)
end

# Функция для парсинга звездочек
function parsing_star(p::ParsingState)
    nd = parsing_base(p)
    while cur_t(p) !== nothing && cur_t(p).type == "star"
        check_token(p, "star")
        nd = star_node(nd)
    end
    return nd
end

# Функция для парсинга базовых выражений
function parsing_base(p::ParsingState)
    token = cur_t(p)
    println("parsing_base = $token")
    if token === nothing
        error("Конец выражения")
    end
    if token.type == "non_cap_open"
        check_token(p, "non_cap_open")
        nd = parsing_alt(p)
        check_token(p, "close_br")
        return non_group_node(nd)
    elseif token.type == "char"
        v = token.val
        check_token(p, "char")
        return char_node(v)
    elseif token.type == "look_ahead_open"
        if !(p.l_ahead)
            check_token(p, "look_ahead_open")
            temp_l_ahead = p.l_ahead
            p.l_ahead = true
            nd = parsing_alt(p)
            p.l_ahead = temp_l_ahead
            check_token(p, "close_br")
            return look_ahead_node(nd)
        else
            error("Запрещены вложенные опережающие проверки")
        end
    elseif token.type == "expr_ref_open"
        id = token.val
        check_token(p, "expr_ref_open")
        check_token(p, "close_br")
        return expr_ref_node(id)
    elseif token.type == "open_br" # Скобки
        check_token(p, "open_br")
        if p.group == MAX_GROUPS
            error("Количество групп захвата превышает $MAX_GROUPS")
        end
        p.group += 1
        id = p.group
        nd = parsing_alt(p)
        check_token(p, "close_br")
        p.groups[id] = nd
        return group_node(id, nd)
    else
        error("Неизвестный токен $token")
    end
end


# Функция для проверки ссылок на группы
function refs(p::ParsingState, nd, g)
    println("ND = $nd")
    if isa(nd, group_node)
        new_set = refs(p, nd.nd, g)
        push!(new_set, nd.id)
        return new_set
    elseif isa(nd, non_group_node)
        return refs(p, nd.nd, g)
    elseif isa(nd, concat_node)
        current_set = copy(g)
        for j in nd.nds
            current_set = refs(p, j, current_set)
        end
        return current_set
    elseif isa(nd, expr_ref_node)
        if nd.id ∉ keys(p.groups)
            error("Ошибка: Ссылка на несуществующую группу")
        end
        if nd.id ∉ g
            error("Ошибка: Ссылка на группу вне области видимости")
        end
        return g
    elseif isa(nd, char_node)
        return g
    elseif isa(nd, star_node)
        return refs(p, nd.nd, g)
    elseif isa(nd, look_ahead_node)
        internal_check(p, nd.nd, true)
        return refs(p, nd.nd, g)
    elseif isa(nd, alt_node)
        combined_set = Set(g)
        for br in nd.branch
            branch_set = refs(p, br, g)
            union!(combined_set, branch_set)
        end
        return combined_set
    else
        error("Неизвестный тип node")
    end
end

# Функция для внутренней проверки узлов
function internal_check(p::ParsingState, nd, l::Bool)
    if isa(nd, look_ahead_node) || isa(nd, group_node)
        if l
            error("Внутри опережающей проверки невозможны опережающие проверки или захватывающие группы")
        end
    end

    if isa(nd, non_group_node) || isa(nd, look_ahead_node) || isa(nd, star_node)
        internal_check(p, nd.nd, l)
    elseif isa(nd, alt_node)
        for br in nd.branch
            internal_check(p, br, l)
        end
    elseif isa(nd, concat_node)
        for node in nd.nds
            internal_check(p, node, l)
        end
    end
end

# Определение структуры для правил грамматики
struct GrammarRule
    lhs::String
    rhs::Vector{Vector{String}}
end

# Функция для создания уникального нетерминала
function fresh_nonterminal(prefix::String, index::Ref{Int})
    index[] += 1
    return "$(prefix)$(index[])"
end

# Функция для построения каркасной КС-грамматики
function build_cfg(ast)
    rules = Dict{String, Vector{Vector{String}}}()
    nonterminal_index = Ref(0)

    # Вспомогательная функция для создания правил грамматики
    function create_rule(node, lhs::String)
        if isa(node, char_node)
            rules[lhs] = [[string(node.char)]]
        elseif isa(node, group_node)
            group_nt = "G$(node.id)"
            create_rule(node.nd, group_nt)
            rules[lhs] = [[group_nt]]
        elseif isa(node, non_group_node)
            sub_nt = fresh_nonterminal("N", nonterminal_index)
            create_rule(node.nd, sub_nt)
            rules[lhs] = [[sub_nt]]
        elseif isa(node, look_ahead_node)
            rules[lhs] = [[]]  # Опережающая проверка заменяется на ε
        elseif isa(node, concat_node)
            sub_nts = [fresh_nonterminal("C", nonterminal_index) for _ in node.nds]
            for (i, sub_node) in enumerate(node.nds)
                create_rule(sub_node, sub_nts[i])
            end
            rules[lhs] = [sub_nts]
        elseif isa(node, alt_node)
            sub_nts = [fresh_nonterminal("A", nonterminal_index) for _ in node.branch]
            for (i, branch) in enumerate(node.branch)
                create_rule(branch, sub_nts[i])
            end
            rules[lhs] = [[sub_nt] for sub_nt in sub_nts]
        elseif isa(node, star_node)
            sub_nt = fresh_nonterminal("R", nonterminal_index)
            create_rule(node.nd, sub_nt)
            rules[lhs] = [[], [lhs, sub_nt]]  # R -> ε | R sub_nt
        elseif isa(node, expr_ref_node)
            group_nt = "G$(node.id)"
            rules[lhs] = [[group_nt]]
        else
            error("Неизвестный тип узла AST")
        end
    end

    # Начальный нетерминал
    start_symbol = "S"
    create_rule(ast, start_symbol)

    # Преобразование правил в структуру GrammarRule
    grammar_rules = [GrammarRule(lhs, rhs) for (lhs, rhs) in rules]

    return start_symbol, grammar_rules
end

function main()
    try
        expr = readline()
        LEX = lex(expr)
        tokens = tokenize(LEX)
        for t in tokens
            println("$(t.type), $(t.val)")
        end

        for i in 1:20
            print("-")
        end
        println()

        p = ParsingState(1, false, 0, tokens, Dict())
        ast = parsing(p)
        for i in 1:20
            print("-")
        end
        println()

        println("Регулярное выражение корректно")

        # Построение каркасной КС-грамматики
        start_symbol, grammar_rules = build_cfg(ast)
        println("Построенная КС-грамматика (каркас):")
        println("Начальный нетерминал: $start_symbol")
        for rule in grammar_rules
            for rhs in rule.rhs
                rhs_str = rhs == [] ? "ε" : join(rhs, " ")
                println("$(rule.lhs) -> $rhs_str")
            end
        end
    catch e
        println("Ошибка: ", e)
    end
end


main()