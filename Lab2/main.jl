using JSON, HTTP

const ALPH = ["0", "1", "2", "3", "4", "5", "6", "7", "8", "9", "ε"]

function word_membership(word)
    payload = JSON.json(Dict("word" => word))
    resp = HTTP.post("http://localhost:8080/checkWord", body=payload, headers=["Content-Type" => "application/json"])
    if resp.status != 200
        error("Ошибка во время соединения")
    end
    res = JSON.parse(String(resp.body))
    return res["response"] == "1"
end

function all_suffs(word)
    ans = []
    last_ind = lastindex(word)
    for w_ind in eachindex(word)
        push!(ans, word[w_ind:last_ind])
    end
    return ans
end

function printing(main_prefixes, non_main_prefixes, suffixes, Table)
    prefs = unique(vcat(main_prefixes, non_main_prefixes))
    sufs = unique(suffixes)
    println("\t", join(sufs, " | "))
    println('-' ^ (length(sufs) * 5))
    for p in prefs
        r = []
        for s in sufs
            push!(r, get(Table, (p, s), false) ? "1" : "0")
        end
        println(p, "\t", join(r, " | "))
    end
end

function equal(main_prefixes, non_main_prefixes, suffixes, Table)
    m_prefs = collect(copy(main_prefixes))
    n_m_prefs = collect(copy(non_main_prefixes))
    sufs = collect(copy(suffixes))
    new_t = join((Table[pr, su] == true ? "1" : "0" for su ∈ sufs, pr ∈ vcat(m_prefs, n_m_prefs)), " ")
    data = JSON.json(Dict(
        "main_prefixes" => join(m_prefs, " "),
        "non_main_prefixes" => join(n_m_prefs, " "),
        "suffixes" => join(sufs, " "),
        "table" => new_t
    ))
    resp = HTTP.post("http://localhost:8080/checkTable", body = data, headers=["Content-Type" => "application/json"])
    if resp.status == 200
        res = JSON.parse(String(resp.body))
        return res["response"]
    end
end

function solve(main_prefixes, non_main_prefixes, suffixes, Table)
    while true
        r = Set()
        for pr in main_prefixes
            r2 = collect(Table[pr, s] for s in suffixes)
            push!(r, r2)
        end

        for pr in non_main_prefixes
            r2 = collect(Table[pr, s] for s in suffixes)
            if !(r2 in r)
                push!(r, r2)
                non_main_prefixes = filter(elem -> elem != pr, non_main_prefixes)
                push!(main_prefixes, pr)
            end
        end
        counter = equal(main_prefixes, non_main_prefixes, suffixes, Table)
        if counter == "true"
            printing(main_prefixes, non_main_prefixes, suffixes, Table)
            println("THE END")
            exit
            break
        end
        for s in all_suffs(counter)
            if s ∉ suffixes
                push!(suffixes, s)
                for pr in vcat(main_prefixes, non_main_prefixes)
                    w = pr * s
                    w = replace(w, "ε" => "")
                    Table[pr, s] = word_membership(w)
                end
            end
        end
        for pr in main_prefixes
            for symb in filter(x -> x != "ε", ALPH)
                np = pr * symb
                if np ∉ vcat(main_prefixes, non_main_prefixes)
                    push!(non_main_prefixes, np)
                    for s in suffixes
                        w = np * s
                        w = replace(w, "ε" => "")
                        Table[np, s] = word_membership(w)
                    end
                end
            end
        end
    end
end

function main()
    print("Введите режим работы: ")
    mode = readline()
    payload = JSON.json(Dict("mode" => mode))
    _ = HTTP.post("http://localhost:8080/generate", body=payload, headers=["Content-Type" => "application/json"])
    main_prefixes = ["ε"]
    non_main_prefixes = filter(x -> x != "ε", ALPH)
    suffixes = ["ε"]
    Table = Dict()

    for p in vcat(main_prefixes, non_main_prefixes)
        for s in suffixes
            w = p * s
            if !(p == "ε" && s == "ε")
                w = replace(w, "ε" => "")
            else
                w = "ε"
            end
            Table[p, s] = word_membership(w)
        end
    end
    solve(main_prefixes, non_main_prefixes, suffixes, Table)
end

elapsed_time = @elapsed main()
println("TIME: $elapsed_time seconds")
