module Phew

using MethodAnalysis

function functions_in_module(m::Module)
    # Store functions found in the module
    module_functions = []

    # Get all names in the module
    for name in names(m, all=true, imported=false)
        # Check if the name is defined in the module
        if isdefined(m, name)
            # Get the value associated with the name
            potential_func = getfield(m, name)

            # Check if it's a function
            if potential_func isa Function
                push!(module_functions, potential_func)
            end
        end
    end

    # Return unique function names
    return unique(module_functions)
end

n(f::Core.Function) = nameof(f)
n(f::Core.MethodInstance) = f.def.name

function inspect_module(m::Module)
    # Get all functions defined in the module
    module_functions = functions_in_module(m)

    # Create a dictionary to store function callers
    function_callers = Dict()

    # Iterate through each function to find its callers
    for func in module_functions
        function_callers[func] =
            filter(f -> isdefined(m, n(f)),
            map(f -> f.second, direct_backedges(func)))
    end

    graph = "digraph mytest {\n rankdir=\"LR\""

    for (func, callers) in function_callers
        f = n(func)
        for caller in callers
            c = n(caller)
            graph *= "\n  $(c) -> $(f);"
        end
    end

    graph *= "\n}"
    graph
end

end

using .Phew

print(Phew.inspect_module(Blockchain))
