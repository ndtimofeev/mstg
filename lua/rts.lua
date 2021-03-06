#ifdef BACKPORT
if ( _VERSION == "Lua 5.1" ) then
    table.unpack = function( ... ) return unpack( ... ) end
end
#endif

#ifdef SIMPLEALLOC
ALLOC = function( ... )
    return { ... }
end

RELEASE = function( arr )
    arr = nil
end
#else
ARRAY_HOLDER = setmetatable( {}, { __mode = 'v' } )
ARRAY_HOLDER_SUCCESS = 0
ARRAY_HOLDER_MISS = 0

ALLOC = function( ... )
    local ARRAY_HOLDER = ARRAY_HOLDER
    local ref

    if #ARRAY_HOLDER > 0 then
        ref = ARRAY_HOLDER[#ARRAY_HOLDER]
        ARRAY_HOLDER[#ARRAY_HOLDER] = nil

        for i = 1, select( "#", ... ) do
            ref[i] = select( i, ... )
        end

        for i = select( "#", ... ) + 1, #ref do
            ref[i] = nil
        end

        ARRAY_HOLDER_SUCCESS = ARRAY_HOLDER_SUCCESS + 1
        return ref
    end

    ARRAY_HOLDER_MISS = ARRAY_HOLDER_MISS + 1
    return { ... }
end

RELEASE = function( arr )
    table.insert( ARRAY_HOLDER, arr )
    arr = nil
end
#endif

local ARGUMENTS_STACK = {}
local RETURN_STACK = {}
local INDIRECTION_COUNT = 0
local ENTER_COUNT = 0
local MEM_MAX = 0
local ALLOCATED = 0
local THUNK_ALLOCATED = 0
local CONSTR_ALLOC = 0
local CONSTR_UPDATE = 0
local DEFAULT = {}
local NOT_ALLOC = {}

local POP = function()
    local val = ARGUMENTS_STACK[#ARGUMENTS_STACK]
    ARGUMENTS_STACK[#ARGUMENTS_STACK] = nil

    return val
end

local PUSH = function( val )
    ARGUMENTS_STACK[#ARGUMENTS_STACK + 1] = val
end

local ALLOCATE = function( itbl )
    local val = setmetatable( ALLOC(), itbl )

#ifdef RTSINFO
    CONSTR_ALLOC = CONSTR_ALLOC + 1
#endif

    for i = 1, itbl.arity do
        val[#val + 1] = POP()
    end

    return val
end

local BLACKHOLE_TABLE = {
    name = "BLACKHOLE",

    enter = function( self )
        error "BREAK ON THROUGH TO THE BLACKHOLE!!!"
    end
}

local EXIT_CODE_TABLE = {
    name = "EXIT",
    enter = function( self )
        local heapPtr = POP()
        local constr  = POP()

        if heapPtr == NOT_ALLOC then
            heapPtr = ALLOCATE( constr )
        end

        return heapPtr
    end
}
setmetatable( EXIT_CODE_TABLE, EXIT_CODE_TABLE )

local POP_CONTROL = function()
    local val = RETURN_STACK[#RETURN_STACK]
    RETURN_STACK[#RETURN_STACK] = nil

    return val
end

local PUSH_CONTROL = function( val )
    RETURN_STACK[#RETURN_STACK + 1] = val
end

local JUMP = function( cont )
#ifdef OTRACE
    return cont
#else
    return getmetatable( cont ).enter( cont )
#endif
end

local INDIRECTION_TABLE = {
    name = "INDIRECTION",

    enter = function( thunk )
        INDIRECTION_COUNT = INDIRECTION_COUNT + 1
        return JUMP( thunk[1] )
    end,
}

local DROP_STACK_ALL_TABLE = {
    name = "DROP_STACK_ALL",

    enter = function( cont )
        POP()
        local arity = POP().arity

        for i = 1, arity do
            POP()
        end

        return JUMP( POP_CONTROL() )
    end
}
setmetatable( DROP_STACK_ALL_TABLE, DROP_STACK_ALL_TABLE )

local DROP_STACK_VAR_TABLE = {
    name = "DROP_STACK_VAR",

    enter = function( cont )
        local heapPtr = POP()
        local constr  = POP()

        if heapPtr == NOT_ALLOC then
            heapPtr = ALLOCATE( constr )
        end

        for i = 1, constr.arity do
            POP()
        end

        PUSH( heapPtr )

        return JUMP( POP_CONTROL() )
    end
}
setmetatable( DROP_STACK_VAR_TABLE, DROP_STACK_VAR_TABLE )

local UPDATE_NEXT_TABLE = {
    name = "UPDATE_NEXT",

    enter = function( cont )
        local heapPtr = ARGUMENTS_STACK[#ARGUMENTS_STACK]
        local nextVal = POP_CONTROL()

        if heapPtr == NOT_ALLOC then
            local constr  = ARGUMENTS_STACK[#ARGUMENTS_STACK - 1]
            local j       = 1

            setmetatable( nextVal, constr )

            for i = #ARGUMENTS_STACK - 2, #ARGUMENTS_STACK - constr.arity - 1, -1 do
                nextVal[j] = ARGUMENTS_STACK[i]
                j = j + 1
            end

            for i = j, #nextVal do
                nextVal[i] = nil
            end

#ifdef RTSINFO
            CONSTR_UPDATE = CONSTR_UPDATE + 1
#endif

            ARGUMENTS_STACK[#ARGUMENTS_STACK] = nextVal
        else
            setmetatable( nextVal, INDIRECTION_TABLE )
            nextVal[1] = heapPtr

            for i = 2, #nextVal do
                nextVal[i] = nil
            end
        end

        return JUMP( POP_CONTROL() )
    end
}
setmetatable( UPDATE_NEXT_TABLE, UPDATE_NEXT_TABLE )

#ifndef VECRET
#ifndef LFUNCTION
local SWITCH_TABLE = {
    name = "SWITCH",
    enter = function( cont )
        return POP_CONTROL()()
    end
}
setmetatable( SWITCH_TABLE, SWITCH_TABLE )
#endif
#endif

#ifdef LFUNCTION
local LIGHT_FUNCTION_TABLE = {
    name = "LIGHT_FUNCTION",
    enter = function( cont )
        return cont()
    end
}
debug.setmetatable( function() end, LIGHT_FUNCTION_TABLE )
#endif

#ifdef VECRET
local VECTOR_TABLE = {
    name = "VECTOR",

    enter = function( cont )
        local heapPtr = POP()
        local constr  = POP()
        local tbl     = POP_CONTROL()

        return JUMP( tbl[constr] )
    end
}
VECTOR_TABLE[1] = VECTOR_TABLE

local VECTOR_WITH_VAR_TABLE = {
    name = "VECTOR_WITH_VAR",

    enter = function( cont )
        local heapPtr = POP()
        local constr  = POP()
        local tbl     = POP_CONTROL()

        local nextCont = tbl[constr]

        if nextCont == nil then
            nextCont = tbl[DEFAULT]

            if heapPtr == NOT_ALLOC then
                heapPtr = ALLOCATE( constr )
            end

            PUSH( heapPtr )
        end

        return JUMP( nextCont )
    end
}
setmetatable( VECTOR_WITH_VAR_TABLE, VECTOR_WITH_VAR_TABLE )

local VECTOR_WITH_EMPTY_TABLE = {
    name = "VECTOR_WITH_EMPTY",

    enter = function( cont )
        local heapPtr = POP()
        local constr  = POP()
        local tbl     = POP_CONTROL()

        local nextCont = tbl[constr]

        if nextCont == nil then
            for i = 1, constr.arity do
                POP()
            end

            nextCont = tbl[DEFAULT]
        end

        return JUMP( nextCont )
    end
}
setmetatable( VECTOR_WITH_EMPTY_TABLE, VECTOR_WITH_EMPTY_TABLE )
#endif

local FUNCTION0_TABLE = {
    name = "FUNCTION0",

    enter = function( cont )
        return cont[1]()
    end
}

local THUNK_TABLE = {
    name = "THUNK",

    enter = function( thunk )
        PUSH_CONTROL( thunk )
        PUSH_CONTROL( UPDATE_NEXT_TABLE )

        setmetatable( thunk, BLACKHOLE_TABLE )

        return thunk[1]()
    end,
}

local APPLY_THUNK_TABLE = {
    name = "APPLY_THUNK",

    enter = function( thunk )
        PUSH_CONTROL( thunk )
        PUSH_CONTROL( UPDATE_NEXT_TABLE )

        for i = #thunk, 2, -1 do
            PUSH( thunk[i] )
        end

        setmetatable( thunk, BLACKHOLE_TABLE )

        return JUMP( thunk[1] )
    end
}

local APPLY_TABLE = {
    name = "APPLY",

    enter = function( thunk )
        for i = #thunk, 2, -1 do
            PUSH( thunk[i] )
        end

        return JUMP( thunk[1] )
    end
}

local EVAL = function( cont )
    PUSH_CONTROL( EXIT_CODE_TABLE )

    while #RETURN_STACK > 0 do
#ifdef OTRACE
        print( cont, getmetatable( cont ).name )
#endif
#ifdef RTSINFO
        ENTER_COUNT = ENTER_COUNT + 1
#endif
        cont = getmetatable( cont ).enter( cont )
    end

    if cont == EXIT_CODE_TABLE then
        cont = getmetatable( cont ).enter( cont )
    end

    return cont
end

local THUNK = function( fun )
#ifdef RTSINFO
    k, a = collectgarbage("count")

    if ( k > MEM_MAX ) then
        MEM_MAX = k
    end

    ALLOCATED = ALLOCATED + 1
    THUNK_ALLOCATED = THUNK_ALLOCATED + 1
#endif

    return setmetatable( ALLOC( fun ), THUNK_TABLE )
end

local FUNCTION0 = function( fun )
#ifdef RTSINFO
    k, a = collectgarbage("count")

    if ( k > MEM_MAX ) then
        MEM_MAX = k
    end

    ALLOCATED = ALLOCATED + 1
#endif

    return setmetatable( ALLOC( fun ), FUNCTION0_TABLE )
end

local APPLY = function( closure, ... )
    local ARGUMENTS_STACK = ARGUMENTS_STACK
    local val

    for i = select( "#", ... ), 1, -1 do
        val = select( i, ... )

        table.insert( ARGUMENTS_STACK, val )
    end

    return closure
end

local ENTER = function( cont )
    val = FORCE( cont )
    INSPECT( val )
    io.write( "\n" )

#ifdef RTSINFO
    print( "allocated: ", ALLOCATED )
    print( "thunks: ", THUNK_ALLOCATED )
    print( "constr: ", CONSTR_ALLOC )
    print( "constr u: ", CONSTR_UPDATE )
    print( "entered: ", ENTER_COUNT )
    print( "indirected: ", INDIRECTION_COUNT, string.format( "%.0f", INDIRECTION_COUNT / ENTER_COUNT * 100 ) )
    print( "time used: ", os.clock() )
    print( "mem used: ", MEM_MAX )
#endif
end

FORCE = function( thunk )
    if ( type( thunk ) == "table" or type( thunk ) == "function" ) then
        thunk = EVAL( thunk )

        for i = 1, #thunk do
            thunk[i] = FORCE( thunk[i] )
        end

        return thunk
    else
        return thunk
    end
end

INSPECT = function( val )
    if type( val ) == "number" then
        io.write( string.format( "%.0f", val ) )
    elseif type( val ) == "table" then
        io.write( getmetatable( val ).name )
        if #val > 0 then
            io.write( " { " )
            for i = 1, #val do
                INSPECT( val[i] )
            end
            io.write( " } " )
        end
    else
        io.write( "unev" )
    end
end
