-- meta hack to only locally consider metatable changes within this module

local old_meta_clones = {}
local new_meta_to_old = {}
local old_meta_to_new = {}

local function dupmeta(obj)
	local meta = debug.getmetatable(obj)
	if new_meta_to_old(meta) then return meta end
	local _meta = util.merge({},meta)
	old_meta_clones[meta] = util.merge({},meta)
	old_meta_to_new[meta] = _meta
	new_meta_to_old[_meta] = meta
	return _meta
end

local function newmeta(obj)
	local meta = debug.getmetatable(obj)
	local _meta = old_meta_to_new[meta]
	if not _meta then return meta end
	return setmetatable(obj,_meta)
end

local function oldmeta(obj)
	local meta = debug.getmetatable(obj)
	local _meta = new_meta_to_old[meta]
	if not _meta then return meta end
	return setmetatable(obj,_meta)
end

local function all_new_metas()
	for meta, _meta in pairs(old_meta_to_new) do
		util.merge(meta,_meta)
	end
end

local function all_old_metas()
	for meta, _meta in pairs(old_meta_clones) do
		util.merge(meta,_meta)
	end
end

local function ___get(o,k)
	newmeta(o)
	local v = o[k]
	oldmeta(o)
	return v
end

local function ___set(o,k,v)
	newmeta(o)
	o[k] = v
	oldmeta(o)
end

local function ___len(o)
	newmeta(o)
	local n = #o
	oldmeta(o)
	return n
end

-- GLOBAL BASE EXTENSIONS

local unpack = table.unpack

local function getname( )
	return debug.getinfo( 2, "n" ).name or "UNKNOWN"
end

local function pusherror( f, ... )
	local ret = table.pack( pcall( f, ... ) )
	if ret[ 1 ] then return unpack( ret, 2, ret.n ) end
	error( ret[ 2 ], 3 )
end

-- doesn't invoke __index
_ENV.rawnext = next

-- invokes __next
function _ENV.next( t, k )
	local m = newmeta( t )
	local f = m and rawget(m,'__next') or rawnext
	oldmeta(t)
	return pusherror( f, t, k )
end

-- truly raw pairs, ignores __next and __pairs
function _ENV.rawpairs( t )
	return rawnext, t, nil
end

-- doesn't invoke __index just like rawnext
function _ENV.rawinext( t, i )

	if type( t ) ~= "table" then
		error( "bad argument #1 to '" .. getname( ) .. "'(table expected got " .. type( t ) ..")", 2 )
	end

	if i == nil then
		i = 0
	elseif type( i ) ~= "number" then
		error( "bad argument #2 to '" .. getname( ) .. "'(number expected got " .. type( i ) ..")", 2 )
	elseif i < 0 then
		error( "bad argument #2 to '" .. getname( ) .. "'(index out of bounds, too low)", 2 )
	end

	i = i + 1
	local v = rawget( t, i )
	if v ~= nil then
		return i, v
	end
end

-- invokes __inext
function _ENV.inext( t, i )
	local m = newmeta( t )
	local f = m and rawget(m,'__inext') or rawinext
	oldmeta( t )
	return pusherror( f, t, i )
end

-- truly raw ipairs, ignores __inext and __ipairs
function _ENV.rawipairs( t )
	return function( self, key )
		return rawinext( self, key )
	end, t, nil
end

local _ipairs = ipairs

-- fix 5.4's ipairs to still respect __ipairs
function _ENV.ipairs( t )
	local m = newmeta( t )
	local f = m and rawget(m,'__ipairs') or _ipairs
	oldmeta( t )
	return pusherror( f, t, i )
end

function _ENV.setfenv( fn, env )
	if type( fn ) ~= "function" then
		fn = debug.getinfo( ( fn or 1 ) + 1, "f" ).func
	end
	local i = 0
	repeat
		i = i + 1
		local name = debug.getupvalue( fn, i )
		if name == "_ENV" then
			debug.upvaluejoin( fn, i, ( function( )
				return env
			end ), 1 )
			return env
		end
	until not name
end

function _ENV.getfenv( fn )
	if type( fn ) ~= "function" then
		fn = debug.getinfo( ( fn or 1 ) + 1, "f" ).func
	end
	local i = 0
	repeat
		i = i + 1
		local name, env = debug.getupvalue( fn, i )
		if name == "_ENV" then
			return env
		end
	until not name
end

-- GLOBAL UTILITY EXTENSIONS

_ENV.util = {}

function util.perform_lookup(t,s,f)
	for k,v in pairs(t) do
		if f ~= nil then v = f(v) end
		if v == s then return k end
	end
	return nil
end

function util.perform_index(t,s,f)
	for k,v in ipairs(t) do
		if f ~= nil then v = f(v) end
		if v == s then return k end
	end
	return nil
end

function util.build_lookup(t,f)
	local l = {}
	for k,v in pairs(t) do
		if f ~= nil then v = f(v) end
		if v ~= nil then l[v] = k end
	end
	return l
end

function util.build_index(t,f)
	local l = {}
	for k,v in ipairs(t) do
		if f ~= nil then v = f(v) end
		if v ~= nil then l[v] = k end
	end
	return l
end

function util.clear(t)
	for k in pairs(t) do
		t[k] = nil
	end
end

function util.iclear(...)
	local n = 0
	for _,t in util.vararg(...) do
		local l = #t
		if l > n then n = l end
	end
	if n == 0 then return end
	for _,t in util.vararg(...) do
		for i = 1, n do
			t[i] = nil
		end
	end
end

function util.merge(m,...)
	for _,t in util.vararg(...) do
		for k,v in pairs(t) do
			m[k] = v
		end
	end
	return m
end

--http://lua-users.org/wiki/VarargTheSecondClassCitizen
do
	function util.vararg(...)
		local i, t, l = 0, {}
		local function iter(...)
			i = i + 1
			if i > l then return end
			return i, t[i]
		end

		--i = 0
		l = select("#", ...)
		for n = 1, l do
			t[n] = select(n, ...)
		end
		--[[
		for n = l+1, #t do
			t[n] = nil
		end
		--]]
		return iter
	end
end

-- GLOBAL PROXY EXTENSIONS

_ENV.proxy = {}

local function null() return nil end

local function define_proxy_meta(name,get_names,exists,get,set,id_register)
	local _get_names = function(s)
		local id = id_register[s]
		return get_names(id)
	end
	return {
		__name = name,
		__index = function(s,k)
			if type(k) == "number" then
				local names = _get_names(s)
				if names == nil or #names == 0 then return nil end
				k = names[k]
				if k == nil then return nil end
				local id = id_register[s]
				return (get(id,k))
			end
			local id = id_register[s]
			if not exists(id,k) then return nil end
			return (get(id,k))
		end,
		__newindex = function(s,k,v)
			local names = _get_names(s)
			if names == nil or #names == 0 then return end
			if type(k) == "number" then
				k = names[k]
				if k == nil then return end
				local id = id_register[s]
				return set(id,k,(v))
			end
			local id = id_register[s]
			if not exists(id,k) then return end
			return set(id,k,(v))
		end,
		__len = function(s)
			local names = _get_names(s)
			if names == nil or #names == 0 then return 0 end
			return #names
		end,
		__next = function(s,k)
			local names = _get_names(s)
			if names == nil or #names == 0 then return nil end
			local i
			if k == nil then
				i = 0
			elseif type(k) ~= "number" then
				i = util.perform_index(names,k)
			end
			if i == nil then return nil end
			k = names[i+1]
			if k == nil then return nil end
			local id = id_register[s]
			return k, (get(id,k))
		end,
		__pairs = function(s,k)
			local names = _get_names(s)
			if names == nil or #names == 0 then return null end
			local names_lookup = util.build_index(names)
			local id = id_register[s]
			return function(_,k)
				local i
				if k == nil then
					i = 0
				elseif type(k) ~= "number" then
					i = names_lookup[k]
				end
				if i == nil then return nil end
				k = names[i+1]
				if k == nil then return nil end
				return k, (get(id,k))
			end,s,k
		end,
		__inext = function(s,k)
			local names = _get_names(s)
			if names == nil or #names == 0 then return nil end
			local i
			if k == nil then
				i = 0
			elseif type(k) ~= "number" then
				i = util.perform_index(names,k)
			end
			if i == nil then return nil end
			k = names[i+1]
			if k == nil then return nil end
			local id = id_register[s]
			return i, (get(id,k))
		end,
		__ipairs = function(s,k)
			local names = _get_names(s)
			if names == nil or #names == 0 then return null end
			local names_lookup = util.build_index(names)
			local id = id_register[s]
			return function(_,k)
				local i
				if k == nil then
					i = 0
				elseif type(k) ~= "number" then
					i = names_lookup[k]
				end
				if i == nil then return nil end
				k = names[i+1]
				if k == nil then return nil end
				return i, (get(id,k))
			end,s,k
		end
	}
end

local instance_variables_id_register = setmetatable({},{__mode = "k"})
local instance_variables_proxy_register = setmetatable({},{__mode = "v"})

local instance_variables_proxy_meta = define_proxy_meta(
	"Variables",
	gm.variable_instance_get_names,
	gm.variable_instance_exists,
	gm.variable_instance_get,
	gm.variable_instance_set,
	instance_variables_id_register
)

function proxy.variables(id)
	local meta = getmetatable(id)
	if meta and meta.__name and meta.__name:match('CInstance') then id = id.id end
	local proxy = instance_variables_proxy_register[id]
	if proxy then return proxy end
	local proxy = setmetatable({},instance_variables_proxy_meta)
	instance_variables_proxy_register[id] = proxy
	instance_variables_id_register[proxy] = id
	return proxy
end

local struct_id_register = setmetatable({},{__mode = "k"})
local struct_proxy_register = setmetatable({},{__mode = "v"})

local struct_proxy_meta = define_proxy_meta(
	"Struct",
	gm.struct_get_names,
	gm.struct_exists,
	gm.struct_get,
	gm.struct_set,
	struct_id_register
)

proxy.struct = setmetatable({},{
	__call = function(_,id)
		local proxy = struct_proxy_register[id]
		if proxy then return proxy end
		proxy = setmetatable({},struct_proxy_meta)
		struct_proxy_register[id] = proxy
		struct_id_register[proxy] = id
		return proxy
	end
})

local global_variables_proxy_meta = define_proxy_meta(
	"Globals",
	function() return gm.variable_instance_get_names(EVariableType.GLOBAL) end,
	function(_,k) return gm.variable_global_exists(k) end,
	function(_,k) return gm.variable_global_get(k) end,
	function(_,k,v) return gm.variable_global_set(k,v) end,
	{}
)

proxy.globals = setmetatable({},global_variables_proxy_meta)

local script_call_register = setmetatable({},{__mode="v"})
local asset_register = setmetatable({},{__mode="v"})

function get_script_call(name)
	local call = script_call_register[name]
	if call then return call end
	call = function(...) return gm.call(name,...) end
	script_call_register[name] = call
	return call
end

local function get_asset(asset_name,asset_type)
	local wrap = asset_register[asset_name]
	if wrap then return wrap end
	wrap = { index = gm.constants[asset_name], name = asset_name }
	asset_register[asset_name] = wrap
	if asset_type == "script" or asset_type == "gml_script" then
		wrap.call = get_script_call(asset_name)
	end
	return wrap
end

proxy.constants = {}
local constants_lookup = {}
local constants_proxy_meta = {}

for t,v in pairs(gm.constants_type_sorted) do
	constants_lookup[t] = util.build_lookup(v)
	constants_proxy_meta[t] = {
		__name = "Constants",
		__index = function(s,k)
			if type(k) == 'number' then
				local a = v[k]
				if a == nil then return nil end
				return get_asset(a,t)
			end
			if constants_lookup[t][k] == nil then return nil end
			return get_asset(k,t)
		end,
		__newindex = function(s,k,v)
			error(2,"cannot override gamemaker asset.")
		end,
		__len = function(s)
			return #v
		end,
		__next = function(s,k)
			if type(k) == "number" then
				k = v[k]
			end
			local k = next(constants_lookup[t],k)
			return k, s[k]
		end,
		__pairs = function(s,k)
			return function(s,k)
				if type(k) == "number" then
					k = v[k]
				end
				local k = next(constants_lookup[t],k)
				return k, s[k]
			end,s,k
		end
	}
	proxy.constants[t] = setmetatable({},constants_proxy_meta[t])
end

local ds_map_id_register = setmetatable({},{__mode = "k"})
local ds_map_proxy_register = setmetatable({},{__mode = "v"})

local ds_map_proxy_meta = {
	__name = "DataMap",
	__index = function(s,k)
		local id = ds_map_id_register[s]
		return (gm.call('ds_map_find_value',id,k))
	end,
	__newindex = function(s,k,v)
		local id = ds_map_id_register[s]
		return gm.call('ds_map_set',id,k,(v))
	end,
	__next = function(s,k)
		local id = ds_map_id_register[s]
		if k == nil then
			k = gm.call('ds_map_find_first',id)
		else
			k = gm.call('ds_map_find_next',id,k)
		end
		return k, (gm.call('ds_map_find_value',id,k))
	end,
	__pairs = function(s,k)
		local id = ds_map_id_register[s]
		return function(_,k)
			if k == nil then
				k = gm.call('ds_map_find_first',id)
			else
				k = gm.call('ds_map_find_next',id,k)
			end
			return k, (gm.call('ds_map_find_value',id,k))
		end,s,k
	end
}

function proxy.map(id)
	if type(id) ~= "number" or id < 0 then return nil end
	local proxy = ds_map_proxy_register[id]
	if proxy then return proxy end
	local proxy = setmetatable({},ds_map_proxy_meta)
	ds_map_proxy_register[id] = proxy
	ds_map_id_register[proxy] = id
	return proxy
end

local ds_list_id_register = setmetatable({},{__mode = "k"})
local ds_list_proxy_register = setmetatable({},{__mode = "v"})

local ds_list_proxy_meta = {
	__name = "DataList",
	__index = function(s,k)
		if type(k) ~= "number" then return nil end
		local id = ds_list_id_register[s]
		return (gm.call('ds_list_find_value',id,k-1))
	end,
	__newindex = function(s,k,v)
		local id = ds_list_id_register[s]
		return gm.call('ds_list_set',id,k-1,(v))
	end,
	__len = function(s)
		local id = ds_list_id_register[s]
		return math.floor(gm.call('ds_list_size',id))
	end,
	__inext = function(s,k)
		local id = ds_list_id_register[s]
		k = (k or 0)
		if type(k) ~= "number" then return nil end
		local v = gm.call('ds_list_find_value',id,k)
		if v == nil then return end
		return k+1, v
	end,
	__ipairs = function(s,k)
		local id = ds_list_id_register[s]
		return function(_,k)
			k = (k or 0)
			if type(k) ~= "number" then return nil end
			local v = gm.call('ds_list_find_value',id,k)
			if v == nil then return end
			return k+1, v
		end,s,k
	end
}
ds_list_proxy_meta.__next = ds_list_proxy_meta.__inext
ds_list_proxy_meta.__pairs = ds_list_proxy_meta.__ipairs

function proxy.list(id)
	if type(id) ~= "number" or id < 0 then return nil end
	local proxy = ds_list_proxy_register[id]
	if proxy then return proxy end
	local proxy = setmetatable({},ds_list_proxy_meta)
	ds_list_proxy_register[id] = proxy
	ds_list_id_register[proxy] = id
	return proxy
end

_ENV.hardcoded = hardcoded or require("./hardcoded")
hardcoded.enum = {}
for k,v in pairs(hardcoded.array) do
	local enum = {}
	hardcoded.enum[k] = enum
	for i,w in pairs(v) do
		enum[w.name] = w.value or i
	end
end
for k,v in pairs(hardcoded.script) do
	v.call = get_script_call(k)
end

-- GLOBAL OBJECT EXTENSIONS

local _type = type
local _getmeta = debug.getmetatable

function _ENV.type(v)
	local t = _type(v)
	local m = _getmeta(v)
	if m then
		return t, m.__name
	end
	return t
end

local function endow_with_pairs_and_next(meta)
	--[[
	context behind this approach:
		sol objects are userdata or tables that have sol classes as metatables
		sol object attributes are functions in their sol class as the same field
		sol class __index function fallsback to itself so objects inherit class members
		sol __index generates a new 'new' function whenever it is requested
		sol classes have stub __pairs that just errors when called
		sol overrides next to error when that is used on a sol class
	--]]
	if not meta then return end
	local status, _next
	--if rawget(meta,'__pairs') or rawget(meta,'__next') then
	--	status, _next = false--pcall(pairs,object)
	--end
	if not status then
		local _index = rawget(meta,'__index')
		if not _index then return end
		if type(_index) ~= 'function' then
			function _next(s,k)
				return next(_index,k)
			end
		else
			function _next(s,k)
				local v,u,w
				while v == nil do
					k,u = rawnext(meta,k)
					if k == nil then return nil end
					-- ignore 'new' and metatable fields
					if k ~= 'new' and k:sub(1,2) ~= '__' then
						w = s[k]
						-- if the object reports a value different to the class
						if u ~= w then
							-- assume it's actually that object's attribute
							v = w
						end
					end
				end
				return k,v
			end
		end
		rawset(meta,'__pairs',function(s,k)
			return _next,s,k
		end)
	end
	-- __next is implemented by a custom implementation of next
	local status = false--pcall(rawnext,object)
	if not status and _next ~= nil and rawget(meta,'__next') == nil then
		rawset(meta,'__next',_next)
	end
end

local function endow_with_new_properties(meta,properties)
	local new_properties = {}
	for k,v in pairs(properties) do
		if not rawget(meta,k) then
			new_properties[k] = v
			rawset(meta,k,v)
		end
	end
	local index = meta.__index
	meta.__index = function(s,k)
		local v = new_properties[k]
		if v then return v(s) end
		return index(s,k)
	end
end

local function imgui_next_delayed_load()

	local imgui_style = ImGui.GetStyle() -- sol.ImGuiStyle*
	local imgui_vector = imgui_style["WindowPadding"] -- sol.ImVec2*
	endow_with_pairs_and_next(dupmeta(imgui_style))
	endow_with_pairs_and_next(dupmeta(imgui_vector))
	endow_with_pairs_and_next(dupmeta(ImGuiKey))
	endow_with_pairs_and_next(dupmeta(ImGuiKeyMod))
	
	local imgui_stylevar_lookup = util.build_lookup(ImGuiStyleVar)
	imgui_stylevar_lookup[ImGuiStyleVar.COUNT] = nil

	function ImGui_GetStyleVar(var)
		if type(var) == "number" then
			var = imgui_stylevar_lookup[var]
		end
		local s = ImGui.GetStyle()[var]
		if type(s) ~= "userdata" then return s end
		return s['x'],s['y']
	end
end

local function gm_next_delayed_load(ccode)
	local gm_instance_list = gm.CInstance.instances_all
	local gm_instance = gm_instance_list[1]
	if gm_instance == nil then return true end
	local gm_object = gm.variable_global_get("init_player")
	local gm_instance_variables = gm.variable_instance_get_names(gm_instance)
	
	endow_with_pairs_and_next(dupmeta(gm_instance_list))
	endow_with_pairs_and_next(dupmeta(gm_instance))
	endow_with_pairs_and_next(dupmeta(CInstance))
	endow_with_pairs_and_next(dupmeta(gm_object))
	endow_with_pairs_and_next(dupmeta(ccode))

	local function array_inext(t,k)
		local n = #t
		if n == 0 then return nil end
		if k ~= nil and (type(k) ~= "number" or k < 0) then return nil end
		k = (k or 0) + 1
		if n < k then return nil end
		return k,t[k]
	end
	local function array_ipairs(t,k)
		local n = #t
		if n == 0 then return nil end
		if k ~= nil and (type(k) ~= "number" or k < 0) then return nil end
		return function(_,k)
			k = (k or 0) + 1
			if n < k then return nil end
			return k,t[k]
		end,t,k
	end
	local gm_instance_variables_meta = dupmeta(gm_instance_variables)
	gm_instance_variables_meta.__inext = array_inext
	gm_instance_variables_meta.__pairs = array_ipairs
	gm_instance_variables_meta.__ipairs = array_ipairs
	gm_instance_variables_meta.__newindex = function(s,k,v)
		return gm.array_set(s,k-1,v)
	end
	
	local script_prefix = "gml_Script_"
	local script_prefix_index = #script_prefix+1
	local get_object_script_call = function(s)
		if s.type == YYObjectBaseType.SCRIPTREF then
			local k = s.script_name
			if not k then return nil end
			k = k:sub(script_prefix_index)
			return get_script_call(k)
		end
	end
	
	local object_lookup = util.build_lookup(YYObjectBaseType)
	local get_object_type_name = function(s) return object_lookup[s.type] end
	
	endow_with_new_properties(dupmeta(gm_object),{
		type_name = get_object_type_name,
		call = get_object_script_call
	})

	local gm_instance_id_register = setmetatable({},{__mode='k'})
	local gm_class_name_register = setmetatable({},{__mode='k'})
	local gm_instance_fields_register = setmetatable({},{__mode='k'})

	local gm_instance_meta = {
		__index = function(t,k)
			local field = k
			if type(k) ~= "number" then
				local fields = gm_instance_fields_register[t]
				field = fields[k]
				if field == nil then return nil end
			end
			local array = proxy.globals[gm_class_name_register[t]]
			local id = gm_instance_id_register[t]
			array = array[id]
			if array == nil then return nil end
			return array[field]
		end,
		__newindex = function(t,k,v)
			local fields = gm_instance_fields_register[t]
			local field = fields[k]
			if not field then error("setting unknown field " .. k) end
			local array = proxy.globals[gm_class_name_register[t]]
			local id = gm_instance_id_register[t]
			array = array[id]
			if array == nil then return end
			array[field] = v
		end,
		__len = function(t)
			local array = proxy.globals[gm_class_name_register[t]]
			local id = gm_instance_id_register[t]
			local array = array[id]
			if array == nil then return 0 end
			return #array
		end,
		__next = function(t,k)
			local field
			local fields = gm_instance_fields_register[t]
			k, field = next(fields,k)
			if k == nil then return nil end
			local array = proxy.globals[gm_class_name_register[t]]
			local id = gm_instance_id_register[t]
			array = array[id]
			if array == nil then return nil end
			return k,array[field]
		end,
		__pairs = function(t,k)
			local fields = gm_instance_fields_register[t]
			local array = proxy.globals[gm_class_name_register[t]]
			local id = gm_instance_id_register[t]
			array = array[id]
			if array == nil then return null end
			return function(_,k)
				local field
				k, field = next(fields,k)
				if k == nil then return nil end
				return k,array[field]
			end,t,k
		end,
		__inext = function(t,k)
			local array = proxy.globals[gm_class_name_register[t]]
			local id = gm_instance_id_register[t]
			array = array[id]
			if array == nil then return nil end
			local n = #array
			if n == 0 then return nil end
			if k ~= nil and (type(k) ~= "number" or k < 0 or n <= k) then return nil end
			k = (k or 0) + 1
			if n < k then return nil end
			return k,array[k]
		end,
		__ipairs = function(t,k)
			local array = proxy.globals[gm_class_name_register[t]]
			local id = gm_instance_id_register[t]
			array = array[id]
			if array == nil then return nil end
			local n = #array
			if n == 0 then return null end
			if k ~= nil and (type(k) ~= "number" or k < 0 or n <= k) then return nil end
			return function(_,k)
				k = (k or 0) + 1
				if n < k then return nil end
				return k,array[k]
			end,t,k
		end
	}

	-- for interacting with global arrays as classes
	-- thanks to sarn
	local function gm_array_class(name, array, fields)
		local instances = setmetatable({},{__mode='kv'})
		
		local function get_proxy(id)
			local proxy = instances[id]
			if proxy then return proxy end
			local _name = ''
			for s in name:gmatch('_(%w+)') do
				_name = _name .. s:sub(1,1):upper() .. s:sub(2)
			end
			proxy = setmetatable({}, util.merge({ __name = _name }, gm_instance_meta))
			instances[id] = proxy
			gm_instance_id_register[proxy] = math.floor(id)
			gm_class_name_register[proxy] = name
			gm_instance_fields_register[proxy] = fields
			return proxy
		end
		
		return setmetatable({},{
			__name = "Class",
			__call = function(_,id)
				local n = #array
				if n == 0 then return nil end
				if id ~= nil and (type(id) ~= "number" or id < 0 or n <= id) then return nil end
				return get_proxy(id+1)
			end,
			__index = function(_,id)
				local n = #array
				if n == 0 then return nil end
				if id ~= nil and (type(id) ~= "number" or id < 1 or n < id) then return nil end
				return get_proxy(id)
			end,
			__newindex = function(_,k,v)
				array[k] = v
			end,
			__len = function(_)
				return #array
			end,
			__next = function(s,k)
				local n = #array
				if n == 0 then return nil end
				if k ~= nil and (type(k) ~= "number" or k < 0 or n <= k) then return nil end
				k = (k or 0) + 1
				if n < k then return nil end
				return k,s[k]
			end,
			__pairs = function(s,k)
				local n = #array
				if n == 0 then return null end
				if k ~= nil and (type(k) ~= "number" or k < 0 or n <= k) then return nil end
				return function(_,k)
					k = (k or 0) + 1
					if n < k then return nil end
					return k,s[k]
				end,s,k
			end,
			__inext = function(s,k)
				return getmetatable(s).__next(s,k)
			end,
			__ipairs = function(s,k)
				return getmetatable(s).__pairs(s,k)
			end
		})
	end
	
	hardcoded.class = {}
	for k,v in pairs(hardcoded.enum) do
		local class_name = k:lower()
		local class_array = gm.variable_global_get(class_name)
		if class_array then
			hardcoded.class[class_name] = gm_array_class(class_name,class_array,v)
		end
	end
end

endow_with_pairs_and_next(dupmeta(EVariableType))
endow_with_pairs_and_next(dupmeta(RValueType))
endow_with_pairs_and_next(dupmeta(YYObjectBaseType))

gui.add_always_draw_imgui( function()
	if imgui_next_delayed_load and imgui_next_delayed_load() ~= true then
		imgui_next_delayed_load = nil
	end
end )
gm.pre_code_execute( function(_,_,ccode)
	if gm_next_delayed_load and gm_next_delayed_load(ccode) ~= true then
		gm_next_delayed_load = nil
	end
end )

colors = {
	tree = 0xFFFF20FF,
	leaf = 0xFFFFFF20,
	info = 0xFF20FFFF,
	fake = 0xEECCCCCC,
	null = 0xFF2020FF
}

root = {
	gm = gm,
	lua = _G,
	mods = mods,
	style = ImGui.GetStyle(),
	colors = colors,
	helpers = {},
	globals = proxy.globals,
	constants = proxy.constants,
	hardcoded = hardcoded,
}

local filter_modes_browser = {
	"tree",
	"info"
}

local filter_modes_details = {
	"leaf",
	"info"
}

browsers = {}
details = {}
local unfolded = {}
local last_pos = {0,0}
local last_size = {0,0}

local function create_browser(entry,x,y,w,h)
	local id = #browsers + 1
	unfolded[id..'|root'] = true
	browsers[id] = util.merge({
		index = id,
		filter_text = '',
		filter = '',
		texts = {},
		tooltips = {},
		init_pos = table.pack(x,y),
		init_size = table.pack(w,h),
		mode = 1
	},entry)
end

local function create_details(entry,x,y,w,h)
	local id = #details + 1
	details[id] = util.merge({
		index = id,
		filter_text = '',
		filter = '',
		texts = {},
		tooltips = {},
		init_pos = table.pack(x,y),
		init_size = table.pack(w,h),
		mode = 1
	},entry)
end

local function root_entries()
	return { data = root, iter = pairs, chain = {}, funcs = {},
		path = 'root', name = 'root', show = 'root', text = 'root'}
end

function root.helpers.int_to_hex(i)
	return '0x' .. string.format("%x", i):upper()
end

function root.helpers.get_skin_by_id(id)
	if type(id) ~= "number" or id < 0 then return nil end
	return hardcoded.class.class_actor_skin(id)
end

function root.helpers.get_skill_by_id(id)
	if type(id) ~= "number" or id < 0 then return nil end
	return hardcoded.class.class_skill(id)
end

function root.helpers.get_achievement_by_id(id)
	if type(id) ~= "number" or id < 0 then return nil end
	return hardcoded.class.class_achievement(id)
end

local excludedFieldNames = util.build_lookup{ 
	"and", "break", "do", "else", "elseif", "end",
	"false", "for", "function", "if", "in", "local",
	"nil", "not", "or", "repeat", "return", "then",
	"true", "until", "while", "goto", "repeat", "until"
}

local calculate_text_sizes
do
	local calculate_text_sizes_x_buffer = {}
	function calculate_text_sizes(...)
		-- don't need to clear the buffer since 
		-- we only iterate over the region we overwrite
		local frame_padding_x, frame_padding_y = ImGui_GetStyleVar(ImGuiStyleVar.FramePadding)
		local frame_padding_x_2 = 2*frame_padding_x
		local frame_padding_y_2 = 2*frame_padding_y
		local my = 0 -- maximum y value in this row
		local sx = 0 -- sum of x values in this row
		local n -- number of items in this row
		for i,t in util.vararg(...) do
			n = i
			local x,y = ImGui.CalcTextSize(t)
			x = x + frame_padding_x_2
			y = y + frame_padding_y_2
			calculate_text_sizes_x_buffer[i] = x
			sx = sx + x
			if y > my then my = y end
		end
		return n, my, sx, table.unpack(calculate_text_sizes_x_buffer, 1, n)
	end
end

local function tostring_literal(value)
	if type(value) == "string" then
		local lined, _lined = 0, value:gmatch("\n")
		for _ in _lined do lined = lined + 1 end
		local dquoted, _dquoted = 0, value:gmatch([=["]=])
		for _ in _dquoted do dquoted = dquoted + 1 end
		local squoted, _squoted = 0, value:gmatch([=[']=])
		for _ in _squoted do squoted = squoted + 1 end
		local edquoted, _edquoted = 0, value:gmatch([=[\"]=])
		if lined > 0 or (dquoted > 0 and squoted > 0) then
			local special, _special = 0, value:gmatch([=[[=]]=])
			for _ in _special do special = special + 1 end
			local eq = "="
			for i = 1, special do
				eq = eq .. '='
			end
			return '['..eq..'[' .. value .. ']'..eq..']'
		elseif squoted > 0 then
			return '"' .. value .. '"'
		else
			return "'" .. value .. "'"
		end
	end
	return tostring(value)
end

local function tostring_vararg(...)
	local s = ""
	for _,v in util.vararg(...) do
		v = tostring_literal(v)
		s = s .. '\t' .. v
	end
	return s:sub(2,#s)
end

local function path_part_key(key)
	if type(key) == "string" then
		if excludedFieldNames[key] or not key:match("^[_%a][_%w]*$") then
			return '[' .. tostring_literal(key) .. ']' 
		end
		return '.' .. key
	end
	if type(key) == "number" then
		return '[' .. key .. ']' 
	end
end

local function path_part(ed,path)
	path = (path or '')
	if ed.name then
		path = path .. path_part_key(ed.name)
	end
	if ed.keys then
		for _, key in ipairs(ed.keys) do
			if type(key) == "table" then
				-- build new path for a function to wrap over it
				local wrap = nil
				for _, k in ipairs(key) do
					if wrap then
						local part = path_part_key(k)
						if part == nil then part = "[?]" end
						wrap = wrap .. part
					else
						wrap = k
					end
				end
				path = wrap .. '(' .. path .. ')'
			else
				-- extend current path
				local part = path_part_key(key)
				if part == nil then return "<" .. ed.show or '???' .. ">" end
				path = path .. part
			end
		end
	end
	return path
end

local entrify
do
	-- to avoid making these many times
	local keys_hex = {{'root','helpers','int_to_hex'}}
	local keys_map = {{'proxy','map'}}
	local keys_list = {{'proxy','list'}}
	local keys_variables = {{'proxy','variables'}}
	local keys_struct = {{'proxy','struct'}}
	local keys_struct_skin = {{'root','helpers','get_skin_by_id'}}
	local keys_struct_skill = {{'root','helpers','get_skill_by_id'}}
	local keys_struct_achievement = {{'root','helpers','get_achievement_by_id'}}
	
	local extra = {}
	
	function entrify(name,data,base)
		util.clear(extra)
		local data_type, sol_type = type(data)
		local type_name = sol_type or data_type
		local iter = nil
		local keys = nil
		local info = nil
		local func = nil
		if base ~= nil and base.type and (data_type == "number" or data_type == "nil") then
			if name == "id" and base.type:match('instance') then
				table.insert(extra,{
					func = proxy.variables,
					base = base,
					name = name,
					show = "variables",
					keys = keys_variables,
					iter = pairs
				})
			elseif name == "skin_id" and base.type:match('Struct') then
				table.insert(extra,{
					func = root.helpers.get_skin_by_id,
					base = base,
					name = name,
					show = "skin",
					keys = keys_struct_skin,
					iter = pairs,
				})
			elseif name == "skill_id" and base.type:match('Struct') then
				table.insert(extra,{
					func = root.helpers.get_skill_by_id,
					base = base,
					name = name,
					show = "skill",
					keys = keys_struct_skill,
					iter = pairs,
				})
			elseif name == "achievement_id" and base.type:match('Struct') then
				table.insert(extra,{
					func = root.helpers.get_achievement_by_id,
					base = base,
					name = name,
					show = "achievement",
					keys = keys_struct_achievement,
					iter = pairs,
				})
			elseif base.base and base.name == "colors" and base.base.name == "root" then
				table.insert(extra,{
					func = root.helpers.int_to_hex,
					base = base,
					name = name,
					show = "hex",
					keys = keys_hex,
				})
			elseif type(name) == "string" and name:sub(#name-3) == "_map" then
				table.insert(extra,{
					func = proxy.map,
					name = name,
					show = 'data',
					keys = keys_map,
					iter = pairs,
				})
			elseif type(name) == "string" and name:sub(#name-4) == "_list" then
				table.insert(extra,{
					func = proxy.list,
					name = name,
					show = 'data',
					keys = keys_list,
					iter = ipairs,
				})
			end
		elseif data_type == "table" then
			iter = pairs
		elseif sol_type then
			if sol_type:match("Im") then
				iter = pairs
			elseif sol_type:match("Object") then
				iter = pairs
				if data.type == YYObjectBaseType.YYOBJECTBASE then
					func = proxy.struct
					keys = keys_struct
				end
			elseif sol_type:match("Array") then
				iter = ipairs
			elseif tostring(data):match('unordered_map') then
				iter = pairs
			elseif tostring(data):match('<') then -- span or container
				iter = ipairs
			elseif sol_type and sol_type:match("Instance") then
				iter = pairs
				info = data.object_name .." (" .. data.object_index .. " @ " .. data.id .. ")"
			end
		end
		local ed = {
			fake = false,
			info = info,
			base = base,
			func = func,
			name = name,
			keys = keys,
			iter = iter
		}
		if func == nil then ed.data = data end
		for _,sd in ipairs(extra) do
			sd.fake = true
			if not sd.base then sd.base = ed end
		end
		return ed, table.unpack(extra)
	end
end

local function resolve(ed)
	local func = ed.func
	local base = ed.base
	if func then
		if base.iter then
			local data = base.data[ed.name]
			if data then
				ed.data = func(data)
			end
		else
			ed.data = func(base.data)
		end
	elseif base and base.iter then
		ed.data = base and base.data[ed.name]
	end
	return ed
end

local function refresh(ed)
	if ed == nil then return ed end
	if ed.entries == nil then return ed end
	--for _,ed in ipairs(ed.entries) do
	--	refresh(ed)
	--end
	ed.entries = nil
	return ed
end

--[[
local path_cache = setmetatable({},{__mode='v'})
--]]

local unfold
do
	local function type_name(o,t)
		if t == nil then return nil end
		if t:match('DynamicArray') then
			return 'array'
		end
		if t:match('unordered_map') then
			return 'map'
		end
		if t:match('span') then
			return 'span'
		end
		if t:match('vector') then
			return 'vector'
		end
		if t:match('container') then
			return 'container'
		end
		if t:match('CInstance') then
			return 'instance'
		end
		if t:match('Object') then
			if o.type == YYObjectBaseType.SCRIPTREF then
				return 'script'
			end
			return o.type_name
		end
		return t
	end

	local function _len(t)
		return #t
	end
	
	function len(t)
		local s,v = pcall(_len,t)
		if s then return math.floor(v) end
	end

	function unfold(ed)
		if ed.entries then return ed.entries end
		all_new_metas()
		ed.path = ed.path or path_part(ed)
		--path_cache[ed.path] = ed
		local data = ed.data
		if ed.data == nil then data = resolve(ed).data end
		local iter = ed.iter
		local entries = {}
		if iter ~= nil and data ~= nil then
			for k,v in iter(data) do
				for _,sd in util.vararg(entrify(k,v,ed)) do
					if sd ~= nil then
						table.insert(entries,sd)
					end
				end
			end
		end
		for i,sd in ipairs(entries) do
			sd.index = i
			sd.path = path_part(sd,ed.path)
			--sd.chain = util.merge({},ed.chain)
			--table.insert(sd.chain,sd.name)
			--sd.funcs = util.merge({},ed.funcs)
			--if sd.func then
			--	sd.funcs[#sd.chain] = sd.func
			--end
			local data = sd.data
			if data == nil then data = resolve(sd).data end
			local ta,tb = type(data)
			sd.type = sd.type or type_name(data,tb) or ta 
			sd.info = sd.info or sd.type
			local size = len(data)
			if size then
				sd.size = size
				sd.info = sd.info .. '[' .. size .. ']'
			end
			if not sd.show then sd.show = tostring(sd.name) end
			sd.text = sd.info and sd.show .. '|' .. sd.info or sd.show
			if sd.fake then
				local base = sd.base
				local info = base.info
				sd.text = sd.text .. '|' .. (info and (base.show .. '|' .. info) or base.show)
			end
		end
		ed.entries = entries
		all_old_metas()
		return entries
	end
end

function resolve_vararg_simple(...)
	local ed = browsers[1]
	for _,k in util.vararg(...) do
		for j, sd in ipairs(unfold(ed)) do
			if sd.name == k or sd.func == k then
				ed = sd
				break
			end
		end
	end
	return ed
end

--[[

function resolve_vararg(...)
	local ed = browsers[1]
	for _,k in util.vararg(...) do
		for j, sd in ipairs(unfold(ed)) do
			if sd.name == k then
				ed = sd
				break
			end
		end
	end
	return ed
end

function resolve_chain_funcs(chain, funcs)
	local ed = browsers[1]
	for i,k in ipairs(chain) do
		local func
		if funcs then func = funcs[i] end
		for j, sd in ipairs(unfold(ed)) do
			if sd.name == k and sd.func == func then
				ed = sd
				break
			end
		end
	end
	return ed
end

function resolve_path(path)
	local ed = path_cache[path]
	if ed ~= nil then return ed end
	-- TODO: resolve the path using grammar:
	-- PATH = FUNC(PATH) | PATH[KEY] | PATH.FIELD | root
	-- FUNC = FUNC[KEY] | FUNC.FIELD | root
	-- KEY = .*
	-- FIELD = %w+
	-- only works for valid paths that don't use table, userdata, function or thread as keys
	-- as such, disqualify a path if it contains PATH<.+?: [A-F0-9]+>
	local ed = browsers[1]
	for _,k in path:gmatch('%.(.+?)') do
		for j, sd in ipairs(unfold(ed)) do
			if sd.name == k then
				ed = sd
				break
			end
		end
	end
	return ed
end

--]]

local render_details
local render_browser
do
	local script_prefix = "gml_Script_"
	local script_prefix_index = #script_prefix+1

	local function peval(text)
		local func = load("return " .. text)
		if not func then return nil end
		setfenv(func,_G)
		all_new_metas()
		local ret = table.pack(pcall(func))
		all_old_metas()
		if ret.n <= 1 then return end
		if not ret[1] then return end
		return table.unpack(ret,2,ret.n)
	end

	local function try_tooltip(dd,sd,value_part) 
		if ImGui.IsItemHovered() then
			local message
			if value_part then
				if sd.type == "string" then
					message = gm.call('ds_map_find_value',gm.variable_global_get("_language_map"),sd.data)
					if type(message) ~= "string" then message = nil end
				end
			end
			if message == nil then
				local enum = type(dd.name) == "string" and type(sd.data) == "number" and hardcoded.array[dd.name]
				if enum then
					enum = enum[sd.data]
				else
					local _type = select(2,type(dd.data))
					_type = _type and _type:upper()
					if _type and _type:sub(1,6) == "CLASS_" then
						enum = hardcoded.array[_type][hardcoded.enum[_type][sd.show]]
					end
				end
				if not enum then return end
				if enum.value then
					message = tostring_literal(enum.value)
				end
				if enum.description then
					message = (message and message .. ' ' or '') .. '-- ' .. enum.description
				end
			end
			if message ~= nil then
				ImGui.PushStyleColor(ImGuiCol.Text, colors.fake)
				ImGui.SetTooltip(message);
				ImGui.PopStyleColor()
			end
		end
	end

	function render_details(dd)
		local entries = unfold(dd)
		if entries then
			local filter = dd.filter
			local skipped = false
			for _,sd in ipairs(entries) do
				if #filter ~= 0 and not sd.text:match(filter) then
					skipped = true
				else
					local id = dd.index .. '|' .. sd.path
					if sd.iter then
						if dd.mode ~= 1 then
							-- iterable
							ImGui.PushStyleColor(ImGuiCol.HeaderHovered,0)
							ImGui.PushStyleColor(ImGuiCol.HeaderActive,0)
							ImGui.Selectable("##Select" .. id, false)
							ImGui.PopStyleColor()
							ImGui.PopStyleColor()
							if ImGui.IsItemHovered() then
								if ImGui.IsItemClicked(ImGuiMouseButton.Middle) then
									local x,y = ImGui.GetWindowPos()
									local w,h = ImGui.GetWindowSize()
									create_browser(sd,x+w,y,w,h)
								end
								if ImGui.IsItemClicked(ImGuiMouseButton.Right) then
									local x,y = ImGui.GetWindowPos()
									local w,h = ImGui.GetWindowSize()
									create_details(sd,x+w,y,w,h)
								end
							end
							if sd.fake then
								ImGui.PushStyleColor(ImGuiCol.Text, colors.fake)
								ImGui.SameLine()
								ImGui.Text(sd.name)
								ImGui.PopStyleColor()
							end
							ImGui.PushStyleColor(ImGuiCol.Text, colors.tree)
							ImGui.SameLine()
							ImGui.Text(sd.show)
							ImGui.PopStyleColor()
							try_tooltip(dd,sd,false)
							ImGui.PushStyleColor(ImGuiCol.Text, colors.info)
							ImGui.SameLine()
							ImGui.Text(sd.info)
							ImGui.PopStyleColor()
							try_tooltip(dd,sd,true)
						end
					else
						-- not iterable
						ImGui.Text("")
						if sd.fake then
							ImGui.PushStyleColor(ImGuiCol.Text, colors.fake)
							ImGui.SameLine()
							ImGui.Text(sd.name)
							ImGui.PopStyleColor()
						end
						ImGui.SameLine()
						ImGui.PushStyleColor(ImGuiCol.Text, colors.leaf)
						ImGui.Text(sd.show)
						ImGui.PopStyleColor()
						try_tooltip(dd,sd,false)
						if sd.type ~= "function" and sd.type ~= "thread" then
							ImGui.SameLine()
							ImGui.PushStyleVar(ImGuiStyleVar.FramePadding, 0, 0)
							ImGui.PushStyleColor(ImGuiCol.FrameBg, 0)
							ImGui.PushStyleColor(ImGuiCol.Text, colors.info)
							ImGui.PushItemWidth(ImGui.GetContentRegionAvail() - ImGui.CalcTextSize('|'))
							local text, enter_pressed = ImGui.InputText("##Text" .. id, dd.texts[id] or tostring_literal(sd.data), 65535, sd.fake and ImGuiInputTextFlags.ReadOnly or ImGuiInputTextFlags.EnterReturnsTrue)
							ImGui.PopItemWidth()
							ImGui.PopStyleColor()
							ImGui.PopStyleColor()
							ImGui.PopStyleVar()
							try_tooltip(dd,sd,true)
							if enter_pressed then
								dd.data[sd.name] = peval(text)
								dd.texts[id] = nil
								refresh(dd)
							elseif text == "" then 
								dd.texts[id] = nil
							else
								dd.texts[id] = text
							end
						else
							ImGui.PushStyleColor(ImGuiCol.Text, colors.null)
							ImGui.SameLine()
							ImGui.Text(tostring(sd.data))
							ImGui.PopStyleColor()
							if sd.type == "function" then
								ImGui.SameLine()
								ImGui.PushStyleVar(ImGuiStyleVar.FramePadding, 0, 0)
								ImGui.PushStyleColor(ImGuiCol.FrameBg, 0)
								ImGui.PushStyleColor(ImGuiCol.Text, colors.info)
								ImGui.Text("(")
								ImGui.SameLine()
								ImGui.PushItemWidth(ImGui.GetContentRegionAvail() - ImGui.CalcTextSize('(|)'))
								local text, enter_pressed = ImGui.InputText("##Text" .. id, dd.texts[id] or '', 65535, ImGuiInputTextFlags.EnterReturnsTrue)
								local tooltip = dd.tooltips[id]
								if tooltip and ImGui.IsItemHovered() then
									ImGui.PushStyleColor(ImGuiCol.Text, tooltip.color)
									ImGui.SetTooltip(tooltip.message);
									ImGui.PopStyleColor()
								end
								ImGui.PopItemWidth()
								ImGui.SameLine()
								ImGui.Text(")")
								ImGui.PopStyleColor()
								ImGui.PopStyleColor()
								ImGui.PopStyleVar()
								if enter_pressed then
									local result = table.pack(pcall(sd.data, peval(text)))
									if result.n > 1 then
										local color, message
										if result[1] then
											color = colors.leaf
											message = tostring_vararg(table.unpack(result, 2, result.n))
										else 
											color = colors.null
											message = result[2]
										end
										dd.tooltips[id] = { message = message, color = color }
									end
									dd.texts[id] = nil
								else
									dd.texts[id] = text
								end
								if dd.data and sd.show == "call" then
									local params = dd.data.params
									if params == nil then
										local name = dd.data.name
										if name == nil then
											local script_name = dd.data.script_name
											if script_name ~= nil then
												name = script_name:sub(script_prefix_index)
											end
										end
										if name ~= nil then
											local script = hardcoded.script[name]
											if script then 
												params = script.params
											end
										end
									end
									if params ~= nil then
										ImGui.PushStyleColor(ImGuiCol.Text, colors.fake)
										ImGui.Text("")
										ImGui.SameLine()
										ImGui.Text("")
										ImGui.SameLine()
										ImGui.Text("params:")
										for _,p in ipairs(params) do
											ImGui.SameLine()
											ImGui.Text(p.name)
											if p.value and ImGui.IsItemHovered() then
												ImGui.SetTooltip(p.value);
											end
										end
										ImGui.PopStyleColor()
									end
								end
							end
						end
					end
				end
			end
			if skipped then
				ImGui.Text("")
				ImGui.SameLine()
				ImGui.Text("...")
			end
		end
	end

	function render_browser(bd,ed)
		ids = bd.index .. '|' .. ed.path
		local show = ed.path ~= "root"
		local _unfolded = unfolded[ids] == true
		if show then
			if ed.iter then
				-- iterable
				ImGui.PushStyleColor(ImGuiCol.HeaderHovered,0)
				ImGui.PushStyleColor(ImGuiCol.HeaderActive,0)
				ImGui.Selectable("##Select" .. ids, false)
				ImGui.PopStyleColor()
				ImGui.PopStyleColor()
				if ImGui.IsItemHovered() then
					if ImGui.IsItemHovered() and ImGui.IsItemClicked(ImGuiMouseButton.Left) then
						_unfolded = not _unfolded
					end
					if ImGui.IsItemClicked(ImGuiMouseButton.Middle) then
						local x,y = ImGui.GetWindowPos()
						local w,h = ImGui.GetWindowSize()
						create_browser(ed,x+w,y,w,h)
					end
					if ImGui.IsItemClicked(ImGuiMouseButton.Right) then
						local x,y = ImGui.GetWindowPos()
						local w,h = ImGui.GetWindowSize()
						create_details(ed,x+w,y,w,h)
					end
				end
				unfolded[ids] = _unfolded
				ImGui.SetNextItemOpen(_unfolded)
				ImGui.SameLine()
				ImGui.TreeNode("##Node" .. ids)
				if ed.fake then
					ImGui.PushStyleColor(ImGuiCol.Text, colors.fake)
					ImGui.SameLine()
					ImGui.Text(ed.name)
					ImGui.PopStyleColor()
				end
				ImGui.PushStyleColor(ImGuiCol.Text, colors.tree)
				ImGui.SameLine()
				ImGui.Text(ed.show)
				ImGui.PopStyleColor()
				if ed.info ~= nil then
					ImGui.PushStyleColor(ImGuiCol.Text, colors.info)
					ImGui.SameLine()
					ImGui.Text(ed.info)
					ImGui.PopStyleColor()
				end
			else
				-- not iterable
				ImGui.Text("\t")
				ImGui.SameLine()
				ImGui.Text("")
				if ed.fake then
					ImGui.PushStyleColor(ImGuiCol.Text, colors.fake)
					ImGui.SameLine()
					ImGui.Text(ed.name)
					ImGui.PopStyleColor()
				end
				ImGui.SameLine()
				ImGui.PushStyleColor(ImGuiCol.Text, colors.leaf)
				ImGui.Text(ed.show)
				ImGui.PopStyleColor()
				if ed.type ~= "function" and ed.type ~= "thread" then
					ImGui.SameLine()
					ImGui.PushStyleVar(ImGuiStyleVar.FramePadding, 0, 0)
					ImGui.PushStyleColor(ImGuiCol.FrameBg, 0)
					ImGui.PushStyleColor(ImGuiCol.Text, colors.info)
					ImGui.PushItemWidth(ImGui.GetContentRegionAvail() - ImGui.CalcTextSize('|'))
					local text, enter_pressed = ImGui.InputText("##Text" .. ids, bd.texts[ids] or tostring_literal(ed.data), 65535, ed.fake and ImGuiInputTextFlags.ReadOnly or ImGuiInputTextFlags.EnterReturnsTrue)
					ImGui.PopItemWidth()
					ImGui.PopStyleColor()
					ImGui.PopStyleColor()
					ImGui.PopStyleVar()
					try_tooltip(bd,ed,true)
					if enter_pressed then
						ed.base.data[ed.name] = peval(text)
						bd.texts[ids] = nil
						refresh(bd)
					elseif text == "" then 
						bd.texts[ids] = nil
					else
						bd.texts[ids] = text
					end
				else
					ImGui.PushStyleColor(ImGuiCol.Text, colors.null)
					ImGui.SameLine()
					ImGui.Text(tostring(ed.data))
					ImGui.PopStyleColor()
					if ed.type == "function" then
						ImGui.SameLine()
						ImGui.PushStyleVar(ImGuiStyleVar.FramePadding, 0, 0)
						ImGui.PushStyleColor(ImGuiCol.FrameBg, 0)
						ImGui.PushStyleColor(ImGuiCol.Text, colors.info)
						ImGui.Text("(")
						ImGui.SameLine()
						ImGui.PushItemWidth(ImGui.GetContentRegionAvail() - ImGui.CalcTextSize('(|)'))
						local text, enter_pressed = ImGui.InputText("##Text" .. ids, bd.texts[ids] or '', 65535, ImGuiInputTextFlags.EnterReturnsTrue)
						local tooltip = bd.tooltips[ids]
						if tooltip and ImGui.IsItemHovered() then
							ImGui.PushStyleColor(ImGuiCol.Text, tooltip.color)
							ImGui.SetTooltip(tooltip.message);
							ImGui.PopStyleColor()
						end
						ImGui.PopItemWidth()
						ImGui.SameLine()
						ImGui.Text(")")
						ImGui.PopStyleColor()
						ImGui.PopStyleColor()
						ImGui.PopStyleVar()
						if enter_pressed then
							local result = table.pack(pcall(ed.data, peval(text)))
							if result.n > 1 then
								local color, message
								if result[1] then
									color = colors.leaf
									message = tostring_vararg(table.unpack(result, 2, result.n))
								else 
									color = colors.null
									message = result[2]
								end
								bd.tooltips[ids] = { message = message, color = color }
							end
							bd.texts[ids] = nil
						else
							bd.texts[ids] = text
						end
						if bd.data and ed.show == "call" then
							local params = bd.data.params
							if params == nil then
								local name = bd.data.name
								if name == nil then
									local script_name = bd.data.script_name
									if script_name ~= nil then
										name = script_name:sub(script_prefix_index)
									end
								end
								if name ~= nil then
									local script = hardcoded.script[name]
									if script then 
										params = script.params
									end
								end
							end
							if params ~= nil then
								ImGui.PushStyleColor(ImGuiCol.Text, colors.fake)
								ImGui.Text("")
								ImGui.SameLine()
								ImGui.Text("")
								ImGui.SameLine()
								ImGui.Text("params:")
								for _,p in ipairs(params) do
									ImGui.SameLine()
									ImGui.Text(p.name)
									if p.value and ImGui.IsItemHovered() then
										ImGui.SetTooltip(p.value);
									end
								end
								ImGui.PopStyleColor()
							end
						end
					end
				end
			end
		end
		if _unfolded then
			local filter = bd.filter
			local entries = unfold(ed)
			if entries then
				local skipped = false
				for _,sd in ipairs(entries) do
					if sd.iter or bd.mode == 2 then 
						if not unfolded[bd.index .. '|' .. sd.path] and #filter ~= 0 and not sd.text:match(filter) then
							skipped = true
						else
							render_browser(bd,sd)
						end
					end
				end
				if skipped then
					ImGui.Text("")
					ImGui.SameLine()
					ImGui.Text("")
					ImGui.SameLine()
					ImGui.Text("...")
				end
			end
			if show then ImGui.TreePop() end
		end
	end
end

local frame_period = 60
local frame_counter = 60

local tooltip_flags = --ImGuiWindowFlags.Tooltip |
            ImGuiWindowFlags.NoTitleBar |
            ImGuiWindowFlags.NoMove |
            ImGuiWindowFlags.NoResize |
            --ImGuiWindowFlags.NoSavedSettings |
            ImGuiWindowFlags.AlwaysAutoResize |
			ImGuiWindowFlags.AlwaysUseWindowPadding

local function nop() end

local function selector_tooltip(pushc,popc,mouse_x,mouse_y,instance)
	local vars = proxy.variables(instance.id)
	if vars.user_name then 
		pushc(ImGuiCol.Text, colors.fake)
		ImGui.Text(vars.user_name)
		popc()
	end
	pushc(ImGuiCol.Text, colors.tree)
	ImGui.Text(instance.object_name:sub(2))
	ImGui.SameLine()
	popc()
	pushc(ImGuiCol.Text, colors.leaf)
	ImGui.Text(vars.name or '')
	popc()
	ImGui.Separator()
	ImGui.Text('obj:')
	pushc(ImGuiCol.Text, colors.info)
	ImGui.SameLine()
	ImGui.Text(tostring(instance.object_index))
	popc()
	ImGui.SameLine()
	ImGui.Text('id:')
	ImGui.SameLine()
	pushc(ImGuiCol.Text, colors.info)
	ImGui.Text(tostring(instance.id))
	popc()
end

local function imgui_off_render()
	if root.instances then
		local mouse_x = math.floor(gm.variable_global_get("mouse_x"))
		local mouse_y = math.floor(gm.variable_global_get("mouse_y"))
		local instance = gm.instance_nearest(mouse_x, mouse_y, EVariableType.ALL)
		if instance ~= nil then
			root.instances.nearest = instance
		end
		if ImGui_GetStyleVar and ImGui.IsKeyDown(ImGuiKeyMod.Ctrl) then
			local _w,_h,_x,_y = 0,0
			for _,v in pairs(ImGuiCol) do
				ImGui.PushStyleColor(v,0)
			end
			ImGui.SetNextWindowSize(0,0)
			if ImGui.Begin("##Tooltip Position Hack", ImGuiWindowFlags.NoSavedSettings | ImGuiWindowFlags.Tooltip | tooltip_flags ) then
				_x,_y = ImGui.GetWindowPos()
			end
			if instance ~= nil and ImGui.Begin("##Tooltip Size Hack", ImGuiWindowFlags.NoSavedSettings | ImGuiWindowFlags.Tooltip | tooltip_flags ) then
				selector_tooltip(nop,nop,mouse_x,mouse_y,instance)
				_w,_h = ImGui.GetWindowSize()
			end
			for _ in pairs(ImGuiCol) do
				ImGui.PopStyleColor()
			end
			
			local _,_y_text = ImGui.CalcTextSize('|')
			local _,_y_frame = ImGui_GetStyleVar(ImGuiStyleVar.FramePadding)
			local _,_y_win = ImGui_GetStyleVar(ImGuiStyleVar.WindowPadding)
			ImGui.SetNextWindowPos(_x+5,_y+20-2*(_y_text+_y_frame + _y_win))
			if ImGui.Begin("Selector Tooltip: Position", tooltip_flags ) then
				ImGui.Text('x:')
				ImGui.PushStyleColor(ImGuiCol.Text, colors.leaf)
				ImGui.SameLine()
				ImGui.Text(tostring(mouse_x))
				ImGui.PopStyleColor()
				ImGui.Text('y:')
				ImGui.PushStyleColor(ImGuiCol.Text, colors.leaf)
				ImGui.SameLine()
				ImGui.Text(tostring(mouse_y))
				ImGui.PopStyleColor()
			end
			ImGui.End()
			
			if instance ~= nil then
				ImGui.SetNextWindowPos(_x-_w/2-12.5,_y+20)
				if ImGui.Begin("Selector Tooltip: Instance", tooltip_flags ) then
					selector_tooltip(ImGui.PushStyleColor,ImGui.PopStyleColor,mouse_x,mouse_y,instance)
				end
				ImGui.End()
			
				if ImGui.IsMouseClicked(ImGuiMouseButton.Left) then
					root.instances.selected = instance
				end
				if ImGui.IsMouseClicked(ImGuiMouseButton.Middle) then
					local x,y = table.unpack(last_pos)
					local w,h = table.unpack(last_size)
					create_browser(resolve_vararg_simple("instances","stable",instance.id),x+w,y,w,h)
				end
				if ImGui.IsMouseClicked(ImGuiMouseButton.Right) then
					local x,y = table.unpack(last_pos)
					local w,h = table.unpack(last_size)
					create_details(resolve_vararg_simple("instances","stable",instance.id,proxy.variables),x+w,y,w,h)
				end
			end
		end
	end
end

local closable_true = {true,ImGuiWindowFlags.NoSavedSettings}
local closable_false = {}

local function imgui_on_render()
	local should_refresh = false
	if frame_counter >= frame_period then
		frame_counter = 0
		should_refresh = true
	end
	frame_counter = frame_counter + 1
	local rid = 1
	local first = false
	for bid,bd in pairs(browsers) do
		local closable = closable_false
		if bid ~= rid then 
			closable = closable_true
			local x,y = table.unpack(bd.init_pos)
			local w,h = table.unpack(bd.init_size)
			ImGui.SetNextWindowPos(x,y,ImGuiCond.Once)
			ImGui.SetNextWindowSize(w,h,ImGuiCond.Once)
		end
		if ImGui.Begin(bid == rid and "Object Browser" or "Object Browser##" .. bid, table.unpack(closable)) then
			if first or bid == rid then
				last_pos[1], last_pos[2] = ImGui.GetWindowPos()
				last_size[1], last_size[2] = ImGui.GetWindowSize()
				first = false
			end
			bd.index = bid
			local item_spacing_x, item_spacing_y = ImGui_GetStyleVar(ImGuiStyleVar.ItemSpacing)
			local frame_padding_x, frame_padding_y = ImGui_GetStyleVar(ImGuiStyleVar.FramePadding)
			local num, y_max, x_total, x_swap, x_filter = calculate_text_sizes('...','Filter: ')
			local x,y = ImGui.GetContentRegionAvail()
			-- height of InputText == font_size + frame_padding.y
			-- and we're going to change frame_padding.y temporarily later on
			-- such that InputText's height == max y
			local x_input = x - x_total - item_spacing_x*num
			local y_box = y - y_max - item_spacing_y
			local x_box = x
			ImGui.Text("Filter: ")
			ImGui.SameLine()
			ImGui.PushItemWidth(x_input)
			ImGui.PushStyleVar(ImGuiStyleVar.FramePadding, 0, 0)
			ImGui.PushStyleColor(ImGuiCol.FrameBg, 0)
			local enter_pressed
			bd.filter_text, enter_pressed = ImGui.InputText("##Text" .. bid, bd.filter_text, 65535, ImGuiInputTextFlags.EnterReturnsTrue)
			ImGui.PopStyleColor()
			ImGui.PopStyleVar()
			ImGui.PopItemWidth()
			ImGui.PushStyleColor(ImGuiCol.Button, colors[filter_modes_browser[bd.mode]])
			ImGui.SameLine()
			if ImGui.Button("    ##Swap" .. bid) then
				bd.mode = bd.mode%#filter_modes_browser + 1
			end
			ImGui.PopStyleColor()
			if enter_pressed then
				bd.filter = bd.filter_text
			end
			if should_refresh then
				refresh(bd)
			end
			if bid ~= rid then
				local path = bd.path or "???"
				y_box = y_box - y_max - item_spacing_y
				ImGui.Text("Path: ")
				ImGui.SameLine()
				ImGui.PushStyleVar(ImGuiStyleVar.FramePadding, 0, 0)
				ImGui.PushStyleColor(ImGuiCol.FrameBg, 0)
				ImGui.PushItemWidth(x_input)
				ImGui.InputText("##Path" .. bid, path, #path, ImGuiInputTextFlags.ReadOnly)
				ImGui.PopItemWidth()
				ImGui.PopStyleColor()
				ImGui.PopStyleVar()
			end
			ImGui.PushStyleColor(ImGuiCol.FrameBg, 0)
			if ImGui.BeginListBox("##Box" .. bid,x_box,y_box) then
				ImGui.PopStyleColor()
				render_browser(bd,bd)
				ImGui.EndListBox()
			else
				ImGui.PopStyleColor()
			end
			ImGui.End()
		elseif bid ~= rid then
			browsers[bid] = nil
			ImGui.End()
		end
	end
	for did,dd in pairs(details) do
		local x,y = table.unpack(dd.init_pos)
		local w,h = table.unpack(dd.init_size)
		ImGui.SetNextWindowPos(x,y,ImGuiCond.Once)
		ImGui.SetNextWindowSize(w,h,ImGuiCond.Once)
		if ImGui.Begin("Object Details##" .. did, table.unpack(closable_true)) then
			if first then
				last_pos[1], last_pos[2] = ImGui.GetWindowPos()
				last_size[1], last_size[2] = ImGui.GetWindowSize()
				first = false
			end
			dd.index = did
			local item_spacing_x, item_spacing_y = ImGui_GetStyleVar(ImGuiStyleVar.ItemSpacing)
			local frame_padding_x, frame_padding_y = ImGui_GetStyleVar(ImGuiStyleVar.FramePadding)
			local num, y_max, x_total, x_swap, x_filter = calculate_text_sizes('...','Filter: ')
			local x,y = ImGui.GetContentRegionAvail()
			-- height of InputText == font_size + frame_padding.y
			-- and we're going to change frame_padding.y temporarily later on
			-- such that InputText's height == max y
			local y_input = y_max - ImGui.GetFontSize() - frame_padding_y 
			local x_input = x - x_total - item_spacing_x*num
			local y_box = y - y_max - item_spacing_y
			local x_box = x
			ImGui.Text("Filter: ")
			ImGui.SameLine()
			ImGui.PushItemWidth(x_input)
			ImGui.PushStyleVar(ImGuiStyleVar.FramePadding, 0, 0)
			ImGui.PushStyleColor(ImGuiCol.FrameBg, 0)
			local enter_pressed
			dd.filter_text, enter_pressed = ImGui.InputText("##Text" .. did, dd.filter_text, 65535, ImGuiInputTextFlags.EnterReturnsTrue)
			ImGui.PopStyleColor()
			ImGui.PopStyleVar()
			ImGui.PopItemWidth()
			ImGui.PushStyleColor(ImGuiCol.Button, colors[filter_modes_details[dd.mode]])
			ImGui.SameLine()
			if ImGui.Button("    ##Swap" .. did) then
				dd.mode = dd.mode%#filter_modes_details + 1
			end
			ImGui.PopStyleColor()
			do
				local path = dd.path or "???"
				y_box = y_box - y_max - item_spacing_y
				ImGui.Text("Path:  ")
				ImGui.SameLine()
				ImGui.PushStyleVar(ImGuiStyleVar.FramePadding, 0, 0)
				ImGui.PushStyleColor(ImGuiCol.FrameBg, 0)
				ImGui.PushItemWidth(x)
				ImGui.InputText("##Path" .. did, path, #path, ImGuiInputTextFlags.ReadOnly)
				ImGui.PopItemWidth()
				ImGui.PopStyleColor()
				ImGui.PopStyleVar()
			end
			if enter_pressed then
				dd.filter = dd.filter_text
			end
			if should_refresh then
				refresh(dd)
			end
			ImGui.PushStyleColor(ImGuiCol.FrameBg, 0)
			if ImGui.BeginListBox("##Box" .. did,x_box,y_box) then
				ImGui.PopStyleColor()
				render_details(dd)
				ImGui.EndListBox()
			else
				ImGui.PopStyleColor()
			end
			ImGui.End()
		else
			details[did] = nil
			ImGui.End()
		end
	end
end

create_browser(root_entries())
gui.add_imgui(imgui_on_render)
gui.add_always_draw_imgui(function() if not gui.is_open() then return imgui_off_render() end end)
gm.pre_code_execute( function(_,_,ccode)
	if not root.instances then
		root.instances = {
			all = gm.CInstance.instances_all,
			active = gm.CInstance.instances_active,
			stable = gm.CInstance.instance_id_to_CInstance
		}
	end
end )

globals = util.merge({},util,proxy)

globals.root = root

local repl_environment = setmetatable({},{
	__index = function(_,k)
		local v = globals[k]
		if v ~= nil then return v end
		return _G[k]
	end,
	__newindex = globals
})

local autoexec = "autoexec"

local function tostring_literal(value)
	-- TODO: expand tables python-style?
	if type(value) == "string" then
		local lined, _lined = 0, value:gmatch("\n")
		for _ in _lined do lined = lined + 1 end
		local dquoted, _dquoted = 0, value:gmatch([=["]=])
		for _ in _dquoted do dquoted = dquoted + 1 end
		local squoted, _squoted = 0, value:gmatch([=[']=])
		for _ in _squoted do squoted = squoted + 1 end
		local edquoted, _edquoted = 0, value:gmatch([=[\"]=])
		if lined > 0 or (dquoted > 0 and squoted > 0) then
			local special, _special = 0, value:gmatch([=[[=]]=])
			for _ in _special do special = special + 1 end
			local eq = "="
			for i = 1, special do
				eq = eq .. '='
			end
			return '['..eq..'[' .. value .. ']'..eq..']'
		elseif squoted > 0 then
			return '"' .. value .. '"'
		else
			return "'" .. value .. "'"
		end
	end
	return tostring(value)
end

local function tostring_vararg(raw, ...)
	local s = ""
	for _,v in util.vararg(...) do
		v = raw and tostring(v) or tostring_literal(v)
		s = s .. '\t' .. v
	end
	return s:sub(2,#s)
end

console = {}

console.log = {
	error = {
		prefix = {
			debug = "",
			shown = ""
		},
		logger = false,--log.error,
		color = 0xFF2020EE,
	},
	info = {
		prefix = {
			debug = "",
			shown = ""
		},
		logger = log.info,
		color = 0xFFEEEEEE,
	},
	warning = {
		prefix = {
			debug = "",
			shown = ""
		},
		logger = log.warning,
		color = 0xFF20EEEE,
	},
	history = {
		prefix = {
			debug = "",
			shown = "] "
		},
		logger = false,
		color = 0xEECCCCCC,
	},
	echo = {
		prefix = {
			debug = "[Echo]:",
			shown = ""
		},
		logger = log.info,
		color = 0xFFEEEEEE,
	},
	print = {
		prefix = {
			debug = "[Print]:",
			shown = ""
		},
		logger = log.info,
		color = 0xFFEEEEEE,
	},
	returns = {
		prefix = {
			debug = "[Returns]:",
			shown = ""
		},
		logger = log.info,
		color = 0xFFFFFF20,
	}
}

local console_log_meta = { __call = function(lg,...) return lg.log(...) end }

for _,lg in pairs(console.log) do
	lg.log = function(md, raw, ...)
		local text = tostring_vararg(raw, ...)
		table.insert(md.raw, text)
		table.insert(md.shown, lg.prefix.shown .. text)
		md.colors[#md.raw] = lg.color
		if lg.logger then
			return lg.logger( lg.prefix.debug .. md.prefix .. text )
		end
	end
	setmetatable(lg,console_log_meta)
end

local function repl_execute_lua(md, env, text, ...)
	util.merge(globals,md.definitions)
	local func, err = text, ''
	if type(text) == "string" then
		func, err = load( "return " .. text )
		if not func then
			func, err = load( text )
			if not func then
				return false, err
			end
		end
	end
	if env then
		setfenv( func, env == true and repl_environment or env )
	end
	all_new_metas()
	local ret = table.pack(pcall( func, ... ))
	all_old_metas()
	return table.unpack(ret,1,ret.n)
end

--https://stackoverflow.com/a/28664691
local parse_command_text
local parse_multicommand_text
do 
	-- TODO: This needs to be improved regarding properly handling embedded and mixed quotes!
	local parse_buffer = {}
	function parse_command_text(text)
		util.iclear(parse_buffer)
		local spat, epat, buf, quoted = [=[^(['"])]=], [=[(['"])$]=]
		for str in text:gmatch("%S+") do
			local squoted = str:match(spat)
			local equoted = str:match(epat)
			local escaped = str:match([=[(\*)['"]$]=])
			if squoted and not quoted and not equoted then
				buf, quoted = str, squoted
			elseif buf and equoted == quoted and #escaped % 2 == 0 then
				str, buf, quoted = buf .. ' ' .. str, nil, nil
			elseif buf then
				buf = buf .. ' ' .. str
			end
			if not buf then
				local token = str:gsub(spat,""):gsub(epat,"")
				table.insert(parse_buffer,token)
			end
		end
		if buf then return false, "Missing matching quote for "..buf end
		return true, table.unpack(parse_buffer)
	end
	function parse_multicommand_text(text)
		util.iclear(parse_buffer)
		for mstr in text:gmatch("[^\r\n]+") do
			local pquoted, buf = 0
			for str in mstr:gmatch("[^;]+") do
				str = str:gsub("^%s\\*", "")
				local quoted, _quoted = 0, str:gmatch([=[['"]]=])
				for _ in _quoted do quoted = quoted + 1 end
				local escaped, _escaped = 0, str:gmatch([=[\['"]]=])
				for _ in _escaped do escaped = escaped + 1 end
				pquoted = (pquoted+quoted-escaped) % 2
				if not buf and pquoted == 1 then
					buf = str
				elseif buf and pquoted == 0 then
					str, buf = buf .. ';' .. str, nil
				elseif buf and pquoted == 1 then
					buf = buf .. ';' .. str
					str, buf, quoted = buf .. ';' .. str, nil, 0
				end
				if not buf then
					local token = str
					table.insert(parse_buffer,token)
				end
			end
			if buf then return false, "Missing matching quote for "..buf end
		end
		return true, table.unpack(parse_buffer)
	end
end

local run_console_multicommand
local function run_console_command(md, text)
		local parse_result = table.pack(parse_command_text(text))
		local status, command_name = parse_result[1], parse_result[2]
		if not status then
			return console.log.error(md, true, command_name)
		end
		local alias = console.aliases[command_name]
		if alias ~= nil then
			return run_console_multicommand(md, alias)
		end
		local command = console.commands[command_name]
		if command == nil then
			return console.log.error(md, true, 'no command by the name of "' .. command_name .. '" found')
		end
		local ret = table.pack(pcall(command,md,table.unpack(parse_result, 3, parse_result.n)))
		if ret.n <= 1 then return end
		if ret[1] == false then
			return console.log.error(md, true, ret[2])
		end
		return console.log.info(md, false, table.unpack(ret, 2, ret.n))
end
function run_console_multicommand(md, text)
		local parse_result = table.pack(parse_multicommand_text(text))
		local status, err = parse_result[1], parse_result[2]
		if not status then
			console.log.error(md, true, err)
		end
		table.unpack(parse_result, 2, parse_result.n)
		for i = 2, parse_result.n do
			run_console_command(md, parse_result[i])
		end
end


console.aliases = {}
console.binds = {}
console.ibinds = {}

console.command_help = {
	{"help","[0..1]","lists the available commands"},
	{"echo","[..]","prints a message to the console"},
	{"lua","[1..]","executes lua code and shows the result"},
	{"luae","[1]","executes lua file with args and shows the result"},
	{"exec","[1]","executes a file containing a list of console commands"},
	{"alias","[0..2]","defines a command that represents multiple commands"},
	{"bind","[0..2]","binds a key combination to run commands during gameplay"},
	{"ibind","[0..2]","binds a key combination to run commands on the mod gui"}
}

local _MouseButton
local _KeyMod
local _Key

local function check_bind(md,k)
	local bind = ''
	for key in k:upper():gmatch("(%w+)") do
		if _Key == nil then
			_MouseButton = {}
			for k in pairs(ImGuiMouseButton) do
				_MouseButton[k:upper() .. "MOUSE"] = k .. "Mouse"
			end
			_KeyMod = {}
			for k in pairs(ImGuiKeyMod) do
				_KeyMod[k:upper()] = k
			end
			_Key = {}
			for k in pairs(ImGuiKey) do
				_Key[k:upper()] = k
			end
		end
		key = _MouseButton[key] or _KeyMod[key] or _Key[key]
		if not key then 
			return console.log.error(md, true, 'invalid key combo: "' .. k .. '"')
		end
		bind = bind .. '+' .. key
	end
	return bind:sub(2)
end

console.commands = {
	help = function(md,stub)
		if stub then
			local msg = console.command_help[stub]
			if not msg then 
				return console.log.error(md, true, 'no command by the name of "' .. stub .. '" found')
			end
			return console.log.echo(md, true, msg)
		end
		for _,h in ipairs(console.command_help) do
			console.log.echo(md, true, table.unpack(h))
		end
	end,
	echo = function(md,...)
		local text = ""
		for _, arg in util.vararg(...) do
			text = text .. ' ' .. arg
		end
		text = text:sub(2,#text)
		return console.log.echo(md,true,text)
	end,
	lua = function(md,...)
		local text = ""
		for _, arg in util.vararg(...) do
			text = text .. ' ' .. arg
		end
		text = text:sub(2,#text)
		if #text == 0 then
			return console.log.error(md, true, "cannot execute empty lua code.")
		end
		local ret = table.pack(repl_execute_lua(md, true, text))
		if ret.n <= 1 then return end
		if ret[1] == false then
			return console.log.error(md, true, ret[2])
		end
		return console.log.returns(md, false, table.unpack( ret, 2, ret.n ))
	end,
	--https://stackoverflow.com/a/10387949
	luae = function(md,path,...)
		local qualpath = _ENV["!plugins_data_mod_folder_path"] .. '/' .. path
		local file = io.open(qualpath,"rb")
		if not file or type(file) == "string" or type(file) == "number" then
			file = io.open(qualpath .. ".lua","rb")
			if not file or type(file) == "string" or type(file) == "number" then
				return console.log.warning(md, true, 'attempted to read the lua file "' .. path .. '", but failed.')
			end
		end
		local data = file:read("*a")
		file:close()
		local ret = table.pack(repl_execute_lua(md, true, data, ...))
		if ret.n <= 1 then return end
		if ret[1] == false then
			return console.log.error(md, true, ret[2])
		end
		return console.log.returns(md, false, table.unpack( ret, 2, ret.n ))
	end,
	exec = function(md,path)
		local qualpath = _ENV["!plugins_data_mod_folder_path"] .. '/' .. path
		local file = io.open(qualpath,"rb")
		if not file or type(file) == "string" or type(file) == "number" then
			file = io.open(qualpath .. ".txt","rb")
			if not file or type(file) == "string" or type(file) == "number" then
				return console.log.warning(md, true, 'attempted to read the batch file "' .. path .. '", but failed.')
			end
		end
		local data = file:read("*a")
		file:close()
		return run_console_multicommand(md,data)
	end,
	alias = function(md,name,...)
		if name == nil then
			for k,v in pairs(console.aliases) do
				console.log.echo(md, true, k,v)
			end
			return
		end
		local text = ""
		for _, arg in util.vararg(...) do
			text = text .. ' ' .. arg
		end
		text = text:sub(2,#text)
		if #text == 0 then
			local msg = console.aliases[name]
			if not msg then 
				return console.log.error(md, true, 'no alias by the name of "' .. name .. '" exists')
			end
			return console.log.echo(md, true, msg)
		end
		console.aliases[name] = text
	end,
	bind = function(md,name,...)
		if name == nil then
			for k,v in pairs(console.binds) do
				console.log.echo(md, true, k,v)
			end
			return
		end
		name = check_bind(md,name)
		if name == nil then return end
		local text = ""
		for _, arg in util.vararg(...) do
			text = text .. ' ' .. arg
		end
		text = text:sub(2,#text)
		if #text == 0 then
			local msg = console.binds[name]
			if not msg then 
				return console.log.error(md, true, 'no bind for the key combo "' .. name .. '" exists')
			end
			return console.log.echo(md, true, msg)
		end
		console.binds[name] = text
	end,
	ibind = function(md,name,...)
		if name == nil then
			for k,v in pairs(console.ibinds) do
				console.log.echo(md, true, k,v)
			end
			return
		end
		name = check_bind(md,name)
		if name == nil then return end
		local text = ""
		for _, arg in util.vararg(...) do
			text = text .. ' ' .. arg
		end
		text = text:sub(2,#text)
		if #text == 0 then
			local msg = console.ibinds[name]
			if not msg then 
				return console.log.error(md, true, 'no UI bind for the key combo "' .. name .. '" exists')
			end
			return console.log.echo(md, true, msg)
		end
		console.ibinds[name] = text
	end
}

console.modes = {
	{
		name = "Notepad",
		prefix = "[Notes]:",
		on_enter = function(md) return function(text)
			return console.log.info(md, true, text)
		end end
	},
	{
		name = "Console",
		prefix = "[Console]:",
		on_enter = function(md) return function(text)
			console.log.history(md, true, text)
			return run_console_multicommand(md, text)
		end end
	},
	{
		name = "Lua REPL",
		prefix = "[LuaREPL]:",
		on_enter = function(md) return function(text)
			console.log.history(md, true, text)
			local ret = table.pack(repl_execute_lua(md, true, text))
			if ret.n <= 1 then return end
			if ret[1] == false then
				return console.log.error(md, true, ret[2])
			end
			globals._ = ret[2]
			return console.log.returns(md, false, table.unpack(ret, 2, ret.n))
		end end
	}
}

local function console_mode_definitions(get_md)
	return {
		print = function(...)
			return console.log.print(get_md(),true,...)
		end,
		tprint = function(...)
			for _,o in util.vararg(...) do
				console.log.print(get_md(),false,o)
				local t = type(o)
				if t == "table" or t == "userdata" then
					for k,v in pairs(o) do
						console.log.print(get_md(),false,k,v)
					end
				end
			end
		end,
		itprint = function(...)
			for _,o in util.vararg(...) do
				console.log.print(get_md(),false,o)
				local t = type(o)
				if t == "table" or t == "userdata" then
					for k,v in ipairs(o) do
						console.log.print(get_md(),false,k,v)
					end
				end
			end
		end,
		mprint = function(m,...)
			for _,o in util.vararg(...) do
				console.log.print(get_md(),false,o)
				local t = type(o)
				if t == "table" or t == "userdata" then
					for k,v in pairs(o) do
						console.log.print(get_md(),false,k,m(v))
					end
				end
			end
		end,
		imprint = function(m,...)
			for _,o in util.vararg(...) do
				console.log.print(get_md(),false,o)
				local t = type(o)
				if t == "table" or t == "userdata" then
					for k,v in ipairs(o) do
						console.log.print(get_md(),false,k,m(v))
					end
				end
			end
		end,
		eval = function(...)
			return repl_execute_lua(get_md(), ...)
		end
	}
end

for mi,md in ipairs(console.modes) do
	util.merge(md,{
		current_text = "",
		enter_pressed = false,
		history_offset = 0,
		history = {},
		shown = {},
		raw = {},
		selected = {},
		selected_last = nil,
		colors = {},
		index = mi,
		on_enter = md.on_enter(md),
		definitions = console_mode_definitions(function() return md end)
	})
end

console.mode = console.modes[1]
util.merge(globals,console_mode_definitions(function() return console.mode end))

local calculate_text_sizes
do
	local calculate_text_sizes_x_buffer = {}
	function calculate_text_sizes(...)
		-- don't need to clear the buffer since 
		-- we only iterate over the region we overwrite
		local frame_padding_x, frame_padding_y = ImGui_GetStyleVar(ImGuiStyleVar.FramePadding)
		local frame_padding_x_2 = 2*frame_padding_x
		local frame_padding_y_2 = 2*frame_padding_y
		local my = 0 -- maximum y value in this row
		local sx = 0 -- sum of x values in this row
		local n -- number of items in this row
		for i,t in util.vararg(...) do
			n = i
			local x,y = ImGui.CalcTextSize(t)
			x = x + frame_padding_x_2
			y = y + frame_padding_y_2
			calculate_text_sizes_x_buffer[i] = x
			sx = sx + x
			if y > my then my = y end
		end
		return n, my, sx, table.unpack(calculate_text_sizes_x_buffer, 1, n)
	end
end

local function run_bind(m,k,v)
	local pass = true
	for key in k:gmatch("(%w+)") do
		if k:match("Mouse$") and ImGuiMouseButton[key:sub(1,#key-5)] then
			pass = pass and ImGui.IsMouseClicked(ImGuiMouseButton[key:sub(1,#key-5)])
		elseif ImGuiKeyMod[key] then
			pass = pass and ImGui.IsKeyDown(ImGuiKeyMod[key])
		elseif ImGuiKey[key] then
			pass = pass and ImGui.IsKeyPressed(ImGuiKey[key])
		else
			pass = false
		end
		if not pass then break end
	end
	if pass then
		run_console_multicommand(m,v)
	end
end

local tab_selected = false

local function imgui_off_render()
	local m = console.mode
	for k,v in pairs(console.binds) do
		pcall(run_bind,m,k,v)
	end
end

local function imgui_on_render()
	local m = console.mode
	for k,v in pairs(console.ibinds) do
		pcall(run_bind,m,k,v)
	end
	if ImGui.Begin("Script Console", ImGuiWindowFlags.NoTitleBar) then
		if ImGui.BeginTabBar("Mode") then
			local item_spacing_x, item_spacing_y = ImGui_GetStyleVar(ImGuiStyleVar.ItemSpacing)
			local frame_padding_x, frame_padding_y = ImGui_GetStyleVar(ImGuiStyleVar.FramePadding)
			local bot_num, bot_y_max, bot_x_total, x_focus = calculate_text_sizes("|")
			local x,y = ImGui.GetContentRegionAvail()
			local x_input = x - bot_x_total - item_spacing_x*bot_num
			-- height of InputText == font_size + frame_padding.y
			-- and we're going to change frame_padding.y temporarily later on
			-- such that InputText's height == max y
			local y_input = bot_y_max - ImGui.GetFontSize() - frame_padding_y 
			local box_y = y - bot_y_max - item_spacing_y*2
			for mi,md in ipairs(console.modes) do
				local ds = md.name
				local ms = tostring(mi)
				if (tab_selected and console.mode == md) and ImGui.BeginTabItem(ds, ImGuiTabItemFlags.SetSelected) or ImGui.BeginTabItem(ds) then
					if not tab_selected then console.mode = md end
					if console.mode == md then tab_selected = false end
					ImGui.EndTabItem()
					if autoexec then
						local name = autoexec
						autoexec = nil
						run_console_command(md,"exec " .. name)
					end
					ImGui.PushStyleColor(ImGuiCol.FrameBg, 0)
					if ImGui.BeginListBox("##Box" .. ms,x,box_y) then
						ImGui.PopStyleColor()
						local selected = not ImGui.IsMouseClicked(ImGuiMouseButton.Left)
						for li,ls in ipairs(md.shown) do
							local tall = select(2,ImGui.CalcTextSize(ls, false, x-frame_padding_x*2-item_spacing_x))
							ImGui.Selectable("##Select" .. ms .. tostring(li), md.selected[li] or false, ImGuiSelectableFlags.AllowDoubleClick, 0, tall)
							if ImGui.IsItemClicked(ImGuiMouseButton.Left) then
								selected = true
								if ImGui.IsKeyDown(ImGuiKeyMod.Shift) then
									local ll = md.selected_last
									if ll then
										for i in ipairs(md.shown) do
											md.selected[i] = false
										end
										local step = ll<li and -1 or 1
										for i = li, ll, step do
											md.selected[i] = true
										end
									else
										md.selected_last = li
									end
								elseif ImGui.IsKeyDown(ImGuiKeyMod.Ctrl) then
									md.selected_last = li
									md.selected[li] = not md.selected[li]
								else
									md.selected_last = li
									for i in ipairs(md.shown) do
										md.selected[i] = false
									end
									md.selected[li] = true
								end
							end
							ImGui.SameLine()
							local color = md.colors[li]
							if color ~= nil then ImGui.PushStyleColor(ImGuiCol.Text, color) end
							ImGui.TextWrapped(ls)
							if color ~= nil then ImGui.PopStyleColor() end
						end
						if not selected then
							for i in ipairs(md.shown) do
								md.selected[i] = false
							end
						end
						ImGui.EndListBox()
					else
						ImGui.PopStyleColor()
					end
					ImGui.PushItemWidth(x_input)
					ImGui.PushStyleVar(ImGuiStyleVar.FramePadding, frame_padding_x, y_input)
					md.current_text, md.enter_pressed = ImGui.InputText("##Text" .. ms, md.current_text, 65535, ImGuiInputTextFlags.EnterReturnsTrue)
					ImGui.PopStyleVar()
					ImGui.PopItemWidth()
					if ImGui.IsWindowFocused(ImGuiFocusedFlags.RootAndChildWindows) then
						if not ImGui.IsItemFocused() and not ImGui.IsItemActive() then
							if ImGui.IsKeyDown(ImGuiKeyMod.Ctrl) and ImGui.IsKeyPressed(ImGuiKey.C) then
								local text
								for hi,b in ipairs(md.selected) do
									if b then 
										local line = md.raw[hi] or md.shown[hi]
										if text == nil then
											text = line
										else
											text = text .. '\n' .. line
										end
									end
								end
								ImGui.SetClipboardText(text)
							end
							if ImGui.IsKeyDown(ImGuiKeyMod.Ctrl) and ImGui.IsKeyPressed(ImGuiKey.A) then
								for i in ipairs(md.shown) do
									md.selected[i] = true
								end
							end
							if ImGui.IsKeyDown(ImGuiKeyMod.Ctrl) and ImGui.IsKeyPressed(ImGuiKey.D) then
								for i in ipairs(md.shown) do
									md.selected[i] = false
								end
							end
							if ImGui.IsKeyDown(ImGuiKeyMod.Ctrl) and ImGui.IsKeyPressed(ImGuiKey.Z) then
								util.iclear(md.shown,md.raw,md.selected,md.colors)
							end
							if ImGui.IsKeyPressed(ImGuiKey.Tab) then
								tab_selected = true
								local n = #console.modes
								console.mode = console.modes[mi % n + 1]
							end
						end
						local changed_offset
						if ImGui.IsKeyPressed(ImGuiKey.UpArrow) then
							md.history_offset = md.history_offset + 1
							if md.history_offset > #md.history then
								md.history_offset = #md.history
							end
							changed_offset = true
						end
						if ImGui.IsKeyPressed(ImGuiKey.DownArrow) then
							md.history_offset = md.history_offset - 1
							if md.history_offset < 0 then
								md.history_offset = 0
							end
							changed_offset = true
						end
						if changed_offset then
							if md.history_offset == 0 then
								md.current_text = ""
							else
								md.current_text = md.history[#md.history-md.history_offset+1]
							end
						end
					end
				end
			end
			ImGui.EndTabBar()
		end
		ImGui.End()
	end
	-- handling entering input separate from constructing the UI
	-- so actions that use ImGui will be separate from the console's UI
	for mi,md in ipairs(console.modes) do
		if md.enter_pressed then
			md.enter_pressed = false
			md.history_offset = 0
			local text = md.current_text
			table.insert(md.history,text)
			md.current_text = ""
			md.on_enter(text)
		end
	end
end

gui.add_imgui(imgui_on_render)
gui.add_always_draw_imgui(function() if not gui.is_open() then return imgui_off_render() end end)

function rcon(text)
	return run_console_multicommand(console.mode,text)
end

function rlua(text, ...)
	return select(2,repl_execute_lua(console.mode,true,text,...))
end