---@param lists any[][]
---@return fun(): any[]?
local function permute(lists)
	local indexes = {}
	local total = 1
	for _, list in ipairs(lists) do
		table.insert(indexes, 1)
		total = total * #list
	end

	local count = 0
	return function()
		if count >= total then
			return nil
		end
		count = count + 1

		local result = {}
		for i, list in ipairs(lists) do
			result[i] = list[indexes[i]]
		end

		for i = #lists, 1, -1 do
			local list = lists[i]
			local index = indexes[i]
			if index < #list then
				indexes[i] = index + 1
				break
			else
				indexes[i] = 1
			end
		end

		return result
	end
end

return permute
