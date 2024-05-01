local muun = require("type-track.muun")

---@class Inheritable
---@field __class any
local Inheritable = muun("Inheritable")

---@param self unknown
---@param cls unknown
---@return boolean
function Inheritable.is_instance(self, cls)
	if type(self) ~= "table" or not cls then
		return false
	end

	local obj_cls = self.__class
	while obj_cls and obj_cls ~= cls do
		obj_cls = obj_cls.__parent
	end

	return obj_cls == cls
end

return Inheritable
