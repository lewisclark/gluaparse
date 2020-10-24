local function foo(bob, jimmy)
	if true then
		print(bob, fred)
	elseif false then
		cow(bob, fred)
	else
		yeet(bob, fred)
	end

	return 0 - 10 + 2
end

do
	do foo("hello", 0.39) end

	do end
end

foo = true
foo = false
foo = true and false
foo = true and false or true
local dog = {
	abc = true,
	[1] = true,
	[function() end] = true,
	["true"] = true,
}

dog.woof = "yep"
dog.meow = {}
dog.meow.aa = false

dog.meow[yeet] = "ok"
dog.meow[69] = "ok"
