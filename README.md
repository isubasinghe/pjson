# pjson
A parser for a subset of JSON. Is extremely fast (o(n), where n is the number of characters) and can support infinite JSON nesting (as long as you have enough memory) thanks to thunks. 
Currently it has **awful** floating point support and **awful** string support (\n\t\b\r are all not supported inside strings).
