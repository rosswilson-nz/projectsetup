function Math(el)
  if (el.mathtype == "InlineMath") then
    return pandoc.RawInline("typst", "$" .. el.text .. "$")
  else
    return pandoc.RawInline("typst", "$ " .. el.text .. " $")
  end
end
