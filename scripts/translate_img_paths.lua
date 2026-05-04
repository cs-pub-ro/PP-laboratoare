function Pandoc(doc)
  if not doc.meta.lab_name then
    error("missing required metadata field: lab_name")
  end
  if not doc.meta.parent_ns then
    error("missing required metadata field: parent_ns")
  end
  local lab_name = pandoc.utils.stringify(doc.meta.lab_name)
  local parent_ns = pandoc.utils.stringify(doc.meta.parent_ns)
  local parent_ns_colon = string.gsub(parent_ns, "/", ":")
  local prefix = ":" .. parent_ns_colon .. ":img:" .. lab_name .. ":"

  local filter = {
    Image = function(el)
      local _, _, suffix = string.find(el.src, "img%-ocw/(.*)$")
      if suffix then
        suffix = string.gsub(suffix, "/", ":")
        el.src = prefix .. suffix
      end
      return el
    end
  }

  for i, block in ipairs(doc.blocks) do
    doc.blocks[i] = pandoc.walk_block(block, filter)
  end
  return doc
end
