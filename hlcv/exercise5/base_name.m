% return unique string for every filename (without /)
function name = base_name(path)

  slash = findstr(path, '/');
  if length(slash) >= 2
    slash = slash(end-1) + 1;
  else
    slash = 1;
  end
  
  dot = findstr(path(slash:end), '.');
  if length(dot) > 0
    dot = dot(end) - 2 + slash;
  else
    dot = length(path);
  end

  name = path(slash:dot);  % nice huh!
  name(findstr(name, '/')) = '_';

end
