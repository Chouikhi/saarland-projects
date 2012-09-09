function map_fn = get_map_fn(fn)
  
  if not(isequal(fn(end-3:end), '.png'))
    error('image filename not ending in png');
  end

  slashes = strfind(fn, '/');
  if length(slashes) == 0
    last_slash = 0;
  else
    last_slash = slashes(end);
  end

  map_fn = strcat(fn(1:last_slash), 'maps/', ...
      fn(last_slash+1:end-4), '-map', fn(end-3:end));

end
