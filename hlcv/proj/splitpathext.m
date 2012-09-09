% SPLITPATHEXT Split a full file name to directory, filename without extension,
%   and extension.
%   [dir, basename, ext] = splitpathext(fullname)
function [dir, basename, ext] = splitpathext(fullname)

  dots = strfind(fullname, '.');
  slashes = strfind(fullname, '/');

  if length(slashes) > 0
    last_slash = slashes(end);
  else
    last_slash = 0;
  end

  if length(dots) > 0 && dots(end) > last_slash
    last_dot = dots(end);
  else
    last_dot = length(fullname) + 1;
  end

  dir = './';
  ext = '';
  if last_slash > 0
    dir = fullname(1:last_slash);
  end

  if last_dot <= length(fullname)
    ext = fullname(last_dot+1:end);
  end

  basename = fullname(last_slash + 1:last_dot - 1);
end
