function [M] = randmat(n, m, p)
  base = rand(n, m);
  M = uint8(base < p);
end
