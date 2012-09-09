function dist = ln_dist(v1, v2, n)
  if n < 0
    dist = max(abs(v1 - v2));
  else
    dist = sum(abs(v1 - v2) .^ n) ^ (1/n);
  end
end
