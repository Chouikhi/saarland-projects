ol = [5 10 20 25 30 40 50];
ws = (100 - ol) ./ 100;
ns = 2:8;

k = zeros(length(ns), length(ws));

for ni = 1:length(ns)
  for wi = 1:length(ws)
    zn = 1 - ws(wi) ^ ns(ni);
    k(ni, wi) = ceil(log(0.01) / log(zn));
  end
end

k