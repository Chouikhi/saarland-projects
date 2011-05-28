function d = dist_chi2(x,y)
  assert(all(size(x) == size(y)), 'need 2 structures of equal dimensions');
  
  d1 = (x-y).^2;
  d2 = x+y;
  d2(find(d2==0)) = 1;
  
  d = sum(d1 ./ d2);
  
