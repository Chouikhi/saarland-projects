function d = dist_chi2(x,y)
  
  d1 = (x-y).^2;
  d2 = x+y;
  d2(find(d2==0)) = 1;
  
  d = sum(d1 ./ d2);
  
