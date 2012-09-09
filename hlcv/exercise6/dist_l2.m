function d = dist_l2(x,y)
  
  d1 = (x-y).^2;
  d = sqrt(sum(d1));
