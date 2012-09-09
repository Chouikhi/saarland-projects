function [Idx, Dist] = findnn_chi2( D1, D2 )
  
  N = size(D1,1);
  M = size(D2,1);
  Idx  = zeros(N,1);
  Dist = zeros(N,1);
  
  for i=1:N
    minidx  = 1;
    mindist = dist_chi2(D1(i,:),D2(1,:));
    for j=2:M
      d = dist_chi2(D1(i,:),D2(j,:));

      if d<mindist
        mindist = d;
        minidx  = j;
      end;
    end;
    
    Idx(i)  = minidx;
    Dist(i) = mindist;
  end
  

  