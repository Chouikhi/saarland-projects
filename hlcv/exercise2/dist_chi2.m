% 
% compute chi2 distance between x and y (slide 31 in cv-ss11-0504-instance-v1.pdf)
% 

function d = dist_chi2(x,y)
  d = sum(((x - y) .^ 2) ./ (x + y + 2));
end
