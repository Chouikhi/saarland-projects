% 
% compute intersection distance between x and y (slide 26 in the lecture)
% return 1 - intersection, so that smaller values also correspond to more similart histograms
% 

function d = dist_intersect(x, y)
  d = 1 - sum(min(x, y));
end
