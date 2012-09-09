% 
% compute euclidean distance between x and y (slide 27 in the lecture)
% 


function d = dist_l2(x,y)
  d = sum((x - y) .^ 2);
end
