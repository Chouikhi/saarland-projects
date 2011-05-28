%
% computes all distances of potential point correspondances between 
% the images. Uses the descriptors given as input. Computes the distances
% by applying the chi_square metric.
% 
% D1   : set of descriptors computed from interest points in image 1
% D2   : set of descriptors computed from interest points in image 2
%
% D    : Distance matrix, each entry corresponds to a possible pair
%
function D = get_point_dist(D1,D2)
  D = zeros(length(D1), length(D2));
  for i = 1:length(D1)
    for j = 1:length(D2)
      % TODO: Get dimensions right
      D(i,j) = dist_chi2(D1(i), D2(j));
    end
  end
end
