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
function D = get_point_dist(D1, D2)
  sz1 = size(D1, 1);
  sz2 = size(D2, 1);
  D = zeros(sz1, sz2);
  for i = 1:sz1
    for j = 1:sz2
      D(i, j) = dist_chi2(D1(i, :), D2(j, :));
    end
  end
end
