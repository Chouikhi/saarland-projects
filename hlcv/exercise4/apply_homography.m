% The function applies a homography H to a set of 2D or 3D points in
% cartesian coordinates to yeald transformed points
% (again in cartesian coordinates)

function transformed_points = apply_homography(H, points)
  transformed_points_hom = H * cart2hom(points);

  % The resulting points should be like
  %   [ x1 x2 ..
  %     y1 y2 ..
  %     z1 z2 ..]
  assert(size(transformed_points_hom, 1) == 3 ...
      || size(transformed_points_hom, 1) == 4);
  transformed_points = hom2cart(transformed_points_hom);
end
