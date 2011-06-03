%
% estimates the homography between two images by using potential 
% matching points between the images. RANSAC is used for computation of
% the homography. Use the function get_hom.m in order to estimate homography
% from point correspondences.
% 
%
% x1   : the x coordinates of the potential matching points in image 1
% y1   : the y coordinates of the potential matching points in image 1
% x2   : the x coordinates of the potential matching points in image 2
% y2   : the y coordinates of the potential matching points in image 2
% img1 : image 1
% img2 : image 2
%
% H    : Best estimated homography relating both input images using RANSAC
% 
function H = get_ransac_hom(x1,y1,x2,y2,img1,img2)

  assert(isequal(size(x1), size(y1)));
  assert(isequal(size(x2), size(y2)));
  assert(isequal(size(x1), size(x2)));
  sz = max(size(x1));
  p1 = [x1; y1];
  p2 = [x2; y2];

  %% assume a conservative probability for picking a pair of corresponding 
  %% points and estimate the amount of iterations `k' necessary to pick four
  %% true correspondances with, let's say, 99 percent probability
  pInlier = 0.60;     % 60% inliers is a conservative guess
  pFail = 1 - 0.99;
  
  % example: assume probability of true match : pInlier
  % -> all 4 sample pairs match: pInlier^4
  % find the amount of times k we have to draw in order to find a sample 
  % without outliers.
  % -> (1-pInlier^4)^k <= pFail (fail probability smaller than pFail)
  % -> log (pFail) / log (1-pInlier^4) <= k
  %
  % 1. Estimate a number of iterations needed to draw an uncontaminated sample
  % of four corresponding point pairs with a probability of 99%
  real_k = log(pFail) / log(1 - pInlier^4);
  k = ceil(real_k);

  best_num_inliers = 0;
  for i=1:k
    % 2. Randomly draw a sample of four corresponding point pairs
    perm = randperm(sz);
    pos = perm(1:4);

    % 3. Estimate the corresponding homography using your get_hom function
    H_est = get_hom(x1(pos)', y1(pos)', x2(pos)', y2(pos)');

    % 4. Test the homography
    transformed_points = apply_homography(H_est, p1);
    projection_error = dist_l2(transformed_points, p2);
    assert(size(projection_error, 2) == sz);

    % closeness criterion for classifying a point as an inlier
    threshold = 5;
    num_inliers = sum(projection_error < threshold);

    if num_inliers > best_num_inliers
      best_num_inliers = num_inliers;
      best_H = H_est;
      % figure(3);
      % clf;
      % plot(x1, y1, 'b.', 'MarkerSize', 30);
      % hold on;
      % plot(x2, y2, 'r.', 'MarkerSize', 30);
      % plot(transformed_points(1, :), transformed_points(2, :), ...
      %   'go', 'MarkerSize', 10, 'LineWidth', 3);
      % TODO(zori): Use all of the inlier points to re-estimate the amount
      % of iterations needed
    end

    % 5. Repeat steps 2-4 for an necessary number of iterations and remember
    % the homography H which has the highest number of inliers ('good' points)
  end

  % 6. Finally, find the optimal solution by re-estimating H with all inliers
  % of the best solution
  transformed_points = apply_homography(best_H, p1);
  projection_error = dist_l2(transformed_points, p2);
  assert(size(projection_error, 2) == sz);
  mask = projection_error < threshold;
  [xi1 yi1] = find_masked_points(p1, mask);
  [xi2 yi2] = find_masked_points(transformed_points, mask);
  
  H = get_hom(xi1', yi1', xi2', yi2');

  % visualization of inliners (use displaymatches)
  % TODO(zori): maybe _really_ use displaymatches
  % figure(4);
  % set(gcf, 'Name', 'Inliers');
  % clf;
  % plot(x1, y1, 'b.', 'MarkerSize', 30);
  % hold on;
  % plot(x2, y2, 'r.', 'MarkerSize', 30);
  % plot(transformed_points(1, :), transformed_points(2, :), ...
  %   'go', 'MarkerSize', 10, 'LineWidth', 3);

end

function [x y] = find_masked_points(points, mask)
  assert(size(points, 1) == 2);
  assert(size(mask, 1) == 1);

  x = points(1,:) .* mask; 
  y = points(2,:) .* mask; 

  x(x == 0) = [];
  y(y == 0) = [];
end
