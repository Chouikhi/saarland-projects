%
% computes the coordinates of potential matching points between two images
% 
% img1 : image 1
% img2 : image 2
%
% px1 the x coordinates of the potential matching points in image 1
% py1 the y coordinates of the potential matching points in image 1
% px2 the x coordinates of the potential matching points in image 2
% py2 the y coordinates of the potential matching points in image 2
%
function [px1, py1, px2, py2] = get_corresponding_points(img1, img2)
    
  %  parameters, tested and working for the example images

  % Harris detector 
  sigma = 2;
  threshold = 3000;

  % dxdy descriptor
  feature_window_size = 50;
  num_bins = 8;
  distanceThreshold = 0.025;

  % a) detection and description
  % Harris detection
  % We are not interested in the scores of the Harris operator at each pixel
  [x1 y1 ] = harris(img1, sigma, threshold);
  [x2 y2 ] = harris(img2, sigma, threshold);

  % discarding points too close to the border
  % consider feature_window_size for determining how much to discard
  %TODO

  % To verify what you have done so far, plot the interest points into the respective images
  figure(1);
  set(gcf, 'Name', 'Interest points detection');
  p1 = make_points(x1, y1);
  p2 = make_points(x2, y2);
  match_plot(img1, img2, p1, p2);

  % computing descriptors using dx-dy histogram
  D1 = dxdy_hist(img1, num_bins);
  D2 = dxdy_hist(img2, num_bins);

  % b) distance computation
  D = get_point_dist(D1, D2);

  % c) best matches, fill function make_points
  [id1, id2, matchedScores] = match_points(D, distanceThreshold, 1);

  % extract pairs from ids
end

% helper routine to make matrix of pairs of (x y) point coordinates out
% of two separate x y arrays
function points = make_points(x, y)
  assert(length(x) == length(y), 'arrays dimensions should match');

  points = [];
  for i = 1:length(x)
    points = [points; x(i) y(i)];
  end
end
 




   

