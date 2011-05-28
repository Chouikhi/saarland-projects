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
  fprintf('harris 1\n');
  [x1 y1 ] = harris(img1, sigma, threshold);
  fprintf('harris 2\n');
  [x2 y2 ] = harris(img2, sigma, threshold);
  fprintf('harris end\n');

  % discarding points too close to the border
  % consider feature_window_size for determining how much to discard

  % TODO(zori): trimming does not seem to work nicely; is it the scaling done
  % before displaying that ruins the symmetry of point distribution?
  % NOTE(zori): the maxx is the second dimension of the image and maxy - the first
  [maxy1 maxx1] = size(img1);
  [maxy2 maxx2] = size(img2);
  fprintf('trimming borders . . .\n');
  [x1 y1] = trim_border(x1, y1, maxx1, maxy1, floor(feature_window_size / 2));
  [x2 y2] = trim_border(x2, y2, maxx2, maxy2, floor(feature_window_size / 2));
  fprintf('trimming borders done\n');

  % To verify what you have done so far, plot the interest points into the respective images
  % figure(1);
  % set(gcf, 'Name', 'Interest points detection');
  % p1 = make_points(x1, y1);
  % p2 = make_points(x2, y2);
  % match_plot(img1, img2, p1, p2);

  % computing descriptors using dx-dy histogram
  fprintf('computing descriptors . . .\n');
  D1 = compute_descriptors(@dxdy_hist, img1, x1, y1, feature_window_size, num_bins);
  D2 = compute_descriptors(@dxdy_hist, img2, x2, y2, feature_window_size, num_bins);
  fprintf('computing descriptors done\n');

  % b) distance computation
  fprintf('get point dist . . .\n');
  D = get_point_dist(D1, D2);
  fprintf('get point dist done\n');

  % c) best matches, fill function match_points
  fprintf('match points . . .\n');
  [id1, id2, matchedScores] = match_points(D, distanceThreshold, 1);
  fprintf('match points done\n');

  % extract pairs from ids
  assert(isequal(size(id1), size(id2)));
  px1 = x1(id1);
  py1 = y1(id1);
  px2 = x2(id2);
  py2 = y2(id2);

  figure(2);
  set(gcf, 'Name', 'Correspondences found');
  % N = 15;
  % match_plot(img1, img2, make_points(px1(1:N), py1(1:N)), make_points(px2(1:N), py2(1:N)));
  % NOTE: WTF am I supposed to come up with Idx and Dist?
  Idx = 1:length(px2);
  Dist = matchedScores;
  displaymatches(img1, px1', py1', img2, px2', py2', Idx, Dist, 50);
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

% helper routine to remove points in the border of the image
% points matrix is of the form
% [x1 x2 x3
%  y1 y2 y3]

function [xx, yy] = trim_border(xs, ys, maxx, maxy, border_size)
  assert(isequal(size(xs), size(ys)), 'points dimensions should agree');
  fprintf('trim points near border; %d %d %d\n', maxx, maxy, border_size);
  xx = [];
  yy = [];
  for i = 1:size(xs)
    x = xs(i);
    y = ys(i);
    % TODO(zori): style issue: where should the sign be
    % when using multiple lines for an expression?
    if  border_size < x && x < maxx - border_size && ...
        border_size < y && y < maxy - border_size
      xx = [xx x];
      yy = [yy y];
    end
  end
end
