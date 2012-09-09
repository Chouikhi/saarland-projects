% Tutorial 2 Exercise 2 Subtask 2.2
% Iskren Ivov Chernev s9ischer@stud.uni-saarland.de
%
% segmented_histogram -- computes a histogram of an image by first divinding it
%   into seg_x . seg_y segments and calculating the histogram of each segment
%   for each color, concatenating all histograms and returning that
%   
%   img_color -- the image which histogram is to be computed
%   num_bins -- number of bins for each individual segment
%   seg_x -- number of segments in the x direction
%   seg_y -- number of segments in the y direction

function h = segmented_histogram(img_color, num_bins, seg_x, seg_y)

  assert(ndims(img_color) == 3 && size(img_color, 3) == 3, ...
      'segmented_histogram expects color image');

  h = zeros(1, seg_x * seg_y * 3 * num_bins);
  nextid = 1;
  coefx = size(img_color, 2) / seg_x;
  coefy = size(img_color, 1) / seg_y;
  for c = 1:3
    for xi = 0:seg_x-1
      for yi = 0:seg_y-1
        xlb = floor(xi * coefx); xub = floor((xi + 1) * coefx);
        ylb = floor(xi * coefy); yub = floor((xi + 1) * coefy);
        img_segment = img_color(ylb+1:yub, xlb+1:xub, c);
        assert(ndims(img_segment) == 2, 'something went wrong');
        sh = histogram(img_segment, num_bins);
        h(nextid:nextid + num_bins - 1) = sh(:);
        nextid = nextid + num_bins;
      end
    end
  end

  h = h ./ sum(h);

end
