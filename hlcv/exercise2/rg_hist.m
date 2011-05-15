%
%  compute joint histogram for r/g values (see slide 25 in the cv-ss11-0504-instance-v1.pdf)
%  note that r/g values should be in the range [0, 1];
%  histogram should be normalized so that sum of all values equals 1
%
%  img_color - input color image
%  num_bins - number of bins used to discretize each dimension, total number of bins in the histogram should be num_bins^2
%

function h = rg_hist(img_color, num_bins)

  assert(size(img_color, 3) == 3, 'image dimension mismatch');
  assert(isfloat(img_color), 'incorrect image type');
  
  bin_size = 256 / num_bins;
  %define a 2D histogram  with "num_bins^2" number of entries
  h=zeros(num_bins, num_bins);

  for i=1:size(img_color,1)
    for j=1:size(img_color,2)

      %increment a histogram bin which corresponds to the value of pixel i,j; h(R,G,B)
      br = img_color(i, j, 1);
      bg = img_color(i, j, 2);
      bb = img_color(i, j, 3);
      bs = br + bg + bb;
      brr = br / bs; bgg = bg / bs;

      % Another sad day for short code. This is slower ...
      % c = img_color(i, j, :);
      % sc = sum(c(:));
      % brr = c(1) / sc; bgg = c(2) / sc;
      % End of slow code.

      brx = floor(brr * num_bins) + 1;
      bgx = floor(bgg * num_bins) + 1;
      brx = bound(brx, 1, num_bins);
      bgx = bound(bgx, 1, num_bins);
      h(brx, bgx) = h(brx, bgx) + 1;
    end
  end

  %normalize the histogram such that its integral (sum) is equal 1
  h = h / sum(h(:));

  h = h(:);

end

function y = bound(x, lb, ub)
  if x < lb
    y = lb;
  elseif x > ub
    y = ub;
  else
    y = x;
  end
end
