%
%  compute joint histogram of Gaussian partial derivatives of the image in x and y direction
%  for sigma = 6.0, the range of derivatives is approximately [-34, 34]
%  histogram should be normalized so that sum of all values equals 1
%
%  img_gray - input grayvalue image
%  num_bins - number of bins used to discretize each dimension, total number of bins in the histogram should be num_bins^2
%
%  note: you can use the provided function gaussderiv.m to compute Gaussian derivatives
%

function h=dxdy_hist(img_gray, num_bins)

  assert(length(size(img_gray)) == 2, 'image dimension mismatch');
  assert(isfloat(img_gray), 'incorrect image type');
  assert(max(img_gray(:)) - min(img_gray(:)) > 1, 'image pixel data should be in [0, 255]');


  % compute the first derivatives
  sigma = 6;  % I like this number!
  [imgDx, imgDy] = gaussderiv(img_gray, sigma);

  d_min = -34; d_max = 34;
  d_c = double(num_bins) / (d_max - d_min) + eps;

  % Prepare derivatives so they show the exact bin.
  imgDx = floor((imgDx - d_min) .* d_c) + 1;
  imgDy = floor((imgDy - d_min) .* d_c) + 1;
  % If teoretically imgDx,Dy are in [1, num_bins], then in practice it is never like that.
  imgDx(imgDx > num_bins) = num_bins;
  imgDx(imgDx < 1) = 1;
  imgDy(imgDy > num_bins) = num_bins;
  imgDy(imgDy < 1) = 1;

  h = zeros(num_bins, num_bins);
  for i = 1:size(img_gray, 1)
    for j = 1:size(img_gray, 2)
      x = imgDx(i, j);
      y = imgDy(i, j);
      h(x, y) = h(x, y) + 1;
    end
  end

  h = h ./ sum(h(:));
  h = h(:);
end
