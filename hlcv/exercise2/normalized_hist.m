%
%  compute histogram of image intensities, histogram should be normalized so that sum of all values equals 1
%  assume that image intensity varies between 0 and 255
%
%  img_gray - input image in grayscale format
%  num_bins - number of bins in the histogram
%
function h = normalized_hist(img_gray, num_bins)

  assert(length(size(img_gray)) == 2, 'image dimension mismatch');
  assert(isfloat(img_gray), 'incorrect image type');

  h = zeros(1, num_bins);
  bin_size = 256 / num_bins;
  % Make the image one dimentional, since we don't really care about
  % pixel coordinates.
  img_serialized = img_gray(:);
  for i = 1:length(img_serialized)
    bin = floor(img_serialized(i) / bin_size) + 1;
    h(bin) = h(bin) + 1;
  end

  h = h / sum(h(:));
end
