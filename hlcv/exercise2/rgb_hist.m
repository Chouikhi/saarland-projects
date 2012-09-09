%
%  compute joint histogram for each color channel in the image, histogram should be normalized so that sum of all values equals 1
%  assume that values in each channel vary between 0 and 255
%
%  img_color - input color image
%  num_bins - number of bins used to discretize each channel, total number of bins in the histogram should be num_bins^3
%

function h=rgb_hist(img_color, num_bins)

  assert(size(img_color, 3) == 3, 'image dimension mismatch');
  assert(isfloat(img_color), 'incorrect image type');

  %define a 3D histogram  with "num_bins^3" number of entries
  h = zeros(num_bins, num_bins, num_bins);
  
  %quantize the image to "num_bins" number of values
  bin_size = 256 / num_bins;

  %execute the loop for each pixel in the image 
  for i=1:size(img_color, 1)
    for j=1:size(img_color, 2)

      %increment a histogram bin which corresponds to the value of pixel i,j; h(R,G,B)
      br = floor(img_color(i, j, 1) / bin_size) + 1;
      bg = floor(img_color(i, j, 2) / bin_size) + 1;
      bb = floor(img_color(i, j, 3) / bin_size) + 1;
      h(br, bg, bb) = h(br, bg, bb) + 1;

      % Hm, this one is many times slower for some reason.
      % bc = floor(squeeze(img_color(i, j, :)) / bin_size) + 1;
      % h(bc(1), bc(2), bc(3)) = h(bc(1), bc(2), bc(3)) + 1;
      % End of slow code.
    end
  end

  %normalize the histogram such that its integral (sum) is equal 1
  h = h / sum(h(:));

  % make sure it is 1D
  h = h(:);

end
