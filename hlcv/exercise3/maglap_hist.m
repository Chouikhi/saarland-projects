%
% compute joint histogram of gradient magnitudes and Laplacian
% 
% note: use the functions gradmag.m and laplace.m
%
% note: you can assume that gradient magnitude is in the range [0, 100], 
%       and Laplacian is in the range [-60, 60]
% 

function h = maglap_hist(img_gray, num_bins)
  
  % TODO: assignment says to toggle these for better perf
  gradmax_range = [0 100]; % begin, size
  laplace_range = [-60 120]; % begin, size

  assert(length(size(img_gray)) == 2, 'image dimension mismatch');
  assert(isfloat(img_gray), 'incorrect image type');

  sigma = 2.0;

  %define a 2D histogram  with "num_bins^2" number of entries
  h = zeros(num_bins, num_bins);

  % compute the gradient magnitudes and Laplacian
  mag = gradmag(img_gray, sigma);
  lap = laplace(img_gray, sigma);
  
  % quantize the gradient magnitude and Laplacian to "num_bins" number of values
  mag_b = floor((mag - gradmax_range(1)) .* (num_bins / gradmax_range(2))) + 1;
  lap_b = floor((lap - laplace_range(1)) .* (num_bins / laplace_range(2))) + 1;

  % execute the loop for each pixel in the image, 
  for i = 1:size(img_gray, 1)
    for j = 1:size(img_gray, 2)
      h(mag_b(i, j), lap_b(i, j)) = h(mag_b(i, j), lap_b(i, j)) + 1;
    end
  end

  % normalize the histogram such that its integral (sum) is equal 1
  h = h(:);
  h = h ./ sum(h);

end
