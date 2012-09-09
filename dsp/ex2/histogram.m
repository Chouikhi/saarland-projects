% Tutorial 2 Exercise 2 Subtask 2.1
% Iskren Ivov Chernev s9ischer@stud.uni-saarland.de

function h = histogram(img_gray, num_bins)

  assert(ndims(img_gray) == 2, 'histogram expects graylevel image');

  h = zeros(1, num_bins);
  % multiply each graylevel value by this to get the histogram bin id
  coef = double(num_bins) / 256.0;
  
  for i = 1:size(img_gray, 1)
    for j = 1:size(img_gray, 2)
      bin = uint8(floor(double(img_gray(i, j)) * coef)) + 1;
      if bin > num_bins
        keyboard;
      end
      h(bin) = h(bin) + 1;
    end
  end

end
