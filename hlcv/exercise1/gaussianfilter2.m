function imgResult = gaussianfilter(img, sigma)
  function filtered = filterChannel(img, gauss1D)
    filtered = conv2(img, conv2(gauss1D, gauss1D'), 'same');
  end

  if sigma == 0
    imgResult = img;
    return;
  end

  filtSize = 3 * sigma;
  x = -filtSize:filtSize;
  gauss1D = gauss(x, sigma);
  gauss1D = gauss1D ./ sum(gauss1D(:));

  sz = size(img);
  if max(img(:)) > 1.0
    imgDbl = double(img) ./ 255;
  else
    imgDbl = img;
  end

  if ndims(imgDbl) == 2
    imgResult = filterChannel(imgDbl, gauss1D);
  else
    for col = 1:3
      imgResult(:, :, col) = filterChannel(imgDbl(:, :, col), gauss1D);
    end
  end
end
