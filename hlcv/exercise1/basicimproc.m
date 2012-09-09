function outimg = basicimproc(inimg, x, y, sz, rotationangle, filtersize)
  if ndims(inimg) == 2
    incolimg = zeros([size(inimg), 3]);
    for col = 1:3
      incolimg(:,:,col) = inimg;
    end
  elseif ndims(inimg) == 3 && size(inimg, 3) == 3
    incolimg = inimg;
  end

  display(size(incolimg));
  
  if max(incolimg(:)) > 1.0
    incolimg = double(double(incolimg) ./ double(255));
  end
  class(incolimg)

  cropped = incolimg(x:x+sz-1, y:y+sz-1, :);
  rotated = imrotate(cropped, rotationangle, 'bicubic');

  filter2D = double(ones(filtersize, filtersize)) ./ (filtersize ^ 2);
  filter2D = double(filter2D);
  blurred = zeros(size(rotated));
  for col = 1:3
    blurred(:, :, col) = conv2(rotated(:, :, col), filter2D, 'same');
  end
  outimg = blurred;
  % display(any((outimg > 1)(:)));
  % display(any((outimg < 0)(:)));
  outimg(outimg > 1) = double(1);
  outimg(outimg < 0) = double(0);
end
