function [imgDx,imgDy]=gaussderiv(img,sigma)
  D = gaussdx(-3*sigma:3*sigma, sigma);
  if ndims(img) == 3
    img = rgb2gray(img);
  end
  if max(img(:)) > 1
    img = double(img) ./ 255;
  end
  imgDx = conv2(img, D, 'same');
  imgDy = conv2(img, D', 'same');
end
