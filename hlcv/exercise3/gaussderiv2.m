%
% return 2nd order Gaussian derivatives of the image
% 
% note: use functions gauss.m and gaussdx.m from exercise 1
%

function [imgDxx, imgDxy, imgDyy] = gaussderiv2(img, sigma)
  
  assert(length(size(img)) == 2, 'expecting 2d grayscale image');

  G = gauss(sigma);
  Dx = gaussdx(sigma);
  Dxx = gaussdxx(sigma);

  % Assume Dxx means second derivative in X and gauss in Y, not two first
  % derivatives in X and * in Y
  
  imgDxx_i = conv2(double(img), Dxx, 'same');
  imgDxx = conv2(imgDxx_i, G', 'same');

  imgDxy_i = conv2(double(img), Dx, 'same');
  imgDxy = conv2(imgDxy_i, Dx', 'same');

  imgDyy_i = conv2(double(img), G, 'same');
  imgDyy = conv2(imgDyy_i, Dxx', 'same');

end
