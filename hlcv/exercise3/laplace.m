%
% compute Laplacian of the image (see slides of the lecture on 20.04)
%
% note: use the function gaussderiv2.m
%

function imgLap = laplace(img, sigma)

  [imgDxx, imgDxy, imgDyy] = gaussderiv2(img, sigma);

  imgLap = imgDxx + imgDyy;

end

