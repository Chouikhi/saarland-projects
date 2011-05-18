%
% Harris interest point detector (see slides 25-31 in "cv-ss11-0511-interest-points-v1.pdf")
%
% px - vector of x coordinates of interest points
% py - vector oy y coordinates of interest points
% M - value of the cornerness function computed for every image pixel
%


function [px py M] = harris(img, sigma, thresh)
  
  alpha = 0.06;
  g_sigma = 1.6 * sigma;

  [imgDx, imgDy] = gaussderiv(img, sigma);
  imgDxy = conv2(conv2(img, gaussdx(sigma), 'same'), gaussdx(sigma)', 'same');

  Gsigma = sigma * 1.6;
  GimgDx2 = gaussianfilter(imgDx .^ 2, g_sigma);
  GimgDxy = gaussianfilter(imgDxy, g_sigma);
  GimgDy2 = gaussianfilter(imgDy .^ 2, g_sigma);

  Cdet = sigma .^ 4 .* (GimgDx2 .* GimgDy2 - GimgDxy .^ 2);
  Ctrace2 = sigma .^ 4 .* (GimgDx2 + GimgDy2) .^ 2;
  M = Cdet - alpha * Ctrace2;
  Mm = nonmaxsup2d(M);

  [py, px] = find(Mm > thresh);

end
