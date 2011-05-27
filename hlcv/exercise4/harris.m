%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% computes interest points in an image by applying the harris interest
%% point detector
%% 
%%%%%%%%%%%%
%% img    : the image
%% sigma  : the variance of the gaussian derivvative
%% thresh : threshold for determination of interest points
%%
%% px: the x coordinates of the interest points in the image
%% py: the y coordinates of the interest points in the image
%% M : the scores of the harris operator at all pixel
%%
function [px py M] = harris( img, sigma, thresh )
  
  [imgDx imgDy] = gaussderiv(img, sigma);
  imgDx2 = gaussianfilter(sigma^2*imgDx.^2, 1.6*sigma);
  imgDy2 = gaussianfilter(sigma^2*imgDy.^2, 1.6*sigma);
  imgDxy = gaussianfilter(sigma^2*imgDx .* imgDy, 1.6*sigma);

  imgDet   = imgDx2.*imgDy2 - imgDxy.^2;
  imgTrace = imgDx2 + imgDy2;

  M = imgDet - 0.06*imgTrace.^2;
  nmsM = nonmaxsup2d( M );

  [py px] = find(nmsM > thresh);
