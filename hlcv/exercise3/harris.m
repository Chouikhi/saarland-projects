%
% Harris interest point detector (see slides 25-31 in "cv-ss11-0511-interest-points-v1.pdf")
%
% px - vector of x coordinates of interest points
% py - vector oy y coordinates of interest points
% M - value of the cornerness function computed for every image pixel
%


function [px py M] = harris(img, sigma, thresh)
  
  % ... 
