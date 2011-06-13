function [px py, H]=hessian(img, sigma, thresh)

  [imgDxx,imgDxy,imgDyy] = gaussderiv2(img,sigma);

  H = sigma^4 * (imgDxx.*imgDyy-imgDxy.^2);
  %H = imgDxx.*imgDyy-imgDxy.^2;

  nmsH = nonmaxsup2d( H );

  [py px] = find(nmsH > thresh);
