function [imgMag,imgDir]=gradmag(img,sigma)

  [imgDx,imgDy] = gaussderiv(img,sigma);
  imgMag = sqrt((imgDx.^2) + (imgDy.^2));
  imgDir = atan2(imgDy,imgDx);


