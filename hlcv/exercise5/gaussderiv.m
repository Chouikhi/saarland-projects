function [imgDx,imgDy]=gaussderiv(img,sigma)

  G = gauss(sigma);
  D = gaussdx(sigma);
  img1 = conv2(img,D,'same');
  imgDx= conv2(img1,G','same');
  img2 = conv2(img,G,'same');
  imgDy= conv2(img2,D','same');
