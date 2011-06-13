function [imgDxx,imgDxy,imgDyy]=gaussderiv2(img,sigma)
  
  assert(length(size(img)) == 2, 'expecting 2d grayscale image');

  G = gauss(sigma);
  D = gaussdx(sigma);

  img1 = conv2(img,D,'same');
  imgDx= conv2(img1,G','same');
  img2 = conv2(img,G,'same');
  imgDy= conv2(img2,D','same');

  img3   = conv2(imgDx,D,'same');
  imgDxx = conv2(img3,G','same');
  img4   = conv2(imgDx,G,'same');
  imgDxy = conv2(img4,D','same');
  img5   = conv2(imgDy,G,'same');
  imgDyy = conv2(img5,D','same');


