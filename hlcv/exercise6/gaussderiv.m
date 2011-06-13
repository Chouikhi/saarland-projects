function [imgDx,imgDy]=gaussderiv(img,sigma)

x = [floor(-3.0*sigma+0.5):floor(3.0*sigma+0.5)];
G = gauss(x,sigma);
D = gaussdx(x,sigma);
img1 = conv2(img,D,'same');
imgDx= conv2(img1,G','same');
img2 = conv2(img,G,'same');
imgDy= conv2(img2,D','same');

end
