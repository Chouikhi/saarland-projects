function imgResult=gaussianfilter(img,sigma)

x = [floor(-3.0*sigma+0.5):floor(3.0*sigma+0.5)];
G = gauss(x,sigma);
imgTmp    = conv2(img,G,'same');
imgResult = conv2(imgTmp,G','same');

end
