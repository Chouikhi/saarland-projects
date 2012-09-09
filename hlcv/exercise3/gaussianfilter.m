function imgResult=gaussianfilter(img,sigma)

  G = gauss(sigma);
  imgTmp    = conv2(img,G,'same');
  imgResult = conv2(imgTmp,G','same');

