function h=dxdy_hist(img_gray, num_bins)

  assert(length(size(img_gray)) == 2, 'image dimension mismatch');
  assert(isfloat(img_gray), 'incorrect image type');

  % compute the first derivatives
  sigma=6.0;  %maxValue of imgDx,imgDy is 33.5420 for sigma 6.0
  [imgDx,imgDy] = gaussderiv(img_gray, sigma);

  %quantize the images to "num_bins" number of values
  range = 68;
  imgDx = imgDx + range/2;
  imgDy = imgDy + range/2;
  find(imgDx < 0)   = 0;
  find(imgDx >= range) = range-1;
  find(imgDy < 0)   = 0;
  find(imgDy >= range) = range-1;

  imgDx = floor(imgDx*(num_bins/range))+1;
  imgDy = floor(imgDy*(num_bins/range))+1;
  
  %define a 2D histogram  with "num_bins^2" number of entries
  h=zeros(num_bins+1,num_bins+1);

  %execute the loop for each pixel in the image, 
  for i=1:size(img_gray,1)
    for j=1:size(img_gray,2)

      %increment a histogram bin which corresponds to the value 
      %of pixel i,j; 
      dx = imgDx(i,j)+1;
      dy = imgDy(i,j)+1;
      
      h(dx,dy) = h(dx,dy)+1;
    end
  end

  %normalize the histogram such that its integral (sum) is equal 1
  h=h/sum(sum(sum(h)));
  h=reshape(h,(num_bins+1)^2,1);

