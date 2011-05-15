function h=rg_hist(img_color, num_bins)

  assert(size(img_color, 3) == 3, 'image dimension mismatch');
  assert(isfloat(img_color), 'incorrect image type');
  
  sumRGB=sum(img_color,3);
  size(img_color);
  size(sumRGB);

  %define a 3D histogram  with "num_bins^3" number of entries
  h=zeros(num_bins, num_bins);

  %execute the loop for each pixel in the image, 
  for i=1:size(img_color, 1)
    for j=1:size(img_color, 2)

      %increment a histogram bin which corresponds to the value of pixel i,j; h(R,G,B)

      %  R=floor(img_color(i,j,1)/sumRGB(i,j)*num_bins)+1;
      %  G=floor(img_color(i,j,2)/sumRGB(i,j)*num_bins)+1;
      R=floor(img_color(i,j,1)/sumRGB(i,j)*(num_bins-1))+1;
      G=floor(img_color(i,j,2)/sumRGB(i,j)*(num_bins-1))+1;

      %B=img_color(i,j,3);

      %h(R,G,B)=h(R,G,B)+1;
      h(R,G)=h(R,G)+1;
    end
  end
  
  %normalize the histogram such that its integral (sum) is equal 1
  h=h / sum(sum(sum(h)));
  h=reshape(h,num_bins^2,1);
