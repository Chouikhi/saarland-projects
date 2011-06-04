%
% This function stiches two images related by a homography into one image. 
% The image plane of image 1 is extended to fit the additional points of
% image 2. Intensity values are looked up in image 2, using bilinear
% interpolation (use the provided function interpolate_2d). 
% Further parts belonging to image 1 and image 2 are smoothly blended. 
% 
%
% img1 : the first gray value image
% img2 : the second gray value image 
% H    : the homography estimated between the images
% sz   : the amount of pixel to increase the left image on the right 
% st   : amount of overlap between the images
%
% img  : the final panorama image
% 
function img = pan_sample(img1, img2, H, sz, st)
  [height, width] = size(img1);
  img = zeros([height width + sz]);
  img(1:end, 1:width) = img1;

  % loop over all newly appended pixels plus some overlap (`st')
  for x = width + 1:width + sz
    for y = 1:height
      p_H = apply_homography(H, [x; y]);
      grayval = interpolate_2d(img2, p_H(2), p_H(1));
      img(y, x) = grayval;
    end
  end
  %img = unit8(round(img - 1));
  %img = im2uint8(img);
end
