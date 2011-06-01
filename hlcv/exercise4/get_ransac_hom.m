%
% estimates the homography between two images by using potential 
% matching points between the images. RANSAC is used for computation of
% the homography. Use the function get_hom.m in order to estimate homography
% from point correspondences.
% 
%
% x1   : the x coordinates of the potential matching points in image 1
% y1   : the y coordinates of the potential matching points in image 1
% x2   : the x coordinates of the potential matching points in image 2
% y2   : the y coordinates of the potential matching points in image 2
% img1 : image 1
% img2 : image 2
%
% H    : Best estimated homography relating both input images using RANSAC
% 
function H = get_ransac_hom(x1,y1,x2,y2,img1,img2)

  %% assume a conservative probability for picking a pair of corresponding 
  %% points and estimate the amount of iterations N necessary to pick four
  %% true correspondances with, let's say, 99 percent probability
  
  % example: assume probability of true match : pInlier
  % -> all 4 sample pairs match: pInlier^4
  % find the amount of times k we have to draw in order to find a sample 
  % without outliers.
  % -> (1-pInlier^4)^k <= pFail (fail probability smaller than pFail)
  % -> log (pFail) / log (1-pInlier^4) <= k
  
  % ... 


  % re-estimate H from all inliers: 
  
  % ... 
  
  % visualization of inliners (use displaymatches)

  % ...
end
