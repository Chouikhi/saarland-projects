show_q1 = true;
show_q2 = false;
show_q3 = false; 

%
% Question 1: interest point detection and matching
%
if show_q1
  % read images
  img1 = double(imread('left.jpg'));
  img2 = double(imread('right.jpg'));

  % detection, description and matching
  % contains a) b) c) and d)
  [x1,y1,x2,y2] = get_corresponding_points(img1, img2);
end 

%
% Question 2: fitting the affine transformation
%
if show_q2
  test_get_affine();
end

% Homography Estimation with ransac

if show_q3

  % contains a) and b)
  H = get_ransac_hom(x1,y1,x2,y2,img1,img2);


  %
  % Question 3
  % Panorama Stitching
  %

  % contains a) and b)
  imgRes = pan_sample(img1,img2,H,300,150);

  % display result image
  figure(5);
  clf;
  imshow(imgRes);
end 
