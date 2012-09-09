show_q1 = false;
show_q2 = false;
show_q3 = true;

x1 = [];
y1 = [];
x2 = [];
y2 = [];

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
  save interest_points x1 y1 x2 y2 img1 img2;
end

%
% Question 2: fitting the affine transformation
%
if show_q2
  test_get_affine();
end

% Homography Estimation with ransac

if show_q3
  if length(x1) == 0
    load interest_points;
  end

  % contains a) and b)
  H = get_ransac_hom(x1,y1,x2,y2,img1,img2);

  %
  % Question 3
  % Panorama Stitching
  %

  % contains a) and b)
  imgRes = pan_sample(img1, img2, H, 300, 150);
  %imgRes = pan_sample(img1,img2,H,30,150);

  % display result image
  figure(5);
  clf;
  % NOTE(zori): without this - no work :)
  colormap gray;
  imshow(uint8(imgRes));
  % imagesc(uint8(imgRes));
end 
