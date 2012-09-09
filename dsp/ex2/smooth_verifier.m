% Tutorial 2 Exercise 1 Subtask 1.3
% Iskren Ivov Chernev s9ischer@stud.uni-saarland.de
%
% smooth_verifier
%  img_name -- name of image to experiment with
%  s -- percent salt [0, 1]
%  p -- percent pepper [0, 1]
%  smooth -- size of box filter (smooth x smooth)

function smooth_verifier(img_name, s, p, smooth)
  img = imread(img_name);
  if ndims(img) == 3
    img = rgb2gray(img);
  end
  noisy = filter_sp(img, s, p);
  filtered = boxfilter(noisy, smooth, smooth);

  subplot(1, 3, 1); imshow(img);
  subplot(1, 3, 2); imshow(noisy);
  subplot(1, 3, 3); imshow(filtered);
end
