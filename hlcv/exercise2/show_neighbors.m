%
% for each image file from 'query_images' find and visualize the 5 nearest images from 'model_image'.
%
% note: use the previously implemented function 'find_best_match.m'
% note: use subplot command to show all the images in the same Matlab figure, one row per query image
%

function show_neighbors(model_images, query_images, dist_type, hist_type, num_bins)
  
  figure(4);
  clf;

  num_nearest = 5;

  % ...
