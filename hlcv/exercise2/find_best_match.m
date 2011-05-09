%
% model_images - list of file names of model images
% query_images - list of file names of query images
%
% dist_type - string which specifies distance type:  'chi2', 'l2', 'intersect'
% hist_type - string which specifies histogram type:  'grayvalue', 'dxdy', 'rgb', 'rg'
%
% note: use functions 'get_dist_by_name.m', 'get_hist_by_name.m' and 'is_grayvalue_hist.m' to obtain 
%       handles to distance and histogram functions, and to find out whether histogram function 
%       expects grayvalue or color image
%

function [best_match, D] = find_best_match(model_images, query_images, dist_type, hist_type, num_bins)

  dist_func = get_dist_by_name(dist_type);
  hist_func = get_hist_by_name(hist_type);
  hist_isgray = is_grayvalue_hist(hist_type);

  D = zeros(length(model_images), length(query_images));

  % compute distance matrix
  % ...
  
  % find the best match for each query image
  % ...


function image_hist = compute_histograms(image_list, hist_func, hist_isgray, num_bins)
  
  assert(iscell(image_list));
  image_hist = [];

  % compute hisgoram for each image and add it at the bottom of image_hist
  % ...

