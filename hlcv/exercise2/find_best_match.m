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

  model_hists = compute_histograms(read_images_from_file_list(model_images), ...
    hist_func, hist_isgray, num_bins);
  query_hists = compute_histograms(read_images_from_file_list(query_images), ...
    hist_func, hist_isgray, num_bins);

  % compute distance matrix
  for i = 1:length(model_images)
    for j = 1:length(query_images)
      D(i, j) = dist_func(model_hists{i}, query_hists{j});
    end
  end
  
  % find the best match for each query image
  [a, best_match] = min(D);
end

function image_list = read_images_from_file_list(image_file_names)
  image_list = cell(size(image_file_names));
  for i = 1:length(image_file_names)
    image_list{i} = imread(image_file_names{i});
  end
end


function image_hist = compute_histograms(image_list, hist_func, hist_isgray, num_bins)
  
  assert(iscell(image_list));
  image_hist = cell(size(image_list));

  % compute histogram for each image and add it at the bottom of image_hist
  for i = 1:size(image_list)
    img = image_list{i};
    if (hist_isgray)
      img = rgb2gray(img);
    end
    img = double(img);
    hist = hist_func(img, num_bins);
    image_hist{i} = hist;
  end
end

