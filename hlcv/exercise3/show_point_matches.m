function show_point_matches(point_func, hist_type, img_name1, img_name2, match_params, num_display_matches)

  hist_func = get_hist_by_name(hist_type);

  % load images
  I1 = imread(img_name1);
  if length(size(I1)) > 2
    I1_grayscale = double(rgb2gray(I1));
  else 
    I1_grayscale = double(I1);
  end

  I2 = imread(img_name2);
  if length(size(I2)) > 2
    I2_grayscale = double(rgb2gray(I2));
  else
    I2_grayscale = double(I2);
  end

  % compute points

  fprintf('computing points ... \n');
  [px1, py1] = point_func(I1_grayscale, match_params.point_sigma, match_params.point_threshold);
  fprintf('found %d points\n', size(px1, 1));

  [px2, py2] = point_func(I2_grayscale, match_params.point_sigma, match_params.point_threshold);
  fprintf('found %d points\n', size(px2, 1));

  % compute descriptors
  fprintf('computing descriptors ... \n');

  if is_grayvalue_hist(hist_type);
    D1 = compute_descriptors(hist_func, I1_grayscale, px1, py1, match_params.feature_window_size, match_params.num_bins);
    D2 = compute_descriptors(hist_func, I2_grayscale, px2, py2, match_params.feature_window_size, match_params.num_bins);
  else
    D1 = compute_descriptors(hist_func, double(I1), px1, py1, match_params.feature_window_size, match_params.num_bins);
    D2 = compute_descriptors(hist_func, double(I2), px2, py2, match_params.feature_window_size, match_params.num_bins);
  end

  % find and visualize point matches
  fprintf('finding nearest neighbors\n');
  [Idx, Dist] = findnn_chi2(D1, D2);
  displaymatches(I1, px1, py1, I2, px2, py2, Idx, Dist, num_display_matches);


