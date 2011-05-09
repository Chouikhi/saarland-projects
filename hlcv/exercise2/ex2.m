
show_part1 = true;
show_part2 = true;
show_part3 = true;
show_part4 = true;

if show_part1

  %
  % gray-value histograms
  %

  img_color = imread('./model/obj100__0.png');
  img_gray = double(rgb2gray(img_color));

  figure(1);
  clf;
  subplot(1, 3, 1);
  imagesc(img_color);

  subplot(1, 3, 2);

  num_bins_gray = 40;
  hist_gray1 = hist(img_gray(:), num_bins_gray);
  bar(hist_gray1);

  subplot(1, 3, 3);
  hist_gray2 = normalized_hist(img_gray, num_bins_gray);
  bar(hist_gray2);

  %
  % color histograms
  %
  figure(2);
  clf;
  subplot(1, 2, 1);
  imagesc(img_color);

  num_bins_color = 5;
  subplot(1, 2, 2);
  hist_rgb1 = rgb_hist(double(img_color), num_bins_color);
  bar(hist_rgb1);

end

%
% distance functions
% 

if show_part2
  image_files1 = {'./model/obj1__0.png'};
  image_files2 = {'./model/obj91__0.png', './model/obj94__0.png'};

  figure(3);
  clf;
  subplot(1, 3, 1); imagesc(imread(image_files1{1}));
  subplot(1, 3, 2); imagesc(imread(image_files2{1}));
  subplot(1, 3, 3); imagesc(imread(image_files2{2}));

  fprintf('distance functions: \n');
  distance_types = {'l2', 'intersect', 'chi2'}

  fprintf('histogram types: \n');
  hist_types = {'grayvalue', 'rgb', 'rg', 'dxdy'}

  num_bins_color = 30;
  num_bins_gray = 90;

  for imgidx1 = 1:length(image_files1)
    img1_color = imread(image_files1{imgidx1});
    img1_gray = double(rgb2gray(img1_color));
    img1_color = double(img1_color);

    for imgidx2 = 1:length(image_files2) 
      img2_color = imread(image_files2{imgidx2});
      img2_gray = double(rgb2gray(img2_color));
      img2_color = double(img2_color);

      D = zeros(length(distance_types), length(hist_types));

      for didx = 1:length(distance_types)

        dist_func = get_dist_by_name(distance_types{didx});

        for hidx = 1:length(hist_types)

          hist_func = get_hist_by_name(hist_types{hidx});

          if is_grayvalue_hist(hist_types{hidx})
            D(didx, hidx) = dist_func(hist_func(img1_gray, num_bins_gray), hist_func(img2_gray, num_bins_gray));
          else
            D(didx, hidx) = dist_func(hist_func(img1_color, num_bins_color), hist_func(img2_color, num_bins_color));
          end
        end
      end
      
      fprintf('compare image "%s" to "%s": \n', image_files1{imgidx1}, image_files2{imgidx2});
      D

    end
  end

end


%
% evaluate recognition performance 
%

if show_part3

  model_images = textread('model.txt', '%s');
  query_images = textread('query.txt', '%s');

  fprintf('loaded %d model images\n', length(model_images));
  fprintf('loaded %d query images\n', length(query_images));

  eval_dist_type = 'intersect';
  eval_hist_type = 'rg';
  eval_num_bins = 30;

  %
  % visualize nearest neighbors
  %

  query_images_vis = query_images([1, 5, 10]);
  show_neighbors(model_images, query_images_vis, eval_dist_type, eval_hist_type, eval_num_bins);


  %
  % compute recognition percentage
  %

  [best_match, D] = find_best_match(model_images, query_images, ...
                                    eval_dist_type, eval_hist_type, eval_num_bins);

  num_correct = sum(best_match == 1:length(query_images));
  fprintf('number of correct matches: %d (%f)\n', num_correct, num_correct / length(query_images));

end

if show_part4
  % 
  % plot recall_precision curves
  % 

  figure(5);
  clf;
  compare_dist_rpc(model_images, query_images, ...
                   {'chi2', 'intersect', 'l2'}, ...
                   'rg', eval_num_bins, {'r', 'g', 'b'});

  figure(6);
  clf;
  compare_dist_rpc(model_images, query_images, ...
                   {'chi2', 'intersect', 'l2'}, ...
                   'rgb', eval_num_bins / 2, {'r', 'g', 'b'});

  figure(7);
  clf;
  compare_dist_rpc(model_images, query_images, ...
                   {'chi2', 'intersect', 'l2'}, ...
                   'dxdy', eval_num_bins, {'r', 'g', 'b'});
end


