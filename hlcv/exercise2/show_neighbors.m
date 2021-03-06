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

  [best_match, D] = find_best_match(model_images, query_images, dist_type, hist_type, num_bins);
  [vals, inds] = sort(D);
  qsz = length(query_images);

  for i = 1:qsz
    subplot(qsz, num_nearest + 1, (i - 1) * (num_nearest + 1) + 1);
    imagesc(imread(query_images{i}));
    for j = 1:num_nearest
      subplot(qsz, num_nearest + 1, (i - 1) * (num_nearest + 1) + j + 1);
      imagesc(imread(model_images{inds(j, i)}));
    end
  end
end
