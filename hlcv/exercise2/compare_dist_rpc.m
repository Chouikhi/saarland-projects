function compare_dist_rpc(model_images, query_images, dist_types, hist_type, num_bins, plot_colors)

  assert(length(plot_colors) == length(dist_types));

  for idx = 1:length(dist_types)
    [best_match, D] = find_best_match(model_images, query_images, dist_types{idx}, hist_type, num_bins);
    plot_rpc(D, plot_colors{idx});
    hold on;
  end

  axis([0, 1, 0, 1]);
  xlabel('1 - precision');
  ylabel('recall');

  legend(dist_types, 'Location', 'Best');
