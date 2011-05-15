% Use the function find best match.m to compute recognition rate for different combinations of distance and
% histogram functions. The recognition rate is given by a ratio between number of correct matches and total
% number of query images. Experiment with different functions and numbers of histogram bins, try to find
% combination that works best. Submit the summary of your experiments as part of your solution.

function recognition_rate()

  model_images = textread('model.txt', '%s');
  query_images = textread('query.txt', '%s');
  
  fprintf('loaded %d model images\n', length(model_images));
  fprintf('loaded %d query images\n', length(query_images));
  
  dist_types = {'chi2' 'intersect' 'l2'}';
  hist_types = {'grayvalue' 'dxdy' 'rgb' 'rg'}';
  bins_range = [20, 30, 40, 60]';
  
  for bins_i = 1:length(bins_range)
    bins = bins_range(bins_i);
    for hist_i = 1:length(hist_types)
      hist = hist_types{hist_i};
      for dist_i = 1:length(dist_types)
        dist = dist_types{dist_i};

        [best_match, D] = find_best_match(model_images, query_images, ...
                                          dist, hist, bins);
  
        qsz = length(query_images);
        num_correct = sum(best_match == 1:qsz);
        recognition_rate = num_correct / qsz;
  
        fprintf('dist %s, hist %s, bins %d\n', dist, hist, bins);
        fprintf('number of correct matches: %d (recognition rate %f)\n', num_correct, recognition_rate);
      end
    end
  end

end
