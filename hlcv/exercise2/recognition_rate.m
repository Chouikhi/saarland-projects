% Use the function find best match.m to compute recognition rate for different combinations of distance and
% histogram functions. The recognition rate is given by a ratio between number of correct matches and total
% number of query images. Experiment with different functions and numbers of histogram bins, try to find
% combination that works best. Submit the summary of your experiments as part of your solution.

model_images = textread('model.txt', '%s');
query_images = textread('query.txt', '%s');

fprintf('loaded %d model images\n', length(model_images));
fprintf('loaded %d query images\n', length(query_images));

dist_types = {'chi2' 'intersect' 'l2'};
hist_types = {'grayvalue' 'dxdy' 'rgb' 'rg'};
bins_range = 20:5:40

for dist in dist_types
  for hist in hist_types
    for bins in bins_range
      [best_match, D] = find_best_match(model_images, query_images, ...
                                        dist, hist, bins);

      qsz = length(query_images);
      num_correct = sum(best_match == 1:qsz);
      recognition_rate = num_correct / qsz;

      % TODO(zori): collect the results in a table and store in a file
      fprintf('dist %s, hist %s, bins %d\n', dist, hist, bins);
      fprintf('number of correct matches: %d (recognition rate %f)\n', num_correct, recognition_rate);
    end
  end
end
