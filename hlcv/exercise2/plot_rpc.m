% 
% compute and plot the recall/precision curve
%
% D - square matrix, D(i, j) = distance between model image i, and query image j
%
% note: assume that query and model images are in the same order, i.e. correct answer for i-th query image is the i-th model image
%
function plot_rpc(D, plot_color)
  assert(size(D,1) == size(D,2), 'D should be a square matrix');

  precision = [];
  recall = [];
  qsz = size(D, 2);

  for rpc_threshold = 0.0:0.01:0.6
    % compute precision/recall for the current value of threshold, append computed values to "recall" and "precision" vectors
    small_dist = D < rpc_threshold;
    incorrect = ones(qsz, qsz) - eye(qsz);
    small_incorrect = small_dist .* incorrect;
    tp = double(sum(diag(D) < rpc_threshold));
    fp = double(sum(small_incorrect(:)));
    fn = double(sum(diag(D) > rpc_threshold));
    % r = tp / double(tp + fn + 1);
    % p = tp / double(tp + fp + 1);
    if tp + fp == 0
      p = 1;
    else
      p = tp / double(tp + fp);
    end
    if tp + fn == 0
      r = 1;
    else
      r = tp / double(tp + fn);
    end
    precision = [precision p];
    recall = [recall r];
  end

  plot(1-precision, recall, [plot_color '.-'], 'LineWidth', 2, 'MarkerSize', 20);
end
