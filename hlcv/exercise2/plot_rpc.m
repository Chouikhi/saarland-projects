% 
% compute and plot the recall/precision curve
%
% D - square matrix, D(i, j) = distance between model image i, and query image j
%
% note: assume that query and model images are in the same order, i.e. correct answer for i-th query image is the i-th model image
%
function plot_rpc(D, plot_color)

  recall = [];
  precision = [];
  total_imgs = size(D, 2);

  for rpc_threshold = 0.0:0.01:0.6

    % compute precision/recall for the current value of threshold, append computed values to "recall" and "precision" vectors
    % ... 

  end

  plot(1-precision, recall, [plot_color '.-'], 'LineWidth', 2, 'MarkerSize', 20);
