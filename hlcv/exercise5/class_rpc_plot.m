%
% plot recall/precision curve for the classification task
%
function class_rpc_plot(class_margin, true_labels, plot_color)
  N = length(true_labels)
  predicted_labels = zeros(N, 1);

  for idx = 1:N
    if class_margin(idx) > 0
      predicted_labels(idx) = 1;
    else
      predicted_labels(idx) = -1;
    end
  end

  recognition_rate = sum(true_labels == predicted_labels) / length(true_labels);

  fprintf('overall recognition rate: %f\n', recognition_rate);
  
  nrects = N;

  npos = 0;
  totalpos = sum(true_labels == 1);
  fprintf('total number of positives: %f\n', totalpos);

  [sorted_scores, sortidx] = sort(class_margin, 'descend');
  sorted_labels = true_labels(sortidx);
  [sorted_scores, sorted_labels];
  
  recall = zeros(nrects, 1);
  precision = zeros(nrects, 1);

  for ridx = 1:nrects
    if sorted_labels(ridx) == 1
      npos = npos + 1;
    end

    precision(ridx) = npos / ridx;
    recall(ridx) = npos / totalpos;
  end

  plot(1 - precision, recall, [plot_color '-'], 'LineWidth', 2);
  xlabel('1 - precision');
  ylabel('recall');
  title('precision/recall curve');
  axis([0, 1, 0, 1]);

  hold on;
  plot(linspace(1, 0, 2), linspace(0, 1, 2), ':k');


