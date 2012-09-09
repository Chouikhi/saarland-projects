%
% Visualization of the training data, support vectors, and linear SVM decision boundary.
% 
% The linear SVM decision boundary is defined by f(x) = w'*x + w0
%
% parameters:
%
% figidx - figure number 
% X - matrix of training points (one row per point), you can assume that points are 2 dimensional
% y - vector of point labels (+1 or -1)
% model.w and model.w0 are parameters of the decision function
% model.alpha is a vector of Lagrange multipliers (see slides 32 - 38 in cv-ss11-0601-hog-part.pdf)

function vis_svm(figidx, X, y, model)
  assert(size(X, 2) == 2, 'the points are said to be 2D');

  figure(figidx);
  clf;

  % visualize positive and negative points 
  pos = X(y == 1, :);
  neg = X(y == -1, :);
  plot(pos(:, 1), pos(:, 2), 'r.', 'MarkerSize', 20);
  hold on;
  plot(neg(:, 1), neg(:, 2), 'b.', 'MarkerSize', 20);
  
  % visualize support vectors
  [vals inds] = sort(model.alpha, 'descend');
  svi = inds(1:model.nsv);
  plot(X(svi, 1), X(svi, 2), 'mo', 'MarkerSize', 7, 'LineWidth', 3); 

  min_x1 = min(X(:, 1)) .* 1.5;
  max_x1 = max(X(:, 1)) .* 1.5;
  min_x2 = min(X(:, 2)) .* 1.5;
  max_x2 = max(X(:, 2)) .* 1.5;

  % visualze decision boundary 
  assert(size(model.w, 1) == 2);
  x = [min_x1, max_x1];
  d = 1 / sqrt(sum(model.w .^ 2));
  if abs(model.w(2)) > 1e-6
    y1 =  (1 - model.w0 - model.w(1) * x) / model.w(2);
    y2 = (-1 - model.w0 - model.w(1) * x) / model.w(2);
    plot(x, y1, '-r', x, y2, '-r');
  else
    x1 =   (1 - model.w0) / model.w(1);
    x2 = (- 1 - model.w0) / model.w(1);
    plot([x1 x1], [min_x2, max_x2], '-r', ...
        [x2 x2], [min_x2, max_x2], '-r');
  end

  axis equal;
  axis([min_x1, max_x1, min_x2, max_x2]);
  hold off;
end
