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

function vis_svm(X, y, model)
  assert(size(X, 2) == 2, 'the points are said to be 2D');

  figure(figidx);
  clf;

  keyboard;
  % visualize positive and negative points 
  % plot(x1, y1, 'b.', 'MarkerSize', 30);
  % hold on;
  % plot(x2, y2, 'r.', 'MarkerSize', 30);
  % plot(transformed_points(1, :), transformed_points(2, :), ...
  %   'go', 'MarkerSize', 10, 'LineWidth', 3);
  
  % visualize support vectors (see slide 60 in cv-ss09-0603-hog-svm-v0.pdf)
  % ...

  % visualze decision boundary 
  % ...

  min_x1 = min(X(:, 1));
  max_x1 = max(X(:, 1));
  min_x2 = min(X(:, 2));
  max_x2 = max(X(:, 2));

  axis equal;
  axis([1.5 * min_x1, 1.5 * max_x1, 1.5 * min_x2, 1.5 * max_x2]);
  hold off;
end
