function test_fit_line()

  data = [2.1, 0.425; 2.2, 0.351; 2.4, 0.281; 2.6, 0.288; 2.7, 0.137; ...
          2.8, 0.163; 2.9, 0.084; 3.0, 0.047; 3.1, 0.013; 3.3, -0.048; ...
          3.5, -0.099; 3.7, -0.142];
  xs = data(:, 1);
  ys = data(:, 2);
  [a, b] = fit_line(xs, ys);
  
  xmin = min(xs);
  xmax = max(xs);
  
  plot(xs, ys, 'x');
  hold on;
  plot([xmin, xmax], [a * xmin + b, a * xmax + b], '-g');
  hold off;

end
