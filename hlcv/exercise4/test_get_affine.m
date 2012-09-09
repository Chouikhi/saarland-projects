function test_get_affine

  N = 10;

  rand('seed', 1);
  randn('seed', 1);

  x = rand(N, 1);
  y = rand(N, 1);

  % 
  % transform points
  %

  % translation
  T = [1, 0, 1.5;
       0, 1, 0; 
       0, 0, 1];

  % scale 
  S = [0.5, 0, 0;
       0, 0.5, 0; 
       0, 0, 1];

  % rotation
  theta = pi/6;
  c = cos(theta);
  s = sin(theta);
  R = [c, -s, 0;
       s, c, 0;
       0, 0, 1];

  M = T*S*R;

  fprintf('affine transformation: \n');
  disp(M);

  p1 = [x'; 
       y'; 
       ones(1, N)];

  p2 = M*p1;

  %
  % add noise to projected points
  %
  noise_std =  0.01;
  fprintf('adding Gaussian noise with standard deviation: %.2f\n\n', noise_std);

  p2(1:2, 1:N) = p2(1:2, 1:N) + noise_std*randn(2, N);  

  figure(1);
  clf;
  plot(p1(1, :), p1(2, :), 'b.', 'MarkerSize', 30);
  hold on;
  plot(p2(1, :), p2(2, :), 'r.', 'MarkerSize', 30);

  %
  % recover affine transformation
  %
  M2 = get_affine(p1, p2);

  p3 = M2 * p1;
  plot(p3(1, :), p3(2, :), 'go', 'MarkerSize', 10, 'LineWidth', 3);
 
  fprintf('recovered affine transformation: \n');
  disp(M2);
end
