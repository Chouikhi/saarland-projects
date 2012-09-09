function vecs = gen_rand(mu, sigma, n)

  vecs = randn(n, 2);
  vecs(:, 1) = vecs(:, 1) * sigma(1) + mu(1);
  vecs(:, 2) = vecs(:, 2) * sigma(2) + mu(2);

end
