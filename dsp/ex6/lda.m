function [fi, lambda] = lda(vecs)

  K = size(vecs, 1);
  Nk = zeros(K, 1);
  means = zeros(K, 2);
  for k = 1:K
    means(k, :) = mean(vecs{k});
    Nk(k) = size(vecs{k}, 1);
  end
  N = sum(Nk);
  tmean = mean(means);

  Sb = zeros(2, 2);
  for k = 1:K
    v = means(k, :);
    v = v - tmean;
    Sb = Sb + (v' * v) .* Nk(k);
  end
  Sb = Sb ./ N;


  Sw = zeros(2, 2);
  for k = 1:K
    wcc = cov(vecs{k}, 1) * Nk(k);
    Sw = Sw + wcc;
  end
  Sw = Sw ./ N;

  [V, D] = eig(Sb, Sw);
  lambda = diag(D);
  [lambda, idx] = sort(lambda, 'descend');
  fi = V(:, idx);

end
