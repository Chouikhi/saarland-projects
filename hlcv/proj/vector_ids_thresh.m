% Return the indexes of all vectors in pool, whose l2 distance is less than
% thresh.
function ids = vector_ids_thresh(pool, vec, thresh)

  dists = zeros(size(pool, 2), 1);
  for i = 1:size(pool, 2)
    dists(i) = dist_l2(pool(:, i), vec);
  end
  ids = find(dists < thresh);

end
