function hom = cart2hom ( cart )
  %% argument hom = 2D or 3D points in cartesian coordinates

  assert(size(cart, 1) == 2 || size(cart, 1) == 3);
  hom = [cart; ones(1, size(cart, 2))];

