function cart = hom2cart(hom)
  %% argument hom = 2D or 3D points in homogeneous coordinates

  assert(size(cart, 1) == 3 || size(cart, 1) == 4);
  cart = hom(1:end - 1) / hom(end);
end
