function cart = hom2cart(hom)
  %% argument hom = 2D or 3D points in homogeneous coordinates

  assert(size(hom, 1) == 3 || size(hom, 1) == 4);
  sz = size(hom, 2);
  cart = [];
  xx = hom(1:end - 1, :);
  for i=1:sz
    cart = [cart xx(:, i) / hom(end, i)];
  end
end
