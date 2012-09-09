function out = filter_sp(img, s, p)
  % some checks to make sure we have the right arguments
  assert(ndims(img) == 2, '2 dimensions expected for grayscale');
  assert(max(img(:)) > 1, 'values between 0 and 255 expected');
  assert(isnumeric(img), 'integer graylevel values expected');

  % create salt / pepper matrixes
  S = randmat(size(img, 1), size(img, 2), s);
  P = randmat(size(img, 1), size(img, 2), p);
  % add salt -- that is 255 where there is salt
  % and the rest is the official image
  tmp = S .* 255 + img .* uint8(S == 0);
  % add pepper -- that is where there is no pepper take the image
  % the rest is 0 by default
  out = tmp .* uint8(P == 0);
end
