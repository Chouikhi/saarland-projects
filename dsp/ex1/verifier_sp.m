function [s, p] = verifier_sp(img)
  white = sum(img(:) == 255);
  black = sum(img(:) == 0);
  pixels = prod(size(img));
  % percent black pixels == pepper ratio
  p = double(black) / pixels;
  % salt ratio percent white pixels, (1 - p) is because part of the white
  % pixels will become black after the pepper is applied
  s = (double(white) / pixels) / (1 - p);
end
