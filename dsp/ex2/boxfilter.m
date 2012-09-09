% Tutorial 2 Exercise 1 Subtask 1.2
% Iskren Ivov Chernev s9ischer@stud.uni-saarland.de

function filtered = boxfilter(img_gray, h, w)

  ms = max(floor([h, w] ./ 2));
  mir = imgmirror(img_gray, ms);
  filt_big = i_conv2(mir, ones(h, w) ./ (h*w));
  filtered = filt_big(ms+1:end-ms, ms+1:end-ms);

end

function convd = i_conv2(img, kern)

  convd = uint8(zeros(size(img)));
  ks = size(kern);
  of = floor(ks ./ 2 + 1);
  for i = 0:size(img, 1) - ks(1)
    for j = 0:size(img, 2) - ks(2)
      tmp = 0;
      % it is faster with a nested loop, than with extracting the
      % submatrix from img and .* by kern, and then summing
      for ii = 1:ks(1)
        for jj = 1:ks(2)
          tmp = tmp + img(i+ii, j+jj) * kern(ii, jj);
        end
      end
      convd(i+of(1), j+of(2)) = uint8(floor(tmp));
    end
  end

end
