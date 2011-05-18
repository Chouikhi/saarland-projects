%
% compute image descriptors corresponding to the list of interest points 
% 
% D - matrix with number of rows equal to number of interest points, each row should be equal to the corresponding descriptor vector
% px, py - vectors of x/y coordinates of interest points
% desc_size - size of the region around interest point which is used to compute the descriptor
% desc_func - handle of the corresponding descriptor function (i.e. @rg_hist)
% num_bins - number of bins parameter of the descriptor function 
%

function D = compute_descriptors(hist_func, img, px, py, desc_size, num_bins)
  
  assert(length(px) == length(py), 'error in POI description (wrong dimension)');

  desc_radius = round((desc_size-1)/2);
  % TODO: calculate second dimention in a better way
  D = zeros(length(px), num_bins .^ 2);

  fprintf('desc size %d\n', desc_size);
  for i = 1:length(px)
    % cropped = uint8(zeros(desc_size, desc_size, 3));
    % yet another win for conventional languages
    lbx = floor(px(i) - desc_size / 2 + 0.5); ubx = lbx + desc_size - 1;
    [x1i, x1d] = fl(lbx); [x2i, x2d] = fu(ubx, size(img, 2), desc_size);
    lby = floor(py(i) - desc_size / 2 + 0.5); uby = lby + desc_size - 1;
    [y1i, y1d] = fl(lby); [y2i, y2d] = fu(uby, size(img, 1), desc_size);
    % fprintf('%d %d\n', lbx, lby);
    % fprintf('%d-%d %d-%d  %d-%d %d-%d\n', x1i, x2i, y1i, y2i, x1d, x2d, y1d, y2d);
    
    % cropped(y1d:y2d, x1d:x2d, :) = img(y1i:y2i, x1i:x2i, :);
    if ndims(img) == 3
      cropped = img(y1i:y2i, x1i:x2i, :);
    else
      cropped = img(y1i:y2i, x1i:x2i);
    end

    hist = hist_func(cropped, num_bins);
    D(i, :) = hist';
  end

end

function [li, ld] = fl(c)
  if c >= 1  
    li = c;
    ld = 1;
  else
    li = 1;
    ld = 1 + (1-c);
  end
end

function [ui, ud] = fu(c, mxi, mxd)
  if c <= mxi
    ui = c;
    ud = mxd;
  else
    ui = mxi;
    ud = mxd - (c - mxi);
  end
end

