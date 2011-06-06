%
% Compute gradient orientation histogram for one cell of the HOG descriptor.
% Each pixel should contribute to corresponding orientation bin with magnitude
% of its gradient. 
%
% parameters: 
% 
% img_cell_mag - matrix of gradient magnitudes for each pixel on the cell
% img_cell_ori - matrix of gradient orientations for each pixel of the cell
%
% PARAMS.hist_numbins - number of bins in the gradient orientation histogram --
%                       that is how many directions do we recognize
% PARAMS.hist_binsize - size of each gradient bin in radians -- that is how
%                       big is one direction
% PARAMS.hist_min - minimal orientation of the gradient vector
% PARAMS.hist_max - maximal orientation of the gradient vector
%
%
% note: see detector_param.m for the definition of PARAMS
%

function h = comp_cell_hist(PARAMS, img_cell_mag, img_cell_ori)

  img_cell_mag = img_cell_mag(:);
  img_cell_ori = img_cell_ori(:);

  assert(length(img_cell_mag) == length(img_cell_ori));

  img_cell_ori(img_cell_ori < PARAMS.hist_min) = ...
      img_cell_ori(img_cell_ori < PARAMS.hist_min) + 1e-6;
  img_cell_ori(img_cell_ori >= PARAMS.hist_max) = ...
      img_cell_ori(img_cell_ori >= PARAMS.hist_max) - 1e-6;

  assert(all(img_cell_ori >= PARAMS.hist_min));
  assert(all(img_cell_ori < PARAMS.hist_max));

  img_cell_ori_bin_id = ...
      floor((img_cell_ori - PARAMS.hist_min) ./ PARAMS.hist_binsize) + 1;
  h = zeros(PARAMS.hist_numbins, 1);
  for i = 1:length(img_cell_mag)
    bin_id = img_cell_ori_bin_id(i);
    h(bin_id) = h(bin_id) + img_cell_mag(i);
  end

end
