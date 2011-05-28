%
% compute image descriptors corresponding to the list of interest points 
% 
% D - matrix with number of rows equal to number of interest points, each row should be equal to the corresponding descriptor vector
% px, py - vectors of x/y coordinates of interest points
% m - size of the region around interest point which is used to compute the descriptor
% desc_func - handle of the corresponding descriptor function (i.e. @rg_hist)
% bins - number of bins parameter of the descriptor function 
%

function D = compute_descriptors(desc_func, img, px, py, m, bins)
  
  rad = round((m-1)/2);
  [h w c] = size(img);
  D = [];
  
  for i=1:length(px)
    minx = max(px(i)-rad,1);
    maxx = min(px(i)+rad,w);
    miny = max(py(i)-rad,1);
    maxy = min(py(i)+rad,h);

    imgWin = img(miny:maxy, minx:maxx, :);  

    hist = desc_func(imgWin, bins);
    D(i, :) = hist';
  end
end
