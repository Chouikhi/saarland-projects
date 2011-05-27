function D = compute_descriptors(desc_func, img, px, py, m, bins)
  
  rad   = round((m-1)/2);
  [h w c] = size(img);
  D = [];
  
  for i=1:length(px)
    minx = max(px(i)-rad,1);
    maxx = min(px(i)+rad,w);
    miny = max(py(i)-rad,1);
    maxy = min(py(i)+rad,h);

    imgWin = img(miny:maxy, minx:maxx, :);  

    hist = desc_func(imgWin, bins);
    D(i,:) = hist';
  end;
  
end