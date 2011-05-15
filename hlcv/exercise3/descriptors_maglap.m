function D = descriptors_maglap(img, px, py, m, sigma, bins)
  
  rad   = round((m-1)/2);
  [h w] = size(img);
  D = zeros(length(px),bins^2);
  
  for i=1:length(px)
    minx = max(px(i)-rad,1);
    maxx = min(px(i)+rad,w);
    miny = max(py(i)-rad,1);
    maxy = min(py(i)+rad,h);

    imgWin = img(miny:maxy, minx:maxx);
    hist   = histmaglap(imgWin,sigma,bins);
    D(i,:) = hist';
  end;
  
end