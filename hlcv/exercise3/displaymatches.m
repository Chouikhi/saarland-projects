function displaymatches(img1, px1, py1, img2, px2, py2, Idx, Dist, N)

  px2 = px2(Idx);
  py2 = py2(Idx);

  [sortval, sortidx] = sort(Dist, 'ascend');

  px1 = px1(sortidx);
  py1 = py1(sortidx);

  px2 = px2(sortidx);
  py2 = py2(sortidx);

  match_plot(img1, img2, [px1(1:N), py1(1:N)], [px2(1:N), py2(1:N)]);
  
