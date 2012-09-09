function distance_test()

  img_names = {'img1.jpg', 'img2.jpg', 'img3.jpg', 'img4.jpg'};
  imgs = {};
  for i = 1:length(img_names)
    imgs{end+1} = imread(img_names{i});
  end

  segs = [4 16];
  hists = cell(length(segs), length(imgs));
  dists = zeros(2, length(imgs), length(imgs));

  for segi = 1:2
    for i = 1:length(imgs)
      hists{segi,i} = segmented_histogram(imgs{i}, 15, segs(segi), segs(segi));
    end

    for i = 1:length(imgs)
      for j = 1:length(imgs)
        dists(segi, i, j) = ln_dist(hists{segi, i}, hists{segi, j}, 2);
      end
    end
  end

  for s = 1:2
    fprintf('distances for %dx%d\n', segs(s), segs(s));
    for i = 1:size(dists, 2)
      for j = 1:size(dists, 3)
        fprintf(' %.4f', dists(s, i, j));
      end
      fprintf('\n');
    end
  end

end
