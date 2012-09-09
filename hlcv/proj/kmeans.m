function [cluster_centers, assignments] = kmeans(anns, ids, ism_params)

  global train_dir;
  cfn = build_cache_fn('kmeans', 'cache');
  
  if exist(cfn, 'file')
    load(cfn);
  else
    % v_img_names = dir(fullfile(dn, '*.png'));

    sift_descs = [];
    for ann_n = 1:length(ids)
      ann = anns(ids(ann_n));
      img_name = fullfile(train_dir, ann.image.name);

      [csift_frames, csift_desc] = sift_bbox(img_name, ...
          ann.annorect, ism_params);

      sift_descs = [sift_descs csift_desc];
    end
    % vl_kmeans wants floating point
    % sift_descs = single(sift_descs ./ 255);
    sift_descs = single(sift_descs);
    fprintf('computing kmeans of %d points into %d clusters', ...
        size(sift_descs, 2), ism_params.kmeans_num_clusters);
    num_clusters = min(ism_params.kmeans_num_clusters, size(sift_descs, 2));
    [cluster_centers, assignments] = vl_kmeans(sift_descs, num_clusters);
    fprintf(' done\n');
    save(cfn, 'cluster_centers', 'assignments');
  end

end
