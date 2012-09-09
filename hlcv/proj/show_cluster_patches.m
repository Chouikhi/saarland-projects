% show the patches corresponding to a cluster
% -- feature_patches: all patches
% -- assignments: which patch corresponds to which cluster
% -- cluster_idx: which cluster id's patches to show
function show_cluster_patches(figidx, feature_patches, assignments, cluster_idx)
  
  figure(figidx);
  clf;

  cluster_mask = assignments == cluster_idx;
  patch_count = sum(cluster_mask);
  patch_count = min(patch_count, 100);
  grid_size = ceil(sqrt(patch_count));
  patch_idxs = find(cluster_mask, patch_count);

  for i = 1:patch_count
    subplot(grid_size, grid_size, i);
    imshow(feature_patches{patch_idxs(i)});
  end

end
