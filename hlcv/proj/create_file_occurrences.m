% compute cluster occurrences and activation points for a given image
% -- fn  file containing the image
% -- bbox  contains x1 x2 y1 y2 of the region we are interested in
% -- ctag  used to distinguish between occurences on the whole image vs
%    occurences on a person bbox only
% -- cluster_centers  codebook entries
% -- thresh  used when matching sift descriptors to codebook
% -- ism_params
function [cluster_occ, activated_points] = create_file_occurrences(fn, bbox, ...
    cluster_centers, thresh, ism_params)

  cfn = build_cache_fn(fn, sprintf('%s_occurrences', ism_params.stage));

  if exist(cfn, 'file')
    load(cfn);
  else
    [cluster_occ, activated_points] = create_file_occurrences_(fn, bbox, ...
        cluster_centers, thresh, ism_params);
    save(cfn, 'cluster_occ', 'activated_points');
  end

end

function [cluster_occ, activated_points] = create_file_occurrences_(fn, bbox, ...
    cluster_centers, thresh, ism_params)

  cluster_occ = {};
  activated_points = [];
  w = bbox.x2 - bbox.x1;
  h = bbox.y2 - bbox.y1;
  % [w, h] = get_img_size(fn);
  [px, py] = hessian_bbox(fn, bbox, ism_params);
  offsets = [px, py] - ...
      [ones(size(px)) .* (w / (2 * ism_params.scale_factor)), ...
       ones(size(py)) .* (h / (2 * ism_params.scale_factor))];
  [sift_frames, sift_desc] = sift_bbox(fn, bbox, ism_params);
  % the cluster centers are normalized like that
  % sift_desc = single(sift_desc ./ 255);
  sift_desc = single(sift_desc);

  for desc_id = 1:size(sift_desc, 2)
    desc = sift_desc(:, desc_id);
    closest_cl_ids = vector_ids_thresh(cluster_centers, desc, thresh);
    fprintf(' %d', length(closest_cl_ids));
    ar.pos = [px(desc_id), py(desc_id)];
    ar.clust = [];
    if length(closest_cl_ids) > 0
      activated_points(end+1, :) = [px(desc_id), py(desc_id)];
    end
    % keyboard;
    % create occurrences entry for each match
    for cl_id = 1:length(closest_cl_ids)
      entry.codebook_id = closest_cl_ids(cl_id);
      entry.offset = offsets(desc_id, :);
      entry.activated = length(closest_cl_ids);
      cluster_occ{end + 1} = entry;
    end
  end
  fprintf('| %d %d\n', length(cluster_occ), length(activated_points));

end

