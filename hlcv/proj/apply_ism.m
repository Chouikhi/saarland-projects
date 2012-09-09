% detections is a n x 2 matrix, each row is a [x, y] of one detection
% acc is vector of size n x 1, the score of each detection
% 
% result is sorted by accuracy, higher to lower

function [detections, acc] = apply_ism(fn, ism_model, ism_params)

  global train_dir;

  cluster_centers = ism_model.cluster_centers;
  cluster_occurrences = ism_model.cluster_occurrences;
  ism_params.stage = 'test';

  % cfn = build_cache_fn(fn, 'ism');

  % if exist(cfn, 'file')
  %   load(cfn);
  % else
    cluster_id_to_occ = cluster_occurrences_to_map(cluster_occurrences, ism_params);
    [w, h] = get_img_size(fn);
    bbox.x1 = 1;
    bbox.y1 = 1;
    bbox.x2 = w;
    bbox.y2 = h;
    % create records
    occ_records = create_file_occurrences(fn, bbox, cluster_centers, ...
        ism_params.match_tresh, ism_params);
    % prepare hough
    hough = zeros(ceil([w, h] / (ism_params.hough_bin_size * ism_params.scale_factor)));
    norm = [w / (2 * ism_params.scale_factor), h / (2 * ism_params.scale_factor)];

    track = false;
    track_act_num = 1;
    for occ_id = 1:length(occ_records)
      occ = occ_records{occ_id};

      occp = occ.offset + norm;
      track = false;
      if abs(occp(1) - ism_params.track_activated_pnt(1)) < 2 && ...
          abs(occp(2) - ism_params.track_activated_pnt(2)) < 2
        show_cluster_patches(100 + track_act_num, ism_model.feature_patches, ...
            ism_model.assignments, occ.codebook_id);
        show_occurrence_distribution(110 + track_act_num, ism_model.cluster_occurrences, occ.codebook_id);

        track = true;
        track_act_num = track_act_num + 1;
        fprintf('track point %f %f  %d/%d', occp(1), occp(2), ...
            track_act_num, occ.activated);
        track_vote_pos = [];
      end

      offs = cluster_id_to_occ{occ.codebook_id};
      vote = 1 / (size(offs, 1) * occ.activated);
      for i = 1:size(offs, 1)
        off = offs(i, :);
        vote_pos = occp - off;
        if track
          track_vote_pos = [track_vote_pos; vote_pos];
        end
        vote_pos = floor(vote_pos / ism_params.hough_bin_size) + 1;
        % skip it if its outside
        if vote_pos(1) < 1 || vote_pos(1) > size(hough, 1) || ...
            vote_pos(2) < 1 || vote_pos(2) > size(hough, 2)
          continue;
        end
        hough(vote_pos(1), vote_pos(2)) = hough(vote_pos(1), vote_pos(2)) + vote;
      end

      if track
        figure(120 + track_act_num);
        if ism_params.track_votes_outside
          plot(track_vote_pos(:, 1), track_vote_pos(:, 2), 'xr');
          hold on;
          plot([0 0 w w 0], [0 h h 0 0], '-b');
          hold off;
        else
          imshow(get_img(fn));
          hold on;
          plot(track_vote_pos(:, 1), track_vote_pos(:, 2), 'xr');
          hold off;
        end
      end
 
    end

    if ism_params.show_raw_hough
      figure(150);
      imagesc(hough');
    end

    if ism_params.hough_gauss_sigma > 0
      gauss_filter = gauss(ism_params.hough_gauss_sigma);
      hough1 = conv2(hough, gauss_filter, 'same');
      hough_f = conv2(hough1, gauss_filter', 'same');
    else
      hough_f = hough;
    end

    if ism_params.show_smooth_hough
      figure(151);
      imagesc(hough_f');
    end

    nms_hough = nonmaxsup2d(hough_f);

    det_idx = find(nms_hough > ism_params.min_det_score);
    acc = nms_hough(det_idx);

    % sort by accuracy
    [acc_s, acc_si] = sort(acc, 'descend');
    acc = acc_s;
    det_idx = det_idx(acc_si);

    % convert det_idx to x, y in the image
    [detx, dety] = ind2sub(size(hough), det_idx);
    % we should return x, y
    detections = [detx, dety];
    detections = detections .* ism_params.hough_bin_size - (ism_params.hough_bin_size / 2);
    detections = detections * ism_params.scale_factor;
    detections = floor(detections);

  %   save(cfn, 'detections', 'acc');
  % end

end

% returns a cell, each element is a matrix num_clusters x 2, each row is an offset
function cluster_id_to_occ = cluster_occurrences_to_map(cluster_occurrences, ism_params)

  cluster_id_to_occ = cell(ism_params.kmeans_num_clusters, 1);

  for i = 1:ism_params.kmeans_num_clusters
    cluster_id_to_occ{i} = [];
  end

  for i = 1:length(cluster_occurrences)
    occ = cluster_occurrences{i};
    offs = cluster_id_to_occ{occ.codebook_id};
    offs = [offs; occ.offset];
    cluster_id_to_occ{occ.codebook_id} = offs;
  end

end
