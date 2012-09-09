function ism_params = get_ism_params()

  % parameters of the Hessian interest point detector
  ism_params.hessian_sigma = 2.0; % was 2.0
  ism_params.hessian_thresh = 100.0 / (255 * 255); % was 530

  % parameters of the SIFT image descriptor
  ism_params.feature_scale = 2.0;
  ism_params.feature_ori = 0;

  % number of pixels in each direction used for sift calculation
  ism_params.sift_patch_size = get_feature_size(ism_params.feature_scale);

  % params for debugging
  ism_params.extract_patches = false;
  ism_params.show_codebook_patches = 0;
  ism_params.show_occ_distr = 0;
  ism_params.track_activated_pnt = [-5, -5];  % this means no track
  ism_params.track_votes_outside = false;
  ism_params.show_raw_hough = false;
  ism_params.show_smooth_hough = false;  

  % parameters of kmeans
  ism_params.kmeans_num_clusters = 400;

  % distances from sift patches to cluster centers
  ism_params.match_tresh = 300; % testing % was 190
  ism_params.match_reco_tresh = 250; % training % was 190

  % scale down everything to improve ism performance
  ism_params.scale_factor = 1.0;

  % hough parameters
  ism_params.hough_gauss_sigma = 5;
  ism_params.hough_bin_size = 1;
  ism_params.min_det_score = 0;

  % size of bounding box, to use for ism detections
  ism_params.bbox_width = 100;
  ism_params.bbox_height = 200;
  % ism_params.bbox_thresh_overlap = 0.7;

  ism_params.validate_bbox = @validate_bbox;

  % bbox visualization
  ism_params.show_detections_count = 0;
  ism_params.show_detections_rows = 1;
  ism_params.show_detections_cols = 1;

end

function is_valid = validate_bbox(guessed_bbox, correct_bbox)

  is.x1 = max(guessed_bbox.x1, correct_bbox.x1);
  is.x2 = min(guessed_bbox.x2, correct_bbox.x2);
  is.y1 = max(guessed_bbox.y1, correct_bbox.y1);
  is.y2 = min(guessed_bbox.y2, correct_bbox.y2);
  sis = bbox_area(is);
  s1 = bbox_area(guessed_bbox);
  s2 = bbox_area(correct_bbox);

  is_valid = max(sis / s1, sis / s2) > 0.7;

end

function area = bbox_area(bbox)

  area = (bbox.x2 - bbox.x1) * (bbox.y2 - bbox.y1);

end
