function [perf, ids, bboxes] = ism_only_perf(anns, train_ids, test_ids, tag, ism_params)

  global train_dir;

  cfn = build_cache_fn(tag, 'ism_model');
  if exist(cfn, 'file')
    load(cfn);
  else
    fprintf('ism learning initiated\n');
    ism_model = ism_learn(anns, train_ids, ism_params);

    save(cfn, 'ism_model');
  end

  cfn = build_cache_fn(tag, 'ism_results');
  if exist(cfn, 'file')
    load(cfn);
  else
    ism_params.stage = 'test';
    correct = 0;
    ids = [];
    bboxes = [];
    miss_ids = [];
    miss_bboxes = [];
    for tid = 1:length(test_ids)
      ann = anns(test_ids(tid));
      image_name = fullfile(train_dir, ann.image.name);
      fprintf('applying ism %d / %d\n', tid, length(test_ids));
      [dets, accs] = apply_ism(image_name, ism_model, ism_params);
      % TODO: This is cheating -- we assume there is exactly one person.
      % draw_detections(10 + tid, image_name, dets, 3, ddparams, ism_params);
      if size(dets, 1) == 0
        continue;
      end
      det = dets(1, :);
      % bboxc = mean([ann.annorect.x1, ann.annorect.y1; ann.annorect.x2, ann.annorect.y2]);
      % fprintf('%f %f -- %f %f (%f)\n', det(1), det(2), bboxc(1), bboxc(2), dist_l2(det, bboxc));
      bbox = detection_to_bbox(det, ism_params);
      if ism_params.validate_bbox(bbox, ann.annorect)
        correct = correct + 1;
        ids = [ids test_ids(tid)];
        bboxes = [bboxes bbox];
      else
        miss_ids = [miss_ids test_ids(tid)];
        miss_bboxes = [miss_bboxes bbox];
      end
    end
    perf = correct / length(test_ids);

    save(cfn, 'perf', 'ids', 'bboxes', 'miss_ids', 'miss_bboxes');
  end

  figure(1);
  for i = 1:min(ism_params.show_detections_count, length(ids))
    subplot(ism_params.show_detections_rows, ism_params.show_detections_cols, i);
    fn = extract_id(ids(i));
    imshow(imread(fn));
    hold on;
    draw_bbox(bboxes(i), 'blue');
    hold off;
  end

  % figure(2);
  % for i = 1:min(ism_params.show_detections_count, length(miss_ids))
  %   subplot(ism_params.show_detections_rows, ism_params.show_detections_cols, i);
  %   fn = extract_id(miss_ids(i));
  %   imshow(imread(fn));
  %   hold on;
  %   draw_bbox(miss_bboxes(i), 'red');
  %   hold off;
  % end

end

function bbox = detection_to_bbox(det, ism_params)

  bbox.x1 = floor(det(1) - ism_params.bbox_width / 2);
  bbox.x2 = bbox.x1 + ism_params.bbox_width - 1;
  bbox.y1 = floor(det(2) - ism_params.bbox_height / 2);
  bbox.y2 = bbox.y1 + ism_params.bbox_height - 1;

end
