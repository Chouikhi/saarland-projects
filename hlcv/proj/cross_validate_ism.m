function perf = cross_validate_ism(anns, train_ids, all_ids, ism_params)

  global train_dir;

  test_ids = setdiff(all_ids, train_ids);

  ism_model = ism_learn(anns, train_ids, ism_params);

  ddparams.bounding_box_size = [ism_params.bbox_width, ism_params.bbox_height];
  ddparams.show_activations = true;
  ddparams.font_size = 18;

  ism_params.stage = 'test';
  correct = 0;
  for tid = 1:length(test_ids)
    ann = anns(test_ids(tid));
    image_name = fullfile(train_dir, ann.image.name);
    [dets, accs] = apply_ism(image_name, ism_model, ism_params);
    % TODO: This is cheating -- we assume there is exactly one person.
    draw_detections(10 + tid, image_name, dets, 3, ddparams, ism_params);
    if size(dets, 1) == 0
      continue;
    end
    det = dets(1, :);
    bboxc = mean([ann.annorect.x1, ann.annorect.y1; ann.annorect.x2, ann.annorect.y2]);
    % fprintf('%f %f -- %f %f (%f)\n', det(1), det(2), bboxc(1), bboxc(2), dist_l2(det, bboxc));
    bbox = detection_to_bbox(det, ism_params);
    correct = correct + ism_params.validate_bbox(bbox, ann.annorect);
  end

  perf = correct / length(test_ids);

end

function bbox = detection_to_bbox(det, ism_params)

  bbox.x1 = floor(det(1) - ism_params.bbox_width / 2);
  bbox.x2 = bbox.x1 + ism_params.bbox_width - 1;
  bbox.y1 = floor(det(2) - ism_params.bbox_height / 2);
  bbox.y2 = bbox.y1 + ism_params.bbox_height - 1;

end
