function ism_svm_stacked(anns, train_ids, test_ids, tag, ism_params, svm_params)

  [perf, ids, bboxes] = ism_only_perf(anns, train_ids, test_ids, tag, ism_params);

  % Put ism computed bboxes in anns.
  for i = 1:length(ids)
    id = ids(i);
    anns(id).ism_bbox = bboxes(i);
  end

  svm_params.use_ism_bbox = true;
  svm_params.init_figidx = 2;
  svm_only_perf(anns, train_ids, ids, tag, svm_params);

end
