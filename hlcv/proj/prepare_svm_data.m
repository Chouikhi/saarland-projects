function [X, Y] = prepare_svm_data(anns, ids, svm_params)

  X = zeros(length(ids), svm_params.inp_ndims);
  % each row is the angles for different joints for one image
  Y = zeros(length(ids), svm_params.joint_count);
  for i = 1:length(ids)
    id = ids(i);

    [fn, annorect] = extract_id(id);
    if svm_params.use_ism_bbox
      bbox = anns(id).ism_bbox;
    else
      bbox = annorect;
    end
    hog_feature = compute_hog(fn, annorect, svm_params);
    angles = annorect_to_angles(annorect, svm_params);

    X(i, :) = hog_feature(:)';
    Y(i, :) = angles';
  end

end
