function angles = annorect_to_angles(annorect, svm_params)

  joint_ids = [svm_params.joint_ids svm_params.center_joint_id];

  pts = [];
  for i = 1:length(joint_ids)
    ji = joint_ids(i);
    ptsx = annorect.annopoints.point(ji);
    pts = [pts; ptsx.x, ptsx.y];
  end
  
  pts_off = pts - repmat(pts(end, :), size(pts, 1), 1);
  angles = atan2(pts_off(:, 2), pts_off(:, 1));
  % drop the center point angle
  angles = angles(1:end-1);

end
