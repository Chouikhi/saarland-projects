function hog_feature = compute_hog(img_name, bbox, svm_params)

  rbbox = [0, 0; svm_params.bbox];
  rbbox_center = mean(rbbox);

  bbox_center = mean([bbox.x1, bbox.y1; bbox.x2, bbox.y2]);
  offset = bbox_center - rbbox_center;

  rbbox = floor(rbbox + repmat(offset, 2, 1));

  img = get_img(img_name, 'rgb', 'double');

  img_sz = size(img);

  rbbox(rbbox < 1) = 1;
  if rbbox(2, 1) > img_sz(2)
    rbbox(2, 1) = img_sz(2);
  end
  if rbbox(2, 2) > img_sz(1)
    rbbox(2, 2) = img_sz(1);
  end

  patch = img(rbbox(1, 2):rbbox(2, 2), rbbox(1, 1):rbbox(2, 1), :);
  hog_feature = hog(patch, svm_params.cell_size);

end
