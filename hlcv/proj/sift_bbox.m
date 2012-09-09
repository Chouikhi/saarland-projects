function [sift_frames, sift_desc] = sift_bbox(fn, bbox, ism_params)

  cfn = build_cache_fn(fn, 'sift');

  if exist(cfn, 'file')
    load(cfn);
  else
    [px, py] = hessian_bbox(fn, bbox, ism_params);
    px = floor(px + (bbox.x1 - 1) / ism_params.scale_factor);
    py = floor(py + (bbox.y1 - 1) / ism_params.scale_factor);
    img = get_img(fn, 'gray', 'single');

    % imshow(img);
    % hold on;
    % plot(px, py, 'xg');
    % hold off;
    % keyboard;

    simg = imresize(img, 1 / ism_params.scale_factor);
    % img = crop_img(img, bbox, ism_params);
    num_points = length(px);
    fprintf('computing sift for %s', fn);
    sift_frames = [px'; py'; ...
                   ism_params.feature_scale * ones(1, num_points); ...
                   ism_params.feature_ori * ones(1, num_points)];
    [sift_frames, sift_desc] = vl_sift(simg, 'Frames', sift_frames);
    fprintf(' done\n');

    save(cfn, 'sift_frames', 'sift_desc');
  end

end
