% Given a file name compute interest points & extract patches around them.
function patches = extract_file_patches(fn, bbox, ism_params)

  cfn = build_cache_fn(fn, 'patches');

  if exist(cfn, 'file')
    load(cfn);
  else
    [px, py] = hessian_bbox(fn, ism_params);
    if not(min(px) >= 1 && max(px) <= ceil((bbox.x2 - bbox.x1 + 1) / ism_params.scale_factor)) ...
        || not(min(py) >= 1 && max(py) <= ceil((bbox.y2 - bbox.y1 + 1) / ism_params.scale_factor))
      fprintf('cp1\n');
      keyboard;
    end
    assert(min(px) >= 1 && max(px) <= ceil((bbox.x2 - bbox.x1 + 1) / ism_params.scale_factor));
    assert(min(py) >= 1 && max(py) <= ceil((bbox.y2 - bbox.y1 + 1) / ism_params.scale_factor));
    px = floor(px + bbox.x1 / ism_params.scale_factor);
    py = floor(py + bbox.y1 / ism_params.scale_factor);

    patch_size = ism_params.sift_patch_size;

    fimg = get_img(fn, 'gray', 'single');
    % cimg = crop_img(fimg, bbox, ism_params);
    simg = imresize(fimg, 1 / ism_params.scale_factor);
    if not(min(px) >= 1 && max(px) <= size(simg, 2)) ...
        || not(min(py) >= 1 && max(py) <= size(simg, 1))
      keyboard;
    end

    assert(min(px) >= 1 && max(px) <= size(simg, 2));
    assert(min(py) >= 1 && max(py) <= size(simg, 1));
    patches = cell(length(px), 1);

    fprintf('extracting patches from %s', fn);
    % Prepare bounded image for easier extraction
    imgx = zeros(size(simg) + patch_size * 2);
    imgx(patch_size:size(simg, 1) + patch_size - 1, ...
        patch_size:size(simg, 2) + patch_size - 1) = simg;
    px = px + patch_size - 1;
    py = py + patch_size - 1;

    for i = 1:length(px)
      patch = extract_patch(imgx, px(i), py(i), patch_size);
      patches{i} = patch;
    end

    fprintf(' patches extracted -- %d\n', length(px));

    save(cfn, 'patches');
  end

end

function patch = extract_patch(img, x, y, patch_size)

  patch = zeros(patch_size);
  lb = [x y] - floor(patch_size / 2);
  ub = [x y] + floor((patch_size - 1) / 2);
  patch = img(lb(2):ub(2), lb(1):ub(1));

end

