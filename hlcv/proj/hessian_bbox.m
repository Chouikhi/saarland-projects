% Computes hessian of the image in fn, cropped by bbox, and stores the result
% in a cache file.
function [px py, H] = hessian_bbox(fn, bbox, ism_params)

  cfn = build_cache_fn(fn, 'hessian');

  if exist(cfn, 'file')
    load(cfn);
  else
    img = get_img(fn, 'gray', 'double');
    img = crop_img(img, bbox, ism_params);
    [px, py, H] = hessian(img, ...
        ism_params.hessian_sigma, ...
        ism_params.hessian_thresh);
    % in case of training leave only IPs on the pedestrian, not on the
    % background
    if isequal(ism_params.stage, 'train')
      map_img = get_img(get_map_fn(fn), 'gray');
      cmap_img = crop_img(map_img, bbox, ism_params);
      drop = zeros(size(px));
      for pid = 1:length(px)
        x = px(pid);
        y = py(pid);
        if cmap_img(y, x) == 0
          drop(pid) = 1;
        end
      end
      stay_idx = find(drop == 0);
      px = px(stay_idx);
      py = py(stay_idx);
    end
    save(cfn, 'px', 'py', 'H', 'bbox');
  end

end

function [px py, H] = hessian(img, sigma, thresh)

  [imgDxx, imgDxy, imgDyy] = gaussderiv2(img,sigma);

  H = sigma^4 * (imgDxx.*imgDyy-imgDxy.^2);
  %H = imgDxx.*imgDyy-imgDxy.^2;

  nmsH = nonmaxsup2d( H );

  [py px] = find(nmsH > thresh);

end
