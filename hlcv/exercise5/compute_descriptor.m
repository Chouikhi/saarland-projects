%
% compute the HOG descriptor
%

function [DESC, CELLS] = compute_descriptor(PARAMS, imgx)

  if isa(imgx, 'char')
    if PARAMS.descriptor_caching
      cache_fn = sprintf('cache/%s_%s.mat', PARAMS.summary_str, base_name(imgx));
      if exist(cache_fn, 'file')
        load(cache_fn);
        return;
      end
    end
    img_s = imgx;
    img = load_image(img_s);
  else
    img = imgx;
  end

  [img_mag, img_ori] = image_grad(PARAMS, img);

  CELLS = cell(PARAMS.num_cells_height, PARAMS.num_cells_width);
  DESC = [];

  for by = 0:PARAMS.num_cells_height - 1
    for bx = 0:PARAMS.num_cells_width - 1
      cell_mag = get_cell(img_mag, PARAMS, bx, by);
      cell_ori = get_cell(img_ori, PARAMS, bx, by);

      h = comp_cell_hist(PARAMS, cell_mag, cell_ori);
      CELLS{by+1, bx+1} = h;
    end
  end


  assert(mod(PARAMS.num_cells_width, 2) == 0 && ...
         mod(PARAMS.num_cells_height, 2) == 0);

  for by = 1:2:PARAMS.num_cells_height
    for bx = 1:2:PARAMS.num_cells_width
      v = [CELLS{by, bx}; CELLS{by, bx+1}; CELLS{by+1, bx}; CELLS{by+1, bx+1}];

      if PARAMS.normalize
        vl = v(:);
        % compute norm
        l2 = sqrt(sum(vl .^ 2) + eps);

        % normalize
        v = v ./ l2;
      end

      DESC = [DESC; v];
    end
  end

  if isa(imgx, 'char') && PARAMS.descriptor_caching
    % cache_fn = sprintf('cache/%s_%s.mat', PARAMS.summary_str, img_s);
    fprintf('saving desc in %s\n', cache_fn);
    save(cache_fn, 'CELLS', 'DESC');
  end

end
