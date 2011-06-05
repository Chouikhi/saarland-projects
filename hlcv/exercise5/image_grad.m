function [img_mag, img_ori, img_dx, img_dy] =  image_grad(PARAMS, img)
  % ...

  % worked well for 8x8 cells, small sigma is important
  [img_dx, img_dy] = gaussderiv(img, 1.0);

  img_mag = sqrt(img_dx.^2 + img_dy.^2);

  for ix = 1:size(img, 2)
    for iy = 1:size(img, 1)

      %
      % avoid dividing by 0
      %
      if abs(img_dx(iy, ix)) < 1e-6
        if img_dy(iy, ix) > 0
          img_ori(iy, ix) = pi/2;
        else
          img_ori(iy, ix) = -pi/2;
        end
      else
        img_ori(iy, ix) = atan(img_dy(iy, ix)/img_dx(iy, ix));
      end

    end
  end

  % error checking
  assert(all(all(~isnan(img_ori))));
  assert(all(all(~isnan(img_mag))));

  % remove gradients at the border
  border_offset = 2;
  img_mag(1:border_offset, :) = 0;
  img_mag(end - border_offset + 1:end, :) = 0;

  img_mag(:, 1:border_offset) = 0;
  img_mag(:, end-border_offset+1:end) = 0;

