% GET_IMG Read an image file and change the colors and/or data type.
%
%   img = get_img(fn, col, typ)
%
%   PARAMS:
%     fn -- image filename
%     col (optional) -- 'rgb', 'gray' or 'default'
%     typ (optional) -- 'double' or 'default'
%
function img = get_img(fn, col, typ)

  if nargin < 3
    typ = 'default';
  end
  if nargin < 2
    col = 'default';
  end

  rimg = imread(fn);
  icol = size(rimg, 3);

  if isequal(col, 'rgb') && icol == 1
    img = zeros([size(rimg) 3]);
    img(:, :, 1) = img;
    img(:, :, 2) = img;
    img(:, :, 3) = img;
  else if isequal(col, 'gray') && icol == 3
    img = rgb2gray(rimg);
  else
    img = rimg;
  end

  if isequal(typ, 'double')
    img = double(img) / 255.0;
  elseif isequal(typ, 'single')
    img = single(img) / 255.0;
  end

end
