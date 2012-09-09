% Tutorial 3 Exercise 1 Subtask 1.4
% Iskren Ivov Chernev s9ischer@stud.uni-saarland.de

function test_gabor(varargin)

  if length(varargin) > 0
    img_name = varargin{1};
  else
    img_name = 'test_image.jpg';
  end

  img = imread(img_name);
  img = double(img) ./ 255;
  g_nm = show_gabor('no_show');
  for i = 1:6
    subplot(2, 3, i);
    imagesc(conv2(img, g_nm{i}));
    title(sprintf('angle = %.0f deg', (i - 1) * 30));
  end

end
