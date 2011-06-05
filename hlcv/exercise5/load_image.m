function img = load_image(image_name)
  fprintf('loading %s\n', image_name);
  img = imread(image_name);
  if length(size(img)) == 3
    img = rgb2gray(img);
  end

  img = double(img);
  img = img / 255;
