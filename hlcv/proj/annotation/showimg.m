function [h, w, d] = showimg(annotation, show_objoffset)
  if nargin < 2
    show_objoffset = true;
  end

  imgfile = annotation.image.name;
  img = imread(imgfile);
  imagesc(img);
  axis image
  axis ij
  %imshow(img);
  hold on;

  [h, w, d] = size(img);
  if isfield(annotation.annorect(1), 'parts')
    nparts = 8;
    for i = 1:nparts
      [p1, p2] = getpartpts(annotation, i);
      %plot([p1(1), p2(1)], [p1(2), p2(2)], 'ro');
    end
  end

  if isfield(annotation, 'objoffset') && show_objoffset
    if ~isfield(annotation, 'rescale_factor')
      annotation.rescale_factor = 1;
    end

    rectangle('Position', [-annotation.objoffset(1),  -annotation.objoffset(2), ...
                        annotation.rescale_factor*160, annotation.rescale_factor*210], ...
              'LineStyle', '-', 'EdgeColor', 'g');

%    rectangle('Position', [-annotation.objoffset(1),  -annotation.objoffset(2), 160, 210], ...
%              'LineStyle', '-', 'EdgeColor', 'g');
  end

  if isfield(annotation, 'rescale_factor')
    title(['scale: ' num2str(annotation.rescale_factor) ' (' num2str(get_scaledim_from_scale(annotation.rescale_factor)) ')']);
  end

end