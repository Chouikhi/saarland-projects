%
% [h, w, d] = showanno(annotation, varargin)
%
% options: show_objoffset (false), rect_color (g), show_img (true)
%
function [h, w, d] = showanno(annotation, varargin)

  [show_objoffset, rect_color, show_img, min_score] = process_options(varargin, ...
                                                    'show_objoffset', false, ...
                                                    'rect_color', 'g', ...
                                                    'show_img', true, ...
                                                    'min_score', -1e6);

  imgfile = annotation.image.name;
  img = imread(imgfile);

  if show_img
    imagesc(img);
    axis image
    axis ij
    %imshow(img);
    hold on;
    axis off;
  end

  [h, w, d] = size(img);


  if isfield(annotation, 'annorect')
    for ridx = 1:length(annotation.annorect)

      if isfield(annotation.annorect(ridx), 'score')
        if annotation.annorect(ridx).score < min_score
          continue;
        end
      end

      if isfield(annotation.annorect(ridx), 'parts')
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
                  'LineStyle', '-', 'EdgeColor', 'g', 'LineWidth', 2);

        %    rectangle('Position', [-annotation.objoffset(1),  -annotation.objoffset(2), 160, 210], ...
        %              'LineStyle', '-', 'EdgeColor', 'g');
      end

      if isfield(annotation, 'rescale_factor')
        title(['scale: ' num2str(annotation.rescale_factor) ' (' num2str(get_scaledim_from_scale(annotation.rescale_factor)) ')']);
      end
      
      w = (annotation.annorect(ridx).x2 - annotation.annorect(ridx).x1);
      h = (annotation.annorect(ridx).y2 - annotation.annorect(ridx).y1);
      
      if ~isfield(annotation.annorect(ridx), 'x3')
        rectangle('Position', [annotation.annorect(ridx).x1,  annotation.annorect(ridx).y1, w, h], ...
                  'LineStyle', '-', 'EdgeColor', rect_color, 'LineWidth', 4);
      else
        line([annotation.annorect(ridx).x1, annotation.annorect(ridx).x2, annotation.annorect(ridx).x3, ...
              annotation.annorect(ridx).x4, annotation.annorect(ridx).x1], ...
             [annotation.annorect(ridx).y1, annotation.annorect(ridx).y2, annotation.annorect(ridx).y3, ...
              annotation.annorect(ridx).y4, annotation.annorect(ridx).y1], ...
             'LineStyle', '-', 'Color', rect_color);
      end

      if isfield(annotation.annorect(ridx), 'annopoints')
	if isfield(annotation.annorect(ridx).annopoints, 'point')
	  for ptidx = 1:length(annotation.annorect(ridx).annopoints.point)
	    plot(annotation.annorect(ridx).annopoints.point(ptidx).x, annotation.annorect(ridx).annopoints.point(ptidx).y, ...
		 'y+');
	  end
	end
      end


    end % annorects 


  end