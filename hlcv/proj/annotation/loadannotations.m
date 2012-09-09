% function annotations = loadannotations(annotationfile)
function annotations = loadannotations(annotationfile)

  assert(exist(annotationfile, 'file') > 0, 'file does not exist');

  [tmp1, tmp2, annoext] = splitpathext(annotationfile);

  if strcmp(annoext, 'al')
    % load xml 
    annostruct = loadXML(annotationfile);
    annotations = annostruct.annotationlist.annotation;

    % convert string to numberic values
    for i = 1:length(annotations)
      annotations(i).imgnum = i;
      
      if isfield(annotations(i), 'annorect') && length(annotations(i).annorect) > 0
        for ri = 1:length(annotations(i).annorect)
          annotations(i).annorect(ri).x1 = str2num(annotations(i).annorect(ri).x1);
          annotations(i).annorect(ri).y1 = str2num(annotations(i).annorect(ri).y1);
          annotations(i).annorect(ri).x2 = str2num(annotations(i).annorect(ri).x2);
          annotations(i).annorect(ri).y2 = str2num(annotations(i).annorect(ri).y2);

          if isfield(annotations(i).annorect(ri), 'x3')
            assert(isfield(annotations(i).annorect(ri), 'y3'));
            assert(isfield(annotations(i).annorect(ri), 'x4'));
            assert(isfield(annotations(i).annorect(ri), 'y4'));

            annotations(i).annorect(ri).x3 = str2num(annotations(i).annorect(ri).x3);
            annotations(i).annorect(ri).y3 = str2num(annotations(i).annorect(ri).y3);

            annotations(i).annorect(ri).x4 = str2num(annotations(i).annorect(ri).x4);
            annotations(i).annorect(ri).y4 = str2num(annotations(i).annorect(ri).y4);
          end

          if isfield(annotations(i).annorect(ri), 'score')
            if ~isempty(annotations(i).annorect(ri).score)
              annotations(i).annorect(ri).score = str2num(annotations(i).annorect(ri).score);
            end
          end

          if isfield(annotations(i).annorect(ri), 'annopoints') 
            if length(annotations(i).annorect(ri).annopoints) == 1 && isfield(annotations(i).annorect(ri).annopoints, 'point')

              for j = 1:length(annotations(i).annorect(ri).annopoints.point)
                annotations(i).annorect(ri).annopoints.point(j).id = str2num(annotations(i).annorect(ri).annopoints.point(j).id);
                annotations(i).annorect(ri).annopoints.point(j).x = str2num(annotations(i).annorect(ri).annopoints.point(j).x);
                annotations(i).annorect(ri).annopoints.point(j).y = str2num(annotations(i).annorect(ri).annopoints.point(j).y);

		if isfield(annotations(i).annorect(ri).annopoints.point(j), 'is_visible')
		  annotations(i).annorect(ri).annopoints.point(j).is_visible = boolean(str2num(annotations(i).annorect(ri).annopoints.point(j).is_visible));
		end

              end

            end
          end

          if isfield(annotations(i).annorect(ri), 'objpos')
            if isfield(annotations(i).annorect(ri).objpos, 'x')
              annotations(i).annorect(ri).objpos.x = str2num(annotations(i).annorect(ri).objpos.x);
              annotations(i).annorect(ri).objpos.y = str2num(annotations(i).annorect(ri).objpos.y);
            end
          else
            %warning('object position not specified');
          end

          if isfield(annotations(i).annorect(ri), 'silhouette')
            if isfield(annotations(i).annorect(ri).silhouette, 'id')
              annotations(i).annorect(ri).silhouette.id = str2num(annotations(i).annorect(ri).silhouette.id);
            end
          end
          

        end % annorects
      else
        %warning(['no annorect in annotation ' num2str(i)]);
        continue
      end


    end % annotations



  elseif strcmp(annoext, 'idl')
    %fprintf('idl format is not supported yet.\n');
    %assert(false);

    [ISM_hypo,nHypo,imgIds,ISM_score] = extractIDL(annotationfile);

    annotations = [];

    fprintf('creating annolist ... ');

    for imgidx = 1:length(imgIds)

      annotations(imgidx).image.name = imgIds{imgidx};
      annotations(imgidx).imgnum = imgidx;

      for hidx = 1:nHypo(imgidx)
        annotations(imgidx).annorect(hidx).x1 = double(ISM_hypo{imgidx}(hidx, 1));
        annotations(imgidx).annorect(hidx).y1 = double(ISM_hypo{imgidx}(hidx, 2));
        annotations(imgidx).annorect(hidx).x2 = double(ISM_hypo{imgidx}(hidx, 3));
        annotations(imgidx).annorect(hidx).y2 = double(ISM_hypo{imgidx}(hidx, 4));
        annotations(imgidx).annorect(hidx).score = double(ISM_score{imgidx}(hidx));
      end
    end

    fprintf('done.\n');

  else
    fprintf('unknown format\n');
    assert(false);
  end

  %
  % normalize all upright rectangles 
  %

  for aidx = 1:length(annotations)
    if isfield(annotations(aidx), 'annorect')      

      for ridx = 1:length(annotations(aidx).annorect)

        %
        % normalize only upright rectangles
        %
        if ~isfield(annotations(aidx).annorect(ridx), 'x3')
          if abs(annotations(aidx).annorect(ridx).x1 - annotations(aidx).annorect(ridx).x2) < 1e-2 || ...
                abs(annotations(aidx).annorect(ridx).y1 - annotations(aidx).annorect(ridx).y2) < 1e-2
            fprintf('WARNING: degenerated rectangle in image %d\n', aidx);
          end
          
          left = min(annotations(aidx).annorect(ridx).x1, annotations(aidx).annorect(ridx).x2);
          right= max(annotations(aidx).annorect(ridx).x1, annotations(aidx).annorect(ridx).x2);
          top  = min(annotations(aidx).annorect(ridx).y1, annotations(aidx).annorect(ridx).y2);
          bottom = max(annotations(aidx).annorect(ridx).y1, annotations(aidx).annorect(ridx).y2);

          annotations(aidx).annorect(ridx).x1 = left;
          annotations(aidx).annorect(ridx).x2 = right;

          annotations(aidx).annorect(ridx).y1 = top;
          annotations(aidx).annorect(ridx).y2 = bottom;
        end

      end

    end
  end

end

%
% author/source: Paul Schnitzspan
%

function [ISM_hypo,nHypo,imgIds,ISM_score] = extractIDL(IDLfile)
%read hypotheses from IDLfile

  fid = fopen(IDLfile);
  %tmp = textscan(fid, '%s','delimiter','"','BufSize',16383);
  fprintf('loading data ... ');
  tmp = textscan(fid, '%s', 'delimiter', '"', 'BufSize', 10^6);
  fprintf('done. \n');
  fclose(fid);

  for i=3:3:size(tmp{1,1},1)
    tmp2{i/3} = textscan(tmp{1,1}{i,1},'%s %s %s %s','delimiter',',');
  end

  for i = 3:3:size(tmp{1,1},1)
    tmp3 = textscan(tmp{1,1}{i-1,1},'%s');
    imgIds{i/3} = tmp3{1}{1};
  end

  for i = 1:size(tmp2,2)
    fprintf('.');
    if mod(i, 80) == 0
      fprintf('\n');
    end
    
    % first hypotheses has to be treated separately
    h = textscan(tmp2{1,i}{1,1}{1,1},'%*s %d','delimiter','(');
    
    if isempty(h{1,1})
      %fprintf(1,'empty\n');
      nHypo(i) = 0;
    else
      
      ISM_hypo{i}(1,1) = h{1,1};
      ISM_hypo{i}(1,2) = str2num(tmp2{1,i}{1,2}{1,1});
      ISM_hypo{i}(1,3) = str2num(tmp2{1,i}{1,3}{1,1});
      h = textscan(tmp2{1,i}{1,4}{1,1},'%d %*s','delimiter',')');        
      ISM_hypo{i}(1,4) = h{1,1};
      h =  textscan(tmp2{1,i}{1,4}{1,1},'%d):%f');
      ISM_score{i}(1) = h{1,2};
      for j = 2:size(tmp2{1,i}{1,1},1)
        h = textscan(tmp2{1,i}{1,1}{j,1},'%*c %d');
        ISM_hypo{i}(j,1) = h{1,1};
        ISM_hypo{i}(j,2) = str2num(tmp2{1,i}{1,2}{j,1});
        ISM_hypo{i}(j,3) = str2num(tmp2{1,i}{1,3}{j,1});
        h = textscan(tmp2{1,i}{1,4}{j,1},'%d %*s','delimiter',')');
        ISM_hypo{i}(j,4) = h{1,1};
        h =  textscan(tmp2{1,i}{1,4}{j,1},'%d):%f');
        ISM_score{i}(j) = h{1,2};
      end
      nHypo(i) = size(ISM_hypo{i},1);
    end
  end

  if exist('ISM_hypo', 'var') == 0
    ISM_hypo = [];
  end

  if exist('nHypo', 'var') == 0
    nHypo = [];
  end

  if exist('imgIds', 'var') == 0
    imgIds = [];
  end

  if exist('ISM_score', 'var') == 0
    ISM_score = [];    
  end

  fprintf('\n');

end
