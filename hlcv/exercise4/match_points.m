%
% computes the best matching point pairs between two images using a score
% value. Restricts the amount of matches of each point.
% 
% D         : Distance matrix, one entry for all possible pairs
% thresh    : the treshold for possible valid matches only pairs with a
%             score below that value are considered as possible pairs
% nMatches  : the maximal amount of matches for each point. No point is
%             allowed to occur in more than that value in the found pairs.
%
% id1 : array of indices of the matching pairs in image 1, a row index in D
% id2 : array of indices of the matching pairs in image 2, a column index in D
%
% matchedScores: the distance of the returned matches
%

function [id1, id2, ms] = match_points(D, thresh, nMatches)

  mask1 = thresh_mask(D, thresh, nMatches);
  mask2 = thresh_mask(D', thresh, nMatches);

  mask = mask1 & mask2';
  
  % linear indexes
  lid = find(mask);
  % row/col indexes
  [id1, id2] = ind2sub(size(mask), lid);
  ms = D(lid);

end

% the mask represents the elements in D, which are less than thresh and also
% no more than nMatches per column (if more than nMatches are less than thresh
% then the smallest nMatches are picked)
function mask = thresh_mask(D, thresh, nMatches)

  Ds = sort(D);

  mask = [];
  for col = 1:size(D, 2)
    % pick either the worst point or the threshold for cutoff
    cutoff = min(Ds(nMatches, col), thresh);
    mask = [mask (D(:, col) <= cutoff)];
  end

end
