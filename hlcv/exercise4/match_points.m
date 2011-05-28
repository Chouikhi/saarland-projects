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
function [id1, id2, matchedScores] = match_points(D, thresh, nMatches)
  mask = (D < thresh) - eye(size(D,1), size(D,2));
  [vals, inds] = sort(D.*mask);
  
  id1 = [];
  id2 = [];
  matchedScores = [];

  for col = 1:size(D,2)
    [i j v] = find(vals(:,col).*(vals(:,col)>0), nMatches);
    assert(all(j), 'column indexes should be (all) 1');
    row_ix = inds(:,col)';
    id1 = [id1 row_ix(i)];
    col_ix = ones(1, length(i)) .* col;
    id2 = [id2 col_ix];
    matchedScores = [matchedScores v'];
  end
end
