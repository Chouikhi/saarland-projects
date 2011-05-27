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
% id1 : array of indices of the matching pairs in image 1, a line index in D
% id2 : array of indices of the matching pairs in image 2, a column index in D
%
% matchedScores: the distance of the returned matches
%
function [id1, id2, matchedScores] = match_points(D, thresh, nMatches)
 
  % ... 

