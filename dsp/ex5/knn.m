% Tutorial 5 Exercise 1 Subtask 1.1
% Iskren Ivov Chernev s9ischer@stud.uni-saarland.de

function group = knn(data, k, trainSize, query, distf)
  
  assert(isequal(size(query), [1, size(data.data, 2)]), 'query has incorrect dimmensions');

  dists = zeros(trainSize, 1);

  for i = 1:trainSize
    dists(i) = distf(data.data(i, :), query);
  end

  [dists, idx] = sort(dists);

  bestv = {};
  bestc = [];

  for i = 1:k
    val = data.rowheaders{idx(i)};
    found = 0;
    for j = 1:length(bestv)
      if isequal(bestv{j}, val)
        found = 1;
        break;
      end
    end
    if found
      bestc(j) = bestc(j) + 1;
    else
      bestv{end+1} = val;
      bestc(end+1) = 1;
    end
  end

  % Get the index of the first maximum element.
  [bcm, bci] = max(bestc);

  % Return the corresponding value.
  group = bestv{bci(1)};

end
