% Tutorial 5 Exercise 1 Subtask 1.2
% Iskren Ivov Chernev s9ischer@stud.uni-saarland.de

function prec = test_knn(data, k, trainSize, testSize, distf)
  
  assert(size(data.data, 1) >= trainSize + testSize, 'Not enough data provided');

  correct_count = 0;
  for i = 1:testSize
    res = knn(data, k, trainSize, data.data(trainSize + i, :), distf);
    if isequal(res, data.rowheaders{trainSize + i})
      correct_count = correct_count + 1;
    end
  end

  prec = correct_count / testSize;

end
