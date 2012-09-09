% Tutorial 5 Exercise 1 Subtask 1.2
% Iskren Ivov Chernev s9ischer@stud.uni-saarland.de

function plot_knn_precision()
  ks = [1, 5, 11];
  trainSizes = [100, 200, 500, 1000, 2000, 5000, 7500];

  recalculate = 0;
  loaded = 0;

  try
    load knn_results;
    loaded = 1;
    fprintf('loaded successfully\n');
  catch
  end
  
  if not(loaded) || recalculate
    data = importdata('letter-recognition.data');
    fprintf('recalculating\n');
    dist_ma3x = calculate_knn_dist_ma3x(data, ks, trainSizes, 1000, @dist_l2);
    fprintf('saving\n');
    save knn_results dist_ma3x;
  end
  
  plot(trainSizes, dist_ma3x(1, :), 'x-', ...
      trainSizes, dist_ma3x(2, :), 'x-', ...
      trainSizes, dist_ma3x(3, :), 'x-');

  leg = cell(length(ks), 1);
  for ki = 1:length(ks)
    leg{ki} = sprintf('k = %d', ks(ki));
  end
  legend(leg, 'LOCATION', 'SouthEast');
  title('K Nearest Neighbours -- precision');
end


function dist_ma3x = calculate_knn_dist_ma3x(data, ks, trainSizes, testSize, distf)

  dist_ma3x = zeros(length(ks), length(trainSizes));
  for ki = 1:length(ks)
    for tsi = 1:length(trainSizes)
      fprintf('calculating k = %d trainSize = %d\n', ks(ki), trainSizes(tsi));
      dist_ma3x(ki, tsi) = test_knn(data, ks(ki), trainSizes(tsi), testSize, distf);
    end
  end

end
