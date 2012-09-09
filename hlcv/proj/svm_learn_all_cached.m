function models = svm_learn_all_cached(anns, train_ids, tag, svm_params)

  cfn = build_cache_fn(tag, 'models');

  if exist(cfn, 'file')
    load(cfn);
  else
    [X, Y] = prepare_svm_data(anns, train_ids, svm_params);
    models = svm_learn_all(X, Y, svm_params);
    save(cfn, 'models');
  end
  
end
