function set_globals(svm_params)

  global p1;
  global p2;
  global p1_bac;
  global p2_bac;

  p1_bac = p1;
  p2_bac = p2;
  p1 = svm_params.p1;
  p2 = svm_params.p2;

end
