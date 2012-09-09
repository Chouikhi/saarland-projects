function [fn, bbox] = extract_id(id)

  global anns;
  global train_dir;

  fn = fullfile(train_dir, anns(id).image.name);
  bbox = anns(id).annorect;

end
