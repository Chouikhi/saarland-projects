% Extract patches from all images inside a directory.
function patches = extract_all_patches(anns, ann_ids, ism_params)

  global train_dir;

  cfn = build_cache_fn('all', 'patches');

  patches = {};
  if exist(cfn, 'file')
    load(cfn)
  else
    for ann_n = 1:length(ann_ids)
      ann = anns(ann_ids(ann_n));
      img_name = fullfile(train_dir, ann.image.name);
      cpatches = extract_file_patches(img_name, ann.annorect, ism_params);
      patches = [patches; cpatches];
    end

    save(cfn, 'patches');
  end

end
