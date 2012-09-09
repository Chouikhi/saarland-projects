% given a image filename and a tag return a cache-file name
function cfn = build_cache_fn(fn, tag)

  cfn = sprintf('cache/%s_%s.mat', fn, tag);

end
