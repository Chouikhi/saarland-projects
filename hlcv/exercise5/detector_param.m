function PARAMS = detector_param()
  PARAMS.obj_width = 64;
  PARAMS.obj_heigth = 128;
  
  %PARAMS.cellsize = 8;
  PARAMS.cellsize = 16;

  PARAMS.num_cells_width = floor(PARAMS.obj_width / PARAMS.cellsize);
  PARAMS.num_cells_height = floor(PARAMS.obj_heigth / PARAMS.cellsize);

  assert(PARAMS.obj_width == PARAMS.cellsize * PARAMS.num_cells_width);
  assert(PARAMS.obj_heigth == PARAMS.cellsize * PARAMS.num_cells_height);

  PARAMS.hist_numbins = 8;

  PARAMS.hist_min = -pi/2;
  PARAMS.hist_max = pi/2;

  PARAMS.hist_binsize = (PARAMS.hist_max - PARAMS.hist_min) / PARAMS.hist_numbins;

  PARAMS.eps = 10;
  PARAMS.C = 10;

