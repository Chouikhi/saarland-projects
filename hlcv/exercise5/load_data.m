function [pos_train_list, neg_train_list, pos_test_list, neg_test_list] = load_data()

  pos_train_dir = './images/pos_train';
  neg_train_dir = './images/neg_train';
  
  pos_test_dir = './images/pos_test';
  neg_test_dir = './images/neg_test';

  pos_train_list = load_filelist(pos_train_dir);
  neg_train_list = load_filelist(neg_train_dir);

  pos_test_list = load_filelist(pos_test_dir);
  neg_test_list = load_filelist(neg_test_dir);


  