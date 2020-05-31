instanceDataGeneration = function(data) {
  train_ind1 = sample(seq_len(nrow(data)), size = floor(0.5 * nrow(data)))
  train1 = data[train_ind1,]
  train2 = data[-train_ind1,]

  # split train1
  train_ind2 = sample(seq_len(nrow(train1)), size = floor(0.5 * nrow(train1)))
  train3 = train1[train_ind2,]
  train4 = train1[-train_ind2,]

  # split train2
  train_ind3 = sample(seq_len(nrow(train2)), size = floor(0.5 * nrow(train2)))
  train5 = train2[train_ind3,]
  train6 = train2[-train_ind3,]

  # split train3
  train_ind4 = sample(seq_len(nrow(train3)), size = floor(0.5 * nrow(train3)))
  inst1 = train3[train_ind4,]
  inst2 = train3[-train_ind4,]

  # split train4
  train_ind5 = sample(seq_len(nrow(train4)), size = floor(0.5 * nrow(train4)))
  inst3 = train4[train_ind5,]
  inst4 = train4[-train_ind5,]

  # split train5
  train_ind6 = sample(seq_len(nrow(train5)), size = floor(0.5 * nrow(train5)))
  inst5 = train5[train_ind6,]
  inst6 = train5[-train_ind6,]

  # split train6
  train_ind7 = sample(seq_len(nrow(train6)), size = floor(0.5 * nrow(train6)))
  inst7 = train6[train_ind7,]
  inst8 = train6[-train_ind7,]

  return(list(inst1,inst2,inst3,inst4,inst5,inst6,inst7,inst8))
}
