{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Sponge.NonPipelined.SHAKE128B
  ( sponge,
    pad,
    squeezeSlice,
  )
where

import AXI4Stream
import Clash.Prelude hiding (permute, tlast)
import Sponge.NonPipelined (Phase (..), SeenTLAST (..), State (..), complementAt)

type RateBeats = 168 -- 168 bytes = 1344 bits (SHAKE128 rate)

type PadBeats = 168

-- | Padding function for byte-stream SHAKE128
-- XORs domain separator (0x1F) and final pad bit (0x80).
-- Pattern-matched on all indices for predictable synthesis, mirroring SHAKE128 pad style.
pad :: Index PadBeats -> BitVector 1600 -> BitVector 1600
pad 0 =
  complementAt 256
    . complementAt 1587
    . complementAt 1588
    . complementAt 1589
    . complementAt 1590
    . complementAt 1591
pad 1 =
  complementAt 256
    . complementAt 1579
    . complementAt 1580
    . complementAt 1581
    . complementAt 1582
    . complementAt 1583
pad 2 =
  complementAt 256
    . complementAt 1571
    . complementAt 1572
    . complementAt 1573
    . complementAt 1574
    . complementAt 1575
pad 3 =
  complementAt 256
    . complementAt 1563
    . complementAt 1564
    . complementAt 1565
    . complementAt 1566
    . complementAt 1567
pad 4 =
  complementAt 256
    . complementAt 1555
    . complementAt 1556
    . complementAt 1557
    . complementAt 1558
    . complementAt 1559
pad 5 =
  complementAt 256
    . complementAt 1547
    . complementAt 1548
    . complementAt 1549
    . complementAt 1550
    . complementAt 1551
pad 6 =
  complementAt 256
    . complementAt 1539
    . complementAt 1540
    . complementAt 1541
    . complementAt 1542
    . complementAt 1543
pad 7 =
  complementAt 256
    . complementAt 1531
    . complementAt 1532
    . complementAt 1533
    . complementAt 1534
    . complementAt 1535
pad 8 =
  complementAt 256
    . complementAt 1523
    . complementAt 1524
    . complementAt 1525
    . complementAt 1526
    . complementAt 1527
pad 9 =
  complementAt 256
    . complementAt 1515
    . complementAt 1516
    . complementAt 1517
    . complementAt 1518
    . complementAt 1519
pad 10 =
  complementAt 256
    . complementAt 1507
    . complementAt 1508
    . complementAt 1509
    . complementAt 1510
    . complementAt 1511
pad 11 =
  complementAt 256
    . complementAt 1499
    . complementAt 1500
    . complementAt 1501
    . complementAt 1502
    . complementAt 1503
pad 12 =
  complementAt 256
    . complementAt 1491
    . complementAt 1492
    . complementAt 1493
    . complementAt 1494
    . complementAt 1495
pad 13 =
  complementAt 256
    . complementAt 1483
    . complementAt 1484
    . complementAt 1485
    . complementAt 1486
    . complementAt 1487
pad 14 =
  complementAt 256
    . complementAt 1475
    . complementAt 1476
    . complementAt 1477
    . complementAt 1478
    . complementAt 1479
pad 15 =
  complementAt 256
    . complementAt 1467
    . complementAt 1468
    . complementAt 1469
    . complementAt 1470
    . complementAt 1471
pad 16 =
  complementAt 256
    . complementAt 1459
    . complementAt 1460
    . complementAt 1461
    . complementAt 1462
    . complementAt 1463
pad 17 =
  complementAt 256
    . complementAt 1451
    . complementAt 1452
    . complementAt 1453
    . complementAt 1454
    . complementAt 1455
pad 18 =
  complementAt 256
    . complementAt 1443
    . complementAt 1444
    . complementAt 1445
    . complementAt 1446
    . complementAt 1447
pad 19 =
  complementAt 256
    . complementAt 1435
    . complementAt 1436
    . complementAt 1437
    . complementAt 1438
    . complementAt 1439
pad 20 =
  complementAt 256
    . complementAt 1427
    . complementAt 1428
    . complementAt 1429
    . complementAt 1430
    . complementAt 1431
pad 21 =
  complementAt 256
    . complementAt 1419
    . complementAt 1420
    . complementAt 1421
    . complementAt 1422
    . complementAt 1423
pad 22 =
  complementAt 256
    . complementAt 1411
    . complementAt 1412
    . complementAt 1413
    . complementAt 1414
    . complementAt 1415
pad 23 =
  complementAt 256
    . complementAt 1403
    . complementAt 1404
    . complementAt 1405
    . complementAt 1406
    . complementAt 1407
pad 24 =
  complementAt 256
    . complementAt 1395
    . complementAt 1396
    . complementAt 1397
    . complementAt 1398
    . complementAt 1399
pad 25 =
  complementAt 256
    . complementAt 1387
    . complementAt 1388
    . complementAt 1389
    . complementAt 1390
    . complementAt 1391
pad 26 =
  complementAt 256
    . complementAt 1379
    . complementAt 1380
    . complementAt 1381
    . complementAt 1382
    . complementAt 1383
pad 27 =
  complementAt 256
    . complementAt 1371
    . complementAt 1372
    . complementAt 1373
    . complementAt 1374
    . complementAt 1375
pad 28 =
  complementAt 256
    . complementAt 1363
    . complementAt 1364
    . complementAt 1365
    . complementAt 1366
    . complementAt 1367
pad 29 =
  complementAt 256
    . complementAt 1355
    . complementAt 1356
    . complementAt 1357
    . complementAt 1358
    . complementAt 1359
pad 30 =
  complementAt 256
    . complementAt 1347
    . complementAt 1348
    . complementAt 1349
    . complementAt 1350
    . complementAt 1351
pad 31 =
  complementAt 256
    . complementAt 1339
    . complementAt 1340
    . complementAt 1341
    . complementAt 1342
    . complementAt 1343
pad 32 =
  complementAt 256
    . complementAt 1331
    . complementAt 1332
    . complementAt 1333
    . complementAt 1334
    . complementAt 1335
pad 33 =
  complementAt 256
    . complementAt 1323
    . complementAt 1324
    . complementAt 1325
    . complementAt 1326
    . complementAt 1327
pad 34 =
  complementAt 256
    . complementAt 1315
    . complementAt 1316
    . complementAt 1317
    . complementAt 1318
    . complementAt 1319
pad 35 =
  complementAt 256
    . complementAt 1307
    . complementAt 1308
    . complementAt 1309
    . complementAt 1310
    . complementAt 1311
pad 36 =
  complementAt 256
    . complementAt 1299
    . complementAt 1300
    . complementAt 1301
    . complementAt 1302
    . complementAt 1303
pad 37 =
  complementAt 256
    . complementAt 1291
    . complementAt 1292
    . complementAt 1293
    . complementAt 1294
    . complementAt 1295
pad 38 =
  complementAt 256
    . complementAt 1283
    . complementAt 1284
    . complementAt 1285
    . complementAt 1286
    . complementAt 1287
pad 39 =
  complementAt 256
    . complementAt 1275
    . complementAt 1276
    . complementAt 1277
    . complementAt 1278
    . complementAt 1279
pad 40 =
  complementAt 256
    . complementAt 1267
    . complementAt 1268
    . complementAt 1269
    . complementAt 1270
    . complementAt 1271
pad 41 =
  complementAt 256
    . complementAt 1259
    . complementAt 1260
    . complementAt 1261
    . complementAt 1262
    . complementAt 1263
pad 42 =
  complementAt 256
    . complementAt 1251
    . complementAt 1252
    . complementAt 1253
    . complementAt 1254
    . complementAt 1255
pad 43 =
  complementAt 256
    . complementAt 1243
    . complementAt 1244
    . complementAt 1245
    . complementAt 1246
    . complementAt 1247
pad 44 =
  complementAt 256
    . complementAt 1235
    . complementAt 1236
    . complementAt 1237
    . complementAt 1238
    . complementAt 1239
pad 45 =
  complementAt 256
    . complementAt 1227
    . complementAt 1228
    . complementAt 1229
    . complementAt 1230
    . complementAt 1231
pad 46 =
  complementAt 256
    . complementAt 1219
    . complementAt 1220
    . complementAt 1221
    . complementAt 1222
    . complementAt 1223
pad 47 =
  complementAt 256
    . complementAt 1211
    . complementAt 1212
    . complementAt 1213
    . complementAt 1214
    . complementAt 1215
pad 48 =
  complementAt 256
    . complementAt 1203
    . complementAt 1204
    . complementAt 1205
    . complementAt 1206
    . complementAt 1207
pad 49 =
  complementAt 256
    . complementAt 1195
    . complementAt 1196
    . complementAt 1197
    . complementAt 1198
    . complementAt 1199
pad 50 =
  complementAt 256
    . complementAt 1187
    . complementAt 1188
    . complementAt 1189
    . complementAt 1190
    . complementAt 1191
pad 51 =
  complementAt 256
    . complementAt 1179
    . complementAt 1180
    . complementAt 1181
    . complementAt 1182
    . complementAt 1183
pad 52 =
  complementAt 256
    . complementAt 1171
    . complementAt 1172
    . complementAt 1173
    . complementAt 1174
    . complementAt 1175
pad 53 =
  complementAt 256
    . complementAt 1163
    . complementAt 1164
    . complementAt 1165
    . complementAt 1166
    . complementAt 1167
pad 54 =
  complementAt 256
    . complementAt 1155
    . complementAt 1156
    . complementAt 1157
    . complementAt 1158
    . complementAt 1159
pad 55 =
  complementAt 256
    . complementAt 1147
    . complementAt 1148
    . complementAt 1149
    . complementAt 1150
    . complementAt 1151
pad 56 =
  complementAt 256
    . complementAt 1139
    . complementAt 1140
    . complementAt 1141
    . complementAt 1142
    . complementAt 1143
pad 57 =
  complementAt 256
    . complementAt 1131
    . complementAt 1132
    . complementAt 1133
    . complementAt 1134
    . complementAt 1135
pad 58 =
  complementAt 256
    . complementAt 1123
    . complementAt 1124
    . complementAt 1125
    . complementAt 1126
    . complementAt 1127
pad 59 =
  complementAt 256
    . complementAt 1115
    . complementAt 1116
    . complementAt 1117
    . complementAt 1118
    . complementAt 1119
pad 60 =
  complementAt 256
    . complementAt 1107
    . complementAt 1108
    . complementAt 1109
    . complementAt 1110
    . complementAt 1111
pad 61 =
  complementAt 256
    . complementAt 1099
    . complementAt 1100
    . complementAt 1101
    . complementAt 1102
    . complementAt 1103
pad 62 =
  complementAt 256
    . complementAt 1091
    . complementAt 1092
    . complementAt 1093
    . complementAt 1094
    . complementAt 1095
pad 63 =
  complementAt 256
    . complementAt 1083
    . complementAt 1084
    . complementAt 1085
    . complementAt 1086
    . complementAt 1087
pad 64 =
  complementAt 256
    . complementAt 1075
    . complementAt 1076
    . complementAt 1077
    . complementAt 1078
    . complementAt 1079
pad 65 =
  complementAt 256
    . complementAt 1067
    . complementAt 1068
    . complementAt 1069
    . complementAt 1070
    . complementAt 1071
pad 66 =
  complementAt 256
    . complementAt 1059
    . complementAt 1060
    . complementAt 1061
    . complementAt 1062
    . complementAt 1063
pad 67 =
  complementAt 256
    . complementAt 1051
    . complementAt 1052
    . complementAt 1053
    . complementAt 1054
    . complementAt 1055
pad 68 =
  complementAt 256
    . complementAt 1043
    . complementAt 1044
    . complementAt 1045
    . complementAt 1046
    . complementAt 1047
pad 69 =
  complementAt 256
    . complementAt 1035
    . complementAt 1036
    . complementAt 1037
    . complementAt 1038
    . complementAt 1039
pad 70 =
  complementAt 256
    . complementAt 1027
    . complementAt 1028
    . complementAt 1029
    . complementAt 1030
    . complementAt 1031
pad 71 =
  complementAt 256
    . complementAt 1019
    . complementAt 1020
    . complementAt 1021
    . complementAt 1022
    . complementAt 1023
pad 72 =
  complementAt 256
    . complementAt 1011
    . complementAt 1012
    . complementAt 1013
    . complementAt 1014
    . complementAt 1015
pad 73 =
  complementAt 256
    . complementAt 1003
    . complementAt 1004
    . complementAt 1005
    . complementAt 1006
    . complementAt 1007
pad 74 =
  complementAt 256
    . complementAt 995
    . complementAt 996
    . complementAt 997
    . complementAt 998
    . complementAt 999
pad 75 =
  complementAt 256
    . complementAt 987
    . complementAt 988
    . complementAt 989
    . complementAt 990
    . complementAt 991
pad 76 =
  complementAt 256
    . complementAt 979
    . complementAt 980
    . complementAt 981
    . complementAt 982
    . complementAt 983
pad 77 =
  complementAt 256
    . complementAt 971
    . complementAt 972
    . complementAt 973
    . complementAt 974
    . complementAt 975
pad 78 =
  complementAt 256
    . complementAt 963
    . complementAt 964
    . complementAt 965
    . complementAt 966
    . complementAt 967
pad 79 =
  complementAt 256
    . complementAt 955
    . complementAt 956
    . complementAt 957
    . complementAt 958
    . complementAt 959
pad 80 =
  complementAt 256
    . complementAt 947
    . complementAt 948
    . complementAt 949
    . complementAt 950
    . complementAt 951
pad 81 =
  complementAt 256
    . complementAt 939
    . complementAt 940
    . complementAt 941
    . complementAt 942
    . complementAt 943
pad 82 =
  complementAt 256
    . complementAt 931
    . complementAt 932
    . complementAt 933
    . complementAt 934
    . complementAt 935
pad 83 =
  complementAt 256
    . complementAt 923
    . complementAt 924
    . complementAt 925
    . complementAt 926
    . complementAt 927
pad 84 =
  complementAt 256
    . complementAt 915
    . complementAt 916
    . complementAt 917
    . complementAt 918
    . complementAt 919
pad 85 =
  complementAt 256
    . complementAt 907
    . complementAt 908
    . complementAt 909
    . complementAt 910
    . complementAt 911
pad 86 =
  complementAt 256
    . complementAt 899
    . complementAt 900
    . complementAt 901
    . complementAt 902
    . complementAt 903
pad 87 =
  complementAt 256
    . complementAt 891
    . complementAt 892
    . complementAt 893
    . complementAt 894
    . complementAt 895
pad 88 =
  complementAt 256
    . complementAt 883
    . complementAt 884
    . complementAt 885
    . complementAt 886
    . complementAt 887
pad 89 =
  complementAt 256
    . complementAt 875
    . complementAt 876
    . complementAt 877
    . complementAt 878
    . complementAt 879
pad 90 =
  complementAt 256
    . complementAt 867
    . complementAt 868
    . complementAt 869
    . complementAt 870
    . complementAt 871
pad 91 =
  complementAt 256
    . complementAt 859
    . complementAt 860
    . complementAt 861
    . complementAt 862
    . complementAt 863
pad 92 =
  complementAt 256
    . complementAt 851
    . complementAt 852
    . complementAt 853
    . complementAt 854
    . complementAt 855
pad 93 =
  complementAt 256
    . complementAt 843
    . complementAt 844
    . complementAt 845
    . complementAt 846
    . complementAt 847
pad 94 =
  complementAt 256
    . complementAt 835
    . complementAt 836
    . complementAt 837
    . complementAt 838
    . complementAt 839
pad 95 =
  complementAt 256
    . complementAt 827
    . complementAt 828
    . complementAt 829
    . complementAt 830
    . complementAt 831
pad 96 =
  complementAt 256
    . complementAt 819
    . complementAt 820
    . complementAt 821
    . complementAt 822
    . complementAt 823
pad 97 =
  complementAt 256
    . complementAt 811
    . complementAt 812
    . complementAt 813
    . complementAt 814
    . complementAt 815
pad 98 =
  complementAt 256
    . complementAt 803
    . complementAt 804
    . complementAt 805
    . complementAt 806
    . complementAt 807
pad 99 =
  complementAt 256
    . complementAt 795
    . complementAt 796
    . complementAt 797
    . complementAt 798
    . complementAt 799
pad 100 =
  complementAt 256
    . complementAt 787
    . complementAt 788
    . complementAt 789
    . complementAt 790
    . complementAt 791
pad 101 =
  complementAt 256
    . complementAt 779
    . complementAt 780
    . complementAt 781
    . complementAt 782
    . complementAt 783
pad 102 =
  complementAt 256
    . complementAt 771
    . complementAt 772
    . complementAt 773
    . complementAt 774
    . complementAt 775
pad 103 =
  complementAt 256
    . complementAt 763
    . complementAt 764
    . complementAt 765
    . complementAt 766
    . complementAt 767
pad 104 =
  complementAt 256
    . complementAt 755
    . complementAt 756
    . complementAt 757
    . complementAt 758
    . complementAt 759
pad 105 =
  complementAt 256
    . complementAt 747
    . complementAt 748
    . complementAt 749
    . complementAt 750
    . complementAt 751
pad 106 =
  complementAt 256
    . complementAt 739
    . complementAt 740
    . complementAt 741
    . complementAt 742
    . complementAt 743
pad 107 =
  complementAt 256
    . complementAt 731
    . complementAt 732
    . complementAt 733
    . complementAt 734
    . complementAt 735
pad 108 =
  complementAt 256
    . complementAt 723
    . complementAt 724
    . complementAt 725
    . complementAt 726
    . complementAt 727
pad 109 =
  complementAt 256
    . complementAt 715
    . complementAt 716
    . complementAt 717
    . complementAt 718
    . complementAt 719
pad 110 =
  complementAt 256
    . complementAt 707
    . complementAt 708
    . complementAt 709
    . complementAt 710
    . complementAt 711
pad 111 =
  complementAt 256
    . complementAt 699
    . complementAt 700
    . complementAt 701
    . complementAt 702
    . complementAt 703
pad 112 =
  complementAt 256
    . complementAt 691
    . complementAt 692
    . complementAt 693
    . complementAt 694
    . complementAt 695
pad 113 =
  complementAt 256
    . complementAt 683
    . complementAt 684
    . complementAt 685
    . complementAt 686
    . complementAt 687
pad 114 =
  complementAt 256
    . complementAt 675
    . complementAt 676
    . complementAt 677
    . complementAt 678
    . complementAt 679
pad 115 =
  complementAt 256
    . complementAt 667
    . complementAt 668
    . complementAt 669
    . complementAt 670
    . complementAt 671
pad 116 =
  complementAt 256
    . complementAt 659
    . complementAt 660
    . complementAt 661
    . complementAt 662
    . complementAt 663
pad 117 =
  complementAt 256
    . complementAt 651
    . complementAt 652
    . complementAt 653
    . complementAt 654
    . complementAt 655
pad 118 =
  complementAt 256
    . complementAt 643
    . complementAt 644
    . complementAt 645
    . complementAt 646
    . complementAt 647
pad 119 =
  complementAt 256
    . complementAt 635
    . complementAt 636
    . complementAt 637
    . complementAt 638
    . complementAt 639
pad 120 =
  complementAt 256
    . complementAt 627
    . complementAt 628
    . complementAt 629
    . complementAt 630
    . complementAt 631
pad 121 =
  complementAt 256
    . complementAt 619
    . complementAt 620
    . complementAt 621
    . complementAt 622
    . complementAt 623
pad 122 =
  complementAt 256
    . complementAt 611
    . complementAt 612
    . complementAt 613
    . complementAt 614
    . complementAt 615
pad 123 =
  complementAt 256
    . complementAt 603
    . complementAt 604
    . complementAt 605
    . complementAt 606
    . complementAt 607
pad 124 =
  complementAt 256
    . complementAt 595
    . complementAt 596
    . complementAt 597
    . complementAt 598
    . complementAt 599
pad 125 =
  complementAt 256
    . complementAt 587
    . complementAt 588
    . complementAt 589
    . complementAt 590
    . complementAt 591
pad 126 =
  complementAt 256
    . complementAt 579
    . complementAt 580
    . complementAt 581
    . complementAt 582
    . complementAt 583
pad 127 =
  complementAt 256
    . complementAt 571
    . complementAt 572
    . complementAt 573
    . complementAt 574
    . complementAt 575
pad 128 =
  complementAt 256
    . complementAt 563
    . complementAt 564
    . complementAt 565
    . complementAt 566
    . complementAt 567
pad 129 =
  complementAt 256
    . complementAt 555
    . complementAt 556
    . complementAt 557
    . complementAt 558
    . complementAt 559
pad 130 =
  complementAt 256
    . complementAt 547
    . complementAt 548
    . complementAt 549
    . complementAt 550
    . complementAt 551
pad 131 =
  complementAt 256
    . complementAt 539
    . complementAt 540
    . complementAt 541
    . complementAt 542
    . complementAt 543
pad 132 =
  complementAt 256
    . complementAt 531
    . complementAt 532
    . complementAt 533
    . complementAt 534
    . complementAt 535
pad 133 =
  complementAt 256
    . complementAt 523
    . complementAt 524
    . complementAt 525
    . complementAt 526
    . complementAt 527
pad 134 =
  complementAt 256
    . complementAt 515
    . complementAt 516
    . complementAt 517
    . complementAt 518
    . complementAt 519
pad 135 =
  complementAt 256
    . complementAt 507
    . complementAt 508
    . complementAt 509
    . complementAt 510
    . complementAt 511
pad 136 =
  complementAt 256
    . complementAt 499
    . complementAt 500
    . complementAt 501
    . complementAt 502
    . complementAt 503
pad 137 =
  complementAt 256
    . complementAt 491
    . complementAt 492
    . complementAt 493
    . complementAt 494
    . complementAt 495
pad 138 =
  complementAt 256
    . complementAt 483
    . complementAt 484
    . complementAt 485
    . complementAt 486
    . complementAt 487
pad 139 =
  complementAt 256
    . complementAt 475
    . complementAt 476
    . complementAt 477
    . complementAt 478
    . complementAt 479
pad 140 =
  complementAt 256
    . complementAt 467
    . complementAt 468
    . complementAt 469
    . complementAt 470
    . complementAt 471
pad 141 =
  complementAt 256
    . complementAt 459
    . complementAt 460
    . complementAt 461
    . complementAt 462
    . complementAt 463
pad 142 =
  complementAt 256
    . complementAt 451
    . complementAt 452
    . complementAt 453
    . complementAt 454
    . complementAt 455
pad 143 =
  complementAt 256
    . complementAt 443
    . complementAt 444
    . complementAt 445
    . complementAt 446
    . complementAt 447
pad 144 =
  complementAt 256
    . complementAt 435
    . complementAt 436
    . complementAt 437
    . complementAt 438
    . complementAt 439
pad 145 =
  complementAt 256
    . complementAt 427
    . complementAt 428
    . complementAt 429
    . complementAt 430
    . complementAt 431
pad 146 =
  complementAt 256
    . complementAt 419
    . complementAt 420
    . complementAt 421
    . complementAt 422
    . complementAt 423
pad 147 =
  complementAt 256
    . complementAt 411
    . complementAt 412
    . complementAt 413
    . complementAt 414
    . complementAt 415
pad 148 =
  complementAt 256
    . complementAt 403
    . complementAt 404
    . complementAt 405
    . complementAt 406
    . complementAt 407
pad 149 =
  complementAt 256
    . complementAt 395
    . complementAt 396
    . complementAt 397
    . complementAt 398
    . complementAt 399
pad 150 =
  complementAt 256
    . complementAt 387
    . complementAt 388
    . complementAt 389
    . complementAt 390
    . complementAt 391
pad 151 =
  complementAt 256
    . complementAt 379
    . complementAt 380
    . complementAt 381
    . complementAt 382
    . complementAt 383
pad 152 =
  complementAt 256
    . complementAt 371
    . complementAt 372
    . complementAt 373
    . complementAt 374
    . complementAt 375
pad 153 =
  complementAt 256
    . complementAt 363
    . complementAt 364
    . complementAt 365
    . complementAt 366
    . complementAt 367
pad 154 =
  complementAt 256
    . complementAt 355
    . complementAt 356
    . complementAt 357
    . complementAt 358
    . complementAt 359
pad 155 =
  complementAt 256
    . complementAt 347
    . complementAt 348
    . complementAt 349
    . complementAt 350
    . complementAt 351
pad 156 =
  complementAt 256
    . complementAt 339
    . complementAt 340
    . complementAt 341
    . complementAt 342
    . complementAt 343
pad 157 =
  complementAt 256
    . complementAt 331
    . complementAt 332
    . complementAt 333
    . complementAt 334
    . complementAt 335
pad 158 =
  complementAt 256
    . complementAt 323
    . complementAt 324
    . complementAt 325
    . complementAt 326
    . complementAt 327
pad 159 =
  complementAt 256
    . complementAt 315
    . complementAt 316
    . complementAt 317
    . complementAt 318
    . complementAt 319
pad 160 =
  complementAt 256
    . complementAt 307
    . complementAt 308
    . complementAt 309
    . complementAt 310
    . complementAt 311
pad 161 =
  complementAt 256
    . complementAt 299
    . complementAt 300
    . complementAt 301
    . complementAt 302
    . complementAt 303
pad 162 =
  complementAt 256
    . complementAt 291
    . complementAt 292
    . complementAt 293
    . complementAt 294
    . complementAt 295
pad 163 =
  complementAt 256
    . complementAt 283
    . complementAt 284
    . complementAt 285
    . complementAt 286
    . complementAt 287
pad 164 =
  complementAt 256
    . complementAt 275
    . complementAt 276
    . complementAt 277
    . complementAt 278
    . complementAt 279
pad 165 =
  complementAt 256
    . complementAt 267
    . complementAt 268
    . complementAt 269
    . complementAt 270
    . complementAt 271
pad 166 =
  complementAt 256
    . complementAt 259
    . complementAt 260
    . complementAt 261
    . complementAt 262
    . complementAt 263
pad _ =
  complementAt 256
    . complementAt 1595
    . complementAt 1596
    . complementAt 1597
    . complementAt 1598
    . complementAt 1599

-- | Squeeze operation: extract one 8-bit byte from state
squeezeSlice :: KnownNat n => Index n -> BitVector 1600 -> BitVector 8
squeezeSlice counter state =
  let -- Each counter value extracts 8 bits (from MSB side)
      counterU :: Unsigned 16
      counterU = fromIntegral counter
      bitPos = fromIntegral (counterU * 8) :: Int
      -- Shift right to align the target byte to LSB position, then truncate
   in truncateB (state `shiftR` (1592 - bitPos))

-- | Main sponge construction for byte-stream SHAKE128
sponge ::
  forall dom.
  (HiddenClockResetEnable dom, KnownDomain dom) =>
  (Index 24 -> BitVector 1600 -> BitVector 1600) ->
  Signal dom (AXI4Stream 8, Bool, Bool) ->
  Signal dom (AXI4Stream 8, Bool)
sponge permuteFn input = mealy step (State (Absorb 0) 0) input
  where
    step ::
      State RateBeats (Index RateBeats) ->
      (AXI4Stream 8, Bool, Bool) ->
      (State RateBeats (Index RateBeats), (AXI4Stream 8, Bool))
    step (State phase state) (inputStream, tready, flush) =
      case phase of
        Absorb counter ->
          absorb pad xorByte counter state inputStream flush
        Permute permuteCounter seenTLAST ->
          permute permuteFn pad permuteCounter seenTLAST state tready
        Squeeze squeezeCounter ->
          squeeze squeezeCounter state tready

    -- XOR an 8-bit byte into the state at the given beat index
    xorByte :: BitVector 1600 -> BitVector 8 -> Index RateBeats -> BitVector 1600
    xorByte state byte beatIndex =
      let -- Calculate bit position from MSB (beat 0 starts at bit 1599)
          beatU :: Unsigned 16
          beatU = fromIntegral beatIndex
          bitPos = fromIntegral (beatU * 8) :: Int
          shiftAmt = 1592 - bitPos
          -- Extract current byte, XOR it, then reconstruct state
          currentByte = truncateB (state `shiftR` shiftAmt) :: BitVector 8
          xoredByte = currentByte `xor` byte
          -- Create mask to clear the target byte, then OR in the new value
          mask = complement (resize (maxBound :: BitVector 8) `shiftL` shiftAmt) :: BitVector 1600
          cleared = state .&. mask
          newValue = resize xoredByte `shiftL` shiftAmt
       in cleared .|. newValue

    squeeze :: Index RateBeats -> BitVector 1600 -> Bool -> (State RateBeats (Index RateBeats), (AXI4Stream 8, Bool))
    squeeze counter state tready' =
      let outByte = squeezeSlice counter state
          outStream = AXI4Stream {tdata = outByte, tvalid = True, tlast = False}
          nextCounter = if tready' then counter + 1 else counter
       in if tready'
            then
              if counter == maxBound
                then (State (Permute 0 SeenTLASTAndPadded) state, (outStream, False))
                else (State (Squeeze nextCounter) state, (outStream, False))
            else (State (Squeeze counter) state, (outStream, False))

    -- | Absorb phase for byte-stream (8-bit)
    absorb ::
      (Index RateBeats -> BitVector 1600 -> BitVector 1600) ->
      (BitVector 1600 -> BitVector 8 -> Index RateBeats -> BitVector 1600) ->
      Index RateBeats ->
      BitVector 1600 ->
      AXI4Stream 8 ->
      Bool ->
      (State RateBeats (Index RateBeats), (AXI4Stream 8, Bool))
    absorb pad' xorFn counter state' inputMsg flush
      | flush && counter == 0 =
          let padded = pad' maxAbsorbBeat state'
           in (State (Permute 0 SeenTLASTAndPadded) padded, (idleAXI4Stream, False))
      | not (tvalid inputMsg) = (State (Absorb counter) state', (idleAXI4Stream, True))
      | tlast inputMsg && counter < maxAbsorbBeat =
          let state'' = xorFn state' (tdata inputMsg) counter
              padded = pad' counter state''
           in (State (Permute 0 SeenTLASTAndPadded) padded, (idleAXI4Stream, False))
      | tlast inputMsg && otherwise =
          let state'' = xorFn state' (tdata inputMsg) counter
           in (State (Permute 0 SeenTLASTNotPadded) state'', (idleAXI4Stream, False))
      | counter < maxAbsorbBeat =
          let state'' = xorFn state' (tdata inputMsg) counter
           in (State (Absorb (counter + 1)) state'', (idleAXI4Stream, True))
      | otherwise =
          let state'' = xorFn state' (tdata inputMsg) counter
           in (State (Permute 0 NotSeenTLAST) state'', (idleAXI4Stream, False))
      where
        maxAbsorbBeat = maxBound

    -- | Permute phase for byte-stream (8-bit output)
    permute ::
      (Index 24 -> BitVector 1600 -> BitVector 1600) ->
      (Index RateBeats -> BitVector 1600 -> BitVector 1600) ->
      Index 24 ->
      SeenTLAST ->
      BitVector 1600 ->
      Bool ->
      (State RateBeats (Index RateBeats), (AXI4Stream 8, Bool))
    permute permModule pad' counter seenTLAST state' tready' =
      let state'' = permModule counter state'
       in if counter == 23
            then case seenTLAST of
              SeenTLASTAndPadded ->
                let outByte = squeezeSlice (0 :: Index RateBeats) state''
                    outStream = AXI4Stream {tdata = outByte, tvalid = True, tlast = False}
                    nextState = if tready' then State (Squeeze 1) state'' else State (Squeeze 0) state''
                 in (nextState, (outStream, False))
              SeenTLASTNotPadded ->
                let padded = pad' maxBound state''
                 in (State (Permute 0 SeenTLASTAndPadded) padded, (idleAXI4Stream, False))
              NotSeenTLAST -> (State (Absorb 0) state'', (idleAXI4Stream, True))
            else (State (Permute (counter + 1) seenTLAST) state'', (idleAXI4Stream, False))
