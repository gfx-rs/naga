
struct WStruct {
    uint arr[512];
    int atom;
    int atom_arr[8][8];
};

groupshared WStruct w_mem;
RWByteAddressBuffer output : register(u0);

[numthreads(1, 1, 1)]
void main(uint3 __global_invocation_id : SV_DispatchThreadID)
{
    if (all(__global_invocation_id == uint3(0u, 0u, 0u))) {
        w_mem = (WStruct)0;
    }
    GroupMemoryBarrierWithGroupSync();
    uint _expr3[512] = w_mem.arr;
    {
        uint _value2[512] = _expr3;
        output.Store(0, asuint(_value2[0]));
        output.Store(4, asuint(_value2[1]));
        output.Store(8, asuint(_value2[2]));
        output.Store(12, asuint(_value2[3]));
        output.Store(16, asuint(_value2[4]));
        output.Store(20, asuint(_value2[5]));
        output.Store(24, asuint(_value2[6]));
        output.Store(28, asuint(_value2[7]));
        output.Store(32, asuint(_value2[8]));
        output.Store(36, asuint(_value2[9]));
        output.Store(40, asuint(_value2[10]));
        output.Store(44, asuint(_value2[11]));
        output.Store(48, asuint(_value2[12]));
        output.Store(52, asuint(_value2[13]));
        output.Store(56, asuint(_value2[14]));
        output.Store(60, asuint(_value2[15]));
        output.Store(64, asuint(_value2[16]));
        output.Store(68, asuint(_value2[17]));
        output.Store(72, asuint(_value2[18]));
        output.Store(76, asuint(_value2[19]));
        output.Store(80, asuint(_value2[20]));
        output.Store(84, asuint(_value2[21]));
        output.Store(88, asuint(_value2[22]));
        output.Store(92, asuint(_value2[23]));
        output.Store(96, asuint(_value2[24]));
        output.Store(100, asuint(_value2[25]));
        output.Store(104, asuint(_value2[26]));
        output.Store(108, asuint(_value2[27]));
        output.Store(112, asuint(_value2[28]));
        output.Store(116, asuint(_value2[29]));
        output.Store(120, asuint(_value2[30]));
        output.Store(124, asuint(_value2[31]));
        output.Store(128, asuint(_value2[32]));
        output.Store(132, asuint(_value2[33]));
        output.Store(136, asuint(_value2[34]));
        output.Store(140, asuint(_value2[35]));
        output.Store(144, asuint(_value2[36]));
        output.Store(148, asuint(_value2[37]));
        output.Store(152, asuint(_value2[38]));
        output.Store(156, asuint(_value2[39]));
        output.Store(160, asuint(_value2[40]));
        output.Store(164, asuint(_value2[41]));
        output.Store(168, asuint(_value2[42]));
        output.Store(172, asuint(_value2[43]));
        output.Store(176, asuint(_value2[44]));
        output.Store(180, asuint(_value2[45]));
        output.Store(184, asuint(_value2[46]));
        output.Store(188, asuint(_value2[47]));
        output.Store(192, asuint(_value2[48]));
        output.Store(196, asuint(_value2[49]));
        output.Store(200, asuint(_value2[50]));
        output.Store(204, asuint(_value2[51]));
        output.Store(208, asuint(_value2[52]));
        output.Store(212, asuint(_value2[53]));
        output.Store(216, asuint(_value2[54]));
        output.Store(220, asuint(_value2[55]));
        output.Store(224, asuint(_value2[56]));
        output.Store(228, asuint(_value2[57]));
        output.Store(232, asuint(_value2[58]));
        output.Store(236, asuint(_value2[59]));
        output.Store(240, asuint(_value2[60]));
        output.Store(244, asuint(_value2[61]));
        output.Store(248, asuint(_value2[62]));
        output.Store(252, asuint(_value2[63]));
        output.Store(256, asuint(_value2[64]));
        output.Store(260, asuint(_value2[65]));
        output.Store(264, asuint(_value2[66]));
        output.Store(268, asuint(_value2[67]));
        output.Store(272, asuint(_value2[68]));
        output.Store(276, asuint(_value2[69]));
        output.Store(280, asuint(_value2[70]));
        output.Store(284, asuint(_value2[71]));
        output.Store(288, asuint(_value2[72]));
        output.Store(292, asuint(_value2[73]));
        output.Store(296, asuint(_value2[74]));
        output.Store(300, asuint(_value2[75]));
        output.Store(304, asuint(_value2[76]));
        output.Store(308, asuint(_value2[77]));
        output.Store(312, asuint(_value2[78]));
        output.Store(316, asuint(_value2[79]));
        output.Store(320, asuint(_value2[80]));
        output.Store(324, asuint(_value2[81]));
        output.Store(328, asuint(_value2[82]));
        output.Store(332, asuint(_value2[83]));
        output.Store(336, asuint(_value2[84]));
        output.Store(340, asuint(_value2[85]));
        output.Store(344, asuint(_value2[86]));
        output.Store(348, asuint(_value2[87]));
        output.Store(352, asuint(_value2[88]));
        output.Store(356, asuint(_value2[89]));
        output.Store(360, asuint(_value2[90]));
        output.Store(364, asuint(_value2[91]));
        output.Store(368, asuint(_value2[92]));
        output.Store(372, asuint(_value2[93]));
        output.Store(376, asuint(_value2[94]));
        output.Store(380, asuint(_value2[95]));
        output.Store(384, asuint(_value2[96]));
        output.Store(388, asuint(_value2[97]));
        output.Store(392, asuint(_value2[98]));
        output.Store(396, asuint(_value2[99]));
        output.Store(400, asuint(_value2[100]));
        output.Store(404, asuint(_value2[101]));
        output.Store(408, asuint(_value2[102]));
        output.Store(412, asuint(_value2[103]));
        output.Store(416, asuint(_value2[104]));
        output.Store(420, asuint(_value2[105]));
        output.Store(424, asuint(_value2[106]));
        output.Store(428, asuint(_value2[107]));
        output.Store(432, asuint(_value2[108]));
        output.Store(436, asuint(_value2[109]));
        output.Store(440, asuint(_value2[110]));
        output.Store(444, asuint(_value2[111]));
        output.Store(448, asuint(_value2[112]));
        output.Store(452, asuint(_value2[113]));
        output.Store(456, asuint(_value2[114]));
        output.Store(460, asuint(_value2[115]));
        output.Store(464, asuint(_value2[116]));
        output.Store(468, asuint(_value2[117]));
        output.Store(472, asuint(_value2[118]));
        output.Store(476, asuint(_value2[119]));
        output.Store(480, asuint(_value2[120]));
        output.Store(484, asuint(_value2[121]));
        output.Store(488, asuint(_value2[122]));
        output.Store(492, asuint(_value2[123]));
        output.Store(496, asuint(_value2[124]));
        output.Store(500, asuint(_value2[125]));
        output.Store(504, asuint(_value2[126]));
        output.Store(508, asuint(_value2[127]));
        output.Store(512, asuint(_value2[128]));
        output.Store(516, asuint(_value2[129]));
        output.Store(520, asuint(_value2[130]));
        output.Store(524, asuint(_value2[131]));
        output.Store(528, asuint(_value2[132]));
        output.Store(532, asuint(_value2[133]));
        output.Store(536, asuint(_value2[134]));
        output.Store(540, asuint(_value2[135]));
        output.Store(544, asuint(_value2[136]));
        output.Store(548, asuint(_value2[137]));
        output.Store(552, asuint(_value2[138]));
        output.Store(556, asuint(_value2[139]));
        output.Store(560, asuint(_value2[140]));
        output.Store(564, asuint(_value2[141]));
        output.Store(568, asuint(_value2[142]));
        output.Store(572, asuint(_value2[143]));
        output.Store(576, asuint(_value2[144]));
        output.Store(580, asuint(_value2[145]));
        output.Store(584, asuint(_value2[146]));
        output.Store(588, asuint(_value2[147]));
        output.Store(592, asuint(_value2[148]));
        output.Store(596, asuint(_value2[149]));
        output.Store(600, asuint(_value2[150]));
        output.Store(604, asuint(_value2[151]));
        output.Store(608, asuint(_value2[152]));
        output.Store(612, asuint(_value2[153]));
        output.Store(616, asuint(_value2[154]));
        output.Store(620, asuint(_value2[155]));
        output.Store(624, asuint(_value2[156]));
        output.Store(628, asuint(_value2[157]));
        output.Store(632, asuint(_value2[158]));
        output.Store(636, asuint(_value2[159]));
        output.Store(640, asuint(_value2[160]));
        output.Store(644, asuint(_value2[161]));
        output.Store(648, asuint(_value2[162]));
        output.Store(652, asuint(_value2[163]));
        output.Store(656, asuint(_value2[164]));
        output.Store(660, asuint(_value2[165]));
        output.Store(664, asuint(_value2[166]));
        output.Store(668, asuint(_value2[167]));
        output.Store(672, asuint(_value2[168]));
        output.Store(676, asuint(_value2[169]));
        output.Store(680, asuint(_value2[170]));
        output.Store(684, asuint(_value2[171]));
        output.Store(688, asuint(_value2[172]));
        output.Store(692, asuint(_value2[173]));
        output.Store(696, asuint(_value2[174]));
        output.Store(700, asuint(_value2[175]));
        output.Store(704, asuint(_value2[176]));
        output.Store(708, asuint(_value2[177]));
        output.Store(712, asuint(_value2[178]));
        output.Store(716, asuint(_value2[179]));
        output.Store(720, asuint(_value2[180]));
        output.Store(724, asuint(_value2[181]));
        output.Store(728, asuint(_value2[182]));
        output.Store(732, asuint(_value2[183]));
        output.Store(736, asuint(_value2[184]));
        output.Store(740, asuint(_value2[185]));
        output.Store(744, asuint(_value2[186]));
        output.Store(748, asuint(_value2[187]));
        output.Store(752, asuint(_value2[188]));
        output.Store(756, asuint(_value2[189]));
        output.Store(760, asuint(_value2[190]));
        output.Store(764, asuint(_value2[191]));
        output.Store(768, asuint(_value2[192]));
        output.Store(772, asuint(_value2[193]));
        output.Store(776, asuint(_value2[194]));
        output.Store(780, asuint(_value2[195]));
        output.Store(784, asuint(_value2[196]));
        output.Store(788, asuint(_value2[197]));
        output.Store(792, asuint(_value2[198]));
        output.Store(796, asuint(_value2[199]));
        output.Store(800, asuint(_value2[200]));
        output.Store(804, asuint(_value2[201]));
        output.Store(808, asuint(_value2[202]));
        output.Store(812, asuint(_value2[203]));
        output.Store(816, asuint(_value2[204]));
        output.Store(820, asuint(_value2[205]));
        output.Store(824, asuint(_value2[206]));
        output.Store(828, asuint(_value2[207]));
        output.Store(832, asuint(_value2[208]));
        output.Store(836, asuint(_value2[209]));
        output.Store(840, asuint(_value2[210]));
        output.Store(844, asuint(_value2[211]));
        output.Store(848, asuint(_value2[212]));
        output.Store(852, asuint(_value2[213]));
        output.Store(856, asuint(_value2[214]));
        output.Store(860, asuint(_value2[215]));
        output.Store(864, asuint(_value2[216]));
        output.Store(868, asuint(_value2[217]));
        output.Store(872, asuint(_value2[218]));
        output.Store(876, asuint(_value2[219]));
        output.Store(880, asuint(_value2[220]));
        output.Store(884, asuint(_value2[221]));
        output.Store(888, asuint(_value2[222]));
        output.Store(892, asuint(_value2[223]));
        output.Store(896, asuint(_value2[224]));
        output.Store(900, asuint(_value2[225]));
        output.Store(904, asuint(_value2[226]));
        output.Store(908, asuint(_value2[227]));
        output.Store(912, asuint(_value2[228]));
        output.Store(916, asuint(_value2[229]));
        output.Store(920, asuint(_value2[230]));
        output.Store(924, asuint(_value2[231]));
        output.Store(928, asuint(_value2[232]));
        output.Store(932, asuint(_value2[233]));
        output.Store(936, asuint(_value2[234]));
        output.Store(940, asuint(_value2[235]));
        output.Store(944, asuint(_value2[236]));
        output.Store(948, asuint(_value2[237]));
        output.Store(952, asuint(_value2[238]));
        output.Store(956, asuint(_value2[239]));
        output.Store(960, asuint(_value2[240]));
        output.Store(964, asuint(_value2[241]));
        output.Store(968, asuint(_value2[242]));
        output.Store(972, asuint(_value2[243]));
        output.Store(976, asuint(_value2[244]));
        output.Store(980, asuint(_value2[245]));
        output.Store(984, asuint(_value2[246]));
        output.Store(988, asuint(_value2[247]));
        output.Store(992, asuint(_value2[248]));
        output.Store(996, asuint(_value2[249]));
        output.Store(1000, asuint(_value2[250]));
        output.Store(1004, asuint(_value2[251]));
        output.Store(1008, asuint(_value2[252]));
        output.Store(1012, asuint(_value2[253]));
        output.Store(1016, asuint(_value2[254]));
        output.Store(1020, asuint(_value2[255]));
        output.Store(1024, asuint(_value2[256]));
        output.Store(1028, asuint(_value2[257]));
        output.Store(1032, asuint(_value2[258]));
        output.Store(1036, asuint(_value2[259]));
        output.Store(1040, asuint(_value2[260]));
        output.Store(1044, asuint(_value2[261]));
        output.Store(1048, asuint(_value2[262]));
        output.Store(1052, asuint(_value2[263]));
        output.Store(1056, asuint(_value2[264]));
        output.Store(1060, asuint(_value2[265]));
        output.Store(1064, asuint(_value2[266]));
        output.Store(1068, asuint(_value2[267]));
        output.Store(1072, asuint(_value2[268]));
        output.Store(1076, asuint(_value2[269]));
        output.Store(1080, asuint(_value2[270]));
        output.Store(1084, asuint(_value2[271]));
        output.Store(1088, asuint(_value2[272]));
        output.Store(1092, asuint(_value2[273]));
        output.Store(1096, asuint(_value2[274]));
        output.Store(1100, asuint(_value2[275]));
        output.Store(1104, asuint(_value2[276]));
        output.Store(1108, asuint(_value2[277]));
        output.Store(1112, asuint(_value2[278]));
        output.Store(1116, asuint(_value2[279]));
        output.Store(1120, asuint(_value2[280]));
        output.Store(1124, asuint(_value2[281]));
        output.Store(1128, asuint(_value2[282]));
        output.Store(1132, asuint(_value2[283]));
        output.Store(1136, asuint(_value2[284]));
        output.Store(1140, asuint(_value2[285]));
        output.Store(1144, asuint(_value2[286]));
        output.Store(1148, asuint(_value2[287]));
        output.Store(1152, asuint(_value2[288]));
        output.Store(1156, asuint(_value2[289]));
        output.Store(1160, asuint(_value2[290]));
        output.Store(1164, asuint(_value2[291]));
        output.Store(1168, asuint(_value2[292]));
        output.Store(1172, asuint(_value2[293]));
        output.Store(1176, asuint(_value2[294]));
        output.Store(1180, asuint(_value2[295]));
        output.Store(1184, asuint(_value2[296]));
        output.Store(1188, asuint(_value2[297]));
        output.Store(1192, asuint(_value2[298]));
        output.Store(1196, asuint(_value2[299]));
        output.Store(1200, asuint(_value2[300]));
        output.Store(1204, asuint(_value2[301]));
        output.Store(1208, asuint(_value2[302]));
        output.Store(1212, asuint(_value2[303]));
        output.Store(1216, asuint(_value2[304]));
        output.Store(1220, asuint(_value2[305]));
        output.Store(1224, asuint(_value2[306]));
        output.Store(1228, asuint(_value2[307]));
        output.Store(1232, asuint(_value2[308]));
        output.Store(1236, asuint(_value2[309]));
        output.Store(1240, asuint(_value2[310]));
        output.Store(1244, asuint(_value2[311]));
        output.Store(1248, asuint(_value2[312]));
        output.Store(1252, asuint(_value2[313]));
        output.Store(1256, asuint(_value2[314]));
        output.Store(1260, asuint(_value2[315]));
        output.Store(1264, asuint(_value2[316]));
        output.Store(1268, asuint(_value2[317]));
        output.Store(1272, asuint(_value2[318]));
        output.Store(1276, asuint(_value2[319]));
        output.Store(1280, asuint(_value2[320]));
        output.Store(1284, asuint(_value2[321]));
        output.Store(1288, asuint(_value2[322]));
        output.Store(1292, asuint(_value2[323]));
        output.Store(1296, asuint(_value2[324]));
        output.Store(1300, asuint(_value2[325]));
        output.Store(1304, asuint(_value2[326]));
        output.Store(1308, asuint(_value2[327]));
        output.Store(1312, asuint(_value2[328]));
        output.Store(1316, asuint(_value2[329]));
        output.Store(1320, asuint(_value2[330]));
        output.Store(1324, asuint(_value2[331]));
        output.Store(1328, asuint(_value2[332]));
        output.Store(1332, asuint(_value2[333]));
        output.Store(1336, asuint(_value2[334]));
        output.Store(1340, asuint(_value2[335]));
        output.Store(1344, asuint(_value2[336]));
        output.Store(1348, asuint(_value2[337]));
        output.Store(1352, asuint(_value2[338]));
        output.Store(1356, asuint(_value2[339]));
        output.Store(1360, asuint(_value2[340]));
        output.Store(1364, asuint(_value2[341]));
        output.Store(1368, asuint(_value2[342]));
        output.Store(1372, asuint(_value2[343]));
        output.Store(1376, asuint(_value2[344]));
        output.Store(1380, asuint(_value2[345]));
        output.Store(1384, asuint(_value2[346]));
        output.Store(1388, asuint(_value2[347]));
        output.Store(1392, asuint(_value2[348]));
        output.Store(1396, asuint(_value2[349]));
        output.Store(1400, asuint(_value2[350]));
        output.Store(1404, asuint(_value2[351]));
        output.Store(1408, asuint(_value2[352]));
        output.Store(1412, asuint(_value2[353]));
        output.Store(1416, asuint(_value2[354]));
        output.Store(1420, asuint(_value2[355]));
        output.Store(1424, asuint(_value2[356]));
        output.Store(1428, asuint(_value2[357]));
        output.Store(1432, asuint(_value2[358]));
        output.Store(1436, asuint(_value2[359]));
        output.Store(1440, asuint(_value2[360]));
        output.Store(1444, asuint(_value2[361]));
        output.Store(1448, asuint(_value2[362]));
        output.Store(1452, asuint(_value2[363]));
        output.Store(1456, asuint(_value2[364]));
        output.Store(1460, asuint(_value2[365]));
        output.Store(1464, asuint(_value2[366]));
        output.Store(1468, asuint(_value2[367]));
        output.Store(1472, asuint(_value2[368]));
        output.Store(1476, asuint(_value2[369]));
        output.Store(1480, asuint(_value2[370]));
        output.Store(1484, asuint(_value2[371]));
        output.Store(1488, asuint(_value2[372]));
        output.Store(1492, asuint(_value2[373]));
        output.Store(1496, asuint(_value2[374]));
        output.Store(1500, asuint(_value2[375]));
        output.Store(1504, asuint(_value2[376]));
        output.Store(1508, asuint(_value2[377]));
        output.Store(1512, asuint(_value2[378]));
        output.Store(1516, asuint(_value2[379]));
        output.Store(1520, asuint(_value2[380]));
        output.Store(1524, asuint(_value2[381]));
        output.Store(1528, asuint(_value2[382]));
        output.Store(1532, asuint(_value2[383]));
        output.Store(1536, asuint(_value2[384]));
        output.Store(1540, asuint(_value2[385]));
        output.Store(1544, asuint(_value2[386]));
        output.Store(1548, asuint(_value2[387]));
        output.Store(1552, asuint(_value2[388]));
        output.Store(1556, asuint(_value2[389]));
        output.Store(1560, asuint(_value2[390]));
        output.Store(1564, asuint(_value2[391]));
        output.Store(1568, asuint(_value2[392]));
        output.Store(1572, asuint(_value2[393]));
        output.Store(1576, asuint(_value2[394]));
        output.Store(1580, asuint(_value2[395]));
        output.Store(1584, asuint(_value2[396]));
        output.Store(1588, asuint(_value2[397]));
        output.Store(1592, asuint(_value2[398]));
        output.Store(1596, asuint(_value2[399]));
        output.Store(1600, asuint(_value2[400]));
        output.Store(1604, asuint(_value2[401]));
        output.Store(1608, asuint(_value2[402]));
        output.Store(1612, asuint(_value2[403]));
        output.Store(1616, asuint(_value2[404]));
        output.Store(1620, asuint(_value2[405]));
        output.Store(1624, asuint(_value2[406]));
        output.Store(1628, asuint(_value2[407]));
        output.Store(1632, asuint(_value2[408]));
        output.Store(1636, asuint(_value2[409]));
        output.Store(1640, asuint(_value2[410]));
        output.Store(1644, asuint(_value2[411]));
        output.Store(1648, asuint(_value2[412]));
        output.Store(1652, asuint(_value2[413]));
        output.Store(1656, asuint(_value2[414]));
        output.Store(1660, asuint(_value2[415]));
        output.Store(1664, asuint(_value2[416]));
        output.Store(1668, asuint(_value2[417]));
        output.Store(1672, asuint(_value2[418]));
        output.Store(1676, asuint(_value2[419]));
        output.Store(1680, asuint(_value2[420]));
        output.Store(1684, asuint(_value2[421]));
        output.Store(1688, asuint(_value2[422]));
        output.Store(1692, asuint(_value2[423]));
        output.Store(1696, asuint(_value2[424]));
        output.Store(1700, asuint(_value2[425]));
        output.Store(1704, asuint(_value2[426]));
        output.Store(1708, asuint(_value2[427]));
        output.Store(1712, asuint(_value2[428]));
        output.Store(1716, asuint(_value2[429]));
        output.Store(1720, asuint(_value2[430]));
        output.Store(1724, asuint(_value2[431]));
        output.Store(1728, asuint(_value2[432]));
        output.Store(1732, asuint(_value2[433]));
        output.Store(1736, asuint(_value2[434]));
        output.Store(1740, asuint(_value2[435]));
        output.Store(1744, asuint(_value2[436]));
        output.Store(1748, asuint(_value2[437]));
        output.Store(1752, asuint(_value2[438]));
        output.Store(1756, asuint(_value2[439]));
        output.Store(1760, asuint(_value2[440]));
        output.Store(1764, asuint(_value2[441]));
        output.Store(1768, asuint(_value2[442]));
        output.Store(1772, asuint(_value2[443]));
        output.Store(1776, asuint(_value2[444]));
        output.Store(1780, asuint(_value2[445]));
        output.Store(1784, asuint(_value2[446]));
        output.Store(1788, asuint(_value2[447]));
        output.Store(1792, asuint(_value2[448]));
        output.Store(1796, asuint(_value2[449]));
        output.Store(1800, asuint(_value2[450]));
        output.Store(1804, asuint(_value2[451]));
        output.Store(1808, asuint(_value2[452]));
        output.Store(1812, asuint(_value2[453]));
        output.Store(1816, asuint(_value2[454]));
        output.Store(1820, asuint(_value2[455]));
        output.Store(1824, asuint(_value2[456]));
        output.Store(1828, asuint(_value2[457]));
        output.Store(1832, asuint(_value2[458]));
        output.Store(1836, asuint(_value2[459]));
        output.Store(1840, asuint(_value2[460]));
        output.Store(1844, asuint(_value2[461]));
        output.Store(1848, asuint(_value2[462]));
        output.Store(1852, asuint(_value2[463]));
        output.Store(1856, asuint(_value2[464]));
        output.Store(1860, asuint(_value2[465]));
        output.Store(1864, asuint(_value2[466]));
        output.Store(1868, asuint(_value2[467]));
        output.Store(1872, asuint(_value2[468]));
        output.Store(1876, asuint(_value2[469]));
        output.Store(1880, asuint(_value2[470]));
        output.Store(1884, asuint(_value2[471]));
        output.Store(1888, asuint(_value2[472]));
        output.Store(1892, asuint(_value2[473]));
        output.Store(1896, asuint(_value2[474]));
        output.Store(1900, asuint(_value2[475]));
        output.Store(1904, asuint(_value2[476]));
        output.Store(1908, asuint(_value2[477]));
        output.Store(1912, asuint(_value2[478]));
        output.Store(1916, asuint(_value2[479]));
        output.Store(1920, asuint(_value2[480]));
        output.Store(1924, asuint(_value2[481]));
        output.Store(1928, asuint(_value2[482]));
        output.Store(1932, asuint(_value2[483]));
        output.Store(1936, asuint(_value2[484]));
        output.Store(1940, asuint(_value2[485]));
        output.Store(1944, asuint(_value2[486]));
        output.Store(1948, asuint(_value2[487]));
        output.Store(1952, asuint(_value2[488]));
        output.Store(1956, asuint(_value2[489]));
        output.Store(1960, asuint(_value2[490]));
        output.Store(1964, asuint(_value2[491]));
        output.Store(1968, asuint(_value2[492]));
        output.Store(1972, asuint(_value2[493]));
        output.Store(1976, asuint(_value2[494]));
        output.Store(1980, asuint(_value2[495]));
        output.Store(1984, asuint(_value2[496]));
        output.Store(1988, asuint(_value2[497]));
        output.Store(1992, asuint(_value2[498]));
        output.Store(1996, asuint(_value2[499]));
        output.Store(2000, asuint(_value2[500]));
        output.Store(2004, asuint(_value2[501]));
        output.Store(2008, asuint(_value2[502]));
        output.Store(2012, asuint(_value2[503]));
        output.Store(2016, asuint(_value2[504]));
        output.Store(2020, asuint(_value2[505]));
        output.Store(2024, asuint(_value2[506]));
        output.Store(2028, asuint(_value2[507]));
        output.Store(2032, asuint(_value2[508]));
        output.Store(2036, asuint(_value2[509]));
        output.Store(2040, asuint(_value2[510]));
        output.Store(2044, asuint(_value2[511]));
    }
    return;
}
