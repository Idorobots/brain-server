clear all; close all;

%% Cowboy

% CPU utilization
c_cpu0_max_h = [0.2855823131979817,0.5026472855317123,0.6575533346614179,0.7533447877725284,0.8657551374547664,1.0,0.8973571063529925,0.8667223633966599,0.9253848319044973,0.9119145927755996];
c_cpu0_avg_h = [0.12929819919367852,0.32800527072322855,0.49618516597221424,0.6013441976199364,0.6688684728645181,0.7341486310550192,0.7274847108334087,0.7416856050657278,0.727919317512767,0.7817514151967636];

c_cpu1_max_h = [0.2868433345450787,0.5151510570787683,0.6697624716488345,0.7561755608551406,0.8721635506594055,1.0,0.8860277335821908,0.8649677031982017,0.9199129657119364,0.8771130197533229];
c_cpu1_avg_h = [0.12619548856789595,0.32818374450752275,0.495781516848974,0.6002896812335968,0.6682077316657931,0.733413538419633,0.7296878339763089,0.7430389663318197,0.7272934403055213,0.7787410591559845];

c_cpu_max_h = 100*(c_cpu0_max_h + c_cpu1_max_h)/2;
c_cpu_avg_h = 100*(c_cpu0_avg_h + c_cpu1_avg_h)/2;

c_cpu0_max_w = [0.27996890358820803,0.6169494045288602,0.6172951347567962,0.8446072850810327,0.7874085988066671,0.7509270858324144,0.7558673705226528,0.7932045179866779,0.7866696778690788,0.826897027317666];
c_cpu0_avg_w = [0.11419470375906579,0.2683105728002075,0.39082229796498796,0.48482091562393925,0.5620117371754605,0.6055283735851176,0.6301637335398736,0.645159739665767,0.654965996760887,0.6703760007158376];

c_cpu1_max_w = [0.3439095153816716,0.6173287428410825,0.631386324782945,0.8654978183621794,0.7888515964846115,0.7333000677705107,0.7721600793830168,0.8207087640466402,0.7998824177414129,0.8217475368024109];
c_cpu1_avg_w = [0.10610600707427083,0.2676815700139178,0.3944754270760367,0.4864098863468986,0.5618099721268566,0.6026395380883866,0.6321225266641074,0.647522600524273,0.6583582759956401,0.670333893747227];

c_cpu_max_w = 100*(c_cpu0_max_w + c_cpu1_max_w)/2;
c_cpu_avg_w = 100*(c_cpu0_avg_w + c_cpu1_avg_w)/2;

figure();
subplot(1, 2, 1);
hold on;
plot(c_cpu_max_h, '--r', 'LineWidth', 2);
plot(c_cpu_avg_h, '-b', 'LineWidth', 2);

grid on;
title('SMP utilization - Cowboy/HTTP');
xlabel('Connections [k]');
ylabel('Utilization [%]');
axis([1, 10, 0, 100]);
hold off;

subplot(1, 2, 2);
hold on;
plot(c_cpu_max_w, '--r', 'LineWidth', 2);
plot(c_cpu_avg_w, '-b', 'LineWidth', 2);

grid on;
title('SMP utilization - Cowboy/WebSocket');
xlabel('Connections [k]');
ylabel('Utilization [%]');
axis([1, 10, 0, 100]);
hold off;

% Total memory
c_mem_max_h = [65345680,70483704,84479544,101235344,120131816,153735608,166160656,186430632,216153616,220631392];
c_mem_avg_h = [64059243.89983305,70057203.62416108,81222069.15436241,97320386.9544688,116311404.62162162,143142855.36878216,161643668.7042735,181485088.83561644,204941345.31506848,216237592.6746988];

c_mem_max_w = [65654768,75023672,84281464,94571144,106287896,115417160,126372072,137589120,148059984,158826824];
c_mem_avg_w = [65317567.67892977,74665642.22445561,83083692.47731093,92933860.12121212,104851539.92580101,113562006.2027027,123964344.44670051,135378474.48556876,145542793.49659863,156981178.80272108];

figure();
hold on;
grid on;
plot(c_mem_max_h, '--r', 'LineWidth', 2);
plot(c_mem_avg_h, '-r', 'LineWidth', 2);
plot(c_mem_max_w, '--b', 'LineWidth', 2);
plot(c_mem_avg_w, '-b', 'LineWidth', 2);

title('Total memory usage - Cowboy');
xlabel('Connections [k]');
ylabel('Memory [bytes]');
legend('HTTP max', 'HTTP avg', 'WebSocket max', 'WebSocket avg', 'Location', 'Best');
hold off;

% Context switches
c_cs_h = [673737,1723961,3068436,4688931,6376622,8499558,10711697,12998360,14972785,17114155];
c_cs_w = [792338,1830669,3226725,4961863,6774328,8968569,11280444,13873568,16258766,18339596];

figure();
hold on;
grid on;
plot(c_cs_h, '-r', 'LineWidth', 2);
plot(c_cs_w, '-b', 'LineWidth', 2);

title('Context switches - Cowboy');
xlabel('Connections [k]');
ylabel('Context switches');
legend('HTTP', 'WebSocket', 'Location', 'Best');
hold off;


%% MochiWeb

% CPU utilization
m_cpu0_max_h = [0.20751790409602863,0.22226018808748021,0.31317604697927776,0.38420869121517687,0.48626691631393054,0.5829401686013848,0.8471237731987588,0.796115907926561,0.9695111290230807,0.7222404460840967];
m_cpu0_avg_h = [0.07620692104774408,0.14841963767248825,0.22139717174015686,0.2521956284729807,0.3693989977050545,0.4198754654716902,0.6275169538154396,0.5694380124836437,0.6023963253364716,0.5956200622921426];

m_cpu1_max_h = [0.18621297168268475,0.22703138388892508,0.30447882682085714,0.377083282391769,0.49196843538650775,0.5814187074998222,0.8446702589194856,0.7952420864793586,0.9975421443603679,0.719568259578589];
m_cpu1_avg_h = [0.07369883785931226,0.13847344381116888,0.21388776057150158,0.25107179949302405,0.3694921393141124,0.4190511646505725,0.6300707168333274,0.5722140639065671,0.6035760023002193,0.5942765759793996];

m_cpu_max_h = 100*(m_cpu0_max_h + m_cpu1_max_h)/2;
m_cpu_avg_h = 100*(m_cpu0_avg_h + m_cpu1_avg_h)/2;

m_cpu0_max_w = [0.1516493656883624,0.2007268093514794,0.2684516347849199,0.34565643596006584,0.4972822253182459,0.5566396094053554,0.6499857420632756,0.655241618026195,0.6785279343744925,0.7203265269370387];
m_cpu0_avg_w = [0.07549965434948151,0.13407754679773667,0.21028514358834557,0.2496634940632756,0.3453960935558777,0.4473914123141003,0.5141045008502197,0.5427348197355548,0.5507624416129268,0.5651208788278405];

m_cpu1_max_w = [0.15228074172917366,0.19277144887092615,0.27059746758575337,0.4013194883816917,0.5114649725153997,0.59292104379756,0.6311614704980312,0.631604433672608,0.7020743052624732,0.6934563599018103];
m_cpu1_avg_w = [0.06213935101415622,0.13029679766215024,0.20847549960413422,0.2492298971957713,0.34620677141519757,0.45039235157202373,0.5174457026374348,0.5394159655271019,0.5476729988952977,0.5665012854965884];

m_cpu_max_w = 100*(m_cpu0_max_w + m_cpu1_max_w)/2;
m_cpu_avg_w = 100*(m_cpu0_avg_w + m_cpu1_avg_w)/2;

figure();
subplot(1, 2, 1);
hold on;
plot(m_cpu_max_h, '--r', 'LineWidth', 2);
plot(m_cpu_avg_h, '-b', 'LineWidth', 2);

grid on;
title('SMP utilization - MochiWeb/HTTP');
xlabel('Connections [k]');
ylabel('Utilization [%]');
axis([1, 10, 0, 100]);
hold off;

subplot(1, 2, 2);
hold on;
plot(m_cpu_max_w, '--r', 'LineWidth', 2);
plot(m_cpu_avg_w, '-b', 'LineWidth', 2);

grid on;
title('SMP utilization - MochiWeb/WebSocket');
xlabel('Connections [k]');
ylabel('Utilization [%]');
axis([1, 10, 0, 100]);
hold off;

% Total memory
m_mem_max_h = [52961904,69117744,87203520,106187608,122828232,141850760,157524256,170883696,192027688,201418832];
m_mem_avg_h = [51731831.29450915,68249762.48414023,86301882.16,105456581.67612688,121290908.22818792,137443751.4916388,155012774.17449665,169313967.89170897,185778733.29692832,197916528.46179965];

m_mem_max_w = [55349472,65276760,78356352,95775456,109993640,130912960,150092112,169256168,187153064,203329424];
m_mem_avg_w = [51192441.16,62655123.8,76873237.71619366,93831733.93979932,107906600.42809364,128716746.77852349,147967824.57815126,165670303.8783784,183963811.62774956,199890612.2983051];

figure();
hold on;
grid on;
plot(m_mem_max_h, '--r', 'LineWidth', 2);
plot(m_mem_avg_h, '-r', 'LineWidth', 2);
plot(m_mem_max_w, '--b', 'LineWidth', 2);
plot(m_mem_avg_w, '-b', 'LineWidth', 2);

title('Total memory usage - MochiWeb');
xlabel('Connections [k]');
ylabel('Memory [bytes]');
legend('HTTP max', 'HTTP avg', 'WebSocket max', 'WebSocket avg', 'Location', 'Best');
hold off;

% Context switches
m_cs_h = [770669,1773004,3236480,5014417,7033491,8925218,11711714,13919834,16215004,18356490];
m_cs_w = [605784,1739211,3145791,4842943,2739031,5284069,7641279,9880195,12387287,14875580];

figure();
hold on;
grid on;
plot(m_cs_h, '-r', 'LineWidth', 2);
plot(m_cs_w, '-b', 'LineWidth', 2);

title('Context switches - MochiWeb');
xlabel('Connections [k]');
ylabel('Context switches');
legend('HTTP', 'WebSocket', 'Location', 'Best');
hold off;

%% Cowboy vs. MochiWeb

% CPU utilization
figure();
subplot(1, 2, 1);
hold on;
plot(c_cpu_avg_h, '-r', 'LineWidth', 2);
plot(m_cpu_avg_h, '-b', 'LineWidth', 2);

grid on;
title('SMP utilization - HTTP');
xlabel('Connections [k]');
ylabel('Utilization [%]');
legend('Cowboy', 'MochiWeb', 'Location', 'Best');
axis([1, 10, 0, 100]);
hold off;

subplot(1, 2, 2);
hold on;
plot(c_cpu_avg_w, '-r', 'LineWidth', 2);
plot(m_cpu_avg_w, '-b', 'LineWidth', 2);

grid on;
title('SMP utilization - WebSocket');
xlabel('Connections [k]');
ylabel('Utilization [%]');
legend('Cowboy', 'MochiWeb', 'Location', 'Best');
axis([1, 10, 0, 100]);
hold off;

% Total memory
figure();
subplot(1, 2, 1);
hold on;
grid on;
plot(c_mem_avg_h, '-r', 'LineWidth', 2);
plot(m_mem_avg_h, '-b', 'LineWidth', 2);

title('Total memory usage - HTTP');
xlabel('Connections [k]');
ylabel('Memory [bytes]');
legend('Cowboy', 'MochiWeb', 'Location', 'Best');
hold off;

subplot(1, 2, 2);
hold on;
grid on;
plot(c_mem_avg_w, '-r', 'LineWidth', 2);
plot(m_mem_avg_w, '-b', 'LineWidth', 2);

title('Total memory usage - WebSocket');
xlabel('Connections [k]');
ylabel('Memory [bytes]');
legend('Cowboy', 'MochiWeb', 'Location', 'Best');
hold off;

% Context switches
figure();
subplot(1, 2, 1);
hold on;
grid on;
plot(c_cs_h, '-r', 'LineWidth', 2);
plot(m_cs_h, '-b', 'LineWidth', 2);

title('Context switches - HTTP');
xlabel('Connections [k]');
ylabel('Context switches');
legend('Cowboy', 'MochiWeb', 'Location', 'Best');
hold off;

subplot(1, 2, 2);
hold on;
grid on;
plot(c_cs_w, '-r', 'LineWidth', 2);
plot(m_cs_w, '-b', 'LineWidth', 2);

title('Context switches - WebSocket');
xlabel('Connections [k]');
ylabel('Context switches');
legend('Cowboy', 'MochiWeb', 'Location', 'Best');
hold off;