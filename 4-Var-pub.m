(* 
Project: Haircuts
Step 4
VAR calculator
*)

(*

Setup
1. Load mkt data
2. Calculate VARs
3. Export

Result
1. CSV - VAR data (var)

*)

(*Module start time*)
time0 = DateList[]
startTimeString = dateStr@time0;
Print[startTimeString <> " Step 4: VAR calculator for ID: "<>ToString@id]

(*
1. Data
*)


(*Id number as string*)
paperId = ToString@id;

(*VAR prob - here it is random*)
SeedRandom[3]; 
prob = RandomReal[];

(*Inverse CDF Normal dist for prob*)
alpha = Quantile[NormalDistribution[], 1 - prob];

(*Mkt file for an id*)
fileMkt = findFile[pathMkt, id, "mkt"]

(*Import mkt data*)
d = csvImport[pathMkt <> fileMkt];
len = Length@d;
rng = Range@len;

(*Id number list*)
cbid = d[[All, po@"CBID"]];

(*Yield*)
y = d[[All, po@"Y"]];

(*RF Yield*)
yrf = d[[All, po@"YRF"]];

(*DP/P (WAPRICE)*)
dpp = d[[All, po@"D_P"]];

(*DY/Y*)
dyy = d[[All, po@"D_Y"]];
dyyrf = d[[All, po@"D_Y_RF"]];

(*Duration*)
dur = d[[All, po@"DUR"]];

(*Convexity*)
cnv = d[[All, po@"CNV"]];

(*Trade date*)
tdatetext = d[[All, po@"DATE_TR"]];

(*Calc maturity in years*)
ttf = d[[All, po@"T"]];
dttf = Differences@ttf // j0;

(*WAPRICE*)
pr = d[[All, po@"P"]];

(*Face value*)
face = d[[All, po@"FACE"]];

(*Service variable,when to start calc: 
difference of time to maturity must be <0, when > 0 => reset*)
newStart = Flatten@Position[dttf, x_ /; x > 0];

(*Don't perform market indicators calc due to option dates + accrued \
length < horizon*)
itnoc = itnocFunc[newStart, len];

msg = "1. Market data loaded";
printMsg[time0, msg]

(*
2. VAR methods
*)

(*Historical - dY/Y - Duration*)
varDyyHi = 
  Quiet@mrplc@fuij1[Quantile[#, 1 - prob] &, dyy, itnoc, len];
varDyHi = (mrplc@(varDyyHi*rpt[y]));
varYHiD = mrplcCo@(-rpt[dur]*varDyHi);

(*Historical - dYRF/YRF - Duration*)
varDyyrfHi = Quiet@mrplc@fuij1[N@Quantile[#, 1 - prob] &, dyyrf, itnoc, len];
varDyrfHi = mrplc@(varDyyrfHi*rpt@y);
varYrfHiD = mrplcCo@(-rpt[dur]*varDyrfHi);

(*Cornish-Fisher - dY/Y - Duration*)
stdDyy = Quiet@mrplc@fuij1[StandardDeviation, dyy, itnoc, len];
stdDy = mrplc@(stdDyy*rpt@y);
std = mrplc@(rpt[dur]*stdDy);
dppRec = Table[
   If[
    MemberQ[itnoc[[i]], j],
    "",
    (
     -dur[[j]]*dyy[[j - h[[i]] + 1 ;; j]]*y[[j]] + 
      1/2 cnv[[j]]*(dyy[[j - h[[i]] + 1 ;; j]]*y[[j]])^2
     )
    ],
   {i, lenh}, {j, len}];
skewDpp = Table[
   If[
    MemberQ[itnoc[[i]], j],
    "",
    Skewness@dppRec[[i, j]]
    ],
   {i, lenh}, {j, len}];
kurtDpp = Table[
   If[
    MemberQ[itnoc[[i]], j],
    "",
    (*excess kurtosis*)
    Kurtosis@dppRec[[i, j]] - 3
    ],
   {i, lenh}, {j, len}];
alphaS = mrplc@(alpha + 1/6 (alpha^2 - 1)*skewDpp);
alphaSK = 
  mrplc@(alpha + 1/6 (alpha^2 - 1)*skewDpp + 
     1/24 (alpha^3 - 3 alpha)*kurtDpp - 
     1/36 (2 alpha^3 - 5 alpha) skewDpp^2);
varYCfD = mrplcCo@(-alphaSK*std);

(*Normal - dY/Y - Duration*)
varYNoD = mrplcCo@(-alpha*std);

(*Cornish-Fisher - dYRF/YRF - Duration*)
stdDyyrf = fuij1[StandardDeviation, dyyrf, itnoc, len];
stdDyrf = mrplc@(stdDyyrf*rpt@y);
stdRecRf = mrplc@(rpt[dur]*stdDyrf);
dppRecRf = Table[
   If[
    MemberQ[itnoc[[i]], j],
    "",
    (
     -dur[[j]]*dyyrf[[j - h[[i]] + 1 ;; j]]*y[[j]] + 
      1/2 cnv[[j]]*(dyyrf[[j - h[[i]] + 1 ;; j]]*y[[j]])^2
     )
    ],
   {i, lenh}, {j, len}];
skewDppRf = Table[
   If[
    MemberQ[itnoc[[i]], j],
    "",
    Skewness@dppRecRf[[i, j]]
    ],
   {i, lenh}, {j, len}];
kurtDppRf = Table[
   If[
    MemberQ[itnoc[[i]], j],
    "",
    Kurtosis@dppRecRf[[i, j]] - 3
    ],
   {i, lenh}, {j, len}];
alphaSRf = mrplc@(alpha + 1/6 (alpha^2 - 1)*skewDppRf);
alphaSKRf = 
  mrplc@(alpha + 1/6 (alpha^2 - 1)*skewDppRf + 
     1/24 (alpha^3 - 3 alpha)*kurtDppRf - 
     1/36 (2 alpha^3 - 5 alpha) skewDppRf^2);
varYrfCfD = mrplcCo@(-alphaSKRf*stdRecRf);

(*Cornish-Fisher - dYRF/YRF - Duration*)
varYrfNoD = mrplcCo@(-alpha*stdRecRf);

(*Historical - dY/Y - Duration+Convexity*)
varYHiDC = Quiet@mrplcCo@Table[
   If[
    MemberQ[itnoc[[i]], j],
    "",
    Quantile[#, prob] &@dppRec[[i, j]]
    ],
   {i, lenh}, {j, len}];
(*Historical - dYRF/YRF - Duration+Convexity*)
varYrfHiDC = Quiet@mrplcCo@Table[
   If[
    MemberQ[itnoc[[i]], j],
    "",
    Quantile[#, prob] &@dppRecRf[[i, j]]
    ],
   {i, lenh}, {j, len}];

(*Cornish-Fisher - dY/Y - Duration+Convexity*)
dy = j0@Differences@y;
dy2 = dy^2;
dyy2 = dyy^2;
y2 = y^2;
stdDy2 = fuij1[StandardDeviation, dyy2, itnoc, len]*rpt[y2];
covDyyDyy2 = Table[
   If[
    MemberQ[itnoc[[i]], j],
    "",
    Covariance[dyy[[j - h[[i]] + 1 ;; j]], dyy2[[j - h[[i]] + 1 ;; j]]]
    ],
   {i, lenh}, {j, len}];
stdDC = mrplc@(
    Sqrt[
     rpt[dur^2]*(stdDy)^2 + 1/4 rpt[cnv^2]*stdDy2^2 + 
      rpt[dur*cnv]*(rpt[y]*rpt[y2]*covDyyDyy2)
     ]
    );
varYCfDC = mrplcCo@(-alphaSK*stdDC);

(*Normal - dY/Y - Duration+Convexity*)
varYNoDC = mrplcCo@(-alpha*stdDC);

(*Cornish-Fisher - dYRF/YRF - Duration+Convexity*)
dyrf = j0@Differences@yrf;
yrf2 = yrf^2;
dyrf2 = dyrf^2;
dyyrf2 = dyyrf^2;
stdDyrf2 = fuij1[StandardDeviation, dyyrf2, itnoc, len]*rpt[yrf2];
covDyyrfDyyrf2 = Table[
   If[
    MemberQ[itnoc[[i]], j],
    "",
    Covariance[dyyrf[[j - h[[i]] + 1 ;; j]], dyyrf2[[j - h[[i]] + 1 ;; j]]]
    ],
   {i, lenh}, {j, len}];
stdRfDC = mrplc@(
    Sqrt[
     rpt[dur^2]*stdDyrf^2 + 1/4 rpt[cnv^2]*stdDyrf2^2 + 
      rpt[dur*cnv]*rpt[yrf]*rpt[yrf2] covDyyrfDyyrf2
     ]
    );
varYrfCfDC = mrplcCo@(-alphaSKRf*stdRfDC);

(*Normal - dYRF/YRF - Duration+Convexity*)
varYrfNoDC = mrplcCo@(-alpha*stdRfDC);

(*Historical VAR for Y!, not P - dY/Y - Duration+Convexity*)
benchCorr = mrplcCo@(-rpt[dur]*varDyHi + 1/2 rpt[cnv]*varDyHi^2);

msg = "2. VAR calculated";
printMsg[time0, msg]

(*
3. Export
*)

calcTime = calcTimeCall;

(*Edit this list to calculate particular VAR*)
varAll =
  {
   (benchCorr),
   (varYHiD),
   (varYHiDC),
   (varYrfHiD),
   (varYrfHiDC),
   (varYCfD),
   (varYCfDC),
   (varYNoD),
   (varYNoDC),
   (varYrfCfD),
   (varYrfCfDC),
   (varYrfNoD),
   (varYrfNoDC)
   };

(*Exceedances, in rubles (>0 is bad)*)
errAll =
  Table[
     rplc /@ (jb@(Table[
          -((dpp[[i]] - #[[j, i - 1]])*pr[[i]]/100.*face[[i]]),
          {i, 2, len}])), {j, lenh}] & /@ varAll;

(*1 - exceedance flag*)
excAll = errAll /. {x_ /; x > 0 -> 1, x_ /; x <= 0 -> 0};



(*Dates and id - to the same dimension, as VAR*)
datesAll = ca@tdatetext;
cbidAll = ca@cbid;

(*R for Rounded*)
varAllR = (varAll);
varAllR = Table[Most@varAllR[[i, j]] // jb, {i, lenm}, {j, lenh}];
errAllR = (errAll);

(*2d representation of the data*)
varData =
  Flatten[#, 1] &@Table[
    Flatten[#, 1] &@Table[
      Join[
       Part[#, i, j, k] & /@ {
         datesAll,
         cbidAll,
         varAllR,
         excAll,
         errAllR
         },
       {methodFullNames[[i, j]]}
       ],
      {i, lenm}, {j, lenh}],
    {k, len}];

(*Export*)
fileNameVar = paperId <> "_var_" <> calcTime <> ".csv";
csvExport[pathVar, hdVar, varData, fileNameVar]

msg = (
"3. VAR data ready: " <> fileNameVar
);
printMsg[time0, msg]