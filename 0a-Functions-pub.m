(* 
Project: Haircuts
Step 0a
Functions
*)

(*Path*)
nd := NotebookDirectory[];
dateStr := DateString[#, {"Hour", ":", "Minute", ":", "Second"}]&

(*Start time*)
time0 = DateList[]
startTimeString = dateStr@time0;
Print[startTimeString <> " Step 0a: Functions"]

(*Destination paths*)
path = nd<>"Data/";
pathAll = path<>"all/";
pathSav = path<>"sav/";
pathMkt = path<>"mkt/";
pathLog = path<>"log/";
pathVar = path<>"var/";
pathTtl = path<>"ttl/";
pathInd = path<>"ind/";
pathCfl = path<>"cfl/";
pathLiq = path<>"liq/";
pathSta = path<>"sta/";

(*Log*)
currentTime := DateList[];

printMsg[startTime_, msg_]:= (
	
	currentTimeEval = currentTime;
	currentTimeString = DateString[currentTimeEval,
			{"Hour", ":", "Minute", ":", "Second"}];
	Print[currentTimeString <> " (" <>
	  ToString@Round@
	  	DateDifference[
	  		startTime, currentTime, "Second"] <> ") "<> msg
	  		]
	  	 )

<< DatabaseLink`;
<< JLink`;
ReinstallJava[JVMArguments -> "-Xmx1024m"];

(*Dates*)
dateFormatSql =
  {"#", "Month", "/", "Day", "/", "Year", "#"};
dateFormatXls =
  {"Day", "/", "Month", "/", "Year"};
toDsXls := DateString[#, dateFormatXls] &
toDs := DateString[#, dateFormatSql] &;
sqlToDl = (SQLDateTime -> DateList);
dateFormatRus = {"Day", ".", "Month", ".", "Year"};
toDl := DateList[{#, dateFormatRus}] &


(*Gives the current calculation time*)
calcTimeCall := 
 DateString[{"YearShort", "Month", "Day", "_", "Hour", "Minute", 
   "Second"}]
   
(*Join zero / blank to a list*)
j0 := Join[{0}, #] &
jb := Join[{""}, #] &
jbl := Join[#, {""}] &

(*Rounding*)
r2 := If[NumberQ@#, Round[#, 10.^-2], ""] &
r4 := If[NumberQ@#, Round[#, 10.^-4], ""] &
m2 := Map[r2, #, {2}] &
m4 := Map[r4, #, {2}] &
m4l3 := Map[r4, #, {3}] &

(*Replace a non-number with a blank*)
rplc := Replace[#, x_ /; Not@NumberQ@x -> ""] &
mrplc := Map[rplc, #, {2}] &
(*Replace a non-number or a complex number with a blank - for VAR*)
rplcCo := 
 Replace[#, x_ /; Or[Not@NumberQ@x, Head[x] == Complex] -> ""] &
mrplcCo := Map[rplcCo, #, {2}] &
(*Replace a non-string with a blank - for dates*)
rplcd := Replace[#, (x_ /; Not@StringQ@x) -> ""] & 
numVec := Cases[#, x_ /; NumberQ@x] &

(*Zip lists*)
jo[lists_] := MapThread[List, lists]
jo2[list1_, var1_, var2_] :=
 Module[{c1 = 0, c2 = 0, len = Length@Join[var1, var2]},
  Table[
   If[MemberQ[list1, i],
    c1++; var1[[c1]],
    c2++; var2[[c2]]
    ], {i, len}]]
    
(**)
carr[variable_,length_] := 
	ConstantArray[variable, length];

(*Readable CSV import*)
csvImport[file_] := Import[
	  file,
	  "Table",
	  "HeaderLines" -> 1,
	  "NumberPoint" -> ",",
	  "FieldSeparators" -> ";",
	  "RepeatedSeparators" -> False,
	  CharacterEncoding -> "WindowsCyrillic"
	  ];
	  
(*CSV export*)
csvExport[path_,header_,data_,file_]:=
(
	(*Converting scientific representation w/o warnings*)
	Off[NumberForm::sigz];
	dataExp = 
	  data /. (x_Real -> NumberForm[x, ExponentFunction -> (Null &)]);
	dataExpCsv = 
	  StringReplace[#, "." -> ","] & /@ (Map[ToString, #, {2}] &@
	     dataExp);
	toExp = Prepend[dataExpCsv, header];
	Export[
	 path<>file, toExp,
	 "Table",
	 "FieldSeparators" -> ";",
	 CharacterEncoding -> "WindowsCyrillic"
	 ];
)	  
	  
(*Find mkt file for an id*)
findFile[path_,id_,category_] :=
 (
  SetDirectory[path];
  fn = FileNames[];
  SetDirectory[]; 
  Cases[fn, x_ /; And[ToExpression@StringSplit[x, "_"][[1]] == id,
      StringContainsQ[x, category]]][[1]]
  )

(*Mathematica list to SQL list*)
listSql[list_] :=
 StringReplace[
  ToString@list,
  {"{" -> "(", "}" -> ")", ", " -> ","}]
 
(*Optimization for yield*)
minimize[f_] :=
  (x /. Last@FindMinimum[f,
       {x, 0}, Method -> "Newton",
       AccuracyGoal -> 4, PrecisionGoal -> \[Infinity]
       ]
    ) /. x -> Null;  
  
(*
MOEX zero-coupon yield curve
https://www.moex.com/a3642
Derivation: 2aa-Gcurve.nb
*)
rn[b0_, b1_, b2_, tau_, 
	g1_, g2_, g3_, g4_, g5_, g6_, g7_, g8_, g9_, 
   t_] := 
  b0 + (b1 + b2) tau/t (1 - Exp[-t/tau]) - b2*Exp[-t/tau] + 
   E^(-2.7777777777777777` t^2) g1 + 
   E^(-1.0850694444444444` (-0.6` + t)^2) g2 + 
   E^(-0.4238552517361111` (-1.56` + t)^2) g3 + 
   E^(-0.1655684577094184` (-3.096` + t)^2) g4 + 
   E^(-0.06467517879274155` (-5.553600000000001` + t)^2) g5 + 
   E^(-0.025263741715914664` (-9.485760000000003` + t)^2) g6 + 
   E^(-0.009868649107779163` (-15.777216000000006` + t)^2) g7 + 
   E^(-0.0038549410577262354` (-25.84354560000001` + t)^2) g8 + 
   E^(-0.0015058363506743105` (-41.949672960000015` + t)^2) g9;

(*ZC yield with annual compounding*)
yf[t_, d_, dateVec_, parsCurveVec_] := (Exp[Apply[rn, Join[#, {t}]]/10000] - 1) &@
   parsCurveVec[[Position[dateVec, d][[1, 1]], 2]];

(*Second best rating*)
secondBest := If[Length@# > 1, 
	(Reverse@Sort@DeleteCases[#,0])[[2]], Max@#] &;

(*CF structure convert to string*)
Off[NumberForm::sigz];
nfList[list_] := (ToString@(NumberForm[Round[#, 10.^-2], 
       ExponentFunction -> (Null &)] & /@ list))

cfTxt[cashFlow_] := 
 StringDelete[#, " "] &@
  StringReplace[nfList@cashFlow, {".," -> ",", ".}" -> "}"}]
   
yrule = {Indeterminate -> 0, ComplexInfinity -> 0};
(*Calculate avg value for horizon*)
calcAvgHor[length_,indicator_, horizon_] := If[
  length < horizon, ConstantArray["", length],
   Join[ConstantArray["", horizon - 1], 
    N@MovingAverage[indicator, horizon]]]


(*Kupiec statistic for VAR backtesting*)
uc[num_, length_, p_] := With[{x = num, T = length}, 
 	-2 Log[
 	((1 - p)^(T - x) p^x)/((1 - x/T)^(T - x) (x/T)^x)
     ]]
 
msg = "Functions loaded";
printMsg[time0, msg]

