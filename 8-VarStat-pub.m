(* 
Project: Haircuts
Step 8
VAR exceedance statistics
*)

(*

Setup
1. Load var data
2. Calculate the parameters
3. Export

Result
1. SAVs - quick import
2. CSV - statistics data (stat)

*)

(*VAR prob - here it is random*)
SeedRandom[3]; 
prob = RandomReal[];

paperId = ToString@id;

(*Module start time*)
time0 = DateList[]
startTimeString= dateStr@time0;
Print[startTimeString <> " Step 8: VAR exceedance statistics for ID: "<>paperId]

(*
1. Data
*)

fileVar = findFile[pathVar, id, "var"];

d = csvImport[pathVar <> fileVar];

msg = "1. VAR data loaded";
printMsg[time0, msg]

(*
2. Calc
*)


dg = GatherBy[d, Last];

nzVarPos = (Flatten@Position[#, x_ /; x[[3]]<0]) & /@ dg // 
   Quiet;
   
dates = dg[[#, nzVarPos[[#]], 1]] & /@ Range@Length@dg;

datesCommon = Intersection @@ dates;

If[Length@datesCommon == 0, errorLabel = "no common"; Abort[]];

datesPos = Table[
   Sort@(Flatten[Position[dg[[i, All, 1]], #] & /@ datesCommon]), {i, 
    Length@dg}];
    
staData =
  Table[
   {
    id,
    method = dg[[i, 1, -1]],
    numExc = dg[[i, datesPos[[i]], 4]] // Total, 
    cntExc = dg[[i, datesPos[[i]], 4]] // Length, 
    shr = (numExc/cntExc) // N, 
    stat = uc[numExc, cntExc, prob] // rplc // Quiet,
    pass10 = If[stat > qua, 0, 1] // rplc // Quiet, 
    errBR = Cases[dg[[i, datesPos[[i]], 5]], x_ /; x > 0] // Total, 
    errKO = -Cases[dg[[i, datesPos[[i]], 5]], x_ /; x < 0] // Total
    }, {i, Length@dg}];

msg = "2. VAR exceedance statistics calculated";
printMsg[time0, msg]
    
(*
3. Export
*)

calcTime = calcTimeCall;
   
fileNameSta = paperId <> "_sta_" <> calcTime <> ".csv";
csvExport[pathSta, hdSta, staData, fileNameSta]    

msg = (
"3. VAR exceedance statistics ready: " <> fileNameSta
);
printMsg[time0, msg]