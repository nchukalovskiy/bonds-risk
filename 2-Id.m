(* 
Project: Haircuts
Step 2
ID list for calculation 
*)

(*
Setup
1. ID - Cbonds
2. Issuer - from Russia / EABD
3. First date - 01.01.2010
4. Last date - 20.06.2017
5. MOEX trading history

Result
1. CSV - Cbonds_ID State_regnumber - for OFZ+OBR
2. CSV - Cbonds_ID ISIN - for others
3. CSV - Cbonds_ID Bond_name Category - for all
*)

(*Module start time*)
time0 = DateList[]
startTimeString = dateStr@time0;
Print[startTimeString <> " Step 2: ID list for calculation"]

(*
1. OFZ + OBR only
*)

(*Cbonds list*)
listCbondsState = q["
   SELECT 
   	[ID Эмиссии],
   	[Гос Регистрационный номер], 
   	[код ISIN]
   FROM Cbonds_emissions
   WHERE 
   	(
	[Валюта] = 'RUB' OR
	[Валюта] = 'RUR' 
	) AND
   	(
   	[Эмитент] = 'Россия' OR
   	[Эмитент] = 'Банк России'
   	) AND
   	(
   	[Статус] = 'В обращении' OR
   	[Статус] = 'Погашена' OR
   	[Статус] = 'Досрочно погашена' OR
   	[Статус] = 'Реструктуризирована' OR
   	[Статус] = 'Дефолт по погашению'
   	) 
   "];
  
(*Moex list*)
listMoexState =
  q["
    SELECT DISTINCT REGNUMBER, ISIN
    FROM State_Securities
    WHERE 
    	(
    	TRADEDATE >= " <> dateStartSql <> " AND
    	TRADEDATE <= #02/24/2012#
    	) 
    UNION
    SELECT DISTINCT REGNUMBER, ISIN
    FROM Securities
    WHERE 
    	(
    	TRADEDATE >= #02/27/2012# AND
    	TRADEDATE <= " <> dateEndSql <> "
    	) AND
    	BOARDID = Iif(TRADEDATE < #06/01/2015#,'EQOB','TQOB')
    "]; 

(*Mapping*)  
keysCbondsRegn := Association@{cbid -> #[[1]], regn -> #[[2]]} &
keysCbondsIsin := Association@{cbid -> #[[1]], isin -> #[[3]]} &
keysMoexRegn := Association@{regn -> #[[1]]} &
keysMoexIsin := Association@{isin -> #[[2]]} & 
 
dictCbondsStateRegn = keysCbondsRegn /@ listCbondsState; 
dictCbondsStateIsin = keysCbondsIsin /@ listCbondsState;
dictMoexStateRegn = keysMoexRegn /@ listMoexState; 
dictMoexStateIsin =keysMoexIsin /@ listMoexState;

(*Inner join with the respective keys*)  
dictStateRegn = Sort@DeleteDuplicates@JoinAcross[
     dictCbondsStateRegn, dictMoexStateRegn, Key@regn];
dictStateIsin = Sort@DeleteDuplicates@JoinAcross[
     dictCbondsStateIsin, dictMoexStateIsin, Key@isin]; 

(*Outer join*)  
dictStateFinal = DeleteCases[
  Sort@JoinAcross[
    dictStateRegn, dictStateIsin, Key@cbid, "Outer"],
  x_ /; And[x[regn] == Null, x[isin] == Null]
  ];
  
(*Export to CSV*)  
headerState = {"CBID","REGN"};
listStateFinal =
  jo@{
    (#[cbid]) & /@ dictStateFinal,
    (#[regn]) & /@ dictStateFinal
    };
dataExpCsvState = StringReplace[#, "." -> ","] & /@
   (
    Map[ToString, #, {2}] &@listStateFinal
    );
toExpState = Join[{headerState}, dataExpCsvState];
fileNameState = "listStateFinal.csv";
Export[nd <> fileNameState, toExpState,
  "Table",
  "FieldSeparators" -> ";"
  ];

msg = (
"OFZ+OBR list: " <> fileNameState
);
printMsg[time0, msg]


(*
2. Other securities
The same procedure
*)
  
listCbondsOther = q["
   SELECT 
   	[ID Эмиссии],
   	[Гос Регистрационный номер], 
   	[код ISIN]
   FROM Cbonds_emissions
   WHERE
   	(
   	[Страна эмитента] = 'Россия' OR
   	[Эмитент] = 'Евразийский банк развития' 
   	) AND
   	(
	[Валюта] = 'RUB' OR
	[Валюта] = 'RUR' 
	) AND
   	(
   	[Эмитент] <> 'Россия' AND
   	[Эмитент] <> 'Банк России'
   	) AND 
   	(
   	[Статус] = 'В обращении' OR
   	[Статус] = 'Погашена' OR
   	[Статус] = 'Досрочно погашена' OR
   	[Статус] = 'Реструктуризирована' OR
   	[Статус] = 'Дефолт по погашению'
   	) AND
   	(
   	[Вид долговых обязательств] = 'Облигации' OR
   	[Вид долговых обязательств] = 'Еврооблигации'
   	)
   "];
   
listMoexOther =
  q["
    SELECT DISTINCT REGNUMBER, ISIN
    FROM Securities
    WHERE 
    	(
    	TRADEDATE >= " <> dateStartSql <> " AND
    	TRADEDATE <= " <> dateEndSql <> "
    	)
    "];  
  
dictCbondsOtherRegn = keysCbondsRegn /@ listCbondsOther; 
dictCbondsOtherIsin = keysCbondsIsin /@ listCbondsOther;

dictMoexOtherRegn = keysMoexRegn /@ listMoexOther; 
dictMoexOtherIsin = keysMoexIsin /@ listMoexOther;
  
dictOtherRegn = Sort@DeleteDuplicates@JoinAcross[
     dictCbondsOtherRegn, dictMoexOtherRegn, Key@regn];
dictOtherIsin = Sort@DeleteDuplicates@JoinAcross[
     dictCbondsOtherIsin, dictMoexOtherIsin, Key@isin];
     
dictOtherFinal = DeleteCases[
   Sort@JoinAcross[
     dictOtherRegn, dictOtherIsin, Key@cbid, "Outer"],
   x_ /; Or[
     And[x[regn] == Null, x[isin] == Null],
     And[x[regn] == Missing["Unmatched"], x[isin] == Null],
     And[x[regn] == Null, x[isin] == Missing["Unmatched"]]
     ]
   ];
   
headerOther = {"CBID","ISIN"};
listOtherFinal =
  jo@{
    (#[cbid]) & /@ dictOtherFinal,
    (#[isin]) & /@ dictOtherFinal
    };
dataExpCsvOther = StringReplace[#, "." -> ","] & /@
  (
  Map[ToString, #, {2}] &@listOtherFinal
  );
toExpOther = Join[{header}, dataExpCsvOther];
fileNameOther = "listOtherFinal.csv";
Export[nd <> fileNameOther, toExpOther,
  "Table",
  "FieldSeparators" -> ";"
  ];
      
msg = (
"Other securities list: " <> fileNameOther
);
printMsg[time0, msg]

(*
3. List with categories
*)

(*Join 2 lists of bonds*)
idList = Sort@Round@Join[
	listStateFinal[[All, 1]], 
	listOtherFinal[[All, 1]]
	];
idListSql = listSql@idList; 

(*Find ABSs*)
listABSQuery =
  Sort@q["
     SELECT [ID Эмиссии], [Бумага] 
     FROM [Cbonds_emissions] 
     WHERE [ID Эмиссии] IN " <> idListSql <> " AND
     	(
     	[Бумага] LIKE '%-ИП%' OR
     	[Бумага] LIKE '%ипотечный%' OR
     	[Бумага] LIKE '%СФО%' OR
     	[Бумага] LIKE '%Магистраль Двух%'
     	)
     "];
listABS = Join[
   listABSQuery,
   Transpose@{ConstantArray[
   	"ABS", Length@listABSQuery]
   	}, 2];
listABSSql = listSql@(Round /@ listABS[[All, 1]]);

(*Find convertibles*)
listConvertibleQuery =
  Sort@q["
     SELECT [ID Эмиссии], [Бумага] FROM [Cbonds_emissions] 
     WHERE [ID Эмиссии] IN " <> idListSql <> " AND
     	[ID Эмиссии] Not IN " <> listABSSql <> " AND
     	(
     	[Конвертируемость] = 1
     	)
     "];
listConvertible = Join[
   listConvertibleQuery,
   Transpose@{ConstantArray[
   	"Convertible", Length@listConvertibleQuery]
   	}, 2];

(*Find floaters*)
listFloaterQuery =
  Sort@q["
     SELECT [ID Эмиссии], [Бумага] FROM [Cbonds_emissions] 
     WHERE [ID Эмиссии] IN " <> idListSql <> " AND
     	[ID Эмиссии] NOT IN " <> listABSSql <> " AND
     	(
     	[Плавающая ставка] = 1 OR
     		(
     		[Плавающая ставка] = 0 AND
     		[Индексация] LIKE '%Ставка%'
     		)
     	)
     "];
listFloater = Join[
   listFloaterQuery,
   Transpose@{ConstantArray[
   	"Floater", Length@listFloater]
   	}, 2];

(*
Find inflation-linked bonds: missing face value
88759 is a convertible
*)
listLinkedQuery =
  Sort@q["
     SELECT [ID Эмиссии], [Бумага] FROM [Cbonds_emissions] 
     WHERE [ID Эмиссии] IN " <> idListSql <> " AND
     	[ID Эмиссии] NOT IN " <> listABSSql <> " AND
     	[Номинал] IS NULL AND
     	[ID Эмиссии] <> 88759
     "];
listLinked = Join[
   listLinkedQuery,
   Transpose@{ConstantArray[
   	"Linked", Length@listLinkedQuery]
   	}, 2];

(*All of the above lists together*)
notVanilla = Join[
   listABS, listConvertible, listFloater, listLinked];

(*Plain-vanilla bonds - for model calibration*)
idListVanilla = 
  Round /@ Complement[
  	idList, Round@notVanilla[[All, 1]]];
idListVanillaSql = listSql@idListVanilla;
listVanillaQuery =
  Sort@q["
     SELECT [ID Эмиссии], [Бумага] FROM [Cbonds_emissions] 
     WHERE [ID Эмиссии] IN " <> idListVanillaSql <> "
     "];
listVanilla = Join[
   listVanillaQuery,
   Transpose@{ConstantArray[
   	"Vanilla", Length@listVanillaQuery]
   	}, 2];

(*Export bond list with categories to CSV*) 
listAll = SortBy[Join[notVanilla, listVanilla], Last];
listAll[[All,1]] = Round@listAll[[All,1]]

fileListHeader = {
   "CBID", "PAPER", "CATEGORY"
   };

dataExpCsv = 
  StringReplace[#, "." -> ","] & /@ (
  	Map[ToString, #, {2}] &@listAll
  );
toExp = Join[{fileListHeader}, dataExpCsv];

fileNameList = "finalList.csv";

Export[
  nd <> fileNameList, toExp,
  "Table",
  "FieldSeparators" -> ";",
  CharacterEncoding -> "WindowsCyrillic"
  ];

msg = (
"List with categories: " <> fileNameList
);
printMsg[time0, msg]



