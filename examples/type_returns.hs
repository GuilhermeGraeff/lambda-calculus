For the types(3 vezes depth = (1 and 2), size = 3):

typeGenerator
TBool
TRecord [("oioatrnio",TBool),("ioaerero",TBool)]
TFun TBool TBool

TFun (List TNum) (TTuple [TNum,TNum,TBool,TNum])
TRecord [("ioictero",TFun TBool TNum)]
TNum


genRandomType
TFun TNum TNum
TFun TBool TBool
TTuple [TNum,TBool,TBool,TBool,TNum,TBool]

TTuple [TTuple [TBool,TBool,TBool,TNum,TNum,TBool,TNum,TNum,TBool],TTuple [TNum,TNum,TBool,TBool,TNum],TBool,TNum]
TNum
TFun (TFun TNum TNum) TNum


genRecordType 
TRecord [("ioivaatreiro",TBool),("vtrnir",TBool),("obctrro",TNum)]
TRecord [("ooitrni",TBool),("ibovaernir",TNum),("vcter",TNum)]
TRecord [("aterei",TNum),("oboicatrniro",TNum),("oicatereiro",TBool)]

TRecord [("oibacater",TRecord [("oivrniro",TBool),("obivcaterio",TNum),("vatene",TNum),("iboiaateeir",TBool),("oibiacatei",TNum),("atnr",TBool),("oboiaater",TBool)]),("boiatner",TTuple [TBool,TBool,TBool,TNum,TBool,TBool]),("oibiateneir",TBool)]
TRecord [("oiboivctno",TTuple [TNum,TBool]),("oibovcri",TFun TBool TNum),("oboacteniro",List TBool)]
TRecord [("ivacateniro",List TBool),("oivaatrn",TBool),("ovcaterner",TNum)]


genRecordTypeItem
("vcteo",TBool)
("oivcaterneir",TNum)
("oivacateio",TBool)

("boiarir",TBool)
("oiboaaeneiro",TFun TBool TNum)
("ibivaaneiro",TTuple [TBool])


genTupleType
TTuple [TBool,TBool,TNum]
TTuple [TBool,TNum,TNum]
TTuple [TNum,TBool,TNum]

TTuple [TFun TNum TBool,TFun TNum TNum,TBool]
TTuple [List TNum,TTuple [TBool],TFun TBool TNum]
TTuple [List TBool,List TBool,TTuple [TNum,TNum,TBool,TNum,TNum]]


genListType
List TNum
List TBool
List TNum

List (TRecord [("oibovacte",TBool),("oboivcenero",TNum),("bovaan",TNum),("iivacaeriro",TNum)])
List (List TNum)
List (TRecord [("oiiacatnir",TNum),("oteei",TNum),("oiboacteer",TNum),("ibivtero",TNum),("oacteo",TNum),("oibivatero",TBool),("obovcern",TNum)])


genFunType
TFun TNum TNum
TFun TBool TBool
TFun TBool TBool

TFun TNum (TFun TBool TBool)
TFun (TFun TBool TBool) TBool
TFun TNum (TRecord [("ibovaereo",TNum),("oboivacaternr",TNum),("oibvacaerner",TNum)])


genBasicType
TNum
TBool
TBool


genRandomNaturalSize
7
5
8


genRandomName
"iivaaer"
"boiarer"
"obvaateeiro"