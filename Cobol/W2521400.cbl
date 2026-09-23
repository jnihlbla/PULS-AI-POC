000200 ID DIVISION.                                                             
000300 PROGRAM-ID.     W2521400.                                                
000400 AUTHOR.         STENING INGER.                                           
000500 DATE-WRITTEN.   19/04/23.                                                
000600 DATE-COMPILED.                                                           
000700                                                                          
000800*                                                                         
000900*    FUNCTION:                                                            
001000*        PS&L NEW PARTS INFORMATION                                       
001100*                                                                         
001200*                                                                         
001300*    ABENDCODES:                                                          
001400*        U0016 -  . . . .                                                 
001500*        U1000 -  . . . .                                                 
001600*                                                                         
001700                                                                          
001800     SKIP3                                                                
001900 ENVIRONMENT DIVISION.                                                    
002000     SKIP2                                                                
002100 INPUT-OUTPUT SECTION.                                                    
002200                                                                          
002300 FILE-CONTROL.                                                            
002401     SKIP2                                                                
002403     SELECT W2521401                   ASSIGN TO W25214D1.                
002410     SELECT W2521402                   ASSIGN TO W25214D2.                
002600     EJECT                                                                
002700 DATA DIVISION.                                                           
002800     SKIP3                                                                
002900 FILE SECTION.                                                            
003001     SKIP3                                                                
003002 FD  W2521401                                                             
003003     RECORDING       F                                                    
003004     BLOCK CONTAINS  0.                                                   
003005                                                                          
003006*01  -COPY W25209      -L.                                                
003007     SKIP3                                                                
003008 FD  W2521402                                                             
003009     RECORDING       V                                                    
003010     BLOCK CONTAINS  0.                                                   
003011                                                                          
003020 01  OUT-RECORD   PIC X(2000).                                            
003100     EJECT                                                                
003200 WORKING-STORAGE SECTION.                                                 
003300                                                                          
003400 77  IDPGM                       PIC X(8)    VALUE 'W2521400'.            
003500 77  YES                         PIC X       VALUE 'Y'.                   
003600 77  NOO                         PIC X       VALUE 'N'.                   
003700 77  IX1                         PIC S9(9)  VALUE +0    COMP SYNC.        
003710 77  MAX-IX1-DC                  PIC S9(9)  VALUE +6    COMP SYNC.        
003800 77  W-WRITE-HEADER              PIC X(01)   VALUE 'Y'.                   
003810 77  W-PACK-AVAIL                PIC S9(09)  VALUE +0.                    
003811 77  W-DAPUBL                    PIC 9(06).                               
003812 77  W-DAPUBL-AAVV               PIC 9(04).                               
003820 01  W-TIFINLV-AAVVD             PIC 9(05).                               
003821 01  FILLER REDEFINES W-TIFINLV-AAVVD.                                    
003830      03 W-TIFINLV-AAVV          PIC 9(04).                               
003831      03 FILLER                  PIC 9(01).                               
003837                                                                          
003838 77  W2521401-EOF-SW             PIC X       VALUE 'N'.                   
003840     88  END-OF-W2521401                     VALUE 'J'.                   
003900     EJECT                                                                
004000 01  TODAYS-DATE                 PIC 9(6)    VALUE ZERO.                  
004100 01  FILLER REDEFINES TODAYS-DATE.                                        
004200     03  TODAYS-DATE-YEAR        PIC 9(2).                                
004300     03  TODAYS-DATE-MONTH       PIC 9(2).                                
004400     03  TODAYS-DATE-DAY         PIC 9(2).                                
004500     EJECT                                                                
004600 01  GENERAL-SUBPROGRAMS.                                                 
004700*                                                                         
004800     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
004910     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
004920     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
004921     EJECT                                                                
004922*    --- PARAMETRAR TILL WDATKONV                                         
004930*01  -COPY WDATAREA                                                       
004940     EJECT                                                                
004950*01    -COPY WWDC99                                                       
004960     EJECT                                                                
005100*    --- PARAMETERS TO ABEND                                              
005200                                                                          
005300 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
005400 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
005500 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
005600     SKIP2                                                                
005700 01  ERROR-TEXT.                                                          
005800     03  FILLER                  PIC X(8)    VALUE 'ERR-TEXT'.            
005900     03  ERROR-TEXT-STR          PIC X(72)   VALUE SPACE.                 
006001     EJECT                                                                
006002*    --- PARAMETRAR TILL POSTSUM                                          
006003*                                                                         
006010*01  -COPY W0005   -PRE  POSTSUM-                                         
006201     EJECT                                                                
006202 01  IN-AREA-START               PIC X(24)   VALUE                        
006203                                 'IN-AREA-START  '.                       
006204     SKIP2                                                                
006205                                                                          
006206*01  AREA -COPY W25209     -PRE IN-                                       
006207     EJECT                                                                
006208 01  OUT-HEADER.                                                          
006211     03 IDPROJ-H           PIC X(07) VALUE 'PROJECT'.                     
006212     03 FILLER             PIC X(01) VALUE X'05'.                         
006213     03 IDPROJUP-H         PIC X(09) VALUE 'SUB PROJ.'.                   
006214     03 FILLER             PIC X(01) VALUE X'05'.                         
006215     03 IDARTNR-H          PIC X(08) VALUE 'PART NO.'.                    
006216     03 FILLER             PIC X(01) VALUE X'05'.                         
006219     03 BEART-ENG-H        PIC X(06) VALUE 'DESCR.'.                      
006220     03 FILLER             PIC X(01) VALUE X'05'.                         
006222     03 TISOP-AAVV-H       PIC X(09) VALUE 'PUBL.WEEK'.                   
006223     03 FILLER             PIC X(01) VALUE X'05'.                         
006224     03 TISOP-AAMMDD-H     PIC X(09) VALUE 'PUBL.DATE'.                   
006225     03 FILLER             PIC X(01) VALUE X'05'.                         
006226     03 KDPRODSL-H         PIC X(10) VALUE 'PROD GROUP'.                  
006227     03 FILLER             PIC X(01) VALUE X'05'.                         
006228     03 IDFKNGRP-H         PIC X(11) VALUE 'FUNCT.GROUP'.                 
006229     03 FILLER             PIC X(01) VALUE X'05'.                         
006230     03 KDUART-H           PIC X(11) VALUE 'EXCEPT PART'.                 
006231     03 FILLER             PIC X(01) VALUE X'05'.                         
006232     03 KDFARLIG-H         PIC X(10) VALUE 'DANG.GOODS'.                  
006233     03 FILLER             PIC X(01) VALUE X'05'.                         
006234     03 FLPISK-H           PIC X(04) VALUE 'PISK'.                        
006235     03 FILLER             PIC X(01) VALUE X'05'.                         
006236     03 IDAO-H             PIC X(06) VALUE 'DCN NO'.                      
006237     03 FILLER             PIC X(01) VALUE X'05'.                         
006238     03 IDUPPDKU-H         PIC X(05) VALUE 'KU NO'.                       
006239     03 FILLER             PIC X(01) VALUE X'05'.                         
006240     03 IDUPPDSU-H         PIC X(06) VALUE 'SU NO.'.                      
006241     03 FILLER             PIC X(01) VALUE X'05'.                         
006242     03 BEUPPDSU-H         PIC X(08) VALUE 'SU TITLE'.                    
006243     03 FILLER             PIC X(01) VALUE X'05'.                         
006244     03 KDERS-H            PIC X(07) VALUE 'SS CODE'.                     
006245     03 FILLER             PIC X(01) VALUE X'05'.                         
006246     03 IDARTNR-TILLK-H    PIC X(11) VALUE 'SS PART NO.'.                 
006247     03 FILLER             PIC X(01) VALUE X'05'.                         
006248     03 SS-AVAIL-STOCK-DC11-H PIC X(22)                                   
006249                           VALUE 'SS AVAIL.IN STOCK DC11'.                
006250     03 FILLER             PIC X(01) VALUE X'05'.                         
006251     03 SS-AVAIL-STOCK-DC41-H PIC X(22)                                   
006252                           VALUE 'SS AVAIL.IN STOCK DC41'.                
006253     03 FILLER             PIC X(01) VALUE X'05'.                         
006254     03 SS-AVAIL-STOCK-DC71-H PIC X(22)                                   
006255                           VALUE 'SS AVAIL.IN STOCK DC71'.                
006256     03 FILLER             PIC X(01) VALUE X'05'.                         
006257     03 SS-AVAIL-STOCK-DC72-H PIC X(22)                                   
006258                           VALUE 'SS AVAIL.IN STOCK DC72'.                
006259     03 FILLER             PIC X(01) VALUE X'05'.                         
006260     03 TIREGDAT-H         PIC X(09) VALUE 'REG. DATE'.                   
006261     03 FILLER             PIC X(01) VALUE X'05'.                         
006262     03 IDNAMN-FORP-H      PIC X(09) VALUE 'PACK.ENG.'.                   
006263     03 FILLER             PIC X(01) VALUE X'05'.                         
006264     03 KDEMBKOD-2-H       PIC X(09) VALUE 'EMB-KOD-2'.                   
006265     03 FILLER             PIC X(01) VALUE X'05'.                         
006266     03 PACK-AVAIL-DC11-H  PIC X(26)                                      
006267                           VALUE 'PACK. AVAIL. IN STOCK DC11'.            
006268     03 FILLER             PIC X(01) VALUE X'05'.                         
006269     03 PACK-AVAIL-DC41-H  PIC X(26)                                      
006270                           VALUE 'PACK. AVAIL. IN STOCK DC41'.            
006271     03 FILLER             PIC X(01) VALUE X'05'.                         
006272     03 PACK-AVAIL-DC71-H  PIC X(26)                                      
006273                           VALUE 'PACK. AVAIL. IN STOCK DC71'.            
006274     03 FILLER             PIC X(01) VALUE X'05'.                         
006275     03 PACK-AVAIL-DC72-H  PIC X(26)                                      
006276                           VALUE 'PACK. AVAIL. IN STOCK DC72'.            
006277     03 FILLER             PIC X(01) VALUE X'05'.                         
006278     03 TIUPPDAT-EMB-H     PIC X(16) VALUE 'PKG. INSTR. DATE'.            
006279     03 FILLER             PIC X(01) VALUE X'05'.                         
006280     03 IDNAMN-IDANSK-H    PIC X(19) VALUE 'DC11 PURCH. PLANNER'.         
006281     03 FILLER             PIC X(01) VALUE X'05'.                         
006282     03 TIMOTSI-CDC-H      PIC X(19) VALUE 'DC11 REC. AT PURCH.'.         
006283     03 FILLER             PIC X(01) VALUE X'05'.                         
006284     03 IDNAMN-IDANSK-DC41-H PIC X(19)                                    
006285                           VALUE 'DC41 PURCH. PLANNER'.                   
006286     03 FILLER             PIC X(01) VALUE X'05'.                         
006287     03 TIMOTSI-DC41-H     PIC X(20) VALUE 'DC41 REC. AT PURCH.'.         
006288     03 FILLER             PIC X(01) VALUE X'05'.                         
006289     03 IDNAMN-IDANSK-DC71-H PIC X(19)                                    
006290                           VALUE 'DC71 PURCH. PLANNER'.                   
006291     03 FILLER             PIC X(01) VALUE X'05'.                         
006292     03 TIMOTSI-DC71-H     PIC X(19) VALUE 'DC71 REC. AT PURCH.'.         
006293     03 FILLER             PIC X(01) VALUE X'05'.                         
006294     03 IDNAMN-IDANSK-DC72-H PIC X(19)                                    
006295                           VALUE 'DC72 PURCH. PLANNER'.                   
006296     03 FILLER             PIC X(01) VALUE X'05'.                         
006297     03 TIMOTSI-DC72-H     PIC X(19) VALUE 'DC72 REC. AT PURCH.'.         
006298     03 FILLER             PIC X(01) VALUE X'05'.                         
006299     03 IDNAMN-IDINK-H     PIC X(14) VALUE 'DC11 PURCHASER'.              
006300     03 FILLER             PIC X(01) VALUE X'05'.                         
006301     03 KDAVT-H            PIC X(14) VALUE 'DC11 AGREEMENT'.              
006302     03 FILLER             PIC X(01) VALUE X'05'.                         
006303     03 IDLEVNR-H          PIC X(13) VALUE 'DC11 SUPPLIER'.               
006304     03 FILLER             PIC X(01) VALUE X'05'.                         
006305     03 KDFPKPRI-H         PIC X(18) VALUE 'DC11 PACKING INCL.'.          
006306     03 FILLER             PIC X(01) VALUE X'05'.                         
006307     03 TIAVTAL-H          PIC X(14) VALUE 'DC11 AGR. DATE'.              
006308     03 FILLER             PIC X(01) VALUE X'05'.                         
006309     03 IDNAMN-IDINK-DC41-H PIC X(14) VALUE 'DC41 PURCHASER'.             
006310     03 FILLER             PIC X(01) VALUE X'05'.                         
006311     03 KDAVT-DC41-H       PIC X(14) VALUE 'DC41 AGREEMENT'.              
006312     03 FILLER             PIC X(01) VALUE X'05'.                         
006313     03 IDLEVNR-DC41-H     PIC X(13) VALUE 'DC41 SUPPLIER'.               
006314     03 FILLER             PIC X(01) VALUE X'05'.                         
006315     03 KDMATRPR-DC41-H    PIC X(18) VALUE 'DC41 PACKING INCL.'.          
006316     03 FILLER             PIC X(01) VALUE X'05'.                         
006317     03 DAPRLIST-DC41-H    PIC X(14) VALUE 'DC41 AGR. DATE'.              
006318     03 FILLER             PIC X(01) VALUE X'05'.                         
006319     03 IDNAMN-IDINK-DC71-H PIC X(14) VALUE 'DC71 PURCHASER'.             
006320     03 FILLER             PIC X(01) VALUE X'05'.                         
006321     03 KDAVT-DC71-H       PIC X(14) VALUE 'DC71 AGREEMENT'.              
006322     03 FILLER             PIC X(01) VALUE X'05'.                         
006323     03 IDLEVNR-DC71-H     PIC X(13) VALUE 'DC71 SUPPLIER'.               
006324     03 FILLER             PIC X(01) VALUE X'05'.                         
006325     03 KDMATRPR-DC71-H    PIC X(18) VALUE 'DC71 PACKING INCL.'.          
006326     03 FILLER             PIC X(01) VALUE X'05'.                         
006327     03 DAPRLIST-DC71-H    PIC X(14) VALUE 'DC71 AGR. DATE'.              
006328     03 FILLER             PIC X(01) VALUE X'05'.                         
006329     03 IDNAMN-IDINK-DC72-H PIC X(14) VALUE 'DC72 PURCHASER'.             
006330     03 FILLER             PIC X(01) VALUE X'05'.                         
006331     03 KDAVT-DC72-H       PIC X(14) VALUE 'DC72 AGREEMENT'.              
006332     03 FILLER             PIC X(01) VALUE X'05'.                         
006333     03 IDLEVNR-DC72-H     PIC X(13) VALUE 'DC72 SUPPLIER'.               
006334     03 FILLER             PIC X(01) VALUE X'05'.                         
006335     03 KDMATRPR-DC72-H    PIC X(18) VALUE 'DC72 PACKING INCL.'.          
006336     03 FILLER             PIC X(01) VALUE X'05'.                         
006337     03 DAPRLIST-DC72-H    PIC X(14) VALUE 'DC72 AGR. DATE'.              
006338     03 FILLER             PIC X(01) VALUE X'05'.                         
006339     03 KDARTURS-SE-H      PIC X(09) VALUE 'SE ORIGIN'.                   
006340     03 FILLER             PIC X(01) VALUE X'05'.                         
006341     03 KDARTURS-US-H      PIC X(09) VALUE 'US ORIGIN'.                   
006342     03 FILLER             PIC X(01) VALUE X'05'.                         
006343     03 KDARTURS-CN-H      PIC X(09) VALUE 'CN ORIGIN'.                   
006344     03 FILLER             PIC X(01) VALUE X'05'.                         
006345     03 FLUPG-H            PIC X(03) VALUE 'PSW'.                         
006346     03 FILLER             PIC X(01) VALUE X'05'.                         
006347     03 KDTPG-H            PIC X(03) VALUE 'TPD'.                         
006348     03 FILLER             PIC X(01) VALUE X'05'.                         
006349     03 TIFINLV-AAVV-H     PIC X(15) VALUE 'SE L.PUBL. WEEK'.             
006350     03 FILLER             PIC X(01) VALUE X'05'.                         
006351     03 REDIRLEV-H         PIC X(09) VALUE 'DC11 DDGS'.                   
006352     03 FILLER             PIC X(01) VALUE X'05'.                         
006353     03 TELEVBSK-H         PIC X(09) VALUE 'DC11 TEXT'.                   
006354     03 FILLER             PIC X(01) VALUE X'05'.                         
006355     03 TILEVBSK-DISP-H    PIC X(16) VALUE 'DC11 AVAIL. DAY'.             
006356     03 FILLER             PIC X(01) VALUE X'05'.                         
006357     03 DC11-AVAIL-H       PIC X(21) VALUE 'DC11 AVAIL. IN STOCK'.        
006358     03 FILLER             PIC X(01) VALUE X'05'.                         
006359     03 KVVORKO-H          PIC X(08) VALUE 'DC11 VOR'.                    
006360     03 FILLER             PIC X(01) VALUE X'05'.                         
006362     03 TIAVIDAT-H         PIC X(14) VALUE 'DC11 REC. DATE'.              
006363     03 FILLER             PIC X(01) VALUE X'05'.                         
006364     03 DAPUBL-US-H        PIC X(15) VALUE 'US L.PUBL. WEEK'.             
006365     03 FILLER             PIC X(01) VALUE X'05'.                         
006366     03 TELEVBSK-DC41-H    PIC X(09) VALUE 'DC41 TEXT'.                   
006367     03 FILLER             PIC X(01) VALUE X'05'.                         
006368     03 TILEVBSK-DISP-DC41-H PIC X(16) VALUE 'DC41 AVAIL. DAY'.           
006369     03 FILLER             PIC X(01) VALUE X'05'.                         
006370     03 AVAIL-DC41-H       PIC X(21) VALUE 'DC41 AVAIL. IN STOCK'.        
006371     03 FILLER             PIC X(01) VALUE X'05'.                         
006372     03 TIINLMOT-DC41-H    PIC X(14) VALUE 'DC41 REC. DATE'.              
006373     03 FILLER             PIC X(01) VALUE X'05'.                         
006374     03 DAPUBL-CN-H        PIC X(15) VALUE 'CN L.PUBL. WEEK'.             
006375     03 FILLER             PIC X(01) VALUE X'05'.                         
006376     03 TELEVBSK-DC71-H    PIC X(09) VALUE 'DC71 TEXT'.                   
006377     03 FILLER             PIC X(01) VALUE X'05'.                         
006378     03 TILEVBSK-DISP-DC71-H PIC X(16) VALUE 'DC71 AVAIL. DAY'.           
006379     03 FILLER             PIC X(01) VALUE X'05'.                         
006380     03 AVAIL-DC71-H       PIC X(21) VALUE 'DC71 AVAIL. IN STOCK'.        
006381     03 FILLER             PIC X(01) VALUE X'05'.                         
006382     03 TIINLMOT-DC71-H    PIC X(14) VALUE 'DC71 REC. DATE'.              
006383     03 FILLER             PIC X(01) VALUE X'05'.                         
006384     03 TELEVBSK-DC72-H    PIC X(09) VALUE 'DC72 TEXT'.                   
006385     03 FILLER             PIC X(01) VALUE X'05'.                         
006386     03 TILEVBSK-DISP-DC72-H PIC X(16) VALUE 'DC72 AVAIL. DAY'.           
006387     03 FILLER             PIC X(01) VALUE X'05'.                         
006388     03 AVAIL-DC72-H       PIC X(21) VALUE 'DC72 AVAIL. IN STOCK'.        
006389     03 FILLER             PIC X(01) VALUE X'05'.                         
006390     03 TIINLMOT-DC72-H    PIC X(14) VALUE 'DC72 REC. DATE'.              
006391     03 FILLER             PIC X(01) VALUE X'05'.                         
006392     03 TIBERANK-DC61-H    PIC X(12) VALUE 'DC61 GIT ETA'.                
006393     03 FILLER             PIC X(01) VALUE X'05'.                         
006394     03 AVAIL-DC61-H       PIC X(21) VALUE 'DC61 AVAIL. IN STOCK'.        
006395     03 FILLER             PIC X(01) VALUE X'05'.                         
006396     03 TIBERANK-DC62-H    PIC X(12) VALUE 'DC62 GIT ETA'.                
006397     03 FILLER             PIC X(01) VALUE X'05'.                         
006398     03 AVAIL-DC62-H       PIC X(21) VALUE 'DC62 AVAIL. IN STOCK'.        
006399     03 FILLER             PIC X(01) VALUE X'05'.                         
006400     03 TIBERANK-DC67-H    PIC X(12) VALUE 'DC67 GIT ETA'.                
006401     03 FILLER             PIC X(01) VALUE X'05'.                         
006402     03 AVAIL-DC67-H       PIC X(21) VALUE 'DC67 AVAIL. IN STOCK'.        
006403     03 FILLER             PIC X(01) VALUE X'05'.                         
006404     03 KVRADER-H          PIC X(12) VALUE 'AGR. CHANGES'.                
006405     03 FILLER             PIC X(01) VALUE X'05'.                         
006406     03 TIUPPDAT-BTO-H     PIC X(10) VALUE 'PRICE DATE'.                  
006407     03 FILLER             PIC X(01) VALUE X'05'.                         
006408     03 FLPRTSTD-H         PIC X(10) VALUE 'STD.PRICE'.                   
006409     03 FILLER             PIC X(01) VALUE X'05'.                         
006410     03 DADATUM-H          PIC X(10) VALUE 'ACT.DATE'.                    
006411     03 FILLER             PIC X(01) VALUE X'05'.                         
006420     EJECT                                                                
006455 01  OUT-AREA-START        PIC X(24) VALUE 'OUT-AREA-START  '.            
006456 01  OUT-AREA.                                                            
006457     03 OUT-IDPROJ         PIC X(4).                                      
006458     03 FILLER             PIC X(01) VALUE X'05'.                         
006459     03 OUT-IDPROJUP       PIC X(8).                                      
006460     03 FILLER             PIC X(01) VALUE X'05'.                         
006461     03 OUT-IDARTNR        PIC 9(9).                                      
006462     03 FILLER             PIC X(01) VALUE X'05'.                         
006463     03 OUT-BEART-ENG      PIC X(25).                                     
006464     03 FILLER             PIC X(01) VALUE X'05'.                         
006468     03 OUT-TISOP-AAVV     PIC 9(4).                                      
006469     03 FILLER             PIC X(01) VALUE X'05'.                         
006470     03 OUT-TISOP-AAMMDD   PIC 9(6).                                      
006471     03 FILLER             PIC X(01) VALUE X'05'.                         
006472     03 OUT-KDPRODSL       PIC 9(3).                                      
006473     03 FILLER             PIC X(01) VALUE X'05'.                         
006474     03 OUT-IDFKNGRP       PIC 9(5).                                      
006475     03 FILLER             PIC X(01) VALUE X'05'.                         
006476     03 OUT-KDUART         PIC X.                                         
006477     03 FILLER             PIC X(01) VALUE X'05'.                         
006478     03 OUT-KDFARLIG       PIC 9.                                         
006479     03 FILLER             PIC X(01) VALUE X'05'.                         
006480     03 OUT-FLPISK         PIC X.                                         
006481     03 FILLER             PIC X(01) VALUE X'05'.                         
006483     03 OUT-IDAO           PIC X(10).                                     
006484     03 FILLER             PIC X(01) VALUE X'05'.                         
006485     03 OUT-IDUPPDKU       PIC -(06)9.                                    
006486     03 FILLER             PIC X(01) VALUE X'05'.                         
006487     03 OUT-IDUPPDSU       PIC X(08).                                     
006488     03 FILLER             PIC X(01) VALUE X'05'.                         
006489     03 OUT-BEUPPDSU       PIC X(35).                                     
006490     03 FILLER             PIC X(01) VALUE X'05'.                         
006491     03 OUT-KDERS          PIC 9(3).                                      
006492     03 FILLER             PIC X(01) VALUE X'05'.                         
006493     03 OUT-IDARTNR-TILLK  PIC X(9).                                      
006494     03 FILLER             PIC X(01) VALUE X'05'.                         
006495     03 OUT-SS-AVAIL-STOCK-DC11 PIC -(08)9.                               
006496     03 FILLER             PIC X(01) VALUE X'05'.                         
006497     03 OUT-SS-AVAIL-STOCK-DC41 PIC -(08)9.                               
006498     03 FILLER             PIC X(01) VALUE X'05'.                         
006499     03 OUT-SS-AVAIL-STOCK-DC71 PIC -(08)9.                               
006500     03 FILLER             PIC X(01) VALUE X'05'.                         
006501     03 OUT-SS-AVAIL-STOCK-DC72 PIC -(08)9.                               
006502     03 FILLER             PIC X(01) VALUE X'05'.                         
006503     03 OUT-TIREGDAT       PIC 9(7).                                      
006504     03 FILLER             PIC X(01) VALUE X'05'.                         
006505     03 OUT-IDNAMN-FORP    PIC X(40).                                     
006506     03 FILLER             PIC X(01) VALUE X'05'.                         
006507     03 OUT-KDEMBKOD-2     PIC 9(3).                                      
006508     03 FILLER             PIC X(01) VALUE X'05'.                         
006509     03 OUT-PACK-AVAIL-DC11 PIC -(8)9.                                    
006510     03 FILLER             PIC X(01) VALUE X'05'.                         
006511     03 OUT-PACK-AVAIL-DC41 PIC -(8)9.                                    
006512     03 FILLER             PIC X(01) VALUE X'05'.                         
006513     03 OUT-PACK-AVAIL-DC71 PIC -(8)9.                                    
006514     03 FILLER             PIC X(01) VALUE X'05'.                         
006515     03 OUT-PACK-AVAIL-DC72 PIC -(8)9.                                    
006516     03 FILLER             PIC X(01) VALUE X'05'.                         
006517     03 OUT-TIUPPDAT-EMB   PIC 9(7).                                      
006518     03 FILLER             PIC X(01) VALUE X'05'.                         
006519     03 OUT-IDNAMN-IDANSK  PIC X(40).                                     
006520     03 FILLER             PIC X(01) VALUE X'05'.                         
006521     03 OUT-TIMOTSI        PIC 9(6).                                      
006522     03 FILLER             PIC X(01) VALUE X'05'.                         
006523     03 OUT-IDNAMN-IDANSK-DC41 PIC X(40).                                 
006524     03 FILLER             PIC X(01) VALUE X'05'.                         
006525     03 OUT-TIMOTSI-DC41   PIC 9(6).                                      
006526     03 FILLER             PIC X(01) VALUE X'05'.                         
006527     03 OUT-IDNAMN-IDANSK-DC71 PIC X(40).                                 
006528     03 FILLER             PIC X(01) VALUE X'05'.                         
006529     03 OUT-TIMOTSI-DC71   PIC 9(6).                                      
006530     03 FILLER             PIC X(01) VALUE X'05'.                         
006531     03 OUT-IDNAMN-IDANSK-DC72 PIC X(40).                                 
006532     03 FILLER             PIC X(01) VALUE X'05'.                         
006533     03 OUT-TIMOTSI-DC72   PIC 9(6).                                      
006534     03 FILLER             PIC X(01) VALUE X'05'.                         
006535     03 OUT-IDNAMN-IDINK   PIC X(40).                                     
006536     03 FILLER             PIC X(01) VALUE X'05'.                         
006537     03 OUT-KDAVT          PIC 9.                                         
006538     03 FILLER             PIC X(01) VALUE X'05'.                         
006539     03 OUT-IDLEVNR        PIC X(5).                                      
006540     03 FILLER             PIC X(01) VALUE X'05'.                         
006541     03 OUT-KDFPKPRI       PIC X.                                         
006542     03 FILLER             PIC X(01) VALUE X'05'.                         
006543     03 OUT-TIAVTAL        PIC 9(7).                                      
006544     03 FILLER             PIC X(01) VALUE X'05'.                         
006545     03 OUT-IDNAMN-IDINK-DC41 PIC X(40).                                  
006546     03 FILLER             PIC X(01) VALUE X'05'.                         
006547     03 OUT-KDAVT-DC41     PIC 9.                                         
006548     03 FILLER             PIC X(01) VALUE X'05'.                         
006549     03 OUT-IDLEVNR-DC41   PIC X(5).                                      
006550     03 FILLER             PIC X(01) VALUE X'05'.                         
006551     03 OUT-KDMATRPR-DC41  PIC X.                                         
006552     03 FILLER             PIC X(01) VALUE X'05'.                         
006553     03 OUT-DAPRLIST-DC41  PIC 9(7).                                      
006554     03 FILLER             PIC X(01) VALUE X'05'.                         
006555     03 OUT-IDNAMN-IDINK-DC71 PIC X(40).                                  
006556     03 FILLER             PIC X(01) VALUE X'05'.                         
006557     03 OUT-KDAVT-DC71     PIC 9.                                         
006558     03 FILLER             PIC X(01) VALUE X'05'.                         
006559     03 OUT-IDLEVNR-DC71   PIC X(5).                                      
006560     03 FILLER             PIC X(01) VALUE X'05'.                         
006561     03 OUT-KDMATRPR-DC71  PIC X.                                         
006562     03 FILLER             PIC X(01) VALUE X'05'.                         
006563     03 OUT-DAPRLIST-DC71  PIC 9(7).                                      
006564     03 FILLER             PIC X(01) VALUE X'05'.                         
006565     03 OUT-IDNAMN-IDINK-DC72 PIC X(40).                                  
006566     03 FILLER             PIC X(01) VALUE X'05'.                         
006567     03 OUT-KDAVT-DC72     PIC 9.                                         
006568     03 FILLER             PIC X(01) VALUE X'05'.                         
006569     03 OUT-IDLEVNR-DC72   PIC X(5).                                      
006570     03 FILLER             PIC X(01) VALUE X'05'.                         
006571     03 OUT-KDMATRPR-DC72  PIC X.                                         
006572     03 FILLER             PIC X(01) VALUE X'05'.                         
006573     03 OUT-DAPRLIST-DC72  PIC 9(7).                                      
006574     03 FILLER             PIC X(01) VALUE X'05'.                         
006575     03 OUT-KDARTURS-SE    PIC X(2).                                      
006576     03 FILLER             PIC X(01) VALUE X'05'.                         
006577     03 OUT-KDARTURS-US    PIC X(2).                                      
006578     03 FILLER             PIC X(01) VALUE X'05'.                         
006579     03 OUT-KDARTURS-CN    PIC X(2).                                      
006580     03 FILLER             PIC X(01) VALUE X'05'.                         
006581     03 OUT-FLUPG          PIC X(1).                                      
006582     03 FILLER             PIC X(01) VALUE X'05'.                         
006583     03 OUT-KDTPD          PIC X(1).                                      
006584     03 FILLER             PIC X(01) VALUE X'05'.                         
006585     03 OUT-TIFINLV-AAVV   PIC 9(4).                                      
006586     03 FILLER             PIC X(01) VALUE X'05'.                         
006587     03 OUT-REDIRLEV       PIC 9.9(2).                                    
006588     03 FILLER             PIC X(01) VALUE X'05'.                         
006589     03 OUT-TELEVBSK-TEXT.                                                
006590       05 FILLER           PIC X(01) VALUE '"'.                           
006591       05 OUT-TELEVBSK     PIC X(80).                                     
006592       05 FILLER           PIC X(01) VALUE '"'.                           
006593     03 FILLER             PIC X(01) VALUE X'05'.                         
006594     03 OUT-TILEVBSK-DISP  PIC 9(7).                                      
006596     03 FILLER             PIC X(01) VALUE X'05'.                         
006597     03 OUT-AVAIL-STOCK-DC11 PIC -(8)9.                                   
006598     03 FILLER             PIC X(01) VALUE X'05'.                         
006599     03 OUT-KVVORKO        PIC -(7)9.                                     
006600     03 FILLER             PIC X(01) VALUE X'05'.                         
006601     03 OUT-TIAVIDAT       PIC 9(7).                                      
006602     03 FILLER             PIC X(01) VALUE X'05'.                         
006603     03 OUT-DAPUBL-US      PIC 9(4).                                      
006604     03 FILLER             PIC X(01) VALUE X'05'.                         
006605     03 OUT-TELEVBSK-DC41-TEXT.                                           
006606       05 FILLER           PIC X(01) VALUE '"'.                           
006607       05 OUT-TELEVBSK-DC41 PIC X(80).                                    
006608       05 FILLER           PIC X(01) VALUE '"'.                           
006609     03 FILLER             PIC X(01) VALUE X'05'.                         
006610     03 OUT-TILEVBSK-DISP-DC41 PIC 9(8).                                  
006611     03 FILLER             PIC X(01) VALUE X'05'.                         
006612     03 OUT-AVAIL-STOCK-DC41 PIC -(8)9.                                   
006613     03 FILLER             PIC X(01) VALUE X'05'.                         
006614     03 OUT-TIINLMOT-DC41  PIC 9(9).                                      
006615     03 FILLER             PIC X(01) VALUE X'05'.                         
006616     03 OUT-DAPUBL-CN      PIC 9(4).                                      
006617     03 FILLER             PIC X(01) VALUE X'05'.                         
006618     03 OUT-TELEVBSK-DC71-TEXT.                                           
006619       05 FILLER           PIC X(01) VALUE '"'.                           
006620       05 OUT-TELEVBSK-DC71 PIC X(80).                                    
006621       05 FILLER           PIC X(01) VALUE '"'.                           
006622     03 FILLER             PIC X(01) VALUE X'05'.                         
006623     03 OUT-TILEVBSK-DISP-DC71 PIC 9(9).                                  
006624     03 FILLER             PIC X(01) VALUE X'05'.                         
006625     03 OUT-AVAIL-STOCK-DC71 PIC -(8)9.                                   
006626     03 FILLER             PIC X(01) VALUE X'05'.                         
006627     03 OUT-TIINLMOT-DC71  PIC 9(9).                                      
006628     03 FILLER             PIC X(01) VALUE X'05'.                         
006629     03 OUT-TELEVBSK-DC72-TEXT.                                           
006630       05 FILLER           PIC X(01) VALUE '"'.                           
006631       05 OUT-TELEVBSK-DC72 PIC X(80).                                    
006632       05 FILLER           PIC X(01) VALUE '"'.                           
006633     03 FILLER             PIC X(01) VALUE X'05'.                         
006634     03 OUT-TILEVBSK-DISP-DC72 PIC 9(9).                                  
006635     03 FILLER             PIC X(01) VALUE X'05'.                         
006636     03 OUT-AVAIL-STOCK-DC72 PIC -(8)9.                                   
006637     03 FILLER             PIC X(01) VALUE X'05'.                         
006638     03 OUT-TIINLMOT-DC72  PIC 9(8).                                      
006639     03 FILLER             PIC X(01) VALUE X'05'.                         
006640     03 OUT-TIBERANK-DC61  PIC 9(6).                                      
006641     03 FILLER             PIC X(01) VALUE X'05'.                         
006642     03 OUT-AVAIL-STOCK-DC61 PIC -(8)9.                                   
006643     03 FILLER             PIC X(01) VALUE X'05'.                         
006644     03 OUT-TIBERANK-DC62  PIC 9(6).                                      
006645     03 FILLER             PIC X(01) VALUE X'05'.                         
006646     03 OUT-AVAIL-STOCK-DC62 PIC -(8)9.                                   
006647     03 FILLER             PIC X(01) VALUE X'05'.                         
006648     03 OUT-TIBERANK-DC67  PIC 9(6).                                      
006649     03 FILLER             PIC X(01) VALUE X'05'.                         
006650     03 OUT-AVAIL-STOCK-DC67 PIC -(8)9.                                   
006651     03 FILLER             PIC X(01) VALUE X'05'.                         
006653     03 OUT-KVRADER-CDC-SLAG PIC 9(9).                                    
006654     03 FILLER             PIC X(01) VALUE X'05'.                         
006655     03 OUT-TIUPPDAT-BTO   PIC 9(06).                                     
006660     03 FILLER             PIC X(01) VALUE X'05'.                         
006670     03 OUT-FLPRTSTD       PIC X(01).                                     
006680     03 FILLER             PIC X(01) VALUE X'05'.                         
006690     03 OUT-DADATUM        PIC X(10).                                     
006691     03 FILLER             PIC X(01) VALUE X'05'.                         
006692     EJECT                                                                
006705 PROCEDURE DIVISION.                                                      
006706 MAIN SECTION.                                                            
006710     SKIP2                                                                
006800                                                                          
006900     PERFORM A-INIT                                                       
007010     PERFORM S01-READ-W2521401                                            
007020     PERFORM UNTIL END-OF-W2521401                                        
007200                                                                          
007300       PERFORM S11-WRITE-W2521402                                         
007700                                                                          
007810       PERFORM S01-READ-W2521401                                          
007900     END-PERFORM                                                          
008100                                                                          
008200     PERFORM Z-FINIT                                                      
008300                                                                          
008400     MOVE ZERO TO RETURN-CODE                                             
008500     GOBACK                                                               
008600     .                                                                    
008700     EJECT                                                                
008800 A-INIT SECTION.                                                          
008901                                                                          
008910     OPEN INPUT  W2521401                                                 
009001                                                                          
009010     OPEN OUTPUT W2521402                                                 
009100     SKIP2                                                                
009200     ACCEPT TODAYS-DATE  FROM DATE                                        
009310     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
009400     .                                                                    
009500     EJECT                                                                
009600 Z-FINIT SECTION.                                                         
009701     CLOSE W2521401                                                       
009710           W2521402                                                       
009801     SKIP2                                                                
009802     MOVE 'S' TO POSTSUM-OPKOD                                            
009810     CALL POSTSUM USING POSTSUM-PARM                                      
009900     .                                                                    
010001     EJECT                                                                
010002 S01-READ-W2521401 SECTION.                                               
010003     READ W2521401 INTO IN-AREA                                           
010004     AT END                                                               
010005        MOVE HIGH-VALUE TO IN-AREA                                        
010006        SET END-OF-W2521401 TO TRUE                                       
010007                                                                          
010008     NOT AT END                                                           
010009        MOVE 'W2521401' TO POSTSUM-FDNAMN                                 
010010        MOVE 'W25214D1' TO POSTSUM-DDNAMN2                                
010011*       -- CHANGE TO MOVE SPACE IF THE FILE LACKS RECORD TYPES            
010012        MOVE SPACE    TO POSTSUM-TRANSTYP                                 
010013        CALL POSTSUM USING POSTSUM-PARM                                   
010014     END-READ                                                             
010020     .                                                                    
010101     EJECT                                                                
010102 S11-WRITE-W2521402 SECTION.                                              
010103                                                                          
010104     IF W-WRITE-HEADER = YES                                              
010105        WRITE OUT-RECORD FROM OUT-HEADER                                  
010106        MOVE NOO           TO W-WRITE-HEADER                              
010107     END-IF                                                               
010108                                                                          
010111     PERFORM S11A-OUTPUT-LINE                                             
010112     WRITE OUT-RECORD    FROM OUT-AREA                                    
010113                                                                          
010114     MOVE SPACE            TO POSTSUM-TRANSTYP                            
010115     MOVE 'W2521402'       TO POSTSUM-FDNAMN                              
010116     MOVE 'W25214D2'       TO POSTSUM-DDNAMN2                             
010117     CALL POSTSUM       USING POSTSUM-PARM                                
010120     .                                                                    
010300     EJECT                                                                
010310 S11A-OUTPUT-LINE SECTION.                                                
010316                                                                          
010317     PERFORM S11AA-INIT-OUTPUT-SLAG                                       
010318                                                                          
010319     MOVE IN-BEART-ENG         TO OUT-BEART-ENG                           
010320     MOVE IN-BEUPPDSU          TO OUT-BEUPPDSU                            
010321     MOVE IN-DAPUBL-CN(3:6)    TO W-DAPUBL                                
010325     PERFORM S11B-CONVERT-DAPUBL                                          
010326     MOVE W-DAPUBL-AAVV        TO OUT-DAPUBL-CN                           
010328     MOVE IN-DAPUBL-US(3:6)    TO W-DAPUBL                                
010329     PERFORM S11B-CONVERT-DAPUBL                                          
010330     MOVE W-DAPUBL-AAVV        TO OUT-DAPUBL-US                           
010331     MOVE IN-FLPISK            TO OUT-FLPISK                              
010332     MOVE IN-FLUPG             TO OUT-FLUPG                               
010333     MOVE IN-IDAO              TO OUT-IDAO                                
010334     MOVE IN-IDNAMN-FORP       TO OUT-IDNAMN-FORP                         
010335     MOVE IN-IDNAMN-IDANSK     TO OUT-IDNAMN-IDANSK                       
010336     MOVE IN-IDNAMN-IDINK      TO OUT-IDNAMN-IDINK                        
010337     MOVE IN-IDARTNR           TO OUT-IDARTNR                             
010338     IF IN-FLERS = NOO                                                    
010339        MOVE IN-IDARTNR-TILLK  TO OUT-IDARTNR-TILLK                       
010340     ELSE                                                                 
010341        MOVE 'VAR'             TO OUT-IDARTNR-TILLK                       
010343     END-IF                                                               
010344     MOVE IN-IDFKNGRP          TO OUT-IDFKNGRP                            
010345     MOVE IN-IDLEVNR           TO OUT-IDLEVNR                             
010346     MOVE IN-IDPROJ            TO OUT-IDPROJ                              
010347     MOVE IN-IDPROJUP          TO OUT-IDPROJUP                            
010348     MOVE IN-IDUPPDKU          TO OUT-IDUPPDKU                            
010349     MOVE IN-IDUPPDSU          TO OUT-IDUPPDSU                            
010350     MOVE IN-KDARTURS          TO OUT-KDARTURS-SE                         
010351     MOVE IN-KDAVT             TO OUT-KDAVT                               
010352     MOVE IN-KDEMBKOD-2        TO OUT-KDEMBKOD-2                          
010353     COMPUTE OUT-SS-AVAIL-STOCK-DC11 = IN-KVLS-SS                         
010354                                     - IN-KVRESS-SS                       
010355                                     - IN-KVROS-SS                        
010356                                                                          
010357     COMPUTE OUT-PACK-AVAIL-DC11 = IN-KVLS-EMBQ0                          
010358                                 - IN-KVRESS-EMBQ0                        
010359                                 - IN-KVROS-EMBQ0                         
010360                                                                          
010361     MOVE +1                   TO IX1                                     
010362     PERFORM UNTIL IX1 > MAX-IX1-DC                                       
010363       MOVE IN-IDDC-SLAG (IX1) TO WS-IDDC                                 
010364       IF NDC-US-RU                                                       
010365         COMPUTE OUT-PACK-AVAIL-DC41 =                                    
010366                                     IN-KVLS-EMBQ0-SLAG      (IX1)        
010367                                   - IN-KVRESS-EMBQ0-SLAG    (IX1)        
010368                                   - IN-KVOKS-DAG-EMBQ0-SLAG (IX1)        
010369                                   - IN-KVOKS-BULK-EMBQ0-SLAG(IX1)        
010370                                   - IN-KVROS-DAG-EMBQ0-SLAG (IX1)        
010371                                   - IN-KVROS-BULK-EMBQ0-SLAG(IX1)        
010372                                   + IN-KVAKS-SDC-EMBQ0-SLAG (IX1)        
010373       END-IF                                                             
010374                                                                          
010375       IF NDC-CN-71                                                       
010376         COMPUTE OUT-PACK-AVAIL-DC71 =                                    
010377                                     IN-KVLS-EMBQ0-SLAG      (IX1)        
010378                                   - IN-KVRESS-EMBQ0-SLAG    (IX1)        
010379                                   - IN-KVOKS-DAG-EMBQ0-SLAG (IX1)        
010380                                   - IN-KVOKS-BULK-EMBQ0-SLAG(IX1)        
010381                                   - IN-KVROS-DAG-EMBQ0-SLAG (IX1)        
010382                                   - IN-KVROS-BULK-EMBQ0-SLAG(IX1)        
010383                                   + IN-KVAKS-SDC-EMBQ0-SLAG (IX1)        
010384       END-IF                                                             
010385                                                                          
010386       IF NDC-CN-72                                                       
010387         COMPUTE OUT-PACK-AVAIL-DC72 =                                    
010388                                     IN-KVLS-EMBQ0-SLAG      (IX1)        
010389                                   - IN-KVRESS-EMBQ0-SLAG    (IX1)        
010390                                   - IN-KVOKS-DAG-EMBQ0-SLAG (IX1)        
010391                                   - IN-KVOKS-BULK-EMBQ0-SLAG(IX1)        
010392                                   - IN-KVROS-DAG-EMBQ0-SLAG (IX1)        
010393                                   - IN-KVROS-BULK-EMBQ0-SLAG(IX1)        
010394                                   + IN-KVAKS-SDC-EMBQ0-SLAG (IX1)        
010395       END-IF                                                             
010396                                                                          
010397       IF NDC-US-RU                                                       
010398         COMPUTE OUT-SS-AVAIL-STOCK-DC41 =                                
010399                                     IN-KVLS-SS-SLAG      (IX1)           
010400                                   - IN-KVRESS-SS-SLAG    (IX1)           
010401                                   - IN-KVOKS-DAG-SS-SLAG (IX1)           
010402                                   - IN-KVOKS-BULK-SS-SLAG(IX1)           
010403                                   - IN-KVROS-DAG-SS-SLAG (IX1)           
010404                                   - IN-KVROS-BULK-SS-SLAG(IX1)           
010405                                   + IN-KVAKS-SDC-SS-SLAG (IX1)           
010406       END-IF                                                             
010407                                                                          
010408       IF NDC-CN-71                                                       
010409         COMPUTE OUT-SS-AVAIL-STOCK-DC71 =                                
010410                                     IN-KVLS-SS-SLAG      (IX1)           
010411                                   - IN-KVRESS-SS-SLAG    (IX1)           
010412                                   - IN-KVOKS-DAG-SS-SLAG (IX1)           
010413                                   - IN-KVOKS-BULK-SS-SLAG(IX1)           
010414                                   - IN-KVROS-DAG-SS-SLAG (IX1)           
010415                                   - IN-KVROS-BULK-SS-SLAG(IX1)           
010416                                   + IN-KVAKS-SDC-SS-SLAG (IX1)           
010417       END-IF                                                             
010418                                                                          
010419       IF NDC-CN-72                                                       
010420         COMPUTE OUT-SS-AVAIL-STOCK-DC72 =                                
010421                                     IN-KVLS-SS-SLAG      (IX1)           
010422                                   - IN-KVRESS-SS-SLAG    (IX1)           
010423                                   - IN-KVOKS-DAG-SS-SLAG (IX1)           
010424                                   - IN-KVOKS-BULK-SS-SLAG(IX1)           
010425                                   - IN-KVROS-DAG-SS-SLAG (IX1)           
010426                                   - IN-KVROS-BULK-SS-SLAG(IX1)           
010427                                   + IN-KVAKS-SDC-SS-SLAG (IX1)           
010428       END-IF                                                             
010429                                                                          
010430       ADD +1                   TO IX1                                    
010431     END-PERFORM                                                          
010432                                                                          
010433     MOVE IN-KDERS             TO OUT-KDERS                               
010434     MOVE IN-KDFARLIG          TO OUT-KDFARLIG                            
010435     MOVE IN-KDFPKPRI          TO OUT-KDFPKPRI                            
010436     MOVE IN-KDPRODSL          TO OUT-KDPRODSL                            
010437     MOVE IN-KDTPD             TO OUT-KDTPD                               
010438     MOVE IN-KDUART            TO OUT-KDUART                              
010439     COMPUTE OUT-AVAIL-STOCK-DC11 = IN-KVLS                               
010440                                  - IN-KVRESS                             
010441                                  - IN-KVROS                              
010442                                  - IN-SUTPO-TOT                          
010443                 - (IN-KVOKS-BULK + IN-KVOKS-DAG + IN-KVOKS-VOR)          
010444                 + (IN-KVAKS-CDC + IN-KVAKS-PAV  + IN-KVAKS-T)            
010445     COMPUTE OUT-KVRADER-CDC-SLAG = IN-KVRADER-CDC +                      
010446                                    IN-KVRADER-SLAG                       
010447     MOVE IN-KVVORKO           TO OUT-KVVORKO                             
010448     MOVE IN-REDIRLEV          TO OUT-REDIRLEV                            
010449     MOVE IN-TELEVBSK          TO OUT-TELEVBSK                            
010450     MOVE IN-TIMOTSI           TO OUT-TIMOTSI                             
010451     MOVE IN-TILEVBSK-DISP     TO OUT-TILEVBSK-DISP                       
010452     MOVE IN-TIAVIDAT          TO OUT-TIAVIDAT                            
010453     MOVE IN-TIAVTAL           TO OUT-TIAVTAL                             
010454     PERFORM S11AB-CONVERT-TISOP                                          
010455     MOVE IN-TIFINLV           TO W-TIFINLV-AAVVD                         
010456     MOVE W-TIFINLV-AAVV       TO OUT-TIFINLV-AAVV                        
010457     MOVE IN-TIREGDAT          TO OUT-TIREGDAT                            
010458     MOVE IN-TIUPPDAT-BTO      TO OUT-TIUPPDAT-BTO                        
010459     MOVE IN-TIUPPDAT-EMB      TO OUT-TIUPPDAT-EMB                        
010460     IF IN-PRARTSTD = +0                                                  
010461        MOVE NOO               TO OUT-FLPRTSTD                            
010462     ELSE                                                                 
010463        MOVE YES               TO OUT-FLPRTSTD                            
010464     END-IF                                                               
010465     MOVE IN-DADATUM           TO OUT-DADATUM                             
010470                                                                          
010480     MOVE +1                             TO IX1                           
010490     PERFORM UNTIL IX1 > MAX-IX1-DC                                       
010491       MOVE IN-IDDC-SLAG(IX1)            TO WS-IDDC                       
010494       IF NDC-US-RU                                                       
010495         MOVE IN-DAPRLIST-SLAG     (IX1) TO OUT-DAPRLIST-DC41             
010496         MOVE IN-IDNAMN-IDANSK-SLAG(IX1) TO OUT-IDNAMN-IDANSK-DC41        
010497         MOVE IN-IDNAMN-IDINK-SLAG (IX1) TO OUT-IDNAMN-IDINK-DC41         
010498         MOVE IN-IDLEVNR-SLAG      (IX1) TO OUT-IDLEVNR-DC41              
010499         MOVE IN-KDAVT-SLAG        (IX1) TO OUT-KDAVT-DC41                
010501         MOVE IN-KDARTURS-SLAG     (IX1) TO OUT-KDARTURS-US               
010502         IF IN-KDMATRPR-SLAG(IX1) = 1                                     
010503           MOVE NOO                      TO OUT-KDMATRPR-DC41             
010504         ELSE                                                             
010505           IF IN-KDMATRPR-SLAG(IX1) = 2                                   
010506             MOVE YES                    TO OUT-KDMATRPR-DC41             
010507           END-IF                                                         
010508         END-IF                                                           
010510         COMPUTE OUT-AVAIL-STOCK-DC41 = IN-KVLS-SLAG      (IX1)           
010511                                      - IN-KVRESS-SLAG    (IX1)           
010512                                      - IN-KVOKS-DAG-SLAG (IX1)           
010513                                      - IN-KVOKS-BULK-SLAG(IX1)           
010514                                      - IN-KVROS-DAG-SLAG (IX1)           
010515                                      + IN-KVROS-BULK-SLAG(IX1)           
010516                                      + IN-KVAKS-SDC-SLAG (IX1)           
010517         MOVE IN-TELEVBSK-SLAG     (IX1) TO OUT-TELEVBSK-DC41             
010518         MOVE IN-TIINLMOT-SLAG     (IX1) TO OUT-TIINLMOT-DC41             
010519         MOVE IN-TIMOTSI-SLAG      (IX1) TO OUT-TIMOTSI-DC41              
010520         MOVE IN-TILEVBSK-DISP-SLAG(IX1) TO OUT-TILEVBSK-DISP-DC41        
010521       END-IF                                                             
010522                                                                          
010523       IF NDC-JP                                                          
010524         MOVE IN-TIBERANK-SLAG    (IX1) TO OUT-TIBERANK-DC61              
010525         COMPUTE OUT-AVAIL-STOCK-DC61 = IN-KVLS-SLAG      (IX1)           
010526                                      - IN-KVOKS-DAG-SLAG (IX1)           
010527                                      - IN-KVOKS-BULK-SLAG(IX1)           
010528                                      - IN-KVRESS-SLAG    (IX1)           
010529       END-IF                                                             
010530                                                                          
010531       IF NDC-AU                                                          
010532         MOVE IN-TIBERANK-SLAG    (IX1) TO OUT-TIBERANK-DC62              
010533         COMPUTE OUT-AVAIL-STOCK-DC62 = IN-KVLS-SLAG      (IX1)           
010534                                      - IN-KVOKS-DAG-SLAG (IX1)           
010535                                      - IN-KVOKS-BULK-SLAG(IX1)           
010536                                      - IN-KVRESS-SLAG    (IX1)           
010537       END-IF                                                             
010538                                                                          
010539       IF NDC-IN                                                          
010540         MOVE IN-TIBERANK-SLAG    (IX1)  TO OUT-TIBERANK-DC67             
010541         COMPUTE OUT-AVAIL-STOCK-DC67 = IN-KVLS-SLAG      (IX1)           
010542                                      - IN-KVOKS-DAG-SLAG (IX1)           
010543                                      - IN-KVOKS-BULK-SLAG(IX1)           
010544                                      - IN-KVRESS-SLAG    (IX1)           
010550       END-IF                                                             
010551                                                                          
010552       IF NDC-CN-71                                                       
010553         MOVE IN-DAPRLIST-SLAG     (IX1) TO OUT-DAPRLIST-DC71             
010554         MOVE IN-IDNAMN-IDANSK-SLAG(IX1) TO OUT-IDNAMN-IDANSK-DC71        
010555         MOVE IN-IDNAMN-IDINK-SLAG (IX1) TO OUT-IDNAMN-IDINK-DC71         
010556         MOVE IN-IDLEVNR-SLAG      (IX1) TO OUT-IDLEVNR-DC71              
010557         MOVE IN-KDARTURS-SLAG     (IX1) TO OUT-KDARTURS-CN               
010558         MOVE IN-KDAVT-SLAG        (IX1) TO OUT-KDAVT-DC71                
010560         IF IN-KDMATRPR-SLAG(IX1) = 1                                     
010561           MOVE NOO                      TO OUT-KDMATRPR-DC71             
010562         ELSE                                                             
010563           IF IN-KDMATRPR-SLAG(IX1) = 2                                   
010564             MOVE YES                    TO OUT-KDMATRPR-DC71             
010565           END-IF                                                         
010566         END-IF                                                           
010567         COMPUTE OUT-AVAIL-STOCK-DC71 = IN-KVLS-SLAG      (IX1)           
010568                                      - IN-KVRESS-SLAG    (IX1)           
010569                                      - IN-KVOKS-DAG-SLAG (IX1)           
010570                                      - IN-KVOKS-BULK-SLAG(IX1)           
010571                                      - IN-KVROS-DAG-SLAG (IX1)           
010572                                      + IN-KVROS-BULK-SLAG(IX1)           
010573                                      + IN-KVAKS-SDC-SLAG (IX1)           
010574         MOVE IN-TIMOTSI-SLAG      (IX1) TO OUT-TIMOTSI-DC71              
010575         MOVE IN-TIINLMOT-SLAG     (IX1) TO OUT-TIINLMOT-DC71             
010576         MOVE IN-TELEVBSK-SLAG     (IX1) TO OUT-TELEVBSK-DC71             
010577         MOVE IN-TILEVBSK-DISP-SLAG(IX1) TO OUT-TILEVBSK-DISP-DC71        
010578       END-IF                                                             
010579                                                                          
010580       IF NDC-CN-72                                                       
010581         MOVE IN-DAPRLIST-SLAG     (IX1) TO OUT-DAPRLIST-DC72             
010582         MOVE IN-IDNAMN-IDANSK-SLAG(IX1) TO OUT-IDNAMN-IDANSK-DC72        
010583         MOVE IN-IDNAMN-IDINK-SLAG (IX1) TO OUT-IDNAMN-IDINK-DC72         
010584         MOVE IN-IDLEVNR-SLAG      (IX1) TO OUT-IDLEVNR-DC72              
010585         MOVE IN-KDARTURS-SLAG     (IX1) TO OUT-KDARTURS-CN               
010586         MOVE IN-KDAVT-SLAG        (IX1) TO OUT-KDAVT-DC72                
010588         IF IN-KDMATRPR-SLAG(IX1) = 1                                     
010589           MOVE NOO                      TO OUT-KDMATRPR-DC72             
010590         ELSE                                                             
010591           IF IN-KDMATRPR-SLAG(IX1) = 2                                   
010592             MOVE YES                    TO OUT-KDMATRPR-DC72             
010593           END-IF                                                         
010594         END-IF                                                           
010595         COMPUTE OUT-AVAIL-STOCK-DC72 = IN-KVLS-SLAG      (IX1)           
010596                                      - IN-KVRESS-SLAG    (IX1)           
010597                                      - IN-KVOKS-DAG-SLAG (IX1)           
010598                                      - IN-KVOKS-BULK-SLAG(IX1)           
010599                                      - IN-KVROS-DAG-SLAG (IX1)           
010600                                      + IN-KVROS-BULK-SLAG(IX1)           
010601                                      + IN-KVAKS-SDC-SLAG (IX1)           
010602         MOVE IN-TIMOTSI-SLAG      (IX1) TO OUT-TIMOTSI-DC72              
010603         MOVE IN-TIINLMOT-SLAG     (IX1) TO OUT-TIINLMOT-DC72             
010604         MOVE IN-TELEVBSK-SLAG     (IX1) TO OUT-TELEVBSK-DC72             
010605         MOVE IN-TILEVBSK-DISP-SLAG(IX1) TO OUT-TILEVBSK-DISP-DC72        
010606       END-IF                                                             
010607                                                                          
010608       ADD +1                            TO IX1                           
010609     END-PERFORM                                                          
010610     .                                                                    
010611                                                                          
010612 S11AA-INIT-OUTPUT-SLAG SECTION.                                          
010632                                                                          
010640     MOVE +0                         TO OUT-AVAIL-STOCK-DC61              
010641     MOVE ZERO                       TO OUT-TIBERANK-DC61                 
010642     MOVE +0                         TO OUT-AVAIL-STOCK-DC62              
010643     MOVE ZERO                       TO OUT-TIBERANK-DC62                 
010644     MOVE +0                         TO OUT-AVAIL-STOCK-DC67              
010645     MOVE ZERO                       TO OUT-TIBERANK-DC67                 
010646                                                                          
010647     MOVE +0                         TO OUT-PACK-AVAIL-DC41               
010648     MOVE +0                         TO OUT-SS-AVAIL-STOCK-DC41           
010649     MOVE +0                         TO OUT-AVAIL-STOCK-DC41              
010650     MOVE +0                         TO OUT-DAPRLIST-DC41                 
010651     MOVE SPACE                      TO OUT-IDNAMN-IDANSK-DC41            
010652     MOVE SPACE                      TO OUT-IDNAMN-IDINK-DC41             
010653     MOVE SPACE                      TO OUT-IDLEVNR-DC41                  
010654     MOVE ZERO                       TO OUT-KDAVT-DC41                    
010655     MOVE SPACE                      TO OUT-KDMATRPR-DC41                 
010656     MOVE SPACE                      TO OUT-KDARTURS-US                   
010658     MOVE SPACE                      TO OUT-TELEVBSK-DC41                 
010659     MOVE ZERO                       TO OUT-TIINLMOT-DC41                 
010660     MOVE ZERO                       TO OUT-TIMOTSI-DC41                  
010661     MOVE ZERO                       TO OUT-TILEVBSK-DISP-DC41            
010662                                                                          
010663     MOVE +0                         TO OUT-PACK-AVAIL-DC71               
010664     MOVE +0                         TO OUT-SS-AVAIL-STOCK-DC71           
010665     MOVE +0                         TO OUT-AVAIL-STOCK-DC71              
010666     MOVE +0                         TO OUT-DAPRLIST-DC71                 
010667     MOVE SPACE                      TO OUT-IDNAMN-IDANSK-DC71            
010668     MOVE SPACE                      TO OUT-IDNAMN-IDINK-DC71             
010669     MOVE SPACE                      TO OUT-IDLEVNR-DC71                  
010670     MOVE ZERO                       TO OUT-KDAVT-DC71                    
010671     MOVE SPACE                      TO OUT-KDMATRPR-DC71                 
010672     MOVE SPACE                      TO OUT-KDARTURS-US                   
010674     MOVE SPACE                      TO OUT-TELEVBSK-DC71                 
010675     MOVE ZERO                       TO OUT-TIINLMOT-DC71                 
010676     MOVE ZERO                       TO OUT-TIMOTSI-DC71                  
010677     MOVE ZERO                       TO OUT-TILEVBSK-DISP-DC71            
010678                                                                          
010679     MOVE +0                         TO OUT-PACK-AVAIL-DC72               
010680     MOVE +0                         TO OUT-SS-AVAIL-STOCK-DC72           
010681     MOVE +0                         TO OUT-AVAIL-STOCK-DC72              
010682     MOVE +0                         TO OUT-DAPRLIST-DC72                 
010683     MOVE SPACE                      TO OUT-IDNAMN-IDANSK-DC72            
010684     MOVE SPACE                      TO OUT-IDNAMN-IDINK-DC72             
010685     MOVE SPACE                      TO OUT-IDLEVNR-DC72                  
010686     MOVE ZERO                       TO OUT-KDAVT-DC72                    
010687     MOVE SPACE                      TO OUT-KDMATRPR-DC72                 
010688     MOVE SPACE                      TO OUT-KDARTURS-US                   
010690     MOVE SPACE                      TO OUT-TELEVBSK-DC72                 
010691     MOVE ZERO                       TO OUT-TIINLMOT-DC72                 
010692     MOVE ZERO                       TO OUT-TIMOTSI-DC72                  
010693     MOVE ZERO                       TO OUT-TILEVBSK-DISP-DC72            
010694     .                                                                    
010695                                                                          
010696 S11AB-CONVERT-TISOP   SECTION.                                           
010697                                                                          
010698     MOVE ZERO                TO OUT-TISOP-AAMMDD                         
010699     MOVE ZERO                TO OUT-TISOP-AAVV                           
010700     IF IN-TISOP > +0                                                     
010701        MOVE 'AAVVD'          TO DAT-KDDATFORM                            
010702        MOVE IN-TISOP         TO DAT-I-TIDATUM                            
010703        CALL WDATKONV      USING DAT-KDDATFORM                            
010704                                 DAT-I-TIDATUM                            
010705                                 DAT-O-TIDATUM                            
010706                                 DAT-KDSVAR                               
010707        IF DAT-KDSVAR-OK                                                  
010708          MOVE DAT-TIAAMMDD   TO OUT-TISOP-AAMMDD                         
010709          MOVE DAT-TIAAVV-GRP TO OUT-TISOP-AAVV                           
010710        END-IF                                                            
010711     END-IF                                                               
010712     .                                                                    
010713                                                                          
010714 S11B-CONVERT-DAPUBL  SECTION.                                            
010715                                                                          
010716     MOVE ZERO                TO W-DAPUBL-AAVV                            
010718     IF W-DAPUBL > +0                                                     
010719        MOVE 'AAMMDD'         TO DAT-KDDATFORM                            
010720        MOVE W-DAPUBL         TO DAT-I-TIDATUM                            
010721        CALL WDATKONV      USING DAT-KDDATFORM                            
010722                                 DAT-I-TIDATUM                            
010723                                 DAT-O-TIDATUM                            
010724                                 DAT-KDSVAR                               
010725        IF DAT-KDSVAR-OK                                                  
010727          MOVE DAT-TIAAVV-GRP TO W-DAPUBL-AAVV                            
010728        END-IF                                                            
010729     END-IF                                                               
010730     .                                                                    
010731                                                                          
010732 S99-ABEND SECTION.                                                       
010733                                                                          
010734     SKIP2                                                                
010735     MOVE 'S' TO POSTSUM-OPKOD                                            
010736     CALL POSTSUM USING POSTSUM-PARM                                      
010740     CALL ABEND USING RKOD-ABEND                                          
010800     .                                                                    
