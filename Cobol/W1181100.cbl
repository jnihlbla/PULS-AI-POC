000100 PROCESS DYNAM                                                            
000200 ID DIVISION.                                                             
000300 PROGRAM-ID.             W1181100.                                        
000400 AUTHOR.                 ARUP DATTA.                                      
000500 DATE-WRITTEN.           24/09/10.                                        
000600                                                                          
000700*    FUNCTION:                                                            
000800*        VALIDATE PRINS/TC-PLM PART INPUT                                 
000900*        WRITES FILE WITH DIFFERENT ID TYPES FOR FURTHER PROCESS          
001000*        WRITES ERROR FILE IF VALIDATION FAILS                            
001100*                                                                         
001200*    NOTES:                                                               
001300*        FLBYTES-MAN IS A MANUALLY SET FLAG FOR EXCHANGE PARTS            
001400*                    NOT TO BE USED TO SET FLBYTES FOR 1116/1117          
001500*        IDPTYP                                                           
001600*               DIFFERENT ID TYPES ARE SET BASED ON FUNCTIONALITY         
001700*               INS - REGISTER NEW PARTS USING 1116                       
001800*               UPD - UPDATE EXISTING PARTS USING 1117                    
001900*               SSI - SUPERSEEDED PARTS                                   
002000*                     FIRST REGISTER USING 1116                           
002100*                     AFTER SUCCESSFUL INS,TRIGGER 1113                   
002200*               SSU - SUPERSEEDED PARTS                                   
002300*                     FIRST UPDATE USING 1117                             
002400*                     AFTER SUCCESSFUL UPD,TRIGGER 1113                   
002500*               REJ - PART WITH SUPERSESSION CODE 52                      
002600*                     ALL THE OTHER VALIDATIONS WILL BE SKIPPED           
002700*               SWS - SOFTWARE PARTS WITH OTHER SUPERSESSION CODES        
002800*                     FOR NEW PARTS -> TRIGGERS ??                        
002900*                     FOR EXISTING PARTS ->TRIGGERS ??                    
003000*                     FINALLY TRIGGER 1113                                
003100*               NSP - NON SPARE PARTS                                     
003200*                     WILL BE REGISTERED IN NYPON USING BATCH             
003300*                     NO OTHER TRANSACTIONS WILL BE TRIGERRED.            
003400*                     ALSO NEW HW SPARE PARTS WITH CODE 52.               
003500*               EXI - EXCHANGE WITH PARTS INSERT IN WDK6                  
003600*                     TRIGGER 1116 FOR PARTS INSERT                       
003700*                     TRIGGER 3151 FOR EXCHANGE PART REG                  
003800*               EXU - EXCHANGE WITH PARTS UPDATE IN WDK6                  
003900*                     TRIGGER 1117 FOR PARTS UPDATE                       
004000*                     TRIGGER 3151 FOR EXCHANGE PART REG/UPD              
004100*               EXS - EXCHANGE WITH PARTS UPDATE IN WDK6 AND              
004200*                     SUPPERSESSION CHANGE                                
004300*                     TRIGGER 1117 FOR PARTS UPDATE                       
004400*                     TRIGGER 3151 FOR EXCHANGE PART REG/UPD              
004500*                     TRIGGER 1113 FOR SUPERSSION UPDATE                  
004600*               SWI - REGISTER NEW SOFTWARE PARTS USING 1116              
004700*               SWU - UPDATE EXISTING SOFTWARE PARTS USING 1117           
004800*                                                                         
004900     EJECT                                                                
005000 ENVIRONMENT DIVISION.                                                    
005100                                                                          
005200 INPUT-OUTPUT SECTION.                                                    
005300                                                                          
005400 FILE-CONTROL.                                                            
005500***  NEW/CHANGED PARTINFO FROM PRINS/TC-PLM                               
005600     SELECT  W1181101                 ASSIGN  W11811D1.                   
005700***  OUTPUT FILE FOR INS/UPD OF SPARE PART                                
005800     SELECT  W1181102                 ASSIGN  W11811D2.                   
005900***  ERROR FILE FOR PARTS PLANNER                                         
006000     SELECT  W1181103                 ASSIGN  W11811D3.                   
006100 DATA DIVISION.                                                           
006200                                                                          
006300 FILE SECTION.                                                            
006400                                                                          
006500 FD  W1181101                                                             
006600     RECORDING      F                                                     
006700     BLOCK CONTAINS 0.                                                    
006800     SKIP3                                                                
006900*01  -COPY W11810      -L.                                                
007000                                                                          
007100 FD  W1181102                                                             
007200     RECORDING      F                                                     
007300     BLOCK CONTAINS 0.                                                    
007400     SKIP3                                                                
007500*01  POST -COPY W1181101 -PRE  UT1-  -L.                                  
007600                                                                          
007700 FD  W1181103                                                             
007800     RECORDING   F                                                        
007900     BLOCK CONTAINS 0.                                                    
008000*01  POST -COPY W1181102 -PRE  UT2-  -L.                                  
008100     SKIP3                                                                
008200                                                                          
008300 WORKING-STORAGE SECTION.                                                 
008400*    -COPY WY2000W3                                                       
008500     SKIP3                                                                
008600*    -COPY WY2000W2                                                       
008700     SKIP3                                                                
008800*    -COPY WY2000W1                                                       
008900     SKIP3                                                                
009000                                                                          
009100 77  IDPGM                       PIC X(8)    VALUE 'W1181100'.            
009200 77  JA                          PIC X       VALUE 'J'.                   
009300 77  NEJ                         PIC X       VALUE 'N'.                   
009400 77  WS-CURRENT-SECTION          PIC X(32)   VALUE SPACE.                 
009500 77  WS-IMS-SECTION              PIC X(32)   VALUE SPACE.                 
009600 77  WS-KDSORT                   PIC X(2)    VALUE SPACE.                 
009700 77  WS-W009REDU-IN              PIC X(30)   VALUE SPACE.                 
009800 77  WS-W009REDU-UT              PIC X(30)   VALUE SPACE.                 
009900 77  WS-BEART                    PIC X(25)   VALUE SPACE.                 
010000 77  WS-FLRSBEART                PIC X       VALUE SPACE.                 
010100 77  WS-FLPISK                   PIC X       VALUE SPACE.                 
010200 77  WS-IDPRODNR-EXCH            PIC X(9)    VALUE SPACE.                 
010300 77  WS-IDPROJ                   PIC X(4)    VALUE SPACE.                 
010400 77  WS-IDSKYLT                  PIC X(3)    VALUE SPACE.                 
010500 77  WS-IDPTYP                   PIC X(3)    VALUE SPACE.                 
010600 77  WS-KDEMBKOD-2               PIC 9(3)    VALUE ZERO.                  
010700 77  WS-IDARTNR-MOTSV            PIC 9(9)    VALUE ZERO.                  
010800 77  WS-IDPROENH                 PIC X(8)    VALUE SPACES.                
010900 77  WS-IDRITN                   PIC X(10)   VALUE SPACES.                
011000 77  WS-KDSORT-VLFG              PIC X(4)    VALUE SPACES.                
011100 77  WS-VKART-NTO                PIC 9(8)    VALUE ZERO.                  
011200 77  WS-VLFG-NUM                 PIC 9(8)    VALUE ZERO.                  
011300 77  WS-VLFG                     PIC 9(4)V9(3)                            
011400                                             VALUE ZERO.                  
011500 77  WS-INDX                     PIC 9(2)    VALUE ZERO.                  
011510 77  RAD-IX                      PIC 9(3)    VALUE ZERO.                  
011600 77  MAX-INDX                    PIC 9(2)    VALUE 10.                    
011700 77  WS-DISP-FELTEXT             PIC X(100)  VALUE SPACES.                
011800 77  WS-FELTEXT                  PIC X(30)   VALUE SPACES.                
011900 77  WS-CC                       PIC 9(2)    VALUE ZERO.                  
012000 77  WS-NUM-10                   PIC 9(10)   VALUE ZERO.                  
012100 77  WS-NUM-2DP                  PIC 9(7).9(2)                            
012200                                             VALUE ZERO.                  
012300*                                                                         
012400 01  WS-KDPRODSL                 PIC 9(2)    VALUE ZERO.                  
012500 01  FILLER REDEFINES WS-KDPRODSL.                                        
012600     03 WS-KDPRODSL-1            PIC 9(1).                                
012700     03 WS-KDPRODSL-2            PIC 9(1).                                
012800*                                                                         
012900 01    WS-IDPROJK.                                                        
013000    05  W-PROJK-POS-1            PIC X(1)    VALUE SPACE.                 
013100    05  W-PROJK-POS-2            PIC X(1)    VALUE SPACE.                 
013200    05  FILLER                   PIC X(2)    VALUE SPACE.                 
013300*                                                                         
013400 01  WS-IDARTNR                  PIC X(9)    VALUE SPACE.                 
013500 01  IDARTNR-WS REDEFINES WS-IDARTNR                                      
013600                                 PIC 9(9).                                
013700*                                                                         
013800 01  INPUT-RETT                  PIC X       VALUE 'J'.                   
013900 01  NYPON-ARTIKEL               PIC X       VALUE 'N'.                   
014000     SKIP3                                                                
014100 01  WS-TEHOMONYM.                                                        
014200     03  RS-BM-NAMN              PIC X(7).                                
014300     03  FILLER                  PIC X(53).                               
014400*                                                                         
014500 01  WS-TIFINLV-AAMMDD           PIC 9(6)    VALUE ZERO.                  
014600 01  WS-TIFINLV                  PIC 9(5)    VALUE ZERO.                  
014700 01  FILLER REDEFINES WS-TIFINLV.                                         
014800     03  WS-AAR                  PIC 9(2).                                
014900     03  WS-VECKA                PIC 9(2).                                
015000     03  WS-DAG                  PIC 9(1).                                
015100     SKIP2                                                                
015200 01  WS-TISOP-AAMMDD             PIC 9(6)    VALUE ZERO.                  
015300 01  WS-TISOP                    PIC 9(5)    VALUE ZERO.                  
015400 01  FILLER REDEFINES WS-TISOP.                                           
015500     03  WS-AAR-SOP              PIC 9(2).                                
015600     03  WS-VECKA-SOP            PIC 9(2).                                
015700     03  WS-DAG-SOP              PIC 9(1).                                
015800     SKIP2                                                                
015900                                                                          
016000 01  SPAR-TIFINLV-AAVVD          PIC 9(05)   VALUE ZERO.                  
016100                                                                          
016200 01  SPAR-TIFINLV-AAVV.                                                   
016300     10  SPAR-TIFINLV-AA         PIC 9(02)   VALUE ZERO.                  
016400     10  SPAR-TIFINLV-VV         PIC 9(02)   VALUE ZERO.                  
016500 01  SPAR-TIFINLV-AAVV-R  REDEFINES  SPAR-TIFINLV-AAVV                    
016600                                 PIC 9(04).                               
016700                                                                          
016800 01  SPAR-PRARTSTD.                                                       
016900     10  SPAR-PRARTSTD-HELTAL    PIC 9(07) VALUE ZERO.                    
017000     10  SPAR-PRARTSTD-PUNKT     PIC X(01) VALUE SPACE.                   
017100     10  SPAR-PRARTSTD-DECIMAL   PIC 9(02) VALUE ZERO.                    
017200                                                                          
017300                                                                          
017400 01  SPAR-PRARTSTD-R.                                                     
017500     10  SPAR-PRARTSTD-R-HELTAL  PIC 9(07).                               
017600     10  SPAR-PRARTSTD-R-DECIMAL PIC 9(02).                               
017700                                                                          
017800 01  SPAR-PRARTSTD-RED  REDEFINES  SPAR-PRARTSTD-R.                       
017900     10 SPAR-PRARTSTD-HDTAL      PIC 9(07)V9(02).                         
018000                                                                          
018100 01  XX-TIFINLV                  PIC X(5)    VALUE SPACE.                 
018200 01  FILLER REDEFINES XX-TIFINLV.                                         
018300     03  XX-AAR                  PIC X(2).                                
018400     03  XX-VECKA                PIC X(2).                                
018500     03  XX-DAG                  PIC X(1).                                
018600 01  XX-TISOP                    PIC X(5)    VALUE SPACE.                 
018700 01  FILLER REDEFINES XX-TISOP.                                           
018800     03  XX-AAR-SOP              PIC X(2).                                
018900     03  XX-VECKA-SOP            PIC X(2).                                
019000     03  XX-DAG-SOP              PIC X(1).                                
019100                                                                          
019200 01  WS-IN-TISOP                 PIC X(5)    VALUE SPACE.                 
019300 01  FILLER REDEFINES WS-IN-TISOP.                                        
019400     03  WS-IN-AAVV-SOP          PIC X(4).                                
019500     03  WS-IN-DAG-SOP           PIC X(1).                                
019600                                                                          
019610 01  WS-IN-TIERSDAT              PIC X(5)    VALUE SPACE.                 
019620 01  FILLER REDEFINES WS-IN-TIERSDAT.                                     
019630     03  WS-IN-AAVV-ERS          PIC X(4).                                
019640     03  WS-IN-DAG-ERS           PIC X(1).                                
019650                                                                          
019660 01  WS-IN-TIERS-AAMMDD          PIC X(6)    VALUE SPACE.                 
019670 01  FILLER REDEFINES WS-IN-TIERS-AAMMDD.                                 
019680     03  WS-IN-TIERS-AA          PIC X(2).                                
019681     03  WS-IN-TIERS-MM          PIC X(2).                                
019682     03  WS-IN-TIERS-DD          PIC X(2).                                
019691                                                                          
019700 01  WS-IN-AAVVD-OBJ             PIC X(5)    VALUE SPACE.                 
019800 01  FILLER REDEFINES WS-IN-AAVVD-OBJ.                                    
019900     03  WS-IN-AAVV-OBJ          PIC 9(4).                                
020000     03  WS-IN-DAG-OBJ           PIC 9(1).                                
020100                                                                          
020200 01  DAGENS-AAMMDD               PIC 9(6)    VALUE ZERO.                  
020300 01  DAGENS-AAVV.                                                         
020400     03  DAGENS-AA               PIC 9(2)    VALUE ZERO.                  
020500     03  DAGENS-VV               PIC 9(2)    VALUE ZERO.                  
020600                                                                          
020700 01  SPAR-DAGENS-AAVV.                                                    
020800     03  SPAR-DAGENS-AA          PIC 9(02)   VALUE ZERO.                  
020900     03  SPAR-DAGENS-VV          PIC 9(02)   VALUE ZERO.                  
021000 01  SPAR-DAGENS-AAVV-R  REDEFINES  SPAR-DAGENS-AAVV                      
021100                                 PIC 9(04).                               
021200*                                                                         
021300 01  WS-YYWWD                    PIC 9(5).                                
021400 01  FILLER REDEFINES WS-YYWWD.                                           
021500     03  WS-YY                   PIC 9(2).                                
021600     03  WS-WW                   PIC 9(2).                                
021700     03  WS-D                    PIC 9(1).                                
021800                                                                          
021900 01  TINEDB-AAVV.                                                         
022000     03  TINEDB-AA               PIC 9(2)    VALUE ZERO.                  
022100     03  TINEDB-VV               PIC 9(2)    VALUE ZERO.                  
022200                                                                          
022300 01  VECKOR.                                                              
022400     03  AAVVD                   PIC 9(5).                                
022500     03  FILLER REDEFINES AAVVD.                                          
022600       05  AAVV                  PIC 9(4).                                
022700       05  FILLER REDEFINES AAVV.                                         
022800         07  AA                  PIC 9(2).                                
022900         07  VV                  PIC 9(2).                                
023000       05  D                     PIC 9(1).                                
023100                                                                          
023200 01  W-AAR4                      PIC 9(4).                                
023300*                                                                         
023400                                                                          
023500     SKIP2                                                                
023600                                                                          
023700 01  WS-TEST-IDFKNGRP            PIC 9(4)    VALUE ZERO.                  
023800 01  FILLER REDEFINES WS-TEST-IDFKNGRP.                                   
023900     03 FILLER                   PIC 9(3).                                
024000     03 WS-SISTA-SIFFRAN         PIC 9.                                   
024100                                                                          
024200 01  WS-GRP-FELTEXT.                                                      
024300     05 WS-TAB-FELTEXT           PIC X(30)   OCCURS 10.                   
024400                                                                          
024500 77  WS-KDEMBKOD-TEXT            PIC X(20)   VALUE SPACE.                 
024600     88 KDEMBKOD-TYP-00          VALUE 'NOT COMPARABLE      '.            
024700     88 KDEMBKOD-TYP-60          VALUE 'PROBABLY COMPARABLE '.            
024800     88 KDEMBKOD-TYP-70          VALUE 'COMPARABLE          '.            
024900                                                                          
025000 77  SW-REC-TYPE                 PIC X(03)   VALUE SPACE.                 
025100     88 SS-CHANGE                            VALUE 'SUP'.                 
025200     88 NSP-REG                              VALUE 'NSP'.                 
025300     88 INS-ONLY                             VALUE 'INS'.                 
025400     88 UPD-ONLY                             VALUE 'UPD'.                 
025500     88 EXCH-REG                             VALUE 'EXH'.                 
025600     88 SS-REJECT                            VALUE 'REJ'.                 
025700     88 SOFTWARE-REG                         VALUE 'SW '.                 
025800                                                                          
025900                                                                          
026000 77  WS-KDERS                    PIC 9(2).                                
026100     88  GOOD-KDERS                          VALUE 00 01 02 03            
026110                                                   04 05 06 07            
026120                                                   08 52.                 
026200     88  KDERS-52                            VALUE 52.                    
026300                                                                          
026400 77  SW-DANG-GOODS               PIC X(1)    VALUE 'N'.                   
026500     88  DANG-GOODS-JA                       VALUE 'J'.                   
026600                                                                          
026700 77  SW-SS-CHANGE                PIC X(1)    VALUE 'N'.                   
026800     88  SS-CHANGE-JA                        VALUE 'J'.                   
026900                                                                          
027000 77  SW-SS-REJECT                PIC X       VALUE 'N'.                   
027100     88  SS-REJECT-JA                        VALUE 'J'.                   
027200                                                                          
027300 77  SW-PROJK-GODK               PIC X(1).                                
027400     88  PROJK-GODK                          VALUE 'J'.                   
027500                                                                          
027600 77  SW-PROJ-GODK                PIC X(1).                                
027700     88  PROJ-GODK                           VALUE 'J'.                   
027800                                                                          
027900 77  SW-MAX-ERR                  PIC X       VALUE 'N'.                   
028000     88  NOT-MAX-ERR-PART                    VALUE 'N'.                   
028100     88  MAX-ERR-PART                        VALUE 'J'.                   
028200                                                                          
028300 77  SW-VALID-NSP                PIC X       VALUE 'N'.                   
028400     88  VALID-NSP                           VALUE 'J'.                   
028500                                                                          
028600 77  SW-VALID-INSERT             PIC X       VALUE 'N'.                   
028700     88  VALID-INSERT                        VALUE 'J'.                   
028800                                                                          
028900 77  SW-VALID-UPDATE             PIC X       VALUE 'N'.                   
029000     88  VALID-UPDATE                        VALUE 'J'.                   
029100                                                                          
029200 77  SW-VALID-SOFTWARE           PIC X       VALUE 'N'.                   
029300     88  VALID-SOFTWARE                      VALUE 'J'.                   
029400                                                                          
029500 77  SW-VALID-SS                 PIC X       VALUE 'N'.                   
029600     88  VALID-SUP                           VALUE 'J'.                   
029700                                                                          
029800 77  SW-VALID-REJ                PIC X       VALUE 'N'.                   
029900     88  VALID-REJ                           VALUE 'J'.                   
030000                                                                          
030100 77  SW-VALID-EXCH               PIC X       VALUE 'N'.                   
030200     88  VALID-EXCH                          VALUE 'J'.                   
030300                                                                          
030400 77  SW-PART-STATUS              PIC X       VALUE 'N'.                   
030500     88  PART-EXISTS                         VALUE 'J'.                   
030600                                                                          
030700 77  SW-INPUT                    PIC X       VALUE 'N'.                   
030800     88  VALID-INPUT                         VALUE 'J'.                   
030900     88  INVALID-INPUT                       VALUE 'N'.                   
031000                                                                          
031100*                                                                         
031200 77  W1181101-EOF-SW             PIC X       VALUE 'N'.                   
031300     88  END-OF-W1181101                     VALUE 'J'.                   
031400                                                                          
031500*--------------------------------------------------                       
031600*          BYTES-ARTIKELTEST                                              
031700*--------------------------------------------------                       
031800 01  FILLER                      PIC X(16)   VALUE 'BYTES-ART'.           
031900 01  EXCH-TEST-IDARTNR           PIC 9(9)   COMP-3.                       
032000*01  FILLER -COPY WWBYT01   -RED EXCH-TEST-IDARTNR                        
032100*01  FILLER -COPY WWBYT16   -RED EXCH-TEST-IDARTNR                        
032200     EJECT                                                                
032300 01  OBJ-TEST-IDARTNR            PIC 9(9)   COMP-3.                       
032400*01  FILLER -COPY WWBYT03   -RED OBJ-TEST-IDARTNR                         
032500     EJECT                                                                
032600*--------------------------------------------------                       
032700*          VALID IDDC CODES                                               
032800*--------------------------------------------------                       
032900*01 -COPY WWDC99                                                          
033000     EJECT                                                                
033100*--------------------------------------------------                       
033200*          DATE AREA                                                      
033300*--------------------------------------------------                       
033400*01 -COPY WDATAREA                                                        
033500     EJECT                                                                
033600                                                                          
033700*--------------------------------------------------                       
033800*          VALID PRODUCT GROUP                                            
033900*--------------------------------------------------                       
034000*01 -COPY  WWPRODSL                                                       
034100     EJECT                                                                
034200                                                                          
034300*--------------------------------------------------                       
034400*          TO HANDLE DECIMAL FIELDS                                       
034500*--------------------------------------------------                       
034600*01 -COPY WDECAREA                                                        
034700     EJECT                                                                
034800***************************************                                   
034900* DYNAMIC PROGRAMS AND AREAS                                              
035000***************************************                                   
035100*                                                                         
035200 01  DYNAMISKA-SUBPROGRAM.                                                
035300     03  ABEND                   PIC X(8)     VALUE 'ABEND   '.           
035400     03  POSTSUM                 PIC X(8)     VALUE 'POSTSUM '.           
035500     03  WDATKONV                PIC X(8)     VALUE 'WDATKONV'.           
035600     03  W009REDU                PIC X(8)     VALUE 'W009REDU'.           
035700     03  W009VADD                PIC X(8)     VALUE 'W009VADD'.           
035800     03  WDECEDIT                PIC X(8)     VALUE 'WDECEDIT'.           
035900     03  CBLTDLI                 PIC X(8)     VALUE 'CBLTDLI '.           
036000     03  FELLOG                  PIC X(8)     VALUE 'FELLOG  '.           
036100                                                                          
036200 01  RETURKODER.                                                          
036300   03  RKOD-ABEND-UTAN-DUMP      PIC S9(4)   VALUE +16  COMP SYNC.        
036400   03  RKOD                      PIC S9(4)   VALUE +0   COMP SYNC.        
036500   SKIP2                                                                  
036600                                                                          
036700*    ---VARIABLES TO SUBPROGRAM W009VADD                                  
036800 01  DATUM-AAVV                  PIC S9(5)   COMP-3.                      
036900 01  ANTAL-VECKOR                PIC S9(3)   COMP-3.                      
037000*                                                                         
037100*01  -COPY W0005       -PRE POSTSUM-.                                     
037200     EJECT                                                                
037300 01  IN1-AREA-START              PIC X(24)   VALUE                        
037400                                               'IN1-AREA-START  '.        
037500*01  AREA   -COPY W11810      -PRE IN1-                                   
037600     EJECT                                                                
037700 01  UT1-AREA-START              PIC X(24)   VALUE                        
037800                                               'UT1-AREA-START  '.        
037900*01  AREA   -COPY W1181101    -PRE UT1-                                   
038000     EJECT                                                                
038100 01  UT2-AREA-START              PIC X(24)   VALUE                        
038200                                               'UT2-AREA-START  '.        
038300*01  AREA   -COPY W1181102    -PRE UT2-                                   
038400     EJECT                                                                
038500******************************************************************        
038600*        KEYS TO DLI / NYCKLAR TILL DLI                                   
038700******************************************************************        
038800 01    NYCKLAR-TILL-DLI.                                                  
038900                                                                          
039000   03  W-WDJ1CSEQ-X.                                                      
039100       05 W-IDLEVNR-S            PIC X(5)    VALUE SPACE.                 
039200       05 W-BELEVART-S           PIC X(30)   VALUE SPACE.                 
039300       05 W-IDARTNR-S            PIC S9(9)   COMP-3  VALUE ZERO.          
039400                                                                          
039500   03  W-IDPROJ-MIN              PIC X(4).                                
039600   03  W-IDPROJ-MAX              PIC X(4).                                
039700   03  W-IDARTNR-X.                                                       
039800       05 W-IDARTNR              PIC S9(9)   COMP-3.                      
039900   03  W-KDNOTTYP-X.                                                      
040000       05 W-KDNOTTYP             PIC S9      COMP-3.                      
040100   03  W-IDARTNR-TILLK-X.                                                 
040200       05 W-IDARTNR-TILLK        PIC S9(9)   COMP-3.                      
040300   03  W-IDARTNR-ERS-LOW-X.                                               
040400       05 W-IDARTNR-ERS-LOW      PIC S9(9)   COMP-3  VALUE ZERO.          
040500   03  W-IDARTNR-ERS-HIGH-X.                                              
040600       05 W-IDARTNR-ERS-HIGH     PIC S9(9)   COMP-3                       
040700                                             VALUE +999999999.            
040800   03  W-IDKORTNR-LOW-X.                                                  
040900       05 W-IDKORTNR-LOW         PIC S9(3)   COMP-3  VALUE ZERO.          
041000   03  W-IDKORTNR-HIGH-X.                                                 
041100       05 W-IDKORTNR-HIGH        PIC S9(3)   COMP-3  VALUE +999.          
041200                                                                          
041300   03  W-IDSKYLT-X.                                                       
041400       05 W-IDSKYLT              PIC X(3)    VALUE SPACE.                 
041500   03  W-BEART-X.                                                         
041600       05 W-BEART                PIC X(25)   VALUE SPACE.                 
041700                                                                          
041800   03  W-KDSEGKEY-X.                                                      
041900       05 W-KDSEGKEY             PIC  X(1)   VALUE '1'.                   
042000                                                                          
042100   03  W-WDF5A1KY-MIN.                                                    
042200       05  W-SEQA-IDLEVNR-MIN    PIC X(5)    VALUE LOW-VALUE.             
042300       05  W-SEQA-IDLEVART-MIN   PIC X(30)   VALUE LOW-VALUE.             
042400       05  W-SEQA-IDARTNR-MIN    PIC S9(9)   VALUE ZERO COMP-3.           
042500       05  W-SEQA-IDBENR-MIN     PIC S9(1)   VALUE ZERO COMP-3.           
042600   03  W-WDF5A1KY-MAX.                                                    
042700       05  W-SEQA-IDLEVNR-MAX    PIC X(5)    VALUE HIGH-VALUE.            
042800       05  W-SEQA-IDLEVART-MAX   PIC X(30)   VALUE HIGH-VALUE.            
042900       05  W-SEQA-IDARTNR-MAX    PIC S9(9)   VALUE +999999999             
043000                                               COMP-3.                    
043100       05  W-SEQA-IDBENR-MAX     PIC S9(1)   VALUE +9 COMP-3.             
043200                                                                          
043300   03  W-1207-KEY-X.                                                      
043400       05  FILLER                PIC X(4)    VALUE '1207'.                
043500       05  FILLER                PIC X(26)   VALUE LOW-VALUE.             
043600                                                                          
043700   03  W-1131-KEY-X.                                                      
043800       05  FILLER                PIC X(4)    VALUE '1131'.                
043900       05  W-KDPRODSL            PIC S9(3)   COMP-3 VALUE ZERO.           
044000       05  FILLER                PIC X(24)   VALUE LOW-VALUE.             
044100                                                                          
044200   03  W-1132-KEY-X.                                                      
044300       05  W-IDPROJK             PIC X(4)    VALUE LOW-VALUE.             
044400       05  W-IDPROJOBJ           PIC X(4)    VALUE LOW-VALUE.             
044500       05  W-IDPROJ              PIC X(4)    VALUE LOW-VALUE.             
044600       05  FILLER                PIC X(3)    VALUE LOW-VALUE.             
044700     EJECT                                                                
044800******************************************************************        
044900*        ARBETS-AREOR TILL IMS-SEKTIONERNA                                
045000******************************************************************        
045100*                                                                         
045200 01    IMS-WS.                                                            
045300   03    FILLER                  PIC X(16)   VALUE 'IMS-WS     '.         
045400     SKIP3                                                                
045500*                        **** STATUS-KOD FRÅN IMS                         
045600   03    STATUS-WS               PIC XX.                                  
045700     88    SEGMENT-FINNS                     VALUE '  '.                  
045800     88    SEGMENT-SAKNAS                    VALUE 'GE'.                  
045900     88    BASEN-SLUT                        VALUE 'GB'.                  
046000     SKIP3                                                                
046100   03    GODK-STATUSKODER.                                                
046200     05    GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.              
046300     SKIP3                                                                
046400 01    SSA1                      PIC X(121).                              
046500 01    SSA2                      PIC X(64).                               
046600     EJECT                                                                
046700*                                                                         
046800******************************************************************        
046900*        IMS FUNKTIONSKODER                                               
047000******************************************************************        
047100*                                                                         
047200*01    -COPY W0003                                                        
047300     EJECT                                                                
047400*                                                                         
047500******************************************************************        
047600*        DLI INPUT-OUTPUT AREA                                            
047700******************************************************************        
047800*                                                                         
047900***WLARTC                                                                 
048000 01  FILLER                     PIC X(16) VALUE 'DLI-IO-WDK601'.          
048100 01  DLI-IO-WDK601.                                                       
048200*        05  -COPY WDK601                                                 
048300     EJECT                                                                
048400 01  FILLER                     PIC X(16) VALUE 'DLI-IO-WDK611'.          
048500 01  DLI-IO-WDK611.                                                       
048600*        05  -COPY WDK611                                                 
048700     EJECT                                                                
048800 01  FILLER                     PIC X(16) VALUE 'DLI-IO-WDK625'.          
048900 01  DLI-IO-WDK625.                                                       
049000*        05  -COPY WDK625                                                 
049100     EJECT                                                                
049200***WLBENA                                                                 
049300 01  FILLER                     PIC X(16) VALUE 'DLI-IO-WDD301'.          
049400 01  DLI-IO-WDD301.                                                       
049500*        05  -COPY WDD301     -PRE BENA-                                  
049600     EJECT                                                                
049700 01  FILLER                     PIC X(16) VALUE 'DLI-IO-WDD311'.          
049800 01  DLI-IO-WDD311.                                                       
049900*        05  -COPY WDD311     -PRE BENA-                                  
050000     EJECT                                                                
050100 01  FILLER                     PIC X(16) VALUE 'DLI-IO-WDD312'.          
050200 01  DLI-IO-WDD312.                                                       
050300*        05  -COPY WDD312     -PRE BENA-                                  
050400     EJECT                                                                
050500 01  FILLER                     PIC X(16) VALUE 'DLI-IO-WDD313'.          
050600 01  DLI-IO-WDD313.                                                       
050700*        05  -COPY WDD313     -PRE BENA-                                  
050800     EJECT                                                                
050900***WLXXAQ                                                                 
051000 01  FILLER                     PIC X(16) VALUE 'DLI-IO-WLXXAQ01'.        
051100 01  DLI-IO-WLXXAQ01.                                                     
051200*        05  -COPY WDGX1131   -PRE XXAQ-                                  
051300     EJECT                                                                
051400***WLXXAQ                                                                 
051500 01  FILLER                     PIC X(16) VALUE 'DLI-IO-WLXXAQ11'.        
051600 01  DLI-IO-WLXXAQ11.                                                     
051700*        05  -COPY WDGX1132   -PRE XXAQ-                                  
051800     EJECT                                                                
051900***WLSATB                                                                 
052000 01  FILLER                     PIC X(16) VALUE 'DLI-IO-WDJ101'.          
052100 01  DLI-IO-WDJ101.                                                       
052200*        05  -COPY WDJ101     -PRE SATB-                                  
052300     EJECT                                                                
052400 01  FILLER                     PIC X(16) VALUE 'DLI-IO-WDJ111'.          
052500 01  DLI-IO-WDJ111.                                                       
052600*        05  -COPY WDJ111     -PRE SATB-                                  
052700     EJECT                                                                
052800***WDF5                                                                   
052900 01  FILLER                     PIC X(16) VALUE 'DLI-IO-WDF501'.          
053000 01  DLI-IO-WDF501.                                                       
053100*        05  -COPY WDF501                                                 
053200     EJECT                                                                
053300 01  FILLER                     PIC X(16) VALUE 'DLI-IO-WDF5A1'.          
053400 01  DLI-IO-WDF5A1.                                                       
053500*        05  -COPY WDF5A1                                                 
053600     EJECT                                                                
053700 LINKAGE SECTION.                                                         
053800*01    -COPY W0008     -PRE WDK6-                                         
053900     05  FILLER                 PIC X.                                    
054000     EJECT                                                                
054100*01    -COPY W0008     -PRE WDD3-                                         
054200     05  FILLER                 PIC X.                                    
054300     EJECT                                                                
054400*01    -COPY W0008     -PRE XXAQ-                                         
054500     05  FILLER                 PIC X.                                    
054600     EJECT                                                                
054700*01    -COPY W0008     -PRE WDJ1-                                         
054800     05  FILLER                 PIC X.                                    
054900     EJECT                                                                
055000*01    -COPY W0008     -PRE WDJ1C-                                        
055100     05  FILLER                 PIC X.                                    
055200     EJECT                                                                
055300*01    -COPY W0008     -PRE WDF5-                                         
055400     05  FILLER                 PIC X.                                    
055500     EJECT                                                                
055600*01    -COPY W0008     -PRE WDF5A-                                        
055700     05  FILLER                 PIC X.                                    
055800     EJECT                                                                
055900                                                                          
056000 PROCEDURE DIVISION  USING WDK6-PCB  WDD3-PCB XXAQ-PCB  WDJ1-PCB          
056100                           WDJ1C-PCB WDF5-PCB WDF5A-PCB.                  
056200                                                                          
056300     ENTRY 'DLITCBL' USING WDK6-PCB  WDD3-PCB XXAQ-PCB  WDJ1-PCB          
056400                           WDJ1C-PCB WDF5-PCB WDF5A-PCB.                  
056500                                                                          
056600     PERFORM A-INIT                                                       
056700     PERFORM S01-READ-W1181101                                            
056800     PERFORM UNTIL END-OF-W1181101                                        
056900                                                                          
057000       PERFORM B-REINIT-WORK                                              
057100                                                                          
057200       MOVE IN1-IDARTNR              TO WS-IDARTNR                        
057300                                                                          
057400       IF WS-IDARTNR NUMERIC    AND                                       
057500          IDARTNR-WS     > ZERO AND                                       
057600          IDARTNR-WS NOT > 99999999                                       
057700          CONTINUE                                                        
057800       ELSE                                                               
057900          MOVE SPACES                   TO WS-FELTEXT                     
058000          MOVE 'PART NUM INVALID '      TO WS-FELTEXT                     
058100          SET INVALID-INPUT             TO TRUE                           
058200          PERFORM G-POPULATE-ERROR-INDEX                                  
058300       END-IF                                                             
058400*                                                                         
058500       IF IN1-FLSPARE = JA OR NEJ                                         
058600          CONTINUE                                                        
058700       ELSE                                                               
058800          MOVE SPACES                   TO WS-FELTEXT                     
058900          MOVE 'SPARE PART FLAG INVALID'                                  
059000                                        TO WS-FELTEXT                     
059100          SET INVALID-INPUT             TO TRUE                           
059200          PERFORM G-POPULATE-ERROR-INDEX                                  
059300       END-IF                                                             
059400*                                                                         
059420       IF IN1-FLSSCHG = JA OR NEJ                                         
059430          CONTINUE                                                        
059440       ELSE                                                               
059450          MOVE SPACES                   TO WS-FELTEXT                     
059460          MOVE 'REPLACE FLAG INVALID'                                     
059470                                        TO WS-FELTEXT                     
059480          SET INVALID-INPUT             TO TRUE                           
059490          PERFORM G-POPULATE-ERROR-INDEX                                  
059491       END-IF                                                             
059492*                                                                         
059500       IF IN1-FLBYTES-MAN = JA OR NEJ                                     
059600          CONTINUE                                                        
059700       ELSE                                                               
059800          MOVE SPACES                   TO WS-FELTEXT                     
059900          MOVE 'EXCH INFO INVALID'                                        
060000                                        TO WS-FELTEXT                     
060100          SET INVALID-INPUT             TO TRUE                           
060200          PERFORM G-POPULATE-ERROR-INDEX                                  
060300       END-IF                                                             
060400*                                                                         
060500       IF IN1-KDARTSYS = 'TC'                                             
060600          CONTINUE                                                        
060700       ELSE                                                               
060800          MOVE SPACES                   TO WS-FELTEXT                     
060900          MOVE 'PART MASTER NOT TCPLM'                                    
061000                                        TO WS-FELTEXT                     
061100          SET INVALID-INPUT             TO TRUE                           
061200          PERFORM G-POPULATE-ERROR-INDEX                                  
061300       END-IF                                                             
061400                                                                          
061500       IF VALID-INPUT                                                     
061600                                                                          
061700          MOVE IDARTNR-WS               TO W-IDARTNR                      
061800          PERFORM IMS-GET-WDK601                                          
061900                                                                          
062000          IF SEGMENT-FINNS                                                
062100             SET PART-EXISTS            TO TRUE                           
062200             IF ART-KDERS-UTG > ZERO                                      
062300                MOVE SPACES             TO WS-FELTEXT                     
062400                MOVE 'OBSOLETE SPARE PART '                               
062500                                        TO WS-FELTEXT                     
062600                MOVE NEJ                TO INPUT-RETT                     
062700                PERFORM G-POPULATE-ERROR-INDEX                            
062800             END-IF                                                       
062900*                                                                         
063000***          CHECK AND SET RECORD TYPE                                    
063100             PERFORM S31-CHECK-RECORD-TYPE                                
063200*                                                                         
063500             IF NOT SS-REJECT                                             
063700                PERFORM C-VALIDATE-ATTRIBUTES                             
063800                IF INPUT-RETT = JA                                        
063900                   PERFORM F-CHECK-UPDATE-FIELDS                          
064000                END-IF                                                    
064100             END-IF                                                       
064200             IF INPUT-RETT = JA                                           
064300                EVALUATE TRUE                                             
064400                    WHEN SS-CHANGE                                        
064500                         SET VALID-SUP    TO TRUE                         
064600                    WHEN UPD-ONLY                                         
064700                         SET VALID-UPDATE TO TRUE                         
064800                    WHEN NSP-REG                                          
064900                         SET VALID-NSP    TO TRUE                         
065000                    WHEN EXCH-REG                                         
065100                         SET VALID-EXCH   TO TRUE                         
065200                    WHEN SS-REJECT                                        
065300                         SET VALID-REJ    TO TRUE                         
065400                    WHEN SOFTWARE-REG                                     
065500                         SET VALID-SOFTWARE                               
065600                                          TO TRUE                         
065700                                                                          
065800                                                                          
065900                END-EVALUATE                                              
066000             END-IF                                                       
066100          ELSE                                                            
066200***          CHECK AND SET RECORD TYPE                                    
066300             PERFORM S31-CHECK-RECORD-TYPE                                
066400*                                                                         
066700             IF NOT SS-REJECT                                             
066900                PERFORM C-VALIDATE-ATTRIBUTES                             
067000                PERFORM F-CHECK-UPDATE-FIELDS                             
067100             END-IF                                                       
067200             IF INPUT-RETT = JA                                           
067300                EVALUATE TRUE                                             
067400                    WHEN SS-CHANGE                                        
067500                         SET VALID-SUP       TO TRUE                      
067600                    WHEN INS-ONLY                                         
067700                         SET VALID-INSERT    TO TRUE                      
067800                    WHEN NSP-REG                                          
067900                         SET VALID-NSP       TO TRUE                      
068000                    WHEN EXCH-REG                                         
068100                         SET VALID-EXCH      TO TRUE                      
068200                    WHEN SS-REJECT                                        
068300                         SET VALID-REJ       TO TRUE                      
068400                    WHEN SOFTWARE-REG                                     
068500                         SET VALID-SOFTWARE  TO TRUE                      
068600                END-EVALUATE                                              
068700             END-IF                                                       
068800          END-IF                                                          
068900       END-IF                                                             
069000                                                                          
069100       IF VALID-INSERT OR                                                 
069200          VALID-UPDATE OR                                                 
069300          VALID-SUP    OR                                                 
069400          VALID-NSP    OR                                                 
069500          VALID-EXCH   OR                                                 
069600          VALID-REJ    OR                                                 
069700          VALID-SOFTWARE                                                  
069800          PERFORM I-CREATE-PRINS-FILE                                     
069900       ELSE                                                               
070000***       WRITE ERROR FILE                                                
070100          PERFORM H-CREATE-ERROR-FILE                                     
070200       END-IF                                                             
070300                                                                          
070400       PERFORM S01-READ-W1181101                                          
070500                                                                          
070600     END-PERFORM                                                          
070700                                                                          
070800     PERFORM Z-FINIT                                                      
070900     MOVE ZERO TO RETURN-CODE                                             
071000     GOBACK                                                               
071100     .                                                                    
071200     EJECT                                                                
071300 A-INIT SECTION.                                                          
071400                                                                          
071500     MOVE 'A-INIT         '      TO WS-CURRENT-SECTION                    
071600                                                                          
071700     OPEN INPUT  W1181101                                                 
071800          OUTPUT W1181102                                                 
071900                 W1181103                                                 
072000                                                                          
072100     MOVE IDPGM                  TO POSTSUM-PROGNAMN                      
072200                                                                          
072300     ACCEPT DAGENS-AAMMDD FROM DATE                                       
072400                                                                          
072500     MOVE DAGENS-AAMMDD          TO DAT-I-TIDATUM                         
072600     MOVE 'AAMMDD'               TO DAT-KDDATFORM                         
072700     PERFORM S98-WDATKONV                                                 
072800                                                                          
072900     IF DAT-KDSVAR-OK                                                     
073000        MOVE DAT-TIAA-VECKA      TO SPAR-DAGENS-AA                        
073100        MOVE DAT-TIVV            TO SPAR-DAGENS-VV                        
073200     END-IF                                                               
073300                                                                          
073400*                                                                         
073500***  BELOW CALCULATED ONCE IN THE BATCH.                                  
073600***  SOP DATE FOR OBJEKT IS CURRENT WEEK + 1 WEEK (DAY 1)                 
073700*                                                                         
073800     MOVE SPAR-DAGENS-AAVV-R     TO DATUM-AAVV                            
073900     MOVE 1                      TO ANTAL-VECKOR                          
074000                                                                          
074100     CALL W009VADD            USING DATUM-AAVV                            
074200                                    ANTAL-VECKOR                          
074300                                                                          
074400     MOVE DATUM-AAVV             TO WS-IN-AAVV-OBJ                        
074500     MOVE '1'                    TO WS-IN-DAG-OBJ                         
074600     .                                                                    
074700     EJECT                                                                
074800                                                                          
074900 B-REINIT-WORK  SECTION.                                                  
075000                                                                          
075200     INITIALIZE                        WS-GRP-FELTEXT                     
075300                                       WS-IDPTYP                          
075400                                       WS-VLFG                            
075500                                       WS-IDARTNR-MOTSV                   
075600                                       WS-KDEMBKOD-2                      
075700                                       WS-KDEMBKOD-TEXT                   
075800                                       WS-IN-TISOP                        
075810                                       WS-IN-TIERSDAT                     
075820                                       WS-IN-TIERS-AAMMDD                 
075900                                       WS-IDRITN                          
076000                                       WS-VKART-NTO                       
076100                                       WS-KDSORT-VLFG                     
076200                                       WS-KDSORT                          
076300                                       SW-REC-TYPE                        
076400                                                                          
076500     MOVE ALL ZERO                  TO WS-IDPROENH                        
076600                                       WS-VLFG-NUM                        
076700                                                                          
076800     MOVE NEJ                       TO SW-MAX-ERR                         
076900                                       SW-VALID-INSERT                    
077000                                       SW-VALID-UPDATE                    
077100                                       SW-VALID-SOFTWARE                  
077200                                       SW-VALID-SS                        
077300                                       SW-VALID-NSP                       
077400                                       SW-VALID-EXCH                      
077500                                       SW-VALID-REJ                       
077600                                       SW-PART-STATUS                     
077700                                       SW-SS-CHANGE                       
077800                                       SW-SS-REJECT                       
077900                                       SW-DANG-GOODS                      
078000                                       WS-FLRSBEART                       
078100                                       WS-FLPISK                          
078200                                                                          
078300     MOVE JA                        TO SW-INPUT                           
078400                                       INPUT-RETT                         
078500                                       NYPON-ARTIKEL                      
078600                                                                          
078700     MOVE 'GB'                      TO WS-IDSKYLT                         
078800     .                                                                    
078900     EJECT                                                                
079000                                                                          
079100 C-VALIDATE-ATTRIBUTES SECTION.                                           
079200     SKIP2                                                                
079300     MOVE 'C-VALIDATE-ATT '      TO WS-CURRENT-SECTION                    
079400*                                                                         
079500***  IN 1116 REFER SECTION XXX-REG-ARTIKEL-ARTREG-EV-NYPON                
079600*                                                                         
079700                                                                          
079800     PERFORM S71-CHK-COMMON-DATA                                          
079900*                                                                         
080000     IF INS-ONLY  OR                                                      
080100        UPD-ONLY  OR                                                      
080200        SS-CHANGE OR                                                      
080300        EXCH-REG  OR                                                      
080400        SOFTWARE-REG                                                      
080500        PERFORM S72-CHK-REG-INPUTS                                        
080600        IF SOFTWARE-REG                                                   
080700           PERFORM S74-VALIDATE-SW-SPECIFIC                               
080800        END-IF                                                            
080900*                                                                         
081000        PERFORM S86-KOLLA-OM-BEN-FINNS                                    
081100     END-IF                                                               
081200     .                                                                    
081300     EJECT                                                                
081400 F-CHECK-UPDATE-FIELDS         SECTION.                                   
081500     SKIP2                                                                
081600     MOVE 'F-CHECK        '      TO WS-CURRENT-SECTION                    
081700                                                                          
081800     IF NOT NSP-REG                                                       
081900        PERFORM S95-VALIDATE-UPDATE-FIELDS                                
082000     END-IF                                                               
082100     .                                                                    
082200     EJECT                                                                
082300 G-POPULATE-ERROR-INDEX        SECTION.                                   
082400                                                                          
082500     MOVE 1                      TO WS-INDX                               
082600     IF MAX-ERR-PART                                                      
082700                                                                          
082800        STRING 'PART NUM :' DELIMITED BY SIZE                             
082900        WS-IDARTNR          DELIMITED BY SPACES                           
083000        ' CONTAIN ERROR - ' DELIMITED BY SIZE                             
083100        WS-FELTEXT          DELIMITED BY SIZE                             
083200                               INTO WS-DISP-FELTEXT                       
083300     ELSE                                                                 
083400        PERFORM UNTIL WS-INDX   > MAX-INDX                                
083500                                                                          
083600          IF WS-TAB-FELTEXT (WS-INDX)                                     
083700                            NOT > SPACES                                  
083800             MOVE WS-FELTEXT     TO WS-TAB-FELTEXT (WS-INDX)              
083900             IF WS-INDX = MAX-INDX                                        
084000                SET MAX-ERR-PART TO TRUE                                  
084100             ELSE                                                         
084200                MOVE MAX-INDX    TO WS-INDX                               
084300             END-IF                                                       
084400          END-IF                                                          
084500          ADD 1                  TO WS-INDX                               
084600                                                                          
084700        END-PERFORM                                                       
084800     END-IF                                                               
084900     .                                                                    
085000     EJECT                                                                
085100 H-CREATE-ERROR-FILE SECTION.                                             
085200                                                                          
085300     MOVE 'H-CREATE       '      TO WS-CURRENT-SECTION                    
085400                                                                          
085500     INITIALIZE UT2-AREA                                                  
085600     MOVE IN1-IDARTNR            TO UT2-IDARTNR                           
085700     MOVE IN1-IDBERED            TO UT2-IDBERED                           
085800     MOVE IN1-IDCDS              TO UT2-IDCDS                             
085900                                                                          
085910     IF IN1-KDSORT = 'SW'                                                 
085911          MOVE 'SP'              TO UT2-SPAREPARTUNIT                     
085912     ELSE                                                                 
085913          MOVE 'HW'              TO UT2-SPAREPARTUNIT                     
085920     END-IF                                                               
085930                                                                          
086000     MOVE 1                      TO WS-INDX                               
086100                                                                          
086200     PERFORM UNTIL WS-INDX   > MAX-INDX                                   
086300       IF WS-TAB-FELTEXT (WS-INDX) > SPACES                               
086400          MOVE WS-TAB-FELTEXT (WS-INDX)                                   
086500                                 TO UT2-FELTEXT (WS-INDX)                 
086600       ELSE                                                               
086700          MOVE MAX-INDX          TO WS-INDX                               
086800       END-IF                                                             
086900       ADD 1                     TO WS-INDX                               
087000     END-PERFORM                                                          
087100                                                                          
087200     PERFORM S12-WRITE-W1181103                                           
087300     .                                                                    
087400     EJECT                                                                
087500 I-CREATE-PRINS-FILE SECTION.                                             
087600                                                                          
087700     MOVE 'I-CREATE       '      TO WS-CURRENT-SECTION                    
087800                                                                          
087900     MOVE LOW-VALUES             TO UT1-AREA                              
088000*                                                                         
088100     MOVE WS-IDPTYP              TO UT1-IDPTYP                            
088200     MOVE IN1-IDARTNR            TO UT1-IDARTNR                           
088300     MOVE IN1-IDPRODNR           TO UT1-IDPRODNR                          
088400     MOVE IN1-IDBERED            TO UT1-IDBERED                           
088500     MOVE IN1-KDPRODSL           TO UT1-KDPRODSL                          
088600     MOVE  WS-KDSORT             TO UT1-KDSORT                            
088700     MOVE  WS-IDPROENH           TO UT1-IDPROENH                          
088800     MOVE IN1-KDYTBEH            TO UT1-KDYTBEH                           
088900     MOVE IN1-IDPROJ             TO UT1-IDPROJ                            
089000     MOVE IN1-KDFARLIG           TO UT1-KDFARLIG                          
089100     MOVE IN1-KDBPSR             TO UT1-KDBPSR                            
089200     MOVE IN1-IDKAT-1            TO UT1-IDKAT-1                           
089300     MOVE IN1-IDKAT-2            TO UT1-IDKAT-2                           
089400     MOVE IN1-IDKAT-3            TO UT1-IDKAT-3                           
089500     MOVE IN1-KDUART             TO UT1-KDUART                            
089600     MOVE IN1-IDPROJK            TO UT1-IDPROJK                           
089700     MOVE  WS-FLPISK             TO UT1-FLPISK                            
089800     MOVE IN1-IDAO               TO UT1-IDAO                              
089900     MOVE  WS-IN-TISOP           TO UT1-TISOP                             
090000     MOVE  WS-IDSKYLT            TO UT1-IDSKYLT                           
090100     MOVE IN1-FLLSRDEL           TO UT1-FLLSRDEL                          
090200     MOVE IN1-IDPROJUP           TO UT1-IDPROJUP                          
090300     MOVE IN1-BEART              TO UT1-BEART                             
090400     MOVE  WS-FLRSBEART          TO UT1-FLRSBEART                         
090500     MOVE IN1-IDFKNGRP           TO UT1-IDFKNGRP                          
090600     MOVE IN1-TEORSAK-1          TO UT1-TEORSAK-1                         
090700     MOVE IN1-TEARTNOT-2         TO UT1-TEARTNOT-2                        
090800     MOVE  WS-IDRITN             TO UT1-IDRITN                            
090900     MOVE IN1-TEARTNOT-7         TO UT1-TEARTNOT-7                        
091000     MOVE IN1-TEARTNOT-4         TO UT1-TEARTNOT-4                        
091100     MOVE  WS-IDARTNR-MOTSV      TO UT1-IDARTNR-MOTSV                     
091200     MOVE IN1-AVSL-MOT           TO UT1-AVSL-MOT                          
091300     MOVE IN1-IDPSN              TO UT1-IDPSN                             
091400     MOVE IN1-KDARTHNT           TO UT1-KDARTHNT                          
091500     MOVE  WS-KDEMBKOD-2         TO UT1-KDEMBKOD-2                        
091600     MOVE  WS-VLFG               TO UT1-VLFG                              
091700     MOVE  WS-KDSORT-VLFG        TO UT1-KDSORT-VLFG                       
091800     MOVE  WS-VKART-NTO          TO UT1-VKART-NTO                         
091900     MOVE IN1-IDCDS              TO UT1-IDCDS                             
092000     MOVE IN1-KDARTSYS           TO UT1-KDARTSYS                          
092100     MOVE IN1-KDERS              TO UT1-KDERS                             
092200     MOVE IN1-IDLEVNR            TO UT1-IDLEVNR                           
092300     MOVE IN1-IDPLANGR-AG        TO UT1-IDPLANGR-AG                       
092400     MOVE IN1-IDANSK             TO UT1-IDANSK                            
092500     IF IN1-KDSORT = 'SW'                                                 
092600        MOVE SPAR-PRARTSTD       TO UT1-PRARTSTD                          
092700     ELSE                                                                 
092800        MOVE '0000000.00'        TO UT1-PRARTSTD                          
092900     END-IF                                                               
093000     MOVE IN1-KVPB-C1            TO UT1-KVPB-C1                           
093010     MOVE IN1-DIERS-ERS          TO UT1-DIERS-ERS                         
093020     MOVE  WS-IN-TIERSDAT        TO UT1-TIERSDAT-PREL                     
093030     MOVE IN1-KVRADER            TO UT1-KVRADER-MAX9                      
093040                                                                          
093110     MOVE +1                     TO RAD-IX                                
093120     PERFORM UNTIL RAD-IX > UT1-KVRADER-MAX9                              
093121        MOVE IN1-IDARTNR-TILLK (RAD-IX)                                   
093122                                 TO UT1-IDARTNR-TILLK (RAD-IX)            
093123        MOVE IN1-DIERS-TILLK (RAD-IX)                                     
093124                                 TO UT1-DIERS-TILLK (RAD-IX)              
093125        MOVE IN1-BEERS-GRP (RAD-IX)                                       
093126                                 TO UT1-BEERS-GRP (RAD-IX)                
093127                                                                          
093129        ADD +1                   TO RAD-IX                                
093130     END-PERFORM                                                          
093140                                                                          
093200     PERFORM S11-WRITE-W1181102                                           
093300     .                                                                    
093400     EJECT                                                                
093500 S01-READ-W1181101 SECTION.                                               
093600                                                                          
093700     READ W1181101             INTO IN1-AREA                              
093800     AT END                                                               
093900        MOVE HIGH-VALUE          TO IN1-AREA                              
094000        SET END-OF-W1181101      TO TRUE                                  
094100                                                                          
094200     NOT AT END                                                           
094300        MOVE 'W1181101'          TO POSTSUM-FDNAMN                        
094400        MOVE 'W11811D1'          TO POSTSUM-DDNAMN2                       
094500        MOVE 'IN1'               TO POSTSUM-TRANSTYP                      
094600        CALL POSTSUM          USING POSTSUM-PARM                          
094700     END-READ                                                             
094800*                                                                         
095000     IF IN1-KDERS = 52                                                    
095100        MOVE 1                   TO IN1-DIERS-ERS                         
095110        MOVE '-'                 TO IN1-IDAO                              
095200     END-IF                                                               
095300*                                                                         
095400     .                                                                    
095500     EJECT                                                                
095600 S11-WRITE-W1181102 SECTION.                                              
095700                                                                          
095800     WRITE UT1-POST            FROM UT1-AREA                              
095900                                                                          
096000     MOVE 'UT1'                  TO POSTSUM-TRANSTYP                      
096100     MOVE 'W1181102'             TO POSTSUM-FDNAMN                        
096200     MOVE 'W11811D2'             TO POSTSUM-DDNAMN2                       
096300     CALL POSTSUM             USING POSTSUM-PARM                          
096400     .                                                                    
096500     EJECT                                                                
096600 S12-WRITE-W1181103 SECTION.                                              
096700                                                                          
096800     WRITE UT2-POST            FROM UT2-AREA                              
096900                                                                          
097000     MOVE 'UT2'                  TO POSTSUM-TRANSTYP                      
097100     MOVE 'W1181103'             TO POSTSUM-FDNAMN                        
097200     MOVE 'W11811D3'             TO POSTSUM-DDNAMN2                       
097300     CALL POSTSUM             USING POSTSUM-PARM                          
097400     .                                                                    
097500     EJECT                                                                
097600 S31-CHECK-RECORD-TYPE  SECTION.                                          
097700                                                                          
097800     MOVE IN1-KDPRODSL                   TO WS-KDPRODSL                   
097900     PERFORM S32-CHECK-SUPERSESSION                                       
098000                                                                          
098100     IF IN1-FLSPARE = NEJ                                                 
098200        IF IN1-KDSORT = 'SW'                                              
098300           MOVE SPACES                   TO WS-FELTEXT                    
098400           MOVE 'SP-SPAREPART FLAG IS INVALID'                            
098500                                         TO WS-FELTEXT                    
098600           MOVE NEJ                      TO INPUT-RETT                    
098700           PERFORM G-POPULATE-ERROR-INDEX                                 
098800        ELSE                                                              
098900          IF IN1-FLBYTES-MAN = JA                                         
099000             IF PART-EXISTS                                               
099100                MOVE 'EXU'               TO WS-IDPTYP                     
099200             ELSE                                                         
099300                MOVE 'EXI'               TO WS-IDPTYP                     
099400             END-IF                                                       
099500             IF SS-CHANGE-JA                                              
099600                MOVE 'EXS'               TO WS-IDPTYP                     
099700             END-IF                                                       
099800          ELSE                                                            
099900             MOVE 'NSP'                  TO WS-IDPTYP                     
100000          END-IF                                                          
100100        END-IF                                                            
100200     ELSE                                                                 
100300        IF IN1-KDSORT = 'SW'                                              
100400           IF SS-CHANGE-JA                                                
100500              MOVE 'SWS'                 TO WS-IDPTYP                     
100600           ELSE                                                           
100700              IF PART-EXISTS                                              
100800                 MOVE 'SWU'              TO WS-IDPTYP                     
100900              ELSE                                                        
101000                 MOVE 'SWI'              TO WS-IDPTYP                     
101100              END-IF                                                      
101200           END-IF                                                         
101300        ELSE                                                              
101400           IF SS-CHANGE-JA                                                
101500              IF PART-EXISTS                                              
101600                 MOVE 'SSU'              TO WS-IDPTYP                     
101700              ELSE                                                        
101800                 MOVE 'SSI'              TO WS-IDPTYP                     
101900              END-IF                                                      
102000           ELSE                                                           
102100              IF PART-EXISTS                                              
102200                 MOVE 'UPD'              TO WS-IDPTYP                     
102300              ELSE                                                        
102400                 MOVE 'INS'              TO WS-IDPTYP                     
102500              END-IF                                                      
102600           END-IF                                                         
102700        END-IF                                                            
102800     END-IF                                                               
102900                                                                          
103000     IF SS-REJECT-JA                                                      
103100        IF PART-EXISTS                                                    
103200           MOVE 'REJ'                    TO WS-IDPTYP                     
103300        ELSE                                                              
103400           IF IN1-KDSORT = 'SW'                                           
103500              MOVE SPACES                TO WS-FELTEXT                    
103600              MOVE 'INVALID REPL CODE-NEW SP'                             
103700                                         TO WS-FELTEXT                    
103800              MOVE NEJ                   TO INPUT-RETT                    
103900              PERFORM G-POPULATE-ERROR-INDEX                              
104000           ELSE                                                           
104100              MOVE 'NSP'                 TO WS-IDPTYP                     
104200           END-IF                                                         
104300        END-IF                                                            
104400     END-IF                                                               
104500     PERFORM S31A-CHECK-IDTYP                                             
104600     .                                                                    
104700     EJECT                                                                
104800 S31A-CHECK-IDTYP SECTION.                                                
104900                                                                          
105000***  CHECK IF TYPE AND SET SET APPROPRIATE FLAG FOR VALIDATION            
105100***  IF WE GET ABEND FROM BELOW, SUPPORT CHECK THE ISSUE NEXT DAY         
105200***  THE PART NUMBER CAN BE REMOVED AND JOB CAN BE RERUN                  
105300***  FOR EXCHANGE PARTS EXI OR EXU, WE CAN END ROUTINE AND                
105400***  DEVELOPER CAN RERUN NEXT DAY AFTER FIXING ISSUE.                     
105500*                                                                         
105600     EVALUATE WS-IDPTYP                                                   
105700        WHEN 'INS'                                                        
105800           SET INS-ONLY     TO TRUE                                       
105900        WHEN 'UPD'                                                        
106000           SET UPD-ONLY     TO TRUE                                       
106100        WHEN 'NSP'                                                        
106200           SET NSP-REG      TO TRUE                                       
106300        WHEN 'EXI'                                                        
106400        WHEN 'EXU'                                                        
106500        WHEN 'EXS'                                                        
106600           SET EXCH-REG     TO TRUE                                       
106700        WHEN 'SSI'                                                        
106800        WHEN 'SSU'                                                        
106900           SET SS-CHANGE    TO TRUE                                       
107000        WHEN 'REJ'                                                        
107100           SET SS-REJECT    TO TRUE                                       
107200        WHEN 'SWI'                                                        
107300        WHEN 'SWU'                                                        
107400        WHEN 'SWS'                                                        
107500           SET SOFTWARE-REG TO TRUE                                       
107600        WHEN OTHER                                                        
107700           DISPLAY 'INVALID TYP FOR PART :' IN1-IDARTNR                   
107800           CALL FELLOG                                                    
107900     END-EVALUATE                                                         
108000     .                                                                    
108100     EJECT                                                                
108200 S32-CHECK-SUPERSESSION SECTION.                                          
108300                                                                          
108400***  CHECK IF CHANGE OF SUPERSESSION CODE                                 
108500*                                                                         
108600     MOVE NEJ                         TO SW-SS-CHANGE                     
108700                                         SW-SS-REJECT                     
108710     MOVE IN1-KDERS                   TO WS-KDERS                         
108720                                                                          
108800     IF PART-EXISTS                                                       
108900        PERFORM IMS-GNP-WDK611                                            
109000        IF SEGMENT-FINNS                                                  
109010           IF IN1-FLSSCHG    = JA                                         
109020              IF GOOD-KDERS                                               
109021                 IF IN1-KDERS   = ZERO  OR                                
109022                   (                                                      
109023                    IN1-KDERS > 0  AND                                    
109024                    CLAG-KDERS < 10                                       
109026                   )                                                      
109030                   MOVE JA            TO SW-SS-CHANGE                     
109031                 ELSE                                                     
109032                   MOVE SPACES        TO WS-FELTEXT                       
109033                   MOVE 'INVALID REPL CODE,OLD CODE>10'                   
109034                                      TO WS-FELTEXT                       
109035                   MOVE NEJ           TO INPUT-RETT                       
109036                   PERFORM G-POPULATE-ERROR-INDEX                         
109037                 END-IF                                                   
109040              ELSE                                                        
109050                 MOVE SPACES          TO WS-FELTEXT                       
109060                 MOVE 'INVALID REPL CODE'                                 
109070                                      TO WS-FELTEXT                       
109080                 MOVE NEJ             TO INPUT-RETT                       
109090                 PERFORM G-POPULATE-ERROR-INDEX                           
109091              END-IF                                                      
109092           END-IF                                                         
110701                                                                          
110702           IF KDERS-52 AND ( CLAG-KDERS NOT = 52)                         
110703              MOVE JA                 TO SW-SS-REJECT                     
110704              IF IN1-FLBYTES-MAN = JA  AND                                
110705                 KDERS-52                                                 
110706                 MOVE IN1-IDPRODNR    TO WS-IDPRODNR-EXCH                 
110707              END-IF                                                      
110708           END-IF                                                         
110709                                                                          
110710           IF IN1-FLBYTES-MAN = JA AND IN1-IDPROJ = 'OBJ' AND             
110711              IN1-IDPRODNR    = WS-IDPRODNR-EXCH                          
110712*          TO PROCESS OBJ PART AS IN THE SAME WAY AS THE                  
110713*          CORRESPONDING EXCHANGE PART                                    
110714              MOVE JA                 TO SW-SS-REJECT                     
110715              MOVE SPACES             TO WS-IDPRODNR-EXCH                 
110720           END-IF                                                         
110800        END-IF                                                            
110900                                                                          
110910     ELSE                                                                 
111000*NEW PART FLOW                                                            
111100        IF IN1-FLSPARE = JA                                               
111200           IF IN1-KDERS = ZERO                                            
111300              CONTINUE                                                    
111400           ELSE                                                           
111410              IF GOOD-KDERS                                               
111420                 MOVE JA                    TO SW-SS-CHANGE               
111500                 IF IN1-KDERS = 52                                        
111600                    MOVE JA                 TO SW-SS-REJECT               
111700                 END-IF                                                   
111800              ELSE                                                        
112000                 MOVE SPACES                TO WS-FELTEXT                 
112100                 MOVE 'INVALID REPL CODE'                                 
112200                                            TO WS-FELTEXT                 
112300                 MOVE NEJ                   TO INPUT-RETT                 
112400                 PERFORM G-POPULATE-ERROR-INDEX                           
112500              END-IF                                                      
112600           END-IF                                                         
112700        ELSE                                                              
112800           IF IN1-FLBYTES-MAN = JA  AND                                   
112900              IN1-KDERS       > ZERO                                      
113000              MOVE SPACES                   TO WS-FELTEXT                 
113100              MOVE 'INVALID REPL CODE-NEW EXCH'                           
113200                                            TO WS-FELTEXT                 
113300              MOVE NEJ                      TO INPUT-RETT                 
113400              PERFORM G-POPULATE-ERROR-INDEX                              
113500              MOVE IN1-IDPRODNR             TO WS-IDPRODNR-EXCH           
113600           END-IF                                                         
113700                                                                          
113800           IF IN1-FLBYTES-MAN = JA AND IN1-IDPROJ = 'OBJ' AND             
113900              IN1-IDPRODNR    = WS-IDPRODNR-EXCH                          
114000*          TO THROW ERROR FOR OBJ PART IF THE CORRESPONDING               
114100*          EXCHANGE PART IS FAILED DUE TO SS CODE IN ABOVE                
114200*          CONDITION                                                      
114300              MOVE SPACES                   TO WS-FELTEXT                 
114400              MOVE 'INVALID REPL CODE-NEW EXCH'                           
114500                                            TO WS-FELTEXT                 
114600              MOVE NEJ                      TO INPUT-RETT                 
114700              PERFORM G-POPULATE-ERROR-INDEX                              
114800              MOVE SPACES                   TO WS-IDPRODNR-EXCH           
114900           END-IF                                                         
115000                                                                          
115100        END-IF                                                            
115200     END-IF                                                               
115300     .                                                                    
115400     EJECT                                                                
115500 S71-CHK-COMMON-DATA  SECTION.                                            
115600                                                                          
115700     MOVE 'S71-CHK-COMMON '           TO WS-CURRENT-SECTION               
115800*                                                                         
115900***  IN 1116 REFER SECTION XXX-NYA-GEMENSAMMA-DATAELEMENT                 
116000***  IN 1116 REFER SECTION XXX-KOLLA-INDATA-NYREG                         
116100     INSPECT IN1-BEART REPLACING ALL '<' BY SPACE                         
116200     INSPECT IN1-BEART REPLACING ALL '>' BY SPACE                         
116300                                                                          
116400*                                                                         
116500***  FOR NSP MOVE SPACES TO BEART-SVE IN THE PGM W11817                   
116600***  ALLOW SPACES FOR NON SPARE PARTS                                     
116700*                                                                         
116800     IF IN1-BEART = SPACE                                                 
116900        IF NSP-REG                                                        
117000           MOVE IN1-BEART             TO WS-BEART                         
117100        ELSE                                                              
117200           MOVE NEJ TO INPUT-RETT                                         
117300           MOVE SPACE                 TO WS-FELTEXT                       
117400           MOVE 'DESCRIPTION MISSING' TO WS-FELTEXT                       
117500           PERFORM G-POPULATE-ERROR-INDEX                                 
117600        END-IF                                                            
117700     ELSE                                                                 
117800        MOVE IN1-BEART               TO WS-BEART                          
117900     END-IF                                                               
118000*                                                                         
118100     IF IN1-IDRITN = SPACE                                                
118200        MOVE 'SEE TCPLM'             TO WS-IDRITN                         
118300     ELSE                                                                 
118400        MOVE IN1-IDRITN              TO WS-IDRITN                         
118500     END-IF                                                               
118600*                                                                         
118700     IF IN1-IDPROENH   = SPACE                                            
118800        CONTINUE                                                          
118900     ELSE                                                                 
119000        IF IN1-IDPROENH    NUMERIC                                        
119100           MOVE IN1-IDPROENH         TO WS-IDPROENH                       
119200        ELSE                                                              
119300           MOVE NEJ TO INPUT-RETT                                         
119400           MOVE SPACE                TO WS-FELTEXT                        
119500           MOVE 'PROD UNIT-1 INVALID'                                     
119600                                     TO WS-FELTEXT                        
119700           PERFORM G-POPULATE-ERROR-INDEX                                 
119800        END-IF                                                            
119900     END-IF                                                               
120000*                                                                         
120100     IF NSP-REG                                                           
120200        IF IN1-KDSORT = '  ' OR 'MM' OR 'C2' OR 'G ' OR 'ST'              
120300           MOVE IN1-KDSORT           TO WS-KDSORT                         
120400           IF IN1-KDSORT = '  '                                           
120500              MOVE 'ST'              TO WS-KDSORT                         
120600           END-IF                                                         
120700        ELSE                                                              
120800           MOVE 'ST'                 TO WS-KDSORT                         
120900        END-IF                                                            
121000     ELSE                                                                 
121100        IF IN1-KDSORT = 'ST' OR 'SA' OR 'KG' OR 'M ' OR ' M'              
121200                   OR ' L' OR 'L ' OR 'MM' OR 'G ' OR ' G' OR 'C2'        
121300                   OR 'M2' OR 'ML' OR 'SW' OR 'TM' OR 'HW' OR 'PA'        
121400           MOVE IN1-KDSORT           TO WS-KDSORT                         
121500        ELSE                                                              
121600           MOVE NEJ TO INPUT-RETT                                         
121700           MOVE SPACE                TO WS-FELTEXT                        
121800           MOVE 'SORT CODE INVALID '                                      
121900                                     TO WS-FELTEXT                        
122000           PERFORM G-POPULATE-ERROR-INDEX                                 
122100        END-IF                                                            
122200     END-IF                                                               
122300*                                                                         
122400     IF IN1-IDAO =  SPACE                                                 
122500        MOVE NEJ TO INPUT-RETT                                            
122600        MOVE SPACE                   TO WS-FELTEXT                        
122700        MOVE 'CHANGE ORDER MISSING'                                       
122800                                     TO WS-FELTEXT                        
122900        PERFORM G-POPULATE-ERROR-INDEX                                    
123000     END-IF                                                               
123100                                                                          
123200     IF  IN1-IDFKNGRP NUMERIC                                             
123300     AND IN1-IDFKNGRP > ZERO                                              
123400         MOVE IN1-IDFKNGRP           TO WS-TEST-IDFKNGRP                  
123500     ELSE                                                                 
123600         IF NSP-REG                                                       
123700            MOVE ZERO                TO IN1-IDFKNGRP                      
123800         ELSE                                                             
123900           MOVE NEJ                  TO INPUT-RETT                        
124000           MOVE SPACE                TO WS-FELTEXT                        
124100           MOVE 'FUNC GRP INVALID'                                        
124200                                     TO WS-FELTEXT                        
124300           PERFORM G-POPULATE-ERROR-INDEX                                 
124400         END-IF                                                           
124500     END-IF                                                               
124600                                                                          
124700     IF EXCH-REG                                                          
124800        MOVE WS-IDARTNR                  TO EXCH-TEST-IDARTNR             
124900        IF BYT01-BYTES                                                    
125000           CONTINUE                                                       
125100        ELSE                                                              
125200           MOVE NEJ                      TO INPUT-RETT                    
125300           MOVE SPACES                   TO WS-FELTEXT                    
125400           MOVE 'INVALID EXCHANGE NO '   TO WS-FELTEXT                    
125500           PERFORM G-POPULATE-ERROR-INDEX                                 
125600        END-IF                                                            
125700     END-IF                                                               
125800*                                                                         
125900***  IDPRODNR IS FILLED ONLY IF ITS EXCH PART. CNTRL IN W11810            
126000*                                                                         
126100     IF IN1-FLBYTES-MAN = JA                                              
126200        IF IN1-IDPRODNR NUMERIC AND                                       
126300           IN1-IDPRODNR > ZERO                                            
126400           CONTINUE                                                       
126500        ELSE                                                              
126600           MOVE NEJ                      TO INPUT-RETT                    
126700           MOVE SPACES                   TO WS-FELTEXT                    
126800           MOVE 'NO PROD PART FOR EXCH'  TO WS-FELTEXT                    
126900           PERFORM G-POPULATE-ERROR-INDEX                                 
127000        END-IF                                                            
127100     END-IF                                                               
127200*                                                                         
127300***  PRODUCT GRP NEEDS TO BE VALIDATED BEFORE VALIDATING PROJ ID          
127400***  FOR NON-SPARE PART PRODUCT GROUP WILL ALWAYS BE '11'.                
127500*                                                                         
127600     IF NSP-REG                                                           
127700        MOVE '11'                     TO IN1-KDPRODSL                     
127800                                         WS-KDPRODSL                      
127900     END-IF                                                               
128000                                                                          
128100     IF IN1-KDPRODSL NUMERIC                                              
128200        MOVE WS-KDPRODSL              TO TEST-KDPRODSL                    
128300        IF GOOD-KDPRODSL                                                  
128400           IF  IN1-FLBYTES-MAN    = JA     AND                            
128500           NOT KDPRODSL-BYTES                                             
128600              MOVE NEJ                  TO INPUT-RETT                     
128700              MOVE SPACES               TO WS-FELTEXT                     
128800              MOVE 'PG INVALID FOR EXCH'                                  
128900                                        TO WS-FELTEXT                     
129000              PERFORM G-POPULATE-ERROR-INDEX                              
129100           ELSE                                                           
129200             IF KDPRODSL-BYTES            AND                             
129300                IN1-FLBYTES-MAN    = NEJ                                  
129400                MOVE NEJ                TO INPUT-RETT                     
129500                MOVE SPACES             TO WS-FELTEXT                     
129600                MOVE 'EXCH/CORE PART MISSING'                             
129700                                        TO WS-FELTEXT                     
129800                PERFORM G-POPULATE-ERROR-INDEX                            
129900             ELSE                                                         
130000                MOVE IN1-KDPRODSL       TO WS-KDPRODSL                    
130100             END-IF                                                       
130200           END-IF                                                         
130300        ELSE                                                              
130400           MOVE NEJ                    TO INPUT-RETT                      
130500           MOVE SPACES                 TO WS-FELTEXT                      
130600           MOVE 'PRODUCT GRP INVALID'  TO WS-FELTEXT                      
130700           PERFORM G-POPULATE-ERROR-INDEX                                 
130800        END-IF                                                            
130900     ELSE                                                                 
131000        MOVE NEJ                       TO INPUT-RETT                      
131100        MOVE SPACES                    TO WS-FELTEXT                      
131200        MOVE 'PRODUCT GRP NON NUMERIC' TO WS-FELTEXT                      
131300        PERFORM G-POPULATE-ERROR-INDEX                                    
131400     END-IF                                                               
131500*                                                                         
131600***  WHEN NON SPARE PART, IDPROJ VALUE GETS MOVED TO                      
131700***  WDD201-IDPROJK IN THE PROGRAM FOR REGISTERING NSP                    
131800*                                                                         
131900     IF NSP-REG                                                           
132000        CONTINUE                                                          
132100     ELSE                                                                 
132200        IF IN1-IDPROJ = SPACE                                             
132300           MOVE NEJ TO INPUT-RETT                                         
132400           MOVE SPACE                TO WS-FELTEXT                        
132500           MOVE 'PROJECT ID MISSING '                                     
132600                                     TO WS-FELTEXT                        
132700           PERFORM G-POPULATE-ERROR-INDEX                                 
132800        ELSE                                                              
132900           MOVE IN1-IDPROJ           TO WS-IDPROJ                         
133000           PERFORM S92-GODK-PROJ                                          
133100        END-IF                                                            
133200     END-IF                                                               
133300                                                                          
133400***  INITIALIZE SOP DATE TO ZERO FOR NON SPARE PART                       
133500*                                                                         
133600     IF NSP-REG                                                           
133700        MOVE ALL ZEROES              TO WS-IN-TISOP                       
133800     END-IF                                                               
133900     .                                                                    
134000     EJECT                                                                
134100 S72-CHK-REG-INPUTS   SECTION.                                            
134200     SKIP2                                                                
134300     MOVE 'S72-CHK-REG    '            TO WS-CURRENT-SECTION              
134400*                                                                         
134500***  IN 1116 REFER SECTION XXX-KOLLA-INDATA-NYREG                         
134600*                                                                         
134700     IF  IN1-IDBERED NUMERIC                                              
134800     AND IN1-IDBERED > ZERO                                               
134900         CONTINUE                                                         
135000     ELSE                                                                 
135100         MOVE NEJ                      TO INPUT-RETT                      
135200         MOVE SPACE                    TO WS-FELTEXT                      
135300         MOVE 'PART PLANNER ID INVALID'                                   
135400                                       TO WS-FELTEXT                      
135500         PERFORM G-POPULATE-ERROR-INDEX                                   
135600     END-IF                                                               
135700                                                                          
135800     IF IN1-IDCDS = SPACE                                                 
135900        MOVE NEJ                       TO INPUT-RETT                      
136000        MOVE SPACES                    TO WS-FELTEXT                      
136100        MOVE 'CDSID MISSING      '     TO WS-FELTEXT                      
136200        PERFORM G-POPULATE-ERROR-INDEX                                    
136300     END-IF                                                               
136400                                                                          
136500     IF NYPON-ARTIKEL = JA                                                
136600        PERFORM S83-CHECK-NYPON-DATA                                      
136700     END-IF                                                               
136800                                                                          
136900     PERFORM S73-CHK-REM-INPUTS                                           
137000*AD  PERFORM S82-NYA-ARTREG-DATAELEMENT                                   
137100*AD?????? HOW WILL PART BE PRESENT IN KIT DB WHEN NEW PART????            
137200*ST  KIT PARTS FROM TCPLM STRUCTURE TO BE DISCUSSED.                      
137300*ST  PERFORM S84-KOLLA-RASA                                               
137400*ST  FOR LYNK & SIMILAR KIND OF PARTS                                     
137500*ST  PERFORM S85-KOLLA-CROSS                                              
137600     .                                                                    
137700     EJECT                                                                
137800 S73-CHK-REM-INPUTS   SECTION.                                            
137900     SKIP2                                                                
138000     MOVE 'S73-CHK-REM-INPUTS'       TO WS-CURRENT-SECTION                
138100*                                                                         
138200***  IN 1116 REFER SECTION XXX-NYA-GEMENSAMMA-DATAELEMENT                 
138300***  SOME OF THE CODE MOVED TO S73-VALIDATE SECTION                       
138400*                                                                         
138500     IF IN1-FLRSBEART = JA OR NEJ                                         
138600        MOVE IN1-FLRSBEART           TO WS-FLRSBEART                      
138700     ELSE                                                                 
138800        IF IN1-FLRSBEART = SPACE                                          
138900           MOVE NEJ                  TO WS-FLRSBEART                      
139000        ELSE                                                              
139100           MOVE NEJ TO INPUT-RETT                                         
139200           MOVE SPACE                TO WS-FELTEXT                        
139300           MOVE 'FLAG INVALID '      TO WS-FELTEXT                        
139400           PERFORM G-POPULATE-ERROR-INDEX                                 
139500        END-IF                                                            
139600     END-IF                                                               
139700                                                                          
139800     IF IN1-IDSKYLT = 'GB ' OR 'S  '                                      
139900        MOVE IN1-IDSKYLT             TO WS-IDSKYLT                        
140000     ELSE                                                                 
140100        IF IN1-IDSKYLT = SPACE                                            
140200           MOVE 'GB '                TO WS-IDSKYLT                        
140300        ELSE                                                              
140400           MOVE NEJ TO INPUT-RETT                                         
140500           MOVE SPACE                TO WS-FELTEXT                        
140600           MOVE 'LANG ID INVALID'                                         
140700                                     TO WS-FELTEXT                        
140800           PERFORM G-POPULATE-ERROR-INDEX                                 
140900        END-IF                                                            
141000     END-IF                                                               
141100*                                                                         
141200     MOVE IN1-TISOP-PRINS (1:2)      TO WS-CC                             
141300     MOVE IN1-TISOP-PRINS (3:4)      TO WS-IN-AAVV-SOP                    
141400     MOVE '1'                        TO WS-IN-DAG-SOP                     
141500*                                                                         
141600     IF EXCH-REG                                                          
141700        MOVE WS-IDARTNR              TO OBJ-TEST-IDARTNR                  
141800        IF BYT03-OBJEKT                                                   
141900           MOVE WS-IN-AAVVD-OBJ      TO WS-IN-TISOP                       
142000        END-IF                                                            
142100     END-IF                                                               
142200*                                                                         
142300***  CONVERT TISOP YYYYWW TO YYWWD FORMAT                                 
142400*                                                                         
142500     IF IN1-TISOP-PRINS = ZERO                                            
142600        MOVE NEJ TO INPUT-RETT                                            
142700        MOVE SPACE                   TO WS-FELTEXT                        
142800        MOVE 'SOP DATE MISSING'                                           
142900                                     TO WS-FELTEXT                        
143000        PERFORM G-POPULATE-ERROR-INDEX                                    
143100     ELSE                                                                 
143200        MOVE WS-IN-TISOP             TO XX-TISOP                          
143300                                                                          
143400        IF XX-TISOP NUMERIC                                               
143500           MOVE XX-TISOP             TO WS-TISOP                          
143600           MOVE WS-TISOP             TO DAT-I-TIDATUM                     
143700           MOVE 'AAVVD '             TO DAT-KDDATFORM                     
143800           PERFORM S98-WDATKONV                                           
143900           IF DAT-KDSVAR-OK                                               
144000             PERFORM S97-OM-TVA-AAR                                       
144100             MOVE WS-TISOP           TO TMP1-YYWWD                        
144200             MOVE AAVVD              TO TMP2-YYWWD                        
144300             MOVE DAT-TIAAVVD        TO TMP3-YYWWD                        
144400             PERFORM WY2000P2                                             
144500             IF ( TMP1-YYWWD < TMP2-YYWWD AND                             
144600                  WS-TISOP   < AAVVD      AND                             
144700                  WS-CC      = 20 )       OR                              
144800                  WS-CC      = 19                                         
144900                CONTINUE                                                  
145000             ELSE                                                         
145100                MOVE NEJ TO INPUT-RETT                                    
145200                MOVE SPACE           TO WS-FELTEXT                        
145300                MOVE 'SOP DATE INVALID'                                   
145400                                     TO WS-FELTEXT                        
145500                PERFORM G-POPULATE-ERROR-INDEX                            
145600             END-IF                                                       
145700                                                                          
145800           ELSE                                                           
145900              MOVE NEJ TO INPUT-RETT                                      
146000              MOVE SPACE             TO WS-FELTEXT                        
146100              MOVE 'SOP DATE INVALID'                                     
146200                                     TO WS-FELTEXT                        
146300              PERFORM G-POPULATE-ERROR-INDEX                              
146400           END-IF                                                         
146500        ELSE                                                              
146600           MOVE NEJ TO INPUT-RETT                                         
146700           MOVE SPACE                TO WS-FELTEXT                        
146800           MOVE 'SOP DATE NOT NUMERIC'                                    
146900                                     TO WS-FELTEXT                        
147000           PERFORM G-POPULATE-ERROR-INDEX                                 
147100        END-IF                                                            
147200     END-IF                                                               
147300                                                                          
147400     IF WS-IN-TISOP NOT = SPACE                                           
147500        PERFORM S96-TIFINLV-FRAN-SOP                                      
147600     END-IF                                                               
147700                                                                          
147800     MOVE SPAR-TIFINLV-AAVVD         TO XX-TIFINLV                        
147900     IF XX-AAR = '99'                                                     
148000     AND XX-VECKA = '99'                                                  
148100        MOVE '9'                     TO XX-DAG                            
148200     ELSE                                                                 
148300        MOVE '1'                     TO XX-DAG                            
148400     END-IF                                                               
148500                                                                          
148600     MOVE XX-TIFINLV                 TO WS-TIFINLV                        
148700     MOVE WS-TIFINLV                 TO DAT-I-TIDATUM                     
148800     MOVE 'AAVVD '                   TO DAT-KDDATFORM                     
148900     PERFORM S98-WDATKONV                                                 
149000     IF DAT-KDSVAR-OK                                                     
149100        PERFORM S97-OM-TVA-AAR                                            
149200        MOVE WS-TIFINLV              TO TMP1-YYWWD                        
149300        MOVE AAVVD                   TO TMP2-YYWWD                        
149400        MOVE DAT-TIAAVVD             TO TMP3-YYWWD                        
149500        PERFORM WY2000Q2                                                  
149600        IF TMP1-YYWWD < TMP2-YYWWD                                        
149700        AND TMP1-YYWWD > TMP3-YYWWD                                       
149800           MOVE WS-TIFINLV           TO AAVVD                             
149900           MOVE +1                   TO D                                 
150000           MOVE AAVVD                TO WS-TIFINLV                        
150100        ELSE                                                              
150200           MOVE NEJ TO INPUT-RETT                                         
150300           MOVE SPACE                TO WS-FELTEXT                        
150400           MOVE 'PUB DATE INVALID'                                        
150500                                     TO WS-FELTEXT                        
150600           PERFORM G-POPULATE-ERROR-INDEX                                 
150700        END-IF                                                            
150800                                                                          
150900     ELSE                                                                 
151000           MOVE NEJ TO INPUT-RETT                                         
151100           MOVE SPACE                TO WS-FELTEXT                        
151200           MOVE 'PUB DATE INVALID'                                        
151300                                     TO WS-FELTEXT                        
151400           PERFORM G-POPULATE-ERROR-INDEX                                 
151500     END-IF                                                               
151600                                                                          
151700*    IF INPUT-RETT = JA                                                   
151800*    OR NOT-MAX-ERR-PART                                                  
151900        PERFORM S93-KOLLA-SOFTWARE                                        
152000*    END-IF                                                               
152100*                                                                         
152200***  FOR BELOW VALIDATIONS                                                
152300***  IN 1116 REFER SECTION XXX-NYA-ARTREG-DATAELEMENT                     
152400*                                                                         
152500     IF IN1-KDUART = 'A' OR 'M' OR 'S' OR 'P'                             
152600                  OR 'K' OR 'B' OR SPACE                                  
152700        CONTINUE                                                          
152800     ELSE                                                                 
152900        MOVE NEJ TO INPUT-RETT                                            
153000        MOVE SPACE                   TO WS-FELTEXT                        
153100        MOVE 'EXCP ARTICLE CODE INVALID'                                  
153200                                     TO WS-FELTEXT                        
153300        PERFORM G-POPULATE-ERROR-INDEX                                    
153400     END-IF                                                               
153500                                                                          
153600     IF IN1-KDYTBEH NUMERIC AND                                           
153700        IN1-KDYTBEH < 10                                                  
153800       CONTINUE                                                           
153900     ELSE                                                                 
154000        MOVE NEJ TO INPUT-RETT                                            
154100        MOVE SPACE                TO WS-FELTEXT                           
154200        MOVE 'SURFACE CODE INVALID'                                       
154300                                  TO WS-FELTEXT                           
154400        PERFORM G-POPULATE-ERROR-INDEX                                    
154500     END-IF                                                               
154600                                                                          
154700     IF IN1-KDFARLIG NUMERIC                                              
154800        IF IN1-KDFARLIG = 0 OR 3 OR 4 OR 6 OR 7                           
154900           IF IN1-KDFARLIG = 4 OR 6                                       
155000              SET DANG-GOODS-JA   TO TRUE                                 
155100           END-IF                                                         
155200        ELSE                                                              
155300           MOVE NEJ TO INPUT-RETT                                         
155400           MOVE SPACE                TO WS-FELTEXT                        
155500           MOVE 'DANG GOOD CODE INVALID'                                  
155600                                     TO WS-FELTEXT                        
155700           PERFORM G-POPULATE-ERROR-INDEX                                 
155800        END-IF                                                            
155900     ELSE                                                                 
156000           MOVE NEJ TO INPUT-RETT                                         
156100           MOVE SPACE                TO WS-FELTEXT                        
156200           MOVE 'DANG GOOD CODE BE NUMERIC'                               
156300                                     TO WS-FELTEXT                        
156400           PERFORM G-POPULATE-ERROR-INDEX                                 
156500     END-IF                                                               
156600                                                                          
156700     IF IN1-KDBPSR NUMERIC                                                
156800        IF IN1-KDBPSR = 1 OR 2 OR 3 OR 4 OR 5 OR 6 OR 7 OR 8              
156900           CONTINUE                                                       
157000        ELSE                                                              
157100           MOVE NEJ TO INPUT-RETT                                         
157200           MOVE SPACE                TO WS-FELTEXT                        
157300           MOVE 'BPSR CODE INVALID'                                       
157400                                     TO WS-FELTEXT                        
157500           PERFORM G-POPULATE-ERROR-INDEX                                 
157600        END-IF                                                            
157700     ELSE                                                                 
157800           MOVE NEJ TO INPUT-RETT                                         
157900           MOVE SPACE                TO WS-FELTEXT                        
158000           MOVE 'BPSR CODE MUST NUMERIC'                                  
158100                                     TO WS-FELTEXT                        
158200           PERFORM G-POPULATE-ERROR-INDEX                                 
158300     END-IF                                                               
158400                                                                          
158500     IF IN1-FLLSRDEL = JA OR NEJ                                          
158600        CONTINUE                                                          
158700     ELSE                                                                 
158800           MOVE NEJ TO INPUT-RETT                                         
158900           MOVE SPACE                TO WS-FELTEXT                        
159000           MOVE 'INVALID FLAG-SOLD SEPARATELY '                           
159100                                     TO WS-FELTEXT                        
159200           PERFORM G-POPULATE-ERROR-INDEX                                 
159300     END-IF                                                               
159400                                                                          
159410     IF IN1-TIERSDAT-UTC NOT = SPACE                                      
159411        MOVE IN1-TIERSDAT-UTC (3:2)  TO WS-IN-TIERS-AA                    
159412        MOVE IN1-TIERSDAT-UTC (6:2)  TO WS-IN-TIERS-MM                    
159413        MOVE IN1-TIERSDAT-UTC (9:2)  TO WS-IN-TIERS-DD                    
159414*CONVERT TO YYWWD FORMAT                                                  
159415        MOVE WS-IN-TIERS-AAMMDD      TO DAT-I-TIDATUM                     
159416        MOVE 'AAMMDD'                TO DAT-KDDATFORM                     
159417        CALL WDATKONV             USING DAT-KDDATFORM                     
159418                                        DAT-I-TIDATUM                     
159419                                        DAT-O-TIDATUM                     
159420                                        DAT-KDSVAR                        
159421        IF DAT-KDSVAR-OK                                                  
159423           MOVE DAT-TIAAVV-GRP       TO WS-IN-AAVV-ERS                    
159429        END-IF                                                            
159430                                                                          
159432        IF (IN1-TIERSDAT-UTC (1:2) < 20) OR                               
159433            DAT-KDSVAR-FEL                                                
159434            MOVE NEJ                  TO INPUT-RETT                       
159435            MOVE SPACE                TO WS-FELTEXT                       
159436            MOVE 'INVALID REPLACE DATE'                                   
159437                                      TO WS-FELTEXT                       
159438            PERFORM G-POPULATE-ERROR-INDEX                                
159439        END-IF                                                            
159440*                                                                         
159450     END-IF                                                               
159500     .                                                                    
159600 S74-VALIDATE-SW-SPECIFIC  SECTION.                                       
159700                                                                          
159800*PRICE VALIDATION                                                         
159900     MOVE IN1-PRARTSTD          TO SPAR-PRARTSTD                          
160000*TO FORMAT THE WHOLE NUMBER                                               
160100     IF SPAR-PRARTSTD IS NUMERIC                                          
160200        MOVE SPAR-PRARTSTD      TO WS-NUM-10                              
160300        IF WS-NUM-10 < 9999999                                            
160400           MOVE WS-NUM-10       TO WS-NUM-2DP                             
160500           MOVE WS-NUM-2DP      TO SPAR-PRARTSTD                          
160600        END-IF                                                            
160700     END-IF                                                               
160800*                                                                         
160900     IF SPAR-PRARTSTD-HELTAL   NUMERIC  AND                               
161000        SPAR-PRARTSTD-DECIMAL  NUMERIC  AND                               
161100        SPAR-PRARTSTD-PUNKT  = '.'                                        
161200        IF SPAR-PRARTSTD-HELTAL  > ZERO  OR                               
161300           SPAR-PRARTSTD-DECIMAL > ZERO                                   
161400                                                                          
161500           MOVE SPAR-PRARTSTD-HELTAL                                      
161600                               TO SPAR-PRARTSTD-R-HELTAL                  
161700           MOVE SPAR-PRARTSTD-DECIMAL                                     
161800                               TO SPAR-PRARTSTD-R-DECIMAL                 
161900                                                                          
162000           IF PART-EXISTS AND                                             
162100            ( CLAG-PRARTSTD > 0 AND                                       
162200              CLAG-PRARTSTD NOT = SPAR-PRARTSTD-HDTAL )                   
162300              MOVE NEJ             TO INPUT-RETT                          
162400              MOVE SPACE           TO WS-FELTEXT                          
162500              MOVE 'PRICE UPDATE IS NOT ALLOWED'                          
162600                                   TO WS-FELTEXT                          
162700              PERFORM G-POPULATE-ERROR-INDEX                              
162800           END-IF                                                         
162900        ELSE                                                              
163000           MOVE NEJ             TO INPUT-RETT                             
163100           MOVE SPACE           TO WS-FELTEXT                             
163200           MOVE 'PRICE INVALID' TO WS-FELTEXT                             
163300           PERFORM G-POPULATE-ERROR-INDEX                                 
163400        END-IF                                                            
163500     ELSE                                                                 
163600           MOVE NEJ             TO INPUT-RETT                             
163700           MOVE SPACE           TO WS-FELTEXT                             
163800           MOVE 'PRICE INVALID' TO WS-FELTEXT                             
163900           PERFORM G-POPULATE-ERROR-INDEX                                 
164000     END-IF                                                               
164100*                                                                         
164200     .                                                                    
164300 S83-CHECK-NYPON-DATA      SECTION.                                       
164400                                                                          
164500     MOVE 'S83-CHECK-NYPON'          TO WS-CURRENT-SECTION                
164600*                                                                         
164700***  IN 1116 REFER SECTION XXX-NYA-NYPON-DATAELEMENT                      
164800*                                                                         
164900                                                                          
165000     SKIP2                                                                
165100******************************************************************        
165200*    PROJK ÄR OBLIGATORISKT FÖR ARTIKLAR MED PRODUKTSLAG PV-BASL.         
165300*    I DETTA LÄGET SAKNAS NYPON-ART ATT KOPIERA FRÅN,         .           
165400*    PROJK MÅSTE NU ANGES I MID-PROJK.                                    
165500*    DESSA ARTIKLAR MÅSTE HA GODK-PROJK, UPPLAGDA PÅ BILD 1153.           
165600******************************************************************        
165700                                                                          
165800     MOVE WS-KDPRODSL                TO TEST-KDPRODSL                     
165900                                                                          
166000     IF IN1-IDPROJK = SPACE                                               
166100        IF KDPRODSL-UTAN-EMB OR KDPRODSL-LOCAL                            
166200           MOVE IN1-IDPROJ           TO IN1-IDPROJK                       
166300        END-IF                                                            
166400     END-IF                                                               
166500                                                                          
166600     IF IN1-IDPROJK = SPACE                                               
166700        CONTINUE                                                          
166800     ELSE                                                                 
166900        MOVE IN1-IDPROJK             TO WS-IDPROJK                        
167000        IF KDPRODSL-UTAN-EMB OR KDPRODSL-LOCAL                            
167100           PERFORM S91-KTR-GODK-PROJK-PV                                  
167200           IF PROJK-GODK                                                  
167300              CONTINUE                                                    
167400           ELSE                                                           
167500              MOVE NEJ TO INPUT-RETT                                      
167600              MOVE SPACE             TO WS-FELTEXT                        
167700              MOVE 'PROJ-ID K INVALID'                                    
167800                                     TO WS-FELTEXT                        
167900              PERFORM G-POPULATE-ERROR-INDEX                              
168000           END-IF                                                         
168100        END-IF                                                            
168200     END-IF                                                               
168300                                                                          
168400     IF IN1-IDARTNR-MOTSV = SPACE                                         
168500        MOVE ZERO                    TO WS-IDARTNR-MOTSV                  
168600     ELSE                                                                 
168700        IF IN1-IDARTNR-MOTSV NUMERIC                                      
168800           MOVE IN1-IDARTNR-MOTSV    TO WS-IDARTNR-MOTSV                  
168900        ELSE                                                              
169000              MOVE NEJ TO INPUT-RETT                                      
169100              MOVE SPACE             TO WS-FELTEXT                        
169200              MOVE 'CORRESPONDING PART NUM INVALID'                       
169300                                     TO WS-FELTEXT                        
169400              PERFORM G-POPULATE-ERROR-INDEX                              
169500        END-IF                                                            
169600     END-IF                                                               
169700                                                                          
169800     IF IN1-FLPISK = SPACE                                                
169900        MOVE NEJ                     TO WS-FLPISK                         
170000     ELSE                                                                 
170100        IF IN1-FLPISK = JA OR NEJ                                         
170200           MOVE IN1-FLPISK           TO WS-FLPISK                         
170300        ELSE                                                              
170400              MOVE NEJ TO INPUT-RETT                                      
170500              MOVE SPACE             TO WS-FELTEXT                        
170600              MOVE 'FAST PART FLAG INVALID'                               
170700                                     TO WS-FELTEXT                        
170800              PERFORM G-POPULATE-ERROR-INDEX                              
170900        END-IF                                                            
171000     END-IF                                                               
171100     .                                                                    
171200     EJECT                                                                
171300*S84-KOLLA-RASA SECTION.                                                  
171400     SKIP2                                                                
171500*ST  MOVE IDARTNR-WS TO W-IDARTNR                                         
171600*ST  PERFORM IMS-GET-WDJ101                                               
171700*ST  IF SEGMENT-FINNS                                                     
171800*ST     IF WS-KDSORT = 'SA' OR 'TM'                                       
171900*ST        IF SATB-STR-IDSTRTYP = 'S'                                     
172000*ST           CONTINUE                                                    
172100*ST        ELSE                                                           
172200*ST           MOVE NEJ TO INPUT-RETT                                      
172300*ST           MOVE SPACE                TO WS-FELTEXT                     
172400*ST           MOVE 'ERROR IN KIT'                                         
172500*ST                                     TO WS-FELTEXT                     
172600*ST           PERFORM G-POPULATE-ERROR-INDEX                              
172700*ST        END-IF                                                         
172800*ST     ELSE                                                              
172900*ST        IF WS-KDSORT = 'ST'                                            
173000*ST           IF SATB-STR-IDSTRTYP = 'R' OR 'K'                           
173100*ST              CONTINUE                                                 
173200*ST           ELSE                                                        
173300*ST             MOVE NEJ TO INPUT-RETT                                    
173400*ST             MOVE SPACE                TO WS-FELTEXT                   
173500*ST             MOVE 'ERROR IN KIT'                                       
173600*ST                                     TO WS-FELTEXT                     
173700*ST             PERFORM G-POPULATE-ERROR-INDEX                            
173800*ST           END-IF                                                      
173900*ST        ELSE                                                           
174000*ST             MOVE NEJ TO INPUT-RETT                                    
174100*ST             MOVE SPACE                TO WS-FELTEXT                   
174200*ST             MOVE 'ERROR IN KIT'                                       
174300*ST                                     TO WS-FELTEXT                     
174400*ST             PERFORM G-POPULATE-ERROR-INDEX                            
174500*ST        END-IF                                                         
174600*ST     END-IF                                                            
174700*ST  ELSE                                                                 
174800*ST      MOVE IDARTNR-WS TO W-IDARTNR-S                                   
174900*ST      MOVE SPACE TO W-IDLEVNR-S                                        
175000*ST                    W-BELEVART-S                                       
175100*ST      PERFORM IMS-GU-WDJ111-CSEQ                                       
175200*ST      PERFORM UNTIL SEGMENT-SAKNAS OR BASEN-SLUT                       
175300*ST         IF WS-KDSORT = SATB-RAD-KDSORT                                
175400*ST            CONTINUE                                                   
175500*ST         ELSE                                                          
175600*ST          MOVE SATB-RAD-TISTODAT   TO TMP1-YYMMDD                      
175700*ST          MOVE DAGENS-AAMMDD       TO TMP2-YYMMDD                      
175800*ST          PERFORM WY2000P1                                             
175900*ST          IF TMP1-YYMMDD >= TMP2-YYMMDD                                
176000*ST             MOVE NEJ TO INPUT-RETT                                    
176100*ST             MOVE SPACE                TO WS-FELTEXT                   
176200*ST             MOVE 'ERROR IN KIT STOP DATE'                             
176300*ST                                     TO WS-FELTEXT                     
176400*ST             PERFORM G-POPULATE-ERROR-INDEX                            
176500*ST          END-IF                                                       
176600*ST         END-IF                                                        
176700*ST      PERFORM IMS-GN-WDJ111-CSEQ                                       
176800*ST      END-PERFORM                                                      
176900*ST  END-IF                                                               
177000*ST  .                                                                    
177100     EJECT                                                                
177200*S85-KOLLA-CROSS SECTION.                                                 
177300*                                                                         
177400*    IF INPUT-RETT = JA                                                   
177500*       IF IN1-IDLEVNR = SPACE                                            
177600*       AND IN1-BELEV = SPACE                                             
177700*          IF KDPRODSL-LYNK                                               
177800*               MOVE NEJ TO INPUT-RETT                                    
177900*               MOVE SPACE                TO WS-FELTEXT                   
178000*               MOVE 'SUPPL PART REQ FOR THIS PG'                         
178100*                                       TO WS-FELTEXT                     
178200*               PERFORM G-POPULATE-ERROR-INDEX                            
178300*          ELSE                                                           
178400*             CONTINUE                                                    
178500*          END-IF                                                         
178600*       ELSE                                                              
178700*          MOVE IDARTNR-WS TO W-IDARTNR                                   
178800*          PERFORM IMS-GU-WDF501                                          
178900*          IF SEGMENT-FINNS                                               
179000*               MOVE NEJ TO INPUT-RETT                                    
179100*               MOVE SPACE                TO WS-FELTEXT                   
179200*               MOVE 'PART EXISTS IN CROSS INDEX'                         
179300*                                       TO WS-FELTEXT                     
179400*               PERFORM G-POPULATE-ERROR-INDEX                            
179500*          END-IF                                                         
179600*                                                                         
179700*          MOVE IN1-IDLEVNR     TO W-SEQA-IDLEVNR-MIN                     
179800*                                  W-SEQA-IDLEVNR-MAX                     
179900*                                                                         
180000*          MOVE IN1-BELEV       TO WS-W009REDU-IN                         
180100*          CALL W009REDU USING WS-W009REDU-IN WS-W009REDU-UT              
180200*          MOVE WS-W009REDU-UT  TO W-SEQA-IDLEVART-MIN                    
180300*                                  W-SEQA-IDLEVART-MAX                    
180400*          PERFORM IMS-GN-WDF5A1                                          
180500*          IF SEGMENT-FINNS                                               
180600*               MOVE NEJ TO INPUT-RETT                                    
180700*               MOVE SPACE                TO WS-FELTEXT                   
180800*               MOVE 'SUPPL. PART IN CROSSINDEX ALREADY'                  
180900*                                       TO WS-FELTEXT                     
181000*               PERFORM G-POPULATE-ERROR-INDEX                            
181100*          END-IF                                                         
181200*       END-IF                                                            
181300*    END-IF                                                               
181400*    .                                                                    
181500*    EJECT                                                                
181600 S86-KOLLA-OM-BEN-FINNS SECTION.                                          
181700*****************************************************************         
181800*  ÄT NOV 92 VID IDSKYLT = GB  GODKÄNNES BARA NAMNLEX BENÄMNING *         
181900*             -  GÄLLER INTE VID KOPIERING AV BENÄMNING         *         
182000*****************************************************************         
182100     SKIP2                                                                
182200                                                                          
182300     MOVE 'S86-KOLLA      '   TO WS-CURRENT-SECTION                       
182400                                                                          
182500     MOVE WS-IDSKYLT          TO W-IDSKYLT                                
182600     MOVE WS-BEART            TO W-BEART                                  
182700     PERFORM IMS-GU-WDD301-ASEQ                                           
182800                                                                          
182900     IF WS-IDSKYLT = 'S  '                                                
183000        PERFORM UNTIL SEGMENT-SAKNAS                                      
183100                      OR BENA-BEN-KDHOMONYM = 0                           
183200           PERFORM IMS-GN-WDD301-ASEQ                                     
183300        END-PERFORM                                                       
183400                                                                          
183500        IF SEGMENT-FINNS                                                  
183600*****     OM BENÄMNING FINNS PÅ BENÄMNINGSREGISTRET                       
183700           IF WS-FLRSBEART = JA                                           
183800              CONTINUE                                                    
183900           ELSE                                                           
184000              PERFORM IMS-GET-WDD313-ASEQ                                 
184100                                                                          
184200              IF SEGMENT-FINNS                                            
184300***********     OM HOMONYMKOD FINNS                                       
184400                 MOVE BENA-HOM-TEHOMONYM TO WS-TEHOMONYM                  
184500                                                                          
184600                 IF RS-BM-NAMN = 'BM-NAMN' OR 'RS-NAMN'                   
184700                    CONTINUE                                              
184800                 ELSE                                                     
184900                    MOVE NEJ TO INPUT-RETT                                
185000                    MOVE SPACE           TO WS-FELTEXT                    
185100                    MOVE 'HOM.CODE EXISTS  VERIFY'                        
185200                                         TO WS-FELTEXT                    
185300                    PERFORM G-POPULATE-ERROR-INDEX                        
185400                 END-IF                                                   
185500                                                                          
185600              ELSE                                                        
185700***********  OM HOMONYMKOD SAKNAS                                         
185800                 CONTINUE                                                 
185900              END-IF                                                      
186000           END-IF                                                         
186100        ELSE                                                              
186200*****     OM BENÄMNING SAKNAS PÅ BENÄMNINGSREGISTRET                      
186300           IF WS-FLRSBEART = JA                                           
186400              CONTINUE                                                    
186500           ELSE                                                           
186600              MOVE NEJ TO INPUT-RETT                                      
186700              MOVE SPACE                 TO WS-FELTEXT                    
186800              MOVE 'DESCRIPTION MISSING IN DB'                            
186900                                         TO WS-FELTEXT                    
187000              PERFORM G-POPULATE-ERROR-INDEX                              
187100           END-IF                                                         
187200        END-IF                                                            
187300*******  VID IDSKYLT = GB                                                 
187400     ELSE                                                                 
187500        PERFORM UNTIL SEGMENT-SAKNAS                                      
187600                      OR BENA-BEN-KDBENSTAT < 2                           
187700           PERFORM IMS-GN-WDD301-ASEQ                                     
187800        END-PERFORM                                                       
187900                                                                          
188000        IF SEGMENT-FINNS                                                  
188100           CONTINUE                                                       
188200        ELSE                                                              
188300           MOVE NEJ TO INPUT-RETT                                         
188400           MOVE SPACE                    TO WS-FELTEXT                    
188500           MOVE 'DESCRIPTION MISSING IN DB'                               
188600                                         TO WS-FELTEXT                    
188700           PERFORM G-POPULATE-ERROR-INDEX                                 
188800        END-IF                                                            
188900     END-IF                                                               
189000     .                                                                    
189100     EJECT                                                                
189200 S91-KTR-GODK-PROJK-PV SECTION.                                           
189300                                                                          
189400     MOVE 'S91-KTR-GODK   '      TO WS-CURRENT-SECTION                    
189500                                                                          
189600     MOVE NEJ TO SW-PROJK-GODK                                            
189700     MOVE WS-KDPRODSL TO W-KDPRODSL                                       
189800                                                                          
189900     PERFORM IMS-GU-XXAQ01                                                
190000     IF SEGMENT-FINNS                                                     
190100        PERFORM IMS-GNP-XXAQ11                                            
190200                                                                          
190300        PERFORM UNTIL (PROJK-GODK) OR (SEGMENT-SAKNAS)                    
190400          IF XXAQ-1132-IDPROJK = WS-IDPROJK                               
190500             MOVE JA             TO SW-PROJK-GODK                         
190600          ELSE                                                            
190700             PERFORM IMS-GNP-XXAQ11                                       
190800          END-IF                                                          
190900        END-PERFORM                                                       
191000                                                                          
191100     END-IF                                                               
191200     .                                                                    
191300     EJECT                                                                
191400 S92-GODK-PROJ  SECTION.                                                  
191500                                                                          
191600     MOVE 'S92-GODK-PROJ  '      TO WS-CURRENT-SECTION                    
191700                                                                          
191800******************************************************************        
191900*     PROJEKT ÄR OBLIGATORISKT FÖR ALLA ARTIKLAR MED                      
192000*     PRODUKTSLAG PV-BASL OCH PRODUKTSLAG CARPAC.                         
192100*     DESSA ARTIKLAR MÅSTE HA GODK-PROJ, UPPLAGD PÅ BILD 1153.            
192200*     ARTKLAR MED PRODSL. 19 31 34 35 38  INGEN KONTROLL.                 
192300*     ÖVRIGA ARTIKLAR FÅR EJ HA PROJ BLANK.                               
192400******************************************************************        
192500                                                                          
192600     MOVE WS-KDPRODSL           TO W-KDPRODSL                             
192700                                    TEST-KDPRODSL                         
192800     MOVE NEJ TO SW-PROJ-GODK                                             
192900                                                                          
193000     IF KDPRODSL-VOLVO-UTAN-EMB                                           
193100     OR KDPRODSL-LOCAL                                                    
193200                                                                          
193300        PERFORM IMS-GU-XXAQ01                                             
193400                                                                          
193500        IF SEGMENT-FINNS                                                  
193600           PERFORM IMS-GNP-XXAQ11                                         
193700                                                                          
193800           PERFORM UNTIL (PROJ-GODK) OR (SEGMENT-SAKNAS)                  
193900                                                                          
194000             IF XXAQ-1132-IDPROJ = WS-IDPROJ                              
194100                MOVE JA          TO SW-PROJ-GODK                          
194200             ELSE                                                         
194300                PERFORM IMS-GNP-XXAQ11                                    
194400             END-IF                                                       
194500                                                                          
194600           END-PERFORM                                                    
194700                                                                          
194800        END-IF                                                            
194900                                                                          
195000        IF PROJ-GODK                                                      
195100           CONTINUE                                                       
195200        ELSE                                                              
195300           MOVE NEJ TO INPUT-RETT                                         
195400           MOVE SPACE            TO WS-FELTEXT                            
195500           MOVE 'PROJECT ID INVALID '                                     
195600                                 TO WS-FELTEXT                            
195700           PERFORM G-POPULATE-ERROR-INDEX                                 
195800        END-IF                                                            
195900     ELSE                                                                 
196000        IF KDPRODSL-EMB                                                   
196100           CONTINUE                                                       
196200        ELSE                                                              
196300* * * * *  ÖVRIGA PRODUKTSLAG * * * * * * * * * * * * * * * *             
196400           IF WS-IDPROJ = SPACE                                           
196500              MOVE NEJ TO INPUT-RETT                                      
196600              MOVE SPACE         TO WS-FELTEXT                            
196700              MOVE 'PROJECT ID SPACE INVALID'                             
196800                                 TO WS-FELTEXT                            
196900              PERFORM G-POPULATE-ERROR-INDEX                              
197000           ELSE                                                           
197100              CONTINUE                                                    
197200           END-IF                                                         
197300        END-IF                                                            
197400     END-IF                                                               
197500     .                                                                    
197600     EJECT                                                                
197700 S93-KOLLA-SOFTWARE SECTION.                                              
197800                                                                          
197900     MOVE 'S93-KOLLA      '      TO WS-CURRENT-SECTION                    
198000                                                                          
198100     IF WS-KDSORT = 'SW'                                                  
198200        IF WS-SISTA-SIFFRAN = 8                                           
198300           CONTINUE                                                       
198400        ELSE                                                              
198500           MOVE NEJ TO INPUT-RETT                                         
198600           MOVE SPACE            TO WS-FELTEXT                            
198700           MOVE 'INVALID FUNC GRP FOR SW PART'                            
198800                                 TO WS-FELTEXT                            
198900           PERFORM G-POPULATE-ERROR-INDEX                                 
199000        END-IF                                                            
199100     END-IF                                                               
199200                                                                          
199300     IF WS-SISTA-SIFFRAN = 8                                              
199400        IF WS-KDSORT = 'SW'                                               
199500           CONTINUE                                                       
199600        ELSE                                                              
199700           MOVE NEJ TO INPUT-RETT                                         
199800           MOVE SPACE            TO WS-FELTEXT                            
199900           MOVE 'INVALID FUNC GRP '                                       
200000                                 TO WS-FELTEXT                            
200100           PERFORM G-POPULATE-ERROR-INDEX                                 
200200        END-IF                                                            
200300     END-IF                                                               
200400     .                                                                    
200500     EJECT                                                                
200600                                                                          
200700 S95-VALIDATE-UPDATE-FIELDS SECTION.                                      
200800                                                                          
200900     MOVE 'S95-VALIDATE   '      TO WS-CURRENT-SECTION                    
201000                                                                          
201100* SHIPPING NAME                                                           
201200     IF IN1-IDPSN NUMERIC                                                 
201300        CONTINUE                                                          
201400     ELSE                                                                 
201500        MOVE NEJ                 TO INPUT-RETT                            
201600        MOVE SPACE               TO WS-FELTEXT                            
201700        MOVE 'SHIPPING NAME INVALID       '                               
201800                                 TO WS-FELTEXT                            
201900        PERFORM G-POPULATE-ERROR-INDEX                                    
202000     END-IF                                                               
202100* HANDLING CODE                                                           
202200     IF IN1-KDARTHNT NUMERIC                                              
202300        CONTINUE                                                          
202400     ELSE                                                                 
202500        MOVE NEJ                 TO INPUT-RETT                            
202600        MOVE SPACE               TO WS-FELTEXT                            
202700        MOVE 'HANDLING CODE INVALID       '                               
202800                                 TO WS-FELTEXT                            
202900        PERFORM G-POPULATE-ERROR-INDEX                                    
203000     END-IF                                                               
203100* EMBALLAGE CODE                                                          
203200     MOVE FUNCTION UPPER-CASE(IN1-KDEMBKOD-TEXT)                          
203300                                 TO WS-KDEMBKOD-TEXT                      
203400     EVALUATE TRUE                                                        
203500       WHEN KDEMBKOD-TYP-00                                               
203600         MOVE ZERO               TO WS-KDEMBKOD-2                         
203700       WHEN KDEMBKOD-TYP-60                                               
203800         MOVE 60                 TO WS-KDEMBKOD-2                         
203900       WHEN KDEMBKOD-TYP-70                                               
204000         MOVE 70                 TO WS-KDEMBKOD-2                         
204100       WHEN OTHER                                                         
204200         IF SOFTWARE-REG                                                  
204400            CONTINUE                                                      
204500         ELSE                                                             
204600            MOVE NEJ             TO INPUT-RETT                            
204700            MOVE SPACE           TO WS-FELTEXT                            
204800            MOVE 'EMBALLAGE TEXT INVALID      '                           
204900                                 TO WS-FELTEXT                            
205000            PERFORM G-POPULATE-ERROR-INDEX                                
205100         END-IF                                                           
205200     END-EVALUATE                                                         
205300*                                                                         
205400***  A-WT GOES TO KDP-WT WHEN NOT DANG-GOODS (ALWAYS IN GMS)              
205500*                                                                         
205600***  WHEN DANG-GOODS AND IN GM, POPULATE VOLUME WITH A-WT                 
205700***  AFTER CONVERTING TO KG. ALSO POPULATE KDP-WT IN GMS                  
205800*                                                                         
205900***  WHEN DANG-GOODS AND UNIT IS ML, CONVERT TO LITRE                     
206000***  POPULATE VOLUME WITH A-WT (IN LITRE)                                 
206100***  POPULATE KDP-WT AS 1 GM                                              
206200*                                                                         
206300     MOVE IN1-VKART-NTO          TO WS-VKART-NTO                          
206400     IF IN1-VLFG = SPACE                                                  
206500        IF IN1-KDSORT-VLFG = SPACE                                        
206600           CONTINUE                                                       
206700        ELSE                                                              
206800           MOVE NEJ              TO INPUT-RETT                            
206900           MOVE SPACE            TO WS-FELTEXT                            
207000           MOVE 'NO VOL/WT WITH SORT CODE'                                
207100                                 TO WS-FELTEXT                            
207200           PERFORM G-POPULATE-ERROR-INDEX                                 
207300        END-IF                                                            
207400     ELSE                                                                 
207500        IF DANG-GOODS-JA                                                  
207600           IF IN1-VLFG IS NUMERIC                                         
207700              MOVE IN1-VLFG         TO WS-VLFG-NUM                        
207800              COMPUTE WS-VLFG = (WS-VLFG-NUM / 1000)                      
207900           ELSE                                                           
208000              MOVE NEJ              TO INPUT-RETT                         
208100              MOVE SPACE            TO WS-FELTEXT                         
208200              MOVE 'INVALID VOL/WT '                                      
208300                                    TO WS-FELTEXT                         
208400              PERFORM G-POPULATE-ERROR-INDEX                              
208500           END-IF                                                         
208600                                                                          
208700           IF IN1-KDSORT-VLFG = SPACE                                     
208800              MOVE 'KG  '           TO WS-KDSORT-VLFG                     
208900           ELSE                                                           
209000              IF IN1-KDSORT-VLFG    = 'GR  ' OR 'ML  '                    
209100                 IF IN1-KDSORT-VLFG = 'GR  '                              
209200                    MOVE 'KG  '     TO WS-KDSORT-VLFG                     
209300                 ELSE                                                     
209400                    MOVE 'L   '     TO WS-KDSORT-VLFG                     
209500                    MOVE 1          TO WS-VKART-NTO                       
209600                 END-IF                                                   
209700              ELSE                                                        
209800                 MOVE NEJ           TO INPUT-RETT                         
209900                 MOVE SPACE         TO WS-FELTEXT                         
210000                 MOVE 'SORT CODE INVALID WITH VOL/WT'                     
210100                                    TO WS-FELTEXT                         
210200                 PERFORM G-POPULATE-ERROR-INDEX                           
210300              END-IF                                                      
210400           END-IF                                                         
210500        ELSE                                                              
210600           MOVE ALL ZEROES          TO WS-VLFG                            
210700           MOVE SPACES              TO WS-KDSORT-VLFG                     
210800        END-IF                                                            
210900     END-IF                                                               
211000     .                                                                    
211100     EJECT                                                                
211200                                                                          
211300 S96-TIFINLV-FRAN-SOP SECTION.                                            
211400                                                                          
211500     MOVE 'S96-TIFINLV    '      TO WS-CURRENT-SECTION                    
211600                                                                          
211700*  OM TIFINLV INTE ANGES SKALL DET SÄTTAS = TISOP                         
211800*  MEN OM TISOP INTEÄR STÖRRE ÄN INNEVARANDE VECKA                        
211900*  SKALL TISOP SÄTTAS TILL DAGENS VECKA + 1                               
212000*-- TIFINLV BORTTAGET UR MID JAN. 2015                                    
212100                                                                          
212200     MOVE WS-IN-TISOP            TO SPAR-TIFINLV-AAVV                     
212300     MOVE SPAR-TIFINLV-AAVV-R    TO TMP1-YYWW                             
212400     MOVE SPAR-DAGENS-AAVV-R     TO TMP2-YYWW                             
212500                                                                          
212600     PERFORM WY2000P3                                                     
212700                                                                          
212800     IF TMP1-YYWW > TMP2-YYWW                                             
212900        MOVE WS-IN-TISOP         TO SPAR-TIFINLV-AAVVD                    
213000     ELSE                                                                 
213100        MOVE SPAR-DAGENS-AAVV-R(1:2)                                      
213200                                 TO WS-YY                                 
213300        MOVE SPAR-DAGENS-AAVV-R(3:2)                                      
213400                                 TO WS-WW                                 
213500        IF WS-WW < 52                                                     
213600            ADD      1           TO WS-WW                                 
213700        ELSE                                                              
213800            ADD      1           TO WS-YY                                 
213900            MOVE 01              TO WS-WW                                 
214000        END-IF                                                            
214100        MOVE         1           TO WS-D                                  
214200        MOVE WS-YYWWD            TO SPAR-TIFINLV-AAVVD                    
214300     END-IF                                                               
214400     .                                                                    
214500     EJECT                                                                
214600 S97-OM-TVA-AAR SECTION.                                                  
214700     SKIP2                                                                
214800     MOVE 'S97-OM-TVA     '      TO WS-CURRENT-SECTION                    
214900                                                                          
215000     MOVE 'IDAG  '               TO DAT-KDDATFORM                         
215100     PERFORM S98-WDATKONV                                                 
215200     MOVE DAT-TIAAVVD            TO AAVVD                                 
215300                                                                          
215400     MOVE FUNCTION CURRENT-DATE (1:4)                                     
215500                                 TO W-AAR4                                
215600     ADD 2                       TO W-AAR4                                
215700     MOVE W-AAR4 (3:2)           TO AA                                    
215800                                                                          
215900     IF VV = 53                                                           
216000       MOVE 52 TO VV                                                      
216100     END-IF                                                               
216200     MOVE 1 TO D                                                          
216300     .                                                                    
216400     EJECT                                                                
216500 S98-WDATKONV SECTION.                                                    
216600                                                                          
216700     MOVE 'S98-WDATKONV   '      TO WS-CURRENT-SECTION                    
216800                                                                          
216900     SKIP2                                                                
217000     CALL WDATKONV USING DAT-KDDATFORM                                    
217100                         DAT-I-TIDATUM                                    
217200                         DAT-O-TIDATUM                                    
217300                         DAT-KDSVAR                                       
217400     .                                                                    
217500     EJECT                                                                
217600 S99-ABEND SECTION.                                                       
217700     SKIP3                                                                
217800     MOVE RKOD-ABEND-UTAN-DUMP   TO RKOD                                  
217900     CALL ABEND USING RKOD                                                
218000     .                                                                    
218100     EJECT                                                                
218200 Z-FINIT   SECTION.                                                       
218300                                                                          
218400     CLOSE  W1181101                                                      
218500            W1181102                                                      
218600            W1181103                                                      
218700                                                                          
218800     MOVE 'S' TO POSTSUM-OPKOD                                            
218900     CALL POSTSUM USING POSTSUM-PARM                                      
219000     .                                                                    
219100     EJECT                                                                
219200     EJECT                                                                
219300***************************************************************           
219400*                           IMS SECTION                       *           
219500***************************************************************           
219600     SKIP3                                                                
219700 IMS-GET-WDK601 SECTION.                                                  
219800     MOVE 'IMS-GET-WDK601   '     TO WS-IMS-SECTION                       
219900                                                                          
220000     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
220100              DELIMITED BY SIZE INTO SSA1                                 
220200     MOVE '  GE' TO GODK-STATUSKODER                                      
220300     CALL CBLTDLI USING GU WDK6-PCB DLI-IO-WDK601 SSA1                    
220400     MOVE WDK6-STATUS-CODE        TO STATUS-WS                            
220500     PERFORM IMS-STATUSKONTROLL                                           
220600     .                                                                    
220700     SKIP2                                                                
220800 IMS-GNP-WDK611 SECTION.                                                  
220900     MOVE 'IMS-GNP-WDK611   '     TO WS-IMS-SECTION                       
221000                                                                          
221100     MOVE 'WDK611    '            TO SSA1                                 
221200     MOVE '  '   TO GODK-STATUSKODER                                      
221300     CALL CBLTDLI USING GNP WDK6-PCB DLI-IO-WDK611 SSA1                   
221400     MOVE WDK6-STATUS-CODE        TO STATUS-WS                            
221500     PERFORM IMS-STATUSKONTROLL                                           
221600     .                                                                    
221700     SKIP2                                                                
221800 IMS-GET-WDJ101 SECTION.                                                  
221900     MOVE 'IMS-GET-WDJ101   '     TO WS-IMS-SECTION                       
222000                                                                          
222100     STRING 'WDJ101  (IDARTNR  =' W-IDARTNR-X ')'                         
222200              DELIMITED BY SIZE INTO SSA1                                 
222300     MOVE '  GE' TO GODK-STATUSKODER                                      
222400     CALL CBLTDLI USING GU WDJ1-PCB DLI-IO-WDJ101 SSA1                    
222500     MOVE WDJ1-STATUS-CODE        TO STATUS-WS                            
222600     PERFORM IMS-STATUSKONTROLL                                           
222700     .                                                                    
222800     SKIP2                                                                
222900 IMS-GU-WDJ111-CSEQ SECTION.                                              
223000     MOVE 'IMS-GET-WDJ111-CSEQ'   TO WS-IMS-SECTION                       
223100                                                                          
223200     STRING 'WDJ111  (WDJ1CSEQ =' W-WDJ1CSEQ-X ')'                        
223300              DELIMITED BY SIZE INTO SSA1                                 
223400     MOVE '  GE' TO GODK-STATUSKODER                                      
223500     CALL CBLTDLI USING GU WDJ1C-PCB DLI-IO-WDJ111 SSA1                   
223600     MOVE WDJ1C-STATUS-CODE       TO STATUS-WS                            
223700     PERFORM IMS-STATUSKONTROLL                                           
223800     .                                                                    
223900     EJECT                                                                
224000 IMS-GN-WDJ111-CSEQ SECTION.                                              
224100     MOVE 'IMS-GN-WDJ111-CSEQ'    TO WS-IMS-SECTION                       
224200                                                                          
224300     STRING 'WDJ111  (WDJ1CSEQ =' W-WDJ1CSEQ-X ')'                        
224400              DELIMITED BY SIZE INTO SSA1                                 
224500     MOVE '  GBGE' TO GODK-STATUSKODER                                    
224600     CALL CBLTDLI USING GN WDJ1C-PCB DLI-IO-WDJ111 SSA1                   
224700     MOVE WDJ1C-STATUS-CODE       TO STATUS-WS                            
224800     PERFORM IMS-STATUSKONTROLL                                           
224900     .                                                                    
225000     EJECT                                                                
225100 IMS-GU-WDF501 SECTION.                                                   
225200     MOVE 'IMS-GU-WDF501   '      TO WS-IMS-SECTION                       
225300                                                                          
225400     STRING 'WDF501  (IDARTNR  =' W-IDARTNR-X ')'                         
225500              DELIMITED BY SIZE INTO SSA1                                 
225600     MOVE '  GE' TO GODK-STATUSKODER                                      
225700     CALL CBLTDLI USING GU WDF5-PCB DLI-IO-WDF501 SSA1                    
225800     MOVE WDF5-STATUS-CODE        TO STATUS-WS                            
225900     PERFORM IMS-STATUSKONTROLL                                           
226000     .                                                                    
226100     SKIP2                                                                
226200                                                                          
226300 IMS-GU-WDF5A1 SECTION.                                                   
226400     MOVE 'IMS-GU-WDF5A1   '      TO WS-IMS-SECTION                       
226500                                                                          
226600     STRING 'WDF5A1  (WDF5A1KY=>' W-WDF5A1KY-MIN                          
226700                    '&WDF5A1KY=<' W-WDF5A1KY-MAX ')'                      
226800              DELIMITED BY SIZE INTO SSA1                                 
226900     MOVE '  GE' TO GODK-STATUSKODER                                      
227000     CALL CBLTDLI USING GU WDF5A-PCB DLI-IO-WDF5A1 SSA1                   
227100     MOVE WDF5A-STATUS-CODE TO STATUS-WS                                  
227200     PERFORM IMS-STATUSKONTROLL                                           
227300     .                                                                    
227400     EJECT                                                                
227500 IMS-GU-WDD301-ASEQ SECTION.                                              
227600     MOVE 'IMS-GU-WDD301-ASEQ'    TO WS-IMS-SECTION                       
227700                                                                          
227800     STRING 'WDD301  (WDD3ASEQ =' W-IDSKYLT-X                             
227900                      W-BEART-X  ')'                                      
228000              DELIMITED BY SIZE INTO SSA1                                 
228100     MOVE '  GE' TO GODK-STATUSKODER                                      
228200     CALL CBLTDLI USING GU WDD3-PCB DLI-IO-WDD301 SSA1                    
228300     MOVE WDD3-STATUS-CODE TO STATUS-WS                                   
228400     PERFORM IMS-STATUSKONTROLL                                           
228500     .                                                                    
228600     SKIP2                                                                
228700 IMS-GN-WDD301-ASEQ SECTION.                                              
228800     MOVE 'IMS-GN-WDD301-ASEQ'   TO WS-IMS-SECTION                        
228900                                                                          
229000     STRING 'WDD301  (WDD3ASEQ =' W-IDSKYLT-X                             
229100                      W-BEART-X  ')'                                      
229200             DELIMITED BY SIZE INTO SSA1                                  
229300     MOVE '  GBGE' TO GODK-STATUSKODER                                    
229400     CALL CBLTDLI USING GN WDD3-PCB DLI-IO-WDD301 SSA1                    
229500     MOVE WDD3-STATUS-CODE TO STATUS-WS                                   
229600     PERFORM IMS-STATUSKONTROLL                                           
229700     .                                                                    
229800     SKIP2                                                                
229900 IMS-GET-WDD313-ASEQ SECTION.                                             
230000     MOVE 'IMS-GET-WDD313-ASEQ'   TO WS-IMS-SECTION                       
230100                                                                          
230200     MOVE 'WDD313   '      TO SSA1                                        
230300     MOVE '  GE'           TO GODK-STATUSKODER                            
230400     CALL CBLTDLI USING GNP WDD3-PCB DLI-IO-WDD313 SSA1                   
230500     MOVE WDD3-STATUS-CODE TO STATUS-WS                                   
230600     PERFORM IMS-STATUSKONTROLL                                           
230700     .                                                                    
230800     EJECT                                                                
230900 IMS-GU-XXAQ01          SECTION.                                          
231000     MOVE 'IMS-GU-XXAQ01   '      TO WS-IMS-SECTION                       
231100                                                                          
231200     STRING 'WLXXAQ01(WDGXKEY  =' W-1131-KEY-X ')'                        
231300              DELIMITED BY SIZE INTO SSA1                                 
231400     MOVE '  GE' TO GODK-STATUSKODER                                      
231500     CALL CBLTDLI USING GU XXAQ-PCB DLI-IO-WLXXAQ01 SSA1                  
231600     MOVE XXAQ-STATUS-CODE TO STATUS-WS                                   
231700     PERFORM IMS-STATUSKONTROLL                                           
231800     .                                                                    
231900     SKIP2                                                                
232000 IMS-GNP-XXAQ11          SECTION.                                         
232100     MOVE 'IMS-GNP-XXAQ11  '      TO WS-IMS-SECTION                       
232200                                                                          
232300     MOVE   'WLXXAQ11 ' TO SSA1                                           
232400     MOVE '  GE' TO GODK-STATUSKODER                                      
232500     CALL CBLTDLI USING GNP XXAQ-PCB DLI-IO-WLXXAQ11 SSA1                 
232600     MOVE XXAQ-STATUS-CODE TO STATUS-WS                                   
232700     PERFORM IMS-STATUSKONTROLL                                           
232800     .                                                                    
232900     EJECT                                                                
233000 IMS-STATUSKONTROLL SECTION.                                              
233100     SET STATUS-IX TO 1                                                   
233200     SEARCH GODK-STATUS AT END CALL FELLOG                                
233300       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
233400       CONTINUE                                                           
233500     END-SEARCH                                                           
233600     .                                                                    
233700     EJECT                                                                
233800*    -COPY WY2000P1                                                       
233900     EJECT                                                                
234000*    -COPY WY2000P2                                                       
234100     EJECT                                                                
234200*    -COPY WY2000Q2                                                       
234300     EJECT                                                                
234400*    -COPY WY2000P3                                                       
