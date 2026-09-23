000010 PROCESS DYNAM                                                            
000100 ID DIVISION.                                                             
000200 PROGRAM-ID.             WF100100.                                        
000300 AUTHOR.                 BO HAMMARIN.                                     
000400 DATE-WRITTEN.           NOVEMBER 2001.                                   
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*  FUNKTION:                                                              
000800*                                                                         
000900*  RECEIVING ROUTINE FOR DAILY FILE FROM SAP/R3                           
001000*                                                                         
001100*  PGM                                                                    
001200*  - READS FILE FROM SAP/R3                                               
001300*  - CREATES FILE CONTAINING ACCOUNTS                                     
001400*  - CREATES FILE CONTAINING COST CENTERS                                 
001500*  - CREATES FILE CONTAINING ORDER NUMBERS                                
001600*  - CREATES FILE CONTAINING VAT INFO                                     
001700*  - CREATES FILE CONTAINING FINANCIAL CUSTOMER INFO                      
001710*  - CREATES FILE CONTAINING MISSING COUNTRIES  INFO                      
001800*                                                                         
001900*  ABENDCODES:                                                            
002000*      U0016 -  . . . .                                                   
002100*                                                                         
002200*                                                                         
002300*    E-TRACKER 10143271 - CHINA WAREHOUSE PROJECT-1                       
002400*    JIRA 2135 -CHANGE THE WAY PULS HANDLES VAT CODES FROM SAP            
002500*                                                                         
002600     EJECT                                                                
002700                                                                          
002800 ENVIRONMENT DIVISION.                                                    
002900                                                                          
003000 INPUT-OUTPUT SECTION.                                                    
003100 FILE-CONTROL.                                                            
003200                                                                          
003300*    ---- INFIL1:                                                         
003400*                        - SAP/R3 TRANSACTIONS VCCS                       
003500     SELECT  WF1001        ASSIGN  WF1001D1.                              
003600*                                                                         
003700*    ---- INFIL2:                                                         
003800*                        - SAP/R3 TRANSACTIONS VCCN                       
003900     SELECT  WF1007        ASSIGN  WF1001D7.                              
004000*                                                                         
004010*    ---- INFIL3:                                                         
004020*                        - SAP/R3 TRANSACTIONS VCIN                       
004030     SELECT  WF1008        ASSIGN  WF1001D8.                              
004040*                                                                         
004050*    ---- INFIL4:                                                         
004060*                        - SAP/R3 TRANSACTIONS VCUS                       
004070     SELECT  WF1009        ASSIGN  WF1001D9.                              
004071*                                                                         
004072*    ---- INFIL5:                                                         
004073*                        - SAP/R3 TRANSACTIONS VCSC                       
004074     SELECT  WF1010        ASSIGN  WF1001DB.                              
004080*                                                                         
004100*    ---- UTFIL1                                                          
004200*                        - ACCOUNTS                                       
004300     SELECT  WF1002        ASSIGN  WF1001D2.                              
004400*                                                                         
004500*    ---- UTFIL2                                                          
004600*                        - COST CENTERS                                   
004700     SELECT  WF1003        ASSIGN  WF1001D3.                              
004800*                                                                         
004900*    ---- UTFIL3                                                          
005000*                        - ORDER NUMBERS                                  
005100     SELECT  WF1004        ASSIGN  WF1001D4.                              
005200*                                                                         
005300*    ---- UTFIL4                                                          
005400*                        - VAT INFO                                       
005500     SELECT  WF1005        ASSIGN  WF1001D5.                              
005600*                                                                         
005700*    ---- UTFIL5                                                          
005800*                        - FINANCIAL CUSTOMER INFO                        
005900     SELECT  WF1006        ASSIGN  WF1001D6.                              
006000*                                                                         
006010     SELECT  WF1005A       ASSIGN  WF1001DA.                              
006020*                        -ERROR FILE                                      
006100 DATA DIVISION.                                                           
006200                                                                          
006300 FILE SECTION.                                                            
006400 FD  WF1001                                                               
006500     LABEL RECORD STANDARD                                                
006600     RECORDING  F                                                         
006700     BLOCK CONTAINS 0.                                                    
006800 01  IN-POST1  PIC X(1050).                                               
006900                                                                          
007000 FD  WF1007                                                               
007100     LABEL RECORD STANDARD                                                
007200     RECORDING  F                                                         
007300     BLOCK CONTAINS 0.                                                    
007400 01  IN-POST2  PIC X(1050).                                               
007500                                                                          
007510 FD  WF1008                                                               
007520     LABEL RECORD STANDARD                                                
007530     RECORDING  F                                                         
007540     BLOCK CONTAINS 0.                                                    
007550 01  IN-POST3  PIC X(1050).                                               
007560                                                                          
007570 FD  WF1009                                                               
007580     LABEL RECORD STANDARD                                                
007590     RECORDING  F                                                         
007591     BLOCK CONTAINS 0.                                                    
007592 01  IN-POST4  PIC X(1050).                                               
007593                                                                          
007594 FD  WF1010                                                               
007595     LABEL RECORD STANDARD                                                
007596     RECORDING  F                                                         
007597     BLOCK CONTAINS 0.                                                    
007599 01  IN-POST5   PIC X(1054).                                              
007601                                                                          
007610 FD  WF1002                                                               
007700     LABEL RECORD STANDARD                                                
007800     RECORDING  F                                                         
007900     BLOCK CONTAINS 0.                                                    
008000 01  UT-POST1  PIC X(44).                                                 
008100                                                                          
008200 FD  WF1003                                                               
008300     LABEL RECORD STANDARD                                                
008400     RECORDING  F                                                         
008500     BLOCK CONTAINS 0.                                                    
008600 01  UT-POST2  PIC X(34).                                                 
008700                                                                          
008800 FD  WF1004                                                               
008900     LABEL RECORD STANDARD                                                
009000     RECORDING  F                                                         
009100     BLOCK CONTAINS 0.                                                    
009200 01  UT-POST3  PIC X(36).                                                 
009300                                                                          
009400 FD  WF1005                                                               
009500     LABEL RECORD STANDARD                                                
009600     RECORDING  F                                                         
009700     BLOCK CONTAINS 0.                                                    
009800 01  UT-POST4  PIC X(87).                                                 
009900                                                                          
010000 FD  WF1006                                                               
010100     LABEL RECORD STANDARD                                                
010200     RECORDING  F                                                         
010300     BLOCK CONTAINS 0.                                                    
010400 01  UT-POST5  PIC X(450).                                                
010401                                                                          
010410 FD  WF1005A                                                              
010430     RECORDING  V                                                         
010440     BLOCK CONTAINS 0.                                                    
010450 01  WF1005A-ERR  PIC X(75).                                              
010500     EJECT                                                                
010600                                                                          
010700 WORKING-STORAGE SECTION.                                                 
010800 77  PROGRAMNAMN             PIC X(8)    VALUE 'WF100100'.                
010900                                                                          
011000 77  JA                      PIC X       VALUE 'J'.                       
011100 77  NEJ                     PIC X       VALUE 'N'.                       
011110 77  WFIRST                  PIC X       VALUE 'Y'.                       
011120 77  WS-KDTRADP              PIC X(4)    VALUE SPACES.                    
011130 77  WS-IDLAND               PIC X(2)    VALUE SPACES.                    
011200                                                                          
011300 77  IND-IN                  PIC S9(3)   VALUE +0    COMP SYNC.           
011400 77  IND-UT                  PIC S9(3)   VALUE +0    COMP SYNC.           
011500 77  ERROR-TEXT              PIC X(80)   VALUE SPACE.                     
011510 77  W-IDLEGSEL-PREV         PIC X(4)    VALUE SPACE.                     
011600 01  FELTEXT.                                                             
011700     03  FILLER              PIC X(8)    VALUE 'FELTEXT'.                 
011800     03  FELTEXT-STR         PIC X(72)   VALUE SPACE.                     
011801                                                                          
012000 01  ABEND                   PIC X(8)    VALUE 'ABEND   '.                
012100 01  DATKORT                 PIC X(8)    VALUE 'DATKORT'.                 
012200                                                                          
012300 01  RKOD-ABEND-UTAN-DUMP    PIC S9(4)   VALUE +16  COMP SYNC.            
012310 01  RKOD-ABEND-DB2          PIC S9(4)   COMP VALUE +998.                 
012400                                                                          
012500 01  SWITCHAR.                                                            
012600     03  INPOST1-EOF-SW       PIC X       VALUE 'N'.                      
012700       88  INPOST1-EOF                    VALUE 'J'.                      
012800     03  INPOST7-EOF-SW       PIC X       VALUE 'N'.                      
012900       88  INPOST7-EOF                    VALUE 'J'.                      
012910     03  INPOST8-EOF-SW       PIC X       VALUE 'N'.                      
012920       88  INPOST8-EOF                    VALUE 'J'.                      
012930     03  INPOST9-EOF-SW       PIC X       VALUE 'N'.                      
012940       88  INPOST9-EOF                    VALUE 'J'.                      
012950     03  INPOST10-EOF-SW      PIC X       VALUE 'N'.                      
012960       88  INPOST10-EOF                   VALUE 'J'.                      
013000     EJECT                                                                
013100                                                                          
013200 01  WS-ID-TAB.                                                           
013300     03 WS-FILLER-IN.                                                     
013400       05  FILLER                PIC X(10) VALUE SPACE.                   
013500     03 WS-ID-IN REDEFINES WS-FILLER-IN OCCURS 10.                        
013600        05 TAB-ID-IN             PIC X(1).                                
013700     03 WS-FILLER-UT.                                                     
013800       05  FILLER                PIC X(10) VALUE SPACE.                   
013900     03 WS-ID-UT REDEFINES WS-FILLER-UT OCCURS 10.                        
014000        05 TAB-ID-UT             PIC X(1).                                
014100     EJECT                                                                
014200                                                                          
014300 01  WS-REVAT.                                                            
014400     03  WS-REVAT-TAL            PIC 9(5).                                
014500     03  WS-REVAT-DELAR REDEFINES WS-REVAT-TAL.                           
014600         05 WS-REVAT-HELTAL      PIC 9(3).                                
014700         05 WS-REVAT-DECIMAL     PIC 9(2).                                
014800     EJECT                                                                
014900                                                                          
015000 01  WS-DATUM.                                                            
015100     03  WS-DAGENS-DATUM         PIC 9(8).                                
015200     EJECT                                                                
015300                                                                          
015400*    --- PARAMETRAR TILL DATKORT                                          
015500*                                                                         
015600 01  DATUMKORT-ID                PIC X(6)  VALUE 'WDATUM'.                
015700                                                                          
015800*01  -COPY WDATKORT                                                       
015900     EJECT                                                                
016000                                                                          
016100 01  FILLER                        PIC X(16) VALUE 'IN-AREA'.             
016200 01  IN-AREA.                                                             
016300     03  IN-AREA-HDR.                                                     
016301       05  IN-KDTRADP              PIC X(4).                              
016310     03  IN-AREA1.                                                        
016400       05  IN-IDPTYP               PIC X(3).                              
016500       05  XXX-AREA                PIC X(1047).                           
016600       05  M10-AREA REDEFINES XXX-AREA.                                   
016700         07  IN10-KDTRADP          PIC X(4).                              
016800         07  IN10-IDKONTO          PIC X(10).                             
016900         07  FILLER                PIC X(70).                             
017000         07  IN10-FLKST            PIC X(1).                              
017100         07  IN10-FLANALYS         PIC X(1).                              
017200         07  FILLER                PIC X(1).                              
017300         07  IN10-FLOK             PIC X(1).                              
017400         07  IN10-DADATUM-TO       PIC X(8).                              
017500         07  IN10-DADATUM-FR       PIC X(8).                              
017600       05 M11-AREA REDEFINES XXX-AREA.                                    
017700         07  IN11-KDTRADP          PIC X(4).                              
017800         07  FILLER                PIC X(4).                              
017900         07  IN11-IDKST            PIC X(10).                             
018000         07  FILLER                PIC X(60).                             
018100         07  IN11-DADATUM-FR       PIC X(8).                              
018200         07  IN11-DADATUM-TO       PIC X(8).                              
018300       05 M13-AREA REDEFINES XXX-AREA.                                    
018400         07  IN13-IDANALYS         PIC X(12).                             
018500         07  FILLER                PIC X(64).                             
018600         07  IN13-KDTRADP          PIC X(4).                              
018700         07  FILLER                PIC X(15).                             
018800         07  IN13-FLANALYS         PIC X(1).                              
018900         07  FILLER                PIC X(4).                              
019000         07  IN13-FLBORT           PIC X(1).                              
019100       05 M17-AREA REDEFINES XXX-AREA.                                    
019200         07  FILLER                PIC X(3).                              
019300         07  IN17-IDLAND           PIC X(2).                              
019400         07  FILLER                PIC X(1).                              
019500         07  IN17-KDVAT            PIC X(2).                              
019600         07  IN17-KDVATTYP         PIC X(1).                              
019700         07  FILLER                PIC X(1).                              
019800         07  IN17-REVAT.                                                  
019900           09  IN17-REVAT-HELTAL   PIC X(3).                              
020000           09  IN17-REVAT-PUNKT    PIC X(1).                              
020100           09  IN17-REVAT-DECIMAL  PIC X(2).                              
020200         07  FILLER                PIC X(1).                              
020300         07  IN17-BEVAT            PIC X(50).                             
020400     03  IN-AREA2 REDEFINES IN-AREA1.                                     
020500       05 CUST-AREA.                                                      
020600         07  INCUST-IDPARTNR       PIC X(10).                             
020700         07  INCUST-IDCOMP         PIC X(4).                              
020800         07  FILLER                PIC X(29).                             
020900         07  INCUST-BEBET-NAME1    PIC X(35).                             
021000         07  INCUST-BEBET-NAME2    PIC X(35).                             
021100         07  INCUST-BEBET-NAME3    PIC X(35).                             
021200         07  INCUST-BEBET-NAME4    PIC X(35).                             
021300         07  INCUST-IDALPHA        PIC X(10).                             
021400         07  INCUST-ADBET-STREET   PIC X(35).                             
021500         07  INCUST-ADBET-BOX      PIC X(10).                             
021600         07  INCUST-ADBET-CITY     PIC X(35).                             
021700         07  INCUST-ADBET-PCODE    PIC X(10).                             
021800         07  FILLER                PIC X(80).                             
021900         07  INCUST-IDLAND         PIC X(3).                              
022000         07  FILLER                PIC X(4).                              
022100         07  INCUST-IDTFN          PIC X(16).                             
022200         07  INCUST-IDTFX          PIC X(31).                             
022300         07  FILLER                PIC X(75).                             
022400         07  INCUST-IDLEVNR-AP     PIC X(10).                             
022500         07  FILLER                PIC X(4).                              
022600         07  INCUST-KDTRADP        PIC X(4).                              
022700         07  FILLER                PIC X(37).                             
022800         07  INCUST-IDVAT          PIC X(20).                             
022900         07  FILLER                PIC X(55).                             
023000         07  INCUST-KDBETALV       PIC X(4).                              
023100         07  FILLER                PIC X(18).                             
023200         07  INCUST-KDKREDSP       PIC X(1).                              
023300         07  FILLER                PIC X(155).                            
023400         07  INCUST-IDMAIL         PIC X(130).                            
023500     03  IN-AREA3 REDEFINES IN-AREA1.                                     
023600       05 LEV-AREA.                                                       
023700         07  INLEV-IDPARTNR       PIC X(10).                              
023800         07  INLEV-IDCOMP         PIC X(4).                               
023900         07  FILLER                PIC X(29).                             
024000         07  INLEV-BEBET-NAME1    PIC X(35).                              
024100         07  INLEV-BEBET-NAME2    PIC X(35).                              
024200         07  INLEV-BEBET-NAME3    PIC X(35).                              
024300         07  INLEV-BEBET-NAME4    PIC X(35).                              
024400         07  INLEV-IDALPHA        PIC X(10).                              
024500         07  INLEV-ADBET-STREET   PIC X(35).                              
024600         07  INLEV-ADBET-BOX      PIC X(10).                              
024700         07  INLEV-ADBET-CITY     PIC X(35).                              
024800         07  INLEV-ADBET-PCODE    PIC X(10).                              
024900         07  FILLER                PIC X(80).                             
025000         07  INLEV-IDLAND         PIC X(3).                               
025100         07  FILLER                PIC X(4).                              
025200         07  INLEV-IDTFN          PIC X(16).                              
025300         07  INLEV-IDTFX          PIC X(31).                              
025400         07  FILLER                PIC X(75).                             
025500         07  INLEV-IDLEVNR-AP     PIC X(10).                              
025600         07  FILLER                PIC X(4).                              
025700         07  INLEV-KDTRADP        PIC X(4).                               
025800         07  FILLER                PIC X(37).                             
025900         07  INLEV-IDVAT          PIC X(20).                              
026000         07  FILLER                PIC X(55).                             
026100         07  INLEV-KDBETALV       PIC X(4).                               
026200         07  FILLER                PIC X(10).                             
026300         07  INLEV-KDKREDSP       PIC X(1).                               
026400         07  FILLER                PIC X(194).                            
026500         07  INLEV-IDMAIL         PIC X(130).                             
026600         07  FILLER               PIC X(12).                              
026700         07  INLEV-IND            PIC X(8).                               
026800     EJECT                                                                
026912 01  UT-CONTROL.                                                          
026913     03  FILLER                  PIC X(15)   VALUE                        
026914                                 ' ¤DAPWF1005-001'.                       
026915     EJECT                                                                
026916 01  UT-CONTROL-LEGSEL.                                                   
026917     03  FILLER                  PIC X(5)    VALUE ' ¤DAP'.               
026918     03  UT-CTL-IDLEGSEL         PIC X(4)    VALUE SPACE.                 
026919     EJECT                                                                
026920 01  WF1005A-HEAD.                                                        
026921     03  FILLER               PIC X(12) VALUE 'COUNTRY CODE'.             
026922     03  FILLER               PIC X(1)  VALUE ';'.                        
026923     03  FILLER               PIC X(8)  VALUE 'VAT CODE'.                 
026924     03  FILLER               PIC X(1)  VALUE ';'.                        
026925     03  FILLER               PIC X(8)  VALUE 'VAT TYPE'.                 
026926     03  FILLER               PIC X(1)  VALUE ';'.                        
026927     03  FILLER               PIC X(5)  VALUE 'VAT %'.                    
026928     03  FILLER               PIC X(1)  VALUE ';'.                        
026929     03  FILLER               PIC X(15) VALUE 'VAT DESCRIPTION'.          
026930     03  FILLER               PIC X(1)  VALUE ';'.                        
026931                                                                          
026932 01  WF1005A-DATA.                                                        
026933     03  WF1005A-IDLAND          PIC X(2)  VALUE SPACE.                   
026934     03  FILLER                  PIC X     VALUE ';'.                     
026940     03  WF1005A-KDVAT           PIC X(2)  VALUE SPACE.                   
026950     03  FILLER                  PIC X     VALUE ';'.                     
026960     03  WF1005A-KDVATTYP        PIC X(1)  VALUE SPACE.                   
026970     03  FILLER                  PIC X     VALUE ';'.                     
026980     03  WF1005A-REVAT           PIC X(6)  VALUE SPACE.                   
026990     03  FILLER                  PIC X     VALUE ';'.                     
026991     03  WF1005A-BEVAT           PIC X(50) VALUE SPACE.                   
026992     03  FILLER                  PIC X     VALUE ';'.                     
026995                                                                          
027000 01  FILLER                      PIC X(16) VALUE 'UT-AREA1    '.          
027100*01  AREA -COPY WF10M10   -PRE UT10-                                      
027200     EJECT                                                                
027300                                                                          
027400 01  FILLER                      PIC X(16) VALUE 'UT-AREA2    '.          
027500*01  AREA -COPY WF10M11   -PRE UT11-                                      
027600     EJECT                                                                
027700                                                                          
027800 01  FILLER                      PIC X(16) VALUE 'UT-AREA3    '.          
027900*01  AREA -COPY WF10M13   -PRE UT13-                                      
028000     EJECT                                                                
028100                                                                          
028200 01  FILLER                      PIC X(16) VALUE 'UT-AREA4    '.          
028300*01  AREA -COPY WF10M17   -PRE UT17-                                      
028400     EJECT                                                                
028500                                                                          
028600 01  FILLER                      PIC X(16) VALUE 'UT-AREA5   '.           
028700*01  AREA -COPY WF10CUST -PRE UTCUST-                                     
028800     EJECT                                                                
028801                                                                          
028802*    --- WORK-AREAS FOR DB2-SECTIONS                                      
028803 01  FILLER                      PIC X(16) VALUE 'T01COCO-AREA'.          
028804*01  -COPY T01COCO -PRE T01COCO-                                          
028805     EJECT                                                                
028806 01  FILLER                      PIC X(16) VALUE 'T01LSEL-AREA'.          
028807*01  -COPY T01LSEL -PRE T01LSEL-                                          
028808     EJECT                                                                
028809                                                                          
028810 01  FILLER                      PIC X(16) VALUE 'SQLCA-AREA'.            
028820       EXEC SQL INCLUDE SQLCA END-EXEC.                                   
028821 01  FILLER                      PIC X(16) VALUE 'COCO-AREA'.             
028830       EXEC SQL INCLUDE T01COCO END-EXEC.                                 
028831 01  FILLER                      PIC X(16) VALUE 'LSEL-AREA'.             
028832       EXEC SQL INCLUDE T01LSEL END-EXEC.                                 
028833                                                                          
028840 01  DB2-WS.                                                              
028850     03  SQLCODE-WS              PIC 9(3)    VALUE ZERO.                  
028860         88  CURSOR-OK                       VALUE 000.                   
028870         88  LINES-FOUND                     VALUE 000.                   
028880         88  LINES-MISSING                   VALUE 100.                   
028890         88  RESOURCE-WRONG                  VALUE 904.                   
028891     03  GOOD-SQLCODECODES.                                               
028892         05  GOOD-SQLCODE OCCURS 5                                        
028893             INDEXED BY SQLCODE-IX PIC 9(3).                              
028900                                                                          
029000 PROCEDURE DIVISION.                                                      
029100                                                                          
029200 STYR SECTION.                                                            
029300     PERFORM A-INIT                                                       
029400                                                                          
029500     PERFORM S01-READ-WF1001                                              
029600     PERFORM UNTIL INPOST1-EOF                                            
029700       EVALUATE IN-IDPTYP                                                 
029800       WHEN 'M10'                                                         
029900         IF IN10-KDTRADP = 'SEPV' OR 'SE09'                               
030000           PERFORM B-CREATE-ACCOUNT                                       
030100         END-IF                                                           
030200       WHEN 'M11'                                                         
030300         IF IN11-KDTRADP = 'SEPV' OR 'SE09'                               
030400           PERFORM C-CREATE-COSTCTR                                       
030500         END-IF                                                           
030600       WHEN 'M13'                                                         
030700         IF IN13-KDTRADP = 'SEPV' OR 'SE09'                               
030800           PERFORM D-CREATE-ORDERNO                                       
030900         END-IF                                                           
031000       WHEN 'M17'                                                         
031100         IF IN17-KDVATTYP = 'A' OR 'V'                                    
031200           PERFORM E-CREATE-VAT                                           
031300         END-IF                                                           
031400       WHEN OTHER                                                         
031500         IF IN-IDPTYP (1:1) = '0'                                         
031600           IF INCUST-IDCOMP = 'SEPV'                                      
031700             PERFORM F-CREATE-CUSTOMER                                    
031800           END-IF                                                         
031900         END-IF                                                           
032000       END-EVALUATE                                                       
032100                                                                          
032200       PERFORM S01-READ-WF1001                                            
032300     END-PERFORM                                                          
032400                                                                          
032500     PERFORM S01-READ-WF1007                                              
032600     PERFORM UNTIL INPOST7-EOF                                            
032700       EVALUATE IN-IDPTYP                                                 
032800       WHEN 'M10'                                                         
033000           PERFORM B-CREATE-ACCOUNT                                       
033200       WHEN 'M11'                                                         
033400           PERFORM C-CREATE-COSTCTR                                       
033600       WHEN 'M13'                                                         
033800           PERFORM D-CREATE-ORDERNO                                       
034000       WHEN 'M17'                                                         
034100         IF IN17-KDVATTYP = 'A' OR 'V'                                    
034200           PERFORM E-CREATE-VAT                                           
034300         END-IF                                                           
034400       WHEN OTHER                                                         
034500         IF IN-IDPTYP (1:1) = '0'                                         
034700           PERFORM F-CREATE-CUSTOMER                                      
034900         END-IF                                                           
035000       END-EVALUATE                                                       
035100                                                                          
035200       PERFORM S01-READ-WF1007                                            
035300     END-PERFORM                                                          
035400                                                                          
035410     PERFORM S01-READ-WF1008                                              
035420     PERFORM UNTIL INPOST8-EOF                                            
035430       EVALUATE IN-IDPTYP                                                 
035440       WHEN 'M10'                                                         
035460           PERFORM B-CREATE-ACCOUNT                                       
035480       WHEN 'M11'                                                         
035491           PERFORM C-CREATE-COSTCTR                                       
035493       WHEN 'M13'                                                         
035495           PERFORM D-CREATE-ORDERNO                                       
035497       WHEN 'M17'                                                         
035498         IF IN17-KDVATTYP = 'A' OR 'V'                                    
035499           PERFORM E-CREATE-VAT                                           
035500         END-IF                                                           
035501       WHEN OTHER                                                         
035502         IF IN-IDPTYP (1:1) = '0'                                         
035504           PERFORM F-CREATE-CUSTOMER                                      
035506         END-IF                                                           
035507       END-EVALUATE                                                       
035508                                                                          
035509       PERFORM S01-READ-WF1008                                            
035510     END-PERFORM                                                          
035511                                                                          
035512     PERFORM S01-READ-WF1009                                              
035515     PERFORM UNTIL INPOST9-EOF                                            
035517       EVALUATE IN-IDPTYP                                                 
035518       WHEN 'M10'                                                         
035519           PERFORM B-CREATE-ACCOUNT                                       
035520       WHEN 'M11'                                                         
035521           PERFORM C-CREATE-COSTCTR                                       
035523       WHEN 'M13'                                                         
035525           PERFORM D-CREATE-ORDERNO                                       
035527       WHEN 'M17'                                                         
035528         IF IN17-KDVATTYP = 'A' OR 'V'                                    
035529           PERFORM E-CREATE-VAT                                           
035530         END-IF                                                           
035531       WHEN OTHER                                                         
035532         IF IN-IDPTYP (1:1) = '0'                                         
035534           PERFORM F-CREATE-CUSTOMER                                      
035536         END-IF                                                           
035537       END-EVALUATE                                                       
035538                                                                          
035539       PERFORM S01-READ-WF1009                                            
035540     END-PERFORM                                                          
035541                                                                          
035542     PERFORM S01-READ-WF1010                                              
035544     PERFORM UNTIL INPOST10-EOF                                           
035546       EVALUATE IN-IDPTYP                                                 
035547       WHEN 'M10'                                                         
035548           PERFORM B-CREATE-ACCOUNT                                       
035549       WHEN 'M11'                                                         
035550           PERFORM C-CREATE-COSTCTR                                       
035551       WHEN 'M13'                                                         
035552           PERFORM D-CREATE-ORDERNO                                       
035553       WHEN 'M17'                                                         
035554         IF IN17-KDVATTYP = 'A' OR 'V'                                    
035555           PERFORM E-CREATE-VAT                                           
035556         END-IF                                                           
035557       WHEN OTHER                                                         
035558         IF IN-IDPTYP (1:1) = '0'                                         
035559           PERFORM F-CREATE-CUSTOMER                                      
035560         END-IF                                                           
035561       END-EVALUATE                                                       
035562                                                                          
035563       PERFORM S01-READ-WF1010                                            
035564     END-PERFORM                                                          
035570     PERFORM Z-FINIT                                                      
035600     MOVE ZERO TO RETURN-CODE                                             
035700     GOBACK                                                               
035800     .                                                                    
035900     EJECT                                                                
036000                                                                          
036100 A-INIT SECTION.                                                          
036200     OPEN INPUT  WF1001                                                   
036300                 WF1007                                                   
036310                 WF1008                                                   
036320                 WF1009                                                   
036330                 WF1010                                                   
036400          OUTPUT WF1002                                                   
036500                 WF1003                                                   
036600                 WF1004                                                   
036700                 WF1005                                                   
036800                 WF1006                                                   
036900                 WF1005A                                                  
037000                                                                          
037100     CALL DATKORT USING PROGRAMNAMN DATUMKORT-ID DATUMKORT                
037200     MOVE 20           TO WS-DAGENS-DATUM(1:2)                            
037300     MOVE D-AAR        TO WS-DAGENS-DATUM(3:2)                            
037400     MOVE D-MAANAD     TO WS-DAGENS-DATUM(5:2)                            
037500     MOVE D-DAG        TO WS-DAGENS-DATUM(7:2)                            
037600     .                                                                    
037700     EJECT                                                                
037800                                                                          
037900 B-CREATE-ACCOUNT SECTION.                                                
038000     MOVE SPACE               TO  UT10-WF10M10                            
038001     IF IN10-KDTRADP = 'SE09'                                             
038002       MOVE 'VCCS'            TO  UT10-IDLEGSEL                           
038003     ELSE                                                                 
038004       MOVE IN10-KDTRADP      TO  WS-KDTRADP                              
038005       PERFORM DB2-SEARCH-T01LSEL-TAB                                     
038006       IF LINES-FOUND                                                     
038007         MOVE T01LSEL-IDLEGSEL                                            
038008                              TO UT10-IDLEGSEL                            
038009       END-IF                                                             
038010     END-IF                                                               
038800     MOVE IN10-IDKONTO        TO  UT10-IDKONTO                            
038900     MOVE IN10-KDTRADP        TO  UT10-IDGL                               
039000     IF IN10-FLKST = 'O' OR                                               
039100                     'M'                                                  
039200       MOVE JA                TO  UT10-FLKST                              
039300     ELSE                                                                 
039400       MOVE NEJ               TO  UT10-FLKST                              
039500     END-IF                                                               
039600     IF IN10-FLANALYS = 'O' OR                                            
039700                        'M'                                               
039800       MOVE JA                TO  UT10-FLANALYS                           
039900     ELSE                                                                 
040000       MOVE NEJ               TO  UT10-FLANALYS                           
040100     END-IF                                                               
040200     IF (IN10-DADATUM-FR > WS-DAGENS-DATUM)                               
040300       CONTINUE                                                           
040400     ELSE                                                                 
040500       IF (IN10-FLOK   NOT = 'Y') OR                                      
040600          (IN10-DADATUM-TO < WS-DAGENS-DATUM)                             
040700         MOVE ZERO            TO  UT10-DAREGDAT                           
040800                                  UT10-DAUPPDAT                           
040900         MOVE WS-DAGENS-DATUM TO  UT10-DADELDAT                           
041000         PERFORM S02-WRITE-WF1002                                         
041100       ELSE                                                               
041200         MOVE WS-DAGENS-DATUM TO  UT10-DAREGDAT                           
041300                                  UT10-DAUPPDAT                           
041400         MOVE ZERO            TO  UT10-DADELDAT                           
041500         PERFORM S02-WRITE-WF1002                                         
041600       END-IF                                                             
041700     END-IF                                                               
041800     .                                                                    
041900     EJECT                                                                
042000                                                                          
042100 C-CREATE-COSTCTR SECTION.                                                
042200     MOVE SPACE                  TO  UT11-WF10M11                         
042960     IF IN11-KDTRADP = 'SE09'                                             
042970       MOVE 'VCCS'               TO  UT11-IDLEGSEL                        
042980     ELSE                                                                 
042990       MOVE IN11-KDTRADP         TO  WS-KDTRADP                           
042991       PERFORM DB2-SEARCH-T01LSEL-TAB                                     
042992       IF LINES-FOUND                                                     
042993         MOVE T01LSEL-IDLEGSEL   TO  UT11-IDLEGSEL                        
042994                                                                          
042995       END-IF                                                             
042996     END-IF                                                               
043000     MOVE IN11-IDKST             TO  UT11-IDKST                           
043100     MOVE IN11-KDTRADP           TO  UT11-IDGL                            
043200     IF (IN11-DADATUM-FR > WS-DAGENS-DATUM)                               
043300       CONTINUE                                                           
043400     ELSE                                                                 
043500       IF (IN11-DADATUM-TO < WS-DAGENS-DATUM)                             
043600         MOVE ZERO               TO  UT11-DAREGDAT                        
043700         MOVE WS-DAGENS-DATUM    TO  UT11-DADELDAT                        
043800         PERFORM S03-WRITE-WF1003                                         
043900       ELSE                                                               
044000         MOVE WS-DAGENS-DATUM    TO  UT11-DAREGDAT                        
044100         MOVE ZERO               TO  UT11-DADELDAT                        
044200         PERFORM S03-WRITE-WF1003                                         
044300       END-IF                                                             
044400     END-IF                                                               
044500     .                                                                    
044600     EJECT                                                                
044700                                                                          
044800 D-CREATE-ORDERNO SECTION.                                                
044900     MOVE SPACE                  TO  UT13-WF10M13                         
045670     IF IN13-KDTRADP = 'SE09'                                             
045680       MOVE 'VCCS'               TO  UT13-IDLEGSEL                        
045690     ELSE                                                                 
045691       MOVE IN13-KDTRADP         TO  WS-KDTRADP                           
045692       PERFORM DB2-SEARCH-T01LSEL-TAB                                     
045693       IF LINES-FOUND                                                     
045694         MOVE T01LSEL-IDLEGSEL   TO  UT13-IDLEGSEL                        
045695                                                                          
045696       END-IF                                                             
045697     END-IF                                                               
045700     MOVE IN13-IDANALYS          TO  UT13-IDANALYS                        
045800     MOVE IN13-KDTRADP           TO  UT13-IDGL                            
045900     IF (IN13-FLANALYS NOT = '1') OR                                      
046000        (IN13-FLBORT       = 'Y')                                         
046100       MOVE ZERO                 TO  UT13-DAREGDAT                        
046200       MOVE WS-DAGENS-DATUM      TO  UT13-DADELDAT                        
046300     ELSE                                                                 
046400       MOVE WS-DAGENS-DATUM      TO  UT13-DAREGDAT                        
046500       MOVE ZERO                 TO  UT13-DADELDAT                        
046600     END-IF                                                               
046700     PERFORM S04-WRITE-WF1004                                             
046800     .                                                                    
046900     EJECT                                                                
047000                                                                          
047100 E-CREATE-VAT SECTION.                                                    
047200     MOVE SPACE                  TO  UT17-WF10M17                         
047740     MOVE IN17-IDLAND            TO  WS-IDLAND                            
047750     PERFORM DB2-SEARCH-T01LSEL-IDLAND                                    
047760     IF LINES-FOUND                                                       
047770       MOVE T01LSEL-IDLEGSEL     TO  UT17-IDLEGSEL                        
047790     END-IF                                                               
047800                                                                          
047810     IF IN17-IDLAND = 'SE'                                                
047900       IF IN17-KDVAT   > '00' AND <= '30'                                 
048000* MOMSKODER FÖR SVERIGE                                                   
048010         IF IN17-KDVAT NUMERIC                                            
048100           MOVE IN17-IDLAND      TO  UT17-IDLAND                          
048200         ELSE                                                             
048201           MOVE IN17-BEVAT(1:2)  TO  UT17-IDLAND                          
048202         END-IF                                                           
048210       ELSE                                                               
048300         IF IN17-KDVAT >= '60' AND <= '90'                                
048400* GENERELLA KODER FÖR MOMSFRITT                                           
048410           IF IN17-KDVAT NUMERIC                                          
048500             MOVE 'EU'             TO  UT17-IDLAND                        
048510           ELSE                                                           
048520             MOVE IN17-BEVAT(1:2)  TO  UT17-IDLAND                        
048530           END-IF                                                         
048600         ELSE                                                             
048700* MOMSKODER FÖR ÖVRIGA LÄNDER                                             
048800           MOVE IN17-BEVAT(1:2)  TO  UT17-IDLAND                          
048900         END-IF                                                           
049000       END-IF                                                             
049001     ELSE                                                                 
049002       MOVE IN17-IDLAND          TO  UT17-IDLAND                          
049010     END-IF                                                               
049100     MOVE IN17-KDVAT           TO  UT17-KDVAT                             
049200                                                                          
049300     INSPECT IN17-REVAT REPLACING LEADING SPACE BY ZERO                   
049400     MOVE IN17-REVAT-HELTAL    TO WS-REVAT-HELTAL                         
049500     MOVE IN17-REVAT-DECIMAL   TO WS-REVAT-DECIMAL                        
049600                                                                          
049700     COMPUTE UT17-REVAT = WS-REVAT-TAL / 100                              
049800     END-COMPUTE                                                          
049900                                                                          
050000     MOVE IN17-BEVAT           TO  UT17-BEVAT                             
050100     MOVE WS-DAGENS-DATUM      TO  UT17-DAREGDAT                          
050200                                   UT17-DAUPPDAT                          
050300                                   UT17-DADELDAT                          
050301                                                                          
050302*  VERIFY COUNTRY CODE IN T01COCO TAB ***                                 
050310     IF UT17-IDLAND = 'EU'                                                
050320       PERFORM S05-WRITE-WF1005                                           
050330     ELSE                                                                 
050400       PERFORM DB2-SEARCH-T01COCO-TAB                                     
050600       IF LINES-FOUND                                                     
050700         PERFORM S05-WRITE-WF1005                                         
050710       ELSE                                                               
050711         PERFORM S11-CREATE-WF1005A                                       
050712       END-IF                                                             
050800     END-IF                                                               
050900     .                                                                    
051000     EJECT                                                                
051100                                                                          
051200 F-CREATE-CUSTOMER SECTION.                                               
051300     MOVE SPACE                     TO  UTCUST-WF10CUST                   
052093     IF INCUST-IDCOMP = 'SE09'                                            
052094       MOVE 'VCCS'                  TO  UTCUST-IDLEGSEL                   
052095     ELSE                                                                 
052096       MOVE INCUST-IDCOMP           TO  WS-KDTRADP                        
052097       PERFORM DB2-SEARCH-T01LSEL-TAB                                     
052098       IF LINES-FOUND                                                     
052099         MOVE T01LSEL-IDLEGSEL      TO  UTCUST-IDLEGSEL                   
052100                                                                          
052101       END-IF                                                             
052102     END-IF                                                               
052110     MOVE INCUST-IDPARTNR           TO  WS-FILLER-IN                      
052200     MOVE 1                         TO  UTCUST-KDSTATUS                   
052300     PERFORM S10-CONVERT-ID                                               
052400     MOVE WS-FILLER-UT              TO  UTCUST-IDPARTNR                   
052500     MOVE FUNCTION UPPER-CASE(INCUST-BEBET-NAME1)                         
052600                                    TO UTCUST-BEBET-NAME1                 
052700     MOVE FUNCTION UPPER-CASE(INCUST-BEBET-NAME2)                         
052800                                    TO UTCUST-BEBET-NAME2                 
052900     MOVE FUNCTION UPPER-CASE(INCUST-BEBET-NAME3)                         
053000                                    TO UTCUST-BEBET-NAME3                 
053100     MOVE FUNCTION UPPER-CASE(INCUST-BEBET-NAME4)                         
053200                                    TO UTCUST-BEBET-NAME4                 
053300     MOVE UTCUST-BEBET-NAME1(1:10)  TO  UTCUST-IDALPHA                    
053400     MOVE FUNCTION UPPER-CASE(INCUST-ADBET-STREET)                        
053500                                    TO UTCUST-ADBET-STREET                
053600     MOVE FUNCTION UPPER-CASE(INCUST-ADBET-BOX)                           
053700                                    TO UTCUST-ADBET-BOX                   
053800     MOVE FUNCTION UPPER-CASE(INCUST-ADBET-CITY)                          
053900                                    TO UTCUST-ADBET-CITY                  
054000     MOVE FUNCTION UPPER-CASE(INCUST-ADBET-PCODE)                         
054100                                    TO UTCUST-ADBET-PCODE                 
054200     MOVE INCUST-IDLAND             TO  UTCUST-IDLANDX3                   
054300     MOVE 'EN'                      TO  UTCUST-IDSPRAK                    
054400     MOVE INCUST-IDTFN              TO  UTCUST-IDTFN                      
054500     MOVE INCUST-IDTFX              TO  UTCUST-IDTFX                      
054600     MOVE INCUST-IDLEVNR-AP         TO  WS-FILLER-IN                      
054700     MOVE INCUST-KDBETALV           TO  UTCUST-KDBETALV                   
054800     PERFORM S10-CONVERT-ID                                               
054900     MOVE WS-FILLER-UT              TO  UTCUST-IDLEVNR-AP                 
055000     MOVE INCUST-KDTRADP            TO  UTCUST-KDTRADP                    
055100     MOVE INCUST-IDVAT              TO  UTCUST-IDVAT                      
055200     IF INLEV-IND NUMERIC                                                 
055300       MOVE INLEV-KDKREDSP          TO  UTCUST-KDKREDSP                   
055400       MOVE INLEV-IDMAIL            TO  UTCUST-IDMAIL                     
055500     ELSE                                                                 
055600       MOVE INCUST-KDKREDSP         TO  UTCUST-KDKREDSP                   
055700       MOVE INCUST-IDMAIL           TO  UTCUST-IDMAIL                     
055800     END-IF                                                               
055900     MOVE 'EXT'                     TO  UTCUST-KDPARTTY                   
056000     MOVE WS-DAGENS-DATUM           TO  UTCUST-DAREGDAT                   
056100                                        UTCUST-DAUPPDAT                   
056200     MOVE 'SAP-R3'                  TO  UTCUST-IDUSER                     
056300     PERFORM S06-WRITE-WF1006                                             
056400     .                                                                    
056500     EJECT                                                                
056600                                                                          
056700 Z-FINIT SECTION.                                                         
056800     CLOSE  WF1001                                                        
056900            WF1002                                                        
057000            WF1003                                                        
057100            WF1004                                                        
057200            WF1005                                                        
057300            WF1006                                                        
057400            WF1007                                                        
057410            WF1008                                                        
057420            WF1009                                                        
057421            WF1010                                                        
057430            WF1005A                                                       
057500     .                                                                    
057600     EJECT                                                                
057700                                                                          
057800 S01-READ-WF1001 SECTION.                                                 
057900     READ WF1001            INTO IN-AREA1                                 
058000     AT END                                                               
058100         MOVE JA            TO INPOST1-EOF-SW                             
058200     END-READ                                                             
058300     .                                                                    
058400                                                                          
058500 S01-READ-WF1007 SECTION.                                                 
058600     READ WF1007            INTO IN-AREA1                                 
058700     AT END                                                               
058800         MOVE JA            TO INPOST7-EOF-SW                             
058900     END-READ                                                             
059000     .                                                                    
059100     EJECT                                                                
059200                                                                          
059210 S01-READ-WF1008 SECTION.                                                 
059220     READ WF1008            INTO IN-AREA1                                 
059230     AT END                                                               
059240         MOVE JA            TO INPOST8-EOF-SW                             
059250     END-READ                                                             
059260     .                                                                    
059270     EJECT                                                                
059280                                                                          
059290 S01-READ-WF1009 SECTION.                                                 
059291     READ WF1009            INTO IN-AREA1                                 
059292     AT END                                                               
059293         MOVE JA            TO INPOST9-EOF-SW                             
059294     END-READ                                                             
059295     .                                                                    
059296     EJECT                                                                
059297                                                                          
059298 S01-READ-WF1010 SECTION.                                                 
059299     READ WF1010            INTO IN-AREA                                  
059301     AT END                                                               
059302         MOVE JA            TO INPOST10-EOF-SW                            
059303     END-READ                                                             
059304     .                                                                    
059305     EJECT                                                                
059306                                                                          
059310 S02-WRITE-WF1002 SECTION.                                                
059400     WRITE UT-POST1     FROM UT10-WF10M10                                 
059500     .                                                                    
059600     EJECT                                                                
059700                                                                          
059800 S03-WRITE-WF1003 SECTION.                                                
059900     WRITE UT-POST2     FROM UT11-WF10M11                                 
060000     .                                                                    
060100     EJECT                                                                
060200                                                                          
060300 S04-WRITE-WF1004 SECTION.                                                
060400     WRITE UT-POST3     FROM UT13-WF10M13                                 
060500     .                                                                    
060600     EJECT                                                                
060700                                                                          
060800 S05-WRITE-WF1005 SECTION.                                                
060900     WRITE UT-POST4     FROM UT17-WF10M17                                 
061000     .                                                                    
061100     EJECT                                                                
061200                                                                          
061300 S06-WRITE-WF1006 SECTION.                                                
061400     WRITE UT-POST5     FROM UTCUST-WF10CUST                              
061500     .                                                                    
061600     EJECT                                                                
061700                                                                          
061800 S10-CONVERT-ID SECTION.                                                  
061900     INSPECT WS-FILLER-IN REPLACING LEADING '0' BY SPACE                  
062000     MOVE SPACE                  TO  WS-FILLER-UT                         
062100     MOVE 1                      TO  IND-IN                               
062200                                     IND-UT                               
062300     PERFORM UNTIL IND-IN > 10                                            
062400       IF TAB-ID-IN (IND-IN) NOT = SPACE                                  
062500         MOVE TAB-ID-IN (IND-IN) TO  TAB-ID-UT (IND-UT)                   
062600         ADD 1                   TO  IND-IN                               
062700                                     IND-UT                               
062800       ELSE                                                               
062900         ADD 1                   TO  IND-IN                               
063000       END-IF                                                             
063100     END-PERFORM                                                          
063200     .                                                                    
063210 S11-CREATE-WF1005A SECTION.                                              
063220     MOVE UT17-IDLAND        TO WF1005A-IDLAND                            
063230     MOVE IN17-KDVAT         TO WF1005A-KDVAT                             
063240     MOVE IN17-KDVATTYP      TO WF1005A-KDVATTYP                          
063250     MOVE IN17-REVAT         TO WF1005A-REVAT                             
063260     MOVE IN17-BEVAT         TO WF1005A-BEVAT                             
063270                                                                          
063293     IF UT17-IDLEGSEL = W-IDLEGSEL-PREV                                   
063294       CONTINUE                                                           
063295     ELSE                                                                 
063296       MOVE UT17-IDLEGSEL    TO W-IDLEGSEL-PREV                           
063297                                UT-CTL-IDLEGSEL                           
063298                                                                          
063299       WRITE WF1005A-ERR   FROM UT-CONTROL                                
063300       WRITE WF1005A-ERR   FROM UT-CONTROL-LEGSEL                         
063301       WRITE WF1005A-ERR   FROM WF1005A-HEAD                              
063302     END-IF                                                               
063303     WRITE WF1005A-ERR     FROM WF1005A-DATA                              
063304     .                                                                    
063310*** - CHECK THAT REQUESTED COUNTRY EXISTS                                 
063400 DB2-SEARCH-T01COCO-TAB SECTION.                                          
063500                                                                          
063600     MOVE 000100 TO GOOD-SQLCODECODES                                     
063700                                                                          
063800     EXEC SQL                                                             
063900           SELECT  BELAND                                                 
064000                                                                          
064100           INTO   :T01COCO-BELAND                                         
064200                                                                          
064300           FROM    T01COCO                                                
064400                                                                          
064500           WHERE   IDLANDX3 = :UT17-IDLAND                                
064600     END-EXEC                                                             
064700                                                                          
064800     MOVE SQLCODE TO SQLCODE-WS                                           
064900     PERFORM DB2-STATUS-CHECK                                             
065000     .                                                                    
065010 DB2-SEARCH-T01LSEL-TAB SECTION.                                          
065020                                                                          
065030     MOVE 000100305      TO GOOD-SQLCODECODES                             
065040                                                                          
065050     EXEC SQL                                                             
065060      SELECT  IDLEGSEL                                                    
065080                                                                          
065090      INTO    :T01LSEL-IDLEGSEL                                           
065092                                                                          
065093      FROM    T01LSEL                                                     
065094                                                                          
065095      WHERE   KDTRADP = :WS-KDTRADP                                       
065096     END-EXEC                                                             
065097                                                                          
065098     MOVE SQLCODE        TO SQLCODE-WS                                    
065099     PERFORM DB2-STATUS-CHECK                                             
065100     .                                                                    
065101 DB2-SEARCH-T01LSEL-IDLAND SECTION.                                       
065102                                                                          
065103     MOVE 000100305      TO GOOD-SQLCODECODES                             
065104                                                                          
065105     EXEC SQL                                                             
065106      SELECT  IDLEGSEL                                                    
065107                                                                          
065108      INTO    :T01LSEL-IDLEGSEL                                           
065109                                                                          
065110      FROM    T01LSEL                                                     
065111                                                                          
065112      WHERE   IDLANDX3 = :WS-IDLAND                                       
065113     END-EXEC                                                             
065114                                                                          
065115     MOVE SQLCODE        TO SQLCODE-WS                                    
065116     PERFORM DB2-STATUS-CHECK                                             
065117     .                                                                    
065120 DB2-STATUS-CHECK  SECTION.                                               
065200                                                                          
065300     SET SQLCODE-IX TO 1                                                  
065310     SEARCH GOOD-SQLCODE                                                  
065320       AT END                                                             
065330          STRING 'INVALID DB2 SQL STATUS CODE: ' SQLCODE-WS               
065340          DELIMITED BY SIZE INTO ERROR-TEXT                               
065350          CALL ABEND USING RKOD-ABEND-DB2                                 
065360       WHEN GOOD-SQLCODE (SQLCODE-IX) = SQLCODE-WS                        
065370          CONTINUE                                                        
065380     END-SEARCH                                                           
065390     .                                                                    
