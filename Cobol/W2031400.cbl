000100 PROCESS DYNAM                                                            
000200 ID DIVISION.                                                             
000300     SKIP2                                                                
000400 PROGRAM-ID.     W2031400.                                                
000500*AUTHOR.         STEFAN ÅSGÅRDEN.                                         
000600*DATE-WRITTEN.   JUNI 2003.                                               
000700                                                                          
000800*    REMARKS.                                                             
000900*                                                                         
001000*    FUNKTION:                                                            
001100*        PROGRAMMETS UPPGIFT ÄR ATT VISA KAMPANJINFO                      
001200*                                                                         
001300*       EXEMPEL PÅ DB2KOD SE W3011100, WF025200                           
001400*       EXEMPEL PÅ BLÄDDRING SE W4010800                                  
001500*       EXEMPEL PÅ HOPP      SE W4051100                                  
001600*                                                                         
001700*                                                                         
001800*    INDATA.                                                              
001900*        TRANSAKTION: W2T314                                              
002000*        MID:         W2I31401                                            
002100*                                                                         
002200*    UTDATA.                                                              
002300*        MOD:         W2O31401                                            
002400                                                                          
002500     SKIP3                                                                
002600 ENVIRONMENT DIVISION.                                                    
002700     EJECT                                                                
002800 DATA DIVISION.                                                           
002900 WORKING-STORAGE SECTION.                                                 
003000*    -COPY WY2000W1                                                       
003100     SKIP3                                                                
003200 77  IDPGM                       PIC X(08)   VALUE 'W2031400'.            
003300                                                                          
003400*    --- ARBETSFÄLT FÖR FELMEDDELANDEN VID CALL ABEND/FELLOG              
003500 77  FELTEXT                     PIC X(80) VALUE SPACE.                   
003600                                                                          
003700 77  JA                          PIC X       VALUE 'J'.                   
003800 77  YES                         PIC X       VALUE 'Y'.                   
003900 77  NEJ                         PIC X       VALUE 'N'.                   
004000 77  IX                          PIC 9(9)    VALUE ZERO.                  
004100 77  RAD-IX                      PIC 9(9)    VALUE ZERO.                  
004200 77  RAD-MAX                     PIC 9(9)    VALUE 12.                    
004300 77  IX-SISTA-POST               PIC 9(9)    VALUE ZERO.                  
004400 77  DAGENS-DATUM                PIC 9(8)    VALUE ZERO.                  
004500                                                                          
004600 77  SPRAK-IX                    PIC S9(9)  VALUE +0   COMP SYNC.         
004700                                                                          
004800*    --- ARBETSFÄLT FÖR AKTUELLA NYCKELVÄRDEN FRÅN SKÄRMEN                
004900                                                                          
005000 77  SPAR-KDERS                  PIC 9(3)    VALUE ZERO.                  
005100                                                                          
005200 77  ARTIKEL-ERS-SW              PIC X       VALUE 'N'.                   
005300     88  ARTIKEL-ERS-MAERKT                  VALUE 'J'.                   
005400                                                                          
005500 77  ARTIKEL-FINNS-I-PULS-SW     PIC X       VALUE 'J'.                   
005600     88  ARTIKEL-FINNS-EJ-I-PULS             VALUE 'N'.                   
005700                                                                          
005800 77  INDATA-SW                   PIC X       VALUE 'J'.                   
005900     88  INDATA-OK                           VALUE 'J'.                   
006000     88  INDATA-FEL                          VALUE 'N'.                   
006100                                                                          
006200 77  HOPP-SW                     PIC X       VALUE 'J'.                   
006300     88  HOPP-JA                             VALUE 'J'.                   
006400     88  HOPP-NEJ                            VALUE 'N'.                   
006500                                                                          
006600 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
006700     88  NYCKLAR-OK                          VALUE 'J'.                   
006800     88  NYCKLAR-FEL                         VALUE 'N'.                   
006900                                                                          
007000 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
007100     88  HELP-MID                            VALUE '0551'.                
007200     88  EGEN-MID                            VALUE '2314'.                
007300     88  2317-MID                            VALUE '2317'.                
007400     88  GODK-MID                            VALUE '2314' '2342'          
007500                                                   '2343'.                
007600     EJECT                                                                
007700*    --- PARAMETRAR TILL SUBPROGRAM WINTSOR                               
007800 01  TABENTRY-PARM.                                                       
007900     03  STEGLANGD               PIC S9(9) COMP  VALUE 56.                
008000     03  ANTAL                   PIC S9(9) COMP.                          
008100     03  NYCKELLANGD             PIC S9(9) COMP  VALUE 9.                 
008200                                                                          
008300                                                                          
008400 01  WS.                                                                  
008500*********************************************************                 
008600*    WS-MSGI-AREA-2314                                                    
008700*           ANVÄNDS FÖR ATT SPARA PÅ NYCKELDATABASEN WDP7                 
008800*           (I MSGI-SPAR-AREA)                                            
008900*********************************************************                 
009000  05 WS-MSGI-AREA-2314.                                                   
009100    10 WS-MSGI-IDTRANS-2314      PIC X(4)    VALUE '2314'.                
009200    10 WS-MSGI-SSA-KEY-ENTER.                                             
009300      15  WS-ENTER-IDARTNR       PIC 9(9)    VALUE ZERO.                  
009400      15  WS-ENTER-RADNR         PIC 9(3) VALUE ZERO.                     
009500    10 WS-MSGI-SSA-KEY-NEXT.                                              
009600      15  WS-NEXT-IDARTNR        PIC 9(9)    VALUE ZERO.                  
009700      15  WS-NEXT-RADNR          PIC 9(3) VALUE ZERO.                     
009800    10 FILLER                    PIC X(900)  VALUE SPACE.                 
009900                                                                          
010000     03  WS-TABELL-MAX           PIC 9(3)    VALUE 200.                   
010100     03  WS-TABELL.                                                       
010200      04 WS-TAB-POST    OCCURS 200.                                       
010300       05  WS-TAB-RAD.                                                    
010400        06  WS-IDKAMP-TAB        PIC X(7).                                
010500        06  WS-IDARTNR-TAB       PIC S9(9).                               
010600        06  WS-KVREPANT-TAB      PIC S9(3)V9(2).                          
010700        06  WS-FLKVKAMP-TOTAL-TAB  PIC X.                                 
010800        06  WS-KVKAMP-TOTAL-TAB  PIC S9(7).                               
010900        06  WS-KVKAMP-LAUNCH-TAB PIC S9(7).                               
011000        06  WS-KVKAMP-FIRST-TAB  PIC S9(7).                               
011100        06  WS-RERESPRT-TAB      PIC S9(1)V9(2).                          
011200        06  WS-KDKAMP-TAB        PIC X(1).                                
011300       05  WS-TAB-SORT.                                                   
011400         06 WS-IDARTNR-TAB-SORT  PIC S9(9).                               
011500                                                                          
011600     03 WS-IDARTNR               PIC X(9)    VALUE SPACE.                 
011700     03 WS-IDARTNR-NUM           PIC 9(9)    VALUE ZERO.                  
011800     03 WS-AAVVD-NUM             PIC 9(5)    VALUE ZERO.                  
011900     03 WS-IDKAMP                PIC X(7)    VALUE SPACE.                 
012000     03 WS-IDKAMP-GRP            PIC 9(7)    VALUE ZERO.                  
012100     03 WS-TEMFSFEL              PIC X(40)   VALUE SPACE.                 
012200     03 WS-VISA-UNIK             PIC X(01)   VALUE SPACE.                 
012300     03 WS-VISA-DEF-ERS          PIC X(01)   VALUE SPACE.                 
012400     03 WS-IDKAMP-SPARA          PIC X(7)    VALUE SPACE.                 
012500     03 WS-KVKAMP-CARS-SPARA     PIC 9(7)    VALUE ZERO.                  
012600     03 WS-FLKVKAMP-TOTAL        PIC X.                                   
012700     03 WS-KVKAMP-TOTAL          PIC S9(7).                               
012800     03 WS-KVKAMP-LAUNCH         PIC S9(7).                               
012900     03 WS-KVKAMP-FIRST          PIC S9(7).                               
013000     03 WS-TOTAL                 PIC 9(7)    VALUE ZERO.                  
013100     03 WS-RERESPRT-SPARA        PIC S9(1)V9(5)                           
013200                                             VALUE ZERO.                  
013300     03 WS-KVREPANT-RED          PIC Z(2)9.9(2).                          
013400     03 WS-KVREPANT              PIC S9(3)V9(2)                           
013500                                             COMP-3 VALUE ZERO.           
013600     03 WS-RERESPRT              PIC S9(1)V9(5)                           
013700                                             COMP-3 VALUE ZERO.           
013800     03 W-RERESPRT               PIC S9(1)V9(5)                           
013900                                             COMP-3 VALUE ZERO.           
014000     03 WS-RERESPRT-NUM          PIC 9(3)    VALUE ZERO.                  
014100     03 WS-RERESPRT-RED          PIC ZZ9.                                 
014200     03 WS-IDANSK-NUM            PIC 9(3)    VALUE ZERO.                  
014300     03 WS-IDANSK-RED            PIC ZZ9.                                 
014400     03 WS-KDERS-NUM             PIC 9(3)    VALUE ZERO.                  
014500     03 WS-KDERS-RED             PIC Z9.                                  
014600     03 WS-KVKAMP-TOTAL-UPD      PIC S9(7)   COMP-3 VALUE ZERO.           
014700     03 WS-SECTION               PIC X(24)   VALUE SPACE.                 
014800     03 FILLER                   PIC X(16)   VALUE                        
014900                                             'WS-DB2-SEKTION'.            
015000     03 WS-DB2-SEKTION           PIC X(24)   VALUE SPACE.                 
015100                                                                          
015200 77  UPD-RAD-SW                  PIC X       VALUE 'N'.                   
015300     88  UPD-RAD-JA                          VALUE 'J'.                   
015400     88  UPD-RAD-NEJ                         VALUE 'N'.                   
015500                                                                          
015600 77  REG-NY-RAD-SW               PIC X       VALUE 'N'.                   
015700     88  REG-NY-RAD-JA                       VALUE 'J'.                   
015800     88  REG-NY-RAD-NEJ                      VALUE 'N'.                   
015900     EJECT                                                                
016000                                                                          
016100*01 -COPY WWPRODSL                                                        
016200                                                                          
016300*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
016400 01  GENERELLA-SUBPROGRAM.                                                
016500     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
016600     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
016700     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
016800     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
016900     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
017000     03  WDAGKONV                PIC X(8)    VALUE 'WDAGKONV'.            
017100     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
017200     03  WINTSOR                 PIC X(8)    VALUE 'WINTSOR'.             
017300     EJECT                                                                
017400*    --- PARAMETRAR TILL ABEND                                            
017500                                                                          
017600 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
017700 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
017800     SKIP2                                                                
017900*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
018000*01 -COPY WMEDAREA                                                        
018100     EJECT                                                                
018200*    --- PARAMETRAR TILL SUBPROGRAM WDATKONV                              
018300*01  -COPY WDATAREA                                                       
018400     EJECT                                                                
018500*    --- PARAMETRAR TILL SUBPROGRAM WDAGKONV                              
018600*01  -COPY WDAGAREA                                                       
018700     EJECT                                                                
018800*    --- PARAMETRAR TILL SUBPROGRAM W005INIT                              
018900*01  -COPY WMSGINIT                                                       
019000     EJECT                                                                
019100 01  MESSAGE-CODES.                                                       
019200     03  ERR-CORR-HILITE-FLDS    PIC X(3)    VALUE '001'.                 
019300     03  CONFLICT                PIC X(3)    VALUE '002'.                 
019400     03  INF-PRESS-PF11          PIC X(3)    VALUE '003'.                 
019500     03  INF-URVAL-SAKNAS        PIC X(3)    VALUE '005'.                 
019600     03  INF-FIRST-PAGE          PIC X(3)    VALUE '006'.                 
019700     03  ERR-PF11-AND-NO-DATA    PIC X(3)    VALUE '011'.                 
019800     03  TOM-RAD                 PIC X(3)    VALUE '080'.                 
019900     03  INF-UPDATE-DONE         PIC X(3)    VALUE '101'.                 
020000     03  INF-MORE-INFO-EXISTS    PIC X(3)    VALUE '105'.                 
020100     03  INF-SISTA-SIDAN         PIC X(3)    VALUE '115'.                 
020200     03  INF-PRINT-BEGAERD       PIC X(3)    VALUE '118'.                 
020300     03  INF-PRINT-START         PIC X(3)    VALUE '202'.                 
020400     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
020500     03  AREA-MISSING            PIC X(3)    VALUE '705'.                 
020600     03  ERR-FEL-PRINTER         PIC X(3)    VALUE '772'.                 
020700     03  UPDATING-NOT-ALLOWED    PIC X(3)    VALUE '777'.                 
020800 01  FELTEXTER.                                                           
020900     03  MED-1                  PIC X(40)                                 
021000         VALUE 'PART NO ALREADY EXIST IN THE CAMPAIGN'.                   
021100     03  MED-2                  PIC X(40)                                 
021200         VALUE 'NO ROW IS SELECTED (S)               '.                   
021300     03  MED-3                  PIC X(40)                                 
021400         VALUE 'SUPERSESSION CODE > 19               '.                   
021500     03  MED-4                  PIC X(40)                                 
021600         VALUE 'PART DO NOT EXIST IN PULS             '.                  
021700     03  MED-5                  PIC X(40)                                 
021800         VALUE 'PRODUCT GROUP = 18                   '.                   
021900                                                                          
022000*    --- FÄLT FÖR HOPP TILL ANDRA BILDER                                  
022100   77  SW-STARTA-ANNAN-BILD        PIC X       VALUE 'N'.                 
022200     88  STARTA-ANNAN-BILD                     VALUE 'J'.                 
022300                                                                          
022400 01  BILD-HOPP-AREOR.                                                     
022500                                                                          
022600   03    W-BILD               PIC X(4)    VALUE SPACE.                    
022700   03    W-HOPP-IDTRANS.                                                  
022800     05  FILLER               PIC X(1)    VALUE 'W'.                      
022900     05  W-HOPP-IDTRANS-2     PIC X(1).                                   
023000     05  FILLER               PIC X(1)    VALUE 'T'.                      
023100     05  W-HOPP-IDTRANS-4-6   PIC X(3).                                   
023200     05  FILLER               PIC X(2)    VALUE SPACE.                    
023300                                                                          
023400                                                                          
023500   03 FILLER             PIC X(16)   VALUE 'P-TO-P-AREA'.                 
023600   03      P-TO-P-SW.                                                     
023700                                                                          
023800     05  P-TO-P-KVLL             PIC S9(4) VALUE +117 COMP SYNC.          
023900     05  P-TO-P-KDZ1             PIC X(1)  VALUE LOW-VALUE.               
024000     05  P-TO-P-KDZ2             PIC X(1)  VALUE LOW-VALUE.               
024100     05  P-TO-P-KDTRANS          PIC X(8).                                
024200     05  P-TO-P-IDTRANS          PIC X(4).                                
024300     05  P-TO-P-KDMFSFOR         PIC X(1).                                
024400     05  P-TO-P-DATA             PIC X(100) VALUE ALL '+'.                
024500                                                                          
024600     EJECT                                                                
024700*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
024800*                                                                         
024900 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
025000     SKIP3                                                                
025100*01  MID -COPY W2I31401                                                   
025200     EJECT                                                                
025300 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
025400     SKIP3                                                                
025500*01  -COPY WMSGAREA                                                       
025600     EJECT                                                                
025700     03  MOD REDEFINES MSG-AREA.                                          
025800*      05  -COPY W2O31401                                                 
025900     EJECT                                                                
026000 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
026100     SKIP3                                                                
026200*01  -COPY WMFSAREA                                                       
026300     EJECT                                                                
026400*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
026500*                                                                         
026600     EJECT                                                                
026700 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
026800     SKIP3                                                                
026900 01  NYCKLAR-TILL-DLI.                                                    
027000     03  W-IDARTNR-X.                                                     
027100         05  W-IDARTNR           PIC S9(9)    VALUE ZERO COMP-3.          
027200                                                                          
027300     03  W-IDSKYLT-X.                                                     
027400         05  W-IDSKYLT           PIC X(3)    VALUE SPACE.                 
027500                                                                          
027600     03  W-KDSEGKEY-X.                                                    
027700         05  W-KDSEGKEY          PIC X       VALUE '1'.                   
027800                                                                          
027900 01  NYCKLAR-TILL-DB2.                                                    
028000     03  W-IDKAMP                PIC X(7)    VALUE SPACE.                 
028100                                                                          
028200     03  W-IDKAMP-GRP            PIC S9(7)    VALUE ZERO COMP-3.          
028300                                                                          
028400     03  W-KVKAMP-LAUNCH         PIC S9(7)    VALUE ZERO COMP-3.          
028500                                                                          
028600     03  W-KVKAMP-FIRST          PIC S9(7)    VALUE ZERO COMP-3.          
028700                                                                          
028800     03  W-KVKAMP-TOTAL          PIC S9(7)    VALUE ZERO COMP-3.          
028900                                                                          
029000                                                                          
029100     SKIP2                                                                
029200*                                                                         
029300*        ARBETS-AREOR TILL DB2- OCH IMS-SEKTIONERNA                       
029400*                                                                         
029500 01  FILLER                  PIC X(16)   VALUE 'DB2-WS     '.             
029600 01  FILLER                  PIC X(16) VALUE 'TP1GRP-AREA'.               
029700*01  -COPY TP1GRP   -PRE TP1GRP-                                          
029800     EJECT                                                                
029900 01  FILLER                  PIC X(16) VALUE 'TP1KAMP-AREA'.              
030000*01  -COPY TP1KAMP  -PRE TP1KAMP-                                         
030100     EJECT                                                                
030200 01  FILLER                  PIC X(16) VALUE 'TP1ARTG-AREA'.              
030300*01  -COPY TP1ARTG  -PRE TP1ARTG-                                         
030400     EJECT                                                                
030500 01  FILLER                  PIC X(16) VALUE 'TP1ARTK-AREA'.              
030600*01  -COPY TP1ARTK  -PRE TP1ARTK-                                         
030700     EJECT                                                                
030800       EXEC SQL INCLUDE TP1GRP END-EXEC.                                  
030900                                                                          
031000       EXEC SQL INCLUDE TP1KAMP END-EXEC.                                 
031100                                                                          
031200       EXEC SQL INCLUDE TP1ARTG END-EXEC.                                 
031300                                                                          
031400       EXEC SQL INCLUDE TP1ARTK END-EXEC.                                 
031500                                                                          
031600 01  FILLER                      PIC X(16)   VALUE 'SQLCA-AREA'.          
031700       EXEC SQL INCLUDE SQLCA END-EXEC.                                   
031800     SKIP3                                                                
031900*                        **** STATUS-KOD FRÅN DB2                         
032000     EJECT                                                                
032100                                                                          
032200 01  FILLER                      PIC X(16)   VALUE 'SQLCODE-WS'.          
032300 01  DB2-WS.                                                              
032400     03  SQLCODE-WS              PIC 9(3)    VALUE ZERO.                  
032500         88  CURSOR-OK                       VALUE 000.                   
032600         88  LINES-FOUND                     VALUE 000.                   
032700         88  LINES-MISSING                   VALUE 100.                   
032800         88  RESOURCE-WRONG                  VALUE 904.                   
032900     03  GODK-SQLCODESKODER.                                              
033000         05  GODK-SQLCODE OCCURS 5                                        
033100             INDEXED BY SQLCODE-IX PIC 9(3).                              
033200     EJECT                                                                
033300*    --- STATUS-KOD FRÅN IMS                                              
033400 01  STATUS-WS                   PIC XX.                                  
033500     88  SEGMENT-FINNS                       VALUE '  '.                  
033600     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
033700     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
033800     SKIP2                                                                
033900 01  GODK-STATUSKODER.                                                    
034000     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
034100     SKIP3                                                                
034200 01  SSA1                        PIC X(64).                               
034300 01  SSA2                        PIC X(64).                               
034400     EJECT                                                                
034500*    --- IMS FUNKTIONSKODER                                               
034600*01  -COPY W0003                                                          
034700     EJECT                                                                
034800*    ---  DLI INPUT-OUTPUT AREA                                           
034900 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK601'.                      
035000 01  DLI-IO-WDK601.                                                       
035100*    03  -COPY WDK601                                                     
035200     EJECT                                                                
035300                                                                          
035400 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK611'.                      
035500 01  DLI-IO-WDK611.                                                       
035600*    03  -COPY WDK611                                                     
035700                                                                          
035800     EJECT                                                                
035900 01  FILLER         PIC X(16)  VALUE 'DLI-IO-WDD701'.                     
036000     SKIP3                                                                
036100 01  DLI-IO-WDD701.                                                       
036200*    03  -COPY WDD701  -PRE WDD701-                                       
036300     EJECT                                                                
036400 01  FILLER         PIC X(16)  VALUE 'DLI-IO-WDD702'.                     
036500     SKIP3                                                                
036600 01  DLI-IO-WDD702.                                                       
036700*    03  -COPY WDD702  -PRE WDD702-                                       
036800     EJECT                                                                
036900 01  FILLER         PIC X(16)  VALUE 'DLI-IO-WDD704'.                     
037000     SKIP3                                                                
037100 01  DLI-IO-WDD704.                                                       
037200*    03  -COPY WDD704  -PRE WDD704-                                       
037300     EJECT                                                                
037400                                                                          
037500 LINKAGE SECTION.                                                         
037600                                                                          
037700*01  -COPY W0009   -PRE MSG-                                              
037800     EJECT                                                                
037900*01  -COPY W0009   -PRE ALT-                                              
038000     EJECT                                                                
038100*01  -COPY W0008  -PRE  USEA-                                             
038200     05  FILLER                  PIC X.                                   
038300     EJECT                                                                
038400*01  -COPY W0008  -PRE  WDK6-                                             
038500     05  FILLER                  PIC X.                                   
038600     EJECT                                                                
038700*01  -COPY W0008  -PRE  WDD7-                                             
038800     05  FILLER                  PIC X.                                   
038900     EJECT                                                                
039000 PROCEDURE DIVISION  USING MSG-PCB ALT-PCB USEA-PCB                       
039100                                   WDK6-PCB WDD7-PCB.                     
039200     ENTRY 'DLITCBL' USING MSG-PCB ALT-PCB USEA-PCB                       
039300                                   WDK6-PCB WDD7-PCB.                     
039400                                                                          
039500     PERFORM IMS-GET-MSG                                                  
039600     IF SEGMENT-FINNS                                                     
039700       PERFORM A-INIT                                                     
039800       PERFORM B-KOLLA-NYCKLAR                                            
039900       IF NYCKLAR-OK                                                      
040000         IF MFS-UPDATE                                                    
040100           PERFORM G-KOLLA-INPUT                                          
040200           IF INDATA-OK                                                   
040300             PERFORM H-UPPDATERA                                          
040400           END-IF                                                         
040500         ELSE                                                             
040600           IF MFS-SPLIT                                                   
040700             PERFORM J-HOPP-2315                                          
040800           ELSE                                                           
040900             IF MFS-FIRST                                                 
041000                PERFORM C-FOERSTA-SIDA                                    
041100             ELSE                                                         
041200                IF MFS-NEXT                                               
041300                   PERFORM D-NAESTA-SIDA                                  
041400                ELSE                                                      
041500                   PERFORM E-SAMMA-SIDA                                   
041600                END-IF                                                    
041700             END-IF                                                       
041800           END-IF                                                         
041900         END-IF                                                           
042000                                                                          
042100         IF STARTA-ANNAN-BILD                                             
042200            CONTINUE                                                      
042300         ELSE                                                             
042400           IF INDATA-OK                                                   
042500             IF WS-IDKAMP NOT = SPACE                                     
042600             AND (WS-VISA-UNIK = NEJ                                      
042700             OR WS-VISA-UNIK = SPACE)                                     
042800               PERFORM DB2-SELECT-TP1KAMP                                 
042900               IF SQLCODE = ZERO                                          
043000               AND TP1KAMP-IDKAMP-GRP > ZERO                              
043100                 MOVE TP1KAMP-IDKAMP-GRP                                  
043200                               TO WS-IDKAMP-GRP                           
043300                 MOVE WS-IDKAMP-GRP                                       
043400                               TO MOD-IDKAMP-GRP-UT                       
043500                 INSPECT MOD-IDKAMP-GRP-UT                                
043600                               REPLACING LEADING ZERO BY SPACE            
043700                 MOVE SPACE  TO WS-IDKAMP                                 
043800               END-IF                                                     
043900             END-IF                                                       
044000                                                                          
044100             IF WS-IDKAMP NOT = SPACE                                     
044200               MOVE ZERO     TO WS-IDKAMP-GRP                             
044300               MOVE MFS-RENSA-FAELT                                       
044400                               TO MOD-IDKAMP-GRP-UT                       
044500*                                                                         
044600*   ATT LÄSA MED IDKAMP SOM NYCKEL                                        
044700*                                                                         
044800               PERFORM I-LAES-VISA-INFO                                   
044900             ELSE                                                         
045000               MOVE MFS-RENSA-FAELT                                       
045100                               TO MOD-IDKAMP-UT                           
045200*                                                                         
045300*   ATT LÄSA MED IDKAMP-GRP SOM NYCKEL                                    
045400*                                                                         
045500               PERFORM F-LAES-VISA-INFO                                   
045600             END-IF                                                       
045700           END-IF                                                         
045800         END-IF                                                           
045900                                                                          
046000         IF STARTA-ANNAN-BILD                                             
046100           MOVE ALL '+'      TO MSGI-WMSGINIT                             
046200           MOVE '001'        TO MSGI-KDCALL                               
046300           MOVE MSG-SIGNON-USERID                                         
046400                             TO MSGI-IDUSER                               
046500           MOVE MSG-LTERM-NAME                                            
046600                             TO MSGI-IDLTERM-USER                         
046700           MOVE '2314'       TO MSGI-IDTRANS                              
046800           COMPUTE RAD-IX = RAD-IX - 1                                    
046900           MOVE MID-IDARTNR (RAD-IX)                                      
047000                             TO MSGI-IDARTNR                              
047100           CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                     
047200                                                                          
047300         ELSE                                                             
047400           MOVE ALL '+'      TO MSGI-WMSGINIT                             
047500           MOVE '001'        TO MSGI-KDCALL                               
047600           MOVE MSG-SIGNON-USERID                                         
047700                             TO MSGI-IDUSER                               
047800           MOVE MSG-LTERM-NAME                                            
047900                             TO MSGI-IDLTERM-USER                         
048000           MOVE '2314'       TO MSGI-IDTRANS                              
048100           MOVE WS-IDKAMP    TO MSGI-IDKAMP                               
048200           MOVE WS-IDKAMP-GRP                                             
048300                             TO MSGI-IDKAMP-GRP                           
048400           CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                     
048500                                                                          
048600*   ---  UPPDATERA MSGI-SPAR-AREA                                         
048700           MOVE '002'        TO MSGI-KDCALL                               
048800           MOVE MSG-LTERM-NAME                                            
048900                             TO MSGI-IDLTERM-USER                         
049000           MOVE MSG-SIGNON-USERID                                         
049100                             TO MSGI-IDUSER                               
049200           MOVE '2314'       TO MSGI-IDTRANS                              
049300           MOVE WS-MSGI-AREA-2314                                         
049400                             TO MSGI-SPAR-AREA                            
049500           CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                     
049600         END-IF                                                           
049700                                                                          
049800       END-IF                                                             
049900       IF STARTA-ANNAN-BILD                                               
050000          CONTINUE                                                        
050100       ELSE                                                               
050200         COMPUTE MSG-KVLL = LENGTH OF MOD-W2O31401 + 4                    
050300         PERFORM IMS-INSERT-MSG                                           
050400       END-IF                                                             
050500     END-IF                                                               
050600                                                                          
050700     MOVE ZERO TO RETURN-CODE                                             
050800     GOBACK                                                               
050900     .                                                                    
051000     EJECT                                                                
051100 A-INIT SECTION.                                                          
051200                                                                          
051300     IF MSG-DUBBLA-TRANSKODER                                             
051400       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W2I31401                 
051500       MOVE MSG-IDTRANS-2  TO MFS-IDTRANS                                 
051600       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
051700     ELSE                                                                 
051800       MOVE MSG-INDATA-MINUS-1-TRANSKOD TO MID-W2I31401                   
051900       MOVE MSG-IDTRANS-1  TO MFS-IDTRANS                                 
052000       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
052100     END-IF                                                               
052200                                                                          
052300     MOVE MSG-KDTRTYP TO MFS-KDTRTYP                                      
052400     MOVE MSG-IDPFK   TO MFS-IDPFK                                        
052500     MOVE MFS-IDTRANS TO W-IDTRANS                                        
052600                                                                          
052700     MOVE LOW-VALUE  TO MSG-AREA                                          
052800     MOVE 'W2O314N1' TO MFS-IDMOD                                         
052900     MOVE '2314'     TO MOD-IDTRANS                                       
053000     MOVE MFS-RENSA-FAELT TO MOD-TEMFSFEL MOD-TEMFSINF                    
053100                                                                          
053200     IF EGEN-MID OR HELP-MID                                              
053300       CONTINUE                                                           
053400     ELSE                                                                 
053500       MOVE SPACE TO MFS-KDTRTYP                                          
053600       MOVE '7'   TO MFS-IDPFK                                            
053700     END-IF                                                               
053800     MOVE 'GB '              TO MED-IDSKYLT                               
053900                                                                          
054000     MOVE FUNCTION  CURRENT-DATE(1:8)  TO DAGENS-DATUM                    
054100     MOVE SPACE              TO WS-TEMFSFEL                               
054200     MOVE 1                  TO IX                                        
054300     PERFORM UNTIL IX > WS-TABELL-MAX                                     
054400                                                                          
054500        MOVE SPACE           TO WS-IDKAMP-TAB       (IX)                  
054600                                WS-KDKAMP-TAB       (IX)                  
054700                                WS-FLKVKAMP-TOTAL-TAB (IX)                
054800        MOVE ZERO            TO WS-IDARTNR-TAB      (IX)                  
054900                                WS-KVREPANT-TAB     (IX)                  
055000                                WS-KVKAMP-TOTAL-TAB (IX)                  
055100                                WS-KVKAMP-LAUNCH-TAB(IX)                  
055200                                WS-KVKAMP-FIRST-TAB (IX)                  
055300                                WS-RERESPRT-TAB     (IX)                  
055400                                WS-IDARTNR-TAB-SORT (IX)                  
055500                                                                          
055600        ADD 1                TO IX                                        
055700     END-PERFORM                                                          
055800     .                                                                    
055900     EJECT                                                                
056000 B-KOLLA-NYCKLAR SECTION.                                                 
056100                                                                          
056200     MOVE SPACE              TO WS-MSGI-SSA-KEY-ENTER                     
056300                                WS-MSGI-SSA-KEY-NEXT                      
056400                                                                          
056500******   UPPDATERING AV MSGI-BLÄDDRINGSNYCKLAR SKER                       
056600******   I SLUTET AV PROGRAMMET                                           
056700                                                                          
056800     MOVE JA TO NYCKLAR-SW                                                
056900                                                                          
057000*      -- KONTROLL AV IDKAMP                                              
057100                                                                          
057200     MOVE MFS-RENSA-FAELT TO MOD-IDKAMP-IN                                
057300                             MOD-IDKAMP-GRP-IN                            
057400                                                                          
057500     MOVE ALL '+' TO MSGI-WMSGINIT                                        
057600     MOVE '001'             TO MSGI-KDCALL                                
057700     MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                                
057800     MOVE MSG-LTERM-NAME    TO MSGI-IDLTERM-USER                          
057900     MOVE '2314'            TO MSGI-IDTRANS                               
058000     IF EGEN-MID OR 2317-MID                                              
058100       MOVE MID-IDKAMP-IN    TO MSGI-IDKAMP                               
058200       MOVE MID-IDKAMP-GRP-IN                                             
058300                             TO MSGI-IDKAMP-GRP                           
058400     ELSE                                                                 
058500       MOVE ALL '+'          TO MID-VISA-UNIK-IN                          
058600                                MID-VISA-DEF-ERS-IN                       
058700                                MID-IDKAMP-IN                             
058800                                MID-IDKAMP-GRP-IN                         
058900       MOVE SPACE            TO MID-VISA-UNIK-UT                          
059000                                MID-VISA-DEF-ERS-UT                       
059100     END-IF                                                               
059200     IF 2317-MID                                                          
059300       MOVE ALL '+'          TO MID-VISA-UNIK-IN                          
059400                                MID-VISA-DEF-ERS-IN                       
059500       MOVE SPACE            TO MID-VISA-UNIK-UT                          
059600                                MID-VISA-DEF-ERS-UT                       
059700     END-IF                                                               
059800     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
059900                                                                          
060000     IF EGEN-MID OR 2317-MID                                              
060100     AND MSGI-SPAR-AREA(1:4) = '2314'                                     
060200       MOVE MSGI-SPAR-AREA   TO WS-MSGI-AREA-2314                         
060300     END-IF                                                               
060400                                                                          
060500     MOVE +2      TO SPRAK-IX                                             
060600     MOVE 'GB '   TO MED-IDSKYLT                                          
060700                                                                          
060800     MOVE MSGI-IDKAMP        TO WS-IDKAMP                                 
060900                                MOD-IDKAMP-UT                             
061000     MOVE MSGI-IDKAMP-GRP    TO WS-IDKAMP-GRP                             
061100     INSPECT WS-IDKAMP-GRP REPLACING ALL SPACE BY ZERO                    
061200     MOVE WS-IDKAMP-GRP      TO MOD-IDKAMP-GRP-UT                         
061300     INSPECT MOD-IDKAMP-GRP-UT REPLACING LEADING ZERO BY SPACE            
061400                                                                          
061500     IF MID-IDKAMP-IN = ALL '+'                                           
061600       CONTINUE                                                           
061700     ELSE                                                                 
061800       MOVE '7'              TO MFS-IDPFK                                 
061900       MOVE SPACE            TO MFS-KDTRTYP                               
062000                                MID-VISA-UNIK-UT                          
062100                                MID-VISA-DEF-ERS-UT                       
062200       MOVE ZERO             TO WS-IDKAMP-GRP                             
062300     END-IF                                                               
062400                                                                          
062500     IF MID-IDKAMP-GRP-IN = ALL '+'                                       
062600       CONTINUE                                                           
062700     ELSE                                                                 
062800       MOVE '7'              TO MFS-IDPFK                                 
062900       MOVE SPACE            TO MFS-KDTRTYP                               
063000                                WS-IDKAMP                                 
063100                                MID-VISA-UNIK-UT                          
063200                                MID-VISA-DEF-ERS-UT                       
063300     END-IF                                                               
063400*                                                                         
063500     IF WS-IDKAMP-GRP NUMERIC                                             
063600       CONTINUE                                                           
063700     ELSE                                                                 
063800       MOVE NEJ TO NYCKLAR-SW                                             
063900     END-IF                                                               
064000                                                                          
064100*  -- KONTROLL AV VISA-UNIK-IN                                            
064200                                                                          
064300     MOVE MFS-RENSA-FAELT    TO MOD-VISA-UNIK-IN                          
064400                                                                          
064500     IF MID-VISA-UNIK-IN = ALL '+'                                        
064600       MOVE MID-VISA-UNIK-UT TO WS-VISA-UNIK                              
064700       INSPECT WS-VISA-UNIK REPLACING LEADING '+' BY SPACE                
064800     ELSE                                                                 
064900       MOVE MID-VISA-UNIK-IN TO WS-VISA-UNIK                              
065000       MOVE '7'              TO MFS-IDPFK                                 
065100       MOVE SPACE            TO MFS-KDTRTYP                               
065200     END-IF                                                               
065300*                                                                         
065400     IF WS-VISA-UNIK = JA                                                 
065500     OR WS-VISA-UNIK = YES                                                
065600     OR WS-VISA-UNIK = NEJ                                                
065700     OR WS-VISA-UNIK = SPACE                                              
065800       CONTINUE                                                           
065900     ELSE                                                                 
066000       MOVE NEJ              TO NYCKLAR-SW                                
066100     END-IF                                                               
066200     MOVE WS-VISA-UNIK       TO MOD-VISA-UNIK-UT                          
066300                                                                          
066400*  -- KONTROLL AV VISA-DEF-ERS-IN                                         
066500                                                                          
066600     MOVE MFS-RENSA-FAELT    TO MOD-VISA-DEF-ERS-IN                       
066700                                                                          
066800     IF MID-VISA-DEF-ERS-IN = ALL '+'                                     
066900       MOVE MID-VISA-DEF-ERS-UT                                           
067000                             TO WS-VISA-DEF-ERS                           
067100       INSPECT WS-VISA-DEF-ERS REPLACING LEADING '+' BY SPACE             
067200     ELSE                                                                 
067300       MOVE MID-VISA-DEF-ERS-IN                                           
067400                             TO WS-VISA-DEF-ERS                           
067500       MOVE '7'              TO MFS-IDPFK                                 
067600       MOVE SPACE            TO MFS-KDTRTYP                               
067700     END-IF                                                               
067800*                                                                         
067900     IF WS-VISA-DEF-ERS = JA                                              
068000     OR WS-VISA-DEF-ERS = YES                                             
068100     OR WS-VISA-DEF-ERS = NEJ                                             
068200     OR WS-VISA-DEF-ERS = SPACE                                           
068300       CONTINUE                                                           
068400     ELSE                                                                 
068500       MOVE NEJ              TO NYCKLAR-SW                                
068600     END-IF                                                               
068700     MOVE WS-VISA-DEF-ERS    TO MOD-VISA-DEF-ERS-UT                       
068800                                                                          
068900                                                                          
069000     IF (WS-VISA-UNIK = JA                                                
069100     OR  WS-VISA-UNIK = YES)                                              
069200     AND WS-IDKAMP-GRP > ZERO                                             
069300       MOVE NEJ              TO NYCKLAR-SW                                
069400     END-IF                                                               
069500                                                                          
069600     IF NYCKLAR-FEL                                                       
069700       MOVE ERR-WRONG-KEY TO MED-IDMFSFEL                                 
069800       CALL WMEDKONV USING MED-WMEDAREA                                   
069900       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
070000       PERFORM MFS-RENSA-FAELT-UT                                         
070100     END-IF                                                               
070200     .                                                                    
070300     EJECT                                                                
070400 C-FOERSTA-SIDA SECTION.                                                  
070500                                                                          
070600     MOVE INF-FIRST-PAGE     TO MED-IDMFSFEL                              
070700     CALL WMEDKONV USING MED-WMEDAREA                                     
070800     MOVE MED-TEMFSFEL       TO MOD-TEMFSFEL                              
070900                                                                          
071000*    --- BLANKA/NOLLA UT BLÄDDRINGSNYCKEL                                 
071100     MOVE SPACE              TO WS-MSGI-SSA-KEY-ENTER                     
071200                                WS-MSGI-SSA-KEY-NEXT                      
071300     PERFORM MFS-RENSA-FAELT-IN                                           
071400     .                                                                    
071500     EJECT                                                                
071600 D-NAESTA-SIDA SECTION.                                                   
071700                                                                          
071800     MOVE SPACE              TO WS-MSGI-SSA-KEY-ENTER                     
071900     .                                                                    
072000     EJECT                                                                
072100 E-SAMMA-SIDA SECTION.                                                    
072200                                                                          
072300     IF MID-INPUT = ALL '+'                                               
072400       PERFORM MFS-RENSA-FAELT-IN                                         
072500     ELSE                                                                 
072600       IF EGEN-MID OR HELP-MID                                            
072700         MOVE INF-PRESS-PF11 TO MED-IDMFSINF                              
072800         CALL WMEDKONV USING MED-WMEDAREA                                 
072900         MOVE MED-TEMFSINF                                                
073000                             TO MOD-TEMFSINF                              
073100         PERFORM MFS-LAES-IN-IGEN                                         
073200                                                                          
073300         PERFORM EA-MID-INDATA-TILL-MOD                                   
073400       ELSE                                                               
073500         PERFORM MFS-RENSA-FAELT-IN                                       
073600       END-IF                                                             
073700     END-IF                                                               
073800     .                                                                    
073900 EA-MID-INDATA-TILL-MOD SECTION.                                          
074000* * * * * FÖR VARJE MID-FÄLT                                              
074100* * * * * OM MID-FÄLT NOT = ALL '+' FLYTTA MID-FÄLT TILL MOD-INDAT        
074200* * * * * ANNARS FLYTTA RENSA-FÄLT TILL MOD-INDATA-FÄLT                   
074300                                                                          
074400     MOVE 1                  TO RAD-IX                                    
074500     PERFORM UNTIL RAD-IX > RAD-MAX                                       
074600                                                                          
074700       IF MID-CMD (RAD-IX) = ALL '+'                                      
074800         MOVE MFS-RENSA-FAELT                                             
074900                             TO MOD-CMD (RAD-IX)                          
075000       ELSE                                                               
075100         MOVE MID-CMD (RAD-IX)                                            
075200                             TO MOD-CMD (RAD-IX)                          
075300       END-IF                                                             
075400                                                                          
075500       IF MID-IDARTNR (RAD-IX) = ALL '+'                                  
075600         MOVE MFS-RENSA-FAELT                                             
075700                             TO MOD-IDARTNR (RAD-IX)                      
075800       ELSE                                                               
075900           MOVE MID-IDARTNR (RAD-IX)                                      
076000                             TO MOD-IDARTNR (RAD-IX)                      
076100       END-IF                                                             
076200                                                                          
076300       IF MID-KVKAMP-LAUNCH (RAD-IX) = ALL '+'                            
076400         MOVE MFS-RENSA-FAELT                                             
076500                             TO MOD-KVKAMP-LAUNCH (RAD-IX)                
076600       ELSE                                                               
076700         MOVE MID-KVKAMP-LAUNCH (RAD-IX)                                  
076800                             TO MOD-KVKAMP-LAUNCH (RAD-IX)                
076900       END-IF                                                             
077000                                                                          
077100       IF MID-KVKAMP-FIRST (RAD-IX) = ALL '+'                             
077200         MOVE MFS-RENSA-FAELT                                             
077300                             TO MOD-KVKAMP-FIRST (RAD-IX)                 
077400       ELSE                                                               
077500         MOVE MID-KVKAMP-FIRST (RAD-IX)                                   
077600                             TO MOD-KVKAMP-FIRST (RAD-IX)                 
077700       END-IF                                                             
077800                                                                          
077900       IF MID-KVKAMP-TOTAL (RAD-IX) = ALL '+'                             
078000         MOVE MFS-RENSA-FAELT                                             
078100                             TO MOD-KVKAMP-TOTAL (RAD-IX)                 
078200       ELSE                                                               
078300         MOVE MID-KVKAMP-TOTAL (RAD-IX)                                   
078400                             TO MOD-KVKAMP-TOTAL (RAD-IX)                 
078500       END-IF                                                             
078600                                                                          
078700       IF MID-FLKVKAMP-TOTAL (RAD-IX) = ALL '+'                           
078800         MOVE MFS-RENSA-FAELT                                             
078900                             TO MOD-FLKVKAMP-TOTAL (RAD-IX)               
079000       ELSE                                                               
079100         MOVE MID-FLKVKAMP-TOTAL (RAD-IX)                                 
079200                             TO MOD-FLKVKAMP-TOTAL (RAD-IX)               
079300       END-IF                                                             
079400                                                                          
079500       IF MID-RERESPRT (RAD-IX) = ALL '+'                                 
079600         MOVE MFS-RENSA-FAELT                                             
079700                             TO MOD-RERESPRT (RAD-IX)                     
079800       ELSE                                                               
079900         MOVE MID-RERESPRT (RAD-IX)                                       
080000                             TO MOD-RERESPRT (RAD-IX)                     
080100       END-IF                                                             
080200                                                                          
080300       ADD 1                 TO RAD-IX                                    
080400     END-PERFORM                                                          
080500                                                                          
080600     IF MID-NY-IDARTNR = ALL '+'                                          
080700       MOVE MFS-RENSA-FAELT  TO MOD-NY-IDARTNR                            
080800     ELSE                                                                 
080900       MOVE MID-NY-IDARTNR   TO MOD-NY-IDARTNR                            
081000       INSPECT MOD-NY-IDARTNR REPLACING LEADING ZERO BY SPACE             
081100     END-IF                                                               
081200                                                                          
081300     IF MID-NY-KVKAMP-LAUNCH = ALL '+'                                    
081400       MOVE MFS-RENSA-FAELT TO MOD-NY-KVKAMP-LAUNCH                       
081500     ELSE                                                                 
081600       MOVE MID-NY-KVKAMP-LAUNCH                                          
081700                            TO MOD-NY-KVKAMP-LAUNCH                       
081800       INSPECT MOD-NY-KVKAMP-LAUNCH                                       
081900                               REPLACING LEADING ZERO BY SPACE            
082000     END-IF                                                               
082100                                                                          
082200     IF MID-NY-KVKAMP-FIRST = ALL '+'                                     
082300       MOVE MFS-RENSA-FAELT TO MOD-NY-KVKAMP-FIRST                        
082400     ELSE                                                                 
082500       MOVE MID-NY-KVKAMP-FIRST                                           
082600                            TO MOD-NY-KVKAMP-FIRST                        
082700       INSPECT MOD-NY-KVKAMP-FIRST                                        
082800                               REPLACING LEADING ZERO BY SPACE            
082900     END-IF                                                               
083000                                                                          
083100     IF MID-NY-KVKAMP-TOTAL = ALL '+'                                     
083200       MOVE MFS-RENSA-FAELT TO MOD-NY-KVKAMP-TOTAL                        
083300     ELSE                                                                 
083400       MOVE MID-NY-KVKAMP-TOTAL                                           
083500                            TO MOD-NY-KVKAMP-TOTAL                        
083600       INSPECT MOD-NY-KVKAMP-TOTAL                                        
083700                               REPLACING LEADING ZERO BY SPACE            
083800     END-IF                                                               
083900     .                                                                    
084000     EJECT                                                                
084100 F-LAES-VISA-INFO SECTION.                                                
084200                                                                          
084300***********************************************                           
084400*                                                                         
084500*      KAMPANJ INGÅR I KAMPANJGRUPP                                       
084600*                                                                         
084700***********************************************                           
084800                                                                          
084900     MOVE WS-IDKAMP-GRP      TO W-IDKAMP-GRP                              
085000     PERFORM DB2-DCL-OPN-CRS-TP1KAMP                                      
085100     IF SQLCODE = ZERO                                                    
085200       PERFORM DB2-SELECT-TP1GRP                                          
085300     END-IF                                                               
085400                                                                          
085500     IF SQLCODE > ZERO                                                    
085600       MOVE INF-URVAL-SAKNAS                                              
085700                             TO MED-IDMFSFEL                              
085800       CALL WMEDKONV USING MED-WMEDAREA                                   
085900       MOVE MED-TEMFSFEL TO MOD-TEMFSFEL                                  
086000                                                                          
086100     ELSE                                                                 
086200                                                                          
086300       MOVE 1                TO IX                                        
086400       MOVE ZERO             TO IX-SISTA-POST                             
086500       PERFORM DB2-FETCH-TP1KAMP                                          
086600       IF SQLCODE > ZERO                                                  
086700         MOVE INF-URVAL-SAKNAS                                            
086800                           TO MED-IDMFSFEL                                
086900         CALL WMEDKONV USING MED-WMEDAREA                                 
087000         MOVE MED-TEMFSFEL TO MOD-TEMFSFEL                                
087100                                                                          
087200       ELSE                                                               
087300         PERFORM UNTIL SQLCODE > ZERO                                     
087400         OR IX > WS-TABELL-MAX                                            
087500            MOVE TP1KAMP-IDKAMP                                           
087600                             TO WS-IDKAMP-SPARA                           
087700                                W-IDKAMP                                  
087800            MOVE TP1KAMP-KVKAMP-CARS                                      
087900                             TO WS-KVKAMP-CARS-SPARA                      
088000            MOVE ZERO        TO W-IDARTNR                                 
088100                                                                          
088200            PERFORM DB2-DCL-OPN-CRS-TP1ARTK                               
088300            IF SQLCODE = ZERO                                             
088400                                                                          
088500              PERFORM DB2-FETCH-TP1ARTK                                   
088600              PERFORM UNTIL SQLCODE > ZERO                                
088700              OR IX > WS-TABELL-MAX                                       
088800                                                                          
088900                MOVE TP1KAMP-IDKAMP-GRP                                   
089000                             TO W-IDKAMP-GRP                              
089100                MOVE TP1ARTK-IDARTNR                                      
089200                             TO W-IDARTNR                                 
089300                PERFORM DB2-SELECT-TP1ARTG                                
089400                IF SQLCODE > ZERO                                         
089500                  MOVE ZERO  TO TP1ARTG-IDKAMP-GRP                        
089600                                TP1ARTG-IDARTNR                           
089700                                TP1ARTG-KVKAMP-LAUNCH                     
089800                                TP1ARTG-KVKAMP-FIRST                      
089900                                TP1ARTG-KVKAMP-TOTAL                      
090000                                TP1ARTG-RERESPRT                          
090100                  MOVE SPACE TO TP1ARTG-FLKVKAMP-TOTAL                    
090200                  IF TP1ARTK-RERESPRT > ZERO                              
090300                    IF TP1KAMP-RERESPRT > ZERO                            
090400                      COMPUTE WS-RERESPRT =                               
090500                              TP1ARTK-RERESPRT * TP1KAMP-RERESPRT         
090600                    ELSE                                                  
090700                      MOVE ZERO TO WS-RERESPRT                            
090800                    END-IF                                                
090900                  ELSE                                                    
091000                    MOVE ZERO TO WS-RERESPRT                              
091100                  END-IF                                                  
091200                ELSE                                                      
091300                  IF TP1ARTG-RERESPRT > ZERO                              
091400                    IF TP1GRP-RERESPRT > ZERO                             
091500                      COMPUTE WS-RERESPRT =                               
091600                              TP1ARTG-RERESPRT * TP1GRP-RERESPRT          
091700                    ELSE                                                  
091800                      MOVE ZERO TO WS-RERESPRT                            
091900                    END-IF                                                
092000                  ELSE                                                    
092100                    MOVE ZERO TO WS-RERESPRT                              
092200                  END-IF                                                  
092300                END-IF                                                    
092400                                                                          
092500                MOVE 1       TO IX                                        
092600                PERFORM UNTIL IX > WS-TABELL-MAX                          
092700                OR IX > IX-SISTA-POST                                     
092800                OR TP1ARTK-IDARTNR = WS-IDARTNR-TAB (IX)                  
092900                 ADD 1       TO IX                                        
093000                END-PERFORM                                               
093100                IF IX > WS-TABELL-MAX                                     
093200                   CONTINUE                                               
093300                ELSE                                                      
093400                  IF WS-IDARTNR-TAB (IX) = ZERO                           
093500                    MOVE TP1ARTK-IDKAMP                                   
093600                             TO WS-IDKAMP-TAB (IX)                        
093700                    MOVE TP1ARTK-IDARTNR                                  
093800                             TO WS-IDARTNR-TAB (IX)                       
093900                                WS-IDARTNR-TAB-SORT (IX)                  
094000                    MOVE TP1ARTK-KVREPANT                                 
094100                             TO WS-KVREPANT-TAB (IX)                      
094200                    MOVE TP1KAMP-KDKAMP                                   
094300                             TO WS-KDKAMP-TAB (IX)                        
094400                    IF TP1KAMP-KDKAMP = 'W'                               
094500                      IF TP1ARTG-FLKVKAMP-TOTAL = YES                     
094600                        MOVE TP1ARTG-FLKVKAMP-TOTAL                       
094700                             TO WS-FLKVKAMP-TOTAL-TAB (IX)                
094800                      ELSE                                                
094900                        MOVE NEJ TO WS-FLKVKAMP-TOTAL-TAB (IX)            
095000                      END-IF                                              
095100                      IF TP1ARTG-FLKVKAMP-TOTAL = YES                     
095200                        MOVE TP1ARTG-KVKAMP-TOTAL                         
095300                             TO WS-KVKAMP-TOTAL-TAB (IX)                  
095400                      ELSE                                                
095500                        COMPUTE WS-TOTAL ROUNDED =                        
095600                                           TP1KAMP-KVKAMP-CARS            
095700                                         * TP1ARTK-KVREPANT               
095800                                         * WS-RERESPRT                    
095900                        MOVE WS-TOTAL                                     
096000                               TO WS-KVKAMP-TOTAL-TAB (IX)                
096100                      END-IF                                              
096200                    ELSE                                                  
096300                      MOVE TP1ARTK-KVKAMP-TOTAL                           
096400                             TO WS-KVKAMP-TOTAL-TAB (IX)                  
096500                    END-IF                                                
096600                    IF TP1ARTG-KVKAMP-LAUNCH > ZERO                       
096700                      MOVE TP1ARTG-KVKAMP-LAUNCH                          
096800                             TO WS-KVKAMP-LAUNCH-TAB (IX)                 
096900                    ELSE                                                  
097000                      MOVE TP1ARTK-KVKAMP-LAUNCH                          
097100                             TO WS-KVKAMP-LAUNCH-TAB (IX)                 
097200                    END-IF                                                
097300                    IF TP1ARTG-KVKAMP-FIRST > ZERO                        
097400                      MOVE TP1ARTG-KVKAMP-FIRST                           
097500                             TO WS-KVKAMP-FIRST-TAB (IX)                  
097600                    ELSE                                                  
097700                      MOVE TP1ARTK-KVKAMP-FIRST                           
097800                             TO WS-KVKAMP-FIRST-TAB (IX)                  
097900                    END-IF                                                
098000                    IF TP1ARTG-RERESPRT > ZERO                            
098100                      MOVE TP1ARTG-RERESPRT                               
098200                               TO WS-RERESPRT-TAB (IX)                    
098300                    ELSE                                                  
098400                      MOVE TP1ARTK-RERESPRT                               
098500                               TO WS-RERESPRT-TAB (IX)                    
098600                    END-IF                                                
098700                    MOVE IX  TO IX-SISTA-POST                             
098800                  ELSE                                                    
098900                    IF TP1ARTK-IDARTNR = WS-IDARTNR-TAB (IX)              
099000                      ADD TP1ARTK-KVREPANT                                
099100                             TO WS-KVREPANT-TAB (IX)                      
099200                      IF TP1KAMP-KDKAMP = 'W'                             
099300                        IF TP1ARTG-FLKVKAMP-TOTAL = YES                   
099400                          MOVE TP1ARTG-FLKVKAMP-TOTAL                     
099500                               TO WS-FLKVKAMP-TOTAL-TAB (IX)              
099600                        ELSE                                              
099700                          MOVE NEJ TO WS-FLKVKAMP-TOTAL-TAB (IX)          
099800                        END-IF                                            
099900                        IF TP1ARTG-FLKVKAMP-TOTAL = YES                   
100000                          MOVE TP1ARTG-KVKAMP-TOTAL                       
100100                               TO WS-KVKAMP-TOTAL-TAB (IX)                
100200                        ELSE                                              
100300                          COMPUTE WS-TOTAL ROUNDED =                      
100400                                             TP1KAMP-KVKAMP-CARS          
100500                                           * TP1ARTK-KVREPANT             
100600                                           * WS-RERESPRT                  
100700                          ADD WS-TOTAL                                    
100800                               TO WS-KVKAMP-TOTAL-TAB (IX)                
100900                        END-IF                                            
101000                      ELSE                                                
101100                        ADD TP1ARTK-KVKAMP-TOTAL                          
101200                             TO WS-KVKAMP-TOTAL-TAB (IX)                  
101300                      END-IF                                              
101400                        ADD TP1ARTK-KVKAMP-LAUNCH                         
101500                             TO WS-KVKAMP-LAUNCH-TAB (IX)                 
101600                        ADD TP1ARTK-KVKAMP-FIRST                          
101700                             TO WS-KVKAMP-FIRST-TAB (IX)                  
101800                      END-IF                                              
101900                  END-IF                                                  
102000                                                                          
102100                END-IF                                                    
102200                PERFORM DB2-FETCH-TP1ARTK                                 
102300              END-PERFORM                                                 
102400                                                                          
102500              PERFORM DB2-CLOSE-TP1ARTK-CRS                               
102600                                                                          
102700            END-IF                                                        
102800                                                                          
102900           PERFORM DB2-FETCH-TP1KAMP                                      
103000         END-PERFORM                                                      
103100                                                                          
103200         PERFORM FB-SORTERA-PLATSER                                       
103300                                                                          
103400                                                                          
103500***    HÄR LÄSES ETT STARTVÄRDE FÖR IX-RAD FRÅN USER-BASEN                
103600***    SÅ ATT VID BLÄDDRING, START SKER MED RÄTT RAD                      
103700         MOVE 1              TO IX                                        
103800         IF WS-MSGI-SSA-KEY-ENTER NOT = SPACE                             
103900            MOVE WS-ENTER-RADNR                                           
104000                             TO IX                                        
104100         ELSE                                                             
104200           IF WS-MSGI-SSA-KEY-NEXT NOT = SPACE                            
104300              MOVE WS-NEXT-RADNR                                          
104400                             TO IX                                        
104500           END-IF                                                         
104600         END-IF                                                           
104700                                                                          
104800         MOVE 1              TO RAD-IX                                    
104900         PERFORM UNTIL RAD-IX > RAD-MAX                                   
105000         OR IX > IX-SISTA-POST                                            
105100           MOVE WS-IDARTNR-TAB (IX)                                       
105200                             TO W-IDARTNR                                 
105300           PERFORM FA-LAES-GRUNDDATA                                      
105400*                                                                         
105500*      ARTIKELN SKA INTE VISAS :                                          
105600*              - OM DET ÄR PRODUKTSLAG 18 58                              
105700*              - DEFINITIVT ERSATT (KDERS > 19) /I NORMALFALLET/          
105800*                MAN SER DEF. ERSATTA OM MAN FYLLER I                     
105900*                VISA-DEF-ERS = JA SOM NYCKEL I BILDEN                    
106000*                                                                         
106100*      (ARTIKELN VISAS DÄREMOT OM DEN INTE ÄR UPPLAGD I PULS)             
106200*                                                                         
106300                                                                          
106400           IF SEGMENT-FINNS                                               
106500           AND (((WS-VISA-DEF-ERS = NEJ                                   
106600           OR   WS-VISA-DEF-ERS = SPACE)                                  
106700           AND  CLAG-KDERS > 19)                                          
106800           OR   KDPRODSL-TOOLS)                                           
106900             CONTINUE                                                     
107000           ELSE                                                           
107100             MOVE WS-IDARTNR-TAB (IX)                                     
107200                             TO MOD-IDARTNR (RAD-IX)                      
107300             IF RAD-IX = 1                                                
107400               MOVE IX       TO WS-ENTER-RADNR                            
107500               MOVE WS-KDKAMP-TAB (IX)                                    
107600                             TO MOD-KDKAMP                                
107700             END-IF                                                       
107800             IF SEGMENT-FINNS                                             
107900               MOVE CLAG-IDANSK                                           
108000                             TO WS-IDANSK-NUM                             
108100               MOVE WS-IDANSK-NUM                                         
108200                             TO WS-IDANSK-RED                             
108300               MOVE WS-IDANSK-RED                                         
108400                             TO MOD-IDANSK (RAD-IX)                       
108500                                                                          
108600               MOVE CLAG-KDERS TO WS-KDERS-NUM                            
108700               MOVE WS-KDERS-NUM                                          
108800                             TO WS-KDERS-RED                              
108900               MOVE WS-KDERS-RED                                          
109000                             TO MOD-KDERS (RAD-IX)                        
109100             END-IF                                                       
109200             MOVE WS-KVREPANT-TAB (IX)                                    
109300                             TO WS-KVREPANT-RED                           
109400             MOVE WS-KVREPANT-RED                                         
109500                             TO MOD-KVREPANT (RAD-IX)                     
109600             MOVE WS-KVKAMP-LAUNCH-TAB (IX)                               
109700                             TO MOD-KVKAMP-LAUNCH (RAD-IX)                
109800             MOVE WS-KVKAMP-FIRST-TAB (IX)                                
109900                             TO MOD-KVKAMP-FIRST (RAD-IX)                 
110000             MOVE WS-KVKAMP-TOTAL-TAB (IX)                                
110100                             TO MOD-KVKAMP-TOTAL (RAD-IX)                 
110200             MOVE WS-FLKVKAMP-TOTAL-TAB (IX)                              
110300                             TO MOD-FLKVKAMP-TOTAL (RAD-IX)               
110400             MOVE MFS-ADD-LAES-IN-FAELT                                   
110500                             TO MOD-FLKVKAMP-TOTAL-ATTR (RAD-IX)          
110600                                                                          
110700             IF WS-KDKAMP-TAB (IX) = 'W'                                  
110800               IF WS-VISA-UNIK = JA                                       
110900               OR WS-VISA-UNIK = YES                                      
111000                 MOVE MFS-STAENG-FAELT                                    
111100                               TO MOD-KVKAMP-TOTAL-ATTR (RAD-IX)          
111200                 MOVE SPACE    TO MOD-FLKVKAMP-TOTAL (RAD-IX)             
111300                 MOVE MFS-STAENG-FAELT                                    
111400                               TO MOD-FLKVKAMP-TOTAL-ATTR (RAD-IX)        
111500               END-IF                                                     
111600             END-IF                                                       
111700             COMPUTE WS-RERESPRT-NUM =                                    
111800                             WS-RERESPRT-TAB (IX) * 100                   
111900             MOVE WS-RERESPRT-NUM                                         
112000                             TO WS-RERESPRT-RED                           
112100             MOVE WS-RERESPRT-RED                                         
112200                             TO MOD-RERESPRT (RAD-IX)                     
112300             IF ARTIKEL-FINNS-EJ-I-PULS                                   
112400               MOVE 'MISSING IN PULS'                                     
112500                             TO MOD-KOMMENTAR (RAD-IX)                    
112600             ELSE                                                         
112700               PERFORM IMS-GU-D701                                        
112800               IF SEGMENT-FINNS                                           
112900                  PERFORM IMS-GNP-D702                                    
113000                  IF SEGMENT-FINNS                                        
113100                     MOVE WDD702-IDARTNR-TILLK                            
113200                             TO WS-IDARTNR-NUM                            
113300                     MOVE WS-IDARTNR-NUM                                  
113400                             TO WS-IDARTNR                                
113500                     INSPECT WS-IDARTNR                                   
113600                               REPLACING LEADING ZERO BY SPACE            
113700                     MOVE WS-IDARTNR                                      
113800                             TO MOD-KOMMENTAR (RAD-IX)                    
113900                     PERFORM IMS-GNP-D702                                 
114000                     IF SEGMENT-FINNS                                     
114100                       MOVE 'VARIOUS  '                                   
114200                             TO MOD-KOMMENTAR (RAD-IX)                    
114300                     END-IF                                               
114400                  END-IF                                                  
114500                  IF CLAG-KDERS > 19                                      
114600                    PERFORM IMS-GNP-D704                                  
114700                    IF SEGMENT-FINNS                                      
114800                      MOVE WDD704-TIERSDAT-REG                            
114900                             TO WS-AAVVD-NUM                              
115000                      MOVE WS-AAVVD-NUM                                   
115100                             TO MOD-KOMMENTAR (RAD-IX) (11:5)             
115200                    END-IF                                                
115300                  END-IF                                                  
115400               END-IF                                                     
115500             END-IF                                                       
115600                                                                          
115700             ADD 1           TO RAD-IX                                    
115800           END-IF                                                         
115900           ADD 1             TO IX                                        
116000         END-PERFORM                                                      
116100                                                                          
116200***    OM FLER RADER FINNS, SÅ SPARAS I USERBASEN NÄSTA RADNR             
116300***    FRÅN TABELLEN , ANNARS BLANKAS                                     
116400         IF IX           <= ANTAL                                         
116500            MOVE IX          TO WS-NEXT-RADNR                             
116600            MOVE INF-MORE-INFO-EXISTS                                     
116700                             TO MED-IDMFSFEL                              
116800            CALL WMEDKONV USING MED-WMEDAREA                              
116900            MOVE MED-TEMFSFEL TO MOD-TEMFSFEL                             
117000         ELSE                                                             
117100            MOVE SPACE       TO WS-MSGI-SSA-KEY-NEXT                      
117200         END-IF                                                           
117300                                                                          
117400         PERFORM UNTIL RAD-IX > RAD-MAX                                   
117500           MOVE MFS-STAENG-FAELT                                          
117600                             TO MOD-CMD-ATTR           (RAD-IX)           
117700                                MOD-KVKAMP-LAUNCH-ATTR (RAD-IX)           
117800                                MOD-KVKAMP-FIRST-ATTR  (RAD-IX)           
117900                                MOD-KVKAMP-TOTAL-ATTR  (RAD-IX)           
118000                                MOD-FLKVKAMP-TOTAL-ATTR  (RAD-IX)         
118100                                MOD-RERESPRT-ATTR      (RAD-IX)           
118200           MOVE MFS-RENSA-FAELT                                           
118300                             TO MOD-CMD           (RAD-IX)                
118400                                MOD-IDARTNR       (RAD-IX)                
118500                                MOD-KVKAMP-LAUNCH (RAD-IX)                
118600                                MOD-KVKAMP-FIRST  (RAD-IX)                
118700                                MOD-KVKAMP-TOTAL  (RAD-IX)                
118800                                MOD-FLKVKAMP-TOTAL  (RAD-IX)              
118900                                MOD-RERESPRT      (RAD-IX)                
119000                                                                          
119100           ADD 1             TO RAD-IX                                    
119200         END-PERFORM                                                      
119300       END-IF                                                             
119400                                                                          
119500     END-IF                                                               
119600                                                                          
119700     PERFORM DB2-CLOSE-TP1KAMP-CRS                                        
119800     .                                                                    
119900     EJECT                                                                
120000 FA-LAES-GRUNDDATA SECTION.                                               
120100                                                                          
120200     PERFORM IMS-GU-K601                                                  
120300     IF SEGMENT-FINNS                                                     
120310       MOVE ART-KDPRODSL     TO TEST-KDPRODSL                             
120400       MOVE JA               TO ARTIKEL-FINNS-I-PULS-SW                   
120500       PERFORM IMS-GU-K611                                                
120600       IF SEGMENT-SAKNAS                                                  
120700         MOVE NEJ            TO ARTIKEL-FINNS-I-PULS-SW                   
120800       END-IF                                                             
120900     ELSE                                                                 
121000       MOVE NEJ              TO ARTIKEL-FINNS-I-PULS-SW                   
121100     END-IF                                                               
121200     .                                                                    
121300     EJECT                                                                
121400 FB-SORTERA-PLATSER SECTION.                                              
121500                                                                          
121600     MOVE IX-SISTA-POST      TO ANTAL                                     
121700                                                                          
121800     CALL WINTSOR USING WS-TABELL STEGLANGD ANTAL                         
121900                  WS-TAB-SORT (1) NYCKELLANGD                             
122000     .                                                                    
122100     EJECT                                                                
122200 G-KOLLA-INPUT SECTION.                                                   
122300     MOVE 'G-KOLLA-INPUT      ' TO WS-SECTION                             
122400                                                                          
122500     MOVE JA                 TO INDATA-SW                                 
122600     MOVE NEJ                TO UPD-RAD-SW                                
122700                                REG-NY-RAD-SW                             
122800                                                                          
122900     IF  MID-INPUT      = ALL '+'                                         
123000        MOVE ERR-PF11-AND-NO-DATA TO MED-IDMFSFEL                         
123100        MOVE NEJ TO INDATA-SW                                             
123200     ELSE                                                                 
123300                                                                          
123400       IF WS-IDKAMP-GRP > ZERO                                            
123500                                                                          
123600         MOVE WS-IDKAMP-GRP  TO W-IDKAMP-GRP                              
123700         PERFORM DB2-SELECT-TP1GRP                                        
123800                                                                          
123900         IF SQLCODE > ZERO                                                
124000            MOVE NEJ         TO INDATA-SW                                 
124100         END-IF                                                           
124200                                                                          
124300       ELSE                                                               
124400                                                                          
124500         MOVE WS-IDKAMP      TO W-IDKAMP                                  
124600         PERFORM DB2-SELECT-TP1KAMP                                       
124700                                                                          
124800         IF SQLCODE > ZERO                                                
124900            MOVE NEJ         TO INDATA-SW                                 
125000         END-IF                                                           
125100       END-IF                                                             
125200                                                                          
125300       IF INDATA-OK                                                       
125400         MOVE +1             TO RAD-IX                                    
125500         PERFORM UNTIL RAD-IX > RAD-MAX                                   
125600                                                                          
125700           IF MID-CMD           (RAD-IX) NOT = ALL '+'                    
125800           OR MID-KVKAMP-LAUNCH (RAD-IX) NOT = ALL '+'                    
125900           OR MID-KVKAMP-FIRST  (RAD-IX) NOT = ALL '+'                    
126000           OR MID-KVKAMP-TOTAL  (RAD-IX) NOT = ALL '+'                    
126100           OR MID-FLKVKAMP-TOTAL  (RAD-IX) NOT = ALL '+'                  
126200           OR MID-RERESPRT      (RAD-IX) NOT = ALL '+'                    
126300             MOVE JA         TO UPD-RAD-SW                                
126400                                                                          
126500             IF MID-CMD (RAD-IX) NOT = ALL '+'                            
126600                                                                          
126700                IF  (MID-CMD (RAD-IX) = 'B'                               
126800                OR   MID-CMD (RAD-IX) = 'D')                              
126900                AND (WS-IDKAMP NOT = SPACE                                
127000                AND (TP1KAMP-KDKAMP = 'Q'                                 
127100                OR   TP1KAMP-KDKAMP = 'S'))                               
127200                                                                          
127300                  MOVE JA    TO UPD-RAD-SW                                
127400                  MOVE MFS-ALFA-FAELT-RAETT                               
127500                             TO MOD-CMD-ATTR(RAD-IX)                      
127600                ELSE                                                      
127700                    MOVE MFS-ALFA-FAELT-FEL                               
127800                             TO MOD-CMD-ATTR (RAD-IX)                     
127900                    MOVE ERR-CORR-HILITE-FLDS                             
128000                             TO MED-IDMFSFEL                              
128100                    MOVE NEJ TO INDATA-SW                                 
128200                END-IF                                                    
128300             ELSE                                                         
128400                MOVE MFS-RENSA-FAELT                                      
128500                             TO MOD-CMD (RAD-IX)                          
128600                                                                          
128700                IF MID-KVKAMP-LAUNCH (RAD-IX) = ALL '+'                   
128800                  MOVE MFS-RENSA-FAELT                                    
128900                             TO MOD-KVKAMP-LAUNCH (RAD-IX)                
129000                ELSE                                                      
129100                                                                          
129200                  INSPECT MID-KVKAMP-LAUNCH (RAD-IX)                      
129300                              REPLACING LEADING SPACE BY ZERO             
129400*                                                                         
129500                  MOVE MFS-NUM-FAELT-RAETT                                
129600                             TO MOD-KVKAMP-LAUNCH-ATTR (RAD-IX)           
129700                                                                          
129800                  IF  MID-KVKAMP-LAUNCH (RAD-IX) NOT NUMERIC              
129900                  OR ((TP1KAMP-KDKAMP = 'W'                               
130000                  AND  WS-IDKAMP  NOT = SPACE                             
130100                  AND (WS-VISA-UNIK   = JA                                
130200                  OR   WS-VISA-UNIK   = YES))                             
130300                  AND  MID-KVKAMP-LAUNCH (RAD-IX) > ZERO)                 
130400                    MOVE MFS-NUM-FAELT-FEL                                
130500                             TO MOD-KVKAMP-LAUNCH-ATTR (RAD-IX)           
130600                    MOVE ERR-CORR-HILITE-FLDS                             
130700                             TO MED-IDMFSFEL                              
130800                    MOVE NEJ TO INDATA-SW                                 
130900                  END-IF                                                  
131000                END-IF                                                    
131100                                                                          
131200                IF MID-KVKAMP-FIRST (RAD-IX) = ALL '+'                    
131300                  MOVE MFS-RENSA-FAELT                                    
131400                             TO MOD-KVKAMP-FIRST (RAD-IX)                 
131500                ELSE                                                      
131600                  INSPECT MID-KVKAMP-FIRST (RAD-IX)                       
131700                              REPLACING LEADING SPACE BY ZERO             
131800                                                                          
131900                  MOVE MFS-NUM-FAELT-RAETT                                
132000                             TO MOD-KVKAMP-FIRST-ATTR (RAD-IX)            
132100                  IF  MID-KVKAMP-FIRST (RAD-IX) NOT NUMERIC               
132200                  OR ((TP1KAMP-KDKAMP = 'W'                               
132300                  AND  WS-IDKAMP  NOT = SPACE                             
132400                  AND (WS-VISA-UNIK   = JA                                
132500                  OR   WS-VISA-UNIK   = YES))                             
132600                  AND  MID-KVKAMP-FIRST (RAD-IX) > ZERO)                  
132700                    MOVE MFS-NUM-FAELT-FEL                                
132800                             TO MOD-KVKAMP-FIRST-ATTR (RAD-IX)            
132900                    MOVE ERR-CORR-HILITE-FLDS                             
133000                             TO MED-IDMFSFEL                              
133100                    MOVE NEJ TO INDATA-SW                                 
133200                  END-IF                                                  
133300                END-IF                                                    
133400                                                                          
133500                IF MID-KVKAMP-TOTAL (RAD-IX) = ALL '+'                    
133600                  MOVE MFS-RENSA-FAELT                                    
133700                             TO MOD-KVKAMP-TOTAL (RAD-IX)                 
133800                ELSE                                                      
133900                  INSPECT MID-KVKAMP-TOTAL (RAD-IX)                       
134000                              REPLACING LEADING SPACE BY ZERO             
134100                  IF MID-KVKAMP-TOTAL (RAD-IX) NUMERIC                    
134200                    MOVE MFS-NUM-FAELT-RAETT                              
134300                             TO MOD-KVKAMP-TOTAL-ATTR (RAD-IX)            
134400                  ELSE                                                    
134500                    MOVE MFS-NUM-FAELT-FEL                                
134600                             TO MOD-KVKAMP-TOTAL-ATTR (RAD-IX)            
134700                    MOVE ERR-CORR-HILITE-FLDS                             
134800                             TO MED-IDMFSFEL                              
134900                    MOVE NEJ TO INDATA-SW                                 
135000                  END-IF                                                  
135100                END-IF                                                    
135200                                                                          
135300                IF MID-FLKVKAMP-TOTAL (RAD-IX) = ALL '+'                  
135400                  MOVE MFS-RENSA-FAELT                                    
135500                             TO MOD-FLKVKAMP-TOTAL (RAD-IX)               
135600                ELSE                                                      
135700                  IF MID-FLKVKAMP-TOTAL (RAD-IX) = YES OR NEJ             
135800                    MOVE MFS-ALFA-FAELT-RAETT                             
135900                             TO MOD-FLKVKAMP-TOTAL-ATTR (RAD-IX)          
136000                  ELSE                                                    
136100                    MOVE MFS-ALFA-FAELT-FEL                               
136200                             TO MOD-FLKVKAMP-TOTAL-ATTR (RAD-IX)          
136300                    MOVE ERR-CORR-HILITE-FLDS                             
136400                             TO MED-IDMFSFEL                              
136500                    MOVE NEJ TO INDATA-SW                                 
136600                  END-IF                                                  
136700                END-IF                                                    
136800                                                                          
136900                IF MID-RERESPRT (RAD-IX) = ALL '+'                        
137000                  MOVE MFS-RENSA-FAELT                                    
137100                             TO MOD-RERESPRT (RAD-IX)                     
137200                ELSE                                                      
137300                  INSPECT MID-RERESPRT (RAD-IX)                           
137400                              REPLACING LEADING SPACE BY ZERO             
137500                  MOVE MFS-NUM-FAELT-RAETT                                
137600                             TO MOD-RERESPRT-ATTR (RAD-IX)                
137700                                                                          
137800                  IF  MID-RERESPRT (RAD-IX) NOT NUMERIC                   
137900                  OR ((TP1KAMP-KDKAMP = 'W'                               
138000                  AND  WS-IDKAMP  NOT = SPACE                             
138100                  AND (WS-VISA-UNIK  = JA                                 
138200                  OR   WS-VISA-UNIK  = YES))                              
138300                  AND  MID-RERESPRT (RAD-IX) > ZERO)                      
138400                    MOVE MFS-NUM-FAELT-FEL                                
138500                             TO MOD-RERESPRT-ATTR (RAD-IX)                
138600                    MOVE ERR-CORR-HILITE-FLDS                             
138700                             TO MED-IDMFSFEL                              
138800                    MOVE NEJ TO INDATA-SW                                 
138900                  END-IF                                                  
139000                END-IF                                                    
139100                                                                          
139200             END-IF                                                       
139300           END-IF                                                         
139400                                                                          
139500           ADD 1 TO RAD-IX                                                
139600         END-PERFORM                                                      
139700       END-IF                                                             
139800                                                                          
139900       IF MID-REG-NY-RAD NOT = ALL '+'                                    
140000         MOVE WS-IDKAMP      TO W-IDKAMP                                  
140100         PERFORM DB2-SELECT-TP1KAMP                                       
140200         IF   SQLCODE = ZERO                                              
140300         AND (TP1KAMP-KDKAMP = 'Q'                                        
140400         OR   TP1KAMP-KDKAMP = 'S')                                       
140500           MOVE JA           TO REG-NY-RAD-SW                             
140600           MOVE ZERO         TO TEST-KDPRODSL                             
140700           INSPECT MID-NY-IDARTNR                                         
140800                             REPLACING LEADING SPACE BY ZERO              
140900           IF MID-NY-IDARTNR NUMERIC                                      
141000             MOVE MFS-NUM-FAELT-RAETT                                     
141100                             TO MOD-NY-IDARTNR-ATTR                       
141200             MOVE MID-NY-IDARTNR                                          
141300                             TO W-IDARTNR                                 
141400             PERFORM IMS-GU-K601                                          
141500             IF SEGMENT-FINNS                                             
141600               PERFORM IMS-GU-K611                                        
141700               MOVE ART-KDPRODSL TO TEST-KDPRODSL                         
141800             END-IF                                                       
141900             IF SEGMENT-FINNS                                             
142000             AND CLAG-KDERS < 19                                          
142100             AND NOT KDPRODSL-TOOLS                                       
142200               CONTINUE                                                   
142300             ELSE                                                         
142400               IF SEGMENT-FINNS                                           
142500                 IF CLAG-KDERS > 19                                       
142600                   MOVE MED-3                                             
142700                             TO WS-TEMFSFEL                               
142800                 ELSE                                                     
142900                   MOVE MED-5                                             
143000                             TO WS-TEMFSFEL                               
143100                 END-IF                                                   
143200               ELSE                                                       
143300                 MOVE MED-4  TO WS-TEMFSFEL                               
143400               END-IF                                                     
143500               MOVE MFS-NUM-FAELT-FEL                                     
143600                             TO MOD-NY-IDARTNR-ATTR                       
143700               MOVE NEJ      TO INDATA-SW                                 
143800             END-IF                                                       
143900                                                                          
144000             PERFORM DB2-SELECT-TP1ARTK                                   
144100                                                                          
144200             IF SQLCODE = ZERO                                            
144300               MOVE MFS-NUM-FAELT-FEL                                     
144400                             TO MOD-NY-IDARTNR-ATTR                       
144500               MOVE MED-1    TO WS-TEMFSFEL                               
144600               MOVE NEJ      TO INDATA-SW                                 
144700                                                                          
144800             END-IF                                                       
144900           ELSE                                                           
145000             MOVE MFS-NUM-FAELT-FEL                                       
145100                             TO MOD-NY-IDARTNR-ATTR                       
145200             MOVE ERR-CORR-HILITE-FLDS                                    
145300                             TO MED-IDMFSFEL                              
145400             MOVE NEJ        TO INDATA-SW                                 
145500           END-IF                                                         
145600                                                                          
145700           IF MID-NY-KVKAMP-LAUNCH = ALL '+'                              
145800             MOVE ZERO       TO MID-NY-KVKAMP-LAUNCH                      
145900           END-IF                                                         
146000                                                                          
146100           INSPECT MID-NY-KVKAMP-LAUNCH                                   
146200                             REPLACING LEADING SPACE BY ZERO              
146300           IF MID-NY-KVKAMP-LAUNCH NUMERIC                                
146400             MOVE MFS-NUM-FAELT-RAETT                                     
146500                             TO MOD-NY-KVKAMP-LAUNCH-ATTR                 
146600           ELSE                                                           
146700             MOVE MFS-NUM-FAELT-FEL                                       
146800                             TO MOD-NY-KVKAMP-LAUNCH-ATTR                 
146900             MOVE ERR-CORR-HILITE-FLDS                                    
147000                             TO MED-IDMFSFEL                              
147100             MOVE NEJ        TO INDATA-SW                                 
147200           END-IF                                                         
147300                                                                          
147400           IF MID-NY-KVKAMP-FIRST = ALL '+'                               
147500             MOVE ZERO       TO MID-NY-KVKAMP-FIRST                       
147600           END-IF                                                         
147700                                                                          
147800           INSPECT MID-NY-KVKAMP-FIRST                                    
147900                             REPLACING LEADING SPACE BY ZERO              
148000           IF MID-NY-KVKAMP-FIRST NUMERIC                                 
148100             MOVE MFS-NUM-FAELT-RAETT                                     
148200                             TO MOD-NY-KVKAMP-FIRST-ATTR                  
148300           ELSE                                                           
148400             MOVE MFS-NUM-FAELT-FEL                                       
148500                             TO MOD-NY-KVKAMP-FIRST-ATTR                  
148600             MOVE ERR-CORR-HILITE-FLDS                                    
148700                             TO MED-IDMFSFEL                              
148800             MOVE NEJ TO INDATA-SW                                        
148900           END-IF                                                         
149000                                                                          
149100           IF MID-NY-KVKAMP-TOTAL = ALL '+'                               
149200             MOVE ZERO       TO MID-NY-KVKAMP-TOTAL                       
149300           END-IF                                                         
149400                                                                          
149500           INSPECT MID-NY-KVKAMP-TOTAL                                    
149600                             REPLACING LEADING SPACE BY ZERO              
149700           IF MID-NY-KVKAMP-TOTAL NUMERIC                                 
149800             MOVE MFS-NUM-FAELT-RAETT                                     
149900                             TO MOD-NY-KVKAMP-TOTAL-ATTR                  
150000           ELSE                                                           
150100             MOVE MFS-NUM-FAELT-FEL                                       
150200                             TO MOD-NY-KVKAMP-TOTAL-ATTR                  
150300             MOVE ERR-CORR-HILITE-FLDS                                    
150400                             TO MED-IDMFSFEL                              
150500             MOVE NEJ TO INDATA-SW                                        
150600           END-IF                                                         
150700                                                                          
150800           IF TP1KAMP-KDKAMP = 'S'                                        
150900             IF MID-NY-KVKAMP-LAUNCH > ZERO                               
151000               MOVE MFS-NUM-FAELT-FEL                                     
151100                             TO MOD-NY-KVKAMP-LAUNCH-ATTR                 
151200               MOVE ERR-CORR-HILITE-FLDS                                  
151300                             TO MED-IDMFSFEL                              
151400               MOVE NEJ TO INDATA-SW                                      
151500             END-IF                                                       
151600             IF MID-NY-KVKAMP-FIRST > ZERO                                
151700               MOVE MFS-NUM-FAELT-FEL                                     
151800                             TO MOD-NY-KVKAMP-FIRST-ATTR                  
151900               MOVE ERR-CORR-HILITE-FLDS                                  
152000                             TO MED-IDMFSFEL                              
152100               MOVE NEJ TO INDATA-SW                                      
152200             END-IF                                                       
152300           END-IF                                                         
152400         ELSE                                                             
152500           MOVE MFS-NUM-FAELT-FEL                                         
152600                             TO MOD-NY-IDARTNR-ATTR                       
152700           MOVE ERR-CORR-HILITE-FLDS                                      
152800                             TO MED-IDMFSFEL                              
152900           MOVE NEJ TO INDATA-SW                                          
153000         END-IF                                                           
153100       ELSE                                                               
153200         IF UPD-RAD-NEJ                                                   
153300            MOVE ERR-PF11-AND-NO-DATA TO MED-IDMFSFEL                     
153400            MOVE NEJ         TO INDATA-SW                                 
153500         END-IF                                                           
153600       END-IF                                                             
153700     END-IF                                                               
153800                                                                          
153900     IF INDATA-FEL                                                        
154000        IF WS-TEMFSFEL = SPACE                                            
154100          CALL WMEDKONV USING MED-WMEDAREA                                
154200          MOVE MED-TEMFSFEL TO MOD-TEMFSFEL                               
154300        ELSE                                                              
154400          MOVE WS-TEMFSFEL  TO MOD-TEMFSFEL                               
154500        END-IF                                                            
154600        PERFORM MFS-ROER-EJ-FAELT-UT                                      
154700        PERFORM MFS-ROER-EJ-FAELT-IN                                      
154800     ELSE                                                                 
154900                                                                          
155000        CONTINUE                                                          
155100*                                                                         
155200*        IF UPD-RAD-JA                                                    
155300*        AND REG-NY-RAD-JA                                                
155400*****                                                                     
155500*****     INTE MÖJLIGT ATT UPPDATERA BÅDE ENSKILD RAD                     
155600*****     OCH REGISTRERA NY ARTIKEL TILL EN KAMPANJ                       
155700*****                                                                     
155800*            MOVE NEJ     TO INDATA-SW                                    
155900*            MOVE CONFLICT TO MED-IDMFSFEL                                
156000*            CALL WMEDKONV USING MED-WMEDAREA                             
156100*            MOVE MED-TEMFSFEL TO MOD-TEMFSFEL                            
156200*            PERFORM MFS-ROER-EJ-FAELT-IN                                 
156300*            PERFORM MFS-ROER-EJ-FAELT-UT                                 
156400*        END-IF                                                           
156500     END-IF                                                               
156600     .                                                                    
156700     EJECT                                                                
156800                                                                          
156900 H-UPPDATERA SECTION.                                                     
157000     MOVE 'H-UPPDATERA'      TO WS-SECTION                                
157100                                                                          
157200     IF UPD-RAD-JA                                                        
157300                                                                          
157400       MOVE +1 TO IX                                                      
157500       PERFORM UNTIL IX > RAD-MAX                                         
157600          IF MID-CMD            (IX) NOT = ALL '+'                        
157700          OR MID-KVKAMP-LAUNCH  (IX) NOT = ALL '+'                        
157800          OR MID-KVKAMP-FIRST   (IX) NOT = ALL '+'                        
157900          OR MID-KVKAMP-TOTAL   (IX) NOT = ALL '+'                        
158000          OR MID-FLKVKAMP-TOTAL (IX) NOT = ALL '+'                        
158100          OR MID-RERESPRT       (IX) NOT = ALL '+'                        
158200             PERFORM HA-UPD-RAD                                           
158300          END-IF                                                          
158400          ADD 1 TO IX                                                     
158500       END-PERFORM                                                        
158600     END-IF                                                               
158700                                                                          
158800     IF REG-NY-RAD-JA                                                     
158900       PERFORM HB-NY-RAD                                                  
159000     END-IF                                                               
159100                                                                          
159200     MOVE INF-UPDATE-DONE TO MED-IDMFSINF                                 
159300     CALL WMEDKONV USING MED-WMEDAREA                                     
159400     MOVE MED-TEMFSINF TO MOD-TEMFSINF                                    
159500     PERFORM MFS-RENSA-FAELT-IN                                           
159600     .                                                                    
159700     EJECT                                                                
159800 HA-UPD-RAD SECTION.                                                      
159900     MOVE 'HA-UPD-RAD'       TO WS-SECTION                                
160000                                                                          
160100     IF MID-CMD (IX) = 'B'                                                
160200     OR MID-CMD (IX) = 'D'                                                
160300                                                                          
160400       MOVE WS-IDKAMP        TO W-IDKAMP                                  
160500       MOVE MID-IDARTNR (IX) TO W-IDARTNR                                 
160600       PERFORM DB2-DELETE-TP1ARTK                                         
160700                                                                          
160800     ELSE                                                                 
160900                                                                          
161000       IF WS-IDKAMP NOT = SPACE                                           
161100         MOVE WS-IDKAMP      TO W-IDKAMP                                  
161200         MOVE MID-IDARTNR (IX)                                            
161300                             TO W-IDARTNR                                 
161400                                                                          
161500         PERFORM DB2-SELECT-TP1ARTK                                       
161600                                                                          
161700         IF MID-KVKAMP-TOTAL (IX) = ALL '+'                               
161800           CONTINUE                                                       
161900         ELSE                                                             
162000           MOVE MID-KVKAMP-TOTAL (IX)                                     
162100                             TO WS-KVKAMP-TOTAL                           
162200           INSPECT WS-KVKAMP-TOTAL                                        
162300                             REPLACING LEADING SPACE BY ZERO              
162400           IF (TP1KAMP-KDKAMP = 'Q'                                       
162500           OR TP1KAMP-KDKAMP = 'S')                                       
162600             MOVE WS-KVKAMP-TOTAL                                         
162700                               TO TP1ARTK-KVKAMP-TOTAL                    
162800           ELSE                                                           
162900             IF MID-FLKVKAMP-TOTAL (IX) = YES                             
163000               MOVE WS-KVKAMP-TOTAL                                       
163100                                 TO TP1ARTK-KVKAMP-TOTAL                  
163200             END-IF                                                       
163300           END-IF                                                         
163400         END-IF                                                           
163500                                                                          
163600         IF MID-FLKVKAMP-TOTAL (IX) NOT = ALL '+'                         
163700           MOVE MID-FLKVKAMP-TOTAL (IX)                                   
163800                             TO TP1ARTK-FLKVKAMP-TOTAL                    
163900         END-IF                                                           
164000                                                                          
164100         IF MID-KVKAMP-LAUNCH (IX) = ALL '+'                              
164200           CONTINUE                                                       
164300         ELSE                                                             
164400           MOVE MID-KVKAMP-LAUNCH (IX)                                    
164500                             TO WS-KVKAMP-LAUNCH                          
164600           INSPECT WS-KVKAMP-LAUNCH                                       
164700                             REPLACING LEADING SPACE BY ZERO              
164800           MOVE WS-KVKAMP-LAUNCH                                          
164900                             TO TP1ARTK-KVKAMP-LAUNCH                     
165000         END-IF                                                           
165100                                                                          
165200         IF MID-KVKAMP-FIRST (IX) = ALL '+'                               
165300           CONTINUE                                                       
165400         ELSE                                                             
165500           MOVE MID-KVKAMP-FIRST (IX)                                     
165600                             TO WS-KVKAMP-FIRST                           
165700           INSPECT WS-KVKAMP-FIRST                                        
165800                             REPLACING LEADING SPACE BY ZERO              
165900           MOVE WS-KVKAMP-FIRST                                           
166000                             TO TP1ARTK-KVKAMP-FIRST                      
166100         END-IF                                                           
166200                                                                          
166300         IF MID-RERESPRT (IX) = ALL '+'                                   
166400           CONTINUE                                                       
166500         ELSE                                                             
166600           MOVE MID-RERESPRT (IX)                                         
166700                             TO WS-RERESPRT-NUM                           
166800           INSPECT WS-RERESPRT-NUM                                        
166900                             REPLACING LEADING SPACE BY ZERO              
167000           COMPUTE TP1ARTK-RERESPRT = WS-RERESPRT-NUM / 100               
167100         END-IF                                                           
167200                                                                          
167300         PERFORM DB2-UPDATE-TP1ARTK                                       
167400       END-IF                                                             
167500                                                                          
167600       IF WS-IDKAMP-GRP > ZERO                                            
167700         MOVE WS-IDKAMP-GRP  TO W-IDKAMP-GRP                              
167800         MOVE MID-IDARTNR (IX)                                            
167900                             TO W-IDARTNR                                 
168000                                                                          
168100         PERFORM DB2-SELECT-TP1ARTG                                       
168200                                                                          
168300         IF SQLCODE = ZERO                                                
168400                                                                          
168500           IF MID-KVKAMP-LAUNCH (IX) = ALL '+'                            
168600             CONTINUE                                                     
168700           ELSE                                                           
168800             MOVE MID-KVKAMP-LAUNCH (IX)                                  
168900                               TO WS-KVKAMP-LAUNCH                        
169000             INSPECT WS-KVKAMP-LAUNCH                                     
169100                               REPLACING LEADING SPACE BY ZERO            
169200             MOVE WS-KVKAMP-LAUNCH                                        
169300                               TO TP1ARTG-KVKAMP-LAUNCH                   
169400           END-IF                                                         
169500                                                                          
169600           IF MID-KVKAMP-FIRST (IX) = ALL '+'                             
169700             CONTINUE                                                     
169800           ELSE                                                           
169900             MOVE MID-KVKAMP-FIRST (IX)                                   
170000                               TO WS-KVKAMP-FIRST                         
170100             INSPECT WS-KVKAMP-FIRST                                      
170200                               REPLACING LEADING SPACE BY ZERO            
170300             MOVE WS-KVKAMP-FIRST                                         
170400                               TO TP1ARTG-KVKAMP-FIRST                    
170500           END-IF                                                         
170600                                                                          
170700           IF MID-RERESPRT (IX) = ALL '+'                                 
170800             CONTINUE                                                     
170900           ELSE                                                           
171000             MOVE MID-RERESPRT (IX)                                       
171100                               TO WS-RERESPRT-NUM                         
171200             INSPECT WS-RERESPRT-NUM                                      
171300                               REPLACING LEADING SPACE BY ZERO            
171400             COMPUTE TP1ARTG-RERESPRT = WS-RERESPRT-NUM / 100             
171500           END-IF                                                         
171600                                                                          
171700           IF MID-KVKAMP-TOTAL (IX) = ALL '+'                             
171800             CONTINUE                                                     
171900           ELSE                                                           
172000             MOVE MID-KVKAMP-TOTAL (IX)                                   
172100                               TO WS-KVKAMP-TOTAL                         
172200             INSPECT WS-KVKAMP-TOTAL                                      
172300                               REPLACING LEADING SPACE BY ZERO            
172400             MOVE MID-KVKAMP-TOTAL (IX) TO WS-KVKAMP-TOTAL-UPD            
172500             IF MID-FLKVKAMP-TOTAL (IX) = YES                             
172600               MOVE WS-KVKAMP-TOTAL                                       
172700                                 TO TP1ARTG-KVKAMP-TOTAL                  
172800             END-IF                                                       
172900           END-IF                                                         
173000                                                                          
173100           IF MID-FLKVKAMP-TOTAL (IX) NOT = ALL '+'                       
173200             MOVE MID-FLKVKAMP-TOTAL (IX)                                 
173300                               TO TP1ARTG-FLKVKAMP-TOTAL                  
173400           END-IF                                                         
173500                                                                          
173600           PERFORM DB2-UPDATE-TP1ARTG                                     
173700         ELSE                                                             
173800                                                                          
173900           IF MID-KVKAMP-LAUNCH (IX) = ALL '+'                            
174000             MOVE ZERO         TO TP1ARTG-KVKAMP-LAUNCH                   
174100           ELSE                                                           
174200             MOVE MID-KVKAMP-LAUNCH (IX)                                  
174300                               TO WS-KVKAMP-LAUNCH                        
174400             INSPECT WS-KVKAMP-LAUNCH                                     
174500                               REPLACING LEADING SPACE BY ZERO            
174600             MOVE WS-KVKAMP-LAUNCH                                        
174700                               TO TP1ARTG-KVKAMP-LAUNCH                   
174800           END-IF                                                         
174900                                                                          
175000           IF MID-KVKAMP-FIRST (IX) = ALL '+'                             
175100             MOVE ZERO         TO TP1ARTG-KVKAMP-FIRST                    
175200           ELSE                                                           
175300             MOVE MID-KVKAMP-FIRST (IX)                                   
175400                               TO WS-KVKAMP-FIRST                         
175500             INSPECT WS-KVKAMP-FIRST                                      
175600                               REPLACING LEADING SPACE BY ZERO            
175700             MOVE WS-KVKAMP-FIRST                                         
175800                               TO TP1ARTG-KVKAMP-FIRST                    
175900           END-IF                                                         
176000                                                                          
176100           IF MID-RERESPRT (IX) = ALL '+'                                 
176200             MOVE 1            TO TP1ARTG-RERESPRT                        
176300           ELSE                                                           
176400             MOVE MID-RERESPRT (IX)                                       
176500                               TO WS-RERESPRT-NUM                         
176600             INSPECT WS-RERESPRT-NUM                                      
176700                               REPLACING LEADING SPACE BY ZERO            
176800             COMPUTE TP1ARTG-RERESPRT = WS-RERESPRT-NUM / 100             
176900           END-IF                                                         
177000                                                                          
177100           IF MID-KVKAMP-TOTAL (IX) = ALL '+'                             
177200             MOVE ZERO         TO TP1ARTG-KVKAMP-TOTAL                    
177300           ELSE                                                           
177400             MOVE MID-KVKAMP-TOTAL (IX)                                   
177500                               TO WS-KVKAMP-TOTAL                         
177600             INSPECT WS-KVKAMP-TOTAL                                      
177700                               REPLACING LEADING SPACE BY ZERO            
177800             IF MID-FLKVKAMP-TOTAL (IX) = YES                             
177900               MOVE WS-KVKAMP-TOTAL                                       
178000                                 TO TP1ARTG-KVKAMP-TOTAL                  
178100             END-IF                                                       
178200           END-IF                                                         
178300                                                                          
178400           IF MID-FLKVKAMP-TOTAL (IX) = ALL '+'                           
178500             MOVE NEJ          TO TP1ARTG-FLKVKAMP-TOTAL                  
178600           ELSE                                                           
178700             MOVE MID-FLKVKAMP-TOTAL (IX)                                 
178800                               TO TP1ARTG-FLKVKAMP-TOTAL                  
178900           END-IF                                                         
179000                                                                          
179100           PERFORM DB2-INSERT-TP1ARTG                                     
179200         END-IF                                                           
179300       END-IF                                                             
179400     END-IF                                                               
179500     .                                                                    
179600     EJECT                                                                
179700 HB-NY-RAD SECTION.                                                       
179800     MOVE 'HB-NY-RAD'        TO WS-SECTION                                
179900                                                                          
180000     INSPECT MID-NY-IDARTNR  REPLACING LEADING SPACE BY ZERO              
180100     MOVE MID-NY-IDARTNR     TO W-IDARTNR                                 
180200     MOVE MID-NY-KVKAMP-TOTAL                                             
180300                             TO WS-KVKAMP-TOTAL                           
180400     MOVE WS-KVKAMP-TOTAL    TO W-KVKAMP-TOTAL                            
180500     MOVE MID-NY-KVKAMP-LAUNCH                                            
180600                             TO WS-KVKAMP-LAUNCH                          
180700     MOVE WS-KVKAMP-LAUNCH   TO W-KVKAMP-LAUNCH                           
180800     MOVE MID-NY-KVKAMP-FIRST                                             
180900                             TO WS-KVKAMP-FIRST                           
181000     MOVE WS-KVKAMP-FIRST    TO W-KVKAMP-FIRST                            
181100     MOVE ZERO               TO WS-KVREPANT                               
181200     MOVE 1                  TO WS-RERESPRT                               
181300     PERFORM DB2-INSERT-TP1ARTK                                           
181400     MOVE SPACE              TO WS-MSGI-SSA-KEY-ENTER                     
181500                                WS-MSGI-SSA-KEY-NEXT                      
181600     .                                                                    
181700     EJECT                                                                
181800     EJECT                                                                
181900 I-LAES-VISA-INFO SECTION.                                                
182000                                                                          
182100***********************************************                           
182200*                                                                         
182300*      ENSKILD KAMPANJ                                                    
182400*                                                                         
182500***********************************************                           
182600                                                                          
182700     MOVE 1                  TO RAD-IX                                    
182800     MOVE WS-IDKAMP          TO W-IDKAMP                                  
182900     IF  WS-MSGI-SSA-KEY-ENTER = SPACE                                    
183000     AND WS-MSGI-SSA-KEY-NEXT  = SPACE                                    
183100       MOVE ZERO             TO W-IDARTNR                                 
183200     ELSE                                                                 
183300       IF WS-MSGI-SSA-KEY-ENTER NOT = SPACE                               
183400         MOVE WS-ENTER-IDARTNR                                            
183500                             TO W-IDARTNR                                 
183600       ELSE                                                               
183700         MOVE WS-NEXT-IDARTNR                                             
183800                             TO W-IDARTNR                                 
183900       END-IF                                                             
184000     END-IF                                                               
184100     PERFORM DB2-DCL-OPN-CRS-TP1ARTK                                      
184200     IF SQLCODE > ZERO                                                    
184300       MOVE INF-URVAL-SAKNAS                                              
184400                             TO MED-IDMFSFEL                              
184500       CALL WMEDKONV USING MED-WMEDAREA                                   
184600       MOVE MED-TEMFSFEL TO MOD-TEMFSFEL                                  
184700                                                                          
184800     ELSE                                                                 
184900                                                                          
185000       PERFORM DB2-FETCH-TP1ARTK                                          
185100       PERFORM UNTIL SQLCODE > ZERO                                       
185200       OR RAD-IX > RAD-MAX                                                
185300         PERFORM DB2-SELECT-TP1KAMP                                       
185400         MOVE TP1ARTK-IDARTNR                                             
185500                             TO W-IDARTNR                                 
185600         PERFORM FA-LAES-GRUNDDATA                                        
185700*                                                                         
185800*    ARTIKELN SKA INTE VISAS :                                            
185900*            - OM DET ÄR PRODUKTSLAG 18                                   
186000*            - DEFINITIVT ERSATT (KDERS > 19) /I NORMALFALLET/            
186100*              MAN SER DEF. ERSATTA OM MAN FYLLER I                       
186200*              VISA-DEF-ERS = JA SOM NYCKEL I BILDEN                      
186300*                                                                         
186400*    (ARTIKELN VISAS DÄREMOT OM DEN INTE ÄR UPPLAGD I PULS)               
186500*                                                                         
186600                                                                          
186700         IF SEGMENT-FINNS                                                 
186800         AND (((WS-VISA-DEF-ERS = NEJ                                     
186900         OR     WS-VISA-DEF-ERS = SPACE)                                  
187000         AND    CLAG-KDERS > 19)                                          
187100         OR     KDPRODSL-TOOLS)                                           
187200           CONTINUE                                                       
187300         ELSE                                                             
187400           IF SEGMENT-FINNS                                               
187500             MOVE CLAG-IDANSK                                             
187600                             TO WS-IDANSK-NUM                             
187700             MOVE WS-IDANSK-NUM                                           
187800                             TO WS-IDANSK-RED                             
187900             MOVE WS-IDANSK-RED                                           
188000                             TO MOD-IDANSK (RAD-IX)                       
188100                                                                          
188200             MOVE CLAG-KDERS TO WS-KDERS-NUM                              
188300             MOVE WS-KDERS-NUM                                            
188400                             TO WS-KDERS-RED                              
188500             MOVE WS-KDERS-RED                                            
188600                             TO MOD-KDERS (RAD-IX)                        
188700           END-IF                                                         
188800           MOVE TP1ARTK-IDARTNR                                           
188900                             TO MOD-IDARTNR (RAD-IX)                      
189000           IF RAD-IX = 1                                                  
189100             MOVE TP1ARTK-IDARTNR                                         
189200                             TO WS-ENTER-IDARTNR                          
189300             MOVE 1          TO WS-ENTER-RADNR                            
189400             MOVE TP1KAMP-KDKAMP                                          
189500                             TO MOD-KDKAMP                                
189600           END-IF                                                         
189700           MOVE TP1ARTK-KVREPANT                                          
189800                             TO WS-KVREPANT-RED                           
189900           MOVE WS-KVREPANT-RED                                           
190000                             TO MOD-KVREPANT (RAD-IX)                     
190100           IF WS-VISA-UNIK = NEJ                                          
190200           OR WS-VISA-UNIK = SPACE                                        
190300             MOVE TP1ARTK-KVKAMP-LAUNCH                                   
190400                             TO MOD-KVKAMP-LAUNCH (RAD-IX)                
190500             MOVE TP1ARTK-KVKAMP-FIRST                                    
190600                             TO MOD-KVKAMP-FIRST (RAD-IX)                 
190700           ELSE                                                           
190800             MOVE ZERO                                                    
190900                             TO MOD-KVKAMP-LAUNCH (RAD-IX)                
191000                                MOD-KVKAMP-FIRST (RAD-IX)                 
191100           END-IF                                                         
191200           IF TP1KAMP-KDKAMP = 'S'                                        
191300               MOVE MFS-STAENG-FAELT                                      
191400                             TO MOD-KVKAMP-LAUNCH-ATTR (RAD-IX)           
191500                                MOD-KVKAMP-FIRST-ATTR  (RAD-IX)           
191600                                MOD-FLKVKAMP-TOTAL-ATTR  (RAD-IX)         
191700                                MOD-RERESPRT-ATTR      (RAD-IX)           
191800           END-IF                                                         
191900           IF TP1KAMP-KDKAMP = 'Q'                                        
192000               MOVE MFS-STAENG-FAELT                                      
192100                             TO MOD-RERESPRT-ATTR      (RAD-IX)           
192200                                MOD-FLKVKAMP-TOTAL-ATTR  (RAD-IX)         
192300           END-IF                                                         
192400           IF TP1KAMP-KDKAMP = 'W'                                        
192500             IF TP1KAMP-IDKAMP-GRP > ZERO                                 
192600               MOVE ZERO     TO WS-RERESPRT                               
192700             ELSE                                                         
192800               IF TP1ARTK-RERESPRT > ZERO                                 
192900                 IF TP1KAMP-RERESPRT > ZERO                               
193000                   COMPUTE WS-RERESPRT =                                  
193100                           TP1ARTK-RERESPRT * TP1KAMP-RERESPRT            
193200                 ELSE                                                     
193300                   MOVE ZERO TO WS-RERESPRT                               
193400                 END-IF                                                   
193500               ELSE                                                       
193600                 MOVE ZERO   TO WS-RERESPRT                               
193700               END-IF                                                     
193800             END-IF                                                       
193900             IF TP1ARTK-FLKVKAMP-TOTAL = YES                              
194000               MOVE TP1ARTK-KVKAMP-TOTAL                                  
194100                    TO WS-KVKAMP-TOTAL-TAB (RAD-IX)                       
194200             ELSE                                                         
194300               COMPUTE WS-TOTAL ROUNDED =                                 
194400                                  TP1KAMP-KVKAMP-CARS                     
194500                                * TP1ARTK-KVREPANT                        
194600                                * WS-RERESPRT                             
194700               MOVE WS-TOTAL TO WS-KVKAMP-TOTAL-TAB (RAD-IX)              
194800             END-IF                                                       
194900             MOVE WS-KVKAMP-TOTAL-TAB (RAD-IX)                            
195000                             TO MOD-KVKAMP-TOTAL (RAD-IX)                 
195100             IF TP1ARTK-FLKVKAMP-TOTAL = YES                              
195200               MOVE TP1ARTK-FLKVKAMP-TOTAL                                
195300                    TO WS-FLKVKAMP-TOTAL-TAB (RAD-IX)                     
195400             ELSE                                                         
195500               MOVE NEJ TO WS-FLKVKAMP-TOTAL-TAB (RAD-IX)                 
195600             END-IF                                                       
195700             MOVE WS-FLKVKAMP-TOTAL-TAB (RAD-IX)                          
195800                             TO MOD-FLKVKAMP-TOTAL (RAD-IX)               
195900             MOVE MFS-ADD-LAES-IN-FAELT                                   
196000                             TO MOD-FLKVKAMP-TOTAL-ATTR (RAD-IX)          
196100             IF TP1KAMP-IDKAMP-GRP > ZERO                                 
196200               MOVE MFS-STAENG-FAELT                                      
196300                               TO MOD-KVKAMP-TOTAL-ATTR (RAD-IX)          
196400               MOVE SPACE      TO MOD-FLKVKAMP-TOTAL (RAD-IX)             
196500               MOVE MFS-STAENG-FAELT                                      
196600                               TO MOD-FLKVKAMP-TOTAL-ATTR (RAD-IX)        
196700             END-IF                                                       
196800           ELSE                                                           
196900             MOVE TP1ARTK-KVKAMP-TOTAL                                    
197000                             TO MOD-KVKAMP-TOTAL (RAD-IX)                 
197100             IF TP1ARTK-RERESPRT > ZERO                                   
197200               IF TP1KAMP-RERESPRT > ZERO                                 
197300                 COMPUTE WS-RERESPRT =                                    
197400                         TP1ARTK-RERESPRT * TP1KAMP-RERESPRT              
197500               ELSE                                                       
197600                 MOVE ZERO TO WS-RERESPRT                                 
197700               END-IF                                                     
197800             ELSE                                                         
197900               MOVE ZERO     TO WS-RERESPRT                               
198000             END-IF                                                       
198100           END-IF                                                         
198200           COMPUTE WS-RERESPRT-NUM =                                      
198300                             TP1ARTK-RERESPRT * 100                       
198400           MOVE WS-RERESPRT-NUM                                           
198500                             TO WS-RERESPRT-RED                           
198600           MOVE WS-RERESPRT-RED                                           
198700                             TO MOD-RERESPRT (RAD-IX)                     
198800           IF ARTIKEL-FINNS-EJ-I-PULS                                     
198900             MOVE 'MISSING IN PULS'                                       
199000                             TO MOD-KOMMENTAR (RAD-IX)                    
199100           ELSE                                                           
199200             PERFORM IMS-GU-D701                                          
199300             IF SEGMENT-FINNS                                             
199400                PERFORM IMS-GNP-D702                                      
199500                IF SEGMENT-FINNS                                          
199600                   MOVE WDD702-IDARTNR-TILLK                              
199700                             TO WS-IDARTNR-NUM                            
199800                   MOVE WS-IDARTNR-NUM                                    
199900                             TO WS-IDARTNR                                
200000                   INSPECT WS-IDARTNR                                     
200100                             REPLACING LEADING ZERO BY SPACE              
200200                   MOVE WS-IDARTNR                                        
200300                             TO MOD-KOMMENTAR (RAD-IX)                    
200400                   PERFORM IMS-GNP-D702                                   
200500                   IF SEGMENT-FINNS                                       
200600                     MOVE 'VARIOUS'                                       
200700                             TO MOD-KOMMENTAR (RAD-IX)                    
200800                   END-IF                                                 
200900                END-IF                                                    
201000                IF CLAG-KDERS > 19                                        
201100                  PERFORM IMS-GNP-D704                                    
201200                  IF SEGMENT-FINNS                                        
201300                    MOVE WDD704-TIERSDAT-REG                              
201400                               TO WS-AAVVD-NUM                            
201500                    MOVE WS-AAVVD-NUM                                     
201600                               TO MOD-KOMMENTAR (RAD-IX) (11:5)           
201700                  END-IF                                                  
201800                END-IF                                                    
201900             END-IF                                                       
202000           END-IF                                                         
202100                                                                          
202200           ADD 1             TO RAD-IX                                    
202300         END-IF                                                           
202400         PERFORM DB2-FETCH-TP1ARTK                                        
202500       END-PERFORM                                                        
202600                                                                          
202700       IF SQLCODE = ZERO                                                  
202800          MOVE TP1ARTK-IDARTNR                                            
202900                             TO WS-NEXT-IDARTNR                           
203000          MOVE INF-MORE-INFO-EXISTS                                       
203100                             TO MED-IDMFSFEL                              
203200          CALL WMEDKONV USING MED-WMEDAREA                                
203300          MOVE MED-TEMFSFEL  TO MOD-TEMFSFEL                              
203400       ELSE                                                               
203500          MOVE SPACE         TO WS-MSGI-SSA-KEY-NEXT                      
203600       END-IF                                                             
203700                                                                          
203800       PERFORM DB2-CLOSE-TP1ARTK-CRS                                      
203900                                                                          
204000     END-IF                                                               
204100                                                                          
204200     PERFORM UNTIL RAD-IX > RAD-MAX                                       
204300       MOVE MFS-STAENG-FAELT TO MOD-CMD-ATTR           (RAD-IX)           
204400                                MOD-KVKAMP-LAUNCH-ATTR (RAD-IX)           
204500                                MOD-KVKAMP-FIRST-ATTR  (RAD-IX)           
204600                                MOD-KVKAMP-TOTAL-ATTR  (RAD-IX)           
204700                                MOD-FLKVKAMP-TOTAL-ATTR  (RAD-IX)         
204800                                MOD-RERESPRT-ATTR      (RAD-IX)           
204900       MOVE MFS-RENSA-FAELT  TO MOD-CMD           (RAD-IX)                
205000                                MOD-IDARTNR       (RAD-IX)                
205100                                MOD-KVKAMP-LAUNCH (RAD-IX)                
205200                                MOD-KVKAMP-FIRST  (RAD-IX)                
205300                                MOD-KVKAMP-TOTAL  (RAD-IX)                
205400                                MOD-FLKVKAMP-TOTAL  (RAD-IX)              
205500                                MOD-RERESPRT      (RAD-IX)                
205600                                                                          
205700       ADD 1                 TO RAD-IX                                    
205800     END-PERFORM                                                          
205900     .                                                                    
206000     EJECT                                                                
206100 J-HOPP-2315 SECTION.                                                     
206200     MOVE 'J-HOPP-2315        ' TO WS-SECTION                             
206300                                                                          
206400     MOVE JA                 TO INDATA-SW                                 
206500                                                                          
206600     IF  MID-INPUT      = ALL '+'                                         
206700        MOVE ERR-PF11-AND-NO-DATA TO MED-IDMFSFEL                         
206800        MOVE NEJ TO INDATA-SW                                             
206900     ELSE                                                                 
207000                                                                          
207100        MOVE +1              TO RAD-IX                                    
207200        PERFORM UNTIL RAD-IX > RAD-MAX                                    
207300        OR STARTA-ANNAN-BILD                                              
207400        OR INDATA-FEL                                                     
207500                                                                          
207600          IF MID-CMD            (RAD-IX) NOT = ALL '+'                    
207700                                                                          
207800             IF     MID-CMD (RAD-IX) = 'S'                                
207900               MOVE JA       TO SW-STARTA-ANNAN-BILD                      
208000             ELSE                                                         
208100               MOVE MFS-ALFA-FAELT-FEL                                    
208200                             TO MOD-CMD-ATTR (RAD-IX)                     
208300               MOVE ERR-CORR-HILITE-FLDS                                  
208400                             TO MED-IDMFSFEL                              
208500               MOVE NEJ      TO INDATA-SW                                 
208600             END-IF                                                       
208700                                                                          
208800          END-IF                                                          
208900                                                                          
209000          ADD 1 TO RAD-IX                                                 
209100        END-PERFORM                                                       
209200                                                                          
209300     END-IF                                                               
209400                                                                          
209500     IF INDATA-FEL                                                        
209600        CALL WMEDKONV USING MED-WMEDAREA                                  
209700        MOVE MED-TEMFSFEL    TO MOD-TEMFSFEL                              
209800        PERFORM MFS-ROER-EJ-FAELT-UT                                      
209900        PERFORM MFS-ROER-EJ-FAELT-IN                                      
210000     ELSE                                                                 
210100                                                                          
210200       IF STARTA-ANNAN-BILD                                               
210300         MOVE LOW-VALUE      TO P-TO-P-KDZ1                               
210400         MOVE LOW-VALUE      TO P-TO-P-KDZ2                               
210500         MOVE '2'            TO W-HOPP-IDTRANS-2                          
210600         MOVE '315'          TO W-HOPP-IDTRANS-4-6                        
210700         MOVE W-HOPP-IDTRANS TO P-TO-P-KDTRANS                            
210800         MOVE '2314'         TO P-TO-P-IDTRANS                            
210900         MOVE MFS-KDMFSFOR   TO P-TO-P-KDMFSFOR                           
211000         MOVE P-TO-P-SW      TO MSG-IO-AREA                               
211100                                                                          
211200         PERFORM IMS-CHANGE-ALTMSG                                        
211300         PERFORM IMS-INSERT-ALTMSG                                        
211400       ELSE                                                               
211500         MOVE MED-2          TO MOD-TEMFSFEL                              
211600         MOVE NEJ            TO INDATA-SW                                 
211700         PERFORM MFS-ROER-EJ-FAELT-UT                                     
211800         PERFORM MFS-ROER-EJ-FAELT-IN                                     
211900       END-IF                                                             
212000                                                                          
212100     END-IF                                                               
212200     .                                                                    
212300     EJECT                                                                
212400 MFS-RENSA-FAELT-UT SECTION.                                              
212500                                                                          
212600*    --- ALLA UTDATA-FÄLT                                                 
212700     MOVE +1 TO IX                                                        
212800     PERFORM UNTIL IX > RAD-MAX                                           
212900       MOVE MFS-RENSA-FAELT  TO MOD-KVREPANT  (IX)                        
213000                                MOD-IDANSK    (IX)                        
213100                                MOD-KDERS     (IX)                        
213200                                MOD-KOMMENTAR (IX)                        
213300       ADD +1 TO IX                                                       
213400     END-PERFORM                                                          
213500     MOVE MFS-RENSA-FAELT    TO MOD-TEMFSINF                              
213600                                MOD-KDKAMP                                
213700     .                                                                    
213800     SKIP2                                                                
213900 MFS-RENSA-FAELT-IN SECTION.                                              
214000                                                                          
214100*    --- ALLA INDATA-FÄLT                                                 
214200     MOVE +1 TO IX                                                        
214300     PERFORM UNTIL IX > RAD-MAX                                           
214400       MOVE MFS-RENSA-FAELT  TO MOD-CMD (IX)                              
214500                                MOD-IDARTNR (IX)                          
214600                                MOD-KVKAMP-LAUNCH (IX)                    
214700                                MOD-KVKAMP-FIRST  (IX)                    
214800                                MOD-KVKAMP-TOTAL  (IX)                    
214900                                MOD-FLKVKAMP-TOTAL  (IX)                  
215000                                MOD-RERESPRT  (IX)                        
215100       ADD +1 TO IX                                                       
215200     END-PERFORM                                                          
215300     MOVE MFS-RENSA-FAELT    TO MOD-NY-IDARTNR                            
215400                                MOD-NY-KVKAMP-LAUNCH                      
215500                                MOD-NY-KVKAMP-FIRST                       
215600                                MOD-NY-KVKAMP-TOTAL                       
215700     .                                                                    
215800     EJECT                                                                
215900 MFS-ROER-EJ-FAELT-UT  SECTION.                                           
216000                                                                          
216100*    --- ALLA UTDATA-FÄLT                                                 
216200     MOVE +1 TO IX                                                        
216300     PERFORM UNTIL IX > RAD-MAX                                           
216400       MOVE MFS-ROER-EJ-FAELT                                             
216500                             TO MOD-KVREPANT  (IX)                        
216600                                MOD-IDANSK    (IX)                        
216700                                MOD-KDERS     (IX)                        
216800                                MOD-KOMMENTAR (IX)                        
216900       ADD +1 TO IX                                                       
217000     END-PERFORM                                                          
217100     MOVE MFS-ROER-EJ-FAELT  TO MOD-TEMFSINF                              
217200     .                                                                    
217300     SKIP2                                                                
217400 MFS-ROER-EJ-FAELT-IN  SECTION.                                           
217500                                                                          
217600*    --- ALLA INDATA-FÄLT                                                 
217700     MOVE +1 TO IX                                                        
217800     PERFORM UNTIL IX > RAD-MAX                                           
217900       MOVE MFS-ROER-EJ-FAELT                                             
218000                             TO MOD-CMD (IX)                              
218100                                MOD-IDARTNR (IX)                          
218200                                MOD-KVKAMP-LAUNCH (IX)                    
218300                                MOD-KVKAMP-FIRST  (IX)                    
218400                                MOD-KVKAMP-TOTAL  (IX)                    
218500                                MOD-FLKVKAMP-TOTAL  (IX)                  
218600                                MOD-RERESPRT  (IX)                        
218700       ADD +1 TO IX                                                       
218800     END-PERFORM                                                          
218900     MOVE MFS-ROER-EJ-FAELT  TO MOD-NY-IDARTNR                            
219000                                MOD-NY-KVKAMP-LAUNCH                      
219100                                MOD-NY-KVKAMP-FIRST                       
219200                                MOD-NY-KVKAMP-TOTAL                       
219300     .                                                                    
219400     EJECT                                                                
219500 MFS-LAES-IN-IGEN SECTION.                                                
219600                                                                          
219700*    --- ALLA INDATA-FÄLT                                                 
219800     MOVE +1 TO IX                                                        
219900     PERFORM UNTIL IX > RAD-MAX                                           
220000       MOVE MFS-ADD-LAES-IN-FAELT                                         
220100                             TO MOD-CMD-ATTR (IX)                         
220200                                MOD-KVKAMP-LAUNCH-ATTR (IX)               
220300                                MOD-KVKAMP-FIRST-ATTR (IX)                
220400                                MOD-KVKAMP-TOTAL-ATTR (IX)                
220500                                MOD-FLKVKAMP-TOTAL-ATTR (IX)              
220600                                MOD-RERESPRT-ATTR (IX)                    
220700       ADD +1 TO IX                                                       
220800     END-PERFORM                                                          
220900     MOVE MFS-ADD-LAES-IN-FAELT                                           
221000                             TO MOD-NY-IDARTNR-ATTR                       
221100                                MOD-NY-KVKAMP-LAUNCH-ATTR                 
221200                                MOD-NY-KVKAMP-FIRST-ATTR                  
221300                                MOD-NY-KVKAMP-TOTAL-ATTR                  
221400     .                                                                    
221500     EJECT                                                                
221600* --- IMS SEKTIONER ---                                                   
221700     SKIP3                                                                
221800 IMS-GET-MSG SECTION.                                                     
221900                                                                          
222000     MOVE '  QC' TO GODK-STATUSKODER                                      
222100     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
222200     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
222300     PERFORM IMS-STATUSKONTROLL                                           
222400     .                                                                    
222500     SKIP3                                                                
222600 IMS-INSERT-MSG SECTION.                                                  
222700                                                                          
222800     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
222900     MOVE SPACE TO GODK-STATUSKODER                                       
223000     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
223100     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
223200     PERFORM IMS-STATUSKONTROLL                                           
223300     .                                                                    
223400     EJECT                                                                
223500 IMS-CHANGE-ALTMSG SECTION.                                               
223600     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
223700     MOVE '  A1A4' TO GODK-STATUSKODER                                    
223800     CALL CBLTDLI USING CHNG ALT-PCB MSG-KDTRANS-1                        
223900     MOVE ALT-STATUS-CODE TO STATUS-WS                                    
224000     PERFORM IMS-STATUSKONTROLL                                           
224100     .                                                                    
224200     EJECT                                                                
224300 IMS-INSERT-ALTMSG SECTION.                                               
224400     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
224500     MOVE SPACE TO GODK-STATUSKODER                                       
224600     CALL CBLTDLI USING ISRT ALT-PCB MSG-IO-AREA                          
224700     MOVE ALT-STATUS-CODE TO STATUS-WS                                    
224800     PERFORM IMS-STATUSKONTROLL                                           
224900     .                                                                    
225000     EJECT                                                                
225100 IMS-GU-K601 SECTION.                                                     
225200                                                                          
225300     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
225400            DELIMITED BY SIZE INTO SSA1                                   
225500     MOVE '  GE' TO GODK-STATUSKODER                                      
225600     CALL CBLTDLI USING GU    WDK6-PCB DLI-IO-WDK601 SSA1                 
225700     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
225800     PERFORM IMS-STATUSKONTROLL                                           
225900     .                                                                    
226000     EJECT                                                                
226100 IMS-GU-K611 SECTION.                                                     
226200                                                                          
226300     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
226400            DELIMITED BY SIZE INTO SSA1                                   
226500     STRING 'WDK611  (KDSEGKEY =' W-KDSEGKEY-X ')'                        
226600          DELIMITED BY SIZE INTO SSA2                                     
226700     MOVE '  GE' TO GODK-STATUSKODER                                      
226800     CALL CBLTDLI USING GU    WDK6-PCB DLI-IO-WDK611 SSA1 SSA2            
226900     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
227000     PERFORM IMS-STATUSKONTROLL                                           
227100     .                                                                    
227200     EJECT                                                                
227300 IMS-GU-D701 SECTION.                                                     
227400     STRING 'WDD701  (IDARTNR  =' W-IDARTNR-X ')'                         
227500            DELIMITED BY SIZE INTO SSA1                                   
227600     MOVE '  GE' TO GODK-STATUSKODER                                      
227700     CALL CBLTDLI USING GU WDD7-PCB DLI-IO-WDD701 SSA1                    
227800     MOVE WDD7-STATUS-CODE TO STATUS-WS                                   
227900     PERFORM IMS-STATUSKONTROLL                                           
228000     .                                                                    
228100     EJECT                                                                
228200 IMS-GNP-D702 SECTION.                                                    
228300     STRING 'WDD702  (FLTEXT   =N)'                                       
228400            DELIMITED BY SIZE INTO SSA1                                   
228500     MOVE '  GE' TO GODK-STATUSKODER                                      
228600     CALL CBLTDLI USING GNP WDD7-PCB DLI-IO-WDD702 SSA1                   
228700     MOVE WDD7-STATUS-CODE TO STATUS-WS                                   
228800     PERFORM IMS-STATUSKONTROLL                                           
228900     .                                                                    
229000     EJECT                                                                
229100 IMS-GNP-D704 SECTION.                                                    
229200     MOVE 'WDD704   ' TO SSA1                                             
229300     MOVE '  GE' TO GODK-STATUSKODER                                      
229400     CALL CBLTDLI USING GNP WDD7-PCB DLI-IO-WDD704 SSA1                   
229500     MOVE WDD7-STATUS-CODE TO STATUS-WS                                   
229600     PERFORM IMS-STATUSKONTROLL                                           
229700     .                                                                    
229800     EJECT                                                                
229900 IMS-STATUSKONTROLL SECTION.                                              
230000                                                                          
230100     SET STATUS-IX TO 1                                                   
230200     SEARCH GODK-STATUS                                                   
230300       AT END                                                             
230400         STRING 'OTILLÅTEN STATUSKOD FRÅN IMS: ' STATUS-WS                
230500         DELIMITED BY SIZE INTO FELTEXT                                   
230600         CALL FELLOG                                                      
230700       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
230800         CONTINUE                                                         
230900     END-SEARCH                                                           
231000     .                                                                    
231100     EJECT                                                                
231200 DB2-DCL-OPN-CRS-TP1KAMP SECTION.                                         
231300     MOVE 'DB2-DCL-OPN-CRS-TP1KAMP' TO  WS-DB2-SEKTION                    
231400*    DISPLAY WS-DB2-SEKTION                                               
231500* OBS!!! DECLARE GER INGEN SQLCODE I RETUR                                
231600     EXEC SQL DECLARE TP1KAMP-CRS CURSOR FOR                              
231700              SELECT IDKAMP,                                              
231800                     KVKAMP_CARS,                                         
231900                     TISTADAT_KAMP,                                       
232000                     TISTODAT_KAMP,                                       
232100                     RERESPRT,                                            
232200                     KDKAMP,                                              
232300                     BEKAMNOT,                                            
232400                     IDKAMP_GRP,                                          
232500                     IDLOPNR_KAMP                                         
232600              FROM TP1KAMP                                                
232700              WHERE IDKAMP_GRP = :W-IDKAMP-GRP                            
232800     END-EXEC                                                             
232900     MOVE 000               TO GODK-SQLCODESKODER                         
233000     EXEC SQL OPEN TP1KAMP-CRS END-EXEC                                   
233100     MOVE SQLCODE           TO SQLCODE-WS                                 
233200     PERFORM DB2-STATUSKONTROLL                                           
233300     .                                                                    
233400     EJECT                                                                
233500 DB2-FETCH-TP1KAMP SECTION.                                               
233600     MOVE 'DB2-FETCH-TP1KAMP' TO  WS-DB2-SEKTION                          
233700*    DISPLAY WS-DB2-SEKTION                                               
233800     MOVE 000100            TO GODK-SQLCODESKODER                         
233900     EXEC SQL FETCH TP1KAMP-CRS INTO                                      
234000            :TP1KAMP-IDKAMP                                               
234100           ,:TP1KAMP-KVKAMP-CARS                                          
234200           ,:TP1KAMP-TISTADAT-KAMP                                        
234300           ,:TP1KAMP-TISTODAT-KAMP                                        
234400           ,:TP1KAMP-RERESPRT                                             
234500           ,:TP1KAMP-KDKAMP                                               
234600           ,:TP1KAMP-BEKAMNOT                                             
234700           ,:TP1KAMP-IDKAMP-GRP                                           
234800           ,:TP1KAMP-IDLOPNR-KAMP                                         
234900     END-EXEC                                                             
235000     MOVE SQLCODE           TO SQLCODE-WS                                 
235100     PERFORM DB2-STATUSKONTROLL                                           
235200     .                                                                    
235300     EJECT                                                                
235400 DB2-CLOSE-TP1KAMP-CRS SECTION.                                           
235500     MOVE 'DB2-CLOSE-TP1KAMP-CRS' TO  WS-DB2-SEKTION                      
235600*    DISPLAY WS-DB2-SEKTION                                               
235700     SKIP2                                                                
235800     EXEC SQL CLOSE TP1KAMP-CRS END-EXEC                                  
235900     .                                                                    
236000     EJECT                                                                
236100 DB2-DCL-OPN-CRS-TP1ARTK SECTION.                                         
236200     MOVE 'DB2-DCL-OPN-CRS-TP1ARTK' TO  WS-DB2-SEKTION                    
236300*    DISPLAY WS-DB2-SEKTION                                               
236400* OBS!!! DECLARE GER INGEN SQLCODE I RETUR                                
236500     EXEC SQL DECLARE TP1ARTK-CRS CURSOR FOR                              
236600              SELECT IDKAMP                                               
236700                     ,IDARTNR                                             
236800                     ,KVREPANT                                            
236900                     ,KVKAMP_TOTAL                                        
237000                     ,KVKAMP_LAUNCH                                       
237100                     ,KVKAMP_FIRST                                        
237200                     ,RERESPRT                                            
237300                     ,FLKVKAMP_TOTAL                                      
237400              FROM TP1ARTK                                                
237500              WHERE IDKAMP   = :W-IDKAMP                                  
237600              AND   IDARTNR >= :W-IDARTNR                                 
237700     END-EXEC                                                             
237800     MOVE 000               TO GODK-SQLCODESKODER                         
237900     EXEC SQL OPEN TP1ARTK-CRS END-EXEC                                   
238000     MOVE SQLCODE           TO SQLCODE-WS                                 
238100     PERFORM DB2-STATUSKONTROLL                                           
238200     .                                                                    
238300     EJECT                                                                
238400 DB2-FETCH-TP1ARTK SECTION.                                               
238500     MOVE 'DB2-FETCH-TP1ARTK' TO  WS-DB2-SEKTION                          
238600*    DISPLAY WS-DB2-SEKTION                                               
238700     MOVE 000100            TO GODK-SQLCODESKODER                         
238800     EXEC SQL FETCH TP1ARTK-CRS INTO                                      
238900            :TP1ARTK-IDKAMP                                               
239000           ,:TP1ARTK-IDARTNR                                              
239100           ,:TP1ARTK-KVREPANT                                             
239200           ,:TP1ARTK-KVKAMP-TOTAL                                         
239300           ,:TP1ARTK-KVKAMP-LAUNCH                                        
239400           ,:TP1ARTK-KVKAMP-FIRST                                         
239500           ,:TP1ARTK-RERESPRT                                             
239600           ,:TP1ARTK-FLKVKAMP-TOTAL                                       
239700     END-EXEC                                                             
239800     MOVE SQLCODE           TO SQLCODE-WS                                 
239900     PERFORM DB2-STATUSKONTROLL                                           
240000     .                                                                    
240100     EJECT                                                                
240200 DB2-CLOSE-TP1ARTK-CRS SECTION.                                           
240300     MOVE 'DB2-CLOSE-TP1ARTK-CRS' TO  WS-DB2-SEKTION                      
240400*    DISPLAY WS-DB2-SEKTION                                               
240500     SKIP2                                                                
240600     EXEC SQL CLOSE TP1ARTK-CRS END-EXEC                                  
240700     .                                                                    
240800     EJECT                                                                
240900 DB2-SELECT-TP1KAMP     SECTION.                                          
241000     MOVE 'DB2-SELECT-TP1KAMP   ' TO  WS-DB2-SEKTION                      
241100                                                                          
241200     MOVE 000100 TO GODK-SQLCODESKODER                                    
241300                                                                          
241400     EXEC SQL                                                             
241500           SELECT  IDKAMP                                                 
241600                  ,KVKAMP_CARS                                            
241700                  ,RERESPRT                                               
241800                  ,KDKAMP                                                 
241900                  ,IDKAMP_GRP                                             
242000                                                                          
242100           INTO   :TP1KAMP-IDKAMP                                         
242200                 ,:TP1KAMP-KVKAMP-CARS                                    
242300                 ,:TP1KAMP-RERESPRT                                       
242400                 ,:TP1KAMP-KDKAMP                                         
242500                 ,:TP1KAMP-IDKAMP-GRP                                     
242600                                                                          
242700           FROM    TP1KAMP                                                
242800                                                                          
242900           WHERE   IDKAMP    = :WS-IDKAMP                                 
243000     END-EXEC                                                             
243100                                                                          
243200     MOVE SQLCODE TO SQLCODE-WS                                           
243300     PERFORM DB2-STATUSKONTROLL                                           
243400     .                                                                    
243500     EJECT                                                                
243600 DB2-SELECT-TP1GRP      SECTION.                                          
243700     MOVE 'DB2-SELECT-TP1GRP   ' TO   WS-DB2-SEKTION                      
243800                                                                          
243900     MOVE 000100 TO GODK-SQLCODESKODER                                    
244000                                                                          
244100     EXEC SQL                                                             
244200           SELECT  IDKAMP_GRP                                             
244300                  ,BEKAMNOT                                               
244400                  ,RERESPRT                                               
244500                                                                          
244600           INTO   :TP1GRP-IDKAMP-GRP                                      
244700                 ,:TP1GRP-BEKAMNOT                                        
244800                 ,:TP1GRP-RERESPRT                                        
244900                                                                          
245000           FROM    TP1GRP                                                 
245100                                                                          
245200           WHERE   IDKAMP_GRP = :W-IDKAMP-GRP                             
245300     END-EXEC                                                             
245400                                                                          
245500     MOVE SQLCODE TO SQLCODE-WS                                           
245600     PERFORM DB2-STATUSKONTROLL                                           
245700     .                                                                    
245800     EJECT                                                                
245900 DB2-SELECT-TP1ARTG     SECTION.                                          
246000     MOVE 'DB2-SELECT-TP1ARTG   ' TO  WS-DB2-SEKTION                      
246100                                                                          
246200     MOVE 000100 TO GODK-SQLCODESKODER                                    
246300                                                                          
246400     EXEC SQL                                                             
246500           SELECT  IDKAMP_GRP                                             
246600                  ,IDARTNR                                                
246700                  ,KVKAMP_LAUNCH                                          
246800                  ,KVKAMP_FIRST                                           
246900                  ,RERESPRT                                               
247000                  ,KVKAMP_TOTAL                                           
247100                  ,FLKVKAMP_TOTAL                                         
247200                                                                          
247300           INTO   :TP1ARTG-IDKAMP-GRP                                     
247400                 ,:TP1ARTG-IDARTNR                                        
247500                 ,:TP1ARTG-KVKAMP-LAUNCH                                  
247600                 ,:TP1ARTG-KVKAMP-FIRST                                   
247700                 ,:TP1ARTG-RERESPRT                                       
247800                 ,:TP1ARTG-KVKAMP-TOTAL                                   
247900                 ,:TP1ARTG-FLKVKAMP-TOTAL                                 
248000                                                                          
248100           FROM    TP1ARTG                                                
248200                                                                          
248300           WHERE   IDKAMP_GRP = :W-IDKAMP-GRP                             
248400           AND     IDARTNR    = :W-IDARTNR                                
248500     END-EXEC                                                             
248600                                                                          
248700     MOVE SQLCODE TO SQLCODE-WS                                           
248800     PERFORM DB2-STATUSKONTROLL                                           
248900     .                                                                    
249000     EJECT                                                                
249100 DB2-SELECT-TP1ARTK     SECTION.                                          
249200     MOVE 'DB2-SELECT-TP1ARTK   ' TO  WS-DB2-SEKTION                      
249300                                                                          
249400     MOVE 000100 TO GODK-SQLCODESKODER                                    
249500                                                                          
249600     EXEC SQL                                                             
249700           SELECT  IDKAMP                                                 
249800                  ,IDARTNR                                                
249900                  ,KVREPANT                                               
250000                  ,KVKAMP_TOTAL                                           
250100                  ,KVKAMP_LAUNCH                                          
250200                  ,KVKAMP_FIRST                                           
250300                  ,RERESPRT                                               
250400                  ,FLKVKAMP_TOTAL                                         
250500                                                                          
250600           INTO   :TP1ARTK-IDKAMP                                         
250700                 ,:TP1ARTK-IDARTNR                                        
250800                 ,:TP1ARTK-KVREPANT                                       
250900                 ,:TP1ARTK-KVKAMP-TOTAL                                   
251000                 ,:TP1ARTK-KVKAMP-LAUNCH                                  
251100                 ,:TP1ARTK-KVKAMP-FIRST                                   
251200                 ,:TP1ARTK-RERESPRT                                       
251300                 ,:TP1ARTK-FLKVKAMP-TOTAL                                 
251400                                                                          
251500           FROM    TP1ARTK                                                
251600                                                                          
251700           WHERE   IDKAMP    = :W-IDKAMP                                  
251800             AND   IDARTNR   = :W-IDARTNR                                 
251900     END-EXEC                                                             
252000                                                                          
252100     MOVE SQLCODE TO SQLCODE-WS                                           
252200     PERFORM DB2-STATUSKONTROLL                                           
252300     .                                                                    
252400     EJECT                                                                
252500 DB2-DELETE-TP1ARTK SECTION.                                              
252600     MOVE 'DB2-DELETE-TP1ARTK   ' TO  WS-DB2-SEKTION                      
252700                                                                          
252800     MOVE 000   TO GODK-SQLCODESKODER                                     
252900                                                                          
253000     EXEC SQL                                                             
253100         DELETE FROM TP1ARTK                                              
253200                                                                          
253300         WHERE   IDKAMP    = :W-IDKAMP                                    
253400           AND   IDARTNR   = :W-IDARTNR                                   
253500     END-EXEC                                                             
253600                                                                          
253700     MOVE SQLCODE TO SQLCODE-WS                                           
253800     PERFORM DB2-STATUSKONTROLL                                           
253900     .                                                                    
254000     EJECT                                                                
254100 DB2-UPDATE-TP1ARTK  SECTION.                                             
254200     MOVE 'DB2-UPDATE-TP1ARTK   ' TO  WS-DB2-SEKTION                      
254300                                                                          
254400     MOVE 000     TO GODK-SQLCODESKODER                                   
254500     EXEC SQL                                                             
254600         UPDATE TP1ARTK                                                   
254700             SET KVKAMP_TOTAL  = :TP1ARTK-KVKAMP-TOTAL                    
254800               , KVKAMP_LAUNCH = :TP1ARTK-KVKAMP-LAUNCH                   
254900               , KVKAMP_FIRST  = :TP1ARTK-KVKAMP-FIRST                    
255000               , RERESPRT      = :TP1ARTK-RERESPRT                        
255100               , FLKVKAMP_TOTAL  = :TP1ARTK-FLKVKAMP-TOTAL                
255200                                                                          
255300         WHERE   IDKAMP    = :W-IDKAMP                                    
255400           AND   IDARTNR   = :W-IDARTNR                                   
255500     END-EXEC                                                             
255600                                                                          
255700     MOVE SQLCODE TO SQLCODE-WS                                           
255800     PERFORM DB2-STATUSKONTROLL                                           
255900     .                                                                    
256000     EJECT                                                                
256100 DB2-INSERT-TP1ARTK  SECTION.                                             
256200     MOVE 'DB2-INSERT-TP1ARTK   ' TO  WS-DB2-SEKTION                      
256300     SKIP2                                                                
256400     MOVE 000   TO GODK-SQLCODESKODER                                     
256500     EXEC SQL                                                             
256600         INSERT INTO TP1ARTK                                              
256700            (IDKAMP,IDARTNR,KVREPANT,KVKAMP_TOTAL                         
256800            ,KVKAMP_LAUNCH,KVKAMP_FIRST,RERESPRT)                         
256900         VALUES                                                           
257000            (:W-IDKAMP,:W-IDARTNR,:WS-KVREPANT                            
257100            ,:W-KVKAMP-TOTAL,:W-KVKAMP-LAUNCH                             
257200            ,:W-KVKAMP-FIRST,:WS-RERESPRT)                                
257300     END-EXEC                                                             
257400                                                                          
257500     MOVE SQLCODE TO SQLCODE-WS                                           
257600     PERFORM DB2-STATUSKONTROLL                                           
257700     .                                                                    
257800     EJECT                                                                
257900 DB2-UPDATE-TP1ARTG  SECTION.                                             
258000     MOVE 'DB2-UPDATE-TP1ARTG   ' TO  WS-DB2-SEKTION                      
258100                                                                          
258200     MOVE 000     TO GODK-SQLCODESKODER                                   
258300     EXEC SQL                                                             
258400         UPDATE TP1ARTG                                                   
258500             SET KVKAMP_LAUNCH = :TP1ARTG-KVKAMP-LAUNCH                   
258600               , KVKAMP_FIRST  = :TP1ARTG-KVKAMP-FIRST                    
258700               , RERESPRT      = :TP1ARTG-RERESPRT                        
258800               , KVKAMP_TOTAL  = :TP1ARTG-KVKAMP-TOTAL                    
258900               , FLKVKAMP_TOTAL  = :TP1ARTG-FLKVKAMP-TOTAL                
259000                                                                          
259100         WHERE   IDKAMP_GRP = :W-IDKAMP-GRP                               
259200           AND   IDARTNR    = :W-IDARTNR                                  
259300     END-EXEC                                                             
259400                                                                          
259500     MOVE SQLCODE TO SQLCODE-WS                                           
259600     PERFORM DB2-STATUSKONTROLL                                           
259700     .                                                                    
259800     EJECT                                                                
259900 DB2-INSERT-TP1ARTG  SECTION.                                             
260000     MOVE 'DB2-INSERT-TP1ARTG   ' TO  WS-DB2-SEKTION                      
260100     SKIP2                                                                
260200     MOVE 000   TO GODK-SQLCODESKODER                                     
260300     EXEC SQL                                                             
260400         INSERT INTO TP1ARTG                                              
260500            (IDKAMP_GRP,IDARTNR                                           
260600            ,KVKAMP_LAUNCH,KVKAMP_FIRST,RERESPRT                          
260700            ,KVKAMP_TOTAL,FLKVKAMP_TOTAL)                                 
260800         VALUES                                                           
260900            (:W-IDKAMP-GRP,:W-IDARTNR                                     
261000            ,:TP1ARTG-KVKAMP-LAUNCH                                       
261100            ,:TP1ARTG-KVKAMP-FIRST,:TP1ARTG-RERESPRT                      
261200            ,:TP1ARTG-KVKAMP-TOTAL,:TP1ARTG-FLKVKAMP-TOTAL)               
261300     END-EXEC                                                             
261400                                                                          
261500     MOVE SQLCODE TO SQLCODE-WS                                           
261600     PERFORM DB2-STATUSKONTROLL                                           
261700     .                                                                    
261800     EJECT                                                                
261900 DB2-STATUSKONTROLL  SECTION.                                             
262000                                                                          
262100     SET SQLCODE-IX TO 1                                                  
262200     SEARCH GODK-SQLCODE                                                  
262300       AT END CALL FELLOG                                                 
262400       WHEN GODK-SQLCODE (SQLCODE-IX) = SQLCODE-WS CONTINUE               
262500     END-SEARCH                                                           
262600     .                                                                    
