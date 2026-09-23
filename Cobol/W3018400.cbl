000100 PROCESS DYNAM                                                            
000200 ID DIVISION.                                                             
000300 PROGRAM-ID.     W3018400.                                                
000400 AUTHOR.         RANDI BERG.                                              
000500 DATE-WRITTEN.   98/04/14.                                                
000600 DATE-COMPILED.                                                           
000700                                                                          
000800*    FUNKTION:                                                            
000900*        BYTESPROFORMA,                                                   
001000*        OBJEKT MELLAN MAASTRICHT OCH JAPAN/AUSTRALIEN.                   
001100*        BILD FÖR MOTTAGANDE LAGER MAASTRICHT.                            
001200*                                                                         
001300*        PROGRAMMET UPPDATERAR WDR9  EKONOMITRANSAR (FÖR SAP R/3)         
001400*        PROGRAMMET UPPDATERAR WDR4                                       
001500*        PROGRAMMET UPPDATERAR WDK7                                       
001600*                                                                         
001700*    INDATA.                                                              
001800*        TRANSAKTION: W3T184                                              
001900*        MID:         W3I18401                                            
002000*                                                                         
002100*    UTDATA.                                                              
002200*        MOD:         W3O18401                                            
002300*                                                                         
002400*    REMARKS:         ETRACKER ÄRENDE 1493572                             
002500*                                                                         
002600*    REMARKS:         ETRACKER ÄRENDE 1863643 050406/EÅ/KJH               
002700*                                                                         
002800                                                                          
002900     SKIP3                                                                
003000 ENVIRONMENT DIVISION.                                                    
003100     EJECT                                                                
003200 DATA DIVISION.                                                           
003300 WORKING-STORAGE SECTION.                                                 
003400                                                                          
003500 77  IDPGM                       PIC X(08)   VALUE 'W3018400'.            
003600                                                                          
003700*    --- ARBETSFÄLT FÖR FELMEDDELANDEN VID CALL ABEND/FELLOG              
003800 77  FELTEXT                     PIC X(80) VALUE SPACE.                   
003900                                                                          
004000 77  JA                          PIC X       VALUE 'J'.                   
004100 77  NEJ                         PIC X       VALUE 'N'.                   
004200                                                                          
004300*    --- INDEX FÖR BLÄDDRINGSRADER                                        
004400 77  INDX                        PIC S9(4)  VALUE +0    COMP SYNC.        
004500 77  MAX-INDX                    PIC S9(4)  VALUE +11   COMP SYNC.        
004600 77  W-INDX                      PIC S9(4)  VALUE +0    COMP SYNC.        
004700 77  PRT-IX                      PIC S9(4)  VALUE +0    COMP SYNC.        
004800 77  WS-PRT-QTY                  PIC 9(7)   VALUE ZERO.                   
004900*    --- ARBETSFÄLT FÖR AKTUELLA NYCKELVÄRDEN FRÅN SKÄRMEN                
005000                                                                          
005100 77  INDATA-SW                   PIC X       VALUE 'J'.                   
005200     88  INDATA-OK                           VALUE 'J'.                   
005300     88  INDATA-FEL                          VALUE 'N'.                   
005400                                                                          
005500 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
005600     88  NYCKLAR-OK                          VALUE 'J'.                   
005700     88  NYCKLAR-FEL                         VALUE 'N'.                   
005800                                                                          
005900 77  KOLLI-KLAR-SW               PIC X       VALUE 'N'.                   
006000     88  KOLLI-KLAR                          VALUE 'J'.                   
006100                                                                          
006200 77  KOLLI-SW                    PIC X       VALUE 'N'.                   
006300     88  NYTT-KOLLI                          VALUE 'J'.                   
006400                                                                          
006500                                                                          
006600 77  OBJEKT-SW                   PIC X       VALUE 'N'.                   
006700     88  NYTT-OBJEKT                         VALUE 'J'.                   
006800                                                                          
006900 77  ANTAL-SW                    PIC X       VALUE 'N'.                   
007000     88  NYTT-ANTAL                          VALUE 'J'.                   
007100                                                                          
007200 77  PRINT-SW                    PIC X       VALUE 'N'.                   
007300     88  PRINT-LABEL                         VALUE 'J'.                   
007400                                                                          
007500 77  UPDATED-SW                  PIC X       VALUE 'N'.                   
007600     88  UPDATED-YES                         VALUE 'J'.                   
007700                                                                          
007800 77  HEADER-SW                   PIC X       VALUE 'N'.                   
007900     88  HEADER-WRITTEN                      VALUE 'J'.                   
008000     88  HEADER-NOT-WRITTEN                  VALUE 'N'.                   
008100                                                                          
008200 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
008300     88  EGEN-MID                            VALUE '3184'.                
008400     88  GODK-MID                            VALUE '3181' '3182'          
008500                                                   '3183' '3184'          
008600                                                   '3185' '3186'          
008700                                                   '3187' '3188'          
008800                                                   '3189'.                
008900     88  HELP-MID                            VALUE '0551'.                
009000                                                                          
009100 77  W-WDK611-FINNS              PIC X       VALUE 'N'.                   
009200 77  W-3172-IDDC-REC             PIC X(2)    VALUE SPACE.                 
009300 77  W-3172-IDDC-SEND            PIC X(2)    VALUE SPACE.                 
009400 77  WS-IDFAKT                   PIC X(7)    VALUE ZERO.                  
009500 77  WS-IDKOLLI                  PIC X(5)    VALUE ZERO.                  
009600 77  W-KVCLEAR                   PIC S9(7)   VALUE ZERO COMP-3.           
009700 77  W-KVAVVIK                   PIC S9(7)   VALUE ZERO COMP-3.           
009800 77  W-KVANTMOT                  PIC S9(7)   VALUE ZERO COMP-3.           
009900 77  W-KVANTMOT-NY               PIC S9(7)   VALUE ZERO COMP-3.           
010000 77  WS-KVANTMOT-NUM             PIC S9(7)   VALUE ZERO COMP-3.           
010100 77  WS-KVANTMOT                 PIC X(7)    VALUE SPACE.                 
010200 77  WS-IDARTNR-OBJ              PIC 9(9).                                
010300 77  W-DAGENS-DATUM              PIC 9(8).                                
010400 77  TRANS-TID                   PIC 9(9).                                
010500 77  IX                          PIC S9(7)   COMP-3 VALUE ZERO.           
010600 77  IX2                         PIC S9(7)   COMP-3 VALUE ZERO.           
010700                                                                          
010800 77  KDRC-DISPLAY                PIC Z(5).                                
010900                                                                          
011000 01  WS-CURRENT-DATE-TIME.                                                
011100     03  WS-YEAR                 PIC 9(4).                                
011200     03  WS-MONTH                PIC 9(2).                                
011300     03  WS-DAY                  PIC 9(2).                                
011400     03  WS-HOUR                 PIC 9(2).                                
011500     03  WS-MINUTE               PIC 9(2).                                
011600 01  FILLER REDEFINES WS-CURRENT-DATE-TIME.                               
011700     03  FILLER                  PIC X(2).                                
011800     03  WS-TIYYMMDDHHMM         PIC X(10).                               
011900                                                                          
012000 01  WS-IDARTNR                  PIC  X(9)   VALUE ZERO.                  
012100 01  FILLER REDEFINES WS-IDARTNR.                                         
012200     03 FILLER                   PIC 9(5).                                
012300     03 WS-ARTSIFFRA             PIC 9(1).                                
012400        88 ART-0                 VALUE 6.                                 
012500        88 ART-1                 VALUE 4  7.                              
012600        88 ART-2                 VALUE 5  8.                              
012700        88 ART-3                 VALUE 9.                                 
012800     03 FILLER                   PIC 9(3).                                
012900                                                                          
013000*- - - - - - - - - - - - - - - - - - - - CORE LABEL LAYOUT                
013100 01  FILLER                      PIC X(16)   VALUE 'CORE LABEL'.          
013200 01  CORE-DATE.                                                           
013300     03  LBL-TIAAAA              PIC X(4).                                
013400     03  FILLER                  PIC X       VALUE '-'.                   
013500     03  LBL-TIMM                PIC X(2).                                
013600     03  FILLER                  PIC X       VALUE '-'.                   
013700     03  LBL-TIDD                PIC X(2).                                
013800                                                                          
013900 01  CORE-TIME.                                                           
014000     03  LBL-TIHH                PIC X(2).                                
014100     03  FILLER                  PIC X       VALUE ':'.                   
014200     03  LBL-TIMIN               PIC X(2).                                
014300                                                                          
014400*01  -COPY WWDCKONS                                                       
014500                                                                          
014600 01   TEST-IDARTNR             PIC 9(9)  COMP-3.                          
014700*01  FILLER  -COPY WWBYT03  -RED  TEST-IDARTNR.                           
014800*01  FILLER  -COPY WWBYT16  -RED  TEST-IDARTNR                            
014900      EJECT                                                               
015000                                                                          
015100*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
015200 01  GENERELLA-SUBPROGRAM.                                                
015300     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
015400     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
015500     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
015600     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
015700     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
015800     03  W005WDK7                PIC X(8)    VALUE 'W005WDK7'.            
016000     03  WZ04CRUL                PIC X(8)    VALUE 'WZ04CRUL'.            
016100     03  WZ01SEND                PIC X(8)    VALUE 'WZ01SEND'.            
016200     EJECT                                                                
016300*    --- PARAMETRAR TILL W005WDK7                                         
016400 01 FILLER                       PIC X(8)    VALUE 'W005WDK7'.            
016500*   -COPY W005WDK7                                                        
016600     EJECT                                                                
017100*    --- PARAMETRAR TIL SUBPROGRAM WDATKONV                               
017200 01  FILLER                      PIC X(16)   VALUE 'WDATAREA'.            
017300     SKIP2                                                                
017400*01  -COPY WDATAREA.                                                      
017500     EJECT                                                                
017600*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
017700*01 -COPY WMEDAREA                                                        
017800     SKIP3                                                                
017900 01  FILLER                      PIC X(16)   VALUE 'WZ04CRUL'.            
018000*01 -COPY WZ04CRUL                                                        
018100                                                                          
018200 01  FILLER                      PIC X(16)   VALUE 'WZ01SEND'.            
018300*01 -COPY WZ01SEND                                                        
018400 01  FILLER                      PIC X(08)   VALUE 'W3017201'.            
018500 01  SEND-AREA-TO-CORE-LABEL.                                             
018600*03  -COPY W3017201                                                       
018700*                                                                         
018800                                                                          
018900 01  HDR-AREA.                                                            
019000*    03 -COPY WZ01REQU -PRE HDR-                                          
019100*    03 -COPY WZ04HDR                                                     
019200                                                                          
019300     EJECT                                                                
019400 01  MESSAGE-CODES.                                                       
019500     03  ERR-CORR-HILITE-FLDS    PIC X(3)    VALUE '001'.                 
019600     03  ERR-CONFLICT            PIC X(3)    VALUE '002'.                 
019700     03  INF-PRESS-PF11          PIC X(3)    VALUE '003'.                 
019800     03  ERR-PF11-AND-NO-DATA    PIC X(3)    VALUE '011'.                 
019900     03  WRONG-CODE              PIC X(3)    VALUE '013'.                 
020000     03  WRONG-STATUS            PIC X(3)    VALUE '079'.                 
020100     03  INF-UPDATE-DONE         PIC X(3)    VALUE '101'.                 
020200     03  INF-FIRST-PAGE          PIC X(3)    VALUE '006'.                 
020300     03  INF-MORE-INFO-EXISTS    PIC X(3)    VALUE '105'.                 
020400     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
020500     03  ORDER-MISSING           PIC X(3)    VALUE '054'.                 
020600     03  PARTNO-MISSING          PIC X(3)    VALUE '769'.                 
020700     03  MISSING-KOLLI           PIC X(3)    VALUE '758'.                 
020800     03  ERR-NOT-ALLOWED         PIC X(3)    VALUE '007'.                 
020900     03  ERR-ALREADY-EXISTS      PIC X(3)    VALUE '245'.                 
021000     03  ERR-LOW-STOCK           PIC X(3)    VALUE '322'.                 
021100     03  ERR-NOT-NUMERIC         PIC X(3)    VALUE '020'.                 
021200     03  NOT-A-CORE-NO           PIC X(3)    VALUE '211'.                 
021300     03  ERR-NOT-ZERO            PIC X(3)    VALUE '724'.                 
021400     03  KOLLI-EXISTS            PIC X(3)    VALUE '721'.                 
021500     03  USER-NOT-ALLOWED        PIC X(3)    VALUE '405'.                 
021600     03  WRONG-PARTNO            PIC X(3)    VALUE '768'.                 
021700     03  INF-PRINT-REQUESTED     PIC X(3)    VALUE '118'.                 
021800     03  ERR-WRONG-PRINTER       PIC X(3)    VALUE '772'.                 
021900     03  ERR-WRONG-CMD-CODE      PIC X(3)    VALUE '304'.                 
022000                                                                          
022100     EJECT                                                                
022200 01  PROG-TO-PROG-SW.                                                     
022300*03 -COPY WMSGSOP                                                         
022400     EJECT                                                                
022500*    --- PARAMETRAR TILL SUBPROGRAM W005INIT                              
022600*                                                                         
022700 01  FILLER                      PIC X(16)   VALUE 'WMSGINIT'.            
022800     SKIP3                                                                
022900*01 -COPY WMSGINIT                                                        
023000     EJECT                                                                
023100*    --- AREA MED DATA SOM SKA SPARAS MELLAN DIALOGSTEGEN                 
023200*                                                                         
023300 01  SPAR-AREA.                                                           
023400     03  SPAR-IDTRANS             PIC  X(4)  VALUE '3184'.                
023500     03  SPAR-IDARTNR-ENTER       PIC S9(9)  COMP-3 VALUE +0.             
023600     03  SPAR-IDKOLLI-ENTER       PIC S9(5)  COMP-3 VALUE +0.             
023700     03  SPAR-IDARTNR-NEXT        PIC S9(9)  COMP-3 VALUE +0.             
023800     03  SPAR-IDKOLLI-NEXT        PIC S9(5)  COMP-3 VALUE +0.             
023900     EJECT                                                                
024000*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
024100*                                                                         
024200 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
024300     SKIP3                                                                
024400*01  MID -COPY W3I18401                                                   
024500     EJECT                                                                
024600 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
024700     SKIP3                                                                
024800*01  -COPY WMSGAREA                                                       
024900     EJECT                                                                
025000     03  MOD REDEFINES MSG-AREA.                                          
025100*      05  -COPY W3O18401                                                 
025200     EJECT                                                                
025300 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
025400     SKIP3                                                                
025500*01  -COPY WMFSAREA                                                       
025600     EJECT                                                                
025700*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
025800*                                                                         
025900     EJECT                                                                
026000 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
026100     SKIP3                                                                
026200 01  NYCKLAR-TILL-DLI.                                                    
026300                                                                          
026400     03   W-WDGXKEY-3171-X.                                               
026500         05  W-IDHTYP            PIC X(4)   VALUE '3171'.                 
026600         05  FILLER              PIC X(26)  VALUE LOW-VALUE.              
026700                                                                          
026800     03  W-IDFAKT-X.                                                      
026900         05 W-IDFAKT             PIC S9(7) VALUE ZERO COMP-3.             
027000                                                                          
027100     03  W-IDKOLLI-X.                                                     
027200         05 W-IDKOLLI            PIC S9(5) VALUE ZERO COMP-3.             
027300                                                                          
027400     03  W-IDARTNR-OBJ-X.                                                 
027500         05  W-IDARTNR-OBJ       PIC S9(9)  VALUE ZERO COMP-3.            
027600                                                                          
027700     03  W-IDARTNR-X.                                                     
027800         05  W-IDARTNR           PIC S9(9)    VALUE ZERO COMP-3.          
027900                                                                          
028000     03  W-IDDC-X.                                                        
028100         05  W-IDDC              PIC X(2)   VALUE SPACE.                  
028200                                                                          
028300     03  W-IDDC-BEH-X.                                                    
028400         05  W-IDDC-BEH          PIC X(2)   VALUE SPACE.                  
028500                                                                          
028600     03   W-IDSKYLT-X.                                                    
028700         05  W-IDSKYLT           PIC X(3)   VALUE SPACE.                  
028800                                                                          
028900     03  W-IDKOLLI-MIN-X.                                                 
029000         05 W-IDKOLLI-MIN        PIC S9(5) VALUE ZERO COMP-3.             
029100                                                                          
029200     03  W-IDKOLLI-MAX-X.                                                 
029300         05 W-IDKOLLI-MAX        PIC S9(5) VALUE +99999 COMP-3.           
029400                                                                          
029500     03  W-KDTRSTAT-X.                                                    
029600         05 W-KDTRSTAT           PIC S9    VALUE ZERO COMP-3.             
029700                                                                          
029800     03  W-IDARTNRO-MIN-X.                                                
029900         05 W-IDARTNRO-MIN     PIC S9(9) VALUE ZERO COMP-3.               
030000     SKIP2                                                                
030100     03  W-IDARTNRO-MAX-X.                                                
030200         05 W-IDARTNRO-MAX     PIC S9(9) VALUE +999999999 COMP-3.         
030300                                                                          
030400     03  W-IDDC-B6-X.                                                     
030500         05 W-IDDC-B6                  PIC X(2).                          
030600     03  W-IDDC-B6-REC-X.                                                 
030700         05 W-IDDC-B6-REC              PIC X(2).                          
030800     03  W-IDDC-B6-SEND-X.                                                
030900         05 W-IDDC-B6-SEND             PIC X(2).                          
031000                                                                          
031100     SKIP2                                                                
031200*    --- STATUS-KOD FRÅN IMS                                              
031300 01  STATUS-WS                   PIC XX.                                  
031400     88  SEGMENT-FINNS                       VALUE '  '.                  
031500     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
031600     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
031700     88  SEGMENT-DATA                        VALUE 'GA'.                  
031800     88  BAS-SLUT                            VALUE 'GB'.                  
031900     SKIP2                                                                
032000 01  GODK-STATUSKODER.                                                    
032100     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
032200     SKIP3                                                                
032300 01  SSA1                        PIC X(64).                               
032400 01  SSA2                        PIC X(64).                               
032500 01  SSA3                        PIC X(64).                               
032600 01  SSA4                        PIC X(64).                               
032700     EJECT                                                                
032800*    --- IMS FUNKTIONSKODER                                               
032900*01  -COPY W0003                                                          
033000     EJECT                                                                
033100*    ---  DLI INPUT-OUTPUT AREA                                           
033200     SKIP3                                                                
033300 01  FILLER                PIC X(16)   VALUE 'DLI-IO-3172'.               
033400 01  DLI-IO-3172.                                                         
033500*    03  -COPY WDGX3172                                                   
033600     EJECT                                                                
033700 01  FILLER                PIC X(16)   VALUE 'DLI-IO-3174'.               
033800 01  DLI-IO-3174.                                                         
033900*    03  -COPY WDGX3174                                                   
034000     EJECT                                                                
034100 01  FILLER                PIC X(16)   VALUE 'DLI-IO-3176'.               
034200 01  DLI-IO-3176.                                                         
034300*    03  -COPY WDGX3176                                                   
034400     EJECT                                                                
034500 01  FILLER                     PIC X(16)   VALUE 'DLI-IO-WDK601'.        
034600     SKIP3                                                                
034700 01  DLI-IO-WDK601.                                                       
034800*    03  -COPY WDK601  -PRE K6                                            
034900     EJECT                                                                
035000 01  FILLER                     PIC X(16)   VALUE 'DLI-IO-WDK611'.        
035100     SKIP3                                                                
035200 01  DLI-IO-WDK611.                                                       
035300*    03  -COPY WDK611                                                     
035400     EJECT                                                                
035500 01  FILLER                     PIC X(16)   VALUE 'DLI-IO-WDK701'.        
035600     SKIP3                                                                
035700 01  DLI-IO-WDK701.                                                       
035800*    03  -COPY WDK701                                                     
035900     EJECT                                                                
036000 01  FILLER                     PIC X(16)   VALUE 'DLI-IO-WDK711'.        
036100     SKIP3                                                                
036200 01  DLI-IO-WDK711.                                                       
036300*    03  -COPY WDK711                                                     
036400     EJECT                                                                
036500 01  FILLER         PIC X(16) VALUE  'DLI-IO-WDD311'.                     
036600 01  DLI-IO-WDD311.                                                       
036700*    03 -COPY WDD311                                                      
036800     EJECT                                                                
036900 01  FILLER         PIC X(16) VALUE  'DLI-IO-WDR901'.                     
037000 01  DLI-IO-WDR901.                                                       
037100*    03  -COPY WDR901                                                     
037200*      05  -COPY W510EKHA -RED FIL-WDR901-DATA                            
037300     EJECT                                                                
037400 01  FILLER         PIC X(16) VALUE  'DLI-IO-WDL901'.                     
037500 01  DLI-IO-WDL901.                                                       
037600*    03 -COPY WDL901                                                      
037700     EJECT                                                                
037800                                                                          
037900 01  FILLER               PIC X(16)   VALUE 'WDB601 AREA'.                
038000 01   DLI-IO-AREA-B601.                                                   
038100*     03  -COPY WDB601                                                    
038200                                                                          
038300 01  FILLER               PIC X(16)   VALUE 'WDB601 REC '.                
038400 01   DLI-IO-AREA-B601-REC.                                               
038500*     03  -COPY WDB601   -PRE REC-                                        
038600                                                                          
038700 01  FILLER               PIC X(16)   VALUE 'WDB601 SEND'.                
038800 01   DLI-IO-AREA-B601-SEND.                                              
038900*     03  -COPY WDB601   -PRE SEND-                                       
039000                                                                          
039100*                                                                         
039200 01  FILLER                  PIC X(16)   VALUE 'DB2-WS     '.             
039300 01  FILLER                  PIC X(16)   VALUE 'BYART-COPYTEXT'.          
039400*01  -COPY BYART -PRE BYART-                                              
039500     EJECT                                                                
039600 01  FILLER                  PIC X(16)   VALUE 'BYART-AREA'.              
039700       EXEC SQL INCLUDE BYART END-EXEC.                                   
039800     SKIP3                                                                
039900 01  FILLER                  PIC X(16) VALUE 'SQLCA-AREA'.                
040000       EXEC SQL INCLUDE SQLCA END-EXEC.                                   
040100*                        **** STATUS-KOD FRÅN DB2                         
040200 01  FILLER                     PIC X(16) VALUE 'SQLCODE-WS'.             
040300 01  DB2-WS.                                                              
040400   03  SQLCODE-WS                PIC 9(3) VALUE ZERO.                     
040500     88  RADER-FINNS                         VALUE 000.                   
040600     88  RADER-SAKNAS                        VALUE 100.                   
040700     88  904-KOD                             VALUE 904.                   
040800     SKIP1                                                                
040900   03  GODK-SQLCODESKODER.                                                
041000     05  GODK-SQLCODE OCCURS 5                                            
041100         INDEXED BY SQLCODE-IX PIC 999.                                   
041200     EJECT                                                                
041300     SKIP3                                                                
041400 LINKAGE SECTION.                                                         
041500*01  -COPY W0009   -PRE MSG-                                              
041600 EJECT                                                                    
041700*01  -COPY W0009   -PRE ALT-                                              
041800 EJECT                                                                    
041900*01  -COPY W0009   -PRE DISTRDOC-                                         
042000     EJECT                                                                
042100*01  -COPY W0008   -PRE WDP7-                                             
042200     05  FILLER                  PIC X.                                   
042300*01  -COPY W0008   -PRE WDK6-                                             
042400     05  FILLER                  PIC X.                                   
042500*01  -COPY W0008   -PRE WDK7-                                             
042600     05  FILLER                  PIC X.                                   
042900*01  -COPY W0008   -PRE WDD3-                                             
043000     05  FILLER                  PIC X.                                   
043100*01  -COPY W0008   -PRE 3171-                                             
043200     05  FILLER                  PIC X.                                   
043300*01  -COPY W0008   -PRE WDL9-                                             
043400     05  FILLER                  PIC X.                                   
043500*01  -COPY W0008   -PRE WDR9-                                             
043600     05  FILLER                  PIC X.                                   
043700*01  -COPY W0008   -PRE WDB6-                                             
043800     05  FILLER                  PIC X.                                   
043900     EJECT                                                                
044000 PROCEDURE DIVISION  USING MSG-PCB ALT-PCB DISTRDOC-PCB                   
044100       WDP7-PCB WDK6-PCB WDK7-PCB WDD3-PCB 3171-PCB                       
044200       WDL9-PCB WDR9-PCB WDB6-PCB.                                        
044300                                                                          
044400 MAIN SECTION.                                                            
044500     ENTRY 'DLITCBL' USING MSG-PCB ALT-PCB DISTRDOC-PCB                   
044600       WDP7-PCB WDK6-PCB WDK7-PCB WDD3-PCB 3171-PCB                       
044700       WDL9-PCB WDR9-PCB WDB6-PCB.                                        
044800                                                                          
044900     PERFORM IMS-GET-MSG                                                  
045000     IF SEGMENT-FINNS                                                     
045100       PERFORM A-INIT                                                     
045200       PERFORM B-KOLLA-NYCKLAR                                            
045300       IF NYCKLAR-OK                                                      
045400         IF MFS-UPDATE                                                    
045500           PERFORM G-KOLLA-INPUT                                          
045600           IF INDATA-OK                                                   
045700             PERFORM H-UPPDATERA                                          
045800           END-IF                                                         
045900         ELSE                                                             
046000           IF MFS-FIRST                                                   
046100             PERFORM C-FOERSTA-SIDA                                       
046200           ELSE                                                           
046300             IF MFS-NEXT                                                  
046400               PERFORM D-NAESTA-SIDA                                      
046500             ELSE                                                         
046600               PERFORM E-SAMMA-SIDA                                       
046700             END-IF                                                       
046800           END-IF                                                         
046900         END-IF                                                           
047000         PERFORM F-LAES-VISA-INFO                                         
047100       END-IF                                                             
047200       COMPUTE MSG-KVLL = LENGTH OF MOD-W3O18401 + 4                      
047300       PERFORM IMS-INSERT-MSG                                             
047400     END-IF                                                               
047500                                                                          
047600     MOVE ZERO TO RETURN-CODE                                             
047700     GOBACK                                                               
047800     .                                                                    
047900     EJECT                                                                
048000 A-INIT SECTION.                                                          
048100                                                                          
048200                                                                          
048300     IF MSG-DUBBLA-TRANSKODER                                             
048400       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W3I18401                 
048500       MOVE MSG-IDTRANS-2  TO MFS-IDTRANS                                 
048600       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
048700     ELSE                                                                 
048800       MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W3I18401                  
048900       MOVE MSG-IDTRANS-1  TO MFS-IDTRANS                                 
049000       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
049100     END-IF                                                               
049200                                                                          
049300     MOVE MSG-KDTRTYP TO MFS-KDTRTYP                                      
049400     MOVE MSG-IDPFK   TO MFS-IDPFK                                        
049500     MOVE MFS-IDTRANS TO W-IDTRANS                                        
049600                                                                          
049700     MOVE LOW-VALUE TO MSG-AREA                                           
049800     MOVE 'W3O184N1' TO MFS-IDMOD                                         
049900     MOVE '3184' TO MOD-IDTRANS                                           
050000     MOVE MFS-RENSA-FAELT TO MOD-TEMFSFEL MOD-TEMFSINF                    
050100     MOVE SPACE TO MOD-TEMFSINF                                           
050200                   MOD-TEMFSFEL                                           
050300                                                                          
050400     MOVE FUNCTION CURRENT-DATE(1:8) TO W-DAGENS-DATUM                    
050500                                                                          
050600     IF EGEN-MID OR HELP-MID                                              
050700       CONTINUE                                                           
050800     ELSE                                                                 
050900       MOVE SPACE TO MFS-KDTRTYP                                          
051000       MOVE '7' TO MFS-IDPFK                                              
051100     END-IF                                                               
051200     MOVE 'GB' TO MED-IDSKYLT                                             
051300     .                                                                    
051400     EJECT                                                                
051500 B-KOLLA-NYCKLAR SECTION.                                                 
051600                                                                          
051700     MOVE ALL '+'           TO MSGI-WMSGINIT                              
051800     MOVE '001'             TO MSGI-KDCALL                                
051900     MOVE MSG-LTERM-NAME    TO MSGI-IDLTERM-USER                          
052000     MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                                
052100     MOVE '3184'            TO MSGI-IDTRANS                               
052200                                                                          
052300     IF EGEN-MID                                                          
052400        MOVE MID-IDFAKT-IN TO MSGI-IDFAKT                                 
052500                              WS-IDFAKT                                   
052600        MOVE MID-IDDC-REC-IN TO MSGI-IDDC-REC                             
052700        IF MID-IDKOLLI-IN NOT = ALL '+'                                   
052800           IF MID-IDKOLLI-IN = ALL ' '                                    
052900              MOVE ALL ZERO TO MID-IDKOLLI-IN                             
053000           END-IF                                                         
053100           INSPECT MID-IDKOLLI-IN                                         
053200                   REPLACING LEADING ZERO BY SPACE                        
053300           MOVE MID-IDKOLLI-IN TO MSGI-IDKOLLI                            
053400                              WS-IDKOLLI                                  
053500        END-IF                                                            
053600        MOVE MID-FLINLI-IN TO MSGI-FLINLI                                 
053700        MOVE MID-KDPRT     TO MSGI-KDPRT                                  
053800     ELSE                                                                 
053900        INSPECT MID-IDFAKT-IN REPLACING LEADING SPACE BY ZERO             
054000        IF MID-IDFAKT-IN IS NUMERIC                                       
054100           MOVE MID-IDFAKT-IN TO MSGI-IDFAKT                              
054200                                 WS-IDFAKT                                
054300        END-IF                                                            
054400                                                                          
054500        IF MID-IDDC-REC-IN NOT = REC-DCS-IDDC                             
054600           MOVE MID-IDDC-REC-IN TO W-IDDC-B6-REC                          
054700           PERFORM IMS-GU-WDB601-REC                                      
054800        END-IF                                                            
054900        IF REC-DCS-KDDC > SPACE AND NOT REC-DCS-DDC                       
055000           MOVE MID-IDDC-REC-IN TO MSGI-IDDC-REC                          
055100        END-IF                                                            
055200        MOVE SPACES           TO REC-DCS-IDDC                             
055300                                 REC-DCS-KDDC                             
055400                                                                          
055500        INSPECT MID-IDKOLLI-IN REPLACING                                  
055600                   LEADING SPACE BY ZERO                                  
055700                                                                          
055800        IF MID-IDKOLLI-IN IS NUMERIC                                      
055900           MOVE MID-IDKOLLI-IN TO MSGI-IDKOLLI                            
056000                                  WS-IDKOLLI                              
056100        END-IF                                                            
056200                                                                          
056300        MOVE NEJ               TO MSGI-FLINLI                             
056400     END-IF                                                               
056500                                                                          
056600     CALL W005INIT USING MSGI-WMSGINIT WDP7-PCB                           
056700     MOVE MSGI-SPAR-AREA TO SPAR-AREA                                     
056800                                                                          
056900     MOVE MSGI-IDDC           TO W-IDDC-BEH                               
057000                                                                          
057100     MOVE SPACE TO MED-IDMFSFEL                                           
057200                   MED-IDMFSINF                                           
057300     MOVE JA TO NYCKLAR-SW                                                
057400                                                                          
057500     IF  MSGI-IDLAND-SPR = 'GB'                                           
057600       MOVE 'GB ' TO W-IDSKYLT                                            
057700     ELSE                                                                 
057800       MOVE 'S  ' TO W-IDSKYLT                                            
057900     END-IF                                                               
058000                                                                          
058100*    -- KONTROLL AV IDFAKT                                                
058200     MOVE MFS-RENSA-FAELT TO MOD-IDFAKT-IN                                
058300                                                                          
058400     IF MID-IDFAKT-IN NOT = ALL '+'                                       
058500        MOVE '7'     TO MFS-IDPFK                                         
058600        MOVE SPACE   TO MFS-KDTRTYP                                       
058700     END-IF                                                               
058800                                                                          
058900     IF MSGI-IDFAKT NUMERIC AND MSGI-IDFAKT > ZERO                        
059000        MOVE MSGI-IDFAKT    TO W-IDFAKT                                   
059100                               WS-IDFAKT                                  
059200     ELSE                                                                 
059300        MOVE NEJ            TO NYCKLAR-SW                                 
059400     END-IF                                                               
059500                                                                          
059600*    -- KONTROLL AV FLINLI                                                
059700     MOVE MFS-RENSA-FAELT TO MOD-FLINLI-IN                                
059800                                                                          
059900     IF MID-FLINLI-IN NOT = ALL '+'                                       
060000        MOVE '7'     TO MFS-IDPFK                                         
060100        MOVE SPACE   TO MFS-KDTRTYP                                       
060200     END-IF                                                               
060300                                                                          
060400     IF MSGI-FLINLI = 'N' OR 'J' OR 'Y'                                   
060500       CONTINUE                                                           
060600     ELSE                                                                 
060700       MOVE NEJ     TO NYCKLAR-SW                                         
060800     END-IF                                                               
060900                                                                          
061000*    -- KONTROLL AV IDDC                                                  
061100     MOVE MFS-RENSA-FAELT TO MOD-IDDC-REC-IN                              
061200                                                                          
061300     IF MID-IDDC-REC-IN NOT = ALL '+'                                     
061400        MOVE '7'     TO MFS-IDPFK                                         
061500        MOVE SPACE   TO MFS-KDTRTYP                                       
061600     END-IF                                                               
061700                                                                          
061800     IF MSGI-IDDC-REC   NOT = REC-DCS-IDDC                                
061900        MOVE MSGI-IDDC-REC   TO W-IDDC-B6-REC                             
062000        PERFORM IMS-GU-WDB601-REC                                         
062100     END-IF                                                               
062200     IF (REC-DCS-IDDC = WC-SDC-NL-ET) OR                                  
062300        REC-DCS-NDC-PF                                                    
062400       MOVE MSGI-IDDC-REC TO W-IDDC                                       
062500     ELSE                                                                 
062600       MOVE NEJ     TO NYCKLAR-SW                                         
062700       MOVE MID-IDDC-REC-IN TO MOD-TEMFSINF                               
062800     END-IF                                                               
062900                                                                          
063000*    -- KONTROLL AV IDKOLLI                                               
063100     MOVE MFS-RENSA-FAELT TO MOD-IDKOLLI-IN                               
063200                                                                          
063300     IF MID-IDKOLLI-IN NOT = ALL '+'                                      
063400        MOVE '7'     TO MFS-IDPFK                                         
063500        MOVE SPACE   TO MFS-KDTRTYP                                       
063600     END-IF                                                               
063700                                                                          
063800     INSPECT MSGI-IDKOLLI REPLACING LEADING SPACE BY ZERO                 
063900     IF MSGI-IDKOLLI NUMERIC AND MSGI-IDKOLLI > ZERO                      
064000        MOVE MSGI-IDKOLLI   TO W-IDKOLLI-MIN                              
064100                               W-IDKOLLI-MAX                              
064200                               WS-IDKOLLI                                 
064300     ELSE                                                                 
064400        MOVE ZERO           TO WS-IDKOLLI                                 
064500     END-IF                                                               
064600                                                                          
064700                                                                          
064800     IF GODK-MID                                                          
064900        MOVE MSGI-IDFAKT   TO MOD-IDFAKT-UT                               
065000        INSPECT MOD-IDFAKT-UT REPLACING LEADING ZERO BY SPACE             
065100        MOVE MSGI-IDDC-REC TO MOD-IDDC-REC-UT                             
065200        INSPECT MOD-IDDC-REC-UT REPLACING LEADING ZERO BY SPACE           
065300        MOVE WS-IDKOLLI    TO MOD-IDKOLLI-UT                              
065400        INSPECT MOD-IDKOLLI-UT REPLACING LEADING ZERO BY SPACE            
065500        MOVE MSGI-FLINLI   TO MOD-FLINLI-UT                               
065600        MOVE MSGI-KDPRT    TO MOD-KDPRT                                   
065700     ELSE                                                                 
065800        IF NYCKLAR-OK                                                     
065900           MOVE MSGI-IDDC-REC TO MOD-IDDC-REC-UT                          
066000           MOVE MSGI-IDFAKT   TO MOD-IDFAKT-UT                            
066100           MOVE WS-IDKOLLI    TO MOD-IDKOLLI-UT                           
066200           MOVE MSGI-FLINLI   TO MOD-FLINLI-UT                            
066300           MOVE MSGI-KDPRT    TO MOD-KDPRT                                
066400        END-IF                                                            
066500     END-IF                                                               
066600                                                                          
066700     IF NYCKLAR-FEL                                                       
066800       IF EGEN-MID                                                        
066900          MOVE ERR-WRONG-KEY TO MED-IDMFSFEL                              
067000          CALL WMEDKONV USING MED-WMEDAREA                                
067100          MOVE MED-MFSFEL TO MOD-TEMFSFEL                                 
067200       END-IF                                                             
067300       PERFORM MFS-RENSA-FAELT-UT                                         
067400     END-IF                                                               
067500     .                                                                    
067600     EJECT                                                                
067700 C-FOERSTA-SIDA SECTION.                                                  
067800                                                                          
067900     MOVE INF-FIRST-PAGE TO MED-IDMFSINF                                  
068000     CALL WMEDKONV USING MED-WMEDAREA                                     
068100     MOVE MED-MFSINF TO MOD-TEMFSFEL                                      
068200                                                                          
068300     PERFORM MFS-RENSA-FAELT-IN                                           
068400     .                                                                    
068500     EJECT                                                                
068600 D-NAESTA-SIDA SECTION.                                                   
068700                                                                          
068800     IF SPAR-IDTRANS = '3184'                                             
068900                                                                          
069000       MOVE SPAR-IDARTNR-NEXT TO W-IDARTNRO-MIN                           
069100       MOVE SPAR-IDKOLLI-NEXT TO W-IDKOLLI-MIN                            
069200                                                                          
069300     END-IF                                                               
069400     PERFORM MFS-RENSA-FAELT-IN                                           
069500     .                                                                    
069600     EJECT                                                                
069700 E-SAMMA-SIDA SECTION.                                                    
069800                                                                          
069900     IF SPAR-IDTRANS = '3184' OR '0551'                                   
070000       MOVE SPAR-IDARTNR-ENTER TO W-IDARTNRO-MIN                          
070100       MOVE SPAR-IDKOLLI-ENTER TO W-IDKOLLI-MIN                           
070200       MOVE +1 TO INDX                                                    
070300       PERFORM UNTIL INDX > MAX-INDX                                      
070400         IF MID-KVANTMOT (INDX) NOT = ALL '+' OR                          
070500            MID-KDCMD    (INDX) NOT = ALL '+'                             
070600           MOVE +13 TO INDX                                               
070700         ELSE                                                             
070800           ADD +1 TO INDX                                                 
070900         END-IF                                                           
071000       END-PERFORM                                                        
071100       IF INDX = 12 AND MID-IDARTNR-OBJ-NY = ALL '+'                      
071200                    AND MID-KVANTMOT-NY    = ALL '+'                      
071300                    AND MID-IDKOLLI-NY     = ALL '+'                      
071400                    AND MID-IDKOLLI-KLAR   = ALL '+'                      
071500                                                                          
071600        PERFORM MFS-RENSA-FAELT-IN                                        
071700       ELSE                                                               
071800         MOVE INF-PRESS-PF11 TO MED-IDMFSINF                              
071900         CALL WMEDKONV USING MED-WMEDAREA                                 
072000         MOVE MED-MFSINF TO MOD-TEMFSFEL                                  
072100         PERFORM MFS-RENSA-FAELT-IN                                       
072200         PERFORM EA-MID-INDATA-TILL-MOD                                   
072300       END-IF                                                             
072400     ELSE                                                                 
072500       PERFORM MFS-RENSA-FAELT-IN                                         
072600     END-IF                                                               
072700     .                                                                    
072800     EJECT                                                                
072900                                                                          
073000 EA-MID-INDATA-TILL-MOD SECTION.                                          
073100                                                                          
073200     PERFORM                                                              
073300     VARYING INDX FROM +1 BY +1                                           
073400       UNTIL INDX > MAX-INDX                                              
073500       IF MID-KDCMD (INDX) NOT = ALL '+'                                  
073600         MOVE MID-KDCMD (INDX)  TO MOD-KDCMD (INDX)                       
073700         MOVE MFS-ALFA-FAELT-RAETT                                        
073800                                TO MOD-KDCMD-ATTR (INDX)                  
073900       END-IF                                                             
074000     END-PERFORM                                                          
074100                                                                          
074200     IF MID-IDARTNR-OBJ-NY NOT = ALL '+'                                  
074300        MOVE MID-IDARTNR-OBJ-NY TO MOD-IDARTNR-OBJ-NY                     
074400        INSPECT MOD-IDARTNR-OBJ-NY REPLACING LEADING                      
074500        ZERO BY SPACE                                                     
074600        MOVE MFS-ALFA-FAELT-RAETT TO MOD-IDARTNR-OBJ-NY-ATTR              
074700     END-IF                                                               
074800     IF MID-KVANTMOT-NY NOT = ALL '+'                                     
074900        MOVE MID-KVANTMOT-NY TO MOD-KVANTMOT-NY                           
075000        MOVE MFS-NUM-FAELT-RAETT TO MOD-KVANTMOT-NY-ATTR                  
075100     END-IF                                                               
075200     IF MID-IDKOLLI-NY NOT = ALL '+'                                      
075300        MOVE MID-IDKOLLI-NY TO MOD-IDKOLLI-NY                             
075400        MOVE MFS-NUM-FAELT-RAETT TO MOD-IDKOLLI-NY-ATTR                   
075500     END-IF                                                               
075600     IF MID-IDKOLLI-KLAR NOT = ALL '+'                                    
075700        MOVE MID-IDKOLLI-KLAR TO MOD-IDKOLLI-KLAR                         
075800        MOVE MFS-NUM-FAELT-RAETT TO MOD-IDKOLLI-KLAR-ATTR                 
075900     END-IF                                                               
076000     .                                                                    
076100     EJECT                                                                
076200 F-LAES-VISA-INFO SECTION.                                                
076300                                                                          
076400     IF MFS-UPDATE                                                        
076500       IF UPDATED-YES                                                     
076600         CONTINUE                                                         
076700       ELSE                                                               
076800         MOVE SPAR-IDARTNR-ENTER TO W-IDARTNRO-MIN                        
076900         MOVE SPAR-IDKOLLI-ENTER TO W-IDKOLLI-MIN                         
077000       END-IF                                                             
077100     END-IF                                                               
077200                                                                          
077300     PERFORM IMS-GHU-WDGX3172                                             
077400     IF SEGMENT-SAKNAS                                                    
077500       PERFORM FA-INIT-SPAR-AREA                                          
077600       MOVE ORDER-MISSING TO MED-IDMFSFEL                                 
077700       CALL WMEDKONV USING MED-WMEDAREA                                   
077800       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
077900       PERFORM MFS-RENSA-FAELT-UT                                         
078000     ELSE                                                                 
078100       IF MSGI-FLINLI = 'N'                                               
078200         MOVE 3      TO W-KDTRSTAT                                        
078300         PERFORM IMS-GNP-WDGX3174-LT4                                     
078400       ELSE                                                               
078500         MOVE 4      TO W-KDTRSTAT                                        
078600         PERFORM IMS-GNP-WDGX3174-ST                                      
078700       END-IF                                                             
078800       IF SEGMENT-SAKNAS                                                  
078900         PERFORM FA-INIT-SPAR-AREA                                        
079000         MOVE MISSING-KOLLI TO MED-IDMFSFEL                               
079100         CALL WMEDKONV USING MED-WMEDAREA                                 
079200         MOVE MED-MFSFEL TO MOD-TEMFSFEL                                  
079300         PERFORM MFS-RENSA-FAELT-UT                                       
079400       ELSE                                                               
079500         MOVE +1 TO INDX                                                  
079600         MOVE 3174-IDKOLLI TO W-IDKOLLI-MIN                               
079700         PERFORM IMS-GNP-WDGX3176                                         
079800         MOVE +0           TO W-IDARTNRO-MIN                              
079900         PERFORM UNTIL INDX > MAX-INDX                                    
080000           IF SEGMENT-FINNS                                               
080100             IF INDX = +1                                                 
080200               MOVE 3174-IDKOLLI     TO SPAR-IDKOLLI-ENTER                
080300                                        SPAR-IDKOLLI-NEXT                 
080400               MOVE 3176-IDARTNR-OBJ TO SPAR-IDARTNR-ENTER                
080500                                        SPAR-IDARTNR-NEXT                 
080600             END-IF                                                       
080700             MOVE 3174-IDKOLLI       TO MOD-IDKOLLI (INDX)                
080800             MOVE 3176-IDARTNR-OBJ   TO MOD-IDARTNR-OBJ (INDX)            
080900                                        W-IDARTNR                         
081000             PERFORM IMS-GU-WDD311                                        
081100             MOVE TEXT-BEART         TO MOD-BEART    (INDX)               
081200             MOVE 3176-KVANTAL-DEB   TO MOD-KVLEVART (INDX)               
081300             MOVE 3176-KVANTMOT      TO MOD-KVANTMOT (INDX)               
081400                                                                          
081500             PERFORM IMS-GNP-WDGX3176                                     
081600             ADD 1 TO INDX                                                
081700             IF SEGMENT-SAKNAS AND INDX = +12                             
081800*** INDX 13 BETYDER ATT FLER KOLLIN-WDGX3174 KAN FINNAS                   
081900               MOVE +13 TO INDX                                           
082000             END-IF                                                       
082100           ELSE                                                           
082200             IF MSGI-FLINLI = 'N'                                         
082300               MOVE 3      TO W-KDTRSTAT                                  
082400               PERFORM IMS-GNP-WDGX3174-LT4                               
082500             ELSE                                                         
082600               MOVE 4      TO W-KDTRSTAT                                  
082700               PERFORM IMS-GNP-WDGX3174-ST                                
082800             END-IF                                                       
082900             IF SEGMENT-SAKNAS                                            
083000               IF INDX = 1                                                
083100                 PERFORM FA-INIT-SPAR-AREA                                
083200               END-IF                                                     
083300               PERFORM UNTIL INDX > MAX-INDX                              
083400                 MOVE MFS-RENSA-FAELT TO MOD-IDKOLLI     (INDX)           
083500                                         MOD-IDARTNR-OBJ (INDX)           
083600                                         MOD-BEART       (INDX)           
083700                                         MOD-KVLEVART    (INDX)           
083800                                         MOD-KVANTMOT    (INDX)           
083900                 MOVE MFS-CLOSE-FIELD TO MOD-KVANTMOT-ATTR (INDX)         
084000                                         MOD-KDCMD-ATTR  (INDX)           
084100                 ADD 1 TO INDX                                            
084200               END-PERFORM                                                
084300             ELSE                                                         
084400               MOVE 3174-IDKOLLI TO W-IDKOLLI-MIN                         
084500               PERFORM IMS-GNP-WDGX3176                                   
084600             END-IF                                                       
084700           END-IF                                                         
084800         END-PERFORM                                                      
084900                                                                          
085000         IF INDX = +13                                                    
085100           IF MSGI-FLINLI = 'N'                                           
085200             MOVE 3      TO W-KDTRSTAT                                    
085300             PERFORM IMS-GNP-WDGX3174-LT4                                 
085400           ELSE                                                           
085500             MOVE 4      TO W-KDTRSTAT                                    
085600             PERFORM IMS-GNP-WDGX3174-ST                                  
085700           END-IF                                                         
085800           IF SEGMENT-FINNS                                               
085900             MOVE 3174-IDKOLLI TO W-IDKOLLI-MIN                           
086000             PERFORM IMS-GNP-WDGX3176                                     
086100           END-IF                                                         
086200         END-IF                                                           
086300                                                                          
086400         IF SEGMENT-FINNS                                                 
086500           MOVE 3174-IDKOLLI         TO SPAR-IDKOLLI-NEXT                 
086600           MOVE 3176-IDARTNR-OBJ     TO SPAR-IDARTNR-NEXT                 
086700                                                                          
086800           MOVE INF-MORE-INFO-EXISTS TO MED-IDMFSINF                      
086900           CALL WMEDKONV USING MED-WMEDAREA                               
087000           MOVE MED-TEMFSINF TO MOD-TEMFSINF                              
087100         END-IF                                                           
087200                                                                          
087300       END-IF                                                             
087400     END-IF                                                               
087500     MOVE '002'          TO MSGI-KDCALL                                   
087600     MOVE '3184'         TO SPAR-IDTRANS                                  
087700     MOVE SPAR-AREA      TO MSGI-SPAR-AREA                                
087800     CALL W005INIT    USING MSGI-WMSGINIT WDP7-PCB                        
087900     .                                                                    
088000     EJECT                                                                
088100                                                                          
088200 FA-INIT-SPAR-AREA SECTION.                                               
088300                                                                          
088400     MOVE ZEROES                     TO SPAR-IDKOLLI-ENTER                
088500                                        SPAR-IDARTNR-ENTER                
088600                                        SPAR-IDKOLLI-NEXT                 
088700                                        SPAR-IDARTNR-NEXT                 
088800     .                                                                    
088900     EJECT                                                                
089000                                                                          
089100 G-KOLLA-INPUT SECTION.                                                   
089200                                                                          
089300     MOVE JA  TO INDATA-SW                                                
089400     MOVE NEJ TO ANTAL-SW                                                 
089500                 PRINT-SW                                                 
089600                                                                          
089700     PERFORM                                                              
089800     VARYING INDX FROM +1 BY +1                                           
089900       UNTIL INDX > MAX-INDX                                              
090000        IF MID-KVANTMOT (INDX) NOT = ALL '+'                              
090100           MOVE MID-KVANTMOT (INDX) TO WS-KVANTMOT                        
090200           INSPECT WS-KVANTMOT REPLACING LEADING SPACE BY ZERO            
090300           IF WS-KVANTMOT IS NUMERIC                                      
090400              MOVE JA TO ANTAL-SW                                         
090500           ELSE                                                           
090600              MOVE ERR-NOT-NUMERIC   TO MED-IDMFSINF                      
090700              MOVE NEJ TO INDATA-SW                                       
090800              MOVE INDX TO W-INDX                                         
090900              MOVE MFS-NUM-FAELT-FEL TO                                   
091000                   MOD-KVANTMOT-ATTR (W-INDX)                             
091100           END-IF                                                         
091200        END-IF                                                            
091300                                                                          
091400        IF MID-KDCMD (INDX) NOT = ALL '+' AND                             
091500           MID-KDCMD (INDX) NOT = SPACE                                   
091600           IF MID-KDCMD (INDX) = 'P'                                      
091700              MOVE JA TO PRINT-SW                                         
091800           ELSE                                                           
091900              MOVE ERR-WRONG-CMD-CODE TO MED-IDMFSINF                     
092000              MOVE NEJ TO INDATA-SW                                       
092100              MOVE MFS-ALFA-FAELT-FEL TO MOD-KDCMD-ATTR (INDX)            
092200              MOVE MID-KDCMD (INDX)   TO MOD-KDCMD (INDX)                 
092300           END-IF                                                         
092400        END-IF                                                            
092500     END-PERFORM                                                          
092600                                                                          
092700     IF INDATA-OK                                                         
092800        AND ANTAL-SW = NEJ                                                
092900        AND PRINT-SW = NEJ                                                
093000        AND MID-IDKOLLI-NY = ALL '+'                                      
093100        AND MID-IDARTNR-OBJ-NY = ALL '+'                                  
093200        AND MID-KVANTMOT-NY = ALL '+'                                     
093300        AND MID-IDKOLLI-KLAR = ALL '+'                                    
093400       MOVE ERR-PF11-AND-NO-DATA TO MED-IDMFSFEL                          
093500       CALL WMEDKONV USING MED-WMEDAREA                                   
093600       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
093700       MOVE MED-MFSINF TO MOD-TEMFSINF                                    
093800       PERFORM MFS-ROER-EJ-FAELT-UT                                       
093900       PERFORM MFS-ROER-EJ-FAELT-IN                                       
094000     ELSE                                                                 
094100*------ KONTROLL AV IDKOLLI-NY                                            
094200       IF MID-IDKOLLI-NY NOT = ALL '+'                                    
094300          INSPECT MID-IDKOLLI-NY                                          
094400                  REPLACING LEADING SPACE BY ZERO                         
094500                                                                          
094600          IF MID-IDKOLLI-NY IS NUMERIC                                    
094700             MOVE MFS-ALFA-FAELT-RAETT TO                                 
094800                                      MOD-IDKOLLI-NY-ATTR                 
094900             MOVE JA TO KOLLI-SW                                          
095000          ELSE                                                            
095100             MOVE NEJ TO INDATA-SW                                        
095200             MOVE MFS-ALFA-FAELT-FEL TO                                   
095300                                      MOD-IDKOLLI-NY-ATTR                 
095400          END-IF                                                          
095500       END-IF                                                             
095600                                                                          
095700*------ KONTROLL AV IDARTNR-OBJ-NY OCH KVANTMOT-NY                        
095800       IF MID-IDARTNR-OBJ-NY NOT = ALL '+'                                
095900          INSPECT MID-IDARTNR-OBJ-NY                                      
096000                  REPLACING LEADING SPACE BY ZERO                         
096100          IF MID-IDARTNR-OBJ-NY IS NUMERIC                                
096200             INSPECT MID-IDARTNR-OBJ-NY                                   
096300                     REPLACING LEADING ZERO BY SPACE                      
096400             MOVE MID-IDARTNR-OBJ-NY  TO TEST-IDARTNR                     
096500                                         W-IDARTNR-OBJ                    
096600                                         W-IDARTNR                        
096700                                                                          
096800             IF BYT03-OBJEKT                                              
096900                PERFORM IMS-GU-WDK611                                     
097000                IF SEGMENT-FINNS                                          
097100                AND CLAG-PRARTSJK > ZERO                                  
097200                   IF MID-KVANTMOT-NY NOT = ALL '+'                       
097300                      INSPECT MID-KVANTMOT-NY REPLACING                   
097400                      LEADING SPACE BY ZERO                               
097500                      IF MID-KVANTMOT-NY IS NUMERIC                       
097600                         MOVE MID-KVANTMOT-NY TO W-KVCLEAR                
097700                         MOVE MFS-ALFA-FAELT-RAETT TO                     
097800                              MOD-IDARTNR-OBJ-NY-ATTR                     
097900                              MOD-KVANTMOT-NY-ATTR                        
098000                         MOVE JA TO OBJEKT-SW                             
098100                         IF MID-KVANTMOT-NY = ZERO                        
098200                            MOVE NEJ TO INDATA-SW                         
098300                            MOVE ERR-NOT-ZERO TO MED-IDMFSINF             
098400                            MOVE MFS-ALFA-FAELT-RAETT TO                  
098500                                 MOD-IDARTNR-OBJ-NY-ATTR                  
098600                            MOVE MFS-ALFA-FAELT-FEL TO                    
098700                                 MOD-KVANTMOT-NY-ATTR                     
098800                         END-IF                                           
098900                      ELSE                                                
099000                         MOVE NEJ TO INDATA-SW                            
099100                         MOVE ERR-NOT-NUMERIC TO MED-IDMFSINF             
099200                         MOVE MFS-ALFA-FAELT-RAETT TO                     
099300                              MOD-IDARTNR-OBJ-NY-ATTR                     
099400                         MOVE MFS-ALFA-FAELT-FEL TO                       
099500                              MOD-KVANTMOT-NY-ATTR                        
099600                      END-IF                                              
099700                   END-IF                                                 
099800                ELSE                                                      
099900                   MOVE WRONG-PARTNO  TO MED-IDMFSINF                     
100000                   MOVE NEJ TO INDATA-SW                                  
100100                   MOVE MFS-ALFA-FAELT-FEL TO                             
100200                                  MOD-IDARTNR-OBJ-NY-ATTR                 
100300                   MOVE MFS-ALFA-FAELT-RAETT TO                           
100400                                  MOD-KVANTMOT-NY-ATTR                    
100500                END-IF                                                    
100600             ELSE                                                         
100700                MOVE NOT-A-CORE-NO TO MED-IDMFSINF                        
100800                MOVE NEJ TO INDATA-SW                                     
100900                MOVE MFS-ALFA-FAELT-FEL TO                                
101000                     MOD-IDARTNR-OBJ-NY-ATTR                              
101100                MOVE MFS-ALFA-FAELT-RAETT TO                              
101200                     MOD-KVANTMOT-NY-ATTR                                 
101300             END-IF                                                       
101400          ELSE                                                            
101500             MOVE NEJ TO INDATA-SW                                        
101600             MOVE MFS-ALFA-FAELT-FEL TO                                   
101700                  MOD-IDARTNR-OBJ-NY-ATTR                                 
101800                  MOD-KVANTMOT-NY-ATTR                                    
101900          END-IF                                                          
102000       END-IF                                                             
102100                                                                          
102200       IF NYTT-OBJEKT AND MID-IDKOLLI-KLAR NOT = ALL '+'                  
102300           MOVE NEJ TO INDATA-SW                                          
102400           MOVE ERR-CONFLICT   TO MED-IDMFSINF                            
102500       ELSE                                                               
102600         IF MID-IDKOLLI-KLAR NOT = ALL '+'                                
102700           IF MID-IDKOLLI-KLAR NUMERIC AND                                
102800              MID-IDKOLLI-KLAR > ZERO                                     
102900             MOVE JA TO KOLLI-KLAR-SW                                     
103000           ELSE                                                           
103100             MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSINF                    
103200             MOVE NEJ TO INDATA-SW                                        
103300             MOVE MFS-ALFA-FAELT-FEL TO MOD-IDKOLLI-KLAR-ATTR             
103400           END-IF                                                         
103500         END-IF                                                           
103600       END-IF                                                             
103700                                                                          
103800*------ IDARTNR-OBJ-NY OCH KVANTMOT-NY FÅR EJ ANGES OM                    
103900*------ IDKOLLI-NY EJ ANGIVITS                                            
104000       IF MID-IDKOLLI-NY = ALL '+'                                        
104100         IF MID-IDARTNR-OBJ-NY NOT = ALL '+'                              
104200           MOVE NEJ                TO INDATA-SW                           
104300           MOVE MISSING-KOLLI      TO MED-IDMFSINF                        
104400           MOVE MFS-ALFA-FAELT-FEL TO MOD-IDARTNR-OBJ-NY-ATTR             
104500                                      MOD-IDKOLLI-NY-ATTR                 
104600         END-IF                                                           
104700         IF MID-KVANTMOT-NY NOT = ALL '+'                                 
104800           MOVE NEJ TO             INDATA-SW                              
104900           MOVE MISSING-KOLLI      TO MED-IDMFSINF                        
105000           MOVE MFS-ALFA-FAELT-FEL TO MOD-KVANTMOT-NY-ATTR                
105100                                      MOD-IDKOLLI-NY-ATTR                 
105200         END-IF                                                           
105300       END-IF                                                             
105400                                                                          
105500       IF INDATA-OK   AND                                                 
105600          ((NYTT-KOLLI AND                                                
105700            NYTT-OBJEKT AND                                               
105800            MID-KVANTMOT-NY > ZERO) OR                                    
105900           PRINT-LABEL)                                                   
106000          PERFORM GA-KOLLA-PRINTER                                        
106100       END-IF                                                             
106200                                                                          
106300       IF INDATA-FEL                                                      
106400          MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL                       
106500          CALL WMEDKONV USING MED-WMEDAREA                                
106600          MOVE MED-MFSFEL TO MOD-TEMFSFEL                                 
106700          MOVE MED-MFSINF TO MOD-TEMFSINF                                 
106800          PERFORM MFS-ROER-EJ-FAELT-UT                                    
106900          PERFORM MFS-ROER-EJ-FAELT-IN                                    
107000       END-IF                                                             
107100     END-IF                                                               
107200     .                                                                    
107300     EJECT                                                                
107400 GA-KOLLA-PRINTER SECTION.                                                
107500                                                                          
107600     MOVE 'CORE-LABEL'    TO CRUL-IDOUTTYPE                               
107700     MOVE MSGI-KDPRT      TO CRUL-IDOUTREC                                
107800     CALL WZ04CRUL     USING CRUL-WZ04CRUL                                
107900                                                                          
108000     IF CRUL-KDRC = ZERO                                                  
108100       MOVE MFS-ALFA-FAELT-RAETT                                          
108200                          TO MOD-KDPRT-ATTR                               
108300     ELSE                                                                 
108400       MOVE MFS-ALFA-FAELT-FEL                                            
108500                          TO MOD-KDPRT-ATTR                               
108600       MOVE MSGI-KDPRT    TO MOD-KDPRT                                    
108700       MOVE NEJ           TO INDATA-SW                                    
108800       MOVE ERR-WRONG-PRINTER                                             
108900                          TO MED-IDMFSINF                                 
109000     END-IF                                                               
109100     .                                                                    
109200     EJECT                                                                
109300 H-UPPDATERA SECTION.                                                     
109400                                                                          
109500     PERFORM IMS-GHU-WDGX3172                                             
109600     IF SEGMENT-FINNS                                                     
109700        IF W-IDDC-BEH = 3172-IDDC-REC                                     
109800        AND 3172-KDTRSTAT = 3                                             
109900         MOVE 3172-IDDC-REC       TO W-3172-IDDC-REC                      
110000         IF 3172-IDDC-REC NOT = REC-DCS-IDDC                              
110100            MOVE 3172-IDDC-REC TO W-IDDC-B6-REC                           
110200            PERFORM IMS-GU-WDB601-REC                                     
110300         END-IF                                                           
110400         MOVE 3172-IDDC-SEND      TO W-3172-IDDC-SEND                     
110500         IF 3172-IDDC-SEND NOT = SEND-DCS-IDDC                            
110600            MOVE 3172-IDDC-SEND TO W-IDDC-B6-SEND                         
110700            PERFORM IMS-GU-WDB601-SEND                                    
110800         END-IF                                                           
110900         IF KOLLI-KLAR                                                    
111000            PERFORM HA-FAKTURA-KOLLI-KLAR                                 
111100         ELSE                                                             
111200            IF NYTT-OBJEKT                                                
111300            AND NYTT-KOLLI                                                
111400              PERFORM HB-NYTT-OBJEKT                                      
111500            END-IF                                                        
111600            IF NYTT-ANTAL OR PRINT-LABEL                                  
111700              IF MSGI-FLINLI = 'J' OR 'Y'                                 
111800                MOVE WRONG-STATUS   TO MED-IDMFSINF                       
111900                CALL WMEDKONV USING MED-WMEDAREA                          
112000                MOVE MED-MFSINF TO MOD-TEMFSINF                           
112100              ELSE                                                        
112200                PERFORM HC-NYTT-ANTAL                                     
112300              END-IF                                                      
112400            END-IF                                                        
112500         END-IF                                                           
112600         IF HEADER-WRITTEN                                                
112700           PERFORM S29-SEND-CLOSE                                         
112800         END-IF                                                           
112900        ELSE                                                              
113000           MOVE NEJ TO INDATA-SW                                          
113100           IF W-IDDC-BEH = 3172-IDDC-REC                                  
113200              MOVE WRONG-STATUS   TO MED-IDMFSINF                         
113300              CALL WMEDKONV USING MED-WMEDAREA                            
113400              MOVE MED-MFSINF TO MOD-TEMFSINF                             
113500           ELSE                                                           
113600              MOVE USER-NOT-ALLOWED TO MED-IDMFSINF                       
113700              CALL WMEDKONV USING MED-WMEDAREA                            
113800              MOVE MED-MFSINF TO MOD-TEMFSINF                             
113900           END-IF                                                         
114000        END-IF                                                            
114100     END-IF                                                               
114200     PERFORM MFS-RENSA-FAELT-IN                                           
114300     .                                                                    
114400     EJECT                                                                
114500 HA-FAKTURA-KOLLI-KLAR SECTION.                                           
114600                                                                          
114700     MOVE MID-IDKOLLI-KLAR TO W-IDKOLLI-MIN                               
114800                              W-IDKOLLI-MAX                               
114900     MOVE +3                 TO W-KDTRSTAT                                
115000     PERFORM IMS-GNP-WDGX3174-ST                                          
115100     IF SEGMENT-SAKNAS                                                    
115200       MOVE MISSING-KOLLI TO MED-IDMFSFEL                                 
115300       CALL WMEDKONV USING MED-WMEDAREA                                   
115400       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
115500       MOVE MED-MFSINF TO MOD-TEMFSINF                                    
115600       PERFORM MFS-ROER-EJ-FAELT-UT                                       
115700       PERFORM MFS-ROER-EJ-FAELT-IN                                       
115800     ELSE                                                                 
115900       PERFORM IMS-GNP-WDGX3176                                           
116000       PERFORM UNTIL SEGMENT-SAKNAS                                       
116100         MOVE 3176-IDARTNR-OBJ TO W-IDARTNR-OBJ                           
116200         MOVE 3176-KVANTMOT    TO WS-KVANTMOT-NUM                         
116300         COMPUTE W-KVAVVIK = 3176-KVANTAL-DEB -                           
116400                             3176-KVANTMOT                                
116500******************************************                                
116600**** MOTTAGANDE DC ÄR SDC 91          ****                                
116700******************************************                                
116800         IF REC-DCS-IDDC = WC-SDC-NL-ET                                   
116900           MOVE 3176-IDARTNR-OBJ TO W-IDARTNR                             
117000           MOVE NEJ     TO W-WDK611-FINNS                                 
117100           PERFORM IMS-GU-WDK601                                          
117200           IF SEGMENT-FINNS                                               
117300             PERFORM IMS-GNP-WDK611                                       
117400             IF SEGMENT-FINNS                                             
117500               MOVE JA  TO W-WDK611-FINNS                                 
117600             END-IF                                                       
117700           END-IF                                                         
117800                                                                          
117900           MOVE W-3172-IDDC-REC TO W-IDDC                                 
118000           PERFORM IMS-GU-WDK711                                          
118100           IF SEGMENT-FINNS                                               
118200             PERFORM HAB-UPPDATERA-LAGER-SDC                              
118300           ELSE                                                           
118400             PERFORM HAC-NYA-SDC21-SEGMENT-WDK7                           
118500             IF INDATA-OK                                                 
118600               PERFORM HAB-UPPDATERA-LAGER-SDC                            
118700             END-IF                                                       
118800           END-IF                                                         
118900         END-IF                                                           
119000                                                                          
119100         PERFORM IMS-GNP-WDGX3176                                         
119200         MOVE ZERO TO W-KVAVVIK                                           
119300       END-PERFORM                                                        
119400       PERFORM IMS-GHU-WDGX3172                                           
119500       PERFORM IMS-GHNP-WDGX3174-ST3                                      
119600       IF SEGMENT-FINNS                                                   
119700         MOVE 4               TO 3174-KDTRSTAT                            
119800         PERFORM IMS-REPL-WDGX3174                                        
119900       END-IF                                                             
120000       MOVE +0              TO W-IDKOLLI-MIN                              
120100       MOVE +99999          TO W-IDKOLLI-MAX                              
120200       MOVE 4               TO W-KDTRSTAT                                 
120300       PERFORM IMS-GNP-WDGX3174-ST-LT4                                    
120400       IF SEGMENT-SAKNAS                                                  
120500***********ALLA KOLLIN INLAGDA DVS HELA FAKTURAN INLÄGGES                 
120600         PERFORM IMS-GHU-WDGX3172                                         
120700         MOVE 4             TO 3172-KDTRSTAT                              
120800         PERFORM IMS-REPL-WDGX3172                                        
120900       END-IF                                                             
121000       MOVE INF-UPDATE-DONE TO MED-IDMFSINF                               
121100       CALL WMEDKONV USING MED-WMEDAREA                                   
121200       MOVE MED-MFSINF TO MOD-TEMFSINF                                    
121300       MOVE JA TO UPDATED-SW                                              
121400       PERFORM HAE-AKTIVERA-RUTIN-I-SOP                                   
121500     END-IF                                                               
121600     .                                                                    
121700     EJECT                                                                
121800 HAB-UPPDATERA-LAGER-SDC SECTION.                                         
121900                                                                          
122000     MOVE W-3172-IDDC-REC   TO W-IDDC                                     
122100     PERFORM IMS-GHU-WDK711                                               
122200     COMPUTE SLAG-KVLS = SLAG-KVLS +                                      
122300             WS-KVANTMOT-NUM                                              
122400     PERFORM IMS-REPL-WDK711                                              
122500***SOLLOGGNING-KVLS                                                       
122600     PERFORM S03-FLYTTA-LOGG-WDL9                                         
122700     MOVE WS-KVANTMOT-NUM   TO LOGG-KVART-SALDO                           
122800     MOVE '+'               TO LOGG-IDTECKEN-KVLS                         
122900     PERFORM S01-UPPDATERA-LOGG                                           
123000     MOVE SPACE             TO LOGG-WDL901                                
123100                                                                          
123200     PERFORM IMS-GHU-WDK711                                               
123300     COMPUTE SLAG-KVAKS-SDC = SLAG-KVAKS-SDC -                            
123400             3176-KVANTAL-DEB                                             
123500     PERFORM IMS-REPL-WDK711                                              
123600***SOLLOGGNING-KVAKS                                                      
123700     PERFORM S03-FLYTTA-LOGG-WDL9                                         
123800     MOVE 3176-KVANTAL-DEB  TO LOGG-KVART-SALDO                           
123900     MOVE '-'               TO LOGG-IDTECKEN-KVAKS                        
124000     PERFORM S01-UPPDATERA-LOGG                                           
124100     MOVE SPACE             TO LOGG-WDL901                                
124200     COMPUTE  EKH-KVANTAL  = 3176-KVANTAL-DEB -                           
124300                             3176-KVANTMOT                                
124400     IF 3176-KVANTAL-DEB NOT = 3176-KVANTMOT                              
124500       PERFORM S05-SKAPA-EKOLOGG-WDR9                                     
124600     END-IF                                                               
124700     .                                                                    
124800     EJECT                                                                
124900 HAC-NYA-SDC21-SEGMENT-WDK7 SECTION.                                      
125000                                                                          
125100*    LÄGG UPP NYA SEGMENT                                                 
125200     IF W-WDK611-FINNS = JA                                               
125300       MOVE ALL '+'          TO WDK7-W005WDK7                             
125400       MOVE 'WDK711'         TO WDK7-IDSEGM                               
125500       MOVE 3176-IDARTNR-OBJ TO WDK7-IDARTNR-KFB                          
125600       MOVE W-3172-IDDC-REC  TO WDK7-IDDC-KFB                             
125700                                WDK7-IDDC                                 
125800       MOVE 'N'              TO WDK7-FLREFILL                             
125900                                                                          
126000       CALL W005WDK7 USING WDK7-W005WDK7 WDB6-PCB WDK6-PCB                
126100                                         WDK7-PCB                         
126700     END-IF                                                               
126800     .                                                                    
126900     EJECT                                                                
127000 HAE-AKTIVERA-RUTIN-I-SOP SECTION.                                        
127100******************************************************************        
127200*               AKTIVERING AV RUTIN W371S6 I SOP                 *        
127300*          IDFAKT SKICKAS MED SOM SYMBOLISK PARAMETER            *        
127400******************************************************************        
127500                                                                          
127600     MOVE '3184'       TO MSGSOP-IDTRANS                                  
127700     MOVE MFS-KDMFSFOR TO MSGSOP-KDMFSFOR                                 
127800     MOVE 'W371S6'     TO MSGSOP-IDPROCESS                                
127900     MOVE 'O'          TO MSGSOP-KDSOPFUNK                                
128000                                                                          
128100                                                                          
128200     STRING 'IDDC(' MSGI-IDDC-REC ') IDFAKT(' WS-IDFAKT                   
128300            ') IDKOLLI(' MID-IDKOLLI-KLAR ')'                             
128400            DELIMITED BY SIZE INTO MSGSOP-TESYMBV                         
128500                                                                          
128600     PERFORM IMS-INSERT-ALTMSG                                            
128700     .                                                                    
128800     EJECT                                                                
128900 HB-NYTT-OBJEKT SECTION.                                                  
129000                                                                          
129100     MOVE NEJ TO OBJEKT-SW                                                
129200     MOVE NEJ TO KOLLI-SW                                                 
129300     MOVE MID-IDKOLLI-NY     TO W-IDKOLLI                                 
129400     MOVE MID-IDARTNR-OBJ-NY TO W-IDARTNR-OBJ                             
129500                                                                          
129600     PERFORM IMS-GHU-KOLLI-OBJEKT                                         
129700     IF SEGMENT-FINNS                                                     
129800        PERFORM HBA-NYTT-ANTAL                                            
129900     ELSE                                                                 
130000*************************************************************             
130100**** OBS ! GODSET KAN KOMMA FRÅN DC 61 ELLER 62       ****                
130200*************************************************************             
130300        IF REC-DCS-IDDC = WC-SDC-NL-ET                                    
130400        AND (SEND-DCS-NDC-PF)                                             
130500              MOVE W-3172-IDDC-SEND     TO W-IDDC                         
130600              PERFORM IMS-GU-WDK711                                       
130700              IF SEGMENT-SAKNAS                                           
130800************************************************************              
130900********** DEN NYA ARTIKELN SAKNAS PÅ ARTIKEL    ***********              
131000********** REGISTERET FÖR DET SENDANDE LAGRET    ***********              
131100********** DETTA HÄNDER NÄR MAN INTE ÄR ÖVERENS MED ********              
131200********** DET SENDANDE DC:ET OM VALET AV ARTIKEL   ********              
131300************************************************************              
131400                 PERFORM HBC-NYA-SEGMENT-WDK7                             
131500              END-IF                                                      
131600************************************************************              
131700*** OM LAGERSALDO STÖRRE ELLER LIKA MED DEN NYA KVANTITETEN*              
131800*** OM SLAG-KVLS >= W-KVCLEAR                              *              
131900************************************************************              
132000              MOVE MID-IDKOLLI-NY                                         
132100                                TO W-IDKOLLI                              
132200              PERFORM IMS-GHU-KOLLI                                       
132300              IF SEGMENT-FINNS                                            
132400                 IF 3174-KDTRSTAT = +4                                    
132500                   MOVE NEJ TO INDATA-SW                                  
132600                   MOVE WRONG-STATUS   TO MED-IDMFSFEL                    
132700                   CALL WMEDKONV USING MED-WMEDAREA                       
132800                   MOVE MED-MFSFEL TO MOD-TEMFSFEL                        
132900                 ELSE                                                     
133000                   MOVE MID-KVANTMOT-NY                                   
133100                               TO 3176-KVANTMOT                           
133200                   MOVE ZERO  TO 3176-KVANTAL-DEB                         
133300                   MOVE MID-IDARTNR-OBJ-NY                                
133400                               TO 3176-IDARTNR-OBJ                        
133500                   PERFORM IMS-ISRT-KOLLI-RAD                             
133600                       IF SEGMENT-FINNS-REDAN                             
133700                                MOVE NEJ TO INDATA-SW                     
133800                                MOVE KOLLI-EXISTS  TO MED-IDMFSFEL        
133900                                CALL WMEDKONV USING MED-WMEDAREA          
134000                                MOVE MED-MFSFEL TO MOD-TEMFSFEL           
134100                        ELSE                                              
134200                                MOVE JA TO OBJEKT-SW                      
134300                                MOVE JA TO KOLLI-SW                       
134400                       END-IF                                             
134500                 END-IF                                                   
134600              ELSE                                                        
134700                   MOVE MID-IDKOLLI-NY                                    
134800                                  TO 3174-IDKOLLI                         
134900                   MOVE +3   TO 3174-KDTRSTAT                             
135000                   MOVE ZERO TO 3174-VKORDBTO-KOLLI                       
135100                   MOVE ZERO TO 3174-VLORDBTO-KOLLI                       
135200                   PERFORM IMS-ISRT-KOLLI                                 
135300                   PERFORM IMS-GHU-KOLLI                                  
135400                   IF SEGMENT-FINNS                                       
135500                       MOVE MID-KVANTMOT-NY                               
135600                                   TO 3176-KVANTMOT                       
135700                       MOVE ZERO TO 3176-KVANTAL-DEB                      
135800                       MOVE MID-IDARTNR-OBJ-NY                            
135900                                   TO 3176-IDARTNR-OBJ                    
136000                       PERFORM IMS-ISRT-KOLLI-RAD                         
136100                       IF SEGMENT-FINNS-REDAN                             
136200                                MOVE NEJ TO INDATA-SW                     
136300                                MOVE KOLLI-EXISTS  TO MED-IDMFSFEL        
136400                                CALL WMEDKONV USING MED-WMEDAREA          
136500                                MOVE MED-MFSFEL TO MOD-TEMFSFEL           
136600                        ELSE                                              
136700                                MOVE JA TO OBJEKT-SW                      
136800                                MOVE JA TO KOLLI-SW                       
136900                       END-IF                                             
137000                   END-IF                                                 
137100              END-IF                                                      
137200        END-IF                                                            
137300     END-IF                                                               
137400     IF NYTT-OBJEKT                                                       
137500        IF INDATA-OK                                                      
137600           MOVE MID-IDARTNR-OBJ-NY TO WS-IDARTNR-OBJ                      
137700           MOVE MID-KVANTMOT-NY    TO WS-PRT-QTY                          
137800           PERFORM S06-PRINT-LABELS                                       
137900        END-IF                                                            
138000                                                                          
138100        IF INDATA-OK                                                      
138200           MOVE INF-UPDATE-DONE    TO MED-IDMFSINF                        
138300           MOVE JA TO UPDATED-SW                                          
138400           CALL WMEDKONV        USING MED-WMEDAREA                        
138500           MOVE MED-MFSINF         TO MOD-TEMFSINF                        
138600           PERFORM MFS-RENSA-FAELT-IN                                     
138700        END-IF                                                            
138800     END-IF                                                               
138900     IF INDATA-FEL                                                        
139000        PERFORM MFS-ROER-EJ-FAELT-UT                                      
139100        PERFORM MFS-ROER-EJ-FAELT-IN                                      
139200     END-IF                                                               
139300     .                                                                    
139400     EJECT                                                                
139500                                                                          
139600 HBA-NYTT-ANTAL SECTION.                                                  
139700     PERFORM IMS-GU-KOLLI                                                 
139800     IF SEGMENT-FINNS                                                     
139900       IF 3174-KDTRSTAT = +4                                              
140000          MOVE NEJ TO INDATA-SW                                           
140100          MOVE WRONG-STATUS   TO MED-IDMFSFEL                             
140200          CALL WMEDKONV USING MED-WMEDAREA                                
140300          MOVE MED-MFSFEL TO MOD-TEMFSFEL                                 
140400       ELSE                                                               
140500          PERFORM IMS-GHNP-WDGX3176                                       
140600          IF SEGMENT-FINNS                                                
140700            MOVE MID-KVANTMOT-NY TO W-KVANTMOT-NY                         
140800            COMPUTE W-KVANTMOT = W-KVANTMOT-NY +                          
140900                                 3176-KVANTMOT                            
141000            MOVE W-KVANTMOT TO 3176-KVANTMOT                              
141100            PERFORM IMS-REPL-3171-LINE                                    
141200            MOVE JA TO OBJEKT-SW                                          
141300            MOVE JA TO KOLLI-SW                                           
141400          ELSE                                                            
141500            MOVE NEJ TO INDATA-SW                                         
141600            MOVE PARTNO-MISSING TO MED-IDMFSFEL                           
141700            CALL WMEDKONV USING MED-WMEDAREA                              
141800            MOVE MED-MFSFEL TO MOD-TEMFSFEL                               
141900          END-IF                                                          
142000       END-IF                                                             
142100     ELSE                                                                 
142200       MOVE NEJ TO INDATA-SW                                              
142300       MOVE MISSING-KOLLI TO MED-IDMFSFEL                                 
142400       CALL WMEDKONV USING MED-WMEDAREA                                   
142500       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
142600     END-IF                                                               
142700     .                                                                    
142800     EJECT                                                                
142900                                                                          
143000 HBC-NYA-SEGMENT-WDK7 SECTION.                                            
143100                                                                          
143200* LÄGG UPP NYA SEGMENT                                                    
143300     MOVE ALL '+'          TO WDK7-W005WDK7                               
143400     MOVE 'WDK711'         TO WDK7-IDSEGM                                 
143500     MOVE W-IDARTNR-OBJ    TO WDK7-IDARTNR-KFB                            
143600     MOVE W-IDDC           TO WDK7-IDDC-KFB                               
143700                              WDK7-IDDC                                   
143800     MOVE 'N'              TO WDK7-FLREFILL                               
143900                                                                          
144000     CALL W005WDK7 USING WDK7-W005WDK7 WDB6-PCB WDK6-PCB                  
144100                                       WDK7-PCB                           
144700     .                                                                    
144800     EJECT                                                                
144900 HC-NYTT-ANTAL SECTION.                                                   
145000                                                                          
145100     MOVE NEJ TO ANTAL-SW                                                 
145200     MOVE +1 TO INDX                                                      
145300     PERFORM UNTIL INDX > MAX-INDX                                        
145400       INSPECT MID-KVANTMOT (INDX)                                        
145500       REPLACING LEADING SPACE BY ZERO                                    
145600       IF MID-KVANTMOT (INDX) NOT = ALL '+' OR                            
145700          MID-KDCMD    (INDX) NOT = ALL '+'                               
145800         MOVE MID-IDARTNR-OBJ (INDX) TO W-IDARTNR-OBJ                     
145900         MOVE MID-IDKOLLI     (INDX) TO W-IDKOLLI                         
146000                                                                          
146100         PERFORM IMS-GHU-KOLLI-OBJEKT                                     
146200         IF SEGMENT-FINNS                                                 
146300           IF MID-KVANTMOT (INDX) NOT = ALL '+'                           
146400             MOVE MID-KVANTMOT (INDX) TO W-KVANTMOT                       
146500             COMPUTE W-KVAVVIK = W-KVANTMOT -                             
146600                                 3176-KVANTAL-DEB                         
146700             IF REC-DCS-IDDC = WC-SDC-NL-ET                               
146800               MOVE MID-KVANTMOT (INDX) TO 3176-KVANTMOT                  
146900               MOVE JA TO ANTAL-SW                                        
147000               IF 3176-KVANTMOT = ZERO AND                                
147100                  3176-KVANTAL-DEB = ZERO                                 
147200                  PERFORM IMS-DLET-3171                                   
147300               ELSE                                                       
147400                  PERFORM IMS-REPL-3171-LINE                              
147500               END-IF                                                     
147600             ELSE                                                         
147700               MOVE W-3172-IDDC-SEND  TO W-IDDC                           
147800             END-IF                                                       
147900           END-IF                                                         
148000           IF MID-KDCMD (INDX) NOT = ALL '+'                              
148100             MOVE MID-IDARTNR-OBJ (INDX) TO WS-IDARTNR-OBJ                
148200             MOVE 3176-KVANTMOT          TO WS-PRT-QTY                    
148300             PERFORM S06-PRINT-LABELS                                     
148400           END-IF                                                         
148500         END-IF                                                           
148600       END-IF                                                             
148700       ADD +1 TO INDX                                                     
148800     END-PERFORM                                                          
148900     IF NYTT-ANTAL                                                        
149000        MOVE INF-UPDATE-DONE TO MED-IDMFSINF                              
149100        MOVE JA TO UPDATED-SW                                             
149200        CALL WMEDKONV USING MED-WMEDAREA                                  
149300        MOVE MED-MFSINF TO MOD-TEMFSINF                                   
149400     END-IF                                                               
149500     .                                                                    
149600     EJECT                                                                
149700 S01-UPPDATERA-LOGG SECTION.                                              
149800     PERFORM IMS-ISRT-WDL901                                              
149900     IF SEGMENT-FINNS-REDAN                                               
150000        PERFORM UNTIL NOT SEGMENT-FINNS-REDAN                             
150100          ADD -1 TO LOGG-IDSEKVNR                                         
150200          PERFORM IMS-ISRT-WDL901                                         
150300        END-PERFORM                                                       
150400     END-IF                                                               
150500     .                                                                    
150600     EJECT                                                                
150700 S03-FLYTTA-LOGG-WDL9 SECTION.                                            
150800                                                                          
150900* LÄGGER UPP SALDOLOGG I WDL9                                             
151000     MOVE W-IDARTNR-OBJ         TO LOGG-IDARTNR                           
151100     COMPUTE LOGG-DAREGDAT-9KOMPL = 99999999 - W-DAGENS-DATUM             
151200     ACCEPT TRANS-TID FROM TIME                                           
151300     COMPUTE LOGG-TIKLOCK-9KOMPL  = 999999999 - TRANS-TID                 
151400     MOVE 9                       TO LOGG-IDSEKVNR                        
151500     MOVE SLAG-IDDC               TO LOGG-IDDC                            
151600     MOVE 'EXCH'                  TO LOGG-IDHUVTYP                        
151700     MOVE 'OBJ'                   TO LOGG-IDSUBTYP                        
151800     MOVE IDPGM                   TO LOGG-IDPGM                           
151900     MOVE '3184'                  TO LOGG-IDTRANS                         
152000     MOVE MSG-SIGNON-USERID       TO LOGG-IDUSER                          
152100     MOVE SPACE                   TO LOGG-REF                             
152200     MOVE WS-IDFAKT               TO LOGG-IDFAKT                          
152300     MOVE SPACE                   TO LOGG-IDTECKEN-KVAKS-PAV              
152400     MOVE SPACE                   TO LOGG-IDTECKEN-KVEFRS                 
152500     MOVE SPACE                   TO LOGG-IDTECKEN-KVAKS                  
152600     MOVE SPACE                   TO LOGG-IDTECKEN-KVLS                   
152700     MOVE SLAG-KVLS               TO LOGG-KVLS                            
152800     MOVE SLAG-KVAKS-SDC          TO LOGG-KVAKS                           
152900     MOVE SLAG-KVAKS-PAV          TO LOGG-KVAKS-PAV                       
153000     MOVE SLAG-KVEFRS             TO LOGG-KVEFRS                          
153100     MOVE ZERO                    TO LOGG-DAREGDAT-LADD                   
153200                                                                          
153300     .                                                                    
153400     EJECT                                                                
153500 S05-SKAPA-EKOLOGG-WDR9 SECTION.                                          
153600                                                                          
153700     MOVE IDPGM               TO FIL-IDPGM                                
153800     MOVE FUNCTION CURRENT-DATE (1:8) TO FIL-DAREGDAT                     
153900     MOVE FUNCTION CURRENT-DATE (9:8) TO FIL-TIKLOCK                      
154000     MOVE 1                   TO FIL-IDSEKVNR                             
154100     MOVE 'W510EKHA'          TO FIL-IDCPYTXT                             
154200     MOVE MSG-SIGNON-USERID   TO FIL-IDUSER                               
154300     MOVE '202'               TO EKH-KDEKHHT                              
154400     MOVE '203'               TO EKH-KDEKSHT                              
154500     MOVE 'DET'               TO EKH-KDEKNIVA                             
154600     MOVE 3172-IDDC-REC       TO EKH-IDDC-REC                             
154700     MOVE 3172-IDDC-SEND      TO EKH-IDDC-SEND                            
154800                                                                          
154900     IF 3172-IDDC-SEND NOT = DCS-IDDC                                     
155000        MOVE 3172-IDDC-SEND   TO W-IDDC-B6                                
155100        PERFORM IMS-GU-WDB601                                             
155200     END-IF                                                               
155300     IF DCS-CDC                                                           
155400        MOVE '8014'           TO EKH-IDDISTR                              
155500     ELSE                                                                 
155600        MOVE '8013'           TO EKH-IDDISTR                              
155700     END-IF                                                               
155800     MOVE ZERO                TO EKH-IDKUNDNR                             
155900     MOVE 3172-IDFAKT         TO EKH-IDVERGL                              
156000     MOVE 3172-DAANKDAG       TO EKH-DAVERDAT                             
156100     IF EKH-DAVERDAT = ZERO                                               
156200       MOVE FIL-DAREGDAT      TO EKH-DAVERDAT                             
156300     END-IF                                                               
156400     MOVE 3176-IDARTNR-OBJ    TO EKH-IDARTNR                              
156500     MOVE SPACE               TO EKH-FLLSBOK                              
156600     MOVE 'SEK'               TO EKH-KDVALISO                             
156700     MOVE 1                   TO EKH-PRKURS                               
156800     MOVE ZERO                TO EKH-PRARTNTO                             
156900     MOVE ZERO                TO EKH-PRARTSJK                             
157000     MOVE ZERO                TO EKH-PRHEMTAG                             
157100     IF W-WDK611-FINNS = JA                                               
157200        MOVE K6ART-KDSORT     TO EKH-KDSORT                               
157300        MOVE K6ART-KDPRODSL   TO EKH-KDPRODSL                             
157400        MOVE CLAG-PRARTSTD    TO EKH-PRARTSTD                             
157500        MOVE CLAG-KDPSLLOC    TO EKH-KDPSLLOC                             
157600     ELSE                                                                 
157700        MOVE SPACE            TO EKH-KDSORT                               
157800        MOVE ZERO             TO EKH-KDPRODSL                             
157900                                 EKH-PRARTSTD                             
158000                                 EKH-KDPSLLOC                             
158100     END-IF                                                               
158200     MOVE ZERO                TO EKH-PRLANDCO                             
158300                                 EKH-PRINK                                
158400                                 EKH-PRDIRLON                             
158500                                 EKH-PRDMTRL                              
158600                                 EKH-PROVRPAL                             
158700                                 EKH-IDORDNR5                             
158800     MOVE ZERO                TO EKH-SUBEL                                
158900     MOVE SPACE               TO EKH-IDTRANS                              
159000     MOVE SPACE               TO EKH-BEVAT                                
159100                                 EKH-IDANALYS                             
159200                                 EKH-KDANMORS                             
159300                                 EKH-KDTRADP                              
159400                                 EKH-IDKST                                
159500                                 EKH-IDLEVNR                              
159600     MOVE ZERO                TO EKH-KDFRAKT                              
159700                                 EKH-IDKONTO                              
159800                                 EKH-SUVAT                                
159900     MOVE ZERO                TO EKH-DAAVIDAT                             
160000                                 EKH-IDAVINR                              
160100                                 EKH-KDAVVTYP                             
160200                                 EKH-KDRT                                 
160300                                 EKH-KVANTMOT                             
160400                                 EKH-KVAVIS                               
160500     MOVE SPACE               TO EKH-FLDCET                               
160600     MOVE SPACE               TO EKH-IDKUNDRF                             
160600     MOVE SPACE               TO EKH-IDFAKT-EXP                           
160700     PERFORM IMS-ISRT-WDR901                                              
160800     PERFORM UNTIL SEGMENT-FINNS                                          
160900       ADD +1  TO FIL-IDSEKVNR                                            
161000       PERFORM IMS-ISRT-WDR901                                            
161100     END-PERFORM                                                          
161200     .                                                                    
161300     EJECT                                                                
161400 S06-PRINT-LABELS SECTION.                                                
161500     MOVE FUNCTION CURRENT-DATE(1:12)                                     
161600                                 TO WS-CURRENT-DATE-TIME                  
161700     MOVE '1        '            TO CORE-IDAFPRCD                         
161800     MOVE WS-YEAR                TO LBL-TIAAAA                            
161900     MOVE WS-MONTH               TO LBL-TIMM                              
162000     MOVE WS-DAY                 TO LBL-TIDD                              
162100     MOVE WS-HOUR                TO LBL-TIHH                              
162200     MOVE WS-MINUTE              TO LBL-TIMIN                             
162300     MOVE CORE-DATE              TO CORE-PRINT-DATE                       
162400     MOVE CORE-TIME              TO CORE-TIHHMM                           
162500     MOVE MSGI-IDUSER            TO CORE-IDUSER                           
62600      MOVE WS-IDARTNR-OBJ         TO CORE-IDARTNR-OBJ                      
62700                                     TEST-IDARTNR                          
162800                                    WS-IDARTNR                            
162800                                    W-IDARTNR                             
081000     PERFORM IMS-GU-WDD311                                                
081000       IF SEGMENT-FINNS                                                   
162500         MOVE TEXT-BEART         TO CORE-BEART                            
162500       END-IF                                                             
162900     IF BYT16-RADIO                                                       
163000       IF BYT16-RADIO-EXTRA                                               
163100         MOVE 0                  TO WS-ARTSIFFRA                          
163200       ELSE                                                               
163300         MOVE 3                  TO WS-ARTSIFFRA                          
163400       END-IF                                                             
163500     ELSE                                                                 
163600       IF ART-0                                                           
163700         MOVE 0                  TO WS-ARTSIFFRA                          
163800       ELSE                                                               
163900         IF ART-1                                                         
164000           MOVE 1                TO WS-ARTSIFFRA                          
164100         ELSE                                                             
164200           IF ART-2                                                       
164300             MOVE 2              TO WS-ARTSIFFRA                          
164400           ELSE                                                           
164500             IF ART-3                                                     
164600               MOVE 3            TO WS-ARTSIFFRA                          
164700             END-IF                                                       
164800           END-IF                                                         
164900         END-IF                                                           
165000       END-IF                                                             
165100     END-IF                                                               
165200     MOVE WS-IDARTNR             TO BYART-IDARTNR-BYT                     
165300     PERFORM DB2-SELECT-BYART                                             
165400     IF RADER-FINNS                                                       
165500       MOVE BYART-ADLAGOMR       TO CORE-ADLAGOMR                         
165600       MOVE BYART-ADGANG         TO CORE-ADGANG                           
165700       MOVE BYART-ADPLATS        TO CORE-ADPLATS                          
165800     ELSE                                                                 
165900       MOVE ZERO                 TO CORE-ADLAGOMR                         
166000                                    CORE-ADGANG                           
166100                                    CORE-ADPLATS                          
166200     END-IF                                                               
166300                                                                          
166400     IF HEADER-NOT-WRITTEN                                                
166500       PERFORM S21-SEND-OPEN                                              
166600       PERFORM S22-PUT-HEADER                                             
166700       SET HEADER-WRITTEN        TO TRUE                                  
166800     END-IF                                                               
166900                                                                          
167000     PERFORM                                                              
167100     VARYING PRT-IX FROM 1 BY 1                                           
167200       UNTIL PRT-IX > WS-PRT-QTY                                          
167300       PERFORM S25-PUT-LINE                                               
167400     END-PERFORM                                                          
167500     IF HEADER-WRITTEN AND WS-PRT-QTY > ZERO                              
167600       MOVE INF-PRINT-REQUESTED  TO MED-IDMFSFEL                          
167700       CALL WMEDKONV          USING MED-WMEDAREA                          
167800       MOVE MED-MFSFEL           TO MOD-TEMFSFEL                          
167900     END-IF                                                               
168000                                                                          
168100     .                                                                    
168200     EJECT                                                                
168300 S21-SEND-OPEN SECTION.                                                   
168400     MOVE 'CARPARTS.DAP.DISTRDOC' TO SEND-ADDISPABS                       
168500     MOVE 'OPEN'                  TO SEND-KDFUNC                          
168600     CALL WZ01SEND             USING SEND-CONTROL-AREA                    
168700                                     SEND-OPEN-AREA                       
168800     IF SEND-KDRC > ZERO                                                  
168900       MOVE SEND-KDRC            TO KDRC-DISPLAY                          
169000       STRING 'WZ01SEND OPEN ERROR RC= ' KDRC-DISPLAY                     
169100       DELIMITED BY SIZE INTO FELTEXT                                     
169200       CALL FELLOG                                                        
169300     END-IF                                                               
169400     .                                                                    
169500     EJECT                                                                
169600 S22-PUT-HEADER SECTION.                                                  
169700     MOVE 1                       TO HDR-REQU-IDMSGVER                    
169800     MOVE SPACE                   TO HDR-REQU-KDPGMACT                    
169900     MOVE IDPGM                   TO HDR-REQU-IDUSER                      
170000     MOVE 'CORE-LABEL'            TO HDR-IDOUTTYPE                        
170100     MOVE MSGI-KDPRT              TO HDR-IDOUTREC                         
170200     MOVE WS-TIYYMMDDHHMM         TO HDR-IDLIST                           
170300     MOVE 'PUT'                   TO SEND-KDFUNC                          
170400     MOVE LENGTH OF HDR-AREA      TO SEND-KVDLEN                          
170500     CALL WZ01SEND             USING SEND-CONTROL-AREA                    
170600                                     SEND-KVDLEN                          
170700                                     HDR-AREA                             
170800     IF SEND-KDRC > ZERO                                                  
170900       MOVE SEND-KDRC            TO KDRC-DISPLAY                          
171000       STRING 'WZ01SEND GET  ERROR RC= ' KDRC-DISPLAY                     
171100       DELIMITED BY SIZE       INTO FELTEXT                               
171200       CALL FELLOG                                                        
171300     END-IF                                                               
171400     .                                                                    
171500     EJECT                                                                
171600 S25-PUT-LINE SECTION.                                                    
171700     MOVE 'PUT'                   TO SEND-KDFUNC                          
171800     MOVE LENGTH OF SEND-AREA-TO-CORE-LABEL TO SEND-KVDLEN                
171900     CALL WZ01SEND             USING SEND-CONTROL-AREA                    
172000                                     SEND-KVDLEN                          
172100                                     SEND-AREA-TO-CORE-LABEL              
172200     IF SEND-KDRC > ZERO                                                  
172300       MOVE SEND-KDRC            TO KDRC-DISPLAY                          
172400       STRING 'WZ01SEND GET  ERROR RC= ' KDRC-DISPLAY                     
172500       DELIMITED BY SIZE       INTO FELTEXT                               
172600       CALL FELLOG                                                        
172700     END-IF                                                               
172800     .                                                                    
172900     SKIP2                                                                
173000 S29-SEND-CLOSE SECTION.                                                  
173100     MOVE 'CLOSE'                TO SEND-KDFUNC                           
173200     CALL WZ01SEND            USING SEND-CONTROL-AREA                     
173300     IF SEND-KDRC > 0                                                     
173400       MOVE SEND-KDRC            TO KDRC-DISPLAY                          
173500       STRING 'WZ01RECV CLOSE ERROR RC= ' KDRC-DISPLAY                    
173600       DELIMITED BY SIZE       INTO FELTEXT                               
173700       CALL FELLOG                                                        
173800     END-IF                                                               
173900     .                                                                    
174000     EJECT                                                                
174100 MFS-RENSA-FAELT-UT SECTION.                                              
174200                                                                          
174300     MOVE +1 TO INDX                                                      
174400     PERFORM UNTIL INDX > MAX-INDX                                        
174500         MOVE MFS-RENSA-FAELT TO MOD-KDCMD   (INDX)                       
174600                                 MOD-IDKOLLI (INDX)                       
174700                                 MOD-IDARTNR-OBJ (INDX)                   
174800                                 MOD-BEART   (INDX)                       
174900                                 MOD-KVLEVART (INDX)                      
175000                                 MOD-KVANTMOT (INDX)                      
175100         ADD +1 TO INDX                                                   
175200     END-PERFORM                                                          
175300                                                                          
175400     MOVE MFS-RENSA-FAELT TO     MOD-IDARTNR-OBJ-NY                       
175500                                 MOD-IDKOLLI-NY                           
175600                                 MOD-KVANTMOT-NY                          
175700                                 MOD-IDKOLLI-KLAR                         
175800     .                                                                    
175900     EJECT                                                                
176000 MFS-RENSA-FAELT-IN SECTION.                                              
176100                                                                          
176200*    --- ALLA INDATA-FÄLT                                                 
176300     MOVE +1 TO INDX                                                      
176400     PERFORM UNTIL INDX > MAX-INDX                                        
176500         MOVE MFS-RENSA-FAELT TO MOD-KDCMD   (INDX)                       
176600                                 MOD-IDKOLLI (INDX)                       
176700                                 MOD-IDARTNR-OBJ (INDX)                   
176800                                 MOD-BEART   (INDX)                       
176900                                 MOD-KVLEVART (INDX)                      
177000                                 MOD-KVANTMOT (INDX)                      
177100         ADD +1 TO INDX                                                   
177200     END-PERFORM                                                          
177300     MOVE MFS-RENSA-FAELT TO    MOD-IDKOLLI-NY                            
177400                                MOD-IDARTNR-OBJ-NY                        
177500                                MOD-KVANTMOT-NY                           
177600                                MOD-IDKOLLI-KLAR                          
177700     .                                                                    
177800     EJECT                                                                
177900 MFS-ROER-EJ-FAELT-UT  SECTION.                                           
178000                                                                          
178100*    --- ALLA UTDATA-FÄLT                                                 
178200*    --- INKL BLÄDDRINGSNYCKLAR OCH RAD-DATA                              
178300     MOVE MFS-ROER-EJ-FAELT TO   MOD-IDFAKT-UT                            
178400                                 MOD-IDDC-REC-UT                          
178500     MOVE +1 TO INDX                                                      
178600     PERFORM UNTIL INDX > MAX-INDX                                        
178700       MOVE MFS-ROER-EJ-FAELT TO MOD-IDARTNR-OBJ (INDX)                   
178800                                 MOD-KDCMD       (INDX)                   
178900       ADD +1 TO INDX                                                     
179000     END-PERFORM                                                          
179100     SKIP2                                                                
179200     .                                                                    
179300 MFS-ROER-EJ-FAELT-IN  SECTION.                                           
179400                                                                          
179500**   --- ALLA INDATA-FÄLT                                                 
179600     MOVE MFS-ROER-EJ-FAELT TO MOD-IDKOLLI-NY                             
179700                               MOD-IDARTNR-OBJ-NY                         
179800                               MOD-KVANTMOT-NY                            
179900                               MOD-IDKOLLI-KLAR                           
180000     .                                                                    
180100     EJECT                                                                
180200* --- IMS SEKTIONER ---                                                   
180300     SKIP3                                                                
180400 IMS-GET-MSG SECTION.                                                     
180500                                                                          
180600     MOVE '  QC' TO GODK-STATUSKODER                                      
180700     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
180800     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
180900     PERFORM IMS-STATUSKONTROLL                                           
181000     .                                                                    
181100     SKIP3                                                                
181200 IMS-INSERT-MSG SECTION.                                                  
181300                                                                          
181400     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
181500     MOVE SPACE TO GODK-STATUSKODER                                       
181600     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
181700     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
181800     PERFORM IMS-STATUSKONTROLL                                           
181900     .                                                                    
182000     EJECT                                                                
182100 IMS-GU-WDD311  SECTION.                                                  
182200     STRING 'WDD301  (WDD3BSEQ =' W-IDARTNR-X ')'                         
182300          DELIMITED BY SIZE INTO SSA1                                     
182400     STRING 'WDD311  (IDSKYLT  =' W-IDSKYLT-X ')'                         
182500          DELIMITED BY SIZE INTO SSA2                                     
182600     MOVE '  GE' TO GODK-STATUSKODER                                      
182700     CALL CBLTDLI USING GU WDD3-PCB DLI-IO-WDD311 SSA1 SSA2               
182800     MOVE WDD3-STATUS-CODE TO STATUS-WS                                   
182900     PERFORM IMS-STATUSKONTROLL                                           
183000     .                                                                    
183100     SKIP2                                                                
183200 IMS-GHU-WDGX3172 SECTION.                                                
183300                                                                          
183400     STRING 'WDR401  (WDGXKEY  =' W-WDGXKEY-3171-X ')'                    
183500          DELIMITED BY SIZE INTO SSA1                                     
183600     STRING 'WDGX3172(IDFAKT   =' W-IDFAKT-X ')'                          
183700          DELIMITED BY SIZE INTO SSA2                                     
183800     MOVE '  GE' TO GODK-STATUSKODER                                      
183900     CALL CBLTDLI USING GHU 3171-PCB DLI-IO-3172 SSA1 SSA2                
184000     MOVE 3171-STATUS-CODE TO STATUS-WS                                   
184100     PERFORM IMS-STATUSKONTROLL                                           
184200     .                                                                    
184300     SKIP2                                                                
184400                                                                          
184500 IMS-GNP-WDGX3174-ST SECTION.                                             
184600                                                                          
184700     STRING 'WDGX3174(IDKOLLI >=' W-IDKOLLI-MIN-X                         
184800                    '&IDKOLLI <=' W-IDKOLLI-MAX-X                         
184900                    '&KDTRSTAT =' W-KDTRSTAT-X ')'                        
185000          DELIMITED BY SIZE INTO SSA1                                     
185100     MOVE '  GE' TO GODK-STATUSKODER                                      
185200     CALL CBLTDLI USING GNP 3171-PCB DLI-IO-3174 SSA1                     
185300     MOVE 3171-STATUS-CODE TO STATUS-WS                                   
185400     PERFORM IMS-STATUSKONTROLL                                           
185500     .                                                                    
185600     SKIP2                                                                
185700                                                                          
185800 IMS-GNP-WDGX3174-LT4 SECTION.                                            
185900                                                                          
186000     STRING 'WDGX3174(IDKOLLI >=' W-IDKOLLI-MIN-X                         
186100                    '&IDKOLLI <=' W-IDKOLLI-MAX-X                         
186200                    '&KDTRSTAT<=' W-KDTRSTAT-X ')'                        
186300          DELIMITED BY SIZE INTO SSA1                                     
186400     MOVE '  GE' TO GODK-STATUSKODER                                      
186500     CALL CBLTDLI USING GNP 3171-PCB DLI-IO-3174 SSA1                     
186600     MOVE 3171-STATUS-CODE TO STATUS-WS                                   
186700     PERFORM IMS-STATUSKONTROLL                                           
186800     .                                                                    
186900     EJECT                                                                
187000 IMS-GNP-WDGX3176 SECTION.                                                
187100                                                                          
187200     STRING 'WDGX3174(IDKOLLI  =' W-IDKOLLI-MIN-X ')'                     
187300          DELIMITED BY SIZE INTO SSA1                                     
187400     STRING 'WDGX3176(IDARTNRO>=' W-IDARTNRO-MIN-X                        
187500                    '&IDARTNRO<=' W-IDARTNRO-MAX-X ')'                    
187600          DELIMITED BY SIZE INTO SSA2                                     
187700     MOVE '  GE' TO GODK-STATUSKODER                                      
187800     CALL CBLTDLI USING GNP 3171-PCB DLI-IO-3176 SSA1 SSA2                
187900     MOVE 3171-STATUS-CODE TO STATUS-WS                                   
188000     PERFORM IMS-STATUSKONTROLL                                           
188100     .                                                                    
188200     EJECT                                                                
188300 IMS-GHNP-WDGX3176 SECTION.                                               
188400                                                                          
188500     STRING 'WDGX3176(IDARTNRO =' W-IDARTNR-OBJ-X ')'                     
188600          DELIMITED BY SIZE INTO SSA1                                     
188700     MOVE '  GE' TO GODK-STATUSKODER                                      
188800     CALL CBLTDLI USING GHNP 3171-PCB DLI-IO-3176 SSA1                    
188900     MOVE 3171-STATUS-CODE TO STATUS-WS                                   
189000     PERFORM IMS-STATUSKONTROLL                                           
189100     .                                                                    
189200     EJECT                                                                
189300                                                                          
189400 IMS-GU-KOLLI SECTION.                                                    
189500                                                                          
189600     STRING 'WDR401  (WDGXKEY  =' W-WDGXKEY-3171-X ')'                    
189700          DELIMITED BY SIZE INTO SSA1                                     
189800     STRING 'WDGX3172(IDFAKT   =' W-IDFAKT-X ')'                          
189900          DELIMITED BY SIZE INTO SSA2                                     
190000     STRING 'WDGX3174(IDKOLLI  =' W-IDKOLLI-X ')'                         
190100          DELIMITED BY SIZE INTO SSA3                                     
190200     MOVE '  GE' TO GODK-STATUSKODER                                      
190300     CALL CBLTDLI USING GU 3171-PCB DLI-IO-3174 SSA1 SSA2 SSA3            
190400     MOVE 3171-STATUS-CODE TO STATUS-WS                                   
190500     PERFORM IMS-STATUSKONTROLL                                           
190600     .                                                                    
190700     EJECT                                                                
190800                                                                          
190900 IMS-GHU-KOLLI SECTION.                                                   
191000                                                                          
191100     STRING 'WDR401  (WDGXKEY  =' W-WDGXKEY-3171-X ')'                    
191200          DELIMITED BY SIZE INTO SSA1                                     
191300     STRING 'WDGX3172(IDFAKT   =' W-IDFAKT-X ')'                          
191400          DELIMITED BY SIZE INTO SSA2                                     
191500     STRING 'WDGX3174(IDKOLLI  =' W-IDKOLLI-X ')'                         
191600          DELIMITED BY SIZE INTO SSA3                                     
191700     MOVE '  GE' TO GODK-STATUSKODER                                      
191800     CALL CBLTDLI USING GHU 3171-PCB DLI-IO-3174 SSA1 SSA2 SSA3           
191900     MOVE 3171-STATUS-CODE TO STATUS-WS                                   
192000     PERFORM IMS-STATUSKONTROLL                                           
192100     .                                                                    
192200     EJECT                                                                
192300                                                                          
192400 IMS-GHU-KOLLI-OBJEKT SECTION.                                            
192500                                                                          
192600     STRING 'WDR401  (WDGXKEY  =' W-WDGXKEY-3171-X ')'                    
192700          DELIMITED BY SIZE INTO SSA1                                     
192800     STRING 'WDGX3172(IDFAKT   =' W-IDFAKT-X ')'                          
192900          DELIMITED BY SIZE INTO SSA2                                     
193000     STRING 'WDGX3174(IDKOLLI  =' W-IDKOLLI-X ')'                         
193100          DELIMITED BY SIZE INTO SSA3                                     
193200     STRING 'WDGX3176(IDARTNRO =' W-IDARTNR-OBJ-X ')'                     
193300          DELIMITED BY SIZE INTO SSA4                                     
193400     MOVE '  GE' TO GODK-STATUSKODER                                      
193500     CALL CBLTDLI USING GHU 3171-PCB DLI-IO-3176 SSA1 SSA2                
193600                                                 SSA3 SSA4                
193700     MOVE 3171-STATUS-CODE TO STATUS-WS                                   
193800     PERFORM IMS-STATUSKONTROLL                                           
193900     .                                                                    
194000     EJECT                                                                
194100                                                                          
194200 IMS-ISRT-KOLLI SECTION.                                                  
194300                                                                          
194400     STRING 'WDR401  (WDGXKEY  =' W-WDGXKEY-3171-X ')'                    
194500          DELIMITED BY SIZE INTO SSA1                                     
194600     STRING 'WDGX3172(IDFAKT   =' W-IDFAKT-X ')'                          
194700          DELIMITED BY SIZE INTO SSA2                                     
194800     MOVE 'WDGX3174 ' TO SSA3                                             
194900     MOVE '  II' TO GODK-STATUSKODER                                      
195000     CALL CBLTDLI USING ISRT 3171-PCB DLI-IO-3174 SSA1 SSA2 SSA3          
195100     MOVE 3171-STATUS-CODE TO STATUS-WS                                   
195200     PERFORM IMS-STATUSKONTROLL                                           
195300     .                                                                    
195400     EJECT                                                                
195500                                                                          
195600 IMS-ISRT-KOLLI-RAD SECTION.                                              
195700                                                                          
195800     MOVE 'WDGX3176 ' TO SSA1                                             
195900     MOVE '  II' TO GODK-STATUSKODER                                      
196000     CALL CBLTDLI USING ISRT 3171-PCB DLI-IO-3176 SSA1                    
196100     MOVE 3171-STATUS-CODE TO STATUS-WS                                   
196200     PERFORM IMS-STATUSKONTROLL                                           
196300     .                                                                    
196400     SKIP3                                                                
196500                                                                          
196600 IMS-REPL-3171-LINE SECTION.                                              
196700                                                                          
196800     MOVE 'WDGX3176 ' TO SSA1                                             
196900     MOVE '  ' TO GODK-STATUSKODER                                        
197000     CALL CBLTDLI USING REPL 3171-PCB DLI-IO-3176 SSA1                    
197100     MOVE 3171-STATUS-CODE TO STATUS-WS                                   
197200     PERFORM IMS-STATUSKONTROLL                                           
197300     .                                                                    
197400     SKIP3                                                                
197500 IMS-DLET-3171 SECTION.                                                   
197600                                                                          
197700     MOVE '  ' TO GODK-STATUSKODER                                        
197800     CALL CBLTDLI USING DLET 3171-PCB DLI-IO-3176                         
197900     MOVE 3171-STATUS-CODE TO STATUS-WS                                   
198000     PERFORM IMS-STATUSKONTROLL                                           
198100     .                                                                    
198200     EJECT                                                                
198300 IMS-GHNP-WDGX3174-ST3 SECTION.                                           
198400                                                                          
198500     STRING 'WDGX3174(IDKOLLI >=' W-IDKOLLI-MIN-X                         
198600                    '&IDKOLLI <=' W-IDKOLLI-MAX-X                         
198700                    '&KDTRSTAT =' W-KDTRSTAT-X ')'                        
198800          DELIMITED BY SIZE INTO SSA1                                     
198900     MOVE '  GE' TO GODK-STATUSKODER                                      
199000     CALL CBLTDLI USING GHNP 3171-PCB DLI-IO-3174 SSA1                    
199100     MOVE 3171-STATUS-CODE TO STATUS-WS                                   
199200     PERFORM IMS-STATUSKONTROLL                                           
199300     .                                                                    
199400     SKIP2                                                                
199500 IMS-REPL-WDGX3174 SECTION.                                               
199600                                                                          
199700     MOVE '  ' TO GODK-STATUSKODER                                        
199800     CALL CBLTDLI USING REPL 3171-PCB DLI-IO-3174                         
199900     MOVE 3171-STATUS-CODE TO STATUS-WS                                   
200000     PERFORM IMS-STATUSKONTROLL                                           
200100     .                                                                    
200200     SKIP2                                                                
200300 IMS-GNP-WDGX3174-ST-LT4 SECTION.                                         
200400                                                                          
200500     STRING 'WDGX3174*F(IDKOLLI >=' W-IDKOLLI-MIN-X                       
200600                      '&IDKOLLI <=' W-IDKOLLI-MAX-X                       
200700                      '&KDTRSTAT <' W-KDTRSTAT-X ')'                      
200800          DELIMITED BY SIZE INTO SSA1                                     
200900     MOVE '  GE' TO GODK-STATUSKODER                                      
201000     CALL CBLTDLI USING GNP 3171-PCB DLI-IO-3174 SSA1                     
201100     MOVE 3171-STATUS-CODE TO STATUS-WS                                   
201200     PERFORM IMS-STATUSKONTROLL                                           
201300     .                                                                    
201400     EJECT                                                                
201500 IMS-REPL-WDGX3172 SECTION.                                               
201600                                                                          
201700     MOVE '  ' TO GODK-STATUSKODER                                        
201800     CALL CBLTDLI USING REPL 3171-PCB DLI-IO-3172                         
201900     MOVE 3171-STATUS-CODE TO STATUS-WS                                   
202000     PERFORM IMS-STATUSKONTROLL                                           
202100     .                                                                    
202200     EJECT                                                                
202300 IMS-GU-WDK601 SECTION.                                                   
202400     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
202500          DELIMITED BY SIZE INTO SSA1                                     
202600     MOVE '  GE' TO GODK-STATUSKODER                                      
202700     CALL CBLTDLI USING GU WDK6-PCB DLI-IO-WDK601 SSA1                    
202800     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
202900     PERFORM IMS-STATUSKONTROLL                                           
203000     .                                                                    
203100     SKIP3                                                                
203200 IMS-GNP-WDK611 SECTION.                                                  
203300     MOVE 'WDK611 ' TO SSA1                                               
203400     MOVE '  GE' TO GODK-STATUSKODER                                      
203500     CALL CBLTDLI USING GNP WDK6-PCB DLI-IO-WDK611 SSA1                   
203600     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
203700     PERFORM IMS-STATUSKONTROLL                                           
203800     .                                                                    
203900     SKIP3                                                                
204000 IMS-GU-WDK611 SECTION.                                                   
204100     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
204200          DELIMITED BY SIZE INTO SSA1                                     
204300     MOVE 'WDK611 ' TO SSA2                                               
204400     MOVE '  GE' TO GODK-STATUSKODER                                      
204500     CALL CBLTDLI USING GU WDK6-PCB DLI-IO-WDK611 SSA1 SSA2               
204600     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
204700     PERFORM IMS-STATUSKONTROLL                                           
204800     .                                                                    
204900     SKIP3                                                                
205000 IMS-GHU-WDK711 SECTION.                                                  
205100                                                                          
205200     STRING 'WDK701  (IDARTNR = ' W-IDARTNR-OBJ-X ')'                     
205300          DELIMITED BY SIZE INTO SSA1                                     
205400     STRING 'WDK711  (IDDC    = ' W-IDDC-X ')'                            
205500          DELIMITED BY SIZE INTO SSA2                                     
205600     MOVE '  ' TO GODK-STATUSKODER                                        
205700     CALL CBLTDLI USING GHU WDK7-PCB DLI-IO-WDK711 SSA1 SSA2              
205800     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
205900     PERFORM IMS-STATUSKONTROLL                                           
206000     .                                                                    
206100     EJECT                                                                
206200 IMS-GU-WDK711 SECTION.                                                   
206300                                                                          
206400     STRING 'WDK701  (IDARTNR = ' W-IDARTNR-OBJ-X ')'                     
206500          DELIMITED BY SIZE INTO SSA1                                     
206600     STRING 'WDK711  (IDDC    = ' W-IDDC-X ')'                            
206700          DELIMITED BY SIZE INTO SSA2                                     
206800     MOVE '  GE' TO GODK-STATUSKODER                                      
206900     CALL CBLTDLI USING GU WDK7-PCB DLI-IO-WDK711 SSA1 SSA2               
207000     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
207100     PERFORM IMS-STATUSKONTROLL                                           
207200     .                                                                    
207300     EJECT                                                                
207400 IMS-REPL-WDK711 SECTION.                                                 
207500                                                                          
207600     MOVE '  ' TO GODK-STATUSKODER                                        
207700     CALL CBLTDLI USING REPL WDK7-PCB DLI-IO-WDK711                       
207800     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
207900     PERFORM IMS-STATUSKONTROLL                                           
208000     .                                                                    
208100     SKIP3                                                                
208200 IMS-ISRT-WDL901 SECTION.                                                 
208300     SKIP2                                                                
208400     MOVE 'WDL901 ' TO SSA1                                               
208500     MOVE '  II' TO GODK-STATUSKODER                                      
208600     CALL CBLTDLI USING ISRT WDL9-PCB DLI-IO-WDL901 SSA1                  
208700     MOVE WDL9-STATUS-CODE TO STATUS-WS                                   
208800     PERFORM IMS-STATUSKONTROLL                                           
208900     .                                                                    
209000     EJECT                                                                
209100 IMS-ISRT-WDR901 SECTION.                                                 
209200                                                                          
209300     MOVE 'WDR901 ' TO SSA1                                               
209400     MOVE '  II' TO GODK-STATUSKODER                                      
209500     CALL CBLTDLI USING ISRT WDR9-PCB DLI-IO-WDR901 SSA1                  
209600     MOVE WDR9-STATUS-CODE TO STATUS-WS                                   
209700     PERFORM IMS-STATUSKONTROLL                                           
209800     .                                                                    
209900     EJECT                                                                
210000 IMS-INSERT-ALTMSG SECTION.                                               
210100                                                                          
210200     MOVE SPACE TO GODK-STATUSKODER                                       
210300     CALL CBLTDLI USING ISRT ALT-PCB PROG-TO-PROG-SW                      
210400     MOVE ALT-STATUS-CODE TO STATUS-WS                                    
210500     PERFORM IMS-STATUSKONTROLL                                           
210600     .                                                                    
210700     EJECT                                                                
210800 IMS-GU-WDB601    SECTION.                                                
210900     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
211000          DELIMITED BY SIZE INTO SSA1                                     
211100     MOVE '  GE' TO GODK-STATUSKODER                                      
211200     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B601 SSA1                 
211300     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
211400     PERFORM IMS-STATUSKONTROLL                                           
211500     IF SEGMENT-SAKNAS                                                    
211600         MOVE SPACE TO DCS-KDDC                                           
211700     END-IF                                                               
211800     .                                                                    
211900                                                                          
212000 IMS-GU-WDB601-REC   SECTION.                                             
212100     STRING 'WDB601  (IDDC     =' W-IDDC-B6-REC-X ')'                     
212200          DELIMITED BY SIZE INTO SSA1                                     
212300     MOVE '  GE' TO GODK-STATUSKODER                                      
212400     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B601-REC SSA1             
212500     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
212600     PERFORM IMS-STATUSKONTROLL                                           
212700     IF SEGMENT-SAKNAS                                                    
212800         MOVE SPACE TO REC-DCS-KDDC                                       
212900     END-IF                                                               
213000     .                                                                    
213100                                                                          
213200 IMS-GU-WDB601-SEND   SECTION.                                            
213300     STRING 'WDB601  (IDDC     =' W-IDDC-B6-SEND-X ')'                    
213400          DELIMITED BY SIZE INTO SSA1                                     
213500     MOVE '  GE' TO GODK-STATUSKODER                                      
213600     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B601-SEND SSA1            
213700     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
213800     PERFORM IMS-STATUSKONTROLL                                           
213900     IF SEGMENT-SAKNAS                                                    
214000         MOVE SPACE TO SEND-DCS-KDDC                                      
214100     END-IF                                                               
214200     .                                                                    
214300                                                                          
214400 IMS-STATUSKONTROLL SECTION.                                              
214500                                                                          
214600     SET STATUS-IX TO 1                                                   
214700     SEARCH GODK-STATUS                                                   
214800       AT END                                                             
214900         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
215000         DELIMITED BY SIZE INTO FELTEXT                                   
215100         CALL FELLOG                                                      
215200       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
215300         CONTINUE                                                         
215400     END-SEARCH                                                           
215500     .                                                                    
215600 DB2-SELECT-BYART SECTION.                                                
215700                                                                          
215800     MOVE 000100904         TO GODK-SQLCODESKODER                         
215900     EXEC SQL SELECT                                                      
216000                  ADLAGOMR,                                               
216100                  ADGANG,                                                 
216200                  ADPLATS                                                 
216300              INTO                                                        
216400                  :BYART-ADLAGOMR,                                        
216500                  :BYART-ADGANG,                                          
216600                  :BYART-ADPLATS                                          
216700            FROM BYART                                                    
216800            WHERE IDARTNR_BYT = :BYART-IDARTNR-BYT                        
216900     END-EXEC                                                             
217000     MOVE SQLCODE           TO SQLCODE-WS                                 
217100     PERFORM DB2-STATUSKONTROLL                                           
217200     .                                                                    
217300 DB2-STATUSKONTROLL SECTION.                                              
217400     SKIP2                                                                
217500     SET SQLCODE-IX          TO 1                                         
217600     SEARCH GODK-SQLCODE                                                  
217700       AT END                                                             
217800         CALL FELLOG                                                      
217900        WHEN GODK-SQLCODE (SQLCODE-IX) = SQLCODE-WS                       
218000           CONTINUE                                                       
218100     END-SEARCH                                                           
218200     .                                                                    
