000100 PROCESS DYNAM                                                            
000200 ID DIVISION.                                                             
000300 PROGRAM-ID.     W3717200.                                                
000400 AUTHOR.         BO HAMMARIN.                                             
000500 DATE-WRITTEN.   APRIL-2000.                                              
000600 DATE-COMPILED.                                                           
000700                                                                          
000800*    FUNKTION:                                                            
000900*       -PGM SKAPAR/SKRIVER                                               
001000*        SUMMERADE LARMPOSTER FÖR BYTES-ARTIKLAR (CORE)                   
001100*                                                                         
001200*       -PROGRAMMET LÄSER      WDK6                                       
001300*                              WDK7                                       
001400*                              WDA9                                       
001500*                              WDR4   (WDGX3162)                          
001600*                              WDR4   (WDGX3176)                          
001700*                              FSG2   (DB2)                               
001800*                                                                         
001900*    ABENDKODER:                                                          
002000*        U0016 -  . . . .                                                 
002100*        U1000 -  . . . .                                                 
002200*                                                                         
002300*    CHANGE LOG:                                                          
002400*      YY/MM/DD - INITIALS        - DESCRIPTION.                          
002500*                                                                         
002600*      14/03/11 - REDDY RAHUL     - ETRACKER 10198833                     
002700*                                   CORE ALARM CHANGES                    
002800*                                                                         
002900                                                                          
003000 ENVIRONMENT DIVISION.                                                    
003100                                                                          
003200 INPUT-OUTPUT SECTION.                                                    
003300                                                                          
003400 FILE-CONTROL.                                                            
003500*          --- LARM TRANSAKTIONER                                         
003600     SELECT W37172                     ASSIGN TO W37172D1.                
003700                                                                          
003800 DATA DIVISION.                                                           
003900                                                                          
004000 FILE SECTION.                                                            
004100                                                                          
004200 FD  W37172                                                               
004300     RECORDING       F                                                    
004400     BLOCK CONTAINS  0.                                                   
004500                                                                          
004600*01  POST -COPY W37172 -PRE  UT-  -L.                                     
004700     EJECT                                                                
004800                                                                          
004900 WORKING-STORAGE SECTION.                                                 
005000*    -- CHECKED BY WY2000                                                 
005100 77  IDPGM                       PIC X(8)    VALUE 'W3717200'.            
005200 77  JA                          PIC X       VALUE 'Y'.                   
005300 77  NEJ                         PIC X       VALUE 'N'.                   
005400 77  INDX                        PIC S9(2)   VALUE +0 COMP SYNC.          
005500 77  WDA911-SW                   PIC X       VALUE 'J'.                   
005600     88  WDA911-SAKNAS                       VALUE 'N'.                   
005700 77  SKRIV-IDARTNR-SW            PIC X       VALUE 'Y'.                   
005800     88  SKRIV-IDARTNR-OK                    VALUE 'Y'.                   
005900                                                                          
006000 77  WDGX3172-SW                 PIC X       VALUE 'J'.                   
006100     88  WDGX3172-SAKNAS                     VALUE 'N'.                   
006200                                                                          
006300 77  WDGX3174-SW                 PIC X       VALUE 'J'.                   
006400     88  WDGX3174-SAKNAS                     VALUE 'N'.                   
006500                                                                          
006600 77  WDGX3176-SW                 PIC X       VALUE 'J'.                   
006700     88  WDGX3176-SAKNAS                     VALUE 'N'.                   
006800                                                                          
006900     EJECT                                                                
007000                                                                          
007100 01  FELTEXT                     PIC X(80).                               
007200 01  WS-KVPB                     PIC  9(8)V9 COMP-3 VALUE ZERO.           
007300 01  WS-KVJFR                    PIC  9(8)V9 COMP-3 VALUE ZERO.           
007400 01  WS-KVLS-CORE                PIC  S9(9)  COMP-3 VALUE ZERO.           
007500 01  WS-SULEVANT-RAAR            PIC  S9(9)  COMP-3 VALUE ZERO.           
007600 01  WS-RELARM-PER-MAX           PIC  S9V99  COMP-3 VALUE ZERO.           
007700 01  WS-RELARM-PER-MIN           PIC  S9V99  COMP-3 VALUE ZERO.           
007800 01  WS-DEVIATION                PIC S9(9)V99  COMP-3 VALUE ZERO.         
007900 01  WS-CORE-NEEDED              PIC  S9(11) COMP-3 VALUE ZERO.           
008000 01  W-IDARTNR                   PIC  S9(9)  COMP-3 VALUE ZERO.           
008100     EJECT                                                                
008200                                                                          
008300 01  FILLER                      PIC  X(16)  VALUE 'BYTES-DIST'.          
008400 01  TEST-IDDISTR                PIC 9(5)    COMP-3.                      
008500*01  FILLER  -COPY WWDIS134   -RED TEST-IDDISTR.                          
008600     EJECT                                                                
008700                                                                          
008800 01  FILLER                      PIC  X(16)  VALUE 'BYTES-ART '.          
008900 01  TEST-IDARTNR                PIC  9(9)   COMP-3.                      
009000*01  FILLER  -COPY WWBYT02     -RED TEST-IDARTNR.                         
009100     EJECT                                                                
009200                                                                          
009300*01  FILLER  -COPY WWBYT16     -RED TEST-IDARTNR.                         
009400     EJECT                                                                
009500                                                                          
009600 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
009700 01  FILLER REDEFINES DAGENS-DATUM.                                       
009800     03  DAGENS-DATUM-AAR        PIC 9(2).                                
009900     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
010000     03  DAGENS-DATUM-DAG        PIC 9(2).                                
010100                                                                          
010200 01  DAGENS-KLOCKA               PIC 9(8)    VALUE ZERO.                  
010300 01  WS-KLOCKA                   PIC 9(6)    VALUE ZERO.                  
010400     EJECT                                                                
010500                                                                          
010600 01  DYNAMISKA-SUBPROGRAM.                                                
010700*                                                                         
010800     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
010900     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
011000     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
011100     03  DATKORT                 PIC X(8)    VALUE 'DATKORT'.             
011200     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
011300     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
011400                                                                          
011500*    --- PARAMETRAR TILL ABEND                                            
011600                                                                          
011700 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
011800 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
011900 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
012000     EJECT                                                                
012100                                                                          
012200*    --- VALID IDDC CODES                                                 
012300*                                                                         
012400*01  -COPY WWDC99                                                         
012500*    --- PARAMETRAR TILL DATKORT                                          
012600*                                                                         
012700 01  PROGRAM-NAMN                PIC X(6)    VALUE 'W37172'.              
012800 01  DATUMKORT-ID                PIC X(6)    VALUE 'WDATUM'.              
012900                                                                          
013000*01  -COPY WDATKORT                                                       
013100     EJECT                                                                
013200                                                                          
013300*    --- PARAMETRAR TILL POSTSUM                                          
013400*                                                                         
013500*01  -COPY W0005   -PRE  POSTSUM-                                         
013600     EJECT                                                                
013700                                                                          
013800 01  UT-AREA-START               PIC X(24)   VALUE                        
013900                                 'UT-AREA-START  '.                       
014000                                                                          
014100*01  -COPY W37172    -PRE UT-                                             
014200     EJECT                                                                
014300                                                                          
014400*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
014500*                                                                         
014600 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
014700                                                                          
014800 01  NYCKLAR-TILL-DLI.                                                    
014900     03  W-IDARTNR-K6-X.                                                  
015000         05  W-IDARTNR-K6        PIC S9(9)   VALUE ZERO COMP-3.           
015100     03  W-KDSEGKEY-K6-X.                                                 
015200         05  W-KDSEGKEY-K6       PIC X(1)    VALUE '1'.                   
015300                                                                          
015400     03  W-IDARTNR-K7-X.                                                  
015500         05  W-IDARTNR-K7        PIC S9(9)   VALUE ZERO COMP-3.           
015600     03  W-IDDC-K7-X.                                                     
015700         05  W-IDDC-K7           PIC X(2)    VALUE SPACE.                 
015800                                                                          
015900     03  W-WDGXKEY-3161-X.                                                
016000         05  W-IDHTYP-3161       PIC X(4)    VALUE '3161'.                
016100         05  W-IDDISTR-3161      PIC S9(5)   VALUE ZERO COMP-3.           
016200         05  FILLER              PIC X(23)   VALUE LOW-VALUE.             
016300     03  W-KY3162-X.                                                      
016400         05  W-DAORDREG-3162     PIC 9(8)    VALUE ZERO.                  
016500         05  W-IDORDER-3162      PIC S9(7)   VALUE ZERO.                  
016600         05  W-IDARTNR-3162      PIC S9(9)   VALUE ZERO.                  
016700                                                                          
016800     03  W-IDARTNR-A9-X.                                                  
016900         05  W-IDARTNR-A9        PIC S9(9)   VALUE ZERO COMP-3.           
017000     03  W-IDDISTR-A9-X.                                                  
017100         05  W-IDDISTR-A9        PIC S9(5)   VALUE ZERO COMP-3.           
017200                                                                          
017300     03  W-WDGXKEY-3171-X.                                                
017400         05  W-IDHTYP-3171       PIC X(4)    VALUE '3171'.                
017500         05  FILLER              PIC X(26)   VALUE LOW-VALUE.             
017600     03  W-IDFAKT-3172-X.                                                 
017700         05  W-IDFAKT-3172       PIC S9(7)   VALUE ZERO COMP-3.           
017800     03  W-IDKOLLI-3174-X.                                                
017900         05  W-IDKOLLI-3174      PIC S9(5)   VALUE ZERO COMP-3.           
018000     03  W-IDARTNR-3176-X.                                                
018100         05  W-IDARTNR-3176      PIC S9(9)   VALUE ZERO COMP-3.           
018200                                                                          
018210     03  W-WDGX2231-X.                                                    
018220         05  W-IDHTYP-2231       PIC X(4)     VALUE '2231'.               
018230         05  FILLER              PIC X(26)    VALUE LOW-VALUE.            
018240                                                                          
018250     03  W-WDGX2232-X.                                                    
018260         05  W-IDANSK-2232       PIC S9(3)    VALUE ZERO COMP-3.          
018270         05  FILLER              PIC X(3)     VALUE LOW-VALUE.            
018280                                                                          
018300     EJECT                                                                
018400                                                                          
018500*    --- STATUS-KOD FRÅN IMS                                              
018600 01  STATUS-WS                   PIC XX.                                  
018700     88  SEGMENT-FINNS                       VALUE '  '.                  
018800     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
018900     88  SEGMENT-SLUT                        VALUE 'GB'.                  
019000                                                                          
019100 01  GODK-STATUSKODER.                                                    
019200     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
019300                                                                          
019400 01  SSA1                        PIC X(64).                               
019500 01  SSA2                        PIC X(64).                               
019600 01  SSA3                        PIC X(64).                               
019700     EJECT                                                                
019800 01  FILLER                      PIC X(16)   VALUE 'SQLCA-AREA'.          
019900       EXEC SQL INCLUDE SQLCA END-EXEC.                                   
020000                                                                          
020100 01  FILLER                      PIC X(16)   VALUE 'SQLCODE-WS'.          
020200 01  DB2-WS.                                                              
020300     03  SQLCODE-WS              PIC 9(3)    VALUE ZERO.                  
020400         88  CURSOR-OK                       VALUE 000.                   
020500         88  RADER-FINNS                     VALUE 000.                   
020600         88  RADER-SAKNAS                    VALUE 100.                   
020700         88  ATKOMST-FEL                     VALUE 904.                   
020800     03  GODK-SQLCODEKODER.                                               
020900         05  GODK-SQLCODE OCCURS 5                                        
021000             INDEXED BY SQLCODE-IX PIC 9(3).                              
021100     EJECT                                                                
021200                                                                          
021300*    --- IMS FUNKTIONSKODER                                               
021400*01  -COPY W0003                                                          
021500     EJECT                                                                
021600                                                                          
021700*    ---  DLI INPUT-OUTPUT AREA                                           
021800 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK601'.                      
021900 01  DLI-IO-WDK601.                                                       
022000*    03  -COPY WDK601                                                     
022100     EJECT                                                                
022200                                                                          
022300 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK611'.                      
022400 01  DLI-IO-WDK611.                                                       
022500*    03  -COPY WDK611                                                     
022600     EJECT                                                                
022700                                                                          
022800 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK628'.                      
022900 01  DLI-IO-WDK628.                                                       
023000*    03  -COPY WDK628                                                     
023100     EJECT                                                                
023200                                                                          
023300 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK701'.                      
023400 01  DLI-IO-WDK701.                                                       
023500*    03  -COPY WDK701                                                     
023600     EJECT                                                                
023700                                                                          
023800 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK711'.                      
023900 01  DLI-IO-WDK711.                                                       
024000*    03  -COPY WDK711                                                     
024100     EJECT                                                                
024200                                                                          
024300 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDA901'.                      
024400 01  DLI-IO-WDA901.                                                       
024500*    03  -COPY WDA901                                                     
024600     EJECT                                                                
024700                                                                          
024800 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDA911'.                      
024900 01  DLI-IO-WDA911.                                                       
025000*    03  -COPY WDA911                                                     
025100     EJECT                                                                
025200                                                                          
025300 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX3161'.                    
025400 01  DLI-IO-WDGX3161.                                                     
025500*    03  -COPY WDGX3161                                                   
025600     EJECT                                                                
025700                                                                          
025800 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX3162'.                    
025900 01  DLI-IO-WDGX3162.                                                     
026000*    03  -COPY WDGX3162                                                   
026100     EJECT                                                                
026200 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX3171'.                    
026300 01  DLI-IO-WDGX3171.                                                     
026400*    03  -COPY WDGX01                                                     
026500     EJECT                                                                
026600                                                                          
026700 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX3172'.                    
026800 01  DLI-IO-WDGX3172.                                                     
026900*    03  -COPY WDGX3172                                                   
027000     EJECT                                                                
027100                                                                          
027200 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX3174'.                    
027300 01  DLI-IO-WDGX3174.                                                     
027400*    03  -COPY WDGX3174                                                   
027500     EJECT                                                                
027600                                                                          
027700 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX3176'.                    
027800 01  DLI-IO-WDGX3176.                                                     
027900*    03  -COPY WDGX3176                                                   
028000     EJECT                                                                
028100                                                                          
028110 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX2232'.                    
028120 01  DLI-IO-WDGX2232.                                                     
028130*    03  -COPY WDGX2232                                                   
028140     EJECT                                                                
028150                                                                          
028200 01  FILLER         PIC X(16) VALUE 'FSG2-AREA       '.                   
028300*01  -COPY FSG2 -PRE FSG-                                                 
028400     EJECT                                                                
028500 01  FILLER                      PIC X(16)   VALUE 'FSG2-AREA'.           
028600       EXEC SQL INCLUDE FSG2  END-EXEC.                                   
028700     EJECT                                                                
028800                                                                          
028900 LINKAGE SECTION.                                                         
029000                                                                          
029100*01  -COPY W0008   -PRE WDK6-                                             
029200     05  FILLER                  PIC X.                                   
029300                                                                          
029400*01  -COPY W0008   -PRE WDK6B-                                            
029500     05  FILLER                  PIC X.                                   
029600                                                                          
029700*01  -COPY W0008   -PRE WDK7-                                             
029800     05  FILLER                  PIC X.                                   
029900                                                                          
030000*01  -COPY W0008   -PRE WDA9-                                             
030100     05  FILLER                  PIC X.                                   
030200                                                                          
030300*01  -COPY W0008   -PRE 3161-                                             
030400     05  FILLER                  PIC X.                                   
030500                                                                          
030600*01  -COPY W0008   -PRE 3171-                                             
030700     05  FILLER                  PIC X.                                   
030800     EJECT                                                                
030900                                                                          
030910*01  -COPY W0008   -PRE WDR2-                                             
030920     05  FILLER                  PIC X.                                   
030930     EJECT                                                                
030940                                                                          
031000 PROCEDURE DIVISION  USING WDK6-PCB WDK6B-PCB                             
031100                           WDK7-PCB WDA9-PCB                              
031200                           3161-PCB 3171-PCB                              
031210                           WDR2-PCB.                                      
031300 MAIN SECTION.                                                            
031400     ENTRY 'DLITCBL' USING WDK6-PCB WDK6B-PCB                             
031500                           WDK7-PCB WDA9-PCB                              
031600                           3161-PCB 3171-PCB                              
031610                           WDR2-PCB.                                      
031700                                                                          
031800     PERFORM A-INIT                                                       
031900                                                                          
032000     PERFORM IMS-GU-WDK601-OKVAL                                          
032100     PERFORM UNTIL SEGMENT-SLUT                                           
032200       MOVE ART-IDARTNR          TO TEST-IDARTNR                          
032300                                    W-IDARTNR-K6                          
032400       IF BYT02-RENOV                                                     
032500         PERFORM IMS-GU-WDK628                                            
032600         IF SEGMENT-FINNS                                                 
032700           IF BYT-FLLARM-ACT = JA                                         
032800             IF BYT16-BYTES                                               
032900               COMPUTE TEST-IDARTNR = TEST-IDARTNR +                      
033000                                      6000                                
033100               END-COMPUTE                                                
033200             ELSE                                                         
033300               COMPUTE TEST-IDARTNR = TEST-IDARTNR +                      
033400                                      1000                                
033500               END-COMPUTE                                                
033600             END-IF                                                       
033700             MOVE TEST-IDARTNR   TO W-IDARTNR-K6                          
033800                                    W-IDARTNR-K7                          
033900                                    W-IDARTNR-A9                          
034000                                    W-IDARTNR-3176                        
034100                                    W-IDARTNR                             
034200             PERFORM B-BEARBETA                                           
034300             IF SKRIV-IDARTNR-OK                                          
034400               PERFORM S11-SKRIV-W37172                                   
034500             END-IF                                                       
034600           END-IF                                                         
034700         END-IF                                                           
034800       END-IF                                                             
034900       PERFORM IMS-GN-WDK601-OKVAL                                        
035000     END-PERFORM                                                          
035100                                                                          
035200     PERFORM Z-FINIT                                                      
035300                                                                          
035400     MOVE ZERO TO RETURN-CODE                                             
035500     GOBACK                                                               
035600     .                                                                    
035700     EJECT                                                                
035800                                                                          
035900 A-INIT SECTION.                                                          
036000     OPEN OUTPUT W37172                                                   
036100                                                                          
036200     CALL DATKORT USING PROGRAM-NAMN DATUMKORT-ID DATUMKORT               
036300     MOVE D-AAR        TO DAGENS-DATUM-AAR                                
036400     MOVE D-MAANAD     TO DAGENS-DATUM-MAANAD                             
036500     MOVE D-DAG        TO DAGENS-DATUM-DAG                                
036600                                                                          
036700     MOVE IDPGM        TO POSTSUM-PROGNAMN                                
036800                                                                          
036900     ACCEPT DAGENS-KLOCKA FROM TIME                                       
037000     COMPUTE WS-KLOCKA = DAGENS-KLOCKA / 100                              
037100     .                                                                    
037200     EJECT                                                                
037300                                                                          
037400 B-BEARBETA SECTION.                                                      
037500     MOVE ZERO                          TO WS-KVLS-CORE                   
037600                                                                          
037700     MOVE JA                            TO SKRIV-IDARTNR-SW               
037800                                                                          
037900* CORE PART INFO LÄSES/ACKUMULERAS                                        
038000     PERFORM IMS-GU-WDK601                                                
038100     PERFORM IMS-GNP-WDK611                                               
038200     IF SEGMENT-FINNS                                                     
038300       MOVE CLAG-KVLS                   TO WS-KVLS-CORE                   
038400                                                                          
038500       PERFORM IMS-GU-WDK701                                              
038600       PERFORM UNTIL SEGMENT-SAKNAS                                       
038700         PERFORM IMS-GNP-WDK711                                           
038800         IF SEGMENT-FINNS                                                 
038900           ADD SLAG-KVLS                TO WS-KVLS-CORE                   
039000         END-IF                                                           
039100       END-PERFORM                                                        
039200                                                                          
039300       PERFORM IMS-GU-WDA901                                              
039400       IF SEGMENT-FINNS                                                   
039500         MOVE JA                        TO WDA911-SW                      
039600         PERFORM UNTIL WDA911-SAKNAS                                      
039700           PERFORM IMS-GNP-WDA911                                         
039800           IF SEGMENT-FINNS                                               
039900* DENNA ADDITION SKALL GÖRAS FÖR SAMTLIGA RENOVÖRER SOM                   
040000* BÖRJAR ANVÄNDA WEB-BILDERNA 3178/3179                                   
040100             MOVE UPD-IDDISTR           TO TEST-IDDISTR                   
040200             IF DIS134-BYTESREN-WEB                                       
040300               ADD UPD-KVLS-REM         TO WS-KVLS-CORE                   
040400             END-IF                                                       
040500                                                                          
040600             MOVE UPD-IDDISTR           TO W-IDDISTR-3161                 
040700             PERFORM IMS-GU-WDGX3161                                      
040800             IF SEGMENT-FINNS                                             
040900               PERFORM UNTIL SEGMENT-SAKNAS                               
041000                 PERFORM IMS-GNP-WDGX3162                                 
041100                 IF SEGMENT-FINNS                                         
041200                   IF 3162-IDARTNR = UPB-IDARTNR AND                      
041300                      3162-IDUSER  = SPACE                                
041500                     ADD 3162-KVAVIS    TO WS-KVLS-CORE                   
041600                   END-IF                                                 
041700                 END-IF                                                   
041800               END-PERFORM                                                
041900             END-IF                                                       
042000           ELSE                                                           
042100             MOVE NEJ                   TO WDA911-SW                      
042200           END-IF                                                         
042300         END-PERFORM                                                      
042400       END-IF                                                             
042500                                                                          
042600       MOVE JA                          TO WDGX3172-SW                    
042700                                           WDGX3174-SW                    
042800                                           WDGX3176-SW                    
042900       PERFORM IMS-GU-WDGX3171                                            
043000                                                                          
043100       PERFORM UNTIL WDGX3172-SAKNAS                                      
043200         PERFORM IMS-GNP-WDGX3172                                         
043300                                                                          
043400         IF SEGMENT-FINNS AND                                             
043500            3172-KDTRSTAT < 4                                             
043600           MOVE JA                      TO WDGX3174-SW                    
043700                                                                          
043800           PERFORM UNTIL WDGX3174-SAKNAS                                  
043900             MOVE 3172-IDFAKT           TO W-IDFAKT-3172                  
044000                                                                          
044100             PERFORM IMS-GNP-WDGX3174                                     
044200             IF SEGMENT-FINNS                                             
044300               MOVE 3174-IDKOLLI        TO W-IDKOLLI-3174                 
044400               PERFORM UNTIL SEGMENT-SAKNAS                               
044500                 PERFORM IMS-GNP-WDGX3176                                 
044600                 IF SEGMENT-FINNS                                         
044700                   MOVE 3172-IDDC-REC   TO WS-IDDC                        
044800                   IF SDC-NL-ET                                           
045100                     ADD  3176-KVANTMOT TO WS-KVLS-CORE                   
045200                   END-IF                                                 
045300                 END-IF                                                   
045400               END-PERFORM                                                
045500             ELSE                                                         
045600               MOVE NEJ                 TO WDGX3174-SW                    
045700             END-IF                                                       
045800           END-PERFORM                                                    
045900         ELSE                                                             
046000           IF SEGMENT-SAKNAS                                              
046100             MOVE NEJ                   TO WDGX3172-SW                    
046200           END-IF                                                         
046300         END-IF                                                           
046400       END-PERFORM                                                        
046500     ELSE                                                                 
046600       MOVE NEJ TO SKRIV-IDARTNR-SW                                       
046700     END-IF                                                               
046800                                                                          
046900     IF SKRIV-IDARTNR-OK                                                  
047000* RENOVATED PART INFO LÄSES                                               
047100       IF BYT16-BYTES                                                     
047200         COMPUTE W-IDARTNR-K6 = W-IDARTNR-K6 -                            
047300                                6000                                      
047400       ELSE                                                               
047500         COMPUTE W-IDARTNR-K6 = W-IDARTNR-K6 -                            
047600                                1000                                      
047700       END-IF                                                             
047800                                                                          
047900       PERFORM IMS-GU-WDK601                                              
048000       IF SEGMENT-FINNS                                                   
048100         MOVE ART-IDARTNR               TO UT-IDARTNR                     
048200         PERFORM IMS-GNP-WDK611                                           
048300         IF SEGMENT-FINNS                                                 
048310           MOVE CLAG-IDANSK             TO W-IDANSK-2232                  
048320           PERFORM IMS-GU-WDR220                                          
048330           IF SEGMENT-FINNS                                               
048340             MOVE 2232-IDANSK-LARM      TO UT-IDANSK                      
048350           ELSE                                                           
048351             MOVE ZERO                  TO UT-IDANSK                      
048360           END-IF                                                         
048500           MOVE ZERO                    TO UT-KVASSET                     
048600           MOVE DAGENS-DATUM            TO UT-TIREGDAT                    
048700                                                                          
048800           IF BYT16-BYTES                                                 
048900              SUBTRACT 6000           FROM W-IDARTNR                      
049000           ELSE                                                           
049100              SUBTRACT 1000           FROM W-IDARTNR                      
049200           END-IF                                                         
049300                                                                          
049400           PERFORM DB2-SELECT-FSG2-TAB                                    
049500           IF RADER-SAKNAS                                                
049600             MOVE ZERO                  TO WS-SULEVANT-RAAR               
049700           ELSE                                                           
049800             MOVE FSG-SULEVANT-RAAR     TO WS-SULEVANT-RAAR               
049900           END-IF                                                         
050000                                                                          
050100           COMPUTE WS-RELARM-PER-MAX ROUNDED                              
050200                                  = 1 + (BYT-RELARM-PER / 100)            
050300           COMPUTE WS-RELARM-PER-MIN ROUNDED                              
050400                                  = 1 - (BYT-RELARM-PER / 100)            
050500           IF BYT-RELARM-FAC NOT = ZERO                                   
050600              IF WS-SULEVANT-RAAR = ZERO                                  
050700                MOVE 1                  TO WS-SULEVANT-RAAR               
050800              END-IF                                                      
050900              COMPUTE WS-CORE-NEEDED ROUNDED                              
051000                                  = WS-SULEVANT-RAAR /                    
051100                                    BYT-RELARM-FAC                        
051200              COMPUTE WS-DEVIATION ROUNDED                                
051300                              = WS-KVLS-CORE /                            
051400                                WS-CORE-NEEDED                            
051500              IF WS-DEVIATION > WS-RELARM-PER-MAX OR                      
051600                 WS-DEVIATION < WS-RELARM-PER-MIN                         
051700                 MOVE 'I'               TO UT-KDBEH                       
051800              ELSE                                                        
051900                 MOVE 'D'               TO UT-KDBEH                       
052000              END-IF                                                      
052100           ELSE                                                           
052200              MOVE 'D'                  TO UT-KDBEH                       
052300           END-IF                                                         
052823         END-IF                                                           
052824       END-IF                                                             
052825     END-IF                                                               
052826     .                                                                    
052827     EJECT                                                                
052828                                                                          
052830 Z-FINIT SECTION.                                                         
052900     CLOSE W37172                                                         
053000                                                                          
053100     MOVE 'S' TO POSTSUM-OPKOD                                            
053200     CALL POSTSUM USING POSTSUM-PARM                                      
053300     .                                                                    
053400     EJECT                                                                
053500                                                                          
053600 S11-SKRIV-W37172 SECTION.                                                
053700     WRITE UT-POST   FROM UT-W37172                                       
053800                                                                          
053900     MOVE 'UT-'      TO   POSTSUM-TRANSTYP                                
054000     MOVE 'W37172'   TO   POSTSUM-FDNAMN                                  
054100     MOVE 'W37172D1' TO   POSTSUM-DDNAMN2                                 
054200     CALL POSTSUM USING POSTSUM-PARM                                      
054300     .                                                                    
054400     EJECT                                                                
054500                                                                          
054600* --- IMS SEKTIONER ---                                                   
054700 IMS-GU-WDK601-OKVAL SECTION.                                             
054800     MOVE 'WDK601  '       TO SSA1                                        
054900     MOVE '  GE'           TO GODK-STATUSKODER                            
055000     CALL CBLTDLI USING GU WDK6B-PCB DLI-IO-WDK601 SSA1                   
055100     MOVE WDK6B-STATUS-CODE TO STATUS-WS                                  
055200     PERFORM IMS-STATUSKONTROLL                                           
055300     .                                                                    
055400                                                                          
055500 IMS-GN-WDK601-OKVAL SECTION.                                             
055600     MOVE 'WDK601  '       TO SSA1                                        
055700     MOVE '  GB'           TO GODK-STATUSKODER                            
055800     CALL CBLTDLI USING GN WDK6B-PCB DLI-IO-WDK601 SSA1                   
055900     MOVE WDK6B-STATUS-CODE TO STATUS-WS                                  
056000     PERFORM IMS-STATUSKONTROLL                                           
056100     .                                                                    
056200                                                                          
056300     EJECT                                                                
056400 IMS-GU-WDK601 SECTION.                                                   
056500     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-K6-X ')'                      
056600          DELIMITED BY SIZE INTO SSA1                                     
056700     MOVE '  GE'           TO GODK-STATUSKODER                            
056800     CALL CBLTDLI USING GU WDK6-PCB DLI-IO-WDK601 SSA1                    
056900     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
057000     PERFORM IMS-STATUSKONTROLL                                           
057100     .                                                                    
057200                                                                          
057300 IMS-GNP-WDK611 SECTION.                                                  
057400     STRING 'WDK611  (KDSEGKEY =' W-KDSEGKEY-K6-X ')'                     
057500          DELIMITED BY SIZE INTO SSA1                                     
057600     MOVE '  GE'           TO GODK-STATUSKODER                            
057700     CALL CBLTDLI USING GNP WDK6-PCB DLI-IO-WDK611 SSA1                   
057800     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
057900     PERFORM IMS-STATUSKONTROLL                                           
058000     .                                                                    
058100                                                                          
058200 IMS-GU-WDK628 SECTION.                                                   
058300     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-K6-X ')'                      
058400          DELIMITED BY SIZE INTO SSA1                                     
058500     STRING 'WDK611  (KDSEGKEY =' W-KDSEGKEY-K6-X ')'                     
058600          DELIMITED BY SIZE INTO SSA2                                     
058700     STRING 'WDK628  (IDARTNR  =' W-IDARTNR-K6-X ')'                      
058800          DELIMITED BY SIZE INTO SSA3                                     
058900     MOVE '  GE'           TO GODK-STATUSKODER                            
059000     CALL CBLTDLI USING GU WDK6-PCB DLI-IO-WDK628 SSA1 SSA2 SSA3          
059100     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
059200     PERFORM IMS-STATUSKONTROLL                                           
059300     .                                                                    
059400                                                                          
059500 IMS-GU-WDK701 SECTION.                                                   
059600     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-K7-X ')'                      
059700          DELIMITED BY SIZE INTO SSA1                                     
059800     MOVE '  GE'           TO GODK-STATUSKODER                            
059900     CALL CBLTDLI USING GU WDK7-PCB DLI-IO-WDK701 SSA1                    
060000     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
060100     PERFORM IMS-STATUSKONTROLL                                           
060200     .                                                                    
060300                                                                          
060400 IMS-GNP-WDK711 SECTION.                                                  
060500     MOVE 'WDK711   '      TO SSA1                                        
060600     MOVE '  GE'           TO GODK-STATUSKODER                            
060700     CALL CBLTDLI USING GNP WDK7-PCB DLI-IO-WDK711 SSA1                   
060800     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
060900     PERFORM IMS-STATUSKONTROLL                                           
061000     .                                                                    
061100     EJECT                                                                
061200                                                                          
061300 IMS-GU-WDGX3161 SECTION.                                                 
061400     STRING 'WDR401  (WDGXKEY  =' W-WDGXKEY-3161-X ')'                    
061500          DELIMITED BY SIZE INTO SSA1                                     
061600     MOVE '  GE'           TO GODK-STATUSKODER                            
061700     CALL CBLTDLI USING GU  3161-PCB DLI-IO-WDGX3161 SSA1                 
061800     MOVE 3161-STATUS-CODE TO STATUS-WS                                   
061900     PERFORM IMS-STATUSKONTROLL                                           
062000     .                                                                    
062100                                                                          
062200 IMS-GNP-WDGX3162 SECTION.                                                
062300     MOVE 'WDGX3162  '     TO SSA1                                        
062400     MOVE '  GE'           TO GODK-STATUSKODER                            
062500     CALL CBLTDLI USING GNP 3161-PCB DLI-IO-WDGX3162 SSA1                 
062600     MOVE 3161-STATUS-CODE TO STATUS-WS                                   
062700     PERFORM IMS-STATUSKONTROLL                                           
062800     .                                                                    
062900     EJECT                                                                
063000                                                                          
063100 IMS-GU-WDA901 SECTION.                                                   
063200     STRING 'WDA901  (IDARTNR  =' W-IDARTNR-A9-X ')'                      
063300          DELIMITED BY SIZE INTO SSA1                                     
063400     MOVE '  GE'           TO GODK-STATUSKODER                            
063500     CALL CBLTDLI USING GU WDA9-PCB DLI-IO-WDA901 SSA1                    
063600     MOVE WDA9-STATUS-CODE TO STATUS-WS                                   
063700     PERFORM IMS-STATUSKONTROLL                                           
063800     .                                                                    
063900                                                                          
064000 IMS-GNP-WDA911 SECTION.                                                  
064100     MOVE 'WDA911   '      TO SSA1                                        
064200     MOVE '  GE'           TO GODK-STATUSKODER                            
064300     CALL CBLTDLI USING GNP WDA9-PCB DLI-IO-WDA911 SSA1                   
064400     MOVE WDA9-STATUS-CODE TO STATUS-WS                                   
064500     PERFORM IMS-STATUSKONTROLL                                           
064600     .                                                                    
064700     EJECT                                                                
064800                                                                          
064900 IMS-GU-WDGX3171 SECTION.                                                 
065000     STRING 'WDR401  (WDGXKEY  =' W-WDGXKEY-3171-X ')'                    
065100          DELIMITED BY SIZE INTO SSA1                                     
065200     MOVE '  '             TO GODK-STATUSKODER                            
065300     CALL CBLTDLI USING GU  3171-PCB DLI-IO-WDGX3171 SSA1                 
065400     MOVE 3171-STATUS-CODE TO STATUS-WS                                   
065500     PERFORM IMS-STATUSKONTROLL                                           
065600     .                                                                    
065700                                                                          
065800 IMS-GNP-WDGX3172 SECTION.                                                
065900     MOVE 'WDGX3172  '     TO SSA1                                        
066000     MOVE '  GE'           TO GODK-STATUSKODER                            
066100     CALL CBLTDLI USING GNP 3171-PCB DLI-IO-WDGX3172 SSA1                 
066200     MOVE 3171-STATUS-CODE TO STATUS-WS                                   
066300     PERFORM IMS-STATUSKONTROLL                                           
066400     .                                                                    
066500                                                                          
066600 IMS-GNP-WDGX3174 SECTION.                                                
066700     STRING 'WDGX3172(IDFAKT   =' W-IDFAKT-3172-X ')'                     
066800          DELIMITED BY SIZE INTO SSA1                                     
066900     MOVE 'WDGX3174  '     TO SSA2                                        
067000     MOVE '  GE'           TO GODK-STATUSKODER                            
067100     CALL CBLTDLI USING GNP 3171-PCB DLI-IO-WDGX3174 SSA1 SSA2            
067200     MOVE 3171-STATUS-CODE TO STATUS-WS                                   
067300     PERFORM IMS-STATUSKONTROLL                                           
067400     .                                                                    
067500                                                                          
067600 IMS-GNP-WDGX3176 SECTION.                                                
067700     STRING 'WDGX3172(IDFAKT   =' W-IDFAKT-3172-X ')'                     
067800          DELIMITED BY SIZE INTO SSA1                                     
067900     STRING 'WDGX3174(IDKOLLI  =' W-IDKOLLI-3174-X ')'                    
068000          DELIMITED BY SIZE INTO SSA2                                     
068100     STRING 'WDGX3176(IDARTNRO =' W-IDARTNR-3176-X ')'                    
068200          DELIMITED BY SIZE INTO SSA3                                     
068300     MOVE '  GE'           TO GODK-STATUSKODER                            
068400     CALL CBLTDLI USING GNP 3171-PCB DLI-IO-WDGX3176 SSA1 SSA2            
068500                                                     SSA3                 
068600     MOVE 3171-STATUS-CODE TO STATUS-WS                                   
068700     PERFORM IMS-STATUSKONTROLL                                           
068800     .                                                                    
068900     EJECT                                                                
069000                                                                          
069010 IMS-GU-WDR220 SECTION.                                                   
069030                                                                          
069040     STRING 'WDR201  (WDGXKEY  =' W-WDGX2231-X ')'                        
069050          DELIMITED BY SIZE INTO SSA1                                     
069060     STRING 'WDR220  (WDGXKEY  =' W-WDGX2232-X ')'                        
069070          DELIMITED BY SIZE INTO SSA2                                     
069080     MOVE '  GE' TO GODK-STATUSKODER                                      
069090     CALL CBLTDLI USING GU WDR2-PCB DLI-IO-WDGX2232 SSA1 SSA2             
069091     MOVE WDR2-STATUS-CODE TO STATUS-WS                                   
069092     PERFORM IMS-STATUSKONTROLL                                           
069094     .                                                                    
069095     SKIP2                                                                
069100 IMS-STATUSKONTROLL SECTION.                                              
069200     SET STATUS-IX TO 1                                                   
069300     SEARCH GODK-STATUS                                                   
069400       AT END                                                             
069500         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
069600           DELIMITED BY SIZE INTO FELTEXT                                 
069700         DISPLAY FELTEXT                                                  
069800         CALL FELLOG                                                      
069900       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
070000         CONTINUE                                                         
070100     END-SEARCH                                                           
070200     .                                                                    
070300 DB2-SELECT-FSG2-TAB SECTION.                                             
070400                                                                          
070500     MOVE 000100           TO GODK-SQLCODEKODER                           
070600     EXEC SQL                                                             
070700         SELECT SULEVANT_RAAR                                             
070800         INTO :FSG-SULEVANT-RAAR                                          
070900         FROM FSG2                                                        
071000         WHERE IDARTNR = :W-IDARTNR                                       
071100     END-EXEC                                                             
071200     MOVE SQLCODE          TO SQLCODE-WS                                  
071300     PERFORM DB2-STATUSKONTROLL                                           
071400     .                                                                    
071500     EJECT                                                                
071600 DB2-STATUSKONTROLL  SECTION.                                             
071700                                                                          
071800     SET SQLCODE-IX TO 1                                                  
071900     SEARCH GODK-SQLCODE                                                  
072000       AT END CALL FELLOG                                                 
072100       WHEN GODK-SQLCODE (SQLCODE-IX) = SQLCODE-WS CONTINUE               
072200     END-SEARCH                                                           
072300     .                                                                    
