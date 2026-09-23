000100 PROCESS DYNAM                                                            
000200 ID DIVISION.                                                             
000300 PROGRAM-ID.                W1010100.                                     
000400*AUTHOR.                    URBAN ZACKRISSON.                             
000500*DATE-COMPILED.                                                           
000600*DATE-WRITTEN.              NOV  -84.                                     
000700*    FUNKTION.   TP-PROGRAM                                               
000800*                BEREDNINGSINFORMATION                                    
000900*                UPPDATERING AV VISS INFORMATION.                         
001000*                                                                         
001100*    INDATA.                                                              
001200*        TRANSAKTION: W1T101                                              
001300*                     W1T101U                                             
001400*        MID:         W1I10101                                            
001500*    UTDATA.                                                              
001600*        MOD:         W1O10101                                            
001700*    SUBPROGRAM.                                                          
001800*        FELLOG                                                           
001900*                                                                         
002000*    ÄNDRING:                                                             
002100*        2005-FEB  ETRACKER=1476814.  VISA KAMPANJ-INFO                   
002200*                                     TILLAGT DB2-LÄSNING /C.E.           
002300*                                                                         
002400     SKIP3                                                                
002500 ENVIRONMENT DIVISION.                                                    
002600     EJECT                                                                
002700 DATA DIVISION.                                                           
002800     SKIP3                                                                
002900 WORKING-STORAGE SECTION.                                                 
003000                                                                          
003100*    -- CHECKED BY WY2000                                                 
003200 77  IDPGM                       PIC X(8)   VALUE 'W1010100'.             
003300     SKIP3                                                                
003400 77      IDARTNR-WS              PIC X(9)   VALUE SPACE.                  
003500 77      JA                      PIC X(1)   VALUE 'J'.                    
003600 77      NEJ                     PIC X(1)   VALUE 'N'.                    
003700 77      MAX-ANT-KAT-TILLH       PIC S9(9)  VALUE +14   COMP SYNC.        
003800 77      MAX-ANT-IDAO            PIC S9(9)  VALUE +6    COMP SYNC.        
003900 77      INDX                    PIC S9(9)  VALUE +0    COMP SYNC.        
004000 77      IND                     PIC S9(9)  VALUE +0    COMP SYNC.        
004100 77      XIDAO                   PIC S9(9)  VALUE +0    COMP SYNC.        
004200 77      W-KDERS                 PIC S9(3)  VALUE +0    COMP-3.           
004300 77      W-FLERS                 PIC X(1)   VALUE 'N'.                    
004400 77      VCBV                    PIC X      VALUE '4'.                    
004500 77      INPUT-RETT              PIC X(1)   VALUE SPACE.                  
004600 77      WS-KDERS-UTG            PIC 9(3)   VALUE ZERO.                   
004700     EJECT                                                                
004800                                                                          
004900 01  ARBETSAREOR.                                                         
005000     03 FILLER                   PIC X(16)   VALUE                        
005100                                             'WS-DB2-SEKTION'.            
005200     03 WS-DB2-SEKTION           PIC X(24)   VALUE SPACE.                 
005300                                                                          
005400     03  WS-DAGENS-AAAAMMDD      PIC 9(8).                                
005500     03  WS-JMFR-AAAAMMDD        PIC 9(8).                                
005600     03  FILLER REDEFINES WS-JMFR-AAAAMMDD.                               
005700        05 FILLER                PIC 9(2).                                
005800        05 WS-JMFR-AA            PIC 9(2).                                
005900        05 FILLER                PIC 9(4).                                
006000     03  WS-FLAGGA-Q-KAMP        PIC X(1)    VALUE SPACE.                 
006100     03  WS-FLAGGA-W-S-KAMP      PIC X(1)    VALUE SPACE.                 
006200     03  WS-KVLS-REM             PIC S9(7)   VALUE ZERO COMP-3.           
006300     03  WS-ANTAL-KAMP           PIC 9(7)    VALUE ZERO.                  
006400                                                                          
006500     03  WS-TEMFSINF             PIC X(42)   VALUE SPACE.                 
006600     03  WS-TEMFSINF-KAMP        PIC X(10)   VALUE SPACE.                 
006700     03  WS-TEMFSINF-SPLIT       PIC X(3)    VALUE '-- '.                 
006800     EJECT                                                                
006900*      --- VALID IDDC CODES                                               
007000*                                                                         
007100*01    -COPY WWDC99                                                       
007200       EJECT                                                              
007300*01    -COPY WWPRODSL                                                     
007400       EJECT                                                              
007500                                                                          
007600 01  DYNAMISKA-SUBPROGRAM.                                                
007700     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
007800     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
007900     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
008000                                                                          
008100 01  DYNAMISK-SUBMODUL.                                                   
008200   03  WSECURIT                  PIC X(8)   VALUE 'WSECURIT'.             
008300     EJECT                                                                
008400 01      BLADDRINGS-FALT.                                                 
008500   03      SEG-SKIP              PIC 9(3)   VALUE ZERO.                   
008600   03      TOT-KAT               PIC 9(3)   VALUE ZERO.                   
008700   03      LASTA-SEG             PIC 9(3)   VALUE ZERO.                   
008800   03      LASTA-KAT             PIC 9(3)   VALUE ZERO.                   
008900     SKIP3                                                                
009000 01    NYCKLAR-TILL-DLI.                                                  
009100                                                                          
009200   03    W-IDARTNR-X.                                                     
009300     05    W-IDARTNR             PIC S9(9)  VALUE ZERO  COMP-3.           
009400   03    W-KDNOTTYP-X.                                                    
009500     05    W-KDNOTTYP            PIC S9(1)  VALUE ZERO  COMP-3.           
009600   03    W-IDSKYLT-X.                                                     
009700     05    W-IDSKYLT             PIC X(3)   VALUE SPACE.                  
009800   03    W-WDF701KY-X.                                                    
009900     05    W-IDARTNR-F7          PIC S9(9)  VALUE ZERO  COMP-3.           
010000     05    W-IDPRTNER            PIC S9(5)  VALUE +1    COMP-3.           
010100     EJECT                                                                
010200                                                                          
010300 01    MEDDELANDEN.                                                       
010400                                                                          
010500   03    W-FEL-1.                                                         
010600     05    FILLER        PIC X(22)   VALUE                                
010700                             'PARTNUMBER NOT NUMERIC'.                    
010800     05    FILLER        PIC X(22)   VALUE                                
010900                             'PARTNUMBER NOT NUMERIC'.                    
011000   03    FILLER REDEFINES W-FEL-1.                                        
011100     05    FEL-1         PIC X(22) OCCURS 2.                              
011200                                                                          
011300   03    W-FEL-2.                                                         
011400     05    FILLER        PIC X(32)   VALUE                                
011500                             'THIS PART IS NOT IN THE DATABASE'.          
011600     05    FILLER        PIC X(32)   VALUE                                
011700                             'THIS PART IS NOT IN THE DATABASE'.          
011800   03    FILLER REDEFINES W-FEL-2.                                        
011900     05    FEL-2         PIC X(32) OCCURS 2.                              
012000                                                                          
012100   03    W-MED-1.                                                         
012200     05    FILLER        PIC X(34)  VALUE                                 
012300                             'FOR MORE VEHICLE INFO, PRESS ENTER'.        
012400     05    FILLER        PIC X(34)  VALUE                                 
012500                             'FOR MORE VEHICLE INFO, PRESS ENTER'.        
012600   03    FILLER REDEFINES W-MED-1.                                        
012700     05    MED-1         PIC X(34) OCCURS 2.                              
012800                                                                          
012900   03    W-MED-2.                                                         
013000     05    FILLER        PIC X(7)   VALUE                                 
013100                             'UPDATED'.                                   
013200     05    FILLER        PIC X(7)   VALUE                                 
013300                             'UPDATED'.                                   
013400   03    FILLER REDEFINES W-MED-2.                                        
013500     05    MED-2         PIC X(7) OCCURS 2.                               
013600                                                                          
013700   03    W-MED-3.                                                         
013800     05    FILLER        PIC X(26)   VALUE                                
013900                             'THIS IS A SUPERSEDING PART'.                
014000     05    FILLER        PIC X(26)   VALUE                                
014100                             'THIS IS A SUPERSEDING PART'.                
014200   03    FILLER REDEFINES W-MED-3.                                        
014300     05    MED-3         PIC X(26) OCCURS 2.                              
014400                                                                          
014500   03    W-MED-4.                                                         
014600     05    FILLER        PIC X(7)   VALUE                                 
014700                             'DELETED'.                                   
014800     05    FILLER        PIC X(7)   VALUE                                 
014900                             'DELETED'.                                   
015000   03    FILLER REDEFINES W-MED-4.                                        
015100     05    MED-4         PIC X(7) OCCURS 2.                               
015200                                                                          
015300   03    W-MED-5.                                                         
015400     05    FILLER        PIC X(15)   VALUE                                
015500                             'WILL BE DELETED'.                           
015600     05    FILLER        PIC X(15)   VALUE                                
015700                             'WILL BE DELETED'.                           
015800   03    FILLER REDEFINES W-MED-5.                                        
015900     05    MED-5         PIC X(15) OCCURS 2.                              
016000                                                                          
016100   03    W-MED-6.                                                         
016200     05    FILLER        PIC X(23)   VALUE                                
016300                             'THIS PART IS SUPERSEDED'.                   
016400     05    FILLER        PIC X(23)   VALUE                                
016500                             'THIS PART IS SUPERSEDED'.                   
016600   03    FILLER REDEFINES W-MED-6.                                        
016700     05    MED-6         PIC X(23) OCCURS 2.                              
016800                                                                          
016900   03    W-MED-7.                                                         
017000     05    FILLER        PIC X(11)   VALUE                                
017100                             'NOT UPDATED'.                               
017200     05    FILLER        PIC X(11)   VALUE                                
017300                             'NOT UPDATED'.                               
017400   03    FILLER REDEFINES W-MED-7.                                        
017500     05    MED-7         PIC X(11) OCCURS 2.                              
017600                                                                          
017700   03    W-MED-8.                                                         
017800     05    FILLER        PIC X(39)   VALUE                                
017900                      'THIS PART IS SUPERSEDED AND SUPERSEDING'.          
018000     05    FILLER        PIC X(39)   VALUE                                
018100                      'THIS PART IS SUPERSEDED AND SUPERSEDING'.          
018200   03    FILLER REDEFINES W-MED-8.                                        
018300     05    MED-8         PIC X(39) OCCURS 2.                              
018400                                                                          
018500   03    W-MED-9.                                                         
018600     05    FILLER        PIC X(36)   VALUE                                
018700                      'THIS PART IS SUPERSEDING AND DELETED'.             
018800     05    FILLER        PIC X(36)   VALUE                                
018900                      'THIS PART IS SUPERSEDING AND DELETED'.             
019000   03    FILLER REDEFINES W-MED-9.                                        
019100     05    MED-9         PIC X(36) OCCURS 2.                              
019200                                                                          
019300   03    W-MED-10.                                                        
019400     05    FILLER        PIC X(28)   VALUE                                
019500                      'SUPERSEDING, WILL BE DELETED'.                     
019600     05    FILLER        PIC X(28)   VALUE                                
019700                      'SUPERSEDING, WILL BE DELETED'.                     
019800   03    FILLER REDEFINES W-MED-10.                                       
019900     05    MED-10        PIC X(28) OCCURS 2.                              
020000                                                                          
020100   03    W-MED-11.                                                        
020200     05    FILLER        PIC X(28)   VALUE                                
020300                      'UPDATE NOT ALLOWED          '.                     
020400     05    FILLER        PIC X(28)   VALUE                                
020500                      'UPDATE NOT ALLOWED          '.                     
020600   03    FILLER REDEFINES W-MED-11.                                       
020700     05    MED-11        PIC X(28) OCCURS 2.                              
020800                                                                          
020900                                                                          
021000                                                                          
021100                                                                          
021200*                  MED-91 KAN KOMBINERAS MED MED-1 PÅ RAD 23              
021300   03    W-MED-91.                                                        
021400     05    FILLER        PIC X(10)   VALUE 'CAMPAIGN  '.                  
021500     05    FILLER        PIC X(10)   VALUE 'CAMPAIGN  '.                  
021600   03    FILLER REDEFINES W-MED-91.                                       
021700     05    MED-91        PIC X(10) OCCURS 2.                              
021800                                                                          
021900*                  MED-92 KAN KOMBINERAS MED MED-1 PÅ RAD 23              
022000   03    W-MED-92.                                                        
022100     05    FILLER        PIC X(10)   VALUE 'CAMPAIGN  '.                  
022200     05    FILLER        PIC X(10)   VALUE 'CAMPAIGN  '.                  
022300   03    FILLER REDEFINES W-MED-92.                                       
022400     05    MED-92        PIC X(10) OCCURS 2.                              
022500                                                                          
022600                                                                          
022700     EJECT                                                                
022800*01  -COPY WSECAREA                                                       
022900     EJECT                                                                
023000*                   ****    PARAMETRAR TILL W005INIT                      
023100*01  -COPY WMSGINIT                                                       
023200     EJECT                                                                
023300******************************************************************        
023400*                                                                         
023500*                AREOR FÖR MFS OCH SKÄRMHANTERING                         
023600*                                                                         
023700 01      FILLER          PIC X(16)   VALUE 'MFS-WS'.                      
023800     SKIP3                                                                
023900*01      MID -COPY W1I10101 -PRE MID-.                                    
024000     EJECT                                                                
024100*01      -COPY WMSGAREA                                                   
024200     EJECT                                                                
024300*  03    MOD -COPY W1O10101 -PRE MOD- -RED MSG-AREA.                      
024400     EJECT                                                                
024500*01  -COPY WMFSAREA                                                       
024600     EJECT                                                                
024700******************************************************************        
024800*                                                                         
024900*        ARBETS-AREOR TILL DB2-SEKTIONERNA                                
025000*                                                                         
025100 01  FILLER                      PIC X(16)  VALUE 'SQLCA-AREA'.           
025200       EXEC SQL INCLUDE SQLCA END-EXEC.                                   
025300                                                                          
025400 01  FILLER                      PIC X(16)  VALUE 'SQLCODE-WS'.           
025500 01  DB2-WS.                                                              
025600     03  SQLCODE-WS              PIC 9(3)   VALUE ZERO.                   
025700         88  CURSOR-OK                      VALUE 000.                    
025800         88  LINES-FOUND                    VALUE 000.                    
025900         88  LINES-MISSING                  VALUE 100.                    
026010         88  RESOURCE-WRONG                 VALUE 904.                    
026100     03  GOOD-SQLCODECODES.                                               
026200         05  GOOD-SQLCODE OCCURS 5                                        
026300             INDEXED BY SQLCODE-IX PIC 9(3).                              
026400     EJECT                                                                
026500******************************************************************        
026600*                                                                         
026700*        ARBETS-AREOR TILL IMS-SEKTIONERNA                                
026800*                                                                         
026900 01  IMS-WS.                                                              
027000   03    FILLER          PIC X(16)   VALUE 'IMS-WS'.                      
027100     SKIP3                                                                
027200*****                    **** STATUS-KOD FRÅN IMS                         
027300   03    STATUS-WS       PIC XX.                                          
027400         88  SEGMENT-FINNS       VALUE '  '.                              
027500         88  SEGMENT-SAKNAS      VALUE 'GE'.                              
027600         88  SEGMENT-FINNS-REDAN VALUE 'II'.                              
027700     SKIP3                                                                
027800   03    GODK-STATUSKODER.                                                
027900     05  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
028000     SKIP3                                                                
028100 01      SSA1            PIC X(64).                                       
028200 01      SSA2            PIC X(64).                                       
028300 01      SSA3            PIC X(64).                                       
028400     EJECT                                                                
028500*                            IMS FUNKTIONSKODER                           
028600*01      -COPY W0003                                                      
028700     EJECT                                                                
028800*                            DLI INPUT-OUTPUT AREA                        
028900 01  DLI-IO-AREA-1.                                                       
029000     03  IO-AREA-1       PIC X(110)  VALUE SPACE.                         
029100*    03  WLARTC01 -COPY WDK601               -RED IO-AREA-1.              
029200     EJECT                                                                
029300 01  DLI-IO-AREA-2.                                                       
029400     03  IO-AREA-2       PIC X(928)  VALUE SPACE.                         
029500*    03  WLARTC11 -COPY WDK611               -RED IO-AREA-2.              
029600     EJECT                                                                
029700 01  DLI-IO-AREA-3.                                                       
029800     03  IO-AREA-3       PIC X(120) VALUE SPACE.                          
029900*    03  WLARTC25 -COPY WDK625               -RED IO-AREA-3.              
030000     EJECT                                                                
030100*    03  WLBENA01 -COPY WDD301 -PRE BENA-    -RED IO-AREA-3.              
030200     EJECT                                                                
030300*    03  WLBENA11 -COPY WDD311 -PRE BENA-    -RED IO-AREA-3.              
030400     EJECT                                                                
030500*    03  WLKATN01 -COPY WDN601 -PRE KATN-    -RED IO-AREA-3.              
030600     EJECT                                                                
030700*    03  WLKATN11 -COPY WDN611 -PRE KATN-    -RED IO-AREA-3.              
030800     EJECT                                                                
030900 01  DLI-IO-AREA-7.                                                       
031000     03  IO-AREA-7       PIC X(300)  VALUE SPACE.                         
031100*    03  WDF701   -COPY WDF701               -RED IO-AREA-7.              
031200     EJECT                                                                
031300 01  FILLER              PIC X(16) VALUE 'DLI-IO-WDK401'.                 
031400 01  DLI-IO-WDK401.                                                       
031500*    03  -COPY WDK401.                                                    
031600     EJECT                                                                
031700                                                                          
031800*    --------------- DB2 INPUT-OUTPUT AREA ---------------                
031900                                                                          
032000 01  FILLER                      PIC X(16)  VALUE 'TP1KAMP-AREA'.         
032100                                                                          
032200*01  -COPY TP1KAMP -PRE TP1KAMP-                                          
032300     EJECT                                                                
032400 01  FILLER                      PIC X(16)  VALUE 'TP1ARTK-AREA'.         
032500                                                                          
032600*01  -COPY TP1ARTK -PRE TP1ARTK-                                          
032700     EJECT                                                                
032800 01  FILLER                      PIC X(16)  VALUE 'TB1ACCE-AREA'.         
032900                                                                          
033000*01  -COPY TB1ACCE -PRE TB1ACCE-                                          
033100     EJECT                                                                
033200     EXEC SQL INCLUDE TP1KAMP END-EXEC.                                   
033300     EJECT                                                                
033400     EXEC SQL INCLUDE TP1ARTK END-EXEC.                                   
033500     EJECT                                                                
033600     EXEC SQL INCLUDE TB1ACCE END-EXEC.                                   
033700     EJECT                                                                
033800                                                                          
033900 LINKAGE SECTION.                                                         
034000*01  -COPY W0009     -PRE MSG-                                            
034100     EJECT                                                                
034200*01  -COPY W0008     -PRE USEA-                                           
034300         05  FILLER           PIC X.                                      
034400     EJECT                                                                
034500*01  -COPY W0008     -PRE ARTC-                                           
034600         05  FILLER           PIC X.                                      
034700     EJECT                                                                
034800*01  -COPY W0008     -PRE BENA-                                           
034900         05  FILLER           PIC X.                                      
035000     EJECT                                                                
035100*01  -COPY W0008     -PRE KATN-                                           
035200         05  FILLER           PIC X.                                      
035300     EJECT                                                                
035400*01  -COPY W0008     -PRE WDF7-                                           
035500         05  FILLER           PIC X.                                      
035600     EJECT                                                                
035700*01  -COPY W0008     -PRE WDK4-                                           
035800         05  FILLER           PIC X.                                      
035900     EJECT                                                                
036000 PROCEDURE DIVISION USING MSG-PCB USEA-PCB                                
036100                                  ARTC-PCB                                
036200                                  BENA-PCB KATN-PCB WDF7-PCB              
036300                                  WDK4-PCB.                               
036400     ENTRY 'DLITCBL' USING MSG-PCB USEA-PCB                               
036500                                   ARTC-PCB BENA-PCB                      
036600                                   KATN-PCB WDF7-PCB                      
036700                                   WDK4-PCB.                              
036800                                                                          
036900     PERFORM IMS-GET-MSG                                                  
037000                                                                          
037100     IF SEGMENT-FINNS                                                     
037200       PERFORM A-SPARA-NYCKLAR-OCH-INIT                                   
037300                                                                          
037400       PERFORM E-KONTROLL-VCBV                                            
037500                                                                          
037600       IF IDARTNR-WS NOT NUMERIC                                          
037700         MOVE FEL-1 (INDX) TO MOD-TEMFSFEL                                
037800       ELSE                                                               
037900         MOVE IDARTNR-WS TO W-IDARTNR                                     
038000                            W-IDARTNR-F7                                  
038100         IF MFS-UPDATE                                                    
038200           PERFORM B-UPPDATERA                                            
038300         ELSE                                                             
038400           PERFORM C-REDIGERA-BILD                                        
038500                                                                          
038600           IF W-FLERS = JA                                                
038700             IF W-KDERS = +29 OR +52                                      
038800               MOVE MED-9 (INDX) TO MOD-TEMFSFEL                          
038900             ELSE                                                         
039000               EVALUATE TRUE                                              
039100               WHEN W-KDERS = +9 OR +19                                   
039200                 MOVE MED-10 (INDX) TO MOD-TEMFSFEL                       
039300               WHEN W-KDERS > +0                                          
039400                 MOVE MED-8 (INDX) TO MOD-TEMFSFEL                        
039500                WHEN OTHER                                                
039600                 MOVE MED-3 (INDX) TO MOD-TEMFSFEL                        
039700               END-EVALUATE                                               
039800             END-IF                                                       
039900           ELSE                                                           
040000             IF W-KDERS = +29 OR +52                                      
040100               MOVE MED-4 (INDX) TO MOD-TEMFSFEL                          
040200             ELSE                                                         
040300               EVALUATE TRUE                                              
040400               WHEN W-KDERS = +9 OR +19                                   
040500                 MOVE MED-5 (INDX) TO MOD-TEMFSFEL                        
040600               WHEN W-KDERS > +0                                          
040700                 MOVE MED-6 (INDX) TO MOD-TEMFSFEL                        
040800               END-EVALUATE                                               
040900             END-IF                                                       
041000           END-IF                                                         
041100         END-IF                                                           
041200       END-IF                                                             
041300     END-IF                                                               
041400*     ---- SKRIV EV. UT MEDDELANDEN PÅ RAD 23 ------------                
041500     IF WS-TEMFSINF = SPACE  AND WS-TEMFSINF-KAMP = SPACE                 
041600         CONTINUE                                                         
041700     ELSE                                                                 
041800       IF WS-TEMFSINF = SPACE                                             
041900           MOVE WS-TEMFSINF-KAMP TO MOD-TEMFSINF                          
042000       ELSE                                                               
042100         IF WS-TEMFSINF-KAMP = SPACE                                      
042200             MOVE WS-TEMFSINF TO MOD-TEMFSINF                             
042300         ELSE                                                             
042400*            --- OBS MAX. 55 TECKEN                                       
042500             STRING WS-TEMFSINF-KAMP  DELIMITED BY SIZE                   
042600                    WS-TEMFSINF-SPLIT DELIMITED BY SIZE                   
042700                    WS-TEMFSINF       DELIMITED BY SIZE                   
042800             INTO MOD-TEMFSINF                                            
042900         END-IF                                                           
043000       END-IF                                                             
043100     END-IF                                                               
043200                                                                          
043300                                                                          
043400     PERFORM IMS-INSERT-MSG                                               
043500                                                                          
043600     MOVE ZERO TO RETURN-CODE                                             
043700                                                                          
043800     GOBACK                                                               
043900     CONTINUE.                                                            
044000     EJECT                                                                
044100 A-SPARA-NYCKLAR-OCH-INIT SECTION.                                        
044200                                                                          
044300     IF MSG-DUBBLA-TRANSKODER                                             
044400       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W1I10101-CTX             
044500       MOVE MSG-IDTRANS-2 TO MFS-IDTRANS                                  
044600       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
044700       MOVE MSG-KDTRTYP          TO MFS-KDTRTYP                           
044800*SPECIAL-TEST                                                             
044900       MOVE ZERO TO LASTA-SEG                                             
045000     ELSE                                                                 
045100       MOVE MSG-INDATA-MINUS-1-TRANSKOD TO MID-W1I10101-CTX               
045200       MOVE MSG-IDTRANS-1 TO MFS-IDTRANS                                  
045300       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
045400       MOVE ' ' TO MFS-KDTRTYP                                            
045500     END-IF                                                               
045600     MOVE ALL '+' TO MSGI-WMSGINIT                                        
045700     MOVE '001'             TO MSGI-KDCALL                                
045800     MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                                
045900     MOVE MSG-LTERM-NAME    TO MSGI-IDLTERM-USER                          
046000     MOVE '1101'            TO MSGI-IDTRANS                               
046100     IF MFS-IDTRANS = '1101'                                              
046200     OR (MID-IDARTNR-IN NUMERIC                                           
046300     AND MID-IDARTNR-IN > ZERO)                                           
046400         MOVE MID-IDARTNR-IN TO MSGI-IDARTNR                              
046500     END-IF                                                               
046600     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
046700     MOVE MSGI-IDARTNR TO IDARTNR-WS                                      
046800     MOVE MSGI-IDDC    TO WS-IDDC                                         
046900                                                                          
047000     IF MSGI-IDLAND-SPR NOT = 'GB'                                        
047100       MOVE +1 TO INDX                                                    
047200     ELSE                                                                 
047300       MOVE +2 TO INDX                                                    
047400     END-IF                                                               
047500                                                                          
047600     INSPECT IDARTNR-WS REPLACING ALL SPACE BY ZERO                       
047700     IF MID-IDARTNR-IN NOT = ALL '+'                                      
047800       MOVE ' ' TO MFS-KDTRTYP                                            
047900*SPECIAL-TEST                                                             
048000       MOVE ZERO TO MID-BLADDRING-ANT                                     
048100       MOVE ZERO TO LASTA-SEG                                             
048200     END-IF                                                               
048300     MOVE LOW-VALUE  TO MSG-AREA                                          
048400     MOVE 'W1O101N1' TO MFS-IDMOD                                         
048500     MOVE '1101' TO MOD-IDTRANS                                           
048600     MOVE IDARTNR-WS TO MOD-IDARTNR-UT                                    
048700     INSPECT MOD-IDARTNR-UT REPLACING LEADING ZERO BY SPACE               
048800                                                                          
048900     COMPUTE MSG-KVLL = LENGTH OF MOD-W1O10101 + 4                        
049000                                                                          
049100     IF MFS-IDTRANS NOT = '1101'                                          
049200       MOVE ' ' TO MFS-KDTRTYP                                            
049300       MOVE ZERO TO MID-BLADDRING-ANT                                     
049400     END-IF                                                               
049500                                                                          
049600     MOVE FUNCTION CURRENT-DATE (1:8) TO WS-DAGENS-AAAAMMDD               
049700                                                                          
049800     MOVE MFS-RENSA-FAELT TO MOD-IDARTNR-IN                               
049900                             MOD-TEMFSFEL                                 
050000                             MOD-TEMFSINF                                 
050100     MOVE SPACE           TO WS-TEMFSINF                                  
050200                             WS-TEMFSINF-KAMP                             
050300                                                                          
050400     MOVE ZERO  TO W-KDERS                                                
050500                   WS-KDERS-UTG                                           
050600     MOVE 'N'   TO W-FLERS                                                
050700     CONTINUE.                                                            
050800     EJECT                                                                
050900 B-UPPDATERA SECTION.                                                     
051000                                                                          
051100     MOVE JA TO INPUT-RETT                                                
051200     PERFORM IMS-GET-ARTC01                                               
051300     IF SEGMENT-FINNS                                                     
051400        MOVE ART-KDPRODSL       TO TEST-KDPRODSL                          
051500        IF KDPRODSL-VOLVO-BIMA                                            
051600           IF CDC OR SDC                                                  
051700              CONTINUE                                                    
051800           ELSE                                                           
051900              MOVE MED-11(INDX) TO MOD-TEMFSFEL                           
052000              MOVE MED-7 (INDX) TO WS-TEMFSINF                            
052100              MOVE NEJ TO INPUT-RETT                                      
052200           END-IF                                                         
052300        END-IF                                                            
052400     END-IF                                                               
052500                                                                          
052600     IF INPUT-RETT = JA                                                   
052700        IF MID-TEARTNOT-3 NOT = ALL '+'                                   
052800          PERFORM IMS-GU-ARTC11                                           
052900          IF SEGMENT-FINNS                                                
053000            MOVE +3 TO W-KDNOTTYP                                         
053100                                                                          
053200            IF MID-TEARTNOT-3 = SPACE                                     
053300              PERFORM IMS-GET-HOLD-NOTERING                               
053400              IF SEGMENT-FINNS                                            
053500                PERFORM IMS-DELETE-NOTERING                               
053600                MOVE SPACE TO MOD-TEARTNOT-3                              
053700                MOVE MED-2 (INDX) TO WS-TEMFSINF                          
053800              END-IF                                                      
053900            ELSE                                                          
054000              PERFORM IMS-GET-HOLD-NOTERING                               
054100              MOVE W-KDNOTTYP TO NOT-KDNOTTYP                             
054200              MOVE MID-TEARTNOT-3 TO NOT-TEARTNOT                         
054300              IF SEGMENT-FINNS                                            
054400                PERFORM IMS-REPLACE-NOTERING                              
054500              ELSE                                                        
054600                PERFORM IMS-INSERT-NOTERING                               
054700              END-IF                                                      
054800              MOVE MED-2 (INDX)    TO WS-TEMFSINF                         
054900              MOVE NOT-TEARTNOT TO MOD-TEARTNOT-3                         
055000              MOVE MFS-ADD-LYS-UPP-FAELT                                  
055100                           TO MOD-TEARTNOT-3-ATTR                         
055200            END-IF                                                        
055300          ELSE                                                            
055400            MOVE MED-4 (INDX) TO MOD-TEMFSFEL                             
055500          END-IF                                                          
055600                                                                          
055700        ELSE                                                              
055800          MOVE MFS-ROER-EJ-FAELT TO MOD-TEARTNOT-3                        
055900        END-IF                                                            
056000                                                                          
056100        IF WS-TEMFSINF       NOT = MED-2 (INDX)                           
056200          MOVE MED-7 (INDX) TO WS-TEMFSINF                                
056300        END-IF                                                            
056400                                                                          
056500        IF MID-TEARTNOT-7 NOT = ALL '+'                                   
056600          PERFORM IMS-GU-ARTC11                                           
056700          IF SEGMENT-FINNS                                                
056800            MOVE +7 TO W-KDNOTTYP                                         
056900                                                                          
057000            IF MID-TEARTNOT-7 = SPACE                                     
057100              PERFORM IMS-GET-HOLD-NOTERING                               
057200              IF SEGMENT-FINNS                                            
057300                PERFORM IMS-DELETE-NOTERING                               
057400                MOVE SPACE TO MOD-TEARTNOT-7                              
057500                MOVE MED-2 (INDX) TO WS-TEMFSINF                          
057600              END-IF                                                      
057700            ELSE                                                          
057800              PERFORM IMS-GET-HOLD-NOTERING                               
057900              MOVE W-KDNOTTYP TO NOT-KDNOTTYP                             
058000              MOVE MID-TEARTNOT-7 TO NOT-TEARTNOT                         
058100              IF SEGMENT-FINNS                                            
058200                PERFORM IMS-REPLACE-NOTERING                              
058300              ELSE                                                        
058400                PERFORM IMS-INSERT-NOTERING                               
058500              END-IF                                                      
058600              MOVE MED-2 (INDX)    TO WS-TEMFSINF                         
058700              MOVE NOT-TEARTNOT TO MOD-TEARTNOT-7                         
058800              MOVE MFS-ADD-LYS-UPP-FAELT                                  
058900                           TO MOD-TEARTNOT-7-ATTR                         
059000            END-IF                                                        
059100          ELSE                                                            
059200            MOVE MED-4 (INDX) TO MOD-TEMFSFEL                             
059300          END-IF                                                          
059400                                                                          
059500        ELSE                                                              
059600          MOVE MFS-ROER-EJ-FAELT TO MOD-TEARTNOT-7                        
059700        END-IF                                                            
059800                                                                          
059900        IF WS-TEMFSINF       NOT = MED-2 (INDX)                           
060000          MOVE MED-7 (INDX) TO WS-TEMFSINF                                
060100        END-IF                                                            
060200     END-IF                                                               
060300                                                                          
060400     PERFORM D-SPARA-SKARMBILD                                            
060500     CONTINUE.                                                            
060600     EJECT                                                                
060700 C-REDIGERA-BILD SECTION.                                                 
060800                                                                          
060900     PERFORM IMS-GET-ARTC01                                               
061000     IF SEGMENT-SAKNAS                                                    
061100       MOVE FEL-2 (INDX)       TO MOD-TEMFSFEL                            
061200       PERFORM CA-RENSA-FAELT                                             
061300     ELSE                                                                 
061400*      --- KOLLAR OM REG. ARTIKEL ÄR KAMPANJARTIKEL                       
061500       PERFORM CC-KOLLA-KAMPANJ                                           
061600       PERFORM DB2-SELECT-TB1ACCE-TAB                                     
061610       IF LINES-FOUND                                                     
061700         MOVE TB1ACCE-BEUPPDSU   TO MOD-BEUPPDSU                          
061701       ELSE                                                               
061710         MOVE SPACES             TO MOD-BEUPPDSU                          
061800       END-IF                                                             
061810                                                                          
061900       IF ART-FLIART = JA                                                 
062000         MOVE JA TO MOD-KDIART                                            
062100       ELSE                                                               
062200         MOVE NEJ TO MOD-KDIART                                           
062300       END-IF                                                             
062400       MOVE '-'                TO MOD-STRECK                              
062500       MOVE ART-REKSIFFR       TO MOD-REKSIFFR                            
062600       MOVE ART-TIERSDAT       TO MOD-TIERSDAT                            
062700       IF ART-TIERSDAT = ZERO                                             
062800          INSPECT MOD-TIERSDAT REPLACING LEADING ZERO BY SPACE            
062900       END-IF                                                             
063000       MOVE ART-TISOP          TO MOD-TISOP                               
063100       MOVE ART-KDPRODSL       TO MOD-KDPRODSL                            
063200       MOVE ART-IDFKNGRP       TO MOD-IDFKNGRP                            
063300       MOVE ART-KDSORT         TO MOD-KDSORT                              
063400       MOVE ART-KDERS-UTG      TO W-KDERS                                 
063500       MOD-KDERS                                                          
063600       MOVE ART-KDERS-UTG      TO WS-KDERS-UTG                            
063700       MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-KDERS-ATTR                       
063800       MOVE ART-FLERS          TO W-FLERS                                 
063900       MOVE ART-IDLEVNR        TO MOD-IDLEVNR                             
064000******************************** DATUM KONVERTERING!!!!                   
064100       MOVE ART-TIREGDAT       TO MOD-TIREGDAT                            
064200******************************** DATUM KONVERTERING!!!!                   
064300                                                                          
064400       SET MOD-IX TO +1                                                   
064500       MOVE +1    TO XIDAO                                                
064600       PERFORM UNTIL                                                      
064700        NOT ( XIDAO < MAX-ANT-IDAO )                                      
064800         IF ART-IDAO (XIDAO) = SPACE                                      
064900           MOVE MFS-RENSA-FAELT  TO MOD-IDAO (MOD-IX)                     
065000         ELSE                                                             
065100           MOVE ART-IDAO (XIDAO) TO MOD-IDAO (MOD-IX)                     
065200         END-IF                                                           
065300         SET MOD-IX UP BY +1                                              
065400         ADD +1                  TO XIDAO                                 
065500       END-PERFORM                                                        
065600                                                                          
065700       MOVE MFS-RENSA-FAELT TO MOD-IDSKYLT-TILLV                          
065800       PERFORM IMS-GET-ARTC11                                             
065900       IF SEGMENT-SAKNAS                                                  
066000          PERFORM CB-RENSA-FAELT                                          
066100       ELSE                                                               
066200         MOVE CLAG-IDBERED        TO MOD-IDBERED                          
066300         MOVE CLAG-IDPROJ         TO MOD-IDPROJ                           
066400         MOVE CLAG-IDPROJUP       TO MOD-IDPROJUP                         
066500         MOVE CLAG-IDRITN         TO MOD-IDRITN                           
066600         MOVE CLAG-KDPSLLOC       TO MOD-KDPSLLOC                         
066700         MOVE CLAG-IDKAT(1)       TO MOD-IDKAT-1                          
066800         MOVE CLAG-IDKAT(2)       TO MOD-IDKAT-2                          
066900         MOVE CLAG-IDKAT(3)       TO MOD-IDKAT-3                          
067000                                                                          
067100         IF CLAG-IDPROENH(1) = SPACE                                      
067200            MOVE MFS-RENSA-FAELT TO MOD-IDPROENH(1)                       
067300         ELSE                                                             
067400            MOVE CLAG-IDPROENH(1) TO MOD-IDPROENH (1)                     
067500            INSPECT MOD-IDPROENH(1) REPLACING LEADING ZERO BY             
067600                   SPACE                                                  
067700         END-IF                                                           
067800         IF CLAG-IDPROENH(2) = SPACE                                      
067900            MOVE MFS-RENSA-FAELT TO MOD-IDPROENH(2)                       
068000         ELSE                                                             
068100            MOVE CLAG-IDPROENH(2) TO MOD-IDPROENH (2)                     
068200            INSPECT MOD-IDPROENH(2) REPLACING LEADING ZERO BY             
068300                   SPACE                                                  
068400         END-IF                                                           
068500         IF CLAG-IDPROENH(3) = SPACE                                      
068600            MOVE MFS-RENSA-FAELT TO MOD-IDPROENH(3)                       
068700         ELSE                                                             
068800            MOVE CLAG-IDPROENH(3) TO MOD-IDPROENH (3)                     
068900            INSPECT MOD-IDPROENH(3) REPLACING LEADING ZERO BY             
069000                   SPACE                                                  
069100         END-IF                                                           
069200                                                                          
069300         MOVE CLAG-IDANSK     TO MOD-IDANSK                               
069400         MOVE CLAG-FLGEMART   TO MOD-FLGEMART                             
069500                                                                          
069600         IF SEC-KDSVAR = 4                                                
069700           MOVE MFS-RENSA-FAELT TO MOD-PRARTSTD                           
069800         ELSE                                                             
069900           MOVE CLAG-PRARTSTD  TO MOD-PRARTSTD                            
070000         END-IF                                                           
070100                                                                          
070200         MOVE CLAG-FLLSRDEL  TO MOD-FLLSRDEL                              
070300         MOVE CLAG-KDUART    TO MOD-KDUART                                
070400         MOVE CLAG-KDBPSR    TO MOD-KDBPSR                                
070500                                                                          
070600         MOVE CLAG-KDERS     TO MOD-KDERS                                 
070700                                W-KDERS                                   
070800         MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-KDERS-ATTR                     
070900                                                                          
071000         IF SEC-KDSVAR = 4                                                
071100            MOVE MFS-RENSA-FAELT TO MOD-KVDISP                            
071200         ELSE                                                             
071300            COMPUTE MOD-KVDISP = (CLAG-KVLS - CLAG-KVRESS)                
071400         END-IF                                                           
071500       END-IF                                                             
071600                                                                          
071700       MOVE MFS-RENSA-FAELT TO MOD-TEARTNOT-3                             
071800       MOVE +3 TO W-KDNOTTYP                                              
071900       PERFORM IMS-GET-ARTC25                                             
072000       IF SEGMENT-FINNS                                                   
072100          MOVE NOT-TEARTNOT TO MOD-TEARTNOT-3                             
072200       END-IF                                                             
072300                                                                          
072400       MOVE MFS-RENSA-FAELT TO MOD-TEARTNOT-7                             
072500       MOVE +7 TO W-KDNOTTYP                                              
072600       PERFORM IMS-GET-ARTC25                                             
072700       IF SEGMENT-FINNS                                                   
072800          MOVE NOT-TEARTNOT TO MOD-TEARTNOT-7                             
072900       END-IF                                                             
073000                                                                          
073100       PERFORM IMS-GET-BENA01-BSEQ                                        
073200       IF SEGMENT-FINNS                                                   
073300         MOVE 'GB ' TO W-IDSKYLT                                          
073400         PERFORM IMS-GET-BENA11-BSEQ                                      
073500         IF SEGMENT-FINNS                                                 
073600           MOVE BENA-TEXT-BEART TO MOD-BEART-ENG                          
073700         END-IF                                                           
073800         MOVE 'S  ' TO W-IDSKYLT                                          
073900         PERFORM IMS-GET-BENA11-BSEQ                                      
074000         IF SEGMENT-FINNS                                                 
074100           MOVE BENA-TEXT-BEART TO MOD-BEART-SVE                          
074200         END-IF                                                           
074300       END-IF                                                             
074400       PERFORM IMS-GET-MASTER-WDN6                                        
074500       IF SEGMENT-FINNS                                                   
074600         PERFORM IMS-GET-KATINFO-MASTER                                   
074700         IF MID-BLADDRING-ANT NOT NUMERIC                                 
074800            MOVE ZERO TO MID-BLADDRING-ANT                                
074900         END-IF                                                           
075000         IF MID-BLADDRING-ANT = ZERO                                      
075100           CONTINUE                                                       
075200         ELSE                                                             
075300           SET MOD-IZ      TO +1                                          
075400           MOVE MID-BLADDRING-ANT TO LASTA-SEG                            
075500           PERFORM UNTIL                                                  
075600            NOT ( MOD-IZ < LASTA-SEG + 1 AND SEGMENT-FINNS )              
075700             PERFORM IMS-GET-KATINFO-MASTER                               
075800             SET MOD-IZ   UP BY +1                                        
075900           END-PERFORM                                                    
076000         END-IF                                                           
076100         SET MOD-IZ         TO +1                                         
076200         PERFORM UNTIL                                                    
076300          NOT ( MOD-IZ < 15 AND SEGMENT-FINNS )                           
076400           MOVE KATN-KAT-BEEMBLEM TO MOD-KAT-BEEMBLEM (MOD-IZ)            
076500           ADD +1          TO LASTA-SEG                                   
076600           PERFORM IMS-GET-KATINFO-MASTER                                 
076700           SET MOD-IZ UP BY +1                                            
076800         END-PERFORM                                                      
076900         IF SEGMENT-FINNS                                                 
077000           MOVE MED-1 (INDX) TO WS-TEMFSINF                               
077100           MOVE LASTA-SEG    TO MOD-BLADDRING-ANT                         
077200         ELSE                                                             
077300           MOVE ZERO         TO MOD-BLADDRING-ANT                         
077400         END-IF                                                           
077500       END-IF                                                             
077600     END-IF                                                               
077700     IF MFS-IDTRANS = '1101' AND MID-IDARTNR-IN = SPACE                   
077800     OR MFS-IDTRANS = '1101' AND MID-IDARTNR-IN = ALL '+'                 
077900       MOVE MFS-ROER-EJ-FAELT TO MOD-TEARTNOT-3                           
078000                                 MOD-TEARTNOT-7                           
078100                                                                          
078200       IF MID-TEARTNOT-3 NOT = ALL '+'                                    
078300         MOVE MFS-ALFA-FAELT-RAETT TO MOD-TEARTNOT-3-ATTR                 
078400       END-IF                                                             
078500       IF MID-TEARTNOT-7 NOT = ALL '+'                                    
078600         MOVE MFS-ALFA-FAELT-RAETT TO MOD-TEARTNOT-7-ATTR                 
078700       END-IF                                                             
078800     END-IF                                                               
078900                                                                          
079000*    --- LÄS WDF7  "CROSS REF FORD-VOLVO"                                 
079100     PERFORM IMS-GU-WDF701                                                
079200     IF SEGMENT-FINNS                                                     
079300       MOVE MPNR-FLGEMFMC   TO MOD-FLGEMFMC                               
079400     ELSE                                                                 
079500       MOVE '-'             TO MOD-FLGEMFMC                               
079600     END-IF                                                               
079700                                                                          
079800*    ___ LÄS WDK4 FÖR 'SKALADE' ARTIKLAR                                  
079900     IF WS-KDERS-UTG > 0                                                  
080000        PERFORM IMS-GU-WDK401                                             
080100        IF SEGMENT-FINNS                                                  
080200          MOVE RENS-TEARTNOT-BER    TO MOD-TEARTNOT-3                     
080300          MOVE RENS-TEARTNOT-BERIMP TO MOD-TEARTNOT-7                     
080400        END-IF                                                            
080500     END-IF                                                               
080600     .                                                                    
080700     EJECT                                                                
080800 CA-RENSA-FAELT    SECTION.                                               
080900                                                                          
081000     MOVE MFS-RENSA-FAELT   TO MOD-IDANSK                                 
081100                               MOD-IDLEVNR                                
081200                               MOD-KDPRODSL                               
081300                               MOD-KDUART                                 
081400                               MOD-KDIART                                 
081500                               MOD-IDSKYLT-TILLV                          
081600                               MOD-PRARTSTD                               
081700                               MOD-IDPROJ                                 
081800                               MOD-IDPROJUP                               
081900                               MOD-IDRITN                                 
082000                               MOD-KDERS                                  
082100                               MOD-KVDISP                                 
082200                               MOD-IDFKNGRP                               
082300                               MOD-KDSORT                                 
082400                               MOD-KDBPSR                                 
082500                               MOD-IDBERED                                
082600                               MOD-FLGEMART                               
082700                               MOD-FLLSRDEL                               
082800                               MOD-BEART-SVE                              
082900                               MOD-BEART-ENG                              
082910                               MOD-BEUPPDSU                               
083000                               MOD-TIREGDAT                               
083100                               MOD-TISOP                                  
083200                               MOD-TIERSDAT                               
083300                               MOD-IDKAT-1                                
083400                               MOD-IDKAT-2                                
083500                               MOD-IDKAT-3                                
083600                               MOD-TEARTNOT-3                             
083700                               MOD-TEARTNOT-7                             
083800                               MOD-IDPROENH(1)                            
083900                               MOD-IDPROENH(2)                            
084000                               MOD-IDPROENH(3)                            
084100                                                                          
084200     SET MOD-IX TO +1                                                     
084300     PERFORM UNTIL                                                        
084400      ( MOD-IX > +5 )                                                     
084500       MOVE MFS-RENSA-FAELT TO MOD-IDAO (MOD-IX)                          
084600       SET MOD-IX UP BY +1                                                
084700     END-PERFORM                                                          
084800     SET MOD-IZ TO +1                                                     
084900     PERFORM UNTIL                                                        
085000      ( MOD-IZ > +14 )                                                    
085100       MOVE MFS-RENSA-FAELT TO MOD-KAT-BEEMBLEM (MOD-IZ)                  
085200       SET MOD-IZ UP BY +1                                                
085300     END-PERFORM                                                          
085400     CONTINUE.                                                            
085500     EJECT                                                                
085600 CB-RENSA-FAELT SECTION.                                                  
085700     MOVE MFS-RENSA-FAELT TO MOD-IDBERED                                  
085800                             MOD-IDPROJ                                   
085900                             MOD-IDPROJUP                                 
086000                             MOD-IDRITN                                   
086100                             MOD-IDKAT-1                                  
086200                             MOD-IDKAT-2                                  
086300                             MOD-IDKAT-3                                  
086400                             MOD-IDPROENH(1)                              
086500                             MOD-IDPROENH(2)                              
086600                             MOD-IDPROENH(3)                              
086700                             MOD-IDANSK                                   
086800                             MOD-FLGEMART                                 
086900                             MOD-PRARTSTD                                 
087000                             MOD-FLLSRDEL                                 
087100                             MOD-KDUART                                   
087200                             MOD-KDBPSR                                   
087300                             MOD-KVDISP                                   
087400     .                                                                    
087500     EJECT                                                                
087600 CC-KOLLA-KAMPANJ   SECTION.                                              
087700     SKIP2                                                                
087800     PERFORM DB2-DCL-OPN-TP1ARTK-CRS                                      
087900     IF SQLCODE-WS = ZERO                                                 
088000       PERFORM DB2-FETCH-TP1ARTK-CRS                                      
088100     END-IF                                                               
088200                                                                          
088300     MOVE ZERO               TO WS-ANTAL-KAMP                             
088400     MOVE NEJ                TO WS-FLAGGA-Q-KAMP                          
088500                                WS-FLAGGA-W-S-KAMP                        
088600     PERFORM UNTIL SQLCODE > ZERO                                         
088700       IF TP1KAMP-TISTODAT-KAMP > ZERO                                    
088800         MOVE TP1KAMP-TISTODAT-KAMP                                       
088900                             TO WS-JMFR-AAAAMMDD                          
089000       ELSE                                                               
089100         MOVE TP1KAMP-TISTADAT-KAMP                                       
089200                             TO WS-JMFR-AAAAMMDD                          
089300       END-IF                                                             
089400       IF WS-JMFR-AA > 50                                                 
089500         MOVE 19             TO WS-JMFR-AAAAMMDD (1:2)                    
089600       ELSE                                                               
089700         MOVE 20             TO WS-JMFR-AAAAMMDD (1:2)                    
089800       END-IF                                                             
089900       IF TP1KAMP-TISTODAT-KAMP = ZERO                                    
090000*    LÄGG TILL 5 ÅR                                                       
090100         ADD 50000           TO WS-JMFR-AAAAMMDD                          
090200       END-IF                                                             
090300       IF WS-JMFR-AAAAMMDD >= WS-DAGENS-AAAAMMDD                          
090400         IF TP1KAMP-KDKAMP = 'Q'                                          
090500           MOVE JA           TO WS-FLAGGA-Q-KAMP                          
090600         END-IF                                                           
090700         IF TP1KAMP-KDKAMP = 'W'                                          
090800         OR TP1KAMP-KDKAMP = 'S'                                          
090900           MOVE JA           TO WS-FLAGGA-W-S-KAMP                        
091000         END-IF                                                           
091100       END-IF                                                             
091200       ADD 1                 TO WS-ANTAL-KAMP                             
091300       PERFORM DB2-FETCH-TP1ARTK-CRS                                      
091400     END-PERFORM                                                          
091500                                                                          
091600     IF  WS-FLAGGA-Q-KAMP   = JA                                          
091700     AND WS-FLAGGA-W-S-KAMP = NEJ                                         
091800       MOVE MED-92 (INDX)    TO WS-TEMFSINF-KAMP                          
091900*            SM ETC                                                       
092000     ELSE                                                                 
092100       IF WS-FLAGGA-W-S-KAMP = JA                                         
092200       MOVE MED-91 (INDX)    TO WS-TEMFSINF-KAMP                          
092300*            CAMPAIGN                                                     
092400       END-IF                                                             
092500     END-IF                                                               
092600*    MOVE WS-ANTAL-KAMP      TO WS-TEMFSINF-KAMP                          
092700     PERFORM DB2-CLOSE-TP1ARTK-CRS                                        
092800     .                                                                    
092900     EJECT                                                                
093000 D-SPARA-SKARMBILD SECTION.                                               
093100                                                                          
093200     MOVE MFS-ROER-EJ-FAELT TO MOD-IDARTNR-UT                             
093300                               MOD-IDANSK                                 
093400                               MOD-IDLEVNR                                
093500                               MOD-KDPRODSL                               
093600                               MOD-KDUART                                 
093700                               MOD-KDIART                                 
093800                               MOD-IDSKYLT-TILLV                          
093900                               MOD-IDKAT-1                                
094000                               MOD-IDKAT-2                                
094100                               MOD-IDKAT-3                                
094200     IF SEC-KDSVAR = 4                                                    
094300       MOVE MFS-RENSA-FAELT  TO MOD-PRARTSTD                              
094400     ELSE                                                                 
094500       MOVE MFS-ROER-EJ-FAELT TO MOD-PRARTSTD                             
094600     END-IF                                                               
094700     MOVE MFS-ROER-EJ-FAELT TO MOD-IDPROJ                                 
094800                               MOD-IDPROJUP                               
094900                               MOD-IDRITN                                 
095000                                                                          
095100     SET MOD-IY TO +1                                                     
095200     PERFORM UNTIL                                                        
095300      ( MOD-IY > +3 )                                                     
095400       MOVE MFS-ROER-EJ-FAELT TO MOD-IDPROENH (MOD-IY)                    
095500       SET MOD-IY UP BY +1                                                
095600     END-PERFORM                                                          
095700     MOVE MFS-ROER-EJ-FAELT TO MOD-KDERS                                  
095800     IF SEC-KDSVAR = 4                                                    
095900       MOVE MFS-RENSA-FAELT TO MOD-KVDISP                                 
096000     ELSE                                                                 
096100       MOVE MFS-ROER-EJ-FAELT TO MOD-KVDISP                               
096200     END-IF                                                               
096300     MOVE MFS-ROER-EJ-FAELT TO MOD-IDFKNGRP                               
096400                               MOD-KDSORT                                 
096500                               MOD-KDBPSR                                 
096600                               MOD-IDBERED                                
096700                               MOD-FLGEMART                               
096800                               MOD-FLLSRDEL                               
096900                               MOD-BEART-SVE                              
097000                               MOD-BEART-ENG                              
097010                               MOD-BEUPPDSU                               
097100                               MOD-TIREGDAT                               
097200                               MOD-TISOP                                  
097300                               MOD-TIERSDAT                               
097400                                                                          
097500     SET MOD-IX TO +1                                                     
097600     PERFORM UNTIL                                                        
097700      ( MOD-IX > +5 )                                                     
097800       MOVE MFS-ROER-EJ-FAELT TO MOD-IDAO (MOD-IX)                        
097900       SET MOD-IX UP BY +1                                                
098000     END-PERFORM                                                          
098100     SET MOD-IZ TO +1                                                     
098200     PERFORM UNTIL                                                        
098300      ( MOD-IZ > +14 )                                                    
098400       MOVE MFS-ROER-EJ-FAELT TO MOD-KAT-BEEMBLEM (MOD-IZ)                
098500       SET MOD-IZ UP BY +1                                                
098600     END-PERFORM                                                          
098700     CONTINUE.                                                            
098800     EJECT                                                                
098900 E-KONTROLL-VCBV SECTION.                                                 
099000                                                                          
099100     MOVE MSG-SIGNON-USERID   TO SEC-IDUSER                               
099200     MOVE '1101'              TO SEC-IDTRANS                              
099300                                                                          
099400     CALL WSECURIT USING         SEC-IDUSER                               
099500                                 SEC-IDTRANS                              
099600                                 SEC-IDKEY                                
099700                                 SEC-KDSVAR                               
099800     CONTINUE.                                                            
099900                                                                          
100000     EJECT                                                                
100100* IMS SEKTIONER                                                           
100200     SKIP3                                                                
100300 IMS-GET-MSG SECTION.                                                     
100400                                                                          
100500     MOVE '  QC' TO GODK-STATUSKODER                                      
100600     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
100700     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
100800     PERFORM IMS-STATUSKONTROLL                                           
100900     .                                                                    
101000     SKIP3                                                                
101100 IMS-INSERT-MSG SECTION.                                                  
101200                                                                          
101300     IF MSGI-IDLAND-SPR NOT = 'GB'                                        
101400        MOVE '0' TO MFS-KDHUVOMR                                          
101500     END-IF                                                               
101600     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
101700     MOVE SPACE TO GODK-STATUSKODER                                       
101800     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
101900     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
102000     PERFORM IMS-STATUSKONTROLL                                           
102100     .                                                                    
102200     EJECT                                                                
102300 IMS-GET-ARTC01 SECTION.                                                  
102400                                                                          
102500     STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-X ')'                         
102600            DELIMITED BY SIZE INTO SSA1                                   
102700     MOVE '  GE' TO GODK-STATUSKODER                                      
102800     CALL CBLTDLI USING GU ARTC-PCB DLI-IO-AREA-1 SSA1                    
102900     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
103000     PERFORM IMS-STATUSKONTROLL                                           
103100     .                                                                    
103200     SKIP3                                                                
103300 IMS-GU-ARTC11 SECTION.                                                   
103400                                                                          
103500     STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-X ')'                         
103600            DELIMITED BY SIZE INTO SSA1                                   
103700     MOVE 'WLARTC11 ' TO SSA2                                             
103800     MOVE '  GE' TO GODK-STATUSKODER                                      
103900     CALL CBLTDLI USING GU ARTC-PCB DLI-IO-AREA-2 SSA1 SSA2               
104000     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
104100     PERFORM IMS-STATUSKONTROLL                                           
104200     .                                                                    
104300     SKIP3                                                                
104400 IMS-GET-ARTC11 SECTION.                                                  
104500                                                                          
104600     MOVE 'WLARTC11 ' TO SSA1                                             
104700     MOVE '  GE' TO GODK-STATUSKODER                                      
104800     CALL CBLTDLI USING GNP ARTC-PCB DLI-IO-AREA-2 SSA1                   
104900     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
105000     PERFORM IMS-STATUSKONTROLL                                           
105100     .                                                                    
105200     EJECT                                                                
105300 IMS-GET-ARTC25 SECTION.                                                  
105400                                                                          
105500     MOVE 'WLARTC11 ' TO SSA1                                             
105600     STRING 'WLARTC25*F(KDNOTTYP =' W-KDNOTTYP-X ')'                      
105700            DELIMITED BY SIZE INTO SSA2                                   
105800     MOVE '  GE' TO GODK-STATUSKODER                                      
105900     CALL CBLTDLI USING GHNP ARTC-PCB DLI-IO-AREA-3 SSA1 SSA2             
106000     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
106100     PERFORM IMS-STATUSKONTROLL                                           
106200     .                                                                    
106300     SKIP3                                                                
106400 IMS-GET-HOLD-NOTERING SECTION.                                           
106500                                                                          
106600     STRING 'WLARTC25(KDNOTTYP =' W-KDNOTTYP-X ')'                        
106700            DELIMITED BY SIZE INTO SSA1                                   
106800     MOVE '  GE' TO GODK-STATUSKODER                                      
106900     CALL CBLTDLI USING GHNP ARTC-PCB DLI-IO-AREA-3 SSA1                  
107000     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
107100     PERFORM IMS-STATUSKONTROLL                                           
107200     .                                                                    
107300     EJECT                                                                
107400 IMS-GET-BENA01-BSEQ   SECTION.                                           
107500                                                                          
107600     STRING 'WLBENA01(WDD3BSEQ =' W-IDARTNR-X ')'                         
107700            DELIMITED BY SIZE INTO SSA1                                   
107800     MOVE '  ' TO GODK-STATUSKODER                                        
107900     CALL CBLTDLI USING GU BENA-PCB DLI-IO-AREA-3 SSA1                    
108000     MOVE BENA-STATUS-CODE TO STATUS-WS                                   
108100     PERFORM IMS-STATUSKONTROLL                                           
108200     .                                                                    
108300     SKIP3                                                                
108400 IMS-GET-BENA11-BSEQ   SECTION.                                           
108500                                                                          
108600     STRING 'WLBENA11(IDSKYLT  =' W-IDSKYLT-X ')'                         
108700            DELIMITED BY SIZE INTO SSA1                                   
108800     MOVE '  ' TO GODK-STATUSKODER                                        
108900     CALL CBLTDLI USING GNP BENA-PCB DLI-IO-AREA-3 SSA1                   
109000     MOVE BENA-STATUS-CODE TO STATUS-WS                                   
109100     PERFORM IMS-STATUSKONTROLL                                           
109200     .                                                                    
109300     EJECT                                                                
109400 IMS-GET-MASTER-WDN6 SECTION.                                             
109500                                                                          
109600     STRING 'WLKATN01(IDARTNR  =' W-IDARTNR-X ')'                         
109700            DELIMITED BY SIZE INTO SSA1                                   
109800     MOVE '  GE' TO GODK-STATUSKODER                                      
109900     CALL CBLTDLI USING GU KATN-PCB DLI-IO-AREA-3 SSA1                    
110000     MOVE KATN-STATUS-CODE TO STATUS-WS                                   
110100     PERFORM IMS-STATUSKONTROLL                                           
110200     .                                                                    
110300     SKIP3                                                                
110400 IMS-GET-KATINFO-MASTER SECTION.                                          
110500                                                                          
110600     MOVE 'WLKATN11 ' TO SSA1                                             
110700     MOVE '  GE' TO GODK-STATUSKODER                                      
110800     CALL CBLTDLI USING GNP KATN-PCB DLI-IO-AREA-3 SSA1                   
110900     MOVE KATN-STATUS-CODE TO STATUS-WS                                   
111000     PERFORM IMS-STATUSKONTROLL                                           
111100     .                                                                    
111200     EJECT                                                                
111300 IMS-DELETE-NOTERING SECTION.                                             
111400                                                                          
111500     MOVE '  ' TO GODK-STATUSKODER                                        
111600     CALL CBLTDLI USING DLET ARTC-PCB DLI-IO-AREA-3                       
111700     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
111800     PERFORM IMS-STATUSKONTROLL                                           
111900     .                                                                    
112000     SKIP3                                                                
112100 IMS-INSERT-NOTERING SECTION.                                             
112200                                                                          
112300     STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-X ')'                         
112400            DELIMITED BY SIZE INTO SSA1                                   
112500     MOVE 'WLARTC11 ' TO SSA2                                             
112600     MOVE 'WLARTC25 ' TO SSA3                                             
112700     MOVE '  II' TO GODK-STATUSKODER                                      
112800     CALL CBLTDLI USING                                                   
112900              ISRT ARTC-PCB DLI-IO-AREA-3 SSA1 SSA2 SSA3                  
113000     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
113100     PERFORM IMS-STATUSKONTROLL                                           
113200     .                                                                    
113300     SKIP3                                                                
113400 IMS-REPLACE-NOTERING SECTION.                                            
113500                                                                          
113600     MOVE '  ' TO GODK-STATUSKODER                                        
113700     CALL CBLTDLI USING REPL ARTC-PCB DLI-IO-AREA-3                       
113800     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
113900     PERFORM IMS-STATUSKONTROLL                                           
114000     .                                                                    
114100     EJECT                                                                
114200 IMS-GU-WDF701 SECTION.                                                   
114300                                                                          
114400     STRING 'WDF701  (WDF701KY =' W-WDF701KY-X ')'                        
114500            DELIMITED BY SIZE INTO SSA1                                   
114600     MOVE '  GE' TO GODK-STATUSKODER                                      
114700     CALL CBLTDLI USING GU WDF7-PCB DLI-IO-AREA-7 SSA1                    
114800     MOVE WDF7-STATUS-CODE TO STATUS-WS                                   
114900     PERFORM IMS-STATUSKONTROLL                                           
115000     .                                                                    
115100     SKIP3                                                                
115200 IMS-GU-WDK401 SECTION.                                                   
115300     STRING 'WDK401  (IDARTNR  =' W-IDARTNR-X ')'                         
115400            DELIMITED BY SIZE INTO SSA1                                   
115500     MOVE '  GE' TO GODK-STATUSKODER                                      
115600     CALL CBLTDLI USING GU WDK4-PCB DLI-IO-WDK401 SSA1                    
115700     MOVE WDK4-STATUS-CODE TO STATUS-WS                                   
115800     PERFORM IMS-STATUSKONTROLL                                           
115900     .                                                                    
116000     SKIP3                                                                
116100 IMS-STATUSKONTROLL SECTION.                                              
116200     SET STATUS-IX TO 1                                                   
116300     SEARCH GODK-STATUS                                                   
116400       AT END                                                             
116500         CALL FELLOG                                                      
116600     WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                    
116700     END-SEARCH                                                           
116800     .                                                                    
116900     EJECT                                                                
117000* DB2 SEKTIONER                                                           
117100     SKIP3                                                                
117200                                                                          
117300 DB2-DCL-OPN-TP1ARTK-CRS  SECTION.                                        
117400     MOVE 'DB2-DCL-OPN-TP1ARTK   ' TO  WS-DB2-SEKTION                     
117500                                                                          
117600     MOVE 000100  TO GOOD-SQLCODECODES                                    
117700                                                                          
117800     EXEC SQL                                                             
117900         DECLARE TP1ARTK-CRS CURSOR FOR                                   
118000           SELECT  A.IDKAMP                                               
118100                  ,A.IDARTNR                                              
118200                  ,B.TISTADAT_KAMP                                        
118300                  ,B.TISTODAT_KAMP                                        
118400                  ,B.KDKAMP                                               
118500                                                                          
118600           FROM    TP1ARTK A                                              
118700                  ,TP1KAMP B                                              
118800                                                                          
118900           WHERE   A.IDARTNR = :W-IDARTNR                                 
119000               AND A.IDKAMP  =  B.IDKAMP                                  
119100                                                                          
119200           ORDER BY A.IDARTNR                                             
119300     END-EXEC                                                             
119400                                                                          
119500     MOVE 000100  TO GOOD-SQLCODECODES                                    
119600     EXEC SQL OPEN TP1ARTK-CRS END-EXEC                                   
119700     .                                                                    
119800     SKIP3                                                                
119900 DB2-FETCH-TP1ARTK-CRS  SECTION.                                          
120000     MOVE 'DB2-FETCH-TP1ARTK   ' TO  WS-DB2-SEKTION                       
120100     SKIP2                                                                
120200     MOVE 000100  TO GOOD-SQLCODECODES                                    
120300     EXEC SQL                                                             
120400         FETCH TP1ARTK-CRS INTO                                           
120500                    :TP1KAMP-IDKAMP                                       
120600                   ,:TP1ARTK-IDARTNR                                      
120700                   ,:TP1KAMP-TISTADAT-KAMP                                
120800                   ,:TP1KAMP-TISTODAT-KAMP                                
120900                   ,:TP1KAMP-KDKAMP                                       
121000     END-EXEC                                                             
121100                                                                          
121200     MOVE SQLCODE TO SQLCODE-WS                                           
121300     PERFORM DB2-STATUS-CHECK                                             
121400     .                                                                    
121500     SKIP3                                                                
121600 DB2-CLOSE-TP1ARTK-CRS  SECTION.                                          
121700     MOVE 'DB2-CLOSE-TP1ARTK   ' TO  WS-DB2-SEKTION                       
121800                                                                          
121900     EXEC SQL CLOSE TP1ARTK-CRS END-EXEC                                  
122000     .                                                                    
122100     EJECT                                                                
122200 DB2-SELECT-TB1ACCE-TAB  SECTION.                                         
122300     SKIP2                                                                
122400     MOVE 'DB2-SELECT-TB1ACCE-TAB       ' TO WS-DB2-SEKTION               
122500     MOVE 000100  TO GOOD-SQLCODECODES                                    
122600     EXEC SQL                                                             
122700       SELECT BEUPPDSU                                                    
122900       INTO                                                               
123000          :TB1ACCE-BEUPPDSU                                               
123100       FROM   TB1ACCE                                                     
123200                                                                          
123300       WHERE   IDARTNR = :IDARTNR-WS                                      
123400       ORDER BY TIAOINF DESC                                              
123500       FETCH FIRST 1 ROW ONLY                                             
123530                                                                          
123600     END-EXEC                                                             
123700                                                                          
123800     MOVE SQLCODE TO SQLCODE-WS                                           
123900     PERFORM DB2-STATUS-CHECK                                             
124000     .                                                                    
124100     EJECT                                                                
124200 DB2-STATUS-CHECK  SECTION.                                               
124300                                                                          
124400     SET SQLCODE-IX TO 1                                                  
124500     SEARCH GOOD-SQLCODE                                                  
124600       AT END                                                             
124700*         STRING 'INVALID DB2 SQL STATUS CODE: ' SQLCODE-WS               
124800*         DELIMITED BY SIZE INTO ERROR-TEXT                               
124900          CALL FELLOG                                                     
125000       WHEN GOOD-SQLCODE (SQLCODE-IX) = SQLCODE-WS CONTINUE               
125100     END-SEARCH                                                           
125200     .                                                                    
125300     EJECT                                                                
