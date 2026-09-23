000100 PROCESS DYNAM                                                            
000200 ID DIVISION.                                                             
000300 PROGRAM-ID.                W9040200.                                     
000400*AUTHOR.                    URBAN ZACKRISSON.                             
000500*DATE-COMPILED.                                                           
000600*DATE-WRITTEN.              NOV  -84.                                     
000700*    FUNKTION.   TP-PROGRAM                                               
000800*                BEREDNINGSINFORMATION                                    
000900*                UPPDATERING AV VISS INFORMATION.                         
001000*                                                                         
001100*    INDATA.                                                              
001200*        TRANSAKTION: W9T402                                              
001300*                     W9T402U                                             
001400*        MID:         W90402I1                                            
001500*    UTDATA.                                                              
001600*        MOD:         W90402O1                                            
001700*    SUBPROGRAM.                                                          
001800*        FELLOG                                                           
001900*                                                                         
002000*    ÄNDRING:                                                             
002100*        2005-FEB  ETRACKER=1476814.  VISA KAMPANJ-INFO                   
002200*                                     TILLAGT DB2-LÄSNING                 
002300*                                                                         
002400*        2005-SEP  ETRACKER=2632858.  VISA FORD-MPNR INFO                 
002500*                                     TILLAGT WDF7-LÄSNING                
002600*                                                                         
002700     SKIP3                                                                
002800 ENVIRONMENT DIVISION.                                                    
002900     EJECT                                                                
003000 DATA DIVISION.                                                           
003100     SKIP3                                                                
003200 WORKING-STORAGE SECTION.                                                 
003300                                                                          
003400*    -- CHECKED BY WY2000                                                 
003500 77  IDPGM                       PIC X(8)   VALUE 'W9040200'.             
003600     SKIP3                                                                
003700 77      IDARTNR-WS              PIC X(9)   VALUE SPACE.                  
003800 77      JA                      PIC X(1)   VALUE 'J'.                    
003900 77      NEJ                     PIC X(1)   VALUE 'N'.                    
004000 77      MAX-MOD-LENGD           PIC S9(4)  VALUE +560  COMP SYNC.        
004100 77      MAX-ANT-KAT-TILLH       PIC S9(9)  VALUE +14   COMP SYNC.        
004200 77      MAX-ANT-IDAO            PIC S9(9)  VALUE +6    COMP SYNC.        
004300 77      INDX                    PIC S9(9)  VALUE +0    COMP SYNC.        
004400 77      IND                     PIC S9(9)  VALUE +0    COMP SYNC.        
004500 77      XIDAO                   PIC S9(9)  VALUE +0    COMP SYNC.        
004600 77      W-KDERS                 PIC S9(3)  VALUE +0    COMP-3.           
004700 77      W-FLERS                 PIC X(1)   VALUE 'N'.                    
004800 77      VCBV                    PIC X      VALUE '4'.                    
004900 77      INPUT-RETT              PIC X(1)   VALUE SPACE.                  
005000 77      WS-KDERS-UTG            PIC 9(3)   VALUE ZERO.                   
005100     EJECT                                                                
005200                                                                          
005300 01  ARBETSAREOR.                                                         
005400     03 FILLER                   PIC X(16)   VALUE                        
005500                                             'WS-DB2-SEKTION'.            
005600     03 WS-DB2-SEKTION           PIC X(24)   VALUE SPACE.                 
005700                                                                          
005800     03  WS-DAGENS-AAAAMMDD      PIC 9(8).                                
005900     03  WS-JMFR-AAAAMMDD        PIC 9(8).                                
006000     03  FILLER REDEFINES WS-JMFR-AAAAMMDD.                               
006100        05 FILLER                PIC 9(2).                                
006200        05 WS-JMFR-AA            PIC 9(2).                                
006300        05 FILLER                PIC 9(4).                                
006400     03  WS-FLAGGA-Q-KAMP        PIC X(1)    VALUE SPACE.                 
006500     03  WS-FLAGGA-W-S-KAMP      PIC X(1)    VALUE SPACE.                 
006600     03  WS-KVLS-REM             PIC S9(7)   VALUE ZERO COMP-3.           
006700     03  WS-ANTAL-KAMP           PIC 9(7)    VALUE ZERO.                  
006800                                                                          
006900     03  WS-TEMFSINF             PIC X(42)   VALUE SPACE.                 
007000     03  WS-TEMFSINF-KAMP        PIC X(10)   VALUE SPACE.                 
007100     03  WS-TEMFSINF-SPLIT       PIC X(3)    VALUE '-- '.                 
007200     EJECT                                                                
007300*01    -COPY WWPRODSL                                                     
007400*                                                                         
007500*      --- VALID IDDC CODES                                               
007600*                                                                         
007700*01    -COPY WWDC99                                                       
007800       EJECT                                                              
007900                                                                          
008000 01  DYNAMISKA-SUBPROGRAM.                                                
008100     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
008200     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
008300     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
008400                                                                          
008500 01  DYNAMISK-SUBMODUL.                                                   
008600   03  WSECURIT                  PIC X(8)   VALUE 'WSECURIT'.             
008700     EJECT                                                                
008800 01      BLADDRINGS-FALT.                                                 
008900   03      SEG-SKIP              PIC 9(3)   VALUE ZERO.                   
009000   03      TOT-KAT               PIC 9(3)   VALUE ZERO.                   
009100   03      LASTA-SEG             PIC 9(3)   VALUE ZERO.                   
009200   03      LASTA-KAT             PIC 9(3)   VALUE ZERO.                   
009300     SKIP3                                                                
009400 01    NYCKLAR-TILL-DLI.                                                  
009500                                                                          
009600   03    W-IDARTNR-X.                                                     
009700     05    W-IDARTNR             PIC S9(9)  VALUE ZERO  COMP-3.           
009800   03    W-KDNOTTYP-X.                                                    
009900     05    W-KDNOTTYP            PIC S9(1)  VALUE ZERO  COMP-3.           
010000   03    W-IDSKYLT-X.                                                     
010100     05    W-IDSKYLT             PIC X(3)   VALUE SPACE.                  
010200   03    W-WDF701KY-X.                                                    
010300     05    W-IDARTNR-F7          PIC S9(9)  VALUE ZERO  COMP-3.           
010400     05    W-IDPRTNER            PIC S9(5)  VALUE +1    COMP-3.           
010500     EJECT                                                                
010600                                                                          
010700 01    MEDDELANDEN.                                                       
010800                                                                          
010900   03    W-FEL-1.                                                         
011000     05    FILLER        PIC X(22)   VALUE                                
011100                             'PARTNUMBER NOT NUMERIC'.                    
011200     05    FILLER        PIC X(22)   VALUE                                
011300                             'PARTNUMBER NOT NUMERIC'.                    
011400   03    FILLER REDEFINES W-FEL-1.                                        
011500     05    FEL-1         PIC X(22) OCCURS 2.                              
011600                                                                          
011700   03    W-FEL-2.                                                         
011800     05    FILLER        PIC X(32)   VALUE                                
011900                             'THIS PART IS NOT IN THE DATABASE'.          
012000     05    FILLER        PIC X(32)   VALUE                                
012100                             'THIS PART IS NOT IN THE DATABASE'.          
012200   03    FILLER REDEFINES W-FEL-2.                                        
012300     05    FEL-2         PIC X(32) OCCURS 2.                              
012400                                                                          
012500   03    W-MED-1.                                                         
012600     05    FILLER        PIC X(34)  VALUE                                 
012700                             'FOR MORE VEHICLE INFO, PRESS ENTER'.        
012800     05    FILLER        PIC X(34)  VALUE                                 
012900                             'FOR MORE VEHICLE INFO, PRESS ENTER'.        
013000   03    FILLER REDEFINES W-MED-1.                                        
013100     05    MED-1         PIC X(34) OCCURS 2.                              
013200                                                                          
013300   03    W-MED-2.                                                         
013400     05    FILLER        PIC X(7)   VALUE                                 
013500                             'UPDATED'.                                   
013600     05    FILLER        PIC X(7)   VALUE                                 
013700                             'UPDATED'.                                   
013800   03    FILLER REDEFINES W-MED-2.                                        
013900     05    MED-2         PIC X(7) OCCURS 2.                               
014000                                                                          
014100   03    W-MED-3.                                                         
014200     05    FILLER        PIC X(26)   VALUE                                
014300                             'THIS IS A SUPERSEDING PART'.                
014400     05    FILLER        PIC X(26)   VALUE                                
014500                             'THIS IS A SUPERSEDING PART'.                
014600   03    FILLER REDEFINES W-MED-3.                                        
014700     05    MED-3         PIC X(26) OCCURS 2.                              
014800                                                                          
014900   03    W-MED-4.                                                         
015000     05    FILLER        PIC X(7)   VALUE                                 
015100                             'DELETED'.                                   
015200     05    FILLER        PIC X(7)   VALUE                                 
015300                             'DELETED'.                                   
015400   03    FILLER REDEFINES W-MED-4.                                        
015500     05    MED-4         PIC X(7) OCCURS 2.                               
015600                                                                          
015700   03    W-MED-5.                                                         
015800     05    FILLER        PIC X(15)   VALUE                                
015900                             'WILL BE DELETED'.                           
016000     05    FILLER        PIC X(15)   VALUE                                
016100                             'WILL BE DELETED'.                           
016200   03    FILLER REDEFINES W-MED-5.                                        
016300     05    MED-5         PIC X(15) OCCURS 2.                              
016400                                                                          
016500   03    W-MED-6.                                                         
016600     05    FILLER        PIC X(23)   VALUE                                
016700                             'THIS PART IS SUPERSEDED'.                   
016800     05    FILLER        PIC X(23)   VALUE                                
016900                             'THIS PART IS SUPERSEDED'.                   
017000   03    FILLER REDEFINES W-MED-6.                                        
017100     05    MED-6         PIC X(23) OCCURS 2.                              
017200                                                                          
017300   03    W-MED-7.                                                         
017400     05    FILLER        PIC X(11)   VALUE                                
017500                             'NOT UPDATED'.                               
017600     05    FILLER        PIC X(11)   VALUE                                
017700                             'NOT UPDATED'.                               
017800   03    FILLER REDEFINES W-MED-7.                                        
017900     05    MED-7         PIC X(11) OCCURS 2.                              
018000                                                                          
018100   03    W-MED-8.                                                         
018200     05    FILLER        PIC X(39)   VALUE                                
018300                      'THIS PART IS SUPERSEDED AND SUPERSEDING'.          
018400     05    FILLER        PIC X(39)   VALUE                                
018500                      'THIS PART IS SUPERSEDED AND SUPERSEDING'.          
018600   03    FILLER REDEFINES W-MED-8.                                        
018700     05    MED-8         PIC X(39) OCCURS 2.                              
018800                                                                          
018900   03    W-MED-9.                                                         
019000     05    FILLER        PIC X(36)   VALUE                                
019100                      'THIS PART IS SUPERSEDING AND DELETED'.             
019200     05    FILLER        PIC X(36)   VALUE                                
019300                      'THIS PART IS SUPERSEDING AND DELETED'.             
019400   03    FILLER REDEFINES W-MED-9.                                        
019500     05    MED-9         PIC X(36) OCCURS 2.                              
019600                                                                          
019700   03    W-MED-10.                                                        
019800     05    FILLER        PIC X(28)   VALUE                                
019900                      'SUPERSEDING, WILL BE DELETED'.                     
020000     05    FILLER        PIC X(28)   VALUE                                
020100                      'SUPERSEDING, WILL BE DELETED'.                     
020200   03    FILLER REDEFINES W-MED-10.                                       
020300     05    MED-10        PIC X(28) OCCURS 2.                              
020400                                                                          
020500   03    W-MED-11.                                                        
020600     05    FILLER        PIC X(28)   VALUE                                
020700                      'UPDATE NOT ALLOWED          '.                     
020800     05    FILLER        PIC X(28)   VALUE                                
020900                      'UPDATE NOT ALLOWED          '.                     
021000   03    FILLER REDEFINES W-MED-11.                                       
021100     05    MED-11        PIC X(28) OCCURS 2.                              
021200                                                                          
021300                                                                          
021400                                                                          
021500                                                                          
021600                                                                          
021700*                  MED-91 KAN KOMBINERAS MED MED-1 PÅ RAD 23              
021800   03    W-MED-91.                                                        
021900     05    FILLER        PIC X(10)   VALUE 'CAMPAIGN  '.                  
022000     05    FILLER        PIC X(10)   VALUE 'CAMPAIGN  '.                  
022100   03    FILLER REDEFINES W-MED-91.                                       
022200     05    MED-91        PIC X(10) OCCURS 2.                              
022300                                                                          
022400*                  MED-92 KAN KOMBINERAS MED MED-1 PÅ RAD 23              
022500   03    W-MED-92.                                                        
022600     05    FILLER        PIC X(10)   VALUE 'CAMPAIGN  '.                  
022700     05    FILLER        PIC X(10)   VALUE 'CAMPAIGN  '.                  
022800   03    FILLER REDEFINES W-MED-92.                                       
022900     05    MED-92        PIC X(10) OCCURS 2.                              
023000                                                                          
023100     EJECT                                                                
023200*01  -COPY WSECAREA                                                       
023300     EJECT                                                                
023400*                   ****    PARAMETRAR TILL W005INIT                      
023500*01  -COPY WMSGINIT                                                       
023600     EJECT                                                                
023700******************************************************************        
023800*                                                                         
023900*                AREOR FÖR MFS OCH SKÄRMHANTERING                         
024000*                                                                         
024100 01      FILLER          PIC X(16)   VALUE 'MFS-WS'.                      
024200     SKIP3                                                                
024300*01      MID -COPY W90402I1 -PRE MID-.                                    
024400     EJECT                                                                
024500*01      -COPY WMSGAREA                                                   
024600     EJECT                                                                
024700*  03    MOD -COPY W90402O1 -PRE MOD- -RED MSG-AREA.                      
024800     EJECT                                                                
024900*01  -COPY WMFSAREA                                                       
025000     EJECT                                                                
025100******************************************************************        
025200*                                                                         
025300*        ARBETS-AREOR TILL DB2-SEKTIONERNA                                
025400*                                                                         
025500 01  FILLER                      PIC X(16)  VALUE 'SQLCA-AREA'.           
025600       EXEC SQL INCLUDE SQLCA END-EXEC.                                   
025700                                                                          
025800 01  FILLER                      PIC X(16)  VALUE 'SQLCODE-WS'.           
025900 01  DB2-WS.                                                              
026000     03  SQLCODE-WS              PIC 9(3)   VALUE ZERO.                   
026100         88  CURSOR-OK                      VALUE 000.                    
026200         88  LINES-FOUND                    VALUE 000.                    
026300         88  LINES-MISSING                  VALUE 100.                    
026400         88  RESOURCE-WRONG                 VALUE 904.                    
026500     03  GOOD-SQLCODECODES.                                               
026600         05  GOOD-SQLCODE OCCURS 5                                        
026700             INDEXED BY SQLCODE-IX PIC 9(3).                              
026800     EJECT                                                                
026900******************************************************************        
027000*                                                                         
027100*        ARBETS-AREOR TILL IMS-SEKTIONERNA                                
027200*                                                                         
027300 01  IMS-WS.                                                              
027400   03    FILLER          PIC X(16)   VALUE 'IMS-WS'.                      
027500     SKIP3                                                                
027600*****                    **** STATUS-KOD FRÅN IMS                         
027700   03    STATUS-WS       PIC XX.                                          
027800         88  SEGMENT-FINNS       VALUE '  '.                              
027900         88  SEGMENT-SAKNAS      VALUE 'GE'.                              
028000         88  SEGMENT-FINNS-REDAN VALUE 'II'.                              
028100     SKIP3                                                                
028200   03    GODK-STATUSKODER.                                                
028300     05  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
028400     SKIP3                                                                
028500 01      SSA1            PIC X(64).                                       
028600 01      SSA2            PIC X(64).                                       
028700 01      SSA3            PIC X(64).                                       
028800     EJECT                                                                
028900*                            IMS FUNKTIONSKODER                           
029000*01      -COPY W0003                                                      
029100     EJECT                                                                
029200*                            DLI INPUT-OUTPUT AREA                        
029300 01  DLI-IO-AREA-1.                                                       
029400*    03  -COPY WDK601                                                     
029500     EJECT                                                                
029600 01  DLI-IO-AREA-2.                                                       
029700*    03  -COPY WDK611                                                     
029800     EJECT                                                                
029900 01  DLI-IO-AREA-3.                                                       
030000     03  IO-AREA-3       PIC X(120) VALUE SPACE.                          
030100*    03  WLARTC25 -COPY WDK625               -RED IO-AREA-3.              
030200     EJECT                                                                
030300*    03  WLBENA01 -COPY WDD301 -PRE BENA-    -RED IO-AREA-3.              
030400     EJECT                                                                
030500*    03  WLBENA11 -COPY WDD311 -PRE BENA-    -RED IO-AREA-3.              
030600     EJECT                                                                
030700*    03  WLKATN01 -COPY WDN601 -PRE KATN-    -RED IO-AREA-3.              
030800     EJECT                                                                
030900*    03  WLKATN11 -COPY WDN611 -PRE KATN-    -RED IO-AREA-3.              
031000     EJECT                                                                
031100 01  DLI-IO-AREA-7.                                                       
031200     03  IO-AREA-7       PIC X(300)  VALUE SPACE.                         
031300*    03  WDF701   -COPY WDF701               -RED IO-AREA-7.              
031400     EJECT                                                                
031500 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK401'.                      
031600 01  DLI-IO-WDK401.                                                       
031700*    03  -COPY WDK401                                                     
031800     EJECT                                                                
031900*    --------------- DB2 INPUT-OUTPUT AREA ---------------                
032000                                                                          
032100 01  FILLER                      PIC X(16)  VALUE 'TP1KAMP-AREA'.         
032200                                                                          
032300*01  -COPY TP1KAMP -PRE TP1KAMP-                                          
032400     EJECT                                                                
032500 01  FILLER                      PIC X(16)  VALUE 'TP1ARTK-AREA'.         
032600                                                                          
032700*01  -COPY TP1ARTK -PRE TP1ARTK-                                          
032800     EJECT                                                                
032900     EXEC SQL INCLUDE TP1KAMP END-EXEC.                                   
033000     EJECT                                                                
033100     EXEC SQL INCLUDE TP1ARTK END-EXEC.                                   
033200     EJECT                                                                
033300                                                                          
033400 LINKAGE SECTION.                                                         
033500*01  -COPY W0009     -PRE MSG-                                            
033600     EJECT                                                                
033700*01  -COPY W0008     -PRE USEA-                                           
033800         05  FILLER           PIC X.                                      
033900     EJECT                                                                
034000*01  -COPY W0008     -PRE ARTC-                                           
034100         05  FILLER           PIC X.                                      
034200     EJECT                                                                
034300*01  -COPY W0008     -PRE BENA-                                           
034400         05  FILLER           PIC X.                                      
034500     EJECT                                                                
034600*01  -COPY W0008     -PRE KATN-                                           
034700         05  FILLER           PIC X.                                      
034800     EJECT                                                                
034900*01  -COPY W0008     -PRE WDF7-                                           
035000         05  FILLER           PIC X.                                      
035100     EJECT                                                                
035200*01  -COPY W0008     -PRE WDK4-                                           
035300         05  FILLER           PIC X.                                      
035400     EJECT                                                                
035500 PROCEDURE DIVISION USING MSG-PCB USEA-PCB                                
035600                                  ARTC-PCB                                
035700                                  BENA-PCB KATN-PCB WDF7-PCB              
035800                                  WDK4-PCB.                               
035900     ENTRY 'DLITCBL' USING MSG-PCB USEA-PCB                               
036000                                   ARTC-PCB BENA-PCB                      
036100                                   KATN-PCB WDF7-PCB WDK4-PCB.            
036200                                                                          
036300     PERFORM IMS-GET-MSG                                                  
036400                                                                          
036500     IF SEGMENT-FINNS                                                     
036600       PERFORM A-SPARA-NYCKLAR-OCH-INIT                                   
036700                                                                          
036800       PERFORM E-KONTROLL-VCBV                                            
036900                                                                          
037000       IF IDARTNR-WS NOT NUMERIC                                          
037100         MOVE FEL-1 (INDX) TO MOD-TEMFSFEL                                
037200       ELSE                                                               
037300         MOVE IDARTNR-WS TO W-IDARTNR                                     
037400                            W-IDARTNR-F7                                  
037500                                                                          
037600         IF MFS-UPDATE                                                    
037700           PERFORM B-UPPDATERA                                            
037800         ELSE                                                             
037900           PERFORM C-REDIGERA-BILD                                        
038000           IF W-FLERS = JA                                                
038100             IF W-KDERS = +29 OR +52                                      
038200               MOVE MED-9 (INDX) TO MOD-TEMFSFEL                          
038300             ELSE                                                         
038400               EVALUATE TRUE                                              
038500               WHEN W-KDERS = +9 OR +19                                   
038600                 MOVE MED-10 (INDX) TO MOD-TEMFSFEL                       
038700               WHEN W-KDERS > +0                                          
038800                 MOVE MED-8 (INDX) TO MOD-TEMFSFEL                        
038900                WHEN OTHER                                                
039000                 MOVE MED-3 (INDX) TO MOD-TEMFSFEL                        
039100               END-EVALUATE                                               
039200             END-IF                                                       
039300           ELSE                                                           
039400             IF W-KDERS = +29 OR +52                                      
039500               MOVE MED-4 (INDX) TO MOD-TEMFSFEL                          
039600             ELSE                                                         
039700               EVALUATE TRUE                                              
039800               WHEN W-KDERS = +9 OR +19                                   
039900                 MOVE MED-5 (INDX) TO MOD-TEMFSFEL                        
040000               WHEN W-KDERS > +0                                          
040100                 MOVE MED-6 (INDX) TO MOD-TEMFSFEL                        
040200               END-EVALUATE                                               
040300             END-IF                                                       
040400           END-IF                                                         
040500         END-IF                                                           
040600       END-IF                                                             
040700     END-IF                                                               
040800*     ---- SKRIV EV. UT MEDDELANDEN PÅ RAD 23 (TEMFSINF) -----            
040900     IF WS-TEMFSINF = SPACE  AND WS-TEMFSINF-KAMP = SPACE                 
041000         CONTINUE                                                         
041100     ELSE                                                                 
041200       IF WS-TEMFSINF = SPACE                                             
041300           MOVE WS-TEMFSINF-KAMP TO MOD-TEMFSINF                          
041400       ELSE                                                               
041500         IF WS-TEMFSINF-KAMP = SPACE                                      
041600             MOVE WS-TEMFSINF TO MOD-TEMFSINF                             
041700         ELSE                                                             
041800*            --- OBS MAX. 55 TECKEN                                       
041900             STRING WS-TEMFSINF-KAMP  DELIMITED BY SIZE                   
042000                    WS-TEMFSINF-SPLIT DELIMITED BY SIZE                   
042100                    WS-TEMFSINF       DELIMITED BY SIZE                   
042200             INTO MOD-TEMFSINF                                            
042300         END-IF                                                           
042400       END-IF                                                             
042500     END-IF                                                               
042600                                                                          
042700                                                                          
042800     PERFORM IMS-INSERT-MSG                                               
042900                                                                          
043000     MOVE ZERO TO RETURN-CODE                                             
043100                                                                          
043200     GOBACK                                                               
043300     CONTINUE                                                             
043400     .                                                                    
043500     EJECT                                                                
043600 A-SPARA-NYCKLAR-OCH-INIT SECTION.                                        
043700                                                                          
043800     IF MSG-DUBBLA-TRANSKODER                                             
043900       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W90402I1-CTX             
044000       MOVE MSG-IDTRANS-2 TO MFS-IDTRANS                                  
044100       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
044200       MOVE MSG-KDTRTYP          TO MFS-KDTRTYP                           
044300*SPECIAL-TEST                                                             
044400       MOVE ZERO TO LASTA-SEG                                             
044500     ELSE                                                                 
044600       MOVE MSG-INDATA-MINUS-1-TRANSKOD TO MID-W90402I1-CTX               
044700       MOVE MSG-IDTRANS-1 TO MFS-IDTRANS                                  
044800       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
044900       MOVE ' ' TO MFS-KDTRTYP                                            
045000     END-IF                                                               
045100     MOVE ALL '+' TO MSGI-WMSGINIT                                        
045200     MOVE '001'             TO MSGI-KDCALL                                
045300     MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                                
045400                               MSGI-IDLTERM-USER                          
045500     MOVE '9402'            TO MSGI-IDTRANS                               
045600     IF MFS-IDTRANS = '9402'                                              
045700     OR (MID-IDARTNR-IN NUMERIC                                           
045800     AND MID-IDARTNR-IN > ZERO)                                           
045900         MOVE MID-IDARTNR-IN TO MSGI-IDARTNR                              
046000     END-IF                                                               
046100     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
046200     MOVE MSGI-IDARTNR TO IDARTNR-WS                                      
046300     MOVE MSGI-IDDC    TO WS-IDDC                                         
046400                                                                          
046500     IF MSGI-IDLAND-SPR NOT = 'GB'                                        
046600       MOVE +1 TO INDX                                                    
046700     ELSE                                                                 
046800       MOVE +2 TO INDX                                                    
046900     END-IF                                                               
047000                                                                          
047100     INSPECT IDARTNR-WS REPLACING ALL SPACE BY ZERO                       
047200     IF MID-IDARTNR-IN NOT = ALL '+'                                      
047300       MOVE ' ' TO MFS-KDTRTYP                                            
047400*SPECIAL-TEST                                                             
047500       MOVE ZERO TO MID-BLADDRING-ANT                                     
047600       MOVE ZERO TO LASTA-SEG                                             
047700     END-IF                                                               
047800     MOVE LOW-VALUE  TO MSG-AREA                                          
047900     MOVE 'W90402O1' TO MFS-IDMOD                                         
048000     MOVE '9402' TO MOD-IDTRANS                                           
048100     MOVE IDARTNR-WS TO MOD-IDARTNR-UT                                    
048200     INSPECT MOD-IDARTNR-UT REPLACING LEADING ZERO BY SPACE               
048300     MOVE MAX-MOD-LENGD TO MSG-KVLL                                       
048400                                                                          
048500     IF MFS-IDTRANS NOT = '9402'                                          
048600       MOVE ' ' TO MFS-KDTRTYP                                            
048700       MOVE ZERO TO MID-BLADDRING-ANT                                     
048800     END-IF                                                               
048900                                                                          
049000     MOVE FUNCTION CURRENT-DATE (1:8) TO WS-DAGENS-AAAAMMDD               
049100                                                                          
049200     MOVE MFS-RENSA-FAELT TO                                              
049300*                            MOD-IDARTNR-IN                               
049400                             MOD-TEMFSFEL                                 
049500                             MOD-TEMFSINF                                 
049600     MOVE SPACE           TO WS-TEMFSINF                                  
049700                             WS-TEMFSINF-KAMP                             
049800                                                                          
049900     MOVE ZERO  TO W-KDERS                                                
050000                   WS-KDERS-UTG                                           
050100     MOVE 'N'   TO W-FLERS                                                
050200     CONTINUE.                                                            
050300     EJECT                                                                
050400 B-UPPDATERA SECTION.                                                     
050500                                                                          
050600     MOVE JA TO INPUT-RETT                                                
050700     PERFORM IMS-GET-ARTC01                                               
050800     IF SEGMENT-FINNS                                                     
050900        MOVE ART-KDPRODSL       TO TEST-KDPRODSL                          
051000        IF KDPRODSL-VOLVO-BIMA                                            
051100           IF CDC OR SDC                                                  
051200              CONTINUE                                                    
051300           ELSE                                                           
051400              MOVE MED-11(INDX) TO MOD-TEMFSFEL                           
051500              MOVE MED-7 (INDX) TO WS-TEMFSINF                            
051600              MOVE NEJ TO INPUT-RETT                                      
051700           END-IF                                                         
051800        END-IF                                                            
051900     END-IF                                                               
052000                                                                          
052100     IF INPUT-RETT = JA                                                   
052200        IF MID-TEARTNOT-3 NOT = ALL '+'                                   
052300          PERFORM IMS-GU-ARTC11                                           
052400          IF SEGMENT-FINNS                                                
052500            MOVE +3 TO W-KDNOTTYP                                         
052600                                                                          
052700            IF MID-TEARTNOT-3 = SPACE                                     
052800              PERFORM IMS-GET-HOLD-NOTERING                               
052900              IF SEGMENT-FINNS                                            
053000                PERFORM IMS-DELETE-NOTERING                               
053100                MOVE SPACE TO MOD-TEARTNOT-3                              
053200                MOVE MED-2 (INDX) TO WS-TEMFSINF                          
053300              END-IF                                                      
053400            ELSE                                                          
053500              PERFORM IMS-GET-HOLD-NOTERING                               
053600              MOVE W-KDNOTTYP TO NOT-KDNOTTYP                             
053700              MOVE MID-TEARTNOT-3 TO NOT-TEARTNOT                         
053800              IF SEGMENT-FINNS                                            
053900                PERFORM IMS-REPLACE-NOTERING                              
054000              ELSE                                                        
054100                PERFORM IMS-INSERT-NOTERING                               
054200              END-IF                                                      
054300              MOVE MED-2 (INDX)    TO WS-TEMFSINF                         
054400              MOVE NOT-TEARTNOT TO MOD-TEARTNOT-3                         
054500              MOVE MFS-ADD-LYS-UPP-FAELT                                  
054600                           TO MOD-TEARTNOT-3-ATTR                         
054700            END-IF                                                        
054800          ELSE                                                            
054900            MOVE MED-4 (INDX) TO MOD-TEMFSFEL                             
055000          END-IF                                                          
055100                                                                          
055200        ELSE                                                              
055300          MOVE MFS-ROER-EJ-FAELT TO MOD-TEARTNOT-3                        
055400        END-IF                                                            
055500                                                                          
055600        IF WS-TEMFSINF NOT = MED-2 (INDX)                                 
055700          MOVE MED-7 (INDX) TO WS-TEMFSINF                                
055800        END-IF                                                            
055900                                                                          
056000        IF MID-TEARTNOT-7 NOT = ALL '+'                                   
056100          PERFORM IMS-GU-ARTC11                                           
056200          IF SEGMENT-FINNS                                                
056300            MOVE +7 TO W-KDNOTTYP                                         
056400                                                                          
056500            IF MID-TEARTNOT-7 = SPACE                                     
056600              PERFORM IMS-GET-HOLD-NOTERING                               
056700              IF SEGMENT-FINNS                                            
056800                PERFORM IMS-DELETE-NOTERING                               
056900                MOVE SPACE TO MOD-TEARTNOT-7                              
057000                MOVE MED-2 (INDX) TO WS-TEMFSINF                          
057100              END-IF                                                      
057200            ELSE                                                          
057300              PERFORM IMS-GET-HOLD-NOTERING                               
057400              MOVE W-KDNOTTYP TO NOT-KDNOTTYP                             
057500              MOVE MID-TEARTNOT-7 TO NOT-TEARTNOT                         
057600              IF SEGMENT-FINNS                                            
057700                PERFORM IMS-REPLACE-NOTERING                              
057800              ELSE                                                        
057900                PERFORM IMS-INSERT-NOTERING                               
058000              END-IF                                                      
058100              MOVE MED-2 (INDX)    TO WS-TEMFSINF                         
058200              MOVE NOT-TEARTNOT TO MOD-TEARTNOT-7                         
058300              MOVE MFS-ADD-LYS-UPP-FAELT                                  
058400                           TO MOD-TEARTNOT-7-ATTR                         
058500            END-IF                                                        
058600          ELSE                                                            
058700            MOVE MED-4 (INDX) TO MOD-TEMFSFEL                             
058800          END-IF                                                          
058900                                                                          
059000        ELSE                                                              
059100          MOVE MFS-ROER-EJ-FAELT TO MOD-TEARTNOT-7                        
059200        END-IF                                                            
059300                                                                          
059400        IF WS-TEMFSINF NOT = MED-2 (INDX)                                 
059500          MOVE MED-7 (INDX) TO WS-TEMFSINF                                
059600        END-IF                                                            
059700     END-IF                                                               
059800                                                                          
059900     PERFORM D-SPARA-SKARMBILD                                            
060000     CONTINUE.                                                            
060100     EJECT                                                                
060200 C-REDIGERA-BILD SECTION.                                                 
060300                                                                          
060400     MOVE MFS-RENSA-FAELT TO MOD-KDERS(2)                                 
060500     PERFORM IMS-GET-ARTC01                                               
060600     IF SEGMENT-SAKNAS                                                    
060700       MOVE FEL-2 (INDX)       TO MOD-TEMFSFEL                            
060800       PERFORM CA-RENSA-FAELT                                             
060900     ELSE                                                                 
061000*      --- KOLLAR OM REG. ARTIKEL ÄR KAMPANJARTIKEL                       
061100       PERFORM CC-KOLLA-KAMPANJ                                           
061200                                                                          
061300       IF ART-FLIART = JA                                                 
061400         MOVE JA TO MOD-KDIART                                            
061500       ELSE                                                               
061600         MOVE NEJ TO MOD-KDIART                                           
061700       END-IF                                                             
061800*      MOVE '-'                TO MOD-STRECK                              
061900*      MOVE ART-REKSIFFR       TO MOD-REKSIFFR                            
062000       MOVE ART-TIERSDAT       TO MOD-TIERSDAT                            
062100       IF ART-TIERSDAT = ZERO                                             
062200          INSPECT MOD-TIERSDAT REPLACING LEADING ZERO BY SPACE            
062300       END-IF                                                             
062400       MOVE ART-TISOP          TO MOD-TISOP                               
062500       MOVE ART-KDPRODSL       TO MOD-KDPRODSL                            
062600       MOVE ART-IDFKNGRP       TO MOD-IDFKNGRP                            
062700       MOVE ART-KDSORT         TO MOD-KDSORT                              
062800       MOVE ART-KDERS-UTG      TO W-KDERS                                 
062900       MOD-KDERS (1)                                                      
063000       MOVE ART-KDERS-UTG      TO WS-KDERS-UTG                            
063100*      MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-KDERS-ATTR (1)                   
063200       MOVE ART-FLERS          TO W-FLERS                                 
063300       MOVE ART-IDLEVNR        TO MOD-IDLEVNR                             
063400******************************** DATUM KONVERTERING!!!!                   
063500       MOVE ART-TIREGDAT       TO MOD-TIREGDAT                            
063600******************************** DATUM KONVERTERING!!!!                   
063700                                                                          
063800       SET MOD-IX TO +1                                                   
063900       MOVE +1    TO XIDAO                                                
064000       PERFORM UNTIL                                                      
064100        NOT ( XIDAO < MAX-ANT-IDAO )                                      
064200         IF ART-IDAO (XIDAO) = SPACE                                      
064300           MOVE MFS-RENSA-FAELT  TO MOD-IDAO (MOD-IX)                     
064400         ELSE                                                             
064500           MOVE ART-IDAO (XIDAO) TO MOD-IDAO (MOD-IX)                     
064600         END-IF                                                           
064700         SET MOD-IX UP BY +1                                              
064800         ADD +1                  TO XIDAO                                 
064900       END-PERFORM                                                        
065000                                                                          
065100*      MOVE MFS-RENSA-FAELT TO MOD-IDSKYLT-TILLV                          
065200       MOVE MFS-RENSA-FAELT TO MOD-KVPB-TOT(2)                            
065300       MOVE MFS-RENSA-FAELT TO MOD-KVDISP (2)                             
065400       PERFORM IMS-GET-ARTC11                                             
065500       IF SEGMENT-SAKNAS                                                  
065600          PERFORM CB-RENSA-FAELT                                          
065700       ELSE                                                               
065800         MOVE CLAG-IDBERED        TO MOD-IDBERED                          
065900*        MOVE CLAG-IDPROJ         TO MOD-IDPROJ                           
066000         MOVE CLAG-IDPROJUP       TO MOD-IDPROJUP                         
066100         MOVE CLAG-IDRITN         TO MOD-IDRITN                           
066200         MOVE CLAG-KDPSLLOC       TO MOD-KDPSLLOC                         
066300         MOVE CLAG-IDKAT(1)       TO MOD-IDKAT-1                          
066400         MOVE CLAG-IDKAT(2)       TO MOD-IDKAT-2                          
066500         MOVE CLAG-IDKAT(3)       TO MOD-IDKAT-3                          
066600                                                                          
066700*        IF CLAG-IDPROENH(1) = SPACE                                      
066800*           MOVE MFS-RENSA-FAELT TO MOD-IDPROENH(1)                       
066900*        ELSE                                                             
067000*           MOVE CLAG-IDPROENH(1) TO MOD-IDPROENH (1)                     
067100*           INSPECT MOD-IDPROENH(1) REPLACING LEADING ZERO BY             
067200*                  SPACE                                                  
067300*        END-IF                                                           
067400*        IF CLAG-IDPROENH(2) = SPACE                                      
067500*           MOVE MFS-RENSA-FAELT TO MOD-IDPROENH(2)                       
067600*        ELSE                                                             
067700*           MOVE CLAG-IDPROENH(2) TO MOD-IDPROENH (2)                     
067800*           INSPECT MOD-IDPROENH(2) REPLACING LEADING ZERO BY             
067900*                  SPACE                                                  
068000*        END-IF                                                           
068100*        IF CLAG-IDPROENH(3) = SPACE                                      
068200*           MOVE MFS-RENSA-FAELT TO MOD-IDPROENH(3)                       
068300*        ELSE                                                             
068400*           MOVE CLAG-IDPROENH(3) TO MOD-IDPROENH (3)                     
068500*           INSPECT MOD-IDPROENH(3) REPLACING LEADING ZERO BY             
068600*                  SPACE                                                  
068700*        END-IF                                                           
068800                                                                          
068900         MOVE CLAG-IDANSK     TO MOD-IDANSK                               
069000         MOVE CLAG-FLGEMART   TO MOD-FLGEMART                             
069100                                                                          
069200         ADD CLAG-KVPB-SATS CLAG-KVPB-SEP                                 
069300                              GIVING MOD-KVPB-TOT (1)                     
069400                                                                          
069500         IF SEC-KDSVAR = 4                                                
069600           MOVE MFS-RENSA-FAELT TO MOD-PRARTSTD                           
069700         ELSE                                                             
069800           MOVE CLAG-PRARTSTD  TO MOD-PRARTSTD                            
069900         END-IF                                                           
070000                                                                          
070100         MOVE CLAG-FLLSRDEL  TO MOD-FLLSRDEL                              
070200         MOVE CLAG-KDUART    TO MOD-KDUART                                
070300         MOVE CLAG-KDBPSR    TO MOD-KDBPSR                                
070400                                                                          
070500         MOVE CLAG-KDERS     TO MOD-KDERS (1)                             
070600                                W-KDERS                                   
070700*        MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-KDERS-ATTR(1)                  
070800                                                                          
070900         IF SEC-KDSVAR = 4                                                
071000            MOVE MFS-RENSA-FAELT TO MOD-KVDISP (1)                        
071100         ELSE                                                             
071200            COMPUTE MOD-KVDISP (1) = (CLAG-KVLS - CLAG-KVRESS)            
071300         END-IF                                                           
071400       END-IF                                                             
071500                                                                          
071600       MOVE MFS-RENSA-FAELT TO MOD-TEARTNOT-3                             
071700       MOVE +3 TO W-KDNOTTYP                                              
071800       PERFORM IMS-GET-ARTC25                                             
071900       IF SEGMENT-FINNS                                                   
072000          MOVE NOT-TEARTNOT TO MOD-TEARTNOT-3                             
072100       END-IF                                                             
072200                                                                          
072300       MOVE MFS-RENSA-FAELT TO MOD-TEARTNOT-7                             
072400       MOVE +7 TO W-KDNOTTYP                                              
072500       PERFORM IMS-GET-ARTC25                                             
072600       IF SEGMENT-FINNS                                                   
072700          MOVE NOT-TEARTNOT TO MOD-TEARTNOT-7                             
072800       END-IF                                                             
072900                                                                          
073000       PERFORM IMS-GET-BENA01-BSEQ                                        
073100       IF SEGMENT-FINNS                                                   
073200         MOVE 'GB ' TO W-IDSKYLT                                          
073300         PERFORM IMS-GET-BENA11-BSEQ                                      
073400         IF SEGMENT-FINNS                                                 
073500           MOVE BENA-TEXT-BEART TO MOD-BEART-ENG                          
073600         END-IF                                                           
073700         MOVE 'S  ' TO W-IDSKYLT                                          
073800         PERFORM IMS-GET-BENA11-BSEQ                                      
073900         IF SEGMENT-FINNS                                                 
074000           MOVE BENA-TEXT-BEART TO MOD-BEART-SVE                          
074100         END-IF                                                           
074200       END-IF                                                             
074300       PERFORM IMS-GET-MASTER-WDN6                                        
074400       IF SEGMENT-FINNS                                                   
074500         PERFORM IMS-GET-KATINFO-MASTER                                   
074600         IF MID-BLADDRING-ANT NOT NUMERIC                                 
074700            MOVE ZERO TO MID-BLADDRING-ANT                                
074800         END-IF                                                           
074900         IF MID-BLADDRING-ANT = ZERO                                      
075000           CONTINUE                                                       
075100         ELSE                                                             
075200           SET MOD-IZ      TO +1                                          
075300           MOVE MID-BLADDRING-ANT TO LASTA-SEG                            
075400           PERFORM UNTIL                                                  
075500            NOT ( MOD-IZ < LASTA-SEG + 1 AND SEGMENT-FINNS )              
075600             PERFORM IMS-GET-KATINFO-MASTER                               
075700             SET MOD-IZ   UP BY +1                                        
075800           END-PERFORM                                                    
075900         END-IF                                                           
076000         SET MOD-IZ         TO +1                                         
076100         PERFORM UNTIL                                                    
076200          NOT ( MOD-IZ < 15 AND SEGMENT-FINNS )                           
076300           MOVE KATN-KAT-BEEMBLEM TO MOD-KAT-BEEMBLEM (MOD-IZ)            
076400           ADD +1          TO LASTA-SEG                                   
076500           PERFORM IMS-GET-KATINFO-MASTER                                 
076600           SET MOD-IZ UP BY +1                                            
076700         END-PERFORM                                                      
076800         IF SEGMENT-FINNS                                                 
076900           MOVE MED-1 (INDX) TO WS-TEMFSINF                               
077000           MOVE LASTA-SEG    TO MOD-BLADDRING-ANT                         
077100         ELSE                                                             
077200           MOVE ZERO         TO MOD-BLADDRING-ANT                         
077300           CONTINUE                                                       
077400         END-IF                                                           
077500       END-IF                                                             
077600     END-IF                                                               
077700     IF MFS-IDTRANS = '9402' AND MID-IDARTNR-IN = SPACE                   
077800     OR MFS-IDTRANS = '9402' AND MID-IDARTNR-IN = ALL '+'                 
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
080600     CONTINUE.                                                            
080700     EJECT                                                                
080800 CA-RENSA-FAELT    SECTION.                                               
080900                                                                          
081000     MOVE MFS-RENSA-FAELT   TO MOD-IDANSK                                 
081100                               MOD-IDLEVNR                                
081200                               MOD-KDPRODSL                               
081300                               MOD-KDUART                                 
081400                               MOD-KDIART                                 
081500*                              MOD-IDSKYLT-TILLV                          
081600                               MOD-PRARTSTD                               
081700*                              MOD-IDPROJ                                 
081800                               MOD-IDPROJUP                               
081900                               MOD-IDRITN                                 
082000                               MOD-KDERS(1)                               
082100                               MOD-KDERS(2)                               
082200                               MOD-KVDISP(1)                              
082300                               MOD-KVDISP(2)                              
082400                               MOD-KVPB-TOT(1)                            
082500                               MOD-KVPB-TOT(2)                            
082600                               MOD-IDFKNGRP                               
082700                               MOD-KDSORT                                 
082800                               MOD-KDBPSR                                 
082900                               MOD-IDBERED                                
083000                               MOD-FLGEMART                               
083100                               MOD-FLLSRDEL                               
083200                               MOD-BEART-SVE                              
083300                               MOD-BEART-ENG                              
083400                               MOD-TIREGDAT                               
083500                               MOD-TISOP                                  
083600                               MOD-TIERSDAT                               
083700                               MOD-IDKAT-1                                
083800                               MOD-IDKAT-2                                
083900                               MOD-IDKAT-3                                
084000                               MOD-TEARTNOT-3                             
084100                               MOD-TEARTNOT-7                             
084200*                              MOD-IDPROENH(1)                            
084300*                              MOD-IDPROENH(2)                            
084400*                              MOD-IDPROENH(3)                            
084500                                                                          
084600     SET MOD-IX TO +1                                                     
084700     PERFORM UNTIL                                                        
084800      ( MOD-IX > +5 )                                                     
084900       MOVE MFS-RENSA-FAELT TO MOD-IDAO (MOD-IX)                          
085000       SET MOD-IX UP BY +1                                                
085100     END-PERFORM                                                          
085200     SET MOD-IZ TO +1                                                     
085300     PERFORM UNTIL                                                        
085400      ( MOD-IZ > +14 )                                                    
085500       MOVE MFS-RENSA-FAELT TO MOD-KAT-BEEMBLEM (MOD-IZ)                  
085600       SET MOD-IZ UP BY +1                                                
085700     END-PERFORM                                                          
085800     CONTINUE.                                                            
085900     EJECT                                                                
086000 CB-RENSA-FAELT SECTION.                                                  
086100     MOVE MFS-RENSA-FAELT TO MOD-IDBERED                                  
086200*                            MOD-IDPROJ                                   
086300                             MOD-IDPROJUP                                 
086400                             MOD-IDRITN                                   
086500                             MOD-IDKAT-1                                  
086600                             MOD-IDKAT-2                                  
086700                             MOD-IDKAT-3                                  
086800*                            MOD-IDPROENH(1)                              
086900*                            MOD-IDPROENH(2)                              
087000*                            MOD-IDPROENH(3)                              
087100                             MOD-IDANSK                                   
087200                             MOD-FLGEMART                                 
087300                             MOD-KVPB-TOT(1)                              
087400                             MOD-KVPB-TOT(2)                              
087500                             MOD-PRARTSTD                                 
087600                             MOD-FLLSRDEL                                 
087700                             MOD-KDUART                                   
087800                             MOD-KDBPSR                                   
087900                             MOD-KDERS(2)                                 
088000                             MOD-KVDISP(1)                                
088100                             MOD-KVDISP(2)                                
088200     .                                                                    
088300     EJECT                                                                
088400 CC-KOLLA-KAMPANJ   SECTION.                                              
088500     SKIP2                                                                
088600     PERFORM DB2-DCL-OPN-TP1ARTK-CRS                                      
088700     IF SQLCODE-WS = ZERO                                                 
088800       PERFORM DB2-FETCH-TP1ARTK-CRS                                      
088900     END-IF                                                               
089000                                                                          
089100     MOVE ZERO               TO WS-ANTAL-KAMP                             
089200     MOVE NEJ                TO WS-FLAGGA-Q-KAMP                          
089300                                WS-FLAGGA-W-S-KAMP                        
089400     PERFORM UNTIL SQLCODE > ZERO                                         
089500       IF TP1KAMP-TISTODAT-KAMP > ZERO                                    
089600         MOVE TP1KAMP-TISTODAT-KAMP                                       
089700                             TO WS-JMFR-AAAAMMDD                          
089800       ELSE                                                               
089900         MOVE TP1KAMP-TISTADAT-KAMP                                       
090000                             TO WS-JMFR-AAAAMMDD                          
090100       END-IF                                                             
090200       IF WS-JMFR-AA > 50                                                 
090300         MOVE 19             TO WS-JMFR-AAAAMMDD (1:2)                    
090400       ELSE                                                               
090500         MOVE 20             TO WS-JMFR-AAAAMMDD (1:2)                    
090600       END-IF                                                             
090700       IF TP1KAMP-TISTODAT-KAMP = ZERO                                    
090800*    LÄGG TILL 5 ÅR                                                       
090900         ADD 50000           TO WS-JMFR-AAAAMMDD                          
091000       END-IF                                                             
091100       IF WS-JMFR-AAAAMMDD >= WS-DAGENS-AAAAMMDD                          
091200         IF TP1KAMP-KDKAMP = 'Q'                                          
091300           MOVE JA           TO WS-FLAGGA-Q-KAMP                          
091400         END-IF                                                           
091500         IF TP1KAMP-KDKAMP = 'W'                                          
091600         OR TP1KAMP-KDKAMP = 'S'                                          
091700           MOVE JA           TO WS-FLAGGA-W-S-KAMP                        
091800         END-IF                                                           
091900       END-IF                                                             
092000       ADD 1                 TO WS-ANTAL-KAMP                             
092100       PERFORM DB2-FETCH-TP1ARTK-CRS                                      
092200     END-PERFORM                                                          
092300                                                                          
092400     IF  WS-FLAGGA-Q-KAMP   = JA                                          
092500     AND WS-FLAGGA-W-S-KAMP = NEJ                                         
092600       MOVE MED-92 (INDX)    TO WS-TEMFSINF-KAMP                          
092700*            SM ETC                                                       
092800     ELSE                                                                 
092900       IF WS-FLAGGA-W-S-KAMP = JA                                         
093000       MOVE MED-91 (INDX)    TO WS-TEMFSINF-KAMP                          
093100*            CAMPAIGN                                                     
093200       END-IF                                                             
093300     END-IF                                                               
093400*    MOVE WS-ANTAL-KAMP      TO WS-TEMFSINF-KAMP                          
093500     PERFORM DB2-CLOSE-TP1ARTK-CRS                                        
093600     .                                                                    
093700     EJECT                                                                
093800 D-SPARA-SKARMBILD SECTION.                                               
093900                                                                          
094000     MOVE MFS-ROER-EJ-FAELT TO                                            
094100                               MOD-IDARTNR-UT                             
094200                               MOD-IDANSK                                 
094300                               MOD-IDLEVNR                                
094400                               MOD-KDPRODSL                               
094500                               MOD-KDUART                                 
094600                               MOD-KDIART                                 
094700*                              MOD-IDSKYLT-TILLV                          
094800                               MOD-IDKAT-1                                
094900                               MOD-IDKAT-2                                
095000                               MOD-IDKAT-3                                
095100     IF SEC-KDSVAR = 4                                                    
095200       MOVE MFS-RENSA-FAELT  TO MOD-PRARTSTD                              
095300     ELSE                                                                 
095400       MOVE MFS-ROER-EJ-FAELT TO MOD-PRARTSTD                             
095500     END-IF                                                               
095600     MOVE MFS-ROER-EJ-FAELT TO                                            
095700*                              MOD-IDPROJ                                 
095800                               MOD-IDPROJUP                               
095900                               MOD-IDRITN                                 
096000                                                                          
096100*    SET MOD-IY TO +1                                                     
096200*    PERFORM UNTIL                                                        
096300*     ( MOD-IY > +3 )                                                     
096400*      MOVE MFS-ROER-EJ-FAELT TO MOD-IDPROENH (MOD-IY)                    
096500*      SET MOD-IY UP BY +1                                                
096600*    END-PERFORM                                                          
096700     MOVE MFS-ROER-EJ-FAELT TO MOD-KDERS (1)                              
096800                               MOD-KDERS (2)                              
096900     IF SEC-KDSVAR = 4                                                    
097000       MOVE MFS-RENSA-FAELT TO MOD-KVDISP (1)                             
097100                               MOD-KVDISP (2)                             
097200     ELSE                                                                 
097300       MOVE MFS-ROER-EJ-FAELT TO MOD-KVDISP (1)                           
097400                                 MOD-KVDISP (2)                           
097500     END-IF                                                               
097600     MOVE MFS-ROER-EJ-FAELT TO MOD-KVPB-TOT (1)                           
097700                               MOD-KVPB-TOT (2)                           
097800                               MOD-IDFKNGRP                               
097900                               MOD-KDSORT                                 
098000                               MOD-KDBPSR                                 
098100                               MOD-IDBERED                                
098200                               MOD-FLGEMART                               
098300                               MOD-FLLSRDEL                               
098400                               MOD-BEART-SVE                              
098500                               MOD-BEART-ENG                              
098600                               MOD-TIREGDAT                               
098700                               MOD-TISOP                                  
098800                               MOD-TIERSDAT                               
098900                                                                          
099000     SET MOD-IX TO +1                                                     
099100     PERFORM UNTIL                                                        
099200      ( MOD-IX > +5 )                                                     
099300       MOVE MFS-ROER-EJ-FAELT TO MOD-IDAO (MOD-IX)                        
099400       SET MOD-IX UP BY +1                                                
099500     END-PERFORM                                                          
099600     SET MOD-IZ TO +1                                                     
099700     PERFORM UNTIL                                                        
099800      ( MOD-IZ > +14 )                                                    
099900       MOVE MFS-ROER-EJ-FAELT TO MOD-KAT-BEEMBLEM (MOD-IZ)                
100000       SET MOD-IZ UP BY +1                                                
100100     END-PERFORM                                                          
100200     CONTINUE.                                                            
100300     EJECT                                                                
100400 E-KONTROLL-VCBV SECTION.                                                 
100500                                                                          
100600     MOVE MSG-SIGNON-USERID   TO SEC-IDUSER                               
100700     MOVE '9402'              TO SEC-IDTRANS                              
100800                                                                          
100900     CALL WSECURIT USING         SEC-IDUSER                               
101000                                 SEC-IDTRANS                              
101100                                 SEC-IDKEY                                
101200                                 SEC-KDSVAR                               
101300     CONTINUE.                                                            
101400                                                                          
101500     EJECT                                                                
101600* IMS SEKTIONER                                                           
101700     SKIP3                                                                
101800 IMS-GET-MSG SECTION.                                                     
101900                                                                          
102000     MOVE '  QC' TO GODK-STATUSKODER                                      
102100     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
102200     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
102300     PERFORM IMS-STATUSKONTROLL                                           
102400     .                                                                    
102500     SKIP3                                                                
102600 IMS-INSERT-MSG SECTION.                                                  
102700                                                                          
102800*    IF MSGI-IDLAND-SPR NOT = 'GB'                                        
102900*       MOVE '0' TO MFS-KDHUVOMR                                          
103000*    END-IF                                                               
103100     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
103200     MOVE SPACE TO GODK-STATUSKODER                                       
103300     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
103400     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
103500     PERFORM IMS-STATUSKONTROLL                                           
103600     .                                                                    
103700     EJECT                                                                
103800 IMS-GET-ARTC01 SECTION.                                                  
103900                                                                          
104000     STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-X ')'                         
104100            DELIMITED BY SIZE INTO SSA1                                   
104200     MOVE '  GE' TO GODK-STATUSKODER                                      
104300     CALL CBLTDLI USING GU ARTC-PCB DLI-IO-AREA-1 SSA1                    
104400     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
104500     PERFORM IMS-STATUSKONTROLL                                           
104600     .                                                                    
104700     SKIP3                                                                
104800 IMS-GU-ARTC11 SECTION.                                                   
104900                                                                          
105000     STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-X ')'                         
105100            DELIMITED BY SIZE INTO SSA1                                   
105200     MOVE 'WLARTC11 ' TO SSA2                                             
105300     MOVE '  GE' TO GODK-STATUSKODER                                      
105400     CALL CBLTDLI USING GU ARTC-PCB DLI-IO-AREA-2 SSA1 SSA2               
105500     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
105600     PERFORM IMS-STATUSKONTROLL                                           
105700     .                                                                    
105800     SKIP3                                                                
105900 IMS-GET-ARTC11 SECTION.                                                  
106000                                                                          
106100     MOVE 'WLARTC11 ' TO SSA1                                             
106200     MOVE '  GE' TO GODK-STATUSKODER                                      
106300     CALL CBLTDLI USING GNP ARTC-PCB DLI-IO-AREA-2 SSA1                   
106400     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
106500     PERFORM IMS-STATUSKONTROLL                                           
106600     .                                                                    
106700     EJECT                                                                
106800 IMS-GET-ARTC25 SECTION.                                                  
106900                                                                          
107000     MOVE 'WLARTC11 ' TO SSA1                                             
107100     STRING 'WLARTC25*F(KDNOTTYP =' W-KDNOTTYP-X ')'                      
107200            DELIMITED BY SIZE INTO SSA2                                   
107300     MOVE '  GE' TO GODK-STATUSKODER                                      
107400     CALL CBLTDLI USING GHNP ARTC-PCB DLI-IO-AREA-3 SSA1 SSA2             
107500     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
107600     PERFORM IMS-STATUSKONTROLL                                           
107700     .                                                                    
107800     SKIP3                                                                
107900 IMS-GET-HOLD-NOTERING SECTION.                                           
108000                                                                          
108100     STRING 'WLARTC25(KDNOTTYP =' W-KDNOTTYP-X ')'                        
108200            DELIMITED BY SIZE INTO SSA1                                   
108300     MOVE '  GE' TO GODK-STATUSKODER                                      
108400     CALL CBLTDLI USING GHNP ARTC-PCB DLI-IO-AREA-3 SSA1                  
108500     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
108600     PERFORM IMS-STATUSKONTROLL                                           
108700     .                                                                    
108800     EJECT                                                                
108900 IMS-GET-BENA01-BSEQ   SECTION.                                           
109000                                                                          
109100     STRING 'WLBENA01(WDD3BSEQ =' W-IDARTNR-X ')'                         
109200            DELIMITED BY SIZE INTO SSA1                                   
109300     MOVE '  ' TO GODK-STATUSKODER                                        
109400     CALL CBLTDLI USING GU BENA-PCB DLI-IO-AREA-3 SSA1                    
109500     MOVE BENA-STATUS-CODE TO STATUS-WS                                   
109600     PERFORM IMS-STATUSKONTROLL                                           
109700     .                                                                    
109800     SKIP3                                                                
109900 IMS-GET-BENA11-BSEQ   SECTION.                                           
110000                                                                          
110100     STRING 'WLBENA11(IDSKYLT  =' W-IDSKYLT-X ')'                         
110200            DELIMITED BY SIZE INTO SSA1                                   
110300     MOVE '  ' TO GODK-STATUSKODER                                        
110400     CALL CBLTDLI USING GNP BENA-PCB DLI-IO-AREA-3 SSA1                   
110500     MOVE BENA-STATUS-CODE TO STATUS-WS                                   
110600     PERFORM IMS-STATUSKONTROLL                                           
110700     .                                                                    
110800     EJECT                                                                
110900 IMS-GET-MASTER-WDN6 SECTION.                                             
111000                                                                          
111100     STRING 'WLKATN01(IDARTNR  =' W-IDARTNR-X ')'                         
111200            DELIMITED BY SIZE INTO SSA1                                   
111300     MOVE '  GE' TO GODK-STATUSKODER                                      
111400     CALL CBLTDLI USING GU KATN-PCB DLI-IO-AREA-3 SSA1                    
111500     MOVE KATN-STATUS-CODE TO STATUS-WS                                   
111600     PERFORM IMS-STATUSKONTROLL                                           
111700     .                                                                    
111800     SKIP3                                                                
111900 IMS-GET-KATINFO-MASTER SECTION.                                          
112000                                                                          
112100     MOVE 'WLKATN11 ' TO SSA1                                             
112200     MOVE '  GE' TO GODK-STATUSKODER                                      
112300     CALL CBLTDLI USING GNP KATN-PCB DLI-IO-AREA-3 SSA1                   
112400     MOVE KATN-STATUS-CODE TO STATUS-WS                                   
112500     PERFORM IMS-STATUSKONTROLL                                           
112600     .                                                                    
112700     EJECT                                                                
112800 IMS-DELETE-NOTERING SECTION.                                             
112900                                                                          
113000     MOVE '  ' TO GODK-STATUSKODER                                        
113100     CALL CBLTDLI USING DLET ARTC-PCB DLI-IO-AREA-3                       
113200     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
113300     PERFORM IMS-STATUSKONTROLL                                           
113400     .                                                                    
113500     SKIP3                                                                
113600 IMS-INSERT-NOTERING SECTION.                                             
113700                                                                          
113800     STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-X ')'                         
113900            DELIMITED BY SIZE INTO SSA1                                   
114000     MOVE 'WLARTC11 ' TO SSA2                                             
114100     MOVE 'WLARTC25 ' TO SSA3                                             
114200     MOVE '  II' TO GODK-STATUSKODER                                      
114300     CALL CBLTDLI USING                                                   
114400              ISRT ARTC-PCB DLI-IO-AREA-3 SSA1 SSA2 SSA3                  
114500     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
114600     PERFORM IMS-STATUSKONTROLL                                           
114700     .                                                                    
114800     SKIP3                                                                
114900 IMS-REPLACE-NOTERING SECTION.                                            
115000                                                                          
115100     MOVE '  ' TO GODK-STATUSKODER                                        
115200     CALL CBLTDLI USING REPL ARTC-PCB DLI-IO-AREA-3                       
115300     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
115400     PERFORM IMS-STATUSKONTROLL                                           
115500     .                                                                    
115600     EJECT                                                                
115700 IMS-GU-WDF701 SECTION.                                                   
115800                                                                          
115900     STRING 'WDF701  (WDF701KY =' W-WDF701KY-X ')'                        
116000            DELIMITED BY SIZE INTO SSA1                                   
116100     MOVE '  GE' TO GODK-STATUSKODER                                      
116200     CALL CBLTDLI USING GU WDF7-PCB DLI-IO-AREA-7 SSA1                    
116300     MOVE WDF7-STATUS-CODE TO STATUS-WS                                   
116400     PERFORM IMS-STATUSKONTROLL                                           
116500     .                                                                    
116600     SKIP3                                                                
116700 IMS-GU-WDK401 SECTION.                                                   
116800     STRING 'WDK401  (IDARTNR  =' W-IDARTNR-X ')'                         
116900            DELIMITED BY SIZE INTO SSA1                                   
117000     MOVE '  GE' TO GODK-STATUSKODER                                      
117100     CALL CBLTDLI USING GU WDK4-PCB DLI-IO-WDK401 SSA1                    
117200     MOVE WDK4-STATUS-CODE TO STATUS-WS                                   
117300     PERFORM IMS-STATUSKONTROLL                                           
117400     .                                                                    
117500     SKIP3                                                                
117600 IMS-STATUSKONTROLL SECTION.                                              
117700     SET STATUS-IX TO 1                                                   
117800     SEARCH GODK-STATUS                                                   
117900       AT END                                                             
118000         CALL FELLOG                                                      
118100     WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                    
118200     END-SEARCH                                                           
118300     .                                                                    
118400* DB2 SEKTIONER                                                           
118500     SKIP3                                                                
118600                                                                          
118700 DB2-DCL-OPN-TP1ARTK-CRS  SECTION.                                        
118800     MOVE 'DB2-DCL-OPN-TP1ARTK   ' TO  WS-DB2-SEKTION                     
118900                                                                          
119000     MOVE 000100  TO GOOD-SQLCODECODES                                    
119100                                                                          
119200     EXEC SQL                                                             
119300         DECLARE TP1ARTK-CRS CURSOR FOR                                   
119400           SELECT  A.IDKAMP                                               
119500                  ,A.IDARTNR                                              
119600                  ,B.TISTADAT_KAMP                                        
119700                  ,B.TISTODAT_KAMP                                        
119800                  ,B.KDKAMP                                               
119900                                                                          
120000           FROM    TP1ARTK A                                              
120100                  ,TP1KAMP B                                              
120200                                                                          
120300           WHERE   A.IDARTNR = :W-IDARTNR                                 
120400               AND A.IDKAMP  =  B.IDKAMP                                  
120500                                                                          
120600           ORDER BY A.IDARTNR                                             
120700     END-EXEC                                                             
120800                                                                          
120900     MOVE 000100  TO GOOD-SQLCODECODES                                    
121000     EXEC SQL OPEN TP1ARTK-CRS END-EXEC                                   
121100     .                                                                    
121200     SKIP3                                                                
121300 DB2-FETCH-TP1ARTK-CRS  SECTION.                                          
121400     MOVE 'DB2-FETCH-TP1ARTK   ' TO  WS-DB2-SEKTION                       
121500     SKIP2                                                                
121600     MOVE 000100  TO GOOD-SQLCODECODES                                    
121700     EXEC SQL                                                             
121800         FETCH TP1ARTK-CRS INTO                                           
121900                    :TP1KAMP-IDKAMP                                       
122000                   ,:TP1ARTK-IDARTNR                                      
122100                   ,:TP1KAMP-TISTADAT-KAMP                                
122200                   ,:TP1KAMP-TISTODAT-KAMP                                
122300                   ,:TP1KAMP-KDKAMP                                       
122400     END-EXEC                                                             
122500                                                                          
122600     MOVE SQLCODE TO SQLCODE-WS                                           
122700     PERFORM DB2-STATUS-CHECK                                             
122800     .                                                                    
122900     SKIP3                                                                
123000 DB2-CLOSE-TP1ARTK-CRS  SECTION.                                          
123100     MOVE 'DB2-CLOSE-TP1ARTK   ' TO  WS-DB2-SEKTION                       
123200                                                                          
123300     EXEC SQL CLOSE TP1ARTK-CRS END-EXEC                                  
123400     .                                                                    
123500     EJECT                                                                
123600 DB2-STATUS-CHECK  SECTION.                                               
123700                                                                          
123800     SET SQLCODE-IX TO 1                                                  
123900     SEARCH GOOD-SQLCODE                                                  
124000       AT END                                                             
124100*         STRING 'INVALID DB2 SQL STATUS CODE: ' SQLCODE-WS               
124200*         DELIMITED BY SIZE INTO ERROR-TEXT                               
124300          CALL FELLOG                                                     
124400       WHEN GOOD-SQLCODE (SQLCODE-IX) = SQLCODE-WS CONTINUE               
124500     END-SEARCH                                                           
124600     .                                                                    
124700     EJECT                                                                
