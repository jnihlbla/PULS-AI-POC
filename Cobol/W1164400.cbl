000100 ID DIVISION.                                                             
000200                                                                          
000300 PROGRAM-ID.     W1164400.                                                
000400 AUTHOR.         KENT JEBSEN.                                             
000500 DATE-WRITTEN.   97/08/06.                                                
000600 DATE-COMPILED.                                                           
000700                                                                          
000800                                                                          
000900*    FUNKTION:                                                            
001000*        STYR VILKA ERSÄTTNINGAR SOM SKA SKICKAS TILL VIPS.               
001100*        ÖVRIGA VÄRLDEN. (EJ NDC).                                        
001200*                                                                         
001300*        PROGRAMMET LÄSER      WLARTC (WDK6)                              
001400*        PROGRAMMET LÄSER      WLERSA (WDD7)                              
001500*                                                                         
001600*    ABENDKODER:                                                          
001700*        U0016 -  . . . .                                                 
001800*        U1000 -  . . . .                                                 
001900*                                                                         
002000                                                                          
002100     SKIP3                                                                
002200 ENVIRONMENT DIVISION.                                                    
002300     SKIP2                                                                
002400 INPUT-OUTPUT SECTION.                                                    
002500                                                                          
002600 FILE-CONTROL.                                                            
002700     SKIP2                                                                
002800*          --- ERSATTA ARTIKLAR                                           
002900     SELECT W11641A                    ASSIGN TO W11644D1.                
003000     SKIP2                                                                
003100*          --- ERSATTA ARTIKLAR FÖR BEVAKNING AV ERS-MEDDELANDEN          
003200     SELECT W11645                     ASSIGN TO W11644D2.                
003300     SKIP2                                                                
003400*          --- ERSÄTTNINGSMEDDELANDEN VIPS DEF + PREL                     
003500     SELECT W11647                     ASSIGN TO W11644D3.                
003600     EJECT                                                                
003700 DATA DIVISION.                                                           
003800     SKIP3                                                                
003900 FILE SECTION.                                                            
004000     SKIP3                                                                
004100 FD  W11641A                                                              
004200     RECORDING       F                                                    
004300     BLOCK CONTAINS  0.                                                   
004400                                                                          
004500*01  -COPY W11641A     -L.                                                
004600     SKIP3                                                                
004700 FD  W11645                                                               
004800     RECORDING       F                                                    
004900     BLOCK CONTAINS  0.                                                   
005000                                                                          
005100*01  POST -COPY W11645 -PRE  W11645-  -L.                                 
005200     SKIP3                                                                
005300 FD  W11647                                                               
005400     RECORDING       F                                                    
005500     BLOCK CONTAINS  0.                                                   
005600                                                                          
005700*01  POST -COPY W11646 -PRE  W11647-  -L.                                 
005800     EJECT                                                                
005900 WORKING-STORAGE SECTION.                                                 
006000     SKIP2                                                                
006100                                                                          
006200*    -- CHECKED BY WY2000                                                 
006300 77  IDPGM                       PIC X(8)    VALUE 'W1164400'.            
006400 77  JA                          PIC X       VALUE 'J'.                   
006500 77  NEJ                         PIC X       VALUE 'N'.                   
006600 77  WS-KDTEXTGR-RAKNARE         PIC 9(2).                                
006700 77  WS-FL-TYP1                  PIC X.                                   
006800 77  WS-IDARTNR-NUM              PIC 9(9)  VALUE ZERO.                    
006900 77  WS-FLSALDO-EU               PIC X.                                   
007000 77  WS-KVAKS                    PIC S9(7) VALUE ZERO COMP-3.             
007100 77  WS-RETUR                    PIC S9(7) VALUE ZERO COMP-3.             
007200     SKIP2                                                                
007300 01 WS-KDERS-NUM                PIC 9(2).                                 
007400 01 WS-KDERS                             REDEFINES WS-KDERS-NUM.          
007500     03 WS-KDERSPOS1            PIC X.                                    
007600     03 WS-KDERSPOS2            PIC X.                                    
007700                                                                          
007800 01  WS-IDARTNR.                                                          
007900     03 WS-IDARTNR-BLANK         PIC X(11) VALUE SPACE.                   
008000     03 WS-IDARTNR-ALFA          PIC X(9).                                
008100                                                                          
008200 01  WS-IDARTNR-TILLK.                                                    
008300     03 WS-IDARTNR-TILLK-BLANK   PIC X(11) VALUE SPACE.                   
008400     03 WS-IDARTNR-TILLK-ALFA    PIC X(9).                                
008500                                                                          
008600 01  FELTEXT.                                                             
008700     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
008800     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
008900                                                                          
009000 77  WS-KDERS-CHK                PIC 9(3)    VALUE ZERO.                  
009100     88  SUP2X-GR-USEUP                      VALUE 21 24 27 28 29.        
009200                                                                          
009300 77  W11641A-EOF-SW              PIC X       VALUE 'N'.                   
009400     88  END-OF-W11641A                      VALUE 'J'.                   
009500     EJECT                                                                
009600 01  DYNAMISKA-SUBPROGRAM.                                                
009700*                                                                         
009800     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
009900     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
010000     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
010100     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
010200     EJECT                                                                
010300*    ---- PARAMETRAR TILL WDATKONV                                        
010400 01  FILLER               PIC X(16)   VALUE  'DATKONV-AREA'.              
010500*01  -COPY WDATAREA                                                       
010600*    --- PARAMETRAR TILL POSTSUM                                          
010700*                                                                         
010800*01  -COPY W0005   -PRE  POSTSUM-                                         
010900     EJECT                                                                
011000 01  IN-AREA-START               PIC X(24)   VALUE                        
011100                                             'IN-AREA-START'.             
011200     SKIP2                                                                
011300                                                                          
011400*01  AREA -COPY W11641A    -PRE IN-                                       
011500     EJECT                                                                
011600 01  UT1-AREA-START              PIC X(24)   VALUE                        
011700                                             'UT1-AREA-START'.            
011800     SKIP2                                                                
011900                                                                          
012000*01  AREA -COPY W11645     -PRE UT1-                                      
012100     EJECT                                                                
012200 01  UT2-AREA-START              PIC X(24)   VALUE                        
012300                                             'UT2-AREA-START'.            
012400     SKIP2                                                                
012500                                                                          
012600*01  AREA -COPY W11646     -PRE UT2-                                      
012700     EJECT                                                                
012800 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
012900     SKIP3                                                                
013000 01  NYCKLAR-TILL-DLI.                                                    
013100     03  W-IDARTNR-X.                                                     
013200         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
013300     03  W-IDDC-MIN-X.                                                    
013400         05  W-IDDC-MIN          PIC X(2)    VALUE SPACE.                 
013500     03  W-IDDC-MAX-X.                                                    
013600         05  W-IDDC-MAX          PIC X(2)    VALUE SPACE.                 
013700     03  W-IDPTYP-X.                                                      
013800         05  W-IDPTYP            PIC X(3)    VALUE SPACE.                 
013900     SKIP2                                                                
014000*    --- STATUS-KOD FRÅN IMS                                              
014100 01  STATUS-WS                   PIC XX.                                  
014200     88  SEGMENT-FINNS                       VALUE '  '.                  
014300     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
014400     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
014500     88  SEGMENT-SLUT                        VALUE 'GB'.                  
014600     88  IMS-EJ-OK                           VALUE 'XD'.                  
014700     SKIP2                                                                
014800 01  GODK-STATUSKODER.                                                    
014900     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
015000     SKIP3                                                                
015100 01  SSA1                        PIC X(64).                               
015200 01  SSA2                        PIC X(64).                               
015300     EJECT                                                                
015400*    --- IMS FUNKTIONSKODER                                               
015500*01  -COPY W0003                                                          
015600     EJECT                                                                
015700*    ---  DLI INPUT-OUTPUT AREA                                           
015800                                                                          
015900 01  FILLER         PIC X(24) VALUE 'DLI-IO-WLARTC01'.                    
016000 01  DLI-IO-WLARTC01.                                                     
016100*    03  -COPY WDK601  -PRE ARTC-                                         
016200 01  FILLER         PIC X(24) VALUE 'DLI-IO-WLERSA01'.                    
016300 01  DLI-IO-WLERSA01.                                                     
016400*    03  -COPY WDD701  -PRE ERSA-                                         
016500     EJECT                                                                
016600 01  FILLER         PIC X(24) VALUE 'DLI-IO-WLERSA11'.                    
016700 01  DLI-IO-WLERSA11.                                                     
016800*    03  -COPY WDD702  -PRE ERSA-                                         
016900     EJECT                                                                
017000 01  FILLER         PIC X(24) VALUE 'DLI-IO-WLERSA13'.                    
017100 01  DLI-IO-WLERSA13.                                                     
017200*    03  -COPY WDD704  -PRE ERSA-                                         
017300     EJECT                                                                
017400 01  FILLER         PIC X(24) VALUE 'DLI-IO-WDL601'.                      
017500 01  DLI-IO-WDL601.                                                       
017600     03  -COPY WDL601                                                     
017700     EJECT                                                                
017800 01  FILLER         PIC X(24) VALUE 'DLI-IO-WDL611'.                      
017900 01  DLI-IO-WDL611.                                                       
018000     03  -COPY WDL611                                                     
018100     EJECT                                                                
018200 LINKAGE SECTION.                                                         
018300                                                                          
018400*01  -COPY W0009   -PRE MSG-                                              
018500     EJECT                                                                
018600*01  -COPY W0008  -PRE ARTC-                                              
018700     05  FILLER                  PIC X.                                   
018800     EJECT                                                                
018900*01  -COPY W0008  -PRE ERSA-                                              
019000     05  FILLER                  PIC X.                                   
019100     EJECT                                                                
019200*01  -COPY W0008  -PRE WDL6-                                              
019300     05  FILLER                  PIC X.                                   
019400     EJECT                                                                
019500 PROCEDURE DIVISION  USING MSG-PCB ARTC-PCB ERSA-PCB WDL6-PCB.            
019600 MAIN SECTION.                                                            
019700     ENTRY 'DLITCBL' USING MSG-PCB ARTC-PCB ERSA-PCB WDL6-PCB.            
019800                                                                          
019900     SKIP2                                                                
020000     PERFORM A-INIT                                                       
020100     PERFORM S01-LAES-W11641A                                             
020200                                                                          
020300     PERFORM UNTIL END-OF-W11641A                                         
020400       PERFORM B-BEARBETA                                                 
020500       PERFORM S01-LAES-W11641A                                           
020600     END-PERFORM                                                          
020700                                                                          
020800     PERFORM Z-FINIT                                                      
020900                                                                          
021000     MOVE ZERO TO RETURN-CODE                                             
021100     GOBACK                                                               
021200     .                                                                    
021300     EJECT                                                                
021400 A-INIT SECTION.                                                          
021500                                                                          
021600     OPEN INPUT  W11641A                                                  
021700                                                                          
021800     OPEN OUTPUT W11645                                                   
021900                 W11647                                                   
022000                                                                          
022100     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
022200     .                                                                    
022300     EJECT                                                                
022400 B-BEARBETA SECTION.                                                      
022500                                                                          
022600     MOVE IN-IDARTNR    TO W-IDARTNR                                      
022700     MOVE IN-KDERS      TO WS-KDERS-NUM                                   
022800                           WS-KDERS-CHK                                   
022900     MOVE NEJ           TO WS-FLSALDO-EU                                  
023000***                                                                       
023100*** CHECK SUPER SESSION CODE. FOR PARTS CODED WITH USE-UP OR              
023200*** EXPIRED SUPERSESSIONS, WE CHECK STOCK BALANCES & RETURNS              
023300*** BELOW CODED ARE CHECKED - 21,24,27,28,29                              
023400***                                                                       
023500     IF SUP2X-GR-USEUP  AND                                               
023600        IN-FLERSATT = 'N'                                                 
023700        PERFORM S21-CHECK-STOCK-BAL                                       
023800     END-IF                                                               
023900     IF IN-KDERS = 0                                                      
024000       PERFORM BA-SKAPA-BACKTRANS                                         
024100       IF IN-FLERSATT = 'J'                                               
024200          PERFORM S13-SKRIV-W11647                                        
024300       ELSE                                                               
024400          PERFORM S13-SKRIV-W11647                                        
024500       END-IF                                                             
024600     ELSE                                                                 
024700        IF IN-KDERS > 20                                                  
024800***                                                                       
024900*** IF STOCK BALANCE EXISTS, DELAY SENDING INFORMATION TO VIPS            
025000***                                                                       
025100           MOVE 'J'          TO UT1-FLERSATT                              
025200                                                                          
025300           IF IN-FLERSATT = 'N'                                           
025400           AND WS-FLSALDO-EU = NEJ                                        
025500              PERFORM BB-SKAPA-FIL                                        
025600           ELSE                                                           
025700              IF WS-FLSALDO-EU = JA                                       
025800                 MOVE 'N' TO UT1-FLERSATT                                 
025900              END-IF                                                      
026000           END-IF                                                         
026100           PERFORM BC-SKAPA-W11645                                        
026200        ELSE                                                              
026300           IF IN-KDERS < 10                                               
026400              IF IN-FLERSATT = 'J'                                        
026500                 PERFORM BA-SKAPA-BACKTRANS                               
026600                 PERFORM BG-SKAPA-PREL-FIL                                
026700              ELSE                                                        
026800                 PERFORM BG-SKAPA-PREL-FIL                                
026900              END-IF                                                      
027000           ELSE                                                           
027100              IF IN-FLERSATT = 'N'                                        
027200                 PERFORM BG-SKAPA-PREL-FIL                                
027300              ELSE                                                        
027400                 PERFORM BA-SKAPA-BACKTRANS                               
027500                 PERFORM BG-SKAPA-PREL-FIL                                
027600              END-IF                                                      
027700           END-IF                                                         
027800        END-IF                                                            
027900     END-IF                                                               
028000     .                                                                    
028100     EJECT                                                                
028200 BA-SKAPA-BACKTRANS SECTION.                                              
028300                                                                          
028400     MOVE IN-IDARTNR      TO WS-IDARTNR-NUM                               
028500     MOVE WS-IDARTNR-NUM  TO WS-IDARTNR-ALFA                              
028600     INSPECT WS-IDARTNR-ALFA REPLACING LEADING ZERO BY SPACE              
028700     MOVE WS-IDARTNR      TO UT2-IDARTNR20                                
028800                                                                          
028900     PERFORM IMS-GET-ARTC-ARTC                                            
029000                                                                          
029100     IF SEGMENT-FINNS                                                     
029200        MOVE ARTC-ART-TIERSDAT    TO DAT-I-TIDATUM                        
029300        MOVE 'AAVVD'              TO DAT-KDDATFORM                        
029400                                                                          
029500        PERFORM S02-WDATKONV                                              
029600                                                                          
029700        IF DAT-KDSVAR-OK                                                  
029800           MOVE DAT-TIAAMMDD TO UT2-TIERSDAT-002                          
029900        ELSE                                                              
030000           MOVE 0            TO UT2-TIERSDAT-002                          
030100        END-IF                                                            
030200     ELSE                                                                 
030300        MOVE 0              TO UT2-TIERSDAT-002                           
030400     END-IF                                                               
030500                                                                          
030600     MOVE 'Y'             TO UT2-FLDEL                                    
030700     MOVE 0               TO UT2-KDERS                                    
030800     MOVE 0               TO UT2-IDKORTNR                                 
030900     MOVE 0               TO UT2-KDTEXTGR                                 
031000     MOVE 'N'             TO UT2-FLTEXT                                   
031100     MOVE SPACE           TO UT2-IDARTNR20-TILLK                          
031200     MOVE 0               TO UT2-DIERS-TILLK                              
031300     MOVE 0               TO UT2-KDARTUTG                                 
031400     MOVE SPACE           TO UT2-KDUTGSTA                                 
031500     MOVE SPACE           TO UT2-KDUTGSTR                                 
031600     MOVE 0               TO UT2-KDPRODSL                                 
031700     .                                                                    
031800     EJECT                                                                
031900 BB-SKAPA-FIL SECTION.                                                    
032000                                                                          
032100     MOVE IN-IDARTNR      TO WS-IDARTNR-NUM                               
032200     MOVE WS-IDARTNR-NUM  TO WS-IDARTNR-ALFA                              
032300     INSPECT WS-IDARTNR-ALFA REPLACING LEADING ZERO BY SPACE              
032400     MOVE WS-IDARTNR      TO UT2-IDARTNR20                                
032500                                                                          
032600     PERFORM IMS-GET-ARTC-ARTC                                            
032700                                                                          
032800     IF SEGMENT-FINNS                                                     
032900       MOVE ARTC-ART-TIERSDAT    TO DAT-I-TIDATUM                         
033000       MOVE 'AAVVD'              TO DAT-KDDATFORM                         
033100                                                                          
033200       PERFORM S02-WDATKONV                                               
033300                                                                          
033400       IF DAT-KDSVAR-OK                                                   
033500         MOVE DAT-TIAAMMDD TO UT2-TIERSDAT-002                            
033600       ELSE                                                               
033700         MOVE 0            TO UT2-TIERSDAT-002                            
033800       END-IF                                                             
033900                                                                          
034000       MOVE ARTC-ART-KDPRODSL  TO UT2-KDPRODSL                            
034100     ELSE                                                                 
034200       MOVE 0             TO UT2-TIERSDAT-002                             
034300       MOVE 0             TO UT2-KDPRODSL                                 
034400     END-IF                                                               
034500                                                                          
034600     MOVE 'N'             TO UT2-FLDEL                                    
034700     MOVE IN-KDERS        TO UT2-KDERS                                    
034800     IF WS-KDERSPOS2 = '2' OR '5'                                         
034900       MOVE 2  TO UT2-KDARTUTG                                            
035000     ELSE                                                                 
035100         MOVE 1  TO UT2-KDARTUTG                                          
035200     END-IF                                                               
035300     MOVE 'F'    TO UT2-KDUTGSTA                                          
035400                                                                          
035500     PERFORM IMS-GET-ERSA-WLERSA                                          
035600     IF SEGMENT-FINNS                                                     
035700       IF WS-KDERSPOS2 = '1' OR '2' OR '3' OR '7'                         
035800         IF ERSA-KVKORT = 1                                               
035900           MOVE 'S'   TO UT2-KDUTGSTR                                     
036000         ELSE                                                             
036100           MOVE 'M'   TO UT2-KDUTGSTR                                     
036200         END-IF                                                           
036300       ELSE                                                               
036400         IF WS-KDERSPOS2 = '4' OR '5' OR '6' OR '8'                       
036500           MOVE 'V'   TO UT2-KDUTGSTR                                     
036600         END-IF                                                           
036700       END-IF                                                             
036800                                                                          
036900       IF IN-KDERS = 29 OR 52                                             
037000          MOVE 0        TO UT2-IDKORTNR                                   
037100          MOVE 0        TO UT2-KDTEXTGR                                   
037200          MOVE 'N'      TO UT2-FLTEXT                                     
037300          MOVE SPACE    TO UT2-IDARTNR20-TILLK                            
037400          MOVE 0        TO UT2-DIERS-TILLK                                
037500          MOVE SPACE    TO UT2-KDUTGSTR                                   
037600                                                                          
037700          PERFORM S13-SKRIV-W11647                                        
037800       END-IF                                                             
037900                                                                          
038000       PERFORM IMS-GET-ERSA-ERSA11                                        
038100                                                                          
038200       IF SEGMENT-FINNS                                                   
038300         MOVE 1     TO WS-KDTEXTGR-RAKNARE                                
038400         MOVE 'N'   TO WS-FL-TYP1                                         
038500                                                                          
038600         PERFORM UNTIL SEGMENT-SAKNAS                                     
038700                                                                          
038800           MOVE ERSA-IDKORTNR   TO UT2-IDKORTNR                           
038900                                                                          
039000           IF WS-KDERSPOS2 = '1' OR '2' OR '3' OR '9' OR '7'              
039100             MOVE 0        TO UT2-KDTEXTGR                                
039200           ELSE                                                           
039300             IF ERSA-FLTEXT = 'N'                                         
039400               MOVE 'J'  TO WS-FL-TYP1                                    
039500             END-IF                                                       
039600             IF ERSA-FLTEXT = 'J' AND WS-FL-TYP1 = 'J'                    
039700               ADD  +1   TO WS-KDTEXTGR-RAKNARE                           
039800               MOVE 'N'  TO WS-FL-TYP1                                    
039900             END-IF                                                       
040000             MOVE WS-KDTEXTGR-RAKNARE TO UT2-KDTEXTGR                     
040100           END-IF                                                         
040200                                                                          
040300           MOVE ERSA-FLTEXT     TO UT2-FLTEXT                             
040400           IF ERSA-FLTEXT = 'N'                                           
040500             MOVE ERSA-IDARTNR-TILLK  TO WS-IDARTNR-NUM                   
040600             MOVE WS-IDARTNR-NUM      TO WS-IDARTNR-TILLK-ALFA            
040700             INSPECT WS-IDARTNR-TILLK-ALFA                                
040800                                 REPLACING LEADING ZERO BY SPACE          
040900             MOVE WS-IDARTNR-TILLK TO UT2-IDARTNR20-TILLK                 
041000             MOVE ERSA-DIERS-TILLK     TO UT2-DIERS-TILLK                 
041100           ELSE                                                           
041200             IF ERSA-FLTEXT = 'J'                                         
041300               MOVE ERSA-BEERS  TO UT2-IDARTNR20-TILLK                    
041400               MOVE  0     TO UT2-DIERS-TILLK                             
041500             END-IF                                                       
041600           END-IF                                                         
041700                                                                          
041800           PERFORM S13-SKRIV-W11647                                       
041900                                                                          
042000           PERFORM IMS-GET-ERSA-ERSA11                                    
042100                                                                          
042200         END-PERFORM                                                      
042300       END-IF                                                             
042400                                                                          
042500     ELSE                                                                 
042600                                                                          
042700       IF IN-KDERS = 29 OR 52                                             
042800         MOVE 0        TO UT2-IDKORTNR                                    
042900         MOVE 0        TO UT2-KDTEXTGR                                    
043000         MOVE 'N'      TO UT2-FLTEXT                                      
043100         MOVE SPACE    TO UT2-IDARTNR20-TILLK                             
043200         MOVE 0        TO UT2-DIERS-TILLK                                 
043300         MOVE SPACE    TO UT2-KDUTGSTR                                    
043400                                                                          
043500         PERFORM S13-SKRIV-W11647                                         
043600       END-IF                                                             
043700     END-IF                                                               
043800     .                                                                    
043900     EJECT                                                                
044000 BC-SKAPA-W11645 SECTION.                                                 
044100                                                                          
044200     MOVE IN-IDARTNR       TO UT1-IDARTNR                                 
044300     MOVE IN-KDERS         TO UT1-KDERS                                   
044400     PERFORM S11-SKRIV-W11645                                             
044500     .                                                                    
044600     EJECT                                                                
044700 BG-SKAPA-PREL-FIL SECTION.                                               
044800                                                                          
044900     MOVE IN-IDARTNR      TO WS-IDARTNR-NUM                               
045000     MOVE WS-IDARTNR-NUM  TO WS-IDARTNR-ALFA                              
045100     INSPECT WS-IDARTNR-ALFA REPLACING LEADING ZERO BY SPACE              
045200     MOVE WS-IDARTNR      TO UT2-IDARTNR20                                
045300                                                                          
045400     PERFORM IMS-GET-ARTC-ARTC                                            
045500     IF SEGMENT-FINNS                                                     
045600        MOVE ARTC-ART-KDPRODSL TO UT2-KDPRODSL                            
045700        MOVE 0                 TO UT2-TIERSDAT-002                        
045800     ELSE                                                                 
045900        MOVE 0                 TO UT2-TIERSDAT-002                        
046000        MOVE 0                 TO UT2-KDPRODSL                            
046100     END-IF                                                               
046200                                                                          
046300     MOVE 'N'             TO UT2-FLDEL                                    
046400     MOVE IN-KDERS        TO UT2-KDERS                                    
046500     IF WS-KDERSPOS2 = '1' OR '3' OR '4' OR '6' OR '9' OR                 
046600                       '7' OR '8'                                         
046700        MOVE 1            TO UT2-KDARTUTG                                 
046800     ELSE                                                                 
046900        IF WS-KDERSPOS2 = '2' OR '5'                                      
047000           MOVE 2         TO UT2-KDARTUTG                                 
047100        END-IF                                                            
047200     END-IF                                                               
047300                                                                          
047400     MOVE 'P'             TO UT2-KDUTGSTA                                 
047500                                                                          
047600     PERFORM IMS-GET-ERSA-WLERSA                                          
047700     IF SEGMENT-FINNS                                                     
047800        IF WS-KDERSPOS2 = '1' OR '2' OR '3' OR '7'                        
047900           IF ERSA-KVKORT = 1                                             
048000              MOVE 'S'   TO UT2-KDUTGSTR                                  
048100           ELSE                                                           
048200              MOVE 'M'   TO UT2-KDUTGSTR                                  
048300           END-IF                                                         
048400        ELSE                                                              
048500           IF WS-KDERSPOS2 = '4' OR '5' OR '6' OR '8'                     
048600              MOVE 'V'   TO UT2-KDUTGSTR                                  
048700           END-IF                                                         
048800        END-IF                                                            
048900                                                                          
049000        PERFORM IMS-GET-ERSA-ERSA13                                       
049100        IF SEGMENT-FINNS                                                  
049200           IF IN-KDERS > 20                                               
049300              CONTINUE                                                    
049400           ELSE                                                           
049500              IF IN-KDERS > 10 AND < 20                                   
049600                 MOVE ERSA-TIERSDAT-PREL-C1 TO DAT-I-TIDATUM              
049700                 MOVE 'AAVVD' TO DAT-KDDATFORM                            
049800                 PERFORM S02-WDATKONV                                     
049900                 IF DAT-KDSVAR-OK                                         
050000                    MOVE DAT-TIAAMMDD TO UT2-TIERSDAT-002                 
050100                 ELSE                                                     
050200                    MOVE 0            TO UT2-TIERSDAT-002                 
050300                 END-IF                                                   
050400              ELSE                                                        
050500                 IF IN-KDERS > 0 AND < 10                                 
050600                    MOVE ERSA-TIERSDAT-REG TO DAT-I-TIDATUM               
050700                    MOVE 'AAVVD'      TO DAT-KDDATFORM                    
050800                    PERFORM S02-WDATKONV                                  
050900                    IF DAT-KDSVAR-OK                                      
051000                       MOVE DAT-TIAAMMDD TO UT2-TIERSDAT-002              
051100                    ELSE                                                  
051200                       MOVE 0            TO UT2-TIERSDAT-002              
051300                    END-IF                                                
051400                 END-IF                                                   
051500              END-IF                                                      
051600           END-IF                                                         
051700        END-IF                                                            
051800                                                                          
051900        PERFORM IMS-GET-ERSA-ERSA11-FIRST                                 
052000                                                                          
052100        IF SEGMENT-FINNS                                                  
052200           MOVE 1     TO WS-KDTEXTGR-RAKNARE                              
052300           MOVE 'N'   TO WS-FL-TYP1                                       
052400                                                                          
052500           PERFORM UNTIL SEGMENT-SAKNAS                                   
052600                                                                          
052700              MOVE ERSA-IDKORTNR   TO UT2-IDKORTNR                        
052800              IF WS-KDERSPOS2 = '1' OR '2' OR '3' OR '9' OR '7'           
052900                 MOVE 0            TO UT2-KDTEXTGR                        
053000              ELSE                                                        
053100                 IF ERSA-FLTEXT = 'N'                                     
053200                    MOVE 'J'  TO WS-FL-TYP1                               
053300                 END-IF                                                   
053400                 IF ERSA-FLTEXT = 'J' AND WS-FL-TYP1 = 'J'                
053500                    ADD  +1   TO WS-KDTEXTGR-RAKNARE                      
053600                    MOVE 'N'  TO WS-FL-TYP1                               
053700                 END-IF                                                   
053800                 MOVE WS-KDTEXTGR-RAKNARE TO UT2-KDTEXTGR                 
053900              END-IF                                                      
054000                                                                          
054100              MOVE ERSA-FLTEXT     TO UT2-FLTEXT                          
054200              IF ERSA-FLTEXT = 'N'                                        
054300                 MOVE ERSA-IDARTNR-TILLK  TO WS-IDARTNR-NUM               
054400                 MOVE WS-IDARTNR-NUM      TO WS-IDARTNR-TILLK-ALFA        
054500                 INSPECT WS-IDARTNR-TILLK-ALFA                            
054600                              REPLACING LEADING ZERO BY SPACE             
054700                 MOVE WS-IDARTNR-TILLK TO UT2-IDARTNR20-TILLK             
054800                 MOVE ERSA-DIERS-TILLK     TO UT2-DIERS-TILLK             
054900              ELSE                                                        
055000                 IF ERSA-FLTEXT = 'J'                                     
055100                    MOVE ERSA-BEERS  TO UT2-IDARTNR20-TILLK               
055200                    MOVE  0     TO UT2-DIERS-TILLK                        
055300                 END-IF                                                   
055400              END-IF                                                      
055500                                                                          
055600              PERFORM S13-SKRIV-W11647                                    
055700                                                                          
055800              PERFORM IMS-GET-ERSA-ERSA11                                 
055900           END-PERFORM                                                    
056000        ELSE                                                              
056100           IF IN-KDERS = 09 OR 19                                         
056200              MOVE 0        TO UT2-IDKORTNR                               
056300              MOVE 0        TO UT2-KDTEXTGR                               
056400              MOVE 'N'      TO UT2-FLTEXT                                 
056500              MOVE SPACE    TO UT2-IDARTNR20-TILLK                        
056600              MOVE 0        TO UT2-DIERS-TILLK                            
056700              MOVE SPACE    TO UT2-KDUTGSTR                               
056800                                                                          
056900              PERFORM S13-SKRIV-W11647                                    
057000           END-IF                                                         
057100        END-IF                                                            
057200     END-IF                                                               
057300     .                                                                    
057400     EJECT                                                                
057500 Z-FINIT SECTION.                                                         
057600                                                                          
057700     CLOSE W11641A                                                        
057800           W11645                                                         
057900           W11647                                                         
058000     MOVE 'S'     TO POSTSUM-OPKOD                                        
058100     CALL POSTSUM USING POSTSUM-PARM                                      
058200     .                                                                    
058300     EJECT                                                                
058400 S01-LAES-W11641A  SECTION.                                               
058500                                                                          
058600     READ W11641A         INTO IN-AREA                                    
058700     AT END                                                               
058800        SET END-OF-W11641A TO TRUE                                        
058900                                                                          
059000     NOT AT END                                                           
059100        MOVE 'W11641A'     TO    POSTSUM-FDNAMN                           
059200        MOVE 'W11644D1'    TO    POSTSUM-DDNAMN2                          
059300        CALL POSTSUM       USING POSTSUM-PARM                             
059400     END-READ                                                             
059500     .                                                                    
059600     EJECT                                                                
059700 S02-WDATKONV SECTION.                                                    
059800     SKIP2                                                                
059900     CALL WDATKONV USING DAT-KDDATFORM                                    
060000                         DAT-I-TIDATUM                                    
060100                         DAT-O-TIDATUM                                    
060200                         DAT-KDSVAR                                       
060300     .                                                                    
060400     EJECT                                                                
060500 S11-SKRIV-W11645 SECTION.                                                
060600                                                                          
060700     WRITE W11645-POST    FROM UT1-AREA                                   
060800                                                                          
060900     MOVE 'W11645 '       TO POSTSUM-FDNAMN                               
061000     MOVE 'W11644D2'      TO POSTSUM-DDNAMN2                              
061100     CALL POSTSUM         USING POSTSUM-PARM                              
061200     .                                                                    
061300     EJECT                                                                
061400 S13-SKRIV-W11647 SECTION.                                                
061500                                                                          
061600     MOVE 'WB1'           TO UT2-IDPTYP                                   
061700     WRITE W11647-POST    FROM UT2-AREA                                   
061800                                                                          
061900     MOVE 'W11647 '       TO POSTSUM-FDNAMN                               
062000     MOVE 'W11644D3'      TO POSTSUM-DDNAMN2                              
062100     CALL POSTSUM         USING POSTSUM-PARM                              
062200     .                                                                    
062300     EJECT                                                                
062400 S21-CHECK-STOCK-BAL SECTION.                                             
062500*    ******    CHECK STOCK BALANCE                 ******                 
062600*    ******    IF STOCK EXISTS IN EU DCS SET FLAG  ******                 
062700                                                                          
062800     IF IN-KVTILLG-TOT  > ZERO                                            
062900        MOVE JA            TO WS-FLSALDO-EU                               
063000     END-IF                                                               
063100     IF WS-FLSALDO-EU = JA                                                
063200        CONTINUE                                                          
063300     ELSE                                                                 
063400        IF IN-KVAKS-SDC > ZERO                                            
063500           MOVE IN-KVAKS-SDC  TO WS-KVAKS                                 
063600           MOVE '1A'          TO W-IDDC-MIN                               
063700           MOVE '39'          TO W-IDDC-MAX                               
063800           PERFORM S21A-CHECK-RETURNS                                     
063900           IF WS-KVAKS > ZERO                                             
064000              MOVE JA         TO WS-FLSALDO-EU                            
064100           END-IF                                                         
064200        END-IF                                                            
064300        IF IN-KVAKS-CDC  > ZERO  AND                                      
064400           WS-FLSALDO-EU = NEJ                                            
064500           MOVE IN-KVAKS-CDC  TO WS-KVAKS                                 
064600           MOVE '11'          TO W-IDDC-MIN                               
064700           MOVE '11'          TO W-IDDC-MAX                               
064800           PERFORM S21A-CHECK-RETURNS                                     
064900           IF WS-KVAKS > ZERO                                             
065000              MOVE JA         TO WS-FLSALDO-EU                            
065100           END-IF                                                         
065200        END-IF                                                            
065300     END-IF                                                               
065400     .                                                                    
065500     EJECT                                                                
065600 S21A-CHECK-RETURNS   SECTION.                                            
065700*    **********THEN DELETE THSE COMMENTED LINES    ******                 
065800*    **********MODIFY PSB                          ******                 
065900                                                                          
066000     PERFORM IMS-GU-WDL601                                                
066100     IF SEGMENT-FINNS                                                     
066200        MOVE '310' TO W-IDPTYP                                            
066300        PERFORM IMS-GNP-WDL611                                            
066400        PERFORM UNTIL SEGMENT-SAKNAS                                      
066500           IF INL-KDRT = 07                                               
066600              COMPUTE WS-RETUR     = INL-KVAVIS - INL-KVANTMOT            
066700              SUBTRACT WS-RETUR FROM WS-KVAKS                             
066800           END-IF                                                         
066900           PERFORM IMS-GNP-WDL611                                         
067000        END-PERFORM                                                       
067100     END-IF                                                               
067200     .                                                                    
067300     EJECT                                                                
067400* --- IMS SEKTIONER ---                                                   
067500 IMS-GET-ARTC-ARTC SECTION.                                               
067600                                                                          
067700     STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-X ')'                         
067800          DELIMITED BY SIZE INTO SSA1                                     
067900     MOVE '  GE' TO GODK-STATUSKODER                                      
068000     CALL CBLTDLI USING GU ARTC-PCB DLI-IO-WLARTC01 SSA1                  
068100     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
068200     PERFORM IMS-STATUSKONTROLL                                           
068300     .                                                                    
068400     EJECT                                                                
068500 IMS-GET-ERSA-WLERSA SECTION.                                             
068600                                                                          
068700     STRING 'WLERSA01(IDARTNR  =' W-IDARTNR-X ')'                         
068800          DELIMITED BY SIZE INTO SSA1                                     
068900     MOVE '  GE' TO GODK-STATUSKODER                                      
069000     CALL CBLTDLI USING GU ERSA-PCB DLI-IO-WLERSA01 SSA1                  
069100     MOVE ERSA-STATUS-CODE TO STATUS-WS                                   
069200     PERFORM IMS-STATUSKONTROLL                                           
069300     .                                                                    
069400     EJECT                                                                
069500 IMS-GET-ERSA-ERSA11 SECTION.                                             
069600                                                                          
069700     STRING 'WLERSA11 '                                                   
069800          DELIMITED BY SIZE INTO SSA1                                     
069900     MOVE '  GE' TO GODK-STATUSKODER                                      
070000     CALL CBLTDLI USING GNP ERSA-PCB DLI-IO-WLERSA11 SSA1                 
070100     MOVE ERSA-STATUS-CODE TO STATUS-WS                                   
070200     PERFORM IMS-STATUSKONTROLL                                           
070300     .                                                                    
070400     EJECT                                                                
070500 IMS-GET-ERSA-ERSA11-FIRST SECTION.                                       
070600                                                                          
070700     MOVE 'WLERSA11*F' TO SSA1                                            
070800     MOVE '  GE' TO GODK-STATUSKODER                                      
070900     CALL CBLTDLI USING GNP ERSA-PCB DLI-IO-WLERSA11 SSA1                 
071000     MOVE ERSA-STATUS-CODE TO STATUS-WS                                   
071100     PERFORM IMS-STATUSKONTROLL                                           
071200     .                                                                    
071300     EJECT                                                                
071400 IMS-GET-ERSA-ERSA13 SECTION.                                             
071500                                                                          
071600     STRING 'WLERSA13 '                                                   
071700          DELIMITED BY SIZE INTO SSA1                                     
071800     MOVE '  GE' TO GODK-STATUSKODER                                      
071900     CALL CBLTDLI USING GNP ERSA-PCB DLI-IO-WLERSA13 SSA1                 
072000     MOVE ERSA-STATUS-CODE TO STATUS-WS                                   
072100     PERFORM IMS-STATUSKONTROLL                                           
072200     .                                                                    
072300     EJECT                                                                
072400 IMS-GU-WDL601 SECTION.                                                   
072500                                                                          
072600     STRING 'WDL601  (IDARTNR  =' W-IDARTNR-X ')'                         
072700          DELIMITED BY SIZE INTO SSA1                                     
072800     MOVE '  GEGB' TO GODK-STATUSKODER                                    
072900     CALL CBLTDLI USING GU WDL6-PCB DLI-IO-WDL601 SSA1                    
073000     MOVE WDL6-STATUS-CODE TO STATUS-WS                                   
073100     PERFORM IMS-STATUSKONTROLL                                           
073200     .                                                                    
073300     SKIP2                                                                
073400 IMS-GNP-WDL611 SECTION.                                                  
073500                                                                          
073600     STRING 'WDL611  (IDDC    >=' W-IDDC-MIN-X                            
073700                    '&IDDC    <=' W-IDDC-MAX-X                            
073800                    '&IDPTYP   =' W-IDPTYP-X ')'                          
073900            DELIMITED BY SIZE INTO SSA1                                   
074000     MOVE '  GEGB' TO GODK-STATUSKODER                                    
074100     CALL CBLTDLI USING GNP WDL6-PCB DLI-IO-WDL611 SSA1                   
074200     MOVE WDL6-STATUS-CODE TO STATUS-WS                                   
074300     PERFORM IMS-STATUSKONTROLL                                           
074400     .                                                                    
074500     EJECT                                                                
074600 IMS-STATUSKONTROLL SECTION.                                              
074700     SKIP2                                                                
074800     SET STATUS-IX TO 1                                                   
074900     SEARCH GODK-STATUS                                                   
075000       AT END                                                             
075100         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
075200           DELIMITED BY SIZE INTO FELTEXT                                 
075300         DISPLAY FELTEXT                                                  
075400         CALL FELLOG                                                      
075500       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
075600         CONTINUE                                                         
075700     END-SEARCH                                                           
075800     .                                                                    
