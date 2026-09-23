000100 ID DIVISION.                                                             
000200                                                                          
000300 PROGRAM-ID.     W1163400.                                                
000400 AUTHOR.         KENT JEBSEN.                                             
000500 DATE-WRITTEN.   96/12/11.                                                
000600 DATE-COMPILED.                                                           
000700                                                                          
000800                                                                          
000900*    FUNKTION:                                                            
001000*        STYR VILKA ERSÄTTNINGAR SOM SKA SKICKAS TILL VIPS.               
001100*        SKICKAR POSTER TILL INVENTERINGEN.                               
001200*                                                                         
001300*        PROGRAMMET LÄSER      WLARTS (WDK7)                              
001400*        PROGRAMMET LÄSER      WLARTC (WDK6)                              
001500*        PROGRAMMET LÄSER      WLERSA (WDD7)                              
001600*                                                                         
001700*    ABENDKODER:                                                          
001800*        U0016 -  . . . .                                                 
001900*        U1000 -  . . . .                                                 
002000*                                                                         
002100                                                                          
002200     SKIP3                                                                
002300 ENVIRONMENT DIVISION.                                                    
002400     SKIP2                                                                
002500 INPUT-OUTPUT SECTION.                                                    
002600                                                                          
002700 FILE-CONTROL.                                                            
002800     SKIP2                                                                
002900*          --- ERSATTA ARTIKLAR                                           
003000     SELECT W11631                     ASSIGN TO W11634D1.                
003100     SKIP2                                                                
003200*          --- ERSATTA ARTIKLAR FÖR BEVAKNING AV ERS-MEDDELANDEN          
003300     SELECT W11635                     ASSIGN TO W11634D2.                
003400     SKIP2                                                                
003500*          --- ERSÄTTNINGSMEDDELANDEN USA                                 
003600     SELECT W11636                     ASSIGN TO W11634D3.                
003700     SKIP2                                                                
003800*          --- ERSÄTTNINGSMEDDELANDEN CANADA                              
003900     SELECT W11637                     ASSIGN TO W11634D4.                
003910     SKIP2                                                                
004000*          --- INFORMATION TILL INVENTERINGEN                             
004100     SELECT W11638                     ASSIGN TO W11634D5.                
004101     SKIP2                                                                
004110*          --- UPPDATERINGAR TILL W1167D00 TIERSDAT-VIPS ETC.             
004120     SELECT W1163D                     ASSIGN TO W11634D6.                
004200     EJECT                                                                
004300 DATA DIVISION.                                                           
004400     SKIP3                                                                
004500 FILE SECTION.                                                            
004600     SKIP3                                                                
004700 FD  W11631                                                               
004800     RECORDING       F                                                    
004900     BLOCK CONTAINS  0.                                                   
005000                                                                          
005100*01  -COPY W11631      -L.                                                
005200     SKIP3                                                                
005300 FD  W11635                                                               
005400     RECORDING       F                                                    
005500     BLOCK CONTAINS  0.                                                   
005600                                                                          
005700*01  POST -COPY W11635 -PRE  W11635-  -L.                                 
005800     SKIP3                                                                
005900 FD  W11636                                                               
006000     RECORDING       F                                                    
006100     BLOCK CONTAINS  0.                                                   
006200                                                                          
006300*01  POST -COPY W11636 -PRE  W11636-  -L.                                 
006400     SKIP3                                                                
006500 FD  W11637                                                               
006600     RECORDING       F                                                    
006700     BLOCK CONTAINS  0.                                                   
006800                                                                          
006900*01  POST -COPY W11636 -PRE  W11637-  -L.                                 
006910     SKIP3                                                                
007000 FD  W11638                                                               
007100     RECORDING       F                                                    
007200     BLOCK CONTAINS  0.                                                   
007300                                                                          
007400*01  POST -COPY W11121 -PRE  W11638-  -L.                                 
007410     SKIP3                                                                
007420 FD  W1163D                                                               
007430     RECORDING       F                                                    
007440     BLOCK CONTAINS  0.                                                   
007450                                                                          
007460*01  POST -COPY W1167D -PRE  W1163D-  -L.                                 
007480                                                                          
007500     EJECT                                                                
007600 WORKING-STORAGE SECTION.                                                 
007700     SKIP2                                                                
007800                                                                          
007900*    -- CHECKED BY WY2000                                                 
008000 77  IDPGM                       PIC X(8)    VALUE 'W1163400'.            
008100 77  JA                          PIC X       VALUE 'J'.                   
008200 77  NEJ                         PIC X       VALUE 'N'.                   
008210 77  CURRENT-SECTION             PIC X(32)   VALUE SPACE.                 
008220 77  DBS-SECTION                 PIC X(32)   VALUE SPACE.                 
008250                                                                          
008300 77  WS-KVAKS-SDC                PIC S9(7)   VALUE ZERO COMP-3.           
008400 77  WS-RETUR                    PIC S9(7)   VALUE ZERO COMP-3.           
008410 77  ANTAL-TRAFF-ERS             PIC S9(7)   VALUE ZERO COMP-3.           
008420 77  ANTAL-ERS                   PIC S9(7)   VALUE ZERO COMP-3.           
008500 77  FL-USA-FINNS                PIC X.                                   
008600 77  FL-DC41-FINNS               PIC X.                                   
008700 77  FL-DC42-FINNS               PIC X.                                   
008800 77  FL-DC43-FINNS               PIC X.                                   
008810 77  FL-DC44-FINNS               PIC X.                                   
008900 77  FL-DC51-FINNS               PIC X.                                   
009000 77  WS-FLSALDO-USA              PIC X.                                   
009100 77  WS-FLSALDO-CAN              PIC X.                                   
009200 77  WS-FLERSATT-USA             PIC X.                                   
009300 77  WS-FLERSATT-CAN             PIC X.                                   
009400 77  WS-FLTILLK-SALDO-USA        PIC X     VALUE SPACE.                   
009500 77  WS-FLTILLK-SALDO-CAN        PIC X     VALUE SPACE.                   
009600 77  WS-KDTEXTGR-RAKNARE         PIC 9(2).                                
009700 77  WS-FL-TYP1                  PIC X.                                   
009800 77  WS-IDARTNR-NUM              PIC 9(9)  VALUE ZERO.                    
009900     SKIP2                                                                
009910 01  NDC-US-IX                   PIC 9(2)  VALUE ZERO.                    
009911 01  NDC-US-IX-MAX               PIC 9(2)  VALUE 25.                      
009912 01  WS-NDC-US-TAB.                                                       
009920     03 WS-NDC-US                PIC X(2)  OCCURS 25.                     
009930     03 WS-NDC-US-FINNS          PIC X(1)  OCCURS 25.                     
009940                                                                          
010000 01 WS-KDERS-NUM                PIC 9(2).                                 
010100 01 WS-KDERS                             REDEFINES WS-KDERS-NUM.          
010200     03 WS-KDERSPOS1            PIC X.                                    
010300     03 WS-KDERSPOS2            PIC X.                                    
010400                                                                          
010500 01  WS-IDARTNR.                                                          
010600     03 WS-IDARTNR-BLANK         PIC X(11) VALUE SPACE.                   
010700     03 WS-IDARTNR-ALFA          PIC X(9).                                
010800                                                                          
010900 01  WS-IDARTNR-TILLK.                                                    
011000     03 WS-IDARTNR-TILLK-BLANK   PIC X(11) VALUE SPACE.                   
011100     03 WS-IDARTNR-TILLK-ALFA    PIC X(9).                                
011200                                                                          
011300 01  FELTEXT.                                                             
011400     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
011500     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
011600                                                                          
011700 77  SW-CAN                      PIC X       VALUE 'N'.                   
011710 77  W11631-EOF-SW               PIC X       VALUE 'N'.                   
011800     88  END-OF-W11631                       VALUE 'J'.                   
011900     EJECT                                                                
012000*      --- VALID IDDC CODES                                               
012100*                                                                         
012200*01    -COPY WWLNDKON                                                     
012210*01    -COPY WWDCKONS                                                     
012300       EJECT                                                              
012400 01  DYNAMISKA-SUBPROGRAM.                                                
012500*                                                                         
012600     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
012700     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
012800     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
012900     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
013000     EJECT                                                                
013100*    ---- PARAMETRAR TILL WDATKONV                                        
013200 01  FILLER               PIC X(16)   VALUE  'DATKONV-AREA'.              
013300*01  -COPY WDATAREA                                                       
013400*    --- PARAMETRAR TILL POSTSUM                                          
013500*                                                                         
013600*01  -COPY W0005   -PRE  POSTSUM-                                         
013700     EJECT                                                                
013800 01  IN-AREA-START               PIC X(24)   VALUE                        
013900                                             'IN-AREA-START'.             
014000     SKIP2                                                                
014100                                                                          
014200*01  AREA -COPY W11631     -PRE IN-                                       
014300     EJECT                                                                
014400 01  UT1-AREA-START              PIC X(24)   VALUE                        
014500                                             'UT1-AREA-START'.            
014600     SKIP2                                                                
014700                                                                          
014800*01  AREA -COPY W11635     -PRE UT1-                                      
014900     EJECT                                                                
015000 01  UT2-AREA-START              PIC X(24)   VALUE                        
015100                                             'UT2-AREA-START'.            
015200     SKIP2                                                                
015300                                                                          
015400*01  AREA -COPY W11636     -PRE UT2-                                      
015500     EJECT                                                                
015600 01  UT3-AREA-START              PIC X(24)   VALUE                        
015700                                             'UT3-AREA-START'.            
015800     SKIP2                                                                
015900                                                                          
016000*01  AREA -COPY W11121     -PRE UT3-                                      
016100     EJECT                                                                
016101 01  UT4-AREA-START              PIC X(24)   VALUE                        
016102                                             'UT4-AREA-START'.            
016103                                                                          
016104*01  AREA -COPY W1167D     -PRE UT4-                                      
016110     EJECT                                                                
016200 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
016300     SKIP3                                                                
016400 01  NYCKLAR-TILL-DLI.                                                    
016500     03  W-IDARTNR-X.                                                     
016600         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
016700     03  W-IDDC-X.                                                        
016800         05  W-IDDC              PIC X(2)    VALUE SPACE.                 
016900     03  W-IDPTYP-X.                                                      
017000         05  W-IDPTYP            PIC X(3)    VALUE SPACE.                 
017100     03  W-IDKORTNR-X.                                                    
017200         05  W-IDKORTNR          PIC S9(2)   VALUE ZERO COMP-3.           
017300     SKIP2                                                                
017400*    --- STATUS-KOD FRÅN IMS                                              
017500 01  STATUS-WS                   PIC XX.                                  
017600     88  SEGMENT-FINNS                       VALUE '  '.                  
017700     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
017800     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
017900     88  SEGMENT-SLUT                        VALUE 'GB'.                  
018000     88  IMS-EJ-OK                           VALUE 'XD'.                  
018100     SKIP2                                                                
018200 01  GODK-STATUSKODER.                                                    
018300     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
018400     SKIP3                                                                
018500 01  SSA1                        PIC X(64).                               
018600 01  SSA2                        PIC X(64).                               
018700     EJECT                                                                
018800*    --- IMS FUNKTIONSKODER                                               
018900*01  -COPY W0003                                                          
019000     EJECT                                                                
019100*    ---  DLI INPUT-OUTPUT AREA                                           
019200                                                                          
019300 01  FILLER         PIC X(24) VALUE 'DLI-IO-WLARTS01'.                    
019400 01  DLI-IO-WLARTS01.                                                     
019500*    03  -COPY WDK701  -PRE ARTS-                                         
019600     EJECT                                                                
019700 01  FILLER         PIC X(24) VALUE 'DLI-IO-WLARTS11'.                    
019800 01  DLI-IO-WLARTS11.                                                     
019900*    03  -COPY WDK711  -PRE ARTS-                                         
020000 01  FILLER         PIC X(24) VALUE 'DLI-IO-WLARTC01'.                    
020100 01  DLI-IO-WLARTC01.                                                     
020200*    03  -COPY WDK601  -PRE ARTC-                                         
020300     EJECT                                                                
020400 01  FILLER         PIC X(24) VALUE 'DLI-IO-WLERSA01'.                    
020500 01  DLI-IO-WLERSA01.                                                     
020600*    03  -COPY WDD701  -PRE ERSA-                                         
020700     EJECT                                                                
020800 01  FILLER         PIC X(24) VALUE 'DLI-IO-WLERSA11'.                    
020900 01  DLI-IO-WLERSA11.                                                     
021000*    03  -COPY WDD702  -PRE ERSA-                                         
021100     EJECT                                                                
021200 01  FILLER         PIC X(24) VALUE 'DLI-IO-WLERSA13'.                    
021300 01  DLI-IO-WLERSA13.                                                     
021400*    03  -COPY WDD704  -PRE ERSA-                                         
021500     EJECT                                                                
021600 01  FILLER         PIC X(24) VALUE 'DLI-IO-WDL601'.                      
021700 01  DLI-IO-WDL601.                                                       
021800     03  -COPY WDL601                                                     
021900     EJECT                                                                
022310 01  FILLER         PIC X(24) VALUE 'DLI-IO-WDL611'.                      
022320 01  DLI-IO-WDL611.                                                       
022330     03  -COPY WDL611                                                     
022340     EJECT                                                                
022350 01  FILLER         PIC X(24) VALUE 'DLI-IO-WDB601'.                      
022360 01  DLI-IO-WDB601.                                                       
022370     03  -COPY WDB601                                                     
022380     EJECT                                                                
022400 LINKAGE SECTION.                                                         
022500                                                                          
022600*01  -COPY W0009   -PRE MSG-                                              
022700     EJECT                                                                
022800*01  -COPY W0008  -PRE ARTS-                                              
022900     05  FILLER                  PIC X.                                   
023000     EJECT                                                                
023100*01  -COPY W0008  -PRE ARTC-                                              
023200     05  FILLER                  PIC X.                                   
023300     EJECT                                                                
023400*01  -COPY W0008  -PRE ERSA-                                              
023500     05  FILLER                  PIC X.                                   
023600     EJECT                                                                
023700*01  -COPY W0008  -PRE WDL6-                                              
023800     05  FILLER                  PIC X.                                   
023900     EJECT                                                                
023910*01  -COPY W0008  -PRE WDB6-                                              
023920     05  FILLER                  PIC X.                                   
023930     EJECT                                                                
024000 PROCEDURE DIVISION  USING MSG-PCB ARTS-PCB ARTC-PCB ERSA-PCB             
024100                           WDL6-PCB WDB6-PCB.                             
024200 MAIN SECTION.                                                            
024300     ENTRY 'DLITCBL' USING MSG-PCB ARTS-PCB ARTC-PCB ERSA-PCB             
024400                           WDL6-PCB WDB6-PCB.                             
024500                                                                          
024600     SKIP2                                                                
024700     PERFORM A-INIT                                                       
024800     PERFORM S01-LAES-W11631                                              
024900                                                                          
025000     PERFORM UNTIL END-OF-W11631                                          
025100       PERFORM B-BEARBETA                                                 
025200       PERFORM S01-LAES-W11631                                            
025300     END-PERFORM                                                          
025400                                                                          
025500     PERFORM Z-FINIT                                                      
025600                                                                          
025700     MOVE ZERO TO RETURN-CODE                                             
025800     GOBACK                                                               
025900     .                                                                    
026000     EJECT                                                                
026100 A-INIT SECTION.                                                          
026200                                                                          
026300     OPEN INPUT  W11631                                                   
026400                                                                          
026500     OPEN OUTPUT W11635                                                   
026600                 W11636                                                   
026700                 W11637                                                   
026800                 W11638                                                   
026810                 W1163D                                                   
026900                                                                          
027000     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
027001                                                                          
027025     PERFORM IMS-GN-WDB601                                                
027026     MOVE 1 TO NDC-US-IX                                                  
027027     PERFORM UNTIL SEGMENT-SLUT                                           
027028                OR NDC-US-IX > NDC-US-IX-MAX                              
027029        IF DCS-USA                                                        
027031           MOVE DCS-IDDC TO WS-NDC-US(NDC-US-IX)                          
027040           MOVE NEJ      TO WS-NDC-US-FINNS(NDC-US-IX)                    
027050           ADD 1 TO NDC-US-IX                                             
027060        END-IF                                                            
027070        PERFORM IMS-GN-WDB601                                             
027080     END-PERFORM                                                          
027090                                                                          
027091     IF NDC-US-IX > NDC-US-IX-MAX                                         
027092        MOVE 'NDC-US TABELL FULL' TO FELTEXT                              
027093        CALL FELLOG                                                       
027094     ELSE                                                                 
027096        PERFORM UNTIL NDC-US-IX > NDC-US-IX-MAX                           
027098           MOVE SPACE TO WS-NDC-US(NDC-US-IX)                             
027099                         WS-NDC-US-FINNS(NDC-US-IX)                       
027100           ADD 1 TO NDC-US-IX                                             
027103        END-PERFORM                                                       
027104     END-IF                                                               
027110     .                                                                    
027200     EJECT                                                                
027300 B-BEARBETA SECTION.                                                      
027400     MOVE 'B-BEARBETA '  TO CURRENT-SECTION                               
027410                                                                          
027500     MOVE IN-IDARTNR  TO W-IDARTNR                                        
027600     MOVE IN-KDERS    TO WS-KDERS-NUM                                     
027700     MOVE NEJ         TO WS-FLTILLK-SALDO-USA                             
027800                         WS-FLTILLK-SALDO-CAN                             
028000       IF IN-KDERS = 0                                                    
028100         PERFORM BA-SKAPA-BACKTRANS                                       
028200       ELSE                                                               
028300         IF IN-KDERS > 20                                                 
028400            MOVE 'N'       TO  WS-FLSALDO-USA                             
028500            MOVE 'N'       TO  WS-FLSALDO-CAN                             
028600            IF IN-KDERS = 22 OR 23 OR 25 OR 26 OR 52                      
028700              PERFORM BE-KOLLA-FINNS                                      
028800            ELSE                                                          
028900              IF IN-FLSALDOCH-USA = 'J' OR IN-FLSALDOCH-CAN = 'J'         
029000                PERFORM BB-SALDOCHECK                                     
029100              END-IF                                                      
029200            END-IF                                                        
029300                                                                          
029400            IF IN-KDERS = 22                                              
029500              PERFORM BH-TILLK-SALDO                                      
029600*                                                                         
029700* FÖR ERSKOD = 22 SKA ARTIKELN ERSÄTTAS UTE PÅ RESP. NDC                  
029800* VID FÖRSTA INLEVERANS DVS HAR FÅTT SALDO                                
029900* SÅ LÄNGE SALDO EJ FINNS SÄTTS WS-FLSALDO-XXX TILL JA                    
030000* FÖR ATT EJ BEARBETAS SENARE I PROGRAMMET                                
030100*                                                                         
030200              IF WS-FLTILLK-SALDO-USA = 'N'                               
030300                MOVE JA      TO WS-FLSALDO-USA                            
030400              END-IF                                                      
030500              IF WS-FLTILLK-SALDO-CAN = 'N'                               
030600                MOVE JA      TO WS-FLSALDO-CAN                            
030700              END-IF                                                      
030800            END-IF                                                        
030900                                                                          
031000            IF IN-FLERSATT-USA = 'N' OR IN-FLERSATT-CAN = 'N'             
031100              IF WS-FLSALDO-USA = 'N' OR WS-FLSALDO-CAN = 'N'             
031200                PERFORM BC-SKAPA-FIL                                      
031300              END-IF                                                      
031400            END-IF                                                        
031500                                                                          
031600******* SÄTTER FLAGGOR TILL W11635-FILEN                                  
031700            IF IN-FLERSATT-USA = 'N'                                      
031800              IF WS-FLSALDO-USA = 'N'                                     
031900                MOVE 'J' TO WS-FLERSATT-USA                               
032000              ELSE                                                        
032100                MOVE 'N' TO WS-FLERSATT-USA                               
032200              END-IF                                                      
032300            ELSE                                                          
032400              MOVE 'J'   TO WS-FLERSATT-USA                               
032500            END-IF                                                        
032600                                                                          
032700            IF IN-FLERSATT-CAN = 'N'                                      
032800              IF WS-FLSALDO-CAN = 'N'                                     
032900                MOVE 'J' TO WS-FLERSATT-CAN                               
033000              ELSE                                                        
033100                MOVE 'N' TO WS-FLERSATT-CAN                               
033200              END-IF                                                      
033300            ELSE                                                          
033400              MOVE 'J'   TO WS-FLERSATT-CAN                               
033500            END-IF                                                        
033600                                                                          
033700         ELSE                                                             
033800            MOVE 'N' TO WS-FLERSATT-USA                                   
033900                        WS-FLERSATT-CAN                                   
034300                        FL-USA-FINNS                                      
034310                        FL-DC51-FINNS                                     
034400                                                                          
034500            PERFORM IMS-GET-ARTS-ARTS                                     
034600            IF SEGMENT-FINNS                                              
034610               MOVE 1 TO NDC-US-IX                                        
034620               PERFORM UNTIL NDC-US-IX > NDC-US-IX-MAX                    
034630                          OR WS-NDC-US(NDC-US-IX) = SPACE                 
034700                  MOVE WS-NDC-US(NDC-US-IX) TO W-IDDC                     
034710                  MOVE NEJ TO WS-NDC-US-FINNS(NDC-US-IX)                  
034800                  PERFORM IMS-GET-ARTS-SLAG                               
034900                  IF SEGMENT-FINNS                                        
034910                     IF ARTS-SLAG-KVLS > ZERO                             
034930                        IF ARTS-SLAG-ADLAGOMR > ZERO OR                   
034940                           ARTS-SLAG-ADGANG   > ZERO OR                   
034950                           ARTS-SLAG-ADPLATS  > ZERO                      
035000                           MOVE JA TO WS-NDC-US-FINNS(NDC-US-IX)          
035001                           MOVE JA TO FL-USA-FINNS                        
035010                        END-IF                                            
035020                     END-IF                                               
035030                  END-IF                                                  
035100                  ADD 1 TO NDC-US-IX                                      
035110               END-PERFORM                                                
036400                                                                          
036410               PERFORM IMS-GET-ARTS-ARTS                                  
036500               MOVE WC-NDC-CA TO W-IDDC                                   
036600               PERFORM IMS-GET-ARTS-SLAG                                  
036700               IF SEGMENT-FINNS                                           
036710                 IF ARTS-SLAG-KVLS > ZERO                                 
036730                   IF ARTS-SLAG-ADLAGOMR > ZERO OR                        
036740                      ARTS-SLAG-ADGANG   > ZERO OR                        
036750                      ARTS-SLAG-ADPLATS  > ZERO                           
036800                     MOVE JA TO FL-DC51-FINNS                             
036900                   END-IF                                                 
036910                 END-IF                                                   
036920               END-IF                                                     
037000            END-IF                                                        
037100                                                                          
037200                                                                          
037300            IF FL-USA-FINNS = JA                                          
037410            OR FL-DC51-FINNS = JA                                         
037500               IF IN-FLSALDOCH-USA = 'J' OR                               
037600                  IN-FLSALDOCH-CAN = 'J'                                  
037700                  PERFORM BG-SKAPA-EV-PREL-FIL                            
037800               END-IF                                                     
037900            END-IF                                                        
038000         END-IF                                                           
038100                                                                          
038200         PERFORM BD-SKAPA-W11635                                          
038300       END-IF                                                             
038400     .                                                                    
038500     EJECT                                                                
038600 BA-SKAPA-BACKTRANS SECTION.                                              
038610     MOVE 'BA-SKAPA-BACKTRANS '  TO CURRENT-SECTION                       
038700                                                                          
038800     MOVE IN-IDARTNR      TO WS-IDARTNR-NUM                               
038900     MOVE WS-IDARTNR-NUM  TO WS-IDARTNR-ALFA                              
039000     INSPECT WS-IDARTNR-ALFA REPLACING LEADING ZERO BY SPACE              
039100     MOVE WS-IDARTNR      TO UT2-IDARTNR20                                
039200                                                                          
039300     PERFORM IMS-GET-ARTC-ARTC                                            
039400                                                                          
039500     IF SEGMENT-FINNS                                                     
039600       MOVE ARTC-ART-TIERSDAT    TO DAT-I-TIDATUM                         
039700       MOVE 'AAVVD'              TO DAT-KDDATFORM                         
039800                                                                          
039900       PERFORM S02-WDATKONV                                               
040000                                                                          
040100       IF DAT-KDSVAR-OK                                                   
040200         MOVE DAT-TIAAMMDD TO UT2-TIERSDAT-002                            
040300       ELSE                                                               
040400         MOVE 0            TO UT2-TIERSDAT-002                            
040500       END-IF                                                             
040600     ELSE                                                                 
040700       MOVE 0              TO UT2-TIERSDAT-002                            
040800     END-IF                                                               
040900                                                                          
041000     MOVE 'Y'             TO UT2-FLDEL                                    
041100     MOVE IN-KDERS        TO UT2-KDERS                                    
041200     MOVE 0               TO UT2-IDKORTNR                                 
041300     MOVE 0               TO UT2-KDTEXTGR                                 
041400     MOVE 'N'             TO UT2-FLTEXT                                   
041500     MOVE SPACE           TO UT2-IDARTNR20-TILLK                          
041600     MOVE 0               TO UT2-DIERS-TILLK                              
041700     MOVE 0               TO UT2-KDARTUTG                                 
041800     MOVE SPACE           TO UT2-KDUTGSTA                                 
041900     MOVE SPACE           TO UT2-KDUTGSTR                                 
042000     MOVE 0               TO UT2-KDPRODSL                                 
042100                                                                          
042200     IF IN-FLERSATT-USA = 'J'                                             
042300        PERFORM S12-SKRIV-W11636                                          
042310        MOVE 'BAC'      TO UT4-IDPTYP                                     
042311        MOVE WC-LAND-US TO UT4-IDLANDX2                                   
042320        PERFORM S15-SKRIV-W1163D                                          
042400     ELSE                                                                 
042500        PERFORM IMS-GET-ARTS-ARTS                                         
042600        IF SEGMENT-FINNS                                                  
042610           MOVE 1 TO NDC-US-IX                                            
042620           PERFORM UNTIL NDC-US-IX > NDC-US-IX-MAX                        
042700              MOVE WS-NDC-US (NDC-US-IX) TO W-IDDC                        
042800              PERFORM IMS-GET-ARTS-SLAG                                   
042900              IF SEGMENT-FINNS                                            
043000                 PERFORM S12-SKRIV-W11636                                 
043001                 MOVE 'BAC'      TO UT4-IDPTYP                            
043002                 MOVE WC-LAND-US TO UT4-IDLANDX2                          
043003                 PERFORM S15-SKRIV-W1163D                                 
043004                                                                          
043010                 MOVE NDC-US-IX-MAX TO NDC-US-IX                          
043020              END-IF                                                      
043030              ADD 1 TO NDC-US-IX                                          
043040           END-PERFORM                                                    
044400        END-IF                                                            
044500     END-IF                                                               
044600                                                                          
044700     IF IN-FLERSATT-CAN = 'J'                                             
044800        PERFORM S13-SKRIV-W11637                                          
044810        MOVE 'BAC'      TO UT4-IDPTYP                                     
044811        MOVE WC-LAND-CA TO UT4-IDLANDX2                                   
044820        PERFORM S15-SKRIV-W1163D                                          
044900     ELSE                                                                 
045000        PERFORM IMS-GET-ARTS-ARTS                                         
045100        IF SEGMENT-FINNS                                                  
045200           MOVE WC-NDC-CA TO W-IDDC                                       
045300           PERFORM IMS-GET-ARTS-SLAG                                      
045400           IF SEGMENT-FINNS                                               
045500             PERFORM S13-SKRIV-W11637                                     
045510             MOVE 'BAC'      TO UT4-IDPTYP                                
045511             MOVE WC-LAND-CA TO UT4-IDLANDX2                              
045520             PERFORM S15-SKRIV-W1163D                                     
045600           END-IF                                                         
045700        END-IF                                                            
045800     END-IF                                                               
045900     .                                                                    
046000     EJECT                                                                
046100 BB-SALDOCHECK SECTION.                                                   
046110     MOVE 'BB-SALDOCHECK '  TO CURRENT-SECTION                            
046200                                                                          
046300     MOVE NEJ TO FL-USA-FINNS                                             
046610     MOVE NEJ TO FL-DC51-FINNS                                            
046700     PERFORM IMS-GET-ARTS-ARTS                                            
046800     IF SEGMENT-FINNS                                                     
046900       IF IN-FLSALDOCH-USA = 'J'                                          
046910          MOVE 1 TO NDC-US-IX                                             
046920          PERFORM UNTIL NDC-US-IX > NDC-US-IX-MAX                         
046930                     OR WS-NDC-US(NDC-US-IX) = SPACE                      
046940             MOVE WS-NDC-US(NDC-US-IX) TO W-IDDC                          
046950             MOVE NEJ TO WS-NDC-US-FINNS(NDC-US-IX)                       
046960             PERFORM IMS-GET-ARTS-SLAG                                    
046970             IF SEGMENT-FINNS                                             
046972                MOVE JA TO FL-USA-FINNS                                   
046980                IF ARTS-SLAG-KVLS > ZERO                                  
046990                   IF ARTS-SLAG-ADLAGOMR > ZERO OR                        
046991                      ARTS-SLAG-ADGANG   > ZERO OR                        
046992                      ARTS-SLAG-ADPLATS  > ZERO                           
046994                      MOVE JA TO WS-NDC-US-FINNS(NDC-US-IX)               
046995                   END-IF                                                 
046996                END-IF                                                    
046997                MOVE ARTS-SLAG-KVAKS-SDC TO WS-KVAKS-SDC                  
046998                IF ARTS-SLAG-KVAKS-SDC > 0                                
046999                   PERFORM BBA-KOLLA-KVAKS-SDC                            
047000                END-IF                                                    
047001                IF WS-KVAKS-SDC > ZERO                                    
047002                   MOVE 'J' TO WS-FLSALDO-USA                             
047004                ELSE                                                      
047005                   IF ARTS-SLAG-KVLS + ARTS-SLAG-KVAKS-PAV +              
047006                      ARTS-SLAG-KVBEART > 0                               
047007                      MOVE 'J' TO WS-FLSALDO-USA                          
047009                   END-IF                                                 
047010                END-IF                                                    
047011             END-IF                                                       
047012             ADD 1 TO NDC-US-IX                                           
047013          END-PERFORM                                                     
047020                                                                          
052500** OM INGET SALDO HITTATS I USA KOLLAS OM ARTIKELN ÖVERHUVUDTAGET         
052600** ÄR REGISTRERAD PÅ WDK711-USA-LAGER.                                    
052700** I ANNAT FALL BEHANDLAS ARTIKELN SOM OM LAGERSALDO EXISTERAT            
052800         IF WS-FLSALDO-USA = NEJ AND                                      
052900            FL-USA-FINNS = NEJ                                            
053000             MOVE JA TO WS-FLSALDO-USA                                    
053100         END-IF                                                           
053200       END-IF                                                             
053300                                                                          
053400       IF IN-FLSALDOCH-CAN = 'J'                                          
053420         PERFORM IMS-GET-ARTS-ARTS                                        
053500         MOVE WC-NDC-CA TO W-IDDC                                         
053600         PERFORM IMS-GET-ARTS-SLAG                                        
053700         IF SEGMENT-FINNS                                                 
053720           IF ARTS-SLAG-KVLS > ZERO                                       
053731             IF ARTS-SLAG-ADLAGOMR > ZERO OR                              
053732                ARTS-SLAG-ADGANG   > ZERO OR                              
053733                ARTS-SLAG-ADPLATS  > ZERO                                 
053740               MOVE JA  TO FL-DC51-FINNS                                  
053750             END-IF                                                       
053760           END-IF                                                         
053800                                                                          
053900             PERFORM BBB-KOLL-SALDO-CAN                                   
054000                                                                          
055000         ELSE                                                             
055100           MOVE 'J'   TO WS-FLSALDO-CAN                                   
055200         END-IF                                                           
055300       END-IF                                                             
055400     ELSE                                                                 
055500       MOVE 'J'   TO WS-FLSALDO-USA                                       
055600*      MOVE 'J'   TO WS-FLSALDO-CAN                                       
055700     END-IF                                                               
055800     .                                                                    
055900     EJECT                                                                
056000 BBA-KOLLA-KVAKS-SDC SECTION.                                             
056010     MOVE 'BBA-KOLLA-KVAKS-SDC '  TO CURRENT-SECTION                      
056100                                                                          
056200     PERFORM IMS-GU-WDL601                                                
056300     IF SEGMENT-FINNS                                                     
056310        MOVE '310' TO W-IDPTYP                                            
056400        PERFORM IMS-GNP-WDL611                                            
056500        PERFORM UNTIL SEGMENT-SAKNAS                                      
056600           IF INL-KDRT = 07                                               
056700              COMPUTE WS-RETUR = INL-KVAVIS - INL-KVANTMOT                
056800              SUBTRACT WS-RETUR FROM WS-KVAKS-SDC                         
056900           END-IF                                                         
057000           PERFORM IMS-GNP-WDL611                                         
057100        END-PERFORM                                                       
057200     END-IF                                                               
057300     .                                                                    
057400     EJECT                                                                
057500 BBB-KOLL-SALDO-CAN SECTION.                                              
057510     MOVE 'BBB-KOLL-SALDO-CAN '  TO CURRENT-SECTION                       
057600                                                                          
057602     PERFORM IMS-GET-ARTS-ARTS                                            
057603     IF SEGMENT-FINNS                                                     
057604         MOVE WC-NDC-CA    TO W-IDDC                                      
057605         PERFORM IMS-GET-ARTS-SLAG                                        
057606         IF SEGMENT-FINNS                                                 
057607           MOVE ARTS-SLAG-KVAKS-SDC TO WS-KVAKS-SDC                       
057608           IF ARTS-SLAG-KVAKS-SDC > 0                                     
057609             PERFORM BBA-KOLLA-KVAKS-SDC                                  
057610           END-IF                                                         
057611           IF WS-KVAKS-SDC > 0                                            
057612                MOVE 'J' TO WS-FLSALDO-CAN                                
057613           ELSE                                                           
057614              IF ARTS-SLAG-KVLS + ARTS-SLAG-KVAKS-PAV +                   
057615                          ARTS-SLAG-KVBEART > 0                           
057616                MOVE 'J' TO WS-FLSALDO-CAN                                
057617              END-IF                                                      
057618           END-IF                                                         
057619         END-IF                                                           
057652     ELSE                                                                 
057653       MOVE 'J'   TO WS-FLSALDO-CAN                                       
057654** OM INGET SALDO HITTATS I USA KOLLAS OM ARTIKELN ÖVERHUVUDTAGET         
057655** ÄR REGISTRERAD PÅ WDK711-USA-LAGER.                                    
057656** I ANNAT FALL BEHANDLAS ARTIKELN SOM OM LAGERSALDO EXISTERAT            
057657*        IF WS-FLSALDO-USA = NEJ AND                                      
057658*           FL-USA-FINNS = NEJ                                            
057659*            MOVE JA TO WS-FLSALDO-CAN                                    
057660*        END-IF                                                           
057661     END-IF                                                               
057662     .                                                                    
057663     EJECT                                                                
057664 BC-SKAPA-FIL SECTION.                                                    
057666     MOVE 'BC-SKAPA-FIL '  TO CURRENT-SECTION                             
057667                                                                          
057700     MOVE IN-IDARTNR      TO WS-IDARTNR-NUM                               
057800     MOVE WS-IDARTNR-NUM  TO WS-IDARTNR-ALFA                              
057900     INSPECT WS-IDARTNR-ALFA REPLACING LEADING ZERO BY SPACE              
058000     MOVE WS-IDARTNR      TO UT2-IDARTNR20                                
058200     PERFORM IMS-GET-ARTC-ARTC                                            
058300                                                                          
058400     IF SEGMENT-FINNS                                                     
058500       MOVE ARTC-ART-TIERSDAT    TO DAT-I-TIDATUM                         
058600       MOVE 'AAVVD'              TO DAT-KDDATFORM                         
058700                                                                          
058800       PERFORM S02-WDATKONV                                               
058900                                                                          
059000       IF DAT-KDSVAR-OK                                                   
059100         MOVE DAT-TIAAMMDD TO UT2-TIERSDAT-002                            
059200       ELSE                                                               
059300         MOVE 0            TO UT2-TIERSDAT-002                            
059400       END-IF                                                             
059500                                                                          
059600       MOVE ARTC-ART-KDPRODSL  TO UT2-KDPRODSL                            
059700     ELSE                                                                 
059800       MOVE 0             TO UT2-TIERSDAT-002                             
059900       MOVE 0             TO UT2-KDPRODSL                                 
060000     END-IF                                                               
060100                                                                          
060200     MOVE 'N'             TO UT2-FLDEL                                    
060300     MOVE IN-KDERS        TO UT2-KDERS                                    
060400     IF WS-KDERSPOS2 = '1' OR '3' OR '4' OR '6' OR '9' OR                 
060500                       '7' OR '8'                                         
060600       MOVE 1  TO UT2-KDARTUTG                                            
060700     ELSE                                                                 
060800       IF WS-KDERSPOS2 = '2' OR '5'                                       
060900         MOVE 2  TO UT2-KDARTUTG                                          
061000       END-IF                                                             
061100     END-IF                                                               
061200     MOVE 'F'    TO UT2-KDUTGSTA                                          
061300                                                                          
061310     IF IN-KDERS = 29 OR 52                                               
061320        MOVE 0        TO UT2-IDKORTNR                                     
061330        MOVE 0        TO UT2-KDTEXTGR                                     
061340        MOVE 'N'      TO UT2-FLTEXT                                       
061350        MOVE SPACE    TO UT2-IDARTNR20-TILLK                              
061360        MOVE 0        TO UT2-DIERS-TILLK                                  
061370        MOVE SPACE    TO UT2-KDUTGSTR                                     
061380                                                                          
061390        IF IN-FLERSATT-USA = 'N'                                          
061391           IF WS-FLSALDO-USA = 'N'                                        
061393              PERFORM S12-SKRIV-W11636                                    
061394              MOVE 'ERS'      TO UT4-IDPTYP                               
061395              MOVE WC-LAND-US TO UT4-IDLANDX2                             
061396              PERFORM S15-SKRIV-W1163D                                    
061397              PERFORM BF-SKAPA-INVFIL                                     
061398           END-IF                                                         
061399        END-IF                                                            
061400                                                                          
061401        IF IN-FLERSATT-CAN = 'N'                                          
061402           IF WS-FLSALDO-CAN = 'N'                                        
061403              PERFORM S13-SKRIV-W11637                                    
061404              MOVE 'ERS'      TO UT4-IDPTYP                               
061405              MOVE WC-LAND-CA TO UT4-IDLANDX2                             
061406              PERFORM S15-SKRIV-W1163D                                    
061407              IF FL-DC51-FINNS = 'J'                                      
061408                MOVE WC-NDC-CA TO UT3-IDDC                                
061409                PERFORM S14-SKRIV-W11638                                  
061410              END-IF                                                      
061411           END-IF                                                         
061412        END-IF                                                            
061414     ELSE                                                                 
061420        PERFORM IMS-GET-ERSA-WLERSA                                       
061500        IF SEGMENT-FINNS                                                  
061600           IF WS-KDERSPOS2 = '1' OR '2' OR '3' OR '7'                     
061700              IF ERSA-KVKORT = 1                                          
061800                 MOVE 'S'   TO UT2-KDUTGSTR                               
061900              ELSE                                                        
062000                 MOVE 'M'   TO UT2-KDUTGSTR                               
062100              END-IF                                                      
062200           ELSE                                                           
062300              IF WS-KDERSPOS2 = '4' OR '5' OR '6' OR '8'                  
062400                 MOVE 'V'   TO UT2-KDUTGSTR                               
062500              END-IF                                                      
062600           END-IF                                                         
062700                                                                          
062800                                                                          
062900           PERFORM IMS-GET-ERSA-ERSA11                                    
063000                                                                          
063100           IF SEGMENT-FINNS                                               
063200              MOVE 1        TO WS-KDTEXTGR-RAKNARE                        
063300              MOVE 'N'      TO WS-FL-TYP1                                 
063400                                                                          
063500              PERFORM UNTIL SEGMENT-SAKNAS                                
063600                                                                          
063700                MOVE ERSA-IDKORTNR   TO UT2-IDKORTNR                      
063800                                                                          
063900                IF WS-KDERSPOS2 = '1' OR '2' OR '3' OR '9' OR '7'         
064000                   MOVE 0        TO UT2-KDTEXTGR                          
064100                ELSE                                                      
064200                   IF ERSA-FLTEXT = 'N'                                   
064300                      MOVE 'J'  TO WS-FL-TYP1                             
064400                   END-IF                                                 
064500                   IF ERSA-FLTEXT = 'J' AND WS-FL-TYP1 = 'J'              
064600                      ADD  +1   TO WS-KDTEXTGR-RAKNARE                    
064700                      MOVE 'N'  TO WS-FL-TYP1                             
064800                   END-IF                                                 
064900                   MOVE WS-KDTEXTGR-RAKNARE TO UT2-KDTEXTGR               
065000                END-IF                                                    
065100                                                                          
065200                MOVE ERSA-FLTEXT     TO UT2-FLTEXT                        
065300                IF ERSA-FLTEXT = 'N'                                      
065400                   MOVE ERSA-IDARTNR-TILLK  TO WS-IDARTNR-NUM             
065510                   MOVE WS-IDARTNR-NUM   TO WS-IDARTNR-TILLK-ALFA         
065600                   INSPECT WS-IDARTNR-TILLK-ALFA                          
065700                                 REPLACING LEADING ZERO BY SPACE          
065810                   MOVE WS-IDARTNR-TILLK TO UT2-IDARTNR20-TILLK           
065910                   MOVE ERSA-DIERS-TILLK TO UT2-DIERS-TILLK               
066000                ELSE                                                      
066100                   IF ERSA-FLTEXT = 'J'                                   
066200                      MOVE ERSA-BEERS  TO UT2-IDARTNR20-TILLK             
066300                      MOVE  0          TO UT2-DIERS-TILLK                 
066400                   END-IF                                                 
066500                END-IF                                                    
066600                                                                          
066620                IF IN-FLERSATT-USA = 'N'                                  
066630                   IF WS-FLSALDO-USA = 'N'                                
066640                      PERFORM S12-SKRIV-W11636                            
066650                      MOVE 'ERS'      TO UT4-IDPTYP                       
066660                      MOVE WC-LAND-US TO UT4-IDLANDX2                     
066670                      PERFORM S15-SKRIV-W1163D                            
066680                   END-IF                                                 
066690                END-IF                                                    
066700                                                                          
066800                IF IN-FLERSATT-CAN = 'N'                                  
066900                   IF WS-FLSALDO-CAN = 'N'                                
067000                      PERFORM S13-SKRIV-W11637                            
067100                      MOVE 'ERS'      TO UT4-IDPTYP                       
067511                      MOVE WC-LAND-CA TO UT4-IDLANDX2                     
067520                      PERFORM S15-SKRIV-W1163D                            
067600                   END-IF                                                 
067700                END-IF                                                    
067800                                                                          
067900                PERFORM IMS-GET-ERSA-ERSA11                               
068000                                                                          
068100              END-PERFORM                                                 
068200                                                                          
068300              IF IN-FLERSATT-USA = 'N'                                    
068400                 IF WS-FLSALDO-USA = 'N'                                  
068500                    PERFORM BF-SKAPA-INVFIL                               
068600                 END-IF                                                   
068700              END-IF                                                      
068800                                                                          
068900              IF IN-FLERSATT-CAN = 'N'                                    
069000                 IF WS-FLSALDO-CAN = 'N'                                  
069110                    IF FL-DC51-FINNS = 'J'                                
069120                      MOVE WC-NDC-CA TO UT3-IDDC                          
069200                      PERFORM S14-SKRIV-W11638                            
069210                    END-IF                                                
069300                 END-IF                                                   
069400              END-IF                                                      
069500                                                                          
069600           END-IF                                                         
072100        END-IF                                                            
072200     END-IF                                                               
072300     .                                                                    
072400     EJECT                                                                
072500 BD-SKAPA-W11635 SECTION.                                                 
072510     MOVE 'BD-SKAPA-W11635 '  TO CURRENT-SECTION                          
072600                                                                          
072700     MOVE IN-IDARTNR       TO UT1-IDARTNR                                 
072800     MOVE IN-KDERS         TO UT1-KDERS                                   
072900     MOVE WS-FLERSATT-USA  TO UT1-FLERSATT-USA                            
073000     MOVE WS-FLERSATT-CAN  TO UT1-FLERSATT-CAN                            
073100     PERFORM S11-SKRIV-W11635                                             
073200     .                                                                    
073300     EJECT                                                                
073400 BE-KOLLA-FINNS SECTION.                                                  
073410     MOVE 'BE-KOLLA-FINNS '  TO CURRENT-SECTION                           
073500                                                                          
073600     MOVE 'J' TO WS-FLSALDO-USA                                           
073700     MOVE 'J' TO WS-FLSALDO-CAN                                           
074010     MOVE 'N' TO FL-DC51-FINNS                                            
074100     PERFORM IMS-GET-ARTS-ARTS                                            
074200     IF SEGMENT-FINNS                                                     
074210        MOVE 1   TO NDC-US-IX                                             
074220        PERFORM UNTIL NDC-US-IX > NDC-US-IX-MAX                           
074230                   OR WS-NDC-US(NDC-US-IX) = SPACE                        
074240           MOVE 'N' TO WS-NDC-US-FINNS(NDC-US-IX)                         
074300           MOVE WS-NDC-US(NDC-US-IX) TO W-IDDC                            
074400           PERFORM IMS-GET-ARTS-SLAG                                      
074500           IF SEGMENT-FINNS                                               
074600              MOVE 'N' TO WS-FLSALDO-USA                                  
074700**** ADD CHECK FOR EACH DC                                                
074701              IF ARTS-SLAG-ADLAGOMR = ZERO                                
074702              AND ARTS-SLAG-KVLS = ZERO                                   
074703                 CONTINUE                                                 
074704              ELSE                                                        
074706                 MOVE 'J' TO WS-NDC-US-FINNS(NDC-US-IX)                   
074707              END-IF                                                      
074708           END-IF                                                         
074709           ADD 1 TO NDC-US-IX                                             
074710        END-PERFORM                                                       
074800                                                                          
074900        PERFORM IMS-GET-ARTS-ARTS                                         
076200        MOVE WC-NDC-CA      TO W-IDDC                                     
076300        PERFORM IMS-GET-ARTS-SLAG                                         
076400        IF SEGMENT-FINNS                                                  
076500           MOVE 'N'   TO WS-FLSALDO-CAN                                   
076510           IF ARTS-SLAG-ADLAGOMR = ZERO                                   
076520           AND ARTS-SLAG-KVLS = ZERO                                      
076530              CONTINUE                                                    
076540           ELSE                                                           
076550              MOVE 'J' TO FL-DC51-FINNS                                   
076560           END-IF                                                         
076600        END-IF                                                            
076700     END-IF                                                               
076800     .                                                                    
076900     EJECT                                                                
076910                                                                          
077000 BF-SKAPA-INVFIL SECTION.                                                 
077010     MOVE 'BF-SKAPA-INVFIL '  TO CURRENT-SECTION                          
077100                                                                          
077110     MOVE 1 TO NDC-US-IX                                                  
077120     PERFORM UNTIL NDC-US-IX > NDC-US-IX-MAX                              
077130                OR WS-NDC-US(NDC-US-IX) = SPACE                           
077200        IF WS-NDC-US-FINNS(NDC-US-IX) = 'J'                               
077300            MOVE WS-NDC-US(NDC-US-IX) TO UT3-IDDC                         
077400            PERFORM S14-SKRIV-W11638                                      
077500        END-IF                                                            
077510        ADD 1 TO NDC-US-IX                                                
077520     END-PERFORM                                                          
078400     .                                                                    
078500     EJECT                                                                
078510                                                                          
078600 BG-SKAPA-EV-PREL-FIL SECTION.                                            
078700     MOVE 'BG-SKAPA-EV-PREL-FIL '  TO CURRENT-SECTION                     
078710                                                                          
078800     MOVE IN-IDARTNR      TO WS-IDARTNR-NUM                               
078900     MOVE WS-IDARTNR-NUM  TO WS-IDARTNR-ALFA                              
079000     INSPECT WS-IDARTNR-ALFA REPLACING LEADING ZERO BY SPACE              
079100     MOVE WS-IDARTNR      TO UT2-IDARTNR20                                
079200                                                                          
079300     PERFORM IMS-GET-ARTC-ARTC                                            
079400     IF SEGMENT-FINNS                                                     
079500        MOVE ARTC-ART-KDPRODSL TO UT2-KDPRODSL                            
079600        MOVE 0                 TO UT2-TIERSDAT-002                        
079700     ELSE                                                                 
079800        MOVE 0 TO UT2-TIERSDAT-002                                        
079900        MOVE 0 TO UT2-KDPRODSL                                            
080000     END-IF                                                               
080100                                                                          
080200     MOVE 'N'             TO UT2-FLDEL                                    
080300     MOVE IN-KDERS        TO UT2-KDERS                                    
080400     IF WS-KDERSPOS2 = '1' OR '3' OR '4' OR '6' OR '9' OR                 
080500                       '7' OR '8'                                         
080600        MOVE 1 TO UT2-KDARTUTG                                            
080700     ELSE                                                                 
080800        IF WS-KDERSPOS2 = '2' OR '5'                                      
080900           MOVE 2 TO UT2-KDARTUTG                                         
081000        END-IF                                                            
081100     END-IF                                                               
081200                                                                          
081300     MOVE 'P' TO UT2-KDUTGSTA                                             
081400                                                                          
081500     PERFORM IMS-GET-ERSA-WLERSA                                          
081600     IF SEGMENT-FINNS                                                     
081700        IF WS-KDERSPOS2 = '1' OR '2' OR '3' OR '7'                        
081800           IF ERSA-KVKORT = 1                                             
081900              MOVE 'S'   TO UT2-KDUTGSTR                                  
082000           ELSE                                                           
082100              MOVE 'M'   TO UT2-KDUTGSTR                                  
082200           END-IF                                                         
082300        ELSE                                                              
082400           IF WS-KDERSPOS2 = '4' OR '5' OR '6' OR '8'                     
082500              MOVE 'V'   TO UT2-KDUTGSTR                                  
082600           END-IF                                                         
082700        END-IF                                                            
082800                                                                          
082900        PERFORM IMS-GET-ERSA-ERSA13                                       
083000        IF SEGMENT-FINNS                                                  
083100           IF IN-KDERS > 20                                               
083200              CONTINUE                                                    
083300           ELSE                                                           
083400              IF IN-KDERS > 10 AND < 20                                   
083500                 MOVE ERSA-TIERSDAT-PREL-C1 TO DAT-I-TIDATUM              
083600                 MOVE 'AAVVD' TO DAT-KDDATFORM                            
083700                 PERFORM S02-WDATKONV                                     
083800                 IF DAT-KDSVAR-OK                                         
083900                    MOVE DAT-TIAAMMDD TO UT2-TIERSDAT-002                 
084000                 ELSE                                                     
084100                    MOVE 0            TO UT2-TIERSDAT-002                 
084200                 END-IF                                                   
084300              ELSE                                                        
084400                 IF IN-KDERS > 0 AND < 10                                 
084500                    MOVE ERSA-TIERSDAT-REG TO DAT-I-TIDATUM               
084600                    MOVE 'AAVVD'      TO DAT-KDDATFORM                    
084700                    PERFORM S02-WDATKONV                                  
084800                    IF DAT-KDSVAR-OK                                      
084900                       MOVE DAT-TIAAMMDD TO UT2-TIERSDAT-002              
085000                    ELSE                                                  
085100                       MOVE 0            TO UT2-TIERSDAT-002              
085200                    END-IF                                                
085300                 END-IF                                                   
085400              END-IF                                                      
085500           END-IF                                                         
085600        END-IF                                                            
085700                                                                          
085800        PERFORM IMS-GET-ERSA-ERSA11-FIRST                                 
085900                                                                          
086000        IF SEGMENT-FINNS                                                  
086100           MOVE 1     TO WS-KDTEXTGR-RAKNARE                              
086200           MOVE 'N'   TO WS-FL-TYP1                                       
086300                                                                          
086400           PERFORM UNTIL SEGMENT-SAKNAS                                   
086500              MOVE ERSA-IDKORTNR   TO UT2-IDKORTNR                        
086600              IF WS-KDERSPOS2 = '1' OR '2' OR '3' OR '9' OR '7'           
086700                 MOVE 0        TO UT2-KDTEXTGR                            
086800              ELSE                                                        
086900                 IF ERSA-FLTEXT = 'N'                                     
087000                    MOVE 'J'  TO WS-FL-TYP1                               
087100                 END-IF                                                   
087200                 IF ERSA-FLTEXT = 'J' AND WS-FL-TYP1 = 'J'                
087300                    ADD  +1   TO WS-KDTEXTGR-RAKNARE                      
087400                    MOVE 'N'  TO WS-FL-TYP1                               
087500                 END-IF                                                   
087600                 MOVE WS-KDTEXTGR-RAKNARE TO UT2-KDTEXTGR                 
087700              END-IF                                                      
087800                                                                          
087900              MOVE ERSA-FLTEXT     TO UT2-FLTEXT                          
088000              IF ERSA-FLTEXT = 'N'                                        
088100                 MOVE ERSA-IDARTNR-TILLK  TO WS-IDARTNR-NUM               
088200                 MOVE WS-IDARTNR-NUM      TO WS-IDARTNR-TILLK-ALFA        
088300                 INSPECT WS-IDARTNR-TILLK-ALFA                            
088400                                 REPLACING LEADING ZERO BY SPACE          
088500                 MOVE WS-IDARTNR-TILLK TO UT2-IDARTNR20-TILLK             
088600                 MOVE ERSA-DIERS-TILLK     TO UT2-DIERS-TILLK             
088700              ELSE                                                        
088800                 IF ERSA-FLTEXT = 'J'                                     
088900                    MOVE ERSA-BEERS  TO UT2-IDARTNR20-TILLK               
089000                    MOVE  0     TO UT2-DIERS-TILLK                        
089100                 END-IF                                                   
089200              END-IF                                                      
089300                                                                          
089400              IF FL-USA-FINNS = JA                                        
089600                 PERFORM S12-SKRIV-W11636                                 
089700              END-IF                                                      
089800                                                                          
089900              IF FL-DC51-FINNS = JA                                       
090000                 PERFORM S13-SKRIV-W11637                                 
090100              END-IF                                                      
090200                                                                          
090300              PERFORM IMS-GET-ERSA-ERSA11                                 
090400           END-PERFORM                                                    
090500                                                                          
090600        ELSE                                                              
090700           IF IN-KDERS = 09 OR 19                                         
090800              MOVE 0        TO UT2-IDKORTNR                               
090900              MOVE 0        TO UT2-KDTEXTGR                               
091000              MOVE 'N'      TO UT2-FLTEXT                                 
091100              MOVE SPACE    TO UT2-IDARTNR20-TILLK                        
091200              MOVE 0        TO UT2-DIERS-TILLK                            
091300              MOVE SPACE    TO UT2-KDUTGSTR                               
091400                                                                          
091500              IF FL-USA-FINNS = JA                                        
091700                 PERFORM S12-SKRIV-W11636                                 
091800              END-IF                                                      
091900                                                                          
092000              IF FL-DC51-FINNS = JA                                       
092100                 PERFORM S13-SKRIV-W11637                                 
092200              END-IF                                                      
092300           END-IF                                                         
092400        END-IF                                                            
092500     END-IF                                                               
092600     .                                                                    
092700     EJECT                                                                
092800 BH-TILLK-SALDO SECTION.                                                  
092810     MOVE 'BH-TILLK-SALDO '  TO CURRENT-SECTION                           
092900                                                                          
092910     MOVE ZERO TO ANTAL-ERS                                               
092920                  ANTAL-TRAFF-ERS                                         
092930     MOVE NEJ  TO SW-CAN                                                  
093000     PERFORM IMS-GET-ERSA-WLERSA                                          
093100     IF SEGMENT-FINNS                                                     
093200                                                                          
093300       PERFORM IMS-GET-ERSA-ERSA11                                        
093400                                                                          
093500       PERFORM UNTIL SEGMENT-SAKNAS                                       
093510         ADD +1 TO ANTAL-ERS                                              
093600                                                                          
093700         IF ERSA-FLTEXT = 'N'                                             
093800           MOVE ERSA-IDARTNR-TILLK    TO W-IDARTNR                        
093900           PERFORM IMS-GET-ARTS-ARTS                                      
094000           IF SEGMENT-FINNS                                               
094010              MOVE 1 TO NDC-US-IX                                         
094020              PERFORM UNTIL NDC-US-IX > NDC-US-IX-MAX                     
094030                         OR WS-NDC-US(NDC-US-IX) = SPACE                  
094100                 MOVE WS-NDC-US(NDC-US-IX) TO W-IDDC                      
094200                 PERFORM IMS-GET-ARTS-SLAG                                
094300                 IF SEGMENT-FINNS                                         
094400                    IF (ARTS-SLAG-KVLS +                                  
094500                       ARTS-SLAG-KVAKS-SDC) > 0                           
094510                       ADD +1 TO ANTAL-TRAFF-ERS                          
094710                    ELSE                                                  
094720                       PERFORM IMS-GU-WDL601                              
094721                       IF SEGMENT-FINNS                                   
094722                          MOVE 'R32' TO W-IDPTYP                          
094730                          PERFORM IMS-GNP-WDL611                          
094750                          IF SEGMENT-FINNS                                
094751                             ADD +1 TO ANTAL-TRAFF-ERS                    
094780                          END-IF                                          
094800                       END-IF                                             
094810                    END-IF                                                
094820                    MOVE NDC-US-IX-MAX TO NDC-US-IX                       
094830                 END-IF                                                   
094840                 ADD 1 TO NDC-US-IX                                       
094850              END-PERFORM                                                 
097000                                                                          
097010              PERFORM IMS-GET-ARTS-ARTS                                   
097100              MOVE WC-NDC-CA TO W-IDDC                                    
097200              PERFORM IMS-GET-ARTS-SLAG                                   
097300              IF SEGMENT-FINNS                                            
097400                 MOVE JA TO SW-CAN                                        
097800              END-IF                                                      
097900           END-IF                                                         
098000                                                                          
098100         END-IF                                                           
098200                                                                          
098300         PERFORM IMS-GET-ERSA-ERSA11                                      
098400                                                                          
098500       END-PERFORM                                                        
098510       IF ANTAL-ERS = ANTAL-TRAFF-ERS                                     
098520         MOVE JA TO WS-FLTILLK-SALDO-USA                                  
098530         IF SW-CAN = JA                                                   
098540            MOVE JA TO WS-FLTILLK-SALDO-CAN                               
098541         END-IF                                                           
098542       END-IF                                                             
098600                                                                          
098700     END-IF                                                               
098800                                                                          
098900********************************************************                  
099000*                                                                         
099100*        ÅTERSTÄLL NYCKELN                                                
099200*                                                                         
099300********************************************************                  
099400                                                                          
099500     MOVE IN-IDARTNR         TO W-IDARTNR                                 
099600     .                                                                    
099700     EJECT                                                                
099990 Z-FINIT SECTION.                                                         
100000                                                                          
100100     CLOSE W11631                                                         
100200           W11635                                                         
100300           W11636                                                         
100400           W11637                                                         
100500           W11638                                                         
100510           W1163D                                                         
100520                                                                          
100600     MOVE 'S'     TO POSTSUM-OPKOD                                        
100700     CALL POSTSUM USING POSTSUM-PARM                                      
100800     .                                                                    
100900     EJECT                                                                
101000 S01-LAES-W11631  SECTION.                                                
101100                                                                          
101200     READ W11631          INTO IN-AREA                                    
101300     AT END                                                               
101400        SET END-OF-W11631 TO TRUE                                         
101500                                                                          
101600     NOT AT END                                                           
101700        MOVE 'W11631'     TO    POSTSUM-FDNAMN                            
101800        MOVE 'W11634D1'   TO    POSTSUM-DDNAMN2                           
101900        CALL POSTSUM      USING POSTSUM-PARM                              
102000     END-READ                                                             
102100     .                                                                    
102200     EJECT                                                                
102300 S02-WDATKONV SECTION.                                                    
102400     SKIP2                                                                
102500     CALL WDATKONV USING DAT-KDDATFORM                                    
102600                         DAT-I-TIDATUM                                    
102700                         DAT-O-TIDATUM                                    
102800                         DAT-KDSVAR                                       
102900     .                                                                    
103000     EJECT                                                                
103100 S11-SKRIV-W11635 SECTION.                                                
103200                                                                          
103300     WRITE W11635-POST    FROM UT1-AREA                                   
103400                                                                          
103500     MOVE 'W11635 '       TO POSTSUM-FDNAMN                               
103600     MOVE 'W11634D2'      TO POSTSUM-DDNAMN2                              
103700     CALL POSTSUM         USING POSTSUM-PARM                              
103800     .                                                                    
103900     EJECT                                                                
104000 S12-SKRIV-W11636 SECTION.                                                
104100                                                                          
104200     MOVE 'WB1'           TO UT2-IDPTYP                                   
104300     WRITE W11636-POST    FROM UT2-AREA                                   
104400                                                                          
104500     MOVE 'W11636 '       TO POSTSUM-FDNAMN                               
104600     MOVE 'W11634D3'      TO POSTSUM-DDNAMN2                              
104700     CALL POSTSUM         USING POSTSUM-PARM                              
104800     .                                                                    
104900     EJECT                                                                
105000 S13-SKRIV-W11637 SECTION.                                                
105100                                                                          
105200     MOVE 'WB1'           TO UT2-IDPTYP                                   
105300     WRITE W11637-POST    FROM UT2-AREA                                   
105400                                                                          
105500     MOVE 'W11637 '       TO POSTSUM-FDNAMN                               
105600     MOVE 'W11634D4'      TO POSTSUM-DDNAMN2                              
105700     CALL POSTSUM         USING POSTSUM-PARM                              
105800     .                                                                    
105900     EJECT                                                                
106000 S14-SKRIV-W11638 SECTION.                                                
106200     MOVE IN-IDARTNR      TO UT3-IDARTNR                                  
106300     MOVE +5              TO UT3-KDINVKAT                                 
106400     WRITE W11638-POST    FROM UT3-AREA                                   
106500                                                                          
106600     MOVE 'W11638 '       TO POSTSUM-FDNAMN                               
106700     MOVE 'W11634D5'      TO POSTSUM-DDNAMN2                              
106800     CALL POSTSUM         USING POSTSUM-PARM                              
106900     .                                                                    
107000     EJECT                                                                
107010 S15-SKRIV-W1163D SECTION.                                                
107020                                                                          
107030     MOVE IN-IDARTNR      TO UT4-IDARTNR                                  
107050     WRITE W1163D-POST    FROM UT4-AREA                                   
107060                                                                          
107070     MOVE 'W1163D '       TO POSTSUM-FDNAMN                               
107080     MOVE 'W11634D6'      TO POSTSUM-DDNAMN2                              
107090     CALL POSTSUM         USING POSTSUM-PARM                              
107091     .                                                                    
107092     EJECT                                                                
107100* --- IMS SEKTIONER ---                                                   
107200     SKIP3                                                                
107300 IMS-GET-ARTS-ARTS SECTION.                                               
107310     MOVE 'IMS-GET-ARTS-ARTS '  TO DBS-SECTION                            
107400                                                                          
107500     STRING 'WLARTS01(IDARTNR  =' W-IDARTNR-X ')'                         
107600          DELIMITED BY SIZE INTO SSA1                                     
107700     MOVE '  GE' TO GODK-STATUSKODER                                      
107800     CALL CBLTDLI USING GU ARTS-PCB DLI-IO-WLARTS01 SSA1                  
107900     MOVE ARTS-STATUS-CODE TO STATUS-WS                                   
108000     PERFORM IMS-STATUSKONTROLL                                           
108100     .                                                                    
108200     EJECT                                                                
108300 IMS-GET-ARTS-SLAG SECTION.                                               
108310     MOVE 'IMS-GET-ARTS-SLAG '  TO DBS-SECTION                            
108400                                                                          
108500     STRING 'WLARTS11(IDDC     =' W-IDDC-X ')'                            
108600          DELIMITED BY SIZE INTO SSA1                                     
108700     MOVE '  GE' TO GODK-STATUSKODER                                      
108800     CALL CBLTDLI USING GNP ARTS-PCB DLI-IO-WLARTS11 SSA1                 
108900     MOVE ARTS-STATUS-CODE TO STATUS-WS                                   
109000     PERFORM IMS-STATUSKONTROLL                                           
109100     .                                                                    
109200     EJECT                                                                
109300 IMS-GET-ARTC-ARTC SECTION.                                               
109310     MOVE 'IMS-GET-ARTC-ARTC '  TO DBS-SECTION                            
109400                                                                          
109500     STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-X ')'                         
109600          DELIMITED BY SIZE INTO SSA1                                     
109700     MOVE '  GE' TO GODK-STATUSKODER                                      
109800     CALL CBLTDLI USING GU ARTC-PCB DLI-IO-WLARTC01 SSA1                  
109900     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
110000     PERFORM IMS-STATUSKONTROLL                                           
110100     .                                                                    
110200     EJECT                                                                
110300 IMS-GET-ERSA-WLERSA SECTION.                                             
110310     MOVE 'IMS-GET-ERSA-WLERSA '  TO DBS-SECTION                          
110400                                                                          
110500     STRING 'WLERSA01(IDARTNR  =' W-IDARTNR-X ')'                         
110600          DELIMITED BY SIZE INTO SSA1                                     
110700     MOVE '  GE' TO GODK-STATUSKODER                                      
110800     CALL CBLTDLI USING GU ERSA-PCB DLI-IO-WLERSA01 SSA1                  
110900     MOVE ERSA-STATUS-CODE TO STATUS-WS                                   
111000     PERFORM IMS-STATUSKONTROLL                                           
111100     .                                                                    
111200     SKIP3                                                                
111300 IMS-GET-ERSA-ERSA11-FIRST SECTION.                                       
111310     MOVE 'IMS-GET-ERSA-ERSA11-FIRST '  TO DBS-SECTION                    
111400                                                                          
111500     MOVE 'WLERSA11*F' TO SSA1                                            
111600     MOVE '  GE' TO GODK-STATUSKODER                                      
111700     CALL CBLTDLI USING GNP ERSA-PCB DLI-IO-WLERSA11 SSA1                 
111800     MOVE ERSA-STATUS-CODE TO STATUS-WS                                   
111900     PERFORM IMS-STATUSKONTROLL                                           
112000     .                                                                    
112100     SKIP3                                                                
112200 IMS-GET-ERSA-ERSA13 SECTION.                                             
112210     MOVE 'IMS-GET-ERSA-ERSA13 '  TO DBS-SECTION                          
112300                                                                          
112400     STRING 'WLERSA13 '                                                   
112500          DELIMITED BY SIZE INTO SSA1                                     
112600     MOVE '  GE' TO GODK-STATUSKODER                                      
112700     CALL CBLTDLI USING GNP ERSA-PCB DLI-IO-WLERSA13 SSA1                 
112800     MOVE ERSA-STATUS-CODE TO STATUS-WS                                   
112900     PERFORM IMS-STATUSKONTROLL                                           
113000     .                                                                    
113100     EJECT                                                                
113200 IMS-GET-ERSA-ERSA11 SECTION.                                             
113210     MOVE 'IMS-GET-ERSA-ERSA11 '  TO DBS-SECTION                          
113300                                                                          
113400     STRING 'WLERSA11 '                                                   
113500          DELIMITED BY SIZE INTO SSA1                                     
113600     MOVE '  GE' TO GODK-STATUSKODER                                      
113700     CALL CBLTDLI USING GNP ERSA-PCB DLI-IO-WLERSA11 SSA1                 
113800     MOVE ERSA-STATUS-CODE TO STATUS-WS                                   
113900     PERFORM IMS-STATUSKONTROLL                                           
114000     .                                                                    
114100     EJECT                                                                
114200 IMS-GU-WDL601 SECTION.                                                   
114210     MOVE 'IMS-GU-WDL601 '  TO DBS-SECTION                                
114220                                                                          
114300     STRING 'WDL601  (IDARTNR  =' W-IDARTNR-X ')'                         
114400          DELIMITED BY SIZE INTO SSA1                                     
114500     MOVE '  GEGB' TO GODK-STATUSKODER                                    
114600     CALL CBLTDLI USING GU WDL6-PCB DLI-IO-WDL601 SSA1                    
114700     MOVE WDL6-STATUS-CODE TO STATUS-WS                                   
114800     PERFORM IMS-STATUSKONTROLL                                           
114900     .                                                                    
115000     SKIP2                                                                
115100 IMS-GNP-WDL611 SECTION.                                                  
115101     MOVE 'IMS-GNP-WDL611 '  TO DBS-SECTION                               
115110                                                                          
115200     STRING 'WDL611  (IDDC     =' W-IDDC-X                                
115300                    '&IDPTYP   =' W-IDPTYP-X ')'                          
115400            DELIMITED BY SIZE INTO SSA1                                   
115500     MOVE '  GEGB' TO GODK-STATUSKODER                                    
115600     CALL CBLTDLI USING GNP WDL6-PCB DLI-IO-WDL611 SSA1                   
115700     MOVE WDL6-STATUS-CODE TO STATUS-WS                                   
115800     PERFORM IMS-STATUSKONTROLL                                           
115900     .                                                                    
116000     EJECT                                                                
116010     SKIP2                                                                
116020 IMS-GN-WDB601 SECTION.                                                   
116021     MOVE 'IMS-GN-WDB601 '  TO DBS-SECTION                                
116022                                                                          
116030     MOVE 'WDB601 '        TO SSA1                                        
116060     MOVE '  GB' TO GODK-STATUSKODER                                      
116070     CALL CBLTDLI USING GN WDB6-PCB DLI-IO-WDB601 SSA1                    
116080     MOVE WDB6-STATUS-CODE TO STATUS-WS                                   
116090     PERFORM IMS-STATUSKONTROLL                                           
116091     .                                                                    
116092     EJECT                                                                
116100 IMS-STATUSKONTROLL SECTION.                                              
116200     SKIP2                                                                
116300     SET STATUS-IX TO 1                                                   
116400     SEARCH GODK-STATUS                                                   
116500       AT END                                                             
116600         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
116700           DELIMITED BY SIZE INTO FELTEXT                                 
116800         DISPLAY FELTEXT                                                  
116900         CALL FELLOG                                                      
117000       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
117100         CONTINUE                                                         
117200     END-SEARCH                                                           
117300     .                                                                    
