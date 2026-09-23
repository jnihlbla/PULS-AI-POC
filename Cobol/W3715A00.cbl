000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W3715A00.                                                
000300 AUTHOR.         INGVAR SKJELBRED.                                        
000400 DATE-WRITTEN.   98/03/03.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNKTION:                                                            
000800*        SKAPAR UPPFÖLJNINGSFIL AV FÖRSÄLNINGSSTATISTIKEN                 
000900*        OCH AV KVITTNINGSTABELLEN OCH AV ARTIKELREGISTRET                
001000*        FÖR EN SPECIELL VECKA AV ARTIKELSTATISTIKEN                      
001100*                                                                         
001200*        PROGRAMMET LÄSER      WLXXCP (WDGX)                              
001300*                                                                         
001400*    ABENDKODER:                                                          
001500*        U0016 -  . . . .                                                 
001600*        U1000 -  . . . .                                                 
001700*                                                                         
001800                                                                          
001900     SKIP3                                                                
002000 ENVIRONMENT DIVISION.                                                    
002100     SKIP2                                                                
002200 INPUT-OUTPUT SECTION.                                                    
002300                                                                          
002400 FILE-CONTROL.                                                            
002500     SKIP2                                                                
002600*   --- ARTIKELSTATISTIKS REGISTER (FÖRSÄLJNINGSSTATISTIK) ---            
002700     SELECT INFIL                      ASSIGN TO W3715AD1.                
002800     SKIP2                                                                
002900*          --- ARTIKELREGISTRET PÅ FIL                                    
003000     SELECT W01160                     ASSIGN TO W3715AD2.                
003100     SKIP2                                                                
003200*          --- BENÄMNINGSREGISTRET PÅ FIL                                 
003300     SELECT W01174                     ASSIGN TO W3715AD3.                
003400     SKIP2                                                                
003500*          --- L                                                          
003600     SELECT W3715A                     ASSIGN TO W3715AD4.                
003700     EJECT                                                                
003800 DATA DIVISION.                                                           
003900     SKIP2                                                                
004000 FILE SECTION.                                                            
004100     SKIP3                                                                
004200 FD  INFIL                                                                
004300     LABEL RECORD   STANDARD                                              
004400     RECORDING      V                                                     
004500     BLOCK CONTAINS 0.                                                    
004600     SKIP2                                                                
004700*    -COPY W330310   -L.                                                  
004800     SKIP2                                                                
004900*    -COPY W330300   -L.                                                  
005000     SKIP2                                                                
005100*    -COPY W330320   -L.                                                  
005200     SKIP2                                                                
005300*    -COPY W330330   -L.                                                  
005400     SKIP2                                                                
005500*    -COPY W330340   -L.                                                  
005600     SKIP2                                                                
005700*    -COPY W330350   -L.                                                  
005800     EJECT                                                                
005900     SKIP3                                                                
006000 FD  W01160                                                               
006100     RECORDING       F                                                    
006200     BLOCK CONTAINS  0.                                                   
006300                                                                          
006400*01  -COPY W01160      -L.                                                
006500     SKIP3                                                                
006600 FD  W01174                                                               
006700     RECORDING       F                                                    
006800     BLOCK CONTAINS  0.                                                   
006900                                                                          
007000*01  -COPY W01174      -L.                                                
007100     SKIP3                                                                
007200 FD  W3715A                                                               
007300     RECORDING       F                                                    
007400     BLOCK CONTAINS  0.                                                   
007500                                                                          
007600*01  POST -COPY W3715A -PRE  UT-  -L.                                     
007700     EJECT                                                                
007800 WORKING-STORAGE SECTION.                                                 
007900                                                                          
008000                                                                          
008100*    -- CHECKED BY WY2000                                                 
008200 77  IDPGM                       PIC X(8)    VALUE 'W3715A00'.            
008300 77  JA                          PIC X       VALUE 'J'.                   
008400 77  NEJ                         PIC X       VALUE 'N'.                   
008500                                                                          
008600 77  POST-DATUM-SW-OK            PIC X       VALUE 'N'.                   
008700     88  POST-DATUM-OK                       VALUE 'J'.                   
008800                                                                          
008900 77  W33013-EOF-SW               PIC X       VALUE 'N'.                   
009000     88  END-OF-W33013                       VALUE 'J'.                   
009100                                                                          
009200 77  W01160-EOF-SW               PIC X       VALUE 'N'.                   
009300     88  END-OF-W01160                       VALUE 'J'.                   
009400                                                                          
009500 77  INFIL-EOF-SW                PIC X       VALUE 'N'.                   
009600     88  END-OF-INFIL                        VALUE 'J'.                   
009700                                                                          
009800 77  W01174-EOF-SW               PIC X       VALUE 'N'.                   
009900     88  END-OF-W01174                       VALUE 'J'.                   
010000                                                                          
010100 77  W-INFIL-KVPOST-IN           PIC S9(7)   VALUE ZERO COMP-3.           
010200 77  W-UTFIL-KVPOST-UT           PIC S9(7)   VALUE ZERO COMP-3.           
010300 77  W-INFIL-KVPOST-W01160       PIC S9(7)   VALUE ZERO COMP-3.           
010400 77  W-INFIL-KVPOST-W01174       PIC S9(7)   VALUE ZERO COMP-3.           
010500 77  W-ANTAL-DATABAS-LAS         PIC S9(7)   VALUE ZERO COMP-3.           
010600                                                                          
010700 01  FILLER                  PIC X(16)   VALUE 'WS-SEKTION'.              
010800 01  WS-SEKTION                  PIC X(30)   VALUE SPACE.                 
010900 01  FILLER                  PIC X(16)   VALUE 'WS-IMS-SEKTION'.          
011000 01  WS-IMS-SEKTION              PIC X(30)   VALUE SPACE.                 
011100 01  FILLER                  PIC X(16)   VALUE 'WS-FIL-SEKTION'.          
011200 01  WS-FIL-SEKTION              PIC X(30)   VALUE SPACE.                 
011300                                                                          
011400                                                                          
011500 01  SPAR-TIFSGVV-2000          PIC 9(7).                                 
011600 01  FILLER  REDEFINES SPAR-TIFSGVV-2000.                                 
011700     03  FILLER                  PIC 9.                                   
011800     03  FIRST-SS-2000           PIC 9(2).                                
011900     03  FIRST-AA-2000           PIC 9(2).                                
012000     03  FIRST-VV-2000           PIC 9(2).                                
012100 01  SPAR-TIFSGVV               PIC 9(6).                                 
012200 01  FILLER REDEFINES SPAR-TIFSGVV.                                       
012300     03  FIRST-SS                PIC 9(2).                                
012400     03  FIRST-AA                PIC 9(2).                                
012500     03  FIRST-VV                PIC 9(2).                                
012600                                                                          
012700     EJECT                                                                
012800                                                                          
012900 01  DAGENS-SSAAVV               PIC 9(7).                                
013000 01  FILLER     REDEFINES DAGENS-SSAAVV.                                  
013100     03  FILLER                  PIC 9.                                   
013200     03  DAGENS-SS-VV            PIC 9(2).                                
013300     03  DAGENS-AA-VV            PIC 9(2).                                
013400     03  DAGENS-VV               PIC 9(2).                                
013500                                                                          
013600 01  SPAR-IDARTNR                PIC S9(9) VALUE ZERO COMP-3.             
013700                                                                          
013800     EJECT                                                                
013900 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
014000 01  FILLER REDEFINES DAGENS-DATUM.                                       
014100     03  DAGENS-DATUM-AAR        PIC 9(2).                                
014200     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
014300     03  DAGENS-DATUM-DAG        PIC 9(2).                                
014400     EJECT                                                                
014500 01  DYNAMISKA-SUBPROGRAM.                                                
014600*                                                                         
014700     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
014800     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
014900     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
015000     03  DATKORT                 PIC X(8)    VALUE 'DATKORT'.             
015100     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
015200     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
015300     SKIP2                                                                
015400*    --- PARAMETRAR TILL ABEND                                            
015500                                                                          
015600 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
015700 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
015800 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
015900     SKIP2                                                                
016000 01  FELTEXT.                                                             
016100     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
016200     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
016300     EJECT                                                                
016400*    --- PARAMETRAR TILL DATKORT                                          
016500*                                                                         
016600 01  PROGRAM-NAMN                PIC X(6)    VALUE 'W3715A'.              
016700     SKIP2                                                                
016800 01  DATUMKORT-ID                PIC X(6)    VALUE 'WDATUM'.              
016900     SKIP2                                                                
017000*01  -COPY WDATKORT                                                       
017100     EJECT                                                                
017200*    --- PARAMETRAR TILL POSTSUM                                          
017300*                                                                         
017400*01  -COPY W0005   -PRE  POSTSUM-                                         
017500     EJECT                                                                
017600*01  -COPY WDATAREA                                                       
017700     EJECT                                                                
017800*01  -COPY WWPRODSL                                                       
017900     EJECT                                                                
018000 01  TEST-IDARTNR              PIC 9(9) COMP-3.                           
018100*01  FILLER -COPY WWBYT19    -RED TEST-IDARTNR                            
018200     EJECT                                                                
018300 01  INFIL-AREA-START            PIC X(24)   VALUE                        
018400                                             'INFIL-AREA-START'.          
018500     SKIP2                                                                
018600 01  INFIL-AREA.                                                          
018700     03  INFIL-AREA-0.                                                    
018800         05  INFIL-IDPTYP        PIC X(3).                                
018900         05  FILLER              PIC X(400).                              
019000*   03  FILLER -COPY W330310  -PRE I310-   -RED  INFIL-AREA-0             
019100*   03  FILLER -COPY W330300  -PRE I300-   -RED  INFIL-AREA-0             
019200*   03  FILLER -COPY W330320  -PRE I320-   -RED  INFIL-AREA-0             
019300*   03  FILLER -COPY W330330  -PRE I330-   -RED  INFIL-AREA-0             
019400*   03  FILLER -COPY W330340  -PRE I340-   -RED  INFIL-AREA-0             
019500*   03  FILLER -COPY W330350  -PRE I350-   -RED  INFIL-AREA-0             
019600     EJECT                                                                
019700 01  IN1-AREA-START              PIC X(24)   VALUE                        
019800                                 'IN1-AREA-START  '.                      
019900     EJECT                                                                
020000 01  IN310-AREA-START           PIC X(24)   VALUE                         
020100                                           'IN310-AREA-START  '.          
020200     SKIP3                                                                
020300 01  IN31-AREA                  PIC X(28).                                
020400*01  FILLER  -PRE IN310-  -COPY W330310 -RED IN31-AREA                    
020500     SKIP3                                                                
020600 01  IN30-AREA                  PIC X(16).                                
020700*01  FILLER  -PRE IN300-  -COPY W330300 -RED IN30-AREA                    
020800     EJECT                                                                
020900 01  IN32-AREA                  PIC X(14).                                
021000*01  FILLER  -PRE IN320-  -COPY W330320 -RED IN32-AREA                    
021100     EJECT                                                                
021200 01  IN33-AREA                  PIC X(14).                                
021300*01  FILLER  -PRE IN330-  -COPY W330330 -RED IN33-AREA                    
021400     EJECT                                                                
021500 01  IN34-AREA                  PIC X(14).                                
021600*01  FILLER  -PRE IN340-  -COPY W330340 -RED IN34-AREA                    
021700     EJECT                                                                
021800 01  IN35-AREA                  PIC X(14).                                
021900*01  FILLER  -PRE IN350-  -COPY W330350 -RED IN35-AREA                    
022000     EJECT                                                                
022100                                                                          
022200 01  IN2-AREA-START              PIC X(24)   VALUE                        
022300                                 'IN2-AREA-START  '.                      
022400     SKIP2                                                                
022500                                                                          
022600*01  AREA -COPY W01160     -PRE IN2-                                      
022700     EJECT                                                                
022800 01  UT-AREA-START               PIC X(24)   VALUE                        
022900                                 'UT-AREA-START  '.                       
023000     SKIP2                                                                
023100                                                                          
023200 01  IN3-AREA-START              PIC X(24)   VALUE                        
023300                                 'IN3-AREA-START  '.                      
023400     SKIP2                                                                
023500                                                                          
023600*01  AREA -COPY W01174     -PRE IN3-                                      
023700     EJECT                                                                
023800 01  UT-AREA-START               PIC X(24)   VALUE                        
023900                                 'UT-AREA-START  '.                       
024000     SKIP2                                                                
024100                                                                          
024200*01  AREA -COPY W3715A     -PRE UT-                                       
024300     EJECT                                                                
024400*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
024500*                                                                         
024600     EJECT                                                                
024700 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
024800     SKIP3                                                                
024900 01  NYCKLAR-TILL-DLI.                                                    
025000     03  W-WDGXKEY-X.                                                     
025100         05   FILLER             PIC X(4)    VALUE '3139'.                
025200         05   FILLER             PIC X(26)   VALUE LOW-VALUE.             
025300     03  W-WDGXKEY-LOW-X.                                                 
025400         05  W-IDARTNR-LOW-X     PIC S9(9)   VALUE ZERO COMP-3.           
025500         05  W-IDDISTR-LOW-X     PIC S9(5)   VALUE ZERO COMP-3.           
025600         05  W-IDTABNR-LOW-X     PIC S9(3)   VALUE ZERO COMP-3.           
025700         SKIP2                                                            
025800     03  W-WDGXKEY-HIGH-X.                                                
025900         05  W-IDARTNR-HIGH-X    PIC S9(9)   VALUE ZERO COMP-3.           
026000         05  W-IDDISTR-HIGH-X    PIC S9(5)   VALUE ZERO COMP-3.           
026100         05  W-IDTABNR-HIGH-X    PIC S9(3)   VALUE ZERO COMP-3.           
026200                                                                          
026300     SKIP2                                                                
026400*    --- STATUS-KOD FRÅN IMS                                              
026500 01  STATUS-WS                   PIC XX.                                  
026600     88  SEGMENT-FINNS                       VALUE '  '.                  
026700     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
026800     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
026900     SKIP2                                                                
027000 01  GODK-STATUSKODER.                                                    
027100     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
027200     SKIP3                                                                
027300 01  SSA1                        PIC X(64).                               
027400 01  SSA2                        PIC X(64).                               
027500     EJECT                                                                
027600*    --- IMS FUNKTIONSKODER                                               
027700*01  -COPY W0003                                                          
027800     EJECT                                                                
027900*    ---  DLI INPUT-OUTPUT AREA                                           
028000 01  FILLER         PIC X(16) VALUE 'DLI-IO-WLXXCP11'.                    
028100 01  DLI-IO-WLXXCP11.                                                     
028200*    03  -COPY WDGX3140 -PRE XXCP-                                        
028300     EJECT                                                                
028400 LINKAGE SECTION.                                                         
028500                                                                          
028600                                                                          
028700*01  -COPY W0008  -PRE XXCP-                                              
028800     05  FILLER                  PIC X.                                   
028900     EJECT                                                                
029000 PROCEDURE DIVISION  USING XXCP-PCB.                                      
029100 MAIN SECTION.                                                            
029200     ENTRY 'DLITCBL' USING XXCP-PCB.                                      
029300                                                                          
029400     PERFORM A-INIT                                                       
029500                                                                          
029600     PERFORM S01-LAES-INFIL                                               
029700     PERFORM S02-LAES-W01160                                              
029800     PERFORM S03-LAES-W01174                                              
029900     PERFORM UNTIL END-OF-INFIL                                           
030000        PERFORM B-BEARBETA                                                
030100        PERFORM S01-LAES-INFIL                                            
030200     END-PERFORM                                                          
030300                                                                          
030400                                                                          
030500     PERFORM Z-FINIT                                                      
030600                                                                          
030700     MOVE ZERO TO RETURN-CODE                                             
030800     GOBACK                                                               
030900     .                                                                    
031000     EJECT                                                                
031100 A-INIT SECTION.                                                          
031200     MOVE 'A-INIT '   TO WS-SEKTION                                       
031300                                                                          
031400     SKIP2                                                                
031500     OPEN INPUT INFIL                                                     
031600                 W01160                                                   
031700                 W01174                                                   
031800                                                                          
031900     OPEN OUTPUT W3715A                                                   
032000                                                                          
032100     CALL DATKORT USING PROGRAM-NAMN DATUMKORT-ID DATUMKORT               
032200     MOVE D-AAR       TO DAGENS-DATUM-AAR                                 
032300                         DAGENS-AA-VV                                     
032400     MOVE D-MAANAD    TO DAGENS-DATUM-MAANAD                              
032500     MOVE D-DAG       TO DAGENS-DATUM-DAG                                 
032600     MOVE D-VECKA     TO DAGENS-VV                                        
032700     SUBTRACT 1 FROM     DAGENS-VV                                        
032800                                                                          
032900     IF D-AAR > 60                                                        
033000        MOVE 19      TO DAGENS-SS-VV                                      
033100     ELSE                                                                 
033200        MOVE 20      TO DAGENS-SS-VV                                      
033300     END-IF                                                               
033400                                                                          
033500     DISPLAY 'DAGENS-SSAAVV ' DAGENS-SSAAVV                               
033600     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
033700     .                                                                    
033800     EJECT                                                                
033900                                                                          
034000 B-BEARBETA SECTION.                                                      
034100     MOVE 'B-BEARBETA '   TO WS-SEKTION                                   
034200                                                                          
034300     IF INFIL-IDPTYP = '300'                                              
034400        MOVE 'N'             TO POST-DATUM-SW-OK                          
034500        MOVE I300-DAFSGVV      TO SPAR-TIFSGVV                            
034600        MOVE FIRST-AA   TO FIRST-AA-2000                                  
034700        MOVE FIRST-VV   TO FIRST-VV-2000                                  
034800        MOVE FIRST-SS   TO FIRST-SS-2000                                  
034900        IF SPAR-TIFSGVV-2000 = DAGENS-SSAAVV                              
035000           MOVE 'J'          TO POST-DATUM-SW-OK                          
035100           MOVE SPAR-TIFSGVV-2000                                         
035200                               TO UT-TIFSGVV                              
035300           MOVE I300-IDARTNR   TO UT-IDARTNR                              
035400                                  UT-IDARTNR-OBJ                          
035500                                     SPAR-IDARTNR                         
035600                                     TEST-IDARTNR                         
035700           IF BYT19-BYTES                                                 
035800           OR BYT19-RADIO                                                 
035900              IF BYT19-BYTES                                              
036000                 ADD +6000     TO UT-IDARTNR-OBJ                          
036100                                   W-IDARTNR-LOW-X                        
036200                                   W-IDARTNR-HIGH-X                       
036300              ELSE                                                        
036400                 IF BYT19-RADIO                                           
036500                    ADD +1000  TO UT-IDARTNR-OBJ                          
036600                                   W-IDARTNR-LOW-X                        
036700                                   W-IDARTNR-HIGH-X                       
036800                 END-IF                                                   
036900              END-IF                                                      
037000           END-IF                                                         
037100           PERFORM BA-HMTA-PRODSL-FKNGRP                                  
037200           PERFORM BB-HMTA-BENAMNING                                      
037300           PERFORM BC-HMTA-TABELLNR                                       
037400        END-IF                                                            
037500     END-IF                                                               
037600     IF INFIL-IDPTYP = '310'                                              
037700        IF POST-DATUM-OK                                                  
037800           MOVE I310-IDDISTR   TO UT-IDDISTR                              
037900           MOVE I310-SUARTFSG  TO UT-SUARTFSG                             
038000           MOVE I310-SULEVANT  TO UT-SULEVANT                             
038100           MOVE IN2-CLAG-KDPRODSL                                         
038200                               TO TEST-KDPRODSL                           
038300           IF KDPRODSL-VOLVO-BYTES                                        
038400              PERFORM S11-SKRIV-W3715A                                    
038500           END-IF                                                         
038600        END-IF                                                            
038700     END-IF                                                               
038800                                                                          
038900     .                                                                    
039000     EJECT                                                                
039100                                                                          
039200 BA-HMTA-PRODSL-FKNGRP SECTION.                                           
039300     MOVE 'BA-HMTA-PRODSL-FKNGRP' TO WS-SEKTION                           
039400                                                                          
039500     PERFORM UNTIL END-OF-W01160                                          
039600          OR SPAR-IDARTNR = IN2-CLAG-IDARTNR                              
039700          OR SPAR-IDARTNR < IN2-CLAG-IDARTNR                              
039800             PERFORM S02-LAES-W01160                                      
039900     END-PERFORM                                                          
040000     IF SPAR-IDARTNR = IN2-CLAG-IDARTNR                                   
040100        MOVE IN2-CLAG-KDPRODSL TO UT-KDPRODSL                             
040200        MOVE IN2-CLAG-IDFKNGRP TO UT-IDFKNGRP                             
040300     ELSE                                                                 
040400        MOVE ZERO              TO UT-KDPRODSL                             
040500        MOVE ZERO              TO UT-IDFKNGRP                             
040600     END-IF                                                               
040700                                                                          
040800     .                                                                    
040900     EJECT                                                                
041000                                                                          
041100 BB-HMTA-BENAMNING SECTION.                                               
041200     MOVE 'BB-HMTA-BENAMNING' TO WS-SEKTION                               
041300                                                                          
041400     PERFORM UNTIL END-OF-W01174                                          
041500          OR SPAR-IDARTNR = IN3-IDARTNR                                   
041600          OR SPAR-IDARTNR < IN3-IDARTNR                                   
041700             PERFORM S03-LAES-W01174                                      
041800     END-PERFORM                                                          
041900     IF SPAR-IDARTNR = IN3-IDARTNR                                        
042000        MOVE IN3-BEART(8)      TO UT-BEART                                
042100     ELSE                                                                 
042200        MOVE SPACE             TO UT-BEART                                
042300     END-IF                                                               
042400     .                                                                    
042500     EJECT                                                                
042600                                                                          
042700 BC-HMTA-TABELLNR SECTION.                                                
042800     MOVE 'BC-HMTA-TABELLNR' TO WS-SEKTION                                
042900                                                                          
043000     MOVE ZERO        TO W-IDDISTR-LOW-X                                  
043100     MOVE ZERO        TO W-IDTABNR-LOW-X                                  
043200     MOVE +99999      TO W-IDDISTR-HIGH-X                                 
043300     MOVE +999        TO W-IDTABNR-HIGH-X                                 
043400                                                                          
043500     PERFORM IMS-GET-ARTIKEL-WDGX                                         
043600     IF SEGMENT-FINNS                                                     
043700       MOVE XXCP-3140-IDTABNR     TO UT-IDTABNR                           
043800     ELSE                                                                 
043900       MOVE ZERO                  TO UT-IDTABNR                           
044000     END-IF                                                               
044100                                                                          
044200     .                                                                    
044300     EJECT                                                                
044400                                                                          
044500 Z-FINIT SECTION.                                                         
044600                                                                          
044700     MOVE 'Z-FINIT'   TO WS-SEKTION                                       
044800                                                                          
044900     CLOSE INFIL                                                          
045000           W01160                                                         
045100           W01174                                                         
045200           W3715A                                                         
045300     SKIP2                                                                
045400     MOVE 'S' TO POSTSUM-OPKOD                                            
045500     CALL POSTSUM USING POSTSUM-PARM                                      
045600     .                                                                    
045700     EJECT                                                                
045800                                                                          
045900 S01-LAES-INFIL   SECTION.                                                
046000     MOVE 'S01-LAES-INFIL '  TO WS-FIL-SEKTION                            
046100     SKIP2                                                                
046200     READ INFIL INTO INFIL-AREA                                           
046300     AT END                                                               
046400        SET END-OF-INFIL TO TRUE                                          
046500     NOT AT END                                                           
046600       ADD +1 TO W-INFIL-KVPOST-IN                                        
046700       MOVE 'INFIL ' TO POSTSUM-FDNAMN                                    
046800       MOVE 'W3715AD1' TO POSTSUM-DDNAMN2                                 
046900       MOVE INFIL-IDPTYP TO POSTSUM-TRANSTYP                              
047000       CALL POSTSUM USING POSTSUM-PARM                                    
047100     END-READ                                                             
047200     .                                                                    
047300     EJECT                                                                
047400                                                                          
047500 S02-LAES-W01160  SECTION.                                                
047600     MOVE 'S02-LAS-W01160' TO WS-FIL-SEKTION                              
047700     READ W01160 INTO IN2-AREA                                            
047800     AT END                                                               
047900        MOVE HIGH-VALUE TO IN2-AREA                                       
048000        SET END-OF-W01160 TO TRUE                                         
048100                                                                          
048200     NOT AT END                                                           
048300        ADD +1        TO W-INFIL-KVPOST-W01160                            
048400        MOVE 'W01160' TO POSTSUM-FDNAMN                                   
048500        MOVE 'W3715AD2' TO POSTSUM-DDNAMN2                                
048600        MOVE SPACE      TO POSTSUM-TRANSTYP                               
048700        CALL POSTSUM USING POSTSUM-PARM                                   
048800     END-READ                                                             
048900     .                                                                    
049000     EJECT                                                                
049100 S03-LAES-W01174 SECTION.                                                 
049200     MOVE 'S03-LAES-W01174' TO WS-FIL-SEKTION                             
049300                                                                          
049400     READ W01174 INTO IN3-AREA                                            
049500     AT END                                                               
049600        MOVE HIGH-VALUE TO IN3-AREA                                       
049700        SET END-OF-W01174 TO TRUE                                         
049800                                                                          
049900     NOT AT END                                                           
050000        ADD +1        TO W-INFIL-KVPOST-W01174                            
050100        MOVE 'W01174' TO POSTSUM-FDNAMN                                   
050200        MOVE 'W3715AD3' TO POSTSUM-DDNAMN2                                
050300        MOVE SPACE      TO POSTSUM-TRANSTYP                               
050400        CALL POSTSUM USING POSTSUM-PARM                                   
050500     END-READ                                                             
050600     .                                                                    
050700     EJECT                                                                
050800 S11-SKRIV-W3715A SECTION.                                                
050900     MOVE 'S03-SKRIV-W3715A' TO WS-FIL-SEKTION                            
051000                                                                          
051100     WRITE UT-POST FROM UT-AREA                                           
051200     ADD +1        TO W-UTFIL-KVPOST-UT                                   
051300                                                                          
051400     MOVE 'W3715A' TO POSTSUM-FDNAMN                                      
051500     MOVE 'W3715AD4' TO POSTSUM-DDNAMN2                                   
051600     MOVE SPACE      TO POSTSUM-TRANSTYP                                  
051700     CALL POSTSUM USING POSTSUM-PARM                                      
051800     .                                                                    
051900     EJECT                                                                
052000 S99-ABEND SECTION.                                                       
052100                                                                          
052200     SKIP2                                                                
052300     MOVE 'S' TO POSTSUM-OPKOD                                            
052400     CALL POSTSUM USING POSTSUM-PARM                                      
052500     CALL ABEND USING RKOD-ABEND                                          
052600     .                                                                    
052700     EJECT                                                                
052800* --- IMS SEKTIONER ---                                                   
052900                                                                          
053000     EJECT                                                                
053100                                                                          
053200 IMS-GET-ARTIKEL-WDGX SECTION.                                            
053300                                                                          
053400     MOVE 'IMS-GET-ARTIKEL-WDGX' TO WS-IMS-SEKTION                        
053500     STRING 'WLXXCP01(WDGXKEY  =' W-WDGXKEY-X ')'                         
053600          DELIMITED BY SIZE INTO SSA1                                     
053700     STRING 'WLXXCP11(WDGXKEY =>' W-WDGXKEY-LOW-X                         
053800                    '&WDGXKEY <=' W-WDGXKEY-HIGH-X ')'                    
053900          DELIMITED BY SIZE INTO SSA2                                     
054000                                                                          
054100     MOVE '  GE' TO GODK-STATUSKODER                                      
054200     CALL CBLTDLI USING GU XXCP-PCB DLI-IO-WLXXCP11 SSA1 SSA2             
054300     MOVE XXCP-STATUS-CODE TO STATUS-WS                                   
054400     PERFORM IMS-STATUSKONTROLL                                           
054500     ADD +1 TO W-ANTAL-DATABAS-LAS                                        
054600     .                                                                    
054700     EJECT                                                                
054800 IMS-STATUSKONTROLL SECTION.                                              
054900                                                                          
055000     SET STATUS-IX TO 1                                                   
055100     SEARCH GODK-STATUS                                                   
055200       AT END                                                             
055300         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
055400           DELIMITED BY SIZE INTO FELTEXT                                 
055500         DISPLAY FELTEXT                                                  
055600         CALL FELLOG                                                      
055700       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
055800         CONTINUE                                                         
055900     END-SEARCH                                                           
056000     .                                                                    
