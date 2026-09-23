000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W6153600.                                                
000300 AUTHOR.         TOMMIE JIVARP.                                           
000400 DATE-WRITTEN.   98/01/08.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNKTION:                                                            
000800*        PROGRAMMET KOLLAR LAGERPLATSHISTORIKEN (WDJ9) FÖR                
000900*        ARTIKLAR SOM FALLIT FÖR 'RELOCATION RULES', SAMT LÄSER           
001000*        BENÄMNINGSREGISTRET (WDD3).                                      
001100*                                                                         
001200*        PROGRAMMET LÄSER      WLLOCB (WDJ9)                              
001300*        PROGRAMMET LÄSER      WLBENA (WDD3)                              
001400*                                                                         
001500*    ABENDKODER:                                                          
001600*        U0016 -  . . . .                                                 
001700*        U1000 -  . . . .                                                 
001800*                                                                         
001900                                                                          
002000     SKIP3                                                                
002100 ENVIRONMENT DIVISION.                                                    
002200     SKIP2                                                                
002300 INPUT-OUTPUT SECTION.                                                    
002400                                                                          
002500 FILE-CONTROL.                                                            
002600     SKIP2                                                                
002700*          --- INFIL MED LAGERPLATSFREKVENS                               
002800     SELECT W61534                     ASSIGN TO W61536D1.                
002900     SKIP2                                                                
003000*          --- UTFIL MED KOMPL. UPPG. IFRÅN WDD3 & WDJ9                   
003100     SELECT W61536                     ASSIGN TO W61536D2.                
003200     EJECT                                                                
003300 DATA DIVISION.                                                           
003400     SKIP2                                                                
003500 FILE SECTION.                                                            
003600     SKIP3                                                                
003700 FD  W61534                                                               
003800     RECORDING       F                                                    
003900     BLOCK CONTAINS  0.                                                   
004000                                                                          
004100*01  -COPY W61530      -L.                                                
004200     SKIP3                                                                
004300 FD  W61536                                                               
004400     RECORDING       F                                                    
004500     BLOCK CONTAINS  0.                                                   
004600                                                                          
004700*01  POST -COPY W61530 -PRE  UT1-  -L.                                    
004800     EJECT                                                                
004900                                                                          
005000 WORKING-STORAGE SECTION.                                                 
005100                                                                          
005200                                                                          
005300*    -COPY WY2000W1                                                       
005400     SKIP3                                                                
005500 77  IDPGM                       PIC X(8)    VALUE 'W6153600'.            
005600 77  JA                          PIC X       VALUE 'J'.                   
005700 77  NEJ                         PIC X       VALUE 'N'.                   
005800                                                                          
005900 77  W61534-EOF-SW               PIC X       VALUE 'N'.                   
006000     88  END-OF-W61534                       VALUE 'J'.                   
006100                                                                          
006200 77  PLOCKPLATS-HITTAD-SW        PIC X       VALUE 'N'.                   
006300     88  PLOCKPLATS-HITTAD                   VALUE 'J'.                   
006400     EJECT                                                                
006500 01  WS-DATUM-HISTPLATS          PIC 9(8).                                
006600 01  WS-TIDATUM-HISTPLATS REDEFINES WS-DATUM-HISTPLATS.                   
006700     03 FILLER                   PIC 9(2).                                
006800     03 WS-DATUM-HISTPLATS-6POS  PIC 9(6).                                
006900                                                                          
007000 01  WS-TALLY-PLACE-TOO-OLD      PIC 9(9)    VALUE ZERO.                  
007100 01  WS-TALLY-WDJ9-NOT-CORRECT   PIC 9(9)    VALUE ZERO.                  
007200 01  WS-TALLY-WDJ9-NOT-FOUND     PIC 9(9)    VALUE ZERO.                  
007300 01  WS-TALLY-WDJ9-FOUND         PIC 9(9)    VALUE ZERO.                  
007400                                                                          
007500 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
007600 01  FILLER REDEFINES DAGENS-DATUM.                                       
007700     03  DAGENS-DATUM-AAR        PIC 9(2).                                
007800     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
007900     03  DAGENS-DATUM-DAG        PIC 9(2).                                
008000     EJECT                                                                
008100 01  DYNAMISKA-SUBPROGRAM.                                                
008200*                                                                         
008300     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
008400     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
008500     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
008600     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
008700     03  WDAGKONV                PIC X(8)    VALUE 'WDAGKONV'.            
008800     SKIP2                                                                
008900*    -COPY WDAGAREA                                                       
009000                                                                          
009100*    --- PARAMETRAR TILL ABEND                                            
009200                                                                          
009300 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
009400 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
009500 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
009600     SKIP2                                                                
009700 01  FELTEXT.                                                             
009800     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
009900     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
010000     EJECT                                                                
010100*    --- PARAMETRAR TILL POSTSUM                                          
010200*                                                                         
010300*01  -COPY W0005   -PRE  POSTSUM-                                         
010400     EJECT                                                                
010500 01  IN1-AREA-START              PIC X(24)   VALUE                        
010600                                 'IN1-AREA-START  '.                      
010700     SKIP2                                                                
010800                                                                          
010900*01  AREA -COPY W61530     -PRE IN1-                                      
011000     EJECT                                                                
011100 01  UT1-AREA-START              PIC X(24)   VALUE                        
011200                                 'UT1-AREA-START  '.                      
011300     SKIP2                                                                
011400                                                                          
011500*01  AREA -COPY W61530     -PRE UT1-                                      
011600     EJECT                                                                
011700*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
011800*                                                                         
011900     EJECT                                                                
012000 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
012100     SKIP3                                                                
012200 01  NYCKLAR-TILL-DLI.                                                    
012300     03  W-IDARTNR-X.                                                     
012400         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
012700     03  W-IDSKYLT-X.                                                     
012800         05  W-IDSKYLT           PIC X(3)    VALUE SPACE.                 
012900     03  W-WDJ911KY-MIN-X.                                                
013000         05  W-IDDC-MIN          PIC X(2)    VALUE SPACE.                 
013100         05  FILLER              PIC X(18)   VALUE LOW-VALUE.             
013200     03  W-WDJ911KY-MAX-X.                                                
013300         05  W-IDDC-MAX          PIC X(2)    VALUE SPACE.                 
013400         05  FILLER              PIC X(18)   VALUE HIGH-VALUE.            
013500     SKIP2                                                                
013600*    --- STATUS-KOD FRÅN IMS                                              
013700 01  STATUS-WS                   PIC XX.                                  
013800     88  SEGMENT-FINNS                       VALUE '  '.                  
013900     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
014000     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
014100     SKIP2                                                                
014200                                                                          
014300 01  GODK-STATUSKODER.                                                    
014400     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
014500     SKIP3                                                                
014600 01  SSA1                        PIC X(96).                               
014700 01  SSA2                        PIC X(96).                               
014800     EJECT                                                                
014900*    --- IMS FUNKTIONSKODER                                               
015000*01  -COPY W0003                                                          
015100     EJECT                                                                
015200*    ---  DLI INPUT-OUTPUT AREA                                           
015300 01  FILLER         PIC X(16) VALUE 'DLI-IO-WLLOCB01'.                    
015400 01  DLI-IO-WLLOCB01.                                                     
015500*    03  -COPY WDJ901  -PRE LOCB-                                         
015600     EJECT                                                                
015700 01  FILLER         PIC X(16) VALUE 'DLI-IO-WLLOCB11'.                    
015800 01  DLI-IO-WLLOCB11.                                                     
015900*    03  -COPY WDJ911  -PRE LOCB-                                         
016000 01  FILLER         PIC X(16) VALUE 'DLI-IO-WLBENA11'.                    
016100 01  DLI-IO-WLBENA11.                                                     
016200*    03  -COPY WDD311  -PRE BENA-                                         
016300     EJECT                                                                
016400                                                                          
016500 LINKAGE SECTION.                                                         
016600                                                                          
016700     EJECT                                                                
016800*01  -COPY W0008  -PRE LOCB-                                              
016900     05  FILLER                  PIC X.                                   
017000     EJECT                                                                
017100*01  -COPY W0008  -PRE BENA-                                              
017200     05  FILLER                  PIC X.                                   
017300     EJECT                                                                
017400                                                                          
017500 PROCEDURE DIVISION  USING LOCB-PCB BENA-PCB.                             
017600 MAIN SECTION.                                                            
017700     ENTRY 'DLITCBL' USING LOCB-PCB BENA-PCB.                             
017800                                                                          
017900                                                                          
018000     PERFORM A-INIT                                                       
018100                                                                          
018200     PERFORM S01-LAES-W61534                                              
018300     PERFORM UNTIL END-OF-W61534                                          
018400     MOVE IN1-IDARTNR TO W-IDARTNR-X                                      
018500     MOVE '11'        TO W-IDDC-MIN                                       
018600                         W-IDDC-MAX                                       
018700       PERFORM B-TILLDELA-KLAR-DATA                                       
018800                                                                          
018900***** HÄMTA SVENSK BENÄMNING FRÅN WDD3                                    
019000       PERFORM IMS-GU-BENA01-BSEQ                                         
019100       IF SEGMENT-FINNS                                                   
019200         MOVE BENA-TEXT-BEART TO UT1-BEART-ENG                            
019300       ELSE                                                               
019400         MOVE SPACE TO UT1-BEART-ENG                                      
019500       END-IF                                                             
019600                                                                          
019700***** HÄMTA FÖREDETTA LAGERPLATS FRÅN WDJ9                                
019800       PERFORM IMS-GU-LOCB01                                              
019900       IF SEGMENT-FINNS                                                   
020300          PERFORM IMS-GNP-LOCB11                                          
020500          MOVE NEJ TO PLOCKPLATS-HITTAD-SW                                
020600          PERFORM UNTIL PLOCKPLATS-HITTAD                                 
020700             IF SEGMENT-FINNS                                             
022100                IF ((LOCB-HIST-KDLOC       = 'C' OR 'P')      AND         
022200                    (LOCB-HIST-DASTODAT    > 0 ))                         
022500                   PERFORM C-KOLLA-WDJ9                                   
022800                END-IF                                                    
022900             ELSE                                                         
023300                 PERFORM S20-LAGERPLATS-SAKNAS                            
023400             END-IF                                                       
023500             PERFORM IMS-GNP-LOCB11                                       
023700          END-PERFORM                                                     
023800       ELSE                                                               
024200          PERFORM S20-LAGERPLATS-SAKNAS                                   
024300       END-IF                                                             
024400       PERFORM S11-SKRIV-W61536                                           
024500       PERFORM S01-LAES-W61534                                            
024600     END-PERFORM                                                          
024700                                                                          
024800                                                                          
024900     PERFORM Z-FINIT                                                      
025000                                                                          
025100     MOVE ZERO TO RETURN-CODE                                             
025200     GOBACK                                                               
025300     .                                                                    
025400     EJECT                                                                
025500                                                                          
025600 A-INIT SECTION.                                                          
025700                                                                          
025800     OPEN INPUT  W61534                                                   
025900                                                                          
026000     OPEN OUTPUT W61536                                                   
026100                                                                          
026200     ACCEPT DAGENS-DATUM  FROM DATE                                       
026300     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
026400     MOVE 'S  ' TO W-IDSKYLT                                              
026500                                                                          
026600     MOVE 003 TO DAG-KDCALL                                               
026700     MOVE 366 TO DAG-KVKALDAG                                             
026800     MOVE DAGENS-DATUM TO DAG-TIAAMMDD-TOM                                
026900     CALL WDAGKONV USING DAG-KDCALL                                       
027000                         DAG-DATUM-AREA                                   
027100                         DAG-KDSVAR                                       
027200     .                                                                    
027300     EJECT                                                                
027400                                                                          
027500 B-TILLDELA-KLAR-DATA SECTION.                                            
027600                                                                          
027700     MOVE IN1-IDARTNR     TO UT1-IDARTNR                                  
027800     MOVE IN1-IDDC        TO UT1-IDDC                                     
027900     MOVE IN1-ADLAGOMR    TO UT1-ADLAGOMR                                 
028000     MOVE IN1-ADGANG      TO UT1-ADGANG                                   
028100     MOVE IN1-ADPLATS     TO UT1-ADPLATS                                  
028200     MOVE IN1-KDSTOR      TO UT1-KDSTOR                                   
028300     MOVE IN1-KDFREQ      TO UT1-KDFREQ                                   
028400     MOVE IN1-KVPB-TOT    TO UT1-KVPB-TOT                                 
028500     MOVE IN1-FLSEASON    TO UT1-FLSEASON                                 
028600     MOVE IN1-KDTECKEN    TO UT1-KDTECKEN                                 
028700     MOVE IN1-KVPB-FOM    TO UT1-KVPB-FOM                                 
028800     MOVE IN1-KVPB-TOM    TO UT1-KVPB-TOM                                 
028900     MOVE IN1-RELOCFAC    TO UT1-RELOCFAC                                 
029000     MOVE IN1-KVQPACK-3   TO UT1-KVQPACK-3                                
029100     MOVE IN1-FLCDART     TO UT1-FLCDART                                  
029200     MOVE IN1-VKART       TO UT1-VKART                                    
029300     .                                                                    
029400     EJECT                                                                
029500                                                                          
029600 C-KOLLA-WDJ9 SECTION.                                                    
029700                                                                          
029800     COMPUTE WS-DATUM-HISTPLATS = 99999999 -                              
029900                                  LOCB-HIST-DASTADAT-9KOMPL               
030000                                                                          
030100*    MOVE WS-DATUM-HISTPLATS-6POS  TO TMP1-YYMMDD                         
030200*    MOVE DAG-TIAAMMDD-FOM         TO TMP2-YYMMDD                         
030300*    PERFORM WY2000P1                                                     
030700*    IF TMP1-YYMMDD >= TMP2-YYMMDD AND                                    
030800*      (LOCB-HIST-IDDC = IN1-IDDC AND                                     
030900*       NOT (LOCB-HIST-ADLAGOMR = IN1-ADLAGOMR                            
031000*       AND  LOCB-HIST-ADGANG  = IN1-ADGANG                               
031100*       AND  LOCB-HIST-ADPLATS = IN1-ADPLATS))                            
031300       ADD +1                  TO WS-TALLY-WDJ9-FOUND                     
031400       MOVE WS-DATUM-HISTPLATS-6POS TO UT1-TIRELOC-DATE                   
031500       MOVE LOCB-HIST-ADLAGOMR TO UT1-ADLAGOMR-OLD                        
031600       MOVE LOCB-HIST-ADGANG   TO UT1-ADGANG-OLD                          
031700       MOVE LOCB-HIST-ADPLATS  TO UT1-ADPLATS-OLD                         
031800*    ELSE                                                                 
031900*      ADD +1                  TO WS-TALLY-PLACE-TOO-OLD                  
032000*      MOVE 0                  TO UT1-TIRELOC-DATE                        
032100*      MOVE ZERO               TO UT1-ADLAGOMR-OLD                        
032200*                                 UT1-ADGANG-OLD                          
032300*                                 UT1-ADPLATS-OLD                         
032400*    END-IF                                                               
032500     MOVE JA    TO   PLOCKPLATS-HITTAD-SW                                 
032600     .                                                                    
032700     EJECT                                                                
032800                                                                          
032900 Z-FINIT SECTION.                                                         
033000     CLOSE W61534                                                         
033100           W61536                                                         
033200     SKIP2                                                                
033300     MOVE 'S' TO POSTSUM-OPKOD                                            
033400     CALL POSTSUM USING POSTSUM-PARM                                      
033500                                                                          
033600*    DISPLAY 'GAMMAL  ' WS-TALLY-PLACE-TOO-OLD                            
033700*    DISPLAY 'WDJ9FEL ' WS-TALLY-WDJ9-NOT-CORRECT                         
033800     DISPLAY 'SAKNAS  ' WS-TALLY-WDJ9-NOT-FOUND                           
033900     DISPLAY 'KORREKT ' WS-TALLY-WDJ9-FOUND                               
034000     .                                                                    
034100     EJECT                                                                
034200                                                                          
034300 S01-LAES-W61534  SECTION.                                                
034400     READ W61534 INTO IN1-AREA                                            
034500     AT END                                                               
034600        MOVE HIGH-VALUE TO IN1-AREA                                       
034700        SET END-OF-W61534 TO TRUE                                         
034800                                                                          
034900     NOT AT END                                                           
035000        MOVE 'W61534' TO POSTSUM-FDNAMN                                   
035100        MOVE 'W61536D1' TO POSTSUM-DDNAMN2                                
035200        MOVE SPACE TO POSTSUM-TRANSTYP                                    
035300        CALL POSTSUM USING POSTSUM-PARM                                   
035400     END-READ                                                             
035500     .                                                                    
035600     EJECT                                                                
035700                                                                          
035800 S11-SKRIV-W61536 SECTION.                                                
035900                                                                          
036000     WRITE UT1-POST FROM UT1-AREA                                         
036100     MOVE SPACE TO POSTSUM-TRANSTYP                                       
036200     MOVE 'W61536' TO POSTSUM-FDNAMN                                      
036300     MOVE 'W61536D2' TO POSTSUM-DDNAMN2                                   
036400     CALL POSTSUM USING POSTSUM-PARM                                      
036500     .                                                                    
036600     EJECT                                                                
036700                                                                          
036800 S20-LAGERPLATS-SAKNAS SECTION.                                           
036900***** LAGERPLATSHISTORIK SAKNAS                                           
037000     ADD  +1      TO WS-TALLY-WDJ9-NOT-FOUND                              
037100     MOVE 0       TO UT1-TIRELOC-DATE                                     
037200                     UT1-ADLAGOMR-OLD                                     
037300                     UT1-ADGANG-OLD                                       
037400                     UT1-ADPLATS-OLD                                      
037500     MOVE JA      TO PLOCKPLATS-HITTAD-SW                                 
037600     .                                                                    
037700     EJECT                                                                
037800 S99-ABEND SECTION.                                                       
037900                                                                          
038000     SKIP2                                                                
038100     MOVE 'S' TO POSTSUM-OPKOD                                            
038200     CALL POSTSUM USING POSTSUM-PARM                                      
038300     CALL ABEND USING RKOD-ABEND                                          
038400     .                                                                    
038500     EJECT                                                                
038600* --- IMS SEKTIONER ---                                                   
038700     SKIP3                                                                
038800     EJECT                                                                
038900                                                                          
039000 IMS-GU-LOCB01 SECTION.                                                   
039100                                                                          
039200     STRING 'WDJ901  (IDARTNR  =' W-IDARTNR-X ')'                         
039300          DELIMITED BY SIZE INTO SSA1                                     
039400     MOVE '  GE' TO GODK-STATUSKODER                                      
039500     CALL CBLTDLI USING GU LOCB-PCB DLI-IO-WLLOCB01 SSA1                  
039600     MOVE LOCB-STATUS-CODE TO STATUS-WS                                   
039700     PERFORM IMS-STATUSKONTROLL                                           
039800     .                                                                    
039900     EJECT                                                                
040000                                                                          
040100 IMS-GNP-LOCB11 SECTION.                                                  
040200                                                                          
040300     STRING 'WDJ911  (WDJ911KY>=' W-WDJ911KY-MIN-X                        
040400                    '&WDJ911KY<=' W-WDJ911KY-MAX-X ')'                    
040500          DELIMITED BY SIZE INTO SSA1                                     
040600     MOVE '  GE' TO GODK-STATUSKODER                                      
040700     CALL CBLTDLI USING GNP LOCB-PCB DLI-IO-WLLOCB11 SSA1                 
040800     MOVE LOCB-STATUS-CODE TO STATUS-WS                                   
040900     PERFORM IMS-STATUSKONTROLL                                           
041000     .                                                                    
041100     EJECT                                                                
041200                                                                          
041300 IMS-GU-BENA01-BSEQ SECTION.                                              
041400                                                                          
041500     STRING 'WLBENA01(WDD3BSEQ =' W-IDARTNR-X ')'                         
041600          DELIMITED BY SIZE INTO SSA1                                     
041700     STRING 'WLBENA11(IDSKYLT  =' W-IDSKYLT-X ')'                         
041800          DELIMITED BY SIZE INTO SSA2                                     
041900     MOVE '  GE'               TO GODK-STATUSKODER                        
042000     CALL CBLTDLI USING GU BENA-PCB DLI-IO-WLBENA11 SSA1 SSA2             
042100     MOVE BENA-STATUS-CODE     TO STATUS-WS                               
042200     PERFORM IMS-STATUSKONTROLL                                           
042300     .                                                                    
042400     EJECT                                                                
042500                                                                          
042600                                                                          
042700 IMS-STATUSKONTROLL SECTION.                                              
042800                                                                          
042900     SET STATUS-IX TO 1                                                   
043000     SEARCH GODK-STATUS                                                   
043100       AT END                                                             
043200         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
043300           DELIMITED BY SIZE INTO FELTEXT                                 
043400         DISPLAY FELTEXT                                                  
043500         CALL FELLOG                                                      
043600       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
043700         CONTINUE                                                         
043800     END-SEARCH                                                           
043900     .                                                                    
044000     EJECT                                                                
044100*    -COPY WY2000P1                                                       
