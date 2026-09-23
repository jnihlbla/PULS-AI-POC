000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W4793400.                                                
000300 AUTHOR.         BERT ANDERSSON.                                          
000400 DATE-WRITTEN.   08/07/02.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNKTION:                                                            
000800*        LÄSER IN FIL W47932 OCH SKAPAR DAP POSTER                        
000900*        MANGEMENT - ACCUMULATED NIL PICKS AND DEVIATIONS                 
001000*                    PERIODICALLY FOLLOW UP.                              
001100                                                                          
001200     SKIP3                                                                
001300 ENVIRONMENT DIVISION.                                                    
001400     SKIP2                                                                
001500 INPUT-OUTPUT SECTION.                                                    
001600                                                                          
001700 FILE-CONTROL.                                                            
001800     SKIP2                                                                
001900*          --- FIL MED NIL PICKS AND DEVIATIONS                           
002000     SELECT W47932                     ASSIGN TO W47934D1.                
002100     SKIP2                                                                
002200 DATA DIVISION.                                                           
002300     SKIP3                                                                
002400 FILE SECTION.                                                            
002500     SKIP3                                                                
002600 FD  W47932                                                               
002700     RECORDING       F                                                    
002800     BLOCK CONTAINS  0.                                                   
002900                                                                          
003000*01  -COPY W47932       -L.                                               
003100     SKIP3                                                                
003200 WORKING-STORAGE SECTION.                                                 
003300                                                                          
003400 77  IDPGM                       PIC X(8)    VALUE 'W4793400'.            
003500 77  FILLER                      PIC X(8)    VALUE 'PGMPOS:'.             
003600 77  WS-PGM-POS                  PIC X(40)   VALUE SPACE.                 
003700 77  FILLER                      PIC X(8)    VALUE 'IMSPOS:'.             
003800 77  WS-PGM-IMS-POS              PIC X(40)   VALUE SPACE.                 
003900 77  JA                          PIC X       VALUE 'J'.                   
004000 77  NEJ                         PIC X       VALUE 'N'.                   
004100 77  SORT-INDX                   PIC S9(9) COMP-3.                        
004200 77  INDX-WS                     PIC  9(9).                               
004300 77  ANTAL-POSTER                PIC S9(9) COMP-3.                        
004400 77  WS-ADRESS                   PIC X(50)                                
004500                   VALUE 'CARPARTS.DAP.DISTRDOC'.                         
004600 77  WZ04-SEND-IDCOM             PIC S9(9)   COMP VALUE +0.               
004700 77  FELTEXT                     PIC X(80)  VALUE SPACE.                  
004800 77  ERROR-TEXT                  PIC X(80)  VALUE SPACE.                  
004900 77  KDRC-DISPLAY                PIC Z(5).                                
005000 77  WS-IDDC                     PIC X(2)   VALUE SPACE.                  
005100 77  WS-KDMFUP                   PIC X(2)   VALUE SPACE.                  
005200 77  WS-ADCITY                   PIC X(20)  VALUE SPACE.                  
005300 77  WS-IDLANDX2                 PIC X(2)   VALUE SPACE.                  
005400                                                                          
005500 01  WS-YYMMDDHHMM.                                                       
005600     03 WS-YYMMDD                PIC  9(6).                               
005700     03 WS-TIME                  PIC  9(4).                               
005800                                                                          
005900 01  WS-AAVV                     PIC  9(4).                               
006000                                                                          
006100 01  WS-HHMMSSTH                 PIC  9(8).                               
006200 01  FILLER REDEFINES WS-HHMMSSTH.                                        
006300       03  WS-HHMM               PIC 9(4).                                
006400       03  WS-SSTH               PIC 9(4).                                
006500 01  WS-IDDC-WEB                 PIC X(2) VALUE SPACE.                    
006600*---------------------------------------                                  
006700                                                                          
006800 01  W-IDDC-B6-X.                                                         
006900     03 W-IDDC-B6        PIC X(2).                                        
007000                                                                          
007100     EJECT                                                                
007200*---------------------------------------                                  
007300                                                                          
007400     SKIP2                                                                
007500 01  FELTEXT.                                                             
007600     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
007700     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
007800                                                                          
007900 77  W47932-EOF-SW               PIC X       VALUE 'N'.                   
008000     88  END-OF-W47932                       VALUE 'J'.                   
008100     EJECT                                                                
008200 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
008300 01  FILLER REDEFINES DAGENS-DATUM.                                       
008400     03  DAGENS-DATUM-AAR        PIC 9(2).                                
008500     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
008600     03  DAGENS-DATUM-DAG        PIC 9(2).                                
008700     EJECT                                                                
008800 01  DYNAMISKA-SUBPROGRAM.                                                
008900*                                                                         
009000     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
009100     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
009200     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
009300     03  WZ01SEND                PIC X(8)    VALUE 'WZ01SEND'.            
009400     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
009500     03  WINTSOR                 PIC X(8)    VALUE 'WINTSOR'.             
009600     SKIP2                                                                
009700* -SORT-TABELL ------                                                     
009800 01  TABENTRY-PARM.                                                       
009900     03  STEGLAANGD              PIC S9(9) COMP.                          
010000     03  ANTAL                   PIC S9(9) COMP.                          
010100     03  NYCKELLAANGD            PIC S9(9) COMP.                          
010200*                                                                         
010300 01  FILLER                      PIC X(16) VALUE 'SORT-TABELL'.           
010400 01  SORT-TABELL.                                                         
010500     03  TAB-RAD OCCURS 500.                                              
010600        05  TAB-SORT-BEGREPP1.                                            
010700            07 TAB-KDMFUP        PIC X(2).                                
010800            07 TAB-IDLANDX2      PIC X(2).                                
010900            07 TAB-IDDC          PIC X(2).                                
011000        05  TAB-ANTAL-AVVIKELSE-RADER  PIC 9(6).                          
011100        05  TAB-TOTALT-ANTAL-RADER     PIC 9(6).                          
011200        05  TAB-PROCENT          PIC 9(3).9(3).                           
011300        05  TAB-TIAAVV           PIC 9(4).                                
011400        05  TAB-ADCITY           PIC X(20).                               
011500                                                                          
011600     EJECT                                                                
011700*    --- PARAMETERS TO ABEND                                              
011800                                                                          
011900 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
012000 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
012100 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
012200                                                                          
012300*    --- PARAMETRAR TILL POSTSUM                                          
012400*                                                                         
012500*01  -COPY W0005   -PRE  POSTSUM-                                         
012600     EJECT                                                                
012700 01  FILLER                      PIC X(16)   VALUE 'WZ01SEND'.            
012800*01  -COPY WZ01SEND                                                       
012900     EJECT                                                                
013000 01  IN-AREA-START               PIC X(24)   VALUE                        
013100                                             'IN-AREA-START'.             
013200     SKIP2                                                                
013300                                                                          
013400*01  AREA -COPY W47932      -PRE IN-                                      
013500     EJECT                                                                
013600 01  UT-AREA-START               PIC X(25)   VALUE                        
013700                                             'UT-AREA-START'.             
013800     SKIP2                                                                
013900 01  HDR-AREA.                                                            
014000*   03  -COPY WZ01REQU -PRE HDR-                                          
014100*   03  -COPY WZ04HDR                                                     
014200*                                                                         
014300 01  LINE-AREA                   PIC X(25)    VALUE 'LINE-AREA'.          
014400 01  DOC-LINE-AREA.                                                       
014500*    03 -COPY W479321 -PRE LINE-                                          
014600     EJECT                                                                
014700     SKIP3                                                                
014800 01  FILLER               PIC X(16)   VALUE 'WDB601 AREA'.                
014900 01   DLI-IO-AREA-B601.                                                   
015000*     03  -COPY WDB601                                                    
015100                                                                          
015200                                                                          
015300 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
015400*    --- STATUS-KOD FRÅN IMS                                              
015500 01  STATUS-WS                   PIC XX.                                  
015600     88  SEGMENT-FINNS                       VALUE '  '.                  
015700     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
015800     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
015900     88  SEGMENT-SLUT                        VALUE 'GB'.                  
016000     88  IMS-EJ-OK                           VALUE 'XD'.                  
016100     SKIP2                                                                
016200 01  GODK-STATUSKODER.                                                    
016300     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
016400     SKIP3                                                                
016500 01  SSA1                        PIC X(64).                               
016600 01  SSA2                        PIC X(64).                               
016700     EJECT                                                                
016800*    --- IMS FUNKTIONSKODER                                               
016900*01  -COPY W0003                                                          
017000     EJECT                                                                
017100                                                                          
017200 LINKAGE SECTION.                                                         
017300*01  -COPY W0009   -PRE MSG-                                              
017400     EJECT                                                                
017500 01  DAP-PCB              PIC X.                                          
017600     EJECT                                                                
017700*01  -COPY W0008 -PRE WDB6-                                               
017800     05  FILLER           PIC X.                                          
017900                                                                          
018000 PROCEDURE DIVISION  USING MSG-PCB DAP-PCB WDB6-PCB.                      
018100 MAIN SECTION.                                                            
018200     ENTRY 'DLITCBL' USING MSG-PCB DAP-PCB WDB6-PCB.                      
018300                                                                          
018400     SKIP2                                                                
018500     PERFORM A-INIT                                                       
018600     PERFORM S01-LAES-W47932                                              
018700     IF NOT END-OF-W47932                                                 
018800                                                                          
018900       PERFORM UNTIL END-OF-W47932                                        
019000         MOVE IN-IDDC TO W-IDDC-B6                                        
019100         PERFORM IMS-GU-WDB601                                            
019200                                                                          
019300         PERFORM B-WEB-LISTA                                              
019400         MOVE IN-IDDC  TO WS-IDDC-WEB                                     
019500         PERFORM S01-LAES-W47932                                          
019600       END-PERFORM                                                        
019700*                                                                         
019800       IF SORT-INDX > +0                                                  
019900         PERFORM C-SORT-TABELL                                            
020000         PERFORM D-SKAPA-DAP-LINES                                        
020100       END-IF                                                             
020200                                                                          
020300       PERFORM S05-SEND-CLOSE                                             
020400     END-IF                                                               
020500     PERFORM Z-FINIT                                                      
020600     MOVE ZERO TO RETURN-CODE                                             
020700     GOBACK                                                               
020800     .                                                                    
020900     EJECT                                                                
021000 A-INIT SECTION.                                                          
021100     SKIP2                                                                
021200                                                                          
021300     MOVE NEJ              TO W47932-EOF-SW                               
021400     OPEN INPUT W47932                                                    
021500                                                                          
021600     ACCEPT WS-YYMMDD      FROM DATE                                      
021700     ACCEPT WS-HHMMSSTH    FROM TIME                                      
021800     MOVE WS-HHMM          TO WS-TIME                                     
021900     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
022000                                                                          
022100     MOVE +0    TO SORT-INDX                                              
022200     .                                                                    
022300     EJECT                                                                
022400 B-WEB-LISTA  SECTION.                                                    
022500     MOVE 'B-WEB-LISTA        '      TO WS-PGM-POS                        
022600                                                                          
022700     ADD +1 TO SORT-INDX                                                  
022800     MOVE IN-IDDC               TO TAB-IDDC     (SORT-INDX)               
022900     MOVE IN-KDMFUP             TO TAB-KDMFUP   (SORT-INDX)               
023000     IF SEGMENT-FINNS                                                     
023100       IF   DCS-IDLANDX2 > SPACE                                          
023200         MOVE DCS-IDLANDX2      TO TAB-IDLANDX2 (SORT-INDX)               
023300       ELSE                                                               
023400         MOVE 'MISSING'         TO TAB-IDLANDX2 (SORT-INDX)               
023500       END-IF                                                             
023600       MOVE DCS-ADGMT-PADR(11:20) TO TAB-ADCITY (SORT-INDX)               
023700     END-IF                                                               
023800     MOVE IN-TIAAVV             TO TAB-TIAAVV   (SORT-INDX)               
023900     MOVE IN-ANTAL-AVVIKELSE-RADER                                        
024000                   TO TAB-ANTAL-AVVIKELSE-RADER (SORT-INDX)               
024100     MOVE IN-TOTALT-ANTAL-RADER                                           
024200                   TO TAB-TOTALT-ANTAL-RADER    (SORT-INDX)               
024300     MOVE IN-PROCENT            TO TAB-PROCENT  (SORT-INDX)               
024600     .                                                                    
024700     SKIP3                                                                
024800 C-SORT-TABELL SECTION.                                                   
024900     MOVE 'C-SORT-TABELL      '      TO WS-PGM-POS                        
025000                                                                          
025100     MOVE +49   TO STEGLAANGD                                             
025200     MOVE SORT-INDX TO ANTAL                                              
025300     MOVE +6    TO NYCKELLAANGD                                           
025400                                                                          
025500     CALL WINTSOR USING SORT-TABELL STEGLAANGD ANTAL                      
025600     TAB-SORT-BEGREPP1(1) NYCKELLAANGD                                    
025700     .                                                                    
025800     EJECT                                                                
025900 D-SKAPA-DAP-LINES    SECTION.                                            
026000     MOVE 'D-SKAPA-DAP-LINES  '      TO WS-PGM-POS                        
026100                                                                          
026200     MOVE SORT-INDX TO ANTAL-POSTER                                       
026300     MOVE +1 TO SORT-INDX                                                 
026400     MOVE TAB-IDLANDX2 (SORT-INDX) TO WS-IDLANDX2                         
026500     MOVE TAB-IDDC     (SORT-INDX) TO WS-IDDC                             
026600     MOVE TAB-KDMFUP   (SORT-INDX) TO WS-KDMFUP                           
026700     PERFORM DA-SKAPA-HEADER                                              
026800                                                                          
026900     PERFORM UNTIL SORT-INDX > ANTAL-POSTER                               
027000       IF TAB-KDMFUP (SORT-INDX) NOT = WS-KDMFUP                          
027100         PERFORM S05-SEND-CLOSE                                           
027200         MOVE TAB-IDLANDX2 (SORT-INDX) TO WS-IDLANDX2                     
027300         MOVE TAB-IDDC     (SORT-INDX) TO WS-IDDC                         
027400         MOVE TAB-KDMFUP   (SORT-INDX) TO WS-KDMFUP                       
027500         PERFORM DA-SKAPA-HEADER                                          
027600       ELSE                                                               
027700         IF TAB-IDLANDX2 (SORT-INDX) NOT = WS-IDLANDX2                    
027800           MOVE ALL '+'                  TO LINE-W479321                  
027900           MOVE '1'                      TO LINE-IDAFPRCD                 
028000*          DISPLAY ' DOC-LINE-AREA===' DOC-LINE-AREA                      
028100           PERFORM S05-PUT-REPORT-LINE                                    
028200           MOVE TAB-IDLANDX2 (SORT-INDX) TO WS-IDLANDX2                   
028300         END-IF                                                           
028400       END-IF                                                             
028500                                                                          
028600       MOVE '1'                        TO LINE-IDAFPRCD                   
028700       MOVE TAB-IDDC            (SORT-INDX) TO LINE-IDDC                  
028800       MOVE TAB-TIAAVV          (SORT-INDX) TO LINE-TIAAVV                
028900       MOVE TAB-IDLANDX2        (SORT-INDX) TO LINE-IDLANDX2              
029000       MOVE TAB-ADCITY          (SORT-INDX) TO LINE-ADCITY                
029100       MOVE TAB-ANTAL-AVVIKELSE-RADER (SORT-INDX)                         
029200                            TO LINE-ANTAL-AVVIKELSE-RADER                 
029300       MOVE TAB-TOTALT-ANTAL-RADER (SORT-INDX)                            
029400                            TO LINE-TOTALT-ANTAL-RADER                    
029500       MOVE TAB-PROCENT (SORT-INDX)   TO LINE-PROCENT                     
029700       PERFORM S05-PUT-REPORT-LINE                                        
029800       ADD +1 TO SORT-INDX                                                
029900     END-PERFORM                                                          
030000     .                                                                    
030100     EJECT                                                                
030200 DA-SKAPA-HEADER SECTION.                                                 
030300     MOVE 'DA-SKAPA-HEADER    '      TO WS-PGM-POS                        
030400                                                                          
030500     MOVE 1                          TO HDR-REQU-IDMSGVER                 
030600     MOVE 'E'                        TO HDR-REQU-KDPGMACT                 
030700     MOVE IDPGM                      TO HDR-REQU-IDUSER                   
030800                                                                          
030900     MOVE SPACE                      TO HDR-IDOUTREC                      
031000     MOVE 'MANNILPICWEEK'            TO HDR-IDOUTTYPE                     
031100     MOVE WS-KDMFUP                  TO HDR-IDOUTREC(1:2)                 
031200     MOVE 'W47934'                   TO HDR-IDOUTREC(3:6)                 
031300     MOVE WS-YYMMDDHHMM              TO HDR-IDLIST                        
031400                                                                          
031500***  IF WZ04-SEND-IDCOM = ZERO                                            
031600       PERFORM S05-SEND-OPEN                                              
031700       MOVE SEND-IDCOM TO WZ04-SEND-IDCOM                                 
031800***  END-IF                                                               
031900*HDR                                                                      
032000     PERFORM S05-PUT-HEADER                                               
032100     .                                                                    
032200     SKIP3                                                                
032300 Z-FINIT SECTION.                                                         
032400                                                                          
032500     CLOSE W47932                                                         
032600                                                                          
032700     SKIP2                                                                
032800     MOVE 'S' TO POSTSUM-OPKOD                                            
032900     CALL POSTSUM USING POSTSUM-PARM                                      
033000     .                                                                    
033100     EJECT                                                                
033200 S01-LAES-W47932  SECTION.                                                
033300     SKIP2                                                                
033400     MOVE 'S01-LAES-W47932    '      TO WS-PGM-POS                        
033500                                                                          
033600     READ W47932 INTO IN-AREA                                             
033700     AT END                                                               
033800****    MOVE HIGH-VALUE TO IN-IDDC                                        
033900        SET END-OF-W47932 TO TRUE                                         
034000                                                                          
034100     NOT AT END                                                           
034200        MOVE 'W47934' TO POSTSUM-FDNAMN                                   
034300        MOVE 'W47934D1' TO POSTSUM-DDNAMN2                                
034400        MOVE 'IN-'     TO POSTSUM-TRANSTYP                                
034500        CALL POSTSUM USING POSTSUM-PARM                                   
034600     END-READ                                                             
034700     .                                                                    
034800     EJECT                                                                
034900 S05-SEND-OPEN SECTION.                                                   
035000     MOVE 'S05-SEND-OPEN      '      TO WS-PGM-POS                        
035100                                                                          
035200     MOVE 'CARPARTS.DAP.DISTRDOC'         TO SEND-ADDISPABS               
035300     MOVE 'OPEN'                          TO SEND-KDFUNC                  
035400     CALL WZ01SEND USING SEND-CONTROL-AREA                                
035500                         SEND-OPEN-AREA                                   
035600     IF SEND-KDRC > ZERO                                                  
035700       MOVE SEND-KDRC                     TO KDRC-DISPLAY                 
035800       STRING 'WZ01SEND OPEN ERROR RC=' KDRC-DISPLAY                      
035900       DELIMITED BY SIZE INTO ERROR-TEXT                                  
036000       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
036100     END-IF                                                               
036200                                                                          
036300     .                                                                    
036400     SKIP3                                                                
036500 S05-PUT-HEADER SECTION.                                                  
036600     MOVE 'S05-PUT-HEADER     '      TO WS-PGM-POS                        
036700                                                                          
036800     MOVE 'PUT'                           TO SEND-KDFUNC                  
036900     MOVE WZ04-SEND-IDCOM                 TO SEND-IDCOM                   
037000     MOVE LENGTH OF HDR-AREA              TO SEND-KVDLEN                  
037100     CALL WZ01SEND USING SEND-CONTROL-AREA                                
037200                         SEND-KVDLEN                                      
037300                         HDR-AREA                                         
037310                                                                          
037400     IF SEND-KDRC > ZERO                                                  
037500       MOVE SEND-KDRC                     TO KDRC-DISPLAY                 
037600       STRING 'WZ01SEND PUT ERROR RC=' KDRC-DISPLAY                       
037700       DELIMITED BY SIZE INTO ERROR-TEXT                                  
037800       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
037900     END-IF                                                               
038000     .                                                                    
038100     EJECT                                                                
038200 S05-PUT-REPORT-LINE    SECTION.                                          
038300     MOVE 'S05-PUT-REPORT-LINE '     TO WS-PGM-POS                        
038400                                                                          
038500     MOVE 'PUT'                           TO SEND-KDFUNC                  
038600     MOVE WZ04-SEND-IDCOM                 TO SEND-IDCOM                   
038700     MOVE LENGTH OF DOC-LINE-AREA         TO SEND-KVDLEN                  
038800     CALL WZ01SEND USING SEND-CONTROL-AREA                                
038900                         SEND-KVDLEN                                      
039000                         DOC-LINE-AREA                                    
039100     IF SEND-KDRC > ZERO                                                  
039200       MOVE SEND-KDRC                     TO KDRC-DISPLAY                 
039300       STRING 'WZ01SEND PUT ERROR RC=' KDRC-DISPLAY                       
039400       DELIMITED BY SIZE INTO ERROR-TEXT                                  
039500       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
039600     END-IF                                                               
039700     .                                                                    
039800     SKIP3                                                                
039900 S05-SEND-CLOSE SECTION.                                                  
040000     MOVE 'S05-SEND-CLOSE  '         TO WS-PGM-POS                        
040100                                                                          
040200     MOVE 'CLOSE'                         TO SEND-KDFUNC                  
040300     MOVE WZ04-SEND-IDCOM                 TO SEND-IDCOM                   
040400     CALL WZ01SEND USING SEND-CONTROL-AREA                                
040500                                                                          
040600     IF SEND-KDRC > ZERO                                                  
040700       MOVE SEND-KDRC                     TO KDRC-DISPLAY                 
040800       STRING 'WZ01SEND CLOSE ERROR RC=' KDRC-DISPLAY                     
040900       DELIMITED BY SIZE INTO ERROR-TEXT                                  
041000       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
041100     END-IF                                                               
041200     .                                                                    
041300 IMS-GU-WDB601    SECTION.                                                
041400     MOVE 'IMS-GU-WDB601   '         TO WS-PGM-IMS-POS                    
041500                                                                          
041600     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
041700          DELIMITED BY SIZE INTO SSA1                                     
041800     MOVE '  GE' TO GODK-STATUSKODER                                      
041900     CALL CBLTDLI USING GU  WDB6-PCB DLI-IO-AREA-B601 SSA1                
042000     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
042100     PERFORM IMS-STATUSKONTROLL                                           
042200     .                                                                    
042300     EJECT                                                                
042400 IMS-STATUSKONTROLL SECTION.                                              
042500                                                                          
042600     SET STATUS-IX TO 1                                                   
042700     SEARCH GODK-STATUS AT END CALL FELLOG                                
042800        WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                          
042900        CONTINUE                                                          
043000     END-SEARCH                                                           
043100     .                                                                    
