000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W4793500.                                                
000300 AUTHOR.         BERT ANDERSSON.                                          
000400 DATE-WRITTEN.   08/07/10.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNKTION:                                                            
000800*        LÄSER IN FIL W47935 OCH SKAPAR DAP POSTER                        
000900*                                                                         
001000*        MANAGEMENT - ACCUMULATED NIL PICKS AND DEVIATIONS                
001100*                    PERIODICALLY FOLLOW UP.                              
001200*                                                                         
001300                                                                          
001400     SKIP3                                                                
001500 ENVIRONMENT DIVISION.                                                    
001600     SKIP2                                                                
001700 INPUT-OUTPUT SECTION.                                                    
001800                                                                          
001900 FILE-CONTROL.                                                            
002000     SKIP2                                                                
002100*          --- FIL MED NIL PICKS AND DEVIATIONS                           
002200     SELECT W47935                     ASSIGN TO W47935D1.                
002300     SKIP2                                                                
002400 DATA DIVISION.                                                           
002500     SKIP3                                                                
002600 FILE SECTION.                                                            
002700     SKIP3                                                                
002800 FD  W47935                                                               
002900     RECORDING       F                                                    
003000     BLOCK CONTAINS  0.                                                   
003100                                                                          
003200*01  -COPY W47932       -L.                                               
003300     SKIP3                                                                
003400 WORKING-STORAGE SECTION.                                                 
003500                                                                          
003600 77  IDPGM                       PIC X(8)    VALUE 'W4793500'.            
003700 77  FILLER                      PIC X(8)    VALUE 'PGMPOS:'.             
003800 77  WS-PGM-POS                  PIC X(40)   VALUE SPACE.                 
003900 77  FILLER                      PIC X(8)    VALUE 'IMSPOS:'.             
004000 77  WS-PGM-IMS-POS              PIC X(40)   VALUE SPACE.                 
004100 77  JA                          PIC X       VALUE 'J'.                   
004200 77  NEJ                         PIC X       VALUE 'N'.                   
004300 77  WS-ADRESS                   PIC X(50)                                
004400                   VALUE 'CARPARTS.DAP.DISTRDOC'.                         
004500 77  WZ04-SEND-IDCOM             PIC S9(9)   COMP VALUE +0.               
004600 77  FELTEXT                     PIC X(80)  VALUE SPACE.                  
004700 77  ERROR-TEXT                  PIC X(80)  VALUE SPACE.                  
004800 77  KDRC-DISPLAY                PIC Z(5).                                
004900 77  WS-IDDC                     PIC X(2)   VALUE SPACE.                  
005000 77  WS-KDMFUP                   PIC X(2)   VALUE SPACE.                  
005100 77  WS-ADCITY                   PIC X(20)  VALUE SPACE.                  
005200 77  WS-IDLANDX2                 PIC X(2)   VALUE SPACE.                  
005300                                                                          
005400 01  WS-YYMMDDHHMM.                                                       
005500     03 WS-YYMMDD                PIC  9(6).                               
005600     03 WS-TIME                  PIC  9(4).                               
005700                                                                          
005800 01  WS-AAVV                     PIC  9(4).                               
005900                                                                          
006000 01  WS-HHMMSSTH                 PIC  9(8).                               
006100 01  FILLER REDEFINES WS-HHMMSSTH.                                        
006200       03  WS-HHMM               PIC 9(4).                                
006300       03  WS-SSTH               PIC 9(4).                                
006400 01  WS-IDDC-WEB                 PIC X(2) VALUE SPACE.                    
006500*---------------------------------------                                  
006600                                                                          
006700 01  W-IDDC-B6-X.                                                         
006800     03 W-IDDC-B6        PIC X(2).                                        
006900                                                                          
007000     EJECT                                                                
007100*---------------------------------------                                  
007200                                                                          
007300     SKIP2                                                                
007400 01  FELTEXT.                                                             
007500     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
007600     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
007700                                                                          
007800 77  W47935-EOF-SW               PIC X       VALUE 'N'.                   
007900     88  END-OF-W47935                       VALUE 'J'.                   
008000                                                                          
008100 77  EMPTY-LINE-SW               PIC X       VALUE 'N'.                   
008200     88  EMPTY-LINE                          VALUE 'J'.                   
008300     EJECT                                                                
008400 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
008500 01  FILLER REDEFINES DAGENS-DATUM.                                       
008600     03  DAGENS-DATUM-AAR        PIC 9(2).                                
008700     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
008800     03  DAGENS-DATUM-DAG        PIC 9(2).                                
008900     EJECT                                                                
009000 01  DYNAMISKA-SUBPROGRAM.                                                
009100*                                                                         
009200     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
009300     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
009400     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
009500     03  WZ01SEND                PIC X(8)    VALUE 'WZ01SEND'.            
009600     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
009700     03  ABEND-CODE          PIC S9(4)   COMP  VALUE +0  SYNC.            
009800     EJECT                                                                
009900*    --- PARAMETERS TO ABEND                                              
010000                                                                          
010100 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
010200 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
010300 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
010400                                                                          
010500*    --- PARAMETRAR TILL POSTSUM                                          
010600*                                                                         
010700*01  -COPY W0005   -PRE  POSTSUM-                                         
010800     EJECT                                                                
010900 01  FILLER                      PIC X(16)   VALUE 'WZ01SEND'.            
011000*01  -COPY WZ01SEND                                                       
011100     EJECT                                                                
011200 01  IN-AREA-START               PIC X(24)   VALUE                        
011300                                             'IN-AREA-START'.             
011400     SKIP2                                                                
011500                                                                          
011600*01  AREA -COPY W47932      -PRE IN-                                      
011700     EJECT                                                                
011800 01  UT-AREA-START               PIC X(25)   VALUE                        
011900                                             'UT-AREA-START'.             
012000     SKIP2                                                                
012100 01  HDR-AREA.                                                            
012200*   03  -COPY WZ01REQU -PRE HDR-                                          
012300*   03  -COPY WZ04HDR                                                     
012400*                                                                         
012500 01  LINE-AREA                   PIC X(25)    VALUE 'LINE-AREA'.          
012600 01  DOC-LINE-AREA.                                                       
012700*    03 -COPY W479321 -PRE LINE-                                          
012800     EJECT                                                                
012900     SKIP3                                                                
013000 01  FILLER               PIC X(16)   VALUE 'WDB601 AREA'.                
013100 01   DLI-IO-AREA-B601.                                                   
013200*     03  -COPY WDB601                                                    
013300                                                                          
013400                                                                          
013500 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
013600*    --- STATUS-KOD FRÅN IMS                                              
013700 01  STATUS-WS                   PIC XX.                                  
013800     88  SEGMENT-FINNS                       VALUE '  '.                  
013900     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
014000     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
014100     88  SEGMENT-SLUT                        VALUE 'GB'.                  
014200     88  IMS-EJ-OK                           VALUE 'XD'.                  
014300     SKIP2                                                                
014400 01  GODK-STATUSKODER.                                                    
014500     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
014600     SKIP3                                                                
014700 01  SSA1                        PIC X(64).                               
014800 01  SSA2                        PIC X(64).                               
014900     EJECT                                                                
015000*    --- IMS FUNKTIONSKODER                                               
015100*01  -COPY W0003                                                          
015200     EJECT                                                                
015300                                                                          
015400 LINKAGE SECTION.                                                         
015500*01  -COPY W0009   -PRE MSG-                                              
015600     EJECT                                                                
015700 01  DAP-PCB              PIC X.                                          
015800     EJECT                                                                
015900*01  -COPY W0008 -PRE WDB6-                                               
016000     05  FILLER           PIC X.                                          
016100                                                                          
016200 PROCEDURE DIVISION  USING MSG-PCB DAP-PCB WDB6-PCB.                      
016300 MAIN SECTION.                                                            
016400     ENTRY 'DLITCBL' USING MSG-PCB DAP-PCB WDB6-PCB.                      
016500                                                                          
016600     SKIP2                                                                
016700     PERFORM A-INIT                                                       
016800     PERFORM S01-LAES-W47935                                              
016900     IF NOT END-OF-W47935                                                 
017000       MOVE IN-IDDC TO W-IDDC-B6                                          
017100       PERFORM IMS-GU-WDB601                                              
017200       MOVE IN-KDMFUP       TO WS-KDMFUP                                  
017300       MOVE DCS-IDLANDX2    TO WS-IDLANDX2                                
017400                                                                          
017500       PERFORM BA-SKAPA-HEADER                                            
017600       MOVE IN-IDDC    TO WS-IDDC-WEB                                     
017700       PERFORM UNTIL END-OF-W47935                                        
017800         MOVE IN-IDDC TO W-IDDC-B6                                        
017900         PERFORM IMS-GU-WDB601                                            
018000                                                                          
018100         PERFORM B-WEB-LISTA                                              
018200         MOVE IN-IDDC  TO WS-IDDC-WEB                                     
018300         PERFORM S01-LAES-W47935                                          
018400       END-PERFORM                                                        
018500       PERFORM S05-SEND-CLOSE                                             
018600     END-IF                                                               
018700     PERFORM Z-FINIT                                                      
018800     MOVE ZERO TO RETURN-CODE                                             
018900     GOBACK                                                               
019000     .                                                                    
019100     EJECT                                                                
019200 A-INIT SECTION.                                                          
019300     SKIP2                                                                
019400                                                                          
019500     MOVE NEJ              TO W47935-EOF-SW                               
019600     MOVE NEJ              TO EMPTY-LINE-SW                               
019700     OPEN INPUT W47935                                                    
019800                                                                          
019900     ACCEPT WS-YYMMDD      FROM DATE                                      
020000     ACCEPT WS-HHMMSSTH    FROM TIME                                      
020100     MOVE WS-HHMM          TO WS-TIME                                     
020200     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
020300     .                                                                    
020400     EJECT                                                                
020500 B-WEB-LISTA  SECTION.                                                    
020600     MOVE 'B-WEB-LISTA        '      TO WS-PGM-POS                        
020700                                                                          
020800     IF IN-KDMFUP NOT = WS-KDMFUP                                         
020900       PERFORM S05-SEND-CLOSE                                             
021000       MOVE IN-KDMFUP            TO WS-KDMFUP                             
021100       MOVE DCS-IDLANDX2         TO WS-IDLANDX2                           
021200       PERFORM BA-SKAPA-HEADER                                            
021300     ELSE                                                                 
021400       IF DCS-IDLANDX2 NOT = WS-IDLANDX2                                  
021500         MOVE DCS-IDLANDX2       TO WS-IDLANDX2                           
021600         PERFORM BD-SKAPA-EMPTY-LINE                                      
021700       END-IF                                                             
021800     END-IF                                                               
021900     PERFORM BC-SKAPA-LINE                                                
022000     .                                                                    
022100     SKIP3                                                                
022200 BA-SKAPA-HEADER SECTION.                                                 
022300     MOVE 'BA-SKAPA-HEADER    '      TO WS-PGM-POS                        
022400                                                                          
022500     MOVE 1                          TO HDR-REQU-IDMSGVER                 
022600     MOVE 'E'                        TO HDR-REQU-KDPGMACT                 
022700     MOVE IDPGM                      TO HDR-REQU-IDUSER                   
022800                                                                          
022900     MOVE SPACE                      TO HDR-IDOUTREC                      
023000     MOVE 'MANNILPICPER'             TO HDR-IDOUTTYPE                     
023100     MOVE WS-KDMFUP                  TO HDR-IDOUTREC(1:2)                 
023200     MOVE 'W47935'                   TO HDR-IDOUTREC(3:6)                 
023300     MOVE WS-YYMMDDHHMM              TO HDR-IDLIST                        
023400                                                                          
023500***  IF WZ04-SEND-IDCOM = ZERO                                            
023600       PERFORM S05-SEND-OPEN                                              
023700       MOVE SEND-IDCOM TO WZ04-SEND-IDCOM                                 
023800***  END-IF                                                               
023900*HDR                                                                      
024000     PERFORM S05-PUT-HEADER                                               
024100     .                                                                    
024200     SKIP3                                                                
024300 BC-SKAPA-LINE SECTION.                                                   
024400     MOVE 'BC-SKAPA-LINE      '      TO WS-PGM-POS                        
024500                                                                          
024600     MOVE '1'                        TO LINE-IDAFPRCD                     
024700                                                                          
024800     MOVE IN-IDDC                    TO LINE-IDDC                         
024900                                                                          
025000     MOVE IN-TIAAVV                  TO LINE-TIAAVV                       
025100                                                                          
025200     MOVE DCS-IDLANDX2               TO LINE-IDLANDX2                     
025300                                                                          
025400     MOVE DCS-ADGMT-PADR(11:20)      TO LINE-ADCITY                       
025500                                                                          
025600     MOVE IN-ANTAL-AVVIKELSE-RADER                                        
025700                          TO LINE-ANTAL-AVVIKELSE-RADER                   
025800                                                                          
025900     MOVE IN-TOTALT-ANTAL-RADER                                           
026000                          TO LINE-TOTALT-ANTAL-RADER                      
026100                                                                          
026200     MOVE IN-PROCENT                 TO LINE-PROCENT                      
026300                                                                          
026400     PERFORM S05-PUT-REPORT-LINE                                          
026500     .                                                                    
026600     EJECT                                                                
026700 BD-SKAPA-EMPTY-LINE SECTION.                                             
026800     MOVE 'BC-SKAPA-EMPTY-LINE'      TO WS-PGM-POS                        
026900                                                                          
027000                                                                          
027100     MOVE ALL '+'                    TO LINE-W479321                      
027200                                                                          
027300     MOVE '1'                        TO LINE-IDAFPRCD                     
027400                                                                          
027500     PERFORM S05-PUT-REPORT-LINE                                          
027600     .                                                                    
027700     EJECT                                                                
027800 Z-FINIT SECTION.                                                         
027900                                                                          
028000     CLOSE W47935                                                         
028100                                                                          
028200     SKIP2                                                                
028300     MOVE 'S' TO POSTSUM-OPKOD                                            
028400     CALL POSTSUM USING POSTSUM-PARM                                      
028500     .                                                                    
028600     EJECT                                                                
028700 S01-LAES-W47935  SECTION.                                                
028800     SKIP2                                                                
028900     MOVE 'S01-LAES-W47935    '      TO WS-PGM-POS                        
029000                                                                          
029100     READ W47935 INTO IN-AREA                                             
029200     AT END                                                               
029300****    MOVE HIGH-VALUE TO IN-IDDC                                        
029400        SET END-OF-W47935 TO TRUE                                         
029500                                                                          
029600     NOT AT END                                                           
029700        MOVE 'W47935' TO POSTSUM-FDNAMN                                   
029800        MOVE 'W47935D1' TO POSTSUM-DDNAMN2                                
029900        MOVE 'IN-'     TO POSTSUM-TRANSTYP                                
030000        CALL POSTSUM USING POSTSUM-PARM                                   
030100     END-READ                                                             
030200     .                                                                    
030300     EJECT                                                                
030400 S05-SEND-OPEN SECTION.                                                   
030500     MOVE 'S05-SEND-OPEN      '      TO WS-PGM-POS                        
030600                                                                          
030700     MOVE 'CARPARTS.DAP.DISTRDOC'         TO SEND-ADDISPABS               
030800     MOVE 'OPEN'                          TO SEND-KDFUNC                  
030900     CALL WZ01SEND USING SEND-CONTROL-AREA                                
031000                         SEND-OPEN-AREA                                   
031100     IF SEND-KDRC > ZERO                                                  
031200       MOVE SEND-KDRC                     TO KDRC-DISPLAY                 
031300       STRING 'WZ01SEND OPEN ERROR RC=' KDRC-DISPLAY                      
031400       DELIMITED BY SIZE INTO ERROR-TEXT                                  
031500       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
031600     END-IF                                                               
031700                                                                          
031800     .                                                                    
031900     SKIP3                                                                
032000 S05-PUT-HEADER SECTION.                                                  
032100     MOVE 'S05-PUT-HEADER     '      TO WS-PGM-POS                        
032200                                                                          
032300     MOVE 'PUT'                           TO SEND-KDFUNC                  
032400     MOVE WZ04-SEND-IDCOM                 TO SEND-IDCOM                   
032500     MOVE LENGTH OF HDR-AREA              TO SEND-KVDLEN                  
032600     CALL WZ01SEND USING SEND-CONTROL-AREA                                
032700                         SEND-KVDLEN                                      
032800                         HDR-AREA                                         
032900     IF SEND-KDRC > ZERO                                                  
033000       MOVE SEND-KDRC                     TO KDRC-DISPLAY                 
033100       STRING 'WZ01SEND PUT ERROR RC=' KDRC-DISPLAY                       
033200       DELIMITED BY SIZE INTO ERROR-TEXT                                  
033300       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
033400     END-IF                                                               
033500     .                                                                    
033600     EJECT                                                                
033700 S05-PUT-REPORT-LINE    SECTION.                                          
033800     MOVE 'S05-PUT-REPORT-LINE '     TO WS-PGM-POS                        
033900                                                                          
034000     MOVE 'PUT'                           TO SEND-KDFUNC                  
034100     MOVE WZ04-SEND-IDCOM                 TO SEND-IDCOM                   
034200     MOVE LENGTH OF DOC-LINE-AREA         TO SEND-KVDLEN                  
034300     CALL WZ01SEND USING SEND-CONTROL-AREA                                
034400                         SEND-KVDLEN                                      
034500                         DOC-LINE-AREA                                    
034600     IF SEND-KDRC > ZERO                                                  
034700       MOVE SEND-KDRC                     TO KDRC-DISPLAY                 
034800       STRING 'WZ01SEND PUT ERROR RC=' KDRC-DISPLAY                       
034900       DELIMITED BY SIZE INTO ERROR-TEXT                                  
035000       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
035100     END-IF                                                               
035200*    DISPLAY ' LINE-AREA' DOC-LINE-AREA                                   
035300     .                                                                    
035400     SKIP3                                                                
035500 S05-SEND-CLOSE SECTION.                                                  
035600     MOVE 'S05-SEND-CLOSE  '         TO WS-PGM-POS                        
035700                                                                          
035800     MOVE 'CLOSE'                         TO SEND-KDFUNC                  
035900     MOVE WZ04-SEND-IDCOM                 TO SEND-IDCOM                   
036000     CALL WZ01SEND USING SEND-CONTROL-AREA                                
036100                                                                          
036200     IF SEND-KDRC > ZERO                                                  
036300       MOVE SEND-KDRC                     TO KDRC-DISPLAY                 
036400       STRING 'WZ01SEND CLOSE ERROR RC=' KDRC-DISPLAY                     
036500       DELIMITED BY SIZE INTO ERROR-TEXT                                  
036600       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
036700     END-IF                                                               
036800     .                                                                    
036900 IMS-GU-WDB601    SECTION.                                                
037000     MOVE 'IMS-GU-WDB601   '         TO WS-PGM-IMS-POS                    
037100                                                                          
037200     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
037300          DELIMITED BY SIZE INTO SSA1                                     
037400     MOVE '  GE' TO GODK-STATUSKODER                                      
037500     CALL CBLTDLI USING GU  WDB6-PCB DLI-IO-AREA-B601 SSA1                
037600     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
037700     PERFORM IMS-STATUSKONTROLL                                           
037800     .                                                                    
037900     EJECT                                                                
038000 IMS-STATUSKONTROLL SECTION.                                              
038100                                                                          
038200     SET STATUS-IX TO 1                                                   
038300     SEARCH GODK-STATUS AT END CALL FELLOG                                
038400        WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                          
038500        CONTINUE                                                          
038600     END-SEARCH                                                           
038700     .                                                                    
