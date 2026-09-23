000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W4793300.                                                
000300 AUTHOR.         BERT ANDERSSON.                                          
000400 DATE-WRITTEN.   08/07/02.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700                                                                          
000800*    FUNKTION:                                                            
000900*        LÄSER IN FIL W47932 OCH SKAPAR DAP POSTER                        
001000*                                                                         
001100*                                                                         
001200                                                                          
001300     SKIP3                                                                
001400 ENVIRONMENT DIVISION.                                                    
001500     SKIP2                                                                
001600 INPUT-OUTPUT SECTION.                                                    
001700                                                                          
001800 FILE-CONTROL.                                                            
001900     SKIP2                                                                
002000*          --- FIL MED NIL PICKS AND DEVIATIONS                           
002100     SELECT W47932                     ASSIGN TO W47933D1.                
002200     SKIP2                                                                
002300 DATA DIVISION.                                                           
002400     SKIP3                                                                
002500 FILE SECTION.                                                            
002600     SKIP3                                                                
002700 FD  W47932                                                               
002800     RECORDING       F                                                    
002900     BLOCK CONTAINS  0.                                                   
003000                                                                          
003100*01  -COPY W47932       -L.                                               
003200     SKIP3                                                                
003300 WORKING-STORAGE SECTION.                                                 
003400                                                                          
003500 77  IDPGM                       PIC X(8)    VALUE 'W4793300'.            
003600 77  FILLER                      PIC X(8)    VALUE 'PGM-POS:'.            
003700 77  PGM-POS                     PIC X(64)   VALUE SPACE.                 
003800 77  JA                          PIC X       VALUE 'J'.                   
003900 77  NEJ                         PIC X       VALUE 'N'.                   
004000 77  WS-ADRESS                   PIC X(50)                                
004100                   VALUE 'CARPARTS.DAP.DISTRDOC'.                         
004200 77  WZ04-SEND-IDCOM             PIC S9(9)   COMP VALUE +0.               
004300 77  FELTEXT                     PIC X(80)  VALUE SPACE.                  
004400 77  ERROR-TEXT                  PIC X(80)  VALUE SPACE.                  
004500 77  KDRC-DISPLAY                PIC Z(5).                                
004600 77  WS-IDDC                     PIC X(2)   VALUE SPACE.                  
004700 77  WS-ADCITY                   PIC X(20)  VALUE SPACE.                  
004800 77  WS-IDLANDX2                 PIC X(2)   VALUE SPACE.                  
004900                                                                          
005000 01  WS-YYMMDDHHMM.                                                       
005100     03 WS-YYMMDD                PIC  9(6).                               
005200     03 WS-TIME                  PIC  9(4).                               
005300                                                                          
005400 01  WS-AAVV                     PIC  9(4).                               
005500                                                                          
005600 01  WS-HHMMSSTH                 PIC  9(8).                               
005700 01  FILLER REDEFINES WS-HHMMSSTH.                                        
005800       03  WS-HHMM               PIC 9(4).                                
005900       03  WS-SSTH               PIC 9(4).                                
006000 01  WS-IDDC-WEB                 PIC X(2) VALUE SPACE.                    
006100*---------------------------------------                                  
006200                                                                          
006300 01  W-IDDC-B6-X.                                                         
006400     03 W-IDDC-B6        PIC X(2).                                        
006500                                                                          
006600     EJECT                                                                
006700*---------------------------------------                                  
006800                                                                          
006900     SKIP2                                                                
007000 01  FELTEXT.                                                             
007100     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
007200     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
007300                                                                          
007400 77  W47932-EOF-SW               PIC X       VALUE 'N'.                   
007500     88  END-OF-W47932                       VALUE 'J'.                   
007600     EJECT                                                                
007700 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
007800 01  FILLER REDEFINES DAGENS-DATUM.                                       
007900     03  DAGENS-DATUM-AAR        PIC 9(2).                                
008000     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
008100     03  DAGENS-DATUM-DAG        PIC 9(2).                                
008200     EJECT                                                                
008300 01  DYNAMISKA-SUBPROGRAM.                                                
008400*                                                                         
008500     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
008600     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
008700     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
008800     03  WZ01SEND                PIC X(8)    VALUE 'WZ01SEND'.            
008900     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
009000     03  ABEND-CODE          PIC S9(4)   COMP  VALUE +0  SYNC.            
009100     EJECT                                                                
009200*    --- PARAMETERS TO ABEND                                              
009300                                                                          
009400 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
009500 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
009600 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
009700                                                                          
009800*    --- PARAMETRAR TILL POSTSUM                                          
009900*                                                                         
010000*01  -COPY W0005   -PRE  POSTSUM-                                         
010100     EJECT                                                                
010200 01  FILLER                      PIC X(16)   VALUE 'WZ01SEND'.            
010300*01  -COPY WZ01SEND                                                       
010400     EJECT                                                                
010500 01  IN-AREA-START               PIC X(24)   VALUE                        
010600                                             'IN-AREA-START'.             
010700     SKIP2                                                                
010800                                                                          
010900*01  AREA -COPY W47932      -PRE IN-                                      
011000     EJECT                                                                
011100 01  UT-AREA-START               PIC X(24)   VALUE                        
011200                                             'UT-AREA-START'.             
011300     SKIP2                                                                
011400 01  HDR-AREA.                                                            
011500*   03  -COPY WZ01REQU -PRE HDR-                                          
011600*   03  -COPY WZ04HDR                                                     
011700*                                                                         
011800 01  LINE-AREA                   PIC X(25)    VALUE 'LINE-AREA'.          
011900 01  DOC-LINE-AREA.                                                       
012000*    03 -COPY W479321 -PRE LINE-                                          
012100     EJECT                                                                
012200     SKIP3                                                                
012300 01  FILLER               PIC X(16)   VALUE 'WDB601 AREA'.                
012400 01   DLI-IO-AREA-B601.                                                   
012500*     03  -COPY WDB601                                                    
012600                                                                          
012700                                                                          
012800 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
012900*    --- STATUS-KOD FRÅN IMS                                              
013000 01  STATUS-WS                   PIC XX.                                  
013100     88  SEGMENT-FINNS                       VALUE '  '.                  
013200     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
013300     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
013400     88  SEGMENT-SLUT                        VALUE 'GB'.                  
013500     88  IMS-EJ-OK                           VALUE 'XD'.                  
013600     SKIP2                                                                
013700 01  GODK-STATUSKODER.                                                    
013800     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
013900     SKIP3                                                                
014000 01  SSA1                        PIC X(64).                               
014100 01  SSA2                        PIC X(64).                               
014200     EJECT                                                                
014300*    --- IMS FUNKTIONSKODER                                               
014400*01  -COPY W0003                                                          
014500     EJECT                                                                
014600                                                                          
014700 LINKAGE SECTION.                                                         
014800*01  -COPY W0009   -PRE MSG-                                              
014900     EJECT                                                                
015000 01  DAP-PCB              PIC X.                                          
015100     EJECT                                                                
015200*01  -COPY W0008 -PRE WDB6-                                               
015300     05  FILLER           PIC X.                                          
015400                                                                          
015500 PROCEDURE DIVISION  USING MSG-PCB DAP-PCB WDB6-PCB.                      
015600 MAIN SECTION.                                                            
015700     ENTRY 'DLITCBL' USING MSG-PCB DAP-PCB WDB6-PCB.                      
015800                                                                          
015900     SKIP2                                                                
016000     PERFORM A-INIT                                                       
016100     PERFORM S01-LAES-W47932                                              
016200     IF NOT END-OF-W47932                                                 
016300       MOVE IN-IDDC TO W-IDDC-B6                                          
016400       PERFORM IMS-GU-WDB601                                              
016500       PERFORM BA-SKAPA-HEADER                                            
016600       MOVE IN-IDDC    TO WS-IDDC-WEB                                     
016700       PERFORM UNTIL END-OF-W47932                                        
016800         MOVE IN-IDDC TO W-IDDC-B6                                        
016900         PERFORM IMS-GU-WDB601                                            
017000         PERFORM B-WEB-LISTA                                              
017100         MOVE IN-IDDC  TO WS-IDDC-WEB                                     
017200         PERFORM S01-LAES-W47932                                          
017300       END-PERFORM                                                        
017400       PERFORM S05-SEND-CLOSE                                             
017500     END-IF                                                               
017600     PERFORM Z-FINIT                                                      
017700     MOVE ZERO TO RETURN-CODE                                             
017800     GOBACK                                                               
017900     .                                                                    
018000     EJECT                                                                
018100 A-INIT SECTION.                                                          
018200     SKIP2                                                                
018300                                                                          
018400     MOVE NEJ              TO W47932-EOF-SW                               
018500     OPEN INPUT W47932                                                    
018600                                                                          
018700     ACCEPT WS-YYMMDD      FROM DATE                                      
018800     ACCEPT WS-HHMMSSTH    FROM TIME                                      
018900     MOVE WS-HHMM          TO WS-TIME                                     
019000     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
019100     .                                                                    
019200     EJECT                                                                
019300 B-WEB-LISTA  SECTION.                                                    
019400     MOVE    ' B-WEB-LISTA'               TO PGM-POS                      
019500                                                                          
019600     IF IN-IDDC NOT = WS-IDDC-WEB                                         
019700       MOVE IN-IDDC TO WS-IDDC-WEB                                        
019800       PERFORM S05-SEND-CLOSE                                             
019900       PERFORM BA-SKAPA-HEADER                                            
020000       PERFORM BC-SKAPA-LINE                                              
020100     ELSE                                                                 
020200       PERFORM BC-SKAPA-LINE                                              
020300     END-IF                                                               
020400     .                                                                    
020500     SKIP3                                                                
020600 BA-SKAPA-HEADER SECTION.                                                 
020700     MOVE    ' BA-SKAPA-HEADER SECTION '  TO PGM-POS                      
020800                                                                          
020900     MOVE 1                          TO HDR-REQU-IDMSGVER                 
021000     MOVE 'E'                        TO HDR-REQU-KDPGMACT                 
021100     MOVE IDPGM                      TO HDR-REQU-IDUSER                   
021200                                                                          
021300     MOVE SPACE                      TO HDR-IDOUTREC                      
021400     MOVE 'NILPICKSDEVWEEK'          TO HDR-IDOUTTYPE                     
021500     MOVE IN-IDDC                    TO HDR-IDOUTREC(1:2)                 
021600     MOVE 'W47933'                   TO HDR-IDOUTREC(3:8)                 
021700     MOVE WS-YYMMDDHHMM              TO HDR-IDLIST                        
021800                                                                          
021900***  IF WZ04-SEND-IDCOM = ZERO                                            
022000       PERFORM S05-SEND-OPEN                                              
022100*      DISPLAY 'OPEN'                                                     
022200       MOVE SEND-IDCOM TO WZ04-SEND-IDCOM                                 
022300***  END-IF                                                               
022400*HDR                                                                      
022500     PERFORM S05-PUT-HEADER                                               
022600     .                                                                    
022700     SKIP3                                                                
022800 BC-SKAPA-LINE SECTION.                                                   
022900     MOVE    ' BC-SKAPA-LINE SECTION'     TO PGM-POS                      
023000                                                                          
023100     MOVE '1'                        TO LINE-IDAFPRCD                     
023200                                                                          
023300     MOVE IN-IDDC                    TO LINE-IDDC                         
023400                                                                          
023500     MOVE IN-TIAAVV                  TO LINE-TIAAVV                       
023600                                                                          
023700     MOVE DCS-IDLANDX2               TO LINE-IDLANDX2                     
023800                                                                          
023900     MOVE DCS-ADGMT-PADR(11:20)      TO LINE-ADCITY                       
024000                                                                          
024100     MOVE IN-ANTAL-AVVIKELSE-RADER                                        
024200                          TO LINE-ANTAL-AVVIKELSE-RADER                   
024300                                                                          
024400     MOVE IN-TOTALT-ANTAL-RADER                                           
024500                          TO LINE-TOTALT-ANTAL-RADER                      
024600                                                                          
024700     MOVE IN-PROCENT                 TO LINE-PROCENT                      
024800                                                                          
024900     PERFORM S05-PUT-REPORT-LINE                                          
025000     .                                                                    
025100     EJECT                                                                
025200 Z-FINIT SECTION.                                                         
025300                                                                          
025400     CLOSE W47932                                                         
025500                                                                          
025600     SKIP2                                                                
025700     MOVE 'S' TO POSTSUM-OPKOD                                            
025800     CALL POSTSUM USING POSTSUM-PARM                                      
025900     .                                                                    
026000     EJECT                                                                
026100 S01-LAES-W47932  SECTION.                                                
026200     MOVE    ' S01-LAES-W47932      '     TO PGM-POS                      
026300     SKIP2                                                                
026400     READ W47932 INTO IN-AREA                                             
026500     AT END                                                               
026600****    MOVE HIGH-VALUE TO IN-IDDC                                        
026700        SET END-OF-W47932 TO TRUE                                         
026800                                                                          
026900     NOT AT END                                                           
027000        MOVE 'W47933' TO POSTSUM-FDNAMN                                   
027100        MOVE 'W47933D1' TO POSTSUM-DDNAMN2                                
027200        MOVE 'IN-'     TO POSTSUM-TRANSTYP                                
027300        CALL POSTSUM USING POSTSUM-PARM                                   
027400     END-READ                                                             
027500*     DISPLAY 'W47932-EOF-SW:' W47932-EOF-SW                              
027600*     DISPLAY 'POSTSUM-TOTTRANS=' POSTSUM-TOTTRANS                        
027700     .                                                                    
027800     EJECT                                                                
027900 S05-SEND-OPEN SECTION.                                                   
028000     MOVE    ' S05-SEND-OPEN SECTION '    TO PGM-POS                      
028100                                                                          
028200     MOVE 'CARPARTS.DAP.DISTRDOC'         TO SEND-ADDISPABS               
028300     MOVE 'OPEN'                          TO SEND-KDFUNC                  
028400     CALL WZ01SEND USING SEND-CONTROL-AREA                                
028500                         SEND-OPEN-AREA                                   
028600     IF SEND-KDRC > ZERO                                                  
028700       MOVE SEND-KDRC                     TO KDRC-DISPLAY                 
028800       STRING 'WZ01SEND OPEN ERROR RC=' KDRC-DISPLAY                      
028900       DELIMITED BY SIZE INTO ERROR-TEXT                                  
029000       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
029100     END-IF                                                               
029200                                                                          
029300     .                                                                    
029400     SKIP3                                                                
029500 S05-PUT-HEADER SECTION.                                                  
029600     MOVE    ' S05-PUT-HEADER SECTION '   TO PGM-POS                      
029700                                                                          
029800     MOVE 'PUT'                           TO SEND-KDFUNC                  
029900     MOVE WZ04-SEND-IDCOM                 TO SEND-IDCOM                   
030000     MOVE LENGTH OF HDR-AREA              TO SEND-KVDLEN                  
030100     CALL WZ01SEND USING SEND-CONTROL-AREA                                
030200                         SEND-KVDLEN                                      
030300                         HDR-AREA                                         
030400     IF SEND-KDRC > ZERO                                                  
030500       MOVE SEND-KDRC                     TO KDRC-DISPLAY                 
030600       STRING 'WZ01SEND PUT ERROR RC=' KDRC-DISPLAY                       
030700       DELIMITED BY SIZE INTO ERROR-TEXT                                  
030800       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
030900     END-IF                                                               
031000*    DISPLAY ' HEAD-AREA ' HDR-AREA                                       
031100     .                                                                    
031200     EJECT                                                                
031300 S05-PUT-REPORT-LINE    SECTION.                                          
031400     MOVE    ' S05-PUT-REPORT-LINE '      TO PGM-POS                      
031500                                                                          
031600     MOVE 'PUT'                           TO SEND-KDFUNC                  
031700     MOVE WZ04-SEND-IDCOM                 TO SEND-IDCOM                   
031800     MOVE LENGTH OF DOC-LINE-AREA         TO SEND-KVDLEN                  
031900     CALL WZ01SEND USING SEND-CONTROL-AREA                                
032000                         SEND-KVDLEN                                      
032100                         DOC-LINE-AREA                                    
032200     IF SEND-KDRC > ZERO                                                  
032300       MOVE SEND-KDRC                     TO KDRC-DISPLAY                 
032400       STRING 'WZ01SEND PUT ERROR RC=' KDRC-DISPLAY                       
032500       DELIMITED BY SIZE INTO ERROR-TEXT                                  
032600       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
032700     END-IF                                                               
032800*    DISPLAY ' LINE-AREA' DOC-LINE-AREA                                   
032900     .                                                                    
033000     SKIP3                                                                
033100 S05-SEND-CLOSE SECTION.                                                  
033200     MOVE     'S05-SEND-CLOSE SECTION '   TO PGM-POS                      
033300                                                                          
033400     MOVE 'CLOSE'                         TO SEND-KDFUNC                  
033500     MOVE WZ04-SEND-IDCOM                 TO SEND-IDCOM                   
033600     CALL WZ01SEND USING SEND-CONTROL-AREA                                
033700                                                                          
033800*    DISPLAY 'SEND-IDCOM' SEND-IDCOM                                      
033900                                                                          
034000     IF SEND-KDRC > ZERO                                                  
034100       MOVE SEND-KDRC                     TO KDRC-DISPLAY                 
034200       STRING 'WZ01SEND CLOSE ERROR RC=' KDRC-DISPLAY                     
034300       DELIMITED BY SIZE INTO ERROR-TEXT                                  
034400       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
034500     END-IF                                                               
034600     .                                                                    
034700 IMS-GU-WDB601    SECTION.                                                
034800     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
034900          DELIMITED BY SIZE INTO SSA1                                     
035000     MOVE '  GE' TO GODK-STATUSKODER                                      
035100     CALL CBLTDLI USING GU  WDB6-PCB DLI-IO-AREA-B601 SSA1                
035200     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
035300     PERFORM IMS-STATUSKONTROLL                                           
035400     .                                                                    
035500     EJECT                                                                
035600 IMS-STATUSKONTROLL SECTION.                                              
035700                                                                          
035800     SET STATUS-IX TO 1                                                   
035900     SEARCH GODK-STATUS AT END CALL FELLOG                                
036000        WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                          
036100        CONTINUE                                                          
036200     END-SEARCH                                                           
036300     .                                                                    
