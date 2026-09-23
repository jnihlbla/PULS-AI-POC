000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W4793100.                                                
000300 AUTHOR.         BERT ANDERSSON.                                          
000400 DATE-WRITTEN.   08/06/24.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700                                                                          
000800*    FUNKTION:                                                            
000900*        LÄSER IN FIL W47931 OCH SKAPAR DAP POSTER                        
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
002100     SELECT W47930                     ASSIGN TO W47931D1.                
002200     SKIP2                                                                
002300     EJECT                                                                
002400 DATA DIVISION.                                                           
002500     SKIP3                                                                
002600 FILE SECTION.                                                            
002700     SKIP3                                                                
002800 FD  W47930                                                               
002900     RECORDING       F                                                    
003000     BLOCK CONTAINS  0.                                                   
003100                                                                          
003200*01  -COPY W47930       -L.                                               
003300     SKIP3                                                                
003400     EJECT                                                                
003500 WORKING-STORAGE SECTION.                                                 
003600                                                                          
003700 77  IDPGM                       PIC X(8)    VALUE 'W4793100'.            
003800 77  FILLER                      PIC X(8)    VALUE 'PGM-POS:'.            
003900 77  PGM-POS                     PIC X(64)   VALUE SPACE.                 
004000 77  JA                          PIC X       VALUE 'J'.                   
004100 77  NEJ                         PIC X       VALUE 'N'.                   
004200 77  WS-ADRESS                   PIC X(50)                                
004300                   VALUE 'CARPARTS.DAP.DISTRDOC'.                         
004400 77  WZ04-SEND-IDCOM             PIC S9(9)   COMP VALUE +0.               
004500 77  FELTEXT                     PIC X(80)  VALUE SPACE.                  
004600 77  ERROR-TEXT                  PIC X(80)  VALUE SPACE.                  
004700 77  KDRC-DISPLAY                PIC Z(5).                                
004800 77  WS-IDDC                     PIC X(2)   VALUE SPACE.                  
004900 77  WS-ADCITY                   PIC X(20)  VALUE SPACE.                  
005000 77  WS-IDLANDX2                 PIC X(2)   VALUE SPACE.                  
005100                                                                          
005200 01  WS-YYMMDDHHMM.                                                       
005300     03 WS-YYMMDD                PIC  9(6).                               
005400     03 WS-TIME                  PIC  9(4).                               
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
007400 77  W47931-EOF-SW               PIC X       VALUE 'N'.                   
007500     88  END-OF-W47931                       VALUE 'J'.                   
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
008700     03  DATKORT                 PIC X(8)    VALUE 'DATKORT'.             
008800     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
008900     03  WZ01SEND                PIC X(8)    VALUE 'WZ01SEND'.            
009000     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
009100     03  ABEND-CODE          PIC S9(4)   COMP  VALUE +0  SYNC.            
009200     EJECT                                                                
009300*    --- PARAMETERS TO ABEND                                              
009400                                                                          
009500 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
009600 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
009700 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
009800                                                                          
009900*    --- PARAMETRAR TILL POSTSUM                                          
010000*                                                                         
010100*01  -COPY W0005   -PRE  POSTSUM-                                         
010200     EJECT                                                                
010300 01  FILLER                      PIC X(16)   VALUE 'WZ01SEND'.            
010400*01  -COPY WZ01SEND                                                       
010500     EJECT                                                                
010600 01  IN-AREA-START               PIC X(24)   VALUE                        
010700                                             'IN-AREA-START'.             
010800     SKIP2                                                                
010900                                                                          
011000*01  AREA -COPY W47930      -PRE IN-                                      
011100     EJECT                                                                
011200 01  UT-AREA-START               PIC X(24)   VALUE                        
011300                                             'UT-AREA-START'.             
011400*    --- PARAMETRAR TILL DATKORT                                          
011500*                                                                         
011600 01  PROGRAM-NAMN                PIC X(6)    VALUE 'W47931'.              
011700     SKIP2                                                                
011800 01  DATUMKORT-ID                PIC X(6)    VALUE '000001'.              
011900     SKIP2                                                                
012000*01  -COPY WDATKORT                                                       
012100     EJECT                                                                
012200     SKIP2                                                                
012300 01  HDR-AREA.                                                            
012400*   03  -COPY WZ01REQU -PRE HDR-                                          
012500*   03  -COPY WZ04HDR                                                     
012600*                                                                         
012700 01  LINE-AREA                   PIC X(24)    VALUE 'LINE-AREA'.          
012800 01  DOC-LINE-AREA.                                                       
012900*    03 -COPY W479301 -PRE LINE-                                          
013000     EJECT                                                                
013100     SKIP3                                                                
013200 01  FILLER               PIC X(16)   VALUE 'WDB601 AREA'.                
013300 01   DLI-IO-AREA-B601.                                                   
013400*     03  -COPY WDB601                                                    
013500                                                                          
013600                                                                          
013700 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
013800*    --- STATUS-KOD FRÅN IMS                                              
013900 01  STATUS-WS                   PIC XX.                                  
014000     88  SEGMENT-FINNS                       VALUE '  '.                  
014100     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
014200     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
014300     88  SEGMENT-SLUT                        VALUE 'GB'.                  
014400     88  IMS-EJ-OK                           VALUE 'XD'.                  
014500     SKIP2                                                                
014600 01  GODK-STATUSKODER.                                                    
014700     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
014800     SKIP3                                                                
014900 01  SSA1                        PIC X(64).                               
015000 01  SSA2                        PIC X(64).                               
015100     EJECT                                                                
015200*    --- IMS FUNKTIONSKODER                                               
015300*01  -COPY W0003                                                          
015400     EJECT                                                                
015500                                                                          
015600 LINKAGE SECTION.                                                         
015700*01  -COPY W0009   -PRE MSG-                                              
015800     EJECT                                                                
015900 01  DAP-PCB              PIC X.                                          
016000     EJECT                                                                
016100*01  -COPY W0008 -PRE WDB6-                                               
016200     05  FILLER           PIC X.                                          
016300                                                                          
016400 PROCEDURE DIVISION  USING MSG-PCB DAP-PCB WDB6-PCB.                      
016500 MAIN SECTION.                                                            
016600     ENTRY 'DLITCBL' USING MSG-PCB DAP-PCB WDB6-PCB.                      
016700                                                                          
016800     SKIP2                                                                
016900     PERFORM A-INIT                                                       
017000     PERFORM S01-LAES-W47930                                              
017100     IF NOT END-OF-W47931                                                 
017200       MOVE IN-IDDC TO W-IDDC-B6                                          
017300       PERFORM IMS-GU-WDB601                                              
017400       PERFORM BA-SKAPA-HEADER                                            
017500       MOVE IN-IDDC    TO WS-IDDC-WEB                                     
017600       PERFORM UNTIL END-OF-W47931                                        
017700         MOVE IN-IDDC TO W-IDDC-B6                                        
017800         PERFORM IMS-GU-WDB601                                            
017900         PERFORM B-WEB-LISTA                                              
018000         PERFORM S01-LAES-W47930                                          
018100       END-PERFORM                                                        
018200       PERFORM S05-SEND-CLOSE                                             
018300     END-IF                                                               
018400     PERFORM Z-FINIT                                                      
018500     MOVE ZERO TO RETURN-CODE                                             
018600     GOBACK                                                               
018700     .                                                                    
018800     EJECT                                                                
018900 A-INIT SECTION.                                                          
019000     SKIP2                                                                
019100                                                                          
019200     OPEN INPUT W47930                                                    
019300                                                                          
019400     ACCEPT WS-YYMMDD      FROM DATE                                      
019500     ACCEPT WS-HHMMSSTH    FROM TIME                                      
019600     MOVE WS-HHMM          TO WS-TIME                                     
019700     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
019800*                                                                         
019900     CALL DATKORT USING PROGRAM-NAMN DATUMKORT-ID DATUMKORT               
020000     MOVE D-AAR                TO  DAGENS-DATUM-AAR                       
020100     MOVE D-MAANAD             TO  DAGENS-DATUM-MAANAD                    
020200     MOVE D-DAG                TO  DAGENS-DATUM-DAG                       
020300     .                                                                    
020400     EJECT                                                                
020500 B-WEB-LISTA  SECTION.                                                    
020600     MOVE    ' B-WEB-LISTA'     TO PGM-POS                                
020700*                                                                         
020800*     DISPLAY '############'                                              
020900*     DISPLAY 'IN-IDDISTR :'      IN-IDDISTR                              
021000*     DISPLAY 'IN-IDKUNDNR :'     IN-IDKUNDNR                             
021100*     DISPLAY 'IN-IDORDNR5 :'     IN-IDORDNR5                             
021200     IF IN-IDDC NOT = WS-IDDC-WEB                                         
021300       MOVE IN-IDDC TO WS-IDDC-WEB                                        
021400       PERFORM S05-SEND-CLOSE                                             
021500       PERFORM BA-SKAPA-HEADER                                            
021600       PERFORM BC-SKAPA-LINE                                              
021700     ELSE                                                                 
021800       PERFORM BC-SKAPA-LINE                                              
021900     END-IF                                                               
022000     .                                                                    
022100     SKIP3                                                                
022200 BA-SKAPA-HEADER SECTION.                                                 
022300     MOVE    'BA-SKAPA-HEADER SECTION '   TO PGM-POS                      
022400                                                                          
022500     MOVE 1                          TO HDR-REQU-IDMSGVER                 
022600     MOVE 'E'                        TO HDR-REQU-KDPGMACT                 
022700     MOVE IDPGM                      TO HDR-REQU-IDUSER                   
022800                                                                          
022900     MOVE SPACE                      TO HDR-IDOUTREC                      
023000     MOVE 'NILPICKSDEVDAY'           TO HDR-IDOUTTYPE                     
023100     MOVE IN-IDDC                    TO HDR-IDOUTREC(1:2)                 
023200     MOVE 'W47931'                   TO HDR-IDOUTREC(3:8)                 
023300     MOVE WS-YYMMDDHHMM              TO HDR-IDLIST                        
023400                                                                          
023500***  IF WZ04-SEND-IDCOM = ZERO                                            
023600       PERFORM S05-SEND-OPEN                                              
023700*      DISPLAY 'OPEN'                                                     
023800       MOVE SEND-IDCOM TO WZ04-SEND-IDCOM                                 
023900***  END-IF                                                               
024000*HDR                                                                      
024100     PERFORM S05-PUT-HEADER                                               
024200     .                                                                    
024300     SKIP3                                                                
024400 BC-SKAPA-LINE SECTION.                                                   
024500     MOVE    ' BC-SKAPA-LINE SECTIONON '    TO PGM-POS                    
024600                                                                          
024700     MOVE '1'                        TO LINE-IDAFPRCD                     
024800     IF IN-IDDC  = WS-IDDC                                                
024900       MOVE SPACE                    TO LINE-IDDC                         
025000     ELSE                                                                 
025100       MOVE IN-IDDC                  TO LINE-IDDC                         
025200       MOVE IN-IDDC                  TO WS-IDDC                           
025300     END-IF                                                               
025400*    MOVE WS-YYMMDD                  TO LINE-TIAAMMDD                     
025500     MOVE DAGENS-DATUM               TO LINE-TIAAMMDD                     
025600     MOVE IN-IDDISTR                 TO LINE-IDDISTR                      
025700     MOVE IN-IDKUNDNR                TO LINE-IDKUNDNR                     
025800     MOVE IN-IDORDNR5                TO LINE-IDORDNR5                     
025900     MOVE IN-IDARTNR                 TO LINE-IDARTNR                      
026000     MOVE IN-KVBEART                 TO LINE-KVBEART                      
026100     MOVE IN-KVLEVART                TO LINE-KVLEVART                     
026200     MOVE IN-KVLS                    TO LINE-KVLS                         
026300     MOVE IN-ADLAGOMR                TO LINE-ADLAGOMR                     
026400     MOVE IN-ADGANG                  TO LINE-ADGANG                       
026500     MOVE IN-ADPLATS                 TO LINE-ADPLATS                      
026600     IF DCS-IDLANDX2 = WS-IDLANDX2                                        
026700       MOVE SPACE                    TO LINE-IDLANDX2                     
026800     ELSE                                                                 
026900       MOVE DCS-IDLANDX2             TO LINE-IDLANDX2                     
027000       MOVE DCS-IDLANDX2             TO WS-IDLANDX2                       
027100     END-IF                                                               
027200     IF DCS-ADGMT-PADR(11:20) = WS-ADCITY                                 
027300       MOVE SPACE                    TO LINE-ADCITY                       
027400     ELSE                                                                 
027500       MOVE DCS-ADGMT-PADR(11:20)    TO LINE-ADCITY                       
027600       MOVE LINE-ADCITY              TO WS-ADCITY                         
027700     END-IF                                                               
027800     PERFORM S05-PUT-REPORT-LINE                                          
027900     .                                                                    
028000     EJECT                                                                
028100 Z-FINIT SECTION.                                                         
028200                                                                          
028300                                                                          
028400     CLOSE W47930                                                         
028500                                                                          
028600     SKIP2                                                                
028700     MOVE 'S' TO POSTSUM-OPKOD                                            
028800     CALL POSTSUM USING POSTSUM-PARM                                      
028900     .                                                                    
029000     EJECT                                                                
029100 S01-LAES-W47930  SECTION.                                                
029200     SKIP2                                                                
029300     READ W47930 INTO IN-AREA                                             
029400     AT END                                                               
029500****    MOVE HIGH-VALUE TO IN-ID                                          
029600        SET END-OF-W47931 TO TRUE                                         
029700                                                                          
029800     NOT AT END                                                           
029900        MOVE 'W47930' TO POSTSUM-FDNAMN                                   
030000        MOVE 'W47930D1' TO POSTSUM-DDNAMN2                                
030100        MOVE 'IN-'     TO POSTSUM-TRANSTYP                                
030200        CALL POSTSUM USING POSTSUM-PARM                                   
030300     END-READ                                                             
030400     .                                                                    
030500     EJECT                                                                
030600 S05-SEND-OPEN SECTION.                                                   
030700     MOVE    ' S05-SEND-OPEN SECTION '      TO PGM-POS                    
030800                                                                          
030900     MOVE 'CARPARTS.DAP.DISTRDOC'         TO SEND-ADDISPABS               
031000     MOVE 'OPEN'                          TO SEND-KDFUNC                  
031100     CALL WZ01SEND USING SEND-CONTROL-AREA                                
031200                         SEND-OPEN-AREA                                   
031300     IF SEND-KDRC > ZERO                                                  
031400       MOVE SEND-KDRC                     TO KDRC-DISPLAY                 
031500       STRING 'WZ01SEND OPEN ERROR RC=' KDRC-DISPLAY                      
031600       DELIMITED BY SIZE INTO ERROR-TEXT                                  
031700       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
031800     END-IF                                                               
031900                                                                          
032000     .                                                                    
032100     SKIP3                                                                
032200 S05-PUT-HEADER SECTION.                                                  
032300     MOVE    ' S05-PUT-HEADER SECTION '     TO PGM-POS                    
032400                                                                          
032500     MOVE 'PUT'                           TO SEND-KDFUNC                  
032600     MOVE WZ04-SEND-IDCOM                 TO SEND-IDCOM                   
032700     MOVE LENGTH OF HDR-AREA              TO SEND-KVDLEN                  
032800     CALL WZ01SEND USING SEND-CONTROL-AREA                                
032900                         SEND-KVDLEN                                      
033000                         HDR-AREA                                         
033100     IF SEND-KDRC > ZERO                                                  
033200       MOVE SEND-KDRC                     TO KDRC-DISPLAY                 
033300       STRING 'WZ01SEND PUT ERROR RC=' KDRC-DISPLAY                       
033400       DELIMITED BY SIZE INTO ERROR-TEXT                                  
033500       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
033600     END-IF                                                               
033700*    DISPLAY ' HEAD-AREA ' HDR-AREA                                       
033800     .                                                                    
033900     EJECT                                                                
034000 S05-PUT-REPORT-LINE    SECTION.                                          
034100     MOVE    ' S05-PUT-REPORT-LINE '        TO PGM-POS                    
034200                                                                          
034300     MOVE 'PUT'                           TO SEND-KDFUNC                  
034400     MOVE WZ04-SEND-IDCOM                 TO SEND-IDCOM                   
034500     MOVE LENGTH OF DOC-LINE-AREA         TO SEND-KVDLEN                  
034600     CALL WZ01SEND USING SEND-CONTROL-AREA                                
034700                         SEND-KVDLEN                                      
034800                         DOC-LINE-AREA                                    
034900     IF SEND-KDRC > ZERO                                                  
035000       MOVE SEND-KDRC                     TO KDRC-DISPLAY                 
035100       STRING 'WZ01SEND PUT ERROR RC=' KDRC-DISPLAY                       
035200       DELIMITED BY SIZE INTO ERROR-TEXT                                  
035300       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
035400     END-IF                                                               
035500*    DISPLAY ' LINE-AREA' DOC-LINE-AREA                                   
035600     .                                                                    
035700     SKIP3                                                                
035800 S05-SEND-CLOSE SECTION.                                                  
035900     MOVE    ' S05-SEND-CLOSE SECTION '     TO PGM-POS                    
036000                                                                          
036100     MOVE 'CLOSE'                         TO SEND-KDFUNC                  
036200     MOVE WZ04-SEND-IDCOM                 TO SEND-IDCOM                   
036300     CALL WZ01SEND USING SEND-CONTROL-AREA                                
036400                                                                          
036500*    DISPLAY 'SEND-IDCOM' SEND-IDCOM                                      
036600                                                                          
036700     IF SEND-KDRC > ZERO                                                  
036800       MOVE SEND-KDRC                     TO KDRC-DISPLAY                 
036900       STRING 'WZ01SEND CLOSE ERROR RC=' KDRC-DISPLAY                     
037000       DELIMITED BY SIZE INTO ERROR-TEXT                                  
037100       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
037200     END-IF                                                               
037300     .                                                                    
037400 IMS-GU-WDB601    SECTION.                                                
037500     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
037600          DELIMITED BY SIZE INTO SSA1                                     
037700     MOVE '  GE' TO GODK-STATUSKODER                                      
037800     CALL CBLTDLI USING GU  WDB6-PCB DLI-IO-AREA-B601 SSA1                
037900     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
038000     PERFORM IMS-STATUSKONTROLL                                           
038100     .                                                                    
038200     EJECT                                                                
038300 IMS-STATUSKONTROLL SECTION.                                              
038400                                                                          
038500     SET STATUS-IX TO 1                                                   
038600     SEARCH GODK-STATUS AT END CALL FELLOG                                
038700        WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                          
038800        CONTINUE                                                          
038900     END-SEARCH                                                           
039000     .                                                                    
