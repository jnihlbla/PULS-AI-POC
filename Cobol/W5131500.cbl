000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W5131500.                                                
000300 AUTHOR.         ASPFJÄLL MARKUS.                                         
000400 DATE-WRITTEN.   08/03/05.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700                                                                          
000800*    FUNKTION:                                                            
000900*    LÄSER IN FIL W51315 OCH SKAPAR DAP POSTER FÖR LDC-FOLLOW UP          
001000*    LÄSER IN FIL W51313 OCH SKAPAR DAP POSTER SOM MAIL SKICK             
001100*        WEEKLY REPORT PER AREA                                           
001200*        UPPFÖLJNINGSLISTA , HUR MYCKET SOM ÄR INVENTERAT PER AREA        
001300*        LCD FÅR SINA PÅ FOLLOW UP PÅ WEBBEN                              
001400*        ÖVRIGA DC:N SKICKA DE SOM MAIL IFRÅN DAP                         
001500                                                                          
001600     SKIP3                                                                
001700 ENVIRONMENT DIVISION.                                                    
001800     SKIP2                                                                
001900 INPUT-OUTPUT SECTION.                                                    
002000                                                                          
002100 FILE-CONTROL.                                                            
002200     SKIP2                                                                
002300*          --- FIL MED WEEKLY REPORT       POSTER                         
002400     SELECT W51315                     ASSIGN TO W51315D1.                
002500     SKIP2                                                                
002600*          --- FIL MED WEEKLY REPORT       POSTER                         
002700     SELECT W51313                     ASSIGN TO W51315D2.                
002800     SKIP2                                                                
002900     EJECT                                                                
003000 DATA DIVISION.                                                           
003100     SKIP3                                                                
003200 FILE SECTION.                                                            
003300     SKIP3                                                                
003400 FD  W51315                                                               
003500     RECORDING       F                                                    
003600     BLOCK CONTAINS  0.                                                   
003700                                                                          
003800*01  -COPY W51315       -L.                                               
003900     SKIP3                                                                
004000     SKIP3                                                                
004100 FD  W51313                                                               
004200     RECORDING       F                                                    
004300     BLOCK CONTAINS  0.                                                   
004400                                                                          
004500 01  IN-POST-2      PIC X(126).                                           
004600     SKIP3                                                                
004700     EJECT                                                                
004800 WORKING-STORAGE SECTION.                                                 
004900                                                                          
005000 77  IDPGM                       PIC X(8)    VALUE 'W5131500'.            
005100 77  JA                          PIC X       VALUE 'J'.                   
005200 77  NEJ                         PIC X       VALUE 'N'.                   
005300 77  WS-ADRESS                   PIC X(50)                                
005400                   VALUE 'CARPARTS.DAP.DISTRDOC'.                         
005500 77  WZ04-SEND-IDCOM             PIC S9(9)   COMP VALUE +0.               
005600 77  FELTEXT                     PIC X(80) VALUE SPACE.                   
005700 77  ERROR-TEXT                  PIC X(80)  VALUE SPACE.                  
005800 77  KDRC-DISPLAY                PIC Z(5).                                
005900 77  W-TYP                       PIC X(4).                                
006000                                                                          
006100 01  WS-YYMMDDHHMM.                                                       
006200     03 WS-YYMMDD                PIC  9(6).                               
006300     03 WS-TIME                  PIC  9(4).                               
006400                                                                          
006500 01  WS-HHMMSSTH.                                                         
006600     03 WS-HHMM                  PIC  9(4).                               
006700     03 WS-SSTH                  PIC  9(4).                               
006800                                                                          
006900                                                                          
007000 01  WS-IDDC-WEB                 PIC X(2) VALUE SPACE.                    
007100*--------------------------------------- NYCKLAR TILL BASERNA             
007200                                                                          
007300 01  W-IDDC-B6-X.                                                         
007400     03 W-IDDC-B6        PIC X(2).                                        
007500                                                                          
007600     EJECT                                                                
007700*---------------------------------------                                  
007800                                                                          
007900     SKIP2                                                                
008000 01  FELTEXT.                                                             
008100     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
008200     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
008300                                                                          
008400 77  W51315-EOF-SW               PIC X       VALUE 'N'.                   
008500     88  END-OF-W51315                       VALUE 'J'.                   
008600                                                                          
008700 77  W51313-EOF-SW               PIC X       VALUE 'N'.                   
008800     88  END-OF-W51313                       VALUE 'J'.                   
008900     EJECT                                                                
009000 77  DAP-STATUS                  PIC X       VALUE 'N'.                   
009100     88  DAP-OPEN                            VALUE 'J'.                   
009200     88  DAP-CLOSE                           VALUE 'N'.                   
009300     EJECT                                                                
009400 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
009500 01  FILLER REDEFINES DAGENS-DATUM.                                       
009600     03  DAGENS-DATUM-AAR        PIC 9(2).                                
009700     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
009800     03  DAGENS-DATUM-DAG        PIC 9(2).                                
009900     EJECT                                                                
010000 01  DYNAMISKA-SUBPROGRAM.                                                
010100*                                                                         
010200     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
010300     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
010400     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
010500     03  WZ01SEND                PIC X(8)    VALUE 'WZ01SEND'.            
010600     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
010700     03  ABEND-CODE          PIC S9(4)   COMP  VALUE +0  SYNC.            
010800     EJECT                                                                
010900*    --- PARAMETERS TO ABEND                                              
011000                                                                          
011100 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
011200 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
011300 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
011400                                                                          
011500*    --- PARAMETRAR TILL POSTSUM                                          
011600*                                                                         
011700*01  -COPY W0005   -PRE  POSTSUM-                                         
011800     EJECT                                                                
011900 01  FILLER                      PIC X(16)   VALUE 'WZ01SEND'.            
012000*01  -COPY WZ01SEND                                                       
012100     EJECT                                                                
012200 01  IN-AREA-START               PIC X(24)   VALUE                        
012300                                             'IN-AREA-START'.             
012400     SKIP2                                                                
012500                                                                          
012600*01  AREA -COPY W51315      -PRE IN-                                      
012700     EJECT                                                                
012800                                                                          
012900 01  IN-AREA-2-START               PIC X(24)   VALUE                      
013000                                        'IN2-AREA-START'.                 
013100 01  IN2-AREA.                                                            
013200     03  IN2-BRYTTECKEN         PIC X.                                    
013300     03  IN2-LISTNR             PIC X(6).                                 
013400     03  IN2-FILLER             PIC X(2).                                 
013500     03  IN2-IDDC               PIC X(2).                                 
013600     03  IN2-DATA               PIC X(116).                               
013700     EJECT                                                                
013800 01  UT-AREA-START               PIC X(24)   VALUE                        
013900                                             'UT-AREA-START'.             
014000     SKIP2                                                                
014100 01  HDR-AREA.                                                            
014200*   03  -COPY WZ01REQU -PRE HDR-                                          
014300*   03  -COPY WZ04HDR                                                     
014400*                                                                         
014500 01  LINE-AREA                   PIC X(24)    VALUE 'LINE-AREA'.          
014600 01  DOC-LINE-AREA.                                                       
014700*    03 -COPY W513151 -PRE LINE-                                          
014800     EJECT                                                                
014900                                                                          
015000 01  LINE2-AREA                   PIC X(24)    VALUE 'LINE2-AREA'.        
015100 01  DOC-LINE2-AREA.                                                      
015200     03 LINE2-DATA                PIC X(126).                             
015300     EJECT                                                                
015400 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
015500     SKIP3                                                                
015600 01  FILLER               PIC X(16)   VALUE 'WDB601 AREA'.                
015700 01   DLI-IO-AREA-B601.                                                   
015800*     03  -COPY WDB601                                                    
015900                                                                          
016000     SKIP3                                                                
016100*    --- STATUS-KOD FRÅN IMS                                              
016200 01  STATUS-WS                   PIC XX.                                  
016300     88  SEGMENT-FINNS                       VALUE '  '.                  
016400     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
016500     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
016600     88  SEGMENT-SLUT                        VALUE 'GB'.                  
016700     88  IMS-EJ-OK                           VALUE 'XD'.                  
016800     SKIP2                                                                
016900 01  GODK-STATUSKODER.                                                    
017000     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
017100     SKIP3                                                                
017200 01  SSA1                        PIC X(64).                               
017300 01  SSA2                        PIC X(64).                               
017400     EJECT                                                                
017500*    --- IMS FUNKTIONSKODER                                               
017600*01  -COPY W0003                                                          
017700     EJECT                                                                
017800                                                                          
017900 LINKAGE SECTION.                                                         
018000*01  -COPY W0009   -PRE MSG-                                              
018100     EJECT                                                                
018200 01  DAP-PCB              PIC X.                                          
018300     EJECT                                                                
018400*01  -COPY W0008 -PRE WDB6-                                               
018500     05  FILLER           PIC X.                                          
018600                                                                          
018700 PROCEDURE DIVISION  USING MSG-PCB DAP-PCB WDB6-PCB.                      
018800 MAIN SECTION.                                                            
018900     ENTRY 'DLITCBL' USING MSG-PCB DAP-PCB WDB6-PCB.                      
019000                                                                          
019100     SKIP2                                                                
019200     PERFORM A-INIT                                                       
019300     PERFORM S01-LAES-W51315                                              
019400     IF NOT END-OF-W51315                                                 
019500       MOVE 'DAP' TO W-TYP                                                
019600         PERFORM UNTIL END-OF-W51315                                      
019700           MOVE IN-IDDC TO W-IDDC-B6                                      
019800           MOVE IN-IDDC TO WS-IDDC-WEB                                    
019900           PERFORM IMS-GU-WDB601                                          
020000           IF DCS-FLWEBDC = JA                                            
020100             PERFORM BA-SKAPA-HEADER                                      
020200               MOVE IN-IDDC    TO WS-IDDC-WEB                             
020300               PERFORM UNTIL END-OF-W51315                                
020400                 MOVE IN-IDDC TO W-IDDC-B6                                
020500                 PERFORM IMS-GU-WDB601                                    
020600                 IF DCS-FLWEBDC = JA                                      
020700                   PERFORM B-WEB-LISTA                                    
020800                 END-IF                                                   
020900                 PERFORM S01-LAES-W51315                                  
021000               END-PERFORM                                                
021100             PERFORM S05-SEND-CLOSE                                       
021200           END-IF                                                         
021300           IF NOT END-OF-W51315                                           
021400             PERFORM S01-LAES-W51315                                      
021500           END-IF                                                         
021600         END-PERFORM                                                      
021700     END-IF                                                               
021800*** SKAPA MAIL INFORMATION TILL D&P                                       
021900     PERFORM S01-LAES-W51313                                              
022000     IF NOT END-OF-W51313                                                 
022100      MOVE 'MAIL' TO W-TYP                                                
022200      PERFORM UNTIL END-OF-W51313                                         
022300       IF IN2-LISTNR = 'W51313'                                           
022400        MOVE IN2-IDDC TO W-IDDC-B6                                        
022500        MOVE IN2-IDDC TO WS-IDDC-WEB                                      
022600        PERFORM IMS-GU-WDB601                                             
022700        IF DCS-FLWEBDC = NEJ                                              
022800          PERFORM CA-SKAPA-HEADER                                         
022900          PERFORM C-WEB-LISTA                                             
023000          PERFORM S01-LAES-W51313                                         
023100                                                                          
023200          PERFORM UNTIL IN2-LISTNR = 'W51313' OR END-OF-W51313            
023300            PERFORM C-WEB-LISTA                                           
023400            PERFORM S01-LAES-W51313                                       
023500          END-PERFORM                                                     
023600          IF IN2-LISTNR = 'W51313'                                        
023700            PERFORM S05-SEND-CLOSE                                        
023800          END-IF                                                          
023900        ELSE                                                              
024000          PERFORM S01-LAES-W51313                                         
024100        END-IF                                                            
024200       ELSE                                                               
024300        PERFORM S01-LAES-W51313                                           
024400       END-IF                                                             
024500      END-PERFORM                                                         
024600     END-IF                                                               
024700     IF DAP-OPEN                                                          
024800       PERFORM S05-SEND-CLOSE                                             
024900     END-IF                                                               
025000     PERFORM Z-FINIT                                                      
025100     MOVE ZERO TO RETURN-CODE                                             
025200     GOBACK                                                               
025300     .                                                                    
025400     EJECT                                                                
025500 A-INIT SECTION.                                                          
025600     SKIP2                                                                
025700                                                                          
025800     OPEN INPUT W51315                                                    
025900     OPEN INPUT W51313                                                    
026000                                                                          
026100     ACCEPT WS-YYMMDD      FROM DATE                                      
026200     ACCEPT WS-HHMMSSTH    FROM TIME                                      
026300     MOVE   WS-HHMM      TO WS-TIME                                       
026400     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
026500     .                                                                    
026600     EJECT                                                                
026700 B-WEB-LISTA  SECTION.                                                    
026800     IF IN-IDDC NOT = WS-IDDC-WEB                                         
026900       MOVE IN-IDDC TO WS-IDDC-WEB                                        
027000       PERFORM S05-SEND-CLOSE                                             
027100       PERFORM BA-SKAPA-HEADER                                            
027200       PERFORM BC-SKAPA-LINE                                              
027300     ELSE                                                                 
027400       PERFORM BC-SKAPA-LINE                                              
027500     END-IF                                                               
027600     .                                                                    
027700     SKIP3                                                                
027800 BA-SKAPA-HEADER SECTION.                                                 
027900***  IF WZ04-SEND-IDCOM = ZERO                                            
028000       PERFORM S05-SEND-OPEN                                              
028100       MOVE SEND-IDCOM TO WZ04-SEND-IDCOM                                 
028200***  END-IF                                                               
028300                                                                          
028400     MOVE 001                        TO HDR-REQU-IDMSGVER                 
028500     MOVE 'E'                        TO HDR-REQU-KDPGMACT                 
028600     MOVE IDPGM                      TO HDR-REQU-IDUSER                   
028700                                                                          
028800     MOVE SPACE                      TO HDR-IDOUTREC                      
028900     MOVE 'WEEKLY-AREA'              TO HDR-IDOUTTYPE                     
029000                                                                          
029100     MOVE IN-IDDC                    TO HDR-IDOUTREC(1:2)                 
029200     MOVE '      '                   TO HDR-IDOUTREC(3:8)                 
029300     MOVE WS-YYMMDDHHMM              TO HDR-IDLIST                        
029400*HDR                                                                      
029500     PERFORM S05-PUT-HEADER                                               
029600     .                                                                    
029700     SKIP3                                                                
029800 BC-SKAPA-LINE SECTION.                                                   
029900                                                                          
030000     MOVE  IN-IDAFPRCD               TO LINE-IDAFPRCD                     
030100     MOVE  IN-IDDC                   TO LINE-IDDC                         
030200     MOVE  IN-TIVV                   TO LINE-TIVV                         
030300     MOVE  IN-ADLAGOMR               TO LINE-ADLAGOMR                     
030400     MOVE  IN-BETEXT                 TO LINE-BETEXT                       
030500     MOVE  IN-SUARTSTD-WEEK          TO LINE-SUARTSTD-WEEK                
030600     MOVE  IN-SUARTSTD-WEEKAVG       TO LINE-SUARTSTD-WEEKAVG             
030700     MOVE  IN-SUARTSTD-TOT           TO LINE-SUARTSTD-TOT                 
030800     MOVE  IN-REDIFF                 TO LINE-REDIFF                       
030900     MOVE  IN-SUARTSTD-DIFF          TO LINE-SUARTSTD-DIFF                
031000     MOVE  IN-REDIFF-LYEAR           TO LINE-REDIFF-LYEAR                 
031100     PERFORM S05-PUT-REPORT-LINE                                          
031200     .                                                                    
031300     EJECT                                                                
031400 C-WEB-LISTA  SECTION.                                                    
031500*    IF IN2-IDDC NOT = WS-IDDC-WEB                                        
031600*      MOVE IN2-IDDC TO WS-IDDC-WEB                                       
031700*      PERFORM S05-SEND-CLOSE                                             
031800*      PERFORM CA-SKAPA-HEADER                                            
031900*      PERFORM CC-SKAPA-LINE                                              
032000*    ELSE                                                                 
032100       PERFORM CC-SKAPA-LINE                                              
032200*    END-IF                                                               
032300     .                                                                    
032400     SKIP3                                                                
032500 CA-SKAPA-HEADER SECTION.                                                 
032600***  IF WZ04-SEND-IDCOM = ZERO                                            
032700       PERFORM S05-SEND-OPEN                                              
032800       MOVE SEND-IDCOM TO WZ04-SEND-IDCOM                                 
032900       MOVE 'J' TO DAP-STATUS                                             
033000***  END-IF                                                               
033100                                                                          
033200     MOVE 001                        TO HDR-REQU-IDMSGVER                 
033300     MOVE 'E'                        TO HDR-REQU-KDPGMACT                 
033400     MOVE IDPGM                      TO HDR-REQU-IDUSER                   
033500                                                                          
033600     MOVE SPACE                      TO HDR-IDOUTREC                      
033700     MOVE 'WEEKLY-AREA2'             TO HDR-IDOUTTYPE                     
033800                                                                          
033900     MOVE IN2-IDDC                   TO HDR-IDOUTREC(1:2)                 
034000     MOVE '      '                   TO HDR-IDOUTREC(3:8)                 
034100     MOVE WS-YYMMDDHHMM              TO HDR-IDLIST                        
034200*HDR                                                                      
034300     PERFORM S05-PUT-HEADER                                               
034400     .                                                                    
034500     SKIP3                                                                
034600 CC-SKAPA-LINE SECTION.                                                   
034700                                                                          
034800     MOVE  IN2-AREA                  TO LINE2-DATA                        
034900                                                                          
035000     PERFORM S05-PUT-REPORT-LINE2                                         
035100     .                                                                    
035200     EJECT                                                                
035300 Z-FINIT SECTION.                                                         
035400                                                                          
035500                                                                          
035600     CLOSE W51315                                                         
035700     CLOSE W51313                                                         
035800                                                                          
035900     SKIP2                                                                
036000     MOVE 'S' TO POSTSUM-OPKOD                                            
036100     CALL POSTSUM USING POSTSUM-PARM                                      
036200     .                                                                    
036300     EJECT                                                                
036400 S01-LAES-W51315  SECTION.                                                
036500     SKIP2                                                                
036600     READ W51315 INTO IN-AREA                                             
036700     AT END                                                               
036800****    MOVE HIGH-VALUE TO IN-ID                                          
036900        SET END-OF-W51315 TO TRUE                                         
037000                                                                          
037100     NOT AT END                                                           
037200        MOVE 'W51315' TO POSTSUM-FDNAMN                                   
037300        MOVE 'W51315D1' TO POSTSUM-DDNAMN2                                
037400        MOVE 'IN-'     TO POSTSUM-TRANSTYP                                
037500        CALL POSTSUM USING POSTSUM-PARM                                   
037600     END-READ                                                             
037700     .                                                                    
037800     EJECT                                                                
037900 S01-LAES-W51313  SECTION.                                                
038000     SKIP2                                                                
038100     READ W51313 INTO IN2-AREA                                            
038200     AT END                                                               
038300****    MOVE HIGH-VALUE TO IN-ID                                          
038400        SET END-OF-W51313 TO TRUE                                         
038500                                                                          
038600     NOT AT END                                                           
038700        MOVE 'W51313' TO POSTSUM-FDNAMN                                   
038800        MOVE 'W51315D2' TO POSTSUM-DDNAMN2                                
038900        MOVE 'IN2-'     TO POSTSUM-TRANSTYP                               
039000        CALL POSTSUM USING POSTSUM-PARM                                   
039100     END-READ                                                             
039200     .                                                                    
039300     EJECT                                                                
039400 S05-SEND-OPEN SECTION.                                                   
039500                                                                          
039600     MOVE 'CARPARTS.DAP.DISTRDOC'         TO SEND-ADDISPABS               
039700     MOVE 'OPEN'                          TO SEND-KDFUNC                  
039800     CALL WZ01SEND USING SEND-CONTROL-AREA                                
039900                         SEND-OPEN-AREA                                   
040000     IF SEND-KDRC > ZERO                                                  
040100       MOVE SEND-KDRC                     TO KDRC-DISPLAY                 
040200       STRING 'WZ01SEND OPEN ERROR RC=' KDRC-DISPLAY                      
040300       DELIMITED BY SIZE INTO ERROR-TEXT                                  
040400       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
040500     END-IF                                                               
040600     .                                                                    
040700     SKIP3                                                                
040800 S05-PUT-HEADER SECTION.                                                  
040900                                                                          
041000     MOVE 'PUT'                           TO SEND-KDFUNC                  
041100     MOVE WZ04-SEND-IDCOM                 TO SEND-IDCOM                   
041200     MOVE LENGTH OF HDR-AREA              TO SEND-KVDLEN                  
041300     CALL WZ01SEND USING SEND-CONTROL-AREA                                
041400                         SEND-KVDLEN                                      
041500                         HDR-AREA                                         
041600     IF SEND-KDRC > ZERO                                                  
041700       MOVE SEND-KDRC                     TO KDRC-DISPLAY                 
041800       STRING 'WZ01SEND PUT ERROR RC=' KDRC-DISPLAY                       
041900       DELIMITED BY SIZE INTO ERROR-TEXT                                  
042000       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
042100     END-IF                                                               
042200     .                                                                    
042300     EJECT                                                                
042400 S05-PUT-REPORT-LINE    SECTION.                                          
042500                                                                          
042600     MOVE 'PUT'                           TO SEND-KDFUNC                  
042700     MOVE WZ04-SEND-IDCOM                 TO SEND-IDCOM                   
042800     MOVE LENGTH OF DOC-LINE-AREA         TO SEND-KVDLEN                  
042900     CALL WZ01SEND USING SEND-CONTROL-AREA                                
043000                         SEND-KVDLEN                                      
043100                         DOC-LINE-AREA                                    
043200     IF SEND-KDRC > ZERO                                                  
043300       MOVE SEND-KDRC                     TO KDRC-DISPLAY                 
043400       STRING 'WZ01SEND PUT ERROR RC=' KDRC-DISPLAY                       
043500       DELIMITED BY SIZE INTO ERROR-TEXT                                  
043600       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
043700     END-IF                                                               
043800     MOVE 'W51315'    TO POSTSUM-FDNAMN                                   
043900     MOVE WS-IDDC-WEB TO POSTSUM-DDNAMN2                                  
044000     MOVE W-TYP       TO POSTSUM-TRANSTYP                                 
044100     CALL POSTSUM USING POSTSUM-PARM                                      
044200     .                                                                    
044300     SKIP3                                                                
044400 S05-PUT-REPORT-LINE2   SECTION.                                          
044500                                                                          
044600     MOVE 'PUT'                           TO SEND-KDFUNC                  
044700     MOVE WZ04-SEND-IDCOM                 TO SEND-IDCOM                   
044800     MOVE LENGTH OF DOC-LINE2-AREA        TO SEND-KVDLEN                  
044900     CALL WZ01SEND USING SEND-CONTROL-AREA                                
045000                         SEND-KVDLEN                                      
045100                         DOC-LINE2-AREA                                   
045200     IF SEND-KDRC > ZERO                                                  
045300       MOVE SEND-KDRC                     TO KDRC-DISPLAY                 
045400       STRING 'WZ01SEND PUT ERROR RC=' KDRC-DISPLAY                       
045500       DELIMITED BY SIZE INTO ERROR-TEXT                                  
045600       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
045700     END-IF                                                               
045800        MOVE 'W51313'    TO POSTSUM-FDNAMN                                
045900        MOVE WS-IDDC-WEB TO POSTSUM-DDNAMN2                               
046000        MOVE W-TYP       TO POSTSUM-TRANSTYP                              
046100        CALL POSTSUM USING POSTSUM-PARM                                   
046200     .                                                                    
046300     SKIP3                                                                
046400 S05-SEND-CLOSE SECTION.                                                  
046500     MOVE 'N' TO DAP-STATUS                                               
046600     MOVE 'CLOSE'                         TO SEND-KDFUNC                  
046700     MOVE WZ04-SEND-IDCOM                 TO SEND-IDCOM                   
046800     CALL WZ01SEND USING SEND-CONTROL-AREA                                
046900     .                                                                    
047000 IMS-GU-WDB601    SECTION.                                                
047100     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
047200          DELIMITED BY SIZE INTO SSA1                                     
047300     MOVE '  GE' TO GODK-STATUSKODER                                      
047400     CALL CBLTDLI USING GU  WDB6-PCB DLI-IO-AREA-B601 SSA1                
047500     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
047600     PERFORM IMS-STATUSKONTROLL                                           
047700     .                                                                    
047800     EJECT                                                                
047900 IMS-STATUSKONTROLL SECTION.                                              
048000                                                                          
048100     SET STATUS-IX TO 1                                                   
048200     SEARCH GODK-STATUS AT END CALL FELLOG                                
048300        WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                          
048400        CONTINUE                                                          
048500     END-SEARCH                                                           
048600     .                                                                    
