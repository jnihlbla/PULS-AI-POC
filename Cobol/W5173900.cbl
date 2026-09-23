000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W5173900.                                                
000300 AUTHOR.         ASPFJÄLL MARKUS.                                         
000400 DATE-WRITTEN.   05/06/03.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700                                                                          
000800*    FUNKTION:                                                            
000900*        LÄSER IN FIL W51738 OCH SKAPAR DAP POSTER                        
001000*        WEEKLY ADJUSTMENTS  POSTER                                       
001100*                                                                         
001200                                                                          
001300     SKIP3                                                                
001400 ENVIRONMENT DIVISION.                                                    
001500     SKIP2                                                                
001600 INPUT-OUTPUT SECTION.                                                    
001700                                                                          
001800 FILE-CONTROL.                                                            
001900     SKIP2                                                                
002000*          --- FIL MED WEEKLY ADJUSTMENTS  POSTER                         
002100     SELECT W51739                     ASSIGN TO W51739D1.                
002200     SKIP2                                                                
002500     EJECT                                                                
002600 DATA DIVISION.                                                           
002700     SKIP3                                                                
002800 FILE SECTION.                                                            
002900     SKIP3                                                                
003000 FD  W51739                                                               
003100     RECORDING       F                                                    
003200     BLOCK CONTAINS  0.                                                   
003300                                                                          
003400*01  -COPY W517391      -L.                                               
003500     SKIP3                                                                
004100     EJECT                                                                
004200 WORKING-STORAGE SECTION.                                                 
004300                                                                          
004400 77  IDPGM                       PIC X(8)    VALUE 'W5173900'.            
004500 77  JA                          PIC X       VALUE 'J'.                   
004600 77  NEJ                         PIC X       VALUE 'N'.                   
004610 77  WS-ADRESS                   PIC X(50)                                
004611                   VALUE 'CARPARTS.DAP.DISTRDOC'.                         
004612 77  WZ04-SEND-IDCOM             PIC S9(9)   COMP VALUE +0.               
004614 77  FELTEXT                     PIC X(80) VALUE SPACE.                   
004615 77  ERROR-TEXT                  PIC X(80)  VALUE SPACE.                  
004616 77  KDRC-DISPLAY                PIC Z(5).                                
004617                                                                          
004630 01  WS-YYMMDDHHMM.                                                       
004640     03 WS-YYMMDD                PIC  9(6).                               
004650     03 WS-TIME                  PIC  9(4).                               
004651                                                                          
004652 01  WS-HHMMSSTH.                                                         
004653     03 WS-HHMM                  PIC  9(4).                               
004654     03 WS-SSTH                  PIC  9(4).                               
004655                                                                          
004656                                                                          
004660 01  WS-IDDC-WEB                 PIC X(2) VALUE SPACE.                    
004661*--------------------------------------- NYCKLAR TILL BASERNA             
004662                                                                          
004663 01  W-IDDC-B6-X.                                                         
004664     03 W-IDDC-B6        PIC X(2).                                        
004665                                                                          
004666     EJECT                                                                
004667*---------------------------------------                                  
004670                                                                          
004700     SKIP2                                                                
004800 01  FELTEXT.                                                             
004900     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
005000     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
005100                                                                          
005200 77  W51739-EOF-SW               PIC X       VALUE 'N'.                   
005300     88  END-OF-W51739                       VALUE 'J'.                   
005400     EJECT                                                                
005500 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
005600 01  FILLER REDEFINES DAGENS-DATUM.                                       
005700     03  DAGENS-DATUM-AAR        PIC 9(2).                                
005800     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
005900     03  DAGENS-DATUM-DAG        PIC 9(2).                                
006000     EJECT                                                                
006100 01  DYNAMISKA-SUBPROGRAM.                                                
006200*                                                                         
006300     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
006400     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
006500     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
006600     03  WZ01SEND                PIC X(8)    VALUE 'WZ01SEND'.            
006610     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
006620     03  ABEND-CODE          PIC S9(4)   COMP  VALUE +0  SYNC.            
006700     EJECT                                                                
006710*    --- PARAMETERS TO ABEND                                              
006720                                                                          
006730 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
006740 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
006750 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
006760                                                                          
006800*    --- PARAMETRAR TILL POSTSUM                                          
006900*                                                                         
007000*01  -COPY W0005   -PRE  POSTSUM-                                         
007100     EJECT                                                                
007110 01  FILLER                      PIC X(16)   VALUE 'WZ01SEND'.            
007200*01  -COPY WZ01SEND                                                       
007300     EJECT                                                                
007400 01  IN-AREA-START               PIC X(24)   VALUE                        
007500                                             'IN-AREA-START'.             
007600     SKIP2                                                                
007700                                                                          
007800*01  AREA -COPY W517391     -PRE IN-                                      
007900     EJECT                                                                
008000 01  UT-AREA-START               PIC X(24)   VALUE                        
008100                                             'UT-AREA-START'.             
008200     SKIP2                                                                
008300 01  HDR-AREA.                                                            
008400*   03  -COPY WZ01REQU -PRE HDR-                                          
008410*   03  -COPY WZ04HDR                                                     
008500*                                                                         
008510 01  LINE-AREA                   PIC X(24)    VALUE 'LINE-AREA'.          
008520 01  DOC-LINE-AREA.                                                       
008530*    03 -COPY W517392 -PRE LINE-                                          
008600     EJECT                                                                
008700 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
008800     SKIP3                                                                
008900 01  FILLER               PIC X(16)   VALUE 'WDB601 AREA'.                
009000 01   DLI-IO-AREA-B601.                                                   
009010*     03  -COPY WDB601                                                    
009020                                                                          
009030     SKIP3                                                                
009100*    --- STATUS-KOD FRÅN IMS                                              
009200 01  STATUS-WS                   PIC XX.                                  
009300     88  SEGMENT-FINNS                       VALUE '  '.                  
009400     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
009500     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
009600     88  SEGMENT-SLUT                        VALUE 'GB'.                  
009700     88  IMS-EJ-OK                           VALUE 'XD'.                  
009800     SKIP2                                                                
009900 01  GODK-STATUSKODER.                                                    
010000     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
010100     SKIP3                                                                
010200 01  SSA1                        PIC X(64).                               
010300 01  SSA2                        PIC X(64).                               
010400     EJECT                                                                
010500*    --- IMS FUNKTIONSKODER                                               
010600*01  -COPY W0003                                                          
010700     EJECT                                                                
010900                                                                          
011100 LINKAGE SECTION.                                                         
011410*01  -COPY W0009   -PRE MSG-                                              
011420     EJECT                                                                
011430 01  DAP-PCB              PIC X.                                          
011440     EJECT                                                                
011450*01  -COPY W0008 -PRE WDB6-                                               
011460     05  FILLER           PIC X.                                          
011470                                                                          
011500 PROCEDURE DIVISION  USING MSG-PCB DAP-PCB WDB6-PCB.                      
011600 MAIN SECTION.                                                            
011700     ENTRY 'DLITCBL' USING MSG-PCB DAP-PCB WDB6-PCB.                      
011800                                                                          
012400     SKIP2                                                                
012500     PERFORM A-INIT                                                       
012600     PERFORM S01-LAES-W51739                                              
012610     IF NOT END-OF-W51739                                                 
012611       MOVE IN-IDDC TO W-IDDC-B6                                          
012612       PERFORM IMS-GU-WDB601                                              
012613       IF DCS-FLWEBDC = JA                                                
012620         PERFORM BA-SKAPA-HEADER                                          
012640           MOVE IN-IDDC    TO WS-IDDC-WEB                                 
012700           PERFORM UNTIL END-OF-W51739                                    
012710           MOVE IN-IDDC TO W-IDDC-B6                                      
012720           PERFORM IMS-GU-WDB601                                          
012730           IF DCS-FLWEBDC = JA                                            
013300             PERFORM B-WEB-LISTA                                          
013310           END-IF                                                         
013400           PERFORM S01-LAES-W51739                                        
013500         END-PERFORM                                                      
013600         PERFORM S05-SEND-CLOSE                                           
013700       END-IF                                                             
013710     END-IF                                                               
013800     PERFORM Z-FINIT                                                      
014000     MOVE ZERO TO RETURN-CODE                                             
014100     GOBACK                                                               
014200     .                                                                    
014300     EJECT                                                                
014400 A-INIT SECTION.                                                          
014500     SKIP2                                                                
014600                                                                          
014700     OPEN INPUT W51739                                                    
015100                                                                          
015110     ACCEPT WS-YYMMDD      FROM DATE                                      
015120     ACCEPT WS-HHMMSSTH    FROM TIME                                      
015130     MOVE   WS-HHMM      TO WS-TIME                                       
015200     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
015300     .                                                                    
015400     EJECT                                                                
015410 B-WEB-LISTA  SECTION.                                                    
015420     IF IN-IDDC NOT = WS-IDDC-WEB                                         
015430       MOVE IN-IDDC TO WS-IDDC-WEB                                        
015440       PERFORM S05-SEND-CLOSE                                             
015450       PERFORM BA-SKAPA-HEADER                                            
015460       PERFORM BC-SKAPA-LINE                                              
015470     ELSE                                                                 
015480       PERFORM BC-SKAPA-LINE                                              
015490     END-IF                                                               
015491     .                                                                    
015492     SKIP3                                                                
015493 BA-SKAPA-HEADER SECTION.                                                 
015494***  IF WZ04-SEND-IDCOM = ZERO                                            
015495       PERFORM S05-SEND-OPEN                                              
015496       MOVE SEND-IDCOM TO WZ04-SEND-IDCOM                                 
015497***  END-IF                                                               
015498                                                                          
015501     MOVE 001                        TO HDR-REQU-IDMSGVER                 
015502     MOVE 'E'                        TO HDR-REQU-KDPGMACT                 
015503     MOVE IDPGM                      TO HDR-REQU-IDUSER                   
015504                                                                          
015506     MOVE SPACE                      TO HDR-IDOUTREC                      
015508     MOVE 'WEEKLY-ADJ'               TO HDR-IDOUTTYPE                     
015509     MOVE IN-IDDC                    TO HDR-IDOUTREC(1:2)                 
015510     MOVE 'W51739'                   TO HDR-IDOUTREC(3:8)                 
015511     MOVE WS-YYMMDDHHMM              TO HDR-IDLIST                        
015512*HDR                                                                      
015513     PERFORM S05-PUT-HEADER                                               
015514     .                                                                    
015515     SKIP3                                                                
015516 BC-SKAPA-LINE SECTION.                                                   
015517                                                                          
015518     MOVE  IN-IDAFPRCD               TO LINE-IDAFPRCD                     
015519     MOVE  IN-IDDC                   TO LINE-IDDC                         
015520     MOVE  IN-TIVV                   TO LINE-TIVV                         
015521     MOVE  IN-BEPRODSL               TO LINE-BEPRODSL                     
015522     MOVE  IN-BETEXT                 TO LINE-BETEXT                       
015523     MOVE  IN-SUARTSTD-WEEK          TO LINE-SUARTSTD-WEEK                
015524     MOVE  IN-SUARTSTD-WEEKAVG       TO LINE-SUARTSTD-WEEKAVG             
015525     MOVE  IN-SUARTSTD-TOT           TO LINE-SUARTSTD-TOT                 
015526     MOVE  IN-REDIFF                 TO LINE-REDIFF                       
015527     MOVE  IN-SUARTSTD-DIFF          TO LINE-SUARTSTD-DIFF                
015528     MOVE  IN-REDIFF-LYEAR           TO LINE-REDIFF-LYEAR                 
015533     PERFORM S05-PUT-REPORT-LINE                                          
015534     .                                                                    
015535     EJECT                                                                
015540 Z-FINIT SECTION.                                                         
015600                                                                          
015700                                                                          
015800     CLOSE W51739                                                         
016000                                                                          
016100     SKIP2                                                                
016200     MOVE 'S' TO POSTSUM-OPKOD                                            
016300     CALL POSTSUM USING POSTSUM-PARM                                      
016400     .                                                                    
016500     EJECT                                                                
016600 S01-LAES-W51739  SECTION.                                                
016700     SKIP2                                                                
016800     READ W51739 INTO IN-AREA                                             
016900     AT END                                                               
017000****    MOVE HIGH-VALUE TO IN-ID                                          
017100        SET END-OF-W51739 TO TRUE                                         
017200                                                                          
017300     NOT AT END                                                           
017400        MOVE 'W51739' TO POSTSUM-FDNAMN                                   
017500        MOVE 'W51739D1' TO POSTSUM-DDNAMN2                                
017600        MOVE 'IN-'     TO POSTSUM-TRANSTYP                                
017700        CALL POSTSUM USING POSTSUM-PARM                                   
017800     END-READ                                                             
017900     .                                                                    
018000     EJECT                                                                
018100 S05-SEND-OPEN SECTION.                                                   
018200                                                                          
018300     MOVE 'CARPARTS.DAP.DISTRDOC'         TO SEND-ADDISPABS               
018400     MOVE 'OPEN'                          TO SEND-KDFUNC                  
018500     CALL WZ01SEND USING SEND-CONTROL-AREA                                
018600                         SEND-OPEN-AREA                                   
018700     IF SEND-KDRC > ZERO                                                  
018800       MOVE SEND-KDRC                     TO KDRC-DISPLAY                 
018900       STRING 'WZ01SEND OPEN ERROR RC=' KDRC-DISPLAY                      
019000       DELIMITED BY SIZE INTO ERROR-TEXT                                  
019010       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
019020     END-IF                                                               
019030     .                                                                    
019040     SKIP3                                                                
019050 S05-PUT-HEADER SECTION.                                                  
019060                                                                          
019070     MOVE 'PUT'                           TO SEND-KDFUNC                  
019080     MOVE WZ04-SEND-IDCOM                 TO SEND-IDCOM                   
019090     MOVE LENGTH OF HDR-AREA              TO SEND-KVDLEN                  
019091     CALL WZ01SEND USING SEND-CONTROL-AREA                                
019092                         SEND-KVDLEN                                      
019093                         HDR-AREA                                         
019094     IF SEND-KDRC > ZERO                                                  
019095       MOVE SEND-KDRC                     TO KDRC-DISPLAY                 
019096       STRING 'WZ01SEND PUT ERROR RC=' KDRC-DISPLAY                       
019097       DELIMITED BY SIZE INTO ERROR-TEXT                                  
019098       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
019099     END-IF                                                               
019100     .                                                                    
019101     EJECT                                                                
019102 S05-PUT-REPORT-LINE    SECTION.                                          
019103                                                                          
019104     MOVE 'PUT'                           TO SEND-KDFUNC                  
019105     MOVE WZ04-SEND-IDCOM                 TO SEND-IDCOM                   
019106     MOVE LENGTH OF DOC-LINE-AREA         TO SEND-KVDLEN                  
019107     CALL WZ01SEND USING SEND-CONTROL-AREA                                
019108                         SEND-KVDLEN                                      
019109                         DOC-LINE-AREA                                    
019110     IF SEND-KDRC > ZERO                                                  
019111       MOVE SEND-KDRC                     TO KDRC-DISPLAY                 
019112       STRING 'WZ01SEND PUT ERROR RC=' KDRC-DISPLAY                       
019113       DELIMITED BY SIZE INTO ERROR-TEXT                                  
019114       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
019115     END-IF                                                               
019116     .                                                                    
019117     SKIP3                                                                
019118 S05-SEND-CLOSE SECTION.                                                  
019119                                                                          
019120     MOVE 'CLOSE'                         TO SEND-KDFUNC                  
019121     MOVE WZ04-SEND-IDCOM                 TO SEND-IDCOM                   
019122     CALL WZ01SEND USING SEND-CONTROL-AREA                                
019123     .                                                                    
019124 IMS-GU-WDB601    SECTION.                                                
019125     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
019126          DELIMITED BY SIZE INTO SSA1                                     
019127     MOVE '  GE' TO GODK-STATUSKODER                                      
019128     CALL CBLTDLI USING GU  WDB6-PCB DLI-IO-AREA-B601 SSA1                
019129     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
019130     PERFORM IMS-STATUSKONTROLL                                           
019140     .                                                                    
019150     EJECT                                                                
019160 IMS-STATUSKONTROLL SECTION.                                              
019170                                                                          
019180     SET STATUS-IX TO 1                                                   
019190     SEARCH GODK-STATUS AT END CALL FELLOG                                
019200        WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                          
019300        CONTINUE                                                          
019400     END-SEARCH                                                           
019500     .                                                                    
