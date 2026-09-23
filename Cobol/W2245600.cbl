000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W2245600.                                                
000300 AUTHOR.         STENING INGER.                                           
000400 DATE-WRITTEN.   13/03/14.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700                                                                          
000800*    FUNCTION:                                                            
000900*        DELETE AND REPLACE WDD9                                          
001000*                                                                         
001100*        THE PROGRAM UPDATES   WDD9                                       
001200*                                                                         
001210*                                                                         
001211*--------------------------------------------------------------           
001220* CHANGE LOG:                                                             
001230* 2015-09-17   ETRACKER 10209749    (TAG BORT WDD903)                     
001240*              ÄNDRA 2103/2403 ORSAK EXTRALEVERANSER OCH MERA.            
001250*                                                                         
001260*                                                                         
001300                                                                          
001400     SKIP3                                                                
001500 ENVIRONMENT DIVISION.                                                    
001600     SKIP2                                                                
001700 INPUT-OUTPUT SECTION.                                                    
001800                                                                          
001900 FILE-CONTROL.                                                            
002000     SKIP2                                                                
002100*          --- INPUT FILE WITH DELETE AND REPLACE RECORDS                 
002200     SELECT W2245501                   ASSIGN TO W22456D1.                
002300     EJECT                                                                
002400 DATA DIVISION.                                                           
002500     SKIP3                                                                
002600 FILE SECTION.                                                            
002700     SKIP3                                                                
002800 FD  W2245501                                                             
002900     RECORDING       F                                                    
003000     BLOCK CONTAINS  0.                                                   
003100                                                                          
003200*01  -COPY W2245501      -L.                                              
003300     EJECT                                                                
003400 WORKING-STORAGE SECTION.                                                 
003500                                                                          
003600 77  IDPGM                       PIC X(8)    VALUE 'W2245600'.            
003700 77  CURRENT-SECTION             PIC X(30)  VALUE SPACE.                  
003800 77  DBS-SECTION                 PIC X(30)  VALUE SPACE.                  
003900 01  CHKP-VAR.                                                            
004000     03 CHKP-MSG-IO-AREA-LENGTH  PIC S9(9)   VALUE +32 COMP SYNC.         
004100     03 CHKP-MSG-IO-AREA         PIC X(32)   VALUE SPACE.                 
004200     03 CHKP-AREA-LENGTH         PIC S9(9)   VALUE +32 COMP SYNC.         
004300     03 CHKP-AREA                PIC X(32)   VALUE SPACE.                 
004400     03 CHKP-ANT                 PIC S9(3)   VALUE +0   COMP-3.           
004500     03 CHKP-MAX                 PIC S9(3)   VALUE +500 COMP-3.           
004600 01  W-KVPOST-IN                 PIC S9(9)  VALUE ZERO COMP SYNC.         
004700 77  YES                         PIC X       VALUE 'J'.                   
004800 77  NOO                         PIC X       VALUE 'N'.                   
004900     SKIP2                                                                
005000*    --- PARAMETERS TO ABEND                                              
005100                                                                          
005200 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
005300 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
005400 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
005500 01  ERROR-TEXT.                                                          
005600     03  FILLER                  PIC X(10)   VALUE 'ERROR-TEXT'.          
005700     03  ERROR-TEXT-STR          PIC X(72)   VALUE SPACE.                 
005800                                                                          
005900 77  W2245501-EOF-SW             PIC X       VALUE 'N'.                   
006000     88  END-OF-W2245501                     VALUE 'Y'.                   
006100     EJECT                                                                
006200 01  TODAYS-DATE                 PIC 9(6)    VALUE ZERO.                  
006300 01  FILLER REDEFINES TODAYS-DATE.                                        
006400     03  TODAYS-DATE-YEAR        PIC 9(2).                                
006500     03  TODAYS-DATE-MONTH       PIC 9(2).                                
006600     03  TODAYS-DATE-DAY         PIC 9(2).                                
006700     EJECT                                                                
006800 01  GENERAL-SUBPROGRAMS.                                                 
006900*                                                                         
007000     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
007100     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
007200     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
007300     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
007400     EJECT                                                                
007500*    --- PARAMETRAR TILL POSTSUM                                          
007600*                                                                         
007700*01  -COPY W0005   -PRE  POSTSUM-                                         
007800     EJECT                                                                
007900 01  IN-AREA-START               PIC X(24)   VALUE                        
008000                                             'IN-AREA-START'.             
008100     SKIP2                                                                
008200                                                                          
008300*01  AREA -COPY W2245501     -PRE IN-                                     
008400*                                                                         
008500     EJECT                                                                
008600 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
008700     SKIP3                                                                
008800 01  KEYS-TILL-DLI.                                                       
008900     03  W-WDD901KY-X            PIC X(07)   VALUE SPACE.                 
009000     03  W-IDLEVNR-X             PIC X(05)   VALUE SPACE.                 
009200     03  W-WDD905KY-X            PIC X(07)   VALUE SPACE.                 
009300     03  W-IDLOPNRM-X            PIC X(05)   VALUE SPACE.                 
009500     03  W-DALEVBSK-X            PIC X(08)   VALUE SPACE.                 
009600     03  W-IDLEVBSK-X            PIC X(01)   VALUE SPACE.                 
009700                                                                          
009800     03  W-WDGXKEY-X.                                                     
009900         05  W-IDHTYP            PIC X(4)    VALUE '4579'.                
010000         05  W-IDPGM             PIC X(8)    VALUE 'W2245600'.            
010100         05  FILLER              PIC X(18)   VALUE LOW-VALUE.             
010200                                                                          
010300*    --- STATUS-KOD FRÅN IMS                                              
010400 01  STATUS-WS                   PIC XX.                                  
010500     88  SEGMENT-FOUND                       VALUE '  '.                  
010600     88  SEGMENT-FOUND-EXISTS                VALUE 'II'.                  
010700     88  SEGMENT-MISSING                     VALUE 'GE'.                  
010800     88  SEGMENT-NOMORE                      VALUE 'GB'.                  
010900     88  IMS-NOT-OK                          VALUE 'XD'.                  
011000     SKIP2                                                                
011100 01  GOOD-STATUSCODES.                                                    
011200     03  GOOD-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
011300     SKIP3                                                                
011400 01  SSA1                        PIC X(64).                               
011500 01  SSA2                        PIC X(64).                               
011600 01  SSA3                        PIC X(64).                               
011700 01  SSA4                        PIC X(64).                               
011800     EJECT                                                                
011900*    --- IMS FUNCTION CODES                                               
012000*01  -COPY W0003                                                          
012100     EJECT                                                                
012200*    ---  DLI INPUT-OUTPUT AREA                                           
012300                                                                          
012400 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDD9'.                        
012500 01  DLI-IO-WDD9.                                                         
012600      03 IO-WDD9      PIC X(100).                                         
012700 *    03 WDD901 -COPY WDD901 -PRE WDD901- -RED IO-WDD9.                   
012800 *    03 WDD902 -COPY WDD902 -PRE WDD902- -RED IO-WDD9.                   
013000 *    03 WDD905 -COPY WDD905 -PRE WDD905- -RED IO-WDD9.                   
013100 *    03 WDD905 -COPY WDD906 -PRE WDD906- -RED IO-WDD9.                   
013200 *    03 WDD924 -COPY WDD924 -PRE WDD924- -RED IO-WDD9.                   
013300 *    03 WDD925 -COPY WDD925 -PRE WDD925- -RED IO-WDD9.                   
013400     EJECT                                                                
013500*-ÅTERSTARTSREGISTER WDR4                                                 
013600 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX4580'.                    
013700 01  DLI-IO-WDGX4580.                                                     
013800*    03  -COPY WDGX4580                                                   
013900     EJECT                                                                
014000 LINKAGE SECTION.                                                         
014100                                                                          
014200*01  -COPY W0009   -PRE MSG-                                              
014300     EJECT                                                                
014400*01  -COPY W0008  -PRE WDD9-                                              
014500     05  FILLER                  PIC X.                                   
014600     EJECT                                                                
014700*01  -COPY W0008  -PRE 4579-                                              
014800     05  FILLER                  PIC X.                                   
014900     EJECT                                                                
015000 PROCEDURE DIVISION  USING MSG-PCB WDD9-PCB 4579-PCB.                     
015100 MAIN SECTION.                                                            
015200     ENTRY 'DLITCBL' USING MSG-PCB WDD9-PCB 4579-PCB.                     
015300                                                                          
015400     PERFORM A-INIT                                                       
015500                                                                          
015600     PERFORM IMS-LAS-ATERSTART                                            
015700     IF 4580-KVPOST > +0                                                  
015800        PERFORM B-LAES-FRAM-TILL-CHKPOINT                                 
015900     ELSE                                                                 
016000       PERFORM S01-READ-W2245501                                          
016100     END-IF                                                               
016200                                                                          
016300                                                                          
016400     PERFORM UNTIL END-OF-W2245501                                        
016500       IF CHKP-ANT > CHKP-MAX                                             
016600         PERFORM X-TAKE-CHECKPOINT                                        
016700       END-IF                                                             
016800                                                                          
016900       EVALUATE IN-SEG-NAME-FB                                            
017000                                                                          
018000                                                                          
018100          WHEN 'WDD905'                                                   
018200            MOVE IN-IDKEY-WDD901   TO W-WDD901KY-X                        
018300            MOVE IN-IDKEY-WDD902   TO W-IDLEVNR-X                         
018400            MOVE IN-IDKEY-WDD905   TO W-WDD905KY-X                        
018500            PERFORM IMS-GHU-WDD905                                        
018600            IF SEGMENT-FOUND                                              
018700               PERFORM IMS-DLET-WDD9                                      
018800               ADD  +1             TO CHKP-ANT                            
018900            END-IF                                                        
019000                                                                          
019100          WHEN 'WDD906'                                                   
019200            MOVE IN-IDKEY-WDD901   TO W-WDD901KY-X                        
019300            MOVE IN-IDKEY-WDD902   TO W-IDLEVNR-X                         
019400            MOVE IN-IDKEY-WDD905   TO W-WDD905KY-X                        
019500            MOVE IN-IDKEY-WDD906   TO W-IDLOPNRM-X                        
019600            PERFORM IMS-GHU-WDD906                                        
019700            IF SEGMENT-FOUND                                              
019800               PERFORM IMS-DLET-WDD9                                      
019900               ADD  +1             TO CHKP-ANT                            
020000            END-IF                                                        
020100                                                                          
020200          WHEN 'WDD924'                                                   
020300            MOVE IN-IDKEY-WDD901   TO W-WDD901KY-X                        
020400            MOVE IN-IDKEY-WDD902   TO W-IDLEVNR-X                         
020500            MOVE IN-IDKEY-WDD924   TO W-DALEVBSK-X                        
020600            PERFORM IMS-GHU-WDD924                                        
020700            IF SEGMENT-FOUND                                              
020800               IF IN-KDUPD = 'R'                                          
020900                  MOVE IN-FLSENLEV TO WDD924-LEV-FLSENLEV                 
021000                  PERFORM IMS-REPL-WDD9                                   
021100                  ADD  +1          TO CHKP-ANT                            
021200               ELSE                                                       
021300                  IF IN-KDUPD = 'D'                                       
021400                     PERFORM IMS-DLET-WDD9                                
021500                     ADD  +1       TO CHKP-ANT                            
021600                  END-IF                                                  
021700               END-IF                                                     
021800            END-IF                                                        
021900                                                                          
022000          WHEN 'WDD925'                                                   
022100            MOVE IN-IDKEY-WDD901 TO W-WDD901KY-X                          
022200            MOVE IN-IDKEY-WDD902 TO W-IDLEVNR-X                           
022300            MOVE IN-IDKEY-WDD925 TO W-IDLEVBSK-X                          
022400            PERFORM IMS-GHU-WDD925                                        
022500            IF SEGMENT-FOUND                                              
022600               ADD  +1           TO CHKP-ANT                              
022700               PERFORM IMS-DLET-WDD9                                      
022800            END-IF                                                        
022900       END-EVALUATE                                                       
023000                                                                          
023100       PERFORM S01-READ-W2245501                                          
023200     END-PERFORM                                                          
023300                                                                          
023400                                                                          
023500     PERFORM Z-FINIT                                                      
023600                                                                          
023700     MOVE ZERO TO RETURN-CODE                                             
023800     GOBACK                                                               
023900     .                                                                    
024000     EJECT                                                                
024100 A-INIT SECTION.                                                          
024200     MOVE 'A-INIT                   ' TO CURRENT-SECTION                  
024300                                                                          
024400     PERFORM IMS-RESTART                                                  
024500                                                                          
024600     OPEN INPUT W2245501                                                  
024700                                                                          
024800     MOVE +0                 TO W-KVPOST-IN                               
024900                                CHKP-ANT                                  
025000                                                                          
025100     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
025200     .                                                                    
025300     EJECT                                                                
025400 B-LAES-FRAM-TILL-CHKPOINT SECTION.                                       
025500     MOVE 'B-LAES-FRAM-TILL-CHKPOINT' TO CURRENT-SECTION                  
025600                                                                          
025700     PERFORM S01-READ-W2245501                                            
025800     PERFORM UNTIL END-OF-W2245501 OR                                     
025900                   W-KVPOST-IN = 4580-KVPOST                              
026000        PERFORM S01-READ-W2245501                                         
026100     END-PERFORM                                                          
026200                                                                          
026300     IF END-OF-W2245501                                                   
026400        MOVE 'INPUTFIL EOF = JA, VID ÅTERSTART'                           
026500                      TO ERROR-TEXT                                       
026600        CALL ABEND USING RKOD-ABEND-NO-DUMP                               
026700     END-IF                                                               
026800     .                                                                    
026900     EJECT                                                                
027000 Z-FINIT SECTION.                                                         
027100     MOVE 'Z-FINIT                  ' TO CURRENT-SECTION                  
027200                                                                          
027300     CLOSE W2245501                                                       
027400                                                                          
027500*    NOLLA ÅTERSTARTINFORMATIONEN                                         
027600     PERFORM IMS-LAS-ATERSTART                                            
027700     MOVE +0                TO 4580-KVPOST                                
027800     ACCEPT 4580-TIUPPDAT FROM DATE                                       
027900     ACCEPT 4580-TIUPPTID FROM TIME                                       
028000                                                                          
028100     PERFORM IMS-REPL-ATERSTART                                           
028200                                                                          
028300     MOVE 'S' TO POSTSUM-OPKOD                                            
028400     CALL POSTSUM USING POSTSUM-PARM                                      
028500     .                                                                    
028600     EJECT                                                                
028700 S01-READ-W2245501  SECTION.                                              
028800     MOVE 'S01-READ-W2245501        ' TO CURRENT-SECTION                  
028900     SKIP2                                                                
029000     READ W2245501        INTO IN-AREA                                    
029100     AT END                                                               
029200        SET END-OF-W2245501 TO TRUE                                       
029300                                                                          
029400     NOT AT END                                                           
029500        MOVE 'W2245601'     TO POSTSUM-FDNAMN                             
029600        MOVE 'W22456D1'     TO POSTSUM-DDNAMN2                            
029700        MOVE IN-SEG-NAME-FB TO POSTSUM-TRANSTYP                           
029800        CALL POSTSUM     USING POSTSUM-PARM                               
029900                                                                          
030000        ADD +1              TO W-KVPOST-IN                                
030100     END-READ                                                             
030200     .                                                                    
030300     EJECT                                                                
030400 X-TAKE-CHECKPOINT   SECTION.                                             
030500     MOVE 'X-TAKE-CHECKPOINT        ' TO CURRENT-SECTION                  
030600                                                                          
030700*    UPPDATERA ÅTERSTARTREGISTRET                                         
030800     PERFORM IMS-LAS-ATERSTART                                            
030900                                                                          
031000     MOVE W-KVPOST-IN       TO 4580-KVPOST                                
031100     ACCEPT 4580-TIUPPDAT FROM DATE                                       
031200     ACCEPT 4580-TIUPPTID FROM TIME                                       
031300                                                                          
031400     PERFORM IMS-REPL-ATERSTART                                           
031500                                                                          
031600*    TAG CHECKPOINT                                                       
031700     PERFORM IMS-CHECKPOINT                                               
031800                                                                          
031900     MOVE +0                TO CHKP-ANT                                   
032000     .                                                                    
032100     EJECT                                                                
032200* --- IMS SECTIONS  ---                                                   
032300                                                                          
033900 IMS-GHU-WDD905 SECTION.                                                  
034000     MOVE 'IMS-GHU-WDD905       ' TO DBS-SECTION                          
034100                                                                          
034200     STRING 'WDD901  (WDD901KY =' W-WDD901KY-X ')'                        
034300          DELIMITED BY SIZE INTO SSA1                                     
034400     STRING 'WDD902  (IDLEVNR  =' W-IDLEVNR-X ')'                         
034500          DELIMITED BY SIZE INTO SSA2                                     
034600     STRING 'WDD905  (WDD905KY =' W-WDD905KY-X ')'                        
034700          DELIMITED BY SIZE INTO SSA3                                     
034800     MOVE '  GE' TO GOOD-STATUSCODES                                      
034900     CALL CBLTDLI USING GHU WDD9-PCB DLI-IO-WDD9 SSA1 SSA2 SSA3           
035000     MOVE WDD9-STATUS-CODE TO STATUS-WS                                   
035100     PERFORM IMS-STATUSCHECK                                              
035200     .                                                                    
035300                                                                          
035400 IMS-GHU-WDD906 SECTION.                                                  
035500     MOVE 'IMS-GHU-WDD906       ' TO DBS-SECTION                          
035600                                                                          
035700     STRING 'WDD901  (WDD901KY =' W-WDD901KY-X ')'                        
035800          DELIMITED BY SIZE INTO SSA1                                     
035900     STRING 'WDD902  (IDLEVNR  =' W-IDLEVNR-X ')'                         
036000          DELIMITED BY SIZE INTO SSA2                                     
036100     STRING 'WDD905  (WDD905KY =' W-WDD905KY-X ')'                        
036200          DELIMITED BY SIZE INTO SSA3                                     
036300     STRING 'WDD906  (IDLOPNRM =' W-IDLOPNRM-X ')'                        
036400          DELIMITED BY SIZE INTO SSA4                                     
036500     MOVE '  GE' TO GOOD-STATUSCODES                                      
036600     CALL CBLTDLI USING GHU WDD9-PCB DLI-IO-WDD9                          
036700                                     SSA1 SSA2 SSA3 SSA4                  
036800     MOVE WDD9-STATUS-CODE TO STATUS-WS                                   
036900     PERFORM IMS-STATUSCHECK                                              
037000     .                                                                    
037100                                                                          
037200 IMS-GHU-WDD924 SECTION.                                                  
037300     MOVE 'IMS-GHU-WDD924       ' TO DBS-SECTION                          
037400                                                                          
037500     STRING 'WDD901  (WDD901KY =' W-WDD901KY-X ')'                        
037600          DELIMITED BY SIZE INTO SSA1                                     
037700     STRING 'WDD902  (IDLEVNR  =' W-IDLEVNR-X ')'                         
037800          DELIMITED BY SIZE INTO SSA2                                     
037900     STRING 'WDD924  (DALEVBSK =' W-DALEVBSK-X ')'                        
038000          DELIMITED BY SIZE INTO SSA3                                     
038100     MOVE '  GE' TO GOOD-STATUSCODES                                      
038200     CALL CBLTDLI USING GHU WDD9-PCB DLI-IO-WDD9                          
038300                                     SSA1 SSA2 SSA3                       
038400     MOVE WDD9-STATUS-CODE TO STATUS-WS                                   
038500     PERFORM IMS-STATUSCHECK                                              
038600     .                                                                    
038700                                                                          
038800 IMS-GHU-WDD925 SECTION.                                                  
038900     MOVE 'IMS-GHU-WDD925       ' TO DBS-SECTION                          
039000                                                                          
039100     STRING 'WDD901  (WDD901KY =' W-WDD901KY-X ')'                        
039200          DELIMITED BY SIZE INTO SSA1                                     
039300     STRING 'WDD902  (IDLEVNR  =' W-IDLEVNR-X ')'                         
039400          DELIMITED BY SIZE INTO SSA2                                     
039500     STRING 'WDD925  (IDLEVBSK =' W-IDLEVBSK-X ')'                        
039600          DELIMITED BY SIZE INTO SSA3                                     
039700     MOVE '  GE' TO GOOD-STATUSCODES                                      
039800     CALL CBLTDLI USING GHU WDD9-PCB DLI-IO-WDD9                          
039900                                     SSA1 SSA2 SSA3                       
040000     MOVE WDD9-STATUS-CODE TO STATUS-WS                                   
040100     PERFORM IMS-STATUSCHECK                                              
040200     .                                                                    
040300                                                                          
040400 IMS-DLET-WDD9 SECTION.                                                   
040500     MOVE 'IMS-DLET-WDD9        ' TO DBS-SECTION                          
040600                                                                          
040700     MOVE '  ' TO GOOD-STATUSCODES                                        
040800     CALL CBLTDLI USING DLET WDD9-PCB DLI-IO-WDD9                         
040900     MOVE WDD9-STATUS-CODE TO STATUS-WS                                   
041000     PERFORM IMS-STATUSCHECK                                              
041100     .                                                                    
041200                                                                          
041300 IMS-REPL-WDD9 SECTION.                                                   
041400     MOVE 'IMS-REPL-WDD9        ' TO DBS-SECTION                          
041500                                                                          
041600     MOVE '  ' TO GOOD-STATUSCODES                                        
041700     CALL CBLTDLI USING REPL WDD9-PCB DLI-IO-WDD9                         
041800     MOVE WDD9-STATUS-CODE TO STATUS-WS                                   
041900     PERFORM IMS-STATUSCHECK                                              
042000     .                                                                    
042100     EJECT                                                                
042200 IMS-RESTART SECTION.                                                     
042300     MOVE 'IMS-RESTART          ' TO DBS-SECTION                          
042400                                                                          
042500     MOVE SPACE TO CHKP-MSG-IO-AREA                                       
042600     MOVE '  ' TO GOOD-STATUSCODES                                        
042700     CALL CBLTDLI USING XRST MSG-PCB                                      
042800                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
042900                        CHKP-AREA-LENGTH CHKP-AREA                        
043000     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
043100     PERFORM IMS-STATUSCHECK                                              
043200     .                                                                    
043300     SKIP3                                                                
043400 IMS-CHECKPOINT SECTION.                                                  
043500     MOVE 'IMS-CHECKPOINT       ' TO DBS-SECTION                          
043600                                                                          
043700     MOVE SPACE TO CHKP-MSG-IO-AREA                                       
043800     MOVE '  XD' TO GOOD-STATUSCODES                                      
043900     CALL CBLTDLI USING CHKP MSG-PCB                                      
044000                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
044100                        CHKP-AREA-LENGTH CHKP-AREA                        
044200     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
044300     PERFORM IMS-STATUSCHECK                                              
044400                                                                          
044500     IF IMS-NOT-OK                                                        
044600       MOVE 'IMS CONTROL REGION IS NOT ACCESSIBLE'                        
044700                                    TO ERROR-TEXT-STR                     
044800       DISPLAY ERROR-TEXT                                                 
044900       CALL FELLOG                                                        
045000     END-IF                                                               
045100     .                                                                    
045200     EJECT                                                                
045300 IMS-LAS-ATERSTART SECTION.                                               
045400     MOVE 'IMS-LAS-ATERSTART    ' TO DBS-SECTION                          
045500                                                                          
045600     STRING 'WDR401  (WDGXKEY  =' W-WDGXKEY-X ')'                         
045700                    DELIMITED BY SIZE INTO SSA1                           
045800     MOVE 'WDR470 '        TO SSA2                                        
045900     MOVE '  '             TO GOOD-STATUSCODES                            
046000     CALL CBLTDLI USING GHU 4579-PCB DLI-IO-WDGX4580 SSA1 SSA2            
046100     MOVE 4579-STATUS-CODE TO STATUS-WS                                   
046200     PERFORM IMS-STATUSCHECK                                              
046300     .                                                                    
046400                                                                          
046500 IMS-REPL-ATERSTART SECTION.                                              
046600     MOVE 'IMS-REPL-ATERSTART   ' TO DBS-SECTION                          
046700                                                                          
046800     MOVE '  '             TO GOOD-STATUSCODES                            
046900     CALL CBLTDLI USING REPL 4579-PCB DLI-IO-WDGX4580                     
047000     MOVE 4579-STATUS-CODE TO STATUS-WS                                   
047100     PERFORM IMS-STATUSCHECK                                              
047200     .                                                                    
047300                                                                          
047400     EJECT                                                                
047500 IMS-STATUSCHECK SECTION.                                                 
047600     SKIP2                                                                
047700     SET STATUS-IX TO 1                                                   
047800     SEARCH GOOD-STATUS                                                   
047900       AT END                                                             
048000         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
048100           DELIMITED BY SIZE INTO ERROR-TEXT                              
048200         DISPLAY ERROR-TEXT                                               
048300         CALL FELLOG                                                      
048400       WHEN GOOD-STATUS (STATUS-IX) = STATUS-WS                           
048500         CONTINUE                                                         
048600     END-SEARCH                                                           
048700     .                                                                    
