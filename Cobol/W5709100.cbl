000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W5709100.                                                
000300 AUTHOR.         JAIN RAHUL.                                              
000400 DATE-WRITTEN.   13/01/16.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNCTION:                                                            
000800*        RECALCULATE MATERIAL PRICE.                                      
000900*                                                                         
001000*        THE PROGRAM READS     WDB6, WDK7A                                
001100*        THE PROGRAM READS     WDG2 (WDGX9305)                            
001200*        THE PROGRAM UPDATES   WDK7                                       
001300*        THE PROGRAM UPDATES   WDR4 (WDR470 - WDGX4580)                   
001400*                                                                         
001500*    CHANGE LOG:                                                          
001600*      14/01/09 - REDDY RAHUL     - CHANGE DB READ KEYS TO FIX            
001700*                                   ABEND AFTER TAKING CHECKPOINT         
001800                                                                          
001900     SKIP3                                                                
002000 ENVIRONMENT DIVISION.                                                    
002100     SKIP2                                                                
002200 INPUT-OUTPUT SECTION.                                                    
002300                                                                          
002400 FILE-CONTROL.                                                            
002500     EJECT                                                                
002600 DATA DIVISION.                                                           
002700     SKIP3                                                                
002800 FILE SECTION.                                                            
002900     EJECT                                                                
003000 WORKING-STORAGE SECTION.                                                 
003100                                                                          
003200 77  IDPGM                       PIC X(8)    VALUE 'W5709100'.            
003300 77  YES                         PIC X       VALUE 'J'.                   
003400 77  NOO                         PIC X       VALUE 'N'.                   
003500 77  WS-UPD-COUNT                PIC S9(9)   VALUE +0   COMP-3.           
003510 77  W-DATE-AAMM                 PIC 9(4)    VALUE ZERO.                  
003600 01  CHKP-VAR.                                                            
003700     03 CHKP-MSG-IO-AREA-LENGTH  PIC S9(9)   VALUE +32 COMP SYNC.         
003800     03 CHKP-MSG-IO-AREA         PIC X(32)   VALUE SPACE.                 
003900     03 CHKP-AREA-LENGTH         PIC S9(9)   VALUE +32 COMP SYNC.         
004000     03 CHKP-AREA                PIC X(32)   VALUE SPACE.                 
004100     03 CHKP-ANT                 PIC S9(3)   VALUE +0   COMP-3.           
004200     03 CHKP-MAX                 PIC S9(3)   VALUE +500 COMP-3.           
004300     SKIP2                                                                
004400 01  ERROR-TEXT.                                                          
004500     03  FILLER                  PIC X(8)    VALUE 'ERROR-TX'.            
004600     03  ERROR-TEXT-STR          PIC X(72)   VALUE SPACE.                 
004700     EJECT                                                                
004800 01  TODAYS-DATE                 PIC 9(8)    VALUE ZERO.                  
004900 01  FILLER REDEFINES TODAYS-DATE.                                        
005000     03  TODAYS-DATE-CENTURY     PIC 9(2).                                
005100     03  TODAYS-DATE-YEAR        PIC 9(2).                                
005200     03  TODAYS-DATE-MONTH       PIC 9(2).                                
005300     03  TODAYS-DATE-DAY         PIC 9(2).                                
005400     EJECT                                                                
005600 01  WS-IDDC                     PIC X(2) VALUE SPACE.                    
005700                                                                          
005800 01  W-REVALUTA                  PIC S9(5)   VALUE +0   COMP-3.           
005900 01  W-PRKURS                    PIC S9(5)V9(5)                           
006000                                             VALUE +0   COMP-3.           
006100 01  W-PRMATRL-CONV              PIC S9(5)V9(4)                           
006200                                             VALUE +0   COMP-3.           
006300 01  WS-IDARTNR-COMP             PIC S9(9)   COMP-3 VALUE ZERO.           
006400 01  WS-IDARTNR-ALPHA REDEFINES WS-IDARTNR-COMP                           
006500                                 PIC X(5).                                
006600     EJECT                                                                
006700 01  GENERAL-SUBPROGRAMS.                                                 
006800*                                                                         
006900     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
007000     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
007010     03  W510CURR                PIC X(8)    VALUE 'W510CURR'.            
007100*                                                                         
007200     EJECT                                                                
007300 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
007400     SKIP3                                                                
007500 01  KEYS-TILL-DLI.                                                       
007600     03  W-IDDC-X.                                                        
007700         05  W-IDDC              PIC X(2)    VALUE SPACE.                 
007800     03  W-IDARTNR-X.                                                     
007900         05  W-IDARTNR           PIC S9(9)   COMP-3 VALUE ZERO.           
008000     03  W-IDLANDX2-X.                                                    
008100         05  W-IDLANDX2          PIC X(2)    VALUE SPACE.                 
008200     03  W-WDK7A1KY-MIN-X.                                                
008300         05  W-IDDC-A1-MIN       PIC X(2)    VALUE SPACE.                 
008400         05  W-ADART-A1-MIN      PIC X(7)    VALUE LOW-VALUE.             
008500         05  W-IDARTNR-A1-MIN    PIC S9(9)   COMP-3 VALUE ZERO.           
008600     03  W-WDK7A1KY-MAX-X.                                                
008700         05  W-IDDC-A1-MAX       PIC X(2)    VALUE SPACE.                 
008800         05  FILLER              PIC X(7)    VALUE HIGH-VALUE.            
008900         05  FILLER              PIC S9(9)                                
009000                                         VALUE +999999999 COMP-3.         
009900     SKIP2                                                                
010000*    --- STATUS-KOD FRÅN IMS                                              
010100 01  STATUS-WS                   PIC XX.                                  
010200     88  SEGMENT-FOUND                       VALUE '  '.                  
010300     88  SEGMENT-FOUND-EXISTS                VALUE 'II'.                  
010400     88  SEGMENT-MISSING                     VALUE 'GE'.                  
010500     88  SEGMENT-END-OF-DB                   VALUE 'GB'.                  
010600     88  IMS-NOT-OK                          VALUE 'XD'.                  
010700     SKIP2                                                                
010800 01  GOOD-STATUSCODES.                                                    
010900     03  GOOD-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
011000     SKIP3                                                                
011100 01  SSA1                        PIC X(64).                               
011200 01  SSA2                        PIC X(64).                               
011300     EJECT                                                                
011400*01 -COPY WWDCKONS                                                        
011401     EJECT                                                                
011410*01 -COPY W510CURR                                                        
011500*    --- IMS FUNCTION CODES                                               
011600*01  -COPY W0003                                                          
011700     EJECT                                                                
011800*    ---  DLI INPUT-OUTPUT AREA                                           
011900                                                                          
012000 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDB601'.                      
012100 01  DLI-IO-WDB601.                                                       
012200*    03  -COPY WDB601                                                     
012300     EJECT                                                                
012400 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK711'.                      
012500 01  DLI-IO-WDK711.                                                       
012600*    03  -COPY WDK711                                                     
012700     EJECT                                                                
012800 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK712'.                      
012900 01  DLI-IO-WDK712.                                                       
013000*    03  -COPY WDK712                                                     
013100     EJECT                                                                
013200 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK7A1'.                      
013300 01  DLI-IO-WDK7A1.                                                       
013400*    03  -COPY WDK7A1                                                     
013500     EJECT                                                                
014000 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX4580'.                    
014100 01  DLI-IO-WDGX4580.                                                     
014200*    03  -COPY WDGX4580                                                   
014300*                                                                         
014400*01  -COPY  WDGX01                                                        
014500     EJECT                                                                
014600 LINKAGE SECTION.                                                         
014700                                                                          
014800*01  -COPY W0009   -PRE MSG-                                              
014900                                                                          
015000*01  -COPY W0008  -PRE WDB6-                                              
015100     05  FILLER                  PIC X.                                   
015200                                                                          
015300*01  -COPY W0008  -PRE WDK7-                                              
015400     05  FILLER                  PIC X.                                   
015500                                                                          
015600*01  -COPY W0008  -PRE WDK7A-                                             
015700     05  FILLER                  PIC X.                                   
015800                                                                          
015900*01  -COPY W0008  -PRE 4579-                                              
016000     05  FILLER                  PIC X.                                   
016100                                                                          
016200*01  -COPY W0008  -PRE WDG2-                                              
016300     05  FILLER                  PIC X.                                   
016400     EJECT                                                                
016500 PROCEDURE DIVISION  USING MSG-PCB WDB6-PCB WDK7-PCB WDK7A-PCB            
016600                           4579-PCB WDG2-PCB.                             
016700 MAIN SECTION.                                                            
016800     ENTRY 'DLITCBL' USING MSG-PCB WDB6-PCB WDK7-PCB WDK7A-PCB            
016900                           4579-PCB WDG2-PCB.                             
017000     SKIP2                                                                
017100                                                                          
017200     PERFORM A-INIT                                                       
017201                                                                          
017300     PERFORM UNTIL SEGMENT-END-OF-DB                                      
017400       MOVE WS-IDDC                TO W-IDDC                              
017500                                      W-IDDC-A1-MIN                       
017600                                      W-IDDC-A1-MAX                       
017900                                                                          
018100       IF SEGMENT-FOUND                                                   
018110       AND DCS-FLMAINDC = YES                                             
018120         IF DCS-NDC                                                       
018200           MOVE DCS-IDLANDX2       TO W-IDLANDX2                          
018300           MOVE DCS-KDVALISO       TO CURR-KDVALISO-ROW                   
018400           MOVE TODAYS-DATE-YEAR   TO W-DATE-AAMM(1:2)                    
018410           MOVE 'SEK'              TO CURR-KDVALISO-HUV                   
018420           MOVE 01                 TO W-DATE-AAMM(3:2)                    
018430                                                                          
018460           MOVE W-DATE-AAMM        TO CURR-TIAAMM                         
018470           MOVE 'A'                TO CURR-KDVALTYP                       
018480           CALL W510CURR USING CURR-W510CURR WDG2-PCB                     
018490           IF CURR-KDSVAR = ' '                                           
018800             MOVE CURR-PRKURS-NEW  TO W-PRKURS                            
018900             MOVE CURR-REVALUTA-TO TO W-REVALUTA                          
019000           ELSE                                                           
019100             MOVE 1                TO W-PRKURS                            
019200             MOVE 1                TO W-REVALUTA                          
019300           END-IF                                                         
019400          COMPUTE W-PRMATRL-CONV ROUNDED = (W-PRKURS / W-REVALUTA)        
019500                                                                          
019600           PERFORM IMS-GN-WDK7A                                           
019700           PERFORM UNTIL SEGMENT-MISSING                                  
019800             MOVE SEQA-IDDC        TO W-IDDC                              
019900             MOVE SEQA-IDARTNR     TO W-IDARTNR                           
020000             PERFORM IMS-GU-WDK711                                        
020100             IF SLAG-PRAVCOST > ZERO                                      
020200               PERFORM IMS-GHU-WDK712                                     
020210               IF SEGMENT-MISSING                                         
020220                 CONTINUE                                                 
020230               ELSE                                                       
020300                 COMPUTE LART-PRMATRL ROUNDED                             
020400                                  = SLAG-PRAVCOST * W-PRMATRL-CONV        
020500                 PERFORM IMS-REPL-WDK712                                  
020600                 ADD +1         TO CHKP-ANT                               
020700                                     WS-UPD-COUNT                         
020800               END-IF                                                     
020810             END-IF                                                       
020900                                                                          
021000             IF CHKP-ANT >= CHKP-MAX                                      
021100               PERFORM X-TAKE-CHECKPOINT                                  
021200             END-IF                                                       
021300                                                                          
021400             PERFORM IMS-GN-WDK7A                                         
021500                                                                          
021600             IF SEGMENT-MISSING                                           
021700               DISPLAY 'TOTAL UPDATES FOR DC ' WS-IDDC                    
021800                       ' = ' WS-UPD-COUNT                                 
021900               MOVE ZEROES         TO WS-UPD-COUNT                        
022000                                                                          
022100               MOVE SPACES         TO W-IDDC-A1-MIN                       
022200                                        W-IDDC-A1-MAX                     
022300               MOVE LOW-VALUE      TO W-ADART-A1-MIN                      
022400               MOVE ZEROES         TO W-IDARTNR-A1-MIN                    
022500             END-IF                                                       
022600           END-PERFORM                                                    
022610         ELSE                                                             
022620           CONTINUE                                                       
022630         END-IF                                                           
022700       END-IF                                                             
022800       PERFORM IMS-GN-WDB601                                              
022900     END-PERFORM                                                          
023000     PERFORM Z-FINIT                                                      
023100                                                                          
023200     MOVE ZERO TO RETURN-CODE                                             
023300     GOBACK                                                               
023400     .                                                                    
023500     EJECT                                                                
023600 A-INIT SECTION.                                                          
023700                                                                          
024700     PERFORM IMS-RESTART                                                  
024800     PERFORM IMS-GHU-RESTART                                              
024900                                                                          
025000     IF 4580-IDWDK7A1KY NOT = SPACE                                       
025100       MOVE 4580-IDWDK7A1KY        TO W-WDK7A1KY-MIN-X                    
025200       MOVE 4580-IDWDK7A1KY (1:2)  TO W-IDDC-A1-MIN                       
025300       MOVE 4580-IDWDK7A1KY (1:2)  TO W-IDDC-A1-MAX                       
025400       MOVE W-IDDC-A1-MAX          TO WS-IDDC                             
025500       MOVE W-IDDC-A1-MAX          TO W-IDDC                              
025600                                                                          
025700       PERFORM IMS-GU-WDK7A                                               
025710       PERFORM IMS-GU-WDB601                                              
025720     ELSE                                                                 
025730       PERFORM IMS-GN-WDB601                                              
025800     END-IF                                                               
025801                                                                          
025810     MOVE FUNCTION CURRENT-DATE (1:8) TO TODAYS-DATE                      
025900     .                                                                    
026000     EJECT                                                                
026010                                                                          
026100 Z-FINIT SECTION.                                                         
026300     PERFORM IMS-GHU-RESTART                                              
026400     MOVE SPACE      TO 4580-IDWDK7A1KY                                   
026500     ACCEPT 4580-TIUPPDAT FROM DATE                                       
026600     ACCEPT 4580-TIUPPTID FROM TIME                                       
026700     PERFORM IMS-REPL-RESTART                                             
026800     .                                                                    
026900     EJECT                                                                
026910                                                                          
027000 X-TAKE-CHECKPOINT   SECTION.                                             
027200     PERFORM IMS-GHU-RESTART                                              
027300     MOVE SEQA-IDDC                TO 4580-IDWDK7A1KY (1:2)               
027400     MOVE SEQA-ADART               TO 4580-IDWDK7A1KY (3:7)               
027500     MOVE SEQA-IDARTNR             TO WS-IDARTNR-COMP                     
027600     MOVE WS-IDARTNR-ALPHA         TO 4580-IDWDK7A1KY (10:5)              
027700     ACCEPT 4580-TIUPPDAT FROM DATE                                       
027800     ACCEPT 4580-TIUPPTID FROM TIME                                       
027900     PERFORM IMS-REPL-RESTART                                             
028000                                                                          
028100     PERFORM IMS-CHECKPOINT                                               
028200     MOVE ZERO TO CHKP-ANT                                                
028300                                                                          
028400     MOVE 4580-IDWDK7A1KY          TO W-WDK7A1KY-MIN-X                    
028500     MOVE 4580-IDWDK7A1KY (1:2)    TO W-IDDC-A1-MAX                       
028600     PERFORM IMS-GU-WDK7A                                                 
028700     .                                                                    
028800     EJECT                                                                
028810                                                                          
028900* --- IMS SECTIONS  ---                                                   
030000 IMS-GU-WDB601 SECTION.                                                   
030200     STRING 'WDB601  (IDDC     =' W-IDDC-X ')'                            
030300          DELIMITED BY SIZE INTO SSA1                                     
030400     MOVE '  GE' TO GOOD-STATUSCODES                                      
030500     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-WDB601 SSA1                    
030600     MOVE WDB6-STATUS-CODE TO STATUS-WS                                   
030700     PERFORM IMS-STATUSCHECK                                              
030800     .                                                                    
030900     EJECT                                                                
031000                                                                          
031100 IMS-GN-WDB601 SECTION.                                                   
031110     MOVE 'WDB601  ' TO SSA1                                              
031120     MOVE '  GEGB' TO GOOD-STATUSCODES                                    
031130     CALL CBLTDLI USING GN WDB6-PCB DLI-IO-WDB601 SSA1                    
031140     MOVE WDB6-STATUS-CODE TO STATUS-WS                                   
031150     PERFORM IMS-STATUSCHECK                                              
031160     .                                                                    
031170     EJECT                                                                
031180                                                                          
031200 IMS-GU-WDK711 SECTION.                                                   
031400     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
031500          DELIMITED BY SIZE INTO SSA1                                     
031600     STRING 'WDK711  (IDDC     =' W-IDDC-X ')'                            
031700          DELIMITED BY SIZE INTO SSA2                                     
031800     MOVE '  GE' TO GOOD-STATUSCODES                                      
031900     CALL CBLTDLI USING GU WDK7-PCB DLI-IO-WDK711 SSA1 SSA2               
032000     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
032100     PERFORM IMS-STATUSCHECK                                              
032200     .                                                                    
032300     EJECT                                                                
032310                                                                          
032400 IMS-GHU-WDK712 SECTION.                                                  
032600     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
032700          DELIMITED BY SIZE INTO SSA1                                     
032800     STRING 'WDK712  (IDLAND   =' W-IDLANDX2-X ')'                        
032900          DELIMITED BY SIZE INTO SSA2                                     
033000     MOVE '  GE' TO GOOD-STATUSCODES                                      
033100     CALL CBLTDLI USING GHU WDK7-PCB DLI-IO-WDK712 SSA1 SSA2              
033200     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
033300     PERFORM IMS-STATUSCHECK                                              
033400     .                                                                    
033500     EJECT                                                                
033510                                                                          
033600 IMS-REPL-WDK712 SECTION.                                                 
033700     MOVE '    ' TO GOOD-STATUSCODES                                      
033800     CALL CBLTDLI USING REPL WDK7-PCB DLI-IO-WDK712                       
033900     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
034000     PERFORM IMS-STATUSCHECK                                              
034100     .                                                                    
034200     EJECT                                                                
034210                                                                          
034300 IMS-GN-WDK7A SECTION.                                                    
034500     STRING 'WDK7A1  (WDK7A1KY>=' W-WDK7A1KY-MIN-X                        
034600                    '&WDK7A1KY<=' W-WDK7A1KY-MAX-X ')'                    
034700             DELIMITED BY SIZE INTO SSA1                                  
034800     MOVE '  GE'                 TO GOOD-STATUSCODES                      
034900     CALL CBLTDLI USING GN WDK7A-PCB DLI-IO-WDK7A1 SSA1                   
035000     MOVE WDK7A-STATUS-CODE      TO STATUS-WS                             
035100     PERFORM IMS-STATUSCHECK                                              
035200     .                                                                    
035300     SKIP3                                                                
035310                                                                          
035400 IMS-GU-WDK7A SECTION.                                                    
035600     STRING 'WDK7A1  (WDK7A1KY =' W-WDK7A1KY-MIN-X ')'                    
035700             DELIMITED BY SIZE INTO SSA1                                  
035800     MOVE '  GE'                 TO GOOD-STATUSCODES                      
035900     CALL CBLTDLI USING GU WDK7A-PCB DLI-IO-WDK7A1 SSA1                   
036000     MOVE WDK7A-STATUS-CODE      TO STATUS-WS                             
036100     PERFORM IMS-STATUSCHECK                                              
036200     .                                                                    
036300     SKIP3                                                                
036310                                                                          
036400 IMS-GHU-RESTART  SECTION.                                                
036500     MOVE '4579'         TO IDHTYP                                        
036600     MOVE LOW-VALUE      TO NYCKEL-VALFRI                                 
036700     MOVE 'W5709100'     TO NYCKEL-VALFRI(1:8)                            
036800     STRING 'WDR401  (WDGXKEY  =' WDGX01 ')'                              
036900          DELIMITED BY SIZE INTO SSA1                                     
037000     MOVE 'WDR470   '    TO SSA2                                          
037100     MOVE '  GE'         TO GOOD-STATUSCODES                              
037200     CALL CBLTDLI USING GHU 4579-PCB DLI-IO-WDGX4580 SSA1 SSA2            
037300     MOVE 4579-STATUS-CODE TO STATUS-WS                                   
037400     PERFORM IMS-STATUSCHECK                                              
037500     .                                                                    
037600     SKIP2                                                                
037610                                                                          
037700 IMS-REPL-RESTART SECTION.                                                
037800     MOVE '  '             TO GOOD-STATUSCODES                            
037900     CALL CBLTDLI USING REPL 4579-PCB DLI-IO-WDGX4580                     
038000     MOVE 4579-STATUS-CODE TO STATUS-WS                                   
038100     PERFORM IMS-STATUSCHECK                                              
038200     .                                                                    
038300     SKIP2                                                                
038310                                                                          
038400 IMS-RESTART SECTION.                                                     
038500     SKIP2                                                                
038600     MOVE SPACE  TO CHKP-MSG-IO-AREA                                      
038700     MOVE '    ' TO GOOD-STATUSCODES                                      
038800     CALL CBLTDLI USING XRST MSG-PCB                                      
038900                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
039000                        CHKP-AREA-LENGTH CHKP-AREA                        
039100     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
039200     PERFORM IMS-STATUSCHECK                                              
039300     .                                                                    
039400     SKIP3                                                                
039410                                                                          
039500 IMS-CHECKPOINT SECTION.                                                  
039600     SKIP2                                                                
039700     MOVE SPACE  TO CHKP-MSG-IO-AREA                                      
039800     MOVE '  XD' TO GOOD-STATUSCODES                                      
039900     CALL CBLTDLI USING CHKP MSG-PCB                                      
040000                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
040100                        CHKP-AREA-LENGTH CHKP-AREA                        
040200     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
040300     PERFORM IMS-STATUSCHECK                                              
040400                                                                          
040500     IF IMS-NOT-OK                                                        
040600       MOVE 'IMS CONTROL REGION IS NOT ACCESSIBLE'                        
040700                                       TO ERROR-TEXT-STR                  
040800       DISPLAY ERROR-TEXT                                                 
040900       CALL FELLOG                                                        
041000     END-IF                                                               
041100     .                                                                    
041200     EJECT                                                                
041210                                                                          
041300 IMS-STATUSCHECK SECTION.                                                 
041400     SKIP2                                                                
041500     SET STATUS-IX TO 1                                                   
041600     SEARCH GOOD-STATUS                                                   
041700       AT END                                                             
041800         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
041900           DELIMITED BY SIZE INTO ERROR-TEXT                              
042000         DISPLAY ERROR-TEXT                                               
042100         CALL FELLOG                                                      
042200       WHEN GOOD-STATUS (STATUS-IX) = STATUS-WS                           
042300         CONTINUE                                                         
042400     END-SEARCH                                                           
042500     .                                                                    
