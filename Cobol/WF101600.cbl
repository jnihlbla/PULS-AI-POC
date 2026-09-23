000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     WF101600.                                                
000300 AUTHOR.         BO HAMMARIN.                                             
000400 DATE-WRITTEN.   FEBRUARI 2002.                                           
000500 DATE-COMPILED.                                                           
000600                                                                          
000700                                                                          
000800*    FUNCTION:                                                            
000900*        THE PGM                                                          
001000*        - READS FILE WITH NEW CURRENCY-RECORDS                           
001100*        - SENDS VCCS CURRENCY DATA TO:                                   
001200*          . PULS BY USING WZ01SEND (CARPARTS.PULS.RECCURRENCY)           
001300*        -ADD MULTIPLE CURRENCY RECORDS TO WF1023                         
001400                                                                          
001500 ENVIRONMENT DIVISION.                                                    
001600                                                                          
001700 INPUT-OUTPUT SECTION.                                                    
001800 FILE-CONTROL.                                                            
001900*          --- INPUT CURRENCY RECORDS - VCC AND NON-VCC                   
002000     SELECT WF1013                     ASSIGN TO WF1016D1.                
002100     EJECT                                                                
002200*          --- OUTPUT CURRENCY-RECORDS - VCC AND NON-VCC                  
002300     SELECT WF1023                     ASSIGN TO WF1016D2.                
002400     EJECT                                                                
002500*          --- OUTPUT - TO UPDATE BILLIT                                  
002600     SELECT WF1023A                    ASSIGN TO WF1016D3.                
002700     EJECT                                                                
002800                                                                          
002900 DATA DIVISION.                                                           
003000                                                                          
003100 FILE SECTION.                                                            
003200 FD  WF1013                                                               
003300     RECORDING       F                                                    
003400     BLOCK CONTAINS  0.                                                   
003500                                                                          
003600*01  POST -COPY WF10P002    -PRE IN-   -L.                                
003700     EJECT                                                                
003800                                                                          
003900 FD  WF1023                                                               
004000     RECORDING       F                                                    
004100     BLOCK CONTAINS  0.                                                   
004200                                                                          
004300*01  POST -COPY WF10P002    -PRE UT-   -L.                                
004400     EJECT                                                                
004500                                                                          
004600 FD  WF1023A                                                              
004700     RECORDING       V                                                    
004800     BLOCK CONTAINS  0.                                                   
004900                                                                          
005000 01  UT1-RECORD       PIC X(40).                                          
005100     EJECT                                                                
005200                                                                          
005300 WORKING-STORAGE SECTION.                                                 
005400 77  IDPGM                       PIC X(8)    VALUE 'WF101600'.            
005500 77  WF1013-EOF-SW               PIC X       VALUE 'N'.                   
005600     88  END-OF-WF1013                       VALUE 'J'.                   
005700 77  WS-BILLIT-CNT               PIC 9(3)    VALUE ZERO.                  
005800 77  WS-BILLIT-TEXT              PIC X(40)   VALUE                        
005900               ' SEK DATA SENT TO BILLIT'.                                
006000 01  ERRTEXT.                                                             
006100     03  FILLER                  PIC X(8)    VALUE 'ERRTEXT'.             
006200     03  ERRTEXT-STR             PIC X(72)   VALUE SPACE.                 
006300 01  KDRC-DISPLAY                PIC Z(5).                                
006400     EJECT                                                                
006500                                                                          
006600 01  TODAYS-DATE                 PIC 9(6)    VALUE ZERO.                  
006700 01  FILLER REDEFINES TODAYS-DATE.                                        
006800     03  TODAYS-DATE-YEAR        PIC 9(2).                                
006900     03  TODAYS-DATE-MONTH       PIC 9(2).                                
007000     03  TODAYS-DATE-DAY         PIC 9(2).                                
007100     EJECT                                                                
007200                                                                          
007300 01  GENERAL-SUBPROGRAMS.                                                 
007400     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
007500     03  WZ01SEND                PIC X(8)    VALUE 'WZ01SEND'.            
007600     EJECT                                                                
007700                                                                          
007800*    --- PARAMETERS FOR SUBPROGRAM ABEND                                  
007900                                                                          
008000 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
008100 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
008200 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
008300     EJECT                                                                
008400                                                                          
008500*    --- AREOR FÖR KOMMUNIKATION                                          
008600 01  FILLER                      PIC X(16)   VALUE 'SEND-CONTROL'.        
008700*01  -COPY WZ01SEND                                                       
008800     EJECT                                                                
008900                                                                          
009000 01  IN-AREA-START              PIC X(24)   VALUE                         
009100                                             'IN-AREA-START'.             
009200 01  IN-AREA.                                                             
009300*    03  -COPY WF10P002         -PRE IN-                                  
009400*                                                                         
009500 01  UT-AREA-START              PIC X(24)   VALUE                         
009600                                             'UT-AREA-START'.             
009700 01  UT-AREA.                                                             
009800*    03  -COPY WF10P002         -PRE UT-                                  
009900*                                                                         
010000 01  UT1-AREA.                                                            
010100     03  UT1-AREA-LINE          PIC X(40)   VALUE SPACES.                 
010200*                                                                         
010300     EJECT                                                                
010400                                                                          
010500 LINKAGE SECTION.                                                         
010600*01  -COPY W0009   -PRE MSG-                                              
010700     EJECT                                                                
010800                                                                          
010900 PROCEDURE DIVISION  USING MSG-PCB.                                       
011000                                                                          
011100 MAIN SECTION.                                                            
011200     ENTRY 'DLITCBL' USING MSG-PCB.                                       
011300                                                                          
011400     PERFORM A-INIT                                                       
011500                                                                          
011600     PERFORM B-CURRENCY                                                   
011700                                                                          
011800     PERFORM Z-FINIT                                                      
011900                                                                          
012000     MOVE ZERO TO RETURN-CODE                                             
012100     GOBACK                                                               
012200     .                                                                    
012300     EJECT                                                                
012400                                                                          
012500 A-INIT SECTION.                                                          
012600     OPEN INPUT  WF1013                                                   
012700     OPEN OUTPUT WF1023                                                   
012800                 WF1023A                                                  
012900     .                                                                    
013000     EJECT                                                                
013100                                                                          
013200 B-CURRENCY SECTION.                                                      
013300                                                                          
013400     PERFORM S01-READ-WF1013                                              
013500                                                                          
013600     PERFORM UNTIL END-OF-WF1013                                          
013700       PERFORM BA-MOVE-DATA                                               
013800       IF IN-IDLEGSEL = 'VCCS'                                            
013900       AND WS-BILLIT-CNT = 0                                              
014000         PERFORM S11-CURRENCY-OPEN                                        
014100         ADD +1    TO WS-BILLIT-CNT                                       
014200       END-IF                                                             
014300       IF WS-BILLIT-CNT > 0                                               
014400         PERFORM S13-CURRENCY-PUT                                         
014500       END-IF                                                             
014600       PERFORM S03-WRITE-WF1023                                           
014700       PERFORM S01-READ-WF1013                                            
014800     END-PERFORM                                                          
014900                                                                          
015000     IF WS-BILLIT-CNT > 0                                                 
015100       PERFORM S12-CURRENCY-CLOSE                                         
015200       PERFORM S03-CREATE-WF1023A                                         
015300     END-IF                                                               
015400                                                                          
015500     .                                                                    
015600     EJECT                                                                
015700 BA-MOVE-DATA SECTION.                                                    
015800                                                                          
015900     MOVE IN-IDLEGSEL      TO UT-IDLEGSEL                                 
016000     MOVE IN-KDVALISO      TO UT-KDVALISO                                 
016100     MOVE IN-DASTADAT      TO UT-DASTADAT                                 
016200     MOVE IN-REVALUTA      TO UT-REVALUTA                                 
016210     MOVE IN-REVALUTA-FROM TO UT-REVALUTA-FROM                            
016300     MOVE IN-REVALUTA-TO   TO UT-REVALUTA-TO                              
016400     MOVE IN-PRKURS        TO UT-PRKURS                                   
016500     MOVE IN-DAREGDAT      TO UT-DAREGDAT                                 
016600     .                                                                    
016700     EJECT                                                                
016800 Z-FINIT SECTION.                                                         
016900     CLOSE WF1013                                                         
017000           WF1023                                                         
017100           WF1023A                                                        
017200     .                                                                    
017300     EJECT                                                                
017400                                                                          
017500 S01-READ-WF1013  SECTION.                                                
017600     READ WF1013 INTO IN-AREA                                             
017700     AT END                                                               
017800        MOVE HIGH-VALUE TO IN-AREA                                        
017900        SET END-OF-WF1013 TO TRUE                                         
018000     END-READ                                                             
018100     .                                                                    
018200     EJECT                                                                
018300                                                                          
018400 S03-WRITE-WF1023 SECTION.                                                
018500     WRITE UT-POST   FROM UT-AREA                                         
018600     .                                                                    
018700 S03-CREATE-WF1023A SECTION.                                              
018800                                                                          
018900     MOVE WS-BILLIT-TEXT   TO UT1-AREA-LINE                               
019000     WRITE UT1-RECORD FROM UT1-AREA                                       
019100     .                                                                    
019200                                                                          
019300 S11-CURRENCY-OPEN SECTION.                                               
019400     MOVE 'CARPARTS.PULS.RECCURRENCY'     TO SEND-ADDISPABS               
019500     MOVE 'OPEN'                          TO SEND-KDFUNC                  
019600     CALL WZ01SEND USING SEND-CONTROL-AREA                                
019700                         SEND-OPEN-AREA                                   
019800     IF SEND-KDRC > ZERO                                                  
019900       MOVE SEND-KDRC                     TO KDRC-DISPLAY                 
020000       STRING 'WZ01SEND OPEN ERROR RC=' KDRC-DISPLAY                      
020100       DELIMITED BY SIZE INTO ERRTEXT-STR                                 
020200       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
020300     END-IF                                                               
020400     .                                                                    
020500                                                                          
020600 S12-CURRENCY-CLOSE SECTION.                                              
020700     MOVE 'CLOSE'                    TO SEND-KDFUNC                       
020800     CALL WZ01SEND USING SEND-CONTROL-AREA                                
020900     .                                                                    
021000     EJECT                                                                
021100                                                                          
021200 S13-CURRENCY-PUT SECTION.                                                
021300     MOVE 'PUT'                           TO SEND-KDFUNC                  
021400     MOVE LENGTH OF IN-AREA               TO SEND-KVDLEN                  
021500     CALL WZ01SEND USING SEND-CONTROL-AREA                                
021600                         SEND-KVDLEN                                      
021700                         IN-AREA                                          
021800     IF SEND-KDRC > 1                                                     
021900       MOVE SEND-KDRC                     TO KDRC-DISPLAY                 
022000       STRING 'WZ01SEND PUT ERROR RC=' KDRC-DISPLAY                       
022100       DELIMITED BY SIZE INTO ERRTEXT-STR                                 
022200       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
022300     END-IF                                                               
022400     .                                                                    
