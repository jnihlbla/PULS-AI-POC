000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W4075200.                                                
000300 AUTHOR.         UMESH JAIN.                                              
000400 DATE-WRITTEN.   09/11/17.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNCTION:                                                            
000800*        RETURN CODE MATRIX. THIS PROGRAM IS USED TO DISPLAY              
000900*        THE LIST OF REPORT CODES FOR EVERY ASSORTMENT                    
001000*                                                                         
001100*        THE PROGRAM UPDATES   WDA8                                       
001200*                                                                         
001300*    INDATA.                                                              
001400*        TRANSACTION: W4T752                                              
001500*        MID:         W4I75201                                            
001600*                                                                         
001700*    OUTDATA.                                                             
001800*        MOD:         W4O75201                                            
001900                                                                          
002000     SKIP3                                                                
002100 ENVIRONMENT DIVISION.                                                    
002200                                                                          
002300 DATA DIVISION.                                                           
002400     EJECT                                                                
002500 WORKING-STORAGE SECTION.                                                 
002600 77  IDPGM                       PIC X(08)   VALUE 'W4075200'.            
002700                                                                          
002800*    --- WORK FIELDS FOR ERROR MESSAGES WHEN CALLING ABEND/FELLOG         
002900 77  ERROR-TEXT                  PIC X(80) VALUE SPACE.                   
003000                                                                          
003100 77  YES                         PIC X       VALUE 'Y'.                   
003200 77  NOO                         PIC X       VALUE 'N'.                   
003300*                                                                         
003400 01  RETURN-CODE-ARRAY.                                                   
003500     05 TABLE-VALUES PIC X(28) VALUE                                      
003600        '7298122227425254627475829294'.                                   
003700     05 RETURN-KODE REDEFINES TABLE-VALUES OCCURS 14 TIMES                
003800                                 PIC X(02).                               
003900*                                                                         
004000 77  UPDATE-SW                   PIC X       VALUE 'N'.                   
004100     88  UPDATE-SUCCESS                      VALUE 'Y'.                   
004200     88  NO-UPDATE                           VALUE 'N'.                   
004300                                                                          
004400*    --- INDEX FOR SCROLL LINES                                           
004500 77  INDX                        PIC S9(4)  VALUE +0    COMP SYNC.        
004600 77  INDX1                       PIC S9(4)  VALUE +0    COMP SYNC.        
004700 77  MAX-INDX                    PIC S9(4)  VALUE +11   COMP SYNC.        
004800 77  MAX-INDX1                   PIC S9(4)  VALUE +14   COMP SYNC.        
004900 77  W-CMD-CNT                   PIC 9(2)   VALUE ZERO.                   
005000                                                                          
005100*    --- WORK FIELDS FOR ACTUAL KEYVALUES OF SCREEN                       
005200                                                                          
005300 77  INDATA-SW                   PIC X       VALUE 'Y'.                   
005400     88  INDATA-OK                           VALUE 'Y'.                   
005500     88  INDATA-WRONG                        VALUE 'N'.                   
005600                                                                          
005700 77  CHK-CMD-SW                  PIC X       VALUE 'Y'.                   
005800     88  CMD-CHK-OK                          VALUE 'Y'.                   
005900     88  CMD-CHK-WRONG                       VALUE 'N'.                   
006000                                                                          
006100 77  KEYS-SW                     PIC X       VALUE 'J'.                   
006200     88  KEYS-OK                             VALUE 'J'.                   
006300     88  KEYS-WRONG                          VALUE 'N'.                   
006400                                                                          
006500 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
006600     88  OWN-MID                             VALUE '4752'.                
006700     88  GOOD-MID                            VALUE '4751' '4752'          
006800                                                   '4753' '4754'          
006900                                                   '4755' '4756'          
007000                                                   '4757' '4758'          
007100                                                   '4759'.                
007200     88  HELP-MID                            VALUE '0551'.                
007300     EJECT                                                                
007400*    --- SUBPROGRAMS AND PARAMETER AREAS                                  
007500 01  GENERAL-SUBPROGRAMS.                                                 
007600     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
007700     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
007800     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
007900     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
008000     EJECT                                                                
008100*    --- PARAMETERS FOR SUBPROGRAM WMEDKONV                               
008200*01 -COPY WMEDAREA                                                        
008300     SKIP3                                                                
008400 01  MESSAGE-CODES.                                                       
008500     03  ERR-CORR-HILITE-FLDS    PIC X(3)    VALUE '001'.                 
008600     03  INF-PRESS-PF11          PIC X(3)    VALUE '003'.                 
008700     03  INF-LAST-PAGE           PIC X(3)    VALUE '106'.                 
008800     03  INF-NO-RECORDS          PIC X(3)    VALUE '010'.                 
008900     03  ERR-PF11-AND-NO-DATA    PIC X(3)    VALUE '011'.                 
009000     03  INF-UPDATE-DONE         PIC X(3)    VALUE '101'.                 
009100     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
009200     EJECT                                                                
009300*    --- PARAMETERS FOR SUB PROGRAM W005INIT                              
009400*                                                                         
009500 01  FILLER                      PIC X(16)   VALUE 'WMSGINIT'.            
009600     SKIP3                                                                
009700*01 -COPY WMSGINIT                                                        
009800     EJECT                                                                
009900*    --- AREA CONTAINING DATA TO BE SAVED BETWEEN DIALOG STEPS            
010000*                                                                         
010100 01  SAVE-AREA.                                                           
010200     03  SAVE-IDTRANS           PIC X(4)    VALUE '4752'.                 
010300*                                                                         
010400     03  SAVE-BESORTRT-E.                                                 
010500         05  SAVE-SORT-BESORTRT-E   PIC X(20) VALUE SPACE.                
010600*                                                                         
010700     03  SAVE-BESORTRT-ROWCNT   PIC 9(2)    VALUE ZEROS.                  
010800     EJECT                                                                
010900*    --- AREAS FOR MFS AND SCREEN MANAGEMENT                              
011000*                                                                         
011100 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
011200     SKIP3                                                                
011300*01  MID -COPY W4I75201                                                   
011400     EJECT                                                                
011500 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
011600     SKIP3                                                                
011700*01  -COPY WMSGAREA                                                       
011800     EJECT                                                                
011900     03  MOD REDEFINES MSG-AREA.                                          
012000*      05  -COPY W4O75201                                                 
012100     EJECT                                                                
012200 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
012300     SKIP3                                                                
012400*01  -COPY WMFSAREA                                                       
012500     EJECT                                                                
012600*    --- WORK-AREAS FOR IMS-SECTIONS                                      
012700*                                                                         
012800 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
012900     SKIP3                                                                
013000 01  KEYS-FOR-DLI.                                                        
013100     03  W-BESORTRT-X.                                                    
013200         05  W-BESORTRT          PIC X(20) VALUE LOW-VALUES.              
013300     SKIP2                                                                
013400*    --- STATUS CODES FROM IMS                                            
013500 01  STATUS-WS                   PIC XX.                                  
013600     88  SEGMENT-FOUND                       VALUE '  '.                  
013700     88  SEGMENT-FOUND-EXISTS                VALUE 'II'.                  
013800     88  SEGMENT-MISSING                     VALUE 'GE'.                  
013900     88  SEGMENT-NOMORE                      VALUE 'GB'.                  
014000     SKIP2                                                                
014100 01  GOOD-STATUSCODES.                                                    
014200     03  GOOD-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
014300     SKIP3                                                                
014400 01  SSA1                        PIC X(64).                               
014500 01  SSA2                        PIC X(64).                               
014600     EJECT                                                                
014700*    --- IMS FUNCTION CODES                                               
014800*01  -COPY W0003                                                          
014900     EJECT                                                                
015000*    ---  DLI INPUT-OUTPUT AREA                                           
015100                                                                          
015200 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDA801'.                      
015300 01  DLI-IO-WDA801.                                                       
015400*    03  -COPY WDA801                                                     
015500     EJECT                                                                
015600 LINKAGE SECTION.                                                         
015700*01  -COPY W0009   -PRE MSG-                                              
015800*01  -COPY W0008   -PRE USEA-                                             
015900     05  FILLER                  PIC X.                                   
016000                                                                          
016100*01  -COPY W0008  -PRE WDA8-                                              
016200     05  FILLER                  PIC X.                                   
016300*01  -COPY W0008  -PRE WDA8B-                                             
016400     05  FILLER                  PIC X.                                   
016500     EJECT                                                                
016600 PROCEDURE DIVISION  USING MSG-PCB USEA-PCB WDA8-PCB WDA8B-PCB.           
016700 MAIN SECTION.                                                            
016800     ENTRY 'DLITCBL' USING MSG-PCB USEA-PCB WDA8-PCB WDA8B-PCB.           
016900                                                                          
017000     PERFORM IMS-GET-MSG                                                  
017100     IF SEGMENT-FOUND                                                     
017200       PERFORM A-INIT                                                     
017300       PERFORM B-CHECK-KEYS                                               
017400       IF MFS-UPDATE                                                      
017500         PERFORM G-CHECK-INPUT                                            
017600         IF INDATA-OK                                                     
017700           PERFORM H-UPDATE                                               
017800         END-IF                                                           
017900       ELSE                                                               
018000         PERFORM S01-CHECK-INPUT                                          
018100         IF CMD-CHK-OK                                                    
018200            PERFORM E-SAME-PAGE                                           
018300         END-IF                                                           
018400       END-IF                                                             
018500       IF INDATA-OK                                                       
018600          IF MFS-NEXT THEN                                                
018700             IF SAVE-BESORTRT-ROWCNT = 11 THEN                            
018710                MOVE SAVE-SORT-BESORTRT-E  TO W-BESORTRT                  
018800                PERFORM F-READ-SHOW-INFO                                  
018900             ELSE                                                         
019000                MOVE INF-LAST-PAGE  TO MED-IDMFSINF                       
019010                CALL WMEDKONV USING MED-WMEDAREA                          
019020                MOVE MED-MFSINF TO MOD-TEMFSINF                           
019030                PERFORM EA-MID-INDATA-TO-MOD                              
019100             END-IF                                                       
019200          ELSE                                                            
019210             MOVE LOW-VALUES     TO W-BESORTRT                            
019300             PERFORM F-READ-SHOW-INFO                                     
019400          END-IF                                                          
019500       END-IF                                                             
019600*    --- IF ANSWER TO SCREEN:       MSG-KVLL = MOD-LENGTH + 4             
019700*    --- IF PROGRAM-TO-PROGRAM-SWITCH:       = MOD-LENGTH + 17            
019800       COMPUTE MSG-KVLL = LENGTH OF MOD-W4O75201 + 4                      
019900       PERFORM IMS-INSERT-MSG                                             
020000     END-IF                                                               
020100                                                                          
020200     MOVE ZERO TO RETURN-CODE                                             
020300     GOBACK                                                               
020400     .                                                                    
020500     EJECT                                                                
020600 A-INIT SECTION.                                                          
020700     IF MSG-DOUBLE-TRANSACTIONS                                           
020800       MOVE MSG-INDATA-MINUS-2-TRANSACT   TO MID-W4I75201                 
020900       MOVE MSG-IDTRANS-2  TO MFS-IDTRANS                                 
021000       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
021100     ELSE                                                                 
021200       MOVE MSG-INDATA-MINUS-1-TRANSACT  TO MID-W4I75201                  
021300       MOVE MSG-IDTRANS-1  TO MFS-IDTRANS                                 
021400       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
021500     END-IF                                                               
021600                                                                          
021700     MOVE MSG-KDTRTYP TO MFS-KDTRTYP                                      
021800     MOVE MSG-IDPFK   TO MFS-IDPFK                                        
021900     MOVE MFS-IDTRANS TO W-IDTRANS                                        
022000                                                                          
022100     MOVE LOW-VALUE TO MSG-AREA                                           
022200     MOVE 'W4O752N1' TO MFS-IDMOD                                         
022300     MOVE '4752' TO MOD-IDTRANS                                           
022400     MOVE MFS-ERASE-FIELD TO MOD-TEMFSFEL MOD-TEMFSINF                    
022500                                                                          
022600     IF OWN-MID OR HELP-MID                                               
022700       CONTINUE                                                           
022800     ELSE                                                                 
022900       MOVE SPACE TO MFS-KDTRTYP                                          
023000       MOVE '7' TO MFS-IDPFK                                              
023100     END-IF                                                               
023200     .                                                                    
023300     EJECT                                                                
023400 B-CHECK-KEYS SECTION.                                                    
023500**   CHECK FOR USER DATABASE START **                                     
023600     MOVE ALL '+'           TO MSGI-WMSGINIT                              
023700     MOVE '001'             TO MSGI-KDCALL                                
023800     MOVE MSG-LTERM-NAME    TO MSGI-IDLTERM-USER                          
023900     MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                                
024000     MOVE '4752'            TO MSGI-IDTRANS                               
024100     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
024200     IF GOOD-MID                                                          
024300       MOVE MSGI-SPAR-AREA      TO SAVE-AREA                              
024400     END-IF                                                               
024500     .                                                                    
024600     EJECT                                                                
024700 E-SAME-PAGE SECTION.                                                     
024800     MOVE +1 TO INDX                                                      
024900     PERFORM UNTIL INDX > MAX-INDX                                        
025000      IF MID-KDCMDVAL (INDX) = 'C'                                        
025100        MOVE +1 TO INDX1                                                  
025200        MOVE MID-BESORTRT (INDX) TO MOD-BESORTRT-UP                       
025300        MOVE MFS-CLOSE-FIELD     TO MOD-BESORTRT-UP-ATTR                  
025400        MOVE MFS-ADD-SET-CURSOR  TO MOD-KDRETBEH-UP-ATTR (1)              
025500        PERFORM UNTIL INDX1 > MAX-INDX1                                   
025600          MOVE MID-KDRETBEH (INDX INDX1) TO                               
025700                                    MOD-KDRETBEH-UP (INDX1)               
025800          ADD +1 TO INDX1                                                 
025900        END-PERFORM                                                       
026000        MOVE +11 TO INDX                                                  
026100      END-IF                                                              
026200      ADD +1 TO INDX                                                      
026300     END-PERFORM                                                          
026400                                                                          
026500     IF OWN-MID OR HELP-MID                                               
026600       IF (MID-KDCMDVAL(1)  = ALL '+' OR SPACE) AND                       
026700         (MID-KDCMDVAL (2)  = ALL '+' OR SPACE) AND                       
026800         (MID-KDCMDVAL (3)  = ALL '+' OR SPACE) AND                       
026900         (MID-KDCMDVAL (4)  = ALL '+' OR SPACE) AND                       
027000         (MID-KDCMDVAL (5)  = ALL '+' OR SPACE) AND                       
027100         (MID-KDCMDVAL (6)  = ALL '+' OR SPACE) AND                       
027200         (MID-KDCMDVAL (7)  = ALL '+' OR SPACE) AND                       
027300         (MID-KDCMDVAL (8)  = ALL '+' OR SPACE) AND                       
027400         (MID-KDCMDVAL (9)  = ALL '+' OR SPACE) AND                       
027500         (MID-KDCMDVAL (10) = ALL '+' OR SPACE) AND                       
027600         (MID-KDCMDVAL (11) = ALL '+' OR SPACE) AND                       
027700         (MID-ANMORS-BEH-UP (1)  = ALL '+' OR SPACE) AND                  
027800         (MID-ANMORS-BEH-UP (2)  = ALL '+' OR SPACE) AND                  
027900         (MID-ANMORS-BEH-UP (3)  = ALL '+' OR SPACE) AND                  
028000         (MID-ANMORS-BEH-UP (4)  = ALL '+' OR SPACE) AND                  
028100         (MID-ANMORS-BEH-UP (5)  = ALL '+' OR SPACE) AND                  
028200         (MID-ANMORS-BEH-UP (6)  = ALL '+' OR SPACE) AND                  
028300         (MID-ANMORS-BEH-UP (7)  = ALL '+' OR SPACE) AND                  
028400         (MID-ANMORS-BEH-UP (8)  = ALL '+' OR SPACE) AND                  
028500         (MID-ANMORS-BEH-UP (9)  = ALL '+' OR SPACE) AND                  
028600         (MID-ANMORS-BEH-UP (10) = ALL '+' OR SPACE) AND                  
028700         (MID-ANMORS-BEH-UP (11) = ALL '+' OR SPACE) AND                  
028800         (MID-ANMORS-BEH-UP (12) = ALL '+' OR SPACE) AND                  
028900         (MID-ANMORS-BEH-UP (13) = ALL '+' OR SPACE) AND                  
029000         (MID-ANMORS-BEH-UP (14) = ALL '+' OR SPACE)                      
029100*        MOVE SAVE-SORT-BESORTRT-E   TO W-BESORTRT                        
029200         CONTINUE                                                         
029300       ELSE                                                               
029400         MOVE NOO TO INDATA-SW                                            
029500         MOVE INF-PRESS-PF11 TO MED-IDMFSINF                              
029600         CALL WMEDKONV USING MED-WMEDAREA                                 
029700         MOVE MED-MFSINF TO MOD-TEMFSINF                                  
029800         PERFORM EA-MID-INDATA-TO-MOD                                     
029900**       PERFORM MFS-DONT-TOUCH-FIELD-OUT                                 
030000**       PERFORM MFS-ADD-READ-FIELD-OUT                                   
030100       END-IF                                                             
030200     END-IF                                                               
030300     .                                                                    
030400     EJECT                                                                
030500 EA-MID-INDATA-TO-MOD SECTION.                                            
030600     MOVE 1 TO INDX                                                       
030700     PERFORM UNTIL INDX > MAX-INDX                                        
030800       IF MID-KDCMDVAL (INDX) = ALL '+' OR SPACE                          
030900         MOVE MFS-ERASE-FIELD       TO MOD-KDCMDVAL (INDX)                
031000       ELSE                                                               
031100         MOVE MID-KDCMDVAL (INDX)   TO MOD-KDCMDVAL (INDX)                
031200       END-IF                                                             
031300*                                                                         
031400       IF MID-BESORTRT (INDX) = ALL '+' OR SPACE                          
031500          MOVE MFS-ERASE-FIELD       TO MOD-BESORTRT (INDX)               
031600       ELSE                                                               
031700          MOVE MID-BESORTRT (INDX)   TO MOD-BESORTRT (INDX)               
031800       END-IF                                                             
031900*                                                                         
032000       MOVE 1 TO INDX1                                                    
032100       PERFORM UNTIL INDX1 > MAX-INDX1                                    
032200         IF MID-KDRETBEH (INDX INDX1) = ALL '+' OR SPACE                  
032300           MOVE MFS-ERASE-FIELD TO MID-KDRETBEH (INDX INDX1)              
032400         ELSE                                                             
032500           MOVE MID-KDRETBEH (INDX INDX1) TO                              
032600                                    MOD-KDRETBEH (INDX INDX1)             
032700         END-IF                                                           
032800         ADD 1 TO INDX1                                                   
032900       END-PERFORM                                                        
033000*                                                                         
033100       ADD 1 TO INDX                                                      
033200     END-PERFORM                                                          
033300*                                                                         
033400     .                                                                    
033500     EJECT                                                                
033600 F-READ-SHOW-INFO SECTION.                                                
033700     PERFORM IMS-GU-WDA801                                                
033800     IF SEGMENT-MISSING                                                   
033900       MOVE INF-NO-RECORDS TO MED-IDMFSINF                                
034000       CALL WMEDKONV USING MED-WMEDAREA                                   
034100       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
034200       MOVE +1 TO INDX                                                    
034300       PERFORM UNTIL INDX > MAX-INDX                                      
034400         PERFORM MFS-ERASE-FIELD-OUT                                      
034500         ADD +1 TO INDX                                                   
034600       END-PERFORM                                                        
034700     ELSE                                                                 
034800       MOVE +1 TO INDX                                                    
034900       PERFORM UNTIL INDX > MAX-INDX                                      
035000         IF SEGMENT-FOUND                                                 
035100           PERFORM FA-MOVE-WDA801-TO-MOD                                  
035200           PERFORM IMS-GN-WDA801                                          
035300           MOVE SORT-BESORTRT    TO SAVE-BESORTRT-E                       
035400           MOVE INDX             TO SAVE-BESORTRT-ROWCNT                  
035500         ELSE                                                             
035600           PERFORM MFS-ERASE-FIELD-OUT                                    
035700         END-IF                                                           
035800         ADD +1 TO INDX                                                   
035900       END-PERFORM                                                        
036000     END-IF                                                               
036100                                                                          
036200     MOVE '002'      TO MSGI-KDCALL                                       
036300     MOVE '4752'     TO SAVE-IDTRANS                                      
036400     MOVE SAVE-AREA  TO MSGI-SPAR-AREA                                    
036500     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
036600     .                                                                    
036700     EJECT                                                                
037000 FA-MOVE-WDA801-TO-MOD  SECTION.                                          
037100       MOVE +1 TO INDX1                                                   
037200       MOVE SORT-BESORTRT TO MOD-BESORTRT (INDX)                          
037300       PERFORM UNTIL INDX1 > MAX-INDX1                                    
037400         MOVE SORT-KDRETBEH (INDX1) TO                                    
037500                                 MOD-KDRETBEH (INDX INDX1)                
037600         IF MFS-UPDATE AND UPDATE-SUCCESS                                 
037700           IF INDX = 1                                                    
037800             MOVE MFS-ADD-READ-HILIGHT-FIELD                              
037900                               TO MOD-KDRETBEH-ATTR (INDX INDX1)          
038000                                  MOD-BESORTRT-ATTR (INDX)                
038100           END-IF                                                         
038200         END-IF                                                           
038300         ADD +1 TO INDX1                                                  
038400       END-PERFORM                                                        
038500     .                                                                    
038600     EJECT                                                                
038700 G-CHECK-INPUT SECTION.                                                   
038800     MOVE YES  TO INDATA-SW                                               
038900     IF (MID-KDCMDVAL(1)  = ALL '+' OR SPACE) AND                         
039000       (MID-KDCMDVAL (2)  = ALL '+' OR SPACE) AND                         
039100       (MID-KDCMDVAL (3)  = ALL '+' OR SPACE) AND                         
039200       (MID-KDCMDVAL (4)  = ALL '+' OR SPACE) AND                         
039300       (MID-KDCMDVAL (5)  = ALL '+' OR SPACE) AND                         
039400       (MID-KDCMDVAL (6)  = ALL '+' OR SPACE) AND                         
039500       (MID-KDCMDVAL (7)  = ALL '+' OR SPACE) AND                         
039600       (MID-KDCMDVAL (8)  = ALL '+' OR SPACE) AND                         
039700       (MID-KDCMDVAL (9)  = ALL '+' OR SPACE) AND                         
039800       (MID-KDCMDVAL (10) = ALL '+' OR SPACE) AND                         
039900       (MID-KDCMDVAL (11) = ALL '+' OR SPACE) AND                         
040000       (MID-ANMORS-BEH-UP (1)  = ALL '+' OR SPACE) AND                    
040100       (MID-ANMORS-BEH-UP (2)  = ALL '+' OR SPACE) AND                    
040200       (MID-ANMORS-BEH-UP (3)  = ALL '+' OR SPACE) AND                    
040300       (MID-ANMORS-BEH-UP (4)  = ALL '+' OR SPACE) AND                    
040400       (MID-ANMORS-BEH-UP (5)  = ALL '+' OR SPACE) AND                    
040500       (MID-ANMORS-BEH-UP (6)  = ALL '+' OR SPACE) AND                    
040600       (MID-ANMORS-BEH-UP (7)  = ALL '+' OR SPACE) AND                    
040700       (MID-ANMORS-BEH-UP (8)  = ALL '+' OR SPACE) AND                    
040800       (MID-ANMORS-BEH-UP (9)  = ALL '+' OR SPACE) AND                    
040900       (MID-ANMORS-BEH-UP (10) = ALL '+' OR SPACE) AND                    
041000       (MID-ANMORS-BEH-UP (11) = ALL '+' OR SPACE) AND                    
041100       (MID-ANMORS-BEH-UP (12) = ALL '+' OR SPACE) AND                    
041200       (MID-ANMORS-BEH-UP (13) = ALL '+' OR SPACE) AND                    
041300       (MID-ANMORS-BEH-UP (14) = ALL '+' OR SPACE)                        
041400       MOVE ERR-PF11-AND-NO-DATA TO MED-IDMFSFEL                          
041500       CALL WMEDKONV USING MED-WMEDAREA                                   
041600       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
041700       PERFORM MFS-DONT-TOUCH-FIELD-OUT                                   
041800       MOVE NOO TO INDATA-SW                                              
041900     ELSE                                                                 
042000       MOVE +1 TO INDX                                                    
042100       PERFORM UNTIL INDX > MAX-INDX                                      
042200         IF MID-KDCMDVAL  (INDX) NOT = ALL '+'                            
042300         AND MID-KDCMDVAL (INDX) NOT = SPACE                              
042400            IF  MID-KDCMDVAL (INDX) NOT = 'C'                             
042500               MOVE MFS-ALPHA-FIELD-WRONG                                 
042600                             TO MOD-KDCMDVAL-ATTR (INDX)                  
042700               MOVE NOO TO INDATA-SW                                      
042800            END-IF                                                        
042900*                                                                         
043000            IF INDATA-OK                                                  
043100              MOVE MFS-ALPHA-FIELD-OK                                     
043200                                 TO MOD-KDCMDVAL-ATTR (INDX)              
043300            END-IF                                                        
043400         END-IF                                                           
043500         ADD 1 TO INDX                                                    
043600       END-PERFORM                                                        
043700*                                                                         
043800*      VALIDATE UPDATING FILEDS IN THE MID                                
043900       MOVE +1 TO INDX1                                                   
044000       PERFORM UNTIL INDX1 > MAX-INDX1                                    
044100         IF MID-KDRETBEH-UP (INDX1) NOT = 'S' AND 'Q' AND 'A'             
044200           MOVE MFS-ALPHA-FIELD-WRONG                                     
044300                         TO MOD-KDRETBEH-UP-ATTR (INDX1)                  
044400           MOVE NOO TO INDATA-SW                                          
044500         END-IF                                                           
044600         ADD 1 TO INDX1                                                   
044700       END-PERFORM                                                        
044800*                                                                         
044900       IF INDATA-WRONG                                                    
045000         MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL                        
045100         CALL WMEDKONV USING MED-WMEDAREA                                 
045200         MOVE MED-MFSFEL TO MOD-TEMFSFEL                                  
045300         PERFORM MFS-DONT-TOUCH-FIELD-OUT                                 
045400       END-IF                                                             
045500     END-IF                                                               
045600     .                                                                    
045700     EJECT                                                                
045800 H-UPDATE SECTION.                                                        
045900     MOVE NOO TO UPDATE-SW                                                
046000     MOVE MID-BESORTRT-UP TO W-BESORTRT                                   
046100                                                                          
046200     PERFORM IMS-GHU-WDA801                                               
046300         MOVE MID-BESORTRT-UP TO SORT-BESORTRT                            
046400                                 W-BESORTRT                               
046500*                                SAVE-                                    
046600         MOVE +1 TO INDX1                                                 
046700         PERFORM UNTIL INDX1 > MAX-INDX1                                  
046800           MOVE RETURN-KODE (INDX1)    TO SORT-KDANMORS(INDX1)            
046900           MOVE MID-KDRETBEH-UP(INDX1) TO SORT-KDRETBEH(INDX1)            
047000           ADD 1 TO INDX1                                                 
047100         END-PERFORM                                                      
047200     IF SEGMENT-FOUND                                                     
047300       PERFORM IMS-REPL-WDA801                                            
047400       IF SEGMENT-FOUND                                                   
047500         MOVE YES TO UPDATE-SW                                            
047600         MOVE INF-UPDATE-DONE TO MED-IDMFSINF                             
047700         CALL WMEDKONV USING MED-WMEDAREA                                 
047800         MOVE MED-MFSINF TO MOD-TEMFSINF                                  
047900       END-IF                                                             
048000     END-IF                                                               
048100     .                                                                    
048200     EJECT                                                                
048300 S01-CHECK-INPUT SECTION.                                                 
048400     MOVE +1 TO INDX                                                      
048500     MOVE +0   TO W-CMD-CNT                                               
048600                                                                          
048700     PERFORM UNTIL INDX > MAX-INDX                                        
048800      IF MID-KDCMDVAL (INDX) = 'C'                                        
048900        ADD 1  TO W-CMD-CNT                                               
049000        IF W-CMD-CNT > 1                                                  
049100          MOVE NOO                    TO CHK-CMD-SW                       
049200          MOVE MFS-ADD-READ-HILIGHT-FIELD                                 
049300                                TO MOD-KDCMDVAL-ATTR (INDX)               
049400        END-IF                                                            
049500      END-IF                                                              
049600*                                                                         
049700      IF MID-KDCMDVAL  (INDX) NOT = ALL '+'                               
049800        AND MID-KDCMDVAL (INDX) NOT = SPACE                               
049900        IF MID-KDCMDVAL (INDX) NOT = 'C'                                  
050000          MOVE MFS-ALPHA-FIELD-WRONG                                      
050100                         TO MOD-KDCMDVAL-ATTR (INDX)                      
050200          MOVE NOO                    TO CHK-CMD-SW                       
050300        END-IF                                                            
050400      END-IF                                                              
050500*                                                                         
050600      ADD 1 TO INDX                                                       
050700     END-PERFORM                                                          
050800                                                                          
050900     IF CMD-CHK-WRONG                                                     
051000       MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL                          
051100       CALL WMEDKONV USING MED-WMEDAREA                                   
051200       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
051300       PERFORM MFS-DONT-TOUCH-FIELD-OUT                                   
051400     END-IF                                                               
051500     .                                                                    
051600     EJECT                                                                
051700                                                                          
051800 MFS-ERASE-FIELD-OUT SECTION.                                             
051900     MOVE MFS-ERASE-FIELD TO MOD-KDCMDVAL      (INDX)                     
052000                             MOD-BESORTRT      (INDX)                     
052100     MOVE MFS-CLOSE-FIELD TO MOD-KDCMDVAL-ATTR (INDX)                     
052200                             MOD-BESORTRT-ATTR (INDX)                     
052300     MOVE +1 TO INDX1                                                     
052400     PERFORM UNTIL INDX1 > MAX-INDX1                                      
052500       MOVE MFS-ERASE-FIELD TO MOD-KDRETBEH (INDX INDX1)                  
052600       MOVE MFS-CLOSE-FIELD TO MOD-KDRETBEH-ATTR (INDX INDX1)             
052700       ADD +1 TO INDX1                                                    
052800     END-PERFORM                                                          
052900*                                                                         
053000     .                                                                    
053100     SKIP3                                                                
053200 MFS-DONT-TOUCH-FIELD-OUT  SECTION.                                       
053300     MOVE +1 TO INDX                                                      
053400     PERFORM UNTIL INDX > MAX-INDX                                        
053500       MOVE MFS-DO-NOT-TOUCH-FIELD TO MOD-KDCMDVAL (INDX)                 
053600                                      MOD-BESORTRT (INDX)                 
053700       MOVE +1 TO INDX1                                                   
053800       PERFORM UNTIL INDX1 > MAX-INDX1                                    
053900         MOVE MFS-DO-NOT-TOUCH-FIELD TO                                   
054000                                 MOD-KDRETBEH (INDX INDX1)                
054100         ADD +1 TO INDX1                                                  
054200       END-PERFORM                                                        
054300       ADD +1 TO INDX                                                     
054400     END-PERFORM                                                          
054500*                                                                         
054600     MOVE +1 TO INDX1                                                     
054700     MOVE MFS-DO-NOT-TOUCH-FIELD TO MOD-BESORTRT-UP                       
054800     PERFORM UNTIL INDX1 > MAX-INDX1                                      
054900       MOVE MFS-DO-NOT-TOUCH-FIELD TO                                     
055000                               MOD-KDRETBEH-UP (INDX1)                    
055100       ADD +1 TO INDX1                                                    
055200     END-PERFORM                                                          
055300     .                                                                    
055400 MFS-ADD-READ-FIELD-OUT  SECTION.                                         
055500     MOVE +1 TO INDX1                                                     
055600*    MOVE MFS-ADD-READ-FIELD     TO MOD-BESORTRT-UP-ATTR                  
055700     PERFORM UNTIL INDX1 > MAX-INDX1                                      
055800       MOVE MFS-ADD-READ-FIELD     TO                                     
055900                               MOD-KDRETBEH-UP-ATTR (INDX1)               
056000       ADD +1 TO INDX1                                                    
056100     END-PERFORM                                                          
056200     .                                                                    
056300     SKIP3                                                                
056400* --- IMS SECTIONS ---                                                    
056500     SKIP3                                                                
056600 IMS-GET-MSG SECTION.                                                     
056700     MOVE '  QC' TO GOOD-STATUSCODES                                      
056800     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
056900     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
057000     PERFORM IMS-STATUSCHECK                                              
057100     .                                                                    
057200     SKIP3                                                                
057300 IMS-INSERT-MSG SECTION.                                                  
057400*    IF MSGI-IDLAND-SPR = 'SE'                                            
057500*      MOVE '0' TO MFS-KDHUVOMR                                           
057600*    END-IF                                                               
057700     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
057800     MOVE SPACE TO GOOD-STATUSCODES                                       
057900     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
058000     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
058100     PERFORM IMS-STATUSCHECK                                              
058200     .                                                                    
058300     EJECT                                                                
058400 IMS-GU-WDA801 SECTION.                                                   
058500     STRING 'WDA801  (WDA8BSEQ>=' W-BESORTRT-X ')'                        
058600          DELIMITED BY SIZE INTO SSA1                                     
058700     MOVE '  GEGB' TO GOOD-STATUSCODES                                    
058800     CALL CBLTDLI USING GU WDA8B-PCB DLI-IO-WDA801 SSA1                   
058900     MOVE WDA8B-STATUS-CODE TO STATUS-WS                                  
059000     PERFORM IMS-STATUSCHECK                                              
059100     .                                                                    
059200     SKIP3                                                                
059300 IMS-GN-WDA801 SECTION.                                                   
059400     MOVE '  GEGB' TO GOOD-STATUSCODES                                    
059500     CALL CBLTDLI USING GN WDA8B-PCB DLI-IO-WDA801                        
059600     MOVE WDA8B-STATUS-CODE TO STATUS-WS                                  
059700     PERFORM IMS-STATUSCHECK                                              
059800     .                                                                    
059900     SKIP3                                                                
060000 IMS-GHU-WDA801 SECTION.                                                  
060100     STRING 'WDA801  (BESORTRT =' W-BESORTRT-X ')'                        
060200          DELIMITED BY SIZE INTO SSA1                                     
060300     MOVE '  GE' TO GOOD-STATUSCODES                                      
060400     CALL CBLTDLI USING GHU WDA8-PCB DLI-IO-WDA801 SSA1                   
060500     MOVE WDA8-STATUS-CODE TO STATUS-WS                                   
060600     PERFORM IMS-STATUSCHECK                                              
060700     .                                                                    
060800     SKIP3                                                                
060900 IMS-REPL-WDA801 SECTION.                                                 
061000     MOVE '  ' TO GOOD-STATUSCODES                                        
061100     CALL CBLTDLI USING REPL WDA8-PCB DLI-IO-WDA801                       
061200     MOVE WDA8-STATUS-CODE TO STATUS-WS                                   
061300     PERFORM IMS-STATUSCHECK                                              
061400     .                                                                    
061500     EJECT                                                                
061600 IMS-STATUSCHECK SECTION.                                                 
061700     SET STATUS-IX TO 1                                                   
061800     SEARCH GOOD-STATUS                                                   
061900       AT END                                                             
062000         STRING ' INVALID STATUS CODE FROM IMS: ' STATUS-WS               
062100         DELIMITED BY SIZE INTO ERROR-TEXT                                
062200         CALL FELLOG                                                      
062300       WHEN GOOD-STATUS (STATUS-IX) = STATUS-WS                           
062400         CONTINUE                                                         
062500     END-SEARCH                                                           
062600     .                                                                    
