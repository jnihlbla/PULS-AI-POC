000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W4075100.                                                
000300 AUTHOR.         UMESH JAIN.                                              
000400 DATE-WRITTEN.   09/10/28.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNCTION:                                                            
000800*        DISCREPANCY RETURNS FOR DIFFERENT ASSORTMENTS                    
000900*                                                                         
001000*        THE PROGRAM UPDATES   WDA8                                       
001100*                                                                         
001200*    INDATA.                                                              
001300*        TRANSACTION: W4T751                                              
001400*        MID:         W4I75101                                            
001500*                                                                         
001600*    OUTDATA.                                                             
001700*        MOD:         W4O75101                                            
001800                                                                          
001900     SKIP3                                                                
002000 ENVIRONMENT DIVISION.                                                    
002100                                                                          
002200 DATA DIVISION.                                                           
002300     EJECT                                                                
002400 WORKING-STORAGE SECTION.                                                 
002500 77  IDPGM                       PIC X(08)   VALUE 'W4075100'.            
002600                                                                          
002700 01  RETURN-CODE-ARRAY.                                                   
002800     05 TABLE-VALUES PIC X(28) VALUE                                      
002900        '7298122227425254627475829294'.                                   
003000     05 RETURN-KODE REDEFINES TABLE-VALUES OCCURS 14 TIMES                
003100                                 PIC X(02).                               
003200*    --- WORK FIELDS FOR ERROR MESSAGES WHEN CALLING ABEND/FELLOG         
003300 77  ERROR-TEXT                  PIC X(80) VALUE SPACE.                   
003400 77  YES                         PIC X       VALUE 'Y'.                   
003500 77  NOO                         PIC X       VALUE 'N'.                   
003600*    --- INDEX FOR SCROLL LINES                                           
003700 77  INDX                        PIC S9(4)  VALUE +0    COMP SYNC.        
003800 77  MAX-INDX                    PIC S9(4)  VALUE +12   COMP SYNC.        
003900*    --- WORK FIELDS                                                      
004000 77  ARR-INDX                    PIC S9(4)  VALUE +14   COMP SYNC.        
004100 77  W-CMD-CNT                   PIC 9(2)   VALUE ZERO.                   
004200 77  WS-RULE-CNT                 PIC 9(2)   VALUE ZERO.                   
004300 77  W-TALLY                     PIC 9(2)   VALUE  ZERO.                  
004400 77  W-LEN                       PIC 9(2)   VALUE  ZERO.                  
004500 77  W-IDARTNR-UP                PIC X(9)   VALUE  SPACES.                
004600 77  W-IDARTNR-UP-N              PIC 9(9)   VALUE  ZERO.                  
004700 77  W-KDPRODSL-UP               PIC X(2)   VALUE  SPACES.                
004800 77  W-KDPRODSL-UP-N             PIC 9(2)   VALUE  ZERO.                  
004900                                                                          
005000 77  INDATA-SW                   PIC X       VALUE 'Y'.                   
005100     88  INDATA-OK                           VALUE 'Y'.                   
005200     88  INDATA-WRONG                        VALUE 'N'.                   
005300                                                                          
005400 77  AREA-SW                     PIC X       VALUE 'Y'.                   
005500     88  WS-AREA-WDA801                      VALUE 'Y'.                   
005600     88  WS-AREA-WDA8                        VALUE 'N'.                   
005700                                                                          
005800 77  UPDATE-SW                   PIC X       VALUE 'N'.                   
005900     88  UPDATE-SUCCESS                      VALUE 'Y'.                   
006000     88  NO-UPDATE                           VALUE 'N'.                   
006100                                                                          
006200 77  CHK-CMD-SW                  PIC X       VALUE 'Y'.                   
006300     88  CMD-CHK-OK                          VALUE 'Y'.                   
006400     88  CMD-CHK-WRONG                       VALUE 'N'.                   
006500                                                                          
006600 77  DUP-CHECK                   PIC X       VALUE 'N'.                   
006700     88  DUP-FLAG                            VALUE 'Y'.                   
006800     88  NO-DUP-FLAG                         VALUE 'N'.                   
006900                                                                          
007000 77  KEYS-SW                     PIC X       VALUE 'J'.                   
007100     88  KEYS-OK                             VALUE 'J'.                   
007200     88  KEYS-WRONG                          VALUE 'N'.                   
007300                                                                          
007400 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
007500     88  OWN-MID                             VALUE '4751'.                
007600     88  GOOD-MID                            VALUE '4751' '4752'          
007700                                                   '4753' '4754'          
007800                                                   '4755' '4756'          
007900                                                   '4757' '4758'          
008000                                                   '4759'.                
008100     88  HELP-MID                            VALUE '0551'.                
008200                                                                          
008300  01 WS-UP-STA                  PIC X(03)    VALUE SPACE.                 
008400  01 TEMP-SORT-BESORTRT         PIC X(20)    VALUE SPACE.                 
008500  01 TEMP-BESORTRT              PIC X(20)    VALUE SPACE.                 
008600  01 WS-TEELMT-IDELMT.                                                    
008700     05  WS-TEELMT              PIC X(16)    VALUE SPACE.                 
008800     05  WS-IDELMT-RET          PIC X(16)    VALUE SPACE.                 
008900     EJECT                                                                
009000*    --- SUBPROGRAMS AND PARAMETER AREAS                                  
009100 01  GENERAL-SUBPROGRAMS.                                                 
009200     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
009300     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
009400     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
009500     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
009600     EJECT                                                                
009700*    --- PARAMETERS FOR SUBPROGRAM WMEDKONV                               
009800*01 -COPY WMEDAREA                                                        
009900     SKIP3                                                                
010000 01  MESSAGE-CODES.                                                       
010100     03  ERR-CORR-HILITE-FLDS    PIC X(3)    VALUE '001'.                 
010200     03  ERR-LAST-PAGE-SHOWED    PIC X(3)    VALUE '115'.                 
010300     03  ERR-LAST-PAGE           PIC X(3)    VALUE '106'.                 
010400     03  INF-PRESS-PF11          PIC X(3)    VALUE '003'.                 
010500     03  INF-MORE-INFO-EXISTS    PIC X(3)    VALUE '105'.                 
010600     03  INF-NO-RECORDS          PIC X(3)    VALUE '010'.                 
010700     03  ERR-PF11-AND-NO-DATA    PIC X(3)    VALUE '011'.                 
010800     03  ERR-MORE-THAN-ONE-RULE  PIC X(3)    VALUE '238'.                 
010900     03  INF-FIRST-PAGE          PIC X(3)    VALUE '006'.                 
011000     03  INF-UPDATE-DONE         PIC X(3)    VALUE '101'.                 
011100     03  ERR-RECORD-EXISTS       PIC X(3)    VALUE '245'.                 
011200     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
011300     EJECT                                                                
011400*    --- PARAMETERS FOR SUB PROGRAM W005INIT                              
011500*                                                                         
011600 01  FILLER                      PIC X(16)   VALUE 'WMSGINIT'.            
011700     SKIP3                                                                
011800*01 -COPY WMSGINIT                                                        
011900     EJECT                                                                
012000*    --- AREA CONTAINING DATA TO BE SAVED BETWEEN DIALOG STEPS            
012100*                                                                         
012200 01  SAVE-AREA.                                                           
012300     03  SAVE-IDTRANS               PIC X(4)  VALUE '4751'.               
012400                                                                          
012500     03  SAVE-BESORTRT-E.                                                 
012600         05  SAVE-SORT-BESORTRT-E   PIC X(20) VALUE SPACE.                
012700     03  SAVE-WDA811KY-E.                                                 
012800         05  SAVE-URET-TEELMT-E     PIC X(16) VALUE SPACE.                
012900         05  SAVE-URET-IDELMT-RET-E PIC X(16) VALUE SPACE.                
013000                                                                          
013100     03  SAVE-BESORTRT-N.                                                 
013200         05  SAVE-SORT-BESORTRT-N   PIC X(20) VALUE SPACE.                
013300     03  SAVE-WDA811KY-N.                                                 
013400         05  SAVE-URET-TEELMT-N     PIC X(16) VALUE SPACE.                
013500         05  SAVE-URET-IDELMT-RET-N PIC X(16) VALUE SPACE.                
013600     EJECT                                                                
013700*    --- AREAS FOR MFS AND SCREEN MANAGEMENT                              
013800*                                                                         
013900 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
014000     SKIP3                                                                
014100*01  MID -COPY W4I75101                                                   
014200     EJECT                                                                
014300 01  FILLER                      PIC X(16)   VALUE 'MSG/MOD-AREA'.        
014400     SKIP3                                                                
014500*01  -COPY WMSGAREA                                                       
014600     EJECT                                                                
014700     03  MOD REDEFINES MSG-AREA.                                          
014800*      05  -COPY W4O75101                                                 
014900     EJECT                                                                
015000 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
015100     SKIP3                                                                
015200*01  -COPY WMFSAREA                                                       
015300     EJECT                                                                
015400*    --- WORK-AREAS FOR IMS-SECTIONS                                      
015500*                                                                         
015600 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
015700     SKIP3                                                                
015800 01  KEYS-FOR-DLI.                                                        
015900     03  W-BESORTRT-X.                                                    
016000         05  W-BESORTRT          PIC X(20)   VALUE LOW-VALUES.            
016100     03  W-WDA811KY-X.                                                    
016200         05  W-URET-TEELMT       PIC X(16)   VALUE LOW-VALUES.            
016300         05  W-URET-IDELMT-RET   PIC X(16)   VALUE LOW-VALUES.            
016400     03  W-BESORTRT-1-X.                                                  
016500         05  W-BESORTRT-1        PIC X(20)   VALUE LOW-VALUES.            
016600     03  W-WDA811KY-1-X.                                                  
016700         05  W-URET-TEELMT-1     PIC X(16)   VALUE LOW-VALUES.            
016800         05  W-URET-IDELMT-RET-1 PIC X(16)   VALUE LOW-VALUES.            
016900     SKIP2                                                                
017000*    --- STATUS CODES FROM IMS                                            
017100 01  STATUS-WS                   PIC XX.                                  
017200     88  SEGMENT-FOUND                       VALUE '  '.                  
017300     88  SEGMENT-VALID                       VALUE 'GA'.                  
017400     88  SEGMENT-FOUND-EXISTS                VALUE 'II'.                  
017500     88  SEGMENT-MISSING                     VALUE 'GE'.                  
017600     88  SEGMENT-NOMORE                      VALUE 'GB'.                  
017700     SKIP2                                                                
017800 01  GOOD-STATUSCODES.                                                    
017900     03  GOOD-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
018000     SKIP3                                                                
018100 01  SSA1                        PIC X(64).                               
018200 01  SSA2                        PIC X(64).                               
018300     EJECT                                                                
018400*    --- IMS FUNCTION CODES                                               
018500*01  -COPY W0003                                                          
018600     EJECT                                                                
018700*    ---  DLI INPUT-OUTPUT AREA                                           
018800                                                                          
018900 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDA801'.                      
019000 01  DLI-IO-WDA801.                                                       
019100*    03  -COPY WDA801                                                     
019200     EJECT                                                                
019300 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDA811'.                      
019400 01  DLI-IO-WDA811.                                                       
019500*    03  -COPY WDA811                                                     
019600     EJECT                                                                
019700 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDA8'.                        
019800 01  DLI-IO-WDA8.                                                         
019900     03  IO-AREA-WDA8            PIC X(94)  VALUE SPACE.                  
020000     SKIP3                                                                
020100     03  WDA810 REDEFINES IO-AREA-WDA8.                                   
020200*        05 -COPY WDA801 -PRE RED-                                        
020300     03  WDA811 REDEFINES IO-AREA-WDA8.                                   
020400*        05 -COPY WDA811 -PRE RED-                                        
020500                                                                          
020600     EJECT                                                                
020700 LINKAGE SECTION.                                                         
020800*01  -COPY W0009   -PRE MSG-                                              
020900*01  -COPY W0008   -PRE USEA-                                             
021000     05  FILLER                  PIC X.                                   
021100                                                                          
021200*01  -COPY W0008  -PRE WDA8-                                              
021300     05  FILLER                  PIC X.                                   
021400*01  -COPY W0008  -PRE WDA8B-                                             
021500     05  FILLER                  PIC X.                                   
021600     EJECT                                                                
021700 PROCEDURE DIVISION  USING MSG-PCB USEA-PCB WDA8-PCB WDA8B-PCB.           
021800 MAIN SECTION.                                                            
021900     ENTRY 'DLITCBL' USING MSG-PCB USEA-PCB WDA8-PCB WDA8B-PCB.           
022000                                                                          
022100     PERFORM IMS-GET-MSG                                                  
022200     IF SEGMENT-FOUND                                                     
022300       PERFORM A-INIT                                                     
022400       PERFORM B-CHECK-KEYS                                               
022500       IF MFS-UPDATE                                                      
022600         PERFORM G-CHECK-INPUT                                            
022700         IF INDATA-OK                                                     
022800           PERFORM H-UPDATE                                               
022900         END-IF                                                           
023000       ELSE                                                               
023100         IF MFS-FIRST                                                     
023200           PERFORM C-FIRST-PAGE                                           
023300         ELSE                                                             
023400           IF MFS-NEXT                                                    
023500             PERFORM D-NEXT-PAGE                                          
023600           ELSE                                                           
023700             PERFORM S01-CHECK-INPUT                                      
023800             IF CMD-CHK-OK                                                
023900               PERFORM E-SAME-PAGE                                        
024000             END-IF                                                       
024100           END-IF                                                         
024200         END-IF                                                           
024300       END-IF                                                             
024400       IF INDATA-OK                                                       
024500         PERFORM F-READ-SHOW-INFO                                         
024600       END-IF                                                             
024700*    --- IF ANSWER TO SCREEN:       MSG-KVLL = MOD-LENGTH + 4             
024800*    --- IF PROGRAM-TO-PROGRAM-SWITCH:       = MOD-LENGTH + 17            
024900       COMPUTE MSG-KVLL = LENGTH OF MOD-W4O75101 + 4                      
025000       PERFORM IMS-INSERT-MSG                                             
025100     END-IF                                                               
025200                                                                          
025300     MOVE ZERO TO RETURN-CODE                                             
025400     GOBACK                                                               
025500     .                                                                    
025600     EJECT                                                                
025700 A-INIT SECTION.                                                          
025800     IF MSG-DOUBLE-TRANSACTIONS                                           
025900       MOVE MSG-INDATA-MINUS-2-TRANSACT   TO MID-W4I75101                 
026000       MOVE MSG-IDTRANS-2  TO MFS-IDTRANS                                 
026100       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
026200     ELSE                                                                 
026300       MOVE MSG-INDATA-MINUS-1-TRANSACT  TO MID-W4I75101                  
026400       MOVE MSG-IDTRANS-1  TO MFS-IDTRANS                                 
026500       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
026600     END-IF                                                               
026700                                                                          
026800     MOVE MSG-KDTRTYP TO MFS-KDTRTYP                                      
026900     MOVE MSG-IDPFK   TO MFS-IDPFK                                        
027000     MOVE MFS-IDTRANS TO W-IDTRANS                                        
027100                                                                          
027200     MOVE LOW-VALUE TO MSG-AREA                                           
027300     MOVE 'W4O751N1' TO MFS-IDMOD                                         
027400     MOVE '4751' TO MOD-IDTRANS                                           
027500     MOVE MFS-ERASE-FIELD TO MOD-TEMFSFEL MOD-TEMFSINF                    
027600                                                                          
027700     IF OWN-MID OR HELP-MID                                               
027800       CONTINUE                                                           
027900     ELSE                                                                 
028000       MOVE SPACE TO MFS-KDTRTYP                                          
028100       MOVE '7' TO MFS-IDPFK                                              
028200     END-IF                                                               
028300     .                                                                    
028400     EJECT                                                                
028500 B-CHECK-KEYS SECTION.                                                    
028600** CHECK FOR USER DATABASE START **                                       
028700     MOVE ALL '+'           TO MSGI-WMSGINIT                              
028800     MOVE '001'             TO MSGI-KDCALL                                
028900     MOVE MSG-LTERM-NAME    TO MSGI-IDLTERM-USER                          
029000     MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                                
029100     MOVE '4751'            TO MSGI-IDTRANS                               
029200     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
029300     IF GOOD-MID                                                          
029400       MOVE MSGI-SPAR-AREA      TO SAVE-AREA                              
029500     END-IF                                                               
029600     .                                                                    
029700     EJECT                                                                
029800 C-FIRST-PAGE SECTION.                                                    
029900     MOVE INF-FIRST-PAGE TO MED-IDMFSINF                                  
030000     CALL WMEDKONV USING MED-WMEDAREA                                     
030100     MOVE MED-MFSINF     TO MOD-TEMFSFEL                                  
030200     .                                                                    
030300     EJECT                                                                
030400 D-NEXT-PAGE SECTION.                                                     
030500     IF SAVE-BESORTRT-N = SPACES AND SAVE-WDA811KY-N = SPACES             
030600       CONTINUE                                                           
030700     ELSE                                                                 
030900       MOVE SAVE-SORT-BESORTRT-N   TO W-BESORTRT                          
031000       MOVE SAVE-URET-TEELMT-N     TO W-URET-TEELMT                       
031100       MOVE SAVE-URET-IDELMT-RET-N TO W-URET-IDELMT-RET                   
031200     END-IF                                                               
031300     .                                                                    
031400     EJECT                                                                
031500 E-SAME-PAGE SECTION.                                                     
031600       MOVE +1 TO INDX                                                    
031700       PERFORM UNTIL INDX > MAX-INDX                                      
031800        IF MID-KDCMDVAL (INDX) = 'C'                                      
031900          MOVE MID-KDCMDVAL (INDX)  TO MOD-KDCMDVAL (INDX)                
032000          MOVE MID-BESORTRT (INDX)  TO MOD-BESORTRT-UP                    
032100          MOVE MID-IDARTNR  (INDX)  TO MOD-IDARTNR-UP                     
032200          MOVE MID-KDFARLIG (INDX)  TO MOD-KDFARLIG-UP                    
032300          MOVE MID-KDSORT   (INDX)  TO MOD-KDSORT-UP                      
032400          MOVE MID-IDLEVNR  (INDX)  TO MOD-IDLEVNR-UP                     
032500          MOVE MID-IDFKNGRP (INDX)  TO MOD-IDFKNGRP-UP                    
032600          MOVE MID-KDPRODSL (INDX)  TO MOD-KDPRODSL-UP                    
032700          PERFORM S05-SET-CURSOR                                          
032800          MOVE 12 TO INDX                                                 
032900        ELSE                                                              
033000          IF MID-KDCMDVAL (INDX) = 'I'                                    
033100            MOVE MID-KDCMDVAL (INDX)  TO MOD-KDCMDVAL (INDX)              
033200            MOVE MID-BESORTRT (INDX)  TO MOD-BESORTRT-UP                  
033300            MOVE MFS-CLOSE-FIELD      TO MOD-BESORTRT-UP-ATTR             
033400            MOVE MFS-ADD-SET-CURSOR   TO MOD-IDARTNR-UP-ATTR              
033500            MOVE 12 TO INDX                                               
033600          END-IF                                                          
033700        END-IF                                                            
033800        ADD +1 TO INDX                                                    
033900       END-PERFORM                                                        
034000                                                                          
034100     IF OWN-MID OR HELP-MID                                               
034200       IF (MID-KDCMDVAL (1)  = ALL '+' OR SPACE) AND                      
034300          (MID-KDCMDVAL (2)  = ALL '+' OR SPACE) AND                      
034400          (MID-KDCMDVAL (3)  = ALL '+' OR SPACE) AND                      
034500          (MID-KDCMDVAL (4)  = ALL '+' OR SPACE) AND                      
034600          (MID-KDCMDVAL (5)  = ALL '+' OR SPACE) AND                      
034700          (MID-KDCMDVAL (6)  = ALL '+' OR SPACE) AND                      
034800          (MID-KDCMDVAL (7)  = ALL '+' OR SPACE) AND                      
034900          (MID-KDCMDVAL (8)  = ALL '+' OR SPACE) AND                      
035000          (MID-KDCMDVAL (9)  = ALL '+' OR SPACE) AND                      
035100          (MID-KDCMDVAL (10) = ALL '+' OR SPACE) AND                      
035200          (MID-KDCMDVAL (11) = ALL '+' OR SPACE) AND                      
035300          (MID-KDCMDVAL (12) = ALL '+' OR SPACE) AND                      
035400          (MID-KDCMDVAL-UP   = ALL '+' OR SPACE) AND                      
035500          (MID-BESORTRT-UP   = ALL '+' OR SPACE) AND                      
035600          (MID-IDARTNR-UP    = ALL '+' OR SPACE) AND                      
035700          (MID-KDFARLIG-UP   = ALL '+' OR SPACE) AND                      
035800          (MID-KDSORT-UP     = ALL '+' OR SPACE) AND                      
035900          (MID-IDLEVNR-UP    = ALL '+' OR SPACE) AND                      
036000          (MID-IDFKNGRP-UP   = ALL '+' OR SPACE) AND                      
036100          (MID-KDPRODSL-UP   = ALL '+' OR SPACE)                          
036200*        MOVE SAVE-SORT-BESORTRT-E TO TEMP-SORT-BESORTRT                  
036300         MOVE SAVE-SORT-BESORTRT-E   TO W-BESORTRT                        
036400         MOVE SAVE-URET-TEELMT-E     TO W-URET-TEELMT                     
036500         MOVE SAVE-URET-IDELMT-RET-E TO W-URET-IDELMT-RET                 
036600       ELSE                                                               
036700         MOVE NOO TO INDATA-SW                                            
036800         MOVE INF-PRESS-PF11 TO MED-IDMFSINF                              
036900         CALL WMEDKONV USING MED-WMEDAREA                                 
037000         MOVE MED-MFSINF     TO MOD-TEMFSINF                              
037100         PERFORM EA-MID-INDATA-TO-MOD                                     
037200       END-IF                                                             
037300     END-IF                                                               
037400     .                                                                    
037500     EJECT                                                                
037600 EA-MID-INDATA-TO-MOD SECTION.                                            
037700       MOVE 1 TO INDX                                                     
037800       PERFORM UNTIL INDX > MAX-INDX                                      
037900         IF MID-KDCMDVAL (INDX) = ALL '+'                                 
038000            MOVE MFS-ERASE-FIELD       TO MOD-KDCMDVAL (INDX)             
038100         ELSE                                                             
038200            MOVE MID-KDCMDVAL (INDX)   TO MOD-KDCMDVAL (INDX)             
038300         END-IF                                                           
038400                                                                          
038500         IF MID-BESORTRT (INDX) = ALL '+'                                 
038600            MOVE MFS-ERASE-FIELD       TO MOD-BESORTRT (INDX)             
038700         ELSE                                                             
038800            MOVE MID-BESORTRT (INDX)   TO MOD-BESORTRT (INDX)             
038900         END-IF                                                           
039000                                                                          
039100         IF MID-IDARTNR  (INDX) = ALL '+'                                 
039200            MOVE MFS-ERASE-FIELD       TO MOD-IDARTNR  (INDX)             
039300         ELSE                                                             
039400            MOVE MID-IDARTNR  (INDX)   TO MOD-IDARTNR  (INDX)             
039500         END-IF                                                           
039600                                                                          
039700         IF MID-KDFARLIG (INDX) = ALL '+'                                 
039800            MOVE MFS-ERASE-FIELD       TO MOD-KDFARLIG (INDX)             
039900         ELSE                                                             
040000            MOVE MID-KDFARLIG (INDX)   TO MOD-KDFARLIG (INDX)             
040100         END-IF                                                           
040200                                                                          
040300         IF MID-KDSORT   (INDX) = ALL '+'                                 
040400            MOVE MFS-ERASE-FIELD       TO MOD-KDSORT   (INDX)             
040500         ELSE                                                             
040600            MOVE MID-KDSORT   (INDX)   TO MOD-KDSORT   (INDX)             
040700         END-IF                                                           
040800                                                                          
040900         IF MID-IDLEVNR  (INDX) = ALL '+'                                 
041000            MOVE MFS-ERASE-FIELD       TO MOD-IDLEVNR  (INDX)             
041100         ELSE                                                             
041200            MOVE MID-IDLEVNR  (INDX)   TO MOD-IDLEVNR  (INDX)             
041300         END-IF                                                           
041400                                                                          
041500         IF MID-IDFKNGRP (INDX) = ALL '+' OR                              
041600            MID-IDFKNGRP (INDX) = SPACE                                   
041700            MOVE MFS-ERASE-FIELD       TO MOD-IDFKNGRP (INDX)             
041800         ELSE                                                             
041900            MOVE MID-IDFKNGRP (INDX)   TO MOD-IDFKNGRP (INDX)             
042000         END-IF                                                           
042100                                                                          
042200         IF MID-KDPRODSL (INDX) = ALL '+' OR                              
042300            MID-KDPRODSL (INDX) = SPACE                                   
042400            MOVE MFS-ERASE-FIELD       TO MOD-KDPRODSL (INDX)             
042500         ELSE                                                             
042600            MOVE MID-KDPRODSL (INDX)   TO MOD-KDPRODSL (INDX)             
042700         END-IF                                                           
042800         ADD 1 TO INDX                                                    
042900       END-PERFORM                                                        
043000     .                                                                    
043100     EJECT                                                                
043200 F-READ-SHOW-INFO SECTION.                                                
043300     IF MFS-NEXT                                                          
043500       PERFORM IMS-GU-WDA801                                              
043700       MOVE SORT-BESORTRT        TO SAVE-SORT-BESORTRT-E                  
043800       MOVE URET-TEELMT          TO SAVE-URET-TEELMT-E                    
043900       MOVE URET-IDELMT-RET      TO SAVE-URET-IDELMT-RET-E                
044300     ELSE                                                                 
044400       IF MFS-UPDATE AND UPDATE-SUCCESS AND                               
044500         WS-UP-STA NOT = 'DEL'                                            
044600         PERFORM IMS-GU-WDA801                                            
044800       ELSE                                                               
044900         IF MFS-ENTER                                                     
045200           PERFORM IMS-GU-WDA801                                          
045400           MOVE SORT-BESORTRT     TO SAVE-SORT-BESORTRT-E                 
045500           MOVE URET-TEELMT       TO SAVE-URET-TEELMT-E                   
045600           MOVE URET-IDELMT-RET   TO SAVE-URET-IDELMT-RET-E               
046000         ELSE                                                             
046100           PERFORM IMS-GU-WDA801                                          
046200         END-IF                                                           
046300       END-IF                                                             
046400     END-IF                                                               
046500     SET WS-AREA-WDA801 TO TRUE                                           
046600                                                                          
046700     IF SEGMENT-MISSING                                                   
046800       MOVE INF-NO-RECORDS      TO MED-IDMFSINF                           
046900       CALL WMEDKONV USING MED-WMEDAREA                                   
047000       MOVE MED-MFSFEL          TO MOD-TEMFSFEL                           
047100     ELSE                                                                 
047200       MOVE +1 TO INDX                                                    
047400       PERFORM UNTIL INDX > MAX-INDX                                      
047500         IF SEGMENT-FOUND                                                 
047600           PERFORM IMS-GNP-WDA811                                         
047703           MOVE LOW-VALUES TO W-WDA811KY-X                                
047800           IF INDX = 1 AND SEGMENT-FOUND                                  
047900             MOVE SORT-BESORTRT   TO SAVE-SORT-BESORTRT-E                 
048000             MOVE URET-TEELMT     TO SAVE-URET-TEELMT-E                   
048100             MOVE URET-IDELMT-RET TO SAVE-URET-IDELMT-RET-E               
048200           END-IF                                                         
048300           PERFORM UNTIL SEGMENT-MISSING OR INDX > MAX-INDX               
048400             IF SEGMENT-FOUND                                             
048500               PERFORM FA-MOVE-WDA8-TO-MOD                                
048600               ADD +1 TO INDX                                             
048700               PERFORM IMS-GNP-WDA811                                     
048800             END-IF                                                       
048900           END-PERFORM                                                    
049007           IF INDX > MAX-INDX AND SEGMENT-FOUND                           
049107             CONTINUE                                                     
049207           ELSE                                                           
049307             PERFORM IMS-GN-WDA801                                        
049407           END-IF                                                         
049500         ELSE                                                             
049600           PERFORM MFS-ERASE-FIELD-OUT                                    
049700           ADD +1 TO INDX                                                 
049800         END-IF                                                           
050600       END-PERFORM                                                        
050700                                                                          
050800       IF SEGMENT-FOUND                                                   
050905         EVALUATE WDA8B-SEG-NAME-FB                                       
051000           WHEN 'WDA801'                                                  
051100             MOVE SORT-BESORTRT   TO SAVE-SORT-BESORTRT-N                 
051200             MOVE LOW-VALUES      TO SAVE-URET-TEELMT-N                   
051300             MOVE LOW-VALUES      TO SAVE-URET-IDELMT-RET-N               
051400           WHEN 'WDA811'                                                  
051500             MOVE SORT-BESORTRT   TO SAVE-SORT-BESORTRT-N                 
051600             MOVE URET-TEELMT     TO SAVE-URET-TEELMT-N                   
051700             MOVE URET-IDELMT-RET TO SAVE-URET-IDELMT-RET-N               
051800         END-EVALUATE                                                     
051900         IF MFS-UPDATE                                                    
052000           CONTINUE                                                       
052100         ELSE                                                             
052200           MOVE INF-MORE-INFO-EXISTS TO MED-IDMFSINF                      
052300           CALL WMEDKONV USING MED-WMEDAREA                               
052400           MOVE MED-TEMFSINF   TO MOD-TEMFSINF                            
052500         END-IF                                                           
052600       ELSE                                                               
052700         IF NO-DUP-FLAG                                                   
052800           MOVE ERR-LAST-PAGE    TO MED-IDMFSFEL                          
052900           CALL WMEDKONV USING MED-WMEDAREA                               
053000           MOVE MED-MFSFEL TO MOD-TEMFSFEL                                
053100           MOVE SAVE-BESORTRT-E  TO SAVE-BESORTRT-N                       
053200           MOVE SAVE-WDA811KY-E  TO SAVE-WDA811KY-N                       
053300         END-IF                                                           
053400       END-IF                                                             
053500     END-IF                                                               
053600                                                                          
053700     MOVE '002'      TO MSGI-KDCALL                                       
053800     MOVE '4751'     TO SAVE-IDTRANS                                      
053900     MOVE SAVE-AREA  TO MSGI-SPAR-AREA                                    
054000     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
054100     .                                                                    
054200     EJECT                                                                
054300 FA-MOVE-WDA8-TO-MOD      SECTION.                                        
054400     EVALUATE URET-TEELMT                                                 
054500       WHEN 'IDARTNR'                                                     
054600         MOVE SORT-BESORTRT        TO MOD-BESORTRT (INDX)                 
054700         MOVE URET-IDARTNR         TO MOD-IDARTNR  (INDX)                 
054800       WHEN 'KDFARLIG'                                                    
054900         MOVE SORT-BESORTRT        TO MOD-BESORTRT (INDX)                 
055000         MOVE URET-KDFARLIG        TO MOD-KDFARLIG (INDX)                 
055100       WHEN 'KDSORT'                                                      
055200         MOVE SORT-BESORTRT        TO MOD-BESORTRT (INDX)                 
055300         MOVE URET-KDSORT          TO MOD-KDSORT (INDX)                   
055400       WHEN 'IDLEVNR'                                                     
055500         MOVE SORT-BESORTRT        TO MOD-BESORTRT (INDX)                 
055600         MOVE URET-IDLEVNR         TO MOD-IDLEVNR (INDX)                  
055700       WHEN 'IDFKNGRP'                                                    
055800         MOVE SORT-BESORTRT        TO MOD-BESORTRT (INDX)                 
055900         MOVE URET-IDFKNGRP        TO MOD-IDFKNGRP (INDX)                 
056000       WHEN 'KDPRODSL'                                                    
056100         MOVE SORT-BESORTRT        TO MOD-BESORTRT (INDX)                 
056200         MOVE URET-KDPRODSL        TO MOD-KDPRODSL (INDX)                 
056300     END-EVALUATE                                                         
056400*                                                                         
056500     IF MFS-UPDATE AND UPDATE-SUCCESS AND WS-UP-STA NOT = 'DEL'           
056600       IF INDX = 1                                                        
056700         MOVE MFS-ADD-READ-HILIGHT-FIELD                                  
056800                                 TO MOD-BESORTRT-ATTR (INDX)              
056900                                    MOD-IDARTNR-ATTR (INDX)               
057000                                    MOD-KDFARLIG-ATTR (INDX)              
057100                                    MOD-KDSORT-ATTR (INDX)                
057200                                    MOD-IDLEVNR-ATTR (INDX)               
057300                                    MOD-IDFKNGRP-ATTR (INDX)              
057400                                    MOD-KDPRODSL-ATTR (INDX)              
057500       END-IF                                                             
057600     END-IF                                                               
057700     .                                                                    
057800     EJECT                                                                
060200 G-CHECK-INPUT SECTION.                                                   
060300     MOVE YES  TO INDATA-SW                                               
060400     IF (MID-KDCMDVAL (1)  = ALL '+' OR SPACE) AND                        
060500        (MID-KDCMDVAL (2)  = ALL '+' OR SPACE) AND                        
060600        (MID-KDCMDVAL (3)  = ALL '+' OR SPACE) AND                        
060700        (MID-KDCMDVAL (4)  = ALL '+' OR SPACE) AND                        
060800        (MID-KDCMDVAL (5)  = ALL '+' OR SPACE) AND                        
060900        (MID-KDCMDVAL (6)  = ALL '+' OR SPACE) AND                        
061000        (MID-KDCMDVAL (7)  = ALL '+' OR SPACE) AND                        
061100        (MID-KDCMDVAL (8)  = ALL '+' OR SPACE) AND                        
061200        (MID-KDCMDVAL (9)  = ALL '+' OR SPACE) AND                        
061300        (MID-KDCMDVAL (10) = ALL '+' OR SPACE) AND                        
061400        (MID-KDCMDVAL (11) = ALL '+' OR SPACE) AND                        
061500        (MID-KDCMDVAL (12) = ALL '+' OR SPACE) AND                        
061600        (MID-KDCMDVAL-UP   = ALL '+' OR SPACE) AND                        
061700        (MID-BESORTRT-UP   = ALL '+' OR SPACE) AND                        
061800        (MID-IDARTNR-UP    = ALL '+' OR SPACE) AND                        
061900        (MID-KDFARLIG-UP   = ALL '+' OR SPACE) AND                        
062000        (MID-KDSORT-UP     = ALL '+' OR SPACE) AND                        
062100        (MID-IDLEVNR-UP    = ALL '+' OR SPACE) AND                        
062200        (MID-IDFKNGRP-UP   = ALL '+' OR SPACE) AND                        
062300        (MID-KDPRODSL-UP   = ALL '+' OR SPACE)                            
062400       MOVE ERR-PF11-AND-NO-DATA TO MED-IDMFSFEL                          
062500       CALL WMEDKONV USING MED-WMEDAREA                                   
062600       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
062700       PERFORM MFS-DONT-TOUCH-FIELD-OUT                                   
062800       MOVE NOO TO INDATA-SW                                              
062900     ELSE                                                                 
063000       MOVE +1 TO INDX                                                    
063100       PERFORM UNTIL INDX > MAX-INDX                                      
063200         IF MID-KDCMDVAL  (INDX) NOT = ALL '+'                            
063300         AND MID-KDCMDVAL (INDX) NOT = SPACE                              
063400            IF  MID-KDCMDVAL (INDX) NOT = 'C'                             
063500            AND MID-KDCMDVAL (INDX) NOT = 'D'                             
063600            AND MID-KDCMDVAL (INDX) NOT = 'I'                             
063700               MOVE MFS-ALPHA-FIELD-WRONG                                 
063800                             TO MOD-KDCMDVAL-ATTR (INDX)                  
063900               MOVE NOO TO INDATA-SW                                      
064000            END-IF                                                        
064100*           VALIDATE IF RULE IS ENTERED CORRECTLY FOR NEW RULE            
064200            IF MID-KDCMDVAL (INDX) = 'I'                                  
064300              MOVE 'INS' TO WS-UP-STA                                     
064400              PERFORM S07-VALIDATE-MID-RULES                              
064500              IF WS-RULE-CNT = 0 OR WS-RULE-CNT > 1                       
064600                MOVE NOO TO INDATA-SW                                     
064700               MOVE MFS-CLOSE-FIELD      TO MOD-BESORTRT-UP-ATTR          
064800              END-IF                                                      
064900            ELSE                                                          
065000             IF MID-KDCMDVAL (INDX) = 'C'                                 
065100               MOVE 'CHG' TO WS-UP-STA                                    
065200               PERFORM S07-VALIDATE-MID-RULES                             
065300               IF WS-RULE-CNT = 0                                         
065400                 MOVE NOO TO INDATA-SW                                    
065500                MOVE MFS-CLOSE-FIELD      TO MOD-BESORTRT-UP-ATTR         
065600               END-IF                                                     
065700             END-IF                                                       
065800            END-IF                                                        
065900            IF INDATA-OK                                                  
066000              MOVE MFS-ALPHA-FIELD-OK                                     
066100                                 TO MOD-KDCMDVAL-ATTR (INDX)              
066200            END-IF                                                        
066300         END-IF                                                           
066400         ADD 1 TO INDX                                                    
066500       END-PERFORM                                                        
066600*                                                                         
066700*      VALIDATE IF RULE IS ENTERED CORRECTLY FOR 'NEW' ASSORTMENT         
066800       IF MID-KDCMDVAL-UP = 'N'                                           
066900         PERFORM S07-VALIDATE-MID-RULES                                   
067000         IF WS-RULE-CNT = 0 OR WS-RULE-CNT > 1                            
067100           MOVE NOO TO INDATA-SW                                          
067200         END-IF                                                           
067300       END-IF                                                             
067400*                                                                         
067500*      REMOVE TRAILING SPACES IDARTNR                                     
067600       IF MID-IDARTNR-UP > SPACES                                         
067700         MOVE ZERO TO W-TALLY                                             
067800                      W-LEN                                               
067900                      W-IDARTNR-UP-N                                      
068000         MOVE SPACES TO W-IDARTNR-UP                                      
068100         MOVE MID-IDARTNR-UP      TO W-IDARTNR-UP                         
068200         INSPECT FUNCTION REVERSE(W-IDARTNR-UP) TALLYING                  
068300                 W-TALLY FOR LEADING SPACES                               
068400         COMPUTE W-LEN = (LENGTH OF W-IDARTNR-UP ) - W-TALLY              
068500         MOVE W-IDARTNR-UP(1:W-LEN) TO                                    
068600              W-IDARTNR-UP-N (W-TALLY + 1:W-LEN)                          
068700*        VALIDATE PART NUMBER                                             
068800         IF W-IDARTNR-UP(1:W-LEN) IS NOT NUMERIC                          
068900           MOVE MFS-ALPHA-FIELD-WRONG TO MOD-IDARTNR-UP-ATTR              
069000           MOVE NOO TO INDATA-SW                                          
069100         ELSE                                                             
069200           MOVE W-IDARTNR-UP-N TO MID-IDARTNR-UP                          
069300         END-IF                                                           
069400         IF MID-IDARTNR-UP NOT > ZERO                                     
069500           MOVE MFS-ALPHA-FIELD-WRONG TO MOD-IDARTNR-UP-ATTR              
069600           MOVE NOO TO INDATA-SW                                          
069700         END-IF                                                           
069800       END-IF                                                             
069900*                                                                         
070000*                                                                         
070100*      REMOVE TRAILING SPACES KDPRODSL                                    
070200       IF MID-KDPRODSL-UP > SPACES                                        
070300         MOVE ZERO TO W-TALLY                                             
070400                      W-LEN                                               
070500                      W-KDPRODSL-UP-N                                     
070600         MOVE SPACES TO W-KDPRODSL-UP                                     
070700         MOVE MID-KDPRODSL-UP     TO W-KDPRODSL-UP                        
070800         INSPECT FUNCTION REVERSE(W-KDPRODSL-UP) TALLYING                 
070900                 W-TALLY FOR LEADING SPACES                               
071000         COMPUTE W-LEN = (LENGTH OF W-KDPRODSL-UP ) - W-TALLY             
071100         MOVE W-KDPRODSL-UP(1:W-LEN) TO                                   
071200              W-KDPRODSL-UP-N (W-TALLY + 1:W-LEN)                         
071300*        VALIDATE PRODSL CODE                                             
071400         IF W-KDPRODSL-UP(1:W-LEN) IS NOT NUMERIC                         
071500           MOVE MFS-ALPHA-FIELD-WRONG TO MOD-KDPRODSL-UP-ATTR             
071600           MOVE NOO TO INDATA-SW                                          
071700         ELSE                                                             
071800           MOVE W-KDPRODSL-UP-N TO MID-KDPRODSL-UP                        
071900         END-IF                                                           
072000       END-IF                                                             
072100*                                                                         
072200       IF INDATA-WRONG                                                    
072300         MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL                        
072400         IF WS-RULE-CNT = 0 AND                                           
072500          (MID-KDCMDVAL-UP = 'N' OR WS-UP-STA = 'INS' OR 'CHG')           
072600           MOVE ERR-PF11-AND-NO-DATA TO MED-IDMFSFEL                      
072700         ELSE                                                             
072800           IF WS-RULE-CNT > 1 AND                                         
072900             (MID-KDCMDVAL-UP = 'N' OR WS-UP-STA = 'INS')                 
073000             MOVE ERR-MORE-THAN-ONE-RULE TO MED-IDMFSFEL                  
073100             MOVE MFS-ADD-READ-HILIGHT-FIELD                              
073200                                 TO MOD-IDARTNR-UP-ATTR                   
073300                                    MOD-KDFARLIG-UP-ATTR                  
073400                                    MOD-KDSORT-UP-ATTR                    
073500                                    MOD-IDLEVNR-UP-ATTR                   
073600                                    MOD-IDFKNGRP-UP-ATTR                  
073700                                    MOD-KDPRODSL-UP-ATTR                  
073800           END-IF                                                         
073900         END-IF                                                           
074000         CALL WMEDKONV USING MED-WMEDAREA                                 
074100         MOVE MED-MFSFEL TO MOD-TEMFSFEL                                  
074200         PERFORM MFS-DONT-TOUCH-FIELD-OUT                                 
074300       END-IF                                                             
074400     END-IF                                                               
074500     .                                                                    
074600     EJECT                                                                
074700 H-UPDATE SECTION.                                                        
074800     MOVE NOO TO UPDATE-SW                                                
074900     MOVE SPACE TO WS-UP-STA                                              
075000     MOVE +1 TO INDX                                                      
075100     PERFORM UNTIL INDX > MAX-INDX                                        
075200       IF MID-KDCMDVAL (INDX) = 'D'                                       
075300         MOVE 'DEL' TO WS-UP-STA                                          
075400         PERFORM S03-FIND-TEXT-ELEMENT                                    
075500         PERFORM IMS-GHU-WDA811                                           
075600         IF SEGMENT-FOUND                                                 
075700           PERFORM IMS-DLET-WDA811                                        
075808           MOVE LOW-VALUES TO W-WDA811KY-X                                
075900           PERFORM IMS-GNPF-WDA811                                        
076000         END-IF                                                           
076100         IF SEGMENT-MISSING                                               
076200           PERFORM IMS-GHU-WDA801                                         
076210           IF SEGMENT-FOUND                                               
076300              PERFORM IMS-DLET-WDA801                                     
076310           END-IF                                                         
076400         END-IF                                                           
076500         MOVE YES TO UPDATE-SW                                            
076600       END-IF                                                             
076700                                                                          
076800       IF MID-KDCMDVAL (INDX) = 'C'                                       
076900         PERFORM S03-FIND-TEXT-ELEMENT                                    
077000         PERFORM S06-CHECK-FOR-DUP                                        
077100         IF SEGMENT-FOUND                                                 
077200           MOVE NOO TO UPDATE-SW                                          
077300           SET DUP-FLAG TO TRUE                                           
077400         ELSE                                                             
077500           PERFORM IMS-GHU-WDA811                                         
077600           IF SEGMENT-FOUND                                               
077700             PERFORM IMS-DLET-WDA811                                      
077800             IF SEGMENT-FOUND                                             
077900               MOVE WS-TEELMT       TO URET-TEELMT                        
078000               MOVE WS-IDELMT-RET   TO URET-IDELMT-RET                    
078100               PERFORM IMS-ISRT-WDA811                                    
078200               MOVE W-BESORTRT      TO SAVE-SORT-BESORTRT-E               
078300               MOVE WS-TEELMT       TO SAVE-URET-TEELMT-E                 
078400               MOVE WS-IDELMT-RET   TO SAVE-URET-IDELMT-RET-E             
078500               MOVE YES TO UPDATE-SW                                      
078600             END-IF                                                       
078700           END-IF                                                         
078800         END-IF                                                           
078900       END-IF                                                             
079000                                                                          
079100       IF MID-KDCMDVAL (INDX) = 'I'                                       
079200         MOVE MID-BESORTRT (INDX)  TO W-BESORTRT                          
079300         PERFORM S04-FIND-TEXT-ELEMENT-UP                                 
079400         MOVE W-URET-TEELMT        TO URET-TEELMT                         
079500                                      WS-TEELMT                           
079600         MOVE W-URET-IDELMT-RET    TO URET-IDELMT-RET                     
079700                                      WS-IDELMT-RET                       
079800         PERFORM IMS-ISRT-WDA811                                          
079900         IF SEGMENT-FOUND-EXISTS                                          
080000           MOVE NOO TO UPDATE-SW                                          
080100         ELSE                                                             
080200           MOVE YES TO UPDATE-SW                                          
080300           MOVE W-BESORTRT      TO SAVE-SORT-BESORTRT-E                   
080400           MOVE WS-TEELMT       TO SAVE-URET-TEELMT-E                     
080500           MOVE WS-IDELMT-RET   TO SAVE-URET-IDELMT-RET-E                 
080600         END-IF                                                           
080700       END-IF                                                             
080800                                                                          
080900       ADD +1 TO INDX                                                     
081000     END-PERFORM                                                          
081100                                                                          
081200     IF MID-KDCMDVAL-UP     = 'N'                                         
081300       MOVE SPACE                TO SORT-BESORTRT                         
081400       MOVE MID-BESORTRT-UP      TO SORT-BESORTRT                         
081500                                    W-BESORTRT                            
081600       PERFORM S04-FIND-TEXT-ELEMENT-UP                                   
081700       MOVE W-URET-TEELMT        TO URET-TEELMT                           
081800                                    WS-TEELMT                             
081900       MOVE W-URET-IDELMT-RET    TO URET-IDELMT-RET                       
082000                                    WS-IDELMT-RET                         
082100       PERFORM S02-INIT-REMAINING-801-FIELDS                              
082200       PERFORM IMS-ISRT-WDA801                                            
082300       IF SEGMENT-FOUND-EXISTS                                            
082400         MOVE NOO TO UPDATE-SW                                            
082500       ELSE                                                               
082600         PERFORM IMS-ISRT-WDA811                                          
082700         MOVE YES TO UPDATE-SW                                            
082800         MOVE W-BESORTRT      TO SAVE-SORT-BESORTRT-E                     
082900         MOVE WS-TEELMT       TO SAVE-URET-TEELMT-E                       
083000         MOVE WS-IDELMT-RET   TO SAVE-URET-IDELMT-RET-E                   
083100       END-IF                                                             
083200     END-IF                                                               
083300                                                                          
083400     IF UPDATE-SUCCESS                                                    
083500       MOVE INF-UPDATE-DONE  TO MED-IDMFSINF                              
083600       CALL WMEDKONV USING MED-WMEDAREA                                   
083700       MOVE MED-TEMFSINF TO MOD-TEMFSINF                                  
083800       PERFORM MFS-ERASE-KDCMDVAL-FIELD-OUT                               
083900*      MOVE W-BESORTRT      TO TEMP-SORT-BESORTRT                         
084000       MOVE WS-TEELMT       TO W-URET-TEELMT                              
084100       MOVE WS-IDELMT-RET   TO W-URET-IDELMT-RET                          
084200     ELSE                                                                 
084300       MOVE ERR-RECORD-EXISTS TO MED-IDMFSFEL                             
084400       CALL WMEDKONV USING MED-WMEDAREA                                   
084500       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
084600       PERFORM MFS-ERASE-KDCMDVAL-FIELD-OUT                               
084700     END-IF                                                               
084800     .                                                                    
084900     EJECT                                                                
085000 S01-CHECK-INPUT SECTION.                                                 
085100     MOVE +1 TO INDX                                                      
085200     MOVE +0   TO W-CMD-CNT                                               
085300                                                                          
085400     PERFORM UNTIL INDX > MAX-INDX                                        
085500      IF MID-KDCMDVAL (INDX) = 'D' OR 'C' OR 'I' OR 'N'                   
085600        ADD 1  TO W-CMD-CNT                                               
085700        IF W-CMD-CNT > 1                                                  
085800         MOVE NOO                    TO CHK-CMD-SW                        
085900         MOVE MFS-ADD-READ-HILIGHT-FIELD                                  
086000                                TO MOD-KDCMDVAL-ATTR (INDX)               
086100        END-IF                                                            
086200      END-IF                                                              
086300      ADD 1 TO INDX                                                       
086400     END-PERFORM                                                          
086500                                                                          
086600     IF CMD-CHK-WRONG                                                     
086700       MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL                          
086800       CALL WMEDKONV USING MED-WMEDAREA                                   
086900       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
087000       PERFORM MFS-DONT-TOUCH-FIELD-OUT                                   
087100     END-IF                                                               
087200     .                                                                    
087300     EJECT                                                                
087400 S02-INIT-REMAINING-801-FIELDS SECTION.                                   
087500     MOVE +1 TO INDX                                                      
087600     PERFORM UNTIL INDX > ARR-INDX                                        
087700       MOVE RETURN-KODE (INDX)   TO SORT-KDANMORS (INDX)                  
087800       MOVE 'Q'                  TO SORT-KDRETBEH (INDX)                  
087900       ADD 1  TO INDX                                                     
088000     END-PERFORM                                                          
088100     .                                                                    
088200     EJECT                                                                
088300 S03-FIND-TEXT-ELEMENT   SECTION.                                         
088400     MOVE SPACE TO W-WDA811KY-X                                           
088500     EVALUATE TRUE                                                        
088600       WHEN MID-IDARTNR (INDX) > SPACES                                   
088700         MOVE MID-BESORTRT (INDX) TO W-BESORTRT                           
088800         MOVE 'IDARTNR'           TO W-URET-TEELMT                        
088900                                     WS-TEELMT                            
089000         INSPECT MID-IDARTNR (INDX) REPLACING ALL SPACE BY ZERO           
089100         MOVE MID-IDARTNR (INDX)  TO W-URET-IDELMT-RET                    
089200         INSPECT MID-IDARTNR-UP REPLACING ALL SPACE BY ZERO               
089300         MOVE MID-IDARTNR-UP      TO WS-IDELMT-RET                        
089400       WHEN MID-KDFARLIG (INDX) > SPACE                                   
089500         MOVE MID-BESORTRT (INDX)  TO W-BESORTRT                          
089600         MOVE 'KDFARLIG'           TO W-URET-TEELMT                       
089700                                      WS-TEELMT                           
089800         MOVE MID-KDFARLIG (INDX)  TO W-URET-IDELMT-RET                   
089900         MOVE MID-KDFARLIG-UP      TO WS-IDELMT-RET                       
090000       WHEN MID-KDSORT (INDX) > SPACE                                     
090100         MOVE MID-BESORTRT (INDX) TO W-BESORTRT                           
090200         MOVE 'KDSORT'            TO W-URET-TEELMT                        
090300                                     WS-TEELMT                            
090400         MOVE MID-KDSORT (INDX)   TO W-URET-IDELMT-RET                    
090500         MOVE MID-KDSORT-UP       TO WS-IDELMT-RET                        
090600       WHEN MID-IDLEVNR (INDX) > SPACE                                    
090700         MOVE MID-BESORTRT (INDX) TO W-BESORTRT                           
090800         MOVE 'IDLEVNR'           TO W-URET-TEELMT                        
090900                                     WS-TEELMT                            
091000         MOVE MID-IDLEVNR (INDX)  TO W-URET-IDELMT-RET                    
091100         MOVE MID-IDLEVNR-UP     TO WS-IDELMT-RET                         
091200       WHEN MID-IDFKNGRP (INDX) > 0                                       
091300         MOVE MID-BESORTRT (INDX) TO W-BESORTRT                           
091400         MOVE 'IDFKNGRP'          TO W-URET-TEELMT                        
091500                                     WS-TEELMT                            
091600         MOVE MID-IDFKNGRP (INDX) TO W-URET-IDELMT-RET                    
091700         MOVE MID-IDFKNGRP-UP     TO WS-IDELMT-RET                        
091800       WHEN MID-KDPRODSL (INDX) > 0                                       
091900         MOVE MID-BESORTRT (INDX) TO W-BESORTRT                           
092000         MOVE 'KDPRODSL'          TO W-URET-TEELMT                        
092100                                     WS-TEELMT                            
092200         MOVE MID-KDPRODSL (INDX) TO W-URET-IDELMT-RET                    
092300         MOVE MID-KDPRODSL-UP     TO WS-IDELMT-RET                        
092400     END-EVALUATE                                                         
092500     .                                                                    
092600     EJECT                                                                
092700 S04-FIND-TEXT-ELEMENT-UP    SECTION.                                     
092800     MOVE SPACE TO W-WDA811KY-X                                           
092900     EVALUATE TRUE                                                        
093000       WHEN MID-IDARTNR-UP > SPACES                                       
093100         MOVE 'IDARTNR'           TO W-URET-TEELMT                        
093200         MOVE MID-IDARTNR-UP      TO W-URET-IDELMT-RET                    
093300       WHEN MID-KDFARLIG-UP > SPACE                                       
093400         MOVE 'KDFARLIG'           TO W-URET-TEELMT                       
093500         MOVE MID-KDFARLIG-UP      TO W-URET-IDELMT-RET                   
093600       WHEN MID-KDSORT-UP > SPACE                                         
093700         MOVE 'KDSORT'            TO W-URET-TEELMT                        
093800         MOVE MID-KDSORT-UP       TO W-URET-IDELMT-RET                    
093900       WHEN MID-IDLEVNR-UP > SPACE                                        
094000         MOVE 'IDLEVNR'           TO W-URET-TEELMT                        
094100         MOVE MID-IDLEVNR-UP     TO W-URET-IDELMT-RET                     
094200       WHEN MID-IDFKNGRP-UP   > 0                                         
094300         MOVE 'IDFKNGRP'          TO W-URET-TEELMT                        
094400         MOVE MID-IDFKNGRP-UP     TO W-URET-IDELMT-RET                    
094500       WHEN MID-KDPRODSL-UP   > 0                                         
094600         MOVE 'KDPRODSL'          TO W-URET-TEELMT                        
094700         MOVE MID-KDPRODSL-UP     TO W-URET-IDELMT-RET                    
094800     END-EVALUATE                                                         
094900     .                                                                    
095000     EJECT                                                                
095100 S05-SET-CURSOR         SECTION.                                          
095200     EVALUATE TRUE                                                        
095300       WHEN MOD-IDARTNR-UP > SPACES                                       
095400         MOVE MFS-ADD-SET-CURSOR   TO MOD-IDARTNR-UP-ATTR                 
095500         MOVE MFS-CLOSE-FIELD      TO MOD-BESORTRT-UP-ATTR                
095600                                      MOD-KDFARLIG-UP-ATTR                
095700                                      MOD-KDSORT-UP-ATTR                  
095800                                      MOD-IDLEVNR-UP-ATTR                 
095900                                      MOD-IDFKNGRP-UP-ATTR                
096000                                      MOD-KDPRODSL-UP-ATTR                
096100       WHEN MOD-KDFARLIG-UP > SPACE                                       
096200         MOVE MFS-ADD-SET-CURSOR   TO MOD-KDFARLIG-UP-ATTR                
096300         MOVE MFS-CLOSE-FIELD      TO MOD-BESORTRT-UP-ATTR                
096400                                      MOD-IDARTNR-UP-ATTR                 
096500                                      MOD-KDSORT-UP-ATTR                  
096600                                      MOD-IDLEVNR-UP-ATTR                 
096700                                      MOD-IDFKNGRP-UP-ATTR                
096800                                      MOD-KDPRODSL-UP-ATTR                
096900       WHEN MOD-KDSORT-UP > SPACE                                         
097000         MOVE MFS-ADD-SET-CURSOR   TO MOD-KDSORT-UP-ATTR                  
097100         MOVE MFS-CLOSE-FIELD      TO MOD-BESORTRT-UP-ATTR                
097200                                      MOD-IDARTNR-UP-ATTR                 
097300                                      MOD-KDFARLIG-UP-ATTR                
097400                                      MOD-IDLEVNR-UP-ATTR                 
097500                                      MOD-IDFKNGRP-UP-ATTR                
097600                                      MOD-KDPRODSL-UP-ATTR                
097700       WHEN MOD-IDLEVNR-UP > SPACE                                        
097800         MOVE MFS-ADD-SET-CURSOR   TO MOD-IDLEVNR-UP-ATTR                 
097900         MOVE MFS-CLOSE-FIELD      TO MOD-BESORTRT-UP-ATTR                
098000                                      MOD-IDARTNR-UP-ATTR                 
098100                                      MOD-KDFARLIG-UP-ATTR                
098200                                      MOD-KDSORT-UP-ATTR                  
098300                                      MOD-IDFKNGRP-UP-ATTR                
098400                                      MOD-KDPRODSL-UP-ATTR                
098500       WHEN MOD-IDFKNGRP-UP   > 0                                         
098600         MOVE MFS-ADD-SET-CURSOR   TO MOD-IDFKNGRP-UP-ATTR                
098700         MOVE MFS-CLOSE-FIELD      TO MOD-BESORTRT-UP-ATTR                
098800                                      MOD-IDARTNR-UP-ATTR                 
098900                                      MOD-KDFARLIG-UP-ATTR                
099000                                      MOD-KDSORT-UP-ATTR                  
099100                                      MOD-IDLEVNR-UP-ATTR                 
099200                                      MOD-KDPRODSL-UP-ATTR                
099300       WHEN MOD-KDPRODSL-UP   > 0                                         
099400         MOVE MFS-ADD-SET-CURSOR   TO MOD-KDPRODSL-UP-ATTR                
099500         MOVE MFS-CLOSE-FIELD      TO MOD-BESORTRT-UP-ATTR                
099600                                      MOD-IDARTNR-UP-ATTR                 
099700                                      MOD-KDFARLIG-UP-ATTR                
099800                                      MOD-KDSORT-UP-ATTR                  
099900                                      MOD-IDLEVNR-UP-ATTR                 
100000                                      MOD-IDFKNGRP-UP-ATTR                
100100     END-EVALUATE                                                         
100200     .                                                                    
100300     EJECT                                                                
100400 S06-CHECK-FOR-DUP            SECTION.                                    
100500     MOVE MID-BESORTRT-UP TO W-BESORTRT-1                                 
100600     MOVE WS-TEELMT       TO W-URET-TEELMT-1                              
100700     MOVE WS-IDELMT-RET   TO W-URET-IDELMT-RET-1                          
100800     PERFORM IMS-GU-WDA811-1                                              
100900     .                                                                    
101000     EJECT                                                                
101100 S07-VALIDATE-MID-RULES      SECTION.                                     
101200     MOVE 0 TO WS-RULE-CNT                                                
101300     IF MID-IDARTNR-UP > SPACE                                            
101400       ADD 1 TO WS-RULE-CNT                                               
101500     END-IF                                                               
101610*                                                                         
101710     IF MID-KDFARLIG-UP > SPACE                                           
101812       IF MID-KDFARLIG-UP NOT = '+'                                       
101910         ADD 1 TO WS-RULE-CNT                                             
102010       END-IF                                                             
102110     END-IF                                                               
102210*                                                                         
102310     IF MID-KDSORT-UP > SPACE                                             
102413       IF MID-KDSORT-UP NOT = '++'                                        
102513         ADD 1 TO WS-RULE-CNT                                             
102613       END-IF                                                             
102713     END-IF                                                               
102813*                                                                         
102913     IF MID-IDLEVNR-UP > SPACE                                            
103013       IF MID-IDLEVNR-UP NOT = '+++++'                                    
103113         ADD 1 TO WS-RULE-CNT                                             
103213       END-IF                                                             
103313     END-IF                                                               
103413*                                                                         
103513     IF MID-IDFKNGRP-UP   > 0                                             
103613       ADD 1 TO WS-RULE-CNT                                               
103713     END-IF                                                               
103813*                                                                         
103913     IF MID-KDPRODSL-UP   > 0                                             
104013       ADD 1 TO WS-RULE-CNT                                               
104113     END-IF                                                               
104213     .                                                                    
104313     EJECT                                                                
104413 MFS-ERASE-KDCMDVAL-FIELD-OUT SECTION.                                    
104513     MOVE +1 TO INDX                                                      
104613     PERFORM UNTIL INDX > MAX-INDX                                        
104713       MOVE MFS-ERASE-FIELD TO MOD-KDCMDVAL (INDX)                        
104813       ADD +1 TO INDX                                                     
104913     END-PERFORM                                                          
105013     .                                                                    
105113     SKIP3                                                                
105213 MFS-DONT-TOUCH-FIELD-OUT  SECTION.                                       
105313*    --- ALLA INDATA-FÄLT                                                 
105413     MOVE +1 TO INDX                                                      
105513     PERFORM UNTIL INDX > MAX-INDX                                        
105613       PERFORM MFS-DONT-TOUCH-LINE-FIELD-OUT                              
105713       ADD +1 TO INDX                                                     
105813     END-PERFORM                                                          
105913     MOVE MFS-DO-NOT-TOUCH-FIELD TO MOD-KDCMDVAL-UP                       
106013                                    MOD-KDCMDVAL-UP                       
106113                                    MOD-BESORTRT-UP                       
106213                                    MOD-IDARTNR-UP                        
106313                                    MOD-KDFARLIG-UP                       
106413                                    MOD-KDSORT-UP                         
106513                                    MOD-IDLEVNR-UP                        
106613                                    MOD-IDFKNGRP-UP                       
106713                                    MOD-KDPRODSL-UP                       
106813     .                                                                    
106913     SKIP2                                                                
107013 MFS-DONT-TOUCH-LINE-FIELD-OUT  SECTION.                                  
107113     MOVE MFS-DO-NOT-TOUCH-FIELD TO MOD-KDCMDVAL      (INDX)              
107213                                    MOD-BESORTRT      (INDX)              
107313                                    MOD-IDARTNR       (INDX)              
107413                                    MOD-KDFARLIG      (INDX)              
107513                                    MOD-KDSORT        (INDX)              
107613                                    MOD-IDLEVNR       (INDX)              
107713                                    MOD-IDFKNGRP      (INDX)              
107813                                    MOD-KDPRODSL      (INDX)              
107913     .                                                                    
108013     SKIP3                                                                
108113 MFS-ERASE-FIELD-OUT SECTION.                                             
108213     MOVE MFS-ERASE-FIELD TO MOD-KDCMDVAL      (INDX)                     
108313                             MOD-BESORTRT      (INDX)                     
108413                             MOD-IDARTNR       (INDX)                     
108513                             MOD-KDFARLIG      (INDX)                     
108613                             MOD-KDSORT        (INDX)                     
108713                             MOD-IDLEVNR       (INDX)                     
108813                             MOD-IDFKNGRP      (INDX)                     
108913                             MOD-KDPRODSL      (INDX)                     
109013*                                                                         
109113     MOVE MFS-CLOSE-FIELD TO MOD-KDCMDVAL-ATTR (INDX)                     
109213                             MOD-BESORTRT-ATTR (INDX)                     
109313                             MOD-IDARTNR-ATTR  (INDX)                     
109413                             MOD-KDFARLIG-ATTR (INDX)                     
109513                             MOD-KDSORT-ATTR   (INDX)                     
109613                             MOD-IDLEVNR-ATTR  (INDX)                     
109713                             MOD-IDFKNGRP-ATTR (INDX)                     
109813                             MOD-KDPRODSL-ATTR (INDX)                     
109913     .                                                                    
110013     SKIP3                                                                
110113* --- IMS SECTIONS ---                                                    
110213     SKIP3                                                                
110313 IMS-GET-MSG SECTION.                                                     
110413     MOVE '  QC' TO GOOD-STATUSCODES                                      
110513     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
110613     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
110713     PERFORM IMS-STATUSCHECK                                              
110813     .                                                                    
110913     SKIP3                                                                
111013 IMS-INSERT-MSG SECTION.                                                  
111113**   IF MSGI-IDLAND-SPR = 'SE'                                            
111213**     MOVE '0' TO MFS-KDHUVOMR                                           
111313**   END-IF                                                               
111413     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
111513     MOVE SPACE TO GOOD-STATUSCODES                                       
111613     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
111713     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
111813     PERFORM IMS-STATUSCHECK                                              
111913     .                                                                    
112013     EJECT                                                                
112113 IMS-GU-WDA801 SECTION.                                                   
112213     STRING 'WDA801  (WDA8BSEQ>=' W-BESORTRT-X ')'                        
112313          DELIMITED BY SIZE INTO SSA1                                     
112413     MOVE '  GE' TO GOOD-STATUSCODES                                      
112513     CALL CBLTDLI USING GU WDA8B-PCB DLI-IO-WDA801 SSA1                   
112613     MOVE WDA8B-STATUS-CODE TO STATUS-WS                                  
112713     PERFORM IMS-STATUSCHECK                                              
112813     .                                                                    
112913     SKIP3                                                                
113013 IMS-GNP-WDA811 SECTION.                                                  
113113     STRING 'WDA811  (WDA811KY>=' W-WDA811KY-X ')'                        
113213          DELIMITED BY SIZE INTO SSA1                                     
113313     MOVE '  GE' TO GOOD-STATUSCODES                                      
113413     CALL CBLTDLI USING GNP WDA8B-PCB DLI-IO-WDA811 SSA1                  
113513     MOVE WDA8B-STATUS-CODE TO STATUS-WS                                  
113613     PERFORM IMS-STATUSCHECK                                              
113713     .                                                                    
113813     SKIP3                                                                
113913 IMS-GN-WDA801 SECTION.                                                   
114013     MOVE 'WDA801' TO SSA1                                                
114113     MOVE '  GEGB' TO GOOD-STATUSCODES                                    
114213     CALL CBLTDLI USING GN WDA8B-PCB DLI-IO-WDA801 SSA1                   
114313     MOVE WDA8B-STATUS-CODE TO STATUS-WS                                  
114413     PERFORM IMS-STATUSCHECK                                              
114513     .                                                                    
114613     SKIP3                                                                
114713 IMS-GHU-WDA801 SECTION.                                                  
114813     STRING 'WDA801  (BESORTRT =' W-BESORTRT-X ')'                        
114913          DELIMITED BY SIZE INTO SSA1                                     
115013     MOVE '  GE' TO GOOD-STATUSCODES                                      
115113     CALL CBLTDLI USING GHU WDA8-PCB DLI-IO-WDA801 SSA1                   
115213     MOVE WDA8-STATUS-CODE TO STATUS-WS                                   
115313     PERFORM IMS-STATUSCHECK                                              
115413     .                                                                    
115513     SKIP3                                                                
115613 IMS-GN-WDA8 SECTION.                                                     
115713     MOVE '  GEGAGB' TO GOOD-STATUSCODES                                  
115813     CALL CBLTDLI USING GN WDA8-PCB DLI-IO-WDA8                           
115913     MOVE WDA8-STATUS-CODE TO STATUS-WS                                   
116013     PERFORM IMS-STATUSCHECK                                              
116113     .                                                                    
116213     SKIP3                                                                
116313 IMS-GU-WDA811-1 SECTION.                                                 
116413     STRING 'WDA801  (BESORTRT =' W-BESORTRT-1-X ')'                      
116513          DELIMITED BY SIZE INTO SSA1                                     
116613     STRING 'WDA811  (WDA811KY =' W-WDA811KY-1-X ')'                      
116713          DELIMITED BY SIZE INTO SSA2                                     
116813     MOVE '  GE' TO GOOD-STATUSCODES                                      
116913     CALL CBLTDLI USING GU WDA8-PCB DLI-IO-WDA811 SSA1 SSA2               
117013     MOVE WDA8-STATUS-CODE TO STATUS-WS                                   
117113     PERFORM IMS-STATUSCHECK                                              
117213     .                                                                    
117313     SKIP3                                                                
117413 IMS-GU-WDA811 SECTION.                                                   
117513     STRING 'WDA801  (BESORTRT =' W-BESORTRT-X ')'                        
117613          DELIMITED BY SIZE INTO SSA1                                     
117713     STRING 'WDA811  (WDA811KY =' W-WDA811KY-X ')'                        
117813          DELIMITED BY SIZE INTO SSA2                                     
117913     MOVE '  GE' TO GOOD-STATUSCODES                                      
118013     CALL CBLTDLI USING GU WDA8-PCB DLI-IO-WDA811 SSA1 SSA2               
118113     MOVE WDA8-STATUS-CODE TO STATUS-WS                                   
118213     PERFORM IMS-STATUSCHECK                                              
118313     .                                                                    
118413     SKIP3                                                                
118513 IMS-GNPF-WDA811 SECTION.                                                 
118613     STRING 'WDA801  (BESORTRT =' W-BESORTRT-X ')'                        
118713          DELIMITED BY SIZE INTO SSA1                                     
118813     STRING 'WDA811  *F(WDA811KY>=' W-WDA811KY-X ')'                      
118913          DELIMITED BY SIZE INTO SSA2                                     
119013     MOVE '  GEGB' TO GOOD-STATUSCODES                                    
119113     CALL CBLTDLI USING GN WDA8-PCB DLI-IO-WDA811 SSA1 SSA2               
119213     MOVE WDA8-STATUS-CODE TO STATUS-WS                                   
119313     PERFORM IMS-STATUSCHECK                                              
119413     .                                                                    
119513     SKIP3                                                                
119613 IMS-GHU-WDA811 SECTION.                                                  
119713     STRING 'WDA801  (BESORTRT =' W-BESORTRT-X ')'                        
119813          DELIMITED BY SIZE INTO SSA1                                     
119913     STRING 'WDA811  (WDA811KY =' W-WDA811KY-X ')'                        
120013          DELIMITED BY SIZE INTO SSA2                                     
120113     MOVE '  GE' TO GOOD-STATUSCODES                                      
120213     CALL CBLTDLI USING GHU WDA8-PCB DLI-IO-WDA811 SSA1 SSA2              
120313     MOVE WDA8-STATUS-CODE TO STATUS-WS                                   
120413     PERFORM IMS-STATUSCHECK                                              
120513     .                                                                    
120613     SKIP3                                                                
120713 IMS-ISRT-WDA801 SECTION.                                                 
120813     MOVE 'WDA801 ' TO SSA1                                               
120913     MOVE '  II' TO GOOD-STATUSCODES                                      
121013     CALL CBLTDLI USING ISRT WDA8-PCB DLI-IO-WDA801 SSA1                  
121113     MOVE WDA8-STATUS-CODE TO STATUS-WS                                   
121213     PERFORM IMS-STATUSCHECK                                              
121313     .                                                                    
121413     SKIP3                                                                
121513 IMS-DLET-WDA801 SECTION.                                                 
121613     MOVE '  ' TO GOOD-STATUSCODES                                        
121713     CALL CBLTDLI USING DLET WDA8-PCB DLI-IO-WDA801                       
121813     MOVE WDA8-STATUS-CODE TO STATUS-WS                                   
121913     PERFORM IMS-STATUSCHECK                                              
122013     .                                                                    
122113     EJECT                                                                
122213 IMS-ISRT-WDA811 SECTION.                                                 
122313     STRING 'WDA801  (BESORTRT =' W-BESORTRT-X ')'                        
122413          DELIMITED BY SIZE INTO SSA1                                     
122513     MOVE 'WDA811 ' TO SSA2                                               
122613     MOVE '  II' TO GOOD-STATUSCODES                                      
122713     CALL CBLTDLI USING ISRT WDA8-PCB DLI-IO-WDA811 SSA1 SSA2             
122813     MOVE WDA8-STATUS-CODE TO STATUS-WS                                   
122913     PERFORM IMS-STATUSCHECK                                              
123013     .                                                                    
123113     SKIP3                                                                
123213 IMS-DLET-WDA811 SECTION.                                                 
123313     MOVE '  ' TO GOOD-STATUSCODES                                        
123413     CALL CBLTDLI USING DLET WDA8-PCB DLI-IO-WDA811                       
123513     MOVE WDA8-STATUS-CODE TO STATUS-WS                                   
123613     PERFORM IMS-STATUSCHECK                                              
123713     .                                                                    
123813     EJECT                                                                
123913 IMS-STATUSCHECK SECTION.                                                 
124013     SET STATUS-IX TO 1                                                   
124113     SEARCH GOOD-STATUS                                                   
124213       AT END                                                             
124313         STRING ' INVALID STATUS CODE FROM IMS: ' STATUS-WS               
124413         DELIMITED BY SIZE INTO ERROR-TEXT                                
124513         CALL FELLOG                                                      
124613       WHEN GOOD-STATUS (STATUS-IX) = STATUS-WS                           
124713         CONTINUE                                                         
124813     END-SEARCH                                                           
125000     .                                                                    
