000100**********************************************************                
000200 ID DIVISION.                                                             
000300 PROGRAM-ID.     W4040100.                                                
000400 AUTHOR.         PRIYASOPHIA GALBAO.                                      
000500 DATE-WRITTEN.   23/07/05.                                                
000600 DATE-COMPILED.                                                           
000700                                                                          
000800*    FUNCTION:                                                            
000900*        REFILL AND TRANSFER RELEASE DAYS                                 
001000*                                                                         
001100*        THE PROGRAM UPDATES   WDB6                                       
001200*                                                                         
001300*    INDATA.                                                              
001400*        TRANSACTION: W4T401/U                                            
001500*        MID:         W4I40101                                            
001600*                                                                         
001700*    OUTDATA.                                                             
001800*        MOD:         W4O401N1                                            
001900                                                                          
002000     SKIP3                                                                
002100 ENVIRONMENT DIVISION.                                                    
002200                                                                          
002300 DATA DIVISION.                                                           
002400     EJECT                                                                
002500 WORKING-STORAGE SECTION.                                                 
002600 77  IDPGM                       PIC X(08)   VALUE 'W4040100'.            
002700                                                                          
002800*    --- WORK FIELDS FOR ERROR MESSAGES WHEN CALLING ABEND/FELLOG         
002900 77  ERROR-TEXT                  PIC X(80) VALUE SPACE.                   
003000                                                                          
003100 77  CURR-SECTION                PIC X(16)   VALUE 'MAIN'.                
003200 77  CURR-IMS-SECTION            PIC X(16).                               
003300                                                                          
003400 77  YES                         PIC X       VALUE 'Y'.                   
003500 77  NOO                         PIC X       VALUE 'N'.                   
003600                                                                          
003700 77  INDX                        PIC S9(4)   VALUE ZERO COMP SYNC.        
003800 77  MAX-INDX                    PIC S9(4)   VALUE +7  COMP SYNC.         
003900                                                                          
004000*    --- VALID VALUES FOR THE WEEKDAY TABLE                               
004100 77  WKRLSE-FLAG                 PIC X.                                   
004200     88  WKRLSE-VAL                          VALUE 'Y' 'N' ' '.           
004300                                                                          
004400 77  KDREFDG-CODE                PIC X.                                   
004500     88  KDREFDG-VAL                         VALUE 'E' 'O'                
004600                                                   'Y' 'N' ' '.           
004700*    --- SWITCHES                                                         
004800                                                                          
004900 77  ALL-SW                      PIC X       VALUE 'Y'.                   
005000     88  ALL-OK                              VALUE 'Y'.                   
005100                                                                          
005200 77  INDATA-SW                   PIC X       VALUE 'Y'.                   
005300     88  INDATA-OK                           VALUE 'Y'.                   
005400     88  INDATA-WRONG                        VALUE 'N'.                   
005500                                                                          
005600 77  KEYS-SW                     PIC X       VALUE 'Y'.                   
005700     88  KEYS-OK                             VALUE 'Y'.                   
005800     88  KEYS-WRONG                          VALUE 'N'.                   
005900                                                                          
006000 77  SAME-PAGE-SW                PIC X       VALUE 'Y'.                   
006100     88  SAME-PAGE-YES                       VALUE 'Y'.                   
006200     88  SAME-PAGE-NO                        VALUE 'N'.                   
006300                                                                          
006400 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
006500     88  OWN-MID                             VALUE '4401'.                
006600     88  HELP-MID                            VALUE '0551'.                
006700     EJECT                                                                
006800*    --- SUBPROGRAMS AND PARAMETER AREAS                                  
006900 01  GENERAL-SUBPROGRAMS.                                                 
007000     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
007100     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
007200     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
007300     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
007400     EJECT                                                                
007500*    --- PARAMETERS FOR SUBPROGRAM WMEDKONV                               
007600*01 -COPY WMEDAREA                                                        
007700     SKIP3                                                                
007800 01  MESSAGE-CODES.                                                       
007900     03  ERR-CORR-HILITE-FLDS    PIC X(3)    VALUE '001'.                 
008000     03  INF-PRESS-PF11          PIC X(3)    VALUE '003'.                 
008100     03  ERR-PF11-AND-NO-DATA    PIC X(3)    VALUE '011'.                 
008200     03  INF-UPDATE-DONE         PIC X(3)    VALUE '101'.                 
008300     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
008400     EJECT                                                                
008500*    --- PARAMETERS FOR SUB PROGRAM W005INIT                              
008600*                                                                         
008700 01  FILLER                      PIC X(16)   VALUE 'WMSGINIT'.            
008800     SKIP3                                                                
008900*01 -COPY WMSGINIT                                                        
009000     EJECT                                                                
009100*    --- AREA CONTAINING DATA TO BE SAVED BETWEEN DIALOG STEPS            
009200*                                                                         
009300*    --- AREAS FOR MFS AND SCREEN MANAGEMENT                              
009400*                                                                         
009500 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
009600     SKIP3                                                                
009700*01  MID -COPY W4I40101                                                   
009800     EJECT                                                                
009900 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
010000     SKIP3                                                                
010100*01  -COPY WMSGAREA                                                       
010200     EJECT                                                                
010300     03  MOD REDEFINES MSG-AREA.                                          
010400*      05  -COPY W4O40101                                                 
010500     EJECT                                                                
010600 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
010700     SKIP3                                                                
010800*01  -COPY WMFSAREA                                                       
010900     EJECT                                                                
011000*    --- WORK-AREAS FOR IMS-SECTIONS                                      
011100*                                                                         
011200 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
011300     SKIP3                                                                
011400 01  KEYS-FOR-DLI.                                                        
011500     03  W-IDDC-REC-X.                                                    
011600         05  W-IDDC-REC          PIC X(2)    VALUE SPACE.                 
011700     03  W-IDDC-REF-X.                                                    
011800         05  W-IDDC-REF          PIC X(2)    VALUE SPACE.                 
011900     SKIP2                                                                
012000*    --- STATUS CODES FROM IMS                                            
012100 01  STATUS-WS                   PIC XX.                                  
012200     88  SEGMENT-FOUND                       VALUE '  '.                  
012300     88  SEGMENT-MISSING                     VALUE 'GE'.                  
012400     SKIP2                                                                
012500 01  GOOD-STATUSCODES.                                                    
012600     03  GOOD-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
012700     SKIP3                                                                
012800 01  SSA1                        PIC X(64).                               
012900 01  SSA2                        PIC X(64).                               
013000     EJECT                                                                
013100*    --- IMS FUNCTION CODES                                               
013200*01  -COPY W0003                                                          
013300     EJECT                                                                
013400*    ---  DLI INPUT-OUTPUT AREA                                           
013500                                                                          
013600 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDB601'.                      
013700 01  DLI-IO-WDB601.                                                       
013800*    03  -COPY WDB601                                                     
013900     EJECT                                                                
014000 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDB616'.                      
014100 01  DLI-IO-WDB616.                                                       
014200*    03  -COPY WDB616                                                     
014300     EJECT                                                                
014400 LINKAGE SECTION.                                                         
014500*01  -COPY W0009   -PRE MSG-                                              
014600*01  -COPY W0008   -PRE WDP7-                                             
014700     05  FILLER                  PIC X.                                   
014800                                                                          
014900*01  -COPY W0008  -PRE WDB6-                                              
015000     05  FILLER                  PIC X.                                   
015100     EJECT                                                                
015200 PROCEDURE DIVISION  USING MSG-PCB WDP7-PCB WDB6-PCB.                     
015300 MAIN SECTION.                                                            
015400     ENTRY 'DLITCBL' USING MSG-PCB WDP7-PCB WDB6-PCB.                     
015500                                                                          
015600     PERFORM IMS-GET-MSG                                                  
015700     IF SEGMENT-FOUND                                                     
015800       PERFORM A-INIT                                                     
015900       PERFORM B-CHECK-KEYS                                               
016000       IF KEYS-OK                                                         
016100         IF MFS-UPDATE                                                    
016200           PERFORM G-CHECK-INPUT                                          
016300           IF INDATA-OK                                                   
016400             PERFORM H-UPDATE                                             
016500           END-IF                                                         
016600         ELSE                                                             
016700           IF MFS-FIRST                                                   
016800             PERFORM C-FIRST-PAGE                                         
016900           ELSE                                                           
017000             PERFORM E-SAME-PAGE                                          
017100           END-IF                                                         
017200         END-IF                                                           
017300         IF INDATA-OK AND ALL-OK                                          
017400           PERFORM F-READ-SHOW-INFO                                       
017500         END-IF                                                           
017600       END-IF                                                             
017700*    --- IF ANSWER TO SCREEN:       MSG-KVLL = MOD-LENGTH + 4             
017800*    --- IF PROGRAM-TO-PROGRAM-SWITCH:       = MOD-LENGTH + 17            
017900       COMPUTE MSG-KVLL = LENGTH OF MOD-W4O40101 + 4                      
018000       PERFORM IMS-INSERT-MSG                                             
018100     END-IF                                                               
018200                                                                          
018300     MOVE ZERO TO RETURN-CODE                                             
018400     GOBACK                                                               
018500     .                                                                    
018600     EJECT                                                                
018700 A-INIT SECTION.                                                          
018800     MOVE 'A-INIT          ' TO CURR-SECTION.                             
018900                                                                          
019000     IF MSG-DOUBLE-TRANSACTIONS                                           
019100       MOVE MSG-INDATA-MINUS-2-TRANSACT   TO MID-W4I40101                 
019200       MOVE MSG-IDTRANS-2  TO MFS-IDTRANS                                 
019300       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
019400     ELSE                                                                 
019500       MOVE MSG-INDATA-MINUS-1-TRANSACT  TO MID-W4I40101                  
019600       MOVE MSG-IDTRANS-1  TO MFS-IDTRANS                                 
019700       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
019800     END-IF                                                               
019900                                                                          
020000     MOVE MSG-KDTRTYP TO MFS-KDTRTYP                                      
020100     MOVE MSG-IDPFK   TO MFS-IDPFK                                        
020200     MOVE MFS-IDTRANS TO W-IDTRANS                                        
020300                                                                          
020400     MOVE LOW-VALUE TO MSG-AREA                                           
020500     MOVE 'W4O401N1' TO MFS-IDMOD                                         
020600     MOVE '4401' TO MOD-IDTRANS                                           
020700     MOVE MFS-ERASE-FIELD TO MOD-TEMFSFEL MOD-TEMFSINF                    
020800                                                                          
020900     IF OWN-MID OR HELP-MID                                               
021000       CONTINUE                                                           
021100     ELSE                                                                 
021200       MOVE SPACE TO MFS-KDTRTYP                                          
021300       MOVE '7' TO MFS-IDPFK                                              
021400     END-IF                                                               
021500     .                                                                    
021600     EJECT                                                                
021700 B-CHECK-KEYS SECTION.                                                    
021800     MOVE 'B-CHECK-KEYS    ' TO CURR-SECTION.                             
021900                                                                          
022000     MOVE ALL '+'           TO MSGI-WMSGINIT                              
022100     MOVE '001'             TO MSGI-KDCALL                                
022200     MOVE MSG-LTERM-NAME    TO MSGI-IDLTERM-USER                          
022300     MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                                
022400     MOVE '4401'            TO MSGI-IDTRANS                               
022500     MOVE MID-IDDC-REC-IN   TO MSGI-IDDC-REC                              
022600     MOVE MID-IDDC-REF-IN   TO MSGI-IDDC-REF                              
022700     CALL W005INIT USING MSGI-WMSGINIT WDP7-PCB                           
022800                                                                          
022900*    - LANGUAGE TO BE USED BY MEDKONV                                     
023000     MOVE MSGI-IDLAND-SPR   TO MED-IDSKYLT                                
023100                                                                          
023200     MOVE YES TO KEYS-SW                                                  
023300                 INDATA-SW                                                
023400                 SAME-PAGE-SW                                             
023500                                                                          
023600*    -- CHECK OF MOD-IDDC-REC-IN                                          
023700     MOVE MFS-ERASE-FIELD TO MOD-IDDC-REC-IN                              
023800                                                                          
023900     IF MID-IDDC-REC-IN NOT = ALL '+'                                     
024000       MOVE '7'         TO MFS-IDPFK                                      
024100       MOVE SPACE       TO MFS-KDTRTYP                                    
024200     END-IF                                                               
024300     IF MID-IDDC-REC-IN = ALL '+'                                         
024400       MOVE MSGI-IDDC-REC   TO W-IDDC-REC                                 
024500     ELSE                                                                 
024600       MOVE MID-IDDC-REC-IN TO W-IDDC-REC                                 
024700     END-IF                                                               
024800                                                                          
024900*    -- CHECK OF MOD-IDDC-REF-IN                                          
025000     MOVE MFS-ERASE-FIELD TO MOD-IDDC-REF-IN                              
025100                                                                          
025200     IF MID-IDDC-REF-IN NOT = ALL '+'                                     
025300       MOVE '7'         TO MFS-IDPFK                                      
025400       MOVE SPACE       TO MFS-KDTRTYP                                    
025500     END-IF                                                               
025600     IF MID-IDDC-REF-IN = ALL '+'                                         
025700       MOVE MSGI-IDDC-REF   TO W-IDDC-REF                                 
025800     ELSE                                                                 
025900       MOVE MID-IDDC-REF-IN TO W-IDDC-REF                                 
026000     END-IF                                                               
026100                                                                          
026200     PERFORM IMS-GU-WDB601                                                
026300                                                                          
026400     IF SEGMENT-MISSING                                                   
026500       MOVE 'GOODS RECEIVER MISSING'  TO MOD-TEMFSFEL                     
026600       PERFORM MFS-ERASE-FIELD-IN                                         
026700       PERFORM MFS-ERASE-FIELD-OUT                                        
026800     ELSE                                                                 
026900       PERFORM IMS-GHNP-WDB616                                            
027000       IF SEGMENT-MISSING                                                 
027100         MOVE 'GOODS SENDER MISSING'  TO MOD-TEMFSFEL                     
027200         PERFORM MFS-ERASE-FIELD-IN                                       
027300         PERFORM MFS-ERASE-FIELD-OUT                                      
027400       END-IF                                                             
027500     END-IF                                                               
027600                                                                          
027700     IF KEYS-OK                                                           
027800       MOVE MSGI-IDDC-REC    TO MOD-IDDC-REC-UT                           
027900       MOVE MSGI-IDDC-REF    TO MOD-IDDC-REF-UT                           
028000     ELSE                                                                 
028100       MOVE MFS-ERASE-FIELD TO MOD-IDDC-REC-UT                            
028200       MOVE MFS-ERASE-FIELD TO MOD-IDDC-REF-UT                            
028300     END-IF                                                               
028400                                                                          
028500     IF KEYS-WRONG                                                        
028600       MOVE ERR-WRONG-KEY TO MED-IDMFSFEL                                 
028700       CALL WMEDKONV USING MED-WMEDAREA                                   
028800       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
028900       PERFORM MFS-ERASE-FIELD-IN                                         
029000       PERFORM MFS-ERASE-FIELD-OUT                                        
029100     END-IF                                                               
029200     .                                                                    
029300     EJECT                                                                
029400 C-FIRST-PAGE SECTION.                                                    
029500     MOVE 'C-FIRST-PAGE    ' TO CURR-SECTION.                             
029600                                                                          
029700     PERFORM MFS-ERASE-FIELD-IN                                           
029800     .                                                                    
029900     EJECT                                                                
030000 E-SAME-PAGE SECTION.                                                     
030100     MOVE 'E-SAME-PAGE     ' TO CURR-SECTION.                             
030200                                                                          
031300     IF MID-WKDAYTABLES(1) = ALL '+' AND                                  
031400        MID-WKDAYTABLES(2) = ALL '+' AND                                  
031500        MID-WKDAYTABLES(3) = ALL '+' AND                                  
031600        MID-WKDAYTABLES(4) = ALL '+' AND                                  
031700        MID-WKDAYTABLES(5) = ALL '+' AND                                  
031800        MID-WKDAYTABLES(6) = ALL '+' AND                                  
031900        MID-WKDAYTABLES(7) = ALL '+' AND                                  
032000        MID-FLFRAKDG = ALL '+'                                            
032100       PERFORM MFS-ERASE-FIELD-IN                                         
032200       MOVE YES        TO ALL-SW                                          
032300     ELSE                                                                 
032400                                                                          
032500       PERFORM EB-DECIDE-SAME-PAGE                                        
032600       IF SAME-PAGE-NO                                                    
032700         MOVE NOO        TO ALL-SW                                        
032800         MOVE INF-PRESS-PF11 TO MED-IDMFSINF                              
032900         CALL WMEDKONV USING MED-WMEDAREA                                 
033000         MOVE MED-MFSINF TO MOD-TEMFSINF                                  
033100       END-IF                                                             
033200       PERFORM MFS-DONT-TOUCH-FIELD-IN                                    
033300       PERFORM MFS-DONT-TOUCH-FIELD-OUT                                   
033400       PERFORM EA-MID-INDATA-TO-MOD                                       
033500     END-IF                                                               
033600     .                                                                    
033700     EJECT                                                                
033800 EA-MID-INDATA-TO-MOD SECTION.                                            
033900     MOVE 'EA-MID-INDATA-  ' TO CURR-SECTION.                             
034000                                                                          
034100     PERFORM VARYING INDX FROM 1 BY 1 UNTIL INDX > MAX-INDX               
034200        IF MID-FLREFDAY (INDX) NOT = ALL '+'                              
034300          MOVE MFS-DO-NOT-TOUCH-FIELD TO MOD-FLREFDAY  (INDX)             
034400          MOVE MID-FLREFDAY (INDX)    TO MOD-FLREFDAY (INDX)              
034500          MOVE MFS-ADD-READ-FIELD     TO MOD-FLREFDAY-ATTR (INDX)         
034600        ELSE                                                              
034700          MOVE MFS-ERASE-FIELD        TO MOD-FLREFDAY  (INDX)             
034800        END-IF                                                            
034900        IF MID-FLREFBLK (INDX) NOT = ALL '+'                              
035000          MOVE MID-FLREFBLK (INDX)    TO MOD-FLREFBLK (INDX)              
035100          MOVE MFS-ADD-READ-FIELD     TO MOD-FLREFBLK-ATTR (INDX)         
035200        ELSE                                                              
035300          MOVE MFS-ERASE-FIELD        TO MOD-FLREFBLK  (INDX)             
035400        END-IF                                                            
035500        IF MID-KDREFDG  (INDX) NOT = ALL '+'                              
035600          MOVE MID-KDREFDG  (INDX)    TO MOD-KDREFDG  (INDX)              
035700          MOVE MFS-ADD-READ-FIELD     TO MOD-KDREFDG-ATTR (INDX)          
035800        ELSE                                                              
035900          MOVE MFS-ERASE-FIELD        TO MOD-KDREFDG   (INDX)             
036000        END-IF                                                            
036100     END-PERFORM                                                          
036200     IF MID-FLFRAKDG  NOT = ALL '+'                                       
036300        MOVE MID-FLFRAKDG           TO MOD-FLFRAKDG                       
036400        MOVE MFS-ADD-READ-FIELD     TO MOD-FLFRAKDG-ATTR                  
036500     ELSE                                                                 
036600        MOVE MFS-ERASE-FIELD        TO MOD-FLFRAKDG                       
036700     END-IF                                                               
036800     .                                                                    
036900     EJECT                                                                
037000 EB-DECIDE-SAME-PAGE SECTION.                                             
037100     MOVE 'EA-DECIDE-SAME- ' TO CURR-SECTION.                             
037200                                                                          
037300     PERFORM VARYING INDX FROM 1 BY 1 UNTIL INDX > MAX-INDX               
037400        IF (INDX = 1 OR 6)                                                
037500          CONTINUE                                                        
037600        ELSE                                                              
037700          IF (REF-FLREFBLK (INDX) = 'J' AND                               
037800              MID-FLREFBLK (INDX) = 'Y') OR                               
037900             (REF-FLREFBLK (INDX) = 'N' AND                               
038000              MID-FLREFBLK (INDX) = ' ') OR                               
038100             (REF-FLREFBLK (INDX) = MID-FLREFBLK (INDX))                  
038200            CONTINUE                                                      
038300          ELSE                                                            
038400            MOVE NOO                TO SAME-PAGE-SW                       
038500          END-IF                                                          
038600          IF (REF-FLREFDAY (INDX) = 'J' AND                               
038700              MID-FLREFDAY (INDX) = 'Y') OR                               
038800             (REF-FLREFDAY (INDX) = 'N' AND                               
038900              MID-FLREFDAY (INDX) = ' ') OR                               
039000             (REF-FLREFDAY (INDX) = MID-FLREFDAY (INDX))                  
039100            CONTINUE                                                      
039200          ELSE                                                            
039300            MOVE NOO                TO SAME-PAGE-SW                       
039400          END-IF                                                          
039500          IF (REF-KDREFDG  (INDX) = 'J' AND                               
039600              MID-KDREFDG  (INDX) = 'Y') OR                               
039700             (REF-KDREFDG  (INDX) = 'N' AND                               
039800              MID-KDREFDG  (INDX) = ' ') OR                               
039900             (REF-KDREFDG  (INDX) = MID-KDREFDG  (INDX))                  
040000            CONTINUE                                                      
040100          ELSE                                                            
040200            MOVE NOO                TO SAME-PAGE-SW                       
040300          END-IF                                                          
040400        END-IF                                                            
040500     END-PERFORM                                                          
040600     IF (REF-FLFRAKDG = 'J' AND                                           
040700         MID-FLFRAKDG = 'Y') OR                                           
040800        (REF-FLFRAKDG = MID-FLFRAKDG)                                     
040900        CONTINUE                                                          
041000     ELSE                                                                 
041100        MOVE NOO                TO SAME-PAGE-SW                           
041200     END-IF                                                               
041300     .                                                                    
041400     EJECT                                                                
041500 F-READ-SHOW-INFO SECTION.                                                
041600     MOVE 'F-READ-SHOW-INFO' TO CURR-SECTION.                             
041700                                                                          
041800     PERFORM VARYING INDX FROM 1 BY 1 UNTIL INDX > MAX-INDX               
041900        IF (INDX = 1 OR 6)                                                
042000          MOVE SPACE                TO MOD-FLREFDAY(INDX)                 
042100                                       MOD-FLREFBLK(INDX)                 
042200                                       MOD-KDREFDG(INDX)                  
042300          MOVE MFS-FORMAT-DEFAULT-ATTR                                    
042400                                    TO MOD-FLREFBLK-ATTR(INDX)            
042500                                       MOD-FLREFDAY-ATTR(INDX)            
042600                                       MOD-KDREFDG-ATTR(INDX)             
042700        ELSE                                                              
042800          IF REF-FLREFBLK(INDX) = 'J'                                     
042900            MOVE 'Y'                TO MOD-FLREFBLK(INDX)                 
043000          ELSE                                                            
043100            IF REF-FLREFBLK(INDX) = 'N'                                   
043200              MOVE SPACE            TO MOD-FLREFBLK(INDX)                 
043300            END-IF                                                        
043400          END-IF                                                          
043500          MOVE MFS-ADD-READ-FIELD    TO MOD-FLREFBLK-ATTR(INDX)           
043600          IF REF-FLREFDAY(INDX) = 'J'                                     
043700            MOVE 'Y'                 TO MOD-FLREFDAY(INDX)                
043800          ELSE                                                            
043900            IF REF-FLREFDAY(INDX) = 'N'                                   
044000              MOVE SPACE             TO MOD-FLREFDAY(INDX)                
044100            END-IF                                                        
044200          END-IF                                                          
044300          MOVE MFS-ADD-READ-FIELD    TO MOD-FLREFDAY-ATTR(INDX)           
044400          IF REF-KDREFDG(INDX) =  'E' OR 'O'                              
044500            MOVE REF-KDREFDG(INDX)   TO MOD-KDREFDG(INDX)                 
044600          ELSE                                                            
044610            IF REF-KDREFDG(INDX) =  'J'                                   
044620              MOVE 'Y'                 TO MOD-KDREFDG(INDX)               
044630            ELSE                                                          
044700              IF REF-KDREFDG(INDX)  = 'N'                                 
044800                MOVE SPACE             TO MOD-KDREFDG(INDX)               
044900              END-IF                                                      
045000            END-IF                                                        
045010          END-IF                                                          
045100          MOVE MFS-ADD-READ-FIELD    TO MOD-KDREFDG-ATTR(INDX)            
045200        END-IF                                                            
045300     END-PERFORM                                                          
045310     IF REF-FLFRAKDG      =  'J'                                          
045320       MOVE 'Y'                 TO MOD-FLFRAKDG                           
045340     ELSE                                                                 
045400       MOVE REF-FLFRAKDG        TO MOD-FLFRAKDG                           
045410     END-IF                                                               
045500     MOVE MFS-ADD-READ-FIELD  TO MOD-FLFRAKDG-ATTR                        
045600                                                                          
045700     .                                                                    
045800     EJECT                                                                
045900 G-CHECK-INPUT SECTION.                                                   
046000     MOVE 'C-CHECK-INPUT   ' TO CURR-SECTION.                             
046100                                                                          
046200     MOVE YES  TO INDATA-SW                                               
046300     MOVE SPACE TO MED-IDMFSFEL                                           
046400     IF MID-WKDAYTABLES(1) = ALL '+' AND                                  
046500        MID-WKDAYTABLES(2) = ALL '+' AND                                  
046600        MID-WKDAYTABLES(3) = ALL '+' AND                                  
046700        MID-WKDAYTABLES(4) = ALL '+' AND                                  
046800        MID-WKDAYTABLES(5) = ALL '+' AND                                  
046900        MID-WKDAYTABLES(6) = ALL '+' AND                                  
047000        MID-WKDAYTABLES(7) = ALL '+' AND                                  
047100        MID-FLFRAKDG = ALL '+'                                            
047200       MOVE ERR-PF11-AND-NO-DATA TO MED-IDMFSFEL                          
047300       MOVE NOO TO INDATA-SW                                              
047400     ELSE                                                                 
047500       IF INDATA-OK                                                       
047600         PERFORM GA-CHECK-FLREFDAY                                        
047700         PERFORM GB-CHECK-FLREFBLK                                        
047800         PERFORM GC-CHECK-KDREFDG                                         
047900         PERFORM GD-CHECK-FLFRAKDG                                        
048000       END-IF                                                             
048100     END-IF                                                               
048200                                                                          
048300     IF INDATA-WRONG                                                      
048400       IF MED-IDMFSFEL = SPACE                                            
048500         MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL                        
048600       END-IF                                                             
048700       CALL WMEDKONV USING MED-WMEDAREA                                   
048800       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
048900       PERFORM MFS-DONT-TOUCH-FIELD-OUT                                   
049000       PERFORM MFS-DONT-TOUCH-FIELD-IN                                    
049100     END-IF                                                               
049200     .                                                                    
049300     EJECT                                                                
049400 GA-CHECK-FLREFDAY SECTION.                                               
049500     MOVE 'GA-CHECK-FLREFDAY' TO CURR-SECTION.                            
049600                                                                          
049700     PERFORM VARYING INDX FROM 1 BY 1 UNTIL INDX > MAX-INDX               
049800                                                                          
049900         IF (INDX = 1 OR 6) OR                                            
050000            (MID-FLREFDAY (INDX) = ALL '+')                               
050100           CONTINUE                                                       
050200         ELSE                                                             
050300           MOVE MID-FLREFDAY (INDX) TO WKRLSE-FLAG                        
050400           IF WKRLSE-VAL                                                  
050500             MOVE MFS-ALPHA-FIELD-OK  TO                                  
050600                  MOD-FLREFDAY-ATTR (INDX)                                
050700           ELSE                                                           
050800             MOVE MFS-ALPHA-FIELD-WRONG TO                                
050900                  MOD-FLREFDAY-ATTR (INDX)                                
051000             MOVE NOO TO INDATA-SW                                        
051100           END-IF                                                         
051200         END-IF                                                           
051300                                                                          
051400     END-PERFORM                                                          
051500     .                                                                    
051600     EJECT                                                                
051700 GB-CHECK-FLREFBLK SECTION.                                               
051800     MOVE 'GB-CHECK-FLREFBLK' TO CURR-SECTION.                            
051900                                                                          
052000     MOVE +1    TO INDX                                                   
052100                                                                          
052200     PERFORM VARYING INDX FROM 1 BY 1 UNTIL INDX > MAX-INDX               
052300                                                                          
052400         IF (INDX = 1 OR 6) OR                                            
052500            (MID-FLREFBLK (INDX) = ALL '+')                               
052600           CONTINUE                                                       
052700         ELSE                                                             
052800           MOVE MID-FLREFBLK (INDX) TO WKRLSE-FLAG                        
052900           IF WKRLSE-VAL                                                  
053000             MOVE MFS-ALPHA-FIELD-OK   TO                                 
053100                  MOD-FLREFBLK-ATTR (INDX)                                
053200           ELSE                                                           
053300             MOVE MFS-ALPHA-FIELD-WRONG TO                                
053400                  MOD-FLREFBLK-ATTR (INDX)                                
053500             MOVE NOO TO INDATA-SW                                        
053600           END-IF                                                         
053700         END-IF                                                           
053800                                                                          
053900     END-PERFORM                                                          
054000     .                                                                    
054100     EJECT                                                                
054200 GC-CHECK-KDREFDG SECTION.                                                
054300     MOVE 'GC-CHECK-KDREFDG ' TO CURR-SECTION.                            
054400                                                                          
054500     PERFORM VARYING INDX FROM 1 BY 1 UNTIL INDX > MAX-INDX               
054600                                                                          
054700         IF (INDX = 1 OR 6) OR                                            
054800            (MID-KDREFDG (INDX) = ALL '+')                                
054900           CONTINUE                                                       
055000         ELSE                                                             
055100           MOVE MID-KDREFDG (INDX)  TO KDREFDG-CODE                       
055200           IF KDREFDG-VAL                                                 
055300             MOVE MFS-ALPHA-FIELD-OK   TO                                 
055400                  MOD-KDREFDG-ATTR (INDX)                                 
055500           ELSE                                                           
055600             MOVE MFS-ALPHA-FIELD-WRONG  TO                               
055700                  MOD-KDREFDG-ATTR (INDX)                                 
055800             MOVE NOO TO INDATA-SW                                        
055900           END-IF                                                         
056000         END-IF                                                           
056100                                                                          
056200     END-PERFORM                                                          
056300     .                                                                    
056400 GD-CHECK-FLFRAKDG SECTION.                                               
056500     MOVE 'GD-CHECK-FLFRAKDG' TO CURR-SECTION.                            
056600                                                                          
056700     IF (MID-FLFRAKDG = 'Y' OR 'N' )                                      
056800         MOVE MFS-ALPHA-FIELD-OK   TO                                     
056900              MOD-FLFRAKDG-ATTR                                           
057000     ELSE                                                                 
057100         MOVE MFS-ALPHA-FIELD-WRONG  TO                                   
057200              MOD-FLFRAKDG-ATTR                                           
057300         MOVE NOO TO INDATA-SW                                            
057400     END-IF                                                               
057500     .                                                                    
057600     EJECT                                                                
057700 H-UPDATE SECTION.                                                        
057800     MOVE 'H-UPDATE        ' TO CURR-SECTION.                             
057900                                                                          
058000     PERFORM HA-POPULATE-REPLDATA                                         
058100     IF INDATA-WRONG                                                      
058200       IF MED-IDMFSFEL = SPACE                                            
058300         MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL                        
058400       END-IF                                                             
058500       CALL WMEDKONV USING MED-WMEDAREA                                   
058600       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
058700       PERFORM MFS-DONT-TOUCH-FIELD-OUT                                   
058800       PERFORM MFS-DONT-TOUCH-FIELD-IN                                    
058900     ELSE                                                                 
059000       PERFORM IMS-REPL-WDB616                                            
059100       MOVE INF-UPDATE-DONE TO MED-IDMFSINF                               
059200       CALL WMEDKONV USING MED-WMEDAREA                                   
059300       MOVE MED-MFSINF TO MOD-TEMFSINF                                    
059400       PERFORM MFS-FORM-ATTR                                              
059500       PERFORM MFS-ERASE-FIELD-IN                                         
059600     END-IF                                                               
059700     .                                                                    
059800     EJECT                                                                
146200 HA-POPULATE-REPLDATA SECTION.                                            
146300     MOVE 'HA-POPULATE-REPL' TO CURR-SECTION.                             
146400                                                                          
149610     PERFORM VARYING INDX FROM 1 BY 1 UNTIL INDX > MAX-INDX               
149611                                                                          
149612        IF (MID-FLREFDAY (INDX)  NOT = ALL '+' OR                         
149620            MID-FLREFBLK (INDX)  NOT = ALL '+' OR                         
149630            MID-KDREFDG  (INDX)  NOT = ALL '+')                           
149700                                                                          
149800            IF MID-FLREFDAY (INDX) NOT = ALL '+'                          
149801              IF MID-FLREFDAY (INDX) = SPACE                              
149802                IF REF-FLREFDAY (INDX) = 'N' OR 'J'                       
149900                  MOVE 'N'               TO REF-FLREFDAY (INDX)           
149901                END-IF                                                    
149902              ELSE                                                        
149903                IF MID-FLREFDAY (INDX) = 'Y'                              
149904                  MOVE 'J'                 TO REF-FLREFDAY (INDX)         
149905                ELSE                                                      
149906                  MOVE MID-FLREFDAY (INDX) TO REF-FLREFDAY (INDX)         
149907                END-IF                                                    
149908              END-IF                                                      
150000            END-IF                                                        
150100                                                                          
150200            IF MID-FLREFBLK (INDX) NOT = ALL '+'                          
150300              IF MID-FLREFBLK (INDX) = SPACE                              
150301                IF REF-FLREFBLK (INDX) = 'N' OR 'J'                       
150302                  MOVE 'N'               TO REF-FLREFBLK (INDX)           
150303                END-IF                                                    
150304              ELSE                                                        
150305                IF MID-FLREFBLK (INDX) = 'Y'                              
150306                  MOVE 'J'                 TO REF-FLREFBLK (INDX)         
150307                ELSE                                                      
150308                  MOVE MID-FLREFBLK (INDX) TO REF-FLREFBLK (INDX)         
150309                END-IF                                                    
150310              END-IF                                                      
150400            END-IF                                                        
150500                                                                          
150600            IF MID-KDREFDG  (INDX) NOT = ALL '+'                          
150601              IF MID-KDREFDG  (INDX) = SPACE                              
150602                IF REF-KDREFDG  (INDX) = 'N' OR 'J' OR 'E' OR 'O'         
150603                  MOVE 'N'               TO REF-KDREFDG  (INDX)           
150604                END-IF                                                    
150605              ELSE                                                        
150606                IF MID-KDREFDG  (INDX) = 'Y'                              
150607                  MOVE 'J'                 TO REF-KDREFDG  (INDX)         
150608                ELSE                                                      
150700                  MOVE MID-KDREFDG (INDX)  TO REF-KDREFDG (INDX)          
150800                END-IF                                                    
150801              END-IF                                                      
150810            END-IF                                                        
150900                                                                          
154200        END-IF                                                            
154300                                                                          
154400     END-PERFORM                                                          
154500     IF MID-FLFRAKDG  NOT = ALL '+'                                       
154510       IF MID-FLFRAKDG  = 'Y'                                             
154511          MOVE 'J'          TO REF-FLFRAKDG                               
154520       ELSE                                                               
154600          MOVE MID-FLFRAKDG TO REF-FLFRAKDG                               
154700       END-IF                                                             
154710     END-IF                                                               
154800                                                                          
154900     .                                                                    
155000     EJECT                                                                
155100 MFS-ERASE-FIELD-OUT SECTION.                                             
155200                                                                          
155300*    --- ALLA UTDATA-FÄLT                                                 
155400     PERFORM VARYING INDX FROM 1 BY 1 UNTIL INDX > MAX-INDX               
155500        MOVE MFS-ERASE-FIELD   TO MOD-FLREFDAY (INDX)                     
155600                                  MOD-FLREFBLK (INDX)                     
155700                                  MOD-KDREFDG (INDX)                      
155800     END-PERFORM                                                          
155900     MOVE MFS-ERASE-FIELD      TO MOD-FLFRAKDG                            
156000     .                                                                    
156100     SKIP3                                                                
156200 MFS-ERASE-FIELD-IN SECTION.                                              
156300                                                                          
156400*    --- ALLA INDATA-FÄLT                                                 
156500     PERFORM VARYING INDX FROM 1 BY 1 UNTIL INDX > MAX-INDX               
156600        MOVE MFS-ERASE-FIELD   TO MOD-FLREFDAY (INDX)                     
156700                                  MOD-FLREFBLK (INDX)                     
156800                                  MOD-KDREFDG (INDX)                      
156900     END-PERFORM                                                          
157000     MOVE MFS-ERASE-FIELD      TO MOD-FLFRAKDG                            
157100     .                                                                    
157200     EJECT                                                                
157300 MFS-DONT-TOUCH-FIELD-OUT  SECTION.                                       
157400                                                                          
157500*    --- ALLA UTDATA-FÄLT                                                 
157600     PERFORM VARYING INDX FROM 1 BY 1 UNTIL INDX > MAX-INDX               
157700        MOVE MFS-DO-NOT-TOUCH-FIELD  TO MOD-FLREFDAY (INDX)               
157800                                        MOD-FLREFBLK (INDX)               
157900                                        MOD-KDREFDG (INDX)                
158000     END-PERFORM                                                          
158100     MOVE MFS-DO-NOT-TOUCH-FIELD     TO MOD-FLFRAKDG                      
158200     .                                                                    
158300     SKIP3                                                                
158400 MFS-DONT-TOUCH-FIELD-IN  SECTION.                                        
158500                                                                          
158600*    --- ALLA INDATA-FÄLT                                                 
158700     PERFORM VARYING INDX FROM 1 BY 1 UNTIL INDX > MAX-INDX               
158800        MOVE MFS-DO-NOT-TOUCH-FIELD  TO MOD-FLREFDAY (INDX)               
158900                                        MOD-FLREFBLK (INDX)               
159000                                        MOD-KDREFDG (INDX)                
159100     END-PERFORM                                                          
159200     MOVE MFS-DO-NOT-TOUCH-FIELD     TO MOD-FLFRAKDG                      
159300     .                                                                    
159400     EJECT                                                                
159500 MFS-FORM-ATTR SECTION.                                                   
159600                                                                          
159700     PERFORM VARYING INDX FROM 1 BY 1 UNTIL INDX > MAX-INDX               
159800        MOVE MFS-FORMAT-DEFAULT-ATTR TO MOD-FLREFDAY-ATTR (INDX)          
159900                                        MOD-FLREFBLK-ATTR (INDX)          
160000                                        MOD-KDREFDG-ATTR (INDX)           
160100     END-PERFORM                                                          
160200     MOVE MFS-FORMAT-DEFAULT-ATTR    TO MOD-FLFRAKDG                      
160300     .                                                                    
160400     SKIP2                                                                
160500* --- IMS SECTIONS ---                                                    
160600     SKIP3                                                                
160700 IMS-GET-MSG SECTION.                                                     
160800     MOVE 'IMS-GET-MSG     '   TO CURR-IMS-SECTION                        
160900                                                                          
161000     MOVE '  QC' TO GOOD-STATUSCODES                                      
161100     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
161200     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
161300     PERFORM IMS-STATUSCHECK                                              
161400     .                                                                    
161500     SKIP3                                                                
161600 IMS-INSERT-MSG SECTION.                                                  
161700     MOVE 'IMS-INSERT-MSG  '   TO CURR-IMS-SECTION                        
161800                                                                          
161900     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
162000     MOVE SPACE TO GOOD-STATUSCODES                                       
162100     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
162200     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
162300     PERFORM IMS-STATUSCHECK                                              
162400     .                                                                    
162500     EJECT                                                                
162600 IMS-GU-WDB601 SECTION.                                                   
162700     MOVE 'IMS-GU-WDB601   '   TO CURR-IMS-SECTION                        
162800                                                                          
162900     STRING 'WDB601  (IDDC     =' W-IDDC-REC-X ')'                        
163000          DELIMITED BY SIZE INTO SSA1                                     
163100     MOVE '  GE' TO GOOD-STATUSCODES                                      
163200     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-WDB601 SSA1                    
163300     MOVE WDB6-STATUS-CODE TO STATUS-WS                                   
163400     PERFORM IMS-STATUSCHECK                                              
163500     .                                                                    
163600     SKIP3                                                                
163700 IMS-GHNP-WDB616 SECTION.                                                 
163800     MOVE 'IMS-GHNP-WDB616 '   TO CURR-IMS-SECTION                        
163900                                                                          
164000     STRING 'WDB616  (IDDCREF  =' W-IDDC-REF-X ')'                        
164100          DELIMITED BY SIZE INTO SSA1                                     
164200     MOVE '  GE' TO GOOD-STATUSCODES                                      
164300     CALL CBLTDLI USING GHNP WDB6-PCB DLI-IO-WDB616 SSA1                  
164400     MOVE WDB6-STATUS-CODE TO STATUS-WS                                   
164500     PERFORM IMS-STATUSCHECK                                              
164600     .                                                                    
164700     SKIP3                                                                
164800 IMS-REPL-WDB616 SECTION.                                                 
164900     MOVE 'IMS-REPL-WDB616 '   TO CURR-IMS-SECTION                        
165000                                                                          
165100     MOVE '  ' TO GOOD-STATUSCODES                                        
165200     CALL CBLTDLI USING REPL WDB6-PCB DLI-IO-WDB616                       
165300     MOVE WDB6-STATUS-CODE TO STATUS-WS                                   
165400     PERFORM IMS-STATUSCHECK                                              
165500     .                                                                    
165600     EJECT                                                                
165700 IMS-STATUSCHECK SECTION.                                                 
165800                                                                          
165900     SET STATUS-IX TO 1                                                   
166000     SEARCH GOOD-STATUS                                                   
166100       AT END                                                             
166200         STRING ' INVALID STATUS CODE FROM IMS: ' STATUS-WS               
166300         DELIMITED BY SIZE INTO ERROR-TEXT                                
166400         CALL FELLOG                                                      
166500       WHEN GOOD-STATUS (STATUS-IX) = STATUS-WS                           
166600         CONTINUE                                                         
166700     END-SEARCH                                                           
166800     .                                                                    
