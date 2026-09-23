000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W5014500.                                                
000300 AUTHOR.         ARCHANA BHAT.                                            
000400 DATE-WRITTEN.   15/09/10.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNCTION:                                                            
000800*        THIS PROGRAM UPDATES THE STANDARD PRICE PROCESS                  
000900*        PARAMETERS ON WDR2(WDGX5108)                                     
001000*                                                                         
001100*                                                                         
001200*    INDATA.                                                              
001300*        TRANSACTION: W5T145                                              
001400*        MID:         W5I14501                                            
001500*                                                                         
001600*    OUTDATA.                                                             
001700*        MOD:         W5O14501                                            
001800                                                                          
001900     SKIP3                                                                
002000 ENVIRONMENT DIVISION.                                                    
002100                                                                          
002200 DATA DIVISION.                                                           
002300     EJECT                                                                
002400 WORKING-STORAGE SECTION.                                                 
002500 77  IDPGM                       PIC X(08)   VALUE 'W5014500'.            
002600                                                                          
002700*    --- WORK FIELDS FOR ERROR MESSAGES WHEN CALLING ABEND/FELLOG         
002800 77  ERROR-TEXT                  PIC X(80) VALUE SPACE.                   
002900                                                                          
003000 77  YES                         PIC X       VALUE 'J'.                   
003100 77  NOO                         PIC X       VALUE 'N'.                   
003200 77  TODAYS-DATE                 PIC 9(6)    VALUE ZERO.                  
003211 77  WS-IDFRIDATA                PIC X(25).                               
003220 77  WS-REEMBINF-MIN             PIC S9(3)V9(2) VALUE -99.9.              
003221 77  WS-REEMBINF-MAX             PIC S9(3)V9(2) VALUE +99.9.              
003222 77  WS-REEMBINF-NEW             PIC S9(3)V9(2).                          
003230 77  WS-SULSNIV-MIN-NEW          PIC S9(11).                              
003240 77  WS-SULSNIV-MAX-NEW          PIC S9(11).                              
003250 77  WS-SULSNIV-MIN-ACTUAL       PIC S9(5)   VALUE ZERO.                  
003260 77  WS-SULSNIV-MAX-ACTUAL       PIC S9(5)   VALUE ZERO.                  
003300                                                                          
003400*    --- WORK FIELDS FOR ACTUAL KEYVALUES OF SCREEN                       
003500                                                                          
003600                                                                          
003700 77  KEYS-SW                     PIC X       VALUE 'J'.                   
003800     88  KEYS-OK                             VALUE 'J'.                   
003900     88  KEYS-WRONG                          VALUE 'N'.                   
004000                                                                          
004100 77  INDATA-SW                   PIC X       VALUE 'J'.                   
004200     88  INDATA-OK                           VALUE 'J'.                   
004300     88  INDATA-WRONG                        VALUE 'N'.                   
004400                                                                          
004500 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
004600     88  OWN-MID                             VALUE '5145'.                
004700     88  GOOD-MID                            VALUE '5141' '5142'          
004800                                                   '5143' '5144'          
004900                                                   '5145' '5146'          
005000                                                   '5147' '5148'          
005100                                                   '5149'.                
005200     88  HELP-MID                            VALUE '0551'.                
005300     EJECT                                                                
005390                                                                          
005400*    --- SUBPROGRAMS AND PARAMETER AREAS                                  
005500 01  GENERAL-SUBPROGRAMS.                                                 
005600     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
005700     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
005800     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
005900     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
005910     03  WDECEDIT                PIC X(8)    VALUE 'WDECEDIT'.            
006000     EJECT                                                                
006100*    --- PARAMETERS FOR SUBPROGRAM WMEDKONV                               
006200*01 -COPY WMEDAREA                                                        
006300     SKIP3                                                                
006400 01  MESSAGE-CODES.                                                       
006500     03  ERR-CORR-HILITE-FLDS    PIC X(3)    VALUE '001'.                 
006600     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
006700     03  ERR-NOT-NUMERIC         PIC X(3)    VALUE '020'.                 
006800     03  ERR-PF11-AND-NO-DATA    PIC X(3)    VALUE '011'.                 
006900     03  INF-UPDATE-DONE         PIC X(3)    VALUE '101'.                 
007000     03  INF-PRESS-PF11          PIC X(3)    VALUE '003'.                 
007300     EJECT                                                                
007301*    --- PARAMETERS FOR SUB PROGRAM WDECEDIT                              
007310*01  -COPY WDECAREA                                                       
007320     EJECT                                                                
007400*    --- PARAMETERS FOR SUB PROGRAM W005INIT                              
007500*                                                                         
007600 01  FILLER                      PIC X(16)   VALUE 'WMSGINIT'.            
007700     SKIP3                                                                
007800*01 -COPY WMSGINIT                                                        
007900     EJECT                                                                
008500*    --- AREAS FOR MFS AND SCREEN MANAGEMENT                              
008600*                                                                         
008700 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
008800     SKIP3                                                                
008900*01  MID -COPY W5I14501                                                   
009000     EJECT                                                                
009100 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
009200     SKIP3                                                                
009300*01  -COPY WMSGAREA                                                       
009400     EJECT                                                                
009500     03  MOD REDEFINES MSG-AREA.                                          
009600*      05  -COPY W5O14501                                                 
009700     EJECT                                                                
009800 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
009900     SKIP3                                                                
010000*01  -COPY WMFSAREA                                                       
010100     EJECT                                                                
010200                                                                          
010300 01  FILLER                      PIC X(16)   VALUE 'IDFTG VALUES'.        
010400     SKIP3                                                                
010500*01  -COPY WWIDFTG                                                        
010600*    --- WORK-AREAS FOR IMS-SECTIONS                                      
010700*                                                                         
010800 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
010900     SKIP3                                                                
011000 01  KEYS-FOR-DLI.                                                        
011100   03  W-WDGXKEY-X.                                                       
011200     05  W-IDHTYP-5107           PIC X(4)    VALUE '5107'.                
011300     05  FILLER                  PIC X(26)   VALUE LOW-VALUE.             
011400                                                                          
011500   03  W-IDFTG-X.                                                         
011600     05  W-IDFTG                 PIC 9(2).                                
011700     SKIP2                                                                
011800*    --- STATUS CODES FROM IMS                                            
011900 01  STATUS-WS                   PIC XX.                                  
012000     88  SEGMENT-FOUND                       VALUE '  '.                  
012100     88  SEGMENT-FOUND-EXISTS                VALUE 'II'.                  
012200     88  SEGMENT-MISSING                     VALUE 'GE'.                  
012300     SKIP2                                                                
012400 01  GOOD-STATUSCODES.                                                    
012500     03  GOOD-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
012600     SKIP3                                                                
012700 01  SSA1                        PIC X(64).                               
012800 01  SSA2                        PIC X(64).                               
012900     EJECT                                                                
013000*    --- IMS FUNCTION CODES                                               
013100*01  -COPY W0003                                                          
013200     EJECT                                                                
013300*    ---  DLI INPUT-OUTPUT AREA                                           
013400 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX5108'.                    
013500 01  DLI-IO-AREA.                                                         
013600*    03  -COPY WDGX5108                                                   
013700                                                                          
013800     EJECT                                                                
013900 LINKAGE SECTION.                                                         
014000*01  -COPY W0009   -PRE MSG-                                              
014100*01  -COPY W0008   -PRE WDP7-                                             
014200     05  FILLER                  PIC X.                                   
014300                                                                          
014400*01  -COPY W0008   -PRE WDR2-                                             
014500     05  FILLER                  PIC X.                                   
014600     EJECT                                                                
014700 PROCEDURE DIVISION  USING MSG-PCB WDP7-PCB WDR2-PCB.                     
014800 MAIN SECTION.                                                            
014900     ENTRY 'DLITCBL' USING MSG-PCB WDP7-PCB WDR2-PCB.                     
015000                                                                          
015100     PERFORM IMS-GET-MSG                                                  
015200     IF SEGMENT-FOUND                                                     
015300       PERFORM A-INIT                                                     
015400       PERFORM B-CHECK-KEYS                                               
015500       IF KEYS-OK                                                         
015600          IF MFS-UPDATE                                                   
015700             PERFORM G-CHECK-INPUT                                        
015800             IF INDATA-OK                                                 
015900                PERFORM H-UPDATE                                          
016000             END-IF                                                       
016100          ELSE                                                            
016200             IF MFS-FIRST                                                 
016210                PERFORM C-FIRST-TIME                                      
016220             ELSE                                                         
016300                PERFORM E-SAME-PAGE                                       
016400             END-IF                                                       
016410          END-IF                                                          
016500          PERFORM F-READ-SHOW-INFO                                        
016600       END-IF                                                             
016700       COMPUTE MSG-KVLL = LENGTH OF MOD-W5O14501 + 4                      
016800       PERFORM IMS-INSERT-MSG                                             
016900     END-IF                                                               
017000                                                                          
017100     MOVE ZERO TO RETURN-CODE                                             
017200     GOBACK                                                               
017300     .                                                                    
017400     EJECT                                                                
017500 A-INIT SECTION.                                                          
017600                                                                          
017700     IF MSG-DOUBLE-TRANSACTIONS                                           
017800       MOVE MSG-INDATA-MINUS-2-TRANSACT   TO MID-W5I14501                 
017900       MOVE MSG-IDTRANS-2  TO MFS-IDTRANS                                 
018000       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
018100     ELSE                                                                 
018200       MOVE MSG-INDATA-MINUS-1-TRANSACT  TO MID-W5I14501                  
018300       MOVE MSG-IDTRANS-1  TO MFS-IDTRANS                                 
018400       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
018500     END-IF                                                               
018600                                                                          
018700     MOVE MSG-KDTRTYP TO MFS-KDTRTYP                                      
018800     MOVE MSG-IDPFK   TO MFS-IDPFK                                        
018900     MOVE MFS-IDTRANS TO W-IDTRANS                                        
019000                                                                          
019100     MOVE LOW-VALUE TO MSG-AREA                                           
019200     MOVE 'W5O145N1' TO MFS-IDMOD                                         
019300     MOVE '5145' TO MOD-IDTRANS                                           
019400     MOVE MFS-ERASE-FIELD TO MOD-TEMFSFEL MOD-TEMFSINF                    
019500                                                                          
019600     IF OWN-MID OR HELP-MID                                               
019700       CONTINUE                                                           
019800     ELSE                                                                 
019900       MOVE SPACE TO MFS-KDTRTYP                                          
020000       MOVE '7' TO MFS-IDPFK                                              
020100     END-IF                                                               
020200     ACCEPT TODAYS-DATE FROM DATE                                         
020300     .                                                                    
020400     EJECT                                                                
020500 B-CHECK-KEYS SECTION.                                                    
020600                                                                          
020700     MOVE ALL '+'           TO MSGI-WMSGINIT                              
020800     MOVE '001'             TO MSGI-KDCALL                                
020900     MOVE MSG-LTERM-NAME    TO MSGI-IDLTERM-USER                          
021000     MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                                
021100     MOVE '5145'            TO MSGI-IDTRANS                               
021200     CALL W005INIT USING MSGI-WMSGINIT WDP7-PCB                           
021400                                                                          
021500*    - LANGUAGE TO BE USED BY MEDKONV                                     
021600     MOVE MSGI-IDLAND-SPR TO MED-IDSKYLT                                  
021700                                                                          
021800     MOVE YES TO KEYS-SW                                                  
021900                                                                          
022000     PERFORM BA-CHECK-IDFTG                                               
022100                                                                          
022200     IF KEYS-WRONG                                                        
022300       MOVE ERR-WRONG-KEY TO MED-IDMFSFEL                                 
022400       CALL WMEDKONV USING MED-WMEDAREA                                   
022500       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
022600       PERFORM MFS-ERASE-FIELD-IN                                         
022700       PERFORM MFS-ERASE-FIELD-OUT                                        
022800     END-IF                                                               
022900     .                                                                    
023000     EJECT                                                                
023100 BA-CHECK-IDFTG SECTION.                                                  
023200                                                                          
023300     IF MSGI-IDFTG NUMERIC                                                
023400     AND MSGI-IDFTG > ZERO                                                
023500       MOVE MSGI-IDFTG              TO MOD-IDFTG                          
023600                                       WS-IDFTG                           
023610                                       W-IDFTG                            
023700       IF IDFTG-GODKAEND                                                  
023800          CONTINUE                                                        
023900       ELSE                                                               
024000          MOVE NOO                  TO KEYS-SW                            
024100       END-IF                                                             
024200     END-IF                                                               
024300     .                                                                    
024400     EJECT                                                                
024500 C-FIRST-TIME SECTION.                                                    
024501                                                                          
024502     PERFORM MFS-ERASE-FIELD-IN                                           
024503     .                                                                    
024504     EJECT                                                                
024510 E-SAME-PAGE SECTION.                                                     
024600                                                                          
024700     IF MID-REEMBINF-NEW-IN = ALL '+'                                     
024800     AND MID-SULSNIV-MIN-NEW-IN = ALL '+'                                 
024900     AND MID-SULSNIV-MAX-NEW-IN = ALL '+'                                 
025000       PERFORM MFS-ERASE-FIELD-IN                                         
025100     ELSE                                                                 
025200       MOVE INF-PRESS-PF11 TO MED-IDMFSFEL                                
025300       CALL WMEDKONV    USING MED-WMEDAREA                                
025400       MOVE MED-MFSFEL     TO MOD-TEMFSFEL                                
025500       PERFORM MFS-READ-IN-AGAIN                                          
025600       PERFORM EA-MID-INDATA-TILL-MOD                                     
025700     END-IF                                                               
025800     .                                                                    
025900     EJECT                                                                
026000 EA-MID-INDATA-TILL-MOD SECTION.                                          
026100                                                                          
026200     IF MID-REEMBINF-NEW-IN = ALL '+'                                     
026300        MOVE MFS-ERASE-FIELD        TO MOD-REEMBINF-NEW                   
026400     ELSE                                                                 
026500        MOVE MFS-DO-NOT-TOUCH-FIELD TO MOD-REEMBINF-NEW                   
026600     END-IF                                                               
026700                                                                          
026800     IF MID-SULSNIV-MIN-NEW-IN = ALL '+'                                  
026900        MOVE MFS-ERASE-FIELD        TO MOD-SULSNIV-MIN-NEW                
027000     ELSE                                                                 
027100        MOVE MFS-DO-NOT-TOUCH-FIELD TO MOD-SULSNIV-MIN-NEW                
027200     END-IF                                                               
027300                                                                          
027400     IF MID-SULSNIV-MAX-NEW-IN = ALL '+'                                  
027500        MOVE MFS-ERASE-FIELD        TO MOD-SULSNIV-MAX-NEW                
027600     ELSE                                                                 
027700        MOVE MFS-DO-NOT-TOUCH-FIELD TO MOD-SULSNIV-MAX-NEW                
027800     END-IF                                                               
027900     .                                                                    
028000     EJECT                                                                
028100 F-READ-SHOW-INFO SECTION.                                                
028200                                                                          
028300     PERFORM IMS-GU-WDGX5108                                              
028400                                                                          
028500     IF SEGMENT-FOUND                                                     
028600        MOVE 5108-REEMBINF       TO MOD-REEMBINF-ACTUAL                   
028601                                                                          
028610        COMPUTE WS-SULSNIV-MIN-ACTUAL =                                   
028620                        5108-SULSNIV-MIN / 1000000                        
028700        MOVE WS-SULSNIV-MIN-ACTUAL TO MOD-SULSNIV-MIN-ACTUAL              
028702                                                                          
028710        COMPUTE WS-SULSNIV-MAX-ACTUAL =                                   
028720                        5108-SULSNIV-MAX / 1000000                        
028800        MOVE WS-SULSNIV-MAX-ACTUAL TO MOD-SULSNIV-MAX-ACTUAL              
028810                                                                          
028900        MOVE 5108-IDUSER           TO MOD-IDUSER                          
029000        MOVE 5108-TIUPPDAT         TO MOD-TIUPPDAT                        
029100     ELSE                                                                 
029500        PERFORM MFS-ERASE-FIELD-OUT                                       
029600     END-IF                                                               
029700     .                                                                    
029800     EJECT                                                                
029900 G-CHECK-INPUT SECTION.                                                   
030000                                                                          
030100     IF MID-REEMBINF-NEW-IN = ALL '+'                                     
030200     AND MID-SULSNIV-MIN-NEW-IN = ALL '+'                                 
030300     AND MID-SULSNIV-MAX-NEW-IN = ALL '+'                                 
030400        MOVE ERR-PF11-AND-NO-DATA    TO MED-IDMFSFEL                      
030500        CALL WMEDKONV             USING MED-WMEDAREA                      
030600        MOVE MED-TEMFSFEL            TO MOD-TEMFSFEL                      
030700        PERFORM MFS-ERASE-FIELD-IN                                        
030800        PERFORM MFS-ERASE-FIELD-OUT                                       
030900        MOVE NOO                     TO INDATA-SW                         
031000     ELSE                                                                 
031100        PERFORM GA-CHECK-REEMBINF                                         
031200        PERFORM GB-CHECK-SULSNIV                                          
031300        IF INDATA-OK                                                      
031400           CONTINUE                                                       
031500        ELSE                                                              
031600           MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL                      
031700           CALL WMEDKONV          USING MED-WMEDAREA                      
031800           MOVE MED-TEMFSFEL         TO MOD-TEMFSFEL                      
031900           PERFORM MFS-DONT-TOUCH-FIELD-IN                                
032000           PERFORM MFS-DONT-TOUCH-FIELD-OUT                               
032100        END-IF                                                            
032200     END-IF                                                               
032300     .                                                                    
032400     EJECT                                                                
032500 GA-CHECK-REEMBINF SECTION.                                               
032600                                                                          
032700     IF MID-REEMBINF-NEW-IN = ALL '+'                                     
032800        MOVE MFS-NUM-FIELD-OK       TO MOD-REEMBINF-NEW-ATTR              
032900     ELSE                                                                 
032910        MOVE MID-REEMBINF-NEW-IN    TO WS-IDFRIDATA                       
032921        MOVE WS-IDFRIDATA           TO DEC-IDFRIDATA                      
032922        MOVE 5                      TO DEC-KVHELTAL                       
032924        MOVE 2                      TO DEC-KVDECIMAL                      
032926        CALL WDECEDIT            USING DEC-WDECAREA                       
032927                                                                          
032940        IF DEC-KDSVAR-OK                                                  
032950           MOVE DEC-IDEDITDATA      TO WS-REEMBINF-NEW                    
032951           IF WS-REEMBINF-NEW >= WS-REEMBINF-MIN                          
032952          AND WS-REEMBINF-NEW <= WS-REEMBINF-MAX                          
032953              MOVE WS-REEMBINF-NEW     TO MOD-REEMBINF-NEW                
032954              MOVE MFS-NUM-FIELD-OK    TO MOD-REEMBINF-NEW-ATTR           
032955          ELSE                                                            
032956              MOVE MFS-NUM-FIELD-WRONG TO MOD-REEMBINF-NEW-ATTR           
032957              MOVE NOO                 TO INDATA-SW                       
032958          END-IF                                                          
032960        ELSE                                                              
032991           MOVE MFS-NUM-FIELD-WRONG    TO MOD-REEMBINF-NEW-ATTR           
032993           MOVE NOO                    TO INDATA-SW                       
032994        END-IF                                                            
032997                                                                          
033800     END-IF                                                               
033900     .                                                                    
034000     EJECT                                                                
034100 GB-CHECK-SULSNIV SECTION.                                                
034200                                                                          
034300     IF MID-SULSNIV-MIN-NEW-IN = ALL '+'                                  
034301     AND MID-SULSNIV-MAX-NEW-IN NOT = ALL '+'                             
034302        MOVE NOO                       TO INDATA-SW                       
034304        MOVE MFS-NUM-FIELD-WRONG       TO MOD-SULSNIV-MIN-NEW-ATTR        
034305        MOVE MFS-NUM-FIELD-OK          TO MOD-SULSNIV-MAX-NEW-ATTR        
034306     END-IF                                                               
034307                                                                          
034308     IF MID-SULSNIV-MIN-NEW-IN NOT = ALL '+'                              
034309     AND MID-SULSNIV-MAX-NEW-IN = ALL '+'                                 
034310        MOVE NOO                       TO INDATA-SW                       
034311        MOVE MFS-NUM-FIELD-WRONG       TO MOD-SULSNIV-MAX-NEW-ATTR        
034312        MOVE MFS-NUM-FIELD-OK          TO MOD-SULSNIV-MIN-NEW-ATTR        
034313     END-IF                                                               
034314                                                                          
034315     IF INDATA-OK                                                         
034320       IF MID-SULSNIV-MIN-NEW-IN = ALL '+'                                
034400          MOVE MFS-NUM-FIELD-OK        TO MOD-SULSNIV-MIN-NEW-ATTR        
034500       ELSE                                                               
035310          MOVE MID-SULSNIV-MIN-NEW-IN  TO WS-IDFRIDATA                    
035321          MOVE WS-IDFRIDATA            TO DEC-IDFRIDATA                   
035322          MOVE 5                       TO DEC-KVHELTAL                    
035324          MOVE 0                       TO DEC-KVDECIMAL                   
035325          CALL WDECEDIT             USING DEC-WDECAREA                    
035330                                                                          
035340          IF DEC-KDSVAR-OK                                                
035350             MOVE DEC-IDEDITDATA       TO WS-SULSNIV-MIN-NEW              
035351                                          MOD-SULSNIV-MIN-NEW             
035360             MOVE MFS-NUM-FIELD-OK     TO MOD-SULSNIV-MIN-NEW-ATTR        
035380          ELSE                                                            
035390             MOVE MFS-NUM-FIELD-WRONG  TO MOD-SULSNIV-MIN-NEW-ATTR        
035392             MOVE NOO                  TO INDATA-SW                       
035393          END-IF                                                          
035400       END-IF                                                             
035500                                                                          
035510       IF MID-SULSNIV-MAX-NEW-IN = ALL '+'                                
035520          MOVE MFS-NUM-FIELD-OK        TO MOD-SULSNIV-MAX-NEW-ATTR        
035530       ELSE                                                               
035531          MOVE MID-SULSNIV-MAX-NEW-IN  TO WS-IDFRIDATA                    
035532          MOVE WS-IDFRIDATA            TO DEC-IDFRIDATA                   
035533          MOVE 5                       TO DEC-KVHELTAL                    
035534          MOVE 0                       TO DEC-KVDECIMAL                   
035535          CALL WDECEDIT             USING DEC-WDECAREA                    
035536                                                                          
035570          IF DEC-KDSVAR-OK                                                
035580             MOVE DEC-IDEDITDATA       TO WS-SULSNIV-MAX-NEW              
035581                                          MOD-SULSNIV-MAX-NEW             
035590             MOVE MFS-NUM-FIELD-OK     TO MOD-SULSNIV-MAX-NEW-ATTR        
035592          ELSE                                                            
035593             MOVE MFS-NUM-FIELD-WRONG  TO MOD-SULSNIV-MAX-NEW-ATTR        
035595             MOVE NOO                  TO INDATA-SW                       
035596          END-IF                                                          
035597       END-IF                                                             
035598                                                                          
035599       IF INDATA-OK                                                       
035600          PERFORM GBA-CHECK-SULSNIV-MIN                                   
036400       END-IF                                                             
036500       IF INDATA-OK                                                       
036600          PERFORM GBB-CHECK-SULSNIV-MAX                                   
036700       END-IF                                                             
036710     END-IF                                                               
036800     .                                                                    
036900     EJECT                                                                
036901 GBA-CHECK-SULSNIV-MIN SECTION.                                           
036902                                                                          
036910     IF MID-SULSNIV-MIN-NEW-IN NOT = ALL '+'                              
036920        IF MID-SULSNIV-MAX-NEW-IN NOT = ALL '+'                           
036930           IF WS-SULSNIV-MIN-NEW < WS-SULSNIV-MAX-NEW                     
036940               MOVE MFS-NUM-FIELD-OK   TO MOD-SULSNIV-MIN-NEW-ATTR        
036950           ELSE                                                           
036960               MOVE MFS-NUM-FIELD-WRONG                                   
036970                                       TO MOD-SULSNIV-MIN-NEW-ATTR        
036980               MOVE NOO                TO INDATA-SW                       
036981           END-IF                                                         
036992        END-IF                                                            
036993     END-IF                                                               
036994     .                                                                    
036995     EJECT                                                                
036996 GBB-CHECK-SULSNIV-MAX SECTION.                                           
036997                                                                          
036998     IF MID-SULSNIV-MAX-NEW-IN NOT = ALL '+'                              
036999        IF MID-SULSNIV-MIN-NEW-IN NOT = ALL '+'                           
037000           IF WS-SULSNIV-MAX-NEW > WS-SULSNIV-MIN-NEW                     
037010               MOVE MFS-NUM-FIELD-OK   TO MOD-SULSNIV-MAX-NEW-ATTR        
037011           ELSE                                                           
037012               MOVE MFS-NUM-FIELD-WRONG                                   
037013                                       TO MOD-SULSNIV-MAX-NEW-ATTR        
037014               MOVE NOO                TO INDATA-SW                       
037015           END-IF                                                         
037026        END-IF                                                            
037027     END-IF                                                               
037028     .                                                                    
037029     EJECT                                                                
037030 H-UPDATE SECTION.                                                        
037100                                                                          
037200     PERFORM IMS-GHU-WDGX5108                                             
037300     IF SEGMENT-FOUND                                                     
037400        PERFORM HB-MOVE-FIELDS                                            
037500        PERFORM IMS-REPL-WDGX5108                                         
037600     ELSE                                                                 
037700        PERFORM HA-INIT-FIELDS                                            
037710        PERFORM HB-MOVE-FIELDS                                            
037800        PERFORM IMS-ISRT-WDGX5108                                         
037900     END-IF                                                               
038000     MOVE INF-UPDATE-DONE TO MED-IDMFSINF                                 
038100     CALL WMEDKONV USING MED-WMEDAREA                                     
038200     MOVE MED-MFSINF TO MOD-TEMFSINF                                      
038300     PERFORM MFS-FORM-ATTR                                                
038400     PERFORM MFS-ERASE-FIELD-IN                                           
038500     .                                                                    
038600     EJECT                                                                
038700 HA-INIT-FIELDS SECTION.                                                  
038701     INITIALIZE    5108-REEMBINF                                          
038702                   5108-SULSNIV-MIN                                       
038703                   5108-SULSNIV-MAX                                       
038704     MOVE MSGI-IDFTG             TO 5108-IDFTG                            
038705     MOVE TODAYS-DATE            TO 5108-TIUPPDAT                         
038706     MOVE MSGI-IDUSER            TO 5108-IDUSER                           
038707     .                                                                    
038708     EJECT                                                                
038710 HB-MOVE-FIELDS SECTION.                                                  
038800                                                                          
038900     IF MID-REEMBINF-NEW-IN = ALL '+'                                     
039000        CONTINUE                                                          
039100     ELSE                                                                 
039200        MOVE WS-REEMBINF-NEW     TO 5108-REEMBINF                         
039300     END-IF                                                               
039400                                                                          
039500     IF MID-SULSNIV-MIN-NEW-IN = ALL '+'                                  
039600        CONTINUE                                                          
039700     ELSE                                                                 
039710        COMPUTE WS-SULSNIV-MIN-NEW = WS-SULSNIV-MIN-NEW                   
039720                                     * 1000000                            
039800        MOVE WS-SULSNIV-MIN-NEW  TO 5108-SULSNIV-MIN                      
039900     END-IF                                                               
040000     IF MID-SULSNIV-MAX-NEW-IN = ALL '+'                                  
040100        CONTINUE                                                          
040200     ELSE                                                                 
040210        COMPUTE WS-SULSNIV-MAX-NEW = WS-SULSNIV-MAX-NEW                   
040220                                     * 1000000                            
040300        MOVE WS-SULSNIV-MAX-NEW  TO 5108-SULSNIV-MAX                      
040400     END-IF                                                               
040500                                                                          
040600     MOVE MSGI-IDFTG             TO 5108-IDFTG                            
040610     MOVE TODAYS-DATE            TO 5108-TIUPPDAT                         
040700     MOVE MSGI-IDUSER            TO 5108-IDUSER                           
040800     .                                                                    
040900     EJECT                                                                
041000 MFS-ERASE-FIELD-OUT SECTION.                                             
041100                                                                          
041200*    --- ALLA UTDATA-FÄLT                                                 
041300     MOVE MFS-ERASE-FIELD TO MOD-REEMBINF-ACTUAL                          
041400                             MOD-SULSNIV-MIN-ACTUAL                       
041500                             MOD-SULSNIV-MAX-ACTUAL                       
041600                             MOD-IDUSER                                   
041700                             MOD-TIUPPDAT                                 
041800     .                                                                    
041900     SKIP3                                                                
042000 MFS-ERASE-FIELD-IN SECTION.                                              
042100                                                                          
042200*    --- ALLA INDATA-FÄLT                                                 
042300     MOVE MFS-ERASE-FIELD TO MOD-REEMBINF-NEW                             
042400                             MOD-SULSNIV-MIN-NEW                          
042500                             MOD-SULSNIV-MAX-NEW                          
042600     .                                                                    
042700     EJECT                                                                
042800 MFS-DONT-TOUCH-FIELD-OUT  SECTION.                                       
042900                                                                          
043000*    --- ALLA UTDATA-FÄLT                                                 
043100     MOVE MFS-DO-NOT-TOUCH-FIELD TO MOD-REEMBINF-ACTUAL                   
043200                                    MOD-SULSNIV-MIN-ACTUAL                
043300                                    MOD-SULSNIV-MAX-ACTUAL                
043400                                    MOD-IDUSER                            
043500                                    MOD-TIUPPDAT                          
043600     .                                                                    
043700     SKIP3                                                                
043800 MFS-DONT-TOUCH-FIELD-IN  SECTION.                                        
043900                                                                          
044000*    --- ALLA INDATA-FÄLT                                                 
044100     MOVE MFS-DO-NOT-TOUCH-FIELD TO MOD-REEMBINF-NEW                      
044200                                    MOD-SULSNIV-MIN-NEW                   
044300                                    MOD-SULSNIV-MAX-NEW                   
044400     .                                                                    
044500     EJECT                                                                
044600 MFS-FORM-ATTR SECTION.                                                   
044700                                                                          
044800*    --- ALL INDATA-FIELDS                                                
044900     MOVE MFS-FORMAT-DEFAULT-ATTR TO MOD-REEMBINF-NEW-ATTR                
045000                                     MOD-SULSNIV-MIN-NEW-ATTR             
045100                                     MOD-SULSNIV-MAX-NEW-ATTR             
045200     .                                                                    
045300     SKIP2                                                                
045400 MFS-READ-IN-AGAIN SECTION.                                               
045500                                                                          
045600*    --- ALL INDATA-FIELDS                                                
045700     MOVE MFS-ADD-READ-FIELD TO MOD-REEMBINF-NEW-ATTR                     
045800                                MOD-SULSNIV-MIN-NEW-ATTR                  
045900                                MOD-SULSNIV-MAX-NEW-ATTR                  
046000     .                                                                    
046100     EJECT                                                                
046200* --- IMS SECTIONS ---                                                    
046300     SKIP3                                                                
046400 IMS-GET-MSG SECTION.                                                     
046500                                                                          
046600     MOVE '  QC' TO GOOD-STATUSCODES                                      
046700     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
046800     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
046900     PERFORM IMS-STATUSCHECK                                              
047000     .                                                                    
047100     SKIP3                                                                
047200 IMS-INSERT-MSG SECTION.                                                  
047300                                                                          
047700     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
047800     MOVE SPACE TO GOOD-STATUSCODES                                       
047900     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
048000     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
048100     PERFORM IMS-STATUSCHECK                                              
048200     .                                                                    
048300     EJECT                                                                
048400 IMS-GU-WDGX5108 SECTION.                                                 
048500                                                                          
048600     STRING 'WDR201  (WDGXKEY  =' W-WDGXKEY-X ')'                         
048700          DELIMITED BY SIZE INTO SSA1                                     
048800     STRING 'WDGX5108(IDFTG    =' W-IDFTG-X ')'                           
048900          DELIMITED BY SIZE INTO SSA2                                     
049000     MOVE '  GE'   TO GOOD-STATUSCODES                                    
049100     CALL CBLTDLI USING GU WDR2-PCB DLI-IO-AREA SSA1 SSA2                 
049200     MOVE WDR2-STATUS-CODE TO STATUS-WS                                   
049300     PERFORM IMS-STATUSCHECK                                              
049400     .                                                                    
049500     EJECT                                                                
049600 IMS-GHU-WDGX5108 SECTION.                                                
049700                                                                          
049800     STRING 'WDR201  (WDGXKEY  =' W-WDGXKEY-X ')'                         
049900          DELIMITED BY SIZE INTO SSA1                                     
050000     STRING 'WDGX5108(IDFTG    =' W-IDFTG-X ')'                           
050100          DELIMITED BY SIZE INTO SSA2                                     
050200     MOVE '  GE'   TO GOOD-STATUSCODES                                    
050300     CALL CBLTDLI USING GHU WDR2-PCB DLI-IO-AREA SSA1 SSA2                
050400     MOVE WDR2-STATUS-CODE TO STATUS-WS                                   
050500     PERFORM IMS-STATUSCHECK                                              
050600     .                                                                    
050700     EJECT                                                                
050800 IMS-REPL-WDGX5108  SECTION.                                              
050900     MOVE '  ' TO GOOD-STATUSCODES                                        
051000     CALL CBLTDLI USING REPL WDR2-PCB DLI-IO-AREA                         
051100     MOVE WDR2-STATUS-CODE TO STATUS-WS                                   
051200     PERFORM IMS-STATUSCHECK                                              
051300     .                                                                    
051400     SKIP1                                                                
051500 IMS-ISRT-WDGX5108 SECTION.                                               
051600                                                                          
051700     STRING 'WDR201  (WDGXKEY  =' W-WDGXKEY-X ')'                         
051800          DELIMITED BY SIZE INTO SSA1                                     
051900     MOVE 'WDGX5108'          TO SSA2                                     
052000     MOVE '  '                TO GOOD-STATUSCODES                         
052100     CALL CBLTDLI          USING ISRT WDR2-PCB                            
052200                                 DLI-IO-AREA SSA1 SSA2                    
052300     MOVE WDR2-STATUS-CODE    TO STATUS-WS                                
052400     PERFORM IMS-STATUSCHECK                                              
052500     .                                                                    
052600     EJECT                                                                
052700 IMS-STATUSCHECK SECTION.                                                 
052800                                                                          
052900     SET STATUS-IX TO 1                                                   
053000     SEARCH GOOD-STATUS                                                   
053100       AT END                                                             
053200         STRING ' INVALID STATUS CODE FROM IMS: ' STATUS-WS               
053300         DELIMITED BY SIZE INTO ERROR-TEXT                                
053400         CALL FELLOG                                                      
053500       WHEN GOOD-STATUS (STATUS-IX) = STATUS-WS                           
053600         CONTINUE                                                         
053700     END-SEARCH                                                           
053800     .                                                                    
