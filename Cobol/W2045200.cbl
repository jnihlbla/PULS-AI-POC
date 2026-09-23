000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W2045200.                                                
000300 AUTHOR.         REDDY RAHUL.                                             
000400 DATE-WRITTEN.   12/08/16.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNCTION:                                                            
000800*        ASSIGNMENT OF PROCURER ID.                                       
000900*                                                                         
001000*        THE PROGRAM UPDATES   WDG2                                       
001100*                                                                         
001200*    INDATA.                                                              
001300*        TRANSACTION: W2T452 W2T452U                                      
001400*        MID:         W2I45201                                            
001500*                                                                         
001600*    OUTDATA.                                                             
001700*        MOD:         W2O45201                                            
001800                                                                          
001900     SKIP3                                                                
002000 ENVIRONMENT DIVISION.                                                    
002100                                                                          
002200 DATA DIVISION.                                                           
002300     EJECT                                                                
002400 WORKING-STORAGE SECTION.                                                 
002500 77  IDPGM                       PIC X(08)   VALUE 'W2045200'.            
002600                                                                          
002700*    --- WORK FIELDS FOR ERROR MESSAGES WHEN CALLING ABEND/FELLOG         
002800 77  ERROR-TEXT                  PIC X(80)   VALUE SPACE.                 
002900                                                                          
003000 77  YES                         PIC X       VALUE 'Y'.                   
003100 77  NOO                         PIC X       VALUE 'N'.                   
003200                                                                          
003300*    --- INDEX FOR SCROLL LINES                                           
003400 77  ROW-INDX                    PIC S9(4)   VALUE +0   COMP SYNC.        
003500 77  COL-INDX                    PIC S9(4)   VALUE +0   COMP SYNC.        
003600 77  MAX-ROW-INDX                PIC S9(4)   VALUE +10  COMP SYNC.        
003700 77  MAX-COL-INDX                PIC S9(4)   VALUE +3   COMP SYNC.        
003800                                                                          
003900 77  WS-KDCMD                    PIC X(2)    VALUE SPACE.                 
004000     88  WS-GOOD-KDCMD                       VALUE 'N' 'C' 'D'.           
004100     88  WS-KDCMD-INSERT                     VALUE 'N'.                   
004200     88  WS-KDCMD-CHANGE                     VALUE 'C'.                   
004300     88  WS-KDCMD-DELETE                     VALUE 'D'.                   
004400                                                                          
003900 77  WS-MARKET                   PIC X(2)    VALUE SPACE.                 
004000     88  WS-GOOD-MARKET                      VALUE 'CN'.                  
004400                                                                          
004500 01  WS-IDFKNGRP-FOM-IN          PIC  X(4)   VALUE ZERO.                  
004600 01  WS-IDFKNGRP-FOM-IN-NUM REDEFINES WS-IDFKNGRP-FOM-IN                  
004700                                 PIC  9(4).                               
004800 01  WS-IDFKNGRP-TOM-IN          PIC  X(4)   VALUE ZERO.                  
004900 01  WS-IDFKNGRP-TOM-IN-NUM REDEFINES WS-IDFKNGRP-TOM-IN                  
005000                                 PIC  9(4).                               
005100 01  WS-IDANSK-IN                PIC  X(3)   VALUE ZERO.                  
005200*    --- WORK FIELDS FOR ACTUAL KEYVALUES OF SCREEN                       
005300                                                                          
005400 77  INDATA-SW                   PIC X       VALUE 'Y'.                   
005500     88  INDATA-OK                           VALUE 'Y'.                   
005600     88  INDATA-WRONG                        VALUE 'N'.                   
005700                                                                          
005800 77  KEYS-SW                     PIC X       VALUE 'Y'.                   
005900     88  KEYS-OK                             VALUE 'Y'.                   
006000     88  KEYS-WRONG                          VALUE 'N'.                   
006100                                                                          
006200 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
006300     88  OWN-MID                             VALUE '2452'.                
006400     88  GOOD-MID                            VALUE '2452'.                
006900     88  HELP-MID                            VALUE '0551'.                
007000     EJECT                                                                
007100*    --- SUBPROGRAMS AND PARAMETER AREAS                                  
007200 01  GENERAL-SUBPROGRAMS.                                                 
007300     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
007400     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
007500     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
007600     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
007700     EJECT                                                                
007800*    --- PARAMETERS FOR SUBPROGRAM WMEDKONV                               
007900*01 -COPY WMEDAREA                                                        
008000     SKIP3                                                                
008100 01  MESSAGE-CODES.                                                       
008200     03  ERR-CORR-HILITE-FLDS    PIC X(3)    VALUE '001'.                 
008300     03  ERR-PRESS-PF11          PIC X(3)    VALUE '003'.                 
008400     03  ERR-PF11-AND-NO-DATA    PIC X(3)    VALUE '011'.                 
008500     03  INF-UPDATE-DONE         PIC X(3)    VALUE '101'.                 
008600     03  INF-FIRST-PAGE          PIC X(3)    VALUE '006'.                 
008700     03  INF-MORE-INFO-EXISTS    PIC X(3)    VALUE '105'.                 
008800     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
008900     03  ERR-NOT-AUTHORIZED      PIC X(3)    VALUE '405'.                 
009000     03  ERR-KEYS-MISSING        PIC X(3)    VALUE '005'.                 
009100     03  ERR-LINE-UPDATED        PIC X(3)    VALUE '779'.                 
009200     03  ERR-INT-PART-INT-REPORTED PIC X(3)  VALUE '723'.                 
009300     03  ERR-WRONG-INTERVAL-INFO PIC X(3)    VALUE '738'.                 
009400     03  ERR-FIELDS-NOT-NUMERIC  PIC X(3)    VALUE '020'.                 
009500     03  ERR-HILITE-FIELDS-WRONG PIC X(3)    VALUE '748'.                 
009600     03  ERR-FUNC-DO-NOT-EXIST   PIC X(3)    VALUE '110'.                 
009700     03  ERR-CHNG-USING-FUNC-D-N PIC X(3)    VALUE '432'.                 
009800     03  ERR-BOTH-FOM-TOM-NEEDED PIC X(3)    VALUE '739'.                 
009900     03  ERR-WRONG-INTERVAL      PIC X(3)    VALUE '738'.                 
010000     EJECT                                                                
010100*    --- PARAMETERS FOR SUB PROGRAM W005INIT                              
010200*                                                                         
010300 01  FILLER                      PIC X(16)   VALUE 'WMSGINIT'.            
010400     SKIP3                                                                
010500*01 -COPY WMSGINIT                                                        
010600     EJECT                                                                
010700*    --- AREA CONTAINING DATA TO BE SAVED BETWEEN DIALOG STEPS            
010800*                                                                         
010900 01  SAVE-AREA.                                                           
011000     03  SAVE-IDTRANS            PIC X(4)    VALUE '2452'.                
011100     03  SAVE-IDFKNGRP-FOM-ENTER PIC 9(4)               COMP-3.           
011200     03  SAVE-IDFKNGRP-FOM-NEXT  PIC 9(4)               COMP-3.           
011300     03  SAVE-IDFKNGRP-TOM-ENTER PIC 9(4)               COMP-3.           
011400     03  SAVE-IDFKNGRP-TOM-NEXT  PIC 9(4)               COMP-3.           
011500     EJECT                                                                
011600*    --- AREAS FOR MFS AND SCREEN MANAGEMENT                              
011700*                                                                         
011800 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
011900     SKIP3                                                                
012000*01  MID -COPY W2I45201                                                   
012100     EJECT                                                                
012200 01  FILLER                      PIC X(16)   VALUE 'MSG/MOD-AREA'.        
012300     SKIP3                                                                
012400*01  -COPY WMSGAREA                                                       
012500     EJECT                                                                
012600     03  MOD REDEFINES MSG-AREA.                                          
012700*      05  -COPY W2O45201                                                 
012800     EJECT                                                                
012900 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
013000     SKIP3                                                                
013100*01  -COPY WMFSAREA                                                       
013200     EJECT                                                                
013300*    --- WORK-AREAS FOR IMS-SECTIONS                                      
013400*                                                                         
013500 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
013600     SKIP3                                                                
013700 01  KEYS-FOR-DLI.                                                        
013800     03  W-1139-KEY-X.                                                    
013900       05  W-IDHTYP              PIC X(4)    VALUE '1139'.                
014000       05  W-KDPRODSL            PIC S9(3)   VALUE ZERO  COMP-3.          
014100       05  W-LOWVALUE            PIC X(24)   VALUE LOW-VALUE.             
014200     03  W-1140-KEY-X.                                                    
014300       05  W-IDLANDX2            PIC X(2)    VALUE SPACE.                 
014400       05  W-IDFKNGRP-FOM        PIC S9(5)   VALUE ZERO  COMP-3.          
014500       05  W-IDFKNGRP-TOM        PIC S9(5)   VALUE ZERO  COMP-3.          
014600     SKIP2                                                                
014700*    --- STATUS CODES FROM IMS                                            
014800 01  STATUS-WS                   PIC XX.                                  
014900     88  SEGMENT-FOUND                       VALUE '  '.                  
015000     88  SEGMENT-FOUND-EXISTS                VALUE 'II'.                  
015100     88  SEGMENT-MISSING                     VALUE 'GE'.                  
015200     SKIP2                                                                
015300 01  GOOD-STATUSCODES.                                                    
015400     03  GOOD-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
015500     SKIP3                                                                
015600 01  SSA1                        PIC X(64).                               
015700 01  SSA2                        PIC X(64).                               
015800     EJECT                                                                
015900*    --- IMS FUNCTION CODES                                               
016000*01  -COPY W0003                                                          
016100     EJECT                                                                
016200*    ---  DLI INPUT-OUTPUT AREA                                           
016300                                                                          
016400 01  FILLER                     PIC X(16) VALUE 'DLI-IO-WDGX01'.          
016500 01  DLI-IO-WDGX01.                                                       
016600*    03  -COPY WDGX1139                                                   
016700     EJECT                                                                
016800 01  FILLER                     PIC X(16) VALUE 'DLI-IO-WDGX1140'.        
016900 01  DLI-IO-WDGX1140.                                                     
017000*    03  -COPY WDGX1140                                                   
017100     EJECT                                                                
017200 LINKAGE SECTION.                                                         
017300*01  -COPY W0009   -PRE MSG-                                              
017400*01  -COPY W0008  -PRE WDP7-                                              
017500     05  FILLER                  PIC X.                                   
017600                                                                          
017700*01  -COPY W0008  -PRE WDG2-                                              
017800     05  FILLER                  PIC X.                                   
017900     EJECT                                                                
018000 PROCEDURE DIVISION  USING MSG-PCB WDP7-PCB WDG2-PCB.                     
018100 MAIN SECTION.                                                            
018200     ENTRY 'DLITCBL' USING MSG-PCB WDP7-PCB WDG2-PCB.                     
018300                                                                          
018500     PERFORM IMS-GET-MSG                                                  
018600     IF SEGMENT-FOUND                                                     
018700       PERFORM A-INIT                                                     
018800       PERFORM B-CHECK-KEYS                                               
018900       IF KEYS-OK                                                         
019000         IF MFS-UPDATE                                                    
019100           PERFORM G-CHECK-INPUT                                          
019200           IF INDATA-OK                                                   
019300             PERFORM H-UPDATE                                             
019400           END-IF                                                         
019500         ELSE                                                             
019600           IF MFS-FIRST                                                   
019700             PERFORM C-FIRST-PAGE                                         
019800           ELSE                                                           
019900             IF MFS-NEXT                                                  
020000               PERFORM D-NEXT-PAGE                                        
020100             ELSE                                                         
020200               PERFORM E-SAME-PAGE                                        
020300             END-IF                                                       
020400           END-IF                                                         
020500         END-IF                                                           
020510         IF INDATA-OK                                                     
020600           PERFORM F-READ-SHOW-INFO                                       
020610         END-IF                                                           
020700       END-IF                                                             
020800       COMPUTE MSG-KVLL = LENGTH OF MOD-W2O45201-CTX + 4                  
020900       PERFORM IMS-INSERT-MSG                                             
021000     END-IF                                                               
021100                                                                          
021200     MOVE ZERO                   TO RETURN-CODE                           
021300     GOBACK                                                               
021400     .                                                                    
021500     EJECT                                                                
021600 A-INIT SECTION.                                                          
021700                                                                          
021900     IF MSG-DOUBLE-TRANSACTIONS                                           
022000       MOVE MSG-INDATA-MINUS-2-TRANSACT                                   
022100                                 TO MID-W2I45201-CTX                      
022200       MOVE MSG-IDTRANS-2        TO MFS-IDTRANS                           
022300       MOVE MSG-KDMFSFOR-2       TO MFS-KDMFSFOR                          
022400     ELSE                                                                 
022500       MOVE MSG-INDATA-MINUS-1-TRANSACT                                   
022600                                 TO MID-W2I45201-CTX                      
022700       MOVE MSG-IDTRANS-1        TO MFS-IDTRANS                           
022800       MOVE MSG-KDMFSFOR-1       TO MFS-KDMFSFOR                          
022900     END-IF                                                               
023000                                                                          
023100     MOVE MSG-KDTRTYP            TO MFS-KDTRTYP                           
023200     MOVE MSG-IDPFK              TO MFS-IDPFK                             
023300     MOVE MFS-IDTRANS            TO W-IDTRANS                             
023400                                                                          
023500     MOVE LOW-VALUE              TO MSG-AREA                              
023600     MOVE 'W2O452N1'             TO MFS-IDMOD                             
023700     MOVE '2452'                 TO MOD-IDTRANS                           
023800     MOVE MFS-ERASE-FIELD        TO MOD-TEMFSFEL                          
023900                                    MOD-TEMFSINF                          
           MOVE SPACE                  TO MED-IDMFSFEL                          
                                          MED-IDMFSINF                          
024000                                                                          
074400     PERFORM MFS-FORM-ATTR                                                
024100     IF OWN-MID OR HELP-MID                                               
024200       CONTINUE                                                           
024300     ELSE                                                                 
024400       MOVE SPACE                TO MFS-KDTRTYP                           
024500       MOVE '7'                  TO MFS-IDPFK                             
024600     END-IF                                                               
024700     .                                                                    
024800     EJECT                                                                
024900 B-CHECK-KEYS SECTION.                                                    
025000                                                                          
025200     MOVE ALL '+'                TO MSGI-WMSGINIT                         
025300     MOVE '001'                  TO MSGI-KDCALL                           
025400     MOVE MSG-LTERM-NAME         TO MSGI-IDLTERM-USER                     
025500     MOVE MSG-SIGNON-USERID      TO MSGI-IDUSER                           
025600     MOVE '2452'                 TO MSGI-IDTRANS                          
025700     IF GOOD-MID                                                          
025800       MOVE MID-KDPRODSL-IN      TO MSGI-KDPRODSL                         
025900       MOVE MID-IDLANDX2-IN      TO MSGI-IDLANDX2                         
026000     END-IF                                                               
026100     CALL W005INIT            USING MSGI-WMSGINIT                         
026200                                    WDP7-PCB                              
026300     MOVE MSGI-SPAR-AREA         TO SAVE-AREA                             
026400                                                                          
026500*    - LANGUAGE TO BE USED BY MEDKONV                                     
026600     MOVE MSGI-IDLAND-SPR        TO MED-IDSKYLT                           
026700                                                                          
026800     MOVE YES                    TO KEYS-SW                               
026900                                                                          
027000                                                                          
027100*    -- CHECK OF KDPRODSL                                                 
027200     MOVE MFS-ERASE-FIELD        TO MOD-KDPRODSL-IN                       
027300                                                                          
027400     IF MID-KDPRODSL-IN NOT = ALL '+'                                     
027500       MOVE '7'                  TO MFS-IDPFK                             
027600       MOVE SPACE                TO MFS-KDTRTYP                           
027700     END-IF                                                               
           IF MSGI-KDPRODSL = ALL '+' OR SPACE                                  
             MOVE NOO                  TO KEYS-SW                               
           ELSE                                                                 
027800       MOVE MSGI-KDPRODSL        TO W-KDPRODSL                            
           END-IF                                                               
027900                                                                          
028000*    -- CHECK OF IDLANDX2                                                 
028100     MOVE MFS-ERASE-FIELD        TO MOD-IDLANDX2-IN                       
028200                                                                          
028300     IF MID-IDLANDX2-IN NOT = ALL '+'                                     
028400       MOVE '7'                  TO MFS-IDPFK                             
028500       MOVE SPACE                TO MFS-KDTRTYP                           
028600     END-IF                                                               
           MOVE MSGI-IDLANDX2          TO WS-MARKET                             
           IF WS-GOOD-MARKET                                                    
028700       MOVE MSGI-IDLANDX2        TO W-IDLANDX2                            
           ELSE                                                                 
             MOVE NOO                  TO KEYS-SW                               
           END-IF                                                               
028800                                                                          
028900     IF GOOD-MID OR KEYS-OK                                               
029000       MOVE MSGI-KDPRODSL        TO MOD-KDPRODSL-UT                       
029100       MOVE MSGI-IDLANDX2        TO MOD-IDLANDX2-UT                       
029200     ELSE                                                                 
029300       MOVE MFS-ERASE-FIELD      TO MOD-KDPRODSL-UT                       
029400                                    MOD-IDLANDX2-UT                       
029500     END-IF                                                               
029600                                                                          
029700     IF KEYS-WRONG                                                        
029800       MOVE ERR-WRONG-KEY        TO MED-IDMFSFEL                          
029900       CALL WMEDKONV          USING MED-WMEDAREA                          
030000       MOVE MED-TEMFSFEL         TO MOD-TEMFSFEL                          
030100       PERFORM MFS-ERASE-FIELD-IN                                         
030200       PERFORM MFS-ERASE-FIELD-OUT                                        
030300     END-IF                                                               
030400     .                                                                    
030500     EJECT                                                                
030600 C-FIRST-PAGE SECTION.                                                    
030700                                                                          
           PERFORM MFS-ERASE-FIELD-IN                                           
030900     MOVE INF-FIRST-PAGE         TO MED-IDMFSINF                          
031000     CALL WMEDKONV            USING MED-WMEDAREA                          
031100     MOVE MED-TEMFSINF           TO MOD-TEMFSINF                          
031200                                                                          
031400     .                                                                    
031500     EJECT                                                                
031600 D-NEXT-PAGE SECTION.                                                     
031700                                                                          
           PERFORM MFS-ERASE-FIELD-IN                                           
031900     IF SAVE-IDTRANS = '2452'                                             
032000       MOVE SAVE-IDFKNGRP-FOM-NEXT                                        
032100                                 TO W-IDFKNGRP-FOM                        
032200       MOVE SAVE-IDFKNGRP-TOM-NEXT                                        
032300                                 TO W-IDFKNGRP-TOM                        
032600     END-IF                                                               
032700     .                                                                    
032800     EJECT                                                                
032900 E-SAME-PAGE SECTION.                                                     
033000                                                                          
033200     IF SAVE-IDTRANS = '2452' OR '0551'                                   
033300       MOVE SAVE-IDFKNGRP-FOM-ENTER                                       
033400                                 TO W-IDFKNGRP-FOM                        
033500       MOVE SAVE-IDFKNGRP-TOM-ENTER                                       
033600                                 TO W-IDFKNGRP-TOM                        
033700       IF MID-W2I45201-001-GRP = ALL '+'                                  
033800         PERFORM MFS-ERASE-FIELD-IN                                       
033900       ELSE                                                               
034000         MOVE ERR-PRESS-PF11     TO MED-IDMFSFEL                          
034100         CALL WMEDKONV        USING MED-WMEDAREA                          
034200         MOVE MED-TEMFSFEL       TO MOD-TEMFSFEL                          
034300         PERFORM EA-MID-INDATA-FOR-MOD                                    
034400       END-IF                                                             
034500     ELSE                                                                 
034600       PERFORM MFS-ERASE-FIELD-IN                                         
034700     END-IF                                                               
034800     .                                                                    
034900     EJECT                                                                
035000 EA-MID-INDATA-FOR-MOD SECTION.                                           
035100                                                                          
035300     IF MID-IDFKNGRP-FOM-IN NOT = ALL '+'                                 
035400       MOVE MID-IDFKNGRP-FOM-IN  TO MOD-IDFKNGRP-FOM-IN                   
035500       MOVE MFS-ADD-READ-FIELD   TO MOD-IDFKNGRP-FOM-IN-ATTR              
035600     ELSE                                                                 
035700       MOVE MFS-ERASE-FIELD      TO MOD-IDFKNGRP-FOM-IN                   
035800     END-IF                                                               
035900                                                                          
036000     IF MID-IDFKNGRP-TOM-IN NOT = ALL '+'                                 
036100       MOVE MID-IDFKNGRP-TOM-IN  TO MOD-IDFKNGRP-TOM-IN                   
036200       MOVE MFS-ADD-READ-FIELD   TO MOD-IDFKNGRP-TOM-IN-ATTR              
036300     ELSE                                                                 
036400       MOVE MFS-ERASE-FIELD      TO MOD-IDFKNGRP-TOM-IN                   
036500     END-IF                                                               
036600                                                                          
036700     IF MID-IDANSK-IN NOT = ALL '+'                                       
036800       MOVE MID-IDANSK-IN        TO MOD-IDANSK-IN                         
036900       MOVE MFS-ADD-READ-FIELD   TO MOD-IDANSK-IN-ATTR                    
037000     ELSE                                                                 
037100       MOVE MFS-ERASE-FIELD      TO MOD-IDANSK-IN                         
037200     END-IF                                                               
037300                                                                          
037400     IF MID-KDCMD-IN NOT = ALL '+'                                        
037500       MOVE MID-KDCMD-IN         TO MOD-KDCMD-IN                          
037600       MOVE MFS-ADD-READ-FIELD   TO MOD-KDCMD-IN-ATTR                     
037700     ELSE                                                                 
037800       MOVE MFS-ERASE-FIELD      TO MOD-KDCMD-IN                          
037900     END-IF                                                               
038000     .                                                                    
038100     EJECT                                                                
038200 F-READ-SHOW-INFO SECTION.                                                
038300                                                                          
038500     PERFORM IMS-GU-WDGX01                                                
038600                                                                          
038700     IF SEGMENT-MISSING                                                   
             IF MED-IDMFSFEL = SPACE                                            
038800         MOVE ERR-KEYS-MISSING   TO MED-IDMFSFEL                          
038900         CALL WMEDKONV        USING MED-WMEDAREA                          
039000         MOVE MED-TEMFSFEL       TO MOD-TEMFSFEL                          
               MOVE MFS-ERASE-FIELD    TO MOD-TEMFSINF                          
             END-IF                                                             
039100       PERFORM MFS-ERASE-FIELD-OUT                                        
039200     ELSE                                                                 
039300       PERFORM IMS-GNP-KEY-WDGX1140                                       
039400       IF SEGMENT-FOUND                                                   
               IF 1140-IDLANDX2 = W-IDLANDX2                                    
039500           MOVE 1140-IDFKNGRP-FOM                                         
039500                                 TO SAVE-IDFKNGRP-FOM-ENTER               
039600           MOVE 1140-IDFKNGRP-TOM                                         
039600                                 TO SAVE-IDFKNGRP-TOM-ENTER               
               ELSE                                                             
                 SET SEGMENT-MISSING   TO TRUE                                  
                 IF MED-IDMFSFEL = SPACE                                        
                   MOVE ERR-KEYS-MISSING                                        
                                       TO MED-IDMFSFEL                          
                   CALL WMEDKONV    USING MED-WMEDAREA                          
                   MOVE MED-TEMFSFEL   TO MOD-TEMFSFEL                          
                   MOVE MFS-ERASE-FIELD                                         
                                       TO MOD-TEMFSINF                          
                 END-IF                                                         
039100           PERFORM MFS-ERASE-FIELD-OUT                                    
               END-IF                                                           
039700       ELSE                                                               
               SET SEGMENT-MISSING     TO TRUE                                  
               IF MED-IDMFSFEL = SPACE                                          
                 MOVE ERR-KEYS-MISSING TO MED-IDMFSFEL                          
                 CALL WMEDKONV      USING MED-WMEDAREA                          
                 MOVE MED-TEMFSFEL     TO MOD-TEMFSFEL                          
                 MOVE MFS-ERASE-FIELD  TO MOD-TEMFSINF                          
               END-IF                                                           
039100         PERFORM MFS-ERASE-FIELD-OUT                                      
040000       END-IF                                                             
040100                                                                          
040200       PERFORM                                                            
040300       VARYING COL-INDX FROM +1 BY +1                                     
040400         UNTIL COL-INDX > MAX-COL-INDX                                    
040500         PERFORM                                                          
040600         VARYING ROW-INDX FROM +1 BY +1                                   
040700           UNTIL ROW-INDX > MAX-ROW-INDX                                  
040800           IF SEGMENT-FOUND                                               
040900             MOVE 1140-IDFKNGRP-FOM                                       
041000                         TO MOD-IDFKNGRP-FOM (ROW-INDX , COL-INDX)        
041100             MOVE 1140-IDFKNGRP-TOM                                       
041200                         TO MOD-IDFKNGRP-TOM (ROW-INDX , COL-INDX)        
041300             MOVE 1140-IDANSK                                             
041400                         TO MOD-IDANSK       (ROW-INDX , COL-INDX)        
041500             PERFORM IMS-GNP-WDGX1140                                     
                   IF SEGMENT-FOUND AND                                         
                      1140-IDLANDX2 NOT = W-IDLANDX2                            
                     SET SEGMENT-MISSING                                        
                                       TO TRUE                                  
                   END-IF                                                       
041600           ELSE                                                           
041700             MOVE MFS-ERASE-FIELD                                         
041800                         TO MOD-IDFKNGRP-FOM (ROW-INDX , COL-INDX)        
041900                            MOD-IDFKNGRP-TOM (ROW-INDX , COL-INDX)        
042000                            MOD-IDANSK       (ROW-INDX , COL-INDX)        
042100           END-IF                                                         
042200         END-PERFORM                                                      
042300       END-PERFORM                                                        
042400                                                                          
042500       IF SEGMENT-FOUND                                                   
               IF MED-IDMFSFEL = SPACE                                          
042600           MOVE INF-MORE-INFO-EXISTS                                      
042700                                 TO MED-IDMFSFEL                          
042800           CALL WMEDKONV      USING MED-WMEDAREA                          
042900           MOVE MED-TEMFSFEL     TO MOD-TEMFSFEL                          
               END-IF                                                           
042910         MOVE 1140-IDFKNGRP-FOM  TO SAVE-IDFKNGRP-FOM-NEXT                
042920         MOVE 1140-IDFKNGRP-TOM  TO SAVE-IDFKNGRP-TOM-NEXT                
042930       ELSE                                                               
042940         MOVE ZERO               TO SAVE-IDFKNGRP-FOM-NEXT                
042950                                    SAVE-IDFKNGRP-TOM-NEXT                
043000       END-IF                                                             
043100                                                                          
043400       MOVE '002'                TO MSGI-KDCALL                           
043500       MOVE '2452'               TO SAVE-IDTRANS                          
043600       MOVE SAVE-AREA            TO MSGI-SPAR-AREA                        
043700       CALL W005INIT          USING MSGI-WMSGINIT                         
043800                                    WDP7-PCB                              
043900     END-IF                                                               
044000     .                                                                    
044100     EJECT                                                                
044200 G-CHECK-INPUT SECTION.                                                   
044300                                                                          
044500     MOVE YES                    TO INDATA-SW                             
044600     IF MID-W2I45201-001-GRP = ALL '+'                                    
044700       MOVE ERR-PF11-AND-NO-DATA TO MED-IDMFSFEL                          
044800       CALL WMEDKONV          USING MED-WMEDAREA                          
044900       MOVE MED-TEMFSFEL         TO MOD-TEMFSFEL                          
045000       PERFORM MFS-DONT-TOUCH-FIELD-IN                                    
045100       PERFORM MFS-DONT-TOUCH-FIELD-OUT                                   
045200       MOVE NOO                  TO INDATA-SW                             
045300     ELSE                                                                 
045310       PERFORM IMS-GU-WDGX01                                              
045320       IF SEGMENT-MISSING                                                 
045340         MOVE ERR-KEYS-MISSING                                            
045350                                 TO MED-IDMFSFEL                          
045360         MOVE NOO                TO INDATA-SW                             
045370         CALL WMEDKONV        USING MED-WMEDAREA                          
045380         MOVE MED-TEMFSFEL       TO MOD-TEMFSFEL                          
045391         PERFORM MFS-ERASE-FIELD-OUT                                      
045392       ELSE                                                               
045400                                                                          
045500         IF MID-IDFKNGRP-FOM-IN = ALL '+'                                 
045600           MOVE MFS-ERASE-FIELD  TO MOD-IDFKNGRP-FOM-IN                   
045700           MOVE MFS-NUM-FIELD-WRONG                                       
045800                                 TO MOD-IDFKNGRP-FOM-IN-ATTR              
045900           MOVE NOO              TO INDATA-SW                             
046000           MOVE ERR-HILITE-FIELDS-WRONG                                   
046100                                 TO MED-IDMFSFEL                          
046200           CALL WMEDKONV      USING MED-WMEDAREA                          
046300           MOVE MED-TEMFSFEL     TO MOD-TEMFSFEL                          
046400         ELSE                                                             
046500           MOVE MID-IDFKNGRP-FOM-IN                                       
046600                                 TO WS-IDFKNGRP-FOM-IN                    
046700           INSPECT WS-IDFKNGRP-FOM-IN REPLACING LEADING SPACE             
046800                                                     BY ZERO              
046900           MOVE MFS-DO-NOT-TOUCH-FIELD                                    
047000                                 TO MOD-IDFKNGRP-FOM-IN                   
047100           IF WS-IDFKNGRP-FOM-IN NUMERIC                                  
047200             MOVE MFS-NUM-FIELD-OK                                        
047210                                 TO MOD-IDFKNGRP-FOM-IN-ATTR              
047300           ELSE                                                           
047400             MOVE MFS-NUM-FIELD-WRONG                                     
047500                                 TO MOD-IDFKNGRP-FOM-IN-ATTR              
047600             MOVE NOO            TO INDATA-SW                             
047700             MOVE ERR-FIELDS-NOT-NUMERIC                                  
047800                                 TO MED-IDMFSFEL                          
047900             CALL WMEDKONV    USING MED-WMEDAREA                          
048000             MOVE MED-TEMFSFEL   TO MOD-TEMFSFEL                          
048100           END-IF                                                         
048200         END-IF                                                           
048300                                                                          
048400         IF MID-IDFKNGRP-TOM-IN = ALL '+'                                 
048500           MOVE MFS-ERASE-FIELD  TO MOD-IDFKNGRP-TOM-IN                   
048600           MOVE MFS-NUM-FIELD-WRONG                                       
048700                                 TO MOD-IDFKNGRP-TOM-IN-ATTR              
048800           MOVE NOO              TO INDATA-SW                             
048900           MOVE ERR-HILITE-FIELDS-WRONG                                   
049000                                 TO MED-IDMFSFEL                          
049100           CALL WMEDKONV      USING MED-WMEDAREA                          
049200           MOVE MED-TEMFSFEL     TO MOD-TEMFSFEL                          
049300         ELSE                                                             
049400           MOVE MID-IDFKNGRP-TOM-IN                                       
049500                                 TO WS-IDFKNGRP-TOM-IN                    
049600           INSPECT WS-IDFKNGRP-TOM-IN REPLACING LEADING SPACE             
049700                                                     BY ZERO              
049800           MOVE MFS-DO-NOT-TOUCH-FIELD                                    
049900                                 TO MOD-IDFKNGRP-TOM-IN                   
050000           IF WS-IDFKNGRP-TOM-IN NUMERIC                                  
050100            MOVE MFS-NUM-FIELD-OK                                         
050110                                 TO MOD-IDFKNGRP-TOM-IN-ATTR              
050200           ELSE                                                           
050300             MOVE MFS-NUM-FIELD-WRONG                                     
050400                                 TO MOD-IDFKNGRP-TOM-IN-ATTR              
050500             MOVE NOO            TO INDATA-SW                             
050600             MOVE ERR-FIELDS-NOT-NUMERIC                                  
050700                                 TO MED-IDMFSFEL                          
050800             CALL WMEDKONV    USING MED-WMEDAREA                          
050900             MOVE MED-TEMFSFEL   TO MOD-TEMFSFEL                          
051000           END-IF                                                         
051100         END-IF                                                           
051200                                                                          
051300         IF MID-KDCMD-IN = ALL '+'                                        
051400           MOVE MFS-ERASE-FIELD  TO MOD-KDCMD-IN                          
051500           MOVE MFS-ALPHA-FIELD-WRONG                                     
051600                                 TO MOD-KDCMD-IN-ATTR                     
051700           MOVE NOO              TO INDATA-SW                             
051800           MOVE ERR-HILITE-FIELDS-WRONG                                   
051900                                 TO MED-IDMFSFEL                          
052000           CALL WMEDKONV      USING MED-WMEDAREA                          
052100           MOVE MED-TEMFSFEL     TO MOD-TEMFSFEL                          
052200         ELSE                                                             
052300           MOVE MID-KDCMD-IN     TO WS-KDCMD                              
052400           MOVE MFS-DO-NOT-TOUCH-FIELD                                    
052500                                 TO MOD-KDCMD-IN                          
052600           IF WS-GOOD-KDCMD                                               
052700             MOVE MFS-ALPHA-FIELD-OK                                      
052800                                 TO MOD-KDCMD-IN-ATTR                     
052900           ELSE                                                           
053000             MOVE MFS-ALPHA-FIELD-WRONG                                   
053100                                 TO MOD-KDCMD-IN-ATTR                     
053200             MOVE NOO            TO INDATA-SW                             
053300             MOVE ERR-HILITE-FIELDS-WRONG                                 
053400                                 TO MED-IDMFSFEL                          
053500             CALL WMEDKONV    USING MED-WMEDAREA                          
053600             MOVE MED-TEMFSFEL   TO MOD-TEMFSFEL                          
053700           END-IF                                                         
053800         END-IF                                                           
053900                                                                          
054000         IF WS-KDCMD-DELETE                                               
054100           MOVE MFS-ERASE-FIELD  TO MOD-IDANSK-IN                         
054200           MOVE MFS-NUM-FIELD-OK TO MOD-IDANSK-IN-ATTR                    
054300         ELSE                                                             
054400           IF MID-IDANSK-IN = ALL '+'                                     
054500             MOVE MFS-ERASE-FIELD                                         
054510                                 TO MOD-IDANSK-IN                         
054600             MOVE MFS-NUM-FIELD-WRONG                                     
054700                                 TO MOD-IDANSK-IN-ATTR                    
054800             MOVE NOO            TO INDATA-SW                             
054900             MOVE ERR-HILITE-FIELDS-WRONG                                 
055000                                 TO MED-IDMFSFEL                          
055100             CALL WMEDKONV    USING MED-WMEDAREA                          
055200             MOVE MED-TEMFSFEL   TO MOD-TEMFSFEL                          
055300           ELSE                                                           
055400             MOVE MID-IDANSK-IN  TO WS-IDANSK-IN                          
055500             INSPECT WS-IDANSK-IN REPLACING LEADING SPACE                 
055600                                                 BY ZERO                  
055700             MOVE MFS-DO-NOT-TOUCH-FIELD                                  
055800                                 TO MOD-IDANSK-IN                         
055900             IF WS-IDANSK-IN NUMERIC                                      
056000               MOVE MFS-NUM-FIELD-OK                                      
056100                                 TO MOD-IDANSK-IN-ATTR                    
056200             ELSE                                                         
056300               MOVE MFS-NUM-FIELD-WRONG                                   
056400                                 TO MOD-IDANSK-IN-ATTR                    
056500               MOVE NOO          TO INDATA-SW                             
056600               MOVE ERR-FIELDS-NOT-NUMERIC                                
056700                                 TO MED-IDMFSFEL                          
056800               CALL WMEDKONV  USING MED-WMEDAREA                          
056900               MOVE MED-TEMFSFEL TO MOD-TEMFSFEL                          
057000             END-IF                                                       
057100           END-IF                                                         
057200         END-IF                                                           
057300                                                                          
057310         IF WS-IDFKNGRP-FOM-IN > WS-IDFKNGRP-TOM-IN                       
057320           MOVE MFS-NUM-FIELD-WRONG                                       
057330                                 TO MOD-IDFKNGRP-FOM-IN-ATTR              
057340                                    MOD-IDFKNGRP-TOM-IN-ATTR              
057350           MOVE NOO              TO INDATA-SW                             
                 MOVE ERR-CORR-HILITE-FLDS                                      
                                       TO MED-IDMFSFEL                          
057360           MOVE ERR-WRONG-INTERVAL                                        
057370                                 TO MED-IDMFSINF                          
057380           CALL WMEDKONV      USING MED-WMEDAREA                          
057390           MOVE MED-TEMFSFEL     TO MOD-TEMFSFEL                          
057390           MOVE MED-TEMFSINF     TO MOD-TEMFSINF                          
057391         END-IF                                                           
057392         IF MID-IDFKNGRP-FOM-IN = ALL '+' AND                             
057393            MID-IDFKNGRP-TOM-IN = ALL '+'                                 
057394           MOVE MFS-NUM-FIELD-WRONG                                       
057395                                 TO MOD-IDFKNGRP-FOM-IN-ATTR              
057396                                    MOD-IDFKNGRP-TOM-IN-ATTR              
057397           MOVE NOO              TO INDATA-SW                             
                 MOVE ERR-CORR-HILITE-FLDS                                      
                                       TO MED-IDMFSFEL                          
057398           MOVE ERR-BOTH-FOM-TOM-NEEDED                                   
057399                                 TO MED-IDMFSINF                          
057400           CALL WMEDKONV      USING MED-WMEDAREA                          
057401           MOVE MED-TEMFSFEL     TO MOD-TEMFSFEL                          
057401           MOVE MED-TEMFSINF     TO MOD-TEMFSINF                          
057402         END-IF                                                           
057403         IF INDATA-OK                                                     
057404           EVALUATE TRUE                                                  
057405             WHEN WS-KDCMD-CHANGE                                         
057406               PERFORM GA-CHECK-CHANGE                                    
057407             WHEN WS-KDCMD-DELETE                                         
057408               PERFORM GB-CHECK-DELETE                                    
057409             WHEN WS-KDCMD-INSERT                                         
057410               PERFORM GC-CHECK-INSERT                                    
057411           END-EVALUATE                                                   
057412         END-IF                                                           
057420         IF INDATA-WRONG                                                  
057500           PERFORM MFS-DONT-TOUCH-FIELD-OUT                               
057600           PERFORM MFS-DONT-TOUCH-FIELD-IN                                
059800         END-IF                                                           
060700       END-IF                                                             
060800     END-IF                                                               
060900     .                                                                    
061000     EJECT                                                                
061100 GA-CHECK-CHANGE SECTION.                                                 
061200                                                                          
061400     MOVE WS-IDFKNGRP-FOM-IN     TO W-IDFKNGRP-FOM                        
061500     MOVE WS-IDFKNGRP-TOM-IN     TO W-IDFKNGRP-TOM                        
061600     IF WS-IDANSK-IN = ZERO                                               
061700       MOVE MFS-NUM-FIELD-WRONG  TO MOD-IDANSK-IN-ATTR                    
061800       MOVE NOO                  TO INDATA-SW                             
061900       MOVE ERR-HILITE-FIELDS-WRONG                                       
062000                                 TO MED-IDMFSFEL                          
062100       CALL WMEDKONV          USING MED-WMEDAREA                          
062200       MOVE MED-TEMFSFEL         TO MOD-TEMFSFEL                          
062300     ELSE                                                                 
062400       MOVE MFS-NUM-FIELD-OK     TO MOD-IDANSK-IN-ATTR                    
062500     END-IF                                                               
062600     IF INDATA-OK                                                         
062700       PERFORM IMS-GHU-WDGX1140                                           
062800       IF SEGMENT-FOUND                                                   
062900         MOVE WS-IDANSK-IN       TO 1140-IDANSK                           
063000       ELSE                                                               
063100         MOVE MFS-NUM-FIELD-WRONG                                         
063200                                 TO MOD-IDFKNGRP-FOM-IN-ATTR              
063300                                    MOD-IDFKNGRP-TOM-IN-ATTR              
063400         MOVE NOO                TO INDATA-SW                             
063500         MOVE ERR-CHNG-USING-FUNC-D-N                                     
063600                                 TO MED-IDMFSINF                          
063700         MOVE ERR-HILITE-FIELDS-WRONG                                     
063800                                 TO MED-IDMFSFEL                          
063900         CALL WMEDKONV        USING MED-WMEDAREA                          
064000         MOVE MED-TEMFSINF       TO MOD-TEMFSINF                          
064100         MOVE MED-TEMFSFEL       TO MOD-TEMFSFEL                          
064200       END-IF                                                             
064300     END-IF                                                               
064400                                                                          
064500     .                                                                    
064600     EJECT                                                                
064700 GB-CHECK-DELETE SECTION.                                                 
064800                                                                          
065000     MOVE WS-IDFKNGRP-FOM-IN     TO W-IDFKNGRP-FOM                        
065100     MOVE WS-IDFKNGRP-TOM-IN     TO W-IDFKNGRP-TOM                        
065200     PERFORM IMS-GHU-WDGX1140                                             
065300     IF SEGMENT-FOUND                                                     
065400       CONTINUE                                                           
065500     ELSE                                                                 
065600       MOVE MFS-NUM-FIELD-WRONG  TO MOD-IDFKNGRP-FOM-IN-ATTR              
065700                                    MOD-IDFKNGRP-TOM-IN-ATTR              
065800       MOVE NOO                  TO INDATA-SW                             
             MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL                          
065900       MOVE ERR-FUNC-DO-NOT-EXIST                                         
066000                                 TO MED-IDMFSINF                          
066100       CALL WMEDKONV          USING MED-WMEDAREA                          
066200       MOVE MED-TEMFSFEL         TO MOD-TEMFSFEL                          
066200       MOVE MED-TEMFSINF         TO MOD-TEMFSINF                          
066300     END-IF                                                               
066400                                                                          
066500     .                                                                    
066600     EJECT                                                                
066700 GC-CHECK-INSERT SECTION.                                                 
066800                                                                          
067000     IF WS-IDANSK-IN = ZERO                                               
067100       MOVE MFS-NUM-FIELD-WRONG  TO MOD-IDANSK-IN-ATTR                    
067200       MOVE NOO                  TO INDATA-SW                             
067300       MOVE ERR-HILITE-FIELDS-WRONG                                       
067400                                 TO MED-IDMFSFEL                          
067500       CALL WMEDKONV          USING MED-WMEDAREA                          
067600       MOVE MED-TEMFSFEL         TO MOD-TEMFSFEL                          
067700     ELSE                                                                 
067800       MOVE MFS-NUM-FIELD-OK     TO MOD-IDANSK-IN-ATTR                    
068000     END-IF                                                               
068100                                                                          
068200     IF INDATA-OK                                                         
068300       PERFORM IMS-GNP-WDGX1140                                           
068400       IF SEGMENT-FOUND AND                                               
                1140-IDLANDX2 = W-IDLANDX2                                      
068500         PERFORM GCA-CHECK-INTERVAL                                       
068600         PERFORM                                                          
068700           UNTIL SEGMENT-MISSING OR INDATA-WRONG OR                       
068800                 WS-IDFKNGRP-TOM-IN-NUM < 1140-IDFKNGRP-FOM               
068900           PERFORM IMS-GNP-WDGX1140                                       
069000           IF SEGMENT-FOUND AND                                           
                    1140-IDLANDX2 = W-IDLANDX2                                  
069100             PERFORM GCA-CHECK-INTERVAL                                   
                 ELSE                                                           
                   SET SEGMENT-MISSING TO TRUE                                  
069200           END-IF                                                         
069300         END-PERFORM                                                      
069400       END-IF                                                             
069500     END-IF                                                               
069600                                                                          
           IF INDATA-OK                                                         
             MOVE W-IDLANDX2           TO 1140-IDLANDX2                         
             MOVE WS-IDFKNGRP-TOM-IN-NUM                                        
                                       TO 1140-IDFKNGRP-TOM                     
             MOVE WS-IDFKNGRP-FOM-IN-NUM                                        
                                       TO 1140-IDFKNGRP-FOM                     
067900       MOVE WS-IDANSK-IN         TO 1140-IDANSK                           
           END-IF                                                               
                                                                                
069700     .                                                                    
069800     EJECT                                                                
069900 GCA-CHECK-INTERVAL SECTION.                                              
070000                                                                          
070200     EVALUATE TRUE                                                        
070300       WHEN WS-IDFKNGRP-FOM-IN-NUM =  1140-IDFKNGRP-FOM AND               
070400            WS-IDFKNGRP-TOM-IN-NUM =  1140-IDFKNGRP-TOM                   
070500       WHEN WS-IDFKNGRP-FOM-IN-NUM >= 1140-IDFKNGRP-FOM AND               
070600            WS-IDFKNGRP-FOM-IN-NUM <= 1140-IDFKNGRP-TOM                   
070700       WHEN WS-IDFKNGRP-TOM-IN-NUM >= 1140-IDFKNGRP-FOM AND               
070800            WS-IDFKNGRP-TOM-IN-NUM <= 1140-IDFKNGRP-TOM                   
070900       WHEN WS-IDFKNGRP-FOM-IN-NUM <  1140-IDFKNGRP-FOM AND               
071000            WS-IDFKNGRP-TOM-IN-NUM >  1140-IDFKNGRP-TOM                   
071100         MOVE NOO                TO INDATA-SW                             
071200         MOVE MFS-NUM-FIELD-WRONG                                         
071300                                 TO MOD-IDFKNGRP-FOM-IN-ATTR              
071400                                    MOD-IDFKNGRP-TOM-IN-ATTR              
               MOVE ERR-CORR-HILITE-FLDS                                        
                                       TO MED-IDMFSFEL                          
071500         MOVE ERR-INT-PART-INT-REPORTED                                   
071600                                 TO MED-IDMFSINF                          
071700         CALL WMEDKONV        USING MED-WMEDAREA                          
071800         MOVE MED-TEMFSFEL       TO MOD-TEMFSFEL                          
071800         MOVE MED-TEMFSINF       TO MOD-TEMFSINF                          
071900     END-EVALUATE                                                         
072000     .                                                                    
072100     EJECT                                                                
072200 H-UPDATE SECTION.                                                        
072300                                                                          
072500     IF WS-KDCMD-INSERT                                                   
072600       PERFORM IMS-GHU-WDGX01                                             
072700       MOVE WS-IDFKNGRP-FOM-IN   TO 1140-IDFKNGRP-FOM                     
072710                                    W-IDFKNGRP-FOM                        
072800       MOVE WS-IDFKNGRP-TOM-IN   TO 1140-IDFKNGRP-TOM                     
072810                                    W-IDFKNGRP-TOM                        
072900       MOVE WS-IDANSK-IN         TO 1140-IDANSK                           
073000       PERFORM IMS-ISRT-WDGX1140                                          
073300     ELSE                                                                 
073400       IF WS-KDCMD-DELETE                                                 
073500         PERFORM IMS-DLET-WDGX1140                                        
073600       ELSE                                                               
073700         PERFORM IMS-REPL-WDGX1140                                        
073800       END-IF                                                             
073900     END-IF                                                               
074300     IF WS-KDCMD-DELETE                                                   
074500       PERFORM MFS-ERASE-FIELD-IN                                         
074510       MOVE ZERO                 TO W-IDFKNGRP-FOM                        
074520                                    W-IDFKNGRP-TOM                        
075201       MOVE INF-UPDATE-DONE      TO MED-IDMFSFEL                          
074600     ELSE                                                                 
074700       MOVE MFS-ADD-HILIGHT-FIELD                                         
074800                                 TO MOD-IDFKNGRP-FOM-ATTR (1 , 1)         
074900                                    MOD-IDFKNGRP-TOM-ATTR (1 , 1)         
075000                                    MOD-IDANSK-ATTR       (1 , 1)         
075100       PERFORM MFS-ERASE-FIELD-IN                                         
075110       MOVE ERR-LINE-UPDATED     TO MED-IDMFSFEL                          
075200     END-IF                                                               
075210     CALL WMEDKONV            USING MED-WMEDAREA                          
075230     MOVE MED-TEMFSFEL           TO MOD-TEMFSFEL                          
075300     .                                                                    
075400     EJECT                                                                
075500 MFS-ERASE-FIELD-OUT SECTION.                                             
075600                                                                          
075800     PERFORM                                                              
075900     VARYING COL-INDX FROM +1 BY +1                                       
076000       UNTIL COL-INDX > MAX-COL-INDX                                      
076100       PERFORM MFS-ERASE-LINE-FIELD-OUT                                   
076200       VARYING ROW-INDX FROM +1 BY +1                                     
076300         UNTIL ROW-INDX > MAX-ROW-INDX                                    
076400     END-PERFORM                                                          
076500     .                                                                    
076600     SKIP3                                                                
076700 MFS-ERASE-LINE-FIELD-OUT SECTION.                                        
076800                                                                          
077100     MOVE MFS-ERASE-FIELD        TO                                       
077200                          MOD-IDFKNGRP-FOM (ROW-INDX , COL-INDX)          
077300                          MOD-IDFKNGRP-TOM (ROW-INDX , COL-INDX)          
077400                          MOD-IDANSK       (ROW-INDX , COL-INDX)          
077500     .                                                                    
077600     SKIP3                                                                
077700 MFS-ERASE-FIELD-IN SECTION.                                              
077800                                                                          
078100     MOVE MFS-ERASE-FIELD        TO MOD-IDFKNGRP-FOM-IN                   
078200                                    MOD-IDFKNGRP-TOM-IN                   
078300                                    MOD-IDANSK-IN                         
078400                                    MOD-KDCMD-IN                          
078500     .                                                                    
078600     EJECT                                                                
078700 MFS-DONT-TOUCH-FIELD-OUT  SECTION.                                       
078800                                                                          
079000     PERFORM                                                              
079100     VARYING COL-INDX FROM +1 BY +1                                       
079200       UNTIL COL-INDX > MAX-COL-INDX                                      
079300       PERFORM MFS-DONT-TOUCH-LINE-FIELD-OUT                              
079400       VARYING ROW-INDX FROM +1 BY +1                                     
079500         UNTIL ROW-INDX > MAX-ROW-INDX                                    
079600     END-PERFORM                                                          
079700     .                                                                    
079800     SKIP2                                                                
079900 MFS-DONT-TOUCH-LINE-FIELD-OUT  SECTION.                                  
080000                                                                          
080200     MOVE MFS-DO-NOT-TOUCH-FIELD TO                                       
080300                          MOD-IDFKNGRP-FOM (ROW-INDX , COL-INDX)          
080400                          MOD-IDFKNGRP-TOM (ROW-INDX , COL-INDX)          
080500                          MOD-IDANSK       (ROW-INDX , COL-INDX)          
080600     .                                                                    
080700     SKIP3                                                                
080800 MFS-DONT-TOUCH-FIELD-IN  SECTION.                                        
080900                                                                          
081100     MOVE MFS-DO-NOT-TOUCH-FIELD TO MOD-IDFKNGRP-FOM-IN                   
081200                                    MOD-IDFKNGRP-TOM-IN                   
081300                                    MOD-IDANSK-IN                         
081400                                    MOD-KDCMD-IN                          
081500     .                                                                    
081600     EJECT                                                                
081700 MFS-FORM-ATTR SECTION.                                                   
081800                                                                          
082000     MOVE MFS-FORMAT-DEFAULT-ATTR                                         
082100                                 TO MOD-IDFKNGRP-FOM-IN-ATTR              
082200                                    MOD-IDFKNGRP-TOM-IN-ATTR              
082300                                    MOD-IDANSK-IN-ATTR                    
082400                                    MOD-KDCMD-IN-ATTR                     
082500     .                                                                    
082600     SKIP2                                                                
082700* --- IMS SECTIONS ---                                                    
082800     SKIP3                                                                
082900 IMS-GET-MSG SECTION.                                                     
083000                                                                          
083200     MOVE '  QC'                 TO GOOD-STATUSCODES                      
083300     CALL CBLTDLI             USING GU                                    
083400                                    MSG-PCB                               
083500                                    MSG-IO-AREA                           
083600     MOVE MSG-STATUS-CODE        TO STATUS-WS                             
083700     PERFORM IMS-STATUSCHECK                                              
083800     .                                                                    
083900     SKIP3                                                                
084000 IMS-INSERT-MSG SECTION.                                                  
084100                                                                          
084300*    IF MSGI-IDLAND-SPR = 'SE'                                            
084400*      MOVE '0'                  TO MFS-KDHUVOMR                          
084500*    END-IF                                                               
084600     MOVE LOW-VALUE              TO MSG-KDZ1 MSG-KDZ2                     
084700     MOVE SPACE                  TO GOOD-STATUSCODES                      
084800     CALL CBLTDLI             USING ISRT                                  
084900                                    MSG-PCB                               
085000                                    MSG-IO-AREA                           
085100                                    MFS-IDMOD                             
085200     MOVE MSG-STATUS-CODE        TO STATUS-WS                             
085300     PERFORM IMS-STATUSCHECK                                              
085400     .                                                                    
085500     EJECT                                                                
085600 IMS-GU-WDGX01 SECTION.                                                   
085700                                                                          
085900     STRING 'WDG201  (WDGXKEY  =' W-1139-KEY-X ')'                        
086000          DELIMITED BY SIZE INTO SSA1                                     
086100     MOVE '  GE'                 TO GOOD-STATUSCODES                      
086200     CALL CBLTDLI             USING GU                                    
086300                                    WDG2-PCB                              
086400                                    DLI-IO-WDGX01                         
086500                                    SSA1                                  
086600     MOVE WDG2-STATUS-CODE       TO STATUS-WS                             
086700     PERFORM IMS-STATUSCHECK                                              
086800     .                                                                    
086900     SKIP3                                                                
087000 IMS-GHU-WDGX01 SECTION.                                                  
087100                                                                          
087300     STRING 'WDG201  (WDGXKEY  =' W-1139-KEY-X ')'                        
087400          DELIMITED BY SIZE INTO SSA1                                     
087500     MOVE '  GE'                 TO GOOD-STATUSCODES                      
087600     CALL CBLTDLI             USING GHU                                   
087700                                    WDG2-PCB                              
087800                                    DLI-IO-WDGX01                         
087900                                    SSA1                                  
088000     MOVE WDG2-STATUS-CODE       TO STATUS-WS                             
088100     PERFORM IMS-STATUSCHECK                                              
088200     .                                                                    
088300     SKIP3                                                                
088400 IMS-GNP-WDGX1140 SECTION.                                                
088500                                                                          
088700     MOVE 'WDGX1140'             TO SSA2                                  
088900     MOVE '  GE'                 TO GOOD-STATUSCODES                      
089000     CALL CBLTDLI             USING GNP                                   
089100                                    WDG2-PCB                              
089200                                    DLI-IO-WDGX1140                       
089300                                    SSA2                                  
089400     MOVE WDG2-STATUS-CODE       TO STATUS-WS                             
089500     PERFORM IMS-STATUSCHECK                                              
089600     .                                                                    
089700     SKIP3                                                                
089800 IMS-GNP-KEY-WDGX1140 SECTION.                                            
089900                                                                          
090100     STRING 'WDGX1140(KY1140  >=' W-1140-KEY-X ')'                        
090200          DELIMITED BY SIZE INTO SSA1                                     
090300     MOVE '  GE'                 TO GOOD-STATUSCODES                      
090400     CALL CBLTDLI             USING GNP                                   
090500                                    WDG2-PCB                              
090600                                    DLI-IO-WDGX1140                       
090700                                    SSA1                                  
090800     MOVE WDG2-STATUS-CODE       TO STATUS-WS                             
090900     PERFORM IMS-STATUSCHECK                                              
091000     .                                                                    
091100     SKIP3                                                                
091200 IMS-GHU-WDGX1140 SECTION.                                                
091300                                                                          
091500     STRING 'WDGX1140(KY1140   =' W-1140-KEY-X ')'                        
091600          DELIMITED BY SIZE INTO SSA1                                     
091700     MOVE '  GE'                 TO GOOD-STATUSCODES                      
091800     CALL CBLTDLI             USING GHU                                   
091900                                    WDG2-PCB                              
092000                                    DLI-IO-WDGX1140                       
092100                                    SSA1                                  
092200     MOVE WDG2-STATUS-CODE       TO STATUS-WS                             
092300     PERFORM IMS-STATUSCHECK                                              
092400     .                                                                    
092500     SKIP3                                                                
092600 IMS-ISRT-WDGX1140 SECTION.                                               
092700                                                                          
092900     MOVE 'WDGX1140'             TO SSA2                                  
093200     MOVE '  '                   TO GOOD-STATUSCODES                      
093300     CALL CBLTDLI             USING ISRT                                  
093400                                    WDG2-PCB                              
093500                                    DLI-IO-WDGX1140                       
093600                                    SSA2                                  
093700     MOVE WDG2-STATUS-CODE       TO STATUS-WS                             
093800     PERFORM IMS-STATUSCHECK                                              
093900     .                                                                    
094000     SKIP3                                                                
094100 IMS-REPL-WDGX1140 SECTION.                                               
094200                                                                          
094400     MOVE '  '                   TO GOOD-STATUSCODES                      
094500     CALL CBLTDLI             USING REPL                                  
094600                                    WDG2-PCB                              
094700                                    DLI-IO-WDGX1140                       
094800     MOVE WDG2-STATUS-CODE       TO STATUS-WS                             
094900     PERFORM IMS-STATUSCHECK                                              
095000     .                                                                    
095100     SKIP3                                                                
095200 IMS-DLET-WDGX1140 SECTION.                                               
095300                                                                          
095500     MOVE '  '                   TO GOOD-STATUSCODES                      
095600     CALL CBLTDLI             USING DLET                                  
095700                                    WDG2-PCB                              
095800                                    DLI-IO-WDGX1140                       
095900     MOVE WDG2-STATUS-CODE       TO STATUS-WS                             
096000     PERFORM IMS-STATUSCHECK                                              
096100     .                                                                    
096200     EJECT                                                                
096300 IMS-STATUSCHECK SECTION.                                                 
096400                                                                          
096600     SET STATUS-IX               TO 1                                     
096700     SEARCH GOOD-STATUS                                                   
096800       AT END                                                             
096900         STRING ' INVALID STATUS CODE FROM IMS: ' STATUS-WS               
097000         DELIMITED BY SIZE INTO ERROR-TEXT                                
097100         CALL FELLOG                                                      
097200       WHEN GOOD-STATUS (STATUS-IX) = STATUS-WS                           
097300         CONTINUE                                                         
097400     END-SEARCH                                                           
097500     .                                                                    
