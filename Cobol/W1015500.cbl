000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W1015500.                                                
000300 AUTHOR.         ARCHANA BHAT.                                            
000400 DATE-WRITTEN.   16/03/02.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNCTION:                                                            
000800*        THIS PROGRAM UPDATES THE PLANNER ID ON WDK6 FOR A                
000900*        FUNCTION GROUP RANGE.                                            
001000*        A BACKGROUND BATCH(W115S4) IS TRIGGERED IN ORDER                 
001100*        TO DO THE UPDATE                                                 
001200*                                                                         
001300*        THE PROGRAM READS     WDP3                                       
001400*                                                                         
001500*    INDATA.                                                              
001600*        TRANSACTION: W1T155                                              
001700*        MID:         W1I15501                                            
001800*                                                                         
001900*    OUTDATA.                                                             
002000*        MOD:         W1O15501                                            
002100                                                                          
002200     SKIP3                                                                
002300 ENVIRONMENT DIVISION.                                                    
002400                                                                          
002500 DATA DIVISION.                                                           
002600     EJECT                                                                
002700 WORKING-STORAGE SECTION.                                                 
002800 77  IDPGM                       PIC X(08)   VALUE 'W1015500'.            
002900                                                                          
003000*    --- WORK FIELDS FOR ERROR MESSAGES WHEN CALLING ABEND/FELLOG         
003100 77  ERROR-TEXT                  PIC X(80) VALUE SPACE.                   
003200                                                                          
003300 77  YES                         PIC X       VALUE 'J'.                   
003400 77  NOO                         PIC X       VALUE 'N'.                   
003500 77  IX                          PIC 9(2)    VALUE ZEROS.                 
003510 77  IX2                         PIC 9(2)    VALUE ZEROS.                 
003600 77  MAX-IX                      PIC 9(2)    VALUE 5.                     
003710 77  WS-STRING                   PIC X(4)    VALUE SPACES.                
003711 77  WS-CNT                      PIC 9(1)    VALUE 0.                     
003712 77  WS-CNT1                     PIC 9(1)    VALUE 0.                     
003720 77  WS-IDFRIDATA                PIC X(25).                               
003800 77  WS-IDFKNGRP-FOM             PIC 9(4) VALUE ZEROS.                    
003900 77  WS-IDFKNGRP-TOM             PIC 9(4) VALUE ZEROS.                    
004000                                                                          
004100*    --- WORK FIELDS FOR ACTUAL KEYVALUES OF SCREEN                       
004200                                                                          
004300                                                                          
004400 77  INDATA-SW                   PIC X       VALUE 'J'.                   
004500     88  INDATA-OK                           VALUE 'J'.                   
004600     88  INDATA-NOT-OK                       VALUE 'N'.                   
004700                                                                          
004710 77  INDATA-1-SW                 PIC X       VALUE 'J'.                   
004720     88  INDATA-1-OK                         VALUE 'J'.                   
004800 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
004900     88  OWN-MID                             VALUE '1155'.                
005000     88  GOOD-MID                            VALUE '1151' '1152'          
005100                                                   '1153' '1154'          
005200                                                   '1155' '1156'          
005300                                                   '1157' '1158'          
005400                                                   '1159'.                
005500     88  HELP-MID                            VALUE '0551'.                
005600 77  W-VALID-KDPRODSL            PIC X(2)    VALUE SPACE.                 
005700     88  GOOD-KDPRODSL                       VALUE '11' '13' '14'         
005800                                                   '15' '16' '18'.        
005900 77  W-VALID-KDSORT              PIC X(2)    VALUE SPACE.                 
006000     88  GOOD-KDSORT                         VALUE 'ST' 'SA' 'M '         
006100                                 'MM' 'ML' 'KG' 'G' 'SW' 'HW' 'L'.        
006101                                                                          
006110 77  W-VALID-GCP                 PIC X(1)    VALUE SPACE.                 
006120     88  GOOD-GCP                            VALUE 'J' 'N' 'E'.           
006200     EJECT                                                                
006300*    --- SUBPROGRAMS AND PARAMETER AREAS                                  
006400 01  GENERAL-SUBPROGRAMS.                                                 
006500     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
006600     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
006700     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
006800     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
006900     03  WDECEDIT                PIC X(8)    VALUE 'WDECEDIT'.            
007000     EJECT                                                                
007100*    --- PARAMETERS FOR SUB PROGRAM WDECEDIT                              
007200*01  -COPY WDECAREA                                                       
007300*    --- PARAMETERS FOR SUBPROGRAM WMEDKONV                               
007400*01 -COPY WMEDAREA                                                        
007500     SKIP3                                                                
007600 01  MESSAGE-CODES.                                                       
007700     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
007800     03  ERR-CORR-HIGHLIT-FLDS   PIC X(3)    VALUE '001'.                 
007900     03  INF-PRESS-PF11          PIC X(3)    VALUE '003'.                 
008000     03  ERR-PF11-AND-NO-DATA    PIC X(3)    VALUE '011'.                 
008100     03  INF-BMP-START           PIC X(11)   VALUE                        
008200                                 'BMP STARTAD'.                           
008300     EJECT                                                                
008400 01  PROG-TO-PROG-SW.                                                     
008500*    03  -COPY WMSGSOP                                                    
008600     EJECT                                                                
008700 01  WS-BMP-PARM.                                                         
008800   03  WS-BMP-KDPRODSL           PIC  9(2)  VALUE ZERO.                   
008900   03  WS-BMP-KDSORT-I-GRP OCCURS 5 TIMES.                                
009000       05 WS-BMP-KDSORT-INCL     PIC  X(2)  VALUE SPACES.                 
009100   03  WS-BMP-KDSORT-E-GRP OCCURS 5 TIMES.                                
009200       05 WS-BMP-KDSORT-EXCL     PIC  X(2)  VALUE SPACES.                 
009300   03  WS-BMP-GCP-FLAG           PIC  X(1)  VALUE SPACES.                 
009310   03  WS-BMP-IDFKNGRP-FOM       PIC  9(4)  VALUE ZERO.                   
009400   03  WS-BMP-IDFKNGRP-TOM       PIC  9(4)  VALUE ZERO.                   
009500   03  WS-BMP-IDBERED            PIC  9(2)  VALUE ZERO.                   
009600 01  WS-IDMAIL.                                                           
009700   03  IDMAIL                    PIC  X(58) VALUE SPACES.                 
009800*    --- PARAMETERS FOR SUB PROGRAM W005INIT                              
009900*                                                                         
010000 01  FILLER                      PIC X(16)   VALUE 'WMSGINIT'.            
010100     SKIP3                                                                
010200*01 -COPY WMSGINIT                                                        
010300     EJECT                                                                
010400*    --- AREAS FOR MFS AND SCREEN MANAGEMENT                              
010500*                                                                         
010600 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
010700     SKIP3                                                                
010800*01  MID -COPY W1I15501                                                   
010900     EJECT                                                                
011000 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
011100     SKIP3                                                                
011200*01  -COPY WMSGAREA                                                       
011300     EJECT                                                                
011400     03  MOD REDEFINES MSG-AREA.                                          
011500*      05  -COPY W1O15501                                                 
011600     EJECT                                                                
011700 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
011800     SKIP3                                                                
011900*01  -COPY WMFSAREA                                                       
012000     EJECT                                                                
012100*    --- WORK-AREAS FOR IMS-SECTIONS                                      
012200*                                                                         
012300 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
012400     SKIP3                                                                
012500 01  KEYS-FOR-DLI.                                                        
012600     03  W-KDARBTYP-X.                                                    
012700         05  W-KDARBTYP          PIC X(8)    VALUE SPACE.                 
012800     03  W-IDPERSON-X.                                                    
012900         05  W-IDPERSON          PIC S9(3)   VALUE ZERO COMP-3.           
013000     SKIP2                                                                
013100*    --- STATUS CODES FROM IMS                                            
013200 01  STATUS-WS                   PIC XX.                                  
013300     88  SEGMENT-FOUND                       VALUE '  '.                  
013400     88  SEGMENT-FOUND-EXISTS                VALUE 'II'.                  
013500     88  SEGMENT-MISSING                     VALUE 'GE'.                  
013600     SKIP2                                                                
013700 01  GOOD-STATUSCODES.                                                    
013800     03  GOOD-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
013900     SKIP3                                                                
014000 01  SSA1                        PIC X(64).                               
014100 01  SSA2                        PIC X(64).                               
014200     EJECT                                                                
014300*    --- IMS FUNCTION CODES                                               
014400*01  -COPY W0003                                                          
014500     EJECT                                                                
014600*    ---  DLI INPUT-OUTPUT AREA                                           
014700                                                                          
014800 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDP301'.                      
014900 01  DLI-IO-WDP301.                                                       
015000*    03  -COPY WDP301                                                     
015100     EJECT                                                                
015200 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDP311'.                      
015300 01  DLI-IO-WDP311.                                                       
015400*    03  -COPY WDP311                                                     
015500     EJECT                                                                
015600 LINKAGE SECTION.                                                         
015700*01  -COPY W0009   -PRE MSG-                                              
015800*01  -COPY W0009   -PRE ALT-                                              
015900*01  -COPY W0008   -PRE WDP7-                                             
016000     05  FILLER                  PIC X.                                   
016100                                                                          
016200*01  -COPY W0008  -PRE WDP3-                                              
016300     05  FILLER                  PIC X.                                   
016400     EJECT                                                                
016500 PROCEDURE DIVISION  USING MSG-PCB ALT-PCB WDP7-PCB WDP3-PCB.             
016600 MAIN SECTION.                                                            
016700     ENTRY 'DLITCBL' USING MSG-PCB ALT-PCB WDP7-PCB WDP3-PCB.             
016800                                                                          
016900     PERFORM IMS-GET-MSG                                                  
017000     IF SEGMENT-FOUND                                                     
017100       PERFORM A-INIT                                                     
017200       IF MFS-UPDATE                                                      
017300          PERFORM G-CHECK-INPUT                                           
017400          IF INDATA-OK                                                    
017500            PERFORM H-UPDATE                                              
017600          END-IF                                                          
017700       ELSE                                                               
017810          IF MFS-FIRST                                                    
017820             PERFORM C-FIRST-PAGE                                         
017830          ELSE                                                            
017840             PERFORM E-SAME-PAGE                                          
017850          END-IF                                                          
017900       END-IF                                                             
018000                                                                          
018100       COMPUTE MSG-KVLL = LENGTH OF MOD-W1O15501 + 4                      
018200       PERFORM IMS-INSERT-MSG                                             
018300     END-IF                                                               
018400     MOVE ZERO TO RETURN-CODE                                             
018500     GOBACK                                                               
018600     .                                                                    
018700     EJECT                                                                
018800 A-INIT SECTION.                                                          
018900                                                                          
019000     IF MSG-DOUBLE-TRANSACTIONS                                           
019100       MOVE MSG-INDATA-MINUS-2-TRANSACT   TO MID-W1I15501                 
019200       MOVE MSG-IDTRANS-2  TO MFS-IDTRANS                                 
019300       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
019400     ELSE                                                                 
019500       MOVE MSG-INDATA-MINUS-1-TRANSACT  TO MID-W1I15501                  
019600       MOVE MSG-IDTRANS-1  TO MFS-IDTRANS                                 
019700       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
019800     END-IF                                                               
019900                                                                          
019940                                                                          
020000     MOVE MSG-KDTRTYP TO MFS-KDTRTYP                                      
020100     MOVE MSG-IDPFK   TO MFS-IDPFK                                        
020200     MOVE MFS-IDTRANS TO W-IDTRANS                                        
020300                                                                          
020400     MOVE LOW-VALUE TO MSG-AREA                                           
020500     MOVE 'W1O15501' TO MFS-IDMOD                                         
020600     MOVE '1155' TO MOD-IDTRANS                                           
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
021800                                                                          
021900     MOVE ALL '+'           TO MSGI-WMSGINIT                              
022000     MOVE '001'             TO MSGI-KDCALL                                
022100     MOVE MSG-LTERM-NAME    TO MSGI-IDLTERM-USER                          
022200     MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                                
022300     MOVE '1155'            TO MSGI-IDTRANS                               
022400     CALL W005INIT USING MSGI-WMSGINIT WDP7-PCB                           
022500                                                                          
022600*    - LANGUAGE TO BE USED BY MEDKONV                                     
022700     MOVE MSGI-IDLAND-SPR TO MED-IDSKYLT                                  
022800     .                                                                    
022900     EJECT                                                                
022910 C-FIRST-PAGE SECTION.                                                    
022920                                                                          
022930     PERFORM MFS-ERASE-FIELD-IN                                           
022940     .                                                                    
022950     EJECT                                                                
023000 E-SAME-PAGE SECTION.                                                     
023100                                                                          
023200     IF MID-INPUT = ALL '+'                                               
023300       PERFORM MFS-ERASE-FIELD-IN                                         
023400     ELSE                                                                 
023500       MOVE INF-PRESS-PF11 TO MED-IDMFSFEL                                
023600       CALL WMEDKONV    USING MED-WMEDAREA                                
023700       MOVE MED-MFSFEL     TO MOD-TEMFSFEL                                
023800       PERFORM MFS-READ-IN-AGAIN                                          
023900       PERFORM EA-MID-INDATA-TILL-MOD                                     
024000     END-IF                                                               
024100     .                                                                    
024200     EJECT                                                                
024300 EA-MID-INDATA-TILL-MOD SECTION.                                          
024400                                                                          
024500     IF MID-KDARBTYP-IN = ALL '+'                                         
024600        MOVE MFS-ERASE-FIELD        TO MOD-KDARBTYP-IN                    
024700     ELSE                                                                 
024800        MOVE MFS-DO-NOT-TOUCH-FIELD TO MOD-KDARBTYP-IN                    
024900     END-IF                                                               
025000                                                                          
025100     IF MID-IDPERSON-IN = ALL '+'                                         
025200        MOVE MFS-ERASE-FIELD        TO MOD-IDPERSON-IN                    
025300     ELSE                                                                 
025400        MOVE MFS-DO-NOT-TOUCH-FIELD TO MOD-IDPERSON-IN                    
025500     END-IF                                                               
025600                                                                          
025700     IF MID-KDPRODSL-IN = ALL '+'                                         
025800        MOVE MFS-ERASE-FIELD        TO MOD-KDPRODSL-IN                    
025900     ELSE                                                                 
026000        MOVE MFS-DO-NOT-TOUCH-FIELD TO MOD-KDPRODSL-IN                    
026100     END-IF                                                               
026200                                                                          
026210     IF MID-FLAGGA-GCP-IN = ALL '+'                                       
026220        MOVE MFS-ERASE-FIELD        TO MOD-FLAGGA-GCP-IN                  
026230     ELSE                                                                 
026240        MOVE MFS-DO-NOT-TOUCH-FIELD TO MOD-FLAGGA-GCP-IN                  
026250     END-IF                                                               
026260                                                                          
026300     IF MID-IDFKNGRP-FOM-IN = ALL '+'                                     
026400        MOVE MFS-ERASE-FIELD        TO MOD-IDFKNGRP-FOM-IN                
026500     ELSE                                                                 
026600        MOVE MFS-DO-NOT-TOUCH-FIELD TO MOD-IDFKNGRP-FOM-IN                
026700     END-IF                                                               
026800                                                                          
026900     IF MID-IDFKNGRP-TOM-IN = ALL '+'                                     
027000        MOVE MFS-ERASE-FIELD        TO MOD-IDFKNGRP-TOM-IN                
027100     ELSE                                                                 
027200        MOVE MFS-DO-NOT-TOUCH-FIELD TO MOD-IDFKNGRP-TOM-IN                
027300     END-IF                                                               
027400                                                                          
027500     IF MID-IDBERED-IN = ALL '+'                                          
027600        MOVE MFS-ERASE-FIELD        TO MOD-IDBERED-IN                     
027700     ELSE                                                                 
027800        MOVE MFS-DO-NOT-TOUCH-FIELD TO MOD-IDBERED-IN                     
027900     END-IF                                                               
028000                                                                          
028100     PERFORM VARYING IX FROM 1 BY 1                                       
028200     UNTIL IX > MAX-IX                                                    
028300       IF MID-KDSORT-INCL-IN(IX) = ALL '+'                                
028400          MOVE MFS-ERASE-FIELD        TO MOD-KDSORT-INCL-IN(IX)           
028500       ELSE                                                               
028600          MOVE MFS-DO-NOT-TOUCH-FIELD TO MOD-KDSORT-INCL-IN(IX)           
028700       END-IF                                                             
028800                                                                          
028900       IF MID-KDSORT-INCL-IN(IX) = ALL '+'                                
029000          MOVE MFS-ERASE-FIELD        TO MOD-KDSORT-INCL-IN(IX)           
029100       ELSE                                                               
029200          MOVE MFS-DO-NOT-TOUCH-FIELD TO MOD-KDSORT-INCL-IN(IX)           
029300       END-IF                                                             
029400                                                                          
029500       IF MID-KDSORT-INCL-IN(IX) = ALL '+'                                
029600          MOVE MFS-ERASE-FIELD        TO MOD-KDSORT-INCL-IN(IX)           
029700       ELSE                                                               
029800          MOVE MFS-DO-NOT-TOUCH-FIELD TO MOD-KDSORT-INCL-IN(IX)           
029900       END-IF                                                             
030000                                                                          
030100       IF MID-KDSORT-INCL-IN(IX) = ALL '+'                                
030200          MOVE MFS-ERASE-FIELD        TO MOD-KDSORT-INCL-IN(IX)           
030300       ELSE                                                               
030400          MOVE MFS-DO-NOT-TOUCH-FIELD TO MOD-KDSORT-INCL-IN(IX)           
030500       END-IF                                                             
030600                                                                          
030700       IF MID-KDSORT-INCL-IN(IX) = ALL '+'                                
030800          MOVE MFS-ERASE-FIELD        TO MOD-KDSORT-INCL-IN(IX)           
030900       ELSE                                                               
031000          MOVE MFS-DO-NOT-TOUCH-FIELD TO MOD-KDSORT-INCL-IN(IX)           
031100       END-IF                                                             
031200                                                                          
031300       IF MID-KDSORT-EXCL-IN(IX) = ALL '+'                                
031400          MOVE MFS-ERASE-FIELD        TO MOD-KDSORT-EXCL-IN(IX)           
031500       ELSE                                                               
031600          MOVE MFS-DO-NOT-TOUCH-FIELD TO MOD-KDSORT-EXCL-IN(IX)           
031700       END-IF                                                             
031800                                                                          
031900       IF MID-KDSORT-EXCL-IN(IX) = ALL '+'                                
032000          MOVE MFS-ERASE-FIELD        TO MOD-KDSORT-EXCL-IN(IX)           
032100       ELSE                                                               
032200          MOVE MFS-DO-NOT-TOUCH-FIELD TO MOD-KDSORT-EXCL-IN(IX)           
032300       END-IF                                                             
032400                                                                          
032500       IF MID-KDSORT-EXCL-IN(IX) = ALL '+'                                
032600          MOVE MFS-ERASE-FIELD        TO MOD-KDSORT-EXCL-IN(IX)           
032700       ELSE                                                               
032800          MOVE MFS-DO-NOT-TOUCH-FIELD TO MOD-KDSORT-EXCL-IN(IX)           
032900       END-IF                                                             
033000                                                                          
033100       IF MID-KDSORT-EXCL-IN(IX) = ALL '+'                                
033200          MOVE MFS-ERASE-FIELD        TO MOD-KDSORT-EXCL-IN(IX)           
033300       ELSE                                                               
033400          MOVE MFS-DO-NOT-TOUCH-FIELD TO MOD-KDSORT-EXCL-IN(IX)           
033500       END-IF                                                             
033600                                                                          
033700       IF MID-KDSORT-EXCL-IN(IX) = ALL '+'                                
033800          MOVE MFS-ERASE-FIELD        TO MOD-KDSORT-EXCL-IN(IX)           
033900       ELSE                                                               
034000          MOVE MFS-DO-NOT-TOUCH-FIELD TO MOD-KDSORT-EXCL-IN(IX)           
034100       END-IF                                                             
034200     END-PERFORM                                                          
034300                                                                          
034400     .                                                                    
034500     EJECT                                                                
034600                                                                          
034700 G-CHECK-INPUT   SECTION.                                                 
034800                                                                          
034900     MOVE YES   TO INDATA-SW                                              
035000                                                                          
035100     IF MID-INPUT = ALL '+'                                               
035200        MOVE ERR-PF11-AND-NO-DATA TO MED-IDMFSFEL                         
035300        MOVE NOO                  TO INDATA-SW                            
035400        CALL WMEDKONV USING MED-WMEDAREA                                  
035500        MOVE MED-TEMFSFEL         TO MOD-TEMFSFEL                         
035600        PERFORM MFS-ERASE-FIELD-IN                                        
035700     ELSE                                                                 
035800        PERFORM GA-CHECK-MAIL-ID                                          
036000        PERFORM GB-CHECK-KDPRODSL                                         
036300        PERFORM GC-CHECK-KDSORT                                           
036420        PERFORM GD-CHECK-GCP-FLAG                                         
036600        PERFORM GE-CHECK-IDFKNGRP                                         
036900        PERFORM GF-CHECK-PLANNER                                          
037100     END-IF                                                               
037200                                                                          
037300     IF INDATA-NOT-OK                                                     
037400        MOVE ERR-CORR-HIGHLIT-FLDS     TO MED-IDMFSFEL                    
037500        CALL WMEDKONV USING MED-WMEDAREA                                  
037600        MOVE MED-TEMFSFEL TO MOD-TEMFSFEL                                 
037700        PERFORM MFS-DONT-TOUCH-FIELD-IN                                   
037800     END-IF                                                               
037900     .                                                                    
038000     EJECT                                                                
038100 GA-CHECK-MAIL-ID  SECTION.                                               
038200                                                                          
038300     IF  MID-KDARBTYP-IN NOT = ALL '+'                                    
038400     AND MID-IDPERSON-IN NOT = ALL '+'                                    
038500         IF  MID-KDARBTYP-IN(1:3) NOT = 'BER'                             
038510         OR  MID-KDARBTYP-IN(4:1) NOT = SPACES                            
038600           MOVE MFS-ALPHA-FIELD-WRONG TO MOD-KDARBTYP-IN-ATTR             
038700           MOVE NOO                   TO INDATA-SW                        
038800         END-IF                                                           
038900         IF INDATA-OK                                                     
039000           MOVE MID-KDARBTYP-IN(1:3)    TO W-KDARBTYP                     
039010           IF MID-IDPERSON-IN(3:1) = SPACES OR LOW-VALUE                  
039100              MOVE MID-IDPERSON-IN(1:2) TO W-IDPERSON                     
039101           ELSE                                                           
039110              MOVE MID-IDPERSON-IN      TO W-IDPERSON                     
039120           END-IF                                                         
039200           PERFORM IMS-GU-WDP311                                          
039300           IF SEGMENT-FOUND                                               
039400             MOVE MFS-ALPHA-FIELD-OK    TO MOD-KDARBTYP-IN-ATTR           
039500             MOVE MFS-NUM-FIELD-OK      TO MOD-IDPERSON-IN-ATTR           
039600             MOVE PERS-IDMAIL           TO IDMAIL                         
039700           ELSE                                                           
039900             MOVE MFS-NUM-FIELD-WRONG   TO MOD-IDPERSON-IN-ATTR           
040000             MOVE NOO                   TO INDATA-SW                      
040100           END-IF                                                         
040200         END-IF                                                           
040600     ELSE                                                                 
040700         MOVE MFS-ALPHA-FIELD-WRONG     TO MOD-KDARBTYP-IN-ATTR           
040800         MOVE MFS-NUM-FIELD-WRONG       TO MOD-IDPERSON-IN-ATTR           
040900         MOVE NOO                       TO INDATA-SW                      
041000     END-IF                                                               
041100     .                                                                    
041200     EJECT                                                                
041300 GB-CHECK-KDPRODSL SECTION.                                               
041400                                                                          
041500     IF MID-KDPRODSL-IN = ALL '+'                                         
041600        MOVE NOO                       TO INDATA-SW                       
041700        MOVE MFS-NUM-FIELD-WRONG       TO MOD-KDPRODSL-IN-ATTR            
041800     ELSE                                                                 
041900        IF MID-KDPRODSL-IN NUMERIC                                        
042000           MOVE MID-KDPRODSL-IN        TO W-VALID-KDPRODSL                
042100           IF GOOD-KDPRODSL                                               
042200              MOVE MFS-NUM-FIELD-OK    TO MOD-KDPRODSL-IN-ATTR            
042300              MOVE MID-KDPRODSL-IN     TO WS-BMP-KDPRODSL                 
042400           ELSE                                                           
042500              MOVE NOO                 TO INDATA-SW                       
042600              MOVE MFS-NUM-FIELD-WRONG TO MOD-KDPRODSL-IN-ATTR            
042700           END-IF                                                         
042800        END-IF                                                            
042900     END-IF                                                               
043000     .                                                                    
043100     EJECT                                                                
043200 GC-CHECK-KDSORT SECTION.                                                 
043300                                                                          
043310     MOVE +1 TO IX                                                        
043320                IX2                                                       
043330     PERFORM UNTIL IX > MAX-IX                                            
043340      PERFORM UNTIL IX2 > MAX-IX                                          
043350        IF MID-KDSORT-INCL-IN(IX) NOT = ALL '+'                           
043360        AND MID-KDSORT-EXCL-IN(IX2) NOT = ALL '+'                         
043370          IF MID-KDSORT-INCL-IN(IX) = MID-KDSORT-EXCL-IN(IX2)             
043380             MOVE NOO                   TO INDATA-SW                      
043390             MOVE MFS-ALPHA-FIELD-WRONG TO                                
043391                                     MOD-KDSORT-INCL-IN-ATTR(IX)          
043392                                     MOD-KDSORT-EXCL-IN-ATTR(IX2)         
043393          END-IF                                                          
043394        END-IF                                                            
043395        ADD +1                       TO IX2                               
043396      END-PERFORM                                                         
043397      ADD +1                         TO IX                                
043398      MOVE +1                        TO IX2                               
043399     END-PERFORM                                                          
043400                                                                          
043401     IF INDATA-OK                                                         
043410       MOVE +1 TO IX                                                      
043500       PERFORM UNTIL IX > MAX-IX                                          
043600         IF MID-KDSORT-INCL-IN(IX) NOT = ALL '+'                          
043700            MOVE MID-KDSORT-INCL-IN(IX) TO W-VALID-KDSORT                 
043800            IF GOOD-KDSORT                                                
043900               MOVE MFS-ALPHA-FIELD-OK  TO                                
044000                                      MOD-KDSORT-INCL-IN-ATTR(IX)         
044100               MOVE MID-KDSORT-INCL-IN(IX) TO                             
044200                                     WS-BMP-KDSORT-INCL(IX)               
044300            ELSE                                                          
044400               MOVE NOO                 TO INDATA-SW                      
044500               MOVE MFS-ALPHA-FIELD-WRONG TO                              
044600                                     MOD-KDSORT-INCL-IN-ATTR(IX)          
044700            END-IF                                                        
044800         END-IF                                                           
044900         ADD +1 TO IX                                                     
045000       END-PERFORM                                                        
045100                                                                          
045200       MOVE +1 TO IX                                                      
045300       PERFORM UNTIL IX > MAX-IX                                          
045400         IF MID-KDSORT-EXCL-IN(IX) NOT = ALL '+'                          
045500            MOVE MID-KDSORT-EXCL-IN(IX) TO W-VALID-KDSORT                 
045600            IF GOOD-KDSORT                                                
045700              MOVE MFS-ALPHA-FIELD-OK  TO                                 
045800                                     MOD-KDSORT-EXCL-IN-ATTR(IX)          
045900              MOVE MID-KDSORT-EXCL-IN(IX) TO                              
046000                                     WS-BMP-KDSORT-EXCL(IX)               
046100            ELSE                                                          
046200              MOVE NOO                 TO INDATA-SW                       
046300              MOVE MFS-ALPHA-FIELD-WRONG TO                               
046400                                     MOD-KDSORT-EXCL-IN-ATTR(IX)          
046500            END-IF                                                        
046600         END-IF                                                           
046700         ADD +1 TO IX                                                     
046800       END-PERFORM                                                        
046801     END-IF                                                               
046910     .                                                                    
047000     EJECT                                                                
047010 GD-CHECK-GCP-FLAG SECTION.                                               
047020                                                                          
047030     IF MID-FLAGGA-GCP-IN = ALL '+'                                       
047040        MOVE NOO                    TO INDATA-SW                          
047050        MOVE MFS-ALPHA-FIELD-WRONG  TO MOD-FLAGGA-GCP-IN-ATTR             
047060     ELSE                                                                 
047070        MOVE MID-FLAGGA-GCP-IN      TO W-VALID-GCP                        
047080        IF GOOD-GCP                                                       
047090           MOVE MFS-ALPHA-FIELD-OK  TO MOD-FLAGGA-GCP-IN-ATTR             
047091           MOVE MID-FLAGGA-GCP-IN   TO WS-BMP-GCP-FLAG                    
047092        ELSE                                                              
047093           MOVE NOO                 TO INDATA-SW                          
047094           MOVE MFS-ALPHA-FIELD-WRONG TO MOD-FLAGGA-GCP-IN-ATTR           
047095        END-IF                                                            
047096     END-IF                                                               
047097     .                                                                    
047098     EJECT                                                                
047100 GE-CHECK-IDFKNGRP SECTION.                                               
047200                                                                          
047210     MOVE YES                       TO INDATA-1-SW                        
047300     IF MID-IDFKNGRP-FOM-IN = ALL '+'                                     
047400     AND MID-IDFKNGRP-TOM-IN =  ALL '+'                                   
047500        MOVE NOO                       TO INDATA-SW                       
047510                                          INDATA-1-SW                     
047600        MOVE MFS-NUM-FIELD-WRONG       TO MOD-IDFKNGRP-FOM-IN-ATTR        
047700        MOVE MFS-NUM-FIELD-WRONG       TO MOD-IDFKNGRP-TOM-IN-ATTR        
047800     END-IF                                                               
047900                                                                          
048000     IF MID-IDFKNGRP-FOM-IN = ALL '+'                                     
048100     AND MID-IDFKNGRP-TOM-IN NOT = ALL '+'                                
048200         MOVE NOO                  TO INDATA-SW                           
048210                                      INDATA-1-SW                         
048300         MOVE MFS-NUM-FIELD-WRONG  TO MOD-IDFKNGRP-FOM-IN-ATTR            
048400         MOVE MFS-NUM-FIELD-OK     TO MOD-IDFKNGRP-TOM-IN-ATTR            
048500     END-IF                                                               
048600                                                                          
048700     IF MID-IDFKNGRP-FOM-IN NOT = ALL '+'                                 
048800     AND MID-IDFKNGRP-TOM-IN = ALL '+'                                    
048900        MOVE NOO                   TO INDATA-SW                           
048910                                      INDATA-1-SW                         
049000        MOVE MFS-NUM-FIELD-WRONG   TO MOD-IDFKNGRP-TOM-IN-ATTR            
049100        MOVE MFS-NUM-FIELD-OK      TO MOD-IDFKNGRP-FOM-IN-ATTR            
049200     END-IF                                                               
049300                                                                          
049310     IF INDATA-1-OK                                                       
049400       MOVE 0                        TO WS-CNT                            
049410       MOVE MID-IDFKNGRP-FOM-IN     TO WS-STRING                          
049510       INSPECT WS-STRING TALLYING WS-CNT FOR ALL SPACES                   
049700       IF WS-CNT > 0                                                      
049701          MOVE MFS-NUM-FIELD-WRONG  TO MOD-IDFKNGRP-FOM-IN-ATTR           
049702          MOVE NOO                  TO INDATA-SW                          
049703       ELSE                                                               
049800         MOVE MID-IDFKNGRP-FOM-IN     TO WS-IDFRIDATA                     
049900         MOVE WS-IDFRIDATA            TO DEC-IDFRIDATA                    
050000         MOVE 4                       TO DEC-KVHELTAL                     
050100         MOVE 0                       TO DEC-KVDECIMAL                    
050200         CALL WDECEDIT             USING DEC-WDECAREA                     
050300                                                                          
050400         IF DEC-KDSVAR-OK                                                 
050500           MOVE DEC-IDEDITDATA       TO WS-IDFKNGRP-FOM                   
050600                                        MOD-IDFKNGRP-FOM-IN               
050700           MOVE MFS-NUM-FIELD-OK     TO MOD-IDFKNGRP-FOM-IN-ATTR          
050800         ELSE                                                             
050900           MOVE MFS-NUM-FIELD-WRONG  TO MOD-IDFKNGRP-FOM-IN-ATTR          
051000           MOVE NOO                  TO INDATA-SW                         
051110         END-IF                                                           
051111       END-IF                                                             
051120                                                                          
051121       MOVE 0                        TO WS-CNT1                           
051122       MOVE MID-IDFKNGRP-TOM-IN     TO WS-STRING                          
051130       INSPECT WS-STRING  TALLYING WS-CNT1 FOR ALL SPACES                 
051150       IF WS-CNT1 > 0                                                     
051160          MOVE MFS-NUM-FIELD-WRONG  TO MOD-IDFKNGRP-TOM-IN-ATTR           
051170          MOVE NOO                  TO INDATA-SW                          
051171       ELSE                                                               
051700          MOVE MID-IDFKNGRP-TOM-IN     TO WS-IDFRIDATA                    
051800          MOVE WS-IDFRIDATA            TO DEC-IDFRIDATA                   
051900          MOVE 4                       TO DEC-KVHELTAL                    
052000          MOVE 0                       TO DEC-KVDECIMAL                   
052100          CALL WDECEDIT             USING DEC-WDECAREA                    
052200                                                                          
052300          IF DEC-KDSVAR-OK                                                
052400            MOVE DEC-IDEDITDATA       TO WS-IDFKNGRP-TOM                  
052500            MOVE MFS-NUM-FIELD-OK     TO MOD-IDFKNGRP-TOM-IN-ATTR         
052600          ELSE                                                            
052700            MOVE MFS-NUM-FIELD-WRONG  TO MOD-IDFKNGRP-TOM-IN-ATTR         
052800            MOVE NOO                  TO INDATA-SW                        
052910          END-IF                                                          
052920       END-IF                                                             
052930     END-IF                                                               
053100                                                                          
053200     IF INDATA-OK                                                         
053300        PERFORM GEA-CHECK-IDFKNGRP-FOM-IN                                 
053600        PERFORM GEB-CHECK-IDFKNGRP-TOM-IN                                 
054100     END-IF                                                               
054300     .                                                                    
054400     EJECT                                                                
054500 GEA-CHECK-IDFKNGRP-FOM-IN SECTION.                                       
054600                                                                          
054900     IF WS-IDFKNGRP-FOM <= WS-IDFKNGRP-TOM                                
055000        MOVE MFS-NUM-FIELD-OK   TO MOD-IDFKNGRP-FOM-IN-ATTR               
055010        MOVE WS-IDFKNGRP-FOM    TO WS-BMP-IDFKNGRP-FOM                    
055100     ELSE                                                                 
055200        MOVE MFS-NUM-FIELD-WRONG                                          
055300                                TO MOD-IDFKNGRP-FOM-IN-ATTR               
055400        MOVE NOO                TO INDATA-SW                              
055500     END-IF                                                               
055800     .                                                                    
055900     EJECT                                                                
056000 GEB-CHECK-IDFKNGRP-TOM-IN SECTION.                                       
056100                                                                          
056400     IF WS-IDFKNGRP-TOM  >= WS-IDFKNGRP-FOM                               
056500        MOVE MFS-NUM-FIELD-OK   TO MOD-IDFKNGRP-TOM-IN-ATTR               
056510        MOVE WS-IDFKNGRP-TOM    TO WS-BMP-IDFKNGRP-TOM                    
056600     ELSE                                                                 
056700        MOVE MFS-NUM-FIELD-WRONG                                          
056800                                TO MOD-IDFKNGRP-TOM-IN-ATTR               
056900        MOVE NOO                TO INDATA-SW                              
057000     END-IF                                                               
057300     .                                                                    
057400     EJECT                                                                
057500 GF-CHECK-PLANNER SECTION.                                                
057600                                                                          
057700     IF MID-IDBERED-IN = ALL '+'                                          
057800        MOVE NOO                       TO INDATA-SW                       
057900        MOVE MFS-NUM-FIELD-WRONG       TO MOD-IDBERED-IN-ATTR             
058000     ELSE                                                                 
058010        MOVE MID-IDBERED-IN            TO WS-BMP-IDBERED                  
058100        IF WS-BMP-IDBERED NUMERIC                                         
058110        AND WS-BMP-IDBERED > 0                                            
058200           MOVE MFS-NUM-FIELD-OK       TO MOD-IDBERED-IN-ATTR             
058400        ELSE                                                              
058500           MOVE NOO                    TO INDATA-SW                       
058600           MOVE MFS-NUM-FIELD-WRONG    TO MOD-IDBERED-IN-ATTR             
058610           MOVE ZERO                   TO WS-BMP-IDBERED                  
058700        END-IF                                                            
058800     END-IF                                                               
058900     .                                                                    
059000     EJECT                                                                
059110 H-UPDATE  SECTION.                                                       
059200                                                                          
059300*                                                                         
059400     MOVE '1155'   TO MSGSOP-IDTRANS                                      
059500     MOVE '1'      TO MSGSOP-KDMFSFOR                                     
059600     MOVE 'O'      TO MSGSOP-KDSOPFUNK                                    
059700* ORDER BMP                                                               
059800     MOVE 'W115S4' TO MSGSOP-IDPROCESS                                    
059900                                                                          
060000     STRING 'IDUSER(' MSG-SIGNON-USERID ') PARM('                         
060100             WS-BMP-PARM ') MAIL(' WS-IDMAIL ')'                          
060200            DELIMITED BY SIZE INTO MSGSOP-TESYMBV                         
060300                                                                          
060400     MOVE INF-BMP-START  TO MOD-TEMFSINF                                  
060500     PERFORM IMS-INSERT-ALTMSG                                            
060510     PERFORM MFS-ERASE-FIELD-IN                                           
060600     .                                                                    
060700     EJECT                                                                
060800 MFS-ERASE-FIELD-IN SECTION.                                              
060900                                                                          
061000*    --- ALLA INDATA-FÄLT                                                 
061100     MOVE MFS-ERASE-FIELD TO MOD-KDARBTYP-IN                              
061200                             MOD-KDARBTYP-IN                              
061300                             MOD-IDPERSON-IN                              
061400                             MOD-KDPRODSL-IN                              
061410                             MOD-FLAGGA-GCP-IN                            
061500                             MOD-IDFKNGRP-FOM-IN                          
061600                             MOD-IDFKNGRP-TOM-IN                          
061700                             MOD-IDBERED-IN                               
061800     PERFORM VARYING IX FROM 1 BY 1                                       
061900     UNTIL IX > MAX-IX                                                    
062000         MOVE MFS-ERASE-FIELD TO MOD-KDSORT-INCL-IN(IX)                   
062100                                 MOD-KDSORT-EXCL-IN(IX)                   
062200     END-PERFORM                                                          
062300     .                                                                    
062400     EJECT                                                                
062500 MFS-DONT-TOUCH-FIELD-IN  SECTION.                                        
062600                                                                          
062700*    --- ALLA INDATA-FÄLT                                                 
062800     MOVE MFS-DO-NOT-TOUCH-FIELD TO                                       
062900                                MOD-KDARBTYP-IN                           
063000                                MOD-IDPERSON-IN                           
063100                                MOD-KDPRODSL-IN                           
063110                                MOD-FLAGGA-GCP-IN                         
063200                                MOD-IDFKNGRP-FOM-IN                       
063300                                MOD-IDFKNGRP-TOM-IN                       
063400                                MOD-IDBERED-IN                            
063500     PERFORM VARYING IX FROM 1 BY 1                                       
063600     UNTIL IX > MAX-IX                                                    
063700         MOVE MFS-DO-NOT-TOUCH-FIELD TO                                   
063800                                MOD-KDSORT-INCL-IN(IX)                    
063900                                MOD-KDSORT-EXCL-IN(IX)                    
064000     END-PERFORM                                                          
064100     .                                                                    
064200     EJECT                                                                
064300 MFS-FORM-ATTR SECTION.                                                   
064400                                                                          
064500*    --- ALL INDATA-FIELDS                                                
064600     MOVE MFS-FORMAT-DEFAULT-ATTR TO                                      
064700                                MOD-KDARBTYP-IN-ATTR                      
064800                                MOD-IDPERSON-IN-ATTR                      
064900                                MOD-KDPRODSL-IN-ATTR                      
064910                                MOD-FLAGGA-GCP-IN-ATTR                    
065000                                MOD-IDFKNGRP-FOM-IN-ATTR                  
065100                                MOD-IDFKNGRP-TOM-IN-ATTR                  
065200                                MOD-IDBERED-IN-ATTR                       
065300     PERFORM VARYING IX FROM 1 BY 1                                       
065400     UNTIL IX > MAX-IX                                                    
065500         MOVE MFS-FORMAT-DEFAULT-ATTR TO                                  
065600                                MOD-KDSORT-INCL-IN-ATTR(IX)               
065700                                MOD-KDSORT-EXCL-IN-ATTR(IX)               
065800     END-PERFORM                                                          
065900     .                                                                    
066000     SKIP2                                                                
066100 MFS-READ-IN-AGAIN SECTION.                                               
066200                                                                          
066300*    --- ALL INDATA-FIELDS                                                
066400     MOVE MFS-ADD-READ-FIELD TO MOD-KDARBTYP-IN-ATTR                      
066500                                MOD-IDPERSON-IN-ATTR                      
066600                                MOD-KDPRODSL-IN-ATTR                      
066610                                MOD-FLAGGA-GCP-IN-ATTR                    
066700                                MOD-IDFKNGRP-FOM-IN-ATTR                  
066800                                MOD-IDFKNGRP-TOM-IN-ATTR                  
066900                                MOD-IDBERED-IN-ATTR                       
067000     PERFORM VARYING IX FROM 1 BY 1                                       
067100     UNTIL IX > MAX-IX                                                    
067200         MOVE MFS-ADD-READ-FIELD TO MOD-KDSORT-INCL-IN-ATTR(IX)           
067300                                    MOD-KDSORT-EXCL-IN-ATTR(IX)           
067400     END-PERFORM                                                          
067500                                                                          
067600     .                                                                    
067700     EJECT                                                                
067800* --- IMS SECTIONS ---                                                    
067900     SKIP3                                                                
068000 IMS-GET-MSG SECTION.                                                     
068100                                                                          
068200     MOVE '  QC' TO GOOD-STATUSCODES                                      
068300     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
068400     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
068500     PERFORM IMS-STATUSCHECK                                              
068600     .                                                                    
068700     SKIP3                                                                
068800 IMS-INSERT-MSG SECTION.                                                  
068900                                                                          
069000     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
069100     MOVE SPACE TO GOOD-STATUSCODES                                       
069200     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
069300     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
069400     PERFORM IMS-STATUSCHECK                                              
069500     .                                                                    
069600     EJECT                                                                
069700 IMS-INSERT-ALTMSG SECTION.                                               
069800                                                                          
069900     MOVE SPACES TO GOOD-STATUSCODES                                      
070000     CALL CBLTDLI USING ISRT ALT-PCB PROG-TO-PROG-SW                      
070100     MOVE ALT-STATUS-CODE TO STATUS-WS                                    
070200     PERFORM IMS-STATUSCHECK                                              
070300     .                                                                    
070400     EJECT                                                                
070500 IMS-GU-WDP311 SECTION.                                                   
070600     STRING 'WDP301  (KDARBTYP =' W-KDARBTYP-X ')'                        
070700            DELIMITED BY SIZE INTO SSA1                                   
070800     STRING 'WDP311  (IDPERSON ='  W-IDPERSON-X ')'                       
070900            DELIMITED BY SIZE INTO SSA2                                   
071000     MOVE '  GE' TO GOOD-STATUSCODES                                      
071100     CALL CBLTDLI USING GU WDP3-PCB DLI-IO-WDP311 SSA1 SSA2               
071200     MOVE WDP3-STATUS-CODE TO STATUS-WS                                   
071300     PERFORM IMS-STATUSCHECK                                              
071400     .                                                                    
071500     SKIP3                                                                
071600 IMS-STATUSCHECK SECTION.                                                 
071700                                                                          
071800     SET STATUS-IX TO 1                                                   
071900     SEARCH GOOD-STATUS                                                   
072000       AT END                                                             
072100         STRING ' INVALID STATUS CODE FROM IMS: ' STATUS-WS               
072200         DELIMITED BY SIZE INTO ERROR-TEXT                                
072300         CALL FELLOG                                                      
072400       WHEN GOOD-STATUS (STATUS-IX) = STATUS-WS                           
072500         CONTINUE                                                         
072600     END-SEARCH                                                           
072700     .                                                                    
