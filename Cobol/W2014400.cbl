000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W2014400.                                                
000300 AUTHOR.         HENRIKSSON ANDERS.                                       
000400 DATE-WRITTEN.   04/07/08.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNCTION:                                                            
000800*        SAFTEY WAREHOUSE COUNTING                                        
000900*                                                                         
001000*        THE PROGRAM UPDATES   WDP6                                       
001100*                                                                         
001200*    INDATA.                                                              
001300*        TRANSACTION: W2T144                                              
001400*        TRANSACTION: W2T144U FOR UPDATE                                  
001500*        MID:         W2I14401                                            
001600*                                                                         
001700*    OUTDATA.                                                             
001800*        MOD:         W2O14401                                            
001900                                                                          
002000     SKIP3                                                                
002100 ENVIRONMENT DIVISION.                                                    
002200                                                                          
002300 DATA DIVISION.                                                           
002400     EJECT                                                                
002500 WORKING-STORAGE SECTION.                                                 
002600 77  IDPGM                       PIC X(08)   VALUE 'W2014400'.            
002700                                                                          
002800*    --- WORK FIELDS FOR ERROR MESSAGES WHEN CALLING ABEND/FELLOG         
002900 77  ERROR-TEXT                  PIC X(80) VALUE SPACE.                   
003000                                                                          
003100 77  YES                         PIC X       VALUE 'J'.                   
003200 77  NOO                         PIC X       VALUE 'N'.                   
003300 77  WS-KVVECKOR-MINSL           PIC 9(2)V9(1).                           
003400 77  WS-KVVECKOR-MAXSL           PIC 9(2)V9(1).                           
003500 77  WS-KFAKT                    PIC 9(1)V9(2).                           
003600 77  WS-REOLAGK                  PIC 9(1)V9(2).                           
003700 77  WS-RETARGET                 PIC 9(1)V9(3).                           
003800 77  WS-KVVECKOR-MINSL-2         PIC 9(2)V9(1).                           
003900 77  WS-KVVECKOR-MINSL-3         PIC X(4).                                
004000 77  WS-KVVECKOR-MAXSL-2         PIC 9(2)V9(1).                           
004100 77  WS-KVVECKOR-MAXSL-3         PIC X(4).                                
004200 77  WS-KFAKT-2                  PIC 9(1)V9(2).                           
004300 77  WS-KFAKT-3                  PIC X(4).                                
004310 77  WS-RED-KVVECKOR-MINSL-IN    PIC Z(2)9.9 VALUE ZERO.                  
004320 77  WS-RED-KVVECKOR-MAXSL-IN    PIC Z(2)9.9 VALUE ZERO.                  
004321 77  WS-RED-RETARGET-IN          PIC Z(2)9.9 VALUE ZERO.                  
004330 77  WS-RED-RETARGET-IN2         PIC 9(2)V9 VALUE ZERO.                   
004340 77  WS-RETARGET-IN              PIC 9(3) VALUE ZERO.                     
004350 77  WS-RED-KFAKT-IN             PIC Z9.9(2) VALUE ZERO.                  
004400                                                                          
004500*    --- WORK FIELDS FOR ACTUAL KEYVALUES OF SCREEN                       
004600                                                                          
004700 77  INDATA-SW                   PIC X       VALUE 'J'.                   
004800     88  INDATA-OK                           VALUE 'J'.                   
004900     88  INDATA-WRONG                        VALUE 'N'.                   
005000                                                                          
005100 77  KEYS-SW                     PIC X       VALUE 'J'.                   
005200     88  KEYS-OK                             VALUE 'J'.                   
005300     88  KEYS-WRONG                          VALUE 'N'.                   
005400                                                                          
005500 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
005600     88  OWN-MID                             VALUE '2144'.                
005700     88  GOOD-MID                            VALUE '2144'.                
005800     88  HELP-MID                            VALUE '0551'.                
005900     EJECT                                                                
006000*    --- SUBPROGRAMS AND PARAMETER AREAS                                  
006100 01  GENERAL-SUBPROGRAMS.                                                 
006200     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
006300     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
006400     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
006500     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
006510     03  WDECEDIT                PIC X(8)    VALUE 'WDECEDIT'.            
006600     EJECT                                                                
006700*    --- PARAMETERS FOR SUBPROGRAM WMEDKONV                               
006800*01 -COPY WMEDAREA                                                        
006900     SKIP3                                                                
006910*    --- COPYTEXT TILL SUBPROGRAM WDECEDIT                                
006920*01  -COPY WDECAREA                                                       
006930     EJECT                                                                
007000 01  MESSAGE-CODES.                                                       
007100     03  ERR-CORR-HILITE-FLDS    PIC X(3)    VALUE '001'.                 
007200     03  INF-PRESS-PF11          PIC X(3)    VALUE '003'.                 
007300     03  ERR-PF11-AND-NO-DATA    PIC X(3)    VALUE '011'.                 
007400     03  INF-UPDATE-DONE         PIC X(3)    VALUE '101'.                 
007500     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
007600     EJECT                                                                
007700*    --- PARAMETERS FOR SUB PROGRAM W005INIT                              
007800*                                                                         
007900 01  FILLER                      PIC X(16)   VALUE 'WMSGINIT'.            
008000     SKIP3                                                                
008100*01 -COPY WMSGINIT                                                        
008200     EJECT                                                                
008300*    --- AREA CONTAINING DATA TO BE SAVED BETWEEN DIALOG STEPS            
008400*                                                                         
008500 01  SAVE-AREA.                                                           
008600     03  SAVE-IDTRANS           PIC X(4)    VALUE '2144'.                 
008700     EJECT                                                                
008800*    --- AREAS FOR MFS AND SCREEN MANAGEMENT                              
008900*                                                                         
009000 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
009100     SKIP3                                                                
009200*01  MID -COPY W2I14401                                                   
009300     EJECT                                                                
009400 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
009500     SKIP3                                                                
009600*01  -COPY WMSGAREA                                                       
009700     EJECT                                                                
009800     03  MOD REDEFINES MSG-AREA.                                          
009900*      05  -COPY W2O14401                                                 
010000     EJECT                                                                
010100 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
010200     SKIP3                                                                
010300*01  -COPY WMFSAREA                                                       
010400     EJECT                                                                
010500*    --- WORK-AREAS FOR IMS-SECTIONS                                      
010600*                                                                         
010700 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
010800     SKIP3                                                                
010900 01  KEYS-TO-DLI.                                                         
011000     03  W-KDPRODSL-X.                                                    
011100         05  W-KDPRODSL          PIC S9(3)   VALUE ZERO COMP-3.           
011200     03  W-KDPRISKL-X.                                                    
011300         05  W-KDPRISKL          PIC X       VALUE SPACE.                 
011400     03  W-KDFREKKL-X.                                                    
011500         05  W-KDFREKKL          PIC X       VALUE SPACE.                 
011600     SKIP2                                                                
011700*    --- STATUS-KOD FRÅN IMS                                              
011800 01  STATUS-WS                   PIC XX.                                  
011900     88  SEGMENT-FOUND                       VALUE '  '.                  
012000     88  SEGMENT-FOUND-EXISTS                VALUE 'II'.                  
012100     88  SEGMENT-MISSING                     VALUE 'GE'.                  
012200     SKIP2                                                                
012300 01  GOOD-STATUSCODES.                                                    
012400     03  GOOD-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
012500     SKIP3                                                                
012600 01  SSA1                        PIC X(64).                               
012700 01  SSA2                        PIC X(64).                               
012800     EJECT                                                                
012900*    --- IMS FUNCTION CODES                                               
013000*01  -COPY W0003                                                          
013100     EJECT                                                                
013200*    ---  DLI INPUT-OUTPUT AREA                                           
013300                                                                          
013400 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDP601'.                      
013500 01  DLI-IO-WDP601.                                                       
013600*    03  -COPY WDP601                                                     
013700     EJECT                                                                
013800 LINKAGE SECTION.                                                         
013900*01  -COPY W0009   -PRE MSG-                                              
014000*01  -COPY W0008   -PRE WDP7-                                             
014100     05  FILLER                  PIC X.                                   
014200*01  -COPY W0008   -PRE WDP6-                                             
014300     05  FILLER                  PIC X.                                   
014400     EJECT                                                                
014500                                                                          
014600 PROCEDURE DIVISION  USING MSG-PCB WDP7-PCB WDP6-PCB.                     
014700 MAIN SECTION.                                                            
014800     ENTRY 'DLITCBL' USING MSG-PCB WDP7-PCB WDP6-PCB.                     
014900                                                                          
015000     PERFORM IMS-GET-MSG                                                  
015100     IF SEGMENT-FOUND                                                     
015200       PERFORM A-INIT                                                     
015300       PERFORM B-CHECK-KEYS                                               
015400       IF KEYS-OK                                                         
015500         IF MFS-UPDATE                                                    
015600           PERFORM G-CHECK-INPUT                                          
015700           IF INDATA-OK                                                   
015800             PERFORM H-UPDATE                                             
015900           END-IF                                                         
016000         ELSE                                                             
016100           IF MFS-FIRST                                                   
016200             PERFORM C-FIRST-PAGE                                         
016300           ELSE                                                           
016400             PERFORM E-SAME-PAGE                                          
016500           END-IF                                                         
016600         END-IF                                                           
016700         PERFORM F-READ-SHOW-INFO                                         
016800       END-IF                                                             
016900       COMPUTE MSG-KVLL = LENGTH OF MOD-W2O14401 + 4                      
017000       PERFORM IMS-INSERT-MSG                                             
017100     END-IF                                                               
017200                                                                          
017300     MOVE ZERO TO RETURN-CODE                                             
017400     GOBACK                                                               
017500     .                                                                    
017600     EJECT                                                                
017700                                                                          
017800 A-INIT SECTION.                                                          
017900     IF MSG-DOUBLE-TRANSACTIONS                                           
018000       MOVE MSG-INDATA-MINUS-2-TRANSACT   TO MID-W2I14401                 
018100       MOVE MSG-IDTRANS-2  TO MFS-IDTRANS                                 
018200       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
018300     ELSE                                                                 
018400       MOVE MSG-INDATA-MINUS-1-TRANSACT  TO MID-W2I14401                  
018500       MOVE MSG-IDTRANS-1  TO MFS-IDTRANS                                 
018600       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
018700     END-IF                                                               
018800                                                                          
018900     MOVE MSG-KDTRTYP TO MFS-KDTRTYP                                      
019000     MOVE MSG-IDPFK   TO MFS-IDPFK                                        
019100     MOVE MFS-IDTRANS TO W-IDTRANS                                        
019200                                                                          
019300     MOVE LOW-VALUE  TO MSG-AREA                                          
019400     MOVE 'W2O14401' TO MFS-IDMOD                                         
019500     MOVE '2144'     TO MOD-IDTRANS                                       
019600     MOVE MFS-ERASE-FIELD TO MOD-TEMFSFEL MOD-TEMFSINF                    
019700                                                                          
019800     IF OWN-MID OR HELP-MID                                               
019900       CONTINUE                                                           
020000     ELSE                                                                 
020100       MOVE SPACE TO MFS-KDTRTYP                                          
020200       MOVE '7' TO MFS-IDPFK                                              
020300     END-IF                                                               
020400     .                                                                    
020500     EJECT                                                                
020600                                                                          
020700 B-CHECK-KEYS SECTION.                                                    
020800     MOVE ALL '+'           TO MSGI-WMSGINIT                              
020900     MOVE '001'             TO MSGI-KDCALL                                
021000     MOVE MSG-LTERM-NAME    TO MSGI-IDLTERM-USER                          
021100     MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                                
021200     MOVE '2144'            TO MSGI-IDTRANS                               
021300     IF GOOD-MID                                                          
021700       IF MID-KDPRODSL-IN NOT = ALL '+'                                   
021800         MOVE MID-KDPRODSL-IN     TO MID-KDPRODSL-UT                      
021900       END-IF                                                             
022000       IF MID-KDPRISKL-IN NOT = ALL '+'                                   
022100         MOVE MID-KDPRISKL-IN     TO MID-KDPRISKL-UT                      
022200       END-IF                                                             
022300       IF MID-KDFREKKL-IN NOT = ALL '+'                                   
022400         MOVE MID-KDFREKKL-IN     TO MID-KDFREKKL-UT                      
022500       END-IF                                                             
022600     END-IF                                                               
022700     CALL W005INIT USING MSGI-WMSGINIT WDP7-PCB                           
022800     MOVE MSGI-SPAR-AREA TO SAVE-AREA                                     
022900                                                                          
023000*    - LANGUAGE TO BE USED BY MEDKONV                                     
023100     MOVE MSGI-IDLAND-SPR TO MED-IDSKYLT                                  
023200                                                                          
023300     MOVE YES TO KEYS-SW                                                  
023400                                                                          
023500                                                                          
023600*    -- CHECK OF KDPRODSL                                                 
023700     MOVE MFS-ERASE-FIELD TO MOD-KDPRODSL-IN                              
023800                                                                          
023900     IF  MID-KDPRODSL-IN NOT = ALL '+'                                    
024000     AND MID-KDPRISKL-IN NOT = ALL '+'                                    
024100     AND MID-KDFREKKL-IN NOT = ALL '+'                                    
024200       MOVE '7'         TO MFS-IDPFK                                      
024300       MOVE SPACE       TO MFS-KDTRTYP                                    
024400     END-IF                                                               
026100     IF MID-KDPRODSL-UT NUMERIC                                           
026200       MOVE MID-KDPRODSL-UT TO W-KDPRODSL                                 
026300     ELSE                                                                 
026400       MOVE NOO TO KEYS-SW                                                
026500     END-IF                                                               
026600     IF MID-KDPRISKL-UT NOT = ALL '+'                                     
026700       MOVE MID-KDPRISKL-UT TO W-KDPRISKL                                 
026800     ELSE                                                                 
026900       MOVE NOO TO KEYS-SW                                                
027000     END-IF                                                               
027100     IF MID-KDFREKKL-UT NOT = ALL '+'                                     
027200       MOVE MID-KDFREKKL-UT TO W-KDFREKKL                                 
027300     ELSE                                                                 
027400       MOVE NOO TO KEYS-SW                                                
027500     END-IF                                                               
027600                                                                          
027700     IF GOOD-MID AND KEYS-OK                                              
028100       MOVE MID-KDPRODSL-UT      TO MOD-KDPRODSL-UT                       
028200       MOVE MID-KDPRISKL-UT      TO MOD-KDPRISKL-UT                       
028300       MOVE MID-KDFREKKL-UT      TO MOD-KDFREKKL-UT                       
028400     ELSE                                                                 
028500       MOVE MFS-ERASE-FIELD TO MOD-KDPRODSL-UT                            
028600       MOVE MFS-ERASE-FIELD TO MOD-KDPRISKL-UT                            
028700       MOVE MFS-ERASE-FIELD TO MOD-KDFREKKL-UT                            
028800     END-IF                                                               
028900                                                                          
029000     IF KEYS-WRONG                                                        
029100       MOVE ERR-WRONG-KEY TO MED-IDMFSFEL                                 
029200       CALL WMEDKONV USING MED-WMEDAREA                                   
029300       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
029400       PERFORM MFS-ERASE-FIELD-IN                                         
029500       PERFORM MFS-ERASE-FIELD-OUT                                        
029600     END-IF                                                               
029700     .                                                                    
029800     EJECT                                                                
029900                                                                          
030000 C-FIRST-PAGE SECTION.                                                    
030100     PERFORM MFS-ERASE-FIELD-IN                                           
030200     .                                                                    
030300     EJECT                                                                
030400                                                                          
030500 E-SAME-PAGE SECTION.                                                     
030600     IF OWN-MID OR HELP-MID                                               
030700       IF MID-W2I14401 = ALL '+'                                          
030800         PERFORM MFS-ERASE-FIELD-IN                                       
030900       ELSE                                                               
031000         MOVE INF-PRESS-PF11 TO MED-IDMFSINF                              
031100         CALL WMEDKONV USING MED-WMEDAREA                                 
031200         MOVE MED-MFSINF TO MOD-TEMFSINF                                  
031300       END-IF                                                             
031400     ELSE                                                                 
031500       PERFORM MFS-ERASE-FIELD-IN                                         
031600     END-IF                                                               
031700     .                                                                    
031800     EJECT                                                                
031900                                                                          
032000 F-READ-SHOW-INFO SECTION.                                                
032100     PERFORM FA-READ-BASICDATA                                            
032200                                                                          
032300     IF SEGMENT-MISSING                                                   
032400       MOVE ERR-WRONG-KEY TO MED-IDMFSFEL                                 
032500       CALL WMEDKONV USING MED-WMEDAREA                                   
032600       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
032700       PERFORM MFS-ERASE-FIELD-OUT                                        
032800     ELSE                                                                 
032900       MOVE SLB-KVVECKOR-MINSL       TO WS-KVVECKOR-MINSL-2               
033000       MOVE WS-KVVECKOR-MINSL-2(1:2) TO WS-KVVECKOR-MINSL-3(1:2)          
033100       MOVE '.'                      TO WS-KVVECKOR-MINSL-3(3:1)          
033200       MOVE WS-KVVECKOR-MINSL-2(3:1) TO WS-KVVECKOR-MINSL-3(4:1)          
033300       MOVE WS-KVVECKOR-MINSL-3      TO MOD-KVVECKOR-MINSL-UT             
033400       MOVE SLB-KVVECKOR-MAXSL       TO WS-KVVECKOR-MAXSL-2               
033500       MOVE WS-KVVECKOR-MAXSL-2(1:2) TO WS-KVVECKOR-MAXSL-3(1:2)          
033600       MOVE '.'                      TO WS-KVVECKOR-MAXSL-3(3:1)          
033700       MOVE WS-KVVECKOR-MAXSL-2(3:1) TO WS-KVVECKOR-MAXSL-3(4:1)          
033800       MOVE WS-KVVECKOR-MAXSL-3      TO MOD-KVVECKOR-MAXSL-UT             
033900       MOVE SLB-REOLAGK              TO WS-REOLAGK                        
034000       MOVE WS-REOLAGK(2:2)          TO MOD-REOLAGK-UT(1:2)               
034100       MOVE SLB-RETARGET             TO WS-RETARGET                       
034200       MOVE WS-RETARGET(2:2)         TO MOD-RETARGET-UT(1:2)              
034300       MOVE '.'                      TO MOD-RETARGET-UT(3:1)              
034400       MOVE WS-RETARGET(4:1)         TO MOD-RETARGET-UT(4:1)              
034500       MOVE SLB-KFAKT                TO WS-KFAKT-2                        
034600       MOVE WS-KFAKT-2(1:1)          TO WS-KFAKT-3(1:1)                   
034700       MOVE '.'                      TO WS-KFAKT-3(2:1)                   
034800       MOVE WS-KFAKT-2(2:2)          TO WS-KFAKT-3(3:2)                   
034900       MOVE WS-KFAKT-3               TO MOD-KFAKT-UT                      
035000     END-IF                                                               
035100     .                                                                    
035200     EJECT                                                                
035300                                                                          
035400 FA-READ-BASICDATA SECTION.                                               
035500     PERFORM IMS-GET-WDP601                                               
035600     .                                                                    
035700     EJECT                                                                
035800                                                                          
035900 G-CHECK-INPUT SECTION.                                                   
036000     MOVE YES  TO INDATA-SW                                               
036100     IF MID-W2I14401 = ALL '+'                                            
036200       MOVE ERR-PF11-AND-NO-DATA TO MED-IDMFSFEL                          
036300       CALL WMEDKONV USING MED-WMEDAREA                                   
036400       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
036500       PERFORM MFS-DONT-TOUCH-FIELD-IN                                    
036600       PERFORM MFS-DONT-TOUCH-FIELD-OUT                                   
036700       MOVE NOO TO INDATA-SW                                              
036800     ELSE                                                                 
036900       IF INDATA-WRONG                                                    
037000         MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL                        
037100         CALL WMEDKONV USING MED-WMEDAREA                                 
037200         MOVE MED-MFSFEL TO MOD-TEMFSFEL                                  
037300         PERFORM MFS-DONT-TOUCH-FIELD-OUT                                 
037400         PERFORM MFS-DONT-TOUCH-FIELD-IN                                  
037500       ELSE                                                               
037600         PERFORM IMS-GET-WDP601                                           
037700                                                                          
037800         IF SEGMENT-MISSING                                               
037900           MOVE ERR-WRONG-KEY TO MED-IDMFSFEL                             
038000           CALL WMEDKONV USING MED-WMEDAREA                               
038100           MOVE MED-MFSFEL TO MOD-TEMFSFEL                                
038200           PERFORM MFS-ERASE-FIELD-IN                                     
038300           PERFORM MFS-ERASE-FIELD-OUT                                    
038400           MOVE NOO TO INDATA-SW                                          
038500         ELSE                                                             
038600           MOVE YES TO INDATA-SW                                          
038700         END-IF                                                           
038800       END-IF                                                             
038900     END-IF                                                               
039000     .                                                                    
039100     EJECT                                                                
039200                                                                          
039300 H-UPDATE SECTION.                                                        
039400     IF MID-KVVECKOR-MINSL-IN NOT = ALL '+'                               
041700*      KONVERTERA IFRÅN FRITT FORMAT TILL 2 + 1 DECIMAL                   
041710       MOVE MID-KVVECKOR-MINSL-IN    TO DEC-IDFRIDATA                     
041720                                        MOD-KVVECKOR-MINSL-UT             
041730       MOVE 2                        TO DEC-KVHELTAL                      
041740       MOVE 1                        TO DEC-KVDECIMAL                     
041750       CALL WDECEDIT USING DEC-WDECAREA                                   
041760       IF DEC-KDSVAR-OK                                                   
041770         MOVE MFS-ADD-LAES-IN-FAELT                                       
041780                           TO MOD-KVVECKOR-MINSL-IN-ATTR                  
041790         MOVE DEC-IDEDITDATA                                              
041791                           TO WS-RED-KVVECKOR-MINSL-IN                    
041793         MOVE WS-RED-KVVECKOR-MINSL-IN                                    
041794                           TO MOD-KVVECKOR-MINSL-UT                       
041795                              SLB-KVVECKOR-MINSL                          
041796       ELSE                                                               
041797         MOVE 'WRONG MINGRÄNS'    TO MOD-TEMFSFEL                         
041798         MOVE MFS-ADD-LAES-IN-FAELT-HI                                    
041799                           TO MOD-KVVECKOR-MINSL-IN-ATTR                  
041800         MOVE NOO          TO INDATA-SW                                   
041801         MOVE MID-KVVECKOR-MINSL-IN TO MOD-KVVECKOR-MINSL-UT              
041802       END-IF                                                             
042000     END-IF                                                               
042010     IF MID-KVVECKOR-MAXSL-IN NOT = ALL '+'                               
042020*      KONVERTERA IFRÅN FRITT FORMAT TILL 2 + 1 DECIMAL                   
042030       MOVE MID-KVVECKOR-MAXSL-IN    TO DEC-IDFRIDATA                     
042040                                        MOD-KVVECKOR-MAXSL-UT             
042050       MOVE 2                        TO DEC-KVHELTAL                      
042060       MOVE 1                        TO DEC-KVDECIMAL                     
042070       CALL WDECEDIT USING DEC-WDECAREA                                   
042080       IF DEC-KDSVAR-OK                                                   
042090         MOVE MFS-ADD-LAES-IN-FAELT                                       
042091                           TO MOD-KVVECKOR-MAXSL-IN-ATTR                  
042092         MOVE DEC-IDEDITDATA                                              
042093                           TO WS-RED-KVVECKOR-MAXSL-IN                    
042094         MOVE WS-RED-KVVECKOR-MAXSL-IN                                    
042095                           TO MOD-KVVECKOR-MAXSL-UT                       
042096                              SLB-KVVECKOR-MAXSL                          
042097       ELSE                                                               
042098         MOVE 'WRONG MAXGRÄNS'    TO MOD-TEMFSFEL                         
042099         MOVE MFS-ADD-LAES-IN-FAELT-HI                                    
042100                           TO MOD-KVVECKOR-MAXSL-IN-ATTR                  
042101         MOVE NOO          TO INDATA-SW                                   
042102         MOVE MID-KVVECKOR-MAXSL-IN TO MOD-KVVECKOR-MAXSL-UT              
042103       END-IF                                                             
042104     END-IF                                                               
042105     IF MID-RETARGET-IN NOT = ALL '+'                                     
042113*      KONVERTERA IFRÅN FRITT FORMAT TILL 2 + 1 DECIMAL                   
042115       MOVE MID-RETARGET-IN          TO DEC-IDFRIDATA                     
042116                                        MOD-RETARGET-UT                   
042117       MOVE 2                        TO DEC-KVHELTAL                      
042118       MOVE 1                        TO DEC-KVDECIMAL                     
042119       CALL WDECEDIT USING DEC-WDECAREA                                   
042120       IF DEC-KDSVAR-OK                                                   
042121         MOVE MFS-ADD-LAES-IN-FAELT                                       
042122                           TO MOD-RETARGET-IN-ATTR                        
042123         MOVE DEC-IDEDITDATA                                              
042124                           TO WS-RED-RETARGET-IN                          
042125                              WS-RED-RETARGET-IN2                         
042126         MOVE WS-RED-RETARGET-IN                                          
042127                           TO MOD-RETARGET-UT                             
042129         COMPUTE SLB-RETARGET ROUNDED = WS-RED-RETARGET-IN2 / 100         
042137       ELSE                                                               
042138         MOVE 'WRONG SERVICEGRAD'    TO MOD-TEMFSFEL                      
042139         MOVE MFS-ADD-LAES-IN-FAELT-HI                                    
042140                           TO MOD-RETARGET-IN-ATTR                        
042141         MOVE NOO          TO INDATA-SW                                   
042142         MOVE MID-RETARGET-IN TO MOD-RETARGET-UT                          
042143       END-IF                                                             
042150     END-IF                                                               
044700     IF MID-REOLAGK-IN NOT = ALL '+'                                      
044800       MOVE MID-REOLAGK-IN        TO WS-REOLAGK(2:2)                      
044900       MOVE WS-REOLAGK            TO SLB-REOLAGK                          
045000       MOVE MID-REOLAGK-IN        TO MOD-REOLAGK-UT                       
045100     END-IF                                                               
045200     IF MID-KFAKT-IN NOT = ALL '+'                                        
045300*      KONVERTERA IFRÅN FRITT FORMAT TILL 1 + 2 DECIMAL                   
045400       MOVE MID-KFAKT-IN             TO DEC-IDFRIDATA                     
045500                                        MOD-KFAKT-UT                      
045600       MOVE 1                        TO DEC-KVHELTAL                      
045700       MOVE 2                        TO DEC-KVDECIMAL                     
045800       CALL WDECEDIT USING DEC-WDECAREA                                   
045810       IF DEC-KDSVAR-OK                                                   
045820         MOVE MFS-ADD-LAES-IN-FAELT                                       
045830                           TO MOD-KFAKT-IN-ATTR                           
045840         MOVE DEC-IDEDITDATA                                              
045850                           TO WS-RED-KFAKT-IN                             
045860         MOVE WS-RED-KFAKT-IN                                             
045870                           TO MOD-KFAKT-UT                                
045880                              SLB-KFAKT                                   
045890       ELSE                                                               
045891         MOVE 'WRONG K-FAKTOR'    TO MOD-TEMFSFEL                         
045892         MOVE MFS-ADD-LAES-IN-FAELT-HI                                    
045893                           TO MOD-KFAKT-IN-ATTR                           
045894         MOVE NOO          TO INDATA-SW                                   
045895         MOVE MID-KFAKT-IN TO MOD-KFAKT-UT                                
045896       END-IF                                                             
045897     END-IF                                                               
048300     MOVE MFS-ADD-HILIGHT-FIELD TO MOD-KVVECKOR-MINSL-IN-ATTR             
048400                                   MOD-KVVECKOR-MAXSL-IN-ATTR             
048500                                   MOD-REOLAGK-IN-ATTR                    
048600                                   MOD-RETARGET-IN-ATTR                   
048700                                   MOD-KFAKT-IN-ATTR                      
048800                                                                          
048810     IF INDATA-OK                                                         
048900       PERFORM IMS-REPL-WDP601                                            
049000                                                                          
049100       MOVE INF-UPDATE-DONE TO MED-IDMFSINF                               
049200       CALL WMEDKONV USING MED-WMEDAREA                                   
049300       MOVE MED-MFSINF TO MOD-TEMFSINF                                    
049400       PERFORM MFS-FORM-ATTR                                              
049500       PERFORM MFS-ERASE-FIELD-IN                                         
049510     END-IF                                                               
049600     .                                                                    
049700     EJECT                                                                
049800                                                                          
049900 MFS-ERASE-FIELD-OUT SECTION.                                             
050000*    --- ALLA UTDATA-FÄLT                                                 
050100     MOVE MFS-ERASE-FIELD TO MOD-KVVECKOR-MINSL-UT                        
050200                             MOD-KVVECKOR-MAXSL-UT                        
050300                             MOD-REOLAGK-UT                               
050400                             MOD-RETARGET-UT                              
050500                             MOD-KFAKT-UT                                 
050600                             MOD-KDPRODSL-UT                              
050700                             MOD-KDPRISKL-UT                              
050800                             MOD-KDFREKKL-UT                              
050900     .                                                                    
051000     SKIP3                                                                
051100                                                                          
051200 MFS-ERASE-FIELD-IN SECTION.                                              
051300*    --- ALLA INDATA-FÄLT                                                 
051400     MOVE MFS-ERASE-FIELD TO MOD-KVVECKOR-MINSL-IN                        
051500                             MOD-KVVECKOR-MAXSL-IN                        
051600                             MOD-REOLAGK-IN                               
051700                             MOD-RETARGET-IN                              
051800                             MOD-KFAKT-IN                                 
051900                             MOD-KDPRODSL-IN                              
052000                             MOD-KDPRISKL-IN                              
052100                             MOD-KDFREKKL-IN                              
052200     .                                                                    
052300     EJECT                                                                
052400                                                                          
052500 MFS-DONT-TOUCH-FIELD-OUT  SECTION.                                       
052600*    --- ALLA UTDATA-FÄLT                                                 
052700     MOVE MFS-DO-NOT-TOUCH-FIELD TO MOD-KVVECKOR-MINSL-UT                 
052800                                    MOD-KVVECKOR-MAXSL-UT                 
052900                                    MOD-REOLAGK-UT                        
053000                                    MOD-RETARGET-UT                       
053100                                    MOD-KFAKT-UT                          
053200                                    MOD-KDPRODSL-UT                       
053300                                    MOD-KDPRISKL-UT                       
053400                                    MOD-KDFREKKL-UT                       
053500     .                                                                    
053600     SKIP3                                                                
053700                                                                          
053800 MFS-DONT-TOUCH-FIELD-IN  SECTION.                                        
053900*    --- ALLA INDATA-FÄLT                                                 
054000     MOVE MFS-DO-NOT-TOUCH-FIELD TO MOD-KVVECKOR-MINSL-IN                 
054100                                    MOD-KVVECKOR-MAXSL-IN                 
054200                                    MOD-REOLAGK-IN                        
054300                                    MOD-RETARGET-IN                       
054400                                    MOD-KFAKT-IN                          
054500                                    MOD-KDPRODSL-IN                       
054600                                    MOD-KDPRISKL-IN                       
054700                                    MOD-KDFREKKL-IN                       
054800     .                                                                    
054900     EJECT                                                                
055000                                                                          
055100 MFS-FORM-ATTR SECTION.                                                   
055200*    --- ALL INDATA-FIELDS                                                
055300     MOVE MFS-FORMAT-DEFAULT-ATTR TO MOD-KVVECKOR-MINSL-IN-ATTR           
055400                                     MOD-KVVECKOR-MAXSL-IN-ATTR           
055500                                     MOD-REOLAGK-IN-ATTR                  
055600                                     MOD-RETARGET-IN-ATTR                 
055700                                     MOD-KFAKT-IN-ATTR                    
055800     .                                                                    
055900     SKIP2                                                                
056000                                                                          
056100 IMS-GET-MSG SECTION.                                                     
056200     MOVE '  QC' TO GOOD-STATUSCODES                                      
056300     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
056400     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
056500     PERFORM IMS-STATUSCHECK                                              
056600     .                                                                    
056700     SKIP3                                                                
056800                                                                          
056900 IMS-INSERT-MSG SECTION.                                                  
057000     IF MSGI-IDLAND-SPR = 'SE'                                            
057100       MOVE '0' TO MFS-KDHUVOMR                                           
057200     END-IF                                                               
057300     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
057400     MOVE SPACE TO GOOD-STATUSCODES                                       
057500     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
057600     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
057700     PERFORM IMS-STATUSCHECK                                              
057800     .                                                                    
057900     EJECT                                                                
058000                                                                          
058100 IMS-GET-WDP601 SECTION.                                                  
058200     STRING 'WDP601  (KDPRODSL =' W-KDPRODSL-X                            
058300                    '&KDPRISKL =' W-KDPRISKL-X                            
058400                    '&KDFREKKL =' W-KDFREKKL-X ')'                        
058500          DELIMITED BY SIZE INTO SSA1                                     
058600     MOVE '  GE' TO GOOD-STATUSCODES                                      
058700     CALL CBLTDLI USING GHU WDP6-PCB DLI-IO-WDP601 SSA1                   
058800     MOVE WDP6-STATUS-CODE TO STATUS-WS                                   
058900     PERFORM IMS-STATUSCHECK                                              
059000     .                                                                    
059100     SKIP3                                                                
059200                                                                          
059300 IMS-REPL-WDP601 SECTION.                                                 
059400     MOVE '  ' TO GOOD-STATUSCODES                                        
059500     CALL CBLTDLI USING REPL WDP6-PCB DLI-IO-WDP601                       
059600     MOVE WDP6-STATUS-CODE TO STATUS-WS                                   
059700     PERFORM IMS-STATUSCHECK                                              
059800     .                                                                    
059900     EJECT                                                                
060000                                                                          
060100 IMS-STATUSCHECK SECTION.                                                 
060200     SET STATUS-IX TO 1                                                   
060300     SEARCH GOOD-STATUS                                                   
060400       AT END                                                             
060500         STRING ' INVALID STATUS CODE FROM IMS: ' STATUS-WS               
060600         DELIMITED BY SIZE INTO ERROR-TEXT                                
060700         CALL FELLOG                                                      
060800       WHEN GOOD-STATUS (STATUS-IX) = STATUS-WS                           
060900         CONTINUE                                                         
061000     END-SEARCH                                                           
061100     .                                                                    
