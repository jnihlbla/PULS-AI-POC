000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W6031100.                                                
000300 AUTHOR.         MARTIEN HOMPES.                                          
000400 DATE-WRITTEN.   97/02/06.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNCTION:                                                            
000800*        MAINTANCE FREQUENCY TABEL                                        
000900*                                                                         
001000*        THE PROGRAM UPDATES   WL6313 (WDGX)                              
001100*                                                                         
001200*    INDATA.                                                              
001300*        TRANSACTION: W6T311                                              
001400*        MID:         W6I31101                                            
001500*                                                                         
001600*    OUTDATA.                                                             
001700*        MOD:         W6O31101                                            
001800                                                                          
001900     SKIP3                                                                
002000 ENVIRONMENT DIVISION.                                                    
002100     EJECT                                                                
002200 DATA DIVISION.                                                           
002300 WORKING-STORAGE SECTION.                                                 
002400                                                                          
002500*    -- CHECKED BY WY2000                                                 
002600 77  IDPGM                       PIC X(08)   VALUE 'W6031100'.            
002700                                                                          
002800*    --- WORK FIELDS FOR ERROR MESSAGES WHEN CALLING ABEND/FELLOG         
002900 77  ERROR-TEXT                  PIC X(80) VALUE SPACE.                   
003000                                                                          
003100 77  YES                         PIC X       VALUE 'Y'.                   
003200 77  NOO                         PIC X       VALUE 'N'.                   
003300                                                                          
003400 77  NEW                         PIC X        VALUE 'N'.                  
003500 77  DEL                         PIC X        VALUE 'D'.                  
003600 77  CHG                         PIC X        VALUE 'C'.                  
003700                                                                          
003800*    --- INDEX FOR SCROLL LINES                                           
003900 77  INDX                        PIC S9(4)  VALUE +0    COMP SYNC.        
004000 77  MAX-INDX                    PIC S9(4)  VALUE +13   COMP SYNC.        
004100*    --- WORK FIELDS FOR ACTUAL KEYVALUES OF SCREEN                       
004200                                                                          
004300 77  INDATA-SW                   PIC X       VALUE 'Y'.                   
004400     88  INDATA-OK                           VALUE 'Y'.                   
004500     88  INDATA-WRONG                        VALUE 'N'.                   
004600                                                                          
004700 77  KEYS-SW                     PIC X       VALUE 'J'.                   
004800     88  KEYS-OK                             VALUE 'Y'.                   
004900     88  KEYS-WRONG                          VALUE 'N'.                   
005000                                                                          
005100 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
005200     88  OWN-MID                             VALUE '6311'.                
005300     88  GOOD-MID                            VALUE '6311'.                
005400     88  HELP-MID                            VALUE '0551'.                
005500                                                                          
005600 77  WS-KDFREQ-IN                PIC 9(2)   VALUE ZERO.                   
005700 77  WS-KDFREQ                   PIC 9(2)   VALUE ZERO.                   
005800 77  WS-TEFREQ                   PIC X(10)  VALUE SPACE.                  
005900 77  WS-KVPB-FOM                 PIC 9(5)V9(1).                           
006000 77  WS-KVPB-TOM                 PIC 9(5)V9(2).                           
006100 77  WS-RELOCFAC                 PIC 9(1)V9(2).                           
006200                                                                          
006300       EJECT                                                              
006400*    --- SUBPROGRAMS AND PARAMETER AREAS                                  
006500 01  GENERAL-SUBPROGRAMS.                                                 
006600     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
006700     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
006800     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
006900     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
007000     03  WDECEDIT                PIC X(8)    VALUE 'WDECEDIT'.            
007100     EJECT                                                                
007200*    --- PARAMETERS FOR SUBPROGRAM WMEDKONV                               
007300*01 -COPY WMEDAREA                                                        
007400     SKIP3                                                                
007500 01  MESSAGE-CODES.                                                       
007600     03  ERR-CORR-HILITE-FLDS    PIC X(3)    VALUE '001'.                 
007700     03  INF-PRESS-PF11          PIC X(3)    VALUE '003'.                 
007800     03  INF-FIRST-PAGE          PIC X(3)    VALUE '006'.                 
007900     03  ERR-PF11-AND-NO-DATA    PIC X(3)    VALUE '011'.                 
008000     03  ITEMS-MISSING           PIC X(3)    VALUE '029'.                 
008100     03  INF-UPDATE-DONE         PIC X(3)    VALUE '101'.                 
008200     03  INF-MORE-INFO-EXISTS    PIC X(3)    VALUE '105'.                 
008300     03  INF-LAST-PAGE-SHOWN     PIC X(3)    VALUE '115'.                 
008400     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
008500     EJECT                                                                
008600*    --- PARAMETERS FOR SUB PROGRAM W005INIT                              
008700*                                                                         
008800 01  FILLER                      PIC X(16)   VALUE 'WMSGINIT'.            
008900     SKIP3                                                                
009000*01 -COPY WMSGINIT                                                        
009100*    --- PARAMETERS FOR SUB PROGRAM WDECAREA                              
009200*                                                                         
009300 01  FILLER                      PIC X(16)  VALUE 'WDECAREA '.            
009400*01    -COPY WDECAREA                                                     
009500     SKIP3                                                                
009600     EJECT                                                                
009700*    --- AREA CONTAINING DATA TO BE SAVED BETWEEN DIALOG STEPS            
009800*                                                                         
009900 01  SAVE-AREA.                                                           
010000     03  SAVE-IDTRANS        PIC X(4)    VALUE  SPACE.                    
010100     03  SAVE-KDFREQ-ENTER   PIC X(2).                                    
010200     03  SAVE-KDFREQ-NEXT    PIC X(2).                                    
010300     EJECT                                                                
010400*    --- AREAS FOR MFS AND SCREEN MANAGEMENT                              
010500*                                                                         
010600 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
010700     SKIP3                                                                
010800*01  MID -COPY W6I31101                                                   
010900     EJECT                                                                
011000 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
011100     SKIP3                                                                
011200*01  -COPY WMSGAREA                                                       
011300     EJECT                                                                
011400     03  MOD REDEFINES MSG-AREA.                                          
011500*      05  -COPY W6O31101                                                 
011600     EJECT                                                                
011700 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
011800     SKIP3                                                                
011900*01  -COPY WMFSAREA                                                       
012000     EJECT                                                                
012100*    --- WORK-AREAS FOR IMS-SECTIONS                                      
012200*                                                                         
012300     EJECT                                                                
012400 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
012500     SKIP3                                                                
012600 01  KEYS-TO-DLI.                                                         
012700*    --- VALUE OF SCROLLING KEY FOR FIRST LINE ON THE SCREEN              
012800     03  W-KDFREQ-MIN-X.                                                  
012900          05  W-KDFREQ-MIN       PIC X(2).                                
013000                                                                          
013100     03  W-KDFREQ-MAX-X.                                                  
013200          05  W-KDFREQ-MAX       PIC X(2)    VALUE HIGH-VALUE.            
013300                                                                          
013400     03  W-WDGXKEY-6313-X.                                                
013500          05 W-6313-IDHTYP       PIC X(4)    VALUE '6313'.                
013600          05 W-6313-IDDC         PIC X(2)    VALUE SPACE.                 
013700          05 W-6313-LOWVALUE     PIC X(24)   VALUE LOW-VALUE.             
013800                                                                          
013900     03  W-WDGXKEY-6314-X.                                                
014000         05  W-KDFREQ            PIC X(2)    VALUE SPACE.                 
014100                                                                          
014200     03  W-IDDC-B6-X.                                                     
014300         05  W-IDDC-B6       PIC X(2)   VALUE SPACE.                      
014400     SKIP2                                                                
014500*    --- STATUS-KOD FRÅN IMS                                              
014600 01  STATUS-WS                   PIC XX.                                  
014700     88  SEGMENT-FOUND                       VALUE '  '.                  
014800     88  SEGMENT-FOUND-EXISTS                VALUE 'II'.                  
014900     88  SEGMENT-MISSING                     VALUE 'GE'.                  
015000     88  END-OF-DATABASE                     VALUE 'GB'.                  
015100     SKIP2                                                                
015200 01  GOOD-STATUSCODES.                                                    
015300     03  GOOD-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
015400     SKIP3                                                                
015500 01  SSA1                        PIC X(64).                               
015600 01  SSA2                        PIC X(64).                               
015700     EJECT                                                                
015800*    --- IMS FUNCTION CODES                                               
015900*01  -COPY W0003                                                          
016000     EJECT                                                                
016100*    ---  DLI INPUT-OUTPUT AREA                                           
016200                                                                          
016300 01  FILLER         PIC X(30) VALUE 'WL631301-AREA'.                      
016400 01  WL631301-AREA.                                                       
016500*    03  -COPY WDGX6313                                                   
016600     EJECT                                                                
016700 01  FILLER         PIC X(23) VALUE 'WL631311-AREA'.                      
016800 01  WL631311-AREA.                                                       
016900*    03  -COPY WDGX6314                                                   
017000     EJECT                                                                
017100 01  FILLER               PIC X(16)   VALUE 'WDB601 AREA'.                
017200 01   DLI-IO-AREA-B601.                                                   
017300*     03  -COPY WDB601                                                    
017400     EJECT                                                                
017500 LINKAGE SECTION.                                                         
017600*01  -COPY W0009   -PRE MSG-                                              
017700*01  -COPY W0008   -PRE USEA-                                             
017800     05  FILLER                  PIC X.                                   
017900     EJECT                                                                
018000*01  -COPY W0008  -PRE 6313-                                              
018100     05  FILLER                  PIC X.                                   
018200     EJECT                                                                
018300*01  -COPY W0008      -PRE WDB6-                                          
018400     05  FILLER                  PIC X.                                   
018500     EJECT                                                                
018600 PROCEDURE DIVISION  USING MSG-PCB USEA-PCB 6313-PCB                      
018700                           WDB6-PCB.                                      
018800 MAIN SECTION.                                                            
018900     ENTRY 'DLITCBL' USING MSG-PCB USEA-PCB 6313-PCB                      
019000                           WDB6-PCB.                                      
019100                                                                          
019200     PERFORM IMS-GET-MSG                                                  
019300     IF SEGMENT-FOUND                                                     
019400       PERFORM A-INIT                                                     
019500          PERFORM B-CHECK-KEYS                                            
019600          IF KEYS-OK                                                      
019700             IF MFS-UPDATE                                                
019800                PERFORM G-CHECK-INPUT                                     
019900                IF INDATA-OK                                              
020000                   PERFORM H-UPDATE                                       
020100                   PERFORM F-READ-SHOW-INFO                               
020200                 END-IF                                                   
020300              ELSE                                                        
020400                IF MFS-FIRST                                              
020500                   PERFORM C-FIRST-PAGE                                   
020600                ELSE                                                      
020700                   IF MFS-NEXT                                            
020800                      PERFORM D-NEXT-PAGE                                 
020900                   ELSE                                                   
021000                      PERFORM E-SAME-PAGE                                 
021100                   END-IF                                                 
021200               END-IF                                                     
021300               PERFORM F-READ-SHOW-INFO                                   
021400             END-IF                                                       
021500          END-IF                                                          
021600       COMPUTE MSG-KVLL = LENGTH OF MOD-W6O31101 + 4                      
021700       PERFORM IMS-INSERT-MSG                                             
021800     END-IF                                                               
021900                                                                          
022000     MOVE ZERO TO RETURN-CODE                                             
022100     GOBACK                                                               
022200     .                                                                    
022300     EJECT                                                                
022400 A-INIT SECTION.                                                          
022500                                                                          
022600     IF MSG-DOUBLE-TRANSACTIONS                                           
022700       MOVE MSG-INDATA-MINUS-2-TRANSACT   TO MID-W6I31101                 
022800       MOVE MSG-IDTRANS-2                 TO MFS-IDTRANS                  
022900       MOVE MSG-KDMFSFOR-2                TO MFS-KDMFSFOR                 
023000     ELSE                                                                 
023100       MOVE MSG-INDATA-MINUS-1-TRANSACT   TO MID-W6I31101                 
023200       MOVE MSG-IDTRANS-1                 TO MFS-IDTRANS                  
023300       MOVE MSG-KDMFSFOR-1                TO MFS-KDMFSFOR                 
023400     END-IF                                                               
023500                                                                          
023600     MOVE MSG-KDTRTYP      TO MFS-KDTRTYP                                 
023700     MOVE MSG-IDPFK        TO MFS-IDPFK                                   
023800     MOVE MFS-IDTRANS      TO W-IDTRANS                                   
023900                                                                          
024000     MOVE LOW-VALUE        TO MSG-AREA                                    
024100     MOVE 'W6O311N1'       TO MFS-IDMOD                                   
024200     MOVE '6311'           TO MOD-IDTRANS                                 
024300     MOVE MFS-ERASE-FIELD  TO MOD-TEMFSFEL MOD-TEMFSINF                   
024400                                                                          
024500     IF GOOD-MID OR HELP-MID                                              
024600       CONTINUE                                                           
024700     ELSE                                                                 
024800       MOVE SPACE                         TO MFS-KDTRTYP                  
024900       MOVE '7'                           TO MFS-IDPFK                    
025000       PERFORM MFS-INIT-KEY-FIELD-IN                                      
025100       PERFORM MFS-INIT-KEY-FIELD-OUT                                     
025200       PERFORM MFS-ERASE-FIELD-IN                                         
025300       PERFORM MFS-ERASE-LINE-FIELD-OUT                                   
025400     END-IF                                                               
025500                                                                          
025600     IF MID-KDFREQ-IN NOT = ALL '+'                                       
025700       MOVE '7'         TO MFS-IDPFK                                      
025800       MOVE SPACE       TO MFS-KDTRTYP                                    
025900     END-IF                                                               
026000     .                                                                    
026100     EJECT                                                                
026200 B-CHECK-KEYS SECTION.                                                    
026300                                                                          
026400                                                                          
026500     MOVE YES               TO KEYS-SW                                    
026600                                                                          
026700     MOVE ALL '+'           TO MSGI-WMSGINIT                              
026800     MOVE '001'             TO MSGI-KDCALL                                
026900     MOVE MSG-LTERM-NAME    TO MSGI-IDLTERM-USER                          
027000     MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                                
027100     MOVE '6311'            TO MSGI-IDTRANS                               
027200                                                                          
027300     IF OWN-MID                                                           
027400        MOVE MID-KDFREQ-IN TO MSGI-KDFREQ                                 
027500     END-IF                                                               
027600                                                                          
027700     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
027800*                                                                         
027900     MOVE MFS-RENSA-FAELT   TO MOD-KDFREQ-IN                              
028000                               MOD-IDDC-IN                                
028100                                                                          
028200     MOVE MSGI-IDLAND-SPR TO MED-IDSKYLT                                  
028300     MOVE MSGI-SPAR-AREA TO SAVE-AREA                                     
028400                                                                          
028500*    -- CONTROL  ON FREQUENCY                                             
028600     IF MSGI-KDFREQ NUMERIC                                               
028700        IF MSGI-KDFREQ < 10                                               
028800           MOVE '10' TO MSGI-KDFREQ                                       
028900           MOVE '10' TO W-KDFREQ-MIN                                      
029000           MOVE '99' TO W-KDFREQ-MAX                                      
029100        END-IF                                                            
029200     ELSE                                                                 
029300        IF MSGI-KDFREQ = '0 '  OR MSGI-KDFREQ = SPACE                     
029400           MOVE '10' TO MSGI-KDFREQ                                       
029500           MOVE '10' TO W-KDFREQ-MIN                                      
029600           MOVE '99' TO W-KDFREQ-MAX                                      
029700        ELSE                                                              
029800           MOVE NOO                 TO KEYS-SW                            
029900        END-IF                                                            
030000     END-IF                                                               
030100                                                                          
030200*    -- CONTROL  ON IDDC                                                  
030300     MOVE MSGI-IDDC         TO W-IDDC-B6                                  
030400     PERFORM IMS-GU-WDB601                                                
030500     IF SEGMENT-FOUND                                                     
030600     AND (DCS-SDC                                                         
030700     OR   DCS-NDC-NA                                                      
030800     OR   DCS-NDC-PF                                                      
030900     OR   DCS-NDC-OTHERS)                                                 
031000       CONTINUE                                                           
031100     ELSE                                                                 
031200       MOVE NOO                 TO KEYS-SW                                
031300     END-IF                                                               
031400                                                                          
031500*    --  END OF CONTROLS                                                  
031600                                                                          
031700                                                                          
031800     IF GOOD-MID OR KEYS-OK                                               
031900        MOVE MSGI-KDFREQ    TO WS-KDFREQ                                  
032000                                                                          
032100        MOVE WS-KDFREQ      TO W-KDFREQ                                   
032200                                                                          
032300        MOVE WS-KDFREQ      TO MOD-KDFREQ-UT                              
032400        MOVE W-IDDC-B6      TO MOD-IDDC-UT                                
032500     ELSE                                                                 
032600        MOVE MFS-RENSA-FAELT TO MOD-KDFREQ-UT                             
032700     END-IF                                                               
032800                                                                          
032900     IF KEYS-WRONG                                                        
033000       MOVE ERR-WRONG-KEY TO MED-IDMFSFEL                                 
033100       CALL WMEDKONV USING MED-WMEDAREA                                   
033200       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
033300       PERFORM MFS-ERASE-FIELD-IN                                         
033400       PERFORM MFS-ERASE-LINE-FIELD-OUT                                   
033500     END-IF                                                               
033600     .                                                                    
033700     EJECT                                                                
033800 C-FIRST-PAGE SECTION.                                                    
033900                                                                          
034000     MOVE INF-FIRST-PAGE TO MED-IDMFSINF                                  
034100     CALL WMEDKONV USING MED-WMEDAREA                                     
034200     MOVE MED-MFSINF     TO MOD-TEMFSINF                                  
034300                                                                          
034400     PERFORM MFS-ERASE-FIELD-IN                                           
034500     MOVE WS-KDFREQ      TO W-KDFREQ                                      
034600     .                                                                    
034700     EJECT                                                                
034800 D-NEXT-PAGE SECTION.                                                     
034900                                                                          
035000     IF SAVE-IDTRANS = '6311'                                             
035100       MOVE SAVE-KDFREQ-NEXT TO W-KDFREQ                                  
035200     ELSE                                                                 
035300       MOVE ZEROES           TO W-KDFREQ                                  
035400     END-IF                                                               
035500     .                                                                    
035600     EJECT                                                                
035700 E-SAME-PAGE SECTION.                                                     
035800                                                                          
035900     IF OWN-MID OR HELP-MID                                               
036000       MOVE SAVE-KDFREQ-ENTER TO W-KDFREQ                                 
036100       IF MID-KDFREQ-MAIN   = ALL '+' AND                                 
036200          MID-TEFREQ-MAIN   = ALL '+' AND                                 
036300          MID-KVPB-FOM-MAIN = ALL '+' AND                                 
036400          MID-KVPB-TOM-MAIN = ALL '+' AND                                 
036500          MID-RELOCFAC-MAIN = ALL '+' AND                                 
036600          MID-KDANDR-MAIN   = ALL '+'                                     
036700          PERFORM MFS-ERASE-FIELD-IN                                      
036800       ELSE                                                               
036900         MOVE INF-PRESS-PF11 TO MED-IDMFSINF                              
037000         CALL WMEDKONV USING MED-WMEDAREA                                 
037100         MOVE MED-MFSINF TO MOD-TEMFSINF                                  
037200         PERFORM EA-MID-INDATA-TO-MOD                                     
037300       END-IF                                                             
037400     ELSE                                                                 
037500       PERFORM MFS-ERASE-FIELD-IN                                         
037600     END-IF                                                               
037700     .                                                                    
037800     EJECT                                                                
037900 EA-MID-INDATA-TO-MOD SECTION.                                            
038000                                                                          
038100     IF MID-RELOCFAC-MAIN  = ALL '+'                                      
038200        MOVE MFS-ERASE-FIELD        TO MOD-RELOCFAC-ATTR                  
038300      ELSE                                                                
038400        MOVE MFS-DO-NOT-TOUCH-FIELD TO MOD-RELOCFAC-MAIN                  
038500        MOVE MFS-ADD-READ-FIELD     TO MOD-RELOCFAC-ATTR                  
038600     END-IF                                                               
038700                                                                          
038800     IF MID-KVPB-FOM-MAIN  = ALL '+'                                      
038900        MOVE MFS-ERASE-FIELD        TO MOD-KVPB-FOM-ATTR                  
039000      ELSE                                                                
039100        MOVE MFS-DO-NOT-TOUCH-FIELD TO MOD-KVPB-FOM-MAIN                  
039200        MOVE MFS-ADD-READ-FIELD     TO MOD-KVPB-FOM-ATTR                  
039300     END-IF                                                               
039400                                                                          
039500     IF MID-KVPB-TOM-MAIN = ALL '+'                                       
039600        MOVE MFS-ERASE-FIELD        TO MOD-KVPB-TOM-ATTR                  
039700      ELSE                                                                
039800        MOVE MFS-DO-NOT-TOUCH-FIELD TO MOD-KVPB-TOM-MAIN                  
039900        MOVE MFS-ADD-READ-FIELD     TO MOD-KVPB-TOM-ATTR                  
040000     END-IF                                                               
040100                                                                          
040200     IF MID-TEFREQ-MAIN  = ALL '+'                                        
040300        MOVE MFS-ERASE-FIELD        TO MOD-TEFREQ-ATTR                    
040400      ELSE                                                                
040500        MOVE MFS-DO-NOT-TOUCH-FIELD TO MOD-TEFREQ-MAIN                    
040600        MOVE MFS-ADD-READ-FIELD     TO MOD-TEFREQ-ATTR                    
040700     END-IF                                                               
040800                                                                          
040900     IF MID-KDANDR-MAIN  = ALL '+'                                        
041000        MOVE MFS-ERASE-FIELD        TO MOD-KDANDR-ATTR                    
041100      ELSE                                                                
041200        MOVE MFS-DO-NOT-TOUCH-FIELD TO MOD-KDANDR-MAIN                    
041300        MOVE MFS-ADD-READ-FIELD     TO MOD-KDANDR-ATTR                    
041400     END-IF                                                               
041500                                                                          
041600     IF MID-KDFREQ-MAIN  = ALL '+'                                        
041700        MOVE MFS-ERASE-FIELD        TO MOD-KDFREQ-ATTR                    
041800      ELSE                                                                
041900        MOVE MFS-DO-NOT-TOUCH-FIELD TO MOD-KDFREQ-MAIN                    
042000        MOVE MFS-ADD-READ-FIELD     TO MOD-KDFREQ-ATTR                    
042100     END-IF                                                               
042200     .                                                                    
042300     EJECT                                                                
042400 F-READ-SHOW-INFO SECTION.                                                
042500                                                                          
042600     MOVE W-IDDC-B6    TO  W-6313-IDDC                                    
042700                                                                          
042800     PERFORM  IMS-GU-6313-6314                                            
042900                                                                          
043000     IF SEGMENT-MISSING                                                   
043100        MOVE ITEMS-MISSING TO MED-IDMFSFEL                                
043200        CALL WMEDKONV USING MED-WMEDAREA                                  
043300        MOVE MED-MFSFEL    TO MOD-TEMFSFEL                                
043400        PERFORM MFS-ERASE-LINE-FIELD-OUT                                  
043500        MOVE WS-KDFREQ     TO SAVE-KDFREQ-ENTER                           
043600        MOVE WS-KDFREQ     TO SAVE-KDFREQ-NEXT                            
043700     ELSE                                                                 
043800        MOVE 6314-KDFREQ   TO SAVE-KDFREQ-ENTER                           
043900        MOVE +1            TO INDX                                        
044000        PERFORM UNTIL SEGMENT-MISSING OR END-OF-DATABASE OR               
044100                      INDX > MAX-INDX                                     
044200         MOVE 6314-KDFREQ         TO MOD-KDFREQ-LINE   (INDX)             
044300         MOVE 6314-TEFREQ         TO MOD-TEFREQ-LINE   (INDX)             
044400         MOVE 6314-KVPB-FOM       TO MOD-KVPB-FOM-LINE (INDX)             
044500         MOVE 6314-KVPB-TOM       TO MOD-KVPB-TOM-LINE (INDX)             
044600         MOVE 6314-RELOCFAC       TO MOD-RELOCFAC-LINE (INDX)             
044700         PERFORM IMS-GN-6313-6314                                         
044800         ADD 1 TO INDX                                                    
044900        END-PERFORM                                                       
045000        IF SEGMENT-FOUND                                                  
045100           MOVE 6314-KDFREQ  TO SAVE-KDFREQ-NEXT                          
045200           MOVE INF-MORE-INFO-EXISTS TO MED-IDMFSINF                      
045300           CALL WMEDKONV USING MED-WMEDAREA                               
045400           MOVE MED-TEMFSINF TO MOD-TEMFSINF                              
045500         ELSE                                                             
045600           MOVE 6314-KDFREQ         TO SAVE-KDFREQ-NEXT                   
045700           IF MFS-NEXT                                                    
045800              MOVE INF-LAST-PAGE-SHOWN TO MED-IDMFSFEL                    
045900              CALL WMEDKONV         USING MED-WMEDAREA                    
046000              MOVE MED-MFSFEL       TO    MOD-TEMFSFEL                    
046100           END-IF                                                         
046200           PERFORM UNTIL INDX > MAX-INDX                                  
046300             MOVE MFS-ERASE-FIELD   TO   MOD-KDFREQ-LINE   (INDX)         
046400                                         MOD-TEFREQ-LINE   (INDX)         
046500                                         MOD-KVPB-FOM-LINE (INDX)         
046600                                         MOD-KVPB-TOM-LINE (INDX)         
046700                                         MOD-RELOCFAC-LINE (INDX)         
046800             ADD 1 TO INDX                                                
046900           END-PERFORM                                                    
047000         END-IF                                                           
047100                                                                          
047200       MOVE '002'      TO MSGI-KDCALL                                     
047300       MOVE '6311'     TO SAVE-IDTRANS                                    
047400       MOVE SAVE-AREA  TO MSGI-SPAR-AREA                                  
047500       CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                         
047600     END-IF                                                               
047700     .                                                                    
047800     EJECT                                                                
047900 G-CHECK-INPUT SECTION.                                                   
048000                                                                          
048100     MOVE YES  TO INDATA-SW                                               
048200                                                                          
048300     IF MID-KDFREQ-MAIN   = ALL '+' AND                                   
048400        MID-TEFREQ-MAIN   = ALL '+' AND                                   
048500        MID-KVPB-FOM-MAIN = ALL '+' AND                                   
048600        MID-KVPB-TOM-MAIN = ALL '+' AND                                   
048700        MID-RELOCFAC-MAIN = ALL '+' AND                                   
048800        MID-KDANDR-MAIN   = ALL '+'                                       
048900          MOVE ERR-PF11-AND-NO-DATA TO MED-IDMFSFEL                       
049000          CALL WMEDKONV USING MED-WMEDAREA                                
049100          MOVE MED-MFSFEL TO MOD-TEMFSFEL                                 
049200          MOVE NOO TO INDATA-SW                                           
049300     END-IF                                                               
049400     IF INDATA-OK                                                         
049500        IF MID-KDFREQ-MAIN = ALL '+'                                      
049600           MOVE MFS-ALPHA-FIELD-WRONG  TO MOD-KDFREQ-ATTR                 
049700           MOVE NOO                    TO INDATA-SW                       
049800         ELSE                                                             
049900           IF MID-KDFREQ-MAIN NUMERIC                                     
050000              IF MID-KDFREQ-MAIN < 10                                     
050100                 MOVE MFS-NUM-FIELD-WRONG TO MOD-KDANDR-ATTR              
050200                 MOVE NOO              TO INDATA-SW                       
050300              ELSE                                                        
050400                 MOVE MFS-ALPHA-FIELD-OK TO MOD-KDFREQ-ATTR               
050500              END-IF                                                      
050600            ELSE                                                          
050700              MOVE MFS-NUM-FIELD-WRONG TO MOD-KDANDR-ATTR                 
050800              MOVE NOO                 TO INDATA-SW                       
050900           END-IF                                                         
051000        END-IF                                                            
051100        IF MID-KDANDR-MAIN = ALL '+'                                      
051200           MOVE MFS-ALPHA-FIELD-WRONG  TO MOD-KDANDR-ATTR                 
051300           MOVE NOO                    TO INDATA-SW                       
051400         ELSE                                                             
051500           IF MID-KDANDR-MAIN = NEW OR DEL OR CHG                         
051600              MOVE MFS-ALPHA-FIELD-OK  TO MOD-KDANDR-ATTR                 
051700           ELSE                                                           
051800              MOVE MFS-ALPHA-FIELD-WRONG     TO MOD-KDANDR-ATTR           
051900              MOVE NOO                       TO INDATA-SW                 
052000           END-IF                                                         
052100        END-IF                                                            
052200        IF INDATA-WRONG                                                   
052300           PERFORM GD-MID-INDATA-TO-MOD                                   
052400           MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL                      
052500           CALL WMEDKONV USING MED-WMEDAREA                               
052600           MOVE MED-MFSFEL           TO MOD-TEMFSFEL                      
052700        END-IF                                                            
052800     END-IF                                                               
052900                                                                          
053000     IF INDATA-OK                                                         
053100        IF MID-KDANDR-MAIN = NEW                                          
053200           PERFORM GA-CHECK-INPUT-NEW                                     
053300        END-IF                                                            
053400        IF MID-KDANDR-MAIN = CHG                                          
053500           PERFORM GB-CHECK-INPUT-CHANGE                                  
053600        END-IF                                                            
053700        IF MID-KDANDR-MAIN = DEL                                          
053800           PERFORM GC-CHECK-INPUT-DELETE                                  
053900        END-IF                                                            
054000        IF INDATA-WRONG                                                   
054100           MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL                      
054200           CALL WMEDKONV USING MED-WMEDAREA                               
054300           MOVE MED-MFSFEL           TO MOD-TEMFSFEL                      
054400        END-IF                                                            
054500     END-IF                                                               
054600                                                                          
054700     IF INDATA-WRONG                                                      
054800        PERFORM MFS-DONT-TOUCH-FIELD-OUT                                  
054900     END-IF                                                               
055000                                                                          
055100     .                                                                    
055200     EJECT                                                                
055300 GA-CHECK-INPUT-NEW SECTION.                                              
055400                                                                          
055500*                                                                         
055600*   --- CHECK RELOCATION FACTOR                                           
055700*                                                                         
055800     IF MID-RELOCFAC-MAIN = ALL '+'                                       
055900        MOVE NOO                    TO INDATA-SW                          
056000        MOVE MFS-ALPHA-FIELD-WRONG  TO MOD-RELOCFAC-ATTR                  
056100       ELSE                                                               
056200        MOVE MID-RELOCFAC-MAIN      TO DEC-IDFRIDATA                      
056300        MOVE 1                      TO DEC-KVHELTAL                       
056400        MOVE 2                      TO DEC-KVDECIMAL                      
056500        CALL WDECEDIT USING DEC-WDECAREA                                  
056600        END-CALL                                                          
056700        IF DEC-KDSVAR-OK                                                  
056800          MOVE MFS-ALPHA-FIELD-OK   TO MOD-RELOCFAC-ATTR                  
056900          MOVE DEC-IDEDITDATA       TO WS-RELOCFAC                        
057000         ELSE                                                             
057100          MOVE MFS-ALPHA-FIELD-WRONG TO MOD-RELOCFAC-ATTR                 
057200          MOVE NOO                   TO INDATA-SW                         
057300        END-IF                                                            
057400     END-IF                                                               
057500*                                                                         
057600*   --- CHECK FORECAST FACTOR THRU                                        
057700*                                                                         
057800     IF MID-KVPB-FOM-MAIN = ALL '+'                                       
057900        MOVE NOO                    TO INDATA-SW                          
058000        MOVE MFS-ALPHA-FIELD-WRONG  TO MOD-KVPB-FOM-ATTR                  
058100       ELSE                                                               
058200        MOVE MID-KVPB-FOM-MAIN      TO DEC-IDFRIDATA                      
058300        MOVE 6                      TO DEC-KVHELTAL                       
058400        MOVE 1                      TO DEC-KVDECIMAL                      
058500        CALL WDECEDIT USING DEC-WDECAREA                                  
058600        END-CALL                                                          
058700        IF DEC-KDSVAR-OK                                                  
058800          MOVE MFS-ALPHA-FIELD-OK   TO MOD-KVPB-FOM-ATTR                  
058900          MOVE DEC-IDEDITDATA       TO WS-KVPB-FOM                        
059000         ELSE                                                             
059100          MOVE MFS-ALPHA-FIELD-WRONG TO MOD-KVPB-FOM-ATTR                 
059200          MOVE NOO                   TO INDATA-SW                         
059300        END-IF                                                            
059400     END-IF                                                               
059500*                                                                         
059600*   --- CHECK FORECAST FACTOR FROM                                        
059700*                                                                         
059800     IF MID-KVPB-TOM-MAIN = ALL '+'                                       
059900        MOVE NOO                    TO INDATA-SW                          
060000        MOVE MFS-ALPHA-FIELD-WRONG  TO MOD-KVPB-TOM-ATTR                  
060100       ELSE                                                               
060200        MOVE MID-KVPB-TOM-MAIN      TO DEC-IDFRIDATA                      
060300        MOVE 6                      TO DEC-KVHELTAL                       
060400        MOVE 1                      TO DEC-KVDECIMAL                      
060500        CALL WDECEDIT USING DEC-WDECAREA                                  
060600        END-CALL                                                          
060700        IF DEC-KDSVAR-OK                                                  
060800          MOVE MFS-ALPHA-FIELD-OK   TO MOD-KVPB-TOM-ATTR                  
060900          MOVE DEC-IDEDITDATA       TO WS-KVPB-TOM                        
061000         ELSE                                                             
061100          MOVE MFS-ALPHA-FIELD-WRONG TO MOD-KVPB-TOM-ATTR                 
061200          MOVE NOO                   TO INDATA-SW                         
061300        END-IF                                                            
061400     END-IF                                                               
061500*                                                                         
061600*   --- CHECK FREQUENCY DESCRIPTION                                       
061700*                                                                         
061800     IF MID-TEFREQ-MAIN = ALL '+'                                         
061900        MOVE NOO                     TO INDATA-SW                         
062000        MOVE MFS-ALPHA-FIELD-WRONG   TO MOD-TEFREQ-ATTR                   
062100      ELSE                                                                
062200        MOVE MFS-ALPHA-FIELD-OK      TO MOD-TEFREQ-ATTR                   
062300        MOVE MID-TEFREQ-MAIN         TO WS-TEFREQ                         
062400     END-IF                                                               
062500                                                                          
062600*                                                                         
062700*   --- CHECK FREQUENCY ALREADY EXIST                                     
062800*                                                                         
062900     MOVE W-IDDC-B6                  TO W-6313-IDDC                       
063000     MOVE MID-KDFREQ-MAIN            TO W-KDFREQ                          
063100                                                                          
063200     PERFORM  IMS-GU-6313-6314                                            
063300                                                                          
063400     IF SEGMENT-FOUND                                                     
063500        MOVE NOO                     TO INDATA-SW                         
063600        MOVE MFS-ALPHA-FIELD-WRONG   TO MOD-KDFREQ-ATTR                   
063700       ELSE                                                               
063800        MOVE MFS-ALPHA-FIELD-OK      TO MOD-KDFREQ-ATTR                   
063900        MOVE MID-KDFREQ-MAIN         TO WS-KDFREQ                         
064000     END-IF                                                               
064100                                                                          
064200     .                                                                    
064300     EJECT                                                                
064400 GB-CHECK-INPUT-CHANGE SECTION.                                           
064500*                                                                         
064600*   --- CHECK RELOCATION FACTOR                                           
064700*                                                                         
064800     IF MID-RELOCFAC-MAIN NOT = ALL '+'                                   
064900        MOVE MID-RELOCFAC-MAIN      TO DEC-IDFRIDATA                      
065000        MOVE 1                      TO DEC-KVHELTAL                       
065100        MOVE 2                      TO DEC-KVDECIMAL                      
065200        CALL WDECEDIT USING DEC-WDECAREA                                  
065300        END-CALL                                                          
065400        IF DEC-KDSVAR-OK                                                  
065500          MOVE MFS-ALPHA-FIELD-OK   TO MOD-RELOCFAC-ATTR                  
065600          MOVE DEC-IDEDITDATA       TO WS-RELOCFAC                        
065700*         MOVE DEC-IDEDITDATA       TO FREQ-RELOCFAC                      
065800         ELSE                                                             
065900          MOVE MFS-ALPHA-FIELD-WRONG TO MOD-RELOCFAC-ATTR                 
066000          MOVE NOO                   TO INDATA-SW                         
066100        END-IF                                                            
066200     END-IF                                                               
066300*                                                                         
066400*   --- CHECK FORECAST FACTOR THRU                                        
066500*                                                                         
066600     IF MID-KVPB-FOM-MAIN NOT = ALL '+'                                   
066700        MOVE MID-KVPB-FOM-MAIN      TO DEC-IDFRIDATA                      
066800        MOVE 6                      TO DEC-KVHELTAL                       
066900        MOVE 1                      TO DEC-KVDECIMAL                      
067000        CALL WDECEDIT USING DEC-WDECAREA                                  
067100        END-CALL                                                          
067200        IF DEC-KDSVAR-OK                                                  
067300          MOVE MFS-ALPHA-FIELD-OK   TO MOD-KVPB-FOM-ATTR                  
067400          MOVE DEC-IDEDITDATA       TO WS-KVPB-FOM                        
067500*         MOVE DEC-IDEDITDATA       TO FREQ-KVPB-FOM                      
067600         ELSE                                                             
067700          MOVE MFS-ALPHA-FIELD-WRONG TO MOD-KVPB-FOM-ATTR                 
067800          MOVE NOO                   TO INDATA-SW                         
067900        END-IF                                                            
068000     END-IF                                                               
068100*                                                                         
068200*   --- CHECK FORECAST FACTOR THRU                                        
068300*                                                                         
068400     IF MID-KVPB-TOM-MAIN NOT = ALL '+'                                   
068500        MOVE MID-KVPB-TOM-MAIN      TO DEC-IDFRIDATA                      
068600        MOVE 6                      TO DEC-KVHELTAL                       
068700        MOVE 1                      TO DEC-KVDECIMAL                      
068800        CALL WDECEDIT USING DEC-WDECAREA                                  
068900        END-CALL                                                          
069000        IF DEC-KDSVAR-OK                                                  
069100          MOVE MFS-ALPHA-FIELD-OK   TO MOD-KVPB-TOM-ATTR                  
069200          MOVE DEC-IDEDITDATA       TO WS-KVPB-TOM                        
069300*         MOVE DEC-IDEDITDATA       TO FREQ-KVPB-TOM                      
069400         ELSE                                                             
069500          MOVE MFS-ALPHA-FIELD-WRONG TO MOD-KVPB-TOM-ATTR                 
069600          MOVE NOO                   TO INDATA-SW                         
069700        END-IF                                                            
069800     END-IF                                                               
069900*                                                                         
070000*   --- CHECK FREQUENCY DESCRIPTION                                       
070100*                                                                         
070200     IF MID-TEFREQ-MAIN NOT = ALL '+'                                     
070300        MOVE MFS-ALPHA-FIELD-OK      TO MOD-TEFREQ-ATTR                   
070400        MOVE MID-TEFREQ-MAIN         TO WS-TEFREQ                         
070500     END-IF                                                               
070600                                                                          
070700*                                                                         
070800*   --- CHECK IF FREQUENCY KODE EXIST                                     
070900*                                                                         
071000     MOVE W-IDDC-B6                  TO W-6313-IDDC                       
071100     MOVE MID-KDFREQ-MAIN            TO W-KDFREQ                          
071200                                                                          
071300     PERFORM  IMS-GHU-6314                                                
071400                                                                          
071500     IF SEGMENT-MISSING                                                   
071600        MOVE NOO                     TO INDATA-SW                         
071700        MOVE MFS-ALPHA-FIELD-WRONG   TO MOD-KDFREQ-ATTR                   
071800       ELSE                                                               
071900        MOVE MFS-ALPHA-FIELD-OK      TO MOD-KDFREQ-ATTR                   
072000        MOVE MID-KDFREQ-MAIN         TO WS-KDFREQ                         
072100     END-IF                                                               
072200     .                                                                    
072300     EJECT                                                                
072400                                                                          
072500 GC-CHECK-INPUT-DELETE SECTION.                                           
072600                                                                          
072700     MOVE W-IDDC-B6                  TO W-6313-IDDC                       
072800     MOVE MID-KDFREQ-MAIN            TO W-KDFREQ                          
072900                                                                          
073000     PERFORM  IMS-GHU-6314                                                
073100*                                                                         
073200*   --- CHECK IF FREQUENCY KODE EXIST                                     
073300*                                                                         
073400     IF SEGMENT-MISSING                                                   
073500        MOVE NOO                     TO INDATA-SW                         
073600        MOVE MFS-ALPHA-FIELD-WRONG   TO MOD-KDFREQ-ATTR                   
073700       ELSE                                                               
073800        MOVE MFS-ALPHA-FIELD-OK      TO MOD-KDFREQ-ATTR                   
073900     END-IF                                                               
074000     .                                                                    
074100     EJECT                                                                
074200 GD-MID-INDATA-TO-MOD SECTION.                                            
074300                                                                          
074400     IF MID-RELOCFAC-MAIN  = ALL '+'                                      
074500        MOVE MFS-ERASE-FIELD        TO MOD-RELOCFAC-ATTR                  
074600      ELSE                                                                
074700        MOVE MFS-ADD-READ-FIELD     TO MOD-RELOCFAC-ATTR                  
074800     END-IF                                                               
074900                                                                          
075000     IF MID-KVPB-FOM-MAIN  = ALL '+'                                      
075100        MOVE MFS-ERASE-FIELD        TO MOD-KVPB-FOM-ATTR                  
075200      ELSE                                                                
075300        MOVE MFS-ADD-READ-FIELD     TO MOD-KVPB-FOM-ATTR                  
075400     END-IF                                                               
075500                                                                          
075600     IF MID-KVPB-TOM-MAIN = ALL '+'                                       
075700        MOVE MFS-ERASE-FIELD        TO MOD-KVPB-TOM-ATTR                  
075800      ELSE                                                                
075900        MOVE MFS-ADD-READ-FIELD     TO MOD-KVPB-TOM-ATTR                  
076000     END-IF                                                               
076100                                                                          
076200     IF MID-TEFREQ-MAIN  = ALL '+'                                        
076300        MOVE MFS-ERASE-FIELD        TO MOD-TEFREQ-ATTR                    
076400      ELSE                                                                
076500        MOVE MFS-ADD-READ-FIELD     TO MOD-TEFREQ-ATTR                    
076600     END-IF                                                               
076700                                                                          
076800     .                                                                    
076900     EJECT                                                                
077000 H-UPDATE SECTION.                                                        
077100                                                                          
077200     IF MID-KDANDR-MAIN = NEW                                             
077300        PERFORM HA-NEW                                                    
077400     END-IF                                                               
077500     IF MID-KDANDR-MAIN = CHG                                             
077600        PERFORM HB-CHANGE                                                 
077700     END-IF                                                               
077800     IF MID-KDANDR-MAIN = DEL                                             
077900        PERFORM HC-DELETE                                                 
078000     END-IF                                                               
078100                                                                          
078200     MOVE INF-UPDATE-DONE TO MED-IDMFSINF                                 
078300     CALL WMEDKONV USING MED-WMEDAREA                                     
078400     MOVE MED-MFSINF TO MOD-TEMFSINF                                      
078500     PERFORM MFS-FORM-ATTR                                                
078600     PERFORM MFS-ERASE-FIELD-IN                                           
078700     .                                                                    
078800     EJECT                                                                
078900 HA-NEW SECTION.                                                          
079000       MOVE W-IDDC-B6         TO W-6313-IDDC                              
079100       MOVE MID-KDFREQ-MAIN   TO W-KDFREQ                                 
079200                                 6314-KDFREQ                              
079300                                 W-KDFREQ                                 
079400                                 MOD-KDFREQ-UT                            
079500       MOVE WS-TEFREQ         TO 6314-TEFREQ                              
079600       MOVE WS-KVPB-FOM       TO 6314-KVPB-FOM                            
079700       MOVE WS-KVPB-TOM       TO 6314-KVPB-TOM                            
079800       MOVE WS-RELOCFAC       TO 6314-RELOCFAC                            
079900                                                                          
080000       PERFORM IMS-ISRT-6313-6314                                         
080100                                                                          
080200     .                                                                    
080300     EJECT                                                                
080400 HB-CHANGE SECTION.                                                       
080500     IF MID-RELOCFAC-MAIN NOT = ALL '+'                                   
080600        MOVE WS-RELOCFAC          TO 6314-RELOCFAC                        
080700     END-IF                                                               
080800     IF MID-KVPB-FOM-MAIN NOT = ALL '+'                                   
080900        MOVE WS-KVPB-FOM          TO 6314-KVPB-FOM                        
081000     END-IF                                                               
081100     IF MID-KVPB-TOM-MAIN NOT = ALL '+'                                   
081200        MOVE WS-KVPB-TOM          TO 6314-KVPB-TOM                        
081300     END-IF                                                               
081400     IF MID-TEFREQ-MAIN   NOT = ALL '+'                                   
081500        MOVE WS-TEFREQ            TO 6314-TEFREQ                          
081600     END-IF                                                               
081700                                                                          
081800     PERFORM IMS-REPL-6313-6314                                           
081900                                                                          
082000     MOVE WS-KDFREQ            TO W-KDFREQ                                
082100                                  MOD-KDFREQ-UT                           
082200     .                                                                    
082300     EJECT                                                                
082400 HC-DELETE SECTION.                                                       
082500                                                                          
082600     PERFORM IMS-DLET-6313-6314                                           
082700     .                                                                    
082800     EJECT                                                                
082900 MFS-INIT-KEY-FIELD-IN SECTION.                                           
083000                                                                          
083100*    --- ALL INPUT KEY FIELDS                                             
083200                                                                          
083300     MOVE MFS-ERASE-FIELD          TO MOD-IDDC-IN                         
083400                                      MOD-KDFREQ-IN                       
083500     .                                                                    
083600     EJECT                                                                
083700 MFS-INIT-KEY-FIELD-OUT SECTION.                                          
083800                                                                          
083900*    --- ALL OUTPUT KEY FIELDS                                            
084000                                                                          
084100     MOVE MSGI-IDDC                TO MOD-IDDC-UT                         
084200     MOVE MFS-ERASE-FIELD          TO MOD-KDFREQ-UT                       
084300     .                                                                    
084400     EJECT                                                                
084500 MFS-ERASE-LINE-FIELD-OUT SECTION.                                        
084600                                                                          
084700*    --- OUTDATA-FIELD ON SCROLL KEYS                                     
084800                                                                          
084900     MOVE +1 TO INDX                                                      
085000     PERFORM UNTIL INDX > MAX-INDX                                        
085100      MOVE MFS-ERASE-FIELD     TO MOD-KDFREQ-LINE   (INDX)                
085200                                  MOD-TEFREQ-LINE   (INDX)                
085300                                  MOD-KVPB-FOM-LINE (INDX)                
085400                                  MOD-KVPB-TOM-LINE (INDX)                
085500                                  MOD-RELOCFAC-LINE (INDX)                
085600      ADD +1 TO INDX                                                      
085700     END-PERFORM                                                          
085800     .                                                                    
085900     SKIP3                                                                
086000 MFS-ERASE-FIELD-IN SECTION.                                              
086100                                                                          
086200*    --- ALL INPUT DATA FIELDS                                            
086300                                                                          
086400     MOVE MFS-ERASE-FIELD          TO MOD-KDFREQ-MAIN                     
086500                                      MOD-TEFREQ-MAIN                     
086600                                      MOD-KVPB-FOM-MAIN                   
086700                                      MOD-KVPB-TOM-MAIN                   
086800                                      MOD-RELOCFAC-MAIN                   
086900                                      MOD-KDANDR-MAIN                     
087000     .                                                                    
087100     EJECT                                                                
087200 MFS-DONT-TOUCH-FIELD-OUT  SECTION.                                       
087300                                                                          
087400*    --- ALL OUTDATA FIELDS                                               
087500*    --- INCL SCROLL KEYS AND LINEDATA                                    
087600     MOVE MFS-DO-NOT-TOUCH-FIELD TO MOD-KDFREQ-MAIN                       
087700                                    MOD-TEFREQ-MAIN                       
087800                                    MOD-KVPB-FOM-MAIN                     
087900                                    MOD-KVPB-TOM-MAIN                     
088000                                    MOD-RELOCFAC-MAIN                     
088100                                    MOD-KDANDR-MAIN                       
088200     MOVE +1 TO INDX                                                      
088300     PERFORM UNTIL INDX > MAX-INDX                                        
088400       PERFORM MFS-DONT-TOUCH-LINE-FIELD-OUT                              
088500       ADD +1 TO INDX                                                     
088600     END-PERFORM                                                          
088700     .                                                                    
088800     EJECT                                                                
088900 MFS-DONT-TOUCH-LINE-FIELD-OUT  SECTION.                                  
089000                                                                          
089100*    --- OUTDATA FIELD ON SCROLL KEYS                                     
089200     MOVE MFS-DO-NOT-TOUCH-FIELD TO MOD-KDFREQ-LINE   (INDX)              
089300                                    MOD-TEFREQ-LINE   (INDX)              
089400                                    MOD-KVPB-FOM-LINE (INDX)              
089500                                    MOD-KVPB-TOM-LINE (INDX)              
089600                                    MOD-RELOCFAC-LINE (INDX)              
089700     .                                                                    
089800     EJECT                                                                
089900 MFS-FORM-ATTR SECTION.                                                   
090000                                                                          
090100*    --- ALL INDATA-FIELDS                                                
090200     MOVE MFS-FORMAT-DEFAULT-ATTR TO MOD-KDFREQ-ATTR                      
090300                                     MOD-TEFREQ-ATTR                      
090400                                     MOD-KVPB-FOM-ATTR                    
090500                                     MOD-KVPB-TOM-ATTR                    
090600                                     MOD-RELOCFAC-ATTR                    
090700                                     MOD-KDANDR-ATTR                      
090800     .                                                                    
090900     SKIP2                                                                
091000 IMS-GET-MSG SECTION.                                                     
091100                                                                          
091200     MOVE '  QC' TO GOOD-STATUSCODES                                      
091300     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
091400     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
091500     PERFORM IMS-STATUSCHECK                                              
091600     .                                                                    
091700     SKIP3                                                                
091800 IMS-INSERT-MSG SECTION.                                                  
091900                                                                          
092000     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
092100     MOVE SPACE TO GOOD-STATUSCODES                                       
092200     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
092300     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
092400     PERFORM IMS-STATUSCHECK                                              
092500     .                                                                    
092600     EJECT                                                                
092700 IMS-GU-6313-6314 SECTION.                                                
092800                                                                          
092900     STRING 'WL631301(WDGXKEY  =' W-WDGXKEY-6313-X ')'                    
093000          DELIMITED BY SIZE INTO SSA1                                     
093100     STRING 'WL631311(KDFREQ   =' W-WDGXKEY-6314-X                        
093200                    '&KDFREQ  =<' W-KDFREQ-MAX-X ')'                      
093300          DELIMITED BY SIZE INTO SSA2                                     
093400     MOVE '  GE' TO GOOD-STATUSCODES                                      
093500     CALL CBLTDLI USING GU 6313-PCB 6314-WDGX6314 SSA1 SSA2               
093600     MOVE 6313-STATUS-CODE TO STATUS-WS                                   
093700     PERFORM IMS-STATUSCHECK                                              
093800     .                                                                    
093900     SKIP3                                                                
094000 IMS-GN-6313-6314 SECTION.                                                
094100                                                                          
094200     STRING 'WL631301(WDGXKEY  =' W-WDGXKEY-6313-X ')'                    
094300          DELIMITED BY SIZE INTO SSA1                                     
094400     STRING 'WL631311(KDFREQ  >=' W-WDGXKEY-6314-X                        
094500                    '&KDFREQ  =<' W-KDFREQ-MAX-X ')'                      
094600          DELIMITED BY SIZE INTO SSA2                                     
094700     MOVE '  GE' TO GOOD-STATUSCODES                                      
094800     CALL CBLTDLI USING GN 6313-PCB 6314-WDGX6314 SSA1 SSA2               
094900     MOVE 6313-STATUS-CODE TO STATUS-WS                                   
095000     PERFORM IMS-STATUSCHECK                                              
095100     .                                                                    
095200     SKIP3                                                                
095300 IMS-GHU-6314 SECTION.                                                    
095400                                                                          
095500     STRING 'WL631301(WDGXKEY  =' W-WDGXKEY-6313-X ')'                    
095600          DELIMITED BY SIZE INTO SSA1                                     
095700     STRING 'WL631311(KDFREQ   =' W-WDGXKEY-6314-X ')'                    
095800          DELIMITED BY SIZE INTO SSA2                                     
095900     MOVE '  GE' TO GOOD-STATUSCODES                                      
096000     CALL CBLTDLI USING GHU 6313-PCB 6314-WDGX6314 SSA1 SSA2              
096100     MOVE 6313-STATUS-CODE TO STATUS-WS                                   
096200     PERFORM IMS-STATUSCHECK                                              
096300     .                                                                    
096400     SKIP3                                                                
096500 IMS-ISRT-6313-6314 SECTION.                                              
096600                                                                          
096700                                                                          
096800     STRING 'WL631301(WDGXKEY  =' W-WDGXKEY-6313-X ')'                    
096900          DELIMITED BY SIZE INTO SSA1                                     
097000     MOVE 'WL631311 '  TO SSA2                                            
097100     MOVE '  II' TO GOOD-STATUSCODES                                      
097200     CALL CBLTDLI USING ISRT 6313-PCB 6314-WDGX6314 SSA1 SSA2             
097300     MOVE 6313-STATUS-CODE TO STATUS-WS                                   
097400     PERFORM IMS-STATUSCHECK                                              
097500     .                                                                    
097600     SKIP3                                                                
097700 IMS-REPL-6313-6314 SECTION.                                              
097800                                                                          
097900     MOVE '  ' TO GOOD-STATUSCODES                                        
098000     CALL CBLTDLI USING REPL 6313-PCB 6314-WDGX6314                       
098100     MOVE 6313-STATUS-CODE TO STATUS-WS                                   
098200     PERFORM IMS-STATUSCHECK                                              
098300     .                                                                    
098400     SKIP3                                                                
098500 IMS-DLET-6313-6314 SECTION.                                              
098600                                                                          
098700     MOVE '  ' TO GOOD-STATUSCODES                                        
098800     CALL CBLTDLI USING DLET 6313-PCB 6314-WDGX6314                       
098900     MOVE 6313-STATUS-CODE TO STATUS-WS                                   
099000     PERFORM IMS-STATUSCHECK                                              
099100     .                                                                    
099200     EJECT                                                                
099300                                                                          
099400 IMS-GU-WDB601    SECTION.                                                
099500     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
099600          DELIMITED BY SIZE INTO SSA1                                     
099700     MOVE '  GE' TO GOOD-STATUSCODES                                      
099800     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B601 SSA1                 
099900     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
100000     PERFORM IMS-STATUSCHECK                                              
100100     .                                                                    
100200     EJECT                                                                
100300 IMS-STATUSCHECK SECTION.                                                 
100400                                                                          
100500     SET STATUS-IX TO 1                                                   
100600     SEARCH GOOD-STATUS                                                   
100700       AT END                                                             
100800         STRING ' INVALID STATUS CODE FROM IMS: ' STATUS-WS               
100900         DELIMITED BY SIZE INTO ERROR-TEXT                                
101000         CALL FELLOG                                                      
101100       WHEN GOOD-STATUS (STATUS-IX) = STATUS-WS                           
101200         CONTINUE                                                         
101300     END-SEARCH                                                           
101400     .                                                                    
