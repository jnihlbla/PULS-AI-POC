000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W6031200.                                                
000300 AUTHOR.         MARTIEN HOMPES.                                          
000400 DATE-WRITTEN.   97/02/06.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNCTION:                                                            
000800*        MAINTANCE STORAGE CODE TABEL                                     
000900*                                                                         
001000*        THE PROGRAM UPDATES   WL6315 (WDR2)                              
001100*                                                                         
001200*    INDATA.                                                              
001300*        TRANSACTION: W6T312                                              
001400*        MID:         W6I31201                                            
001500*                                                                         
001600*    OUTDATA.                                                             
001700*        MOD:         W6O31201                                            
001800                                                                          
001900     SKIP3                                                                
002000 ENVIRONMENT DIVISION.                                                    
002100     EJECT                                                                
002200 DATA DIVISION.                                                           
002300 WORKING-STORAGE SECTION.                                                 
002400                                                                          
002500*    -- CHECKED BY WY2000                                                 
002600 77  IDPGM                       PIC X(08)   VALUE 'W6031200'.            
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
004700 77  KEYS-SW                     PIC X       VALUE 'Y'.                   
004800     88  KEYS-OK                             VALUE 'Y'.                   
004900     88  KEYS-WRONG                          VALUE 'N'.                   
005000                                                                          
005100 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
005200     88  OWN-MID                             VALUE '6312'.                
005300     88  GOOD-MID                            VALUE '6312'.                
005400     88  HELP-MID                            VALUE '0551'.                
005500                                                                          
005600 01  WS-KDMATT                   PIC X.                                   
005700     88 US-MEASUREMENT           VALUE 'U'.                               
005800     88 SIS-MEASUREMENT          VALUE 'S'.                               
005900                                                                          
006000 77  WS-KDSTOR-IN                PIC X(3)   VALUE SPACE.                  
006100 77  WS-KDSTOR                   PIC X(3)   VALUE SPACE.                  
006200 77  WS-TESTORAGE                PIC X(18)  VALUE SPACE.                  
006300 77  WS-DISTORD                  PIC 9(3)V9(1).                           
006400 77  WS-DISTORB                  PIC 9(3)V9(1).                           
006500 77  WS-DISTORH                  PIC 9(3)V9(1).                           
006600 77  WS-KDVSOP1                  PIC 9(3)   VALUE ZERO.                   
006700 77  WS-KDVSOP2                  PIC 9(3)   VALUE ZERO.                   
006800 77  WS-KDVSOP3                  PIC 9(3)   VALUE ZERO.                   
006900 77  WS-KDVSOP4                  PIC 9(3)   VALUE ZERO.                   
007000 77  WS-KDVSOP5                  PIC 9(3)   VALUE ZERO.                   
007100       EJECT                                                              
007200*    --- SUBPROGRAMS AND PARAMETER AREAS                                  
007300 01  GENERAL-SUBPROGRAMS.                                                 
007400     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
007500     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
007600     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
007700     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
007800     03  WDECEDIT                PIC X(8)    VALUE 'WDECEDIT'.            
007900     03  WWOMVAND                PIC X(8)    VALUE 'WWOMVAND'.            
008000     EJECT                                                                
008100*    --- PARAMETERS FOR SUBPROGRAM WMEDKONV                               
008200*01 -COPY WMEDAREA                                                        
008300     SKIP3                                                                
008400 01  MESSAGE-CODES.                                                       
008500     03  ERR-CORR-HILITE-FLDS    PIC X(3)    VALUE '001'.                 
008600     03  INF-PRESS-PF11          PIC X(3)    VALUE '003'.                 
008700     03  INF-FIRST-PAGE          PIC X(3)    VALUE '006'.                 
008800     03  ERR-PF11-AND-NO-DATA    PIC X(3)    VALUE '011'.                 
008900     03  ITEMS-MISSING           PIC X(3)    VALUE '029'.                 
009000     03  INF-UPDATE-DONE         PIC X(3)    VALUE '101'.                 
009100     03  INF-MORE-INFO-EXISTS    PIC X(3)    VALUE '105'.                 
009200     03  INF-LAST-PAGE-SHOWN     PIC X(3)    VALUE '115'.                 
009300     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
009400     EJECT                                                                
009500*    --- PARAMETERS FOR SUB PROGRAM W005INIT                              
009600*                                                                         
009700 01  FILLER                      PIC X(16)   VALUE 'WMSGINIT'.            
009800     SKIP3                                                                
009900*01 -COPY WMSGINIT                                                        
010000*    --- PARAMETERS FOR SUB PROGRAM WDECAREA                              
010100*                                                                         
010200 01  FILLER                      PIC X(16)  VALUE 'WDECAREA '.            
010300*01    -COPY WDECAREA                                                     
010400     SKIP3                                                                
010500*                                                                         
010600 01  FILLER                      PIC X(16)  VALUE 'WWOMVAND '.            
010700*    -COPY WWOMVAND                                                       
010800     EJECT                                                                
010900*    --- AREA CONTAINING DATA TO BE SAVED BETWEEN DIALOG STEPS            
011000*                                                                         
011100 01  SAVE-AREA.                                                           
011200     03  SAVE-IDTRANS        PIC X(4)    VALUE  SPACE.                    
011300     03  SAVE-KDSTOR-ENTER   PIC X(3).                                    
011400     03  SAVE-KDSTOR-NEXT    PIC X(3).                                    
011500     EJECT                                                                
011600*    --- AREAS FOR MFS AND SCREEN MANAGEMENT                              
011700*                                                                         
011800 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
011900     SKIP3                                                                
012000*01  MID -COPY W6I31201                                                   
012100     EJECT                                                                
012200 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
012300     SKIP3                                                                
012400*01  -COPY WMSGAREA                                                       
012500     EJECT                                                                
012600     03  MOD REDEFINES MSG-AREA.                                          
012700*      05  -COPY W6O31201                                                 
012800     EJECT                                                                
012900 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
013000     SKIP3                                                                
013100*01  -COPY WMFSAREA                                                       
013200     EJECT                                                                
013300*    --- WORK-AREAS FOR IMS-SECTIONS                                      
013400*                                                                         
013500     EJECT                                                                
013600 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
013700     SKIP3                                                                
013800 01  KEYS-TO-DLI.                                                         
013900*    --- VALUE OF SCROLLING KEY FOR FIRST LINE ON THE SCREEN              
014000     03  W-KDSTOR-MIN-X.                                                  
014100          05  W-KDSTOR-MIN       PIC X(3).                                
014200                                                                          
014300     03  W-KDSTOR-MAX-X.                                                  
014400          05  W-KDSTOR-MAX       PIC X(3)    VALUE HIGH-VALUE.            
014500                                                                          
014600     03  W-WDGXKEY-6315-X.                                                
014700          05 W-6315-IDHTYP       PIC X(4)    VALUE '6315'.                
014800          05 W-6315-IDDC         PIC X(2)    VALUE SPACE.                 
014900          05 W-6315-LOWVALUE     PIC X(24)   VALUE LOW-VALUE.             
015000                                                                          
015100     03  W-WDGXKEY-6316-X.                                                
015200         05  W-KDSTOR            PIC X(3)    VALUE SPACE.                 
015300                                                                          
015400     03  W-IDDC-B6-X.                                                     
015500         05  W-IDDC-B6       PIC X(2)   VALUE SPACE.                      
015600     SKIP2                                                                
015700*    --- STATUS-KOD FRÅN IMS                                              
015800 01  STATUS-WS                   PIC XX.                                  
015900     88  SEGMENT-FOUND                       VALUE '  '.                  
016000     88  SEGMENT-FOUND-EXISTS                VALUE 'II'.                  
016100     88  SEGMENT-MISSING                     VALUE 'GE'.                  
016200     88  END-OF-DATABASE                     VALUE 'GB'.                  
016300     SKIP2                                                                
016400 01  GOOD-STATUSCODES.                                                    
016500     03  GOOD-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
016600     SKIP3                                                                
016700 01  SSA1                        PIC X(64).                               
016800 01  SSA2                        PIC X(64).                               
016900     EJECT                                                                
017000*    --- IMS FUNCTION CODES                                               
017100*01  -COPY W0003                                                          
017200     EJECT                                                                
017300*    ---  DLI INPUT-OUTPUT AREA                                           
017400                                                                          
017500 01  FILLER         PIC X(30) VALUE 'WL631501-AREA'.                      
017600 01  WL631501-AREA.                                                       
017700*    03  -COPY WDGX6315                                                   
017800     EJECT                                                                
017900 01  FILLER         PIC X(23) VALUE 'WL631511-AREA'.                      
018000 01  WL631511-AREA.                                                       
018100*    03  -COPY WDGX6316                                                   
018200     EJECT                                                                
018300 01  FILLER               PIC X(16)   VALUE 'WDB601 AREA'.                
018400 01   DLI-IO-AREA-B601.                                                   
018500*     03  -COPY WDB601                                                    
018600     EJECT                                                                
018700 LINKAGE SECTION.                                                         
018800*01  -COPY W0009   -PRE MSG-                                              
018900*01  -COPY W0008   -PRE USEA-                                             
019000     05  FILLER                  PIC X.                                   
019100     EJECT                                                                
019200*01  -COPY W0008  -PRE 6315-                                              
019300     05  FILLER                  PIC X.                                   
019400     EJECT                                                                
019500*01  -COPY W0008      -PRE WDB6-                                          
019600     05  FILLER                  PIC X.                                   
019700     EJECT                                                                
019800     EJECT                                                                
019900 PROCEDURE DIVISION  USING MSG-PCB USEA-PCB 6315-PCB                      
020000                           WDB6-PCB.                                      
020100 MAIN SECTION.                                                            
020200     ENTRY 'DLITCBL' USING MSG-PCB USEA-PCB 6315-PCB                      
020300                           WDB6-PCB.                                      
020400                                                                          
020500     PERFORM IMS-GET-MSG                                                  
020600     IF SEGMENT-FOUND                                                     
020700       PERFORM A-INIT                                                     
020800          PERFORM B-CHECK-KEYS                                            
020900          IF KEYS-OK                                                      
021000             IF MFS-UPDATE                                                
021100                PERFORM G-CHECK-INPUT                                     
021200                IF INDATA-OK                                              
021300                   PERFORM H-UPDATE                                       
021400                   PERFORM F-READ-SHOW-INFO                               
021500                 END-IF                                                   
021600              ELSE                                                        
021700               IF MFS-FIRST                                               
021800                  PERFORM C-FIRST-PAGE                                    
021900                ELSE                                                      
022000                  IF MFS-NEXT                                             
022100                     PERFORM D-NEXT-PAGE                                  
022200                   ELSE                                                   
022300                     PERFORM E-SAME-PAGE                                  
022400                  END-IF                                                  
022500               END-IF                                                     
022600               PERFORM F-READ-SHOW-INFO                                   
022700             END-IF                                                       
022800          END-IF                                                          
022900       COMPUTE MSG-KVLL = LENGTH OF MOD-W6O31201 + 4                      
023000       PERFORM IMS-INSERT-MSG                                             
023100     END-IF                                                               
023200                                                                          
023300     MOVE ZERO TO RETURN-CODE                                             
023400     GOBACK                                                               
023500                                                                          
023600     .                                                                    
023700     EJECT                                                                
023800 A-INIT SECTION.                                                          
023900                                                                          
024000     IF MSG-DOUBLE-TRANSACTIONS                                           
024100       MOVE MSG-INDATA-MINUS-2-TRANSACT   TO MID-W6I31201                 
024200       MOVE MSG-IDTRANS-2                 TO MFS-IDTRANS                  
024300       MOVE MSG-KDMFSFOR-2                TO MFS-KDMFSFOR                 
024400     ELSE                                                                 
024500       MOVE MSG-INDATA-MINUS-1-TRANSACT   TO MID-W6I31201                 
024600       MOVE MSG-IDTRANS-1                 TO MFS-IDTRANS                  
024700       MOVE MSG-KDMFSFOR-1                TO MFS-KDMFSFOR                 
024800     END-IF                                                               
024900                                                                          
025000     MOVE MSG-KDTRTYP      TO MFS-KDTRTYP                                 
025100     MOVE MSG-IDPFK        TO MFS-IDPFK                                   
025200     MOVE MFS-IDTRANS      TO W-IDTRANS                                   
025300                                                                          
025400     MOVE LOW-VALUE        TO MSG-AREA                                    
025500     MOVE 'W6O312N1'       TO MFS-IDMOD                                   
025600     MOVE '6312'           TO MOD-IDTRANS                                 
025700     MOVE MFS-ERASE-FIELD  TO MOD-TEMFSFEL MOD-TEMFSINF                   
025800                                                                          
025900     IF GOOD-MID OR HELP-MID                                              
026000       CONTINUE                                                           
026100     ELSE                                                                 
026200       MOVE SPACE                         TO MFS-KDTRTYP                  
026300       MOVE '7'                           TO MFS-IDPFK                    
026400       PERFORM MFS-INIT-KEY-FIELD-IN                                      
026500       PERFORM MFS-INIT-KEY-FIELD-OUT                                     
026600       PERFORM MFS-ERASE-FIELD-IN                                         
026700       PERFORM MFS-ERASE-LINE-FIELD-OUT                                   
026800     END-IF                                                               
026900                                                                          
027000     IF MID-KDSTOR-IN NOT = ALL '+'                                       
027100       MOVE '7'         TO MFS-IDPFK                                      
027200       MOVE SPACE       TO MFS-KDTRTYP                                    
027300     END-IF                                                               
027400     .                                                                    
027500     EJECT                                                                
027600 B-CHECK-KEYS SECTION.                                                    
027700                                                                          
027800                                                                          
027900     MOVE YES               TO KEYS-SW                                    
028000                                                                          
028100     MOVE MFS-ERASE-FIELD   TO MOD-KDSTOR-IN                              
028200                                                                          
028300     MOVE ALL '+'           TO MSGI-WMSGINIT                              
028400     MOVE '001'             TO MSGI-KDCALL                                
028500     MOVE MSG-LTERM-NAME    TO MSGI-IDLTERM-USER                          
028600     MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                                
028700     MOVE '6312'            TO MSGI-IDTRANS                               
028800                                                                          
028900     IF OWN-MID                                                           
029000        MOVE MID-KDSTOR-IN TO MSGI-KDSTOR                                 
029100     END-IF                                                               
029200                                                                          
029300     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
029400*                                                                         
029500*    -- CONTROL  ON STORAGE CODE                                          
029600     IF MSGI-KDSTOR     NUMERIC                                           
029700        MOVE 'A00' TO MSGI-KDSTOR                                         
029800     ELSE                                                                 
029900        IF MSGI-KDSTOR = SPACE                                            
030000          MOVE 'A00' TO MSGI-KDSTOR                                       
030100        END-IF                                                            
030200     END-IF                                                               
030300*    -- END OF CONTROL                                                    
030400*                                                                         
030500                                                                          
030600     MOVE MSGI-IDLAND-SPR   TO MED-IDSKYLT                                
030700     MOVE MSGI-KDMATT       TO WS-KDMATT                                  
030800     MOVE MSGI-SPAR-AREA    TO SAVE-AREA                                  
030900     MOVE MSGI-KDSTOR       TO WS-KDSTOR                                  
031000     MOVE MSGI-IDDC         TO W-IDDC-B6                                  
031100     PERFORM IMS-GU-WDB601                                                
031200                                                                          
031300     IF SEGMENT-FOUND                                                     
031400     AND (DCS-SDC                                                         
031500     OR   DCS-NDC-NA                                                      
031600     OR   DCS-NDC-PF                                                      
031700     OR   DCS-NDC-OTHERS)                                                 
031800       CONTINUE                                                           
031900     ELSE                                                                 
032000       MOVE NOO                 TO KEYS-SW                                
032100     END-IF                                                               
032200                                                                          
032300     MOVE WS-KDSTOR             TO MOD-KDSTOR-UT                          
032400     MOVE W-IDDC-B6             TO MOD-IDDC                               
032500                                                                          
032600     IF KEYS-WRONG                                                        
032700       MOVE ERR-WRONG-KEY TO MED-IDMFSFEL                                 
032800       CALL WMEDKONV USING MED-WMEDAREA                                   
032900       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
033000       PERFORM MFS-ERASE-FIELD-IN                                         
033100       PERFORM MFS-ERASE-LINE-FIELD-OUT                                   
033200     END-IF                                                               
033300     .                                                                    
033400     EJECT                                                                
033500 C-FIRST-PAGE SECTION.                                                    
033600                                                                          
033700     MOVE INF-FIRST-PAGE TO MED-IDMFSINF                                  
033800     CALL WMEDKONV USING MED-WMEDAREA                                     
033900     MOVE MED-MFSINF     TO MOD-TEMFSINF                                  
034000                                                                          
034100     PERFORM MFS-ERASE-FIELD-IN                                           
034200     MOVE WS-KDSTOR      TO W-KDSTOR                                      
034300     .                                                                    
034400     EJECT                                                                
034500 D-NEXT-PAGE SECTION.                                                     
034600                                                                          
034700     IF SAVE-IDTRANS = '6312'                                             
034800       MOVE SAVE-KDSTOR-NEXT TO W-KDSTOR                                  
034900     ELSE                                                                 
035000       MOVE SPACES           TO W-KDSTOR                                  
035100     END-IF                                                               
035200     .                                                                    
035300     EJECT                                                                
035400 E-SAME-PAGE SECTION.                                                     
035500                                                                          
035600     IF OWN-MID OR HELP-MID                                               
035700        MOVE SAVE-KDSTOR-ENTER TO W-KDSTOR                                
035800                                  MOD-KDSTOR-UT                           
035900        IF MID-KDSTOR-MAIN      = ALL '+' AND                             
036000           MID-TESTORAGE-MAIN   = ALL '+' AND                             
036100           MID-DISTORD-MAIN     = ALL '+' AND                             
036200           MID-DISTORB-MAIN     = ALL '+' AND                             
036300           MID-DISTORH-MAIN     = ALL '+' AND                             
036400           MID-KDVSOP-MAIN(1)   = ALL '+' AND                             
036500           MID-KDVSOP-MAIN(2)   = ALL '+' AND                             
036600           MID-KDVSOP-MAIN(3)   = ALL '+' AND                             
036700           MID-KDVSOP-MAIN(4)   = ALL '+' AND                             
036800           MID-KDVSOP-MAIN(5)   = ALL '+' AND                             
036900           MID-KDANDR-MAIN      = ALL '+'                                 
037000           PERFORM MFS-ERASE-FIELD-IN                                     
037100         ELSE                                                             
037200           MOVE INF-PRESS-PF11 TO MED-IDMFSINF                            
037300           CALL WMEDKONV USING MED-WMEDAREA                               
037400           MOVE MED-MFSINF TO MOD-TEMFSINF                                
037500           PERFORM EA-MID-INDATA-TO-MOD                                   
037600         END-IF                                                           
037700      ELSE                                                                
037800        PERFORM MFS-ERASE-FIELD-IN                                        
037900      END-IF                                                              
038000     .                                                                    
038100     EJECT                                                                
038200 EA-MID-INDATA-TO-MOD SECTION.                                            
038300                                                                          
038400     IF MID-KDVSOP-MAIN (1)  = ALL '+'                                    
038500        MOVE MFS-ERASE-FIELD        TO MOD-KDVSOP1-ATTR                   
038600      ELSE                                                                
038700        MOVE MFS-DO-NOT-TOUCH-FIELD TO MOD-KDVSOP1-MAIN                   
038800        MOVE MFS-ADD-READ-FIELD     TO MOD-KDVSOP1-ATTR                   
038900     END-IF                                                               
039000                                                                          
039100     IF MID-KDVSOP-MAIN (2)  = ALL '+'                                    
039200        MOVE MFS-ERASE-FIELD        TO MOD-KDVSOP2-ATTR                   
039300      ELSE                                                                
039400        MOVE MFS-DO-NOT-TOUCH-FIELD TO MOD-KDVSOP2-MAIN                   
039500        MOVE MFS-ADD-READ-FIELD     TO MOD-KDVSOP2-ATTR                   
039600     END-IF                                                               
039700                                                                          
039800     IF MID-KDVSOP-MAIN (3) = ALL '+'                                     
039900        MOVE MFS-ERASE-FIELD        TO MOD-KDVSOP3-ATTR                   
040000      ELSE                                                                
040100        MOVE MFS-DO-NOT-TOUCH-FIELD TO MOD-KDVSOP3-MAIN                   
040200        MOVE MFS-ADD-READ-FIELD     TO MOD-KDVSOP3-ATTR                   
040300     END-IF                                                               
040400                                                                          
040500     IF MID-KDVSOP-MAIN (4)  = ALL '+'                                    
040600        MOVE MFS-ERASE-FIELD        TO MOD-KDVSOP4-ATTR                   
040700      ELSE                                                                
040800        MOVE MFS-DO-NOT-TOUCH-FIELD TO MOD-KDVSOP4-MAIN                   
040900        MOVE MFS-ADD-READ-FIELD     TO MOD-KDVSOP4-ATTR                   
041000     END-IF                                                               
041100                                                                          
041200     IF MID-KDVSOP-MAIN (5)  = ALL '+'                                    
041300        MOVE MFS-ERASE-FIELD        TO MOD-KDVSOP5-ATTR                   
041400      ELSE                                                                
041500        MOVE MFS-DO-NOT-TOUCH-FIELD TO MOD-KDVSOP5-MAIN                   
041600        MOVE MFS-ADD-READ-FIELD     TO MOD-KDVSOP5-ATTR                   
041700     END-IF                                                               
041800                                                                          
041900     IF MID-DISTORH-MAIN = ALL '+'                                        
042000        MOVE MFS-ERASE-FIELD        TO MOD-DISTORH-ATTR                   
042100      ELSE                                                                
042200        MOVE MFS-DO-NOT-TOUCH-FIELD TO MOD-DISTORH-MAIN                   
042300        MOVE MFS-ADD-READ-FIELD     TO MOD-DISTORH-ATTR                   
042400     END-IF                                                               
042500                                                                          
042600     IF MID-DISTORB-MAIN = ALL '+'                                        
042700        MOVE MFS-ERASE-FIELD        TO MOD-DISTORB-ATTR                   
042800      ELSE                                                                
042900        MOVE MFS-DO-NOT-TOUCH-FIELD TO MOD-DISTORB-MAIN                   
043000        MOVE MFS-ADD-READ-FIELD     TO MOD-DISTORB-ATTR                   
043100     END-IF                                                               
043200                                                                          
043300     IF MID-DISTORD-MAIN = ALL '+'                                        
043400        MOVE MFS-ERASE-FIELD        TO MOD-DISTORD-ATTR                   
043500      ELSE                                                                
043600        MOVE MFS-DO-NOT-TOUCH-FIELD TO MOD-DISTORD-MAIN                   
043700        MOVE MFS-ADD-READ-FIELD     TO MOD-DISTORD-ATTR                   
043800     END-IF                                                               
043900                                                                          
044000     IF MID-TESTORAGE-MAIN = ALL '+'                                      
044100        MOVE MFS-ERASE-FIELD        TO MOD-TESTORAGE-ATTR                 
044200      ELSE                                                                
044300        MOVE MFS-DO-NOT-TOUCH-FIELD TO MOD-TESTORAGE-MAIN                 
044400        MOVE MFS-ADD-READ-FIELD     TO MOD-TESTORAGE-ATTR                 
044500     END-IF                                                               
044600                                                                          
044700     IF MID-KDANDR-MAIN = ALL '+'                                         
044800        MOVE MFS-ERASE-FIELD        TO MOD-KDANDR-ATTR                    
044900      ELSE                                                                
045000        MOVE MFS-DO-NOT-TOUCH-FIELD TO MOD-KDANDR-MAIN                    
045100        MOVE MFS-ADD-READ-FIELD     TO MOD-KDANDR-ATTR                    
045200     END-IF                                                               
045300                                                                          
045400     IF MID-KDSTOR-MAIN = ALL '+'                                         
045500        MOVE MFS-ERASE-FIELD        TO MOD-KDSTOR-ATTR                    
045600      ELSE                                                                
045700        MOVE MFS-DO-NOT-TOUCH-FIELD TO MOD-KDSTOR-MAIN                    
045800        MOVE MFS-ADD-READ-FIELD     TO MOD-KDSTOR-ATTR                    
045900     END-IF                                                               
046000     .                                                                    
046100     EJECT                                                                
046200 F-READ-SHOW-INFO SECTION.                                                
046300                                                                          
046400     MOVE W-IDDC-B6    TO  W-6315-IDDC                                    
046500                                                                          
046600     PERFORM  IMS-GU-6315-6316                                            
046700                                                                          
046800     IF SEGMENT-MISSING                                                   
046900        MOVE ITEMS-MISSING TO MED-IDMFSFEL                                
047000        CALL WMEDKONV USING MED-WMEDAREA                                  
047100        MOVE MED-MFSFEL    TO MOD-TEMFSFEL                                
047200        PERFORM MFS-ERASE-LINE-FIELD-OUT                                  
047300        MOVE WS-KDSTOR     TO SAVE-KDSTOR-ENTER                           
047400        MOVE WS-KDSTOR     TO SAVE-KDSTOR-NEXT                            
047500     ELSE                                                                 
047600        MOVE 6316-KDSTOR   TO SAVE-KDSTOR-ENTER                           
047700        MOVE +1            TO INDX                                        
047800        PERFORM UNTIL SEGMENT-MISSING OR END-OF-DATABASE OR               
047900                      INDX > MAX-INDX                                     
048000         MOVE 6316-KDSTOR         TO MOD-KDSTOR-LINE   (INDX)             
048100         MOVE 6316-DISTORD        TO WS-DISTORD                           
048200         MOVE 6316-DISTORB        TO WS-DISTORB                           
048300         MOVE 6316-DISTORH        TO WS-DISTORH                           
048400         PERFORM S01-EV-CONVERT-TO-US-MEASURE                             
048500         MOVE WS-DISTORD          TO MOD-DISTORD-LINE   (INDX)            
048600         MOVE WS-DISTORB          TO MOD-DISTORB-LINE   (INDX)            
048700         MOVE WS-DISTORH          TO MOD-DISTORH-LINE   (INDX)            
048800         MOVE 6316-TESTORAGE      TO MOD-TESTORAGE-LINE (INDX)            
048900         IF 6316-KDVSOP(1) = 0                                            
049000            MOVE SPACE               TO MOD-KDVSOP1-LINE   (INDX)         
049100          ELSE                                                            
049200            MOVE 6316-KDVSOP(1)      TO MOD-KDVSOP1-LINE   (INDX)         
049300         END-IF                                                           
049400         IF 6316-KDVSOP(2) = 0                                            
049500            MOVE SPACE               TO MOD-KDVSOP2-LINE   (INDX)         
049600          ELSE                                                            
049700            MOVE 6316-KDVSOP(2)      TO MOD-KDVSOP2-LINE   (INDX)         
049800         END-IF                                                           
049900         IF 6316-KDVSOP(3) = 0                                            
050000            MOVE SPACE               TO MOD-KDVSOP3-LINE   (INDX)         
050100          ELSE                                                            
050200            MOVE 6316-KDVSOP(3)      TO MOD-KDVSOP3-LINE   (INDX)         
050300         END-IF                                                           
050400         IF 6316-KDVSOP(4) = 0                                            
050500            MOVE SPACE               TO MOD-KDVSOP4-LINE   (INDX)         
050600          ELSE                                                            
050700             MOVE 6316-KDVSOP(4)     TO MOD-KDVSOP4-LINE   (INDX)         
050800         END-IF                                                           
050900         IF 6316-KDVSOP(5) = 0                                            
051000            MOVE SPACE               TO MOD-KDVSOP5-LINE   (INDX)         
051100          ELSE                                                            
051200             MOVE 6316-KDVSOP(5)     TO MOD-KDVSOP5-LINE   (INDX)         
051300         END-IF                                                           
051400         PERFORM  IMS-GN-6315-6316                                        
051500         ADD 1 TO INDX                                                    
051600        END-PERFORM                                                       
051700        IF SEGMENT-FOUND                                                  
051800           MOVE 6316-KDSTOR  TO SAVE-KDSTOR-NEXT                          
051900           MOVE INF-MORE-INFO-EXISTS TO MED-IDMFSINF                      
052000           CALL WMEDKONV USING MED-WMEDAREA                               
052100           MOVE MED-TEMFSINF TO MOD-TEMFSINF                              
052200         ELSE                                                             
052300           MOVE 6316-KDSTOR         TO SAVE-KDSTOR-NEXT                   
052400           MOVE INF-LAST-PAGE-SHOWN TO    MED-IDMFSFEL                    
052500           CALL WMEDKONV            USING MED-WMEDAREA                    
052600           MOVE MED-MFSFEL          TO    MOD-TEMFSFEL                    
052700           PERFORM UNTIL INDX > MAX-INDX                                  
052800            MOVE MFS-ERASE-FIELD     TO MOD-KDSTOR-LINE    (INDX)         
052900                                        MOD-TESTORAGE-LINE (INDX)         
053000                                        MOD-DISTORD-LINE   (INDX)         
053100                                        MOD-DISTORB-LINE   (INDX)         
053200                                        MOD-DISTORH-LINE   (INDX)         
053300                                        MOD-KDVSOP1-LINE   (INDX)         
053400                                        MOD-KDVSOP2-LINE   (INDX)         
053500                                        MOD-KDVSOP3-LINE   (INDX)         
053600                                        MOD-KDVSOP4-LINE   (INDX)         
053700                                        MOD-KDVSOP5-LINE   (INDX)         
053800            ADD 1 TO INDX                                                 
053900           END-PERFORM                                                    
054000         END-IF                                                           
054100                                                                          
054200       MOVE '002'      TO MSGI-KDCALL                                     
054300       MOVE '6312'     TO SAVE-IDTRANS                                    
054400       MOVE SAVE-AREA  TO MSGI-SPAR-AREA                                  
054500       CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                         
054600     END-IF                                                               
054700     .                                                                    
054800     EJECT                                                                
054900 G-CHECK-INPUT SECTION.                                                   
055000                                                                          
055100     MOVE YES  TO INDATA-SW                                               
055200                                                                          
055300     IF MID-KDSTOR-MAIN      = ALL '+' AND                                
055400        MID-TESTORAGE-MAIN   = ALL '+' AND                                
055500        MID-DISTORD-MAIN     = ALL '+' AND                                
055600        MID-DISTORB-MAIN     = ALL '+' AND                                
055700        MID-DISTORH-MAIN     = ALL '+' AND                                
055800        MID-KDVSOP-MAIN(1)   = ALL '+' AND                                
055900        MID-KDVSOP-MAIN(2)   = ALL '+' AND                                
056000        MID-KDVSOP-MAIN(3)   = ALL '+' AND                                
056100        MID-KDVSOP-MAIN(4)   = ALL '+' AND                                
056200        MID-KDVSOP-MAIN(5)   = ALL '+' AND                                
056300        MID-KDANDR-MAIN      = ALL '+'                                    
056400          MOVE ERR-PF11-AND-NO-DATA TO MED-IDMFSFEL                       
056500          CALL WMEDKONV USING MED-WMEDAREA                                
056600          MOVE MED-MFSFEL TO MOD-TEMFSFEL                                 
056700          MOVE NOO TO INDATA-SW                                           
056800     END-IF                                                               
056900     IF INDATA-OK                                                         
057000        IF MID-KDSTOR-MAIN = ALL '+'                                      
057100           MOVE MFS-ALPHA-FIELD-WRONG  TO MOD-KDSTOR-ATTR                 
057200           MOVE NOO                    TO INDATA-SW                       
057300         ELSE                                                             
057400           MOVE MFS-ALPHA-FIELD-OK     TO MOD-KDSTOR-ATTR                 
057500           MOVE MID-KDSTOR-MAIN        TO WS-KDSTOR                       
057600                                          W-KDSTOR                        
057700        END-IF                                                            
057800        IF MID-KDANDR-MAIN = ALL '+'                                      
057900           MOVE MFS-ALPHA-FIELD-WRONG  TO MOD-KDANDR-ATTR                 
058000           MOVE NOO                    TO INDATA-SW                       
058100         ELSE                                                             
058200           IF MID-KDANDR-MAIN = NEW OR DEL OR CHG                         
058300              MOVE MFS-ALPHA-FIELD-OK        TO MOD-KDANDR-ATTR           
058400           ELSE                                                           
058500              MOVE MFS-ALPHA-FIELD-WRONG     TO MOD-KDANDR-ATTR           
058600              MOVE NOO                       TO INDATA-SW                 
058700           END-IF                                                         
058800        END-IF                                                            
058900        IF INDATA-WRONG                                                   
059000           PERFORM GD-MID-INDATA-TO-MOD                                   
059100           MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL                      
059200           CALL WMEDKONV USING MED-WMEDAREA                               
059300           MOVE MED-MFSFEL           TO MOD-TEMFSFEL                      
059400        END-IF                                                            
059500     END-IF                                                               
059600                                                                          
059700     IF INDATA-OK                                                         
059800        IF MID-KDANDR-MAIN = NEW                                          
059900           PERFORM GA-CHECK-INPUT-NEW                                     
060000        END-IF                                                            
060100        IF MID-KDANDR-MAIN = CHG                                          
060200           PERFORM GB-CHECK-INPUT-CHANGE                                  
060300        END-IF                                                            
060400        IF MID-KDANDR-MAIN = DEL                                          
060500           PERFORM GC-CHECK-INPUT-DELETE                                  
060600        END-IF                                                            
060700        IF INDATA-WRONG                                                   
060800           MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL                      
060900           CALL WMEDKONV USING MED-WMEDAREA                               
061000           MOVE MED-MFSFEL           TO MOD-TEMFSFEL                      
061100        END-IF                                                            
061200     END-IF                                                               
061300                                                                          
061400     IF INDATA-WRONG                                                      
061500        PERFORM MFS-DONT-TOUCH-FIELD-OUT                                  
061600     END-IF                                                               
061700                                                                          
061800     .                                                                    
061900     EJECT                                                                
062000 GA-CHECK-INPUT-NEW SECTION.                                              
062100                                                                          
062200*                                                                         
062300*   --- CHECK VSOP CODE                                                   
062400*                                                                         
062500     IF MID-KDVSOP-MAIN (1) = ALL '+'                                     
062600        MOVE ZEROES                 TO WS-KDVSOP1                         
062700        MOVE MFS-NUM-FIELD-OK       TO MOD-KDVSOP1-ATTR                   
062800       ELSE                                                               
062900        IF MID-KDVSOP-MAIN (1) NUMERIC                                    
063000          MOVE MID-KDVSOP-MAIN (1)  TO WS-KDVSOP1                         
063100          MOVE MFS-NUM-FIELD-OK     TO MOD-KDVSOP1-ATTR                   
063200         ELSE                                                             
063300          MOVE MFS-NUM-FIELD-WRONG  TO MOD-KDVSOP1-ATTR                   
063400          MOVE NOO                  TO INDATA-SW                          
063500        END-IF                                                            
063600     END-IF                                                               
063700                                                                          
063800     IF MID-KDVSOP-MAIN (2) = ALL '+'                                     
063900        MOVE ZEROES                 TO WS-KDVSOP2                         
064000        MOVE MFS-NUM-FIELD-OK       TO MOD-KDVSOP2-ATTR                   
064100       ELSE                                                               
064200        IF MID-KDVSOP-MAIN (2) NUMERIC                                    
064300          MOVE MID-KDVSOP-MAIN (2)  TO WS-KDVSOP2                         
064400          MOVE MFS-NUM-FIELD-OK     TO MOD-KDVSOP2-ATTR                   
064500         ELSE                                                             
064600          MOVE MFS-NUM-FIELD-WRONG  TO MOD-KDVSOP2-ATTR                   
064700          MOVE NOO                  TO INDATA-SW                          
064800        END-IF                                                            
064900     END-IF                                                               
065000                                                                          
065100     IF MID-KDVSOP-MAIN (3) = ALL '+'                                     
065200        MOVE ZEROES                 TO WS-KDVSOP3                         
065300        MOVE MFS-NUM-FIELD-OK       TO MOD-KDVSOP3-ATTR                   
065400       ELSE                                                               
065500        IF MID-KDVSOP-MAIN (3) NUMERIC                                    
065600          MOVE MID-KDVSOP-MAIN (3)  TO WS-KDVSOP3                         
065700          MOVE MFS-NUM-FIELD-OK     TO MOD-KDVSOP3-ATTR                   
065800         ELSE                                                             
065900          MOVE MFS-NUM-FIELD-WRONG  TO MOD-KDVSOP3-ATTR                   
066000          MOVE NOO                  TO INDATA-SW                          
066100        END-IF                                                            
066200     END-IF                                                               
066300                                                                          
066400     IF MID-KDVSOP-MAIN (4) = ALL '+'                                     
066500        MOVE ZEROES                 TO WS-KDVSOP4                         
066600        MOVE MFS-NUM-FIELD-OK       TO MOD-KDVSOP4-ATTR                   
066700       ELSE                                                               
066800        IF MID-KDVSOP-MAIN (4) NUMERIC                                    
066900          MOVE MID-KDVSOP-MAIN (4)  TO WS-KDVSOP4                         
067000          MOVE MFS-NUM-FIELD-OK     TO MOD-KDVSOP4-ATTR                   
067100         ELSE                                                             
067200          MOVE MFS-NUM-FIELD-WRONG  TO MOD-KDVSOP4-ATTR                   
067300          MOVE NOO                  TO INDATA-SW                          
067400        END-IF                                                            
067500     END-IF                                                               
067600                                                                          
067700     IF MID-KDVSOP-MAIN (5) = ALL '+'                                     
067800        MOVE ZEROES                 TO WS-KDVSOP5                         
067900        MOVE MFS-NUM-FIELD-OK       TO MOD-KDVSOP5-ATTR                   
068000       ELSE                                                               
068100        IF MID-KDVSOP-MAIN (5) NUMERIC                                    
068200          MOVE MID-KDVSOP-MAIN (5)  TO WS-KDVSOP5                         
068300          MOVE MFS-NUM-FIELD-OK     TO MOD-KDVSOP5-ATTR                   
068400         ELSE                                                             
068500          MOVE MFS-NUM-FIELD-WRONG  TO MOD-KDVSOP5-ATTR                   
068600          MOVE NOO                  TO INDATA-SW                          
068700        END-IF                                                            
068800     END-IF                                                               
068900*                                                                         
069000*   --- CHECK STORAGE  DESCRIPTION                                        
069100*                                                                         
069200     IF MID-TESTORAGE-MAIN = ALL '+'                                      
069300        MOVE SPACE                  TO WS-TESTORAGE                       
069400        MOVE MFS-ALPHA-FIELD-OK     TO MOD-TESTORAGE-ATTR                 
069500       ELSE                                                               
069600        MOVE MID-TESTORAGE-MAIN     TO WS-TESTORAGE                       
069700        MOVE MFS-ALPHA-FIELD-OK     TO MOD-TESTORAGE-ATTR                 
069800     END-IF                                                               
069900*                                                                         
070000*   --- CHECK DEPTH OF STORAGE                                            
070100*                                                                         
070200     IF MID-DISTORH-MAIN = ALL '+'                                        
070300        MOVE MFS-ALPHA-FIELD-WRONG  TO MOD-DISTORH-ATTR                   
070400        MOVE NOO                    TO INDATA-SW                          
070500       ELSE                                                               
070600        MOVE MID-DISTORH-MAIN       TO DEC-IDFRIDATA                      
070700        MOVE 3                      TO DEC-KVHELTAL                       
070800        MOVE 1                      TO DEC-KVDECIMAL                      
070900        CALL WDECEDIT USING DEC-WDECAREA                                  
071000        END-CALL                                                          
071100        IF DEC-KDSVAR-OK                                                  
071200          MOVE MFS-ALPHA-FIELD-OK   TO MOD-DISTORH-ATTR                   
071300          MOVE DEC-IDEDITDATA        TO WS-DISTORH                        
071400         ELSE                                                             
071500          MOVE MFS-ALPHA-FIELD-WRONG TO MOD-DISTORH-ATTR                  
071600          MOVE NOO                   TO INDATA-SW                         
071700        END-IF                                                            
071800     END-IF                                                               
071900*                                                                         
072000*   --- CHECK WIDTH OF STORAGE                                            
072100*                                                                         
072200     IF MID-DISTORB-MAIN = ALL '+'                                        
072300        MOVE MFS-ALPHA-FIELD-WRONG  TO MOD-DISTORB-ATTR                   
072400        MOVE NOO                    TO INDATA-SW                          
072500       ELSE                                                               
072600        MOVE MID-DISTORB-MAIN       TO DEC-IDFRIDATA                      
072700        MOVE 3                      TO DEC-KVHELTAL                       
072800        MOVE 1                      TO DEC-KVDECIMAL                      
072900        CALL WDECEDIT USING DEC-WDECAREA                                  
073000        END-CALL                                                          
073100        IF DEC-KDSVAR-OK                                                  
073200          MOVE MFS-ALPHA-FIELD-OK   TO MOD-DISTORB-ATTR                   
073300          MOVE DEC-IDEDITDATA        TO WS-DISTORB                        
073400         ELSE                                                             
073500          MOVE MFS-ALPHA-FIELD-WRONG TO MOD-DISTORB-ATTR                  
073600          MOVE NOO                   TO INDATA-SW                         
073700        END-IF                                                            
073800     END-IF                                                               
073900*                                                                         
074000*   --- CHECK LENGTH OF STORAGE                                           
074100*                                                                         
074200     IF MID-DISTORD-MAIN = ALL '+'                                        
074300        MOVE MFS-ALPHA-FIELD-WRONG  TO MOD-DISTORD-ATTR                   
074400        MOVE NOO                    TO INDATA-SW                          
074500       ELSE                                                               
074600        MOVE MID-DISTORD-MAIN       TO DEC-IDFRIDATA                      
074700        MOVE 3                      TO DEC-KVHELTAL                       
074800        MOVE 1                      TO DEC-KVDECIMAL                      
074900        CALL WDECEDIT USING DEC-WDECAREA                                  
075000        END-CALL                                                          
075100        IF DEC-KDSVAR-OK                                                  
075200          MOVE MFS-ALPHA-FIELD-OK   TO MOD-DISTORD-ATTR                   
075300          MOVE DEC-IDEDITDATA        TO WS-DISTORD                        
075400         ELSE                                                             
075500          MOVE MFS-ALPHA-FIELD-WRONG TO MOD-DISTORD-ATTR                  
075600          MOVE NOO                   TO INDATA-SW                         
075700        END-IF                                                            
075800     END-IF                                                               
075900     .                                                                    
076000                                                                          
076100*                                                                         
076200*   --- CHECK STORAGE ALREADY EXIST                                       
076300*                                                                         
076400     MOVE W-IDDC-B6                  TO W-6315-IDDC                       
076500     MOVE MID-KDSTOR-MAIN            TO W-KDSTOR                          
076600                                                                          
076700     PERFORM  IMS-GU-6316                                                 
076800                                                                          
076900     IF SEGMENT-FOUND                                                     
077000        MOVE NOO                     TO INDATA-SW                         
077100        MOVE MFS-ALPHA-FIELD-WRONG   TO MOD-KDSTOR-ATTR                   
077200       ELSE                                                               
077300        MOVE MFS-ALPHA-FIELD-OK      TO MOD-KDSTOR-ATTR                   
077400        MOVE MID-KDSTOR-MAIN         TO WS-KDSTOR                         
077500     END-IF                                                               
077600                                                                          
077700     .                                                                    
077800     EJECT                                                                
077900 GB-CHECK-INPUT-CHANGE SECTION.                                           
078000                                                                          
078100*                                                                         
078200*   --- CHECK VSOP CODE                                                   
078300*                                                                         
078400     IF MID-KDVSOP-MAIN (1) =  ALL '+'                                    
078500        MOVE MFS-NUM-FIELD-OK       TO MOD-KDVSOP1-ATTR                   
078600       ELSE                                                               
078700        IF MID-KDVSOP-MAIN (1) NUMERIC                                    
078800          MOVE MID-KDVSOP-MAIN (1)  TO WS-KDVSOP1                         
078900          MOVE MFS-NUM-FIELD-OK     TO MOD-KDVSOP1-ATTR                   
079000         ELSE                                                             
079100          MOVE MFS-NUM-FIELD-WRONG  TO MOD-KDVSOP1-ATTR                   
079200          MOVE NOO                  TO INDATA-SW                          
079300        END-IF                                                            
079400     END-IF                                                               
079500                                                                          
079600     IF MID-KDVSOP-MAIN (2) =  ALL '+'                                    
079700        MOVE MFS-NUM-FIELD-OK       TO MOD-KDVSOP2-ATTR                   
079800       ELSE                                                               
079900        IF MID-KDVSOP-MAIN (2) NUMERIC                                    
080000          MOVE MID-KDVSOP-MAIN (2)  TO WS-KDVSOP2                         
080100          MOVE MFS-NUM-FIELD-OK     TO MOD-KDVSOP2-ATTR                   
080200         ELSE                                                             
080300          MOVE MFS-NUM-FIELD-WRONG  TO MOD-KDVSOP2-ATTR                   
080400          MOVE NOO                  TO INDATA-SW                          
080500        END-IF                                                            
080600     END-IF                                                               
080700                                                                          
080800     IF MID-KDVSOP-MAIN (3) =  ALL '+'                                    
080900        MOVE MFS-NUM-FIELD-OK       TO MOD-KDVSOP3-ATTR                   
081000       ELSE                                                               
081100        IF MID-KDVSOP-MAIN (3) NUMERIC                                    
081200          MOVE MID-KDVSOP-MAIN (3)  TO WS-KDVSOP3                         
081300          MOVE MFS-NUM-FIELD-OK     TO MOD-KDVSOP3-ATTR                   
081400         ELSE                                                             
081500          MOVE MFS-NUM-FIELD-WRONG  TO MOD-KDVSOP3-ATTR                   
081600          MOVE NOO                  TO INDATA-SW                          
081700        END-IF                                                            
081800     END-IF                                                               
081900                                                                          
082000     IF MID-KDVSOP-MAIN (4) =  ALL '+'                                    
082100        MOVE MFS-NUM-FIELD-OK       TO MOD-KDVSOP4-ATTR                   
082200       ELSE                                                               
082300        IF MID-KDVSOP-MAIN (4) NUMERIC                                    
082400          MOVE MID-KDVSOP-MAIN (4)  TO WS-KDVSOP4                         
082500          MOVE MFS-NUM-FIELD-OK     TO MOD-KDVSOP4-ATTR                   
082600         ELSE                                                             
082700          MOVE MFS-NUM-FIELD-WRONG  TO MOD-KDVSOP4-ATTR                   
082800          MOVE NOO                  TO INDATA-SW                          
082900        END-IF                                                            
083000     END-IF                                                               
083100                                                                          
083200     IF MID-KDVSOP-MAIN (5) =  ALL '+'                                    
083300        MOVE MFS-NUM-FIELD-OK       TO MOD-KDVSOP5-ATTR                   
083400       ELSE                                                               
083500        IF MID-KDVSOP-MAIN (5) NUMERIC                                    
083600          MOVE MID-KDVSOP-MAIN (5)  TO WS-KDVSOP5                         
083700          MOVE MFS-NUM-FIELD-OK     TO MOD-KDVSOP5-ATTR                   
083800         ELSE                                                             
083900          MOVE MFS-NUM-FIELD-WRONG  TO MOD-KDVSOP5-ATTR                   
084000          MOVE NOO                  TO INDATA-SW                          
084100        END-IF                                                            
084200     END-IF                                                               
084300                                                                          
084400*                                                                         
084500*   --- CHECK STORAGE  DESCRIPTION                                        
084600*                                                                         
084700     IF MID-TESTORAGE-MAIN = ALL '+'                                      
084800        MOVE MFS-ALPHA-FIELD-OK     TO MOD-TESTORAGE-ATTR                 
084900       ELSE                                                               
085000        MOVE MID-TESTORAGE-MAIN     TO WS-TESTORAGE                       
085100        MOVE MFS-ALPHA-FIELD-OK     TO MOD-TESTORAGE-ATTR                 
085200     END-IF                                                               
085300*                                                                         
085400*   --- CHECK STORAGE DEPTH                                               
085500*                                                                         
085600     IF MID-DISTORH-MAIN NOT = ALL '+'                                    
085700        MOVE MID-DISTORH-MAIN       TO DEC-IDFRIDATA                      
085800        MOVE 3                      TO DEC-KVHELTAL                       
085900        MOVE 1                      TO DEC-KVDECIMAL                      
086000        CALL WDECEDIT USING DEC-WDECAREA                                  
086100        END-CALL                                                          
086200        IF DEC-KDSVAR-OK                                                  
086300          MOVE MFS-ALPHA-FIELD-OK   TO MOD-DISTORH-ATTR                   
086400          MOVE DEC-IDEDITDATA        TO WS-DISTORH                        
086500         ELSE                                                             
086600          MOVE MFS-ALPHA-FIELD-WRONG TO MOD-DISTORH-ATTR                  
086700          MOVE NOO                   TO INDATA-SW                         
086800        END-IF                                                            
086900     END-IF                                                               
087000*                                                                         
087100*   --- CHECK STORAGE WIDTH                                               
087200*                                                                         
087300     IF MID-DISTORB-MAIN NOT = ALL '+'                                    
087400        MOVE MID-DISTORB-MAIN       TO DEC-IDFRIDATA                      
087500        MOVE 3                      TO DEC-KVHELTAL                       
087600        MOVE 1                      TO DEC-KVDECIMAL                      
087700        CALL WDECEDIT USING DEC-WDECAREA                                  
087800        END-CALL                                                          
087900        IF DEC-KDSVAR-OK                                                  
088000          MOVE MFS-ALPHA-FIELD-OK   TO MOD-DISTORB-ATTR                   
088100          MOVE DEC-IDEDITDATA        TO WS-DISTORB                        
088200         ELSE                                                             
088300          MOVE MFS-ALPHA-FIELD-WRONG TO MOD-DISTORB-ATTR                  
088400          MOVE NOO                   TO INDATA-SW                         
088500        END-IF                                                            
088600     END-IF                                                               
088700*                                                                         
088800*   --- CHECK STORAGE LENGTH                                              
088900*                                                                         
089000     IF MID-DISTORD-MAIN NOT = ALL '+'                                    
089100        MOVE MID-DISTORD-MAIN       TO DEC-IDFRIDATA                      
089200        MOVE 3                      TO DEC-KVHELTAL                       
089300        MOVE 1                      TO DEC-KVDECIMAL                      
089400        CALL WDECEDIT USING DEC-WDECAREA                                  
089500        END-CALL                                                          
089600        IF DEC-KDSVAR-OK                                                  
089700          MOVE MFS-ALPHA-FIELD-OK   TO MOD-DISTORD-ATTR                   
089800          MOVE DEC-IDEDITDATA        TO WS-DISTORD                        
089900         ELSE                                                             
090000          MOVE MFS-ALPHA-FIELD-WRONG TO MOD-DISTORD-ATTR                  
090100          MOVE NOO                   TO INDATA-SW                         
090200        END-IF                                                            
090300     END-IF                                                               
090400                                                                          
090500*                                                                         
090600*   --- CHECK IF STORAGE KODE EXIST                                       
090700*                                                                         
090800     MOVE W-IDDC-B6                  TO W-6315-IDDC                       
090900     MOVE MID-KDSTOR-MAIN            TO W-KDSTOR                          
091000                                                                          
091100     PERFORM  IMS-GHU-6316                                                
091200                                                                          
091300     IF SEGMENT-MISSING                                                   
091400        MOVE NOO                     TO INDATA-SW                         
091500        MOVE MFS-ALPHA-FIELD-WRONG   TO MOD-KDSTOR-ATTR                   
091600       ELSE                                                               
091700        MOVE MFS-ALPHA-FIELD-OK      TO MOD-KDSTOR-ATTR                   
091800        MOVE MID-KDSTOR-MAIN         TO WS-KDSTOR                         
091900     END-IF                                                               
092000     .                                                                    
092100     EJECT                                                                
092200                                                                          
092300 GC-CHECK-INPUT-DELETE SECTION.                                           
092400                                                                          
092500     MOVE W-IDDC-B6                  TO W-6315-IDDC                       
092600     MOVE MID-KDSTOR-MAIN            TO W-KDSTOR                          
092700                                                                          
092800     PERFORM  IMS-GHU-6316                                                
092900*                                                                         
093000*   --- CHECK IF STORAGE KODE EXIST                                       
093100*                                                                         
093200     IF SEGMENT-MISSING                                                   
093300        MOVE NOO                     TO INDATA-SW                         
093400        MOVE MFS-ALPHA-FIELD-WRONG   TO MOD-KDSTOR-ATTR                   
093500       ELSE                                                               
093600        MOVE MFS-ALPHA-FIELD-OK      TO MOD-KDSTOR-ATTR                   
093700     END-IF                                                               
093800     .                                                                    
093900     EJECT                                                                
094000 GD-MID-INDATA-TO-MOD SECTION.                                            
094100                                                                          
094200     IF MID-KDVSOP-MAIN (1)  = ALL '+'                                    
094300        MOVE MFS-ERASE-FIELD        TO MOD-KDVSOP1-ATTR                   
094400      ELSE                                                                
094500        MOVE MFS-ADD-READ-FIELD     TO MOD-KDVSOP1-ATTR                   
094600     END-IF                                                               
094700                                                                          
094800     IF MID-KDVSOP-MAIN (2)  = ALL '+'                                    
094900        MOVE MFS-ERASE-FIELD        TO MOD-KDVSOP2-ATTR                   
095000      ELSE                                                                
095100        MOVE MFS-ADD-READ-FIELD     TO MOD-KDVSOP2-ATTR                   
095200     END-IF                                                               
095300                                                                          
095400     IF MID-KDVSOP-MAIN (3) = ALL '+'                                     
095500        MOVE MFS-ERASE-FIELD        TO MOD-KDVSOP3-ATTR                   
095600      ELSE                                                                
095700        MOVE MFS-ADD-READ-FIELD     TO MOD-KDVSOP3-ATTR                   
095800     END-IF                                                               
095900                                                                          
096000     IF MID-KDVSOP-MAIN (4)  = ALL '+'                                    
096100        MOVE MFS-ERASE-FIELD        TO MOD-KDVSOP4-ATTR                   
096200      ELSE                                                                
096300        MOVE MFS-ADD-READ-FIELD     TO MOD-KDVSOP4-ATTR                   
096400     END-IF                                                               
096500                                                                          
096600     IF MID-KDVSOP-MAIN (5)  = ALL '+'                                    
096700        MOVE MFS-ERASE-FIELD        TO MOD-KDVSOP5-ATTR                   
096800      ELSE                                                                
096900        MOVE MFS-ADD-READ-FIELD     TO MOD-KDVSOP5-ATTR                   
097000     END-IF                                                               
097100                                                                          
097200     IF MID-TESTORAGE-MAIN = ALL '+'                                      
097300        MOVE MFS-ERASE-FIELD        TO MOD-TESTORAGE-ATTR                 
097400      ELSE                                                                
097500        MOVE MFS-ADD-READ-FIELD     TO MOD-TESTORAGE-ATTR                 
097600     END-IF                                                               
097700                                                                          
097800     IF MID-DISTORB-MAIN = ALL '+'                                        
097900        MOVE MFS-ERASE-FIELD        TO MOD-DISTORB-ATTR                   
098000      ELSE                                                                
098100        MOVE MFS-ADD-READ-FIELD     TO MOD-DISTORB-ATTR                   
098200     END-IF                                                               
098300                                                                          
098400     IF MID-DISTORD-MAIN = ALL '+'                                        
098500        MOVE MFS-ERASE-FIELD        TO MOD-DISTORD-ATTR                   
098600      ELSE                                                                
098700        MOVE MFS-ADD-READ-FIELD     TO MOD-DISTORD-ATTR                   
098800     END-IF                                                               
098900                                                                          
099000     IF MID-DISTORH-MAIN = ALL '+'                                        
099100        MOVE MFS-ERASE-FIELD        TO MOD-DISTORH-ATTR                   
099200      ELSE                                                                
099300        MOVE MFS-ADD-READ-FIELD     TO MOD-DISTORH-ATTR                   
099400     END-IF                                                               
099500                                                                          
099600     IF MID-TESTORAGE-MAIN = ALL '+'                                      
099700        MOVE MFS-ERASE-FIELD        TO MOD-TESTORAGE-ATTR                 
099800      ELSE                                                                
099900        MOVE MFS-ADD-READ-FIELD     TO MOD-TESTORAGE-ATTR                 
100000     END-IF                                                               
100100                                                                          
100200     .                                                                    
100300     EJECT                                                                
100400 H-UPDATE SECTION.                                                        
100500                                                                          
100600     IF MID-KDANDR-MAIN = NEW                                             
100700        PERFORM HA-NEW                                                    
100800     END-IF                                                               
100900     IF MID-KDANDR-MAIN = CHG                                             
101000        PERFORM HB-CHANGE                                                 
101100     END-IF                                                               
101200     IF MID-KDANDR-MAIN = DEL                                             
101300        PERFORM HC-DELETE                                                 
101400     END-IF                                                               
101500                                                                          
101600     MOVE INF-UPDATE-DONE TO MED-IDMFSINF                                 
101700     CALL WMEDKONV USING MED-WMEDAREA                                     
101800     MOVE MED-MFSINF TO MOD-TEMFSINF                                      
101900     PERFORM MFS-FORM-ATTR                                                
102000     PERFORM MFS-ERASE-FIELD-IN                                           
102100     .                                                                    
102200     EJECT                                                                
102300 HA-NEW SECTION.                                                          
102400                                                                          
102500     MOVE W-IDDC-B6       TO  W-6315-IDDC                                 
102600                                                                          
102700     MOVE MID-KDSTOR-MAIN TO  W-KDSTOR                                    
102800                              6316-KDSTOR                                 
102900                              MOD-KDSTOR-UT                               
103000                                                                          
103100     MOVE WS-TESTORAGE    TO  6316-TESTORAGE                              
103200     IF US-MEASUREMENT                                                    
103300        COMPUTE 6316-DISTORD  ROUNDED =                                   
103400                   WS-DISTORD * CONV-IN-TO-CM  END-COMPUTE                
103500        COMPUTE 6316-DISTORB  ROUNDED =                                   
103600                   WS-DISTORB * CONV-IN-TO-CM  END-COMPUTE                
103700        COMPUTE 6316-DISTORH  ROUNDED =                                   
103800                   WS-DISTORH * CONV-IN-TO-CM  END-COMPUTE                
103900      ELSE                                                                
104000        MOVE WS-DISTORD      TO  6316-DISTORD                             
104100        MOVE WS-DISTORB      TO  6316-DISTORB                             
104200        MOVE WS-DISTORH      TO  6316-DISTORH                             
104300     END-IF                                                               
104400     MOVE WS-KDVSOP1      TO  6316-KDVSOP (1)                             
104500     MOVE WS-KDVSOP2      TO  6316-KDVSOP (2)                             
104600     MOVE WS-KDVSOP3      TO  6316-KDVSOP (3)                             
104700     MOVE WS-KDVSOP4      TO  6316-KDVSOP (4)                             
104800     MOVE WS-KDVSOP5      TO  6316-KDVSOP (5)                             
104900                                                                          
105000     PERFORM IMS-ISRT-6315-6316                                           
105100                                                                          
105200     MOVE WS-KDSTOR       TO  MOD-KDSTOR-UT                               
105300                              W-KDSTOR                                    
105400     .                                                                    
105500     EJECT                                                                
105600 HB-CHANGE SECTION.                                                       
105700                                                                          
105800     IF MID-KDVSOP-MAIN (1) NOT = ALL '+'                                 
105900        MOVE WS-KDVSOP1       TO  6316-KDVSOP (1)                         
106000     END-IF                                                               
106100                                                                          
106200     IF MID-KDVSOP-MAIN (2) NOT = ALL '+'                                 
106300        MOVE WS-KDVSOP2       TO  6316-KDVSOP (2)                         
106400     END-IF                                                               
106500                                                                          
106600     IF MID-KDVSOP-MAIN (3) NOT = ALL '+'                                 
106700        MOVE WS-KDVSOP3       TO  6316-KDVSOP (3)                         
106800     END-IF                                                               
106900                                                                          
107000     IF MID-KDVSOP-MAIN (4) NOT = ALL '+'                                 
107100        MOVE WS-KDVSOP4       TO  6316-KDVSOP (4)                         
107200     END-IF                                                               
107300                                                                          
107400     IF MID-KDVSOP-MAIN (5) NOT = ALL '+'                                 
107500        MOVE WS-KDVSOP5       TO  6316-KDVSOP (5)                         
107600     END-IF                                                               
107700                                                                          
107800     IF MID-TESTORAGE-MAIN NOT = ALL '+'                                  
107900        MOVE WS-TESTORAGE     TO 6316-TESTORAGE                           
108000     END-IF                                                               
108100                                                                          
108200     IF MID-DISTORD-MAIN NOT = ALL '+'                                    
108300        IF US-MEASUREMENT                                                 
108400           COMPUTE 6316-DISTORD =                                         
108500                   (WS-DISTORD * CONV-IN-TO-CM)                           
108600                   END-COMPUTE                                            
108700        ELSE                                                              
108800          MOVE WS-DISTORD      TO  6316-DISTORD                           
108900        END-IF                                                            
109000     END-IF                                                               
109100                                                                          
109200     IF MID-DISTORB-MAIN NOT = ALL '+'                                    
109300        IF US-MEASUREMENT                                                 
109400           COMPUTE 6316-DISTORB =                                         
109500                   WS-DISTORB * CONV-IN-TO-CM  END-COMPUTE                
109600        ELSE                                                              
109700          MOVE WS-DISTORB      TO  6316-DISTORB                           
109800        END-IF                                                            
109900     END-IF                                                               
110000                                                                          
110100     IF MID-DISTORH-MAIN NOT = ALL '+'                                    
110200        IF US-MEASUREMENT                                                 
110300           COMPUTE 6316-DISTORH ROUNDED =                                 
110400                   WS-DISTORH * CONV-IN-TO-CM  END-COMPUTE                
110500        ELSE                                                              
110600          MOVE WS-DISTORH      TO  6316-DISTORH                           
110700        END-IF                                                            
110800     END-IF                                                               
110900                                                                          
111000     PERFORM IMS-REPL-6315-6316                                           
111100                                                                          
111200     MOVE WS-KDSTOR            TO W-KDSTOR                                
111300                                  MOD-KDSTOR-UT                           
111400     .                                                                    
111500     EJECT                                                                
111600 HC-DELETE SECTION.                                                       
111700                                                                          
111800     PERFORM IMS-DLET-6315-6316                                           
111900     .                                                                    
112000     EJECT                                                                
112100 S01-EV-CONVERT-TO-US-MEASURE        SECTION.                             
112200                                                                          
112300                                                                          
112400     IF US-MEASUREMENT                                                    
112500       COMPUTE WS-DISTORD ROUNDED = WS-DISTORD  * CONV-CM-TO-IN           
112600       END-COMPUTE                                                        
112700                                                                          
112800       COMPUTE WS-DISTORB ROUNDED = WS-DISTORB * CONV-CM-TO-IN            
112900       END-COMPUTE                                                        
113000                                                                          
113100       COMPUTE WS-DISTORH ROUNDED = WS-DISTORH * CONV-CM-TO-IN            
113200       END-COMPUTE                                                        
113300                                                                          
113400     END-IF                                                               
113500     .                                                                    
113600     SKIP2                                                                
113700*                                                                         
113800 MFS-INIT-KEY-FIELD-IN SECTION.                                           
113900                                                                          
114000*    --- ALL INPUT KEY FIELDS                                             
114100                                                                          
114200     MOVE MFS-ERASE-FIELD          TO MOD-KDSTOR-IN                       
114300     .                                                                    
114400     EJECT                                                                
114500 MFS-INIT-KEY-FIELD-OUT SECTION.                                          
114600                                                                          
114700*    --- ALL OUTPUT KEY FIELDS                                            
114800                                                                          
114900     MOVE MSGI-IDDC                TO MOD-IDDC                            
115000     MOVE MFS-ERASE-FIELD          TO MOD-KDSTOR-UT                       
115100     .                                                                    
115200     EJECT                                                                
115300 MFS-ERASE-FIELD-OUT SECTION.                                             
115400                                                                          
115500*    --- ALL  OUTPUT-FIELDS                                               
115600*    --- INCL. SCROLL KEYS                                                
115700     MOVE MFS-ERASE-FIELD          TO MOD-KDSTOR-MAIN                     
115800                                      MOD-TESTORAGE-MAIN                  
115900                                      MOD-DISTORD-MAIN                    
116000                                      MOD-DISTORB-MAIN                    
116100                                      MOD-DISTORH-MAIN                    
116200                                      MOD-KDVSOP1-MAIN                    
116300                                      MOD-KDVSOP2-MAIN                    
116400                                      MOD-KDVSOP3-MAIN                    
116500                                      MOD-KDVSOP4-MAIN                    
116600                                      MOD-KDVSOP5-MAIN                    
116700                                      MOD-KDANDR-MAIN                     
116800     .                                                                    
116900     SKIP3                                                                
117000 MFS-ERASE-LINE-FIELD-OUT SECTION.                                        
117100                                                                          
117200*    --- OUTDATA-FIELD ON SCROLL KEYS                                     
117300                                                                          
117400     MOVE +1 TO INDX                                                      
117500     PERFORM UNTIL INDX > MAX-INDX                                        
117600     MOVE MFS-ERASE-FIELD     TO MOD-KDSTOR-LINE    (INDX)                
117700                                 MOD-TESTORAGE-LINE (INDX)                
117800                                 MOD-DISTORD-LINE   (INDX)                
117900                                 MOD-DISTORB-LINE   (INDX)                
118000                                 MOD-DISTORH-LINE   (INDX)                
118100                                 MOD-KDVSOP1-LINE   (INDX)                
118200                                 MOD-KDVSOP2-LINE   (INDX)                
118300                                 MOD-KDVSOP3-LINE   (INDX)                
118400                                 MOD-KDVSOP4-LINE   (INDX)                
118500                                 MOD-KDVSOP5-LINE   (INDX)                
118600      ADD +1 TO INDX                                                      
118700     END-PERFORM                                                          
118800     .                                                                    
118900     SKIP3                                                                
119000 MFS-ERASE-FIELD-IN SECTION.                                              
119100                                                                          
119200*    --- ALL INPUT DATA FIELDS                                            
119300                                                                          
119400     MOVE MFS-ERASE-FIELD TO MOD-KDSTOR-MAIN                              
119500                             MOD-TESTORAGE-MAIN                           
119600                             MOD-DISTORD-MAIN                             
119700                             MOD-DISTORB-MAIN                             
119800                             MOD-DISTORH-MAIN                             
119900                             MOD-KDVSOP1-MAIN                             
120000                             MOD-KDVSOP2-MAIN                             
120100                             MOD-KDVSOP3-MAIN                             
120200                             MOD-KDVSOP4-MAIN                             
120300                             MOD-KDVSOP5-MAIN                             
120400                             MOD-KDANDR-MAIN                              
120500     .                                                                    
120600     EJECT                                                                
120700 MFS-DONT-TOUCH-FIELD-OUT  SECTION.                                       
120800                                                                          
120900*    --- ALL OUTDATA FIELDS                                               
121000*    --- INCL SCROLL KEYS AND LINEDATA                                    
121100     MOVE MFS-DO-NOT-TOUCH-FIELD TO MOD-KDSTOR-MAIN                       
121200                                    MOD-TESTORAGE-MAIN                    
121300                                    MOD-DISTORD-MAIN                      
121400                                    MOD-DISTORB-MAIN                      
121500                                    MOD-DISTORH-MAIN                      
121600                                    MOD-KDVSOP1-MAIN                      
121700                                    MOD-KDVSOP2-MAIN                      
121800                                    MOD-KDVSOP3-MAIN                      
121900                                    MOD-KDVSOP4-MAIN                      
122000                                    MOD-KDVSOP5-MAIN                      
122100                                    MOD-KDANDR-MAIN                       
122200     MOVE +1 TO INDX                                                      
122300     PERFORM UNTIL INDX > MAX-INDX                                        
122400       PERFORM MFS-DONT-TOUCH-LINE-FIELD-OUT                              
122500       ADD +1 TO INDX                                                     
122600     END-PERFORM                                                          
122700     .                                                                    
122800     EJECT                                                                
122900 MFS-DONT-TOUCH-LINE-FIELD-OUT  SECTION.                                  
123000                                                                          
123100*    --- OUTDATA FIELD ON SCROLL KEYS                                     
123200     MOVE MFS-DO-NOT-TOUCH-FIELD TO MOD-KDSTOR-LINE    (INDX)             
123300                                    MOD-TESTORAGE-LINE (INDX)             
123400                                    MOD-DISTORD-LINE   (INDX)             
123500                                    MOD-DISTORB-LINE   (INDX)             
123600                                    MOD-DISTORH-LINE   (INDX)             
123700                                    MOD-KDVSOP1-LINE   (INDX)             
123800                                    MOD-KDVSOP2-LINE   (INDX)             
123900                                    MOD-KDVSOP3-LINE   (INDX)             
124000                                    MOD-KDVSOP4-LINE   (INDX)             
124100                                    MOD-KDVSOP5-LINE   (INDX)             
124200     .                                                                    
124300     EJECT                                                                
124400 MFS-FORM-ATTR SECTION.                                                   
124500                                                                          
124600*    --- ALL INDATA-FIELDS                                                
124700     MOVE MFS-FORMAT-DEFAULT-ATTR TO MOD-KDSTOR-ATTR                      
124800                                     MOD-TESTORAGE-ATTR                   
124900                                     MOD-DISTORD-ATTR                     
125000                                     MOD-DISTORB-ATTR                     
125100                                     MOD-DISTORH-ATTR                     
125200                                     MOD-KDVSOP1-ATTR                     
125300                                     MOD-KDVSOP2-ATTR                     
125400                                     MOD-KDVSOP3-ATTR                     
125500                                     MOD-KDVSOP4-ATTR                     
125600                                     MOD-KDVSOP5-ATTR                     
125700                                     MOD-KDANDR-ATTR                      
125800     .                                                                    
125900     SKIP2                                                                
126000* --- IMS SECTIONS ---                                                    
126100     SKIP3                                                                
126200 IMS-GET-MSG SECTION.                                                     
126300                                                                          
126400     MOVE '  QC' TO GOOD-STATUSCODES                                      
126500     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
126600     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
126700     PERFORM IMS-STATUSCHECK                                              
126800     .                                                                    
126900     SKIP3                                                                
127000 IMS-INSERT-MSG SECTION.                                                  
127100                                                                          
127200     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
127300     MOVE SPACE TO GOOD-STATUSCODES                                       
127400     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
127500     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
127600     PERFORM IMS-STATUSCHECK                                              
127700     .                                                                    
127800     EJECT                                                                
127900 IMS-GU-6316 SECTION.                                                     
128000                                                                          
128100     STRING 'WL631501(WDGXKEY  =' W-WDGXKEY-6315-X ')'                    
128200          DELIMITED BY SIZE INTO SSA1                                     
128300     STRING 'WL631511(KDSTOR   =' W-WDGXKEY-6316-X ')'                    
128400          DELIMITED BY SIZE INTO SSA2                                     
128500     MOVE '  GE' TO GOOD-STATUSCODES                                      
128600     CALL CBLTDLI USING GU 6315-PCB 6316-WDGX6316 SSA1 SSA2               
128700     MOVE 6315-STATUS-CODE TO STATUS-WS                                   
128800     PERFORM IMS-STATUSCHECK                                              
128900     .                                                                    
129000     SKIP3                                                                
129100 IMS-GHU-6316 SECTION.                                                    
129200                                                                          
129300     STRING 'WL631501(WDGXKEY  =' W-WDGXKEY-6315-X ')'                    
129400          DELIMITED BY SIZE INTO SSA1                                     
129500     STRING 'WL631511(KDSTOR   =' W-WDGXKEY-6316-X ')'                    
129600          DELIMITED BY SIZE INTO SSA2                                     
129700     MOVE '  GE' TO GOOD-STATUSCODES                                      
129800     CALL CBLTDLI USING GHU 6315-PCB 6316-WDGX6316 SSA1 SSA2              
129900     MOVE 6315-STATUS-CODE TO STATUS-WS                                   
130000     PERFORM IMS-STATUSCHECK                                              
130100     .                                                                    
130200     SKIP3                                                                
130300 IMS-GU-6315-6316 SECTION.                                                
130400                                                                          
130500     STRING 'WL631501(WDGXKEY  =' W-WDGXKEY-6315-X ')'                    
130600          DELIMITED BY SIZE INTO SSA1                                     
130700     STRING 'WL631511(KDSTOR  >=' W-WDGXKEY-6316-X                        
130800                    '&KDSTOR  =<' W-KDSTOR-MAX-X ')'                      
130900          DELIMITED BY SIZE INTO SSA2                                     
131000     MOVE '  GE' TO GOOD-STATUSCODES                                      
131100     CALL CBLTDLI USING GU 6315-PCB 6316-WDGX6316 SSA1 SSA2               
131200     MOVE 6315-STATUS-CODE TO STATUS-WS                                   
131300     PERFORM IMS-STATUSCHECK                                              
131400     .                                                                    
131500     SKIP3                                                                
131600 IMS-GN-6315-6316 SECTION.                                                
131700                                                                          
131800     STRING 'WL631501(WDGXKEY  =' W-WDGXKEY-6315-X ')'                    
131900          DELIMITED BY SIZE INTO SSA1                                     
132000     STRING 'WL631511(KDSTOR  >=' W-WDGXKEY-6316-X                        
132100                    '&KDSTOR  =<' W-KDSTOR-MAX-X ')'                      
132200          DELIMITED BY SIZE INTO SSA2                                     
132300     MOVE '  GE' TO GOOD-STATUSCODES                                      
132400     CALL CBLTDLI USING GN 6315-PCB 6316-WDGX6316 SSA1 SSA2               
132500     MOVE 6315-STATUS-CODE TO STATUS-WS                                   
132600     PERFORM IMS-STATUSCHECK                                              
132700     .                                                                    
132800     SKIP3                                                                
132900 IMS-ISRT-6315-6316 SECTION.                                              
133000                                                                          
133100                                                                          
133200     STRING 'WL631501(WDGXKEY  =' W-WDGXKEY-6315-X ')'                    
133300          DELIMITED BY SIZE INTO SSA1                                     
133400     MOVE 'WL631511 ' TO SSA2                                             
133500     MOVE '  II' TO GOOD-STATUSCODES                                      
133600     CALL CBLTDLI USING ISRT 6315-PCB 6316-WDGX6316 SSA1 SSA2             
133700     MOVE 6315-STATUS-CODE TO STATUS-WS                                   
133800     PERFORM IMS-STATUSCHECK                                              
133900     .                                                                    
134000     SKIP3                                                                
134100 IMS-REPL-6315-6316 SECTION.                                              
134200                                                                          
134300     MOVE '  ' TO GOOD-STATUSCODES                                        
134400     CALL CBLTDLI USING REPL 6315-PCB 6316-WDGX6316                       
134500     MOVE 6315-STATUS-CODE TO STATUS-WS                                   
134600     PERFORM IMS-STATUSCHECK                                              
134700     .                                                                    
134800     SKIP3                                                                
134900 IMS-DLET-6315-6316 SECTION.                                              
135000                                                                          
135100     MOVE '  ' TO GOOD-STATUSCODES                                        
135200     CALL CBLTDLI USING DLET 6315-PCB 6316-WDGX6316                       
135300     MOVE 6315-STATUS-CODE TO STATUS-WS                                   
135400     PERFORM IMS-STATUSCHECK                                              
135500     .                                                                    
135600     EJECT                                                                
135700                                                                          
135800 IMS-GU-WDB601    SECTION.                                                
135900     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
136000          DELIMITED BY SIZE INTO SSA1                                     
136100     MOVE '  GE' TO GOOD-STATUSCODES                                      
136200     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B601 SSA1                 
136300     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
136400     PERFORM IMS-STATUSCHECK                                              
136500     .                                                                    
136600     EJECT                                                                
136700 IMS-STATUSCHECK SECTION.                                                 
136800                                                                          
136900     SET STATUS-IX TO 1                                                   
137000     SEARCH GOOD-STATUS                                                   
137100       AT END                                                             
137200         STRING ' INVALID STATUS CODE FROM IMS: ' STATUS-WS               
137300         DELIMITED BY SIZE INTO ERROR-TEXT                                
137400         CALL FELLOG                                                      
137500       WHEN GOOD-STATUS (STATUS-IX) = STATUS-WS                           
137600         CONTINUE                                                         
137700     END-SEARCH                                                           
137800     .                                                                    
