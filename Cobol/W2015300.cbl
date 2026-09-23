000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W2015300.                                                
000300 AUTHOR.         STENING INGER.                                           
000400 DATE-WRITTEN.   14/05/06.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNCTION:                                                            
000800*        STOCKING POLICY                                                  
000900*                                                                         
001000*        THE PROGRAM UPDATES   WDG2 (1143)                                
001100*                              WDGX1144                                   
001200*                                                                         
001300*    INDATA.                                                              
001400*        TRANSACTION: W2T153                                              
001500*        MID:         W2I15301                                            
001600*                                                                         
001700*    OUTDATA.                                                             
001800*        MOD:         W2O15301                                            
001900                                                                          
002000     SKIP3                                                                
002100 ENVIRONMENT DIVISION.                                                    
002200                                                                          
002300 DATA DIVISION.                                                           
002400     EJECT                                                                
002500 WORKING-STORAGE SECTION.                                                 
002600 77  IDPGM                       PIC X(08)   VALUE 'W2015300'.            
002710                                                                          
002800*    --- WORK FIELDS FOR ERROR MESSAGES WHEN CALLING ABEND/FELLOG         
002900 77  CURRENT-SECTION             PIC X(80) VALUE SPACE.                   
003000 77  IMS-SECTION                 PIC X(80) VALUE SPACE.                   
003100 77  ERROR-TEXT                  PIC X(80) VALUE SPACE.                   
003200                                                                          
003300 77  YES                         PIC X       VALUE 'J'.                   
003400 77  NOO                         PIC X       VALUE 'N'.                   
003500                                                                          
003600 77  STRECK                      PIC X       VALUE '-'.                   
003610 77  W-QTY-KDCMD-DEL             PIC S9(4)   VALUE +0   COMP-3.           
003620 77  W-COPY-LINE                 PIC X       VALUE SPACE.                 
003700                                                                          
003800*    --- INDEX FOR SCROLL LINES                                           
003900 77  INDX                        PIC S9(4)  VALUE +0    COMP SYNC.        
004000 77  MAX-INDX                    PIC S9(4)  VALUE +13   COMP SYNC.        
004001                                                                          
004010 77  INDX-SAVE                   PIC S9(4)  VALUE +0    COMP SYNC.        
004020 77  INDX-COPY-DEL               PIC S9(4)  VALUE +0    COMP SYNC.        
004100                                                                          
004200*    --- WORK FIELDS FOR ACTUAL KEYVALUES OF SCREEN                       
004300                                                                          
004400 77  INDATA-SW                   PIC X       VALUE 'J'.                   
004500     88  INDATA-OK                           VALUE 'J'.                   
004600     88  INDATA-WRONG                        VALUE 'N'.                   
004700                                                                          
004800 77  KEYS-SW                     PIC X       VALUE 'J'.                   
004900     88  KEYS-OK                             VALUE 'J'.                   
005000     88  KEYS-WRONG                          VALUE 'N'.                   
005100                                                                          
005200 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
005300     88  OWN-MID                             VALUE '2153'.                
005400     88  GOOD-MID                            VALUE '2151' '2152'          
005500                                                   '2153' '2154'          
005600                                                   '2155' '2156'          
005700                                                   '2157' '2158'          
005800                                                   '2153'.                
005900     88  HELP-MID                            VALUE '0551'.                
006000     EJECT                                                                
006100*    --- SUBPROGRAMS AND PARAMETER AREAS                                  
006200 01  GENERAL-SUBPROGRAMS.                                                 
006300     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
006400     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
006500     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
006600     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
006700     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
006800     EJECT                                                                
006900*    --- PARAMETERS FOR SUBPROGRAM WMEDKONV                               
007000*01 -COPY WMEDAREA                                                        
007100     EJECT                                                                
007200*01  -COPY WDATAREA                                                       
007300     EJECT                                                                
007400     SKIP3                                                                
007500 01  MESSAGE-CODES.                                                       
007600     03  ERR-CORR-HILITE-FLDS    PIC X(3)    VALUE '001'.                 
007700     03  INF-PRESS-PF11          PIC X(3)    VALUE '003'.                 
007800     03  ERR-PF11-AND-NO-DATA    PIC X(3)    VALUE '011'.                 
007900     03  INF-UPDATE-DONE         PIC X(3)    VALUE '101'.                 
008000     03  INF-FIRST-PAGE          PIC X(3)    VALUE '006'.                 
008100     03  INF-MORE-INFO-EXISTS    PIC X(3)    VALUE '105'.                 
008200     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
008300     03  ERR-SUPP-MISSING        PIC X(3)    VALUE '273'.                 
008400     03  ERR-LINE-EXIST          PIC X(3)    VALUE '245'.                 
008410     03  ERR-CONFLICT-CHOICE     PIC X(3)    VALUE '287'.                 
008500                                                                          
008600*    --- PARAMETERS FOR SUB PROGRAM W005INIT                              
008700*                                                                         
008800 01  FILLER                      PIC X(16)   VALUE 'WMSGINIT'.            
008900     SKIP3                                                                
009000*01 -COPY WMSGINIT                                                        
009100     EJECT                                                                
009200 01  PROG-TO-PROG-SW.                                                     
009300*    03  -COPY WMSGSOP                                                    
009400     EJECT                                                                
009500*    --- AREA CONTAINING DATA TO BE SAVED BETWEEN DIALOG STEPS            
009600*                                                                         
009700 01  SAVE-AREA.                                                           
009800     03  SAVE-IDTRANS             PIC X(4)    VALUE '2153'.               
009900                                                                          
010000     03  SAVE-IDFKNGRP-FOM-ENTER  PIC 9(4).                               
010100     03  SAVE-IDFKNGRP-TOM-ENTER  PIC 9(4).                               
010200                                                                          
010300     03  SAVE-IDFKNGRP-FOM-NEXT   PIC 9(4).                               
010400     03  SAVE-IDFKNGRP-TOM-NEXT   PIC 9(4).                               
010500     EJECT                                                                
010600*    --- AREAS FOR MFS AND SCREEN MANAGEMENT                              
010700*                                                                         
010800 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
010900     SKIP3                                                                
011000*01  MID -COPY W2I15301                                                   
011100     EJECT                                                                
011200 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
011300     SKIP3                                                                
011400*01  -COPY WMSGAREA                                                       
011500     EJECT                                                                
011600     03  MOD REDEFINES MSG-AREA.                                          
011700*      05  -COPY W2O15301                                                 
011800     EJECT                                                                
011900 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
012000     SKIP3                                                                
012100*01  -COPY WMFSAREA                                                       
012200     EJECT                                                                
012300*    --- WORK-AREAS FOR IMS-SECTIONS                                      
012400*                                                                         
012500 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
012600     SKIP3                                                                
012700 01  KEYS-FOR-DLI.                                                        
012800*    --- VALUE OF SCROLLING KEY FOR FIRST LINE ON THE SCREEN              
012900     03  W-WDGXKEY-1143-X.                                                
013000         05  W-IDHTYP-1143       PIC X(4)    VALUE '1143'.                
013100         05  FILLER              PIC X(26)   VALUE LOW-VALUE.             
013200                                                                          
013300     03  W-IDFKNGRP-X.                                                    
013400         05  W-IDFKNGRP          PIC S9(5) COMP-3 VALUE +0.               
013500                                                                          
013600     03  W-IDFKNGRP-FOM-X.                                                
013700         05  W-IDFKNGRP-FOM      PIC S9(5) COMP-3 VALUE +0.               
013800                                                                          
013900     03  W-IDFKNGRP-TOM-X.                                                
014000         05  W-IDFKNGRP-TOM      PIC S9(5) COMP-3 VALUE +0.               
014100                                                                          
014110     03  W-IDFKNGRP-FOM-X2.                                               
014120         05  W-IDFKNGRP-FOM-2    PIC S9(5) COMP-3 VALUE +0.               
014130                                                                          
014140     03  W-IDFKNGRP-TOM-X2.                                               
014150         05  W-IDFKNGRP-TOM-2    PIC S9(5) COMP-3 VALUE +0.               
014160                                                                          
014200     SKIP2                                                                
014300*    --- STATUS CODES FROM IMS                                            
014400 01  STATUS-WS                   PIC XX.                                  
014500     88  SEGMENT-FOUND                       VALUE '  '.                  
014600     88  SEGMENT-FOUND-EXISTS                VALUE 'II'.                  
014700     88  SEGMENT-MISSING                     VALUE 'GE'.                  
014800     SKIP2                                                                
014900 01  GOOD-STATUSCODES.                                                    
015000     03  GOOD-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
015100     SKIP3                                                                
015200 01  SSA1                        PIC X(224).                              
015300 01  SSA2                        PIC X(224).                              
015400     EJECT                                                                
015500*    --- IMS FUNCTION CODES                                               
015600*01  -COPY W0003                                                          
015700     EJECT                                                                
015800*    ---  DLI INPUT-OUTPUT AREA                                           
015900                                                                          
016000 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX01'.                      
016100 01  DLI-IO-WDGX01.                                                       
016200*    03  -COPY WDGX01                                                     
016300                                                                          
016400 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX1144'.                    
016500 01  DLI-IO-WDGX1144.                                                     
016600*    03  -COPY WDGX1144                                                   
016700                                                                          
016800     EJECT                                                                
016900 LINKAGE SECTION.                                                         
017000*01  -COPY W0009   -PRE MSG-                                              
017100                                                                          
017200*01  -COPY W0009   -PRE ALT-                                              
017300                                                                          
017400*01  -COPY W0009   -PRE USEA-                                             
017500                                                                          
017600*01  -COPY W0008   -PRE WDG2-                                             
017700     05  FILLER                  PIC X.                                   
017800                                                                          
017900     EJECT                                                                
018000 PROCEDURE DIVISION  USING MSG-PCB ALT-PCB USEA-PCB WDG2-PCB.             
018100                                                                          
018200 MAIN SECTION.                                                            
018300     ENTRY 'DLITCBL' USING MSG-PCB ALT-PCB USEA-PCB WDG2-PCB.             
018400                                                                          
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
020600         PERFORM F-READ-SHOW-INFO                                         
020700       END-IF                                                             
020800*    --- IF ANSWER TO SCREEN:       MSG-KVLL = MOD-LENGTH + 4             
020900*    --- IF PROGRAM-TO-PROGRAM-SWITCH:       = MOD-LENGTH + 17            
021000       COMPUTE MSG-KVLL = LENGTH OF MOD-W2O15301 + 4                      
021100       PERFORM IMS-INSERT-MSG                                             
021200     END-IF                                                               
021300                                                                          
021400     MOVE ZERO TO RETURN-CODE                                             
021500     GOBACK                                                               
021600     .                                                                    
021700     EJECT                                                                
021800 A-INIT SECTION.                                                          
021900     MOVE 'A-INIT SECTION              '  TO CURRENT-SECTION              
022000                                                                          
022100     IF MSG-DOUBLE-TRANSACTIONS                                           
022200       MOVE MSG-INDATA-MINUS-2-TRANSACT   TO MID-W2I15301                 
022300       MOVE MSG-IDTRANS-2                 TO MFS-IDTRANS                  
022400       MOVE MSG-KDMFSFOR-2                TO MFS-KDMFSFOR                 
022500     ELSE                                                                 
022600       MOVE MSG-INDATA-MINUS-1-TRANSACT   TO MID-W2I15301                 
022700       MOVE MSG-IDTRANS-1                 TO MFS-IDTRANS                  
022800       MOVE MSG-KDMFSFOR-1                TO MFS-KDMFSFOR                 
022900     END-IF                                                               
023000                                                                          
023100     MOVE MSG-KDTRTYP                     TO MFS-KDTRTYP                  
023200     MOVE MSG-IDPFK                       TO MFS-IDPFK                    
023300     MOVE MFS-IDTRANS                     TO W-IDTRANS                    
023400                                                                          
023500     MOVE LOW-VALUE                       TO MSG-AREA                     
023600     MOVE 'W2O153N1'                      TO MFS-IDMOD                    
023700     MOVE '2153'                          TO MOD-IDTRANS                  
023800     MOVE MFS-ERASE-FIELD                 TO MOD-TEMFSFEL                 
023900                                             MOD-TEMFSINF                 
024000                                                                          
024100     IF OWN-MID OR HELP-MID                                               
024200       CONTINUE                                                           
024300     ELSE                                                                 
024400       MOVE SPACE TO MFS-KDTRTYP                                          
024500       MOVE '7' TO MFS-IDPFK                                              
024600     END-IF                                                               
024700                                                                          
024800     .                                                                    
024900     EJECT                                                                
025000 B-CHECK-KEYS SECTION.                                                    
025100     MOVE 'B-CHECK-KEYS                '  TO CURRENT-SECTION              
025200                                                                          
025300     MOVE ALL '+'                  TO MSGI-WMSGINIT                       
025400     MOVE '001'                    TO MSGI-KDCALL                         
025500     MOVE MSG-LTERM-NAME           TO MSGI-IDLTERM-USER                   
025600     MOVE MSG-SIGNON-USERID        TO MSGI-IDUSER                         
025700     MOVE '2153'                   TO MSGI-IDTRANS                        
025710                                                                          
025800     IF GOOD-MID                                                          
025810        IF MID-IDFKNGRP-IN NOT = ALL '+'                                  
025900           MOVE MID-IDFKNGRP-IN    TO MSGI-IDFKNGRP                       
026000        END-IF                                                            
026010     END-IF                                                               
026020                                                                          
026100     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
026200     MOVE MSGI-SPAR-AREA           TO SAVE-AREA                           
026300                                                                          
026400*    - LANGUAGE TO BE USED BY MEDKONV                                     
026500     MOVE MSGI-IDLAND-SPR          TO MED-IDSKYLT                         
026600                                                                          
026700     MOVE YES                      TO KEYS-SW                             
026800                                                                          
026900                                                                          
027000*    -- CHECK OF IDFKNGRP                                                 
027100     MOVE MFS-ERASE-FIELD          TO MOD-IDFKNGRP-IN                     
027200                                                                          
027300     IF MID-IDFKNGRP-IN NOT = ALL '+'                                     
027400       MOVE '7'                    TO MFS-IDPFK                           
027500       MOVE SPACE                  TO MFS-KDTRTYP                         
027600     END-IF                                                               
027700     MOVE MSGI-IDFKNGRP            TO W-IDFKNGRP                          
027800                                      W-IDFKNGRP-FOM                      
027810                                      W-IDFKNGRP-TOM                      
027900                                                                          
028000     IF GOOD-MID OR KEYS-OK                                               
028100       MOVE MSGI-IDFKNGRP          TO MOD-IDFKNGRP-UT                     
028110       INSPECT MOD-IDFKNGRP-UT REPLACING LEADING ZERO BY SPACE            
028200     ELSE                                                                 
028300       MOVE MFS-ERASE-FIELD        TO MOD-IDFKNGRP-UT                     
028400     END-IF                                                               
028500                                                                          
028600     IF KEYS-WRONG                                                        
028700       MOVE ERR-WRONG-KEY          TO MED-IDMFSFEL                        
028800       CALL WMEDKONV USING MED-WMEDAREA                                   
028900       MOVE MED-MFSFEL             TO MOD-TEMFSFEL                        
029000       PERFORM MFS-ERASE-FIELD-IN                                         
029100       PERFORM MFS-ERASE-FIELD-OUT                                        
029200     END-IF                                                               
029300     .                                                                    
029400     EJECT                                                                
029500 C-FIRST-PAGE SECTION.                                                    
029600     MOVE 'C-FIRST-PAGE                '  TO CURRENT-SECTION              
029700                                                                          
029800     MOVE INF-FIRST-PAGE           TO MED-IDMFSINF                        
029900     CALL WMEDKONV              USING MED-WMEDAREA                        
030000     MOVE MED-MFSINF               TO MOD-TEMFSFEL                        
030100                                                                          
030200     PERFORM MFS-ERASE-FIELD-IN                                           
030300     .                                                                    
030400     EJECT                                                                
030500 D-NEXT-PAGE SECTION.                                                     
030600     MOVE 'D-NEXT-PAGE                 '  TO CURRENT-SECTION              
030700                                                                          
030800     IF SAVE-IDTRANS = '2153'                                             
030900       MOVE SAVE-IDFKNGRP-FOM-NEXT TO W-IDFKNGRP-FOM                      
031000       MOVE SAVE-IDFKNGRP-TOM-NEXT TO W-IDFKNGRP-TOM                      
031100     ELSE                                                                 
031200       PERFORM MFS-ERASE-FIELD-IN                                         
031300     END-IF                                                               
031400     .                                                                    
031500     EJECT                                                                
031600 E-SAME-PAGE SECTION.                                                     
031700     MOVE 'E-SAME-PAGE                 '  TO CURRENT-SECTION              
031800                                                                          
031900     IF SAVE-IDTRANS = '2153' OR '0551'                                   
032000       MOVE SAVE-IDFKNGRP-FOM-ENTER  TO W-IDFKNGRP-FOM                    
032100       MOVE SAVE-IDFKNGRP-TOM-ENTER  TO W-IDFKNGRP-TOM                    
032200                                                                          
032300       IF MID-IDFKNGRP-IN       = ALL '+'                                 
032400      AND MID-KDCMD-UPD (01)    = ALL '+'                                 
032500      AND MID-KDCMD-UPD (02)    = ALL '+'                                 
032600      AND MID-KDCMD-UPD (03)    = ALL '+'                                 
032700      AND MID-KDCMD-UPD (04)    = ALL '+'                                 
032800      AND MID-KDCMD-UPD (05)    = ALL '+'                                 
032900      AND MID-KDCMD-UPD (06)    = ALL '+'                                 
033000      AND MID-KDCMD-UPD (07)    = ALL '+'                                 
033100      AND MID-KDCMD-UPD (08)    = ALL '+'                                 
033200      AND MID-KDCMD-UPD (09)    = ALL '+'                                 
033300      AND MID-KDCMD-UPD (10)    = ALL '+'                                 
033400      AND MID-KDCMD-UPD (11)    = ALL '+'                                 
033500      AND MID-KDCMD-UPD (12)    = ALL '+'                                 
033600      AND MID-KDCMD-UPD (13)    = ALL '+'                                 
033700      AND MID-UPD-AREA          = ALL '+'                                 
033800         PERFORM MFS-ERASE-FIELD-IN                                       
033900       ELSE                                                               
034000         MOVE INF-PRESS-PF11        TO MED-IDMFSINF                       
034100         CALL WMEDKONV           USING MED-WMEDAREA                       
034200         MOVE MED-MFSINF            TO MOD-TEMFSFEL                       
034300         PERFORM EA-MID-INDATA-FOR-MOD                                    
034400       END-IF                                                             
034500     ELSE                                                                 
034600       PERFORM MFS-ERASE-FIELD-IN                                         
034700     END-IF                                                               
034800     .                                                                    
034900     EJECT                                                                
035000 EA-MID-INDATA-FOR-MOD SECTION.                                           
035100     MOVE 'EA-MID-INDATA-FOR-MOD       '  TO CURRENT-SECTION              
035200                                                                          
035300* * * * * FOR EVERY MID-FIELD                                             
035400* * * * * IF MID-FIELD NOT = ALL '+' MOVE MID-FIELD TO MOD-INPUT-F        
035500* * * * *        MOVE MFS-ADD-READ-FIELD TO MOD-INDATA-ATTR               
035600* * * * * ELSE ERASE MOD-INPUT-FIELD                                      
035700                                                                          
035800     MOVE +1                          TO INDX                             
035810     MOVE +0                          TO INDX-COPY-DEL                    
035900     PERFORM UNTIL INDX > MAX-INDX                                        
036000       IF MID-KDCMD-UPD (INDX) NOT = ALL '+' AND SPACE                    
036002         MOVE MID-KDCMD-UPD    (INDX) TO MOD-KDCMD-UPD     (INDX)         
036003         MOVE MFS-ADD-READ-FIELD      TO MOD-KDCMD-UPD-ATTR(INDX)         
036004         MOVE MID-IDFKNGRP-FOM (INDX) TO MOD-IDFKNGRP-FOM  (INDX)         
036005         MOVE STRECK                  TO MOD-BETEXT        (INDX)         
036006         MOVE MID-IDFKNGRP-TOM (INDX) TO MOD-IDFKNGRP-TOM  (INDX)         
036007         MOVE MID-KVAARLF      (INDX) TO MOD-KVAARLF       (INDX)         
036011         ADD +1                       TO INDX-COPY-DEL                    
036012         MOVE INDX                    TO INDX-SAVE                        
036013       ELSE                                                               
036014         MOVE MFS-ERASE-FIELD         TO MOD-KDCMD-UPD     (INDX)         
036015                                         MOD-IDFKNGRP-FOM  (INDX)         
036016                                         MOD-BETEXT        (INDX)         
036017                                         MOD-IDFKNGRP-TOM  (INDX)         
036018                                         MOD-KVAARLF       (INDX)         
036019       END-IF                                                             
036020       ADD +1                         TO INDX                             
036021     END-PERFORM                                                          
036022                                                                          
036023     MOVE NOO                         TO W-COPY-LINE                      
036024     IF INDX-COPY-DEL = +1                                                
036025        IF MID-KDCMD-UPD (INDX-SAVE) = 'C' OR 'K'                         
036026          MOVE MFS-ERASE-FIELD        TO MOD-KDCMD-UPD(INDX-SAVE)         
036028          MOVE SPACE                  TO MOD-KDCMD-UPD(INDX-SAVE)         
036029          MOVE MID-IDFKNGRP-FOM(INDX-SAVE)                                
036030                                      TO MID-IDFKNGRP-FOM-UPD             
036031          MOVE MID-IDFKNGRP-TOM(INDX-SAVE)                                
036032                                      TO MID-IDFKNGRP-TOM-UPD             
036040          MOVE MID-KVAARLF     (INDX-SAVE)                                
036050                                      TO MID-KVAARLF-UPD                  
036052          INSPECT MID-KVAARLF-UPD REPLACING LEADING SPACE BY ZERO         
036060          MOVE YES                    TO W-COPY-LINE                      
037501        END-IF                                                            
037502     END-IF                                                               
037503                                                                          
037700     IF MID-IDFKNGRP-FOM-UPD NOT = ALL '+'                                
037800        MOVE MID-IDFKNGRP-FOM-UPD TO MOD-IDFKNGRP-FOM-UPD                 
037810        IF W-COPY-LINE = YES                                              
037814          MOVE MFS-CLOSE-FIELD                                            
037816                                  TO MOD-IDFKNGRP-FOM-UPD-ATTR            
037820        ELSE                                                              
037900          MOVE MFS-ADD-READ-FIELD TO MOD-IDFKNGRP-FOM-UPD-ATTR            
037910        END-IF                                                            
038000     ELSE                                                                 
038100        MOVE MFS-ERASE-FIELD      TO MOD-IDFKNGRP-FOM-UPD                 
038200     END-IF                                                               
038300                                                                          
038400     IF MID-IDFKNGRP-TOM-UPD NOT = ALL '+'                                
038500        MOVE MID-IDFKNGRP-TOM-UPD TO MOD-IDFKNGRP-TOM-UPD                 
038510        IF W-COPY-LINE = YES                                              
038530          MOVE MFS-CLOSE-FIELD                                            
038540                                  TO MOD-IDFKNGRP-TOM-UPD-ATTR            
038550        ELSE                                                              
038600          MOVE MFS-ADD-READ-FIELD TO MOD-IDFKNGRP-TOM-UPD-ATTR            
038610        END-IF                                                            
038700     ELSE                                                                 
038800        MOVE MFS-ERASE-FIELD      TO MOD-IDFKNGRP-TOM-UPD                 
038900     END-IF                                                               
039000                                                                          
039100     IF MID-KVAARLF-UPD NOT = ALL '+'                                     
039200        MOVE MID-KVAARLF-UPD      TO MOD-KVAARLF-UPD                      
039210        IF W-COPY-LINE = YES                                              
039220          MOVE MFS-OPEN-ALPHA-FIELD                                       
039240                                  TO MOD-KVAARLF-UPD-ATTR                 
039250        ELSE                                                              
039300          MOVE MFS-ADD-READ-FIELD TO MOD-KVAARLF-UPD-ATTR                 
039310        END-IF                                                            
039400     ELSE                                                                 
039500        MOVE MFS-ERASE-FIELD      TO MOD-KVAARLF-UPD                      
039600     END-IF                                                               
039700     .                                                                    
039800     EJECT                                                                
039900 F-READ-SHOW-INFO SECTION.                                                
040000     MOVE 'F-READ-SHOW-INFO            '  TO CURRENT-SECTION              
040100                                                                          
040200     MOVE +1                           TO INDX                            
040400                                                                          
040410     PERFORM IMS-GU-WDGX1143                                              
040500     PERFORM IMS-GNP-WDGX1144-IDFKNGRP-F-T                                
040510     IF SEGMENT-MISSING                                                   
040512        PERFORM IMS-GU-WDGX1143                                           
040513        PERFORM IMS-GNP-WDGX1144-IDFKNGRP                                 
040520     END-IF                                                               
040530                                                                          
040600     PERFORM UNTIL SEGMENT-MISSING OR INDX > MAX-INDX                     
040700                                                                          
040800        MOVE 1144-IDFKNGRP-FOM      TO MOD-IDFKNGRP-FOM (INDX)            
040900        MOVE STRECK                 TO MOD-BETEXT       (INDX)            
041000        MOVE 1144-IDFKNGRP-TOM      TO MOD-IDFKNGRP-TOM (INDX)            
041100        MOVE 1144-KVAARLF           TO MOD-KVAARLF      (INDX)            
041200                                                                          
041300        IF INDX = +1                                                      
041400          MOVE MOD-IDFKNGRP-FOM(01) TO SAVE-IDFKNGRP-FOM-ENTER            
041500                                       SAVE-IDFKNGRP-FOM-NEXT             
041600          MOVE MOD-IDFKNGRP-TOM(01) TO SAVE-IDFKNGRP-TOM-ENTER            
041700                                       SAVE-IDFKNGRP-TOM-NEXT             
041800        END-IF                                                            
041900                                                                          
042000        PERFORM IMS-GNP-WDGX1144-IDFKNGRP                                 
042100                                                                          
042200        ADD +1                  TO INDX                                   
042300     END-PERFORM                                                          
042400                                                                          
042500     IF SEGMENT-FOUND                                                     
042600       IF INDX > MAX-INDX                                                 
042700         MOVE 1144-IDFKNGRP-FOM      TO SAVE-IDFKNGRP-FOM-NEXT            
042800         MOVE 1144-IDFKNGRP-TOM      TO SAVE-IDFKNGRP-TOM-NEXT            
042900         MOVE INF-MORE-INFO-EXISTS   TO MED-IDMFSINF                      
043000         CALL WMEDKONV            USING MED-WMEDAREA                      
043100         MOVE MED-TEMFSINF           TO MOD-TEMFSINF                      
043200       END-IF                                                             
043300     END-IF                                                               
043400                                                                          
043500     PERFORM UNTIL INDX > MAX-INDX                                        
043600        MOVE MFS-CLOSE-FIELD       TO MOD-KDCMD-UPD-ATTR(INDX)            
043700        MOVE MFS-ERASE-FIELD       TO MOD-KDCMD-UPD     (INDX)            
043800                                      MOD-IDFKNGRP-FOM  (INDX)            
043900                                      MOD-BETEXT        (INDX)            
044000                                      MOD-IDFKNGRP-TOM  (INDX)            
044100                                      MOD-KVAARLF       (INDX)            
044200        ADD 1                      TO INDX                                
044300     END-PERFORM                                                          
044400                                                                          
044500                                                                          
044600     MOVE '002'                    TO MSGI-KDCALL                         
044700     MOVE '2153'                   TO SAVE-IDTRANS                        
044800     MOVE SAVE-AREA                TO MSGI-SPAR-AREA                      
044900     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
045000     .                                                                    
045100     EJECT                                                                
045200 G-CHECK-INPUT SECTION.                                                   
045300     MOVE 'G-CHECK-INPUT               '  TO CURRENT-SECTION              
045400                                                                          
045410     MOVE SPACE                  TO MED-IDMFSFEL                          
045500     MOVE YES  TO INDATA-SW                                               
045600                                                                          
045800      IF MID-KDCMD-UPD (01)    = ALL '+'                                  
045900     AND MID-KDCMD-UPD (02)    = ALL '+'                                  
046000     AND MID-KDCMD-UPD (03)    = ALL '+'                                  
046100     AND MID-KDCMD-UPD (04)    = ALL '+'                                  
046200     AND MID-KDCMD-UPD (05)    = ALL '+'                                  
046300     AND MID-KDCMD-UPD (06)    = ALL '+'                                  
046400     AND MID-KDCMD-UPD (07)    = ALL '+'                                  
046500     AND MID-KDCMD-UPD (08)    = ALL '+'                                  
046600     AND MID-KDCMD-UPD (09)    = ALL '+'                                  
046700     AND MID-KDCMD-UPD (10)    = ALL '+'                                  
046800     AND MID-KDCMD-UPD (11)    = ALL '+'                                  
046900     AND MID-KDCMD-UPD (12)    = ALL '+'                                  
047000     AND MID-KDCMD-UPD (13)    = ALL '+'                                  
047100     AND MID-UPD-AREA          = ALL '+'                                  
047200       MOVE ERR-PF11-AND-NO-DATA TO MED-IDMFSFEL                          
047300       CALL WMEDKONV          USING MED-WMEDAREA                          
047400       MOVE MED-MFSFEL           TO MOD-TEMFSFEL                          
047500       PERFORM MFS-DONT-TOUCH-FIELD-IN                                    
047600       PERFORM MFS-DONT-TOUCH-FIELD-OUT                                   
047700       MOVE NOO                  TO INDATA-SW                             
047800     ELSE                                                                 
047900       MOVE +0                         TO W-QTY-KDCMD-DEL                 
047910       MOVE +1                         TO INDX                            
048000       PERFORM UNTIL INDX > MAX-INDX                                      
048100        IF MID-KDCMD-UPD (INDX) NOT = ALL '+'                             
048101          MOVE MFS-ALPHA-FIELD-OK      TO MOD-KDCMD-UPD-ATTR(INDX)        
048200          IF MID-KDCMD-UPD (INDX) = 'C' OR 'K'                            
048201             MOVE MFS-ALPHA-FIELD-WRONG TO                                
048202                                       MOD-KDCMD-UPD-ATTR(INDX)           
048203             MOVE ERR-CONFLICT-CHOICE  TO MED-IDMFSFEL                    
048204             MOVE NOO                 TO INDATA-SW                        
048205          ELSE                                                            
048206            IF MID-KDCMD-UPD (INDX) = 'B' OR 'D'                          
048207              ADD +1                   TO W-QTY-KDCMD-DEL                 
048210              IF W-QTY-KDCMD-DEL > +1                                     
048212                MOVE MFS-ALPHA-FIELD-WRONG TO                             
048213                                         MOD-KDCMD-UPD-ATTR(INDX)         
048214                MOVE ERR-CONFLICT-CHOICE  TO MED-IDMFSFEL                 
048220                MOVE NOO               TO INDATA-SW                       
048222              END-IF                                                      
048223            END-IF                                                        
048224          END-IF                                                          
048225        END-IF                                                            
049100        ADD +1                         TO INDX                            
049200       END-PERFORM                                                        
049300                                                                          
049400       IF MID-UPD-AREA = ALL '+'                                          
049500         CONTINUE                                                         
049600       ELSE                                                               
049720         IF MID-IDFKNGRP-FOM-UPD = ALL '+' OR ZERO                        
049800           MOVE MFS-NUM-FIELD-WRONG  TO MOD-IDFKNGRP-FOM-UPD-ATTR         
049900           MOVE NOO                  TO INDATA-SW                         
050000         ELSE                                                             
050100           IF (MID-IDFKNGRP-FOM-UPD NOT NUMERIC)                          
050110           OR (W-QTY-KDCMD-DEL = +1)                                      
050200            MOVE MFS-NUM-FIELD-WRONG TO MOD-IDFKNGRP-FOM-UPD-ATTR         
050300             MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL                    
050400             MOVE NOO                TO INDATA-SW                         
050500           ELSE                                                           
051200             MOVE MFS-NUM-FIELD-OK   TO MOD-IDFKNGRP-FOM-UPD-ATTR         
051400           END-IF                                                         
051500         END-IF                                                           
051600                                                                          
051700         IF MID-IDFKNGRP-TOM-UPD = ALL '+' OR ZERO                        
051800           MOVE MFS-NUM-FIELD-WRONG  TO MOD-IDFKNGRP-TOM-UPD-ATTR         
051900           MOVE NOO                  TO INDATA-SW                         
052000         ELSE                                                             
052100           IF (MID-IDFKNGRP-TOM-UPD NOT NUMERIC)                          
052110           OR (W-QTY-KDCMD-DEL = +1)                                      
052200            MOVE MFS-NUM-FIELD-WRONG TO MOD-IDFKNGRP-TOM-UPD-ATTR         
052300             MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL                    
052400             MOVE NOO                TO INDATA-SW                         
052500           ELSE                                                           
053200             MOVE MFS-NUM-FIELD-OK   TO MOD-IDFKNGRP-TOM-UPD-ATTR         
053400           END-IF                                                         
053500         END-IF                                                           
053600                                                                          
053700         IF MID-KVAARLF-UPD = ALL '+'                                     
053800           MOVE MFS-NUM-FIELD-WRONG     TO MOD-KVAARLF-UPD-ATTR           
053900           MOVE NOO                     TO INDATA-SW                      
054000         ELSE                                                             
054010           IF (MID-KVAARLF-UPD NOT NUMERIC)                               
054011           OR (W-QTY-KDCMD-DEL = +1)                                      
054012              MOVE MFS-NUM-FIELD-WRONG  TO MOD-KVAARLF-UPD-ATTR           
054013              MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL                   
054014              MOVE NOO                  TO INDATA-SW                      
054020           ELSE                                                           
054100              MOVE MFS-NUM-FIELD-OK     TO MOD-KVAARLF-UPD-ATTR           
054110           END-IF                                                         
054210         END-IF                                                           
054320                                                                          
054400         IF INDATA-OK                                                     
054500           IF MID-IDFKNGRP-FOM-UPD > MID-IDFKNGRP-TOM-UPD                 
054600             MOVE MFS-NUM-FIELD-WRONG TO MOD-IDFKNGRP-FOM-UPD-ATTR        
054700             MOVE MFS-NUM-FIELD-WRONG TO MOD-IDFKNGRP-TOM-UPD-ATTR        
054800             MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL                    
054900             MOVE NOO                 TO INDATA-SW                        
055000           END-IF                                                         
055100         END-IF                                                           
055200       END-IF                                                             
055300                                                                          
055400       IF INDATA-OK                                                       
055410         IF MID-IDFKNGRP-FOM-UPD = ALL '+'                                
055420        AND MID-IDFKNGRP-TOM-UPD = ALL '+'                                
055430            CONTINUE                                                      
055440         ELSE                                                             
055500            PERFORM GA-CHECK-IDFKNGRP-INTERVAL                            
055510         END-IF                                                           
055600       END-IF                                                             
055601                                                                          
055610       IF INDATA-WRONG                                                    
055700         IF MED-IDMFSFEL = SPACE                                          
055800            MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL                     
055910         END-IF                                                           
056000         CALL WMEDKONV             USING MED-WMEDAREA                     
056100         MOVE MED-MFSFEL              TO MOD-TEMFSFEL                     
056200         PERFORM MFS-DONT-TOUCH-FIELD-OUT                                 
056300         PERFORM MFS-DONT-TOUCH-FIELD-IN                                  
056400       END-IF                                                             
056500     END-IF                                                               
056600     .                                                                    
056700     EJECT                                                                
056800                                                                          
056900 GA-CHECK-IDFKNGRP-INTERVAL            SECTION.                           
057000     MOVE 'GA-CHECK-IDFKNGRP-INTERVAL' TO CURRENT-SECTION                 
057100                                                                          
057200     MOVE MID-IDFKNGRP-FOM-UPD    TO W-IDFKNGRP-FOM-2                     
057210     MOVE MID-IDFKNGRP-TOM-UPD    TO W-IDFKNGRP-TOM-2                     
057220     PERFORM IMS-GU-WDGX1144-X                                            
057230     IF SEGMENT-MISSING                                                   
057300       PERFORM IMS-GU-WDGX1143                                            
057400                                                                          
057500       PERFORM IMS-GNP-WDGX1144                                           
057600       PERFORM UNTIL SEGMENT-MISSING OR INDATA-WRONG                      
057700                                                                          
057930         IF (1144-IDFKNGRP-FOM >= MID-IDFKNGRP-FOM-UPD AND                
057940             1144-IDFKNGRP-FOM <= MID-IDFKNGRP-TOM-UPD)                   
058000         OR (1144-IDFKNGRP-TOM >= MID-IDFKNGRP-FOM-UPD AND                
058100             1144-IDFKNGRP-TOM <= MID-IDFKNGRP-TOM-UPD)                   
058200         OR (1144-IDFKNGRP-FOM >= MID-IDFKNGRP-FOM-UPD AND                
058300             1144-IDFKNGRP-TOM <= MID-IDFKNGRP-TOM-UPD)                   
058400         OR (1144-IDFKNGRP-FOM <= MID-IDFKNGRP-FOM-UPD AND                
058500             1144-IDFKNGRP-TOM >= MID-IDFKNGRP-FOM-UPD)                   
058600         OR (1144-IDFKNGRP-FOM <= MID-IDFKNGRP-TOM-UPD AND                
058700             1144-IDFKNGRP-TOM >= MID-IDFKNGRP-TOM-UPD)                   
058800         OR (1144-IDFKNGRP-FOM < MID-IDFKNGRP-FOM-UPD  AND                
058900             1144-IDFKNGRP-TOM > MID-IDFKNGRP-TOM-UPD)                    
059000          MOVE NOO                  TO INDATA-SW                          
059100          MOVE ERR-LINE-EXIST       TO MED-IDMFSFEL                       
059200          MOVE MFS-NUM-FIELD-WRONG  TO MOD-IDFKNGRP-FOM-UPD-ATTR          
059300          MOVE MFS-NUM-FIELD-WRONG  TO MOD-IDFKNGRP-TOM-UPD-ATTR          
059410         END-IF                                                           
059500                                                                          
059600         PERFORM IMS-GNP-WDGX1144                                         
059700                                                                          
059900       END-PERFORM                                                        
059910     END-IF                                                               
060000                                                                          
060100     .                                                                    
060200     EJECT                                                                
060300 H-UPDATE SECTION.                                                        
060400     MOVE 'H-UPDATE                     ' TO CURRENT-SECTION              
060500                                                                          
060600     MOVE +1                         TO INDX                              
060700     PERFORM UNTIL INDX > MAX-INDX                                        
060800       IF MID-KDCMD-UPD (INDX) NOT = ALL '+'                              
060801         IF MID-KDCMD-UPD (INDX) = 'D' OR 'B'                             
060900            MOVE MID-IDFKNGRP-FOM (INDX) TO W-IDFKNGRP-FOM                
061000            MOVE MID-IDFKNGRP-TOM (INDX) TO W-IDFKNGRP-TOM                
061100            PERFORM IMS-GHU-WDGX1144                                      
061200            IF SEGMENT-FOUND                                              
061300               PERFORM IMS-DLET-WDGX1144                                  
061400            END-IF                                                        
061420         END-IF                                                           
061500       END-IF                                                             
061600       ADD +1                        TO INDX                              
061700     END-PERFORM                                                          
061800                                                                          
061900     IF MID-IDFKNGRP-FOM-UPD NOT = ALL '+'                                
062000        MOVE MID-IDFKNGRP-FOM-UPD        TO W-IDFKNGRP-FOM                
062100        MOVE MID-IDFKNGRP-TOM-UPD        TO W-IDFKNGRP-TOM                
062200        PERFORM IMS-GHU-WDGX1144                                          
062300        IF SEGMENT-FOUND                                                  
062400           MOVE MID-KVAARLF-UPD          TO 1144-KVAARLF                  
062500           PERFORM IMS-REPL-WDGX1144                                      
062600        ELSE                                                              
062700           MOVE MID-IDFKNGRP-FOM-UPD     TO 1144-IDFKNGRP-FOM             
062800           MOVE MID-IDFKNGRP-TOM-UPD     TO 1144-IDFKNGRP-TOM             
062900           MOVE MID-KVAARLF-UPD          TO 1144-KVAARLF                  
063000           PERFORM IMS-ISRT-WDGX1144                                      
063100        END-IF                                                            
063200     END-IF                                                               
063300                                                                          
063400     MOVE INF-UPDATE-DONE TO MED-IDMFSINF                                 
063500     CALL WMEDKONV USING MED-WMEDAREA                                     
063600     MOVE MED-MFSINF TO MOD-TEMFSINF                                      
063700     PERFORM MFS-FORM-ATTR                                                
063800     PERFORM MFS-ERASE-FIELD-IN                                           
063900* * * MFS-DO-NOT-TOUCH-FIELD TO LOCKED VALUES                             
064000     .                                                                    
064100     EJECT                                                                
064200 MFS-ERASE-FIELD-OUT SECTION.                                             
064300                                                                          
064400*    --- ALLA UTDATA-FÄLT                                                 
064500*    --- INCL. SCROLL KEYS                                                
064600     MOVE MFS-ERASE-FIELD          TO MOD-IDFKNGRP-FOM-UPD                
064700                                      MOD-IDFKNGRP-TOM-UPD                
064800                                      MOD-KVAARLF-UPD                     
064900                                                                          
065000*    --- OUTDATA-FIELD ON SCROLL KEYS                                     
065100     MOVE +1                       TO INDX                                
065200     PERFORM UNTIL INDX > MAX-INDX                                        
065300       MOVE MFS-ERASE-FIELD        TO MOD-KDCMD-UPD   (INDX)              
065400                                      MOD-IDFKNGRP-FOM (INDX)             
065500                                      MOD-BETEXT       (INDX)             
065600                                      MOD-IDFKNGRP-TOM (INDX)             
065700                                      MOD-KVAARLF      (INDX)             
065800       ADD +1                      TO INDX                                
065900     END-PERFORM                                                          
066000     .                                                                    
066100     SKIP3                                                                
066200 MFS-ERASE-FIELD-IN SECTION.                                              
066300                                                                          
066400*    --- ALLA INDATA-FÄLT                                                 
066500     MOVE MFS-ERASE-FIELD          TO MOD-IDFKNGRP-FOM-UPD                
066600                                      MOD-IDFKNGRP-TOM-UPD                
066700                                      MOD-KVAARLF-UPD                     
066800                                                                          
066900     MOVE +1 TO INDX                                                      
067000     PERFORM UNTIL INDX > MAX-INDX                                        
067100       MOVE MFS-ERASE-FIELD        TO MOD-KDCMD-UPD   (INDX)              
067200                                      MOD-IDFKNGRP-FOM (INDX)             
067300                                      MOD-BETEXT       (INDX)             
067400                                      MOD-IDFKNGRP-TOM (INDX)             
067500                                      MOD-KVAARLF      (INDX)             
067600       ADD +1                      TO INDX                                
067700     END-PERFORM                                                          
067800     .                                                                    
067900     EJECT                                                                
068000 MFS-DONT-TOUCH-FIELD-OUT  SECTION.                                       
068100                                                                          
068200*    --- ALLA UTDATA-FÄLT                                                 
068300*    --- INCL SCROLL KEYS AND LINEDATA                                    
068400     MOVE MFS-DO-NOT-TOUCH-FIELD   TO MOD-IDFKNGRP-FOM-UPD                
068500                                      MOD-IDFKNGRP-TOM-UPD                
068600                                      MOD-KVAARLF-UPD                     
068700                                                                          
068800*    --- OUTDATA FIELD ON SCROLL KEYS                                     
068900     MOVE +1 TO INDX                                                      
069000     PERFORM UNTIL INDX > MAX-INDX                                        
069100       MOVE MFS-DO-NOT-TOUCH-FIELD TO MOD-KDCMD-UPD   (INDX)              
069200                                      MOD-IDFKNGRP-FOM (INDX)             
069300                                      MOD-BETEXT       (INDX)             
069400                                      MOD-IDFKNGRP-TOM (INDX)             
069500                                      MOD-KVAARLF      (INDX)             
069600       ADD +1 TO INDX                                                     
069700     END-PERFORM                                                          
069800     .                                                                    
069900                                                                          
070000 MFS-DONT-TOUCH-FIELD-IN  SECTION.                                        
070100                                                                          
070200*    --- ALLA INDATA-FÄLT                                                 
070300     MOVE MFS-DO-NOT-TOUCH-FIELD   TO MOD-IDFKNGRP-FOM-UPD                
070400                                      MOD-IDFKNGRP-TOM-UPD                
070500                                      MOD-KVAARLF-UPD                     
070600     MOVE +1 TO INDX                                                      
070700     PERFORM UNTIL INDX > MAX-INDX                                        
070800      MOVE MFS-DO-NOT-TOUCH-FIELD   TO MOD-KDCMD-UPD   (INDX)             
070900                                       MOD-IDFKNGRP-FOM (INDX)            
071000                                       MOD-BETEXT       (INDX)            
071100                                       MOD-IDFKNGRP-TOM (INDX)            
071200                                       MOD-KVAARLF      (INDX)            
071300      ADD +1                        TO INDX                               
071400     END-PERFORM                                                          
071500     .                                                                    
071600     EJECT                                                                
071700 MFS-FORM-ATTR SECTION.                                                   
071800                                                                          
071900*    --- ALL INDATA-FIELDS                                                
072000     MOVE MFS-FORMAT-DEFAULT-ATTR   TO MOD-IDFKNGRP-FOM-UPD-ATTR          
072100                                       MOD-IDFKNGRP-TOM-UPD-ATTR          
072200                                       MOD-KVAARLF-UPD-ATTR               
072300                                                                          
072400     MOVE +1 TO INDX                                                      
072500     PERFORM UNTIL INDX > MAX-INDX                                        
072600      MOVE MFS-FORMAT-DEFAULT-ATTR   TO MOD-KDCMD-UPD-ATTR (INDX)         
072700      ADD +1                         TO INDX                              
072800     END-PERFORM                                                          
072900     .                                                                    
073000     SKIP2                                                                
073100 MFS-READ-IN-AGAIN SECTION.                                               
073200                                                                          
073300*    --- ALL INDATA-FIELDS                                                
073400     MOVE MFS-ADD-READ-FIELD         TO MOD-IDFKNGRP-FOM-UPD-ATTR         
073500                                        MOD-IDFKNGRP-TOM-UPD-ATTR         
073600                                        MOD-KVAARLF-UPD-ATTR              
073700                                                                          
073800     MOVE +1                         TO INDX                              
073900     PERFORM UNTIL INDX > MAX-INDX                                        
074000      MOVE MFS-ADD-READ-FIELD        TO MOD-KDCMD-UPD-ATTR (INDX)         
074100      ADD +1                         TO INDX                              
074200     END-PERFORM                                                          
074300     .                                                                    
074400     EJECT                                                                
074500* --- IMS SECTIONS ---                                                    
074600     SKIP3                                                                
074700 IMS-GET-MSG SECTION.                                                     
074800                                                                          
074900     MOVE '  QC' TO GOOD-STATUSCODES                                      
075000     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
075100     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
075200     PERFORM IMS-STATUSCHECK                                              
075300     .                                                                    
075400     SKIP3                                                                
075500 IMS-INSERT-MSG SECTION.                                                  
075600                                                                          
075700     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
075800     MOVE SPACE TO GOOD-STATUSCODES                                       
075900     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
076000     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
076100     PERFORM IMS-STATUSCHECK                                              
076200     .                                                                    
076300                                                                          
076400 IMS-INSERT-ALTMSG SECTION.                                               
076500                                                                          
076600     MOVE SPACE TO GOOD-STATUSCODES                                       
076700     CALL CBLTDLI USING ISRT ALT-PCB PROG-TO-PROG-SW                      
076800     MOVE ALT-STATUS-CODE TO STATUS-WS                                    
076900     PERFORM IMS-STATUSCHECK                                              
077000     .                                                                    
077100     EJECT                                                                
077200                                                                          
077300 IMS-GU-WDGX1143 SECTION.                                                 
077400     MOVE 'IMS-GU-WDGX1143              ' TO IMS-SECTION                  
077500                                                                          
077600     STRING 'WDG201  (WDGXKEY  =' W-WDGXKEY-1143-X ')'                    
077700            DELIMITED BY SIZE INTO SSA1                                   
077800     MOVE '    '                TO GOOD-STATUSCODES                       
077900     CALL CBLTDLI USING GU WDG2-PCB DLI-IO-WDGX01 SSA1                    
078000     MOVE WDG2-STATUS-CODE      TO STATUS-WS                              
078100     PERFORM IMS-STATUSCHECK                                              
078200     .                                                                    
078300                                                                          
078400 IMS-GNP-WDGX1144-IDFKNGRP-F-T  SECTION.                                  
078500     MOVE 'IMS-GNP-WDGX1144-IDFKNGRP-F-T' TO IMS-SECTION                  
078600                                                                          
078610     STRING 'WDGX1144(IDFKNGRF=<' W-IDFKNGRP-FOM-X                        
078620                    '&IDFKNGRT=>' W-IDFKNGRP-TOM-X ')'                    
078630          DELIMITED BY SIZE  INTO SSA1                                    
079100     MOVE '  GE'               TO GOOD-STATUSCODES                        
079200     CALL CBLTDLI USING GNP WDG2-PCB DLI-IO-WDGX1144 SSA1                 
079300     MOVE WDG2-STATUS-CODE     TO STATUS-WS                               
079400     PERFORM IMS-STATUSCHECK                                              
079500     .                                                                    
079600                                                                          
079620 IMS-GNP-WDGX1144-IDFKNGRP  SECTION.                                      
079621     MOVE 'IMS-GNP-WDGX1144-IDFKNGRP    ' TO IMS-SECTION                  
079630                                                                          
079640     STRING 'WDGX1144(IDFKNGRF=>' W-IDFKNGRP-FOM-X  ')'                   
079650          DELIMITED BY SIZE  INTO SSA1                                    
079660     MOVE '  GE'               TO GOOD-STATUSCODES                        
079670     CALL CBLTDLI USING GNP WDG2-PCB DLI-IO-WDGX1144 SSA1                 
079680     MOVE WDG2-STATUS-CODE     TO STATUS-WS                               
079690     PERFORM IMS-STATUSCHECK                                              
079691     .                                                                    
079692                                                                          
079700 IMS-GNP-WDGX1144  SECTION.                                               
079800     MOVE 'IMS-GNP-WDGX1144             ' TO IMS-SECTION                  
079900                                                                          
080000     MOVE 'WDGX1144 '          TO SSA1                                    
080100     MOVE '  GE'               TO GOOD-STATUSCODES                        
080200     CALL CBLTDLI USING GNP WDG2-PCB DLI-IO-WDGX1144 SSA1                 
080300     MOVE WDG2-STATUS-CODE     TO STATUS-WS                               
080400     PERFORM IMS-STATUSCHECK                                              
080500     .                                                                    
080600     EJECT                                                                
080700 IMS-GU-WDGX1144-X SECTION.                                               
080800     MOVE 'IMS-GU-WDGX1144-X          ' TO IMS-SECTION                    
080900                                                                          
081000     STRING 'WDG201  (WDGXKEY  =' W-WDGXKEY-1143-X ')'                    
081100          DELIMITED BY SIZE  INTO SSA1                                    
081200     STRING 'WDGX1144(IDFKNGRF =' W-IDFKNGRP-FOM-X2                       
081300                    '&IDFKNGRT =' W-IDFKNGRP-TOM-X2 ')'                   
081400          DELIMITED BY SIZE  INTO SSA2                                    
081500     MOVE '  GE'               TO GOOD-STATUSCODES                        
081600     CALL CBLTDLI USING GU WDG2-PCB DLI-IO-WDGX1144 SSA1                  
081700                                                    SSA2                  
081800     MOVE WDG2-STATUS-CODE     TO STATUS-WS                               
081900     PERFORM IMS-STATUSCHECK                                              
082000     .                                                                    
082100                                                                          
082110 IMS-GHU-WDGX1144 SECTION.                                                
082120     MOVE 'IMS-GHU-WDGX1144             ' TO IMS-SECTION                  
082130                                                                          
082140     STRING 'WDG201  (WDGXKEY  =' W-WDGXKEY-1143-X ')'                    
082150          DELIMITED BY SIZE  INTO SSA1                                    
082160     STRING 'WDGX1144(IDFKNGRF =' W-IDFKNGRP-FOM-X                        
082170                    '&IDFKNGRT =' W-IDFKNGRP-TOM-X ')'                    
082180          DELIMITED BY SIZE  INTO SSA2                                    
082190     MOVE '  GE'               TO GOOD-STATUSCODES                        
082191     CALL CBLTDLI USING GHU WDG2-PCB DLI-IO-WDGX1144 SSA1                 
082192                                                     SSA2                 
082193     MOVE WDG2-STATUS-CODE     TO STATUS-WS                               
082194     PERFORM IMS-STATUSCHECK                                              
082195     .                                                                    
082196                                                                          
082200 IMS-REPL-WDGX1144      SECTION.                                          
082300     MOVE 'IMS-REPL-WDGX1144            ' TO IMS-SECTION                  
082400                                                                          
082500     MOVE '  '             TO GOOD-STATUSCODES                            
082600     CALL CBLTDLI USING REPL WDG2-PCB DLI-IO-WDGX1144                     
082700     MOVE WDG2-STATUS-CODE TO STATUS-WS                                   
082800     PERFORM IMS-STATUSCHECK                                              
082900     .                                                                    
083000                                                                          
083100 IMS-ISRT-WDGX1144 SECTION.                                               
083200     MOVE 'IMS-ISRT-WDGX1144            ' TO IMS-SECTION                  
083300                                                                          
083400     STRING 'WDG201  (WDGXKEY  =' W-WDGXKEY-1143-X ')'                    
083500          DELIMITED BY SIZE  INTO SSA1                                    
083600     MOVE 'WDGX1144 '          TO SSA2                                    
083700     MOVE '  II'               TO GOOD-STATUSCODES                        
083800     CALL CBLTDLI USING ISRT WDG2-PCB DLI-IO-WDGX1144 SSA1 SSA2           
083900     MOVE WDG2-STATUS-CODE     TO STATUS-WS                               
084000     PERFORM IMS-STATUSCHECK                                              
084100     .                                                                    
084200                                                                          
084300 IMS-DLET-WDGX1144 SECTION.                                               
084400     MOVE 'IMS-DLET-WDGX1144            ' TO IMS-SECTION                  
084500                                                                          
084600     MOVE '  '                 TO GOOD-STATUSCODES                        
084700     CALL CBLTDLI USING DLET WDG2-PCB DLI-IO-WDGX1144                     
084800     MOVE WDG2-STATUS-CODE     TO STATUS-WS                               
084900     PERFORM IMS-STATUSCHECK                                              
085000     .                                                                    
085100                                                                          
085200     EJECT                                                                
085300 IMS-STATUSCHECK SECTION.                                                 
085400                                                                          
085500     SET STATUS-IX TO 1                                                   
085600     SEARCH GOOD-STATUS                                                   
085700       AT END                                                             
085800         STRING ' INVALID STATUS CODE FROM IMS: ' STATUS-WS               
085900         DELIMITED BY SIZE INTO ERROR-TEXT                                
086000         CALL FELLOG                                                      
086100       WHEN GOOD-STATUS (STATUS-IX) = STATUS-WS                           
086200         CONTINUE                                                         
086300     END-SEARCH                                                           
086400     .                                                                    
086500                                                                          
