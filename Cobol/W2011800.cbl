000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W2011800.                                                
000300 AUTHOR.         RAHUL JAIN.UPPDATERINGSPROGRAM (MPP).                    
000400 DATE-WRITTEN.   NOVEMBER 2011.                                           
000500     REMARKS.                                                             
000600*    FUNKTION.                                                            
000700*       UTFÖR BORTTAG ELLER NYUPPLÄGG AV LEVERANTÖRER PÅ                  
000800*       WDGX-BASEN.IDHTYP = 2205.                                         
000900*                                                                         
001000*       PROGRAMMET UPPDATERAR WDR2  LEVERANTÖRSINFORMATION                
001100*                                   PLANER VIA EDI                        
001200*                                   HTYP=2205, SEGMENT=WDGX2206           
001300*       PROGRAMMET LÄSER WDB6                                             
001400*                                                                         
001500*                                                                         
001600*    INDATA.                                                              
001700*        TRANSAKTION: W2T118                                              
001800*                     W2T118U                                             
001900*        MID:         W2I11801                                            
002000*    UTDATA.                                                              
002100*        MOD:         W2O11801                                            
002200*    SUBPROGRAM.                                                          
002300*        FELLOG                                                           
002400*        CBLTDLI                                                          
002500*                                                                         
002600*    ÄNDRINGAR:                                                           
002700*    2012-09-04  E-TRACKER 10143273 LOCAL SOURCING CHINA                  
002800*                                                                         
002900*    2013-09-12  E-TRACKER 10211060 CHINA CORRECTION 269                  
003000*                                                                         
003100*    2017-06-30  E-TRACKER 10302687 LOCAL SOURCING NA                     
003200*                REWRITE ALL SECTIONS WITHOUT G- AND H- SECTION           
003300*                                                                         
003400                                                                          
003500 ENVIRONMENT DIVISION.                                                    
003600 DATA DIVISION.                                                           
003700 WORKING-STORAGE SECTION.                                                 
003800                                                                          
003900*    -- CHECKED BY WY2000                                                 
004000 77  PROGRAM-NAMN           VALUE 'W2011800'                              
004100                                 PIC  X(08).                              
004200 77  CURRENT-SECTION             PIC  X(16)   VALUE SPACE.                
004300 77  CURRENT-IMS-SECTION         PIC  X(16)   VALUE SPACE.                
004400                                                                          
004500 77  JA                          PIC  X(01)   VALUE 'J'.                  
004600 77  NEJ                         PIC  X(01)   VALUE 'N'.                  
004700 77  YES                         PIC  X(01)   VALUE 'Y'.                  
004800 77  OCH                         PIC  X(01)  VALUE '&'.                   
004900 77  INDX                        PIC S9(04)  VALUE +0   COMP SYNC.        
005000 77  WS-IDLEVNR-SPAR             PIC  X(05).                              
005100 77  WS-IDDC-KEY                 PIC  X(02)  VALUE SPACES.                
005200 77  MAX-IND                     PIC S9(09)  VALUE +10  COMP SYNC.        
005300 77  SW-LEVERANTOR               PIC  X(1)  VALUE 'J'.                    
005400     88  LEVERANTOR-OK                      VALUE 'J'.                    
005401                                                                          
005410 01  WS-VARIABLES.                                                        
005420     03 WS-NDC-40                PIC 9(2)    VALUE 40.                    
005421     03 WS-NDC-41                PIC 9(2)    VALUE 41.                    
005430     03 WS-NDC-49                PIC 9(2)    VALUE 49.                    
005440     03 WS-NDC-70                PIC 9(2)    VALUE 70.                    
005441     03 WS-NDC-71                PIC 9(2)    VALUE 71.                    
005450     03 WS-NDC-79                PIC 9(2)    VALUE 79.                    
005500                                                                          
005510 01  FILLER                      PIC X(7)    VALUE 'WWIDFTG'.             
005520*01 -COPY WWIDFTG                                                         
005530     EJECT                                                                
005540                                                                          
005600 77  KEYS-SW                     PIC X      VALUE 'J'.                    
005700     88 KEYS-OK                             VALUE 'J'.                    
005710     88 KEYS-WRONG                          VALUE 'N'.                    
005720                                                                          
005730 77  WS-IDTRANS                  PIC X(04).                               
005740     88 OWN-MID                             VALUE '2118'.                 
005750     88 GOOD-MID                            VALUE '2111' '2112'           
005760                                                  '2113' '2114'           
005770                                                  '2115' '2116'           
005780                                                  '2117' '2118'.          
005790     88 HELP-MID                            VALUE '0551'.                 
005800                                                                          
005900*------------------------------- SWITCHAR                                 
006000 77  SW-INPUT-RAETT              PIC X(01)  VALUE 'J'.                    
006100     EJECT                                                                
006200*    --- PARAMETERS FOR SUBPROGRAM WMEDKONV                               
006300*01 -COPY WMEDAREA                                                        
006400     SKIP3                                                                
006500 01  MESSAGE-CODES.                                                       
006600     03  INF-FIRST-PAGE          PIC X(3)    VALUE '006'.                 
006700     03  INF-MORE-INFO-EXISTS    PIC X(3)    VALUE '105'.                 
006800     03  INF-PRESS-PF9           PIC X(3)    VALUE '127'.                 
006900     03  INF-PRESS-PF11          PIC X(3)    VALUE '003'.                 
007000     03  ERR-UPDATE-NOT-ALLOWED  PIC X(3)    VALUE '007'.                 
007100     03  ERR-CORR-HILITE-FLDS    PIC X(3)    VALUE '001'.                 
007200     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
007300     03  ERR-NOT-AUTH            PIC X(3)    VALUE '405'.                 
007400     03  ERR-WRONG-SEL-CODE      PIC X(3)    VALUE '416'.                 
007500     03  ERR-NO-LINE-SELECTED    PIC X(3)    VALUE '362'.                 
007600     EJECT                                                                
007700 01  DYNAMISKA-SUBPROGRAM.                                                
007800     03  WMEDKONV                PIC X(08)  VALUE 'WMEDKONV'.             
007900     03  W005INIT                PIC X(08)  VALUE 'W005INIT'.             
008000     03  CBLTDLI                 PIC X(08)  VALUE 'CBLTDLI '.             
008100     03  FELLOG                  PIC X(08)  VALUE 'FELLOG  '.             
008200     EJECT                                                                
008300*    --- PARAMETERS FOR SUB PROGRAM W005INIT                              
008400*                                                                         
008500 01  FILLER                      PIC X(16)   VALUE 'WMSGINIT'.            
008600     SKIP3                                                                
008700*01 -COPY WMSGINIT                                                        
008800     EJECT                                                                
008900*    --- AREA CONTAINING DATA TO BE SAVED BETWEEN DIALOG STEPS            
009000*                                                                         
009100 01  SAVE-AREA.                                                           
009200     03  SAVE-IDTRANS             PIC X(4)   VALUE '2118'.                
009300     03  SAVE-IDLEVNR-ENTER       PIC X(05)  VALUE SPACE.                 
009400     03  SAVE-IDDC-ENTER          PIC X(02)  VALUE SPACE.                 
009500     03  SAVE-IDLEVNR-NEXT        PIC X(05)  VALUE SPACE.                 
009600     03  SAVE-IDDC-NEXT           PIC X(02)  VALUE SPACE.                 
009700     EJECT                                                                
009800 01  NYCKLAR-TILL-DLI.                                                    
009900      03  W-WDGXKEY-ROT.                                                  
010000          05  FILLER             PIC X(04)  VALUE '2205'.                 
010100          05  FILLER             PIC X(26)  VALUE LOW-VALUE.              
010200      03  W-WDGX2206-X.                                                   
010300          05  W-IDLEVNR          PIC X(5).                                
010400          05  W-IDDC             PIC X(2)   VALUE SPACES.                 
011500                                                                          
011600      03  W1-IDDC-MIN-X.                                                  
011700          05 W1-IDDC1-MIN        PIC  X(01).                              
011800          05 W1-IDDC2-MIN        PIC  X(01).                              
011900      03  W1-IDDC-MAX-X.                                                  
012000          05 FILLER              PIC  X(01).                              
012100          05 W1-IDDC2-MAX        PIC  X(01).                              
012200                                                                          
012300     03  W-IDLEVNR-F1-X.                                                  
012400         05  W-IDLEVNR-F1        PIC X(05)  VALUE SPACE.                  
012500     03  W-IDDC-F1-X.                                                     
012600          05 W-IDDC-F1           PIC X(02)  VALUE SPACE.                  
012700                                                                          
012800     03  W-IDDC-X.                                                        
012900          05  W-IDDC-B6          PIC X(02)  VALUE SPACES.                 
013600                                                                          
013700 01  MEDDELANDE.                                                          
013800     03  FEL-1                   PIC X(40) VALUE                          
013900            'PAGE 1 SHOWN, NO MORE SUPPLIERS         '.                   
014000     03  FEL-2                   PIC X(32) VALUE                          
014100            'HIGHLIGHTED FIELDS INCORRECT    '.                           
014200     03  FEL-3                   PIC X(40) VALUE                          
014300            'SUPPLIER ALREADY EXISTS         '.                           
014400     03  FEL-4                   PIC X(32) VALUE                          
014500            'SUPPLIER DOES NOT EXIST         '.                           
014600     03  FEL-5                   PIC X(32) VALUE                          
014700            'SUPPLIER/DC MISSING ON 2111     '.                           
014800     03  MED-1                   PIC X(32) VALUE                          
014900            'MORE INFO ON NEXT PAGE          '.                           
015000     03  MED-2                   PIC X(32) VALUE                          
015100            'PRESS PF11 FOR UPDATE           '.                           
015200     03  MED-3                   PIC X(32) VALUE                          
015300            'UPDATE PERFORMED                '.                           
015400                                                                          
015500                                                                          
015600     EJECT                                                                
015700*                        ****    MFS OCH SKÄRMHANTERING                   
015800 01  FILLER              PIC X(16)   VALUE 'MFS-WS'.                      
015900     SKIP2                                                                
016000*01  MID -COPY W2I11801                                                   
016100     EJECT                                                                
016200 01  FILLER              PIC X(16)   VALUE 'WMSGAREA'.                    
016300     SKIP2                                                                
016400*01  -COPY WMSGAREA                                                       
016500     EJECT                                                                
016600*    03  MOD -COPY W2O11801  -RED MSG-AREA.                               
016700     EJECT                                                                
016800*01  -COPY WMFSAREA.                                                      
016900     EJECT                                                                
017000******************************************************************        
017100*****                                                                     
017200*****    ARBETS-AREOR TILL IMS-SEKTIONERNA                                
017300*****                                                                     
017400 01  IMS-WS.                                                              
017500     03  FILLER                  PIC X(16)   VALUE ' IMS-WS '.            
017600     SKIP3                                                                
017700*****                    **** STATUS-KOD FRÅN IMS                         
017800     03  STATUS-WS               PIC X(2).                                
017900         88  SEGMENT-FINNS                   VALUE '  '.                  
018000         88  SEGMENT-SAKNAS                  VALUE 'GE'.                  
018100         88  SEGMENT-FINNS-REDAN             VALUE 'II'.                  
018200     SKIP3                                                                
018300     03  GODK-STATUSKODER.                                                
018400         05  GODK-STATUS OCCURS 2 INDEXED BY STATUS-IX PIC XX.            
018500     SKIP3                                                                
018600 01  ALL-SSA.                                                             
018700     03 SSA1                     PIC X(128).                              
018800     03 SSA2                     PIC X(128).                              
018900     EJECT                                                                
019000*                            IMS FUNKTIONSKODER                           
019100*01  -COPY W0003                                                          
019200     EJECT                                                                
019300*                            DLI INPUT-OUTPUT AREA                        
019400 01  DLI-IO-AREA.                                                         
019500     03  IO-AREA                 PIC X(60)   VALUE SPACE.                 
019600     SKIP3                                                                
019700*    03  WDR2 -COPY WDGX2206      -PRE WDR2- -RED IO-AREA.                
019800     EJECT                                                                
019900                                                                          
020000 01  FILLER               PIC X(16)   VALUE 'DLI-IO-WDB601'.              
020100 01  DLI-IO-WDB601.                                                       
020200*    03  -COPY WDB601      -PRE WDB601-                                   
020300     EJECT                                                                
020400                                                                          
020500 01  FILLER               PIC X(16)   VALUE 'DLI-IO-WDF116'.              
020600 01  DLI-IO-WDF116.                                                       
020700*    03  -COPY WDF116                                                     
020800     EJECT                                                                
020900                                                                          
021000 LINKAGE SECTION.                                                         
021100     SKIP2                                                                
021200*01  -COPY W0009     -PRE MSG-                                            
021300     EJECT                                                                
021400*01  -COPY W0008     -PRE WDP7-                                           
021500         05  FILLER              PIC X.                                   
021600     EJECT                                                                
021700*01  -COPY W0008     -PRE WDR2-                                           
021800         05  FILLER              PIC X.                                   
021900     EJECT                                                                
022000*01  -COPY W0008     -PRE WDB6-                                           
022100         05  FILLER              PIC X.                                   
022200     EJECT                                                                
022300*01  -COPY W0008     -PRE WDF1-                                           
022400         05  FILLER              PIC X.                                   
022500     EJECT                                                                
022600 PROCEDURE DIVISION USING  MSG-PCB WDP7-PCB WDR2-PCB                      
022700                                   WDB6-PCB WDF1-PCB.                     
022800                                                                          
022900     ENTRY 'DLITCBL' USING MSG-PCB WDP7-PCB WDR2-PCB                      
023000                                   WDB6-PCB WDF1-PCB.                     
023100                                                                          
023200     PERFORM IMS-GET-MSG                                                  
023300     IF SEGMENT-FINNS                                                     
023400        PERFORM A-INIT                                                    
023500        PERFORM B-CHECK-KEYS                                              
023600        IF KEYS-OK                                                        
023700           IF MFS-UPDATE                                                  
023800              PERFORM G-CHECK-INPUT                                       
023900              IF SW-INPUT-RAETT = JA                                      
024000                 PERFORM D-UPPDATERA                                      
024100              END-IF                                                      
024200           ELSE                                                           
024300              IF MFS-FIRST                                                
024400                 PERFORM C-FIRST-PAGE                                     
024500              ELSE                                                        
024600                 IF MFS-NEXT                                              
024700                    PERFORM D-NEXT-PAGE                                   
024800                 ELSE                                                     
024900                    PERFORM E-SAME-PAGE                                   
025000                 END-IF                                                   
025100              END-IF                                                      
025200           END-IF                                                         
025300           PERFORM F-READ-SHOW-INFO                                       
025400        END-IF                                                            
025500                                                                          
025600        COMPUTE MSG-KVLL = LENGTH OF MOD-W2O11801 + 4                     
025700        PERFORM IMS-INSERT-MSG                                            
025800     END-IF                                                               
025900                                                                          
026000     MOVE ZERO TO RETURN-CODE                                             
026100     GOBACK                                                               
026200     .                                                                    
026300     EJECT                                                                
026400 A-INIT             SECTION.                                              
026500     MOVE 'A-INIT          ' TO CURRENT-SECTION.                          
026600     SKIP2                                                                
026700     IF MSG-DUBBLA-TRANSKODER                                             
026800         MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W2I11801               
026900         MOVE MSG-IDTRANS-2        TO MFS-IDTRANS  WS-IDTRANS             
027000         MOVE MSG-KDMFSFOR-2       TO MFS-KDMFSFOR                        
027100         MOVE MSG-KDTRTYP          TO MFS-KDTRTYP                         
027200         MOVE MSG-IDPFK            TO MFS-IDPFK                           
027300     ELSE                                                                 
027400         MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W2I11801                
027500         MOVE MSG-IDTRANS-1 TO MFS-IDTRANS  WS-IDTRANS                    
027600         MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                              
027700         MOVE ' ' TO MFS-KDTRTYP                                          
027800                     MFS-IDPFK                                            
027900     END-IF                                                               
028000                                                                          
028100     MOVE MSG-KDTRTYP              TO MFS-KDTRTYP                         
028200     MOVE MSG-IDPFK                TO MFS-IDPFK                           
028300     MOVE MFS-IDTRANS              TO WS-IDTRANS                          
028400                                                                          
028500     MOVE LOW-VALUE                TO MSG-AREA                            
028600     MOVE 'W2O118N1'               TO MFS-IDMOD                           
028700     MOVE '2118'                   TO MOD-IDTRANS                         
028800                                                                          
028900     MOVE MFS-RENSA-FAELT          TO MOD-TEMFSFEL                        
029000                                     MOD-TEMFSINF                         
029100     IF OWN-MID OR HELP-MID                                               
029200        CONTINUE                                                          
029300     ELSE                                                                 
029400        MOVE SPACE                 TO MFS-KDTRTYP                         
029500        MOVE '7'                   TO MFS-IDPFK                           
029600     END-IF                                                               
029700     .                                                                    
029800     EJECT                                                                
029900 B-CHECK-KEYS SECTION.                                                    
030000     MOVE 'B-CHECK-KEYS      '  TO CURRENT-SECTION                        
030100                                                                          
030200     MOVE ALL '+'                TO MSGI-WMSGINIT                         
030300     MOVE '001'                  TO MSGI-KDCALL                           
030400     MOVE MSG-LTERM-NAME         TO MSGI-IDLTERM-USER                     
030500     MOVE MSG-SIGNON-USERID      TO MSGI-IDUSER                           
030600     MOVE '2118'                 TO MSGI-IDTRANS                          
030700     IF OWN-MID                                                           
030800       MOVE MID-IDLEVNR-IN       TO MSGI-IDLEVNR                          
030900       MOVE MID-IDDC-IN          TO MSGI-IDDC-KEY                         
031000     END-IF                                                               
031100                                                                          
031200     CALL W005INIT USING MSGI-WMSGINIT WDP7-PCB                           
031300     IF MSGI-SPAR-AREA (1:4) = '2118'                                     
031400        MOVE MSGI-SPAR-AREA      TO SAVE-AREA                             
031500     END-IF                                                               
031600                                                                          
031700*    - LANGUAGE TO BE USED BY MEDKONV                                     
031800     MOVE MSGI-IDLAND-SPR        TO MED-IDSKYLT                           
031900                                                                          
032000     MOVE JA                     TO KEYS-SW                               
032100                                                                          
032200*    -- KONTROLL AV IDLEVNR                                               
032300     MOVE MFS-RENSA-FAELT        TO MOD-IDLEVNR-IN                        
032400     IF MID-IDLEVNR-IN NOT = ALL '+'                                      
032500        MOVE '7'                 TO MFS-IDPFK                             
032600        MOVE SPACE               TO MFS-KDTRTYP                           
032700     END-IF                                                               
032800     MOVE MSGI-IDLEVNR           TO W-IDLEVNR                             
032900                                                                          
033000*    -- KONTROLL AV IDDC                                                  
033100     MOVE MFS-RENSA-FAELT        TO MOD-IDDC-IN                           
033200     IF MID-IDDC-IN NOT = ALL '+'                                         
033300        MOVE '7'                 TO MFS-IDPFK                             
033400        MOVE SPACE               TO MFS-KDTRTYP                           
033500     END-IF                                                               
033600     MOVE MSGI-IDDC-KEY          TO W-IDDC                                
033700                                                                          
033800     IF W-IDDC = SPACE                                                    
033900        MOVE MSGI-IDFTG          TO WS-IDFTG                              
034000        IF IDFTG-US                                                       
034100           MOVE WS-NDC-41        TO W1-IDDC-MIN-X                         
034200           MOVE WS-NDC-49        TO W1-IDDC-MAX-X                         
034210           MOVE WS-NDC-40        TO MSGI-IDDC-KEY                         
034300        ELSE                                                              
034400           IF IDFTG-CN                                                    
034500              MOVE WS-NDC-71     TO W1-IDDC-MIN-X                         
034600              MOVE WS-NDC-79     TO W1-IDDC-MAX-X                         
034610              MOVE WS-NDC-70     TO MSGI-IDDC-KEY                         
034700           ELSE                                                           
034800              MOVE NEJ           TO KEYS-SW                               
034900           END-IF                                                         
035000        END-IF                                                            
035100     ELSE                                                                 
035200        MOVE W-IDDC              TO W1-IDDC-MIN-X                         
035300                                    W1-IDDC-MAX-X                         
035400        IF W-IDDC(2:1) = '0'                                              
035500           MOVE '1'              TO W1-IDDC2-MIN                          
035600           MOVE '9'              TO W1-IDDC2-MAX                          
035700        END-IF                                                            
035800     END-IF                                                               
035900                                                                          
036000     IF GOOD-MID OR KEYS-OK                                               
036100       MOVE MSGI-IDLEVNR         TO MOD-IDLEVNR-UT                        
036200       MOVE MSGI-IDDC-KEY        TO MOD-IDDC-UT                           
036300     ELSE                                                                 
036400       MOVE MFS-ERASE-FIELD      TO MOD-IDLEVNR-UT                        
036500                                     MOD-IDDC-UT                          
036600     END-IF                                                               
036700                                                                          
036800     IF KEYS-WRONG                                                        
036900       MOVE ERR-WRONG-KEY        TO MED-IDMFSFEL                          
037000       CALL WMEDKONV          USING MED-WMEDAREA                          
037100       MOVE MED-MFSFEL           TO MOD-TEMFSFEL                          
037200       PERFORM MFS-ERASE-FIELD-IN                                         
037300       PERFORM MFS-ERASE-FIELD-OUT                                        
037400     END-IF                                                               
037500     .                                                                    
037600     EJECT                                                                
037700 C-FIRST-PAGE SECTION.                                                    
037800     MOVE 'C-FIRST-PAGE      '  TO CURRENT-SECTION                        
037900                                                                          
038000     MOVE INF-FIRST-PAGE         TO MED-IDMFSINF                          
038100     CALL WMEDKONV            USING MED-WMEDAREA                          
038200     MOVE MED-MFSINF             TO MOD-TEMFSFEL                          
038300     PERFORM MFS-ERASE-FIELD-IN                                           
038400     PERFORM MFS-ERASE-FIELD-OUT                                          
038500     .                                                                    
038600     EJECT                                                                
038700 D-NEXT-PAGE SECTION.                                                     
038800     MOVE 'D-NEXT-PAGE      '   TO CURRENT-SECTION                        
038900                                                                          
039000     IF SAVE-IDTRANS = '2118'                                             
039100        MOVE SAVE-IDLEVNR-NEXT  TO W-IDLEVNR                              
039200        MOVE SAVE-IDDC-NEXT     TO W-IDDC                                 
039300     ELSE                                                                 
039400        PERFORM MFS-ERASE-FIELD-IN                                        
039500     END-IF                                                               
039600     .                                                                    
039700     EJECT                                                                
039800 E-SAME-PAGE SECTION.                                                     
039900     MOVE 'E-SAME-PAGE       '  TO CURRENT-SECTION                        
040000                                                                          
040100     IF SAVE-IDTRANS = '2118' OR '0551'                                   
040200        MOVE SAVE-IDLEVNR-ENTER TO W-IDLEVNR                              
040300        MOVE SAVE-IDDC-ENTER    TO W-IDDC                                 
040400       IF  MID-IDLEVNR      = ALL '+'                                     
040500       AND MID-IDDC         = ALL '+'                                     
040600       AND MID-KDEDI        = ALL '+'                                     
040700       AND MID-FLAVIS       = ALL '+'                                     
040800       AND MID-IDLEVKND     = ALL '+'                                     
040900       AND MID-FLODETTE     = ALL '+'                                     
041000       AND MID-KDCMD        = ALL '+'                                     
041100           PERFORM MFS-ERASE-FIELD-IN                                     
041200        ELSE                                                              
041300           MOVE INF-PRESS-PF11  TO MED-IDMFSINF                           
041400           CALL WMEDKONV     USING MED-WMEDAREA                           
041500           MOVE MED-MFSINF      TO MOD-TEMFSFEL                           
041600           PERFORM EA-MID-INDATA-FOR-MOD                                  
041700        END-IF                                                            
041800     ELSE                                                                 
041900        PERFORM MFS-ERASE-FIELD-IN                                        
042000     END-IF                                                               
042100     .                                                                    
042200     EJECT                                                                
042300 EA-MID-INDATA-FOR-MOD SECTION.                                           
042400     MOVE 'EA-MID-INDATA-FOR-MOD' TO CURRENT-SECTION                      
042500                                                                          
042600     IF MID-IDLEVNR NOT = ALL '+'                                         
042700        MOVE MID-IDLEVNR             TO MOD-IDLEVNR-UPD                   
042800        MOVE MFS-ADD-READ-FIELD      TO MOD-IDLEVNR-UPD-ATTR              
042900        MOVE MFS-ROER-EJ-FAELT       TO MOD-IDLEVNR-UPD                   
043000     ELSE                                                                 
043100        MOVE MFS-ERASE-FIELD         TO MOD-IDLEVNR-UPD-ATTR              
043200     END-IF                                                               
043300                                                                          
043400     IF MID-IDDC    NOT = ALL '+'                                         
043500        MOVE MID-IDDC                TO MOD-IDDC-UPD                      
043600        MOVE MFS-ADD-READ-FIELD      TO MOD-IDDC-UPD-ATTR                 
043700        MOVE MFS-ROER-EJ-FAELT       TO MOD-IDDC-UPD                      
043800     ELSE                                                                 
043900        MOVE MFS-ERASE-FIELD         TO MOD-IDDC-UPD-ATTR                 
044000     END-IF                                                               
044100                                                                          
044200     IF MID-IDLEVKND NOT = ALL '+'                                        
044300        MOVE MID-IDLEVKND            TO MOD-IDLEVKND-UPD                  
044400        MOVE MFS-ADD-READ-FIELD      TO MOD-IDLEVKND-UPD-ATTR             
044500        MOVE MFS-ROER-EJ-FAELT       TO MOD-IDLEVKND-UPD                  
044600     ELSE                                                                 
044700        MOVE MFS-ERASE-FIELD         TO MOD-IDLEVKND-UPD-ATTR             
044800     END-IF                                                               
044900                                                                          
045000     IF MID-KDEDI NOT = ALL '+'                                           
045100        MOVE MID-KDEDI               TO MOD-KDEDI-UPD                     
045200        MOVE MFS-ADD-READ-FIELD      TO MOD-KDEDI-UPD-ATTR                
045300        MOVE MFS-ROER-EJ-FAELT       TO MOD-KDEDI-UPD                     
045400     ELSE                                                                 
045500        MOVE MFS-ERASE-FIELD         TO MOD-KDEDI-UPD-ATTR                
045600     END-IF                                                               
045700                                                                          
045800     IF MID-FLAVIS NOT = ALL '+'                                          
045900        MOVE MID-FLAVIS              TO MOD-FLAVIS-UPD                    
046000        MOVE MFS-ADD-READ-FIELD      TO MOD-FLAVIS-UPD-ATTR               
046100        MOVE MFS-ROER-EJ-FAELT       TO MOD-FLAVIS-UPD                    
046200     ELSE                                                                 
046300        MOVE MFS-ERASE-FIELD         TO MOD-FLAVIS-UPD-ATTR               
046400     END-IF                                                               
046500                                                                          
046600     IF MID-FLODETTE NOT = ALL '+'                                        
046700        MOVE MID-FLODETTE            TO MOD-FLODETTE-UPD                  
046800        MOVE MFS-ADD-READ-FIELD      TO MOD-FLODETTE-UPD-ATTR             
046900        MOVE MFS-ROER-EJ-FAELT       TO MOD-FLODETTE-UPD                  
047000     ELSE                                                                 
047100        MOVE MFS-ERASE-FIELD         TO MOD-FLODETTE-UPD-ATTR             
047200     END-IF                                                               
047300                                                                          
047400     IF MID-KDCMD NOT = ALL '+'                                           
047500        MOVE MID-KDCMD               TO MOD-KDCMD-UPD                     
047600        MOVE MFS-ADD-READ-FIELD      TO MOD-KDCMD-UPD-ATTR                
047700        MOVE MFS-ROER-EJ-FAELT       TO MOD-KDCMD-UPD                     
047800     ELSE                                                                 
047900        MOVE MFS-ERASE-FIELD         TO MOD-KDCMD-UPD-ATTR                
048000     END-IF                                                               
048100                                                                          
048200     .                                                                    
048300     EJECT                                                                
048400 G-CHECK-INPUT SECTION.                                                   
048500     MOVE 'G-CHECK-INPUT        ' TO CURRENT-SECTION                      
048600                                                                          
048700     MOVE JA                            TO SW-INPUT-RAETT                 
048800                                                                          
048900     PERFORM GA-KOLLA-INPUT                                               
049000     IF SW-INPUT-RAETT = JA                                               
049100       MOVE JA           TO SW-LEVERANTOR                                 
049200       IF MID-KDCMD-INSERT                                                
049300         IF MID-IDLEVNR       NOT = ALL '+' AND                           
049400           MID-FLODETTE      NOT = ALL '+' AND                            
049500           MID-IDDC          NOT = ALL '+' AND                            
049600           MID-IDLEVKND      NOT = ALL '+' AND                            
049700           MID-KDEDI         NOT = ALL '+' AND                            
049800           MID-FLAVIS        NOT = ALL '+'                                
049900                                                                          
050000           PERFORM GB-KOLLA-LEVERANTOR                                    
050100           IF LEVERANTOR-OK                                               
050200              MOVE MFS-ALFA-FAELT-RAETT TO MOD-IDLEVNR-UPD-ATTR           
050300                                           MOD-IDDC-UPD-ATTR              
050400                                           MOD-FLODETTE-UPD-ATTR          
050500                                           MOD-IDLEVKND-UPD-ATTR          
050600                                           MOD-KDEDI-UPD-ATTR             
050700                                           MOD-FLAVIS-UPD-ATTR            
050800                                                                          
050900              MOVE MFS-RENSA-FAELT      TO MOD-IDLEVNR-UPD                
051000                                           MOD-FLODETTE-UPD               
051100                                           MOD-IDDC-UPD                   
051200                                           MOD-IDLEVKND-UPD               
051300                                           MOD-KDEDI-UPD                  
051400                                           MOD-FLAVIS-UPD                 
051500           ELSE                                                           
051600              MOVE NEJ                  TO SW-INPUT-RAETT                 
051700           END-IF                                                         
051800         ELSE                                                             
051900           IF MID-IDLEVNR       NOT = ALL '+' AND                         
052000              MID-FLODETTE      NOT = ALL '+' AND                         
052100              MID-IDDC          NOT = ALL '+' AND                         
052200              MID-IDLEVKND          = ALL '+' AND                         
052300              MID-KDEDI             = ALL '+' AND                         
052400              MID-FLAVIS            = ALL '+'                             
052500                                                                          
052600                                                                          
052700              MOVE SPACE             TO MID-IDLEVKND                      
052800                                        MID-KDEDI                         
052900                                        MID-FLAVIS                        
053000                                                                          
053100              PERFORM GB-KOLLA-LEVERANTOR                                 
053200              IF LEVERANTOR-OK                                            
053300                MOVE MFS-ALFA-FAELT-RAETT TO MOD-IDLEVNR-UPD-ATTR         
053400                                             MOD-IDDC-UPD-ATTR            
053500                                             MOD-FLODETTE-UPD-ATTR        
053600                                             MOD-IDLEVKND-UPD-ATTR        
053700                                             MOD-KDEDI-UPD-ATTR           
053800                                             MOD-FLAVIS-UPD-ATTR          
053900                                                                          
054000                MOVE MFS-RENSA-FAELT   TO MOD-IDLEVNR-UPD                 
054100                                             MOD-FLODETTE-UPD             
054200                                             MOD-IDDC-UPD                 
054300                                             MOD-IDLEVKND-UPD             
054400                                             MOD-KDEDI-UPD                
054500                                             MOD-FLAVIS-UPD               
054600              ELSE                                                        
054700                 MOVE NEJ               TO SW-INPUT-RAETT                 
054800              END-IF                                                      
054900           ELSE                                                           
055000              MOVE NEJ       TO SW-INPUT-RAETT                            
055100           END-IF                                                         
055200         END-IF                                                           
055300       END-IF                                                             
055400       IF SW-INPUT-RAETT = NEJ                                            
055500          IF MID-IDLEVNR              = ALL '+'                           
055600             MOVE MFS-ALFA-FAELT-FEL TO MOD-IDLEVNR-UPD-ATTR              
055700          END-IF                                                          
055800          IF MID-IDDC                 = ALL '+'                           
055900             MOVE MFS-ALFA-FAELT-FEL TO MOD-IDDC-UPD-ATTR                 
056000          END-IF                                                          
056100          IF MID-FLODETTE             = ALL '+'                           
056200             MOVE MFS-ALFA-FAELT-FEL TO MOD-FLODETTE-UPD-ATTR             
056300          END-IF                                                          
056400          IF MID-IDLEVKND             = ALL '+'                           
056500             MOVE MFS-ALFA-FAELT-FEL TO MOD-IDLEVKND-UPD-ATTR             
056600          END-IF                                                          
056700          IF MID-KDEDI                = ALL '+'                           
056800             MOVE MFS-ALFA-FAELT-FEL TO MOD-KDEDI-UPD-ATTR                
056900          END-IF                                                          
057000          IF MID-FLAVIS               = ALL '+'                           
057100             MOVE MFS-ALFA-FAELT-FEL TO MOD-FLAVIS-UPD-ATTR               
057200          END-IF                                                          
057300          MOVE MFS-ROER-EJ-FAELT       TO MOD-IDLEVNR-UPD                 
057400                                       MOD-FLODETTE-UPD                   
057500                                       MOD-IDDC-UPD                       
057600                                       MOD-IDLEVKND-UPD                   
057700                                       MOD-KDEDI-UPD                      
057800                                       MOD-FLAVIS-UPD                     
057900       END-IF                                                             
058000       IF SW-INPUT-RAETT = NEJ                                            
058100          IF LEVERANTOR-OK                                                
058200             MOVE FEL-2                 TO MOD-TEMFSFEL                   
058300          ELSE                                                            
058400             MOVE FEL-5                 TO MOD-TEMFSFEL                   
058500          END-IF                                                          
058600          PERFORM S02-ROER-EJ-FAELT                                       
058700       END-IF                                                             
058800     END-IF                                                               
058900                                                                          
059000     IF SW-INPUT-RAETT = NEJ                                              
059100        MOVE SAVE-IDLEVNR-ENTER         TO W-IDLEVNR                      
059200        MOVE SAVE-IDDC-ENTER            TO W-IDDC                         
059300     END-IF                                                               
059400     .                                                                    
059500     EJECT                                                                
059600                                                                          
059700 GA-KOLLA-INPUT SECTION.                                                  
059800     MOVE 'GB-KOLLA-INPUT       ' TO CURRENT-SECTION                      
059900                                                                          
060000     IF MID-KDCMD-DELETE                                                  
060100        SET MID-INFO-IND  TO  1                                           
060200        MOVE MID-INFO-IDLEVNR (MID-INFO-IND) TO                           
060300                              WS-IDLEVNR-SPAR                             
060400        MOVE MID-INFO-IDDC (MID-INFO-IND)    TO W-IDDC                    
060500        PERFORM UNTIL MID-INFO-IND = MAX-IND   OR                         
060600                (MID-IDLEVNR = MID-INFO-IDLEVNR                           
060700                                  (MID-INFO-IND)                          
060800                AND MID-IDDC = MID-INFO-IDDC                              
060900                                  (MID-INFO-IND))                         
061000           SET MID-INFO-IND UP BY 1                                       
061100           MOVE MID-INFO-IDLEVNR (MID-INFO-IND) TO                        
061200                                 WS-IDLEVNR-SPAR                          
061300           MOVE MID-INFO-IDDC (MID-INFO-IND) TO W-IDDC                    
061400        END-PERFORM                                                       
061500        IF MID-INFO-IND < MAX-IND                                         
061600        OR MID-INFO-IND = MAX-IND                                         
061700           MOVE MFS-ALFA-FAELT-RAETT TO MOD-IDLEVNR-UPD-ATTR              
061800        ELSE                                                              
061900           MOVE MFS-ALFA-FAELT-FEL   TO MOD-IDLEVNR-UPD-ATTR              
062000           MOVE NEJ                  TO SW-INPUT-RAETT                    
062100        END-IF                                                            
062200     ELSE                                                                 
062300        MOVE MFS-ALFA-FAELT-RAETT    TO MOD-IDLEVNR-UPD-ATTR              
062400     END-IF                                                               
062500     MOVE MFS-ROER-EJ-FAELT          TO MOD-IDLEVNR-UPD                   
062600                                                                          
062700     IF MID-KDEDI = ALL '+'                                               
062800        IF MID-KDCMD-INSERT                                               
062900           MOVE MFS-ALFA-FAELT-RAETT TO MOD-KDEDI-UPD-ATTR                
063000        END-IF                                                            
063100     ELSE                                                                 
063200        IF MID-KDEDI = 'V' OR 'O' OR 'T' OR 'F' OR 'E'                    
063300           MOVE MFS-ALFA-FAELT-RAETT    TO MOD-KDEDI-UPD-ATTR             
063400        ELSE                                                              
063500           MOVE MFS-ALFA-FAELT-FEL   TO MOD-KDEDI-UPD-ATTR                
063600           MOVE NEJ                  TO SW-INPUT-RAETT                    
063700        END-IF                                                            
063800        MOVE MFS-ROER-EJ-FAELT TO MOD-KDEDI-UPD                           
063900     END-IF                                                               
064000                                                                          
064100     IF MID-FLAVIS = ALL '+'                                              
064200        IF MID-KDCMD-INSERT                                               
064300           MOVE MFS-ALFA-FAELT-RAETT TO MOD-FLAVIS-UPD-ATTR               
064400        END-IF                                                            
064500     ELSE                                                                 
064600        IF MID-FLAVIS = 'J' OR 'Y' OR 'N'                                 
064700           MOVE MFS-ALFA-FAELT-RAETT    TO MOD-FLAVIS-UPD-ATTR            
064800        ELSE                                                              
064900           MOVE MFS-ALFA-FAELT-FEL   TO MOD-FLAVIS-UPD-ATTR               
065000           MOVE NEJ                  TO SW-INPUT-RAETT                    
065100        END-IF                                                            
065200        MOVE MFS-ROER-EJ-FAELT TO MOD-FLAVIS-UPD                          
065300     END-IF                                                               
065400                                                                          
065500     IF MID-FLODETTE = ALL '+'                                            
065600        IF MID-KDCMD-INSERT                                               
065700           MOVE MFS-ALFA-FAELT-RAETT TO MOD-FLODETTE-UPD-ATTR             
065800        END-IF                                                            
065900     ELSE                                                                 
066000        IF MID-FLODETTE = 'J' OR 'Y' OR 'N'                               
066100           MOVE MFS-ALFA-FAELT-RAETT    TO MOD-FLODETTE-UPD-ATTR          
066200        ELSE                                                              
066300           MOVE MFS-ALFA-FAELT-FEL   TO MOD-FLODETTE-UPD-ATTR             
066400           MOVE NEJ                  TO SW-INPUT-RAETT                    
066500        END-IF                                                            
066600        MOVE MFS-ROER-EJ-FAELT TO MOD-FLODETTE-UPD                        
066700     END-IF                                                               
066800                                                                          
066900     IF MID-IDDC = ALL '+'                                                
067000       IF MID-KDCMD-INSERT                                                
067100         MOVE MFS-ALFA-FAELT-FEL   TO MOD-IDDC-UPD-ATTR                   
067200         MOVE NEJ                  TO SW-INPUT-RAETT                      
067300       END-IF                                                             
067400     ELSE                                                                 
067500       PERFORM GAA-CHECK-DC-FTG-USER                                      
067600                                                                          
067700       IF MID-IDLEVKND = ALL '+'                                          
067800          IF MID-KDCMD-INSERT                                             
067900             MOVE MFS-ALFA-FAELT-RAETT TO MOD-IDLEVKND-UPD-ATTR           
068000          END-IF                                                          
068100       ELSE                                                               
068200          IF MID-IDLEVKND NOT = SPACE                                     
068300             MOVE MFS-ALFA-FAELT-RAETT TO MOD-IDLEVKND-UPD-ATTR           
068400          ELSE                                                            
068500             MOVE MFS-ALFA-FAELT-FEL   TO MOD-IDLEVKND-UPD-ATTR           
068600             MOVE NEJ                  TO SW-INPUT-RAETT                  
068700          END-IF                                                          
068800          MOVE MFS-ROER-EJ-FAELT       TO MOD-IDLEVKND-UPD                
068900       END-IF                                                             
069000                                                                          
069100       IF MID-KDCMD-INSERT                                                
069200       OR MID-KDCMD-DELETE                                                
069300       OR MID-KDCMD-REPLACE                                               
069400          MOVE MFS-ALFA-FAELT-RAETT    TO MOD-KDCMD-UPD-ATTR              
069500          MOVE MFS-ROER-EJ-FAELT       TO MOD-KDCMD-UPD                   
069600       ELSE                                                               
069700          IF MID-KDCMD = ALL '+'                                          
069800             MOVE MFS-RENSA-FAELT      TO MOD-KDCMD-UPD                   
069900          ELSE                                                            
070000             MOVE MFS-ROER-EJ-FAELT    TO MOD-KDCMD-UPD                   
070100          END-IF                                                          
070200          MOVE MFS-ALFA-FAELT-FEL      TO MOD-KDCMD-UPD-ATTR              
070300          MOVE NEJ                     TO SW-INPUT-RAETT                  
070400       END-IF                                                             
070500     END-IF                                                               
070600                                                                          
070700     IF SW-INPUT-RAETT = NEJ                                              
070800        MOVE ERR-NOT-AUTH            TO MED-IDMFSFEL                      
070900        CALL WMEDKONV             USING MED-WMEDAREA                      
071000        MOVE MED-MFSFEL              TO MOD-TEMFSFEL                      
071100        PERFORM S02-ROER-EJ-FAELT                                         
071200     END-IF                                                               
071300     .                                                                    
071400     EJECT                                                                
071500 GAA-CHECK-DC-FTG-USER SECTION.                                           
071600     MOVE 'GAA-CHECK-DC-FTG-USER ' TO CURRENT-SECTION                     
071700                                                                          
071800     MOVE MID-IDDC                      TO W-IDDC-B6                      
071900     PERFORM IMS-GU-WDB601                                                
072000     IF SEGMENT-FINNS                                                     
072100        IF WDB601-DCS-NDC-CN                                              
072200        OR (WDB601-DCS-NDC-NA AND WDB601-DCS-USA)                         
072300           IF (MSGI-IDDC(1:1) = WDB601-DCS-IDDC(1:1) AND                  
072400               MSGI-IDFTG     = WDB601-DCS-IDFTG)                         
072500           OR MSGI-IDFTG  = +57                                           
072600              MOVE MFS-ALFA-FAELT-RAETT TO MOD-IDDC-UPD-ATTR              
072700           ELSE                                                           
072800              MOVE MFS-ALFA-FAELT-FEL   TO MOD-IDDC-UPD-ATTR              
072900              MOVE NEJ                  TO SW-INPUT-RAETT                 
073000           END-IF                                                         
073100        ELSE                                                              
073200           MOVE MFS-ALFA-FAELT-FEL      TO MOD-IDDC-UPD-ATTR              
073300           MOVE NEJ                     TO SW-INPUT-RAETT                 
073400        END-IF                                                            
073500     ELSE                                                                 
073600        MOVE MFS-ALFA-FAELT-FEL         TO MOD-IDDC-UPD-ATTR              
073700        MOVE NEJ                        TO SW-INPUT-RAETT                 
073800     END-IF                                                               
073900     .                                                                    
074000     EJECT                                                                
074100 GB-KOLLA-LEVERANTOR SECTION.                                             
074200     MOVE 'GC-KOLLA-LEVERANTOR ' TO CURRENT-SECTION.                      
074300                                                                          
074400     MOVE MID-IDLEVNR     TO W-IDLEVNR-F1                                 
074500     MOVE MID-IDDC        TO W-IDDC-F1                                    
074600                                                                          
074700     PERFORM IMS-GU-WDF116                                                
074800     IF SEGMENT-FINNS                                                     
074900        MOVE JA           TO SW-LEVERANTOR                                
075000     ELSE                                                                 
075100        MOVE NEJ          TO SW-LEVERANTOR                                
075200                             SW-INPUT-RAETT                               
075300     END-IF                                                               
075400     .                                                                    
075500     EJECT                                                                
075600                                                                          
075700 D-UPPDATERA SECTION.                                                     
075800     MOVE 'D-UPPDATERA     ' TO CURRENT-SECTION.                          
075900     SKIP2                                                                
076000     IF MID-KDCMD-INSERT                                                  
076100        MOVE MID-IDLEVNR     TO WDR2-2206-IDLEVNR                         
076200        MOVE MID-IDDC        TO WDR2-2206-IDDC                            
076300        MOVE ZERO            TO WDR2-2206-IDOVERFNR                       
076400                                WDR2-2206-IDOVERFNR-VV                    
076500                                WDR2-2206-TISEND-SEN                      
076600        MOVE 'D'             TO WDR2-2206-KDVECKOSL                       
076700        MOVE 'J'             TO WDR2-2206-FLLEVVB                         
076800        MOVE 'N'             TO WDR2-2206-FLLEVPLP                        
076900        MOVE MID-KDEDI       TO WDR2-2206-KDEDI                           
077000                                                                          
077100        IF MID-FLAVIS = YES                                               
077200          MOVE 'J'           TO WDR2-2206-FLAVIS                          
077300        ELSE                                                              
077400          MOVE MID-FLAVIS    TO WDR2-2206-FLAVIS                          
077500        END-IF                                                            
077600                                                                          
077700        MOVE MID-IDLEVKND    TO WDR2-2206-IDLEVKND                        
077800                                                                          
077900        IF MID-FLODETTE = YES                                             
078000          MOVE 'J'           TO WDR2-2206-FLODETTE                        
078100        ELSE                                                              
078200          MOVE MID-FLODETTE  TO WDR2-2206-FLODETTE                        
078300        END-IF                                                            
078400                                                                          
078500        PERFORM IMS-ISRT-WDGX2206                                         
078600        IF SEGMENT-FINNS-REDAN                                            
078700           MOVE FEL-3        TO MOD-TEMFSFEL                              
078800           MOVE NEJ          TO SW-INPUT-RAETT                            
078900           PERFORM S02-ROER-EJ-FAELT                                      
079000        ELSE                                                              
079100           MOVE MID-IDLEVNR  TO W-IDLEVNR                                 
079200           MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-INFO-IDLEVNR-ATTR            
079300           MOVE MID-IDDC     TO W-IDDC                                    
079400        END-IF                                                            
079500     ELSE                                                                 
079600        PERFORM IMS-GET-WDR201                                            
079700        MOVE MID-IDLEVNR             TO W-IDLEVNR                         
079800        MOVE MID-IDDC                TO W-IDDC                            
079900        PERFORM IMS-GET-WDGX2206-KVAL-UNIK                                
080000        IF SEGMENT-FINNS                                                  
080100           IF MID-KDCMD-REPLACE                                           
080200              IF MID-IDLEVKND NOT = ALL '+'                               
080300                 MOVE MID-IDLEVKND   TO WDR2-2206-IDLEVKND                
080400              END-IF                                                      
080500              IF MID-KDEDI    NOT = ALL '+'                               
080600                 MOVE MID-KDEDI      TO WDR2-2206-KDEDI                   
080700              END-IF                                                      
080800                                                                          
080900              IF MID-FLAVIS   NOT = ALL '+'                               
081000                IF MID-FLAVIS = YES                                       
081100                  MOVE 'J' TO WDR2-2206-FLAVIS                            
081200                ELSE                                                      
081300                  MOVE MID-FLAVIS    TO WDR2-2206-FLAVIS                  
081400                END-IF                                                    
081500              END-IF                                                      
081600                                                                          
081700              IF MID-FLODETTE NOT = ALL '+'                               
081800                IF MID-FLODETTE = YES                                     
081900                  MOVE 'J'            TO WDR2-2206-FLODETTE               
082000                ELSE                                                      
082100                  MOVE MID-FLODETTE   TO WDR2-2206-FLODETTE               
082200                END-IF                                                    
082300              END-IF                                                      
082400              PERFORM IMS-REPL-WDGX2206                                   
082500           ELSE                                                           
082600              IF MID-KDCMD-DELETE                                         
082700                 PERFORM IMS-DLET-WDGX2206                                
082800                 MOVE MID-INFO-IDLEVNR (1) TO WS-IDLEVNR-SPAR             
082900                 MOVE WS-IDLEVNR-SPAR      TO W-IDLEVNR                   
083000                 MOVE MID-INFO-IDDC (1)    TO WS-IDDC-KEY                 
083100              END-IF                                                      
083200           END-IF                                                         
083300        ELSE                                                              
083400           MOVE FEL-4                TO MOD-TEMFSFEL                      
083500           MOVE NEJ                  TO SW-INPUT-RAETT                    
083600        END-IF                                                            
083700     END-IF                                                               
083800                                                                          
083900     IF SW-INPUT-RAETT = JA                                               
084000        MOVE MFS-RENSA-FAELT            TO MOD-IDLEVNR-UPD                
084100                                           MOD-KDEDI-UPD                  
084200                                           MOD-FLAVIS-UPD                 
084300                                           MOD-IDDC-UPD                   
084400                                           MOD-IDLEVKND-UPD               
084500                                           MOD-FLODETTE-UPD               
084600                                           MOD-KDCMD-UPD                  
084700                                                                          
084800        MOVE MED-3                      TO MOD-TEMFSINF                   
084801                                                                          
084841        IF MID-IDDC NOT = W1-IDDC-MIN-X                                   
084842           IF (MID-IDDC = MSGI-IDDC-KEY)                                  
084843           OR (MSGI-IDDC-KEY(2:1) = '0' AND                               
084844               MID-IDDC(1:1) = MSGI-IDDC-KEY(1:1))                        
084845              CONTINUE                                                    
084846           ELSE                                                           
084847              MOVE MID-IDDC       TO W1-IDDC-MIN-X                        
084848                                     W1-IDDC-MAX-X                        
084849           END-IF                                                         
084850           MOVE ALL '+'                 TO MSGI-WMSGINIT                  
084860           MOVE '001'                   TO MSGI-KDCALL                    
084870           MOVE MSG-LTERM-NAME          TO MSGI-IDLTERM-USER              
084880           MOVE MSG-SIGNON-USERID       TO MSGI-IDUSER                    
084890           MOVE '2118'                  TO MSGI-IDTRANS                   
084891           MOVE MID-IDDC                TO MSGI-IDDC-KEY                  
084893                                                                          
084894           CALL W005INIT USING MSGI-WMSGINIT WDP7-PCB                     
084895           MOVE MSGI-SPAR-AREA          TO SAVE-AREA                      
084896                                                                          
084898           MOVE MSGI-IDDC-KEY           TO MOD-IDDC-UT                    
084901        END-IF                                                            
084902                                                                          
084903        MOVE MID-IDLEVNR          TO W-IDLEVNR                            
084904        MOVE MID-IDDC             TO W-IDDC                               
084910     ELSE                                                                 
085000        PERFORM S02-ROER-EJ-FAELT                                         
085100     END-IF                                                               
085200     .                                                                    
085300     EJECT                                                                
085400 F-READ-SHOW-INFO SECTION.                                                
085500     MOVE 'F-READ-SHOW-INFO' TO CURRENT-SECTION.                          
085600                                                                          
085700     PERFORM IMS-GET-WDR201                                               
087000                                                                          
087100     PERFORM IMS-GNP-WDGX2206-MIN-MAX                                     
087200                                                                          
087300     IF SEGMENT-FINNS                                                     
087400        MOVE WDR2-2206-IDLEVNR     TO SAVE-IDLEVNR-ENTER                  
087500                                      W-IDLEVNR                           
087600        MOVE WDR2-2206-IDDC        TO SAVE-IDDC-ENTER                     
087700        IF MSGI-IDDC-KEY NOT = SPACE                                      
087800           MOVE MSGI-IDDC-KEY      TO W1-IDDC-MIN-X                       
087900                                      W1-IDDC-MAX-X                       
088000           IF MSGI-IDDC-KEY(2:1) = '0'                                    
088100              MOVE '1'             TO W1-IDDC2-MIN                        
088200              MOVE '9'             TO W1-IDDC2-MAX                        
088300           END-IF                                                         
088400        END-IF                                                            
088500     END-IF                                                               
088600                                                                          
088700     SET MOD-INFO-IND TO 1                                                
088800                                                                          
088900     PERFORM UNTIL MOD-INFO-IND > MAX-IND                                 
089000        IF SEGMENT-FINNS                                                  
089100           MOVE WDR2-2206-IDLEVNR  TO MOD-INFO-IDLEVNR                    
089200                                      (MOD-INFO-IND)                      
089300           MOVE WDR2-2206-IDDC     TO MOD-INFO-IDDC                       
089400                                      (MOD-INFO-IND)                      
089500           MOVE WDR2-2206-KDEDI    TO MOD-INFO-KDEDI                      
089600                                      (MOD-INFO-IND)                      
089700                                                                          
089800           IF WDR2-2206-FLAVIS = JA                                       
089900             MOVE 'Y'                TO MOD-INFO-FLAVIS                   
090000                                        (MOD-INFO-IND)                    
090100           ELSE                                                           
090200             MOVE WDR2-2206-FLAVIS   TO MOD-INFO-FLAVIS                   
090300                                        (MOD-INFO-IND)                    
090400           END-IF                                                         
090500                                                                          
090600           MOVE WDR2-2206-IDLEVKND TO MOD-INFO-IDLEVKND                   
090700                                      (MOD-INFO-IND)                      
090800                                                                          
090900           IF WDR2-2206-FLODETTE = JA                                     
091000             MOVE 'Y'                TO MOD-INFO-FLODETTE                 
091100                                        (MOD-INFO-IND)                    
091200           ELSE                                                           
091300             MOVE WDR2-2206-FLODETTE TO MOD-INFO-FLODETTE                 
091400                                        (MOD-INFO-IND)                    
091500           END-IF                                                         
091600           PERFORM IMS-GNP-WDGX2206-MIN-MAX                               
092200        ELSE                                                              
092300           MOVE MFS-RENSA-FAELT TO MOD-INFO-IDLEVNR                       
092400                                     (MOD-INFO-IND)                       
092500                                   MOD-INFO-KDEDI                         
092600                                     (MOD-INFO-IND)                       
092700                                   MOD-INFO-FLAVIS                        
092800                                     (MOD-INFO-IND)                       
092900                                   MOD-INFO-IDDC                          
093000                                     (MOD-INFO-IND)                       
093100                                   MOD-INFO-IDLEVKND                      
093200                                     (MOD-INFO-IND)                       
093300                                   MOD-INFO-FLODETTE                      
093400                                     (MOD-INFO-IND)                       
093500        END-IF                                                            
093600                                                                          
093700        SET MOD-INFO-IND UP BY 1                                          
093800     END-PERFORM                                                          
093900                                                                          
094000     IF SEGMENT-FINNS                                                     
094100        MOVE WDR2-2206-IDLEVNR TO SAVE-IDLEVNR-NEXT                       
094200        MOVE WDR2-2206-IDDC    TO SAVE-IDDC-NEXT                          
094300        MOVE INF-MORE-INFO-EXISTS TO MED-IDMFSINF                         
094400        CALL WMEDKONV       USING MED-WMEDAREA                            
094500        MOVE MED-TEMFSINF      TO MOD-TEMFSINF                            
094600     ELSE                                                                 
094700        MOVE SAVE-IDLEVNR-ENTER TO SAVE-IDLEVNR-NEXT                      
094800        MOVE SAVE-IDDC-ENTER    TO SAVE-IDDC-NEXT                         
094900     END-IF                                                               
095000                                                                          
095100     MOVE '002'             TO MSGI-KDCALL                                
095200     MOVE '2118'            TO SAVE-IDTRANS                               
095300     MOVE SAVE-AREA         TO MSGI-SPAR-AREA                             
095400     CALL W005INIT       USING MSGI-WMSGINIT WDP7-PCB                     
095500     .                                                                    
095600     EJECT                                                                
095700 S02-ROER-EJ-FAELT SECTION.                                               
095800     SKIP3                                                                
095900                                                                          
096000     SET MOD-INFO-IND                TO 1                                 
096100                                                                          
096200     PERFORM UNTIL MOD-INFO-IND > MAX-IND                                 
096300        MOVE MFS-ROER-EJ-FAELT  TO MOD-INFO-IDLEVNR                       
096400                                   (MOD-INFO-IND)                         
096500                                   MOD-INFO-KDEDI                         
096600                                   (MOD-INFO-IND)                         
096700                                   MOD-INFO-FLAVIS                        
096800                                   (MOD-INFO-IND)                         
096900                                   MOD-INFO-IDDC                          
097000                                   (MOD-INFO-IND)                         
097100                                   MOD-INFO-IDLEVKND                      
097200                                   (MOD-INFO-IND)                         
097300                                   MOD-INFO-FLODETTE                      
097400                                   (MOD-INFO-IND)                         
097500        SET MOD-INFO-IND UP BY 1                                          
097600     END-PERFORM                                                          
097700                                                                          
097800     IF MID-IDLEVNR = ALL '+'                                             
097900        MOVE MFS-RENSA-FAELT   TO MOD-IDLEVNR-UPD                         
098000     ELSE                                                                 
098100        MOVE MFS-ROER-EJ-FAELT TO MOD-IDLEVNR-UPD                         
098200     END-IF                                                               
098300     IF MID-IDDC    = ALL '+'                                             
098400        MOVE MFS-RENSA-FAELT   TO MOD-IDDC-UPD                            
098500     ELSE                                                                 
098600        MOVE MFS-ROER-EJ-FAELT TO MOD-IDDC-UPD                            
098700     END-IF                                                               
098800     IF MID-KDEDI   = ALL '+'                                             
098900        MOVE MFS-RENSA-FAELT   TO MOD-KDEDI-UPD                           
099000     ELSE                                                                 
099100        MOVE MFS-ROER-EJ-FAELT TO MOD-KDEDI-UPD                           
099200     END-IF                                                               
099300     IF MID-FLAVIS  = ALL '+'                                             
099400        MOVE MFS-RENSA-FAELT   TO MOD-FLAVIS-UPD                          
099500     ELSE                                                                 
099600        MOVE MFS-ROER-EJ-FAELT TO MOD-FLAVIS-UPD                          
099700     END-IF                                                               
099800     IF MID-IDLEVKND = ALL '+'                                            
099900        MOVE MFS-RENSA-FAELT   TO MOD-IDLEVKND-UPD                        
100000     ELSE                                                                 
100100        MOVE MFS-ROER-EJ-FAELT TO MOD-IDLEVKND-UPD                        
100200     END-IF                                                               
100300     IF MID-FLODETTE = ALL '+'                                            
100400        MOVE MFS-RENSA-FAELT   TO MOD-FLODETTE-UPD                        
100500     ELSE                                                                 
100600        MOVE MFS-ROER-EJ-FAELT TO MOD-FLODETTE-UPD                        
100700     END-IF                                                               
100800     IF MID-KDCMD    = ALL '+'                                            
100900        MOVE MFS-RENSA-FAELT   TO MOD-KDCMD-UPD                           
101000     ELSE                                                                 
101100        MOVE MFS-ROER-EJ-FAELT TO MOD-KDCMD-UPD                           
101200     END-IF                                                               
101300     .                                                                    
101400     EJECT                                                                
101500 MFS-ERASE-FIELD-OUT SECTION.                                             
101600     MOVE 'MFS-ERASE-FIELD-OUT' TO CURRENT-SECTION                        
101700                                                                          
101800*    --- ALLA UTDATA-FÄLT                                                 
101900*    --- INCL. SCROLL KEYS                                                
102000                                                                          
102100*    --- OUTDATA-FIELD ON SCROLL KEYS                                     
102200     SET MOD-INFO-IND        TO 1                                         
102300     PERFORM UNTIL MOD-INFO-IND > MAX-IND                                 
102400       MOVE MFS-ERASE-FIELD  TO MOD-INFO-IDLEVNR    (MOD-INFO-IND)        
102500                                MOD-INFO-IDDC       (MOD-INFO-IND)        
102600                                MOD-INFO-IDLEVKND   (MOD-INFO-IND)        
102700                                MOD-INFO-KDEDI      (MOD-INFO-IND)        
102800                                MOD-INFO-FLAVIS     (MOD-INFO-IND)        
102900                                MOD-INFO-FLODETTE   (MOD-INFO-IND)        
103000        SET MOD-INFO-IND UP BY 1                                          
103100     END-PERFORM                                                          
103200     .                                                                    
103300     SKIP3                                                                
103400 MFS-ERASE-FIELD-IN SECTION.                                              
103500     MOVE 'MFS-ERASE-FIELD-IN'  TO CURRENT-SECTION                        
103600                                                                          
103700*    --- ALLA INDATA-FÄLT                                                 
103800     MOVE MFS-ERASE-FIELD    TO MOD-IDLEVNR-UPD                           
103900                                MOD-IDDC-UPD                              
104000                                MOD-IDLEVKND-UPD                          
104100                                MOD-KDEDI-UPD                             
104200                                MOD-FLAVIS-UPD                            
104300                                MOD-FLODETTE-UPD                          
104400                                MOD-KDCMD-UPD                             
104500                                                                          
104600     SET MOD-INFO-IND        TO 1                                         
104700     PERFORM UNTIL MOD-INFO-IND > MAX-IND                                 
104800       MOVE MFS-ERASE-FIELD  TO MOD-INFO-IDLEVNR    (MOD-INFO-IND)        
104900                                MOD-INFO-IDDC       (MOD-INFO-IND)        
105000                                MOD-INFO-IDLEVKND   (MOD-INFO-IND)        
105100                                MOD-INFO-KDEDI      (MOD-INFO-IND)        
105200                                MOD-INFO-FLAVIS     (MOD-INFO-IND)        
105300                                MOD-INFO-FLODETTE   (MOD-INFO-IND)        
105400        SET MOD-INFO-IND UP BY 1                                          
105500     END-PERFORM                                                          
105600     .                                                                    
105700     EJECT                                                                
105800* IMS SEKTIONER                                                           
105900     SKIP3                                                                
106000 IMS-GET-MSG SECTION.                                                     
106100     SKIP2                                                                
106200     MOVE '  QC' TO GODK-STATUSKODER                                      
106300     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
106400     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
106500     PERFORM IMS-STATUS-KONTROLL                                          
106600     SKIP3                                                                
106700     .                                                                    
106800 IMS-INSERT-MSG SECTION.                                                  
106900     SKIP2                                                                
107000     IF ENGLISH-TEXT                                                      
107100        MOVE 'N' TO MFS-KDHUVOMR                                          
107200     END-IF                                                               
107300     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
107400     MOVE SPACE TO GODK-STATUSKODER                                       
107500     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
107600     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
107700     PERFORM IMS-STATUS-KONTROLL                                          
107800     .                                                                    
107900     EJECT                                                                
108000 IMS-GET-WDR201 SECTION.                                                  
108100     MOVE 'IMS-GET-WDR201  ' TO CURRENT-IMS-SECTION                       
108200     SKIP2                                                                
108300     MOVE SPACE              TO ALL-SSA                                   
108400     STRING 'WDR201  (WDGXKEY  =' W-WDGXKEY-ROT ')'                       
108500             DELIMITED BY SIZE INTO SSA1                                  
108600     MOVE '  ' TO GODK-STATUSKODER                                        
108700     CALL CBLTDLI USING GU WDR2-PCB DLI-IO-AREA SSA1                      
108800     MOVE WDR2-STATUS-CODE TO STATUS-WS                                   
108900     PERFORM IMS-STATUS-KONTROLL                                          
109000     SKIP3                                                                
109100     .                                                                    
109200                                                                          
111800 IMS-GNP-WDGX2206-MIN-MAX SECTION.                                        
111900     MOVE 'IMS-GNP-WDGX2206-MIN-MAX'  TO CURRENT-IMS-SECTION              
112000                                                                          
112100     STRING 'WDGX2206(KY2206  =>' W-WDGX2206-X                            
112200                 OCH 'IDDC    =>' W1-IDDC-MIN-X                           
112300                 OCH 'IDDC    <=' W1-IDDC-MAX-X ')'                       
112400             DELIMITED BY SIZE INTO SSA1                                  
112500     MOVE '  GE' TO GODK-STATUSKODER                                      
112600     CALL CBLTDLI USING GNP WDR2-PCB DLI-IO-AREA SSA1                     
112700     MOVE WDR2-STATUS-CODE TO STATUS-WS                                   
112800     PERFORM IMS-STATUS-KONTROLL                                          
112900     .                                                                    
113000     EJECT                                                                
113100                                                                          
113200 IMS-GET-WDGX2206-KVAL-UNIK SECTION.                                      
113300     MOVE 'IMS-GET-WDGX2206-KVAL-UNIK' TO CURRENT-IMS-SECTION             
113400     SKIP2                                                                
113500     MOVE SPACE              TO ALL-SSA                                   
113600     STRING 'WDGX2206(KY2206   =' W-WDGX2206-X ')'                        
113700            DELIMITED BY SIZE INTO SSA1                                   
113800     MOVE '  GE' TO GODK-STATUSKODER                                      
113900     CALL CBLTDLI USING GHNP WDR2-PCB DLI-IO-AREA SSA1                    
114000     MOVE WDR2-STATUS-CODE TO STATUS-WS                                   
114100     PERFORM IMS-STATUS-KONTROLL                                          
114200     SKIP3                                                                
114300     .                                                                    
114400     EJECT                                                                
114500 IMS-ISRT-WDGX2206 SECTION.                                               
114600     MOVE 'IMS-ISRT-WDGX2206' TO CURRENT-IMS-SECTION                      
114700     SKIP2                                                                
114800     MOVE SPACE              TO ALL-SSA                                   
114900     STRING 'WDR201  *P(WDGXKEY  =' W-WDGXKEY-ROT ')'                     
115000             DELIMITED BY SIZE INTO SSA1                                  
115100     MOVE 'WDGX2206 '  TO SSA2                                            
115200     MOVE '  II' TO GODK-STATUSKODER                                      
115300     CALL CBLTDLI USING ISRT WDR2-PCB DLI-IO-AREA SSA1 SSA2               
115400     MOVE WDR2-STATUS-CODE TO STATUS-WS                                   
115500     PERFORM IMS-STATUS-KONTROLL                                          
115600     SKIP3                                                                
115700     .                                                                    
115800 IMS-REPL-WDGX2206 SECTION.                                               
115900     MOVE 'IMS-REPL-WDGX2206' TO CURRENT-IMS-SECTION                      
116000     SKIP2                                                                
116100     MOVE '  '   TO GODK-STATUSKODER                                      
116200     CALL CBLTDLI USING REPL WDR2-PCB DLI-IO-AREA                         
116300     MOVE WDR2-STATUS-CODE TO STATUS-WS                                   
116400     PERFORM IMS-STATUS-KONTROLL                                          
116500     SKIP3                                                                
116600     .                                                                    
116700 IMS-DLET-WDGX2206 SECTION.                                               
116800     MOVE 'IMS-DLET-WDGX2206' TO CURRENT-IMS-SECTION                      
116900     SKIP2                                                                
117000     MOVE '  '   TO GODK-STATUSKODER                                      
117100     CALL CBLTDLI USING DLET WDR2-PCB DLI-IO-AREA                         
117200     MOVE WDR2-STATUS-CODE TO STATUS-WS                                   
117300     PERFORM IMS-STATUS-KONTROLL                                          
117400     EJECT                                                                
117500     SKIP3                                                                
117600     .                                                                    
117610                                                                          
117700 IMS-GU-WDB601 SECTION.                                                   
117800     MOVE 'IMS-GU-WDB601  ' TO CURRENT-IMS-SECTION                        
117900     SKIP2                                                                
118000     MOVE SPACE              TO ALL-SSA                                   
118100     STRING 'WDB601  (IDDC     =' W-IDDC-X ')'                            
118200          DELIMITED BY SIZE INTO SSA1                                     
118300     MOVE '  GE'              TO GODK-STATUSKODER                         
118400     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-WDB601 SSA1                    
118500     MOVE WDB6-STATUS-CODE TO STATUS-WS                                   
118600     PERFORM IMS-STATUS-KONTROLL                                          
118700     EJECT                                                                
118800     SKIP3                                                                
118900     .                                                                    
119000                                                                          
119100 IMS-GU-WDF116 SECTION.                                                   
119200     MOVE 'IMS-GU-WDF116   '  TO CURRENT-IMS-SECTION                      
119300                                                                          
119400     MOVE SPACE               TO ALL-SSA                                  
119500     STRING 'WDF101  (IDLEVNR  =' W-IDLEVNR-F1-X ')'                      
119600          DELIMITED BY SIZE INTO SSA1                                     
119700     STRING 'WDF116  (IDDC     =' W-IDDC-F1-X ')'                         
119800          DELIMITED BY SIZE INTO SSA2                                     
119900     MOVE '  GE'              TO GODK-STATUSKODER                         
120000     CALL CBLTDLI USING GU WDF1-PCB DLI-IO-WDF116 SSA1 SSA2               
120100     MOVE WDF1-STATUS-CODE    TO STATUS-WS                                
120200     PERFORM IMS-STATUS-KONTROLL                                          
120300     .                                                                    
120400                                                                          
120500 IMS-STATUS-KONTROLL SECTION.                                             
120600     SET STATUS-IX TO 1                                                   
120700     SEARCH GODK-STATUS AT END CALL FELLOG                                
120800       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                  
120900     END-SEARCH                                                           
121000     CONTINUE                                                             
121100     .                                                                    
