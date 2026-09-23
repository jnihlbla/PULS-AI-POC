000100 ID DIVISION.                                                             
000200                                                                          
000300 PROGRAM-ID.     W2034600.                                                
000400 AUTHOR.         STEFAN KIHLBERG.                                         
000500 DATE-WRITTEN.   95/08/31.                                                
000600 DATE-COMPILED.                                                           
000700                                                                          
000800*    FUNKTION:                                                            
000900*        VISAR/UPPDATERAR REFILLINGPUNKT                                  
001000*                                                                         
001100*        PROGRAMMET UPPDATERAR WDGX25 (WDR2)                              
001200*                                                                         
001300*    INDATA.                                                              
001400*        TRANSAKTION: W2T346                                              
001500*        MID:         W2I34601                                            
001600*                                                                         
001700*    UTDATA.                                                              
001800*        MOD:         W2O34601                                            
001900                                                                          
002000     SKIP3                                                                
002100 ENVIRONMENT DIVISION.                                                    
002200     EJECT                                                                
002300 DATA DIVISION.                                                           
002400 WORKING-STORAGE SECTION.                                                 
002500                                                                          
002600*    -- CHECKED BY WY2000                                                 
002700 77  IDPGM                       PIC X(08)   VALUE 'W2034600'.            
002800                                                                          
002900*    --- ARBETSFÄLT FÖR FELMEDDELANDEN VID CALL ABEND/FELLOG              
003000 77  FELTEXT                     PIC X(80) VALUE SPACE.                   
003100                                                                          
003200 77  JA                          PIC X       VALUE 'J'.                   
003300 77  NEJ                         PIC X       VALUE 'N'.                   
003400 77  MAX-MOD-LAENGD              PIC S9(4) VALUE ZERO COMP SYNC.          
003500                                                                          
003600*    --- ARBETSFÄLT FÖR AKTUELLA NYCKELVÄRDEN FRÅN SKÄRMEN                
003700                                                                          
003800 77  INDATA-SW                   PIC X       VALUE 'J'.                   
003900     88  INDATA-OK                           VALUE 'J'.                   
004000     88  INDATA-FEL                          VALUE 'N'.                   
004100                                                                          
004200 77  COPY-DATA-SW                PIC X       VALUE 'J'.                   
004300     88  COPY-DATA-OK                        VALUE 'J'.                   
004400     88  COPY-DATA-FEL                       VALUE 'N'.                   
004500                                                                          
004600 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
004700     88  NYCKLAR-OK                          VALUE 'J'.                   
004800     88  NYCKLAR-FEL                         VALUE 'N'.                   
004900                                                                          
005000 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
005100     88  EGEN-MID                            VALUE '2346'.                
005200     88  GODK-MID                            VALUE '2346' '2347'.         
005300     88  HELP-MID                            VALUE '0551'.                
005400     EJECT                                                                
005500*    --- ÖVRIGA ARBETSFÄLT                                                
005600 01  FILLER                      PIC X(16) VALUE 'ARBETSFALT'.            
005700 01  ARBETSFALT.                                                          
005800     03  WS-IDREFTAB             PIC X(1)    VALUE SPACE.                 
005900                                                                          
006100                                                                          
006200     03  WS-COPY-IDDC            PIC X(2)    VALUE SPACE.                 
006300     03  WS-COPY-IDREFTAB        PIC X(1)    VALUE ZERO.                  
006400                                                                          
006500     03  WS-RAD                  PIC 9(1)    VALUE ZERO.                  
006600     03  WS-FL-SISTA-TABELL      PIC X(1)    VALUE 'N'.                   
006700                                                                          
006800     03  WS-KVPB-REF-INRAD.                                               
006900         05 WS-KVPB-REF-IN OCCURS 8.                                      
007000           07 WS-KVPB-REF          PIC S9(6)V9(1).                        
007100                                                                          
007200     03  JMF-KVPB-REF-INRAD.                                              
007300         05 JMF-KVPB-REF-IN OCCURS 8.                                     
007400           07 JMF-KVPB-REF          PIC S9(6)V9(1).                       
007500                                                                          
007600     03  WS-INRAD.                                                        
007700         05 WS-PRARTBES            PIC S9(7)V9(2).                        
007800         05 WS-PUNKT-IN OCCURS 10.                                        
007900           07 WS-KVREFLIM           PIC 9(7).                             
008000           07 WS-KDREFPKT-LIM       PIC X(1).                             
008100                                                                          
008200     03  WS-ANTAL-TABELLER       PIC 9(9)    VALUE ZERO.                  
008300                                                                          
008400 77  SKAPA-NY-TABELL-SW          PIC X       VALUE 'N'.                   
008500     88  SKAPA-NY-TABELL                     VALUE 'J'.                   
008600                                                                          
008700 01  FILLER                      PIC X(16)   VALUE 'INDEX'.               
008800 01  INDX.                                                                
008900     03  RAD-IX                  PIC 9(2)    VALUE ZERO.                  
009000     03  KOL-IX                  PIC 9(1)    VALUE ZERO.                  
009100     03  IX1                     PIC 9(2)    VALUE ZERO.                  
009200     03  IX2                     PIC 9(2)    VALUE ZERO.                  
009300*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
009400       EJECT                                                              
009500 01  GENERELLA-SUBPROGRAM.                                                
009600     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
009700     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
009800     03  WDECEDIT                PIC X(8)    VALUE 'WDECEDIT'.            
009900     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
010000     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
010100     EJECT                                                                
010200*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
010300*01 -COPY WMEDAREA                                                        
010400     SKIP3                                                                
010500 01  MESSAGE-CODES.                                                       
010600     03  ERR-CORR-HILITE-FLDS    PIC X(3)    VALUE '001'.                 
010700     03  INF-PRESS-PF11          PIC X(3)    VALUE '003'.                 
010800     03  ERR-PF11-AND-NO-DATA    PIC X(3)    VALUE '011'.                 
010900     03  INF-UPDATE-DONE         PIC X(3)    VALUE '101'.                 
011000     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
011700                                                                          
011800 01  FEL-MEDDELANDEN.                                                     
011900     03  FEL-11                  PIC X(40)                                
012000         VALUE 'FEL I NYA TABELLNAMNET                 '.                 
012100     03  FEL-12                  PIC X(40)                                
012200         VALUE 'NYA TABELLEN FINNS REDAN               '.                 
012300     03  FEL-13                  PIC X(40)                                
012400         VALUE 'GAMLA TABELLEN FINNS INTE              '.                 
012500     03  FEL-14                  PIC X(40)                                
012600         VALUE 'SÖKT TABELL FINNS EJ                   '.                 
012700     03  FEL-15                  PIC X(40)                                
012800         VALUE 'INDATA VID BORTTAG                     '.                 
012900     03  FEL-16                  PIC X(40)                                
013000         VALUE 'GRUNDTABELL FÅR EJ TAS BORT            '.                 
013100                                                                          
013200 01  OVR-MEDDELANDEN.                                                     
013300     03 MED-EX1.                                                          
013400        05 UT-ANTAL-TABELLER     PIC Z(2)9.                               
013500        05 UT-TABELLTEXT         PIC X(33).                               
013600        05 UT-NEXT-PAGE          PIC X(22) VALUE SPACE.                   
013700                                                                          
013800     03 TE-PF8-FLER-TABELLER  PIC X(33)                                   
013900                 VALUE ' REFILLTABELLER TILL DETTA DC.   '.               
014000     03 TE-PF8-EN-TABELL      PIC X(33)                                   
014100                 VALUE ' REFILLTABELL TILL DETTA DC.   '.                 
014200     03 TE-PF8-MORE              PIC X(22)                                
014300                 VALUE ' TRYCK PF8 FÖR FLERA. ' .                         
014400     03 TE-LAST-PAGE             PIC X(22)                                
014500                 VALUE ' SISTA TABELL.        ' .                         
014600                                                                          
014700     03 MED-1                    PIC X(40)                                
014800         VALUE 'TABELL BORTTAGEN                       '.                 
014900                                                                          
015000     EJECT                                                                
015100*    --- PARAMETRAR TILL SUBPROGRAM W005INIT                              
015200*                                                                         
015300 01  FILLER                      PIC X(16)   VALUE 'WMSGINIT'.            
015400     SKIP3                                                                
015500*01 -COPY WMSGINIT                                                        
015600     SKIP3                                                                
015700*    --- PARAMETRAR TILL SUBPROGRAM WDECEDIT                              
015800*                                                                         
015900 01  FILLER                      PIC X(16)   VALUE 'WDECEDIT'.            
016000     SKIP3                                                                
016100*01 -COPY WDECAREA                                                        
016200     SKIP3                                                                
016300*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
016400*                                                                         
016500 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
016600     SKIP3                                                                
016700*01  MID -COPY W2I34601                                                   
016800     EJECT                                                                
016900 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
017000     SKIP3                                                                
017100*01  -COPY WMSGAREA                                                       
017200     EJECT                                                                
017300     03  MOD REDEFINES MSG-AREA.                                          
017400*      05  -COPY W2O34601                                                 
017500     EJECT                                                                
017600 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
017700     SKIP3                                                                
017800*01  -COPY WMFSAREA                                                       
017900     EJECT                                                                
018000*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
018100*                                                                         
018200     EJECT                                                                
018300 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
018400     SKIP3                                                                
018500*01  NYCKLAR-TILL-BLAEDDRING.                                             
018600     SKIP3                                                                
018700 01  NYCKLAR-TILL-DLI.                                                    
018800     03  W-WDGXKEY-X.                                                     
018900          05 W-IDHTYP            PIC X(4)    VALUE '2501'.                
019000          05 W-IDDC              PIC X(2)    VALUE SPACE.                 
019100          05 W-LOWVALUE          PIC X(24)   VALUE LOW-VALUE.             
019200                                                                          
019300     03  W-IDREFTAB-X.                                                    
019400         05  W-IDREFTAB          PIC X(1)    VALUE SPACE.                 
019500                                                                          
019600     03  W-OLD-WDGXKEY-X.                                                 
019700          05 W-OLD-IDHTYP        PIC X(4)    VALUE '2501'.                
019800          05 W-OLD-IDDC          PIC X(2)    VALUE SPACE.                 
019900          05 W-OLD-LOWVALUE      PIC X(24)   VALUE LOW-VALUE.             
020000                                                                          
020100     03  W-OLD-IDREFTAB-X.                                                
020200         05  W-OLD-IDREFTAB      PIC X(1)    VALUE SPACE.                 
020300                                                                          
020400     03  W-IDDC-B6-X.                                                     
020500         05  W-IDDC-B6           PIC X(2)    VALUE SPACE.                 
020501                                                                          
020600     SKIP2                                                                
020700*    --- STATUS-KOD FRÅN IMS                                              
020800 01  STATUS-WS                   PIC XX.                                  
020900     88  SEGMENT-FINNS                       VALUE '  '.                  
021000     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
021100     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
021110     88  SEGMENT-SLUT                        VALUE 'GB'.                  
021200     SKIP2                                                                
021300 01  GODK-STATUSKODER.                                                    
021400     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
021500     SKIP3                                                                
021600 01  SSA1                        PIC X(64).                               
021700 01  SSA2                        PIC X(64).                               
021800     EJECT                                                                
021900*    --- IMS FUNKTIONSKODER                                               
022000*01  -COPY W0003                                                          
022100     EJECT                                                                
022200*    ---  DLI INPUT-OUTPUT AREA                                           
022300 01  FILLER                   PIC X(16)   VALUE 'DLI-IO-AREA-1'.          
022400     SKIP3                                                                
022500 01  DLI-IO-AREA-1.                                                       
022600     03  IO-AREA-1            PIC X(1500)  VALUE SPACE.                   
022700     SKIP3                                                                
022800     03  WDGX2501 REDEFINES IO-AREA-1.                                    
022900*        05  -COPY WDGX2501                                               
023000     SKIP3                                                                
023100     03  WDGX2502 REDEFINES IO-AREA-1.                                    
023200*        05  -COPY WDGX2502                                               
023300                                                                          
023400 01  FILLER                   PIC X(16)   VALUE 'DLI-IO-AREA-2'.          
023500     SKIP3                                                                
023600 01  DLI-IO-AREA-2.                                                       
023700     03  IO-AREA-2            PIC X(1500)  VALUE SPACE.                   
023800     SKIP3                                                                
023900     03  WDGX2501 REDEFINES IO-AREA-2.                                    
024000*        05  -COPY WDGX2501 -PRE GAMMAL-                                  
024100     SKIP3                                                                
024200     03  WDGX2502 REDEFINES IO-AREA-2.                                    
024300*        05  -COPY WDGX2502 -PRE GAMMAL-                                  
024400     EJECT                                                                
024500                                                                          
024600 01  FILLER               PIC X(16)   VALUE 'WDB601 AREA'.                
024700 01   DLI-IO-AREA-B601.                                                   
024800*     03  -COPY WDB601                                                    
024810 01  FILLER               PIC X(16)   VALUE 'WDB616 AREA'.                
024820 01   DLI-IO-AREA-B616.                                                   
024830*     03  -COPY WDB616                                                    
024900     EJECT                                                                
025100 LINKAGE SECTION.                                                         
025200                                                                          
025300*01  -COPY W0009   -PRE MSG-                                              
025310     EJECT                                                                
025400*01  -COPY W0008   -PRE WDR2-                                             
025500     05  FILLER                  PIC X.                                   
025600     EJECT                                                                
025700*01  -COPY W0008      -PRE WDB6-                                          
025800     05  FILLER                  PIC X.                                   
025900     EJECT                                                                
026000 PROCEDURE DIVISION  USING MSG-PCB WDR2-PCB WDB6-PCB.                     
026100     ENTRY 'DLITCBL' USING MSG-PCB WDR2-PCB WDB6-PCB.                     
026200                                                                          
026300     PERFORM IMS-GET-MSG                                                  
026400     IF SEGMENT-FINNS                                                     
026500        PERFORM A-INIT                                                    
026600        PERFORM B-KOLLA-NYCKLAR                                           
026700        IF NYCKLAR-OK                                                     
026800           IF MID-KDCMD-INSERT                                            
026900              PERFORM I-SKAPA-NY-TABELL                                   
027000           END-IF                                                         
027100           IF COPY-DATA-OK                                                
027200              IF MFS-UPDATE                                               
027300                 PERFORM G-KOLLA-INPUT                                    
027400                 IF INDATA-OK                                             
027500                    PERFORM H-UPPDATERA                                   
027600                 END-IF                                                   
027700              ELSE                                                        
027800                 IF MFS-FIRST                                             
027900                    PERFORM C-FIRST-LATEST-TABELL                         
028000                 ELSE                                                     
028100                    IF MFS-NEXT                                           
028200                       PERFORM D-NEXT-TABELL                              
028300                    ELSE                                                  
028400                       PERFORM E-SAMMA-TABELL                             
028500                    END-IF                                                
028600                 END-IF                                                   
028700              END-IF                                                      
028800              PERFORM F-LAES-VISA-INFO                                    
028900           ELSE                                                           
029000              PERFORM J-VISA-FEL-VID-COPY                                 
029100           END-IF                                                         
029200        END-IF                                                            
029300        PERFORM IMS-INSERT-MSG                                            
029400     END-IF                                                               
029500                                                                          
029600     MOVE ZERO TO RETURN-CODE                                             
029700     GOBACK                                                               
029800     .                                                                    
029900     EJECT                                                                
030000                                                                          
030100                                                                          
030200 A-INIT SECTION.                                                          
030300                                                                          
030400     IF MSG-DUBBLA-TRANSKODER                                             
030500       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W2I34601                 
030600       MOVE MSG-IDTRANS-2  TO MFS-IDTRANS                                 
030700       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
030800     ELSE                                                                 
030900       MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W2I34601                  
031000       MOVE MSG-IDTRANS-1  TO MFS-IDTRANS                                 
031100       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
031200     END-IF                                                               
031300                                                                          
031400     MOVE MSG-KDTRTYP TO MFS-KDTRTYP                                      
031500     MOVE MSG-IDPFK   TO MFS-IDPFK                                        
031600     MOVE MFS-IDTRANS TO W-IDTRANS                                        
031700                                                                          
031800     MOVE LOW-VALUE TO MSG-AREA                                           
031900     MOVE 'W2O34601' TO MFS-IDMOD                                         
032000     MOVE '2346' TO MOD-IDTRANS                                           
032100     MOVE MFS-RENSA-FAELT TO MOD-TEMFSFEL MOD-TEMFSINF                    
032200                                                                          
032300     COMPUTE MSG-KVLL = LENGTH OF MOD-W2O34601 + 4                        
032400                                                                          
032500     IF EGEN-MID OR HELP-MID                                              
032600       CONTINUE                                                           
032700     ELSE                                                                 
032800       MOVE SPACE TO MFS-KDTRTYP                                          
032900       MOVE '7' TO MFS-IDPFK                                              
033000       MOVE SPACE TO MID-IDREFTAB-FIRST                                   
033100                     MID-IDREFTAB-LAST                                    
033200     END-IF                                                               
033300     .                                                                    
033400     EJECT                                                                
033500 B-KOLLA-NYCKLAR SECTION.                                                 
033600                                                                          
034600     MOVE JA TO NYCKLAR-SW                                                
034700                                                                          
034800                                                                          
034900*    -- KONTROLL AV IDDC                                                  
035000     MOVE MFS-RENSA-FAELT TO MOD-IDDC-IN                                  
035100                                                                          
035200     IF MID-IDDC-IN     = ALL '+'                                         
035300       MOVE MID-IDDC-UT TO W-IDDC-B6                                      
035400     ELSE                                                                 
035500       MOVE MID-IDDC-IN TO W-IDDC-B6                                      
035600       MOVE '7'         TO MFS-IDPFK                                      
035700       MOVE SPACE       TO MFS-KDTRTYP                                    
035800       MOVE SPACE       TO MID-IDREFTAB-FIRST                             
035900                           MID-IDREFTAB-LAST                              
036000     END-IF                                                               
036100                                                                          
036200     PERFORM IMS-GU-WDB601                                                
036300     IF DCS-KDDC = SPACE OR DCS-DDC                                       
036400       MOVE NEJ                TO NYCKLAR-SW                              
036500     ELSE                                                                 
036510       CONTINUE                                                           
036700     END-IF                                                               
037400                                                                          
037500*    -- KONTROLL AV IDREFTAB                                              
037600     MOVE MFS-RENSA-FAELT TO MOD-IDREFTAB-IN                              
037700                                                                          
037800     IF MID-IDREFTAB-IN  = ALL '+'                                        
037900       MOVE MID-IDREFTAB-UT TO WS-IDREFTAB                                
038000       INSPECT WS-IDREFTAB REPLACING LEADING SPACE BY ZERO                
038100     ELSE                                                                 
038200       MOVE MID-IDREFTAB-IN TO WS-IDREFTAB                                
038300       MOVE '7'             TO MFS-IDPFK                                  
038400       MOVE SPACE           TO MFS-KDTRTYP                                
038500       MOVE SPACE           TO MID-IDREFTAB-FIRST                         
038600                               MID-IDREFTAB-LAST                          
038700     END-IF                                                               
038710                                                                          
038800     IF WS-IDREFTAB NUMERIC                                               
038810        CONTINUE                                                          
038820     ELSE                                                                 
038830        MOVE NEJ            TO NYCKLAR-SW                                 
038840     END-IF                                                               
038850                                                                          
038860                                                                          
039000*FÖR ATT KUNNA UPPDATERA DIREKT I NYSKAPAD TABELL                         
039100                                                                          
039200     IF MID-KDCMD-INSERT               AND                                
039300        (MID-IDDC-IN NOT = ALL '+'     OR                                 
039400        MID-IDREFTAB-IN NOT = ALL '+') AND                                
039500        MFS-IDPFK = '7'                AND                                
039600        MFS-KDTRTYP = ' '                                                 
039700        MOVE SPACE         TO MFS-IDPFK                                   
039800        MOVE 'U'           TO MFS-KDTRTYP                                 
039900     END-IF                                                               
040000                                                                          
040100     IF GODK-MID OR NYCKLAR-OK                                            
040200       MOVE DCS-IDDC           TO MOD-IDDC-UT                             
040300       MOVE WS-IDREFTAB        TO MOD-IDREFTAB-UT                         
040600     ELSE                                                                 
040700       MOVE MFS-RENSA-FAELT TO MOD-IDDC-UT                                
040800       MOVE MFS-RENSA-FAELT TO MOD-IDREFTAB-UT                            
041000     END-IF                                                               
041100                                                                          
041200                                                                          
041300     IF NYCKLAR-FEL                                                       
041400       MOVE ERR-WRONG-KEY TO MED-IDMFSFEL                                 
041500       CALL WMEDKONV USING MED-WMEDAREA                                   
041600       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
041700       PERFORM MFS-RENSA-FAELT-IN                                         
041800       PERFORM MFS-RENSA-FAELT-UT                                         
041900     END-IF                                                               
042000     .                                                                    
042100     EJECT                                                                
045000                                                                          
045100 C-FIRST-LATEST-TABELL SECTION.                                           
045200                                                                          
045300                                                                          
045400     MOVE DCS-IDDC     TO W-IDDC                                          
045500     MOVE WS-IDREFTAB  TO W-IDREFTAB                                      
045600                                                                          
045700     IF MID-IDREFTAB-FIRST = SPACE AND                                    
045800        MID-IDREFTAB-LAST = SPACE                                         
045900*       PF7 (ENTER) NYA NYCKLAR                                           
046000        MOVE WS-IDREFTAB TO W-IDREFTAB                                    
046100     ELSE                                                                 
046200        IF MID-IDREFTAB-FIRST > SPACE AND                                 
046300           MID-IDREFTAB-LAST > SPACE                                      
046400*          PF7 BLÄDDRA BAKÅT FÖRSTA GÅNG                                  
046500              MOVE MID-IDREFTAB-FIRST TO W-IDREFTAB                       
046600        ELSE                                                              
046700           IF MID-IDREFTAB-FIRST = SPACE AND                              
046800              MID-IDREFTAB-LAST > SPACE                                   
046900*             PF7 BLÄDDRA BAKÅT GÅNG 2                                    
046910                                                                          
047000              MOVE MID-IDREFTAB-FIRST TO W-IDREFTAB                       
047100           ELSE                                                           
047200              MOVE WS-IDREFTAB    TO W-IDREFTAB                           
047300           END-IF                                                         
047400        END-IF                                                            
047500     END-IF                                                               
047600     PERFORM MFS-RENSA-FAELT-IN                                           
047700     .                                                                    
047800     EJECT                                                                
047900                                                                          
048000                                                                          
048100 D-NEXT-TABELL SECTION.                                                   
048200                                                                          
048400     MOVE DCS-IDDC             TO W-IDDC                                  
048500     MOVE MID-IDREFTAB-LAST TO W-IDREFTAB                                 
048501     PERFORM MFS-RENSA-FAELT-IN                                           
048700     .                                                                    
048800     EJECT                                                                
049100                                                                          
049200 E-SAMMA-TABELL SECTION.                                                  
049300                                                                          
049400     MOVE DCS-IDDC     TO W-IDDC                                          
049500     MOVE WS-IDREFTAB  TO W-IDREFTAB                                      
049600                                                                          
049700     IF MID-INPUT = ALL '+'                                               
049800       PERFORM MFS-RENSA-FAELT-IN                                         
049900     ELSE                                                                 
050000       MOVE INF-PRESS-PF11 TO MED-IDMFSFEL                                
050100       CALL WMEDKONV USING MED-WMEDAREA                                   
050200       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
050300       PERFORM MFS-LAES-IN-IGEN                                           
050400       PERFORM S34-MID-INDATA-TILL-MOD                                    
050500     END-IF                                                               
050600     .                                                                    
050700     EJECT                                                                
050800                                                                          
050900                                                                          
051000 F-LAES-VISA-INFO SECTION.                                                
051100                                                                          
051200     IF MFS-NEXT                                                          
051300        PERFORM FA-NEXT                                                   
051400     ELSE                                                                 
051500        IF MFS-ENTER                                                      
051600           PERFORM FB-ENTER-UPDATE                                        
051700        ELSE                                                              
051800           PERFORM FC-FIRST                                               
051900        END-IF                                                            
052000     END-IF                                                               
052100     .                                                                    
052200     EJECT                                                                
052300                                                                          
052400                                                                          
052500 FA-NEXT SECTION.                                                         
052600                                                                          
052700     PERFORM IMS-GU-2501                                                  
052800     PERFORM IMS-GNP-2502-NEXT                                            
052900     IF SEGMENT-FINNS                                                     
053010        IF 2502-IDREFTAB NUMERIC                                          
053020          MOVE MID-IDREFTAB-LAST TO MOD-IDREFTAB-FIRST                    
053100          MOVE 2502-IDREFTAB   TO MOD-IDREFTAB-LAST                       
053200                                  MOD-IDREFTAB-UT                         
053300                                  W-IDREFTAB                              
053400          PERFORM FX-VISA-INFO                                            
053500          PERFORM IMS-GNP-2502-NEXT                                       
053600          IF SEGMENT-FINNS AND 2502-IDREFTAB NUMERIC                      
053700             CONTINUE                                                     
053800          ELSE                                                            
053900             MOVE JA TO WS-FL-SISTA-TABELL                                
054000          END-IF                                                          
054100          PERFORM FY-HUR-MANGA-TABELLER                                   
054110        END-IF                                                            
054200     ELSE                                                                 
054300        PERFORM IMS-GU-2501                                               
054301        PERFORM IMS-GNP-2502-OKVAL                                        
054310        PERFORM UNTIL SEGMENT-SAKNAS OR SEGMENT-SLUT                      
054401          IF SEGMENT-FINNS AND 2502-IDREFTAB NUMERIC                      
054500            MOVE 2502-IDREFTAB TO MOD-IDREFTAB-FIRST                      
054600                                  MOD-IDREFTAB-LAST                       
054700                                  MOD-IDREFTAB-UT                         
054800            PERFORM FX-VISA-INFO                                          
054801            PERFORM FY-HUR-MANGA-TABELLER                                 
054810          END-IF                                                          
054811          PERFORM IMS-GNP-2502-OKVAL                                      
054820        END-PERFORM                                                       
055000     END-IF                                                               
055100     .                                                                    
055200     EJECT                                                                
055300                                                                          
055400                                                                          
055500 FB-ENTER-UPDATE SECTION.                                                 
055600                                                                          
055700     PERFORM IMS-GU-2502                                                  
055800     IF MFS-UPDATE                                                        
055900        PERFORM IMS-GU-2502                                               
056000        IF SEGMENT-FINNS                                                  
056100           MOVE 2502-IDREFTAB TO MOD-IDREFTAB-FIRST                       
056200                                MOD-IDREFTAB-LAST                         
056300                                MOD-IDREFTAB-UT                           
056400           PERFORM FX-VISA-INFO                                           
056500           PERFORM FY-HUR-MANGA-TABELLER                                  
056600        ELSE                                                              
056700           PERFORM FZ-VISA-SEGMENT-SAKNAS                                 
056800        END-IF                                                            
056900     ELSE                                                                 
057000        IF SEGMENT-FINNS                                                  
057100           MOVE 2502-IDREFTAB    TO MOD-IDREFTAB-UT                       
057200           PERFORM FX-VISA-INFO                                           
057300           PERFORM FY-HUR-MANGA-TABELLER                                  
057400        ELSE                                                              
057500           PERFORM FZ-VISA-SEGMENT-SAKNAS                                 
057600        END-IF                                                            
057700        MOVE MID-IDREFTAB-FIRST TO MOD-IDREFTAB-FIRST                     
057800        MOVE MID-IDREFTAB-LAST TO MOD-IDREFTAB-LAST                       
057900     END-IF                                                               
058000                                                                          
058100     .                                                                    
058200     EJECT                                                                
058300                                                                          
058400                                                                          
058500 FC-FIRST SECTION.                                                        
058600                                                                          
058700     IF MID-IDREFTAB-FIRST = SPACE AND                                    
058800        MID-IDREFTAB-LAST = SPACE                                         
058900*       ENTER(PF7) NYA NYCKLAR                                            
059000        PERFORM IMS-GU-2502                                               
059100        IF SEGMENT-FINNS                                                  
059200           MOVE 2502-IDREFTAB TO MOD-IDREFTAB-FIRST                       
059300                                MOD-IDREFTAB-LAST                         
059400                                MOD-IDREFTAB-UT                           
059500           PERFORM FX-VISA-INFO                                           
059600           PERFORM FY-HUR-MANGA-TABELLER                                  
059700        ELSE                                                              
059800           PERFORM FZ-VISA-SEGMENT-SAKNAS                                 
059900        END-IF                                                            
060000     ELSE                                                                 
060100        IF MID-IDREFTAB-FIRST > SPACE AND                                 
060200           MID-IDREFTAB-LAST > SPACE                                      
060300*          PF7 GÅNG 1,                                                    
060400           PERFORM IMS-GU-2502                                            
060500           IF SEGMENT-FINNS                                               
060600              MOVE SPACE        TO MOD-IDREFTAB-FIRST                     
060700              MOVE 2502-IDREFTAB TO MOD-IDREFTAB-LAST                     
060800                                   MOD-IDREFTAB-UT                        
060900              PERFORM FX-VISA-INFO                                        
061000              PERFORM FY-HUR-MANGA-TABELLER                               
061100           ELSE                                                           
061200              PERFORM FZ-VISA-SEGMENT-SAKNAS                              
061300           END-IF                                                         
061400        ELSE                                                              
061500           IF MID-IDREFTAB-FIRST = SPACE AND                              
061600              MID-IDREFTAB-LAST > SPACE                                   
061700*             PF7 GÅNG 2                                                  
061800              PERFORM IMS-GU-2501                                         
061900              PERFORM IMS-GNP-2502-OKVAL                                  
061910              PERFORM UNTIL SEGMENT-SAKNAS OR SEGMENT-SLUT                
061930                IF SEGMENT-FINNS AND 2502-IDREFTAB NUMERIC                
062100                  MOVE 2502-IDREFTAB TO MOD-IDREFTAB-FIRST                
062200                                        MOD-IDREFTAB-LAST                 
062300                                        MOD-IDREFTAB-UT                   
062400                  PERFORM FX-VISA-INFO                                    
062500                  PERFORM FY-HUR-MANGA-TABELLER                           
062510                  ADD +1  TO IX1                                          
062520                END-IF                                                    
062801                  PERFORM IMS-GNP-2502-OKVAL                              
062810              END-PERFORM                                                 
062820              IF IX1 = 0                                                  
062821                   PERFORM FZ-VISA-SEGMENT-SAKNAS                         
062830              END-IF                                                      
062900           END-IF                                                         
063000        END-IF                                                            
063100     END-IF                                                               
063200     .                                                                    
063300     EJECT                                                                
063400                                                                          
063500                                                                          
063600 FX-VISA-INFO SECTION.                                                    
063700                                                                          
063800     IF INDATA-FEL                                                        
063900        IF MID-TEREFLIM NOT = ALL '+'                                     
064000           MOVE MID-TEREFLIM  TO MOD-TEREFLIM                             
064100        ELSE                                                              
064200           MOVE 2502-TEREFLIM TO MOD-TEREFLIM                             
064300        END-IF                                                            
064400     ELSE                                                                 
064500        MOVE 2502-TEREFLIM    TO MOD-TEREFLIM                             
064600     END-IF                                                               
064800                                                                          
064900     MOVE +1 TO KOL-IX                                                    
065000     PERFORM UNTIL KOL-IX > 8                                             
065100        MOVE 2502-KVPB-REF(KOL-IX) TO MOD-KVPB-REF(KOL-IX)                
065200        ADD +1 TO KOL-IX                                                  
065300     END-PERFORM                                                          
065400                                                                          
065500     MOVE +1 TO RAD-IX                                                    
065600     PERFORM UNTIL RAD-IX > 10                                            
065700        MOVE 2502-PRARTBES(RAD-IX) TO MOD-PRARTBES(RAD-IX)                
065800        MOVE +1 TO KOL-IX                                                 
065900        PERFORM UNTIL KOL-IX > 8                                          
066000           MOVE 2502-KVREFLIM(RAD-IX, KOL-IX) TO                          
066100                     MOD-KVREFLIM(RAD-IX, KOL-IX)                         
066200           MOVE 2502-KDREFPKT-LIM(RAD-IX, KOL-IX) TO                      
066300                     MOD-KDREFPKT-LIM(RAD-IX, KOL-IX)                     
066400           ADD +1  TO KOL-IX                                              
066500        END-PERFORM                                                       
066600        ADD +1 TO RAD-IX                                                  
066700     END-PERFORM                                                          
066800     PERFORM MFS-RENSA-COPY-FAELT                                         
066900     PERFORM MFS-FORM-ATTR-COPYFAELT                                      
066910     IF INDATA-OK                                                         
067000        PERFORM MFS-RENSA-FAELT-IN                                        
067010     END-IF                                                               
067100     .                                                                    
067200     EJECT                                                                
067300                                                                          
067400                                                                          
067500 FY-HUR-MANGA-TABELLER SECTION.                                           
067600                                                                          
067700     PERFORM IMS-GU-2501                                                  
067800     MOVE +0 TO WS-ANTAL-TABELLER                                         
067900     PERFORM IMS-GNP-2502                                                 
068000     PERFORM UNTIL SEGMENT-SAKNAS                                         
068010      IF 2502-IDREFTAB NUMERIC                                            
068100        ADD +1 TO WS-ANTAL-TABELLER                                       
068110      END-IF                                                              
068200      PERFORM IMS-GNP-2502                                                
068300     END-PERFORM                                                          
068400                                                                          
068500     MOVE WS-ANTAL-TABELLER TO UT-ANTAL-TABELLER                          
068600     IF WS-ANTAL-TABELLER = 1                                             
068700        MOVE TE-PF8-EN-TABELL     TO UT-TABELLTEXT                        
068800        MOVE TE-LAST-PAGE      TO UT-NEXT-PAGE                            
068900     ELSE                                                                 
069000        MOVE TE-PF8-FLER-TABELLER TO UT-TABELLTEXT                        
069100        IF WS-FL-SISTA-TABELL = NEJ                                       
069200           MOVE TE-PF8-MORE       TO UT-NEXT-PAGE                         
069300        ELSE                                                              
069400           MOVE TE-LAST-PAGE      TO UT-NEXT-PAGE                         
069500        END-IF                                                            
069600     END-IF                                                               
069700     MOVE MED-EX1 TO MOD-TEMFSINF                                         
069800     .                                                                    
069900     EJECT                                                                
070000                                                                          
070100                                                                          
070200 FZ-VISA-SEGMENT-SAKNAS SECTION.                                          
070300                                                                          
070400     PERFORM MFS-RENSA-FAELT-UT                                           
070500     IF MID-KDCMD-DELETE                                                  
070600        MOVE MED-1 TO MOD-TEMFSINF                                        
070700     ELSE                                                                 
070800        MOVE FEL-14 TO MOD-TEMFSFEL                                       
070900     END-IF                                                               
071000     .                                                                    
071100     EJECT                                                                
071200                                                                          
071300                                                                          
071400 G-KOLLA-INPUT SECTION.                                                   
071500                                                                          
071600     MOVE DCS-IDDC    TO W-IDDC                                           
071700     MOVE WS-IDREFTAB TO W-IDREFTAB                                       
071800                                                                          
071900     MOVE JA  TO INDATA-SW                                                
072000     IF MID-KDCMD = ALL '+' OR MID-KDCMD-INGENTING                        
072100        MOVE MFS-ALFA-FAELT-RAETT TO MOD-KDCMD-IN-ATTR                    
072200        MOVE 'R' TO MID-KDCMD                                             
072300     ELSE                                                                 
072400        IF MID-KDCMD-INSERT OR                                            
072500           MID-KDCMD-DELETE                                               
072600           MOVE MFS-ALFA-FAELT-RAETT TO MOD-KDCMD-IN-ATTR                 
072700        ELSE                                                              
072800           MOVE NEJ TO INDATA-SW                                          
072900           MOVE MFS-ALFA-FAELT-FEL TO MOD-KDCMD-IN-ATTR                   
073000        END-IF                                                            
073100     END-IF                                                               
073200                                                                          
073300     IF INDATA-OK                                                         
073400        IF MID-KDCMD-REPLACE                                              
073500           PERFORM GA-KOLLA-INPUT-ANDRING                                 
073600        ELSE                                                              
073700           IF MID-KDCMD-DELETE                                            
073800              PERFORM GB-KOLLA-INPUT-BORT                                 
073900           ELSE                                                           
074000              IF MID-KDCMD-INSERT                                         
074100                 PERFORM GC-KOLLA-INPUT-INSERT                            
074200              END-IF                                                      
074300           END-IF                                                         
074400        END-IF                                                            
074500     END-IF                                                               
074600     .                                                                    
074700     EJECT                                                                
074800                                                                          
074900                                                                          
075000 GA-KOLLA-INPUT-ANDRING SECTION.                                          
075100                                                                          
075200     IF MID-COPY-DATA = ALL '+'                                           
075300        IF MID-INPUT = ALL '+'                                            
075400           MOVE ERR-PF11-AND-NO-DATA TO MED-IDMFSFEL                      
075500           CALL WMEDKONV USING MED-WMEDAREA                               
075600           MOVE MED-TEMFSFEL TO MOD-TEMFSFEL                              
075700           PERFORM MFS-RENSA-FAELT-IN                                     
075800           PERFORM MFS-RENSA-FAELT-UT                                     
075900           MOVE MID-IDREFTAB-FIRST TO MOD-IDREFTAB-FIRST                  
076000           MOVE MID-IDREFTAB-LAST TO MOD-IDREFTAB-LAST                    
076100           MOVE NEJ TO INDATA-SW                                          
076200        ELSE                                                              
076300           PERFORM IMS-GU-2502                                            
076400           IF SEGMENT-FINNS                                               
076500              PERFORM S31-INDATA-KOLL                                     
076600              IF INDATA-FEL                                               
076700                 MOVE NEJ TO INDATA-SW                                    
076800                 MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL                
076900                 CALL WMEDKONV USING MED-WMEDAREA                         
077000                 MOVE MED-TEMFSFEL TO MOD-TEMFSFEL                        
077100                 MOVE MID-IDREFTAB-FIRST TO MOD-IDREFTAB-FIRST            
077200                 MOVE MID-IDREFTAB-LAST TO MOD-IDREFTAB-LAST              
077300                 PERFORM MFS-ROER-EJ-FAELT-UT                             
077400                 PERFORM MFS-ROER-EJ-FAELT-IN                             
077500              END-IF                                                      
077600           ELSE                                                           
077700              MOVE NEJ TO INDATA-SW                                       
077800              MOVE FEL-14 TO MOD-TEMFSFEL                                 
077900              MOVE MID-IDREFTAB-FIRST TO MOD-IDREFTAB-FIRST               
078000              MOVE MID-IDREFTAB-LAST TO MOD-IDREFTAB-LAST                 
078100              PERFORM MFS-ROER-EJ-FAELT-IN                                
078200              PERFORM MFS-ROER-EJ-FAELT-UT                                
078300           END-IF                                                         
078400        END-IF                                                            
078500     ELSE                                                                 
078600        MOVE NEJ TO INDATA-SW                                             
078700        MOVE FEL-15 TO MOD-TEMFSFEL                                       
078800        MOVE MID-IDREFTAB-FIRST TO MOD-IDREFTAB-FIRST                     
078900        MOVE MID-IDREFTAB-LAST TO MOD-IDREFTAB-LAST                       
079000        PERFORM MFS-ROER-EJ-FAELT-IN                                      
079100        PERFORM MFS-ROER-EJ-FAELT-UT                                      
079200     END-IF                                                               
079300     .                                                                    
079400     EJECT                                                                
079500                                                                          
079600                                                                          
079700 GB-KOLLA-INPUT-BORT SECTION.                                             
079800                                                                          
079900     IF MID-COPY-DATA = ALL '+'                                           
080000        IF MID-INPUT = ALL '+'                                            
080100           IF WS-IDREFTAB = 0                                             
080200              MOVE NEJ TO INDATA-SW                                       
080300              MOVE FEL-16 TO MOD-TEMFSFEL                                 
080400              MOVE MID-IDREFTAB-FIRST TO MOD-IDREFTAB-FIRST               
080500              MOVE MID-IDREFTAB-LAST TO MOD-IDREFTAB-LAST                 
080600              PERFORM MFS-ROER-EJ-FAELT-IN                                
080700              PERFORM MFS-ROER-EJ-FAELT-UT                                
080800           ELSE                                                           
080900              CONTINUE                                                    
081000           END-IF                                                         
081100        ELSE                                                              
081200           MOVE NEJ TO INDATA-SW                                          
081300           MOVE FEL-15 TO MOD-TEMFSFEL                                    
081400           MOVE MID-IDREFTAB-FIRST TO MOD-IDREFTAB-FIRST                  
081500           MOVE MID-IDREFTAB-LAST TO MOD-IDREFTAB-LAST                    
081600           PERFORM MFS-ROER-EJ-FAELT-IN                                   
081700           PERFORM MFS-ROER-EJ-FAELT-UT                                   
081800        END-IF                                                            
081900     ELSE                                                                 
082000        MOVE NEJ TO INDATA-SW                                             
082100        MOVE FEL-15 TO MOD-TEMFSFEL                                       
082200        MOVE MID-IDREFTAB-FIRST TO MOD-IDREFTAB-FIRST                     
082300        MOVE MID-IDREFTAB-LAST TO MOD-IDREFTAB-LAST                       
082400        PERFORM MFS-ROER-EJ-FAELT-IN                                      
082500        PERFORM MFS-ROER-EJ-FAELT-UT                                      
082600     END-IF                                                               
082700     .                                                                    
082800     EJECT                                                                
082900                                                                          
083000                                                                          
083100 GC-KOLLA-INPUT-INSERT SECTION.                                           
083200                                                                          
083300     PERFORM S31-INDATA-KOLL                                              
083400     IF INDATA-FEL                                                        
083500        MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL                         
083600        CALL WMEDKONV USING MED-WMEDAREA                                  
083700        MOVE MED-TEMFSFEL TO MOD-TEMFSFEL                                 
083800        MOVE MID-IDREFTAB-FIRST TO MOD-IDREFTAB-FIRST                     
083900        MOVE MID-IDREFTAB-LAST TO MOD-IDREFTAB-LAST                       
084000        PERFORM MFS-ROER-EJ-FAELT-UT                                      
084100        PERFORM MFS-ROER-EJ-FAELT-IN                                      
084200     END-IF                                                               
084300     .                                                                    
084400     EJECT                                                                
084500                                                                          
084600                                                                          
084700 H-UPPDATERA SECTION.                                                     
084800                                                                          
084900     IF MID-KDCMD-REPLACE OR                                              
085000        MID-KDCMD-INSERT                                                  
085100           PERFORM HA-ANDRA-TABELL                                        
085200     ELSE                                                                 
085300        IF MID-KDCMD-DELETE                                               
085400           PERFORM HB-TA-BORT-TABELL                                      
085500        END-IF                                                            
085600     END-IF                                                               
085700     .                                                                    
085800     EJECT                                                                
085900                                                                          
086000                                                                          
086100 HA-ANDRA-TABELL SECTION.                                                 
086200                                                                          
086300     PERFORM IMS-GHU-2502                                                 
086400                                                                          
086500     IF MID-TEREFLIM = ALL '+'                                            
086600        CONTINUE                                                          
086700     ELSE                                                                 
086800        IF MID-TEREFLIM = SPACE                                           
086900           MOVE SPACE TO 2502-TEREFLIM                                    
087000        ELSE                                                              
087100           MOVE MID-TEREFLIM TO 2502-TEREFLIM                             
087200        END-IF                                                            
087300     END-IF                                                               
087400                                                                          
087500     MOVE +1 TO KOL-IX                                                    
087600     PERFORM UNTIL KOL-IX > 8                                             
087700        IF MID-KVPB-REF(KOL-IX) NOT = ALL '+'                             
087800           MOVE WS-KVPB-REF(KOL-IX) TO 2502-KVPB-REF(KOL-IX)              
087900        END-IF                                                            
088000        ADD +1 TO KOL-IX                                                  
088100     END-PERFORM                                                          
088200                                                                          
088300     MOVE WS-RAD TO RAD-IX                                                
088400     IF MID-PRARTBES NOT = ALL '+'                                        
088500        MOVE WS-PRARTBES TO 2502-PRARTBES(RAD-IX)                         
088600     END-IF                                                               
088700     MOVE +1 TO KOL-IX                                                    
088800     PERFORM UNTIL KOL-IX > 8                                             
088900        IF MID-KVREFLIM(KOL-IX) NOT = ALL '+'                             
089000           MOVE WS-KVREFLIM(KOL-IX) TO                                    
089100                2502-KVREFLIM(RAD-IX, KOL-IX)                             
089200        END-IF                                                            
089300        IF MID-KDREFPKT-LIM(KOL-IX) NOT = ALL '+'                         
089400           MOVE WS-KDREFPKT-LIM(KOL-IX) TO                                
089500                2502-KDREFPKT-LIM(RAD-IX, KOL-IX)                         
089600        END-IF                                                            
089700        ADD +1 TO KOL-IX                                                  
089800     END-PERFORM                                                          
089900                                                                          
090000     PERFORM IMS-REPL-2502                                                
090100                                                                          
090200     MOVE INF-UPDATE-DONE TO MED-IDMFSINF                                 
090300     CALL WMEDKONV USING MED-WMEDAREA                                     
090400     MOVE MED-MFSINF TO MOD-TEMFSINF                                      
090500     PERFORM MFS-FORM-ATTR                                                
090600     PERFORM MFS-RENSA-FAELT-IN                                           
090700     .                                                                    
090800     EJECT                                                                
090900                                                                          
091000                                                                          
091100                                                                          
091200                                                                          
091300 HB-TA-BORT-TABELL SECTION.                                               
091400                                                                          
091500     PERFORM IMS-GHU-2502                                                 
091600                                                                          
091700     PERFORM IMS-DLET-2502                                                
091800                                                                          
091900     PERFORM IMS-GHU-2501                                                 
092000     PERFORM IMS-GNP-2502-OKVAL                                           
092100     IF SEGMENT-FINNS                                                     
092200        CONTINUE                                                          
092300     ELSE                                                                 
092400        PERFORM IMS-GHU-2501                                              
092500        PERFORM IMS-DLET-2501                                             
092600     END-IF                                                               
092700     .                                                                    
092800     EJECT                                                                
092900                                                                          
093000                                                                          
093100 I-SKAPA-NY-TABELL SECTION.                                               
093200                                                                          
093300     PERFORM IA-KOLLA-NYTT-TABELLNAMN                                     
093400     IF COPY-DATA-OK                                                      
093500        PERFORM IB-FINNS-NY-TABELL                                        
093600        IF COPY-DATA-OK                                                   
093700           IF MID-COPY-DATA NOT = ALL '+'                                 
093800              PERFORM IC-KOLLA-GAMMAL-TABELL                              
093900              IF COPY-DATA-OK                                             
094000                 PERFORM ID-SKAPA-KOPIERA-TABELL                          
094100              END-IF                                                      
094200           ELSE                                                           
094300              PERFORM IE-SKAPA-TOM-TABELL                                 
094400           END-IF                                                         
094500        END-IF                                                            
094600     END-IF                                                               
094700     .                                                                    
094800     EJECT                                                                
094900                                                                          
095000                                                                          
095100                                                                          
095200 IA-KOLLA-NYTT-TABELLNAMN SECTION.                                        
095300                                                                          
095400     MOVE JA TO COPY-DATA-SW                                              
095500                                                                          
095600     IF DCS-KDDC = SPACE OR DCS-DDC                                       
095700        MOVE NEJ TO COPY-DATA-SW                                          
095800     END-IF                                                               
095900                                                                          
096000     IF WS-IDREFTAB NUMERIC                                               
096100        CONTINUE                                                          
096200                                                                          
096300     ELSE                                                                 
096400        MOVE NEJ TO COPY-DATA-SW                                          
096500     END-IF                                                               
096600                                                                          
096700                                                                          
096800     IF DCS-IDDC               > SPACE AND                                
096900        WS-IDREFTAB            > ZERO                                     
097000        CONTINUE                                                          
097100     ELSE                                                                 
097200        MOVE NEJ TO INDATA-SW                                             
097300     END-IF                                                               
097400                                                                          
097500     IF COPY-DATA-OK                                                      
097600       CONTINUE                                                           
097700     ELSE                                                                 
097800       MOVE FEL-11     TO MOD-TEMFSFEL                                    
097900     END-IF                                                               
098000                                                                          
098100     .                                                                    
098200     EJECT                                                                
098300                                                                          
098400                                                                          
098500 IB-FINNS-NY-TABELL SECTION.                                              
098600                                                                          
098700     MOVE DCS-IDDC    TO W-IDDC                                           
098800     MOVE WS-IDREFTAB TO W-IDREFTAB                                       
098900     PERFORM IMS-GU-2502                                                  
099000     IF SEGMENT-FINNS                                                     
099100        MOVE NEJ TO COPY-DATA-SW                                          
099200        MOVE FEL-12     TO MOD-TEMFSFEL                                   
099300     END-IF                                                               
099400     .                                                                    
099500     EJECT                                                                
099600                                                                          
099700                                                                          
099800 IC-KOLLA-GAMMAL-TABELL SECTION.                                          
099900                                                                          
100000     IF MID-COPY-IDDC NOT = ALL '+'                                       
100100        MOVE MID-COPY-IDDC TO W-OLD-IDDC                                  
100200     END-IF                                                               
100300                                                                          
100400     IF MID-COPY-IDREFTAB NOT = ALL '+'                                   
100410       IF MID-COPY-IDREFTAB NUMERIC                                       
100500         MOVE MID-COPY-IDREFTAB TO W-OLD-IDREFTAB                         
100600       END-IF                                                             
100610     END-IF                                                               
100700                                                                          
100800     PERFORM IMS-GU-2502-GAMMAL                                           
100900     IF SEGMENT-FINNS                                                     
101000        CONTINUE                                                          
101100     ELSE                                                                 
101200        MOVE FEL-13           TO MOD-TEMFSFEL                             
101300        MOVE NEJ TO COPY-DATA-SW                                          
101400     END-IF                                                               
101500     .                                                                    
101600     EJECT                                                                
101700                                                                          
101800                                                                          
101900 ID-SKAPA-KOPIERA-TABELL SECTION.                                         
102000                                                                          
102100     PERFORM IMS-GU-2501                                                  
102200     IF SEGMENT-FINNS                                                     
102300        CONTINUE                                                          
102400     ELSE                                                                 
102500        PERFORM S30-SKAPA-2501                                            
102600        PERFORM IMS-ISRT-2501                                             
102700     END-IF                                                               
102800     PERFORM IDA-KOPIERA-TABELL                                           
102900     PERFORM IMS-ISRT-2502                                                
103000     .                                                                    
103100     EJECT                                                                
103200                                                                          
103300                                                                          
103400 IDA-KOPIERA-TABELL SECTION.                                              
103500                                                                          
103600     MOVE WS-IDREFTAB   TO 2502-IDREFTAB                                  
103700                           W-IDREFTAB                                     
103800                                                                          
103900     MOVE SPACE  TO 2502-TEREFLIM                                         
104000     MOVE MFS-RENSA-FAELT TO MOD-TEREFLIM                                 
104100                                                                          
104200     MOVE +1 TO KOL-IX                                                    
104300     PERFORM UNTIL KOL-IX > 8                                             
104400        MOVE GAMMAL-2502-KVPB-REF(KOL-IX) TO                              
104500                             2502-KVPB-REF(KOL-IX)                        
104600        ADD +1 TO KOL-IX                                                  
104700     END-PERFORM                                                          
104800                                                                          
104900     MOVE +1 TO RAD-IX                                                    
105000     PERFORM UNTIL RAD-IX > 10                                            
105100        MOVE GAMMAL-2502-PRARTBES(RAD-IX) TO                              
105200                             2502-PRARTBES(RAD-IX)                        
105300                                                                          
105400        MOVE +1 TO KOL-IX                                                 
105500        PERFORM UNTIL KOL-IX > 8                                          
105600*KOPIERA PUNKTER                                                          
105700           MOVE GAMMAL-2502-KVREFLIM(RAD-IX, KOL-IX) TO                   
105800                             2502-KVREFLIM(RAD-IX, KOL-IX)                
105900           MOVE GAMMAL-2502-KDREFPKT-LIM(RAD-IX, KOL-IX) TO               
106000                             2502-KDREFPKT-LIM(RAD-IX, KOL-IX)            
106100*KOPIERA KVANTER                                                          
106200           MOVE GAMMAL-2502-KVREFKVA(RAD-IX, KOL-IX) TO                   
106300                             2502-KVREFKVA(RAD-IX, KOL-IX)                
106400           MOVE GAMMAL-2502-KDREFPKT-KVA(RAD-IX, KOL-IX) TO               
106500                             2502-KDREFPKT-KVA(RAD-IX, KOL-IX)            
106600           ADD +1 TO KOL-IX                                               
106700        END-PERFORM                                                       
106800        ADD +1 TO RAD-IX                                                  
106900     END-PERFORM                                                          
107000     .                                                                    
107100     EJECT                                                                
107200                                                                          
107300                                                                          
107400 IE-SKAPA-TOM-TABELL SECTION.                                             
107500                                                                          
107600     PERFORM IMS-GU-2501                                                  
107700     IF SEGMENT-FINNS                                                     
107800        CONTINUE                                                          
107900     ELSE                                                                 
108000        PERFORM S30-SKAPA-2501                                            
108100        PERFORM IMS-ISRT-2501                                             
108200     END-IF                                                               
108300     PERFORM IEA-TOM-TABELL                                               
108400     PERFORM IMS-ISRT-2502                                                
108500     .                                                                    
108600     EJECT                                                                
108700                                                                          
108800                                                                          
108900 IEA-TOM-TABELL SECTION.                                                  
109000                                                                          
109100     MOVE WS-IDREFTAB       TO 2502-IDREFTAB                              
109200                               W-IDREFTAB                                 
109300     MOVE +1 TO KOL-IX                                                    
109400     PERFORM UNTIL KOL-IX > 8                                             
109500     IF KOL-IX = 8                                                        
109600        MOVE 999999.9     TO 2502-KVPB-REF(KOL-IX)                        
109700     ELSE                                                                 
109800        MOVE ZERO         TO 2502-KVPB-REF(KOL-IX)                        
109900     END-IF                                                               
110000     ADD +1 TO KOL-IX                                                     
110100     END-PERFORM                                                          
110200                                                                          
110300     MOVE +1 TO RAD-IX                                                    
110400                                                                          
110500     PERFORM UNTIL RAD-IX > 10                                            
110600        IF RAD-IX = 9                                                     
110700           MOVE 9999999.99  TO 2502-PRARTBES(RAD-IX)                      
110800        ELSE                                                              
110900           MOVE ZERO       TO   2502-PRARTBES(RAD-IX)                     
111000        END-IF                                                            
111100        MOVE +1 TO KOL-IX                                                 
111200        PERFORM UNTIL KOL-IX > 8                                          
111300           MOVE '1'     TO 2502-KVREFLIM(RAD-IX, KOL-IX)                  
111400           MOVE 'S'     TO 2502-KDREFPKT-LIM(RAD-IX, KOL-IX)              
111500           MOVE '1'     TO 2502-KVREFKVA(RAD-IX, KOL-IX)                  
111600           MOVE 'S'     TO 2502-KDREFPKT-KVA(RAD-IX, KOL-IX)              
111700           ADD +1 TO KOL-IX                                               
111800        END-PERFORM                                                       
111900        ADD +1 TO RAD-IX                                                  
112000     END-PERFORM                                                          
112100                                                                          
112200     MOVE SPACE TO 2502-TEREFLIM                                          
112300     .                                                                    
112400     EJECT                                                                
112500                                                                          
112600                                                                          
112700 J-VISA-FEL-VID-COPY SECTION.                                             
112800                                                                          
112900     PERFORM MFS-RENSA-FAELT-UT                                           
113000     PERFORM MFS-LAES-IN-IGEN                                             
113100     PERFORM S34-MID-INDATA-TILL-MOD                                      
113200     .                                                                    
113300     EJECT                                                                
113400                                                                          
113500                                                                          
113600 S30-SKAPA-2501 SECTION.                                                  
113700                                                                          
113800     MOVE '2501'              TO 2501-IDHTYP                              
113900     MOVE DCS-IDDC            TO 2501-IDDC                                
114000                                 W-IDDC                                   
114100     MOVE LOW-VALUE           TO 2501-LOWVALUE                            
114200     .                                                                    
114300     EJECT                                                                
114400                                                                          
114500                                                                          
114600 S31-INDATA-KOLL SECTION.                                                 
114700                                                                          
114800*KOLLA TEXTRAD                                                            
114900                                                                          
115000     IF MID-TEREFLIM = ALL '+' OR SPACE                                   
115100        MOVE MFS-ALFA-FAELT-RAETT  TO MOD-TEREFLIM-ATTR                   
115200     ELSE                                                                 
115300        MOVE MFS-ALFA-FAELT-RAETT  TO MOD-TEREFLIM-ATTR                   
115400     END-IF                                                               
115500                                                                          
115600                                                                          
115700*KOLLA PB-RAD                                                             
115800                                                                          
115900     MOVE +1 TO KOL-IX                                                    
116000     PERFORM UNTIL KOL-IX > 8                                             
116100        IF MID-KVPB-REF(KOL-IX) NOT = ALL '+'                             
116200           MOVE MID-KVPB-REF(KOL-IX) TO DEC-IDFRIDATA                     
116300           MOVE 5                    TO DEC-KVHELTAL                      
116400           MOVE 1                    TO DEC-KVDECIMAL                     
116500           MOVE 'P'                  TO DEC-KDSVAR                        
116600           CALL WDECEDIT USING DEC-WDECAREA                               
116700           IF DEC-KDSVAR-OK                                               
116800              MOVE DEC-IDEDITDATA TO WS-KVPB-REF(KOL-IX)                  
116900              IF KOL-IX = 8                                               
117000                 MOVE 999999.9    TO WS-KVPB-REF(KOL-IX)                  
117100              END-IF                                                      
117200              MOVE MFS-NUM-FAELT-RAETT  TO                                
117300                           MOD-KVPB-REF-IN-ATTR(KOL-IX)                   
117400           ELSE                                                           
117500              MOVE ZERO TO WS-KVPB-REF(KOL-IX)                            
117600              MOVE NEJ TO INDATA-SW                                       
117700              MOVE MFS-NUM-FAELT-FEL TO                                   
117800                           MOD-KVPB-REF-IN-ATTR(KOL-IX)                   
117900           END-IF                                                         
118000        ELSE                                                              
118100           MOVE MFS-NUM-FAELT-RAETT TO                                    
118200                        MOD-KVPB-REF-IN-ATTR(KOL-IX)                      
118300        END-IF                                                            
118400        ADD +1 TO KOL-IX                                                  
118500     END-PERFORM                                                          
118600                                                                          
118700* SKAPA OCH KONTROLLERA JÄMFÖRELSETABELL FÖR ATT FÅ                       
118800* PB I STIGANDE ORDNING                                                   
118900                                                                          
119000     IF MID-KDCMD-REPLACE                                                 
119100        MOVE 1 TO KOL-IX                                                  
119200        PERFORM UNTIL KOL-IX > 8                                          
119300           IF MID-KVPB-REF(KOL-IX) NOT = ALL '+'                          
119400              MOVE WS-KVPB-REF(KOL-IX) TO JMF-KVPB-REF(KOL-IX)            
119500           ELSE                                                           
119600              MOVE 2502-KVPB-REF(KOL-IX) TO JMF-KVPB-REF(KOL-IX)          
119700           END-IF                                                         
119800           ADD +1 TO KOL-IX                                               
119900        END-PERFORM                                                       
120000                                                                          
120100        MOVE +1 TO KOL-IX                                                 
120200        PERFORM UNTIL KOL-IX > 8                                          
120300           IF KOL-IX = 1                                                  
120400              IF JMF-KVPB-REF(KOL-IX) < JMF-KVPB-REF(KOL-IX + 1)          
120500                 MOVE MFS-NUM-FAELT-RAETT  TO                             
120600                              MOD-KVPB-REF-IN-ATTR(KOL-IX)                
120700              ELSE                                                        
120800                 MOVE NEJ TO INDATA-SW                                    
120900                 MOVE MFS-NUM-FAELT-FEL TO                                
121000                              MOD-KVPB-REF-IN-ATTR(KOL-IX)                
121100              END-IF                                                      
121200           END-IF                                                         
121300                                                                          
121400           IF KOL-IX > 1 AND < 8                                          
121500             IF (JMF-KVPB-REF(KOL-IX) >                                   
121600                                 JMF-KVPB-REF(KOL-IX - 1))  AND           
121700                (JMF-KVPB-REF(KOL-IX) < JMF-KVPB-REF(KOL-IX + 1))         
121800                MOVE MFS-NUM-FAELT-RAETT  TO                              
121900                              MOD-KVPB-REF-IN-ATTR(KOL-IX)                
122000             ELSE                                                         
122100                MOVE NEJ TO INDATA-SW                                     
122200                MOVE MFS-NUM-FAELT-FEL TO                                 
122300                             MOD-KVPB-REF-IN-ATTR(KOL-IX)                 
122400             END-IF                                                       
122500           END-IF                                                         
122600                                                                          
122700           IF KOL-IX = 8                                                  
122800              IF JMF-KVPB-REF(KOL-IX)  > JMF-KVPB-REF(KOL-IX - 1)         
122900                 MOVE MFS-NUM-FAELT-RAETT  TO                             
123000                              MOD-KVPB-REF-IN-ATTR(KOL-IX)                
123100              ELSE                                                        
123200                 MOVE NEJ TO INDATA-SW                                    
123300                 MOVE MFS-NUM-FAELT-FEL TO                                
123400                              MOD-KVPB-REF-IN-ATTR(KOL-IX)                
123500              END-IF                                                      
123600           END-IF                                                         
123700           ADD +1 TO KOL-IX                                               
123800        END-PERFORM                                                       
123900     END-IF                                                               
124000                                                                          
124100*RAD                                                                      
124200                                                                          
124300     IF MID-RAD NOT = ALL '+'                                             
124400        IF MID-RAD NUMERIC                                                
124500           IF MID-RAD > 0 AND <= 9                                        
124600              MOVE MID-RAD TO WS-RAD                                      
124700              MOVE MFS-NUM-FAELT-RAETT  TO                                
124800                           MOD-RAD-IN-ATTR                                
124900           ELSE                                                           
125000              MOVE NEJ TO  INDATA-SW                                      
125100              MOVE MFS-NUM-FAELT-FEL TO MOD-RAD-IN-ATTR                   
125200           END-IF                                                         
125300        ELSE                                                              
125400           MOVE NEJ TO  INDATA-SW                                         
125500           MOVE MFS-NUM-FAELT-FEL TO MOD-RAD-IN-ATTR                      
125600        END-IF                                                            
125700     ELSE                                                                 
125800           MOVE MFS-NUM-FAELT-RAETT TO MOD-RAD-IN-ATTR                    
125900     END-IF                                                               
126000                                                                          
126100                                                                          
126200*KOLLA PRIS-PUNKTRAD                                                      
126300                                                                          
126400     PERFORM S31A-FINNS-RADANGIVELSE                                      
126500                                                                          
126600     IF MID-PRARTBES NOT = ALL '+'                                        
126700        MOVE MID-PRARTBES   TO DEC-IDFRIDATA                              
126800        MOVE 7              TO DEC-KVHELTAL                               
126900        MOVE 2              TO DEC-KVDECIMAL                              
127000        MOVE 'P'                  TO DEC-KDSVAR                           
127100        CALL WDECEDIT USING DEC-WDECAREA                                  
127200        IF DEC-KDSVAR-OK                                                  
127300           IF MID-RAD = 9                                                 
127400              MOVE 9999999.99     TO WS-PRARTBES                          
127500           ELSE                                                           
127600              MOVE DEC-IDEDITDATA TO WS-PRARTBES                          
127700           END-IF                                                         
127800           MOVE MFS-NUM-FAELT-RAETT  TO                                   
127900                        MOD-PRARTBES-IN-ATTR                              
128000        ELSE                                                              
128100           MOVE NEJ TO  INDATA-SW                                         
128200           MOVE MFS-NUM-FAELT-FEL TO MOD-PRARTBES-IN-ATTR                 
128300        END-IF                                                            
128400     ELSE                                                                 
128500           MOVE MFS-NUM-FAELT-RAETT TO MOD-PRARTBES-IN-ATTR               
128600     END-IF                                                               
128700                                                                          
128800                                                                          
128900*KVREFLIM, KDREFPKT-LIM                                                   
129000                                                                          
129100     MOVE +1 TO KOL-IX                                                    
129200     PERFORM UNTIL KOL-IX > 8                                             
129300        IF MID-KVREFLIM(KOL-IX) NOT = ALL '+'                             
129400           IF MID-KVREFLIM(KOL-IX) NUMERIC                                
129500              MOVE MID-KVREFLIM(KOL-IX) TO WS-KVREFLIM(KOL-IX)            
129600              MOVE MFS-NUM-FAELT-RAETT  TO                                
129700                           MOD-KVREFLIM-IN-ATTR(KOL-IX)                   
129800           ELSE                                                           
129900              MOVE NEJ TO INDATA-SW                                       
130000              MOVE MFS-NUM-FAELT-FEL TO                                   
130100                           MOD-KVREFLIM-IN-ATTR(KOL-IX)                   
130200           END-IF                                                         
130300        ELSE                                                              
130400           MOVE MFS-NUM-FAELT-RAETT TO                                    
130500                        MOD-KVREFLIM-IN-ATTR(KOL-IX)                      
130510           IF MID-KDCMD-REPLACE AND WS-RAD > +0                           
130520              MOVE 2502-KVREFLIM(WS-RAD, KOL-IX)                          
130530                                     TO WS-KVREFLIM(KOL-IX)               
130540           ELSE                                                           
130550              MOVE ZERO              TO WS-KVREFLIM(KOL-IX)               
130560           END-IF                                                         
130600        END-IF                                                            
130700                                                                          
130800        IF MID-KDREFPKT-LIM(KOL-IX) NOT = ALL '+'                         
130900           IF MID-KDREFPKT-LIM(KOL-IX) = 'S' OR 'D'                       
131000              MOVE MID-KDREFPKT-LIM(KOL-IX) TO                            
131100                                      WS-KDREFPKT-LIM(KOL-IX)             
131200              MOVE MFS-ALFA-FAELT-RAETT  TO                               
131300                           MOD-KDREFPKT-LIM-IN-ATTR(KOL-IX)               
131400           ELSE                                                           
131500              MOVE NEJ TO INDATA-SW                                       
131600              MOVE MFS-ALFA-FAELT-FEL TO                                  
131700                           MOD-KDREFPKT-LIM-IN-ATTR(KOL-IX)               
131800           END-IF                                                         
131900        ELSE                                                              
132000           MOVE MFS-ALFA-FAELT-RAETT TO                                   
132100                        MOD-KDREFPKT-LIM-IN-ATTR(KOL-IX)                  
132210           IF MID-KDCMD-REPLACE AND WS-RAD > +0                           
132220              MOVE 2502-KDREFPKT-LIM (WS-RAD, KOL-IX)                     
132230                                     TO WS-KDREFPKT-LIM(KOL-IX)           
132240           ELSE                                                           
132250              MOVE ZERO              TO WS-KDREFPKT-LIM(KOL-IX)           
132260           END-IF                                                         
132270        END-IF                                                            
132280                                                                          
132290        IF WS-KDREFPKT-LIM(KOL-IX) = 'D'                                  
132291       AND WS-KVREFLIM    (KOL-IX) = ZERO                                 
132292           MOVE NEJ                  TO INDATA-SW                         
132293           MOVE MFS-NUM-FAELT-FEL    TO                                   
132294                           MOD-KVREFLIM-IN-ATTR    (KOL-IX)               
132295           MOVE MFS-ALFA-FAELT-FEL   TO                                   
132296                           MOD-KDREFPKT-LIM-IN-ATTR(KOL-IX)               
132297        END-IF                                                            
132298                                                                          
132300        ADD +1 TO KOL-IX                                                  
132400     END-PERFORM                                                          
132500     .                                                                    
132600     EJECT                                                                
132700                                                                          
132800                                                                          
132900 S31A-FINNS-RADANGIVELSE SECTION.                                         
133000                                                                          
133100     IF MID-RAD = ALL '+'                                                 
133200        IF MID-PRARTBES NOT = ALL '+'                                     
133300           MOVE NEJ TO INDATA-SW                                          
133400           MOVE MFS-NUM-FAELT-FEL  TO MOD-RAD-IN                          
133500        END-IF                                                            
133600                                                                          
133700        MOVE +1 TO KOL-IX                                                 
133800        PERFORM UNTIL KOL-IX > 8                                          
133900           IF MID-KVREFLIM(KOL-IX) NOT = ALL '+'                          
134000           OR MID-KDREFPKT-LIM(KOL-IX) NOT = ALL '+'                      
134100              MOVE NEJ TO INDATA-SW                                       
134200              MOVE MFS-NUM-FAELT-FEL TO MOD-RAD-IN-ATTR                   
134300           END-IF                                                         
134400           ADD +1 TO KOL-IX                                               
134500        END-PERFORM                                                       
134600     END-IF                                                               
134700                                                                          
134800     IF MID-RAD NOT = ALL '+'                                             
134900        IF MID-PRARTBES       = ALL '+' AND                               
135000                                                                          
135100           MID-KVREFLIM(1)    = ALL '+' AND                               
135200           MID-KVREFLIM(2)    = ALL '+' AND                               
135300           MID-KVREFLIM(3)    = ALL '+' AND                               
135400           MID-KVREFLIM(4)    = ALL '+' AND                               
135500           MID-KVREFLIM(5)    = ALL '+' AND                               
135600           MID-KVREFLIM(6)    = ALL '+' AND                               
135700           MID-KVREFLIM(7)    = ALL '+' AND                               
135800           MID-KVREFLIM(8)    = ALL '+' AND                               
135900                                                                          
136000           MID-KDREFPKT-LIM(1) = ALL '+' AND                              
136100           MID-KDREFPKT-LIM(2) = ALL '+' AND                              
136200           MID-KDREFPKT-LIM(3) = ALL '+' AND                              
136300           MID-KDREFPKT-LIM(4) = ALL '+' AND                              
136400           MID-KDREFPKT-LIM(5) = ALL '+' AND                              
136500           MID-KDREFPKT-LIM(6) = ALL '+' AND                              
136600           MID-KDREFPKT-LIM(7) = ALL '+' AND                              
136700           MID-KDREFPKT-LIM(8) = ALL '+'                                  
136800           MOVE NEJ TO INDATA-SW                                          
136900           MOVE MFS-NUM-FAELT-FEL  TO MOD-RAD-IN                          
137000        END-IF                                                            
137100                                                                          
137200     END-IF                                                               
137300     .                                                                    
137400     EJECT                                                                
137500                                                                          
137600                                                                          
137700                                                                          
137800 S34-MID-INDATA-TILL-MOD SECTION.                                         
137900                                                                          
138000                                                                          
138100* KDCMD                                                                   
138200                                                                          
138300     IF MID-KDCMD = ALL '+'                                               
138400        MOVE MFS-RENSA-FAELT         TO MOD-KDCMD-IN                      
138500     ELSE                                                                 
138600        MOVE MID-KDCMD               TO MOD-KDCMD-IN                      
138700     END-IF                                                               
138800                                                                          
138900                                                                          
139000* COPY-IDDC                                                               
139100                                                                          
139200     IF MID-COPY-IDDC = ALL '+'                                           
139300        MOVE MFS-RENSA-FAELT         TO MOD-COPY-IDDC-IN                  
139400     ELSE                                                                 
139500        MOVE MID-COPY-IDDC           TO MOD-COPY-IDDC-IN                  
139600     END-IF                                                               
139700                                                                          
139800                                                                          
139900* COPY-IDREFTAB                                                           
140000                                                                          
140100     IF MID-COPY-IDREFTAB = ALL '+'                                       
140200        MOVE MFS-RENSA-FAELT         TO MOD-COPY-IDREFTAB-IN              
140300     ELSE                                                                 
140400        MOVE MID-COPY-IDREFTAB       TO MOD-COPY-IDREFTAB-IN              
140500     END-IF                                                               
140600                                                                          
140700                                                                          
140800* TEREFLIM                                                                
140900                                                                          
141000     IF MID-TEREFLIM = ALL '+'                                            
141100        MOVE MFS-RENSA-FAELT         TO MOD-TEREFLIM                      
141200     ELSE                                                                 
141300        MOVE MID-TEREFLIM            TO MOD-TEREFLIM                      
141400     END-IF                                                               
141500                                                                          
141600                                                                          
141700* PB-RAD                                                                  
141800                                                                          
141900     MOVE +1    TO KOL-IX                                                 
142000     PERFORM UNTIL KOL-IX > 8                                             
142100        IF MID-KVPB-REF(KOL-IX) = ALL '+'                                 
142200           MOVE MFS-RENSA-FAELT      TO MOD-KVPB-REF-IN(KOL-IX)           
142300        ELSE                                                              
142400           MOVE MFS-ROER-EJ-FAELT    TO MOD-KVPB-REF-IN(KOL-IX)           
142500        END-IF                                                            
142600        ADD +1 TO KOL-IX                                                  
142700     END-PERFORM                                                          
142800                                                                          
142900                                                                          
143000* PRIS-RAD                                                                
143100                                                                          
143200     IF MID-RAD = ALL '+'                                                 
143300        MOVE MFS-RENSA-FAELT      TO MOD-RAD-IN                           
143400     ELSE                                                                 
143500        MOVE MFS-ROER-EJ-FAELT    TO MOD-RAD-IN                           
143600     END-IF                                                               
143700                                                                          
143800     IF MID-PRARTBES = ALL '+'                                            
143900        MOVE MFS-RENSA-FAELT      TO MOD-PRARTBES-IN                      
144000     ELSE                                                                 
144100        MOVE MFS-ROER-EJ-FAELT    TO MOD-PRARTBES-IN                      
144200     END-IF                                                               
144300                                                                          
144400     MOVE +1    TO KOL-IX                                                 
144500     PERFORM UNTIL KOL-IX > 8                                             
144600        IF MID-KVREFLIM(KOL-IX) = ALL '+'                                 
144700           MOVE MFS-RENSA-FAELT      TO MOD-KVREFLIM-IN(KOL-IX)           
144800        ELSE                                                              
144900           MOVE MFS-ROER-EJ-FAELT    TO MOD-KVREFLIM-IN(KOL-IX)           
145000        END-IF                                                            
145100        IF MID-KDREFPKT-LIM(KOL-IX) = ALL '+'                             
145200           MOVE MFS-RENSA-FAELT   TO MOD-KDREFPKT-LIM-IN(KOL-IX)          
145300        ELSE                                                              
145400           MOVE MFS-ROER-EJ-FAELT TO MOD-KDREFPKT-LIM-IN(KOL-IX)          
145500        END-IF                                                            
145600        ADD +1 TO KOL-IX                                                  
145700     END-PERFORM                                                          
145800     .                                                                    
145900     EJECT                                                                
146000                                                                          
146100                                                                          
146200                                                                          
146300                                                                          
146400                                                                          
146500                                                                          
146600                                                                          
146700 MFS-RENSA-FAELT-UT SECTION.                                              
146800                                                                          
146900     MOVE MFS-RENSA-FAELT TO MOD-TEREFLIM                                 
147000     MOVE +1 TO KOL-IX                                                    
147100     PERFORM UNTIL KOL-IX > 8                                             
147200        MOVE MFS-RENSA-FAELT    TO MOD-KVPB-REF(KOL-IX)                   
147300        ADD +1 TO KOL-IX                                                  
147400     END-PERFORM                                                          
147500                                                                          
147600     MOVE +1 TO RAD-IX                                                    
147700     PERFORM UNTIL RAD-IX > 10                                            
147800        MOVE MFS-RENSA-FAELT TO MOD-PRARTBES(RAD-IX)                      
147900        MOVE +1 TO KOL-IX                                                 
148000        PERFORM UNTIL KOL-IX > 8                                          
148100           MOVE MFS-RENSA-FAELT TO                                        
148200                               MOD-KVREFLIM(RAD-IX, KOL-IX)               
148300                               MOD-KDREFPKT-LIM(RAD-IX, KOL-IX)           
148400           ADD +1 1 TO KOL-IX                                             
148500        END-PERFORM                                                       
148600        ADD +1 TO RAD-IX                                                  
148700     END-PERFORM                                                          
148800     .                                                                    
148900     EJECT                                                                
149000                                                                          
149100                                                                          
149200 MFS-RENSA-FAELT-IN SECTION.                                              
149300                                                                          
149400*    MOVE MFS-RENSA-FAELT TO MOD-TEREFLIM                                 
149500                                                                          
149600     MOVE MFS-RENSA-FAELT    TO MOD-COPY-IDDC-IN                          
149700                                MOD-COPY-IDREFTAB-IN                      
149800                                                                          
149900     MOVE +1 TO KOL-IX                                                    
150000     PERFORM UNTIL KOL-IX > 8                                             
150100        MOVE MFS-RENSA-FAELT TO MOD-KVPB-REF-IN(KOL-IX)                   
150200        ADD +1 TO KOL-IX                                                  
150300     END-PERFORM                                                          
150400                                                                          
150500     MOVE MFS-RENSA-FAELT TO MOD-RAD-IN                                   
150600                             MOD-PRARTBES-IN                              
150700                                                                          
150800     MOVE +1 TO KOL-IX                                                    
150900     PERFORM UNTIL KOL-IX > 8                                             
151000        MOVE MFS-RENSA-FAELT TO MOD-KVREFLIM-IN(KOL-IX)                   
151100                                MOD-KDREFPKT-LIM-IN(KOL-IX)               
151200        ADD +1 TO KOL-IX                                                  
151300     END-PERFORM                                                          
151400     .                                                                    
151500     EJECT                                                                
151600                                                                          
151700                                                                          
151800 MFS-RENSA-COPY-FAELT SECTION.                                            
151900                                                                          
152000     MOVE MFS-RENSA-FAELT   TO MOD-COPY-IDDC-IN                           
152100                               MOD-COPY-IDREFTAB-IN                       
152200                               MOD-KDCMD-IN                               
152300     .                                                                    
152400     EJECT                                                                
152500                                                                          
152600                                                                          
152700 MFS-FORM-ATTR-COPYFAELT SECTION.                                         
152800                                                                          
152900     MOVE MFS-FORMATETS-ATTR   TO MOD-COPY-IDDC-IN-ATTR                   
153000                                  MOD-COPY-IDREFTAB-IN-ATTR               
153100                                  MOD-KDCMD-IN-ATTR                       
153200     .                                                                    
153300     EJECT                                                                
153400                                                                          
153500                                                                          
153600 MFS-ROER-EJ-FAELT-UT  SECTION.                                           
153700                                                                          
153800     MOVE MFS-ROER-EJ-FAELT TO MOD-TEREFLIM                               
153900                                                                          
154000     MOVE +1 TO KOL-IX                                                    
154100     PERFORM UNTIL KOL-IX > 8                                             
154200        MOVE MFS-ROER-EJ-FAELT    TO MOD-KVPB-REF(KOL-IX)                 
154300        ADD +1 1 TO KOL-IX                                                
154400     END-PERFORM                                                          
154500                                                                          
154600     MOVE +1 TO RAD-IX                                                    
154700     PERFORM UNTIL RAD-IX > 10                                            
154800        MOVE MFS-ROER-EJ-FAELT    TO MOD-PRARTBES(RAD-IX)                 
154900        MOVE +1 TO KOL-IX                                                 
155000        PERFORM UNTIL KOL-IX > 8                                          
155100           MOVE MFS-ROER-EJ-FAELT TO                                      
155200                                  MOD-KVREFLIM(RAD-IX, KOL-IX)            
155300                                  MOD-KDREFPKT-LIM(RAD-IX, KOL-IX)        
155400           ADD +1 1 TO KOL-IX                                             
155500        END-PERFORM                                                       
155600        ADD +1 TO RAD-IX                                                  
155700     END-PERFORM                                                          
155800     .                                                                    
155900     EJECT                                                                
156000                                                                          
156100                                                                          
156200 MFS-ROER-EJ-FAELT-IN  SECTION.                                           
156300                                                                          
156400     MOVE MFS-ROER-EJ-FAELT    TO MOD-COPY-IDDC-IN                        
156500                                  MOD-COPY-IDREFTAB-IN                    
156600                                                                          
156700     MOVE MFS-ROER-EJ-FAELT    TO MOD-TEREFLIM                            
156800                                                                          
156900     MOVE +1 TO KOL-IX                                                    
157000     PERFORM UNTIL KOL-IX > 8                                             
157100        MOVE MFS-ROER-EJ-FAELT TO MOD-KVPB-REF-IN(KOL-IX)                 
157200        ADD +1 TO KOL-IX                                                  
157300     END-PERFORM                                                          
157400                                                                          
157500     MOVE MFS-ROER-EJ-FAELT TO MOD-RAD-IN                                 
157600                               MOD-PRARTBES-IN                            
157700                                                                          
157800     MOVE +1 TO KOL-IX                                                    
157900     PERFORM UNTIL KOL-IX > 8                                             
158000        MOVE MFS-ROER-EJ-FAELT TO MOD-KVREFLIM-IN(KOL-IX)                 
158100                                  MOD-KDREFPKT-LIM-IN(KOL-IX)             
158200        ADD +1 TO KOL-IX                                                  
158300     END-PERFORM                                                          
158400     .                                                                    
158500     EJECT                                                                
158600                                                                          
158700                                                                          
158800 MFS-FORM-ATTR SECTION.                                                   
158900                                                                          
159000     MOVE MFS-FORMATETS-ATTR    TO MOD-COPY-IDDC-IN                       
159100                                   MOD-COPY-IDREFTAB-IN                   
159200                                                                          
159300     MOVE MFS-FORMATETS-ATTR    TO MOD-TEREFLIM                           
159400                                                                          
159500     MOVE +1 TO KOL-IX                                                    
159600     PERFORM UNTIL KOL-IX > 8                                             
159700        MOVE MFS-FORMATETS-ATTR TO MOD-KVPB-REF-IN-ATTR(KOL-IX)           
159800        ADD +1 TO KOL-IX                                                  
159900     END-PERFORM                                                          
160000                                                                          
160100     MOVE MFS-FORMATETS-ATTR TO MOD-RAD-IN-ATTR                           
160200                                MOD-PRARTBES-IN-ATTR                      
160300                                                                          
160400     MOVE +1 TO KOL-IX                                                    
160500     PERFORM UNTIL KOL-IX > 8                                             
160600        MOVE MFS-FORMATETS-ATTR TO                                        
160700                                MOD-KVREFLIM-IN-ATTR(KOL-IX)              
160800                                MOD-KDREFPKT-LIM-IN-ATTR(KOL-IX)          
160900        ADD +1 TO KOL-IX                                                  
161000     END-PERFORM                                                          
161100     .                                                                    
161200     EJECT                                                                
161300                                                                          
161400                                                                          
161500 MFS-LAES-IN-IGEN SECTION.                                                
161600                                                                          
161700     MOVE MFS-ADD-LAES-IN-FAELT TO MOD-KDCMD-IN-ATTR                      
161800                                                                          
161900     MOVE MFS-ADD-LAES-IN-FAELT TO MOD-COPY-IDDC-IN                       
162000                                   MOD-COPY-IDREFTAB-IN                   
162100                                                                          
162200     MOVE MFS-ADD-LAES-IN-FAELT TO MOD-TEREFLIM-ATTR                      
162300                                                                          
162400     MOVE MFS-ADD-LAES-IN-FAELT    TO MOD-RAD-IN-ATTR                     
162500                                      MOD-PRARTBES-IN-ATTR                
162600     MOVE +1 TO KOL-IX                                                    
162700     PERFORM UNTIL KOL-IX > 8                                             
162800        MOVE MFS-ADD-LAES-IN-FAELT TO                                     
162900                                  MOD-KVPB-REF-IN-ATTR(KOL-IX)            
163000                                  MOD-KVREFLIM-IN-ATTR(KOL-IX)            
163100                                  MOD-KDREFPKT-LIM-IN-ATTR(KOL-IX)        
163200        ADD +1 TO KOL-IX                                                  
163300     END-PERFORM                                                          
163400     .                                                                    
163500     EJECT                                                                
163600                                                                          
163700                                                                          
163800* --- IMS SEKTIONER ---                                                   
163900     SKIP3                                                                
164000 IMS-GET-MSG SECTION.                                                     
164100                                                                          
164200     MOVE '  QC' TO GODK-STATUSKODER                                      
164300     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
164400     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
164500     PERFORM IMS-STATUSKONTROLL                                           
164600     .                                                                    
164700     SKIP3                                                                
164800 IMS-INSERT-MSG SECTION.                                                  
164900                                                                          
165000     IF ENGLISH-TEXT                                                      
165100       MOVE 'N' TO MFS-KDHUVOMR                                           
165200     END-IF                                                               
165300     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
165400     MOVE SPACE TO GODK-STATUSKODER                                       
165500     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
165600     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
165700     PERFORM IMS-STATUSKONTROLL                                           
165800     .                                                                    
165900     EJECT                                                                
166000                                                                          
166100                                                                          
166200 IMS-GU-2502 SECTION.                                                     
166300     STRING 'WL250101(WDGXKEY  =' W-WDGXKEY-X ')'                         
166400          DELIMITED BY SIZE INTO SSA1                                     
166500     STRING 'WL250111(IDREFTAB =' W-IDREFTAB-X ')'                        
166600          DELIMITED BY SIZE INTO SSA2                                     
166700     MOVE '  GE' TO GODK-STATUSKODER                                      
166800     CALL CBLTDLI USING GU WDR2-PCB DLI-IO-AREA-1 SSA1 SSA2               
166900     MOVE WDR2-STATUS-CODE TO STATUS-WS                                   
167000     PERFORM IMS-STATUSKONTROLL                                           
167100     .                                                                    
167200     EJECT                                                                
167300                                                                          
167400                                                                          
167500 IMS-GNP-2502-NEXT SECTION.                                               
167600     STRING 'WL250111(IDREFTAB >' W-IDREFTAB-X ')'                        
167700          DELIMITED BY SIZE INTO SSA1                                     
167800     MOVE '  GE' TO GODK-STATUSKODER                                      
167900     CALL CBLTDLI USING GNP WDR2-PCB DLI-IO-AREA-1 SSA1                   
168000     MOVE WDR2-STATUS-CODE TO STATUS-WS                                   
168100     PERFORM IMS-STATUSKONTROLL                                           
168200     .                                                                    
168300     EJECT                                                                
168400                                                                          
168500                                                                          
168600 IMS-GU-2501 SECTION.                                                     
168700     STRING 'WL250101(WDGXKEY  =' W-WDGXKEY-X ')'                         
168800          DELIMITED BY SIZE INTO SSA1                                     
168900     MOVE '  GE' TO GODK-STATUSKODER                                      
169000     CALL CBLTDLI USING GU WDR2-PCB DLI-IO-AREA-1 SSA1                    
169100     MOVE WDR2-STATUS-CODE TO STATUS-WS                                   
169200     PERFORM IMS-STATUSKONTROLL                                           
169300     .                                                                    
169400     EJECT                                                                
169500                                                                          
169600                                                                          
169700 IMS-GNP-2502 SECTION.                                                    
169800     MOVE  'WL250111 ' TO SSA1                                            
169900     MOVE '  GEGB' TO GODK-STATUSKODER                                    
170000     CALL CBLTDLI USING GNP WDR2-PCB DLI-IO-AREA-1 SSA1                   
170100     MOVE WDR2-STATUS-CODE TO STATUS-WS                                   
170200     PERFORM IMS-STATUSKONTROLL                                           
170300     .                                                                    
170400     EJECT                                                                
170500                                                                          
170600                                                                          
170700 IMS-GHU-2501 SECTION.                                                    
170800     STRING 'WL250101(WDGXKEY  =' W-WDGXKEY-X ')'                         
170900          DELIMITED BY SIZE INTO SSA1                                     
171000     MOVE '  GE' TO GODK-STATUSKODER                                      
171100     CALL CBLTDLI USING GHU WDR2-PCB DLI-IO-AREA-1 SSA1                   
171200     MOVE WDR2-STATUS-CODE TO STATUS-WS                                   
171300     PERFORM IMS-STATUSKONTROLL                                           
171400     .                                                                    
171500     EJECT                                                                
171600                                                                          
171700                                                                          
171800 IMS-GNP-2502-OKVAL SECTION.                                              
171900     MOVE  'WL250111 ' TO SSA1                                            
172000     MOVE '  GEGB' TO GODK-STATUSKODER                                    
172100     CALL CBLTDLI USING GNP WDR2-PCB DLI-IO-AREA-1 SSA1                   
172200     MOVE WDR2-STATUS-CODE TO STATUS-WS                                   
172300     PERFORM IMS-STATUSKONTROLL                                           
172400     .                                                                    
172500     EJECT                                                                
172600                                                                          
172700                                                                          
172800 IMS-GHU-2502 SECTION.                                                    
172900     STRING 'WL250101(WDGXKEY  =' W-WDGXKEY-X ')'                         
173000          DELIMITED BY SIZE INTO SSA1                                     
173100     STRING 'WL250111(IDREFTAB =' W-IDREFTAB-X ')'                        
173200          DELIMITED BY SIZE INTO SSA2                                     
173300     MOVE '  ' TO GODK-STATUSKODER                                        
173400     CALL CBLTDLI USING GHU WDR2-PCB DLI-IO-AREA-1 SSA1 SSA2              
173500     MOVE WDR2-STATUS-CODE TO STATUS-WS                                   
173600     PERFORM IMS-STATUSKONTROLL                                           
173700     .                                                                    
173800     EJECT                                                                
173900                                                                          
174000                                                                          
174100 IMS-GU-2502-GAMMAL SECTION.                                              
174200     STRING 'WL250101(WDGXKEY  =' W-OLD-WDGXKEY-X ')'                     
174300          DELIMITED BY SIZE INTO SSA1                                     
174400     STRING 'WL250111(IDREFTAB =' W-OLD-IDREFTAB-X ')'                    
174500          DELIMITED BY SIZE INTO SSA2                                     
174600     MOVE '  GE' TO GODK-STATUSKODER                                      
174700     CALL CBLTDLI USING GU WDR2-PCB DLI-IO-AREA-2 SSA1 SSA2               
174800     MOVE WDR2-STATUS-CODE TO STATUS-WS                                   
174900     PERFORM IMS-STATUSKONTROLL                                           
175000     .                                                                    
175100     EJECT                                                                
175200                                                                          
175300                                                                          
175400 IMS-ISRT-2501 SECTION.                                                   
175500                                                                          
175600     MOVE 'WL250101 '          TO SSA1                                    
175700     MOVE '  ' TO GODK-STATUSKODER                                        
175800     CALL CBLTDLI USING ISRT WDR2-PCB DLI-IO-AREA-1 SSA1                  
175900     MOVE WDR2-STATUS-CODE TO STATUS-WS                                   
176000     PERFORM IMS-STATUSKONTROLL                                           
176100     .                                                                    
176200     EJECT                                                                
176300                                                                          
176400                                                                          
176500 IMS-ISRT-2502 SECTION.                                                   
176600                                                                          
176700     STRING 'WL250101(WDGXKEY  =' W-WDGXKEY-X ')'                         
176800          DELIMITED BY SIZE INTO SSA1                                     
176900     MOVE 'WL250111 '         TO SSA2                                     
177000     MOVE '  ' TO GODK-STATUSKODER                                        
177100     CALL CBLTDLI USING ISRT WDR2-PCB DLI-IO-AREA-1 SSA1 SSA2             
177200     MOVE WDR2-STATUS-CODE TO STATUS-WS                                   
177300     PERFORM IMS-STATUSKONTROLL                                           
177400     .                                                                    
177500     EJECT                                                                
177600                                                                          
177700                                                                          
177800 IMS-REPL-2502 SECTION.                                                   
177900                                                                          
178000     MOVE '  ' TO GODK-STATUSKODER                                        
178100     CALL CBLTDLI USING REPL WDR2-PCB DLI-IO-AREA-1                       
178200     MOVE WDR2-STATUS-CODE TO STATUS-WS                                   
178300     PERFORM IMS-STATUSKONTROLL                                           
178400     .                                                                    
178500     EJECT                                                                
178600                                                                          
178700                                                                          
178800 IMS-DLET-2501 SECTION.                                                   
178900                                                                          
179000     MOVE '  ' TO GODK-STATUSKODER                                        
179100     CALL CBLTDLI USING DLET WDR2-PCB DLI-IO-AREA-1                       
179200     MOVE WDR2-STATUS-CODE TO STATUS-WS                                   
179300     PERFORM IMS-STATUSKONTROLL                                           
179400     EJECT                                                                
179500     .                                                                    
179600                                                                          
179700                                                                          
179800 IMS-DLET-2502 SECTION.                                                   
179900                                                                          
180000     MOVE '  ' TO GODK-STATUSKODER                                        
180100     CALL CBLTDLI USING DLET WDR2-PCB DLI-IO-AREA-1                       
180200     MOVE WDR2-STATUS-CODE TO STATUS-WS                                   
180300     PERFORM IMS-STATUSKONTROLL                                           
180400     .                                                                    
180500     EJECT                                                                
180600                                                                          
180700 IMS-GU-WDB601    SECTION.                                                
180800     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
180900          DELIMITED BY SIZE INTO SSA1                                     
181000     MOVE '  GE' TO GODK-STATUSKODER                                      
181100     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B601 SSA1                 
181200     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
181300     PERFORM IMS-STATUSKONTROLL                                           
181400     IF SEGMENT-SAKNAS                                                    
181500        MOVE SPACE TO DCS-KDDC                                            
181600     END-IF                                                               
181700     .                                                                    
181800     EJECT                                                                
181900                                                                          
182100 IMS-STATUSKONTROLL SECTION.                                              
182200                                                                          
182300     SET STATUS-IX TO 1                                                   
182400     SEARCH GODK-STATUS                                                   
182500       AT END                                                             
182600         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
182700         DELIMITED BY SIZE INTO FELTEXT                                   
182800         CALL FELLOG                                                      
182900       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
183000         CONTINUE                                                         
183100     END-SEARCH                                                           
183200     .                                                                    
183300     EJECT                                                                
