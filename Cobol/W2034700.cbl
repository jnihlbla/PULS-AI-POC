000100 ID DIVISION.                                                             
000200                                                                          
000300 PROGRAM-ID.     W2034700.                                                
000400 AUTHOR.         STEFAN KIHLBERG.                                         
000500 DATE-WRITTEN.   95/08/31.                                                
000600 DATE-COMPILED.                                                           
000700                                                                          
000800*    FUNKTION:                                                            
000900*        VISAR/UPPDATERAR REFILLINGKVANT                                  
001000*                                                                         
001100*        PROGRAMMET UPPDATERAR WDGX25 (WDR2)                              
001200*                                                                         
001300*    INDATA.                                                              
001400*        TRANSAKTION: W2T347                                              
001500*        MID:         W2I34701                                            
001600*                                                                         
001700*    UTDATA.                                                              
001800*        MOD:         W2O34701                                            
001900                                                                          
002000     SKIP3                                                                
002100 ENVIRONMENT DIVISION.                                                    
002200     EJECT                                                                
002300 DATA DIVISION.                                                           
002400 WORKING-STORAGE SECTION.                                                 
002500                                                                          
002600*    -- CHECKED BY WY2000                                                 
002700 77  IDPGM                       PIC X(08)   VALUE 'W2034700'.            
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
004200 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
004300     88  NYCKLAR-OK                          VALUE 'J'.                   
004400     88  NYCKLAR-FEL                         VALUE 'N'.                   
004500                                                                          
004600 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
004700     88  EGEN-MID                            VALUE '2347'.                
004800     88  GODK-MID                            VALUE '2346' '2347'.         
004900     88  HELP-MID                            VALUE '0551'.                
005000     EJECT                                                                
005100*    --- ÖVRIGA ARBETSFÄLT                                                
005200 01  FILLER                      PIC X(16) VALUE 'ARBETSFALT'.            
005300 01  ARBETSFALT.                                                          
005400     03  WS-IDDC                 PIC X(2)    VALUE SPACE.                 
005500     03  WS-IDREFTAB             PIC X(1)    VALUE SPACE.                 
005600                                                                          
005900     03  WS-RAD                  PIC 9(1)    VALUE ZERO.                  
006000     03  WS-FL-SISTA-TABELL      PIC X(1)    VALUE 'N'.                   
006100                                                                          
006200     03  WS-INRAD.                                                        
006300         05 WS-KVANT-IN OCCURS 10.                                        
006400           07 WS-KVREFKVA           PIC 9(7).                             
006500           07 WS-KDREFPKT-KVA       PIC X(1).                             
006600                                                                          
006700     03  WS-ANTAL-TABELLER       PIC 9(9)    VALUE ZERO.                  
006800                                                                          
006900 01  FILLER                      PIC X(16)   VALUE 'INDEX'.               
007000 01  INDX.                                                                
007100     03  RAD-IX                  PIC 9(2)    VALUE ZERO.                  
007200     03  KOL-IX                  PIC 9(2)    VALUE ZERO.                  
007300     03  IX-1                    PIC 9(2)    VALUE ZERO.                  
007400     03  IX-2                    PIC 9(2)    VALUE ZERO.                  
007500*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
007600     EJECT                                                                
007700 01  GENERELLA-SUBPROGRAM.                                                
007800     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
007900     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
008000     03  WDECEDIT                PIC X(8)    VALUE 'WDECEDIT'.            
008100     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
008200     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
008300     EJECT                                                                
008400*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
008500*01 -COPY WMEDAREA                                                        
008600     SKIP3                                                                
008700 01  MESSAGE-CODES.                                                       
008800     03  ERR-CORR-HILITE-FLDS    PIC X(3)    VALUE '001'.                 
008900     03  INF-PRESS-PF11          PIC X(3)    VALUE '003'.                 
009000     03  ERR-PF11-AND-NO-DATA    PIC X(3)    VALUE '011'.                 
009100     03  INF-UPDATE-DONE         PIC X(3)    VALUE '101'.                 
009200     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
009900                                                                          
010000 01  FEL-MEDDELANDEN.                                                     
010100     03  FEL-14                  PIC X(40)                                
010200         VALUE 'SÖKT TABELL FINNS EJ                   '.                 
010300                                                                          
010400 01  OVR-MEDDELANDEN.                                                     
010500     03 MED-EX1.                                                          
010600        05 UT-ANTAL-TABELLER     PIC Z(2)9.                               
010700        05 UT-TABELLTEXT         PIC X(33).                               
010800        05 UT-NEXT-PAGE          PIC X(22) VALUE SPACE.                   
010900                                                                          
011000     03 TE-PF8-FLER-TABELLER  PIC X(33)                                   
011100                 VALUE ' REFILLTABELLER TILL DETTA DC.   '.               
011200     03 TE-PF8-EN-TABELL      PIC X(33)                                   
011300                 VALUE ' REFILLTABELL TILL DETTA DC.   '.                 
011400     03 TE-PF8-MORE              PIC X(22)                                
011500                 VALUE ' TRYCK PF8 FÖR FLERA. ' .                         
011600     03 TE-LAST-PAGE             PIC X(22)                                
011700                 VALUE ' SISTA TABELL.        ' .                         
011800                                                                          
011900                                                                          
012000     EJECT                                                                
012100*    --- PARAMETRAR TILL SUBPROGRAM W005INIT                              
012200*                                                                         
012300 01  FILLER                      PIC X(16)   VALUE 'WMSGINIT'.            
012400     SKIP3                                                                
012500*01 -COPY WMSGINIT                                                        
012600     SKIP3                                                                
012700*    --- PARAMETRAR TILL SUBPROGRAM WDECEDIT                              
012800*                                                                         
012900 01  FILLER                      PIC X(16)   VALUE 'WDECEDIT'.            
013000     SKIP3                                                                
013100*01 -COPY WDECAREA                                                        
013200     SKIP3                                                                
013300*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
013400*                                                                         
013500 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
013600     SKIP3                                                                
013700*01  MID -COPY W2I34701                                                   
013800     EJECT                                                                
013900 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
014000     SKIP3                                                                
014100*01  -COPY WMSGAREA                                                       
014200     EJECT                                                                
014300     03  MOD REDEFINES MSG-AREA.                                          
014400*      05  -COPY W2O34701                                                 
014500     EJECT                                                                
014600 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
014700     SKIP3                                                                
014800*01  -COPY WMFSAREA                                                       
014900     EJECT                                                                
015000*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
015100*                                                                         
015200     EJECT                                                                
015300 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
015400     SKIP3                                                                
015500*01  NYCKLAR-TILL-BLAEDDRING.                                             
015600     SKIP3                                                                
015700 01  NYCKLAR-TILL-DLI.                                                    
015800     03  W-WDGXKEY-X.                                                     
015900          05 W-IDHTYP            PIC X(4)    VALUE '2501'.                
016000          05 W-IDDC              PIC X(2)    VALUE SPACE.                 
016100          05 W-LOWVALUE          PIC X(24)   VALUE LOW-VALUE.             
016200                                                                          
016300     03  W-IDREFTAB-X.                                                    
016400         05  W-IDREFTAB          PIC X(1)    VALUE SPACE.                 
016500                                                                          
016600     03  W-IDDC-B6-X.                                                     
016700         05  W-IDDC-B6           PIC X(2)    VALUE SPACE.                 
016800                                                                          
016900     SKIP2                                                                
017000*    --- STATUS-KOD FRÅN IMS                                              
017100 01  STATUS-WS                   PIC XX.                                  
017200     88  SEGMENT-FINNS                       VALUE '  '.                  
017300     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
017400     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
017410     88  SEGMENT-SLUT                        VALUE 'GB'.                  
017500     SKIP2                                                                
017600 01  GODK-STATUSKODER.                                                    
017700     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
017800     SKIP3                                                                
017900 01  SSA1                        PIC X(64).                               
018000 01  SSA2                        PIC X(64).                               
018100     EJECT                                                                
018200*    --- IMS FUNKTIONSKODER                                               
018300*01  -COPY W0003                                                          
018400     EJECT                                                                
018500*    ---  DLI INPUT-OUTPUT AREA                                           
018600 01  FILLER                   PIC X(16)   VALUE 'DLI-IO-AREA-1'.          
018700     SKIP3                                                                
018800 01  DLI-IO-AREA-1.                                                       
018900     03  IO-AREA-1            PIC X(1500)  VALUE SPACE.                   
019000     SKIP3                                                                
019100     03  WDGX2501 REDEFINES IO-AREA-1.                                    
019200*        05  -COPY WDGX2501                                               
019300     SKIP3                                                                
019400     03  WDGX2502 REDEFINES IO-AREA-1.                                    
019500*        05  -COPY WDGX2502                                               
019600     EJECT                                                                
019700                                                                          
019800 01  FILLER               PIC X(16)   VALUE 'WDB601 AREA'.                
019900 01   DLI-IO-AREA-B601.                                                   
020000*     03  -COPY WDB601                                                    
020100                                                                          
020200                                                                          
020300     EJECT                                                                
020400 LINKAGE SECTION.                                                         
020500                                                                          
020600*01  -COPY W0009   -PRE MSG-                                              
020700*01  -COPY W0008   -PRE WDR2-                                             
020800     05  FILLER                  PIC X.                                   
020900     EJECT                                                                
021000*01  -COPY W0008      -PRE WDB6-                                          
021100     05  FILLER                  PIC X.                                   
021200     EJECT                                                                
021300 PROCEDURE DIVISION  USING MSG-PCB WDR2-PCB WDB6-PCB.                     
021400     ENTRY 'DLITCBL' USING MSG-PCB WDR2-PCB WDB6-PCB.                     
021500                                                                          
021600     PERFORM IMS-GET-MSG                                                  
021700     IF SEGMENT-FINNS                                                     
021800        PERFORM A-INIT                                                    
021900        PERFORM B-KOLLA-NYCKLAR                                           
022000        IF NYCKLAR-OK                                                     
022100           IF MFS-UPDATE                                                  
022200              PERFORM G-KOLLA-INPUT                                       
022300              IF INDATA-OK                                                
022400                 PERFORM H-UPPDATERA                                      
022500              END-IF                                                      
022600           ELSE                                                           
022700              IF MFS-FIRST                                                
022800                 PERFORM C-FIRST-LATEST-TABELL                            
022900              ELSE                                                        
023000                 IF MFS-NEXT                                              
023100                    PERFORM D-NEXT-TABELL                                 
023200                 ELSE                                                     
023300                    PERFORM E-SAMMA-TABELL                                
023400                 END-IF                                                   
023500              END-IF                                                      
023600           END-IF                                                         
023700           PERFORM F-LAES-VISA-INFO                                       
023800        END-IF                                                            
023900        PERFORM IMS-INSERT-MSG                                            
024000     END-IF                                                               
024100                                                                          
024200     MOVE ZERO TO RETURN-CODE                                             
024300     GOBACK                                                               
024400     .                                                                    
024500     EJECT                                                                
024600                                                                          
024700                                                                          
024800 A-INIT SECTION.                                                          
024900                                                                          
025000     IF MSG-DUBBLA-TRANSKODER                                             
025100       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W2I34701                 
025200       MOVE MSG-IDTRANS-2  TO MFS-IDTRANS                                 
025300       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
025400     ELSE                                                                 
025500       MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W2I34701                  
025600       MOVE MSG-IDTRANS-1  TO MFS-IDTRANS                                 
025700       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
025800     END-IF                                                               
025900                                                                          
026000     MOVE MSG-KDTRTYP TO MFS-KDTRTYP                                      
026100     MOVE MSG-IDPFK   TO MFS-IDPFK                                        
026200     MOVE MFS-IDTRANS TO W-IDTRANS                                        
026300                                                                          
026400     MOVE LOW-VALUE TO MSG-AREA                                           
026500     MOVE 'W2O34701' TO MFS-IDMOD                                         
026600     MOVE '2347' TO MOD-IDTRANS                                           
026700     MOVE MFS-RENSA-FAELT TO MOD-TEMFSFEL MOD-TEMFSINF                    
026800                                                                          
026900     COMPUTE MSG-KVLL = LENGTH OF MOD-W2O34701 + 4                        
027000                                                                          
027100     IF EGEN-MID OR HELP-MID                                              
027200       CONTINUE                                                           
027300     ELSE                                                                 
027400       MOVE SPACE TO MFS-KDTRTYP                                          
027500       MOVE '7' TO MFS-IDPFK                                              
027600       MOVE SPACE TO MID-IDREFTAB-FIRST                                   
027700                     MID-IDREFTAB-LAST                                    
027800     END-IF                                                               
027900     .                                                                    
028000     EJECT                                                                
028100 B-KOLLA-NYCKLAR SECTION.                                                 
028200                                                                          
029200     MOVE JA TO NYCKLAR-SW                                                
029300                                                                          
029400                                                                          
029500*    -- KONTROLL AV IDDC                                                  
029600     MOVE MFS-RENSA-FAELT TO MOD-IDDC-IN                                  
029700                                                                          
029800     IF MID-IDDC-IN     = ALL '+'                                         
029900       MOVE MID-IDDC-UT TO WS-IDDC                                        
030000     ELSE                                                                 
030100       MOVE MID-IDDC-IN TO WS-IDDC                                        
030200       MOVE '7'         TO MFS-IDPFK                                      
030300       MOVE SPACE       TO MFS-KDTRTYP                                    
030400       MOVE SPACE       TO MID-IDREFTAB-FIRST                             
030500                           MID-IDREFTAB-LAST                              
030600     END-IF                                                               
030700                                                                          
030800     MOVE WS-IDDC       TO W-IDDC-B6                                      
030900     PERFORM IMS-GU-WDB601                                                
031000     IF SEGMENT-FINNS                                                     
031100       CONTINUE                                                           
031300     ELSE                                                                 
031400       MOVE NEJ              TO NYCKLAR-SW                                
031500     END-IF                                                               
031600*                                                                         
032400*    -- KONTROLL AV IDREFTAB                                              
032500     MOVE MFS-RENSA-FAELT TO MOD-IDREFTAB-IN                              
032600                                                                          
032700     IF MID-IDREFTAB-IN  = ALL '+'                                        
032800       MOVE MID-IDREFTAB-UT TO WS-IDREFTAB                                
032900       INSPECT WS-IDREFTAB REPLACING LEADING SPACE BY ZERO                
033000     ELSE                                                                 
033100       MOVE MID-IDREFTAB-IN TO WS-IDREFTAB                                
033200       MOVE '7'             TO MFS-IDPFK                                  
033300       MOVE SPACE           TO MFS-KDTRTYP                                
033400       MOVE SPACE           TO MID-IDREFTAB-FIRST                         
033500                               MID-IDREFTAB-LAST                          
033600     END-IF                                                               
033700                                                                          
033800     IF WS-IDREFTAB NUMERIC                                               
033810        CONTINUE                                                          
033820     ELSE                                                                 
033830        MOVE NEJ            TO NYCKLAR-SW                                 
033840     END-IF                                                               
033850                                                                          
033900     IF GODK-MID OR NYCKLAR-OK                                            
034000       MOVE WS-IDDC            TO MOD-IDDC-UT                             
034100       MOVE WS-IDREFTAB        TO MOD-IDREFTAB-UT                         
034400     ELSE                                                                 
034500       MOVE MFS-RENSA-FAELT TO MOD-IDDC-UT                                
034600       MOVE MFS-RENSA-FAELT TO MOD-IDREFTAB-UT                            
034800     END-IF                                                               
034900                                                                          
035000                                                                          
035100     IF NYCKLAR-FEL                                                       
035200       MOVE ERR-WRONG-KEY TO MED-IDMFSFEL                                 
035300       CALL WMEDKONV USING MED-WMEDAREA                                   
035400       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
035500       PERFORM MFS-RENSA-FAELT-IN                                         
035600       PERFORM MFS-RENSA-FAELT-UT                                         
035700     END-IF                                                               
035800     .                                                                    
035900     EJECT                                                                
036000                                                                          
038900 C-FIRST-LATEST-TABELL SECTION.                                           
039000                                                                          
039100                                                                          
039200     MOVE WS-IDDC      TO W-IDDC                                          
039300     MOVE WS-IDREFTAB  TO W-IDREFTAB                                      
039400                                                                          
039500     IF MID-IDREFTAB-FIRST = SPACE AND                                    
039600        MID-IDREFTAB-LAST = SPACE                                         
039700*       PF7 (ENTER) NYA NYCKLAR                                           
039800        MOVE WS-IDREFTAB TO W-IDREFTAB                                    
039900     ELSE                                                                 
040000        IF MID-IDREFTAB-FIRST > SPACE AND                                 
040100           MID-IDREFTAB-LAST > SPACE                                      
040200*          PF7 BLÄDDRA BAKÅT FÖRSTA GÅNG                                  
040300           MOVE MID-IDREFTAB-FIRST TO W-IDREFTAB                          
040400        ELSE                                                              
040500           IF MID-IDREFTAB-FIRST = SPACE AND                              
040600              MID-IDREFTAB-LAST > SPACE                                   
040700*             PF7 BLÄDDRA BAKÅT GÅNG 2                                    
040800              MOVE MID-IDREFTAB-FIRST TO W-IDREFTAB                       
040900           ELSE                                                           
041000              MOVE WS-IDREFTAB    TO W-IDREFTAB                           
041100           END-IF                                                         
041200        END-IF                                                            
041300     END-IF                                                               
041400     PERFORM MFS-RENSA-FAELT-IN                                           
041500     .                                                                    
041600     EJECT                                                                
041700                                                                          
041800                                                                          
041900 D-NEXT-TABELL SECTION.                                                   
042000                                                                          
042100                                                                          
042200     MOVE WS-IDDC      TO W-IDDC                                          
042300     MOVE MID-IDREFTAB-LAST TO W-IDREFTAB                                 
042400     PERFORM MFS-RENSA-FAELT-IN                                           
042500     .                                                                    
042600     EJECT                                                                
042700                                                                          
042800                                                                          
042900                                                                          
043000 E-SAMMA-TABELL SECTION.                                                  
043100                                                                          
043200     MOVE WS-IDDC      TO W-IDDC                                          
043300     MOVE WS-IDREFTAB  TO W-IDREFTAB                                      
043400                                                                          
043500     IF MID-INPUT = ALL '+'                                               
043600       PERFORM MFS-RENSA-FAELT-IN                                         
043700     ELSE                                                                 
043800       MOVE INF-PRESS-PF11 TO MED-IDMFSFEL                                
043900       CALL WMEDKONV USING MED-WMEDAREA                                   
044000       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
044100       PERFORM MFS-LAES-IN-IGEN                                           
044200       PERFORM S34-MID-INDATA-TILL-MOD                                    
044300     END-IF                                                               
044400     .                                                                    
044500     EJECT                                                                
044600                                                                          
044700                                                                          
044800 F-LAES-VISA-INFO SECTION.                                                
044900                                                                          
045000     IF MFS-NEXT                                                          
045100        PERFORM FA-NEXT                                                   
045200     ELSE                                                                 
045300        IF MFS-ENTER                                                      
045400           PERFORM FB-ENTER-UPDATE                                        
045500        ELSE                                                              
045600           PERFORM FC-FIRST                                               
045700        END-IF                                                            
045800     END-IF                                                               
045900     .                                                                    
046000     EJECT                                                                
046100                                                                          
046200                                                                          
046300 FA-NEXT SECTION.                                                         
046400                                                                          
046500     PERFORM IMS-GU-2501                                                  
046600     PERFORM IMS-GNP-2502-NEXT                                            
046700     IF SEGMENT-FINNS                                                     
046710       IF 2502-IDREFTAB NUMERIC                                           
046800        MOVE MID-IDREFTAB-LAST TO MOD-IDREFTAB-FIRST                      
046900        MOVE 2502-IDREFTAB     TO MOD-IDREFTAB-LAST                       
047000                                  MOD-IDREFTAB-UT                         
047100                                  W-IDREFTAB                              
047200        PERFORM FX-VISA-INFO                                              
047300        PERFORM IMS-GNP-2502-NEXT                                         
047400        IF SEGMENT-FINNS AND 2502-IDREFTAB NUMERIC                        
047500           CONTINUE                                                       
047600        ELSE                                                              
047700           MOVE JA             TO WS-FL-SISTA-TABELL                      
047800        END-IF                                                            
047900        PERFORM FY-HUR-MANGA-TABELLER                                     
047910       END-IF                                                             
048000     ELSE                                                                 
048100        PERFORM IMS-GU-2501                                               
048200        PERFORM IMS-GNP-2502-OKVAL                                        
048210        PERFORM UNTIL SEGMENT-SAKNAS OR SEGMENT-SLUT                      
048220          IF SEGMENT-FINNS AND 2502-IDREFTAB NUMERIC                      
048300            MOVE 2502-IDREFTAB TO MOD-IDREFTAB-FIRST                      
048400                                  MOD-IDREFTAB-LAST                       
048500                                  MOD-IDREFTAB-UT                         
048600            PERFORM FX-VISA-INFO                                          
048700            PERFORM FY-HUR-MANGA-TABELLER                                 
048701          END-IF                                                          
048702        PERFORM IMS-GNP-2502-OKVAL                                        
048710        END-PERFORM                                                       
048800     END-IF                                                               
048900     .                                                                    
049000     EJECT                                                                
049100                                                                          
049200                                                                          
049300 FB-ENTER-UPDATE SECTION.                                                 
049400                                                                          
049500     PERFORM IMS-GU-2502                                                  
049600     IF MFS-UPDATE                                                        
049700        PERFORM IMS-GU-2502                                               
049800        IF SEGMENT-FINNS                                                  
049900           MOVE 2502-IDREFTAB TO MOD-IDREFTAB-FIRST                       
050000                                MOD-IDREFTAB-LAST                         
050100                                MOD-IDREFTAB-UT                           
050200           PERFORM FX-VISA-INFO                                           
050300           PERFORM FY-HUR-MANGA-TABELLER                                  
050400        ELSE                                                              
050500           PERFORM FZ-VISA-SEGMENT-SAKNAS                                 
050600        END-IF                                                            
050700     ELSE                                                                 
050800        IF SEGMENT-FINNS                                                  
050900           MOVE 2502-IDREFTAB    TO MOD-IDREFTAB-UT                       
051000           PERFORM FX-VISA-INFO                                           
051100           PERFORM FY-HUR-MANGA-TABELLER                                  
051200        ELSE                                                              
051300           PERFORM FZ-VISA-SEGMENT-SAKNAS                                 
051400        END-IF                                                            
051500        MOVE MID-IDREFTAB-FIRST TO MOD-IDREFTAB-FIRST                     
051600        MOVE MID-IDREFTAB-LAST TO MOD-IDREFTAB-LAST                       
051700     END-IF                                                               
051800                                                                          
051900     .                                                                    
052000     EJECT                                                                
052100                                                                          
052200                                                                          
052300 FC-FIRST SECTION.                                                        
052400                                                                          
052500     IF MID-IDREFTAB-FIRST = SPACE AND                                    
052600        MID-IDREFTAB-LAST = SPACE                                         
052700*       ENTER(PF7) NYA NYCKLAR                                            
052800        PERFORM IMS-GU-2502                                               
052900        IF SEGMENT-FINNS                                                  
053000           MOVE 2502-IDREFTAB TO MOD-IDREFTAB-FIRST                       
053100                                MOD-IDREFTAB-LAST                         
053200                                MOD-IDREFTAB-UT                           
053300           PERFORM FX-VISA-INFO                                           
053400           PERFORM FY-HUR-MANGA-TABELLER                                  
053500        ELSE                                                              
053600           PERFORM FZ-VISA-SEGMENT-SAKNAS                                 
053700        END-IF                                                            
053800     ELSE                                                                 
053900        IF MID-IDREFTAB-FIRST > SPACE AND                                 
054000           MID-IDREFTAB-LAST > SPACE                                      
054100*          PF7 GÅNG 1,                                                    
054200           PERFORM IMS-GU-2502                                            
054300           IF SEGMENT-FINNS                                               
054400              MOVE SPACE        TO MOD-IDREFTAB-FIRST                     
054500              MOVE 2502-IDREFTAB TO MOD-IDREFTAB-LAST                     
054600                                   MOD-IDREFTAB-UT                        
054700              PERFORM FX-VISA-INFO                                        
054800              PERFORM FY-HUR-MANGA-TABELLER                               
054900           ELSE                                                           
055000              PERFORM FZ-VISA-SEGMENT-SAKNAS                              
055100           END-IF                                                         
055200        ELSE                                                              
055300           IF MID-IDREFTAB-FIRST = SPACE AND                              
055400              MID-IDREFTAB-LAST > SPACE                                   
055500*             PF7 GÅNG 2                                                  
055600              PERFORM IMS-GU-2501                                         
055700              PERFORM IMS-GNP-2502-OKVAL                                  
055710              PERFORM UNTIL SEGMENT-SAKNAS OR SEGMENT-SLUT                
055800                IF SEGMENT-FINNS AND 2502-IDREFTAB NUMERIC                
055900                   MOVE 2502-IDREFTAB TO MOD-IDREFTAB-FIRST               
056000                                         MOD-IDREFTAB-LAST                
056100                                         MOD-IDREFTAB-UT                  
056200                   PERFORM FX-VISA-INFO                                   
056300                   PERFORM FY-HUR-MANGA-TABELLER                          
056310                   ADD +1  TO IX-1                                        
056320                END-IF                                                    
056330                PERFORM IMS-GNP-2502-OKVAL                                
056340              END-PERFORM                                                 
056350              IF IX-1 = 0                                                 
056500                 PERFORM FZ-VISA-SEGMENT-SAKNAS                           
056600              END-IF                                                      
056700           END-IF                                                         
056800        END-IF                                                            
056900     END-IF                                                               
057000     .                                                                    
057100     EJECT                                                                
057200                                                                          
057300                                                                          
057400 FX-VISA-INFO SECTION.                                                    
057500*                                                                         
058500     MOVE 2502-TEREFLIM       TO MOD-TEREFLIM                             
058600                                                                          
058700     MOVE +1 TO KOL-IX                                                    
058800     PERFORM UNTIL KOL-IX > 8                                             
058900        MOVE 2502-KVPB-REF(KOL-IX) TO MOD-KVPB-REF(KOL-IX)                
059000        ADD +1 TO KOL-IX                                                  
059100     END-PERFORM                                                          
059200                                                                          
059300     MOVE +1 TO RAD-IX                                                    
059400     PERFORM UNTIL RAD-IX > 10                                            
059500        MOVE 2502-PRARTBES(RAD-IX) TO MOD-PRARTBES(RAD-IX)                
059600        MOVE +1 TO KOL-IX                                                 
059700        PERFORM UNTIL KOL-IX > 8                                          
059800           MOVE 2502-KVREFKVA(RAD-IX, KOL-IX) TO                          
059900                     MOD-KVREFKVA(RAD-IX, KOL-IX)                         
060000           MOVE 2502-KDREFPKT-KVA(RAD-IX, KOL-IX) TO                      
060100                     MOD-KDREFPKT-KVA(RAD-IX, KOL-IX)                     
060200           ADD +1  TO KOL-IX                                              
060300        END-PERFORM                                                       
060400        ADD +1 TO RAD-IX                                                  
060500     END-PERFORM                                                          
060600     PERFORM MFS-RENSA-FAELT-IN                                           
060700     .                                                                    
060800     EJECT                                                                
060900                                                                          
061000                                                                          
061100 FY-HUR-MANGA-TABELLER SECTION.                                           
061200                                                                          
061300     PERFORM IMS-GU-2501                                                  
061400     MOVE +0 TO WS-ANTAL-TABELLER                                         
061500     PERFORM IMS-GNP-2502                                                 
061600     PERFORM UNTIL SEGMENT-SAKNAS                                         
061610      IF 2502-IDREFTAB NUMERIC                                            
061700        ADD +1 TO WS-ANTAL-TABELLER                                       
061810      END-IF                                                              
061820      PERFORM IMS-GNP-2502                                                
061900     END-PERFORM                                                          
062000                                                                          
062100     MOVE WS-ANTAL-TABELLER TO UT-ANTAL-TABELLER                          
062200     IF WS-ANTAL-TABELLER = 1                                             
062300        MOVE TE-PF8-EN-TABELL     TO UT-TABELLTEXT                        
062400        MOVE TE-LAST-PAGE      TO UT-NEXT-PAGE                            
062500     ELSE                                                                 
062600        MOVE TE-PF8-FLER-TABELLER TO UT-TABELLTEXT                        
062700        IF WS-FL-SISTA-TABELL = NEJ                                       
062800           MOVE TE-PF8-MORE       TO UT-NEXT-PAGE                         
062900        ELSE                                                              
063000           MOVE TE-LAST-PAGE      TO UT-NEXT-PAGE                         
063100        END-IF                                                            
063200     END-IF                                                               
063300     MOVE MED-EX1 TO MOD-TEMFSINF                                         
063400     .                                                                    
063500     EJECT                                                                
063600                                                                          
063700                                                                          
063800 FZ-VISA-SEGMENT-SAKNAS SECTION.                                          
063900                                                                          
064000     MOVE FEL-14 TO MOD-TEMFSFEL                                          
064100     .                                                                    
064200     EJECT                                                                
064300                                                                          
064400                                                                          
064500 G-KOLLA-INPUT SECTION.                                                   
064600                                                                          
064700     MOVE WS-IDDC     TO W-IDDC                                           
064800     MOVE WS-IDREFTAB TO W-IDREFTAB                                       
064900                                                                          
065000     PERFORM GA-KOLLA-INPUT-ANDRING                                       
065100     .                                                                    
065200     EJECT                                                                
065300                                                                          
065400                                                                          
065500 GA-KOLLA-INPUT-ANDRING SECTION.                                          
065600                                                                          
065700     IF MID-INPUT = ALL '+'                                               
065800        MOVE ERR-PF11-AND-NO-DATA TO MED-IDMFSFEL                         
065900        CALL WMEDKONV USING MED-WMEDAREA                                  
066000        MOVE MED-TEMFSFEL TO MOD-TEMFSFEL                                 
066100        PERFORM MFS-RENSA-FAELT-IN                                        
066200        PERFORM MFS-RENSA-FAELT-UT                                        
066300        MOVE MID-IDREFTAB-FIRST TO MOD-IDREFTAB-FIRST                     
066400        MOVE MID-IDREFTAB-LAST TO MOD-IDREFTAB-LAST                       
066500        MOVE NEJ TO INDATA-SW                                             
066600     ELSE                                                                 
066700        PERFORM IMS-GU-2502                                               
066800        IF SEGMENT-FINNS                                                  
066900           PERFORM S31-INDATA-KOLL                                        
067000           IF INDATA-FEL                                                  
067100              MOVE NEJ TO INDATA-SW                                       
067200              MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL                   
067300              CALL WMEDKONV USING MED-WMEDAREA                            
067400              MOVE MED-TEMFSFEL TO MOD-TEMFSFEL                           
067500              MOVE MID-IDREFTAB-FIRST TO MOD-IDREFTAB-FIRST               
067600              MOVE MID-IDREFTAB-LAST TO MOD-IDREFTAB-LAST                 
067700              PERFORM MFS-ROER-EJ-FAELT-UT                                
067800              PERFORM MFS-ROER-EJ-FAELT-IN                                
067900           END-IF                                                         
068000        ELSE                                                              
068100           MOVE NEJ TO INDATA-SW                                          
068200           MOVE FEL-14 TO MOD-TEMFSFEL                                    
068300           MOVE MID-IDREFTAB-FIRST TO MOD-IDREFTAB-FIRST                  
068400           MOVE MID-IDREFTAB-LAST TO MOD-IDREFTAB-LAST                    
068500           PERFORM MFS-ROER-EJ-FAELT-IN                                   
068600           PERFORM MFS-ROER-EJ-FAELT-UT                                   
068700        END-IF                                                            
068800     END-IF                                                               
068900     .                                                                    
069000     EJECT                                                                
069100                                                                          
069200                                                                          
069300 H-UPPDATERA SECTION.                                                     
069400                                                                          
069500     PERFORM HA-ANDRA-TABELL                                              
069600     .                                                                    
069700     EJECT                                                                
069800                                                                          
069900                                                                          
070000 HA-ANDRA-TABELL SECTION.                                                 
070100                                                                          
070200     PERFORM IMS-GHU-2502                                                 
070300                                                                          
070400     IF MID-TEREFLIM = ALL '+'                                            
070500        CONTINUE                                                          
070600     ELSE                                                                 
070700        IF MID-TEREFLIM = SPACE                                           
070800           MOVE SPACE TO 2502-TEREFLIM                                    
070900        ELSE                                                              
071000           MOVE MID-TEREFLIM TO 2502-TEREFLIM                             
071100        END-IF                                                            
071200     END-IF                                                               
071300                                                                          
071400     MOVE +1 TO KOL-IX                                                    
071500     MOVE WS-RAD TO RAD-IX                                                
071600     MOVE +1 TO KOL-IX                                                    
071700     PERFORM UNTIL KOL-IX > 8                                             
071800        IF MID-KVREFKVA(KOL-IX) NOT = ALL '+'                             
071900           MOVE WS-KVREFKVA(KOL-IX) TO                                    
072000                2502-KVREFKVA(RAD-IX, KOL-IX)                             
072100        END-IF                                                            
072200        IF MID-KDREFPKT-KVA(KOL-IX) NOT = ALL '+'                         
072300           MOVE WS-KDREFPKT-KVA(KOL-IX) TO                                
072400                2502-KDREFPKT-KVA(RAD-IX, KOL-IX)                         
072500        END-IF                                                            
072600        ADD +1 TO KOL-IX                                                  
072700     END-PERFORM                                                          
072800                                                                          
072900     PERFORM IMS-REPL-2502                                                
073000                                                                          
073100     MOVE INF-UPDATE-DONE TO MED-IDMFSINF                                 
073200     CALL WMEDKONV USING MED-WMEDAREA                                     
073300     MOVE MED-MFSINF TO MOD-TEMFSINF                                      
073400     PERFORM MFS-FORM-ATTR                                                
073500     PERFORM MFS-RENSA-FAELT-IN                                           
073600     .                                                                    
073700     EJECT                                                                
073800                                                                          
073900                                                                          
074000 S31-INDATA-KOLL SECTION.                                                 
074100                                                                          
074200*KOLLA TEXTRAD                                                            
074300                                                                          
074400     IF MID-TEREFLIM = ALL '+' OR SPACE                                   
074500        MOVE MFS-ALFA-FAELT-RAETT  TO MOD-TEREFLIM-ATTR                   
074600     ELSE                                                                 
074700        MOVE MFS-ALFA-FAELT-RAETT  TO MOD-TEREFLIM-ATTR                   
074800     END-IF                                                               
074900                                                                          
075000                                                                          
075100*RAD                                                                      
075200                                                                          
075300     IF MID-RAD NOT = ALL '+'                                             
075400        IF MID-RAD NUMERIC                                                
075500           IF MID-RAD > 0 AND <= 9                                        
075600              MOVE MID-RAD TO WS-RAD                                      
075700              MOVE MFS-NUM-FAELT-RAETT  TO                                
075800                           MOD-RAD-IN-ATTR                                
075900           ELSE                                                           
076000              MOVE NEJ TO  INDATA-SW                                      
076100              MOVE MFS-NUM-FAELT-FEL TO MOD-RAD-IN-ATTR                   
076200           END-IF                                                         
076300        ELSE                                                              
076400           MOVE NEJ TO  INDATA-SW                                         
076500           MOVE MFS-NUM-FAELT-FEL TO MOD-RAD-IN-ATTR                      
076600        END-IF                                                            
076700     ELSE                                                                 
076800           MOVE MFS-NUM-FAELT-RAETT TO MOD-RAD-IN-ATTR                    
076900     END-IF                                                               
077000                                                                          
077100                                                                          
077200*KOLLA PRIS-PUNKTRAD                                                      
077300                                                                          
077400     PERFORM S31A-FINNS-RADANGIVELSE                                      
077500                                                                          
077600*KVREFKVA, KDREFPKT-KVA                                                   
077700                                                                          
077800     MOVE +1 TO KOL-IX                                                    
077900     PERFORM UNTIL KOL-IX > 8                                             
078000        IF MID-KVREFKVA(KOL-IX) NOT = ALL '+'                             
078100           IF MID-KVREFKVA(KOL-IX) NUMERIC                                
078200              MOVE MID-KVREFKVA(KOL-IX) TO WS-KVREFKVA(KOL-IX)            
078300              MOVE MFS-NUM-FAELT-RAETT  TO                                
078400                           MOD-KVREFKVA-IN-ATTR(KOL-IX)                   
078500           ELSE                                                           
078600              MOVE NEJ TO INDATA-SW                                       
078700              MOVE MFS-NUM-FAELT-FEL TO                                   
078800                           MOD-KVREFKVA-IN-ATTR(KOL-IX)                   
078900           END-IF                                                         
079000        ELSE                                                              
079100           MOVE MFS-NUM-FAELT-RAETT TO                                    
079200                        MOD-KVREFKVA-IN-ATTR(KOL-IX)                      
079300        END-IF                                                            
079400                                                                          
079500        IF MID-KDREFPKT-KVA(KOL-IX) NOT = ALL '+'                         
079600           IF MID-KDREFPKT-KVA(KOL-IX) = 'S' OR 'D'                       
079700              MOVE MID-KDREFPKT-KVA(KOL-IX) TO                            
079800                                      WS-KDREFPKT-KVA(KOL-IX)             
079900              MOVE MFS-ALFA-FAELT-RAETT  TO                               
080000                           MOD-KDREFPKT-KVA-IN-ATTR(KOL-IX)               
080100           ELSE                                                           
080200              MOVE NEJ TO INDATA-SW                                       
080300              MOVE MFS-ALFA-FAELT-FEL TO                                  
080400                           MOD-KDREFPKT-KVA-IN-ATTR(KOL-IX)               
080500           END-IF                                                         
080600        ELSE                                                              
080700           MOVE MFS-ALFA-FAELT-RAETT TO                                   
080800                        MOD-KDREFPKT-KVA-IN-ATTR(KOL-IX)                  
080900        END-IF                                                            
081000        ADD +1 TO KOL-IX                                                  
081100     END-PERFORM                                                          
081200     .                                                                    
081300     EJECT                                                                
081400                                                                          
081500                                                                          
081600 S31A-FINNS-RADANGIVELSE SECTION.                                         
081700                                                                          
081800     IF MID-RAD = ALL '+'                                                 
081900        MOVE +1 TO KOL-IX                                                 
082000        PERFORM UNTIL KOL-IX > 8                                          
082100           IF MID-KVREFKVA(KOL-IX) NOT = ALL '+'                          
082200           OR MID-KDREFPKT-KVA(KOL-IX) NOT = ALL '+'                      
082300              MOVE NEJ TO INDATA-SW                                       
082400              MOVE MFS-NUM-FAELT-FEL TO MOD-RAD-IN-ATTR                   
082500           END-IF                                                         
082600           ADD +1 TO KOL-IX                                               
082700        END-PERFORM                                                       
082800     END-IF                                                               
082900                                                                          
083000     IF MID-RAD NOT = ALL '+'                                             
083100                                                                          
083200        IF MID-KVREFKVA(1)    = ALL '+' AND                               
083300           MID-KVREFKVA(2)    = ALL '+' AND                               
083400           MID-KVREFKVA(3)    = ALL '+' AND                               
083500           MID-KVREFKVA(4)    = ALL '+' AND                               
083600           MID-KVREFKVA(5)    = ALL '+' AND                               
083700           MID-KVREFKVA(6)    = ALL '+' AND                               
083800           MID-KVREFKVA(7)    = ALL '+' AND                               
083900           MID-KVREFKVA(8)    = ALL '+' AND                               
084000                                                                          
084100           MID-KDREFPKT-KVA(1) = ALL '+' AND                              
084200           MID-KDREFPKT-KVA(2) = ALL '+' AND                              
084300           MID-KDREFPKT-KVA(3) = ALL '+' AND                              
084400           MID-KDREFPKT-KVA(4) = ALL '+' AND                              
084500           MID-KDREFPKT-KVA(5) = ALL '+' AND                              
084600           MID-KDREFPKT-KVA(6) = ALL '+' AND                              
084700           MID-KDREFPKT-KVA(7) = ALL '+' AND                              
084800           MID-KDREFPKT-KVA(8) = ALL '+'                                  
084900           MOVE NEJ TO INDATA-SW                                          
085000           MOVE MFS-NUM-FAELT-FEL  TO MOD-RAD-IN                          
085100        END-IF                                                            
085200                                                                          
085300     END-IF                                                               
085400     .                                                                    
085500     EJECT                                                                
085600                                                                          
085700                                                                          
085800                                                                          
085900 S34-MID-INDATA-TILL-MOD SECTION.                                         
086000                                                                          
086100                                                                          
086200* TEREFLIM                                                                
086300                                                                          
086400     IF MID-TEREFLIM = ALL '+'                                            
086500        MOVE MFS-RENSA-FAELT         TO MOD-TEREFLIM                      
086600     ELSE                                                                 
086700        MOVE MID-TEREFLIM            TO MOD-TEREFLIM                      
086800     END-IF                                                               
086900                                                                          
087000                                                                          
087100* PRIS-RAD                                                                
087200                                                                          
087300     IF MID-RAD = ALL '+'                                                 
087400        MOVE MFS-RENSA-FAELT      TO MOD-RAD-IN                           
087500     ELSE                                                                 
087600        MOVE MFS-ROER-EJ-FAELT    TO MOD-RAD-IN                           
087700     END-IF                                                               
087800                                                                          
087900     MOVE +1    TO KOL-IX                                                 
088000     PERFORM UNTIL KOL-IX > 8                                             
088100        IF MID-KVREFKVA(KOL-IX) = ALL '+'                                 
088200           MOVE MFS-RENSA-FAELT      TO MOD-KVREFKVA-IN(KOL-IX)           
088300        ELSE                                                              
088400           MOVE MFS-ROER-EJ-FAELT    TO MOD-KVREFKVA-IN(KOL-IX)           
088500        END-IF                                                            
088600        IF MID-KDREFPKT-KVA(KOL-IX) = ALL '+'                             
088700           MOVE MFS-RENSA-FAELT   TO MOD-KDREFPKT-KVA-IN(KOL-IX)          
088800        ELSE                                                              
088900           MOVE MFS-ROER-EJ-FAELT TO MOD-KDREFPKT-KVA-IN(KOL-IX)          
089000        END-IF                                                            
089100        ADD +1 TO KOL-IX                                                  
089200     END-PERFORM                                                          
089300     .                                                                    
089400     EJECT                                                                
089500                                                                          
089600                                                                          
089700 MFS-RENSA-FAELT-UT SECTION.                                              
089800                                                                          
089900     MOVE MFS-RENSA-FAELT TO MOD-TEREFLIM                                 
090000     MOVE +1 TO KOL-IX                                                    
090100     PERFORM UNTIL KOL-IX > 8                                             
090200        MOVE MFS-RENSA-FAELT    TO MOD-KVPB-REF(KOL-IX)                   
090300        ADD +1 TO KOL-IX                                                  
090400     END-PERFORM                                                          
090500                                                                          
090600     MOVE +1 TO RAD-IX                                                    
090700     PERFORM UNTIL RAD-IX > 10                                            
090800        MOVE MFS-RENSA-FAELT    TO MOD-PRARTBES(RAD-IX)                   
090900        MOVE +1 TO KOL-IX                                                 
091000        PERFORM UNTIL KOL-IX > 8                                          
091100           MOVE MFS-RENSA-FAELT TO                                        
091200                               MOD-KVREFKVA(RAD-IX, KOL-IX)               
091300                               MOD-KDREFPKT-KVA(RAD-IX, KOL-IX)           
091400           ADD +1 1 TO KOL-IX                                             
091500        END-PERFORM                                                       
091600        ADD +1 TO RAD-IX                                                  
091700     END-PERFORM                                                          
091800     .                                                                    
091900     EJECT                                                                
092000                                                                          
092100                                                                          
092200 MFS-RENSA-FAELT-IN SECTION.                                              
092300                                                                          
092400*    MOVE MFS-RENSA-FAELT TO MOD-TEREFLIM                                 
092500                                                                          
092600     MOVE +1 TO KOL-IX                                                    
092700     MOVE MFS-RENSA-FAELT TO MOD-RAD-IN                                   
092800                                                                          
092900     MOVE +1 TO KOL-IX                                                    
093000     PERFORM UNTIL KOL-IX > 8                                             
093100        MOVE MFS-RENSA-FAELT TO MOD-KVREFKVA-IN(KOL-IX)                   
093200                                MOD-KDREFPKT-KVA-IN(KOL-IX)               
093300        ADD +1 TO KOL-IX                                                  
093400     END-PERFORM                                                          
093500     .                                                                    
093600     EJECT                                                                
093700                                                                          
093800                                                                          
093900 MFS-ROER-EJ-FAELT-UT  SECTION.                                           
094000                                                                          
094100     MOVE MFS-ROER-EJ-FAELT TO MOD-TEREFLIM                               
094200                                                                          
094300     MOVE +1 TO KOL-IX                                                    
094400     PERFORM UNTIL KOL-IX > 8                                             
094500        MOVE MFS-ROER-EJ-FAELT    TO MOD-KVPB-REF(KOL-IX)                 
094600        ADD +1 1 TO KOL-IX                                                
094700     END-PERFORM                                                          
094800                                                                          
094900     MOVE +1 TO RAD-IX                                                    
095000     PERFORM UNTIL RAD-IX > 10                                            
095100        MOVE MFS-ROER-EJ-FAELT    TO MOD-PRARTBES(RAD-IX)                 
095200        MOVE +1 TO KOL-IX                                                 
095300        PERFORM UNTIL KOL-IX > 8                                          
095400           MOVE MFS-ROER-EJ-FAELT TO                                      
095500                                  MOD-KVREFKVA(RAD-IX, KOL-IX)            
095600                                  MOD-KDREFPKT-KVA(RAD-IX, KOL-IX)        
095700           ADD +1 1 TO KOL-IX                                             
095800        END-PERFORM                                                       
095900        ADD +1 TO RAD-IX                                                  
096000     END-PERFORM                                                          
096100     .                                                                    
096200     EJECT                                                                
096300                                                                          
096400                                                                          
096500 MFS-ROER-EJ-FAELT-IN  SECTION.                                           
096600                                                                          
096700     MOVE MFS-ROER-EJ-FAELT    TO MOD-TEREFLIM                            
096800                                                                          
096900     MOVE MFS-ROER-EJ-FAELT TO MOD-RAD-IN                                 
097000                                                                          
097100     MOVE +1 TO KOL-IX                                                    
097200     PERFORM UNTIL KOL-IX > 8                                             
097300        MOVE MFS-ROER-EJ-FAELT TO MOD-KVREFKVA-IN(KOL-IX)                 
097400                                  MOD-KDREFPKT-KVA-IN(KOL-IX)             
097500        ADD +1 TO KOL-IX                                                  
097600     END-PERFORM                                                          
097700     .                                                                    
097800     EJECT                                                                
097900                                                                          
098000                                                                          
098100 MFS-FORM-ATTR SECTION.                                                   
098200                                                                          
098300     MOVE MFS-FORMATETS-ATTR    TO MOD-TEREFLIM                           
098400                                                                          
098500     MOVE MFS-FORMATETS-ATTR TO MOD-RAD-IN-ATTR                           
098600                                                                          
098700     MOVE +1 TO KOL-IX                                                    
098800     PERFORM UNTIL KOL-IX > 8                                             
098900        MOVE MFS-FORMATETS-ATTR TO                                        
099000                                MOD-KVREFKVA-IN-ATTR(KOL-IX)              
099100                                MOD-KDREFPKT-KVA-IN-ATTR(KOL-IX)          
099200        ADD +1 TO KOL-IX                                                  
099300     END-PERFORM                                                          
099400     .                                                                    
099500     EJECT                                                                
099600                                                                          
099700                                                                          
099800 MFS-LAES-IN-IGEN SECTION.                                                
099900                                                                          
100000     MOVE MFS-ADD-LAES-IN-FAELT TO MOD-TEREFLIM-ATTR                      
100100                                                                          
100200     MOVE MFS-ADD-LAES-IN-FAELT    TO MOD-RAD-IN-ATTR                     
100300     MOVE +1 TO KOL-IX                                                    
100400     PERFORM UNTIL KOL-IX > 8                                             
100500        MOVE MFS-ADD-LAES-IN-FAELT TO                                     
100600                                  MOD-KVREFKVA-IN-ATTR(KOL-IX)            
100700                                  MOD-KDREFPKT-KVA-IN-ATTR(KOL-IX)        
100800        ADD +1 TO KOL-IX                                                  
100900     END-PERFORM                                                          
101000     .                                                                    
101100     EJECT                                                                
101200                                                                          
101300                                                                          
101400* --- IMS SEKTIONER ---                                                   
101500     SKIP3                                                                
101600 IMS-GET-MSG SECTION.                                                     
101700                                                                          
101800     MOVE '  QC' TO GODK-STATUSKODER                                      
101900     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
102000     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
102100     PERFORM IMS-STATUSKONTROLL                                           
102200     .                                                                    
102300     SKIP3                                                                
102400 IMS-INSERT-MSG SECTION.                                                  
102500                                                                          
102600     IF ENGLISH-TEXT                                                      
102700       MOVE 'N' TO MFS-KDHUVOMR                                           
102800     END-IF                                                               
102900     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
103000     MOVE SPACE TO GODK-STATUSKODER                                       
103100     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
103200     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
103300     PERFORM IMS-STATUSKONTROLL                                           
103400     .                                                                    
103500     EJECT                                                                
103600                                                                          
103700                                                                          
103800 IMS-GU-2502 SECTION.                                                     
103900     STRING 'WL250101(WDGXKEY  =' W-WDGXKEY-X ')'                         
104000          DELIMITED BY SIZE INTO SSA1                                     
104100     STRING 'WL250111(IDREFTAB =' W-IDREFTAB-X ')'                        
104200          DELIMITED BY SIZE INTO SSA2                                     
104300     MOVE '  GE' TO GODK-STATUSKODER                                      
104400     CALL CBLTDLI USING GU WDR2-PCB DLI-IO-AREA-1 SSA1 SSA2               
104500     MOVE WDR2-STATUS-CODE TO STATUS-WS                                   
104600     PERFORM IMS-STATUSKONTROLL                                           
104700     .                                                                    
104800     EJECT                                                                
104900                                                                          
105000                                                                          
105100 IMS-GNP-2502-NEXT SECTION.                                               
105200     STRING 'WL250111(IDREFTAB >' W-IDREFTAB-X ')'                        
105300          DELIMITED BY SIZE INTO SSA1                                     
105400     MOVE '  GEGB' TO GODK-STATUSKODER                                    
105500     CALL CBLTDLI USING GNP WDR2-PCB DLI-IO-AREA-1 SSA1                   
105600     MOVE WDR2-STATUS-CODE TO STATUS-WS                                   
105700     PERFORM IMS-STATUSKONTROLL                                           
105800     .                                                                    
105900     EJECT                                                                
106000                                                                          
106100                                                                          
106200 IMS-GU-2501 SECTION.                                                     
106300     STRING 'WL250101(WDGXKEY  =' W-WDGXKEY-X ')'                         
106400          DELIMITED BY SIZE INTO SSA1                                     
106500     MOVE '  GE' TO GODK-STATUSKODER                                      
106600     CALL CBLTDLI USING GU WDR2-PCB DLI-IO-AREA-1 SSA1                    
106700     MOVE WDR2-STATUS-CODE TO STATUS-WS                                   
106800     PERFORM IMS-STATUSKONTROLL                                           
106900     .                                                                    
107000     EJECT                                                                
107100                                                                          
107200                                                                          
107300 IMS-GNP-2502 SECTION.                                                    
107400     MOVE  'WL250111 ' TO SSA1                                            
107500     MOVE '  GEGB' TO GODK-STATUSKODER                                    
107600     CALL CBLTDLI USING GNP WDR2-PCB DLI-IO-AREA-1 SSA1                   
107700     MOVE WDR2-STATUS-CODE TO STATUS-WS                                   
107800     PERFORM IMS-STATUSKONTROLL                                           
107900     .                                                                    
108000     EJECT                                                                
108100                                                                          
108200                                                                          
108300 IMS-GHU-2501 SECTION.                                                    
108400     STRING 'WL250101(WDGXKEY  =' W-WDGXKEY-X ')'                         
108500          DELIMITED BY SIZE INTO SSA1                                     
108600     MOVE '  GE' TO GODK-STATUSKODER                                      
108700     CALL CBLTDLI USING GHU WDR2-PCB DLI-IO-AREA-1 SSA1                   
108800     MOVE WDR2-STATUS-CODE TO STATUS-WS                                   
108900     PERFORM IMS-STATUSKONTROLL                                           
109000     .                                                                    
109100     EJECT                                                                
109200                                                                          
109300                                                                          
109400 IMS-GNP-2502-OKVAL SECTION.                                              
109500     MOVE  'WL250111 ' TO SSA1                                            
109600     MOVE '  GEGB' TO GODK-STATUSKODER                                    
109700     CALL CBLTDLI USING GNP WDR2-PCB DLI-IO-AREA-1 SSA1                   
109800     MOVE WDR2-STATUS-CODE TO STATUS-WS                                   
109900     PERFORM IMS-STATUSKONTROLL                                           
110000     .                                                                    
110100     EJECT                                                                
110200                                                                          
110300                                                                          
110400 IMS-GHU-2502 SECTION.                                                    
110500     STRING 'WL250101(WDGXKEY  =' W-WDGXKEY-X ')'                         
110600          DELIMITED BY SIZE INTO SSA1                                     
110700     STRING 'WL250111(IDREFTAB =' W-IDREFTAB-X ')'                        
110800          DELIMITED BY SIZE INTO SSA2                                     
110900     MOVE '  ' TO GODK-STATUSKODER                                        
111000     CALL CBLTDLI USING GHU WDR2-PCB DLI-IO-AREA-1 SSA1 SSA2              
111100     MOVE WDR2-STATUS-CODE TO STATUS-WS                                   
111200     PERFORM IMS-STATUSKONTROLL                                           
111300     .                                                                    
111400     EJECT                                                                
111500                                                                          
111600                                                                          
111700 IMS-REPL-2502 SECTION.                                                   
111800                                                                          
111900     MOVE '  ' TO GODK-STATUSKODER                                        
112000     CALL CBLTDLI USING REPL WDR2-PCB DLI-IO-AREA-1                       
112100     MOVE WDR2-STATUS-CODE TO STATUS-WS                                   
112200     PERFORM IMS-STATUSKONTROLL                                           
112300     .                                                                    
112400     EJECT                                                                
112500                                                                          
112600 IMS-GU-WDB601    SECTION.                                                
112700     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
112800          DELIMITED BY SIZE INTO SSA1                                     
112900     MOVE '  GE' TO GODK-STATUSKODER                                      
113000     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B601 SSA1                 
113100     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
113200     PERFORM IMS-STATUSKONTROLL                                           
113300     .                                                                    
113400     EJECT                                                                
113500                                                                          
113600                                                                          
113700 IMS-STATUSKONTROLL SECTION.                                              
113800                                                                          
113900     SET STATUS-IX TO 1                                                   
114000     SEARCH GODK-STATUS                                                   
114100       AT END                                                             
114200         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
114300         DELIMITED BY SIZE INTO FELTEXT                                   
114400         CALL FELLOG                                                      
114500       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
114600         CONTINUE                                                         
114700     END-SEARCH                                                           
114800     .                                                                    
114900     EJECT                                                                
