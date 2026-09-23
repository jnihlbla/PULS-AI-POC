000100 ID DIVISION.                                                             
000200                                                                          
000300 PROGRAM-ID.     W2045600.                                                
000400 AUTHOR.         MARIE ODSELL.                                            
000500 DATE-WRITTEN.   12/11/29.                                                
000600 DATE-COMPILED.                                                           
000700                                                                          
000800*    FUNKTION:                                                            
000900*        VISAR/UPPDATERAR REFILLINGPUNKT (= KVSLAGER FÖR ANSK. )          
001000*        COPY OF SCREEN W20346                                            
001100*                                                                         
001200*        PROGRAMMET UPPDATERAR WDGX25 (WDR2)                              
001300*                                                                         
001400*    INDATA.                                                              
001500*        TRANSAKTION: W2T456                                              
001600*        MID:         W2I45601                                            
001700*                                                                         
001800*    UTDATA.                                                              
001900*        MOD:         W2O45601                                            
002000                                                                          
002100     SKIP3                                                                
002200 ENVIRONMENT DIVISION.                                                    
002300     EJECT                                                                
002400 DATA DIVISION.                                                           
002500 WORKING-STORAGE SECTION.                                                 
002600                                                                          
002700*    -- CHECKED BY WY2000                                                 
002800 77  IDPGM                       PIC X(08)   VALUE 'W2045600'.            
002900                                                                          
003000*    --- WORKFIELDS FOR MESSAGES AND CALL TO ABEND/FELLOG                 
003100 77  ERROR-TEXT                  PIC X(80) VALUE SPACE.                   
003200                                                                          
003300 77  JA                          PIC X       VALUE 'J'.                   
003400 77  YES                         PIC X       VALUE 'Y'.                   
003500 77  NOO                         PIC X       VALUE 'N'.                   
003600 77  MAX-MOD-LAENGD              PIC S9(4) VALUE ZERO COMP SYNC.          
003700                                                                          
003800*    --- WORKFIELDS FOR KEY VALUE FROM SCREEN                             
003900                                                                          
004000 77  INDATA-SW                   PIC X       VALUE 'Y'.                   
004100     88  INDATA-OK                           VALUE 'Y'.                   
004200     88  INDATA-WRONG                        VALUE 'N'.                   
004300                                                                          
004400 77  COPY-DATA-SW                PIC X       VALUE 'Y'.                   
004500     88  COPY-DATA-OK                        VALUE 'Y'.                   
004600     88  COPY-DATA-WRONG                     VALUE 'N'.                   
004700                                                                          
004800 77  KEYS-SW                     PIC X       VALUE 'Y'.                   
004900     88  KEYS-OK                             VALUE 'Y'.                   
005000     88  KEYS-WRONG                          VALUE 'N'.                   
005100                                                                          
005200 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
005300     88  OWN-MID                             VALUE '2456'.                
005400     88  GOOD-MID                            VALUE '2456' '2457'.         
005500     88  HELP-MID                            VALUE '0551'.                
005600     EJECT                                                                
005700*    --- WORKFIELDS                                                       
005800 01  FILLER                      PIC X(16) VALUE 'ARBETSFALT'.            
005900 01  ARBETSFALT.                                                          
006000     03  WS-IDREFTAB             PIC X(1)    VALUE SPACE.                 
006100                                                                          
006200                                                                          
006300     03  WS-COPY-IDDC            PIC X(2)    VALUE SPACE.                 
006400     03  WS-COPY-IDREFTAB        PIC X(1)    VALUE ZERO.                  
006500                                                                          
006600     03  WS-RAD                  PIC 9(1)    VALUE ZERO.                  
006700     03  WS-FL-LAST-TABLE        PIC X(1)    VALUE 'N'.                   
006800                                                                          
006900     03  WS-KVPB-REF-INRAD.                                               
007000         05 WS-KVPB-REF-IN OCCURS 8.                                      
007100           07 WS-KVPB-REF          PIC S9(6)V9(1).                        
007200                                                                          
007300     03  JMF-KVPB-REF-INRAD.                                              
007400         05 JMF-KVPB-REF-IN OCCURS 8.                                     
007500           07 JMF-KVPB-REF          PIC S9(6)V9(1).                       
007600                                                                          
007700     03  WS-INRAD.                                                        
007800         05 WS-PRARTBES            PIC S9(7)V9(2).                        
007900         05 WS-PUNKT-IN OCCURS 10.                                        
008000           07 WS-KVREFLIM           PIC 9(7).                             
008100           07 WS-KDREFPKT-LIM       PIC X(1).                             
008200                                                                          
008300     03  WS-NUMBER-OF-TABLES     PIC 9(9)    VALUE ZERO.                  
008400                                                                          
008500 77  CREATE-NEW-TABLE-SW         PIC X       VALUE 'N'.                   
008600     88  CREATE-NEW-TABLE                    VALUE 'Y'.                   
008700                                                                          
008800 01  FILLER                      PIC X(16)   VALUE 'INDEX'.               
008900 01  INDX.                                                                
009000     03  RAD-IX                  PIC 9(2)    VALUE ZERO.                  
009100     03  KOL-IX                  PIC 9(1)    VALUE ZERO.                  
009200     03  IX1                     PIC 9(2)    VALUE ZERO.                  
009300     03  IX2                     PIC 9(2)    VALUE ZERO.                  
009310     EJECT                                                                
009400*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
009600 01  GENERELLA-SUBPROGRAM.                                                
009700     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
009800     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
009900     03  WDECEDIT                PIC X(8)    VALUE 'WDECEDIT'.            
010000     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
010100     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
010200     EJECT                                                                
010300*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
010400*01 -COPY WMEDAREA                                                        
010500     SKIP3                                                                
010600 01  MESSAGE-CODES.                                                       
010700     03  ERR-CORR-HILITE-FLDS    PIC X(3)    VALUE '001'.                 
010800     03  INF-PRESS-PF11          PIC X(3)    VALUE '003'.                 
010900     03  ERR-PF11-AND-NO-DATA    PIC X(3)    VALUE '011'.                 
011000     03  INF-UPDATE-DONE         PIC X(3)    VALUE '101'.                 
011100     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
011110     03  ERR-USER-NOT-AUTH       PIC X(3)    VALUE '405'.                 
011200                                                                          
011300 01  FEL-MEDDELANDEN.                                                     
011400     03  FEL-11                  PIC X(40)                                
011500         VALUE 'NEW TABLE ID NAME NOT CORRECT          '.                 
011600     03  FEL-12                  PIC X(40)                                
011700         VALUE 'NEW TABLE ID ALREADY EXIST             '.                 
011800     03  FEL-13                  PIC X(40)                                
011900         VALUE 'OLD TABLE ID DO NOT EXIST              '.                 
012000     03  FEL-14                  PIC X(40)                                
012100         VALUE 'TABLE ID DO NOT EXIST                  '.                 
012200     03  FEL-15                  PIC X(40)                                
012300         VALUE 'INPUT VALUE WHEN DELETE                '.                 
012400     03  FEL-16                  PIC X(40)                                
012500         VALUE 'NOT ALLOWED TO DELETE TABLE ID         '.                 
012600                                                                          
012700 01  OVR-MEDDELANDEN.                                                     
012800     03 MED-EX1.                                                          
012900        05 UT-ANTAL-TABELLER     PIC Z(2)9.                               
013000        05 UT-TABELLTEXT         PIC X(33).                               
013100        05 UT-NEXT-PAGE          PIC X(22) VALUE SPACE.                   
013200                                                                          
013300     03 TE-PF8-FLER-TABELLER  PIC X(33)                                   
013400                 VALUE ' PROCUREMENT TABLES TO THIS DC.  '.               
013500     03 TE-PF8-EN-TABELL      PIC X(33)                                   
013600                 VALUE ' PROCUREMENT TABLE TO THIS DC.   '.               
013700     03 TE-PF8-MORE              PIC X(33)                                
013800                 VALUE ' PRESS PF8 FOR MORE INFO.        '.               
013900     03 TE-LAST-PAGE             PIC X(33)                                
014000                 VALUE ' LAST TABLE.                     '.               
014100                                                                          
014200     03 MED-1                    PIC X(40)                                
014300         VALUE 'TABLE ID DELETED                       '.                 
014400                                                                          
014500     EJECT                                                                
014501 01  FILLER                      PIC X(7)    VALUE 'WWIDFTG'.             
014502*01 -COPY WWIDFTG                                                         
014510     EJECT                                                                
014600*    --- PARAMETRAR TILL SUBPROGRAM W005INIT                              
014700*                                                                         
014800 01  FILLER                      PIC X(16)   VALUE 'WMSGINIT'.            
014900     SKIP3                                                                
015000*01 -COPY WMSGINIT                                                        
015100     SKIP3                                                                
015200*    --- PARAMETRAR TILL SUBPROGRAM WDECEDIT                              
015300*                                                                         
015400 01  FILLER                      PIC X(16)   VALUE 'WDECEDIT'.            
015500     SKIP3                                                                
015600*01 -COPY WDECAREA                                                        
015700     SKIP3                                                                
015800*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
015900*                                                                         
016000 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
016100     SKIP3                                                                
016200*01  MID -COPY W2I45601                                                   
016300     EJECT                                                                
016400 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
016500     SKIP3                                                                
016600*01  -COPY WMSGAREA                                                       
016700     EJECT                                                                
016800     03  MOD REDEFINES MSG-AREA.                                          
016900*      05  -COPY W2O45601                                                 
017000     EJECT                                                                
017100 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
017200     SKIP3                                                                
017300*01  -COPY WMFSAREA                                                       
017400     EJECT                                                                
017500*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
017600     EJECT                                                                
017700 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
017800     SKIP3                                                                
017900*01  NYCKLAR-TILL-BLAEDDRING.                                             
018000     SKIP3                                                                
018100 01  NYCKLAR-TILL-DLI.                                                    
018200     03  W-WDGXKEY-X.                                                     
018300          05 W-IDHTYP            PIC X(4)    VALUE '2501'.                
018400          05 W-IDDC              PIC X(2)    VALUE SPACE.                 
018500          05 W-LOWVALUE          PIC X(24)   VALUE LOW-VALUE.             
018600                                                                          
018700     03  W-IDREFTAB-X.                                                    
018800         05  W-IDREFTAB          PIC X(1)    VALUE SPACE.                 
018900                                                                          
019000     03  W-OLD-WDGXKEY-X.                                                 
019100          05 W-OLD-IDHTYP        PIC X(4)    VALUE '2501'.                
019200          05 W-OLD-IDDC          PIC X(2)    VALUE SPACE.                 
019300          05 W-OLD-LOWVALUE      PIC X(24)   VALUE LOW-VALUE.             
019400                                                                          
019500     03  W-OLD-IDREFTAB-X.                                                
019600         05  W-OLD-IDREFTAB      PIC X(1)    VALUE SPACE.                 
019700                                                                          
019800     03  W-IDDC-B6-X.                                                     
019900         05  W-IDDC-B6           PIC X(2)    VALUE SPACE.                 
020000                                                                          
020100     SKIP2                                                                
020200*    --- STATUS-CODES FROM IMS                                            
020300 01  STATUS-WS                   PIC XX.                                  
020400     88  SEGMENT-FOUND                       VALUE '  '.                  
020500     88  SEGMENT-FOUND-EXISTS                VALUE 'II'.                  
020600     88  SEGMENT-MISSING                     VALUE 'GE'.                  
020700     88  SEGMENT-END                         VALUE 'GB'.                  
020800     SKIP2                                                                
020900 01  GOOD-STATUSCODES.                                                    
021000     03  GOOD-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
021100     SKIP3                                                                
021200 01  SSA1                        PIC X(64).                               
021300 01  SSA2                        PIC X(64).                               
021400     EJECT                                                                
021500*    --- IMS FUNCTION CODES                                               
021600*01  -COPY W0003                                                          
021700     EJECT                                                                
021800*    ---  DLI INPUT-OUTPUT AREA                                           
021900 01  FILLER                   PIC X(16)   VALUE 'DLI-IO-AREA-1'.          
022000     SKIP3                                                                
022100 01  DLI-IO-AREA-1.                                                       
022200     03  IO-AREA-1            PIC X(1500)  VALUE SPACE.                   
022300     SKIP3                                                                
022400     03  WDGX2501 REDEFINES IO-AREA-1.                                    
022500*        05  -COPY WDGX2501                                               
022600     SKIP3                                                                
022700     03  WDGX2502 REDEFINES IO-AREA-1.                                    
022800*        05  -COPY WDGX2502                                               
022900                                                                          
023000 01  FILLER                   PIC X(16)   VALUE 'DLI-IO-AREA-2'.          
023100     SKIP3                                                                
023200 01  DLI-IO-AREA-2.                                                       
023300     03  IO-AREA-2            PIC X(1500)  VALUE SPACE.                   
023400     SKIP3                                                                
023500     03  WDGX2501 REDEFINES IO-AREA-2.                                    
023600*        05  -COPY WDGX2501 -PRE OLD-                                     
023700     SKIP3                                                                
023800     03  WDGX2502 REDEFINES IO-AREA-2.                                    
023900*        05  -COPY WDGX2502 -PRE OLD-                                     
024000     EJECT                                                                
024100                                                                          
024200 01  FILLER               PIC X(16)   VALUE 'WDB601 AREA'.                
024300 01   DLI-IO-AREA-B601.                                                   
024400*     03  -COPY WDB601                                                    
024500 01  FILLER               PIC X(16)   VALUE 'WDB616 AREA'.                
024600 01   DLI-IO-AREA-B616.                                                   
024700*     03  -COPY WDB616                                                    
024800     EJECT                                                                
024900 LINKAGE SECTION.                                                         
025000                                                                          
025100*01  -COPY W0009   -PRE MSG-                                              
025110*01  -COPY W0008   -PRE WDP7-                                             
025120     05  FILLER                  PIC X.                                   
025200     EJECT                                                                
025300*01  -COPY W0008   -PRE WDR2-                                             
025400     05  FILLER                  PIC X.                                   
025500     EJECT                                                                
025600*01  -COPY W0008      -PRE WDB6-                                          
025700     05  FILLER                  PIC X.                                   
025800     EJECT                                                                
025900 PROCEDURE DIVISION  USING MSG-PCB WDP7-PCB WDR2-PCB WDB6-PCB.            
026000     ENTRY 'DLITCBL' USING MSG-PCB WDP7-PCB WDR2-PCB WDB6-PCB.            
026100                                                                          
026200     PERFORM IMS-GET-MSG                                                  
026300     IF SEGMENT-FOUND                                                     
026400        PERFORM A-INIT                                                    
026500        PERFORM B-CHECK-KEYS                                              
026600        IF KEYS-OK                                                        
026700           IF MID-KDCMD-INSERT                                            
026710              PERFORM S10-AUTH-USER-CHECK                                 
026720              IF INDATA-OK                                                
026800                PERFORM I-CREATE-NEW-TABLE                                
026810              END-IF                                                      
026900           END-IF                                                         
027000           IF COPY-DATA-OK                                                
027100              IF MFS-UPDATE                                               
027110                 PERFORM S10-AUTH-USER-CHECK                              
027120                 IF INDATA-OK                                             
027200                   PERFORM G-CHECK-INPUT                                  
027300                   IF INDATA-OK                                           
027400                      PERFORM H-UPDATE                                    
027500                   END-IF                                                 
027510                 END-IF                                                   
027600              ELSE                                                        
027700                 IF MFS-FIRST                                             
027800                    PERFORM C-FIRST-LATEST-TABELL                         
027900                 ELSE                                                     
028000                    IF MFS-NEXT                                           
028100                       PERFORM D-NEXT-TABLE                               
028200                    ELSE                                                  
028300                       PERFORM E-SAME-TABLE                               
028400                    END-IF                                                
028500                 END-IF                                                   
028600              END-IF                                                      
028700              PERFORM F-READ-SHOW-INFO                                    
028800           ELSE                                                           
028900              PERFORM J-SHOW-WRONG-WHEN-COPY                              
029000           END-IF                                                         
029100        END-IF                                                            
029200        PERFORM IMS-INSERT-MSG                                            
029300     END-IF                                                               
029400                                                                          
029500     MOVE ZERO TO RETURN-CODE                                             
029600     GOBACK                                                               
029700     .                                                                    
029800     EJECT                                                                
029900                                                                          
030000                                                                          
030100 A-INIT SECTION.                                                          
030200                                                                          
030300     IF MSG-DUBBLA-TRANSKODER                                             
030400       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W2I45601                 
030500       MOVE MSG-IDTRANS-2  TO MFS-IDTRANS                                 
030600       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
030700     ELSE                                                                 
030800       MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W2I45601                  
030900       MOVE MSG-IDTRANS-1  TO MFS-IDTRANS                                 
031000       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
031100     END-IF                                                               
031200                                                                          
031300     MOVE MSG-KDTRTYP TO MFS-KDTRTYP                                      
031400     MOVE MSG-IDPFK   TO MFS-IDPFK                                        
031500     MOVE MFS-IDTRANS TO W-IDTRANS                                        
031600                                                                          
031700     MOVE LOW-VALUE TO MSG-AREA                                           
031800     MOVE 'W2O456N1' TO MFS-IDMOD                                         
031900     MOVE '2456' TO MOD-IDTRANS                                           
032000     MOVE MFS-ERASE-FIELD TO MOD-TEMFSFEL MOD-TEMFSINF                    
032100                                                                          
032200     COMPUTE MSG-KVLL = LENGTH OF MOD-W2O45601 + 4                        
032300                                                                          
032400     IF OWN-MID OR HELP-MID                                               
032500       CONTINUE                                                           
032600     ELSE                                                                 
032700       MOVE SPACE TO MFS-KDTRTYP                                          
032800       MOVE '7' TO MFS-IDPFK                                              
032900       MOVE SPACE TO MID-IDREFTAB-FIRST                                   
033000                     MID-IDREFTAB-LAST                                    
033100     END-IF                                                               
033200     .                                                                    
033300     EJECT                                                                
033400 B-CHECK-KEYS SECTION.                                                    
033500                                                                          
033501     MOVE ALL '+'           TO MSGI-WMSGINIT                              
033502     MOVE '001'             TO MSGI-KDCALL                                
033503     MOVE MSG-LTERM-NAME    TO MSGI-IDLTERM-USER                          
033504     MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                                
033505     MOVE '2456'            TO MSGI-IDTRANS                               
033506     IF OWN-MID                                                           
033508       MOVE MID-IDDC-IN     TO MSGI-IDDC-KEY                              
033509     END-IF                                                               
033510     CALL W005INIT USING MSGI-WMSGINIT WDP7-PCB                           
033511                                                                          
033520                                                                          
033600     MOVE YES TO KEYS-SW                                                  
033700                                                                          
033800*    -- CHECK IDDC                                                        
033900     MOVE MFS-ERASE-FIELD TO MOD-IDDC-IN                                  
034000                                                                          
034100     IF MID-IDDC-IN     = ALL '+'                                         
034200       MOVE MID-IDDC-UT TO W-IDDC-B6                                      
034300     ELSE                                                                 
034400       MOVE MID-IDDC-IN TO W-IDDC-B6                                      
034500       MOVE '7'         TO MFS-IDPFK                                      
034600       MOVE SPACE       TO MFS-KDTRTYP                                    
034700       MOVE SPACE       TO MID-IDREFTAB-FIRST                             
034800                           MID-IDREFTAB-LAST                              
034900     END-IF                                                               
035000                                                                          
035100     PERFORM IMS-GU-WDB601                                                
035200     IF SEGMENT-MISSING                                                   
035300       MOVE NOO TO KEYS-SW                                                
035400     ELSE                                                                 
035500       IF DCS-NDC-CN OR (DCS-NDC-NA AND DCS-USA)                          
035600         CONTINUE                                                         
035700       ELSE                                                               
035800         MOVE NOO            TO KEYS-SW                                   
035900       END-IF                                                             
036000     END-IF                                                               
036100                                                                          
036200*    -- CHECK IDREFTAB                                                    
036300     MOVE MFS-ERASE-FIELD TO MOD-IDREFTAB-IN                              
036400                                                                          
036500     IF MID-IDREFTAB-IN  = ALL '+'                                        
036600       MOVE MID-IDREFTAB-UT TO WS-IDREFTAB                                
036700     ELSE                                                                 
036800       MOVE MID-IDREFTAB-IN TO WS-IDREFTAB                                
036900       MOVE '7'             TO MFS-IDPFK                                  
037000       MOVE SPACE           TO MFS-KDTRTYP                                
037100       MOVE SPACE           TO MID-IDREFTAB-FIRST                         
037200                               MID-IDREFTAB-LAST                          
037300     END-IF                                                               
037400                                                                          
037500     IF WS-IDREFTAB ALPHABETIC                                            
037600        CONTINUE                                                          
037700     ELSE                                                                 
037800        MOVE NOO            TO KEYS-SW                                    
037900     END-IF                                                               
038000                                                                          
038100                                                                          
038200*UPDATE IN NEW TABLE                                                      
038300     IF KEYS-OK                                                           
038400       IF MID-KDCMD-INSERT               AND                              
038500          (MID-IDDC-IN NOT = ALL '+'     OR                               
038600          MID-IDREFTAB-IN NOT = ALL '+') AND                              
038700          MFS-IDPFK = '7'                AND                              
038800          MFS-KDTRTYP = ' '                                               
038900          MOVE SPACE         TO MFS-IDPFK                                 
039000          MOVE 'U'           TO MFS-KDTRTYP                               
039100       END-IF                                                             
039200     END-IF                                                               
039300                                                                          
039400     IF GOOD-MID OR KEYS-OK                                               
039500       MOVE DCS-IDDC           TO MOD-IDDC-UT                             
039600       MOVE WS-IDREFTAB        TO MOD-IDREFTAB-UT                         
039700     ELSE                                                                 
039800       MOVE MFS-ERASE-FIELD TO MOD-IDDC-UT                                
039900       MOVE MFS-ERASE-FIELD TO MOD-IDREFTAB-UT                            
040000     END-IF                                                               
040100                                                                          
040200                                                                          
040300     IF KEYS-WRONG                                                        
040400       MOVE ERR-WRONG-KEY TO MED-IDMFSFEL                                 
040500       CALL WMEDKONV USING MED-WMEDAREA                                   
040600       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
040700       PERFORM MFS-ERASE-FIELD-IN                                         
040800       PERFORM MFS-ERASE-FIELD-OUT                                        
040900     END-IF                                                               
041000     .                                                                    
041100     EJECT                                                                
041200                                                                          
041300 C-FIRST-LATEST-TABELL SECTION.                                           
041400                                                                          
041500     MOVE DCS-IDDC     TO W-IDDC                                          
041600     MOVE WS-IDREFTAB  TO W-IDREFTAB                                      
041700                                                                          
041800     IF MID-IDREFTAB-FIRST = SPACE AND                                    
041900        MID-IDREFTAB-LAST = SPACE                                         
042000*       PF7 (ENTER) NEW KEYS                                              
042100        MOVE WS-IDREFTAB TO W-IDREFTAB                                    
042200     ELSE                                                                 
042300        IF MID-IDREFTAB-FIRST > SPACE AND                                 
042400           MID-IDREFTAB-LAST > SPACE                                      
042500*          PF7 FIRST PAGE BACK                                            
042600           MOVE MID-IDREFTAB-FIRST TO W-IDREFTAB                          
042700        ELSE                                                              
042800           IF MID-IDREFTAB-FIRST = SPACE AND                              
042900              MID-IDREFTAB-LAST > SPACE                                   
043000*             PF7 SECOND PAGE BACK                                        
043100              MOVE MID-IDREFTAB-FIRST TO W-IDREFTAB                       
043200           ELSE                                                           
043300              MOVE WS-IDREFTAB    TO W-IDREFTAB                           
043400           END-IF                                                         
043500        END-IF                                                            
043600     END-IF                                                               
043700     PERFORM MFS-ERASE-FIELD-IN                                           
043800     .                                                                    
043900     EJECT                                                                
044000                                                                          
044100                                                                          
044200 D-NEXT-TABLE SECTION.                                                    
044300                                                                          
044400     MOVE DCS-IDDC     TO W-IDDC                                          
044500     MOVE MID-IDREFTAB-LAST TO W-IDREFTAB                                 
044600     PERFORM MFS-ERASE-FIELD-IN                                           
044700     .                                                                    
044800     EJECT                                                                
044900                                                                          
045000 E-SAME-TABLE SECTION.                                                    
045100                                                                          
045200     MOVE DCS-IDDC     TO W-IDDC                                          
045300     MOVE WS-IDREFTAB  TO W-IDREFTAB                                      
045400                                                                          
045500     IF MID-INPUT = ALL '+'                                               
045600       PERFORM MFS-ERASE-FIELD-IN                                         
045700     ELSE                                                                 
045800       MOVE INF-PRESS-PF11 TO MED-IDMFSFEL                                
045900       CALL WMEDKONV USING MED-WMEDAREA                                   
046000       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
046100       PERFORM MFS-READ-IN-AGAIN                                          
046200       PERFORM S45-MID-INDATA-TO-MOD                                      
046300     END-IF                                                               
046400     .                                                                    
046500     EJECT                                                                
046600                                                                          
046700                                                                          
046800 F-READ-SHOW-INFO SECTION.                                                
046900                                                                          
047000     IF MFS-NEXT                                                          
047100        PERFORM FA-NEXT                                                   
047200     ELSE                                                                 
047300        IF MFS-ENTER                                                      
047400           PERFORM FB-ENTER-UPDATE                                        
047500        ELSE                                                              
047600           PERFORM FC-FIRST                                               
047700        END-IF                                                            
047800     END-IF                                                               
047900     .                                                                    
048000     EJECT                                                                
048100                                                                          
048200                                                                          
048300 FA-NEXT SECTION.                                                         
048400                                                                          
048500     PERFORM IMS-GU-2501                                                  
048600     PERFORM IMS-GNP-2502-NEXT                                            
048700     IF SEGMENT-FOUND                                                     
048800       IF 2502-IDREFTAB ALPHABETIC                                        
048900         MOVE MID-IDREFTAB-LAST TO MOD-IDREFTAB-FIRST                     
049000         MOVE 2502-IDREFTAB     TO MOD-IDREFTAB-LAST                      
049100                                   MOD-IDREFTAB-UT                        
049200                                   W-IDREFTAB                             
049300         PERFORM FX-SHOW-INFO                                             
049400         PERFORM IMS-GNP-2502-NEXT                                        
049500         IF SEGMENT-FOUND AND 2502-IDREFTAB ALPHABETIC                    
049600            CONTINUE                                                      
049700         ELSE                                                             
049800            MOVE YES TO WS-FL-LAST-TABLE                                  
049900         END-IF                                                           
050000         PERFORM FY-NUMBER-OF-TABLES                                      
050100       END-IF                                                             
050200     ELSE                                                                 
050300        PERFORM IMS-GU-2501                                               
050400        PERFORM IMS-GNP-2502-OKVAL                                        
050500        PERFORM UNTIL SEGMENT-MISSING OR SEGMENT-END                      
050600          IF SEGMENT-FOUND AND 2502-IDREFTAB ALPHABETIC                   
050700            MOVE 2502-IDREFTAB TO MOD-IDREFTAB-FIRST                      
050800                                  MOD-IDREFTAB-LAST                       
050900                                  MOD-IDREFTAB-UT                         
051000            PERFORM FX-SHOW-INFO                                          
051100            PERFORM FY-NUMBER-OF-TABLES                                   
051200          END-IF                                                          
051300          PERFORM IMS-GNP-2502-OKVAL                                      
051400        END-PERFORM                                                       
051500     END-IF                                                               
051600     .                                                                    
051700     EJECT                                                                
051800                                                                          
051900                                                                          
052000 FB-ENTER-UPDATE SECTION.                                                 
052100                                                                          
052200     PERFORM IMS-GU-2502                                                  
052300     IF MFS-UPDATE                                                        
052400        PERFORM IMS-GU-2502                                               
052500        IF SEGMENT-FOUND                                                  
052600           MOVE 2502-IDREFTAB TO MOD-IDREFTAB-FIRST                       
052700                                 MOD-IDREFTAB-LAST                        
052800                                 MOD-IDREFTAB-UT                          
052900           PERFORM FX-SHOW-INFO                                           
053000           PERFORM FY-NUMBER-OF-TABLES                                    
053100        ELSE                                                              
053200           PERFORM FZ-SHOW-SEGMENT-MISSING                                
053300        END-IF                                                            
053400     ELSE                                                                 
053500        IF SEGMENT-FOUND                                                  
053600           MOVE 2502-IDREFTAB    TO MOD-IDREFTAB-UT                       
053700           PERFORM FX-SHOW-INFO                                           
053800           PERFORM FY-NUMBER-OF-TABLES                                    
053900        ELSE                                                              
054000           PERFORM FZ-SHOW-SEGMENT-MISSING                                
054100        END-IF                                                            
054200        MOVE MID-IDREFTAB-FIRST TO MOD-IDREFTAB-FIRST                     
054300        MOVE MID-IDREFTAB-LAST TO MOD-IDREFTAB-LAST                       
054400     END-IF                                                               
054500                                                                          
054600     .                                                                    
054700     EJECT                                                                
054800                                                                          
054900                                                                          
055000 FC-FIRST SECTION.                                                        
055100                                                                          
055200     IF MID-IDREFTAB-FIRST = SPACE AND                                    
055300        MID-IDREFTAB-LAST = SPACE                                         
055400*       ENTER(PF7) NEW KEYS                                               
055500        PERFORM IMS-GU-2502                                               
055600        IF SEGMENT-FOUND                                                  
055700           MOVE 2502-IDREFTAB TO MOD-IDREFTAB-FIRST                       
055800                                 MOD-IDREFTAB-LAST                        
055900                                 MOD-IDREFTAB-UT                          
056000           PERFORM FX-SHOW-INFO                                           
056100           PERFORM FY-NUMBER-OF-TABLES                                    
056200        ELSE                                                              
056300           PERFORM FZ-SHOW-SEGMENT-MISSING                                
056400        END-IF                                                            
056500     ELSE                                                                 
056600        IF MID-IDREFTAB-FIRST > SPACE AND                                 
056700           MID-IDREFTAB-LAST > SPACE                                      
056800*          PF7 FIRST TIME                                                 
056900           PERFORM IMS-GU-2502                                            
057000           IF SEGMENT-FOUND                                               
057100              MOVE SPACE         TO MOD-IDREFTAB-FIRST                    
057200              MOVE 2502-IDREFTAB TO MOD-IDREFTAB-LAST                     
057300                                   MOD-IDREFTAB-UT                        
057400              PERFORM FX-SHOW-INFO                                        
057500              PERFORM FY-NUMBER-OF-TABLES                                 
057600           ELSE                                                           
057700              PERFORM FZ-SHOW-SEGMENT-MISSING                             
057800           END-IF                                                         
057900        ELSE                                                              
058000           IF MID-IDREFTAB-FIRST = SPACE AND                              
058100              MID-IDREFTAB-LAST > SPACE                                   
058200*             PF7 SECOND TIME                                             
058300              PERFORM IMS-GU-2501                                         
058400              PERFORM IMS-GNP-2502-OKVAL                                  
058500              PERFORM UNTIL SEGMENT-MISSING OR SEGMENT-END                
058600                IF SEGMENT-FOUND AND 2502-IDREFTAB ALPHABETIC             
058700                   MOVE 2502-IDREFTAB TO MOD-IDREFTAB-FIRST               
058800                                         MOD-IDREFTAB-LAST                
058900                                         MOD-IDREFTAB-UT                  
059000                   PERFORM FX-SHOW-INFO                                   
059100                   PERFORM FY-NUMBER-OF-TABLES                            
059200                   ADD +1  TO IX1                                         
059300                END-IF                                                    
059400                PERFORM IMS-GNP-2502-OKVAL                                
059500              END-PERFORM                                                 
059600              IF IX1 = 0                                                  
059700                PERFORM FZ-SHOW-SEGMENT-MISSING                           
059800              END-IF                                                      
059900           END-IF                                                         
060000        END-IF                                                            
060100     END-IF                                                               
060200     .                                                                    
060300     EJECT                                                                
060400                                                                          
060500                                                                          
060600 FX-SHOW-INFO SECTION.                                                    
060700                                                                          
060810     IF INDATA-WRONG                                                      
060820        IF MID-TEREFLIM NOT = ALL '+'                                     
060830           MOVE MID-TEREFLIM  TO MOD-TEREFLIM                             
060840        ELSE                                                              
060850           MOVE 2502-TEREFLIM TO MOD-TEREFLIM                             
060860        END-IF                                                            
060870     ELSE                                                                 
060880        MOVE 2502-TEREFLIM    TO MOD-TEREFLIM                             
060890     END-IF                                                               
060900                                                                          
061000     MOVE +1 TO KOL-IX                                                    
061100     PERFORM UNTIL KOL-IX > 8                                             
061200        MOVE 2502-KVPB-REF(KOL-IX) TO MOD-KVPB-REF(KOL-IX)                
061300        ADD +1 TO KOL-IX                                                  
061400     END-PERFORM                                                          
061500                                                                          
061600     MOVE +1 TO RAD-IX                                                    
061700     PERFORM UNTIL RAD-IX > 10                                            
061800        MOVE 2502-PRARTBES(RAD-IX) TO MOD-PRARTBES(RAD-IX)                
061900        MOVE +1 TO KOL-IX                                                 
062000        PERFORM UNTIL KOL-IX > 8                                          
062100           MOVE 2502-KVREFLIM(RAD-IX, KOL-IX) TO                          
062200                     MOD-KVREFLIM(RAD-IX, KOL-IX)                         
062300           MOVE 2502-KDREFPKT-LIM(RAD-IX, KOL-IX) TO                      
062400                     MOD-KDREFPKT-LIM(RAD-IX, KOL-IX)                     
062500           ADD +1  TO KOL-IX                                              
062600        END-PERFORM                                                       
062700        ADD +1 TO RAD-IX                                                  
062800     END-PERFORM                                                          
062900     PERFORM MFS-RENSA-COPY-FAELT                                         
063000     PERFORM MFS-FORM-ATTR-COPYFAELT                                      
063010     IF INDATA-OK                                                         
063100        PERFORM MFS-ERASE-FIELD-IN                                        
063110     END-IF                                                               
063200     .                                                                    
063300     EJECT                                                                
063400                                                                          
063500                                                                          
063600 FY-NUMBER-OF-TABLES SECTION.                                             
063700                                                                          
063800     PERFORM IMS-GU-2501                                                  
063900     MOVE +0 TO WS-NUMBER-OF-TABLES                                       
064000     PERFORM IMS-GNP-2502                                                 
064100     PERFORM UNTIL SEGMENT-MISSING                                        
064200       IF 2502-IDREFTAB ALPHABETIC                                        
064300         ADD +1 TO WS-NUMBER-OF-TABLES                                    
064400       END-IF                                                             
064500       PERFORM IMS-GNP-2502                                               
064600     END-PERFORM                                                          
064700                                                                          
064800     MOVE WS-NUMBER-OF-TABLES TO UT-ANTAL-TABELLER                        
064900     IF WS-NUMBER-OF-TABLES = 1                                           
065000        MOVE TE-PF8-EN-TABELL     TO UT-TABELLTEXT                        
065100        MOVE TE-LAST-PAGE      TO UT-NEXT-PAGE                            
065200     ELSE                                                                 
065300        MOVE TE-PF8-FLER-TABELLER TO UT-TABELLTEXT                        
065400        IF WS-FL-LAST-TABLE   = NOO                                       
065500           MOVE TE-PF8-MORE       TO UT-NEXT-PAGE                         
065600        ELSE                                                              
065700           MOVE TE-LAST-PAGE      TO UT-NEXT-PAGE                         
065800        END-IF                                                            
065900     END-IF                                                               
066000     MOVE MED-EX1 TO MOD-TEMFSINF                                         
066100     .                                                                    
066200     EJECT                                                                
066300                                                                          
066400                                                                          
066500 FZ-SHOW-SEGMENT-MISSING SECTION.                                         
066600                                                                          
066700     PERFORM MFS-ERASE-FIELD-OUT                                          
066800     IF MID-KDCMD-DELETE                                                  
066900        MOVE MED-1 TO MOD-TEMFSINF                                        
067000     ELSE                                                                 
067100        MOVE FEL-14 TO MOD-TEMFSFEL                                       
067200     END-IF                                                               
067300     .                                                                    
067400     EJECT                                                                
067500                                                                          
067600                                                                          
067700 G-CHECK-INPUT SECTION.                                                   
067800                                                                          
067900     MOVE DCS-IDDC    TO W-IDDC                                           
068000     MOVE WS-IDREFTAB TO W-IDREFTAB                                       
068100                                                                          
068200     MOVE YES TO INDATA-SW                                                
068300     IF MID-KDCMD = ALL '+' OR MID-KDCMD-INGENTING                        
068400        MOVE MFS-ALFA-FAELT-RAETT TO MOD-KDCMD-IN-ATTR                    
068500        MOVE 'R' TO MID-KDCMD                                             
068600     ELSE                                                                 
068700        IF MID-KDCMD-INSERT OR                                            
068800           MID-KDCMD-DELETE                                               
068900           MOVE MFS-ALFA-FAELT-RAETT TO MOD-KDCMD-IN-ATTR                 
069000        ELSE                                                              
069100           MOVE NOO TO INDATA-SW                                          
069200           MOVE MFS-ALFA-FAELT-FEL TO MOD-KDCMD-IN-ATTR                   
069300        END-IF                                                            
069400     END-IF                                                               
069500                                                                          
069600     IF INDATA-OK                                                         
069700        IF MID-KDCMD-REPLACE                                              
069800           PERFORM GA-CHECK-INPUT-CHANGES                                 
069900        ELSE                                                              
070000           IF MID-KDCMD-DELETE                                            
070100              PERFORM GB-CHECK-INPUT-DELETE                               
070200           ELSE                                                           
070300              IF MID-KDCMD-INSERT                                         
070400                 PERFORM GC-CHECK-INPUT-INSERT                            
070500              END-IF                                                      
070600           END-IF                                                         
070700        END-IF                                                            
070800     END-IF                                                               
070900     .                                                                    
071000     EJECT                                                                
071100                                                                          
071200                                                                          
071300 GA-CHECK-INPUT-CHANGES SECTION.                                          
071400                                                                          
071500     IF MID-COPY-DATA = ALL '+'                                           
071600        IF MID-INPUT = ALL '+'                                            
071700           MOVE ERR-PF11-AND-NO-DATA TO MED-IDMFSFEL                      
071800           CALL WMEDKONV USING MED-WMEDAREA                               
071900           MOVE MED-TEMFSFEL TO MOD-TEMFSFEL                              
072000           PERFORM MFS-ERASE-FIELD-IN                                     
072100           PERFORM MFS-ERASE-FIELD-OUT                                    
072200           MOVE MID-IDREFTAB-FIRST TO MOD-IDREFTAB-FIRST                  
072300           MOVE MID-IDREFTAB-LAST TO MOD-IDREFTAB-LAST                    
072400           MOVE NOO TO INDATA-SW                                          
072500        ELSE                                                              
072600           PERFORM IMS-GU-2502                                            
072700           IF SEGMENT-FOUND                                               
072800              PERFORM S31-CHECK-INDATA                                    
072900              IF INDATA-WRONG                                             
073000                 MOVE NOO TO INDATA-SW                                    
073100                 MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL                
073200                 CALL WMEDKONV USING MED-WMEDAREA                         
073300                 MOVE MED-TEMFSFEL TO MOD-TEMFSFEL                        
073400                 MOVE MID-IDREFTAB-FIRST TO MOD-IDREFTAB-FIRST            
073500                 MOVE MID-IDREFTAB-LAST TO MOD-IDREFTAB-LAST              
073600                 PERFORM MFS-ROER-EJ-FAELT-UT                             
073700                 PERFORM MFS-ROER-EJ-FAELT-IN                             
073800              END-IF                                                      
073900           ELSE                                                           
074000              MOVE NOO TO INDATA-SW                                       
074100              MOVE FEL-14 TO MOD-TEMFSFEL                                 
074200              MOVE MID-IDREFTAB-FIRST TO MOD-IDREFTAB-FIRST               
074300              MOVE MID-IDREFTAB-LAST TO MOD-IDREFTAB-LAST                 
074400              PERFORM MFS-ROER-EJ-FAELT-IN                                
074500              PERFORM MFS-ROER-EJ-FAELT-UT                                
074600           END-IF                                                         
074700        END-IF                                                            
074800     ELSE                                                                 
074900        MOVE NOO TO INDATA-SW                                             
075000        MOVE FEL-15 TO MOD-TEMFSFEL                                       
075100        MOVE MID-IDREFTAB-FIRST TO MOD-IDREFTAB-FIRST                     
075200        MOVE MID-IDREFTAB-LAST TO MOD-IDREFTAB-LAST                       
075300        PERFORM MFS-ROER-EJ-FAELT-IN                                      
075400        PERFORM MFS-ROER-EJ-FAELT-UT                                      
075500     END-IF                                                               
075600     .                                                                    
075700     EJECT                                                                
075800                                                                          
075900                                                                          
076000 GB-CHECK-INPUT-DELETE SECTION.                                           
076100                                                                          
076200     IF MID-COPY-DATA = ALL '+'                                           
076300        IF MID-INPUT = ALL '+'                                            
076400           IF WS-IDREFTAB ALPHABETIC                                      
076500              CONTINUE                                                    
076600           ELSE                                                           
076700              MOVE NOO TO INDATA-SW                                       
076800              MOVE FEL-16 TO MOD-TEMFSFEL                                 
076900              MOVE MID-IDREFTAB-FIRST TO MOD-IDREFTAB-FIRST               
077000              MOVE MID-IDREFTAB-LAST TO MOD-IDREFTAB-LAST                 
077100              PERFORM MFS-ROER-EJ-FAELT-IN                                
077200              PERFORM MFS-ROER-EJ-FAELT-UT                                
077300           END-IF                                                         
077400        ELSE                                                              
077500           MOVE NOO TO INDATA-SW                                          
077600           MOVE FEL-15 TO MOD-TEMFSFEL                                    
077700           MOVE MID-IDREFTAB-FIRST TO MOD-IDREFTAB-FIRST                  
077800           MOVE MID-IDREFTAB-LAST TO MOD-IDREFTAB-LAST                    
077900           PERFORM MFS-ROER-EJ-FAELT-IN                                   
078000           PERFORM MFS-ROER-EJ-FAELT-UT                                   
078100        END-IF                                                            
078200     ELSE                                                                 
078300        MOVE NOO TO INDATA-SW                                             
078400        MOVE FEL-15 TO MOD-TEMFSFEL                                       
078500        MOVE MID-IDREFTAB-FIRST TO MOD-IDREFTAB-FIRST                     
078600        MOVE MID-IDREFTAB-LAST TO MOD-IDREFTAB-LAST                       
078700        PERFORM MFS-ROER-EJ-FAELT-IN                                      
078800        PERFORM MFS-ROER-EJ-FAELT-UT                                      
078900     END-IF                                                               
079000     .                                                                    
079100     EJECT                                                                
079200                                                                          
079300                                                                          
079400 GC-CHECK-INPUT-INSERT SECTION.                                           
079500                                                                          
079600     PERFORM S31-CHECK-INDATA                                             
079700     IF INDATA-WRONG                                                      
079800        MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL                         
079900        CALL WMEDKONV USING MED-WMEDAREA                                  
080000        MOVE MED-TEMFSFEL TO MOD-TEMFSFEL                                 
080100        MOVE MID-IDREFTAB-FIRST TO MOD-IDREFTAB-FIRST                     
080200        MOVE MID-IDREFTAB-LAST TO MOD-IDREFTAB-LAST                       
080300        PERFORM MFS-ROER-EJ-FAELT-UT                                      
080400        PERFORM MFS-ROER-EJ-FAELT-IN                                      
080500     END-IF                                                               
080600     .                                                                    
080700     EJECT                                                                
080800                                                                          
080900                                                                          
081000 H-UPDATE SECTION.                                                        
081100                                                                          
081200     IF MID-KDCMD-REPLACE OR                                              
081300        MID-KDCMD-INSERT                                                  
081400           PERFORM HA-CHANGE-TABLE                                        
081500     ELSE                                                                 
081600        IF MID-KDCMD-DELETE                                               
081700           PERFORM HB-DELETE-TABLE                                        
081800        END-IF                                                            
081900     END-IF                                                               
082000     .                                                                    
082100     EJECT                                                                
082200                                                                          
082300                                                                          
082400 HA-CHANGE-TABLE SECTION.                                                 
082500                                                                          
082600     PERFORM IMS-GHU-2502                                                 
082700                                                                          
082800     IF MID-TEREFLIM = ALL '+'                                            
082900        CONTINUE                                                          
083000     ELSE                                                                 
083100        IF MID-TEREFLIM = SPACE                                           
083200           MOVE SPACE TO 2502-TEREFLIM                                    
083300        ELSE                                                              
083400           MOVE MID-TEREFLIM TO 2502-TEREFLIM                             
083500        END-IF                                                            
083600     END-IF                                                               
083700                                                                          
083800     MOVE +1 TO KOL-IX                                                    
083900     PERFORM UNTIL KOL-IX > 8                                             
084000        IF MID-KVPB-REF(KOL-IX) NOT = ALL '+'                             
084100           MOVE WS-KVPB-REF(KOL-IX) TO 2502-KVPB-REF(KOL-IX)              
084200        END-IF                                                            
084300        ADD +1 TO KOL-IX                                                  
084400     END-PERFORM                                                          
084500                                                                          
084600     MOVE WS-RAD TO RAD-IX                                                
084700     IF MID-PRARTBES NOT = ALL '+'                                        
084800        MOVE WS-PRARTBES TO 2502-PRARTBES(RAD-IX)                         
084900     END-IF                                                               
085000     MOVE +1 TO KOL-IX                                                    
085100     PERFORM UNTIL KOL-IX > 8                                             
085200        IF MID-KVREFLIM(KOL-IX) NOT = ALL '+'                             
085300           MOVE WS-KVREFLIM(KOL-IX) TO                                    
085400                2502-KVREFLIM(RAD-IX, KOL-IX)                             
085500        END-IF                                                            
085600        IF MID-KDREFPKT-LIM(KOL-IX) NOT = ALL '+'                         
085700           MOVE WS-KDREFPKT-LIM(KOL-IX) TO                                
085800                2502-KDREFPKT-LIM(RAD-IX, KOL-IX)                         
085900        END-IF                                                            
086000        ADD +1 TO KOL-IX                                                  
086100     END-PERFORM                                                          
086200                                                                          
086300     PERFORM IMS-REPL-2502                                                
086400                                                                          
086500     MOVE INF-UPDATE-DONE TO MED-IDMFSINF                                 
086600     CALL WMEDKONV USING MED-WMEDAREA                                     
086700     MOVE MED-MFSINF TO MOD-TEMFSINF                                      
086800     PERFORM MFS-FORM-ATTR                                                
086900     PERFORM MFS-ERASE-FIELD-IN                                           
087000     .                                                                    
087100     EJECT                                                                
087200                                                                          
087300                                                                          
087400                                                                          
087500                                                                          
087600 HB-DELETE-TABLE SECTION.                                                 
087700                                                                          
087800     PERFORM IMS-GHU-2502                                                 
087900                                                                          
088000     PERFORM IMS-DLET-2502                                                
088100                                                                          
088200     PERFORM IMS-GHU-2501                                                 
088300     PERFORM IMS-GNP-2502-OKVAL                                           
088400     IF SEGMENT-FOUND                                                     
088500        CONTINUE                                                          
088600     ELSE                                                                 
088700        PERFORM IMS-GHU-2501                                              
088800        PERFORM IMS-DLET-2501                                             
088900     END-IF                                                               
089000     .                                                                    
089100     EJECT                                                                
089200                                                                          
089300                                                                          
089400 I-CREATE-NEW-TABLE SECTION.                                              
089500                                                                          
089600     PERFORM IA-CHECK-NEW-TABLENAME                                       
089700     IF COPY-DATA-OK                                                      
089800        PERFORM IB-NEW-TABLE-EXIST                                        
089900        IF COPY-DATA-OK                                                   
090000           IF MID-COPY-DATA NOT = ALL '+'                                 
090100              PERFORM IC-CHECK-OLD-TABLE                                  
090200              IF COPY-DATA-OK                                             
090300                 PERFORM ID-CREATE-COPY-TABLE                             
090400              END-IF                                                      
090500           ELSE                                                           
090600              PERFORM IE-CREATE-EMPTY-TABLE                               
090700           END-IF                                                         
090800        END-IF                                                            
090900     END-IF                                                               
091000     .                                                                    
091100     EJECT                                                                
091200                                                                          
091300                                                                          
091400                                                                          
091500 IA-CHECK-NEW-TABLENAME SECTION.                                          
091600                                                                          
091700     MOVE YES TO COPY-DATA-SW                                             
091800                                                                          
091900     IF DCS-NDC-CN OR (DCS-NDC-NA AND DCS-USA)                            
092000       CONTINUE                                                           
092100     ELSE                                                                 
092200       MOVE NOO TO COPY-DATA-SW                                           
092300     END-IF                                                               
092400                                                                          
092500     IF WS-IDREFTAB ALPHABETIC                                            
092600        CONTINUE                                                          
092700     ELSE                                                                 
092800        MOVE NOO TO COPY-DATA-SW                                          
092900     END-IF                                                               
093000                                                                          
093100     IF DCS-IDDC    > SPACE AND                                           
093200        WS-IDREFTAB > SPACE                                               
093300        CONTINUE                                                          
093400     ELSE                                                                 
093500        MOVE NOO TO INDATA-SW                                             
093600     END-IF                                                               
093700                                                                          
093800                                                                          
093900     IF COPY-DATA-OK                                                      
094000       CONTINUE                                                           
094100     ELSE                                                                 
094200       MOVE FEL-11     TO MOD-TEMFSFEL                                    
094300     END-IF                                                               
094400                                                                          
094500     .                                                                    
094600     EJECT                                                                
094700                                                                          
094800                                                                          
094900 IB-NEW-TABLE-EXIST SECTION.                                              
095000                                                                          
095100     MOVE DCS-IDDC    TO W-IDDC                                           
095200     MOVE WS-IDREFTAB TO W-IDREFTAB                                       
095300     PERFORM IMS-GU-2502                                                  
095400     IF SEGMENT-FOUND                                                     
095500        MOVE NOO TO COPY-DATA-SW                                          
095600        MOVE FEL-12     TO MOD-TEMFSFEL                                   
095700     END-IF                                                               
095800     .                                                                    
095900     EJECT                                                                
096000                                                                          
096100                                                                          
096200 IC-CHECK-OLD-TABLE SECTION.                                              
096300                                                                          
096400     IF MID-COPY-IDDC NOT = ALL '+'                                       
096500        MOVE MID-COPY-IDDC TO W-OLD-IDDC                                  
096600     END-IF                                                               
096700                                                                          
096800     IF MID-COPY-IDREFTAB NOT = ALL '+'                                   
096900        MOVE MID-COPY-IDREFTAB TO W-OLD-IDREFTAB                          
097000     END-IF                                                               
097100                                                                          
097200     PERFORM IMS-GU-2502-OLD                                              
097300     IF SEGMENT-FOUND                                                     
097400        CONTINUE                                                          
097500     ELSE                                                                 
097600        MOVE FEL-13           TO MOD-TEMFSFEL                             
097700        MOVE NOO TO COPY-DATA-SW                                          
097800     END-IF                                                               
097900     .                                                                    
098000     EJECT                                                                
098100                                                                          
098200                                                                          
098300 ID-CREATE-COPY-TABLE SECTION.                                            
098400                                                                          
098500     PERFORM IMS-GU-2501                                                  
098600     IF SEGMENT-FOUND                                                     
098700        CONTINUE                                                          
098800     ELSE                                                                 
098900        PERFORM S30-CREATE-2501                                           
099000        PERFORM IMS-ISRT-2501                                             
099100     END-IF                                                               
099200     PERFORM IDA-COPY-TABLE                                               
099300     PERFORM IMS-ISRT-2502                                                
099400     .                                                                    
099500     EJECT                                                                
099600                                                                          
099700                                                                          
099800 IDA-COPY-TABLE SECTION.                                                  
099900                                                                          
100000     MOVE WS-IDREFTAB   TO 2502-IDREFTAB                                  
100100                           W-IDREFTAB                                     
100200                                                                          
100300     MOVE SPACE  TO 2502-TEREFLIM                                         
100400     MOVE MFS-ERASE-FIELD TO MOD-TEREFLIM                                 
100500                                                                          
100600     MOVE +1 TO KOL-IX                                                    
100700     PERFORM UNTIL KOL-IX > 8                                             
100800        MOVE OLD-2502-KVPB-REF(KOL-IX) TO                                 
100900                             2502-KVPB-REF(KOL-IX)                        
101000        ADD +1 TO KOL-IX                                                  
101100     END-PERFORM                                                          
101200                                                                          
101300     MOVE +1 TO RAD-IX                                                    
101400     PERFORM UNTIL RAD-IX > 10                                            
101500        MOVE OLD-2502-PRARTBES(RAD-IX) TO                                 
101600                             2502-PRARTBES(RAD-IX)                        
101700                                                                          
101800        MOVE +1 TO KOL-IX                                                 
101900        PERFORM UNTIL KOL-IX > 8                                          
102000*KOPIERA PUNKTER                                                          
102100           MOVE OLD-2502-KVREFLIM(RAD-IX, KOL-IX) TO                      
102200                             2502-KVREFLIM(RAD-IX, KOL-IX)                
102300           MOVE OLD-2502-KDREFPKT-LIM(RAD-IX, KOL-IX) TO                  
102400                             2502-KDREFPKT-LIM(RAD-IX, KOL-IX)            
102500*KOPIERA KVANTER                                                          
102600           MOVE OLD-2502-KVREFKVA(RAD-IX, KOL-IX) TO                      
102700                             2502-KVREFKVA(RAD-IX, KOL-IX)                
102800           MOVE OLD-2502-KDREFPKT-KVA(RAD-IX, KOL-IX) TO                  
102900                             2502-KDREFPKT-KVA(RAD-IX, KOL-IX)            
103000           ADD +1 TO KOL-IX                                               
103100        END-PERFORM                                                       
103200        ADD +1 TO RAD-IX                                                  
103300     END-PERFORM                                                          
103400     .                                                                    
103500     EJECT                                                                
103600                                                                          
103700                                                                          
103800 IE-CREATE-EMPTY-TABLE SECTION.                                           
103900                                                                          
104000     PERFORM IMS-GU-2501                                                  
104100     IF SEGMENT-FOUND                                                     
104200        CONTINUE                                                          
104300     ELSE                                                                 
104400        PERFORM S30-CREATE-2501                                           
104500        PERFORM IMS-ISRT-2501                                             
104600     END-IF                                                               
104700     PERFORM IEA-EMPTY-TABLE                                              
104800     PERFORM IMS-ISRT-2502                                                
104900     .                                                                    
105000     EJECT                                                                
105100                                                                          
105200                                                                          
105300 IEA-EMPTY-TABLE SECTION.                                                 
105400                                                                          
105500     MOVE WS-IDREFTAB       TO 2502-IDREFTAB                              
105600                               W-IDREFTAB                                 
105700     MOVE +1 TO KOL-IX                                                    
105800     PERFORM UNTIL KOL-IX > 8                                             
105900     IF KOL-IX = 8                                                        
106000        MOVE 999999.9     TO 2502-KVPB-REF(KOL-IX)                        
106100     ELSE                                                                 
106200        MOVE ZERO         TO 2502-KVPB-REF(KOL-IX)                        
106300     END-IF                                                               
106400     ADD +1 TO KOL-IX                                                     
106500     END-PERFORM                                                          
106600                                                                          
106700     MOVE +1 TO RAD-IX                                                    
106800                                                                          
106900     PERFORM UNTIL RAD-IX > 10                                            
107000        IF RAD-IX = 9                                                     
107100           MOVE 9999999.99  TO 2502-PRARTBES(RAD-IX)                      
107200        ELSE                                                              
107300           MOVE ZERO       TO   2502-PRARTBES(RAD-IX)                     
107400        END-IF                                                            
107500        MOVE +1 TO KOL-IX                                                 
107600        PERFORM UNTIL KOL-IX > 8                                          
107700           MOVE '1'     TO 2502-KVREFLIM(RAD-IX, KOL-IX)                  
107800           MOVE 'S'     TO 2502-KDREFPKT-LIM(RAD-IX, KOL-IX)              
107900           MOVE '1'     TO 2502-KVREFKVA(RAD-IX, KOL-IX)                  
108000           MOVE 'S'     TO 2502-KDREFPKT-KVA(RAD-IX, KOL-IX)              
108100           ADD +1 TO KOL-IX                                               
108200        END-PERFORM                                                       
108300        ADD +1 TO RAD-IX                                                  
108400     END-PERFORM                                                          
108500                                                                          
108600     MOVE SPACE TO 2502-TEREFLIM                                          
108700     .                                                                    
108800     EJECT                                                                
108900                                                                          
109000                                                                          
109100 J-SHOW-WRONG-WHEN-COPY SECTION.                                          
109200                                                                          
109300     PERFORM MFS-ERASE-FIELD-OUT                                          
109400     PERFORM MFS-READ-IN-AGAIN                                            
109500     PERFORM S45-MID-INDATA-TO-MOD                                        
109600     .                                                                    
109700     EJECT                                                                
109800                                                                          
109900                                                                          
109910 S10-AUTH-USER-CHECK SECTION.                                             
109920                                                                          
109921     MOVE YES TO INDATA-SW                                                
109922                                                                          
109923     MOVE MSGI-IDFTG    TO WS-IDFTG                                       
109926                                                                          
109930     IF MSGI-IDFTG = WC-IDFTG-PV                                          
109940       CONTINUE                                                           
109950     ELSE                                                                 
109952       IF DCS-NDC-NA                                                      
109953         IF IDFTG-US                                                      
109954           CONTINUE                                                       
109955         ELSE                                                             
109956           MOVE NOO                TO INDATA-SW                           
109960         END-IF                                                           
109961       END-IF                                                             
109962       IF DCS-NDC-CN                                                      
109963         IF IDFTG-CN                                                      
109964           CONTINUE                                                       
109965         ELSE                                                             
109966           MOVE NOO                TO INDATA-SW                           
109970         END-IF                                                           
109971       END-IF                                                             
109972       IF INDATA-WRONG                                                    
109974          MOVE ERR-USER-NOT-AUTH  TO MED-IDMFSFEL                         
109975          CALL WMEDKONV USING MED-WMEDAREA                                
109976          MOVE MED-MFSFEL         TO MOD-TEMFSFEL                         
109977          MOVE DCS-IDDC           TO W-IDDC                               
109978          MOVE WS-IDREFTAB        TO W-IDREFTAB                           
109979          MOVE MID-IDREFTAB-FIRST TO MOD-IDREFTAB-FIRST                   
109980          MOVE MID-IDREFTAB-LAST  TO MOD-IDREFTAB-LAST                    
109982          PERFORM MFS-ERASE-FIELD-OUT                                     
109983          PERFORM MFS-ERASE-FIELD-IN                                      
109984       END-IF                                                             
109985     END-IF                                                               
109986     .                                                                    
109987     EJECT                                                                
109988                                                                          
109990                                                                          
110000 S30-CREATE-2501 SECTION.                                                 
110100                                                                          
110200     MOVE '2501'              TO 2501-IDHTYP                              
110300     MOVE DCS-IDDC            TO 2501-IDDC                                
110400                                 W-IDDC                                   
110500     MOVE LOW-VALUE           TO 2501-LOWVALUE                            
110600     .                                                                    
110700     EJECT                                                                
110800                                                                          
110900                                                                          
111000 S31-CHECK-INDATA SECTION.                                                
111100                                                                          
111200*KOLLA TEXTRAD                                                            
111300                                                                          
111400     IF MID-TEREFLIM = ALL '+' OR SPACE                                   
111500        MOVE MFS-ALFA-FAELT-RAETT  TO MOD-TEREFLIM-ATTR                   
111600     ELSE                                                                 
111700        MOVE MFS-ALFA-FAELT-RAETT  TO MOD-TEREFLIM-ATTR                   
111800     END-IF                                                               
111900                                                                          
112000                                                                          
112100*KOLLA PB-RAD                                                             
112200                                                                          
112300     MOVE +1 TO KOL-IX                                                    
112400     PERFORM UNTIL KOL-IX > 8                                             
112500        IF MID-KVPB-REF(KOL-IX) NOT = ALL '+'                             
112600           MOVE MID-KVPB-REF(KOL-IX) TO DEC-IDFRIDATA                     
112700           MOVE 5                    TO DEC-KVHELTAL                      
112800           MOVE 1                    TO DEC-KVDECIMAL                     
112900           MOVE 'P'                  TO DEC-KDSVAR                        
113000           CALL WDECEDIT USING DEC-WDECAREA                               
113100           IF DEC-KDSVAR-OK                                               
113200              MOVE DEC-IDEDITDATA TO WS-KVPB-REF(KOL-IX)                  
113300              IF KOL-IX = 8                                               
113400                 MOVE 999999.9    TO WS-KVPB-REF(KOL-IX)                  
113500              END-IF                                                      
113600              MOVE MFS-NUM-FIELD-OK    TO                                 
113700                           MOD-KVPB-REF-IN-ATTR(KOL-IX)                   
113800           ELSE                                                           
113900              MOVE ZERO TO WS-KVPB-REF(KOL-IX)                            
114000              MOVE NOO TO INDATA-SW                                       
114100              MOVE MFS-NUM-FIELD-WRONG TO                                 
114200                           MOD-KVPB-REF-IN-ATTR(KOL-IX)                   
114300           END-IF                                                         
114400        ELSE                                                              
114500           MOVE MFS-NUM-FIELD-OK       TO                                 
114600                        MOD-KVPB-REF-IN-ATTR(KOL-IX)                      
114700        END-IF                                                            
114800        ADD +1 TO KOL-IX                                                  
114900     END-PERFORM                                                          
115000                                                                          
115100* SKAPA OCH KONTROLLERA JÄMFÖRELSETABELL FÖR ATT FÅ                       
115200* PB I STIGANDE ORDNING                                                   
115300                                                                          
115400     IF MID-KDCMD-REPLACE                                                 
115500        MOVE 1 TO KOL-IX                                                  
115600        PERFORM UNTIL KOL-IX > 8                                          
115700           IF MID-KVPB-REF(KOL-IX) NOT = ALL '+'                          
115800              MOVE WS-KVPB-REF(KOL-IX) TO JMF-KVPB-REF(KOL-IX)            
115900           ELSE                                                           
116000              MOVE 2502-KVPB-REF(KOL-IX) TO JMF-KVPB-REF(KOL-IX)          
116100           END-IF                                                         
116200           ADD +1 TO KOL-IX                                               
116300        END-PERFORM                                                       
116400                                                                          
116500        MOVE +1 TO KOL-IX                                                 
116600        PERFORM UNTIL KOL-IX > 8                                          
116700           IF KOL-IX = 1                                                  
116800              IF JMF-KVPB-REF(KOL-IX) < JMF-KVPB-REF(KOL-IX + 1)          
116900                 MOVE MFS-NUM-FAELT-RAETT TO                              
117000                              MOD-KVPB-REF-IN-ATTR(KOL-IX)                
117100              ELSE                                                        
117200                 MOVE NOO TO INDATA-SW                                    
117300                 MOVE MFS-NUM-FAELT-FEL TO                                
117400                              MOD-KVPB-REF-IN-ATTR(KOL-IX)                
117500              END-IF                                                      
117600           END-IF                                                         
117700                                                                          
117800           IF KOL-IX > 1 AND < 8                                          
117900             IF (JMF-KVPB-REF(KOL-IX) >                                   
118000                                 JMF-KVPB-REF(KOL-IX - 1))  AND           
118100                (JMF-KVPB-REF(KOL-IX) < JMF-KVPB-REF(KOL-IX + 1))         
118200                MOVE MFS-NUM-FIELD-OK     TO                              
118300                              MOD-KVPB-REF-IN-ATTR(KOL-IX)                
118400             ELSE                                                         
118500                MOVE NOO TO INDATA-SW                                     
118600                MOVE MFS-NUM-FAELT-FEL  TO                                
118700                             MOD-KVPB-REF-IN-ATTR(KOL-IX)                 
118800             END-IF                                                       
118900           END-IF                                                         
119000                                                                          
119100           IF KOL-IX = 8                                                  
119200              IF JMF-KVPB-REF(KOL-IX)  > JMF-KVPB-REF(KOL-IX - 1)         
119300                 MOVE MFS-NUM-FAELT-RAETT  TO                             
119400                              MOD-KVPB-REF-IN-ATTR(KOL-IX)                
119500              ELSE                                                        
119600                 MOVE NOO TO INDATA-SW                                    
119700                 MOVE MFS-NUM-FAELT-FEL   TO                              
119800                              MOD-KVPB-REF-IN-ATTR(KOL-IX)                
119900              END-IF                                                      
120000           END-IF                                                         
120100           ADD +1 TO KOL-IX                                               
120200        END-PERFORM                                                       
120300     END-IF                                                               
120400                                                                          
120500*RAD                                                                      
120600                                                                          
120700     IF MID-RAD NOT = ALL '+'                                             
120800        IF MID-RAD NUMERIC                                                
120900           IF MID-RAD > 0 AND <= 9                                        
121000              MOVE MID-RAD TO WS-RAD                                      
121100              MOVE MFS-NUM-FAELT-RAETT  TO                                
121200                           MOD-RAD-IN-ATTR                                
121300           ELSE                                                           
121400              MOVE NOO TO  INDATA-SW                                      
121500              MOVE MFS-NUM-FAELT-FEL   TO MOD-RAD-IN-ATTR                 
121600           END-IF                                                         
121700        ELSE                                                              
121800           MOVE NOO TO  INDATA-SW                                         
121900           MOVE MFS-NUM-FAELT-FEL   TO MOD-RAD-IN-ATTR                    
122000        END-IF                                                            
122100     ELSE                                                                 
122200       MOVE MFS-NUM-FAELT-RAETT     TO MOD-RAD-IN-ATTR                    
122300     END-IF                                                               
122400                                                                          
122500                                                                          
122600*KOLLA PRIS-PUNKTRAD                                                      
122700                                                                          
122800     PERFORM S31A-RADANGIVELSE-EXIST                                      
122900                                                                          
123000     IF MID-PRARTBES NOT = ALL '+'                                        
123100        MOVE MID-PRARTBES   TO DEC-IDFRIDATA                              
123200        MOVE 7              TO DEC-KVHELTAL                               
123300        MOVE 2              TO DEC-KVDECIMAL                              
123400        MOVE 'P'            TO DEC-KDSVAR                                 
123500        CALL WDECEDIT USING DEC-WDECAREA                                  
123600        IF DEC-KDSVAR-OK                                                  
123700           IF MID-RAD = 9                                                 
123800              MOVE 9999999.99     TO WS-PRARTBES                          
123900           ELSE                                                           
124000              MOVE DEC-IDEDITDATA TO WS-PRARTBES                          
124100           END-IF                                                         
124200           MOVE MFS-NUM-FAELT-RAETT  TO                                   
124300                        MOD-PRARTBES-IN-ATTR                              
124400        ELSE                                                              
124500           MOVE NOO TO  INDATA-SW                                         
124600           MOVE MFS-NUM-FAELT-FEL   TO MOD-PRARTBES-IN-ATTR               
124700        END-IF                                                            
124800     ELSE                                                                 
124900           MOVE MFS-NUM-FAELT-RAETT TO MOD-PRARTBES-IN-ATTR               
125000     END-IF                                                               
125100                                                                          
125200                                                                          
125300*KVREFLIM, KDREFPKT-LIM                                                   
125400                                                                          
125500     MOVE +1 TO KOL-IX                                                    
125600     PERFORM UNTIL KOL-IX > 8                                             
125700        IF MID-KVREFLIM(KOL-IX) NOT = ALL '+'                             
125800           IF MID-KVREFLIM(KOL-IX) NUMERIC                                
125900              MOVE MID-KVREFLIM(KOL-IX) TO WS-KVREFLIM(KOL-IX)            
126000              MOVE MFS-NUM-FAELT-RAETT  TO                                
126100                           MOD-KVREFLIM-IN-ATTR(KOL-IX)                   
126200           ELSE                                                           
126300              MOVE NOO TO INDATA-SW                                       
126400              MOVE MFS-NUM-FAELT-FEL   TO                                 
126500                           MOD-KVREFLIM-IN-ATTR(KOL-IX)                   
126600           END-IF                                                         
126700        ELSE                                                              
126800           MOVE MFS-NUM-FAELT-RAETT TO                                    
126900                        MOD-KVREFLIM-IN-ATTR(KOL-IX)                      
127000           IF MID-KDCMD-REPLACE AND WS-RAD > +0                           
127100              MOVE 2502-KVREFLIM(WS-RAD, KOL-IX)                          
127200                                     TO WS-KVREFLIM(KOL-IX)               
127300           ELSE                                                           
127400              MOVE ZERO              TO WS-KVREFLIM(KOL-IX)               
127500           END-IF                                                         
127600        END-IF                                                            
127700                                                                          
127800        IF MID-KDREFPKT-LIM(KOL-IX) NOT = ALL '+'                         
127900           IF MID-KDREFPKT-LIM(KOL-IX) = 'S' OR 'D'                       
128000              MOVE MID-KDREFPKT-LIM(KOL-IX) TO                            
128100                                      WS-KDREFPKT-LIM(KOL-IX)             
128200              MOVE MFS-ALFA-FAELT-RAETT  TO                               
128300                           MOD-KDREFPKT-LIM-IN-ATTR(KOL-IX)               
128400           ELSE                                                           
128500              MOVE NOO TO INDATA-SW                                       
128600              MOVE MFS-ALFA-FAELT-FEL TO                                  
128700                           MOD-KDREFPKT-LIM-IN-ATTR(KOL-IX)               
128800           END-IF                                                         
128900        ELSE                                                              
129000           MOVE MFS-ALFA-FAELT-RAETT TO                                   
129100                        MOD-KDREFPKT-LIM-IN-ATTR(KOL-IX)                  
129200           IF MID-KDCMD-REPLACE AND WS-RAD > +0                           
129300              MOVE 2502-KDREFPKT-LIM (WS-RAD, KOL-IX)                     
129400                                     TO WS-KDREFPKT-LIM(KOL-IX)           
129500           ELSE                                                           
129600              MOVE ZERO              TO WS-KDREFPKT-LIM(KOL-IX)           
129700           END-IF                                                         
129800        END-IF                                                            
129900                                                                          
130000        IF WS-KDREFPKT-LIM(KOL-IX) = 'D'                                  
130100       AND WS-KVREFLIM    (KOL-IX) = ZERO                                 
130200           MOVE NOO                  TO INDATA-SW                         
130300           MOVE MFS-NUM-FAELT-FEL    TO                                   
130400                           MOD-KVREFLIM-IN-ATTR    (KOL-IX)               
130500           MOVE MFS-ALFA-FAELT-FEL   TO                                   
130600                           MOD-KDREFPKT-LIM-IN-ATTR(KOL-IX)               
130700        END-IF                                                            
130800                                                                          
130900        ADD +1 TO KOL-IX                                                  
131000     END-PERFORM                                                          
131100     .                                                                    
131200     EJECT                                                                
131300                                                                          
131400                                                                          
131500 S31A-RADANGIVELSE-EXIST SECTION.                                         
131600                                                                          
131700     IF MID-RAD = ALL '+'                                                 
131800        IF MID-PRARTBES NOT = ALL '+'                                     
131900           MOVE NOO TO INDATA-SW                                          
132000           MOVE MFS-NUM-FAELT-FEL   TO MOD-RAD-IN                         
132100        END-IF                                                            
132200                                                                          
132300        MOVE +1 TO KOL-IX                                                 
132400        PERFORM UNTIL KOL-IX > 8                                          
132500           IF MID-KVREFLIM(KOL-IX) NOT = ALL '+'                          
132600           OR MID-KDREFPKT-LIM(KOL-IX) NOT = ALL '+'                      
132700              MOVE NOO TO INDATA-SW                                       
132800              MOVE MFS-NUM-FAELT-FEL   TO MOD-RAD-IN-ATTR                 
132900           END-IF                                                         
133000           ADD +1 TO KOL-IX                                               
133100        END-PERFORM                                                       
133200     END-IF                                                               
133300                                                                          
133400     IF MID-RAD NOT = ALL '+'                                             
133500        IF MID-PRARTBES       = ALL '+' AND                               
133600                                                                          
133700           MID-KVREFLIM(1)    = ALL '+' AND                               
133800           MID-KVREFLIM(2)    = ALL '+' AND                               
133900           MID-KVREFLIM(3)    = ALL '+' AND                               
134000           MID-KVREFLIM(4)    = ALL '+' AND                               
134100           MID-KVREFLIM(5)    = ALL '+' AND                               
134200           MID-KVREFLIM(6)    = ALL '+' AND                               
134300           MID-KVREFLIM(7)    = ALL '+' AND                               
134400           MID-KVREFLIM(8)    = ALL '+' AND                               
134500                                                                          
134600           MID-KDREFPKT-LIM(1) = ALL '+' AND                              
134700           MID-KDREFPKT-LIM(2) = ALL '+' AND                              
134800           MID-KDREFPKT-LIM(3) = ALL '+' AND                              
134900           MID-KDREFPKT-LIM(4) = ALL '+' AND                              
135000           MID-KDREFPKT-LIM(5) = ALL '+' AND                              
135100           MID-KDREFPKT-LIM(6) = ALL '+' AND                              
135200           MID-KDREFPKT-LIM(7) = ALL '+' AND                              
135300           MID-KDREFPKT-LIM(8) = ALL '+'                                  
135400           MOVE NOO TO INDATA-SW                                          
135500           MOVE MFS-NUM-FAELT-FEL   TO MOD-RAD-IN                         
135600        END-IF                                                            
135700                                                                          
135800     END-IF                                                               
135900     .                                                                    
136000     EJECT                                                                
136100                                                                          
136200                                                                          
136300                                                                          
136400 S45-MID-INDATA-TO-MOD SECTION.                                           
136500                                                                          
136600                                                                          
136700* KDCMD                                                                   
136800                                                                          
136900     IF MID-KDCMD = ALL '+'                                               
137000        MOVE MFS-ERASE-FIELD         TO MOD-KDCMD-IN                      
137100     ELSE                                                                 
137200        MOVE MID-KDCMD               TO MOD-KDCMD-IN                      
137300     END-IF                                                               
137400                                                                          
137500                                                                          
137600* COPY-IDDC                                                               
137700                                                                          
137800     IF MID-COPY-IDDC = ALL '+'                                           
137900        MOVE MFS-ERASE-FIELD         TO MOD-COPY-IDDC-IN                  
138000     ELSE                                                                 
138100        MOVE MID-COPY-IDDC           TO MOD-COPY-IDDC-IN                  
138200     END-IF                                                               
138300                                                                          
138400                                                                          
138500* COPY-IDREFTAB                                                           
138600                                                                          
138700     IF MID-COPY-IDREFTAB = ALL '+'                                       
138800        MOVE MFS-ERASE-FIELD         TO MOD-COPY-IDREFTAB-IN              
138900     ELSE                                                                 
139000        MOVE MID-COPY-IDREFTAB       TO MOD-COPY-IDREFTAB-IN              
139100     END-IF                                                               
139200                                                                          
139300                                                                          
139400* TEREFLIM                                                                
139500                                                                          
139600     IF MID-TEREFLIM = ALL '+'                                            
139700        MOVE MFS-ERASE-FIELD         TO MOD-TEREFLIM                      
139800     ELSE                                                                 
139900        MOVE MID-TEREFLIM            TO MOD-TEREFLIM                      
140000     END-IF                                                               
140100                                                                          
140200                                                                          
140300* PB-RAD                                                                  
140400                                                                          
140500     MOVE +1    TO KOL-IX                                                 
140600     PERFORM UNTIL KOL-IX > 8                                             
140700        IF MID-KVPB-REF(KOL-IX) = ALL '+'                                 
140800           MOVE MFS-ERASE-FIELD      TO MOD-KVPB-REF-IN(KOL-IX)           
140900        ELSE                                                              
141000           MOVE MFS-ROER-EJ-FAELT    TO MOD-KVPB-REF-IN(KOL-IX)           
141100        END-IF                                                            
141200        ADD +1 TO KOL-IX                                                  
141300     END-PERFORM                                                          
141400                                                                          
141500                                                                          
141600* PRICE-RAD                                                               
141700                                                                          
141800     IF MID-RAD = ALL '+'                                                 
141900        MOVE MFS-ERASE-FIELD      TO MOD-RAD-IN                           
142000     ELSE                                                                 
142100        MOVE MFS-ROER-EJ-FAELT    TO MOD-RAD-IN                           
142200     END-IF                                                               
142300                                                                          
142400     IF MID-PRARTBES = ALL '+'                                            
142500        MOVE MFS-ERASE-FIELD      TO MOD-PRARTBES-IN                      
142600     ELSE                                                                 
142700        MOVE MFS-ROER-EJ-FAELT    TO MOD-PRARTBES-IN                      
142800     END-IF                                                               
142900                                                                          
143000     MOVE +1    TO KOL-IX                                                 
143100     PERFORM UNTIL KOL-IX > 8                                             
143200        IF MID-KVREFLIM(KOL-IX) = ALL '+'                                 
143300           MOVE MFS-ERASE-FIELD      TO MOD-KVREFLIM-IN(KOL-IX)           
143400        ELSE                                                              
143500           MOVE MFS-ROER-EJ-FAELT    TO MOD-KVREFLIM-IN(KOL-IX)           
143600        END-IF                                                            
143700        IF MID-KDREFPKT-LIM(KOL-IX) = ALL '+'                             
143800           MOVE MFS-ERASE-FIELD   TO MOD-KDREFPKT-LIM-IN(KOL-IX)          
143900        ELSE                                                              
144000           MOVE MFS-ROER-EJ-FAELT TO MOD-KDREFPKT-LIM-IN(KOL-IX)          
144100        END-IF                                                            
144200        ADD +1 TO KOL-IX                                                  
144300     END-PERFORM                                                          
144400     .                                                                    
144500     EJECT                                                                
144600                                                                          
144700                                                                          
144800 MFS-ERASE-FIELD-OUT SECTION.                                             
144900                                                                          
145000     MOVE MFS-ERASE-FIELD TO MOD-TEREFLIM                                 
145100     MOVE +1 TO KOL-IX                                                    
145200     PERFORM UNTIL KOL-IX > 8                                             
145300        MOVE MFS-ERASE-FIELD    TO MOD-KVPB-REF(KOL-IX)                   
145400        ADD +1 TO KOL-IX                                                  
145500     END-PERFORM                                                          
145600                                                                          
145700     MOVE +1 TO RAD-IX                                                    
145800     PERFORM UNTIL RAD-IX > 10                                            
145900        MOVE MFS-ERASE-FIELD TO MOD-PRARTBES(RAD-IX)                      
146000        MOVE +1 TO KOL-IX                                                 
146100        PERFORM UNTIL KOL-IX > 8                                          
146200           MOVE MFS-ERASE-FIELD TO                                        
146300                               MOD-KVREFLIM(RAD-IX, KOL-IX)               
146400                               MOD-KDREFPKT-LIM(RAD-IX, KOL-IX)           
146500           ADD +1 1 TO KOL-IX                                             
146600        END-PERFORM                                                       
146700        ADD +1 TO RAD-IX                                                  
146800     END-PERFORM                                                          
146900     .                                                                    
147000     EJECT                                                                
147100                                                                          
147200                                                                          
147300 MFS-ERASE-FIELD-IN SECTION.                                              
147400                                                                          
147500     MOVE MFS-ERASE-FIELD    TO MOD-COPY-IDDC-IN                          
147600                                MOD-COPY-IDREFTAB-IN                      
147700                                                                          
147800     MOVE +1 TO KOL-IX                                                    
147900     PERFORM UNTIL KOL-IX > 8                                             
148000        MOVE MFS-ERASE-FIELD TO MOD-KVPB-REF-IN(KOL-IX)                   
148100        ADD +1 TO KOL-IX                                                  
148200     END-PERFORM                                                          
148300                                                                          
148400     MOVE MFS-ERASE-FIELD TO MOD-RAD-IN                                   
148500                             MOD-PRARTBES-IN                              
148600                                                                          
148700     MOVE +1 TO KOL-IX                                                    
148800     PERFORM UNTIL KOL-IX > 8                                             
148900        MOVE MFS-ERASE-FIELD TO MOD-KVREFLIM-IN(KOL-IX)                   
149000                                MOD-KDREFPKT-LIM-IN(KOL-IX)               
149100        ADD +1 TO KOL-IX                                                  
149200     END-PERFORM                                                          
149300     .                                                                    
149400     EJECT                                                                
149500                                                                          
149600                                                                          
149700 MFS-RENSA-COPY-FAELT SECTION.                                            
149800                                                                          
149900     MOVE MFS-ERASE-FIELD   TO MOD-COPY-IDDC-IN                           
150000                               MOD-COPY-IDREFTAB-IN                       
150100                               MOD-KDCMD-IN                               
150200     .                                                                    
150300     EJECT                                                                
150400                                                                          
150500 MFS-FORM-ATTR-COPYFAELT SECTION.                                         
150600                                                                          
150700     MOVE MFS-FORMAT-DEFAULT-ATTR TO MOD-COPY-IDDC-IN-ATTR                
150800                                     MOD-COPY-IDREFTAB-IN-ATTR            
150900                                     MOD-KDCMD-IN-ATTR                    
151000     .                                                                    
151100     EJECT                                                                
151200                                                                          
151300 MFS-ROER-EJ-FAELT-UT  SECTION.                                           
151400                                                                          
151500     MOVE MFS-ROER-EJ-FAELT TO MOD-TEREFLIM                               
151600                                                                          
151700     MOVE +1 TO KOL-IX                                                    
151800     PERFORM UNTIL KOL-IX > 8                                             
151900        MOVE MFS-ROER-EJ-FAELT    TO MOD-KVPB-REF(KOL-IX)                 
152000        ADD +1 1 TO KOL-IX                                                
152100     END-PERFORM                                                          
152200                                                                          
152300     MOVE +1 TO RAD-IX                                                    
152400     PERFORM UNTIL RAD-IX > 10                                            
152500        MOVE MFS-ROER-EJ-FAELT    TO MOD-PRARTBES(RAD-IX)                 
152600        MOVE +1 TO KOL-IX                                                 
152700        PERFORM UNTIL KOL-IX > 8                                          
152800           MOVE MFS-ROER-EJ-FAELT TO                                      
152900                                  MOD-KVREFLIM(RAD-IX, KOL-IX)            
153000                                  MOD-KDREFPKT-LIM(RAD-IX, KOL-IX)        
153100           ADD +1 1 TO KOL-IX                                             
153200        END-PERFORM                                                       
153300        ADD +1 TO RAD-IX                                                  
153400     END-PERFORM                                                          
153500     .                                                                    
153600     EJECT                                                                
153700                                                                          
153800                                                                          
153900 MFS-ROER-EJ-FAELT-IN  SECTION.                                           
154000                                                                          
154100     MOVE MFS-ROER-EJ-FAELT    TO MOD-COPY-IDDC-IN                        
154200                                  MOD-COPY-IDREFTAB-IN                    
154300                                                                          
154400     MOVE MFS-ROER-EJ-FAELT    TO MOD-TEREFLIM                            
154500                                                                          
154600     MOVE +1 TO KOL-IX                                                    
154700     PERFORM UNTIL KOL-IX > 8                                             
154800        MOVE MFS-ROER-EJ-FAELT TO MOD-KVPB-REF-IN(KOL-IX)                 
154900        ADD +1 TO KOL-IX                                                  
155000     END-PERFORM                                                          
155100                                                                          
155200     MOVE MFS-ROER-EJ-FAELT TO MOD-RAD-IN                                 
155300                               MOD-PRARTBES-IN                            
155400                                                                          
155500     MOVE +1 TO KOL-IX                                                    
155600     PERFORM UNTIL KOL-IX > 8                                             
155700        MOVE MFS-ROER-EJ-FAELT TO MOD-KVREFLIM-IN(KOL-IX)                 
155800                                  MOD-KDREFPKT-LIM-IN(KOL-IX)             
155900        ADD +1 TO KOL-IX                                                  
156000     END-PERFORM                                                          
156100     .                                                                    
156200     EJECT                                                                
156300                                                                          
156400                                                                          
156500 MFS-FORM-ATTR SECTION.                                                   
156600                                                                          
156700     MOVE MFS-FORMAT-DEFAULT-ATTR TO MOD-COPY-IDDC-IN                     
156800                                     MOD-COPY-IDREFTAB-IN                 
156900                                                                          
157000     MOVE MFS-FORMAT-DEFAULT-ATTR TO MOD-TEREFLIM                         
157100                                                                          
157200     MOVE +1 TO KOL-IX                                                    
157300     PERFORM UNTIL KOL-IX > 8                                             
157400        MOVE MFS-FORMAT-DEFAULT-ATTR                                      
157500                                TO MOD-KVPB-REF-IN-ATTR(KOL-IX)           
157600        ADD +1 TO KOL-IX                                                  
157700     END-PERFORM                                                          
157800                                                                          
157900     MOVE MFS-FORMAT-DEFAULT-ATTR TO MOD-RAD-IN-ATTR                      
158000                                     MOD-PRARTBES-IN-ATTR                 
158100                                                                          
158200     MOVE +1 TO KOL-IX                                                    
158300     PERFORM UNTIL KOL-IX > 8                                             
158400        MOVE MFS-FORMAT-DEFAULT-ATTR TO                                   
158500                                MOD-KVREFLIM-IN-ATTR(KOL-IX)              
158600                                MOD-KDREFPKT-LIM-IN-ATTR(KOL-IX)          
158700        ADD +1 TO KOL-IX                                                  
158800     END-PERFORM                                                          
158900     .                                                                    
159000     EJECT                                                                
159100                                                                          
159200                                                                          
159300 MFS-READ-IN-AGAIN SECTION.                                               
159400                                                                          
159500     MOVE MFS-ADD-READ-FIELD    TO MOD-KDCMD-IN-ATTR                      
159600                                                                          
159700     MOVE MFS-ADD-READ-FIELD    TO MOD-COPY-IDDC-IN                       
159800                                   MOD-COPY-IDREFTAB-IN                   
159900                                                                          
160000     MOVE MFS-ADD-READ-FIELD    TO MOD-TEREFLIM-ATTR                      
160100                                                                          
160200     MOVE MFS-ADD-READ-FIELD    TO MOD-RAD-IN-ATTR                        
160300                                   MOD-PRARTBES-IN-ATTR                   
160400     MOVE +1 TO KOL-IX                                                    
160500     PERFORM UNTIL KOL-IX > 8                                             
160600        MOVE MFS-ADD-READ-FIELD TO                                        
160700                                  MOD-KVPB-REF-IN-ATTR(KOL-IX)            
160800                                  MOD-KVREFLIM-IN-ATTR(KOL-IX)            
160900                                  MOD-KDREFPKT-LIM-IN-ATTR(KOL-IX)        
161000        ADD +1 TO KOL-IX                                                  
161100     END-PERFORM                                                          
161200     .                                                                    
161300     EJECT                                                                
161400                                                                          
161500                                                                          
161600* --- IMS SEKTIONER ---                                                   
161700     SKIP3                                                                
161800 IMS-GET-MSG SECTION.                                                     
161900                                                                          
162000     MOVE '  QC' TO GOOD-STATUSCODES                                      
162100     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
162200     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
162300     PERFORM IMS-STATUSCHECK                                              
162400     .                                                                    
162500     SKIP3                                                                
162600 IMS-INSERT-MSG SECTION.                                                  
162700                                                                          
162800     IF ENGLISH-TEXT                                                      
162900       MOVE 'N' TO MFS-KDHUVOMR                                           
163000     END-IF                                                               
163100     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
163200     MOVE SPACE TO GOOD-STATUSCODES                                       
163300     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
163400     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
163500     PERFORM IMS-STATUSCHECK                                              
163600     .                                                                    
163700     EJECT                                                                
163800                                                                          
163900                                                                          
164000 IMS-GU-2502 SECTION.                                                     
164100     STRING 'WL250101(WDGXKEY  =' W-WDGXKEY-X ')'                         
164200          DELIMITED BY SIZE INTO SSA1                                     
164300     STRING 'WL250111(IDREFTAB =' W-IDREFTAB-X ')'                        
164400          DELIMITED BY SIZE INTO SSA2                                     
164500     MOVE '  GE' TO GOOD-STATUSCODES                                      
164600     CALL CBLTDLI USING GU WDR2-PCB DLI-IO-AREA-1 SSA1 SSA2               
164700     MOVE WDR2-STATUS-CODE TO STATUS-WS                                   
164800     PERFORM IMS-STATUSCHECK                                              
164900     .                                                                    
165000     EJECT                                                                
165100                                                                          
165200                                                                          
165300 IMS-GNP-2502-NEXT SECTION.                                               
165400     STRING 'WL250111(IDREFTAB >' W-IDREFTAB-X ')'                        
165500          DELIMITED BY SIZE INTO SSA1                                     
165600     MOVE '  GE' TO GOOD-STATUSCODES                                      
165700     CALL CBLTDLI USING GN WDR2-PCB DLI-IO-AREA-1 SSA1                    
165800     MOVE WDR2-STATUS-CODE TO STATUS-WS                                   
165900     PERFORM IMS-STATUSCHECK                                              
166000     .                                                                    
166100     EJECT                                                                
166200                                                                          
166300                                                                          
166400 IMS-GU-2501 SECTION.                                                     
166500     STRING 'WL250101(WDGXKEY  =' W-WDGXKEY-X ')'                         
166600          DELIMITED BY SIZE INTO SSA1                                     
166700     MOVE '  GE' TO GOOD-STATUSCODES                                      
166800     CALL CBLTDLI USING GU WDR2-PCB DLI-IO-AREA-1 SSA1                    
166900     MOVE WDR2-STATUS-CODE TO STATUS-WS                                   
167000     PERFORM IMS-STATUSCHECK                                              
167100     .                                                                    
167200     EJECT                                                                
167300                                                                          
167400                                                                          
167500 IMS-GNP-2502 SECTION.                                                    
167600     MOVE  'WL250111 ' TO SSA1                                            
167700     MOVE '  GEGB' TO GOOD-STATUSCODES                                    
167800     CALL CBLTDLI USING GNP WDR2-PCB DLI-IO-AREA-1 SSA1                   
167900     MOVE WDR2-STATUS-CODE TO STATUS-WS                                   
168000     PERFORM IMS-STATUSCHECK                                              
168100     .                                                                    
168200     EJECT                                                                
168300                                                                          
168400                                                                          
168500 IMS-GHU-2501 SECTION.                                                    
168600     STRING 'WL250101(WDGXKEY  =' W-WDGXKEY-X ')'                         
168700          DELIMITED BY SIZE INTO SSA1                                     
168800     MOVE '  GE' TO GOOD-STATUSCODES                                      
168900     CALL CBLTDLI USING GHU WDR2-PCB DLI-IO-AREA-1 SSA1                   
169000     MOVE WDR2-STATUS-CODE TO STATUS-WS                                   
169100     PERFORM IMS-STATUSCHECK                                              
169200     .                                                                    
169300     EJECT                                                                
169400                                                                          
169500                                                                          
169600 IMS-GNP-2502-OKVAL SECTION.                                              
169700     MOVE  'WL250111 ' TO SSA1                                            
169800     MOVE '  GEGB' TO GOOD-STATUSCODES                                    
169900     CALL CBLTDLI USING GNP WDR2-PCB DLI-IO-AREA-1 SSA1                   
170000     MOVE WDR2-STATUS-CODE TO STATUS-WS                                   
170100     PERFORM IMS-STATUSCHECK                                              
170200     .                                                                    
170300     EJECT                                                                
170400                                                                          
170500                                                                          
170600 IMS-GHU-2502 SECTION.                                                    
170700     STRING 'WL250101(WDGXKEY  =' W-WDGXKEY-X ')'                         
170800          DELIMITED BY SIZE INTO SSA1                                     
170900     STRING 'WL250111(IDREFTAB =' W-IDREFTAB-X ')'                        
171000          DELIMITED BY SIZE INTO SSA2                                     
171100     MOVE '  ' TO GOOD-STATUSCODES                                        
171200     CALL CBLTDLI USING GHU WDR2-PCB DLI-IO-AREA-1 SSA1 SSA2              
171300     MOVE WDR2-STATUS-CODE TO STATUS-WS                                   
171400     PERFORM IMS-STATUSCHECK                                              
171500     .                                                                    
171600     EJECT                                                                
171700                                                                          
171800                                                                          
171900 IMS-GU-2502-OLD SECTION.                                                 
172000     STRING 'WL250101(WDGXKEY  =' W-OLD-WDGXKEY-X ')'                     
172100          DELIMITED BY SIZE INTO SSA1                                     
172200     STRING 'WL250111(IDREFTAB =' W-OLD-IDREFTAB-X ')'                    
172300          DELIMITED BY SIZE INTO SSA2                                     
172400     MOVE '  GE' TO GOOD-STATUSCODES                                      
172500     CALL CBLTDLI USING GU WDR2-PCB DLI-IO-AREA-2 SSA1 SSA2               
172600     MOVE WDR2-STATUS-CODE TO STATUS-WS                                   
172700     PERFORM IMS-STATUSCHECK                                              
172800     .                                                                    
172900     EJECT                                                                
173000                                                                          
173100                                                                          
173200 IMS-ISRT-2501 SECTION.                                                   
173300                                                                          
173400     MOVE 'WL250101 '          TO SSA1                                    
173500     MOVE '  ' TO GOOD-STATUSCODES                                        
173600     CALL CBLTDLI USING ISRT WDR2-PCB DLI-IO-AREA-1 SSA1                  
173700     MOVE WDR2-STATUS-CODE TO STATUS-WS                                   
173800     PERFORM IMS-STATUSCHECK                                              
173900     .                                                                    
174000     EJECT                                                                
174100                                                                          
174200                                                                          
174300 IMS-ISRT-2502 SECTION.                                                   
174400                                                                          
174500     STRING 'WL250101(WDGXKEY  =' W-WDGXKEY-X ')'                         
174600          DELIMITED BY SIZE INTO SSA1                                     
174700     MOVE 'WL250111 '         TO SSA2                                     
174800     MOVE '  ' TO GOOD-STATUSCODES                                        
174900     CALL CBLTDLI USING ISRT WDR2-PCB DLI-IO-AREA-1 SSA1 SSA2             
175000     MOVE WDR2-STATUS-CODE TO STATUS-WS                                   
175100     PERFORM IMS-STATUSCHECK                                              
175200     .                                                                    
175300     EJECT                                                                
175400                                                                          
175500                                                                          
175600 IMS-REPL-2502 SECTION.                                                   
175700                                                                          
175800     MOVE '  ' TO GOOD-STATUSCODES                                        
175900     CALL CBLTDLI USING REPL WDR2-PCB DLI-IO-AREA-1                       
176000     MOVE WDR2-STATUS-CODE TO STATUS-WS                                   
176100     PERFORM IMS-STATUSCHECK                                              
176200     .                                                                    
176300     EJECT                                                                
176400                                                                          
176500                                                                          
176600 IMS-DLET-2501 SECTION.                                                   
176700                                                                          
176800     MOVE '  ' TO GOOD-STATUSCODES                                        
176900     CALL CBLTDLI USING DLET WDR2-PCB DLI-IO-AREA-1                       
177000     MOVE WDR2-STATUS-CODE TO STATUS-WS                                   
177100     PERFORM IMS-STATUSCHECK                                              
177200     EJECT                                                                
177300     .                                                                    
177400                                                                          
177500                                                                          
177600 IMS-DLET-2502 SECTION.                                                   
177700                                                                          
177800     MOVE '  ' TO GOOD-STATUSCODES                                        
177900     CALL CBLTDLI USING DLET WDR2-PCB DLI-IO-AREA-1                       
178000     MOVE WDR2-STATUS-CODE TO STATUS-WS                                   
178100     PERFORM IMS-STATUSCHECK                                              
178200     .                                                                    
178300     EJECT                                                                
178400                                                                          
178500 IMS-GU-WDB601    SECTION.                                                
178600     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
178700          DELIMITED BY SIZE INTO SSA1                                     
178800     MOVE '  GE' TO GOOD-STATUSCODES                                      
178900     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B601 SSA1                 
179000     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
179100     PERFORM IMS-STATUSCHECK                                              
179200     .                                                                    
179300     EJECT                                                                
179400                                                                          
179500 IMS-STATUSCHECK SECTION.                                                 
179600                                                                          
179700     SET STATUS-IX TO 1                                                   
179800     SEARCH GOOD-STATUS                                                   
179900       AT END                                                             
180000         STRING ' INVALID STATUS CODE FROM IMS: ' STATUS-WS               
180100         DELIMITED BY SIZE INTO ERROR-TEXT                                
180200         CALL FELLOG                                                      
180300       WHEN GOOD-STATUS (STATUS-IX) = STATUS-WS                           
180400         CONTINUE                                                         
180500     END-SEARCH                                                           
180600     .                                                                    
180700     EJECT                                                                
