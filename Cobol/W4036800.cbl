000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.     W4036800.                                                
000400 AUTHOR.         BERT ANDERSSON.                                          
000500 DATE-WRITTEN.   90/05/08.                                                
000600                                                                          
000700     REMARKS.                                                             
000800*                                                                         
000900*    FUNKTION.                                                            
001000*        PROGRAMMET ÄR ETT FRÅGE-/UPPDATERINGS-PROGRAM MOT                
001100*        ARBETSTIDSTABELLEN(WLXXKC).PROGRAMMET LÄSER IDPRC FRÅN DB        
001200*        PROGRAMMET ÄR EN UPPDATERINGS-MPP.                               
001300*        PROGRAMMET UPPDATERAR WLXXKC (WDR1).                             
001400*        I PROGRAMMET FINNS MÖJLIGHET ATT:                                
001500*        - SÖKA IDPRC.                                                    
001600*        - ÄNDRA/UPPDATERA OCH NYREGISTRERA IDPRC.                        
001700*    INDATA.                                                              
001800*        TRANSAKTION: W4T368                                              
001900*        MID:         W4I36801                                            
002000*                                                                         
002100*    UTDATA.                                                              
002200*        MOD:         W4O36801                                            
002300                                                                          
002400     SKIP3                                                                
002500 ENVIRONMENT DIVISION.                                                    
002600     EJECT                                                                
002700 DATA DIVISION.                                                           
002800 WORKING-STORAGE SECTION.                                                 
002810     SKIP3                                                                
002811*    --CHECKED BY WY2000                                                  
002820     SKIP3                                                                
002900 77  IDPGM                       PIC X(08)   VALUE 'W4036800'.            
003000 77  SECTION-NAMN                PIC X(25)   VALUE SPACE.                 
003100 77  JA                          PIC X       VALUE 'J'.                   
003200 77  NEJ                         PIC X       VALUE 'N'.                   
003300 77  SPRAK-IX                    PIC S9(9)   VALUE +0   COMP SYNC.        
003400 77  MAX-MOD-LAENGD              PIC S9(4)   VALUE +400 COMP SYNC.        
003500 77  INDX                        PIC S9      VALUE +1   COMP SYNC.        
003600 77  MAX-INDX                    PIC S9      VALUE +3   COMP SYNC.        
003700 77  WS-IDPRC                    PIC X(4)    VALUE SPACE.                 
003900 77  WS-DATUM                    PIC 9(6)    VALUE ZERO.                  
004000 77  WS-START-DATUM              PIC 9(6)    VALUE ZERO.                  
004100 77  WS-KDCALL                   PIC 9(1)    VALUE 1.                     
004610*                                                                         
004611 01  WS-DCUSER.                                                           
004612     03 FILLER                   PIC X(5)   VALUE 'WIDDC'.                
004613     03 WS-DCUSER-IDDC           PIC X(2)   VALUE SPACE.                  
004614     03 FILLER                   PIC X(1)   VALUE SPACE.                  
004710*                                                                         
004720*      --- VALID IDDC CODES                                               
004730*                                                                         
004740*01    -COPY WWDC99                                                       
004750       EJECT                                                              
004800 01  WS-NUMEDIT                  PIC 99V99.                               
004900 01  FILLER      REDEFINES  WS-NUMEDIT.                                   
005000     03 WS-NUMFYRA           PIC 9(4).                                    
005100                                                                          
005200 01  WS-NUM8.                                                             
005300     03 WS-NUM4              PIC 9(4).                                    
005400     03 FILLER               PIC 9(4).                                    
005500                                                                          
005600 01 WS-TILOKTID-NUM8.                                                     
005700     03 WS-FILLER             PIC 9(1).                                   
005800     03 WS-TILOKTID-NUM4      PIC 9(4).                                   
005900     03 WS-ZERO               PIC 9(3).                                   
006000                                                                          
006100 01  WS-DATUM-GRP.                                                        
006200     03  FILLER                  PIC 9.                                   
006300     03  WS-DATUM-NUM6           PIC 9(6).                                
006400                                                                          
006500 01  WS-GRP.                                                              
006600     03  WS-STAPAC     OCCURS  3    PIC 9(5).                             
006700     03  WS-STOPAC     OCCURS  3    PIC 9(5).                             
006800     03  WS-STAADM     OCCURS  3    PIC 9(5).                             
006900     03  WS-STOADM     OCCURS  3    PIC 9(5).                             
007000     03  WS-STALAST    OCCURS  3    PIC 9(5).                             
007100     03  WS-STOLAST    OCCURS  3    PIC 9(5).                             
007200                                                                          
007300 01  IMS-PARAMETRAR.                                                      
007400     03 DLI-FUNCTION             PIC X(4)   VALUE SPACE.                  
007500     03 DLI-STATUS-KOD           PIC X(2)   VALUE SPACE.                  
007600                                                                          
007700 01  WS-KONTROLLTID-ED              PIC 99V99.                            
007800 01  WS-KONTROLLTID     REDEFINES  WS-KONTROLLTID-ED.                     
007900     03 WS-KONTROLLTID-TIMMA        PIC 99.                               
008000        88 TIMMA-OK                         VALUE 00 THRU 23.             
008100     03 WS-KONTROLLTID-MINUT        PIC 99.                               
008200        88 MINUT-OK                         VALUE 00 THRU 59.             
008300                                                                          
008400*    --- ARBETSFÄLT FÖR AKTUELLA NYCKELVÄRDEN FRÅN SKÄRMEN                
008500 77  WS-WDGXKEY                  PIC X(10)   VALUE SPACE.                 
008600                                                                          
008700 77  INDATA-SW                   PIC X       VALUE 'J'.                   
008800     88  INDATA-OK                           VALUE 'J'.                   
008900     88  INDATA-FEL                          VALUE 'N'.                   
009000                                                                          
009100 77  UPPDAT-SW                   PIC X       VALUE 'J'.                   
009200     88  UPPDAT-OK                           VALUE 'J'.                   
009300     88  UPPDAT-FEL                          VALUE 'N'.                   
009400                                                                          
009500 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
009600     88  NYCKLAR-OK                          VALUE 'J'.                   
009700     88  NYCKLAR-FEL                         VALUE 'N'.                   
009800                                                                          
009900 77  NYA-NYCKLAR-SW              PIC X       VALUE 'J'.                   
010000     88  NYA-NYCKLAR                         VALUE 'J'.                   
010100                                                                          
010200 77  ALLT-SW                     PIC X       VALUE 'J'.                   
010300     88  ALLT-OK                             VALUE 'J'.                   
010400     88  INTE-ALLT-OK                        VALUE 'N'.                   
010500                                                                          
010600 77  MID-RAD-SW                  PIC X       VALUE 'J'.                   
010700     88  MID-RAD-TOM                         VALUE 'J'.                   
010800                                                                          
010900 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
011000     88  EGEN-MID                            VALUE '4368'.                
011100     88  GODK-MID              VALUE '4364' '4365' '4368' '4369'.         
011200*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
011300 01  GENERELLA-SUBPROGRAM.                                                
011400     03  WDECEDIT                PIC X(8)    VALUE 'WDECEDIT'.            
011500     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
011600     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
011700     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
011800     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
011900     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
012100     EJECT                                                                
012200*   -COPY WDECAREA                                                        
012300     EJECT                                                                
012400*   -COPY WDATAREA                                                        
012500     EJECT                                                                
012600*   -COPY WMEDAREA                                                        
012700     EJECT                                                                
012800*                   ****    PARAMETRAR TILL W005INIT                      
012900*01  -COPY WMSGINIT                                                       
013000     SKIP3                                                                
013010*01  -COPY WMSGINIT -PRE DC-                                              
013020     SKIP3                                                                
013300     SKIP3                                                                
013400 01  MESSAGE-CODES.                                                       
013500     03  ERR-KEYS-ARE-MISSING    PIC X(3)    VALUE '005'.                 
013600     03  ERR-UPDATE-FORBIDDEN    PIC X(3)    VALUE '007'.                 
013700     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
013800     03  ERR-CORR-HILITE-FLDS    PIC X(3)    VALUE '409'.                 
013900     03  INF-PRESS-PF11          PIC X(3)    VALUE '003'.                 
014000     03  INF-UPDATE-DONE         PIC X(3)    VALUE '101'.                 
014100     03  INF-NO-UPDATE           PIC X(3)    VALUE '414'.                 
014200     EJECT                                                                
014300*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
014400*                                                                         
014500 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
014600     SKIP3                                                                
014700*01  MID -COPY W4I36801                                                   
014800     EJECT                                                                
014900 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
015000     SKIP3                                                                
015100*01  -COPY WMSGAREA                                                       
015200     EJECT                                                                
015300     03  MOD REDEFINES MSG-AREA.                                          
015400*      05  -COPY W4O36801                                                 
015500     EJECT                                                                
015600 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
015700     SKIP3                                                                
015800*01  -COPY WMFSAREA                                                       
015900     EJECT                                                                
016000 01  W-PROG-TO-PROG-SW.                                                   
016100*  03   -COPY WMSGSOP                                                     
016200     EJECT                                                                
016300*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
016400*                                                                         
016500 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
016600     SKIP3                                                                
016700 01  NYCKLAR-TILL-DLI.                                                    
016800     03  W-WDGXKEY-4435-X.                                                
016900         05  W-4435-IDHTYP       PIC X(4)     VALUE '4435'.               
017000         05  W-4435-IDDC         PIC X(2).                                
017100         05  W-4435-LOW-VALUE    PIC X(24)    VALUE LOW-VALUE.            
017200     03  W-WDGXKEY-4436-X.                                                
017300         05  W-4436-IDPRC        PIC X(4).                                
017400         05  W-4436-KVKALTIM     PIC 9(2).                                
017500         05  W-4436-LOW-VALUE    PIC X(4)     VALUE LOW-VALUE.            
017600     03  W-WDGXKEY-MIN-X.                                                 
017700         05  W-MIN-IDPRC         PIC X(4).                                
017800         05  W-FILLER            PIC X(6)     VALUE LOW-VALUE.            
017900     03  W-WDGXKEY-MAX-X.                                                 
018000         05  W-MAX-IDPRC         PIC X(4).                                
018100         05  W-FILLER            PIC X(6)     VALUE HIGH-VALUE.           
018200*    --- STATUS-KOD FRÅN IMS                                              
018300 01  STATUS-WS                   PIC XX.                                  
018400     88  SEGMENT-FINNS                       VALUE '  '.                  
018500     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
018600     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
018700     SKIP2                                                                
018800 01  GODK-STATUSKODER.                                                    
018900     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
019000     SKIP3                                                                
019100 01  SSA1                        PIC X(64).                               
019200 01  SSA2                        PIC X(64).                               
019300     EJECT                                                                
019400*    --- IMS FUNKTIONSKODER                                               
019500*01  -COPY W0003                                                          
019600     EJECT                                                                
019700*    ---  DLI INPUT-OUTPUT AREA                                           
019800 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA1'.        
019900     SKIP3                                                                
020000 01  DLI-IO-AREA1.                                                        
020100     03  IOAREA1                 PIC X(30)   VALUE SPACE.                 
020200     SKIP3                                                                
020300*    03  WLXXKC01 -COPY WDGX4435    -PRE IOAREA1-   -RED IOAREA1.         
020400*    03  WLXXKC11 -COPY WDGX4436    -PRE IOAREA1-   -RED IOAREA1.         
020500 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA2'.        
020600 01  DLI-IO-AREA2.                                                        
020700     03  IOAREA2                 PIC X(30)   VALUE SPACE.                 
020800*    03  WLXXKC11 -COPY WDGX4436    -PRE IOAREA2-   -RED IOAREA2.         
020900     EJECT                                                                
021000 LINKAGE SECTION.                                                         
021100                                                                          
021200*01  -COPY W0009      -PRE MSG-                                           
021300     SKIP2                                                                
021400*01  -COPY W0009      -PRE ALT-                                           
021500     EJECT                                                                
021600*01  -COPY W0008     -PRE USEA-                                           
021700     05  FILLER              PIC X.                                       
021800     EJECT                                                                
021900*01  -COPY W0008      -PRE XXKC-                                          
022000     05  FILLER                  PIC X.                                   
022100     EJECT                                                                
022200 PROCEDURE DIVISION  USING MSG-PCB ALT-PCB                                
022300                                  USEA-PCB XXKC-PCB.                      
022400     ENTRY 'DLITCBL' USING MSG-PCB ALT-PCB                                
022500                                  USEA-PCB XXKC-PCB.                      
022600 STYR SECTION.                                                            
022700     PERFORM IMS-GET-MSG                                                  
022800     IF SEGMENT-FINNS                                                     
022900       PERFORM A-INIT                                                     
023000       PERFORM B-KOLLA-NYCKLAR                                            
023100       IF NYCKLAR-OK                                                      
023200         IF MFS-UPDATE                                                    
023300           PERFORM C-INDATA-KONTROLL                                      
023400           IF INDATA-OK                                                   
023500             PERFORM D-UPPDATERA                                          
023600           END-IF                                                         
023700         ELSE                                                             
023800           PERFORM E-INFAELT-KONTROLL                                     
023900         END-IF                                                           
024000         IF ALLT-OK                                                       
024100           PERFORM F-LAES-VISA-INFO                                       
024200         END-IF                                                           
024300       END-IF                                                             
024400       MOVE MAX-MOD-LAENGD TO MSG-KVLL                                    
024500       PERFORM IMS-INSERT-MSG                                             
024600     END-IF                                                               
024700                                                                          
024800     MOVE ZERO TO RETURN-CODE                                             
024900     GOBACK                                                               
025000     .                                                                    
025100     EJECT                                                                
025200 A-INIT SECTION.                                                          
025300                                                                          
025400     IF MSG-DUBBLA-TRANSKODER                                             
025500       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W4I36801                 
025600       MOVE MSG-IDTRANS-2             TO MFS-IDTRANS                      
025700       MOVE MSG-KDMFSFOR-2            TO MFS-KDMFSFOR                     
025800     ELSE                                                                 
025900       MOVE MSG-INDATA-MINUS-1-TRANSKOD TO MID-W4I36801                   
026000       MOVE MSG-IDTRANS-1             TO MFS-IDTRANS                      
026100       MOVE MSG-KDMFSFOR-1            TO MFS-KDMFSFOR                     
026200     END-IF                                                               
026300                                                                          
026400     MOVE MSG-KDTRTYP                 TO MFS-KDTRTYP                      
026500     MOVE MSG-IDPFK                   TO MFS-IDPFK                        
026600     MOVE MFS-IDTRANS                 TO W-IDTRANS                        
026700                                                                          
026800     MOVE LOW-VALUE                   TO MSG-AREA                         
026900     MOVE ZERO                        TO WS-NUMEDIT                       
027000     MOVE 'W4O368N1'                  TO MFS-IDMOD                        
027100     MOVE '4368'                      TO MOD-IDTRANS                      
027200                                         MSGSOP-IDTRANS                   
027300     MOVE MFS-KDMFSFOR                TO MSGSOP-KDMFSFOR                  
027400     MOVE 'W413S1'                    TO MSGSOP-IDPROCESS                 
027500     MOVE 'O'                         TO MSGSOP-KDSOPFUNK                 
027600     MOVE MFS-RENSA-FAELT             TO MOD-TEMFSFEL MOD-TEMFSINF        
027700     MOVE 1                           TO WS-KDCALL                        
027800                                                                          
027900     IF NOT EGEN-MID                                                      
028000       MOVE SPACE                     TO MFS-KDTRTYP                      
028100       MOVE '7'                       TO MFS-IDPFK                        
028200       PERFORM AA-MOVE-PLUS-TO-INFIELDS                                   
028300     END-IF                                                               
029200     .                                                                    
029300     EJECT                                                                
029400 AA-MOVE-PLUS-TO-INFIELDS SECTION.                                        
029500                                                                          
029600     MOVE  +1        TO INDX                                              
029700     MOVE  '+'       TO MID-FLTABORT                                      
029800     MOVE  '++++++'  TO MID-TIUPDATE-IN                                   
029900     PERFORM UNTIL INDX > MAX-INDX                                        
030000       MOVE '+++++'  TO MID-STAPAC-IN(INDX) MID-STOPAC-IN(INDX)           
030100                        MID-STAADM-IN(INDX) MID-STOADM-IN(INDX)           
030200                        MID-STALAST-IN(INDX) MID-STOLAST-IN(INDX)         
030300       ADD +1        TO INDX                                              
030400     END-PERFORM                                                          
030500     .                                                                    
030600 B-KOLLA-NYCKLAR SECTION.                                                 
030700                                                                          
030800       MOVE ALL '+'           TO MSGI-WMSGINIT                            
030900       MOVE '001'             TO MSGI-KDCALL                              
031000       MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                              
031100       MOVE '4368'            TO MSGI-IDTRANS                             
031200       MOVE MSG-LTERM-NAME    TO MSGI-IDLTERM-USER                        
031300       CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                         
031400                                                                          
031410       IF MSGI-IDLAND-SPR = 'GB'                                          
031420         MOVE 'GB '                   TO MED-IDSKYLT                      
031430         MOVE +2                      TO SPRAK-IX                         
031440       ELSE                                                               
031450         MOVE 'S  '                   TO MED-IDSKYLT                      
031460         MOVE +1                      TO SPRAK-IX                         
031470       END-IF                                                             
031500                                                                          
031600       MOVE MFS-RENSA-FAELT    TO MOD-IDPRC-IN MOD-TIDATUM-IN             
031700                                  MOD-TIUPDATE-IN                         
031800                                  MOD-IDDC-IN                             
031900       MOVE JA                 TO INDATA-SW NYCKLAR-SW                    
032000                                  NYA-NYCKLAR-SW                          
032100       IF MID-IDPRC-IN = ALL '+'                                          
032200         MOVE MID-IDPRC-UT            TO WS-IDPRC                         
032300         MOVE NEJ                     TO NYA-NYCKLAR-SW                   
032400       ELSE                                                               
032500         MOVE MID-IDPRC-IN            TO WS-IDPRC                         
032600         MOVE SPACE                   TO MFS-KDTRTYP                      
032700       END-IF                                                             
032800                                                                          
032900       MOVE MSGI-IDDC               TO WS-IDDC                            
033000                                                                          
033100       IF WS-IDDC IS > SPACE                                              
033200         CONTINUE                                                         
033300       ELSE                                                               
033400         MOVE NEJ                     TO NYCKLAR-SW                       
033500       END-IF                                                             
033600                                                                          
033700       IF NYCKLAR-OK                                                      
033800         MOVE WS-IDDC                 TO W-4435-IDDC                      
033900       END-IF                                                             
034000                                                                          
034100       IF MID-TIDATUM-IN = ALL '+'                                        
034200         MOVE MID-TIDATUM-UT          TO WS-DATUM                         
034300       ELSE                                                               
034400         MOVE MID-TIDATUM-IN          TO WS-DATUM                         
034500       END-IF                                                             
034600* DETTA DATUM ANV. AV 4369-BILDEN. INTE FÖR UPPDATERING.                  
035800       IF WS-DATUM NUMERIC AND WS-DATUM > ZERO                            
035900         MOVE WS-DATUM                TO MOD-TIDATUM-UT                   
036000       ELSE                                                               
036100         MOVE MFS-RENSA-FAELT         TO MOD-TIDATUM-UT                   
036200       END-IF                                                             
036400                                                                          
036500       IF GODK-MID                                                        
036600         MOVE WS-IDPRC              TO MOD-IDPRC-UT W-4436-IDPRC          
036700                                       W-MIN-IDPRC W-MAX-IDPRC            
036800         MOVE WS-IDDC               TO MOD-IDDC-UT                        
036900       ELSE                                                               
037000         MOVE MFS-RENSA-FAELT        TO MOD-IDPRC-UT                      
037100                                        MOD-IDDC-UT                       
037200                                        MOD-TIDATUM-UT                    
037300                                        MOD-TIUPDATE-UT                   
037400       END-IF                                                             
037500                                                                          
037600       IF NYCKLAR-FEL                                                     
037700         MOVE ERR-WRONG-KEY           TO MED-IDMFSFEL                     
037800         PERFORM S01-ERR-RUTINE                                           
037900       END-IF                                                             
038000     .                                                                    
038100     EJECT                                                                
038200 C-INDATA-KONTROLL SECTION.                                               
038300                                                                          
038400     PERFORM CA-INFAELT-KONTROLL                                          
038500     PERFORM CB-KONTROLL-UPPDAT-DATUM                                     
038600     IF MID-RAD-TOM                                                       
038700       IF MID-FLTABORT = ALL '+'                                          
038800         CONTINUE                                                         
038900       ELSE                                                               
039000         PERFORM CJ-KONTROLL-AV-BORTTAG                                   
039100       END-IF                                                             
039200     ELSE                                                                 
039300       IF MID-FLTABORT = ALL '+'                                          
039400         MOVE +1                         TO INDX                          
039500         PERFORM UNTIL INDX > MAX-INDX                                    
039600           PERFORM CC-KONTROLL-STAPAC                                     
039700           PERFORM CD-KONTROLL-STOPAC                                     
039800           PERFORM CE-KONTROLL-STAADM                                     
039900           PERFORM CF-KONTROLL-STOADM                                     
040000           PERFORM CG-KONTROLL-STALAST                                    
040100           PERFORM CH-KONTROLL-STOLAST                                    
040200           PERFORM CI-MOVE-WS-X-TO-MOD                                    
040300           ADD +1                         TO INDX                         
040400         END-PERFORM                                                      
040500       ELSE                                                               
040600         PERFORM CJ-KONTROLL-AV-BORTTAG                                   
040700       END-IF                                                             
040800     END-IF                                                               
040900                                                                          
041000     IF INDATA-FEL                                                        
041100       MOVE NEJ                       TO ALLT-SW                          
041200       MOVE ERR-CORR-HILITE-FLDS      TO MED-IDMFSFEL                     
041300       PERFORM S01-ERR-RUTINE                                             
041400       PERFORM MFS-ROER-EJ-FAELT-IN                                       
041500       PERFORM MFS-ROER-EJ-FAELT-UT                                       
041600     END-IF                                                               
041700                                                                          
041800     IF MID-RAD-TOM AND                                                   
041900        MID-FLTABORT = ALL '+' AND                                        
042000        MID-TIUPDATE-IN = ALL '+'                                         
042100       MOVE NEJ                       TO INDATA-SW                        
042200       MOVE INF-NO-UPDATE             TO MED-IDMFSINF                     
042300       PERFORM S02-INF-RUTINE                                             
042400       PERFORM MFS-ROER-EJ-FAELT-IN                                       
042500       PERFORM MFS-ROER-EJ-FAELT-UT                                       
042600     END-IF                                                               
042700     .                                                                    
042800 CA-INFAELT-KONTROLL SECTION.                                             
042900                                                                          
043000     MOVE +1                         TO INDX                              
043100     PERFORM UNTIL INDX > MAX-INDX                                        
043200       IF MID-STAPAC-IN(INDX) = ALL '+'  AND                              
043300          MID-STOPAC-IN(INDX) = ALL '+'  AND                              
043400          MID-STAADM-IN(INDX) = ALL '+'  AND                              
043500          MID-STOADM-IN(INDX) = ALL '+'  AND                              
043600          MID-STALAST-IN(INDX) = ALL '+' AND                              
043700          MID-STOLAST-IN(INDX) = ALL '+'                                  
043800         CONTINUE                                                         
043900       ELSE                                                               
044000         MOVE NEJ                       TO MID-RAD-SW                     
044100       END-IF                                                             
044200       ADD +1                         TO INDX                             
044300     END-PERFORM                                                          
044400     .                                                                    
044500 CB-KONTROLL-UPPDAT-DATUM SECTION.                                        
044600                                                                          
044700     IF MID-TIUPDATE-IN = ALL '+'                                         
044800         MOVE MFS-NUM-FAELT-RAETT    TO MOD-TIUPDATE-ATTR                 
044900         MOVE ZERO                   TO WS-START-DATUM                    
045000     ELSE                                                                 
045100       IF MID-TIUPDATE-IN IS NUMERIC AND                                  
045200         MID-TIUPDATE-IN > ZERO                                           
045397                                                                          
045400         MOVE MID-TIUPDATE-IN        TO DAT-I-TIDATUM                     
045410                                        WS-START-DATUM                    
045500         MOVE 'AAMMDD'               TO DAT-KDDATFORM                     
045600         CALL WDATKONV USING DAT-KDDATFORM, DAT-I-TIDATUM                 
045700                             DAT-O-TIDATUM, DAT-KDSVAR                    
045800         IF DAT-KDSVAR-OK                                                 
045900           MOVE MFS-NUM-FAELT-RAETT  TO MOD-TIUPDATE-ATTR                 
046000           MOVE WS-START-DATUM       TO MOD-TIUPDATE-UT                   
046100         ELSE                                                             
046200           MOVE MFS-NUM-FAELT-FEL    TO MOD-TIUPDATE-ATTR                 
046300           MOVE NEJ                  TO INDATA-SW                         
046400         END-IF                                                           
046500       ELSE                                                               
046600         MOVE MFS-NUM-FAELT-FEL      TO MOD-TIUPDATE-ATTR                 
046700         MOVE NEJ                    TO INDATA-SW                         
046800       END-IF                                                             
046900     END-IF                                                               
047000     .                                                                    
047100 CC-KONTROLL-STAPAC SECTION.                                              
047200                                                                          
047300     IF MID-STAPAC-IN(INDX) = ALL '+'                                     
047400       IF MID-STAPAC-UT(INDX) = SPACE                                     
047500         IF INDX = 2 OR 3                                                 
047600           MOVE MFS-NUM-FAELT-RAETT   TO MOD-STAPAC-IN-ATTR(INDX)         
047700           MOVE ZERO                  TO WS-STAPAC(INDX)                  
047800         ELSE                                                             
047900           MOVE MFS-NUM-FAELT-FEL     TO MOD-STAPAC-IN-ATTR(INDX)         
048000           MOVE NEJ                   TO INDATA-SW                        
048100         END-IF                                                           
048200       ELSE                                                               
048300*OBS! DATA FRÅN UT-FÄLT I MIDDEN (MID-STAPAC-UT) BLIR HÄR                 
048400*     IN-DATA OCH ANVÄNDS I ARBETS-FÄLT (WS-STAPAC).                      
048500*     (DATA I ARB-FÄLTET ANVÄNDS SEDAN VID UPPDATERING AV HELA            
048600*      RADEN ÄVEN OM DET ÄR ETT ANNAT IN-FÄLT PÅ RADEN SOM HAR            
048700*      SKRIVEN IN-DATA.)                                                  
048800         MOVE MID-STAPAC-UT(INDX)     TO DEC-IDFRIDATA                    
048900         PERFORM S03-FORMATERING-MED-RDECDATA                             
049000         MOVE DEC-IDEDITDATA          TO WS-NUMEDIT                       
049100         MOVE WS-NUMFYRA              TO WS-STAPAC(INDX)                  
049200         MOVE MFS-NUM-FAELT-RAETT     TO MOD-STAPAC-IN-ATTR(INDX)         
049300         MOVE ZERO                    TO DEC-IDFRIDATA                    
049400       END-IF                                                             
049500     ELSE                                                                 
049600         MOVE MID-STAPAC-IN(INDX)     TO DEC-IDFRIDATA                    
049700         PERFORM S03-FORMATERING-MED-RDECDATA                             
049800         IF DEC-KDSVAR-OK                                                 
049900           MOVE DEC-IDEDITDATA        TO WS-KONTROLLTID-ED                
050000           IF TIMMA-OK AND MINUT-OK                                       
050100             MOVE MFS-NUM-FAELT-RAETT TO MOD-STAPAC-IN-ATTR(INDX)         
050200             MOVE DEC-IDEDITDATA      TO WS-NUMEDIT                       
050300             MOVE WS-NUMFYRA          TO WS-STAPAC(INDX)                  
050400           ELSE                                                           
050500             MOVE MFS-NUM-FAELT-FEL   TO MOD-STAPAC-IN-ATTR(INDX)         
050600             MOVE NEJ                 TO INDATA-SW                        
050700           END-IF                                                         
050800         ELSE                                                             
050900           MOVE MID-STAPAC-IN(INDX)   TO WS-STAPAC(INDX)                  
051000           MOVE MFS-NUM-FAELT-FEL     TO MOD-STAPAC-IN-ATTR(INDX)         
051100           MOVE NEJ                   TO INDATA-SW                        
051200         END-IF                                                           
051300     END-IF                                                               
051400                                                                          
052700     .                                                                    
052800 CD-KONTROLL-STOPAC SECTION.                                              
052900                                                                          
053000     IF MID-STOPAC-IN(INDX) = ALL '+'                                     
053100       IF MID-STOPAC-UT(INDX) = SPACE                                     
053200         IF INDX = 2 OR 3                                                 
053300           MOVE MFS-NUM-FAELT-RAETT   TO MOD-STOPAC-IN-ATTR(INDX)         
053400           MOVE ZERO                  TO WS-NUMEDIT                       
053500           MOVE WS-NUMFYRA            TO WS-STOPAC(INDX)                  
053600         ELSE                                                             
053700           MOVE MFS-NUM-FAELT-FEL     TO MOD-STOPAC-IN-ATTR(INDX)         
053800           MOVE NEJ                   TO INDATA-SW                        
053900         END-IF                                                           
054000       ELSE                                                               
054100         MOVE MID-STOPAC-UT(INDX)     TO DEC-IDFRIDATA                    
054200         PERFORM S03-FORMATERING-MED-RDECDATA                             
054300         MOVE DEC-IDEDITDATA          TO WS-NUMEDIT                       
054400         MOVE WS-NUMFYRA              TO WS-STOPAC(INDX)                  
054500         MOVE MFS-NUM-FAELT-RAETT     TO MOD-STOPAC-IN-ATTR(INDX)         
054600       END-IF                                                             
054700     ELSE                                                                 
054800         MOVE MID-STOPAC-IN(INDX)     TO DEC-IDFRIDATA                    
054900         PERFORM S03-FORMATERING-MED-RDECDATA                             
055000         IF DEC-KDSVAR-OK                                                 
055100           MOVE DEC-IDEDITDATA        TO WS-KONTROLLTID-ED                
055200           IF TIMMA-OK AND MINUT-OK                                       
055300             MOVE MFS-NUM-FAELT-RAETT TO MOD-STOPAC-IN-ATTR(INDX)         
055400             MOVE DEC-IDEDITDATA      TO WS-NUMEDIT                       
055500             MOVE WS-NUMFYRA          TO WS-STOPAC(INDX)                  
055600           ELSE                                                           
055700             MOVE MFS-NUM-FAELT-FEL   TO MOD-STOPAC-IN-ATTR(INDX)         
055800             MOVE NEJ                 TO INDATA-SW                        
055900           END-IF                                                         
056000         ELSE                                                             
056100           MOVE MID-STOPAC-IN(INDX)   TO WS-STOPAC(INDX)                  
056200           MOVE MFS-NUM-FAELT-FEL     TO MOD-STOPAC-IN-ATTR(INDX)         
056300           MOVE NEJ                   TO INDATA-SW                        
056400         END-IF                                                           
056500     END-IF                                                               
056600                                                                          
056700     IF WS-STOPAC(INDX) > WS-STAPAC(INDX)                                 
056800       CONTINUE                                                           
056900     ELSE                                                                 
057000       IF WS-STOPAC(INDX) < +0.1 AND                                      
057100          WS-STAPAC(INDX) < +0.1                                          
057200         CONTINUE                                                         
057300       ELSE                                                               
057400         IF MID-STAPAC-IN(INDX) NOT = ALL '+' AND                         
057500            MID-STOPAC-IN(INDX) NOT = ALL '+'                             
057600             MOVE NEJ                  TO INDATA-SW                       
057700             MOVE MFS-NUM-FAELT-FEL    TO MOD-STAPAC-IN-ATTR(INDX)        
057800                                          MOD-STOPAC-IN-ATTR(INDX)        
057900         END-IF                                                           
058000       END-IF                                                             
058100     END-IF                                                               
058200                                                                          
059400     .                                                                    
059500 CE-KONTROLL-STAADM SECTION.                                              
059600                                                                          
059700     IF MID-STAADM-IN(INDX) = ALL '+'                                     
059800       IF MID-STAADM-UT(INDX) = SPACE                                     
059900         IF INDX = 2 OR 3                                                 
060000           MOVE MFS-NUM-FAELT-RAETT   TO MOD-STAADM-IN-ATTR(INDX)         
060100           MOVE ZERO                  TO WS-NUMEDIT                       
060200           MOVE WS-NUMFYRA            TO WS-STAADM(INDX)                  
060300         ELSE                                                             
060400           MOVE MFS-NUM-FAELT-FEL     TO MOD-STAADM-IN-ATTR(INDX)         
060500           MOVE NEJ                   TO INDATA-SW                        
060600         END-IF                                                           
060700       ELSE                                                               
060800         MOVE MID-STAADM-UT(INDX)     TO DEC-IDFRIDATA                    
060900         PERFORM S03-FORMATERING-MED-RDECDATA                             
061000         MOVE DEC-IDEDITDATA          TO WS-NUMEDIT                       
061100         MOVE WS-NUMFYRA              TO WS-STAADM(INDX)                  
061200         MOVE MFS-NUM-FAELT-RAETT     TO MOD-STAADM-IN-ATTR(INDX)         
061300       END-IF                                                             
061400     ELSE                                                                 
061500         MOVE MID-STAADM-IN(INDX)     TO DEC-IDFRIDATA                    
061600         PERFORM S03-FORMATERING-MED-RDECDATA                             
061700         IF DEC-KDSVAR-OK                                                 
061800           MOVE DEC-IDEDITDATA        TO WS-KONTROLLTID-ED                
061900           IF TIMMA-OK AND MINUT-OK                                       
062000             MOVE MFS-NUM-FAELT-RAETT TO MOD-STAADM-IN-ATTR(INDX)         
062100             MOVE DEC-IDEDITDATA      TO WS-NUMEDIT                       
062200             MOVE WS-NUMFYRA          TO WS-STAADM(INDX)                  
062300           ELSE                                                           
062400             MOVE MFS-NUM-FAELT-FEL   TO MOD-STAADM-IN-ATTR(INDX)         
062500             MOVE NEJ                 TO INDATA-SW                        
062600           END-IF                                                         
062700         ELSE                                                             
062800           MOVE MFS-NUM-FAELT-FEL     TO MOD-STAADM-IN-ATTR(INDX)         
062900           MOVE NEJ                   TO INDATA-SW                        
063000         END-IF                                                           
063100     END-IF                                                               
063200                                                                          
064400     .                                                                    
064500 CF-KONTROLL-STOADM SECTION.                                              
064600                                                                          
064700     IF MID-STOADM-IN(INDX) = ALL '+'                                     
064800       IF MID-STOADM-UT(INDX) = SPACE                                     
064900         IF INDX = 2 OR 3                                                 
065000           MOVE MFS-NUM-FAELT-RAETT   TO MOD-STOADM-IN-ATTR(INDX)         
065100           MOVE ZERO                  TO WS-NUMEDIT                       
065200           MOVE WS-NUMFYRA            TO WS-STOADM(INDX)                  
065300         ELSE                                                             
065400           MOVE MFS-NUM-FAELT-FEL     TO MOD-STOADM-IN-ATTR(INDX)         
065500           MOVE NEJ                   TO INDATA-SW                        
065600         END-IF                                                           
065700       ELSE                                                               
065800         MOVE MID-STOADM-UT(INDX)     TO DEC-IDFRIDATA                    
065900         PERFORM S03-FORMATERING-MED-RDECDATA                             
066000         MOVE DEC-IDEDITDATA          TO WS-NUMEDIT                       
066100         MOVE WS-NUMFYRA              TO WS-STOADM(INDX)                  
066200         MOVE MFS-NUM-FAELT-RAETT     TO MOD-STOADM-IN-ATTR(INDX)         
066300       END-IF                                                             
066400     ELSE                                                                 
066500         MOVE MID-STOADM-IN(INDX)     TO DEC-IDFRIDATA                    
066600         PERFORM S03-FORMATERING-MED-RDECDATA                             
066700         IF DEC-KDSVAR-OK                                                 
066800           MOVE DEC-IDEDITDATA        TO WS-KONTROLLTID-ED                
066900           IF TIMMA-OK AND MINUT-OK                                       
067000             MOVE MFS-NUM-FAELT-RAETT TO MOD-STOADM-IN-ATTR(INDX)         
067100             MOVE DEC-IDEDITDATA      TO WS-NUMEDIT                       
067200             MOVE WS-NUMFYRA          TO WS-STOADM(INDX)                  
067300           ELSE                                                           
067400             MOVE MFS-NUM-FAELT-FEL   TO MOD-STOADM-IN-ATTR(INDX)         
067500             MOVE NEJ                 TO INDATA-SW                        
067600           END-IF                                                         
067700         ELSE                                                             
067800           MOVE MFS-NUM-FAELT-FEL     TO MOD-STOADM-IN-ATTR(INDX)         
067900           MOVE NEJ                   TO INDATA-SW                        
068000         END-IF                                                           
068100     END-IF                                                               
068200                                                                          
068300       IF WS-STOADM(INDX) > WS-STAADM(INDX)                               
068400         CONTINUE                                                         
068500       ELSE                                                               
068600         IF WS-STOADM(INDX) < +0.1 AND                                    
068700            WS-STAADM(INDX) < +0.1                                        
068800           CONTINUE                                                       
068900         ELSE                                                             
069000           IF MID-STAADM-IN(INDX) NOT = ALL '+' AND                       
069100              MID-STOADM-IN(INDX) NOT = ALL '+'                           
069200               MOVE NEJ               TO INDATA-SW                        
069300               MOVE MFS-NUM-FAELT-FEL TO MOD-STAADM-IN-ATTR(INDX)         
069400                                         MOD-STOADM-IN-ATTR(INDX)         
069500           END-IF                                                         
069600         END-IF                                                           
069700       END-IF                                                             
069800                                                                          
071000     .                                                                    
071100 CG-KONTROLL-STALAST SECTION.                                             
071200                                                                          
071300     IF MID-STALAST-IN(INDX) = ALL '+'                                    
071400       IF MID-STALAST-UT(INDX) = SPACE                                    
071500         IF INDX = 2 OR 3                                                 
071600           MOVE MFS-NUM-FAELT-RAETT   TO MOD-STALAST-IN-ATTR(INDX)        
071700           MOVE ZERO                  TO WS-NUMEDIT                       
071800           MOVE WS-NUMFYRA             TO WS-STALAST(INDX)                
071900         ELSE                                                             
072000           MOVE MFS-NUM-FAELT-FEL     TO MOD-STALAST-IN-ATTR(INDX)        
072100           MOVE NEJ                   TO INDATA-SW                        
072200         END-IF                                                           
072300       ELSE                                                               
072400         MOVE MID-STALAST-UT(INDX)    TO DEC-IDFRIDATA                    
072500         PERFORM S03-FORMATERING-MED-RDECDATA                             
072600         MOVE DEC-IDEDITDATA           TO WS-NUMEDIT                      
072700         MOVE WS-NUMFYRA               TO WS-STALAST(INDX)                
072800         MOVE MFS-NUM-FAELT-RAETT     TO MOD-STALAST-IN-ATTR(INDX)        
072900       END-IF                                                             
073000     ELSE                                                                 
073100         MOVE MID-STALAST-IN(INDX)    TO DEC-IDFRIDATA                    
073200         PERFORM S03-FORMATERING-MED-RDECDATA                             
073300         IF DEC-KDSVAR-OK                                                 
073400           MOVE DEC-IDEDITDATA        TO WS-KONTROLLTID-ED                
073500           IF TIMMA-OK AND MINUT-OK                                       
073600            MOVE MFS-NUM-FAELT-RAETT TO MOD-STALAST-IN-ATTR(INDX)         
073700             MOVE DEC-IDEDITDATA      TO WS-NUMEDIT                       
073800             MOVE WS-NUMFYRA          TO WS-STALAST(INDX)                 
073900           ELSE                                                           
074000             MOVE MFS-NUM-FAELT-FEL   TO MOD-STALAST-IN-ATTR(INDX)        
074100             MOVE NEJ                 TO INDATA-SW                        
074200           END-IF                                                         
074300         ELSE                                                             
074400           MOVE MFS-NUM-FAELT-FEL     TO MOD-STALAST-IN-ATTR(INDX)        
074500           MOVE NEJ                   TO INDATA-SW                        
074600         END-IF                                                           
074700     END-IF                                                               
074800                                                                          
076000     .                                                                    
076100 CH-KONTROLL-STOLAST SECTION.                                             
076200                                                                          
076300     IF MID-STOLAST-IN(INDX) = ALL '+'                                    
076400       IF MID-STOLAST-UT(INDX) = SPACE                                    
076500         IF INDX = 2 OR 3                                                 
076600           MOVE MFS-NUM-FAELT-RAETT   TO MOD-STOLAST-IN-ATTR(INDX)        
076700           MOVE ZERO                  TO WS-STOLAST(INDX)                 
076800         ELSE                                                             
076900           MOVE MFS-NUM-FAELT-FEL     TO MOD-STOLAST-IN-ATTR(INDX)        
077000           MOVE NEJ                   TO INDATA-SW                        
077100         END-IF                                                           
077200       ELSE                                                               
077300         MOVE MID-STOLAST-UT(INDX)    TO DEC-IDFRIDATA                    
077400         PERFORM S03-FORMATERING-MED-RDECDATA                             
077500         MOVE DEC-IDEDITDATA           TO WS-NUMEDIT                      
077600         MOVE WS-NUMFYRA               TO WS-STOLAST(INDX)                
077700         MOVE MFS-NUM-FAELT-RAETT     TO MOD-STOLAST-IN-ATTR(INDX)        
077800       END-IF                                                             
077900     ELSE                                                                 
078000         MOVE MID-STOLAST-IN(INDX)    TO DEC-IDFRIDATA                    
078100         PERFORM S03-FORMATERING-MED-RDECDATA                             
078200         IF DEC-KDSVAR-OK                                                 
078300           MOVE DEC-IDEDITDATA        TO WS-KONTROLLTID-ED                
078400           IF TIMMA-OK AND MINUT-OK                                       
078500            MOVE MFS-NUM-FAELT-RAETT TO MOD-STOLAST-IN-ATTR(INDX)         
078600             MOVE DEC-IDEDITDATA      TO WS-NUMEDIT                       
078700             MOVE WS-NUMFYRA          TO WS-STOLAST(INDX)                 
078800           ELSE                                                           
078900             MOVE MFS-NUM-FAELT-FEL   TO MOD-STOLAST-IN-ATTR(INDX)        
079000             MOVE NEJ                 TO INDATA-SW                        
079100           END-IF                                                         
079200         ELSE                                                             
079300           MOVE MFS-NUM-FAELT-FEL     TO MOD-STOLAST-IN-ATTR(INDX)        
079400           MOVE NEJ                   TO INDATA-SW                        
079500         END-IF                                                           
079600     END-IF                                                               
079700       IF WS-STOLAST(INDX) >                                              
079800          WS-STALAST(INDX)                                                
079900         CONTINUE                                                         
080000       ELSE                                                               
080100         IF WS-STOLAST(INDX) < +0.1 AND                                   
080200            WS-STALAST(INDX) < +0.1                                       
080300           CONTINUE                                                       
080400         ELSE                                                             
080500           IF MID-STALAST-IN(INDX) NOT = ALL '+' AND                      
080600              MID-STOLAST-IN(INDX) NOT = ALL '+'                          
080700              MOVE NEJ               TO INDATA-SW                         
080800              MOVE MFS-NUM-FAELT-FEL TO MOD-STALAST-IN-ATTR(INDX)         
080900                                       MOD-STOLAST-IN-ATTR(INDX)          
081000           END-IF                                                         
081100         END-IF                                                           
081200       END-IF                                                             
081300                                                                          
082500     .                                                                    
082600     EJECT                                                                
082700 CI-MOVE-WS-X-TO-MOD SECTION.                                             
082800                                                                          
082900     IF MID-STAPAC-UT(INDX) = SPACE                                       
083000       MOVE WS-STAPAC(INDX)          TO MOD-STAPAC-IN(INDX)               
083100     END-IF                                                               
083200                                                                          
083300     IF MID-STOPAC-UT(INDX) = SPACE                                       
083400       MOVE WS-STOPAC(INDX)          TO MOD-STOPAC-IN(INDX)               
083500     END-IF                                                               
083600                                                                          
083700     IF MID-STAADM-UT(INDX) = SPACE                                       
083800       MOVE WS-STAADM(INDX)          TO MOD-STAADM-IN(INDX)               
083900     END-IF                                                               
084000                                                                          
084100     IF MID-STOADM-UT(INDX) = SPACE                                       
084200       MOVE WS-STOADM(INDX)          TO MOD-STOADM-IN(INDX)               
084300     END-IF                                                               
084400                                                                          
084500     IF MID-STALAST-UT(INDX) = SPACE                                      
084600       MOVE WS-STALAST(INDX)         TO MOD-STALAST-IN(INDX)              
084700     END-IF                                                               
084800                                                                          
084900     IF MID-STOLAST-UT(INDX) = SPACE                                      
085000       MOVE WS-STOLAST(INDX)         TO MOD-STOLAST-IN(INDX)              
085100     END-IF                                                               
085200       .                                                                  
085300 CJ-KONTROLL-AV-BORTTAG SECTION.                                          
085400                                                                          
085500         IF MID-FLTABORT = 'J' OR MID-FLTABORT = 'Y'                      
085600           IF WS-IDPRC = '9999'                                           
085700             MOVE NEJ                     TO INDATA-SW                    
085800             MOVE MFS-ALFA-FAELT-FEL      TO MOD-FLTABORT-ATTR            
085900           END-IF                                                         
086000           IF MID-RAD-TOM                                                 
086100             CONTINUE                                                     
086200           ELSE                                                           
086300             MOVE NEJ                     TO INDATA-SW                    
086400             MOVE MFS-ALFA-FAELT-FEL      TO MOD-FLTABORT-ATTR            
086500           END-IF                                                         
086600         ELSE                                                             
086700           MOVE NEJ                     TO INDATA-SW                      
086800           MOVE MFS-ALFA-FAELT-FEL      TO MOD-FLTABORT-ATTR              
086900         END-IF                                                           
087000     .                                                                    
087100 D-UPPDATERA SECTION.                                                     
087200                                                                          
087300     IF MID-FLTABORT = ALL '+'                                            
087400       IF MID-RAD-TOM                                                     
087500         PERFORM DC-START-BMP-W41321                                      
087600       ELSE                                                               
087700         PERFORM IMS-GU-WLXXKC01                                          
087800                                                                          
087900         MOVE +1                        TO INDX                           
088000         PERFORM UNTIL INDX > MAX-INDX                                    
088100           IF MID-RAD(INDX) = ALL '+'                                     
088200             MOVE NEJ                   TO UPPDAT-SW                      
088300           ELSE                                                           
088400             PERFORM DA-MOVE-TO-IOAREA1                                   
088500             PERFORM IMS-ISRT-WLXXKC11                                    
088600             IF SEGMENT-FINNS-REDAN                                       
088700               PERFORM DB-MOVE-TO-IOAREA2                                 
088800               PERFORM IMS-GHN-MIN-MAX-WLXXKC11                           
088900               PERFORM IMS-REPL-WLXXKC                                    
089000             END-IF                                                       
089100           END-IF                                                         
089200           ADD +1                       TO INDX                           
089300         END-PERFORM                                                      
089400         IF MID-TIUPDATE-IN NOT = ALL '+'                                 
089500           PERFORM DC-START-BMP-W41321                                    
089600         END-IF                                                           
089700       END-IF                                                             
089800     ELSE                                                                 
089900       IF MID-FLTABORT = 'J' OR MID-FLTABORT = 'J' OR                     
090000          MID-FLTABORT = 'Y' OR MID-FLTABORT = 'Y'                        
090100         PERFORM IMS-GU-WLXXKC01                                          
090200         IF SEGMENT-FINNS                                                 
090300           MOVE +1                     TO INDX                            
090400           PERFORM IMS-GHN-MIN-MAX-WLXXKC11                               
090500           IF SEGMENT-FINNS                                               
090600             PERFORM UNTIL INDX > MAX-INDX                                
090700               PERFORM IMS-DLET-WLXXKC                                    
090800               PERFORM IMS-GHN-MIN-MAX-WLXXKC11                           
090900               ADD +1                     TO INDX                         
091000             END-PERFORM                                                  
091100             MOVE JA                      TO UPPDAT-SW                    
091200             MOVE NEJ                     TO ALLT-SW                      
091300             MOVE MFS-ALFA-FAELT-RAETT    TO MOD-FLTABORT                 
091400           ELSE                                                           
091500             MOVE NEJ                     TO UPPDAT-SW                    
091600             MOVE MFS-ALFA-FAELT-RAETT    TO MOD-FLTABORT                 
091700           END-IF                                                         
091800         ELSE                                                             
091900           MOVE NEJ                     TO UPPDAT-SW                      
092000           MOVE MFS-ALFA-FAELT-RAETT    TO MOD-FLTABORT                   
092100         END-IF                                                           
092200       ELSE                                                               
092300         MOVE NEJ                       TO UPPDAT-SW                      
092400         MOVE MFS-ALFA-FAELT-FEL        TO MOD-FLTABORT                   
092500       END-IF                                                             
092600     END-IF                                                               
092700                                                                          
092800     IF UPPDAT-OK                                                         
092900       MOVE INF-UPDATE-DONE           TO MED-IDMFSINF                     
093000       PERFORM S02-INF-RUTINE                                             
093100       PERFORM MFS-RENSA-FAELT-IN                                         
093200     ELSE                                                                 
093300       MOVE INF-NO-UPDATE             TO MED-IDMFSINF                     
093400       PERFORM S02-INF-RUTINE                                             
093500       PERFORM MFS-ROER-EJ-FAELT-IN                                       
093600     END-IF                                                               
093700     .                                                                    
093800     EJECT                                                                
093900 DA-MOVE-TO-IOAREA1 SECTION.                                              
094000                                                                          
094100     PERFORM DAA-MOVE-TO-KVKALTIM                                         
094200                                                                          
094300     MOVE WS-IDPRC                  TO IOAREA1-4436-IDPRC                 
094400     MOVE LOW-VALUE                 TO IOAREA1-4436-LOW-VALUE             
094500     MOVE WS-STAPAC(INDX)           TO IOAREA1-4436-TISTAMIN-PAC          
094600     MOVE WS-STOPAC(INDX)           TO IOAREA1-4436-TISTOMIN-PAC          
094700     MOVE WS-STAADM(INDX)           TO IOAREA1-4436-TISTAMIN-ADM          
094800     MOVE WS-STOADM(INDX)           TO IOAREA1-4436-TISTOMIN-ADM          
094900     MOVE WS-STALAST(INDX)          TO IOAREA1-4436-TISTAMIN-LAST         
095000     MOVE WS-STOLAST(INDX)          TO IOAREA1-4436-TISTOMIN-LAST         
095100     MOVE SPACE                     TO IOAREA1-4436-FILLER                
095200     .                                                                    
095300     EJECT                                                                
095400 DAA-MOVE-TO-KVKALTIM SECTION.                                            
095500                                                                          
095600     IF INDX = 1                                                          
095700       MOVE +8                        TO IOAREA1-4436-KVKALTIM            
095800     ELSE                                                                 
095900       IF INDX = 2                                                        
096000         MOVE +5                      TO IOAREA1-4436-KVKALTIM            
096100       ELSE                                                               
096200         IF INDX = 3                                                      
096300           MOVE +0                    TO IOAREA1-4436-KVKALTIM            
096400         ELSE                                                             
096500           CONTINUE                                                       
096600         END-IF                                                           
096700       END-IF                                                             
096800     END-IF                                                               
096900     .                                                                    
097000     EJECT                                                                
097100 DB-MOVE-TO-IOAREA2 SECTION.                                              
097200                                                                          
097300     PERFORM DBA-MOVE-TO-KVKALTIM                                         
097400     MOVE WS-IDPRC                  TO IOAREA2-4436-IDPRC                 
097500     MOVE LOW-VALUE                 TO IOAREA2-4436-LOW-VALUE             
097600     MOVE WS-STAPAC(INDX)           TO IOAREA2-4436-TISTAMIN-PAC          
097700     MOVE WS-STOPAC(INDX)           TO IOAREA2-4436-TISTOMIN-PAC          
097800     MOVE WS-STAADM(INDX)           TO IOAREA2-4436-TISTAMIN-ADM          
097900     MOVE WS-STOADM(INDX)           TO IOAREA2-4436-TISTOMIN-ADM          
098000     MOVE WS-STALAST(INDX)          TO IOAREA2-4436-TISTAMIN-LAST         
098100     MOVE WS-STOLAST(INDX)          TO IOAREA2-4436-TISTOMIN-LAST         
098200     MOVE SPACE                     TO IOAREA2-4436-FILLER                
098300     .                                                                    
098400     EJECT                                                                
098500 DBA-MOVE-TO-KVKALTIM SECTION.                                            
098600                                                                          
098700     IF INDX = 1                                                          
098800       MOVE +8                        TO IOAREA2-4436-KVKALTIM            
098900     ELSE                                                                 
099000                                                                          
099100       IF INDX = 2                                                        
099200         MOVE +5                      TO IOAREA2-4436-KVKALTIM            
099300       ELSE                                                               
099400                                                                          
099500         IF INDX = 3                                                      
099600           MOVE +0                    TO IOAREA2-4436-KVKALTIM            
099700         ELSE                                                             
099800           CONTINUE                                                       
099900         END-IF                                                           
100000       END-IF                                                             
100100     END-IF                                                               
100200     .                                                                    
100300 DC-START-BMP-W41321  SECTION.                                            
100400                                                                          
100500            STRING 'IDPRC(' WS-IDPRC ') IDDC(' WS-IDDC ') TIUPDATE        
100600-                   '(' WS-START-DATUM ') KDCALL(' WS-KDCALL ')'          
100700     DELIMITED BY SIZE INTO MSGSOP-TESYMBV                                
100800     PERFORM IMS-INSERT-ALTMSG                                            
100900     .                                                                    
101000 E-INFAELT-KONTROLL SECTION.                                              
101100                                                                          
101200     IF NYA-NYCKLAR                                                       
101300       PERFORM MFS-RENSA-FAELT-IN                                         
101400       PERFORM MFS-RENSA-FAELT-UT                                         
101500     ELSE                                                                 
101600       IF MID-FLTABORT = ALL '+'                                          
101700         MOVE +1                          TO INDX                         
101800                                                                          
101900         PERFORM UNTIL INDX > MAX-INDX                                    
102000           PERFORM EA-MID-RAD-KONTROLL                                    
102100           IF MID-RAD-TOM                                                 
102200             CONTINUE                                                     
102300           ELSE                                                           
102400             MOVE NEJ                     TO ALLT-SW                      
102500             PERFORM EB-INDATA-BEHANDLING                                 
102600           END-IF                                                         
102700           ADD +1                         TO INDX                         
102800         END-PERFORM                                                      
102900                                                                          
103000         IF MID-TIUPDATE-IN NOT = ALL '+'                                 
103100           MOVE MFS-ADD-LAES-IN-FAELT  TO MOD-TIUPDATE-ATTR               
103200           MOVE NEJ                    TO ALLT-SW                         
103300         END-IF                                                           
103400       ELSE                                                               
103500         MOVE NEJ                         TO ALLT-SW                      
103600         MOVE MFS-ADD-LAES-IN-FAELT       TO MOD-FLTABORT-ATTR            
103700       END-IF                                                             
103800     END-IF                                                               
103900                                                                          
104000     IF ALLT-OK                                                           
104100       CONTINUE                                                           
104200     ELSE                                                                 
104300       MOVE INF-PRESS-PF11                TO MED-IDMFSINF                 
104400       PERFORM S02-INF-RUTINE                                             
104500       PERFORM MFS-ROER-EJ-FAELT-IN                                       
104600       PERFORM MFS-ROER-EJ-FAELT-UT                                       
104700     END-IF                                                               
104800     .                                                                    
104900 EA-MID-RAD-KONTROLL SECTION.                                             
105000                                                                          
105100     IF MID-STAPAC-IN(INDX) = ALL '+'  AND                                
105200        MID-STOPAC-IN(INDX) = ALL '+'  AND                                
105300        MID-STAADM-IN(INDX) = ALL '+'  AND                                
105400        MID-STOADM-IN(INDX) = ALL '+'  AND                                
105500        MID-STALAST-IN(INDX) = ALL '+'  AND                               
105600        MID-STOLAST-IN(INDX) = ALL '+'                                    
105700       CONTINUE                                                           
105800     ELSE                                                                 
105900       MOVE NEJ                       TO MID-RAD-SW                       
106000     END-IF                                                               
106100     .                                                                    
106200 EB-INDATA-BEHANDLING SECTION.                                            
106300                                                                          
106400     IF MID-STAPAC-IN(INDX) NOT = ALL '+'                                 
106500       MOVE MFS-ADD-LAES-IN-FAELT  TO MOD-STAPAC-IN-ATTR(INDX)            
106600       MOVE NEJ                       TO MID-RAD-SW                       
106700     END-IF                                                               
106800                                                                          
106900     IF MID-STOPAC-IN(INDX) NOT = ALL '+'                                 
107000       MOVE MFS-ADD-LAES-IN-FAELT  TO MOD-STOPAC-IN-ATTR(INDX)            
107100       MOVE NEJ                       TO MID-RAD-SW                       
107200     END-IF                                                               
107300                                                                          
107400     IF MID-STAADM-IN(INDX) NOT = ALL '+'                                 
107500       MOVE MFS-ADD-LAES-IN-FAELT  TO MOD-STAADM-IN-ATTR(INDX)            
107600       MOVE NEJ                       TO MID-RAD-SW                       
107700     END-IF                                                               
107800                                                                          
107900     IF MID-STOADM-IN(INDX) NOT = ALL '+'                                 
108000       MOVE MFS-ADD-LAES-IN-FAELT  TO MOD-STOADM-IN-ATTR(INDX)            
108100       MOVE NEJ                       TO MID-RAD-SW                       
108200     END-IF                                                               
108300                                                                          
108400     IF MID-STALAST-IN(INDX) NOT = ALL '+'                                
108500       MOVE MFS-ADD-LAES-IN-FAELT  TO MOD-STALAST-IN-ATTR(INDX)           
108600       MOVE NEJ                       TO MID-RAD-SW                       
108700     END-IF                                                               
108800                                                                          
108900     IF MID-STOLAST-IN(INDX) NOT = ALL '+'                                
109000       MOVE MFS-ADD-LAES-IN-FAELT  TO MOD-STOLAST-IN-ATTR(INDX)           
109100       MOVE NEJ                       TO MID-RAD-SW                       
109200     END-IF                                                               
109300       .                                                                  
109400 F-LAES-VISA-INFO SECTION.                                                
109500                                                                          
109600     PERFORM IMS-GU-WLXXKC01                                              
109700     IF SEGMENT-FINNS                                                     
109800       PERFORM IMS-GNP-MIN-MAX-WLXXKC11                                   
109900       IF SEGMENT-FINNS                                                   
110000         MOVE +3                        TO INDX                           
110100                                                                          
110200         PERFORM UNTIL INDX < +1 OR SEGMENT-SAKNAS                        
110300           PERFORM FA-MOVE-TO-MOD                                         
110400           SUBTRACT +1                  FROM INDX                         
110500           PERFORM IMS-GNP-MIN-MAX-WLXXKC11                               
110600         END-PERFORM                                                      
110700                                                                          
110800       ELSE                                                               
110900         MOVE ERR-KEYS-ARE-MISSING      TO MED-IDMFSFEL                   
111000         PERFORM S01-ERR-RUTINE                                           
111100         PERFORM MFS-RENSA-FAELT-UT                                       
111200       END-IF                                                             
111300     ELSE                                                                 
111400       MOVE ERR-KEYS-ARE-MISSING      TO MED-IDMFSFEL                     
111500       PERFORM S01-ERR-RUTINE                                             
111600       PERFORM MFS-RENSA-FAELT-UT                                         
111700     END-IF                                                               
111800     PERFORM MFS-RENSA-FAELT-IN                                           
111900     .                                                                    
112000     EJECT                                                                
112100 FA-MOVE-TO-MOD SECTION.                                                  
112200                                                                          
112300     MOVE IOAREA1-4436-TISTAMIN-PAC   TO WS-NUMFYRA                       
112500     MOVE WS-NUMEDIT                  TO MOD-STAPAC-UT(INDX)              
112600                                                                          
112700     MOVE IOAREA1-4436-TISTOMIN-PAC   TO WS-NUMFYRA                       
112900     MOVE WS-NUMEDIT                  TO MOD-STOPAC-UT(INDX)              
113000                                                                          
113100     MOVE IOAREA1-4436-TISTAMIN-ADM   TO WS-NUMFYRA                       
113300     MOVE WS-NUMEDIT                  TO MOD-STAADM-UT(INDX)              
113400                                                                          
113500     MOVE IOAREA1-4436-TISTOMIN-ADM   TO WS-NUMFYRA                       
113700     MOVE WS-NUMEDIT                  TO MOD-STOADM-UT(INDX)              
113800                                                                          
113900     MOVE IOAREA1-4436-TISTAMIN-LAST TO WS-NUMFYRA                        
114100     MOVE WS-NUMEDIT                 TO MOD-STALAST-UT(INDX)              
114200                                                                          
114300     MOVE IOAREA1-4436-TISTOMIN-LAST TO WS-NUMFYRA                        
114500     MOVE WS-NUMEDIT                 TO MOD-STOLAST-UT(INDX)              
114600     .                                                                    
114700 S01-ERR-RUTINE SECTION.                                                  
114800     CALL WMEDKONV USING MED-WMEDAREA                                     
114900     MOVE MED-MFSFEL                  TO MOD-TEMFSFEL                     
115000     .                                                                    
115100 S02-INF-RUTINE SECTION.                                                  
115200     CALL WMEDKONV USING MED-WMEDAREA                                     
115300     MOVE MED-MFSINF                  TO MOD-TEMFSINF                     
115400     .                                                                    
115500 S03-FORMATERING-MED-RDECDATA SECTION.                                    
115600     MOVE +2                        TO DEC-KVHELTAL                       
115700     MOVE +2                        TO DEC-KVDECIMAL                      
115800     CALL WDECEDIT USING DEC-WDECAREA                                     
115900     .                                                                    
118200     SKIP2                                                                
118300 MFS-RENSA-FAELT-UT SECTION.                                              
118400                                                                          
118500*    --- ALLA UTDATA-FÄLT                                                 
118600     MOVE MFS-RENSA-FAELT             TO MOD-FLTABORT                     
118700                                         MOD-TIUPDATE-UT                  
118800     MOVE +1                          TO INDX                             
118900     PERFORM UNTIL INDX > MAX-INDX                                        
119000       MOVE MFS-RENSA-FAELT           TO MOD-STAPAC-UT(INDX)              
119100                                         MOD-STOPAC-UT(INDX)              
119200                                         MOD-STAADM-UT(INDX)              
119300                                         MOD-STOADM-UT(INDX)              
119400                                         MOD-STALAST-UT(INDX)             
119500                                         MOD-STOLAST-UT(INDX)             
119600     ADD +1                           TO INDX                             
119700     END-PERFORM                                                          
119800     .                                                                    
119900 MFS-RENSA-FAELT-IN SECTION.                                              
120000                                                                          
120100*    --- ALLA INDATA-FÄLT                                                 
120200     MOVE MFS-RENSA-FAELT             TO MOD-FLTABORT                     
120300                                         MOD-TIUPDATE-IN                  
120400     MOVE +1                          TO INDX                             
120500     PERFORM UNTIL INDX > MAX-INDX                                        
120600       MOVE MFS-RENSA-FAELT           TO MOD-STAPAC-IN(INDX)              
120700                                         MOD-STOPAC-IN(INDX)              
120800                                         MOD-STAADM-IN(INDX)              
120900                                         MOD-STOADM-IN(INDX)              
121000                                         MOD-STALAST-IN(INDX)             
121100                                         MOD-STOLAST-IN(INDX)             
121200     ADD +1                           TO INDX                             
121300     END-PERFORM                                                          
121400     .                                                                    
121500     EJECT                                                                
121600 MFS-ROER-EJ-FAELT-UT  SECTION.                                           
121700                                                                          
121800*    --- ALLA UTDATA-FÄLT                                                 
121900     MOVE MFS-ROER-EJ-FAELT           TO MOD-TIDATUM-UT                   
122000                                         MOD-TIUPDATE-UT                  
122100                                         MOD-FLTABORT                     
122200     MOVE +1                          TO INDX                             
122300     PERFORM UNTIL INDX > MAX-INDX                                        
122400       MOVE MFS-ROER-EJ-FAELT         TO MOD-STAPAC-UT(INDX)              
122500                                         MOD-STOPAC-UT(INDX)              
122600                                         MOD-STAADM-UT(INDX)              
122700                                         MOD-STOADM-UT(INDX)              
122800                                         MOD-STALAST-UT(INDX)             
122900                                         MOD-STOLAST-UT(INDX)             
123000     ADD +1                           TO INDX                             
123100     END-PERFORM                                                          
123200     .                                                                    
123300 MFS-ROER-EJ-FAELT-IN  SECTION.                                           
123400     MOVE MFS-ROER-EJ-FAELT           TO MOD-TIDATUM-IN                   
123500                                         MOD-TIUPDATE-IN                  
123600                                         MOD-FLTABORT                     
123700     MOVE +1                          TO INDX                             
123800     PERFORM UNTIL INDX > MAX-INDX                                        
123900       MOVE MFS-ROER-EJ-FAELT         TO MOD-STAPAC-IN(INDX)              
124000                                         MOD-STOPAC-IN(INDX)              
124100                                         MOD-STAADM-IN(INDX)              
124200                                         MOD-STOADM-IN(INDX)              
124300                                         MOD-STALAST-IN(INDX)             
124400                                         MOD-STOLAST-IN(INDX)             
124500     ADD +1                           TO INDX                             
124600     END-PERFORM                                                          
124700     .                                                                    
124800* --- IMS SEKTIONER ---                                                   
124900     SKIP3                                                                
125000 IMS-GET-MSG SECTION.                                                     
125100                                                                          
125200     MOVE '  QC' TO GODK-STATUSKODER                                      
125300     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
125400     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
125500     PERFORM IMS-STATUSKONTROLL                                           
125600     .                                                                    
125700     SKIP3                                                                
125800 IMS-INSERT-MSG SECTION.                                                  
125900                                                                          
125910     IF MSGI-IDLAND-SPR NOT = 'GB'                                        
126100       MOVE '0' TO MFS-KDHUVOMR                                           
126200     END-IF                                                               
126300     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
126400     MOVE SPACE TO GODK-STATUSKODER                                       
126500     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
126600     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
126700     PERFORM IMS-STATUSKONTROLL                                           
126800     .                                                                    
126900 IMS-INSERT-ALTMSG SECTION.                                               
127000                                                                          
127100     MOVE '  '  TO GODK-STATUSKODER                                       
127200     CALL CBLTDLI USING ISRT ALT-PCB W-PROG-TO-PROG-SW                    
127300     MOVE ALT-STATUS-CODE TO STATUS-WS                                    
127400     PERFORM IMS-STATUSKONTROLL                                           
127500     .                                                                    
127600 IMS-GU-WLXXKC01 SECTION.                                                 
127700                                                                          
127800     STRING 'WLXXKC01(WDGXKEY  =' W-WDGXKEY-4435-X ')'                    
127900          DELIMITED BY SIZE INTO SSA1                                     
128000     MOVE '  GE' TO GODK-STATUSKODER                                      
128100     CALL CBLTDLI USING GU XXKC-PCB DLI-IO-AREA1 SSA1                     
128200     MOVE XXKC-STATUS-CODE TO STATUS-WS                                   
128300     PERFORM IMS-STATUSKONTROLL                                           
128400     .                                                                    
128500 IMS-GNP-MIN-MAX-WLXXKC11 SECTION.                                        
128600                                                                          
128700     STRING 'WLXXKC11(WDGXKEY >=' W-WDGXKEY-MIN-X                         
128800                    '&WDGXKEY <=' W-WDGXKEY-MAX-X ')'                     
128900          DELIMITED BY SIZE INTO SSA1                                     
129000     MOVE '  GE' TO GODK-STATUSKODER                                      
129100     CALL CBLTDLI USING GNP XXKC-PCB DLI-IO-AREA1 SSA1                    
129200     MOVE XXKC-STATUS-CODE TO STATUS-WS                                   
129300     PERFORM IMS-STATUSKONTROLL                                           
129400     .                                                                    
129500 IMS-GHN-MIN-MAX-WLXXKC11 SECTION.                                        
129600                                                                          
129700     STRING 'WLXXKC11(WDGXKEY >=' W-WDGXKEY-MIN-X                         
129800                    '&WDGXKEY <=' W-WDGXKEY-MAX-X ')'                     
129900          DELIMITED BY SIZE INTO SSA1                                     
130000     MOVE '  GEGB' TO GODK-STATUSKODER                                    
130100     CALL CBLTDLI USING GHN XXKC-PCB DLI-IO-AREA1 SSA1                    
130200     MOVE XXKC-STATUS-CODE TO STATUS-WS                                   
130300     PERFORM IMS-STATUSKONTROLL                                           
130400     .                                                                    
130500 IMS-ISRT-WLXXKC11 SECTION.                                               
130600                                                                          
130700     STRING 'WLXXKC01(WDGXKEY  =' W-WDGXKEY-4435-X ')'                    
130800          DELIMITED BY SIZE INTO SSA1                                     
130900     MOVE 'WLXXKC11 ' TO SSA2                                             
131000     MOVE '  II' TO GODK-STATUSKODER                                      
131100     CALL CBLTDLI USING ISRT XXKC-PCB DLI-IO-AREA1 SSA1 SSA2              
131200     MOVE XXKC-STATUS-CODE TO STATUS-WS                                   
131300     PERFORM IMS-STATUSKONTROLL                                           
131400     .                                                                    
131500 IMS-REPL-WLXXKC SECTION.                                                 
131600                                                                          
131700     MOVE '  ' TO GODK-STATUSKODER                                        
131800     CALL CBLTDLI USING REPL XXKC-PCB DLI-IO-AREA2                        
131900     MOVE XXKC-STATUS-CODE TO STATUS-WS                                   
132000     PERFORM IMS-STATUSKONTROLL                                           
132100     .                                                                    
132200 IMS-DLET-WLXXKC SECTION.                                                 
132300                                                                          
132400     MOVE '  ' TO GODK-STATUSKODER                                        
132500     CALL CBLTDLI USING DLET XXKC-PCB DLI-IO-AREA1                        
132600     MOVE XXKC-STATUS-CODE TO STATUS-WS                                   
132700     PERFORM IMS-STATUSKONTROLL                                           
132800     .                                                                    
132900 IMS-STATUSKONTROLL SECTION.                                              
133000                                                                          
133100     SET STATUS-IX TO 1                                                   
133200     SEARCH GODK-STATUS                                                   
133300       AT END CALL FELLOG                                                 
133400       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                  
133500     END-SEARCH                                                           
133600     .                                                                    
133610     EJECT                                                                
