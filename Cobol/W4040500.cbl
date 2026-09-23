000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W4040500.                                                
000300 AUTHOR.         GÖRAN KJELLSON   GUIDE                                   
000400 DATE-WRITTEN.   DECEMBER  2005                                           
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNKTION:                                                            
000800*        HANTERING AV DC-STYRREGISTER WDB6                                
000900*                                                                         
001000*        PROGRAMMET UPPDATERAR WDB6, WDR6                                 
001100*                                                                         
001200*    INDATA.                                                              
001300*        TRANSAKTION: W4T405                                              
001400*        MID:         W4I40501                                            
001500*                                                                         
001600*    UTDATA.                                                              
001700*        MOD:         W4O40501                                            
001800*                                                                         
001900*    E'TRACKER: 5838822 DATED 20071106 LDC DC-EXCP                        
002000*                                                                         
002100                                                                          
002200     SKIP3                                                                
002300 ENVIRONMENT DIVISION.                                                    
002400                                                                          
002500 DATA DIVISION.                                                           
002600     EJECT                                                                
002700 WORKING-STORAGE SECTION.                                                 
002800 77  IDPGM                       PIC X(08)   VALUE 'W4040500'.            
002900 77  FELTEXT                     PIC X(80)   VALUE SPACE.                 
003000 77  CURRENT-SECTION             PIC X(16)   VALUE 'MAIN'.                
003100 77  CURRENT-IMS-SECTION         PIC X(16)   VALUE SPACE.                 
003200                                                                          
003300 77  JA                          PIC X       VALUE 'J'.                   
003400 77  NEJ                         PIC X       VALUE 'N'.                   
003500                                                                          
003600 77  ANM-IX                      PIC 9(2)    VALUE ZERO.                  
003700 77  ANM-IX-MAX                  PIC 9(2)    VALUE 34.                    
003800 77  WC-KDANMORS                 PIC X(8)    VALUE 'KDANMORS'.            
003900 77  FKN-IX                      PIC 9(2)    VALUE ZERO.                  
004000 77  FKN-IX-MAX                  PIC 9(2)    VALUE 16.                    
004100 77  WC-IDFKNGRP                 PIC X(8)    VALUE 'IDFKNGRP'.            
004200 77  ART-IX                      PIC 9(2)    VALUE ZERO.                  
004300 77  ART-IX-MAX                  PIC 9(2)    VALUE 18.                    
004400 77  WC-IDARTNR                  PIC X(8)    VALUE 'IDARTNR '.            
004401 77  DC-IX                       PIC 9(2)    VALUE ZERO.                  
004402 77  DC-IX-MAX                   PIC 9(2)    VALUE 21.                    
004410 77  WC-IDDC-EXCP                PIC X(8)    VALUE 'IDDC    '.            
004500*    --- ARBETSFÄLT FÖR AKTUELLA NYCKELVÄRDEN FRÅN SKÄRMEN                
004600                                                                          
004700 77  INDATA-SW                   PIC X       VALUE 'J'.                   
004800     88  INDATA-OK                           VALUE 'J'.                   
004900     88  INDATA-FEL                          VALUE 'N'.                   
005000                                                                          
005100 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
005200     88  NYCKLAR-OK                          VALUE 'J'.                   
005300     88  NYCKLAR-FEL                         VALUE 'N'.                   
005400                                                                          
005500 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
005600     88  EGEN-MID                            VALUE '4405'.                
005700     88  GODK-MID                            VALUE '4402'                 
005800                                                   '4403'                 
005900                                                   '0551'.                
006000     88  HELP-MID                            VALUE '0551'.                
006100     EJECT                                                                
006200*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
006300 01  GENERELLA-SUBPROGRAM.                                                
006400     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
006500     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
006600     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
006700     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
006800     03  WDECEDIT                PIC X(8)    VALUE 'WDECEDIT'.            
006900     03  W418OKOD                PIC X(8)    VALUE 'W418OKOD'.            
007000     EJECT                                                                
007100                                                                          
007200*   -COPY WDECAREA.                                                       
007300                                                                          
007400 01  FILLER                      PIC X(16) VALUE 'W418OKOD-AREA'.         
007500*   -COPY W418OKOD    -PRE OKOD-                                          
007600                                                                          
007700     EJECT                                                                
007800*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
007900*01 -COPY WMEDAREA                                                        
008000     SKIP3                                                                
008100 01  MESSAGE-CODES.                                                       
008200     03  ERR-CORR-HILITE-FLDS    PIC X(3)    VALUE '001'.                 
008300     03  INF-PRESS-PF11          PIC X(3)    VALUE '003'.                 
008400     03  ERR-UPD-NOT-ALLOWED     PIC X(3)    VALUE '007'.                 
008500     03  ERR-PF11-AND-NO-DATA    PIC X(3)    VALUE '011'.                 
008600     03  ERR-DC-MISSING          PIC X(3)    VALUE '026'.                 
008700     03  INF-UPDATE-DONE         PIC X(3)    VALUE '101'.                 
008800     03  ERR-WRONG-KEY           PIC X(3)    VALUE '401'.                 
008900     EJECT                                                                
009000*    --- PARAMETRAR TILL SUBPROGRAM W005INIT                              
009100*                                                                         
009200 01  FILLER                      PIC X(16)   VALUE 'WMSGINIT'.            
009300     SKIP3                                                                
009400*01 -COPY WMSGINIT                                                        
009500     EJECT                                                                
009600*    --- AREA MED DATA SOM SKA SPARAS MELLAN DIALOGSTEGEN                 
009700*                                                                         
009800 01  SPAR-AREA.                                                           
009900     03  SPAR-IDTRANS           PIC X(4)    VALUE '4405'.                 
010000     EJECT                                                                
010100*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
010200*                                                                         
010300 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
010400     SKIP3                                                                
010500*01  MID -COPY W4I40501                                                   
010600     EJECT                                                                
010700 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
010800     SKIP3                                                                
010900*01  -COPY WMSGAREA                                                       
011000     EJECT                                                                
011100     03  MOD REDEFINES MSG-AREA.                                          
011200*      05  -COPY W4O40501                                                 
011300     EJECT                                                                
011400 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
011500     SKIP3                                                                
011600*01  -COPY WMFSAREA                                                       
011700     EJECT                                                                
011800*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
011900*                                                                         
012000 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
012100     SKIP3                                                                
012200 01  NYCKLAR-TILL-DLI.                                                    
012300     03  W-IDDC-X.                                                        
012400         05  W-IDDC              PIC X(2)    VALUE SPACE.                 
012500     03  W-TEELMT-X.                                                      
012600         05  W-TEELMT            PIC X(16)   VALUE SPACE.                 
012700     03  W-IDELMT-X.                                                      
012800         05  W-IDELMT            PIC X(20)   VALUE SPACE.                 
012900         05  W-KDANMORS-ELMT REDEFINES W-IDELMT.                          
013000             07  W-KDANMORS      PIC X(2).                                
013100             07  FILLER          PIC X(18).                               
013200         05  W-IDFKNGRP-ELMT REDEFINES W-IDELMT.                          
013300             07  W-IDFKNGRP      PIC 9(4).                                
013400             07  FILLER          PIC X(16).                               
013500         05  W-IDARTNR-ELMT REDEFINES W-IDELMT.                           
013600             07  W-IDARTNR       PIC 9(9).                                
013700             07  FILLER          PIC X(11).                               
013800         05  W-IDDC-ELMT    REDEFINES W-IDELMT.                           
013810             07  W-IDDC-EXCP     PIC X(2).                                
013820             07  FILLER          PIC X(18).                               
013900     03  W-IDARTNR-X.                                                     
014000         05  W-IDARTNR-K6        PIC S9(9)   COMP-3.                      
014001                                                                          
014010     03  W-IDDC-B6-X.                                                     
014020         05  W-IDDC-B6           PIC X(2)    VALUE SPACE.                 
014100     SKIP2                                                                
014200*    --- STATUS-KOD FRÅN IMS                                              
014300 01  STATUS-WS                   PIC XX.                                  
014400     88  SEGMENT-FINNS                       VALUE '  '.                  
014500     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
014600     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
014700     SKIP2                                                                
014800 01  GODK-STATUSKODER.                                                    
014900     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
015000     SKIP3                                                                
015100 01  SSA1                        PIC X(128).                              
015200 01  SSA2                        PIC X(128).                              
015300     EJECT                                                                
015400*    --- IMS FUNKTIONSKODER                                               
015500*01  -COPY W0003                                                          
015600     EJECT                                                                
015700*    ---  DLI INPUT-OUTPUT AREA                                           
015800                                                                          
015900 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDB601'.                      
016000 01  DLI-IO-WDB601.                                                       
016100*    03  -COPY WDB601                                                     
016200                                                                          
016300 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDB611'.                      
016400 01  DLI-IO-WDB611.                                                       
016500*    03  -COPY WDB611                                                     
016600                                                                          
016700 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK601'.                      
016800 01  DLI-IO-WDK601.                                                       
016900*    03  -COPY WDK601                                                     
017000                                                                          
017100 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDR601'.                      
017200 01  DLI-IO-WDR601.                                                       
017300*    03  -COPY WDR601                                                     
017400*    05  LOGG  -COPY W414DCSA          -RED FIL-WDR601-DATA.              
017410                                                                          
017420 01  FILLER         PIC X(16) VALUE 'WDB601 AREA LEV'.                    
017430 01  DLI-IO-WDB601-LEV.                                                   
017440*    03  -COPY WDB601 -PRE LEV-                                           
017450                                                                          
017500     EJECT                                                                
017600 LINKAGE SECTION.                                                         
017700*01  -COPY W0009   -PRE MSG-                                              
017800*01  -COPY W0008   -PRE WDP7-                                             
017900     05  FILLER                  PIC X.                                   
018000                                                                          
018100*01  -COPY W0008  -PRE WDB6-                                              
018200     05  FILLER                  PIC X.                                   
018300                                                                          
018400*01  -COPY W0008  -PRE WDK6-                                              
018500     05  FILLER                  PIC X.                                   
018600     EJECT                                                                
018700*01  -COPY W0008  -PRE WDR6-                                              
018800     05  FILLER                  PIC X.                                   
018900                                                                          
019000 PROCEDURE DIVISION  USING MSG-PCB  WDP7-PCB                              
019100                           WDB6-PCB WDK6-PCB WDR6-PCB.                    
019200 MAIN SECTION.                                                            
019300     ENTRY 'DLITCBL' USING MSG-PCB  WDP7-PCB                              
019400                           WDB6-PCB WDK6-PCB WDR6-PCB.                    
019500                                                                          
019600     PERFORM IMS-GET-MSG                                                  
019700     IF SEGMENT-FINNS                                                     
019800        PERFORM A-INIT                                                    
019900        PERFORM B-KOLLA-NYCKLAR                                           
020000        IF NYCKLAR-OK                                                     
020100           IF MFS-UPDATE                                                  
020200              PERFORM G-KOLLA-INPUT                                       
020300              IF INDATA-OK                                                
020400                 PERFORM H-UPPDATERA                                      
020500              END-IF                                                      
020600           ELSE                                                           
020700              IF EGEN-MID                                                 
020800                 PERFORM I-KOLLA-ATT-EJ-UPDATE                            
020900              END-IF                                                      
021000           END-IF                                                         
021100           IF INDATA-OK                                                   
021200              PERFORM F-LAES-VISA-INFO                                    
021300           END-IF                                                         
021400        END-IF                                                            
021500        COMPUTE MSG-KVLL = LENGTH OF MOD-W4O40501 + 4                     
021600        PERFORM IMS-INSERT-MSG                                            
021700     END-IF                                                               
021800                                                                          
021900     MOVE ZERO TO RETURN-CODE                                             
022000     GOBACK                                                               
022100     .                                                                    
022200                                                                          
022300 A-INIT SECTION.                                                          
022400     MOVE 'A-INIT          ' TO CURRENT-SECTION                           
022500                                                                          
022600     IF MSG-DUBBLA-TRANSKODER                                             
022700       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W4I40501                 
022800       MOVE MSG-IDTRANS-2  TO MFS-IDTRANS                                 
022900       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
023000     ELSE                                                                 
023100       MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W4I40501                  
023200       MOVE MSG-IDTRANS-1  TO MFS-IDTRANS                                 
023300       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
023400     END-IF                                                               
023500                                                                          
023600     MOVE MSG-KDTRTYP TO MFS-KDTRTYP                                      
023700     MOVE MSG-IDPFK   TO MFS-IDPFK                                        
023800     MOVE MFS-IDTRANS TO W-IDTRANS                                        
023900                                                                          
024000     MOVE LOW-VALUE TO MSG-AREA                                           
024100     MOVE 'W4O405N1' TO MFS-IDMOD                                         
024200     MOVE '4405' TO MOD-IDTRANS                                           
024300     MOVE MFS-RENSA-FAELT TO MOD-TEMFSFEL MOD-TEMFSINF                    
024400                                                                          
024500     MOVE IDPGM                TO  FIL-IDPGM                              
024600     ACCEPT FIL-TIREGDAT       FROM  DATE                                 
024700     ACCEPT FIL-TIKLOCK        FROM  TIME                                 
024800     MOVE ZERO                 TO  FIL-IDSEKVNR                           
024900     MOVE 'W414'               TO  FIL-CT-IDSYSTEM                        
025000     MOVE 'DCS'                TO  FIL-CT-IDPTYP                          
025100     MOVE 'A'                  TO  FIL-CT-IDVTYP                          
025200     .                                                                    
025300     EJECT                                                                
025400 B-KOLLA-NYCKLAR SECTION.                                                 
025500     MOVE 'B-KOLLA-NYCKLAR ' TO CURRENT-SECTION                           
025600                                                                          
025700     MOVE ALL '+'           TO MSGI-WMSGINIT                              
025800     MOVE '001'             TO MSGI-KDCALL                                
025900     MOVE MSG-LTERM-NAME    TO MSGI-IDLTERM-USER                          
026000     MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                                
026100     MOVE '4405'            TO MSGI-IDTRANS                               
026200     IF GODK-MID OR EGEN-MID                                              
026300        MOVE MID-IDDC-IN    TO MSGI-IDDC-KEY                              
026400     ELSE                                                                 
026500        MOVE '++'           TO MSGI-IDDC-KEY                              
026600     END-IF                                                               
026700     CALL W005INIT USING MSGI-WMSGINIT WDP7-PCB                           
026800     MOVE MSGI-SPAR-AREA TO SPAR-AREA                                     
026900                                                                          
027000*    - SPRÅK SOM SKA ANVÄNDAS AV WMEDKONV                                 
027100     MOVE MSGI-IDLAND-SPR TO MED-IDSKYLT                                  
027200                                                                          
027300     MOVE MSGI-IDUSER       TO DCSL-IDUSER                                
027400     MOVE MSGI-BEANST       TO DCSL-BEANST                                
027500     MOVE MSGI-IDDC-KEY     TO DCSL-IDDC                                  
027600     MOVE 'OLD VALUE: '     TO DCSL-BETEXT-OLD                            
027700     MOVE 'NEW VALUE: '     TO DCSL-BETEXT-NEW                            
027800                                                                          
027900     MOVE MFS-ERASE-FIELD   TO MOD-KDBEHX                                 
028000     MOVE MFS-ERASE-FIELD   TO MOD-KDANMORS-UPD                           
028100     MOVE MFS-ERASE-FIELD   TO MOD-IDFKNGRP-UPD                           
028200     MOVE MFS-ERASE-FIELD   TO MOD-IDARTNR-UPD                            
028210     MOVE MFS-ERASE-FIELD   TO MOD-IDDC-EXCP-UPD                          
028300                                                                          
028400*    -- KONTROLL AV IDDC                                                  
028500     MOVE MFS-RENSA-FAELT TO MOD-IDDC-IN                                  
028600                                                                          
028700     MOVE MSGI-IDDC-KEY TO W-IDDC                                         
028800     PERFORM IMS-01-GU-WDB601                                             
028900                                                                          
029000     IF SEGMENT-SAKNAS                                                    
029100       MOVE ERR-DC-MISSING TO MED-IDMFSFEL                                
029200       CALL WMEDKONV USING MED-WMEDAREA                                   
029300       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
029400       PERFORM MFS-RENSA-FAELT-UT                                         
029500       MOVE NEJ TO NYCKLAR-SW                                             
029600     END-IF                                                               
029700                                                                          
029800     MOVE MSGI-IDDC-KEY   TO MOD-IDDC-UT                                  
029900     .                                                                    
030000                                                                          
030100 F-LAES-VISA-INFO SECTION.                                                
030200     MOVE 'F-LAES-VISA-INFO' TO CURRENT-SECTION                           
030300                                                                          
030400     PERFORM IMS-01-GU-WDB601                                             
030500                                                                          
030600     IF DCS-FLARTDC = 'J'                                                 
030700        MOVE 'Y'              TO MOD-FLARTDC-UPD                          
030800     ELSE                                                                 
030900        MOVE DCS-FLARTDC      TO MOD-FLARTDC-UPD                          
031000     END-IF                                                               
031100     PERFORM FA-VISA-KDANMORS                                             
031200     PERFORM FB-VISA-IDFKNGRP                                             
031300     PERFORM FC-VISA-IDARTNR                                              
031310     PERFORM FD-VISA-IDDC-EXCP                                            
031400     .                                                                    
031500                                                                          
031600 FA-VISA-KDANMORS SECTION.                                                
031700     MOVE 'FA-VISA-KDANMORS' TO CURRENT-SECTION                           
031800                                                                          
031900     PERFORM IMS-01-GU-WDB601                                             
032000     MOVE +1          TO ANM-IX                                           
032100     MOVE WC-KDANMORS TO W-TEELMT                                         
032200     PERFORM IMS-04-GNP-WDB611                                            
032300     PERFORM UNTIL SEGMENT-SAKNAS                                         
032400                OR ANM-IX > ANM-IX-MAX                                    
032500         MOVE URV-KDANMORS-RET TO MOD-KDANMORS(ANM-IX)                    
032600         ADD +1  TO ANM-IX                                                
032700         PERFORM IMS-04-GNP-WDB611                                        
032800     END-PERFORM                                                          
032900     .                                                                    
033000                                                                          
033100 FB-VISA-IDFKNGRP SECTION.                                                
033200     MOVE 'FB-VISA-IDFKNGRP' TO CURRENT-SECTION                           
033300                                                                          
033400     PERFORM IMS-01-GU-WDB601                                             
033500     MOVE +1          TO FKN-IX                                           
033600     MOVE WC-IDFKNGRP TO W-TEELMT                                         
033700     PERFORM IMS-04-GNP-WDB611                                            
033800     PERFORM UNTIL SEGMENT-SAKNAS                                         
033900                OR FKN-IX > FKN-IX-MAX                                    
034000         MOVE URV-IDFKNGRP-EXCP TO MOD-IDFKNGRP(FKN-IX)                   
034100         ADD +1  TO FKN-IX                                                
034200         PERFORM IMS-04-GNP-WDB611                                        
034300     END-PERFORM                                                          
034400     .                                                                    
034500                                                                          
034600 FC-VISA-IDARTNR  SECTION.                                                
034700     MOVE 'FC-VISA-IDARTNR ' TO CURRENT-SECTION                           
034800                                                                          
034900     PERFORM IMS-01-GU-WDB601                                             
035000     MOVE +1          TO ART-IX                                           
035100     MOVE WC-IDARTNR  TO W-TEELMT                                         
035200     PERFORM IMS-04-GNP-WDB611                                            
035300     PERFORM UNTIL SEGMENT-SAKNAS                                         
035400                OR ART-IX > ART-IX-MAX                                    
035500         MOVE URV-IDARTNR-EXCP TO MOD-IDARTNR(ART-IX)                     
035600         ADD +1  TO ART-IX                                                
035700         PERFORM IMS-04-GNP-WDB611                                        
035800     END-PERFORM                                                          
035900     .                                                                    
036000                                                                          
036010 FD-VISA-IDDC-EXCP SECTION.                                               
036020     MOVE 'FC-VISA-IDDC-EXCP ' TO CURRENT-SECTION                         
036030                                                                          
036040     PERFORM IMS-01-GU-WDB601                                             
036050     MOVE +1           TO DC-IX                                           
036060     MOVE WC-IDDC-EXCP TO W-TEELMT                                        
036070     PERFORM IMS-04-GNP-WDB611                                            
036080     PERFORM UNTIL SEGMENT-SAKNAS                                         
036090                OR DC-IX > DC-IX-MAX                                      
036091         MOVE URV-IDDC-EXCP TO MOD-IDDC-EXCP(DC-IX)                       
036092         ADD +1  TO DC-IX                                                 
036093         PERFORM IMS-04-GNP-WDB611                                        
036094     END-PERFORM                                                          
036095     .                                                                    
036096                                                                          
036100 G-KOLLA-INPUT SECTION.                                                   
036200     MOVE 'G-KOLLA-INPUT   ' TO CURRENT-SECTION                           
036300                                                                          
036400     MOVE JA  TO INDATA-SW                                                
036500     IF MID-INPUT = ALL '+'                                               
036600       MOVE ERR-PF11-AND-NO-DATA TO MED-IDMFSFEL                          
036700       CALL WMEDKONV USING MED-WMEDAREA                                   
036800       MOVE MED-MFSFEL TO MOD-TEMFSFEL                                    
036900       PERFORM MFS-ROER-EJ-FAELT-UT                                       
037000       MOVE NEJ TO INDATA-SW                                              
037100     ELSE                                                                 
037200                                                                          
037300       IF MID-FLARTDC-UPD = 'Y' OR 'J' OR 'N'                             
037400          IF MID-FLARTDC-UPD = 'Y'                                        
037500             MOVE 'J' TO MID-FLARTDC-UPD                                  
037600          END-IF                                                          
037700       ELSE                                                               
037800          MOVE NEJ TO INDATA-SW                                           
037900          MOVE MFS-ALFA-FAELT-FEL TO MOD-FLARTDC-ATTR                     
038000       END-IF                                                             
038100                                                                          
038200       IF INDATA-OK                                                       
038300          IF MID-KDBEHX = 'N' OR 'D' OR '+' OR SPACE                      
038400             CONTINUE                                                     
038500          ELSE                                                            
038600             MOVE NEJ TO INDATA-SW                                        
038700             MOVE MFS-ALFA-FAELT-FEL TO MOD-KDBEHX-ATTR                   
038800          END-IF                                                          
038900       END-IF                                                             
039000                                                                          
039100       IF INDATA-OK                                                       
039200          PERFORM GA-KOLLA-KDANMORS                                       
039300       END-IF                                                             
039400                                                                          
039500       IF INDATA-OK                                                       
039600          PERFORM GB-KOLLA-IDFKNGRP                                       
039700       END-IF                                                             
039800                                                                          
039900       IF INDATA-OK                                                       
040000          PERFORM GC-KOLLA-IDARTNR                                        
040100       END-IF                                                             
040200                                                                          
040210       IF INDATA-OK                                                       
040220          PERFORM GD-KOLLA-IDDC-EXCP                                      
040230       END-IF                                                             
040240                                                                          
040300       IF INDATA-FEL                                                      
040400         MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL                        
040500         CALL WMEDKONV USING MED-WMEDAREA                                 
040600         MOVE MED-MFSFEL TO MOD-TEMFSFEL                                  
040700         PERFORM MFS-ROER-EJ-FAELT-UT                                     
040800         PERFORM MFS-ROER-EJ-DATA-UT                                      
040900       END-IF                                                             
041000     END-IF                                                               
041100     .                                                                    
041200                                                                          
041300 GA-KOLLA-KDANMORS SECTION.                                               
041400     MOVE 'GA-KOLLA-KDANMOR' TO CURRENT-SECTION                           
041500                                                                          
041600     IF MID-KDANMORS-UPD = ALL '+' OR SPACE                               
041700        IF MID-KDANMORS-UPD = SPACE                                       
041800           MOVE MFS-ALFA-FAELT-FEL TO MOD-KDANMORS-ATTR                   
041900           MOVE NEJ                TO INDATA-SW                           
042000        END-IF                                                            
042100     ELSE                                                                 
042200        MOVE WC-KDANMORS                 TO W-TEELMT                      
042300        MOVE SPACE                       TO W-IDELMT                      
042400        MOVE MID-KDANMORS-UPD            TO W-KDANMORS                    
042500        PERFORM IMS-05-GU-WDB611                                          
042600        IF MID-KDBEHX = 'N'                                               
042700           IF SEGMENT-FINNS                                               
042800* FINNS REDAN VID NYUPPLÄGG                                               
042900              MOVE MFS-ALFA-FAELT-FEL    TO MOD-KDANMORS-ATTR             
043000              MOVE NEJ                   TO INDATA-SW                     
043100           ELSE                                                           
043200              MOVE MID-KDANMORS-UPD      TO OKOD-KDANMORS                 
043300              CALL W418OKOD USING OKOD-W418OKOD                           
043400              IF OKOD-FL-GODK-KOD = NEJ OR                                
043500* FEL KOD                                                                 
043600                 OKOD-FL-RETILL   = NEJ                                   
043700                 MOVE MFS-ALFA-FAELT-FEL TO MOD-KDANMORS-ATTR             
043800                 MOVE NEJ                TO INDATA-SW                     
043900              END-IF                                                      
044000           END-IF                                                         
044100        ELSE                                                              
044200           IF MID-KDBEHX = 'D'                                            
044300              IF SEGMENT-SAKNAS                                           
044400* SAKNAS VID BORTTAG                                                      
044500                 MOVE MFS-ALFA-FAELT-FEL TO MOD-KDANMORS-ATTR             
044600                 MOVE NEJ                TO INDATA-SW                     
044700              END-IF                                                      
044800           ELSE                                                           
044900* FEL BEHANDLINGSKOD                                                      
045000              MOVE MFS-ALFA-FAELT-FEL    TO MOD-KDBEHX-ATTR               
045100              MOVE NEJ                   TO INDATA-SW                     
045200           END-IF                                                         
045300        END-IF                                                            
045400     END-IF                                                               
045500     .                                                                    
045600                                                                          
045700 GB-KOLLA-IDFKNGRP SECTION.                                               
045800     MOVE 'GB-KOLLA-IDFKNGR' TO CURRENT-SECTION                           
045900                                                                          
046000     IF MID-IDFKNGRP-UPD = ALL '+' OR ZERO                                
046100        CONTINUE                                                          
046200     ELSE                                                                 
046300        IF MID-IDFKNGRP-UPD NOT NUMERIC                                   
046400* ICKENUMERISKT                                                           
046500           MOVE MFS-NUM-FAELT-FEL       TO MOD-IDFKNGRP-ATTR              
046600           MOVE NEJ                     TO INDATA-SW                      
046700        ELSE                                                              
046800           MOVE WC-IDFKNGRP             TO W-TEELMT                       
046900           MOVE SPACE                   TO W-IDELMT                       
047000           MOVE MID-IDFKNGRP-UPD        TO W-IDFKNGRP                     
047100           PERFORM IMS-05-GU-WDB611                                       
047200           IF MID-KDBEHX = 'N'                                            
047300              IF MID-KDBEHX = 'N' AND SEGMENT-FINNS                       
047400* FINNS REDAN VID NYUPPLÄGG                                               
047500                 MOVE MFS-NUM-FAELT-FEL    TO MOD-IDFKNGRP-ATTR           
047600                 MOVE NEJ                  TO INDATA-SW                   
047700              END-IF                                                      
047800           ELSE                                                           
047900              IF MID-KDBEHX = 'D'                                         
048000                 IF SEGMENT-SAKNAS                                        
048100* SAKNAS VID BORTTAG                                                      
048200                    MOVE MFS-NUM-FAELT-FEL TO MOD-IDFKNGRP-ATTR           
048300                    MOVE NEJ               TO INDATA-SW                   
048400                 END-IF                                                   
048500              ELSE                                                        
048600* FEL BEHANDLINGSKOD                                                      
048700                 MOVE MFS-ALFA-FAELT-FEL   TO MOD-KDBEHX-ATTR             
048800                 MOVE NEJ                  TO INDATA-SW                   
048900              END-IF                                                      
049000           END-IF                                                         
049100        END-IF                                                            
049200     END-IF                                                               
049300     .                                                                    
049400                                                                          
049500 GC-KOLLA-IDARTNR  SECTION.                                               
049600     MOVE 'GC-KOLLA-IDARTNR' TO CURRENT-SECTION                           
049700                                                                          
049800     IF MID-IDARTNR-UPD = ALL '+' OR ZERO                                 
049900        CONTINUE                                                          
050000     ELSE                                                                 
050100        IF MID-IDARTNR-UPD NOT NUMERIC                                    
050200* ICKENUMERISKT                                                           
050300           MOVE MFS-NUM-FAELT-FEL          TO MOD-IDARTNR-ATTR            
050400           MOVE NEJ                        TO INDATA-SW                   
050500        ELSE                                                              
050600           MOVE WC-IDARTNR                 TO W-TEELMT                    
050700           MOVE SPACE                      TO W-IDELMT                    
050800           MOVE MID-IDARTNR-UPD            TO W-IDARTNR                   
050900           PERFORM IMS-05-GU-WDB611                                       
051000           IF MID-KDBEHX = 'N'                                            
051100              IF SEGMENT-FINNS                                            
051200* FINNS REDAN VID NYUPPLÄGG                                               
051300                 MOVE MFS-NUM-FAELT-FEL    TO MOD-IDARTNR-ATTR            
051400                 MOVE NEJ                  TO INDATA-SW                   
051500              ELSE                                                        
051600                 MOVE MID-IDARTNR-UPD      TO W-IDARTNR-K6                
051700                 PERFORM IMS-10-GU-WDK601                                 
051800                 IF SEGMENT-SAKNAS                                        
051900* OGILTIG ARTIKEL                                                         
052000                    MOVE MFS-NUM-FAELT-FEL TO MOD-IDARTNR-ATTR            
052100                    MOVE NEJ               TO INDATA-SW                   
052200                 END-IF                                                   
052300              END-IF                                                      
052400           ELSE                                                           
052500              IF MID-KDBEHX = 'D'                                         
052600                 IF SEGMENT-SAKNAS                                        
052700* SAKNAS VID BORTTAG                                                      
052800                    MOVE MFS-NUM-FAELT-FEL TO MOD-IDARTNR-ATTR            
052900                    MOVE NEJ               TO INDATA-SW                   
053000                 END-IF                                                   
053100              ELSE                                                        
053200* FEL BEHANDLINGSKOD                                                      
053300                 MOVE MFS-ALFA-FAELT-FEL   TO MOD-KDBEHX-ATTR             
053400                 MOVE NEJ                  TO INDATA-SW                   
053500              END-IF                                                      
053600           END-IF                                                         
053700        END-IF                                                            
053800     END-IF                                                               
053900     .                                                                    
054000                                                                          
054010 GD-KOLLA-IDDC-EXCP  SECTION.                                             
054020     MOVE 'GD-KOLLA-IDDC-EXCP' TO CURRENT-SECTION                         
054030                                                                          
054040     IF MID-IDDC-EXCP-UPD = ALL '+' OR SPACE                              
054041        IF MID-IDDC-EXCP-UPD = SPACE                                      
054042           MOVE MFS-ALFA-FAELT-FEL TO MOD-IDDC-EXCP-ATTR                  
054043           MOVE NEJ                TO INDATA-SW                           
054044        END-IF                                                            
054060     ELSE                                                                 
054093        MOVE WC-IDDC-EXCP               TO W-TEELMT                       
054094        MOVE SPACE                      TO W-IDELMT                       
054095        MOVE MID-IDDC-EXCP-UPD          TO W-IDDC-EXCP                    
054096        PERFORM IMS-05-GU-WDB611                                          
054097        IF MID-KDBEHX = 'N'                                               
054098           IF SEGMENT-FINNS                                               
054099* FINNS REDAN VID NYUPPLÄGG                                               
054100              MOVE MFS-ALFA-FAELT-FEL   TO MOD-IDDC-EXCP-ATTR             
054101              MOVE NEJ                  TO INDATA-SW                      
054102           ELSE                                                           
054103              MOVE MID-IDDC-EXCP-UPD    TO W-IDDC-B6                      
054104              PERFORM IMS-09-GU-WDB601-LEV                                
054105              IF SEGMENT-SAKNAS                                           
054106* OGILTIGT DC                                                             
054107                 MOVE MFS-ALFA-FAELT-FEL TO MOD-IDDC-EXCP-ATTR            
054108                 MOVE NEJ               TO INDATA-SW                      
054109              END-IF                                                      
054110           END-IF                                                         
054111        ELSE                                                              
054112           IF MID-KDBEHX = 'D'                                            
054113              IF SEGMENT-SAKNAS                                           
054114* SAKNAS VID BORTTAG                                                      
054115                 MOVE MFS-ALFA-FAELT-FEL TO MOD-IDDC-EXCP-ATTR            
054116                 MOVE NEJ                TO INDATA-SW                     
054117              END-IF                                                      
054118           ELSE                                                           
054119* FEL BEHANDLINGSKOD                                                      
054120              MOVE MFS-ALFA-FAELT-FEL   TO MOD-KDBEHX-ATTR                
054121              MOVE NEJ                  TO INDATA-SW                      
054122           END-IF                                                         
054123        END-IF                                                            
054125     END-IF                                                               
054126     .                                                                    
054127                                                                          
054130 H-UPPDATERA SECTION.                                                     
054200     MOVE 'H-UPPDATERA     ' TO CURRENT-SECTION                           
054300                                                                          
054400     IF MID-FLARTDC-UPD NOT = ALL '+' AND                                 
054500        MID-FLARTDC-UPD NOT = DCS-FLARTDC                                 
054600                                                                          
054700        MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-FLARTDC-ATTR                    
054800        MOVE 'FLARTDC'         TO DCSL-BETEXT-ITEM                        
054900        MOVE DCS-FLARTDC       TO DCSL-BETEXT-OLDDATA                     
055000        MOVE MID-FLARTDC-UPD   TO DCS-FLARTDC                             
055100                                 DCSL-BETEXT-NEWDATA                      
055200        ADD +1                 TO FIL-IDSEKVNR                            
055300        PERFORM IMS-11-ISRT-WDR601                                        
055400     END-IF                                                               
055500                                                                          
055600     IF MID-KDANMORS-UPD NOT = ALL '+'                                    
055700        MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-KDANMORS-ATTR                   
055800        MOVE 'KDANMORS'        TO DCSL-BETEXT-ITEM                        
055900        IF MID-KDBEHX = 'N'                                               
056000           MOVE SPACE             TO DCSL-BETEXT-OLDDATA                  
056100           MOVE MID-KDANMORS-UPD  TO URV-KDANMORS-RET                     
056200                                    DCSL-BETEXT-NEWDATA                   
056300        ELSE                                                              
056400           MOVE MID-KDANMORS-UPD  TO DCSL-BETEXT-OLDDATA                  
056500           MOVE SPACE             TO DCSL-BETEXT-NEWDATA                  
056600        END-IF                                                            
056700        ADD +1                 TO FIL-IDSEKVNR                            
056800        PERFORM IMS-11-ISRT-WDR601                                        
056900     END-IF                                                               
057000                                                                          
057100     IF MID-IDFKNGRP-UPD NOT = ALL '+'                                    
057200        MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-IDFKNGRP-ATTR                   
057300        MOVE 'IDFKNGRP'        TO DCSL-BETEXT-ITEM                        
057400        IF MID-KDBEHX = 'N'                                               
057500           MOVE SPACE             TO DCSL-BETEXT-OLDDATA                  
057600           MOVE MID-IDFKNGRP-UPD  TO URV-IDFKNGRP-EXCP                    
057700                                    DCSL-BETEXT-NEWDATA                   
057800        ELSE                                                              
057900           MOVE MID-IDFKNGRP-UPD  TO DCSL-BETEXT-OLDDATA                  
058000           MOVE SPACE             TO DCSL-BETEXT-NEWDATA                  
058100        END-IF                                                            
058200        ADD +1                 TO FIL-IDSEKVNR                            
058300        PERFORM IMS-11-ISRT-WDR601                                        
058400     END-IF                                                               
058500                                                                          
058600     IF MID-IDARTNR-UPD NOT = ALL '+'                                     
058700        MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-IDARTNR-ATTR                    
058800        MOVE 'IDARTNR'         TO DCSL-BETEXT-ITEM                        
058900        IF MID-KDBEHX = 'N'                                               
059000           MOVE SPACE             TO DCSL-BETEXT-OLDDATA                  
059100           MOVE MID-IDARTNR-UPD   TO URV-IDARTNR-EXCP                     
059200                                    DCSL-BETEXT-NEWDATA                   
059300        ELSE                                                              
059400           MOVE MID-IDARTNR-UPD   TO DCSL-BETEXT-OLDDATA                  
059500           MOVE SPACE             TO DCSL-BETEXT-NEWDATA                  
059600        END-IF                                                            
059700        ADD +1                 TO FIL-IDSEKVNR                            
059800        PERFORM IMS-11-ISRT-WDR601                                        
059900     END-IF                                                               
060000                                                                          
060010     IF MID-IDDC-EXCP-UPD NOT = ALL '+'                                   
060020        MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-IDDC-EXCP-ATTR                  
060030        MOVE 'IDDC    '        TO DCSL-BETEXT-ITEM                        
060040        IF MID-KDBEHX = 'N'                                               
060050           MOVE SPACE              TO DCSL-BETEXT-OLDDATA                 
060060           MOVE MID-IDDC-EXCP-UPD  TO URV-IDDC-EXCP                       
060070                                      DCSL-BETEXT-NEWDATA                 
060080        ELSE                                                              
060090           MOVE MID-IDDC-EXCP-UPD  TO DCSL-BETEXT-OLDDATA                 
060091           MOVE SPACE              TO DCSL-BETEXT-NEWDATA                 
060092        END-IF                                                            
060093        ADD +1                 TO FIL-IDSEKVNR                            
060094        PERFORM IMS-11-ISRT-WDR601                                        
060095     END-IF                                                               
060096                                                                          
060100     IF MID-FLARTDC-UPD NOT = ALL '+'                                     
060200        PERFORM HA-UPPDATERA-FLARTDC                                      
060300     END-IF                                                               
060400                                                                          
060500     IF MID-KDANMORS-UPD NOT = ALL '+'                                    
060600        PERFORM HB-UPPDATERA-KDANMORS                                     
060700     END-IF                                                               
060800                                                                          
060900     IF MID-IDFKNGRP-UPD NOT = ALL '+'                                    
061000        PERFORM HC-UPPDATERA-IDFKNGRP                                     
061100     END-IF                                                               
061200                                                                          
061300     IF MID-IDARTNR-UPD NOT = ALL '+'                                     
061400        PERFORM HD-UPPDATERA-IDARTNR                                      
061500     END-IF                                                               
061600                                                                          
061610     IF MID-IDDC-EXCP-UPD NOT = ALL '+'                                   
061620        PERFORM HE-UPPDATERA-IDDC-EXCP                                    
061630     END-IF                                                               
061640                                                                          
061700     MOVE INF-UPDATE-DONE TO MED-IDMFSINF                                 
061800     CALL WMEDKONV USING MED-WMEDAREA                                     
061900     MOVE MED-MFSINF TO MOD-TEMFSINF                                      
062000     PERFORM MFS-FORM-ATTR                                                
062100     .                                                                    
062200                                                                          
062300 HA-UPPDATERA-FLARTDC SECTION.                                            
062400     MOVE 'HA-UPPDATERA-FLA' TO CURRENT-SECTION                           
062500                                                                          
062600     PERFORM IMS-02-GHU-WDB601                                            
062700     MOVE MID-FLARTDC-UPD TO DCS-FLARTDC                                  
062800     PERFORM IMS-03-REPL-WDB601                                           
062900     .                                                                    
063000                                                                          
063100 HB-UPPDATERA-KDANMORS SECTION.                                           
063200     MOVE 'HB-UPPDATERA-KDA' TO CURRENT-SECTION                           
063300                                                                          
063400     IF MID-KDBEHX = 'N'                                                  
063500        MOVE SPACE            TO URV-WDB611                               
063600        MOVE WC-KDANMORS      TO URV-TEELMT                               
063700        MOVE MID-KDANMORS-UPD TO URV-KDANMORS-RET                         
063800        PERFORM IMS-07-ISRT-WDB611                                        
063900     ELSE                                                                 
064000        MOVE WC-KDANMORS      TO W-TEELMT                                 
064100        MOVE SPACE            TO W-IDELMT                                 
064200        MOVE MID-KDANMORS-UPD TO W-KDANMORS                               
064300        PERFORM IMS-06-GHU-WDB611                                         
064400        PERFORM IMS-08-DLET-WDB611                                        
064500     END-IF                                                               
064600     .                                                                    
064700                                                                          
064800 HC-UPPDATERA-IDFKNGRP SECTION.                                           
064900     MOVE 'HC-UPPDATERA-IDF' TO CURRENT-SECTION                           
065000                                                                          
065100     IF MID-KDBEHX = 'N'                                                  
065200        MOVE SPACE            TO URV-WDB611                               
065300        MOVE WC-IDFKNGRP      TO URV-TEELMT                               
065400        MOVE MID-IDFKNGRP-UPD TO URV-IDFKNGRP-EXCP                        
065500        PERFORM IMS-07-ISRT-WDB611                                        
065600     ELSE                                                                 
065700        MOVE WC-IDFKNGRP      TO W-TEELMT                                 
065800        MOVE SPACE            TO W-IDELMT                                 
065900        MOVE MID-IDFKNGRP-UPD TO W-IDFKNGRP                               
066000        PERFORM IMS-06-GHU-WDB611                                         
066100        PERFORM IMS-08-DLET-WDB611                                        
066200     END-IF                                                               
066300     .                                                                    
066400                                                                          
066500 HD-UPPDATERA-IDARTNR SECTION.                                            
066600     MOVE 'HD-UPPDATERA-IDA' TO CURRENT-SECTION                           
066700                                                                          
066800     IF MID-KDBEHX = 'N'                                                  
066900        MOVE SPACE            TO URV-WDB611                               
067000        MOVE WC-IDARTNR       TO URV-TEELMT                               
067100        MOVE MID-IDARTNR-UPD  TO URV-IDARTNR-EXCP                         
067200        PERFORM IMS-07-ISRT-WDB611                                        
067300     ELSE                                                                 
067400        MOVE WC-IDARTNR       TO W-TEELMT                                 
067500        MOVE SPACE            TO W-IDELMT                                 
067600        MOVE MID-IDARTNR-UPD  TO W-IDARTNR                                
067700        PERFORM IMS-06-GHU-WDB611                                         
067800        PERFORM IMS-08-DLET-WDB611                                        
067900     END-IF                                                               
068000     .                                                                    
068100                                                                          
068110 HE-UPPDATERA-IDDC-EXCP SECTION.                                          
068120     MOVE 'HE-UPPDATERA-DC' TO CURRENT-SECTION                            
068130                                                                          
068140     IF MID-KDBEHX = 'N'                                                  
068150        MOVE SPACE            TO URV-WDB611                               
068160        MOVE WC-IDDC-EXCP     TO URV-TEELMT                               
068170        MOVE MID-IDDC-EXCP-UPD  TO URV-IDDC-EXCP                          
068180        PERFORM IMS-07-ISRT-WDB611                                        
068190     ELSE                                                                 
068191        MOVE WC-IDDC-EXCP     TO W-TEELMT                                 
068192        MOVE SPACE            TO W-IDELMT                                 
068193        MOVE MID-IDDC-EXCP-UPD  TO W-IDDC-EXCP                            
068194        PERFORM IMS-06-GHU-WDB611                                         
068195        PERFORM IMS-08-DLET-WDB611                                        
068196     END-IF                                                               
068197     .                                                                    
068198                                                                          
068200 I-KOLLA-ATT-EJ-UPDATE SECTION.                                           
068300     MOVE 'I-KOLLA-ATT-EJ-U' TO CURRENT-SECTION                           
068400                                                                          
068500     IF MID-IDDC-IN = ALL '+'                                             
068600        IF MID-FLARTDC-UPD    NOT = DCS-FLARTDC                           
068700          MOVE MID-FLARTDC-UPD       TO MOD-FLARTDC-UPD                   
068800          MOVE MFS-ADD-LAES-IN-FAELT TO MOD-FLARTDC-ATTR                  
068900          MOVE NEJ                   TO INDATA-SW                         
069000        END-IF                                                            
069100                                                                          
069200        IF MID-KDBEHX         = ALL '+'                                   
069300          MOVE MFS-RENSA-FAELT       TO MOD-KDBEHX                        
069400        ELSE                                                              
069500          MOVE MID-KDBEHX            TO MOD-KDBEHX                        
069600          MOVE MFS-ADD-LAES-IN-FAELT TO MOD-KDBEHX-ATTR                   
069700          MOVE NEJ                   TO INDATA-SW                         
069800        END-IF                                                            
069900                                                                          
070000        IF MID-KDANMORS-UPD   = ALL '+'                                   
070100          MOVE MFS-RENSA-FAELT       TO MOD-KDANMORS-UPD                  
070200        ELSE                                                              
070300          MOVE MID-KDANMORS-UPD      TO MOD-KDANMORS-UPD                  
070400          MOVE MFS-ADD-LAES-IN-FAELT TO MOD-KDANMORS-ATTR                 
070500          MOVE NEJ                   TO INDATA-SW                         
070600        END-IF                                                            
070700                                                                          
070800        IF MID-IDFKNGRP-UPD   = ALL '+'                                   
070900          MOVE MFS-RENSA-FAELT       TO MOD-IDFKNGRP-UPD                  
071000        ELSE                                                              
071100          MOVE MID-IDFKNGRP-UPD      TO MOD-IDFKNGRP-UPD                  
071200          MOVE MFS-ADD-LAES-IN-FAELT TO MOD-IDFKNGRP-ATTR                 
071300          MOVE NEJ                   TO INDATA-SW                         
071400        END-IF                                                            
071500                                                                          
071600        IF MID-IDARTNR-UPD    = ALL '+'                                   
071700          MOVE MFS-RENSA-FAELT       TO MOD-IDARTNR-UPD                   
071800        ELSE                                                              
071900          MOVE MID-IDARTNR-UPD       TO MOD-IDARTNR-UPD                   
072000          MOVE MFS-ADD-LAES-IN-FAELT TO MOD-IDARTNR-ATTR                  
072100          MOVE NEJ                   TO INDATA-SW                         
072200        END-IF                                                            
072300                                                                          
072310        IF MID-IDDC-EXCP-UPD    = ALL '+'                                 
072320          MOVE MFS-RENSA-FAELT       TO MOD-IDDC-EXCP-UPD                 
072330        ELSE                                                              
072340          MOVE MID-IDDC-EXCP-UPD     TO MOD-IDDC-EXCP-UPD                 
072350          MOVE MFS-ADD-LAES-IN-FAELT TO MOD-IDDC-EXCP-ATTR                
072360          MOVE NEJ                   TO INDATA-SW                         
072370        END-IF                                                            
072380                                                                          
072400        IF INDATA-FEL                                                     
072500           MOVE INF-PRESS-PF11 TO MED-IDMFSINF                            
072600           CALL WMEDKONV USING MED-WMEDAREA                               
072700           MOVE MED-MFSINF     TO MOD-TEMFSINF                            
072800           PERFORM MFS-ROER-EJ-FAELT-UT                                   
072900           PERFORM MFS-ROER-EJ-DATA-UT                                    
073000        END-IF                                                            
073100     END-IF                                                               
073200     .                                                                    
073300                                                                          
073400                                                                          
073500 MFS-RENSA-FAELT-UT SECTION.                                              
073600                                                                          
073700*    --- ALLA UTDATA-FÄLT                                                 
073800     MOVE MFS-RENSA-FAELT TO MOD-FLARTDC-UPD                              
073900                                                                          
074000     MOVE +1 TO ANM-IX                                                    
074100     PERFORM UNTIL ANM-IX > ANM-IX-MAX                                    
074200        MOVE MFS-RENSA-FAELT TO MOD-KDANMORS(ANM-IX)                      
074300        ADD +1 TO ANM-IX                                                  
074400     END-PERFORM                                                          
074500                                                                          
074600     MOVE +1 TO FKN-IX                                                    
074700     PERFORM UNTIL FKN-IX > FKN-IX-MAX                                    
074800        MOVE MFS-RENSA-FAELT TO MOD-IDFKNGRP(FKN-IX)                      
074900        ADD +1 TO FKN-IX                                                  
075000     END-PERFORM                                                          
075100                                                                          
075200     MOVE +1 TO ART-IX                                                    
075300     PERFORM UNTIL ART-IX > ART-IX-MAX                                    
075400        MOVE MFS-RENSA-FAELT TO MOD-IDARTNR(ART-IX)                       
075500        ADD +1 TO ART-IX                                                  
075600     END-PERFORM                                                          
075610                                                                          
075620     MOVE +1 TO DC-IX                                                     
075630     PERFORM UNTIL DC-IX > DC-IX-MAX                                      
075640        MOVE MFS-RENSA-FAELT TO MOD-IDDC-EXCP(DC-IX)                      
075650        ADD +1 TO DC-IX                                                   
075660     END-PERFORM                                                          
075700     .                                                                    
075800                                                                          
075900 MFS-ROER-EJ-FAELT-UT  SECTION.                                           
076000                                                                          
076100*    --- ALLA UTDATA-FÄLT                                                 
076200     MOVE MFS-ROER-EJ-FAELT TO MOD-FLARTDC-UPD                            
076300                               MOD-KDBEHX                                 
076400                               MOD-KDANMORS-UPD                           
076500                               MOD-IDFKNGRP-UPD                           
076600                               MOD-IDARTNR-UPD                            
076610                               MOD-IDDC-EXCP-UPD                          
076700     .                                                                    
076800                                                                          
076900 MFS-ROER-EJ-DATA-UT  SECTION.                                            
077000                                                                          
077100*    --- ALLA UTDATA-FÄLT                                                 
077200     MOVE MFS-ROER-EJ-FAELT TO MOD-FLARTDC-UPD                            
077300                                                                          
077400                                                                          
077500                                                                          
077600                                                                          
077700     MOVE +1          TO ANM-IX                                           
077800     PERFORM UNTIL ANM-IX > ANM-IX-MAX                                    
077900         MOVE MFS-ROER-EJ-FAELT TO MOD-KDANMORS(ANM-IX)                   
078000         ADD +1  TO ANM-IX                                                
078100     END-PERFORM                                                          
078200                                                                          
078300     MOVE +1          TO FKN-IX                                           
078400     PERFORM UNTIL FKN-IX > FKN-IX-MAX                                    
078500         MOVE MFS-ROER-EJ-FAELT TO MOD-IDFKNGRP(FKN-IX)                   
078600         ADD +1  TO FKN-IX                                                
078700     END-PERFORM                                                          
078800                                                                          
078900     MOVE +1          TO ART-IX                                           
079000     PERFORM UNTIL ART-IX > ART-IX-MAX                                    
079100         MOVE MFS-ROER-EJ-FAELT TO MOD-IDARTNR(ART-IX)                    
079200         ADD +1  TO ART-IX                                                
079300     END-PERFORM                                                          
079310                                                                          
079320     MOVE +1          TO DC-IX                                            
079330     PERFORM UNTIL DC-IX > DC-IX-MAX                                      
079340         MOVE MFS-ROER-EJ-FAELT TO MOD-IDDC-EXCP(DC-IX)                   
079350         ADD +1  TO DC-IX                                                 
079360     END-PERFORM                                                          
079400     .                                                                    
079500                                                                          
079600 MFS-FORM-ATTR SECTION.                                                   
079700                                                                          
079800*    --- ALLA INDATA-FÄLT                                                 
079900     MOVE MFS-FORMATETS-ATTR TO MOD-FLARTDC-ATTR                          
080000                                MOD-KDBEHX-ATTR                           
080100                                MOD-KDANMORS-ATTR                         
080200                                MOD-IDFKNGRP-ATTR                         
080300                                MOD-IDARTNR-ATTR                          
080310                                MOD-IDDC-EXCP-ATTR                        
080400     .                                                                    
080500     EJECT                                                                
080600* --- IMS SEKTIONER ---                                                   
080700     SKIP3                                                                
080800 IMS-GET-MSG SECTION.                                                     
080900                                                                          
081000     MOVE '  QC' TO GODK-STATUSKODER                                      
081100     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
081200     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
081300     PERFORM IMS-STATUSKONTROLL                                           
081400     .                                                                    
081500     SKIP3                                                                
081600 IMS-INSERT-MSG SECTION.                                                  
081700                                                                          
081800     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
081900     MOVE SPACE TO GODK-STATUSKODER                                       
082000     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
082100     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
082200     PERFORM IMS-STATUSKONTROLL                                           
082300     .                                                                    
082400     EJECT                                                                
082500 IMS-01-GU-WDB601 SECTION.                                                
082600     MOVE 'IMS-01' TO CURRENT-IMS-SECTION                                 
082700                                                                          
082800     STRING 'WDB601  (IDDC     =' W-IDDC-X ')'                            
082900          DELIMITED BY SIZE INTO SSA1                                     
083000     MOVE '  GE' TO GODK-STATUSKODER                                      
083100     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-WDB601 SSA1                    
083200     MOVE WDB6-STATUS-CODE TO STATUS-WS                                   
083300     PERFORM IMS-STATUSKONTROLL                                           
083400     .                                                                    
083500                                                                          
083600 IMS-02-GHU-WDB601 SECTION.                                               
083700     MOVE 'IMS-02' TO CURRENT-IMS-SECTION                                 
083800                                                                          
083900     STRING 'WDB601  (IDDC     =' W-IDDC-X ')'                            
084000          DELIMITED BY SIZE INTO SSA1                                     
084100     MOVE '  ' TO GODK-STATUSKODER                                        
084200     CALL CBLTDLI USING GHU WDB6-PCB DLI-IO-WDB601 SSA1                   
084300     MOVE WDB6-STATUS-CODE TO STATUS-WS                                   
084400     PERFORM IMS-STATUSKONTROLL                                           
084500     .                                                                    
084600                                                                          
084700 IMS-03-REPL-WDB601 SECTION.                                              
084800     MOVE 'IMS-03' TO CURRENT-IMS-SECTION                                 
084900                                                                          
085000     MOVE '  ' TO GODK-STATUSKODER                                        
085100     CALL CBLTDLI USING REPL WDB6-PCB DLI-IO-WDB601                       
085200     MOVE WDB6-STATUS-CODE TO STATUS-WS                                   
085300     PERFORM IMS-STATUSKONTROLL                                           
085400     .                                                                    
085500                                                                          
085600 IMS-04-GNP-WDB611 SECTION.                                               
085700     MOVE 'IMS-04' TO CURRENT-IMS-SECTION                                 
085800                                                                          
085900     STRING 'WDB611  (TEELMT   =' W-TEELMT-X ')'                          
086000          DELIMITED BY SIZE INTO SSA1                                     
086100     MOVE '  GE' TO GODK-STATUSKODER                                      
086200     CALL CBLTDLI USING GNP WDB6-PCB DLI-IO-WDB611 SSA1                   
086300     MOVE WDB6-STATUS-CODE TO STATUS-WS                                   
086400     PERFORM IMS-STATUSKONTROLL                                           
086500     .                                                                    
086600                                                                          
086700 IMS-05-GU-WDB611 SECTION.                                                
086800     MOVE 'IMS-05' TO CURRENT-IMS-SECTION                                 
086900                                                                          
087000     STRING 'WDB601  (IDDC     =' W-IDDC-X ')'                            
087100          DELIMITED BY SIZE INTO SSA1                                     
087200     STRING 'WDB611  (TEELMT   =' W-TEELMT-X                              
087300                    '&IDELMTGR =' W-IDELMT-X ')'                          
087400          DELIMITED BY SIZE INTO SSA2                                     
087500     MOVE '  GE' TO GODK-STATUSKODER                                      
087600     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-WDB611 SSA1 SSA2               
087700     MOVE WDB6-STATUS-CODE TO STATUS-WS                                   
087800     PERFORM IMS-STATUSKONTROLL                                           
087900     .                                                                    
088000                                                                          
088100 IMS-06-GHU-WDB611 SECTION.                                               
088200     MOVE 'IMS-06' TO CURRENT-IMS-SECTION                                 
088300                                                                          
088400     STRING 'WDB601  (IDDC     =' W-IDDC-X ')'                            
088500          DELIMITED BY SIZE INTO SSA1                                     
088600     STRING 'WDB611  (TEELMT   =' W-TEELMT-X                              
088700                    '&IDELMTGR =' W-IDELMT-X ')'                          
088800          DELIMITED BY SIZE INTO SSA2                                     
088900     MOVE '  ' TO GODK-STATUSKODER                                        
089000     CALL CBLTDLI USING GHU WDB6-PCB DLI-IO-WDB611 SSA1 SSA2              
089100     MOVE WDB6-STATUS-CODE TO STATUS-WS                                   
089200     PERFORM IMS-STATUSKONTROLL                                           
089300     .                                                                    
089400                                                                          
089500 IMS-07-ISRT-WDB611 SECTION.                                              
089600     MOVE 'IMS-07' TO CURRENT-IMS-SECTION                                 
089700                                                                          
089800     STRING 'WDB601  (IDDC     =' W-IDDC-X ')'                            
089900          DELIMITED BY SIZE INTO SSA1                                     
090000     MOVE   'WDB611  '        TO SSA2                                     
090100     MOVE '  ' TO GODK-STATUSKODER                                        
090200     CALL CBLTDLI USING ISRT WDB6-PCB DLI-IO-WDB611 SSA1 SSA2             
090300     MOVE WDB6-STATUS-CODE TO STATUS-WS                                   
090400     PERFORM IMS-STATUSKONTROLL                                           
090500     .                                                                    
090600                                                                          
090700 IMS-08-DLET-WDB611 SECTION.                                              
090800     MOVE 'IMS-07' TO CURRENT-IMS-SECTION                                 
090900                                                                          
091000     MOVE '  ' TO GODK-STATUSKODER                                        
091100     CALL CBLTDLI USING DLET WDB6-PCB DLI-IO-WDB611                       
091200     MOVE WDB6-STATUS-CODE TO STATUS-WS                                   
091300     PERFORM IMS-STATUSKONTROLL                                           
091400     .                                                                    
091500                                                                          
091510 IMS-09-GU-WDB601-LEV SECTION.                                            
091520     MOVE 'IMS-09' TO CURRENT-IMS-SECTION                                 
091530                                                                          
091540     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
091550          DELIMITED BY SIZE INTO SSA1                                     
091560     MOVE '  GE' TO GODK-STATUSKODER                                      
091570     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-WDB601-LEV SSA1                
091580     MOVE WDB6-STATUS-CODE TO STATUS-WS                                   
091590     PERFORM IMS-STATUSKONTROLL                                           
091591     .                                                                    
091592                                                                          
091600 IMS-10-GU-WDK601 SECTION.                                                
091700     MOVE 'IMS-10' TO CURRENT-IMS-SECTION                                 
091800                                                                          
091900     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
092000          DELIMITED BY SIZE INTO SSA1                                     
092100     MOVE '  GE' TO GODK-STATUSKODER                                      
092200     CALL CBLTDLI USING GU WDK6-PCB DLI-IO-WDK601 SSA1                    
092300     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
092400     PERFORM IMS-STATUSKONTROLL                                           
092500     .                                                                    
092600                                                                          
092700 IMS-11-ISRT-WDR601 SECTION.                                              
092800     MOVE 'IMS-11' TO CURRENT-IMS-SECTION                                 
092900                                                                          
093000     MOVE 'WDR601 ' TO SSA1                                               
093100     MOVE '    ' TO GODK-STATUSKODER                                      
093200     CALL CBLTDLI USING ISRT WDR6-PCB DLI-IO-WDR601 SSA1                  
093300     MOVE WDR6-STATUS-CODE TO STATUS-WS                                   
093400     PERFORM IMS-STATUSKONTROLL                                           
093500     .                                                                    
093600     SKIP3                                                                
093700 IMS-STATUSKONTROLL SECTION.                                              
093800                                                                          
093900     SET STATUS-IX TO 1                                                   
094000     SEARCH GODK-STATUS                                                   
094100       AT END                                                             
094200         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
094300         DELIMITED BY SIZE INTO FELTEXT                                   
094400         CALL FELLOG                                                      
094500       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
094600         CONTINUE                                                         
094700     END-SEARCH                                                           
094800     .                                                                    
