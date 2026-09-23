000100     SKIP3                                                                
000200 ID DIVISION.                                                             
000300     SKIP2                                                                
000400 PROGRAM-ID.     W3020400.                                                
000500 AUTHOR.         PETER D.                                                 
000600 DATE-WRITTEN.   SEP   89.                                                
000700                                                                          
000800     REMARKS.                                                             
000900*                                                                         
001000*    FUNKTION.                                                            
001100*        PROGRAMMET LÄSER OCH UPPDATERAR ON-LINE BASEN                    
001200*        WLFSGA.(STORSÄLJARURVAL)                                         
001300*                                                                         
001400*                                                                         
001500*    INDATA.                                                              
001600*        TRANSAKTION: W3T204                                              
001700*        MID:         W3I20401                                            
001800*                                                                         
001900*    UTDATA.                                                              
002000*        MOD:         W3O20401                                            
002100     SKIP3                                                                
002200 ENVIRONMENT DIVISION.                                                    
002300     SKIP2                                                                
002400 DATA DIVISION.                                                           
002500     EJECT                                                                
002600 WORKING-STORAGE SECTION.                                                 
002700                                                                          
002800*    -- CHECKED BY WY2000                                                 
002900 77  PROGRAM-NAMN                PIC X(8) VALUE 'W3020400'.               
003000 77  JA                          PIC X       VALUE 'J'.                   
003100 77  NEJ                         PIC X      VALUE 'N'.                    
003200 77  SPRAK-IX                    PIC X(3)   VALUE SPACE.                  
003300 77  RAD-INDX                    PIC S9(9)  VALUE +0   COMP SYNC.         
003400 77  MAX-MOD-LAENGD              PIC S9(4)  VALUE  +397 COMP SYNC.        
003500 01  WS-TIREGTID.                                                         
003600    03  WS-TIREGTID-HHMMSS       PIC 9(6)    VALUE ZERO.                  
003700    03  FILLER                   PIC 9(2)    VALUE ZERO.                  
003800 01  SW-KONFLIKT                 PIC X.                                   
003900    88  KONFLIKT                             VALUE 'J'.                   
004000 01  SW-PRODSL                   PIC X.                                   
004100    88  PRODSL-EJ-IFYLLD                     VALUE 'N'.                   
004200 01  SW-KONCERN                  PIC X.                                   
004300    88  KONCERN-EJ-IFYLLD                    VALUE 'N'.                   
004400 01  SW-DISTRIKT                 PIC X.                                   
004500    88  DISTRIKT-EJ-IFYLLD                   VALUE 'N'.                   
004600 01  SW-MARKNAD                  PIC X.                                   
004700    88  MARKNAD-EJ-IFYLLD                    VALUE 'N'.                   
004800 01  SW-NYCKLAR-OK               PIC X.                                   
004900    88  NYCKLAR-OK                           VALUE 'J'.                   
005000 01  SW-INDATA-OK                PIC X.                                   
005100    88  INDATA-OK                            VALUE 'J'.                   
005200 01  WS-IDTRANS                  PIC X(4).                                
005300    88  EGEN-BILD                            VALUE '3204'.                
005400     EJECT                                                                
005500                                                                          
005600*01  -COPY WWPRODSL                                                       
005700                                                                          
005800 01  NYCKLAR-TILL-DLI.                                                    
005900   03  FILLER                    PIC X(16)   VALUE                        
006000                                            'NYCKLAR-TILL-DLI'.           
006100   03  W-WDM301KY-X.                                                      
006200     05  W-IDUSER                PIC  X(8)   VALUE SPACE.                 
006300     05  W-DAREGDAT              PIC 9(8)    VALUE ZERO.                  
006400     05  W-TIREGTID              PIC S9(7)   VALUE ZERO  COMP-3.          
006500   03  KDSEGKEY-X.                                                        
006600     05  W-KDSEGKEY              PIC  X(1)   VALUE '1'.                   
006700     EJECT                                                                
006800 01  DYNAMISKA-SUBPROGRAM.                                                
006900   03  FELLOG                    PIC X(8)    VALUE 'FELLOG  '.            
007000   03  CBLTDLI                   PIC X(8)    VALUE 'CBLTDLI '.            
007100   03  WMEDKONV                  PIC X(8)    VALUE 'WMEDKONV'.            
007200 01  MEDDELANDE.                                                          
007300   03  FILLER                    PIC X(16)   VALUE 'MEDDELANDE'.          
007400   03  FEL1.                                                              
007500     05 FILLER                   PIC X(40)                                
007600          VALUE 'UPPLYSTA FÄLT FEL'.                                      
007700     05 FILLER                   PIC X(40)                                
007800          VALUE 'HIGHLIGHTED FIELDS ARE WRONG'.                           
007900   03  FILLER REDEFINES FEL1.                                             
008000     05  FEL-1                   PIC X(40)   OCCURS 2.                    
008100                                                                          
008200   03  FEL2.                                                              
008300     05 FILLER                   PIC X(40)                                
008400          VALUE 'ANGE ETT AV NEDANSTÅENDE URVAL:         '.               
008500     05 FILLER                   PIC X(40)                                
008600          VALUE 'SPECIFY  ONE CHOISE (SEE BELOW)         '.               
008700   03  FILLER REDEFINES FEL2.                                             
008800     05  FEL-2                   PIC X(40)   OCCURS 2.                    
008900                                                                          
009000   03  FEL3.                                                              
009100     05 FILLER                   PIC X(40)                                
009200          VALUE 'TRYCK PF11 VID UPPDATERING'.                             
009300     05 FILLER                   PIC X(40)                                
009400          VALUE 'PRESS PF11 WHEN UPDATE'.                                 
009500   03  FILLER REDEFINES FEL3.                                             
009600     05  FEL-3                   PIC X(40)   OCCURS 2.                    
009700                                                                          
009800   03  FEL4.                                                              
009900     05 FILLER                   PIC X(40)                                
010000          VALUE 'KONFLIKT                                '.               
010100     05 FILLER                   PIC X(40)                                
010200          VALUE 'SPECIFY  ONE CHOISE (SEE BELOW)         '.               
010300   03  FILLER REDEFINES FEL4.                                             
010400     05  FEL-4                   PIC X(40)   OCCURS 2.                    
010500                                                                          
010600   03  MED1.                                                              
010700     05 FILLER                   PIC X(40)                                
010800          VALUE 'UPPDATERING GJORD     '.                                 
010900     05 FILLER                   PIC X(40)                                
011000          VALUE 'FIELDS ARE UPDATED'.                                     
011100   03  FILLER REDEFINES MED1.                                             
011200     05  MED-1                   PIC X(40)   OCCURS 2.                    
011300                                                                          
011400   03  MED2.                                                              
011500     05 FILLER                   PIC X(60)                                
011600         VALUE 'KONCERN             DISTRIKT             MARKNAD'.        
011700     05 FILLER                   PIC X(60)                                
011800          VALUE '                                        '.               
011900   03  FILLER REDEFINES MED2.                                             
012000     05  MED-2                   PIC X(60)   OCCURS 2.                    
012100                                                                          
012200     EJECT                                                                
012300*                                                                         
012400*****************************************************************         
012500*                 FELMEDDELANDEMODUL                                      
012600*****************************************************************         
012700*                                                                         
012800 01  FILLER                      PIC X(16)   VALUE 'WMEDAREA'.            
012900*01  -COPY WMEDAREA.                                                      
013000*++INCLUDE WMEDAREAC0                                                     
013100     EJECT                                                                
013200******************************************************************        
013300*                                                                         
013400*                AREOR FÖR MFS OCH SKÄRMHANTERING                         
013500*                                                                         
013600     SKIP3                                                                
013700 01  FILLER                      PIC X(16)   VALUE 'MID-COPY-WS'.         
013800*01  MID -COPY W3I20401                                                   
013900*++INCLUDE W3I20401C0                                                     
014000     EJECT                                                                
014100 01  FILLER                      PIC X(16)   VALUE 'MSG-COPY-WS'.         
014200*01  -COPY WMSGAREA                                                       
014300*++INCLUDE WMSGAREAC0                                                     
014400     EJECT                                                                
014500*  03  MOD -COPY W3O20401           -RED MSG-AREA.                        
014600*++INCLUDE W3O20401C0                                                     
014700     EJECT                                                                
014800 01  FILLER                      PIC X(16)   VALUE 'MFS-COPY-WS'.         
014900*01  -COPY WMFSAREA                                                       
015000*++INCLUDE WMFSAREAC0                                                     
015100     EJECT                                                                
015200*01  WLFSGA01    -COPY WDM301 -PRE WS-.                                   
015300*++INCLUDE WDM301CCC0                                                     
015400     EJECT                                                                
015500*01  WLFSGA15    -COPY WDM315 -PRE WS-.                                   
015600*++INCLUDE WDM315CCC0                                                     
015700     EJECT                                                                
015800******************************************************************        
015900*                                                                         
016000*        ARBETS-AREOR TILL IMS-SEKTIONERNA                                
016100*                                                                         
016200 01  IMS-WS.                                                              
016300   03  FILLER                    PIC X(16)   VALUE 'IMS-WS     '.         
016400     SKIP3                                                                
016500*                        **** STATUS-KOD FRÅN IMS                         
016600   03  STATUS-WS                 PIC XX.                                  
016700     88  SEGMENT-FINNS                       VALUE '  '.                  
016800     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
016900     SKIP3                                                                
017000   03  GODK-STATUSKODER.                                                  
017100     05  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
017200     SKIP3                                                                
017300 01    SSA1                      PIC X(64).                               
017400 01    SSA2                      PIC X(64).                               
017500     EJECT                                                                
017600*                            IMS FUNKTIONSKODER                           
017700*01    -COPY W0003                                                        
017800 ++INCLUDE W0003CCCC0                                                     
017900     EJECT                                                                
018000************************     DLI INPUT-OUTPUT AREA ***************        
018100 01  FILLER                    PIC X(16)   VALUE 'DLI-IO-AREA'.           
018200 01  DLI-IO-AREA.                                                         
018300   03  IO-AREA                   PIC X(140)  VALUE SPACE.                 
018400     SKIP3                                                                
018500*03  WLFSGA01    -COPY WDM301   -RED IO-AREA.                             
018600*++INCLUDE WDM301CCC0                                                     
018700     EJECT                                                                
018800*03  WLFSGA15    -COPY WDM315   -RED IO-AREA.                             
018900*++INCLUDE WDM315CCC0                                                     
019000     EJECT                                                                
019100 LINKAGE SECTION.                                                         
019200*01  -COPY W0009     -PRE MSG-                                            
019300 ++INCLUDE W0009CCCC0                                                     
019400     SKIP2                                                                
019500*01  -COPY W0008     -PRE FSGA-                                           
019600 ++INCLUDE W0008CCCC0                                                     
019700     05  FILLER                  PIC X.                                   
019800     EJECT                                                                
019900 PROCEDURE DIVISION USING MSG-PCB FSGA-PCB.                               
020000     ENTRY 'DLITCBL' USING MSG-PCB FSGA-PCB.                              
020100                                                                          
020200     PERFORM IMS-GET-MSG                                                  
020300     IF SEGMENT-FINNS                                                     
020400        PERFORM A-INIT                                                    
020500        PERFORM B-GOR-IORDNING-NYCKLAR                                    
020600        IF EGEN-BILD                                                      
020700           IF MFS-UPDATE                                                  
020800              PERFORM C-KOLLA-INDATA                                      
020900              IF INDATA-OK                                                
021000                 IF  KONCERN-EJ-IFYLLD                                    
021100                 AND MARKNAD-EJ-IFYLLD                                    
021200                 AND DISTRIKT-EJ-IFYLLD                                   
021300                    MOVE '004' TO MED-IDMFSFEL                            
021400                    MOVE SPRAK-IX TO MED-IDSKYLT                          
021500                    CALL WMEDKONV USING MED-WMEDAREA                      
021600                    MOVE MED-MFSFEL TO MOD-TEMFSFEL                       
021700                                                                          
021800                    MOVE '102' TO MED-IDMFSINF                            
021900                    MOVE SPRAK-IX TO MED-IDSKYLT                          
022000                    CALL WMEDKONV USING MED-WMEDAREA                      
022100                    MOVE MED-MFSINF TO MOD-TEMFSINF                       
022200                 ELSE                                                     
022300                    IF KONFLIKT                                           
022400                       PERFORM S02-SAETT-NUMFAELT-FEL                     
022500                                                                          
022600                       MOVE '002' TO MED-IDMFSFEL                         
022700                       MOVE SPRAK-IX TO MED-IDSKYLT                       
022800                       CALL WMEDKONV USING MED-WMEDAREA                   
022900                       MOVE MED-MFSFEL TO MOD-TEMFSFEL                    
023000                                                                          
023100                       MOVE '102' TO MED-IDMFSINF                         
023200                       MOVE SPRAK-IX TO MED-IDSKYLT                       
023300                       CALL WMEDKONV USING MED-WMEDAREA                   
023400                       MOVE MED-MFSINF TO MOD-TEMFSINF                    
023500                                                                          
023600                    ELSE                                                  
023700                       PERFORM S03-SAETT-NUMFAELT-RAETT                   
023800                                                                          
023900                       MOVE '101' TO MED-IDMFSINF                         
024000                       MOVE SPRAK-IX TO MED-IDSKYLT                       
024100                       CALL WMEDKONV USING MED-WMEDAREA                   
024200                       MOVE MED-MFSINF TO MOD-TEMFSINF                    
024300                                                                          
024400                       PERFORM D-UPPDATERA                                
024500                    END-IF                                                
024600                 END-IF                                                   
024700              ELSE                                                        
024800                 IF KONFLIKT                                              
024900                    PERFORM S02-SAETT-NUMFAELT-FEL                        
025000                 END-IF                                                   
025100                                                                          
025200                 MOVE '001' TO MED-IDMFSFEL                               
025300                 MOVE SPRAK-IX TO MED-IDSKYLT                             
025400                 CALL WMEDKONV USING MED-WMEDAREA                         
025500                 MOVE MED-MFSFEL TO MOD-TEMFSFEL                          
025600                                                                          
025700              END-IF                                                      
025800           ELSE                                                           
025900                                                                          
026000              MOVE '003' TO MED-IDMFSFEL                                  
026100              MOVE SPRAK-IX TO MED-IDSKYLT                                
026200              CALL WMEDKONV USING MED-WMEDAREA                            
026300              MOVE MED-MFSFEL TO MOD-TEMFSFEL                             
026400              PERFORM F-LAES-IN-IGEN                                      
026500           END-IF                                                         
026600           PERFORM E-MFS-ROER-EJ-FAELT                                    
026700        ELSE                                                              
026800           PERFORM G-FELHANTERING                                         
026900        END-IF                                                            
027000        MOVE MAX-MOD-LAENGD TO MSG-KVLL                                   
027100        PERFORM IMS-INSERT-MSG                                            
027200     END-IF                                                               
027300     MOVE ZERO                                  TO RETURN-CODE            
027400     GOBACK                                                               
027500     .                                                                    
027600     EJECT                                                                
027700 A-INIT SECTION.                                                          
027800     SKIP2                                                                
027900     IF MSG-DUBBLA-TRANSKODER                                             
028000        MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W3I20401                
028100        MOVE MSG-IDTRANS-2                 TO MFS-IDTRANS                 
028200        MOVE MSG-KDMFSFOR-2                TO MFS-KDMFSFOR                
028300        MOVE MSG-KDTRTYP                   TO MFS-KDTRTYP                 
028400        MOVE MSG-IDPFK                     TO MFS-IDPFK                   
028500     ELSE                                                                 
028600        MOVE MSG-INDATA-MINUS-1-TRANSKOD   TO MID-W3I20401                
028700        MOVE MSG-IDTRANS-1                 TO MFS-IDTRANS                 
028800        MOVE MSG-KDMFSFOR-1                TO MFS-KDMFSFOR                
028900        MOVE SPACE                         TO MFS-KDTRTYP                 
029000                                              MFS-IDPFK                   
029100     END-IF                                                               
029200                                                                          
029300     MOVE LOW-VALUE                        TO MSG-AREA                    
029400     MOVE 'W3O204N1'                       TO MFS-IDMOD                   
029500     MOVE '3204'                           TO MOD-IDTRANS                 
029600     MOVE MFS-IDTRANS                      TO WS-IDTRANS                  
029700     MOVE MFS-RENSA-FAELT                  TO MOD-TEMFSFEL                
029800                                              MOD-TEMFSINF                
029900                                              MOD-IDFSGURV-IN             
030000                                              MOD-IDUSER-IN               
030100*                                                                         
030200     IF EGEN-BILD                                                         
030300        CONTINUE                                                          
030400     ELSE                                                                 
030500        MOVE SPACE                         TO MFS-KDTRTYP                 
030600        MOVE '7'                           TO MFS-IDPFK                   
030700     END-IF                                                               
030800     IF ENGLISH-TEXT                                                      
030900        MOVE 'GB '                         TO SPRAK-IX                    
031000     ELSE                                                                 
031100        MOVE 'S  '                         TO SPRAK-IX                    
031200     END-IF                                                               
031300     INITIALIZE WS-WLFSGA01                                               
031400                WS-WLFSGA15                                               
031500     .                                                                    
031600     EJECT                                                                
031700 B-GOR-IORDNING-NYCKLAR SECTION.                                          
031800     SKIP3                                                                
031900     MOVE SPACE                      TO MOD-IDFSGURV-UT                   
032000     IF MID-IDUSER-IN = ALL '+'                                           
032100        MOVE MID-IDUSER-UT           TO W-IDUSER                          
032200     ELSE                                                                 
032300        MOVE MID-IDUSER-IN           TO W-IDUSER                          
032400     END-IF                                                               
032500     IF W-IDUSER = SPACE                                                  
032600        MOVE MSG-SIGNON-USERID       TO MOD-IDUSER-UT                     
032700                                        WS-USER-IDUSER                    
032800                                        W-IDUSER                          
032900     ELSE                                                                 
033000        MOVE W-IDUSER                TO MOD-IDUSER-UT                     
033100                                        WS-USER-IDUSER                    
033200     END-IF                                                               
033300     .                                                                    
033400     EJECT                                                                
033500 C-KOLLA-INDATA SECTION.                                                  
033600     SKIP2                                                                
033700     MOVE JA                             TO SW-INDATA-OK                  
033800     PERFORM CA-KOLLA-KVART                                               
033900     PERFORM CB-KOLLA-KDPRODSL-IDPTYP                                     
034000     PERFORM CC-KOLLA-IDFKNGRP                                            
034100     MOVE NEJ                            TO SW-KONFLIKT                   
034200* SW-KONFLIKT HÅLLER REDA PÅ SÅ ATT ENBART ETT URVAL                      
034300* ANGES AV  KONCERNNR  MARKNAD  OCH     DISTRIKT                          
034400* OBS!! SW-INDATA-OK SÄTTS  E J  TILL NEJ VID KONFLIKT                    
034500* UTAN BEHANDLAS SOM FEL I HUVUDSLINGAN                                   
034600     PERFORM CD-KOLLA-IDKONCNR                                            
034700     PERFORM CE-KOLLA-KDMARK                                              
034800     PERFORM CF-KOLLA-IDDISTR                                             
034900     PERFORM CG-KOLLA-IDLEVNR                                             
035000     PERFORM CH-KOLLA-IDANSK                                              
035100     .                                                                    
035200     EJECT                                                                
035300 CA-KOLLA-KVART      SECTION.                                             
035400     SKIP2                                                                
035500     IF MID-KVART = ALL '+'                                               
035600        IF  MID-KDPRODSL(1)      = ALL '+'                                
035700        AND MID-IDFKNGRP-FOM     = ALL '+'                                
035800        AND MID-IDKONCNR(1)      = ALL '+'                                
035900        AND MID-KDMARK-FOM(1)    = ALL '+'                                
036000        AND MID-IDDISTR-FOM(1)   = ALL '+'                                
036100        AND MID-IDANSK-FOM(1)    = ALL '+'                                
036200           MOVE NEJ                      TO SW-INDATA-OK                  
036300           MOVE MFS-NUM-FAELT-FEL        TO MOD-KVART-ATTR                
036400        ELSE                                                              
036500           MOVE 9999                     TO WS-TOP-KVART                  
036600        END-IF                                                            
036700     ELSE                                                                 
036800        IF  MID-KVART NUMERIC                                             
036900        AND MID-KVART > ZERO                                              
037000           MOVE MFS-NUM-FAELT-RAETT      TO MOD-KVART-ATTR                
037100           MOVE MID-KVART                TO WS-TOP-KVART                  
037200        ELSE                                                              
037300           MOVE NEJ                      TO SW-INDATA-OK                  
037400           MOVE MFS-NUM-FAELT-FEL        TO MOD-KVART-ATTR                
037500        END-IF                                                            
037600     END-IF                                                               
037700     .                                                                    
037800     EJECT                                                                
037900 CB-KOLLA-KDPRODSL-IDPTYP   SECTION.                                      
038000     SKIP2                                                                
038100     MOVE +1                             TO RAD-INDX                      
038200     MOVE NEJ TO SW-PRODSL                                                
038300     PERFORM UNTIL RAD-INDX > 4                                           
038400       IF MID-KDPRODSL(RAD-INDX) = ALL '+'                                
038500          CONTINUE                                                        
038600       ELSE                                                               
038700          IF MID-KDPRODSL(RAD-INDX)  NUMERIC                              
038800             MOVE MID-KDPRODSL(RAD-INDX)                                  
038900                                 TO TEST-KDPRODSL                         
039000             IF KDPRODSL-VOLVO-BIMA OR                                    
039100                TEST-KDPRODSL = ZEROES                                    
039200                MOVE MFS-NUM-FAELT-RAETT TO                               
039300                                    MOD-KDPRODSL-ATTR(RAD-INDX)           
039400                MOVE MID-KDPRODSL(RAD-INDX)  TO                           
039500                                     WS-TOP-KDPRODSL(RAD-INDX)            
039600                MOVE JA TO SW-PRODSL                                      
039700             ELSE                                                         
039800                MOVE NEJ                   TO SW-INDATA-OK                
039900                MOVE MFS-NUM-FAELT-FEL TO                                 
040000                                    MOD-KDPRODSL-ATTR  (RAD-INDX)         
040100             END-IF                                                       
040200          ELSE                                                            
040300             MOVE NEJ                      TO SW-INDATA-OK                
040400             MOVE MFS-NUM-FAELT-FEL   TO                                  
040500                                MOD-KDPRODSL-ATTR(RAD-INDX)               
040600          END-IF                                                          
040700       END-IF                                                             
040800     ADD +1                          TO RAD-INDX                          
040900     END-PERFORM                                                          
041000     IF PRODSL-EJ-IFYLLD                                                  
041100          MOVE NEJ                 TO SW-INDATA-OK                        
041200          MOVE MFS-NUM-FAELT-FEL   TO MOD-KDPRODSL-ATTR(1)                
041300     END-IF                                                               
041400     IF MID-IDPTYP = ALL '+'                                              
041500        MOVE NEJ                         TO SW-INDATA-OK                  
041600        MOVE MFS-ALFA-FAELT-FEL          TO MOD-IDPTYP-ATTR               
041700     ELSE                                                                 
041800        IF MID-IDPTYP = 'S1' OR 'S2' OR 'S3'                              
041900                    OR  'S1A' OR 'S2A' OR 'S3A'                           
042000           MOVE MFS-ALFA-FAELT-RAETT     TO MOD-IDPTYP-ATTR               
042100           MOVE MID-IDPTYP               TO WS-TOP-IDPTYP                 
042200        ELSE                                                              
042300           MOVE NEJ                      TO SW-INDATA-OK                  
042400           MOVE MFS-ALFA-FAELT-FEL       TO MOD-IDPTYP-ATTR               
042500        END-IF                                                            
042600     END-IF                                                               
042700     .                                                                    
042800     EJECT                                                                
042900 CC-KOLLA-IDFKNGRP          SECTION.                                      
043000     SKIP2                                                                
043100     IF  MID-IDFKNGRP-FOM = ALL '+'                                       
043200     AND MID-IDFKNGRP-TOM = ALL '+'                                       
043300        CONTINUE                                                          
043400     ELSE                                                                 
043500        IF  MID-IDFKNGRP-FOM NUMERIC                                      
043600        AND MID-IDFKNGRP-TOM NUMERIC                                      
043700           IF  MID-IDFKNGRP-TOM NOT <                                     
043800           MID-IDFKNGRP-FOM                                               
043900              MOVE MFS-NUM-FAELT-RAETT  TO MOD-IDFKNGRP-FOM-ATTR          
044000                                           MOD-IDFKNGRP-TOM-ATTR          
044100              MOVE MID-IDFKNGRP-FOM     TO                                
044200                                  WS-TOP-IDFKNGRP-FOM                     
044300              MOVE MID-IDFKNGRP-TOM     TO                                
044400                                  WS-TOP-IDFKNGRP-TOM                     
044500           ELSE                                                           
044600              MOVE NEJ                  TO SW-INDATA-OK                   
044700              MOVE MFS-NUM-FAELT-FEL    TO MOD-IDFKNGRP-FOM-ATTR          
044800                                           MOD-IDFKNGRP-TOM-ATTR          
044900           END-IF                                                         
045000        ELSE                                                              
045100           MOVE NEJ                      TO SW-INDATA-OK                  
045200           IF  MID-IDFKNGRP-FOM     = ALL '+'                             
045300              MOVE MFS-NUM-FAELT-FEL    TO MOD-IDFKNGRP-TOM-ATTR          
045400           ELSE                                                           
045500              IF  MID-IDFKNGRP-TOM     = ALL '+'                          
045600                 MOVE MFS-NUM-FAELT-FEL TO MOD-IDFKNGRP-FOM-ATTR          
045700              ELSE                                                        
045800                 MOVE MFS-NUM-FAELT-FEL TO MOD-IDFKNGRP-FOM-ATTR          
045900                                           MOD-IDFKNGRP-TOM-ATTR          
046000              END-IF                                                      
046100           END-IF                                                         
046200        END-IF                                                            
046300     END-IF                                                               
046400     .                                                                    
046500     EJECT                                                                
046600 CD-KOLLA-IDKONCNR          SECTION.                                      
046700     SKIP1                                                                
046800     MOVE NEJ                            TO SW-KONCERN                    
046900     MOVE +1                             TO RAD-INDX                      
047000     PERFORM UNTIL RAD-INDX > 8                                           
047100        IF MID-IDKONCNR (RAD-INDX) = ALL '+'                              
047200           CONTINUE                                                       
047300        ELSE                                                              
047400           MOVE JA                       TO SW-KONCERN                    
047500           IF MID-IDKONCNR (RAD-INDX) NUMERIC                             
047600              MOVE MFS-NUM-FAELT-RAETT   TO                               
047700                                    MOD-IDKONCNR-ATTR (RAD-INDX)          
047800              MOVE MID-IDKONCNR (RAD-INDX) TO                             
047900                                     WS-TOP-IDKONCNR (RAD-INDX)           
048000           ELSE                                                           
048100              MOVE NEJ                   TO SW-INDATA-OK                  
048200              MOVE MFS-NUM-FAELT-FEL     TO                               
048300                                    MOD-IDKONCNR-ATTR (RAD-INDX)          
048400           END-IF                                                         
048500        END-IF                                                            
048600        ADD +1                           TO RAD-INDX                      
048700     END-PERFORM                                                          
048800     .                                                                    
048900     EJECT                                                                
049000 CE-KOLLA-KDMARK            SECTION.                                      
049100     SKIP2                                                                
049200     MOVE NEJ                            TO SW-MARKNAD                    
049300     MOVE +1                             TO RAD-INDX                      
049400     PERFORM UNTIL RAD-INDX > 4                                           
049500        IF  MID-KDMARK-FOM (RAD-INDX) = ALL '+'                           
049600        AND MID-KDMARK-TOM (RAD-INDX) = ALL '+'                           
049700           CONTINUE                                                       
049800        ELSE                                                              
049900           MOVE JA                        TO SW-MARKNAD                   
050000           IF SW-KONCERN = JA                                             
050100              MOVE JA                     TO SW-KONFLIKT                  
050200           END-IF                                                         
050300           IF  MID-KDMARK-FOM (RAD-INDX) NUMERIC                          
050400           AND MID-KDMARK-FOM (RAD-INDX) > ZERO                           
050500           AND MID-KDMARK-FOM (RAD-INDX) < 100                            
050600           AND MID-KDMARK-TOM (RAD-INDX) NUMERIC                          
050700           AND MID-KDMARK-TOM (RAD-INDX) > ZERO                           
050800           AND MID-KDMARK-TOM (RAD-INDX) < 100                            
050900              IF MID-KDMARK-TOM (RAD-INDX) NOT <                          
051000              MID-KDMARK-FOM (RAD-INDX)                                   
051100                 MOVE MFS-NUM-FAELT-RAETT TO                              
051200                           MOD-KDMARK-FOM-ATTR (RAD-INDX)                 
051300                           MOD-KDMARK-TOM-ATTR (RAD-INDX)                 
051400                 MOVE MID-KDMARK-FOM (RAD-INDX) TO                        
051500                                  WS-TOP-KDMARK-FOM (RAD-INDX)            
051600                 MOVE MID-KDMARK-TOM (RAD-INDX) TO                        
051700                                  WS-TOP-KDMARK-TOM (RAD-INDX)            
051800              ELSE                                                        
051900                 MOVE NEJ                 TO SW-INDATA-OK                 
052000                 MOVE MFS-NUM-FAELT-FEL   TO                              
052100                           MOD-KDMARK-FOM-ATTR (RAD-INDX)                 
052200                           MOD-KDMARK-TOM-ATTR (RAD-INDX)                 
052300              END-IF                                                      
052400           ELSE                                                           
052500              MOVE NEJ                    TO SW-INDATA-OK                 
052600              IF MID-KDMARK-FOM (RAD-INDX)   = ALL '+'                    
052700                 MOVE MFS-NUM-FAELT-FEL   TO                              
052800                                 MOD-KDMARK-TOM-ATTR (RAD-INDX)           
052900              ELSE                                                        
053000                 IF MID-KDMARK-TOM (RAD-INDX) = ALL '+'                   
053100                    MOVE MFS-NUM-FAELT-FEL TO                             
053200                                 MOD-KDMARK-FOM-ATTR (RAD-INDX)           
053300                 ELSE                                                     
053400                    MOVE MFS-NUM-FAELT-FEL TO                             
053500                                 MOD-KDMARK-FOM-ATTR (RAD-INDX)           
053600                                 MOD-KDMARK-TOM-ATTR (RAD-INDX)           
053700                 END-IF                                                   
053800              END-IF                                                      
053900           END-IF                                                         
054000        END-IF                                                            
054100        ADD +1                           TO RAD-INDX                      
054200     END-PERFORM                                                          
054300     .                                                                    
054400     EJECT                                                                
054500 CF-KOLLA-IDDISTR           SECTION.                                      
054600     SKIP2                                                                
054700     MOVE NEJ                            TO SW-DISTRIKT                   
054800     MOVE +1                             TO RAD-INDX                      
054900     PERFORM UNTIL RAD-INDX > 4                                           
055000        IF  MID-IDDISTR-FOM (RAD-INDX) = ALL '+'                          
055100        AND MID-IDDISTR-TOM (RAD-INDX) = ALL '+'                          
055200           CONTINUE                                                       
055300        ELSE                                                              
055400           MOVE JA                        TO SW-DISTRIKT                  
055500           IF SW-MARKNAD = JA                                             
055600           OR SW-KONCERN = JA                                             
055700              MOVE JA                     TO SW-KONFLIKT                  
055800           END-IF                                                         
055900           IF MID-IDDISTR-FOM  (RAD-INDX) NUMERIC                         
056000           AND MID-IDDISTR-TOM (RAD-INDX) NUMERIC                         
056100              IF MID-IDDISTR-TOM (RAD-INDX) NOT <                         
056200              MID-IDDISTR-FOM (RAD-INDX)                                  
056300                 MOVE MFS-NUM-FAELT-RAETT TO                              
056400                           MOD-IDDISTR-FOM-ATTR (RAD-INDX)                
056500                           MOD-IDDISTR-TOM-ATTR (RAD-INDX)                
056600                 MOVE MID-IDDISTR-FOM (RAD-INDX) TO                       
056700                                  WS-TOP-IDDISTR-FOM (RAD-INDX)           
056800                 MOVE MID-IDDISTR-TOM (RAD-INDX) TO                       
056900                                  WS-TOP-IDDISTR-TOM (RAD-INDX)           
057000              ELSE                                                        
057100                 MOVE NEJ                 TO SW-INDATA-OK                 
057200                 MOVE MFS-NUM-FAELT-FEL  TO                               
057300                          MOD-IDDISTR-FOM-ATTR (RAD-INDX)                 
057400                          MOD-IDDISTR-TOM-ATTR (RAD-INDX)                 
057500              END-IF                                                      
057600           ELSE                                                           
057700              MOVE NEJ                    TO SW-INDATA-OK                 
057800              IF MID-IDDISTR-FOM (RAD-INDX)   = ALL '+'                   
057900                 MOVE MFS-NUM-FAELT-FEL   TO                              
058000                                 MOD-IDDISTR-TOM-ATTR (RAD-INDX)          
058100              ELSE                                                        
058200                 IF MID-IDDISTR-TOM (RAD-INDX) = ALL '+'                  
058300                    MOVE MFS-NUM-FAELT-FEL TO                             
058400                                 MOD-IDDISTR-FOM-ATTR (RAD-INDX)          
058500                 ELSE                                                     
058600                    MOVE MFS-NUM-FAELT-FEL TO                             
058700                                 MOD-IDDISTR-FOM-ATTR (RAD-INDX)          
058800                                 MOD-IDDISTR-TOM-ATTR (RAD-INDX)          
058900                 END-IF                                                   
059000              END-IF                                                      
059100           END-IF                                                         
059200        END-IF                                                            
059300        ADD +1                           TO RAD-INDX                      
059400     END-PERFORM                                                          
059500     .                                                                    
059600     EJECT                                                                
059700 CG-KOLLA-IDLEVNR           SECTION.                                      
059800     SKIP1                                                                
059900     MOVE +1                             TO RAD-INDX                      
060000     PERFORM UNTIL RAD-INDX > 8                                           
060100        IF MID-IDLEVNR (RAD-INDX) = ALL '+'                               
060200           CONTINUE                                                       
060300        ELSE                                                              
060400              MOVE MFS-ALFA-FAELT-RAETT   TO                              
060500                                    MOD-IDLEVNR-ATTR (RAD-INDX)           
060600              MOVE MID-IDLEVNR (RAD-INDX) TO                              
060700                                     WS-TOP-IDLEVNR (RAD-INDX)            
060800        END-IF                                                            
060900        ADD +1                           TO RAD-INDX                      
061000     END-PERFORM                                                          
061100     .                                                                    
061200     EJECT                                                                
061300 CH-KOLLA-IDANSK            SECTION.                                      
061400     SKIP2                                                                
061500     MOVE +1                             TO RAD-INDX                      
061600     PERFORM UNTIL RAD-INDX > 4                                           
061700        IF  MID-IDANSK-FOM (RAD-INDX) = ALL '+'                           
061800        AND MID-IDANSK-TOM (RAD-INDX) = ALL '+'                           
061900           CONTINUE                                                       
062000        ELSE                                                              
062100           IF  MID-IDANSK-FOM (RAD-INDX) NUMERIC                          
062200           AND MID-IDANSK-TOM (RAD-INDX) NUMERIC                          
062300              IF MID-IDANSK-TOM (RAD-INDX) NOT <                          
062400              MID-IDANSK-FOM (RAD-INDX)                                   
062500                 MOVE MFS-NUM-FAELT-RAETT TO                              
062600                           MOD-IDANSK-FOM-ATTR (RAD-INDX)                 
062700                           MOD-IDANSK-TOM-ATTR (RAD-INDX)                 
062800                 MOVE MID-IDANSK-FOM (RAD-INDX) TO                        
062900                                  WS-TOP-IDANSK-FOM (RAD-INDX)            
063000                 MOVE MID-IDANSK-TOM (RAD-INDX) TO                        
063100                                  WS-TOP-IDANSK-TOM (RAD-INDX)            
063200              ELSE                                                        
063300                 MOVE NEJ                 TO SW-INDATA-OK                 
063400                 MOVE MFS-NUM-FAELT-FEL   TO                              
063500                           MOD-IDANSK-FOM-ATTR (RAD-INDX)                 
063600                           MOD-IDANSK-TOM-ATTR (RAD-INDX)                 
063700              END-IF                                                      
063800           ELSE                                                           
063900              MOVE NEJ                    TO SW-INDATA-OK                 
064000              IF MID-IDANSK-FOM (RAD-INDX)   = ALL '+'                    
064100                 MOVE MFS-NUM-FAELT-FEL   TO                              
064200                                 MOD-IDANSK-TOM-ATTR (RAD-INDX)           
064300              ELSE                                                        
064400                 IF MID-IDANSK-TOM (RAD-INDX) = ALL '+'                   
064500                    MOVE MFS-NUM-FAELT-FEL TO                             
064600                                 MOD-IDANSK-FOM-ATTR (RAD-INDX)           
064700                 ELSE                                                     
064800                    MOVE MFS-NUM-FAELT-FEL TO                             
064900                                 MOD-IDANSK-FOM-ATTR (RAD-INDX)           
065000                                 MOD-IDANSK-TOM-ATTR (RAD-INDX)           
065100                 END-IF                                                   
065200              END-IF                                                      
065300           END-IF                                                         
065400        END-IF                                                            
065500        ADD +1                           TO RAD-INDX                      
065600     END-PERFORM                                                          
065700     .                                                                    
065800     EJECT                                                                
065900 D-UPPDATERA SECTION.                                                     
066000     SKIP2                                                                
066100     PERFORM S01-FIXA-NYCKLAR-OCH-WS-AREA                                 
066200     PERFORM IMS-GHU-FSGA01                                               
066300     IF SEGMENT-SAKNAS                                                    
066400     MOVE WS-WLFSGA01 TO WLFSGA01                                         
066500     PERFORM IMS-ISRT-FSGA01                                              
066600     PERFORM IMS-GHU-FSGA01                                               
066700     MOVE '1'                  TO WS-TOP-KDSEGKEY                         
066800     MOVE WS-WLFSGA15 TO WLFSGA15                                         
066900     PERFORM IMS-ISRT-FSGA15                                              
067000     END-IF                                                               
067100     .                                                                    
067200     EJECT                                                                
067300 E-MFS-ROER-EJ-FAELT SECTION.                                             
067400     SKIP2                                                                
067500        MOVE MFS-ROER-EJ-FAELT             TO MOD-KVART                   
067600        MOVE MFS-ROER-EJ-FAELT             TO MOD-IDPTYP                  
067700        MOVE MFS-ROER-EJ-FAELT             TO MOD-IDFKNGRP-FOM            
067800        MOVE MFS-ROER-EJ-FAELT             TO MOD-IDFKNGRP-TOM            
067900     MOVE +1                               TO RAD-INDX                    
068000     PERFORM UNTIL RAD-INDX > 8                                           
068100        IF RAD-INDX > 4                                                   
068200              MOVE MFS-ROER-EJ-FAELT       TO                             
068300                                    MOD-IDKONCNR     (RAD-INDX)           
068400                                    MOD-IDLEVNR      (RAD-INDX)           
068500        ELSE                                                              
068600              MOVE MFS-ROER-EJ-FAELT       TO                             
068700                                    MOD-KDPRODSL     (RAD-INDX)           
068800                                    MOD-IDKONCNR     (RAD-INDX)           
068900                                    MOD-IDLEVNR      (RAD-INDX)           
069000                                    MOD-KDMARK-FOM   (RAD-INDX)           
069100                                    MOD-KDMARK-TOM   (RAD-INDX)           
069200                                    MOD-IDDISTR-FOM  (RAD-INDX)           
069300                                    MOD-IDDISTR-TOM  (RAD-INDX)           
069400                                    MOD-IDANSK-FOM   (RAD-INDX)           
069500                                    MOD-IDANSK-TOM   (RAD-INDX)           
069600        END-IF                                                            
069700        ADD +1                               TO RAD-INDX                  
069800     END-PERFORM                                                          
069900     .                                                                    
070000     EJECT                                                                
070100 F-LAES-IN-IGEN SECTION.                                                  
070200     SKIP2                                                                
070300     MOVE MFS-ADD-LAES-IN-FAELT   TO                                      
070400                                     MOD-KVART-ATTR                       
070500                                     MOD-IDPTYP-ATTR                      
070600                                     MOD-IDFKNGRP-FOM-ATTR                
070700                                     MOD-IDFKNGRP-TOM-ATTR                
070800     MOVE +1                      TO RAD-INDX                             
070900     PERFORM UNTIL RAD-INDX > 8                                           
071000        IF RAD-INDX > 4                                                   
071100           MOVE MFS-ADD-LAES-IN-FAELT TO                                  
071200                               MOD-IDKONCNR-ATTR     (RAD-INDX)           
071300                               MOD-IDLEVNR-ATTR      (RAD-INDX)           
071400        ELSE                                                              
071500           MOVE MFS-ADD-LAES-IN-FAELT TO                                  
071600                                MOD-KDPRODSL-ATTR    (RAD-INDX)           
071700                                MOD-IDKONCNR-ATTR    (RAD-INDX)           
071800                                MOD-IDLEVNR-ATTR     (RAD-INDX)           
071900                                MOD-KDMARK-FOM-ATTR  (RAD-INDX)           
072000                                MOD-KDMARK-TOM-ATTR  (RAD-INDX)           
072100                                MOD-IDDISTR-FOM-ATTR (RAD-INDX)           
072200                                MOD-IDDISTR-TOM-ATTR (RAD-INDX)           
072300                                MOD-IDANSK-FOM-ATTR  (RAD-INDX)           
072400                                MOD-IDANSK-TOM-ATTR  (RAD-INDX)           
072500        END-IF                                                            
072600        ADD +1                    TO RAD-INDX                             
072700     END-PERFORM                                                          
072800     .                                                                    
072900     EJECT                                                                
073000 G-FELHANTERING SECTION.                                                  
073100     SKIP2                                                                
073200     MOVE MFS-RENSA-FAELT         TO MOD-IDFSGURV-UT                      
073300                                     MOD-IDUSER-UT                        
073400                                     MOD-KVART                            
073500                                     MOD-IDPTYP                           
073600                                     MOD-IDFKNGRP-FOM                     
073700                                     MOD-IDFKNGRP-TOM                     
073800     MOVE +1                      TO RAD-INDX                             
073900     PERFORM UNTIL RAD-INDX > 8                                           
074000        IF RAD-INDX > 4                                                   
074100           MOVE MFS-RENSA-FAELT   TO MOD-IDKONCNR    (RAD-INDX)           
074200                                     MOD-IDLEVNR     (RAD-INDX)           
074300        ELSE                                                              
074400           MOVE MFS-RENSA-FAELT   TO MOD-KDPRODSL    (RAD-INDX)           
074500                                     MOD-IDKONCNR    (RAD-INDX)           
074600                                     MOD-IDLEVNR     (RAD-INDX)           
074700                                     MOD-KDMARK-FOM  (RAD-INDX)           
074800                                     MOD-KDMARK-TOM  (RAD-INDX)           
074900                                     MOD-IDDISTR-FOM (RAD-INDX)           
075000                                     MOD-IDDISTR-TOM (RAD-INDX)           
075100                                     MOD-IDANSK-FOM  (RAD-INDX)           
075200                                     MOD-IDANSK-TOM  (RAD-INDX)           
075300        END-IF                                                            
075400        ADD +1                    TO RAD-INDX                             
075500     END-PERFORM                                                          
075600     .                                                                    
075700     EJECT                                                                
075800 S01-FIXA-NYCKLAR-OCH-WS-AREA   SECTION.                                  
075900     SKIP1                                                                
076000     MOVE FUNCTION CURRENT-DATE(1:8) TO  W-DAREGDAT                       
076100     ACCEPT WS-TIREGTID  FROM TIME                                        
076200     MOVE WS-TIREGTID-HHMMSS      TO W-TIREGTID                           
076300     MOVE W-DAREGDAT              TO WS-USER-DAREGDAT                     
076400     MOVE W-TIREGTID              TO WS-USER-TIREGTID                     
076500     MOVE SPACE                   TO WS-USER-IDFSGURV                     
076600     MOVE '3204'                  TO WS-USER-IDTRANS                      
076700     MOVE 'J'                     TO WS-USER-FLLISTA                      
076800     .                                                                    
076900     EJECT                                                                
077000 S02-SAETT-NUMFAELT-FEL SECTION.                                          
077100     SKIP1                                                                
077200     MOVE +1                        TO RAD-INDX                           
077300     PERFORM UNTIL RAD-INDX > 8                                           
077400        IF RAD-INDX > 4                                                   
077500           IF MID-IDKONCNR (RAD-INDX) = ALL '+'                           
077600              CONTINUE                                                    
077700           ELSE                                                           
077800              MOVE MFS-NUM-FAELT-FEL TO                                   
077900                               MOD-IDKONCNR-ATTR (RAD-INDX)               
078000           END-IF                                                         
078100        ELSE                                                              
078200           IF MID-IDKONCNR (RAD-INDX) = ALL '+'                           
078300              CONTINUE                                                    
078400           ELSE                                                           
078500              MOVE MFS-NUM-FAELT-FEL TO                                   
078600                               MOD-IDKONCNR-ATTR (RAD-INDX)               
078700           END-IF                                                         
078800           IF MID-KDMARK-FOM (RAD-INDX) = ALL '+'                         
078900              CONTINUE                                                    
079000           ELSE                                                           
079100              MOVE MFS-NUM-FAELT-FEL TO                                   
079200                               MOD-KDMARK-FOM-ATTR (RAD-INDX)             
079300           END-IF                                                         
079400           IF MID-KDMARK-TOM (RAD-INDX) = ALL '+'                         
079500              CONTINUE                                                    
079600           ELSE                                                           
079700              MOVE MFS-NUM-FAELT-FEL TO                                   
079800                               MOD-KDMARK-TOM-ATTR (RAD-INDX)             
079900           END-IF                                                         
080000           IF MID-IDDISTR-FOM (RAD-INDX) = ALL '+'                        
080100              CONTINUE                                                    
080200           ELSE                                                           
080300              MOVE MFS-NUM-FAELT-FEL TO                                   
080400                               MOD-IDDISTR-FOM-ATTR (RAD-INDX)            
080500           END-IF                                                         
080600           IF MID-IDDISTR-TOM (RAD-INDX) = ALL '+'                        
080700              CONTINUE                                                    
080800           ELSE                                                           
080900              MOVE MFS-NUM-FAELT-FEL TO                                   
081000                               MOD-IDDISTR-TOM-ATTR (RAD-INDX)            
081100           END-IF                                                         
081200           IF MID-IDANSK-FOM (RAD-INDX) = ALL '+'                         
081300              CONTINUE                                                    
081400           ELSE                                                           
081500              MOVE MFS-NUM-FAELT-FEL TO                                   
081600                               MOD-IDANSK-FOM-ATTR (RAD-INDX)             
081700           END-IF                                                         
081800           IF MID-IDANSK-TOM (RAD-INDX) = ALL '+'                         
081900              CONTINUE                                                    
082000           ELSE                                                           
082100              MOVE MFS-NUM-FAELT-FEL TO                                   
082200                               MOD-IDANSK-TOM-ATTR (RAD-INDX)             
082300           END-IF                                                         
082400        END-IF                                                            
082500        ADD +1                      TO RAD-INDX                           
082600     END-PERFORM                                                          
082700     .                                                                    
082800     EJECT                                                                
082900 S03-SAETT-NUMFAELT-RAETT SECTION.                                        
083000     SKIP1                                                                
083100     MOVE +1                        TO RAD-INDX                           
083200     PERFORM UNTIL RAD-INDX > 8                                           
083300        IF RAD-INDX > 4                                                   
083400           IF MID-IDKONCNR (RAD-INDX) = ALL '+'                           
083500              CONTINUE                                                    
083600           ELSE                                                           
083700              MOVE MFS-NUM-FAELT-RAETT TO                                 
083800                               MOD-IDKONCNR-ATTR (RAD-INDX)               
083900           END-IF                                                         
084000        ELSE                                                              
084100           IF MID-IDKONCNR (RAD-INDX) = ALL '+'                           
084200              CONTINUE                                                    
084300           ELSE                                                           
084400              MOVE MFS-NUM-FAELT-RAETT TO                                 
084500                               MOD-IDKONCNR-ATTR (RAD-INDX)               
084600           END-IF                                                         
084700           IF MID-KDMARK-FOM (RAD-INDX) = ALL '+'                         
084800              CONTINUE                                                    
084900           ELSE                                                           
085000              MOVE MFS-NUM-FAELT-RAETT TO                                 
085100                               MOD-KDMARK-FOM-ATTR (RAD-INDX)             
085200           END-IF                                                         
085300           IF MID-KDMARK-TOM (RAD-INDX) = ALL '+'                         
085400              CONTINUE                                                    
085500           ELSE                                                           
085600              MOVE MFS-NUM-FAELT-RAETT TO                                 
085700                               MOD-KDMARK-TOM-ATTR (RAD-INDX)             
085800           END-IF                                                         
085900           IF MID-IDDISTR-FOM (RAD-INDX) = ALL '+'                        
086000              CONTINUE                                                    
086100           ELSE                                                           
086200              MOVE MFS-NUM-FAELT-RAETT TO                                 
086300                               MOD-IDDISTR-FOM-ATTR (RAD-INDX)            
086400           END-IF                                                         
086500           IF MID-IDDISTR-TOM (RAD-INDX) = ALL '+'                        
086600              CONTINUE                                                    
086700           ELSE                                                           
086800              MOVE MFS-NUM-FAELT-RAETT TO                                 
086900                               MOD-IDDISTR-TOM-ATTR (RAD-INDX)            
087000           END-IF                                                         
087100           IF MID-IDANSK-FOM (RAD-INDX) = ALL '+'                         
087200              CONTINUE                                                    
087300           ELSE                                                           
087400              MOVE MFS-NUM-FAELT-RAETT TO                                 
087500                               MOD-IDANSK-FOM-ATTR (RAD-INDX)             
087600           END-IF                                                         
087700           IF MID-IDANSK-TOM (RAD-INDX) = ALL '+'                         
087800              CONTINUE                                                    
087900           ELSE                                                           
088000              MOVE MFS-NUM-FAELT-RAETT TO                                 
088100                               MOD-IDANSK-TOM-ATTR (RAD-INDX)             
088200           END-IF                                                         
088300        END-IF                                                            
088400        ADD +1                      TO RAD-INDX                           
088500     END-PERFORM                                                          
088600     .                                                                    
088700     EJECT                                                                
088800 IMS-GET-MSG SECTION.                                                     
088900     SKIP1                                                                
089000     MOVE '  QC' TO GODK-STATUSKODER                                      
089100     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
089200     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
089300     PERFORM IMS-STATUSKONTROLL                                           
089400     .                                                                    
089500     SKIP3                                                                
089600 IMS-INSERT-MSG SECTION.                                                  
089700     SKIP1                                                                
089800     IF NOT ENGLISH-TEXT                                                  
089900       MOVE '0' TO MFS-KDHUVOMR                                           
090000     END-IF                                                               
090100     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
090200     MOVE SPACE TO GODK-STATUSKODER                                       
090300     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
090400     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
090500     PERFORM IMS-STATUSKONTROLL                                           
090600     .                                                                    
090700     SKIP3                                                                
090800 IMS-GHU-FSGA01 SECTION.                                                  
090900     SKIP1                                                                
091000     STRING 'WLFSGA01(WDM301KY =' W-WDM301KY-X ')'                        
091100            DELIMITED BY SIZE INTO SSA1                                   
091200     MOVE '  GE' TO GODK-STATUSKODER                                      
091300     CALL CBLTDLI USING GHU FSGA-PCB DLI-IO-AREA SSA1                     
091400     MOVE FSGA-STATUS-CODE TO STATUS-WS                                   
091500     PERFORM IMS-STATUSKONTROLL                                           
091600     .                                                                    
091700     SKIP3                                                                
091800 IMS-ISRT-FSGA01 SECTION.                                                 
091900     SKIP1                                                                
092000     MOVE   'WLFSGA01 '            TO SSA1                                
092100     MOVE '  II' TO GODK-STATUSKODER                                      
092200     CALL CBLTDLI USING ISRT FSGA-PCB DLI-IO-AREA SSA1                    
092300     MOVE FSGA-STATUS-CODE TO STATUS-WS                                   
092400     PERFORM IMS-STATUSKONTROLL                                           
092500     .                                                                    
092600     SKIP3                                                                
092700 IMS-ISRT-FSGA15 SECTION.                                                 
092800     SKIP1                                                                
092900     MOVE   'WLFSGA15 '            TO SSA1                                
093000     MOVE '  ' TO GODK-STATUSKODER                                        
093100     CALL CBLTDLI USING ISRT FSGA-PCB DLI-IO-AREA SSA1                    
093200     MOVE FSGA-STATUS-CODE TO STATUS-WS                                   
093300     PERFORM IMS-STATUSKONTROLL                                           
093400     .                                                                    
093500     SKIP3                                                                
093600 IMS-STATUSKONTROLL SECTION.                                              
093700     SKIP1                                                                
093800     SET STATUS-IX TO 1                                                   
093900     SEARCH GODK-STATUS AT END CALL FELLOG                                
094000       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS NEXT SENTENCE             
094100     END-SEARCH                                                           
094200     .                                                                    
