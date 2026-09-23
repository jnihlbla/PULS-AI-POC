000200 ID DIVISION.                                                             
000300     SKIP2                                                                
000400 PROGRAM-ID.     W1015400.                                                
000500 AUTHOR.         PETER D.                                                 
000600 DATE-WRITTEN.   APR   88.                                                
000700                                                                          
000900*                                                                         
001000*    FUNKTION.                                                            
001100*        PROGRAMMET LÄSER OCH UPPDATERAR ON-LINE. MATA IN ETT PROD        
001200*        SLAG SÅ VISAS FUNKTIONSITERVALL FÖR RESP BEREDARE                
001300*        DESSA KAN ÄVEN UPPPDATERAS                                       
001400*                                                                         
001500*    INDATA.                                                              
001600*        TRANSAKTION: W1T154                                              
001700*        MID:         W1I15401                                            
001800*                                                                         
001900*    UTDATA.                                                              
002000*        MOD:         W1O15401                                            
002100     SKIP3                                                                
002200 ENVIRONMENT DIVISION.                                                    
002300     SKIP2                                                                
002400 DATA DIVISION.                                                           
002500     EJECT                                                                
002600 WORKING-STORAGE SECTION.                                                 
002601                                                                          
002610*    -- CHECKED BY WY2000                                                 
002700 77  PROGRAM-NAMN                PIC X(8) VALUE 'W1015400'.               
002800 77  WS-KDPRODSL                 PIC X(2) VALUE SPACE.                    
002900 77  WS-IDFKNGRP-FOM-IN          PIC X(4) VALUE SPACE.                    
003000 01  WS-IDFKNGRP-TOM-IN          PIC X(4).                                
003100 01  FILLER REDEFINES WS-IDFKNGRP-TOM-IN.                                 
003200   03  WSS-IDFKNGRP-TOM-IN       PIC 9(4).                                
003300 01  WS-IDBERED-IN               PIC X(2).                                
003400 01  FILLER REDEFINES WS-IDBERED-IN.                                      
003500   03  WSS-IDBERED-IN            PIC 9(2).                                
003600 77  WS-IDFKNGRP-FOM-JFR         PIC 9(5) VALUE ZERO.                     
003700 77  WS-IDFKNGRP-TOM-JFR         PIC 9(5) VALUE ZERO.                     
003800 77  WS-IDFKNGRP-FOM-SPAR        PIC 9(5) VALUE ZERO.                     
003900 77  WS-IDFKNGRP-TOM-SPAR        PIC 9(5) VALUE ZERO.                     
004000 77  WS-IDFKNGRP-FOM-HI          PIC 9(4) VALUE ZERO.                     
004100 77  W-XXAS11-1136-IDFKNGRP-FOM  PIC X(5) VALUE HIGH-VALUE.               
004200 77  W-XXAS11-1136-IDFKNGRP-TOM  PIC X(5) VALUE HIGH-VALUE.               
004300 77  JA                          PIC X       VALUE 'J'.                   
004400 77  NEJ                         PIC X       VALUE 'N'.                   
004500 77  SPRAK-IX                    PIC S9(9)   VALUE +0   COMP SYNC.        
004600 77  INDX                        PIC S9(9)   VALUE +0   COMP SYNC.        
004700 77  RAD-INDX                    PIC S9(9)   VALUE +0   COMP SYNC.        
004800 77  COL-INDX                    PIC S9(9)   VALUE +0   COMP SYNC.        
004900 77  MAX-RAD-MINUS-1             PIC S9(9)   VALUE +10  COMP SYNC.        
005000 77  MAX-COL                     PIC S9(9)   VALUE  +3  COMP SYNC.        
005100 77  MAX-MOD-LAENGD              PIC S9(4)  VALUE  +830 COMP SYNC.        
005110                                                                          
005120 01  DYNAMISKA-SUBPROGRAM.                                                
005130     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
005140     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
005150                                                                          
005200 01  SW-NYCKLAR-OK               PIC X.                                   
005300    88  NYCKLAR-OK                           VALUE 'J'.                   
005400 01  SW-INDATA-OK                PIC X.                                   
005500    88  INDATA-OK                            VALUE 'J'.                   
005600 01  WS-IDTRANS                  PIC X(4).                                
005700    88  GODKAEND-BILD                        VALUE '1154'.                
005800    88  EGEN-BILD                            VALUE '1154'.                
005900 01  W-KDSVAR                    PIC X.                                   
006000    88  GODKAENT-SVAR                        VALUE 'B' 'Ä' 'N'.           
006100    88  BORTTAG                              VALUE 'B'.                   
006200    88  AENDRING                             VALUE 'Ä'.                   
006300    88  NYUPPLAEGG                           VALUE 'N'.                   
006400     EJECT                                                                
006500 01  NYCKLAR-TILL-DLI.                                                    
006600   03  FILLER                    PIC X(16)   VALUE                        
006700                                            'NYCKLAR-TILL-DLI'.           
006800   03  W-1135-KEY-X.                                                      
006900     05  W-IDHTYP                PIC   X(4)  VALUE '1135'.                
007000     05  W-KDPRODSL              PIC  S9(3)  VALUE ZERO  COMP-3.          
007100     05  W-LOWVALUE              PIC  X(24)  VALUE LOW-VALUE.             
007200   03  W-1136-KEY-X.                                                      
007300     05  W-IDFKNGRP-FOM          PIC S9(5)   VALUE ZERO  COMP-3.          
007400     05  W-ILOWVALUE             PIC  X(2)   VALUE LOW-VALUE.             
007500     EJECT                                                                
007600 01  MEDDELANDE.                                                          
007700   03  FILLER                    PIC X(16)   VALUE 'MEDDELANDE'.          
007800   03  FEL1.                                                              
007900     05 FILLER                   PIC X(40)                                
008000          VALUE 'UPPLYSTA FÄLT FEL'.                                      
008100     05 FILLER                   PIC X(40)                                
008200          VALUE 'CORRECT HIGHLIGHTED FIELDS  '.                           
008300   03  FILLER REDEFINES FEL1.                                             
008400     05  FEL-1                   PIC X(40)   OCCURS 2.                    
008500                                                                          
008600   03  FEL2.                                                              
008700     05 FILLER                   PIC X(40)                                
008800          VALUE 'NYCKLAR EJ GODKÄNDA '.                                   
008900     05 FILLER                   PIC X(40)                                
009000          VALUE 'KEYS NOT VALID  '.                                       
009100   03  FILLER REDEFINES FEL2.                                             
009200     05  FEL-2                   PIC X(40)   OCCURS 2.                    
009300                                                                          
009400   03  FEL3.                                                              
009500     05 FILLER                   PIC X(40)                                
009600          VALUE 'NYCKLAR SAKNAS'.                                         
009700     05 FILLER                   PIC X(40)                                
009800          VALUE 'KEYS NOT FOUND  '.                                       
009900   03  FILLER REDEFINES FEL3.                                             
010000     05  FEL-3                   PIC X(40)   OCCURS 2.                    
010100                                                                          
010200   03  MED1.                                                              
010300     05 FILLER                   PIC X(40)                                
010400          VALUE 'UPPLYST RAD UPPDATERAD'.                                 
010500     05 FILLER                   PIC X(40)                                
010600          VALUE 'UPDATED                       '.                         
010700   03  FILLER REDEFINES MED1.                                             
010800     05  MED-1                   PIC X(40)   OCCURS 2.                    
010900                                                                          
011000   03  MED2.                                                              
011100     05 FILLER                   PIC X(40)                                
011200          VALUE 'TRYCK PF8 FÖR FLERA RADER'.                              
011300     05 FILLER                   PIC X(40)                                
011400          VALUE 'PRESS PF8 FOR MORE LINES'.                               
011500   03  FILLER REDEFINES MED2.                                             
011600     05  MED-2                   PIC X(40)   OCCURS 2.                    
011700                                                                          
011800   03  MED3.                                                              
011900     05 FILLER                   PIC X(40)                                
012000          VALUE 'DETTA ÄR FÖRSTA SIDAN'.                                  
012100     05 FILLER                   PIC X(40)                                
012200          VALUE 'THIS IS THE FIRST PAGE'.                                 
012300   03  FILLER REDEFINES MED3.                                             
012400     05  MED-3                   PIC X(40)   OCCURS 2.                    
012500                                                                          
012600   03  MED4.                                                              
012700     05 FILLER                   PIC X(40)                                
012800          VALUE ' VID ÄNDR AV INTERV ANV FUNK B OCH N '.                  
012900     05 FILLER                   PIC X(40)                                
013000          VALUE 'CH. INTERV. BY USING FUNC. D AND N      '.               
013100   03  FILLER REDEFINES MED4.                                             
013200     05  MED-4                   PIC X(40)   OCCURS 2.                    
013300                                                                          
013400   03  MED5.                                                              
013500     05 FILLER                   PIC X(40)                                
013600          VALUE 'MISSL UPPD. DEL AV INTERV FINNS REDAN'.                  
013700     05 FILLER                   PIC X(40)                                
013800          VALUE 'ISRT FAIL. PART OF INTERV.ALRDY EXIST'.                  
013900   03  FILLER REDEFINES MED5.                                             
014000     05  MED-5                   PIC X(40)   OCCURS 2.                    
014100                                                                          
014200   03  MED6.                                                              
014300     05 FILLER                   PIC X(40)                                
014400          VALUE 'UPPLYSTA FÄLT MÅSTE VARA NUMERISKA'.                     
014500     05 FILLER                   PIC X(40)                                
014600          VALUE 'HIGHLIGHTED FIELDS MUST BE NUMERIC'.                     
014700   03  FILLER REDEFINES MED6.                                             
014800     05  MED-6                   PIC X(40)   OCCURS 2.                    
014900                                                                          
015000   03  MED7.                                                              
015100     05 FILLER                   PIC X(40)                                
015200          VALUE 'UPPLYSTA FÄLT MÅSTE VARA IFYLLDA'.                       
015300     05 FILLER                   PIC X(40)                                
015400          VALUE 'PF11 AND NO INPUT                   '.                   
015500   03  FILLER REDEFINES MED7.                                             
015600     05  MED-7                   PIC X(40)   OCCURS 2.                    
015700                                                                          
015800   03  MED8.                                                              
015900     05 FILLER                   PIC X(40)                                
016000          VALUE 'FOM > TOM'.                                              
016100     05 FILLER                   PIC X(40)                                
016200          VALUE 'FOM > TOM'.                                              
016300   03  FILLER REDEFINES MED8.                                             
016400     05  MED-8                   PIC X(40)   OCCURS 2.                    
016500                                                                          
016600   03  MED9.                                                              
016700     05 FILLER                   PIC X(40)                                
016800          VALUE 'TRYCK PF11 VID UPPDATERING'.                             
016900     05 FILLER                   PIC X(40)                                
017000          VALUE 'PRESS PF11 TO UPDATE  '.                                 
017100   03  FILLER REDEFINES MED9.                                             
017200     05  MED-9                   PIC X(40)   OCCURS 2.                    
017300                                                                          
017400   03  MED10.                                                             
017500     05 FILLER                   PIC X(40)                                
017600          VALUE 'UPPDATERING GJORD'.                                      
017700     05 FILLER                   PIC X(40)                                
017800          VALUE 'UPDATED          '.                                      
017900   03  FILLER REDEFINES MED10.                                            
018000     05  MED-10                  PIC X(40)   OCCURS 2.                    
018100                                                                          
018200   03  MED11.                                                             
018300     05 FILLER                   PIC X(40)                                
018400          VALUE ' FUNKTIONS-INTERVALL SAKNAS'.                            
018500     05 FILLER                   PIC X(40)                                
018600          VALUE 'FUNC.-INTERV. IS MISSING'.                               
018700   03  FILLER REDEFINES MED11.                                            
018800     05  MED-11                  PIC X(40)   OCCURS 2.                    
018900                                                                          
019000     EJECT                                                                
019100******************************************************************        
019200*                                                                         
019300*                AREOR FÖR MFS OCH SKÄRMHANTERING                         
019400*                                                                         
019500     SKIP3                                                                
019600 01  FILLER                      PIC X(16)   VALUE 'MID-COPY-WS'.         
019700*01  MID -COPY W1I15401                                                   
019900     EJECT                                                                
020000 01  FILLER                      PIC X(16)   VALUE 'MSG-COPY-WS'.         
020100*01  -COPY WMSGAREA                                                       
020300     EJECT                                                                
020400*  03  MOD -COPY W1O15401           -RED MSG-AREA.                        
020600     EJECT                                                                
020700 01  FILLER                      PIC X(16)   VALUE 'MFS-COPY-WS'.         
020800*01  -COPY WMFSAREA                                                       
021000     EJECT                                                                
021100******************************************************************        
021200*                                                                         
021300*        ARBETS-AREOR TILL IMS-SEKTIONERNA                                
021400*                                                                         
021500 01  IMS-WS.                                                              
021600   03  FILLER                    PIC X(16)   VALUE 'IMS-WS     '.         
021700     SKIP3                                                                
021800*                        **** STATUS-KOD FRÅN IMS                         
021900   03  STATUS-WS                 PIC XX.                                  
022000     88  SEGMENT-FINNS                       VALUE '  '.                  
022100     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
022200     SKIP3                                                                
022300   03  GODK-STATUSKODER.                                                  
022400     05  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
022500     SKIP3                                                                
022600 01    SSA1                      PIC X(64).                               
022700 01    SSA2                      PIC X(64).                               
022800     EJECT                                                                
022900*                            IMS FUNKTIONSKODER                           
023000*01    -COPY W0003                                                        
023200     EJECT                                                                
023300************************     DLI INPUT-OUTPUT AREA ***************        
023400 01  FILLER                    PIC X(16)   VALUE 'DLI-IO-AREA'.           
023500 01  DLI-IO-AREA.                                                         
023600   03  IO-AREA                   PIC X(100)  VALUE SPACE.                 
023700     SKIP3                                                                
023800*03  WLXXAS01    -COPY WDGX1135  -PRE XXAS01-  -RED IO-AREA.              
024000     EJECT                                                                
024100*03  WLXXAS11    -COPY WDGX1136  -PRE XXAS11-  -RED IO-AREA.              
024300     EJECT                                                                
024400 LINKAGE SECTION.                                                         
024500*01  -COPY W0009     -PRE MSG-                                            
024700     SKIP2                                                                
024800*01  -COPY W0008     -PRE XXAS-                                           
025000     05  FILLER                  PIC X.                                   
025100     EJECT                                                                
025200 PROCEDURE DIVISION USING MSG-PCB XXAS-PCB.                               
025300     ENTRY 'DLITCBL' USING MSG-PCB XXAS-PCB.                              
025400                                                                          
025500     PERFORM IMS-GET-MSG                                                  
025600     IF SEGMENT-FINNS                                                     
025700        PERFORM A-INIT                                                    
025800        PERFORM B-GOR-IORDNING-NYCKLAR                                    
025900        IF NYCKLAR-OK                                                     
026000           IF MFS-UPDATE                                                  
026100              PERFORM C-KOLLA-INDATA                                      
026200              IF INDATA-OK                                                
026300                 PERFORM D-UPPDATERA                                      
026400                 IF BORTTAG                                               
026500                    MOVE MED-10  (SPRAK-IX)     TO MOD-TEMFSINF           
026600                 ELSE                                                     
026700                    MOVE MED-1   (SPRAK-IX)     TO MOD-TEMFSINF           
026800                 END-IF                                                   
026900                 PERFORM G-VISA-SIDAN                                     
027000              ELSE                                                        
027100                 PERFORM E-MFS-ROER-EJ-FAELT                              
027200              END-IF                                                      
027300           ELSE                                                           
027400              PERFORM G-VISA-SIDAN                                        
027500           END-IF                                                         
027600        ELSE                                                              
027700           PERFORM F-FELHANTERING                                         
027800        END-IF                                                            
027900        MOVE MAX-MOD-LAENGD TO MSG-KVLL                                   
028000        PERFORM IMS-INSERT-MSG                                            
028100     END-IF                                                               
028200     MOVE ZERO                            TO RETURN-CODE                  
028300     GOBACK                                                               
028400     .                                                                    
028500     EJECT                                                                
028600 A-INIT SECTION.                                                          
028700     SKIP2                                                                
028800     IF MSG-DUBBLA-TRANSKODER                                             
028900        MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W1I15401                
029000        MOVE MSG-IDTRANS-2                 TO MFS-IDTRANS                 
029100        MOVE MSG-KDMFSFOR-2                TO MFS-KDMFSFOR                
029200        MOVE MSG-KDTRTYP                   TO MFS-KDTRTYP                 
029300        MOVE MSG-IDPFK                     TO MFS-IDPFK                   
029400     ELSE                                                                 
029500        MOVE MSG-INDATA-MINUS-1-TRANSKOD   TO MID-W1I15401                
029600        MOVE MSG-IDTRANS-1                 TO MFS-IDTRANS                 
029700        MOVE MSG-KDMFSFOR-1                TO MFS-KDMFSFOR                
029800        MOVE SPACE                         TO MFS-KDTRTYP                 
029900                                              MFS-IDPFK                   
030000     END-IF                                                               
030100                                                                          
030200     MOVE LOW-VALUE                        TO MSG-AREA                    
030300     MOVE 'W1O154N1'                       TO MFS-IDMOD                   
030400     MOVE '1154'                           TO MOD-IDTRANS                 
030500     MOVE MFS-IDTRANS                      TO WS-IDTRANS                  
030600     MOVE MFS-RENSA-FAELT                  TO MOD-TEMFSFEL                
030700                                              MOD-TEMFSINF                
030800                                              MOD-KDPRODSL-IN             
030900*                                                                         
031000     IF EGEN-BILD                                                         
031100        CONTINUE                                                          
031200     ELSE                                                                 
031300        MOVE SPACE                         TO MFS-KDTRTYP                 
031400        MOVE '7'                           TO MFS-IDPFK                   
031500     END-IF                                                               
031600     IF ENGLISH-TEXT                                                      
031700        MOVE +2                            TO SPRAK-IX                    
031800     ELSE                                                                 
031900        MOVE +1                            TO SPRAK-IX                    
032000     END-IF                                                               
032100     .                                                                    
032200     EJECT                                                                
032300 B-GOR-IORDNING-NYCKLAR SECTION.                                          
032400     SKIP3                                                                
032500     MOVE JA                         TO SW-NYCKLAR-OK                     
032600     SKIP2                                                                
032700     IF MID-KDPRODSL-IN = ALL '+'                                         
032800        MOVE MID-KDPRODSL-UT         TO WS-KDPRODSL                       
032900     ELSE                                                                 
033000        MOVE MID-KDPRODSL-IN         TO WS-KDPRODSL                       
033100        MOVE '7'                     TO MFS-IDPFK                         
033200        MOVE SPACE                   TO MFS-KDTRTYP                       
033300     END-IF                                                               
033400     INSPECT WS-KDPRODSL     REPLACING LEADING SPACE                      
033500                                             BY ZERO                      
033600     IF WS-KDPRODSL    NUMERIC                                            
033700        MOVE WS-KDPRODSL              TO  W-KDPRODSL                      
033800     ELSE                                                                 
033900        MOVE NEJ                     TO SW-NYCKLAR-OK                     
034000        MOVE FEL-2   (SPRAK-IX)      TO MOD-TEMFSFEL                      
034100     END-IF                                                               
034200     MOVE WS-KDPRODSL                TO MOD-KDPRODSL-UT                   
034300     INSPECT MOD-KDPRODSL-UT REPLACING LEADING ZERO                       
034400                                           BY SPACE                       
034500*                                                                         
034600     IF NYCKLAR-OK                                                        
034700        PERFORM IMS-GU-XXAS01                                             
034800        IF SEGMENT-FINNS                                                  
034900           PERFORM BA-KOLLA-TANGENT                                       
035000        ELSE                                                              
035100           MOVE NEJ                     TO SW-NYCKLAR-OK                  
035200           MOVE FEL-3   (SPRAK-IX)      TO MOD-TEMFSFEL                   
035300        END-IF                                                            
035400     END-IF                                                               
035500     .                                                                    
035600     EJECT                                                                
035700 BA-KOLLA-TANGENT SECTION.                                                
035800     SKIP2                                                                
035900     IF EGEN-BILD                                                         
036000       IF MFS-UPDATE                                                      
036100          CONTINUE                                                        
036200       ELSE                                                               
036300          IF  MID-IDFKNGRP-FOM-IN    = ALL '+' AND                        
036400              MID-IDFKNGRP-TOM-IN    = ALL '+' AND                        
036500              MID-IDBERED-IN         = ALL '+' AND                        
036600              MID-KDSVAR-IN          = ALL '+'                            
036700              MOVE MFS-RENSA-FAELT          TO                            
036800                                           MOD-IDFKNGRP-FOM-IN            
036900                                           MOD-IDFKNGRP-TOM-IN            
037000                                           MOD-IDBERED-IN                 
037100                                           MOD-KDSVAR-IN                  
037200             IF MFS-IDPFK = '8'                                           
037300                MOVE MID-IDFKNGRP-FOM-HI      TO                          
037400                                               WS-IDFKNGRP-FOM-IN         
037500                INSPECT WS-IDFKNGRP-FOM-IN REPLACING LEADING SPACE        
037600                                                          BY ZERO         
037700                IF WS-IDFKNGRP-FOM-IN NUMERIC                             
037800                   IF WS-IDFKNGRP-FOM-IN = ZERO                           
037900                      MOVE MED-3 (SPRAK-IX)   TO MOD-TEMFSFEL             
038000                      MOVE ZERO               TO W-IDFKNGRP-FOM           
038100                   ELSE                                                   
038200                      MOVE WS-IDFKNGRP-FOM-IN  TO W-IDFKNGRP-FOM          
038300                   END-IF                                                 
038400                ELSE                                                      
038500                   MOVE MED-3 (SPRAK-IX)      TO MOD-TEMFSFEL             
038600                   MOVE ZERO                  TO W-IDFKNGRP-FOM           
038700                END-IF                                                    
038800             ELSE                                                         
038900                IF MFS-IDPFK = '7'                                        
039000                   MOVE MED-3 (SPRAK-IX)        TO MOD-TEMFSFEL           
039100                   MOVE ZERO                    TO W-IDFKNGRP-FOM         
039200                ELSE                                                      
039300                   MOVE MID-IDFKNGRP-FOM-LO   TO                          
039400                                             WS-IDFKNGRP-FOM-IN           
039500                   INSPECT WS-IDFKNGRP-FOM-IN REPLACING LEADING           
039600                                                  SPACE BY ZERO           
039700                   IF WS-IDFKNGRP-FOM-IN NUMERIC                          
039800                      MOVE WS-IDFKNGRP-FOM-IN      TO                     
039900                                                 W-IDFKNGRP-FOM           
040000                   ELSE                                                   
040100                      MOVE ZERO                    TO                     
040200                                                 W-IDFKNGRP-FOM           
040300                   END-IF                                                 
040400                END-IF                                                    
040500             END-IF                                                       
040600          ELSE                                                            
040700             MOVE MFS-NUM-FAELT-RAETT       TO                            
040800                                       MOD-IDFKNGRP-FOM-IN-ATTR           
040900                                       MOD-IDFKNGRP-TOM-IN-ATTR           
041000                                       MOD-IDBERED-IN-ATTR                
041100             MOVE MFS-ALFA-FAELT-RAETT     TO                             
041200                                       MOD-KDSVAR-IN-ATTR                 
041300             MOVE MFS-ROER-EJ-FAELT        TO                             
041400                                       MOD-IDFKNGRP-FOM-IN                
041500                                       MOD-IDFKNGRP-TOM-IN                
041600                                       MOD-IDBERED-IN                     
041700                                       MOD-KDSVAR-IN                      
041800             MOVE MID-IDFKNGRP-FOM-LO   TO WS-IDFKNGRP-FOM-IN             
041900             INSPECT WS-IDFKNGRP-FOM-IN REPLACING LEADING                 
042000                                            SPACE BY ZERO                 
042100             IF WS-IDFKNGRP-FOM-IN NUMERIC                                
042200                MOVE WS-IDFKNGRP-FOM-IN      TO                           
042300                                           W-IDFKNGRP-FOM                 
042400             ELSE                                                         
042500                MOVE ZERO                    TO                           
042600                                           W-IDFKNGRP-FOM                 
042700             END-IF                                                       
042800             MOVE MED-9 (SPRAK-IX)      TO MOD-TEMFSFEL                   
042900          END-IF                                                          
043000       END-IF                                                             
043100     ELSE                                                                 
043200        MOVE MED-3 (SPRAK-IX)                TO MOD-TEMFSFEL              
043300        MOVE ZERO                            TO W-IDFKNGRP-FOM            
043400     END-IF                                                               
043500     .                                                                    
043600     EJECT                                                                
043700 C-KOLLA-INDATA SECTION.                                                  
043800     MOVE JA                         TO SW-INDATA-OK                      
043900     IF MID-IDFKNGRP-FOM-IN = ALL '+'                                     
044000        MOVE MFS-RENSA-FAELT         TO MOD-IDFKNGRP-FOM-IN               
044100        MOVE MFS-NUM-FAELT-FEL       TO MOD-IDFKNGRP-FOM-IN-ATTR          
044200        MOVE NEJ                     TO SW-INDATA-OK                      
044300        MOVE FEL-1 (SPRAK-IX)        TO MOD-TEMFSFEL                      
044400        MOVE MED-7 (SPRAK-IX)        TO MOD-TEMFSINF                      
044500     ELSE                                                                 
044600        MOVE MID-IDFKNGRP-FOM-IN     TO WS-IDFKNGRP-FOM-IN                
044700        INSPECT  WS-IDFKNGRP-FOM-IN REPLACING LEADING SPACE               
044800                                                    BY ZERO               
044900        MOVE MFS-ROER-EJ-FAELT       TO MOD-IDFKNGRP-FOM-IN               
045000        IF WS-IDFKNGRP-FOM-IN NUMERIC                                     
045100           MOVE MFS-NUM-FAELT-RAETT TO MOD-IDFKNGRP-FOM-IN-ATTR           
045200        ELSE                                                              
045300           MOVE MFS-NUM-FAELT-FEL    TO MOD-IDFKNGRP-FOM-IN-ATTR          
045400           MOVE NEJ                  TO SW-INDATA-OK                      
045500           MOVE FEL-1 (SPRAK-IX)     TO MOD-TEMFSFEL                      
045600           MOVE MED-6 (SPRAK-IX)     TO MOD-TEMFSINF                      
045700        END-IF                                                            
045800     END-IF                                                               
045900*                                                                         
046000     IF MID-IDFKNGRP-TOM-IN = ALL '+'                                     
046100        MOVE MFS-RENSA-FAELT         TO MOD-IDFKNGRP-TOM-IN               
046200        MOVE MFS-NUM-FAELT-FEL       TO MOD-IDFKNGRP-TOM-IN-ATTR          
046300        MOVE NEJ                     TO SW-INDATA-OK                      
046400        MOVE FEL-1 (SPRAK-IX)        TO MOD-TEMFSFEL                      
046500        MOVE MED-7 (SPRAK-IX)        TO MOD-TEMFSINF                      
046600     ELSE                                                                 
046700        MOVE MID-IDFKNGRP-TOM-IN     TO WS-IDFKNGRP-TOM-IN                
046800        INSPECT  WS-IDFKNGRP-TOM-IN REPLACING LEADING SPACE               
046900                                                BY ZERO                   
047000        MOVE MFS-ROER-EJ-FAELT    TO MOD-IDFKNGRP-TOM-IN                  
047100        IF WS-IDFKNGRP-TOM-IN NUMERIC                                     
047200           MOVE MFS-NUM-FAELT-RAETT TO MOD-IDFKNGRP-TOM-IN-ATTR           
047300        ELSE                                                              
047400           MOVE MFS-NUM-FAELT-FEL    TO MOD-IDFKNGRP-TOM-IN-ATTR          
047500           MOVE NEJ                  TO SW-INDATA-OK                      
047600           MOVE FEL-1 (SPRAK-IX)     TO MOD-TEMFSFEL                      
047700           MOVE MED-6 (SPRAK-IX)     TO MOD-TEMFSINF                      
047800        END-IF                                                            
047900     END-IF                                                               
048000*                                                                         
048100*                                                                         
048200     IF MID-IDBERED-IN = ALL '+'                                          
048300        MOVE NEJ                     TO SW-INDATA-OK                      
048400        MOVE MFS-RENSA-FAELT         TO MOD-IDBERED-IN                    
048500        MOVE MFS-NUM-FAELT-FEL       TO MOD-IDBERED-IN-ATTR               
048600        MOVE FEL-1 (SPRAK-IX)           TO MOD-TEMFSFEL                   
048700        MOVE MED-7 (SPRAK-IX)           TO MOD-TEMFSINF                   
048800     ELSE                                                                 
048900        MOVE MID-IDBERED-IN          TO WS-IDBERED-IN                     
049000        INSPECT  WS-IDBERED-IN REPLACING LEADING SPACE                    
049100                                                BY ZERO                   
049200        MOVE MFS-ROER-EJ-FAELT       TO MOD-IDBERED-IN                    
049300        IF WS-IDBERED-IN NUMERIC                                          
049400           MOVE MFS-NUM-FAELT-RAETT TO MOD-IDBERED-IN-ATTR                
049500        ELSE                                                              
049600           MOVE MFS-NUM-FAELT-FEL    TO MOD-IDBERED-IN-ATTR               
049700           MOVE NEJ                  TO SW-INDATA-OK                      
049800           MOVE FEL-1 (SPRAK-IX)     TO MOD-TEMFSFEL                      
049900           MOVE MED-6 (SPRAK-IX)     TO MOD-TEMFSINF                      
050000        END-IF                                                            
050100     END-IF                                                               
050200*                                                                         
050300     IF MID-KDSVAR-IN = ALL '+'                                           
050400        MOVE MFS-RENSA-FAELT            TO MOD-KDSVAR-IN                  
050500        MOVE MFS-ALFA-FAELT-FEL         TO MOD-KDSVAR-IN-ATTR             
050600        MOVE NEJ                        TO SW-INDATA-OK                   
050700        MOVE FEL-1 (SPRAK-IX)           TO MOD-TEMFSFEL                   
050800        MOVE MED-7 (SPRAK-IX)           TO MOD-TEMFSINF                   
050900     ELSE                                                                 
051000        MOVE MID-KDSVAR-IN              TO W-KDSVAR                       
051100        MOVE MFS-ROER-EJ-FAELT          TO MOD-KDSVAR-IN                  
051200        IF GODKAENT-SVAR                                                  
051300           MOVE MFS-ALFA-FAELT-RAETT    TO MOD-KDSVAR-IN-ATTR             
051400        ELSE                                                              
051500           MOVE MFS-ALFA-FAELT-FEL      TO MOD-KDSVAR-IN-ATTR             
051600           MOVE NEJ                     TO SW-INDATA-OK                   
051700           MOVE FEL-1 (SPRAK-IX)        TO MOD-TEMFSFEL                   
051800        END-IF                                                            
051900     END-IF                                                               
052000     IF INDATA-OK                                                         
052100        IF WS-IDFKNGRP-FOM-IN > WS-IDFKNGRP-TOM-IN                        
052200           MOVE MFS-NUM-FAELT-FEL      TO                                 
052300                                       MOD-IDFKNGRP-FOM-IN-ATTR           
052400                                       MOD-IDFKNGRP-TOM-IN-ATTR           
052500           MOVE NEJ                    TO SW-INDATA-OK                    
052600           MOVE FEL-1 (SPRAK-IX)       TO MOD-TEMFSFEL                    
052700           MOVE MED-8 (SPRAK-IX)       TO MOD-TEMFSINF                    
052800        END-IF                                                            
052900     END-IF                                                               
053000     IF INDATA-OK                                                         
053100        MOVE WS-IDFKNGRP-FOM-IN         TO WS-IDFKNGRP-FOM-JFR            
053200        MOVE WS-IDFKNGRP-TOM-IN         TO WS-IDFKNGRP-TOM-JFR            
053300     END-IF                                                               
053400     IF INDATA-OK                                                         
053500        PERFORM CA-KOLLA-MOT-BAS                                          
053600     END-IF                                                               
053700     .                                                                    
053800     EJECT                                                                
053900 CA-KOLLA-MOT-BAS SECTION.                                                
054000     SKIP2                                                                
054100     PERFORM IMS-GU-XXAS01                                                
054200     IF SEGMENT-FINNS                                                     
054300        IF AENDRING                                                       
054400           MOVE WS-IDFKNGRP-FOM-IN        TO W-IDFKNGRP-FOM               
054500           PERFORM IMS-GHU-XXAS11                                         
054600           IF SEGMENT-FINNS                                               
054700              IF XXAS11-1136-IDFKNGRP-TOM = WSS-IDFKNGRP-TOM-IN           
054800                 MOVE WS-IDBERED-IN       TO                              
054900                                            XXAS11-1136-IDBERED           
055000                 PERFORM IMS-REPL-XXAS11                                  
055100              ELSE                                                        
055200                 MOVE MFS-NUM-FAELT-FEL   TO                              
055300                                      MOD-IDFKNGRP-TOM-IN-ATTR            
055400                 MOVE NEJ                 TO SW-INDATA-OK                 
055500                 MOVE FEL-1 (SPRAK-IX)    TO MOD-TEMFSFEL                 
055600                 MOVE MED-4 (SPRAK-IX)    TO MOD-TEMFSINF                 
055700              END-IF                                                      
055800           ELSE                                                           
055900              MOVE MFS-NUM-FAELT-FEL       TO                             
056000                                      MOD-IDFKNGRP-FOM-IN-ATTR            
056100              MOVE NEJ                    TO SW-INDATA-OK                 
056200              MOVE FEL-1 (SPRAK-IX)       TO MOD-TEMFSFEL                 
056300              MOVE MED-4 (SPRAK-IX)       TO MOD-TEMFSINF                 
056400           END-IF                                                         
056500        ELSE                                                              
056600           IF BORTTAG                                                     
056700              MOVE WS-IDFKNGRP-FOM-IN  TO W-IDFKNGRP-FOM                  
056800              PERFORM IMS-GHU-XXAS11                                      
056900              IF SEGMENT-FINNS                                            
057000                 IF XXAS11-1136-IDFKNGRP-TOM =                            
057100                                             WSS-IDFKNGRP-TOM-IN          
057200                    CONTINUE                                              
057300                 ELSE                                                     
057400                    MOVE MFS-NUM-FAELT-FEL   TO                           
057500                                       MOD-IDFKNGRP-TOM-IN-ATTR           
057600                    MOVE FEL-1 (SPRAK-IX)     TO MOD-TEMFSFEL             
057700                    MOVE MED-11  (SPRAK-IX)   TO MOD-TEMFSINF             
057800                    MOVE NEJ                  TO SW-INDATA-OK             
057900                 END-IF                                                   
058000                 IF XXAS11-1136-IDBERED = WSS-IDBERED-IN                  
058100                    MOVE WS-IDFKNGRP-FOM-IN   TO                          
058200                                              W-IDFKNGRP-FOM              
058300                 ELSE                                                     
058400                    MOVE MFS-NUM-FAELT-FEL TO                             
058500                                            MOD-IDBERED-IN-ATTR           
058600                    MOVE NEJ                TO SW-INDATA-OK               
058700                    MOVE FEL-1 (SPRAK-IX)   TO MOD-TEMFSFEL               
058800                    MOVE MED-11  (SPRAK-IX) TO MOD-TEMFSINF               
058900                 END-IF                                                   
059000              ELSE                                                        
059100                 MOVE MFS-NUM-FAELT-FEL TO                                
059200                                       MOD-IDFKNGRP-FOM-IN-ATTR           
059300                 MOVE NEJ                TO SW-INDATA-OK                  
059400                 MOVE FEL-1 (SPRAK-IX)   TO MOD-TEMFSFEL                  
059500                 MOVE MED-11  (SPRAK-IX) TO MOD-TEMFSINF                  
059600              END-IF                                                      
059700           ELSE                                                           
059800              PERFORM CAA-KOLLA-BAS-VID-NYUPP                             
059900           END-IF                                                         
060000        END-IF                                                            
060100     END-IF                                                               
060200     .                                                                    
060300     EJECT                                                                
060400 CAA-KOLLA-BAS-VID-NYUPP SECTION.                                         
060500     PERFORM IMS-GNP-XXAS11                                               
060600     IF SEGMENT-FINNS                                                     
060700        MOVE XXAS11-1136-IDFKNGRP-FOM   TO                                
060800                         W-XXAS11-1136-IDFKNGRP-FOM                       
060900                         WS-IDFKNGRP-FOM-SPAR                             
061000        MOVE XXAS11-1136-IDFKNGRP-TOM   TO                                
061100                         W-XXAS11-1136-IDFKNGRP-TOM                       
061200                         WS-IDFKNGRP-TOM-SPAR                             
061300     ELSE                                                                 
061400        MOVE HIGH-VALUE                 TO                                
061500                         W-XXAS11-1136-IDFKNGRP-FOM                       
061600                         W-XXAS11-1136-IDFKNGRP-TOM                       
061700     END-IF                                                               
061800     PERFORM UNTIL  WS-IDFKNGRP-FOM-JFR <                                 
061900      W-XXAS11-1136-IDFKNGRP-TOM  OR SEGMENT-SAKNAS                       
062000          MOVE   XXAS11-1136-IDFKNGRP-FOM   TO                            
062100                                      WS-IDFKNGRP-FOM-SPAR                
062200          MOVE   XXAS11-1136-IDFKNGRP-TOM   TO                            
062300                                      WS-IDFKNGRP-TOM-SPAR                
062400          PERFORM IMS-GNP-XXAS11                                          
062500          IF SEGMENT-FINNS                                                
062600             MOVE XXAS11-1136-IDFKNGRP-FOM   TO                           
062700                              W-XXAS11-1136-IDFKNGRP-FOM                  
062800             MOVE XXAS11-1136-IDFKNGRP-TOM   TO                           
062900                              W-XXAS11-1136-IDFKNGRP-TOM                  
063000          ELSE                                                            
063100             MOVE HIGH-VALUE                 TO                           
063200                              W-XXAS11-1136-IDFKNGRP-FOM                  
063300                              W-XXAS11-1136-IDFKNGRP-TOM                  
063400          END-IF                                                          
063500     END-PERFORM                                                          
063600     IF SEGMENT-FINNS                                                     
063700        IF WS-IDFKNGRP-FOM-JFR = WS-IDFKNGRP-TOM-SPAR                     
063800           MOVE MFS-NUM-FAELT-FEL       TO                                
063900                                      MOD-IDFKNGRP-FOM-IN-ATTR            
064000                                      MOD-IDFKNGRP-TOM-IN-ATTR            
064100           MOVE NEJ                     TO SW-INDATA-OK                   
064200           MOVE FEL-1 (SPRAK-IX)        TO MOD-TEMFSFEL                   
064300           MOVE MED-5 (SPRAK-IX)        TO MOD-TEMFSINF                   
064400        ELSE                                                              
064500           IF WS-IDFKNGRP-TOM-JFR < W-XXAS11-1136-IDFKNGRP-FOM            
064600              CONTINUE                                                    
064700           ELSE                                                           
064800              MOVE MFS-NUM-FAELT-FEL      TO                              
064900                                      MOD-IDFKNGRP-FOM-IN-ATTR            
065000                                      MOD-IDFKNGRP-TOM-IN-ATTR            
065100              MOVE NEJ                    TO SW-INDATA-OK                 
065200              MOVE FEL-1 (SPRAK-IX)       TO MOD-TEMFSFEL                 
065300              MOVE MED-5 (SPRAK-IX)       TO MOD-TEMFSINF                 
065400           END-IF                                                         
065500        END-IF                                                            
065600     ELSE                                                                 
065700        IF WS-IDFKNGRP-TOM-SPAR > ZERO                                    
065800           IF WS-IDFKNGRP-TOM-SPAR = WS-IDFKNGRP-FOM-JFR                  
065900              MOVE MFS-NUM-FAELT-FEL      TO                              
066000                                      MOD-IDFKNGRP-FOM-IN-ATTR            
066100                                      MOD-IDFKNGRP-TOM-IN-ATTR            
066200              MOVE NEJ                    TO SW-INDATA-OK                 
066300              MOVE FEL-1 (SPRAK-IX)       TO MOD-TEMFSFEL                 
066400              MOVE MED-5 (SPRAK-IX)       TO MOD-TEMFSINF                 
066500           END-IF                                                         
066600        END-IF                                                            
066700     END-IF                                                               
066800     .                                                                    
066900     EJECT                                                                
067000 D-UPPDATERA SECTION.                                                     
067100     IF NYUPPLAEGG                                                        
067200        PERFORM IMS-GHU-XXAS01                                            
067300        MOVE WS-IDFKNGRP-FOM-IN   TO XXAS11-1136-IDFKNGRP-FOM             
067400        MOVE LOW-VALUE            TO XXAS11-1136-LOWVALUE                 
067500        MOVE WS-IDFKNGRP-TOM-IN   TO XXAS11-1136-IDFKNGRP-TOM             
067600        MOVE WS-IDBERED-IN        TO XXAS11-1136-IDBERED                  
067700        PERFORM IMS-ISRT-XXAS11                                           
067800        MOVE WS-IDFKNGRP-FOM-IN   TO W-IDFKNGRP-FOM                       
067900     ELSE                                                                 
068000        IF BORTTAG                                                        
068100           PERFORM IMS-DLET-XXAS11                                        
068200        ELSE                                                              
068300           PERFORM IMS-REPL-XXAS11                                        
068400        END-IF                                                            
068500     END-IF                                                               
068600     .                                                                    
068700     EJECT                                                                
068800 E-MFS-ROER-EJ-FAELT SECTION.                                             
068900     MOVE MFS-ROER-EJ-FAELT         TO                                    
069000                                          MOD-IDFKNGRP-FOM-LO             
069100                                          MOD-IDFKNGRP-FOM-HI             
069200     MOVE +1                         TO COL-INDX                          
069300     MOVE +1                         TO RAD-INDX                          
069400     PERFORM UNTIL COL-INDX > MAX-COL                                     
069500        MOVE MFS-ROER-EJ-FAELT       TO                                   
069600                         MOD-IDFKNGRP-FOM (COL-INDX , RAD-INDX)           
069700                         MOD-IDFKNGRP-TOM (COL-INDX , RAD-INDX)           
069800                         MOD-IDBERED      (COL-INDX , RAD-INDX)           
069900        IF RAD-INDX > MAX-RAD-MINUS-1                                     
070000           ADD +1                       TO COL-INDX                       
070100           MOVE +1                      TO RAD-INDX                       
070200        ELSE                                                              
070300           ADD +1                       TO RAD-INDX                       
070400        END-IF                                                            
070500     END-PERFORM                                                          
070600     .                                                                    
070700     EJECT                                                                
070800 F-FELHANTERING SECTION.                                                  
070900     MOVE +1                         TO COL-INDX                          
071000     MOVE +1                         TO RAD-INDX                          
071100     PERFORM UNTIL COL-INDX > MAX-COL                                     
071200        MOVE MFS-RENSA-FAELT         TO                                   
071300                     MOD-IDFKNGRP-FOM (COL-INDX , RAD-INDX)               
071400                     MOD-IDFKNGRP-TOM (COL-INDX , RAD-INDX)               
071500                     MOD-IDBERED      (COL-INDX , RAD-INDX)               
071600        IF RAD-INDX > MAX-RAD-MINUS-1                                     
071700           ADD +1                       TO COL-INDX                       
071800           MOVE +1                      TO RAD-INDX                       
071900        ELSE                                                              
072000           ADD +1                       TO RAD-INDX                       
072100        END-IF                                                            
072200     END-PERFORM                                                          
072300     MOVE MFS-RENSA-FAELT               TO                                
072400                                        MOD-IDFKNGRP-FOM-IN               
072500                                        MOD-IDFKNGRP-TOM-IN               
072600                                        MOD-IDBERED-IN                    
072700                                        MOD-KDSVAR-IN                     
072800     .                                                                    
072900     EJECT                                                                
073000 G-VISA-SIDAN SECTION.                                                    
073100     MOVE +1                          TO RAD-INDX                         
073200     MOVE +1                          TO COL-INDX                         
073300     PERFORM IMS-GU-XXAS01                                                
073400     IF SEGMENT-FINNS                                                     
073500        PERFORM IMS-GNP-KEY-XXAS11                                        
073600        PERFORM UNTIL COL-INDX > MAX-COL   OR SEGMENT-SAKNAS              
073700           MOVE XXAS11-1136-IDFKNGRP-FOM TO                               
073800                   MOD-IDFKNGRP-FOM (COL-INDX , RAD-INDX)                 
073900           MOVE XXAS11-1136-IDFKNGRP-TOM TO                               
074000                   MOD-IDFKNGRP-TOM (COL-INDX , RAD-INDX)                 
074100           MOVE XXAS11-1136-IDBERED      TO                               
074200                   MOD-IDBERED      (COL-INDX , RAD-INDX)                 
074300           IF RAD-INDX > MAX-RAD-MINUS-1                                  
074400              MOVE +1                    TO RAD-INDX                      
074500              ADD  +1                    TO COL-INDX                      
074600           ELSE                                                           
074700              ADD +1                     TO RAD-INDX                      
074800           END-IF                                                         
074900           PERFORM IMS-GNP-XXAS11                                         
075000        END-PERFORM                                                       
075100        IF RAD-INDX > +1 OR COL-INDX > +1                                 
075200           MOVE MOD-IDFKNGRP-FOM (1 , 1) TO MOD-IDFKNGRP-FOM-LO           
075300        ELSE                                                              
075400           MOVE ZERO                     TO MOD-IDFKNGRP-FOM-LO           
075500        END-IF                                                            
075600        IF MFS-UPDATE                                                     
075700           IF BORTTAG                                                     
075800              CONTINUE                                                    
075900           ELSE                                                           
076000              MOVE MFS-ADD-LYS-UPP-FAELT TO                               
076100                                         MOD-RAD-ATTR (1 , 1)             
076200           END-IF                                                         
076300           MOVE MFS-RENSA-FAELT          TO                               
076400                                          MOD-IDFKNGRP-FOM-IN             
076500                                          MOD-IDFKNGRP-TOM-IN             
076600                                          MOD-IDBERED-IN                  
076700                                          MOD-KDSVAR-IN                   
076800        END-IF                                                            
076900        IF SEGMENT-FINNS                                                  
077000           MOVE XXAS11-1136-IDFKNGRP-FOM TO WS-IDFKNGRP-FOM-HI            
077100           MOVE WS-IDFKNGRP-FOM-HI       TO MOD-IDFKNGRP-FOM-HI           
077200           IF MFS-UPDATE                                                  
077300              CONTINUE                                                    
077400           ELSE                                                           
077500              MOVE MED-2 (SPRAK-IX)      TO MOD-TEMFSINF                  
077600           END-IF                                                         
077700        ELSE                                                              
077800           PERFORM UNTIL COL-INDX > MAX-COL                               
077900              MOVE MFS-RENSA-FAELT       TO                               
078000                         MOD-IDFKNGRP-FOM (COL-INDX , RAD-INDX)           
078100                         MOD-IDFKNGRP-TOM (COL-INDX , RAD-INDX)           
078200                         MOD-IDBERED      (COL-INDX , RAD-INDX)           
078300              IF RAD-INDX > MAX-RAD-MINUS-1                               
078400                 MOVE +1                 TO RAD-INDX                      
078500                 ADD  +1                 TO COL-INDX                      
078600              ELSE                                                        
078700                 ADD +1                  TO RAD-INDX                      
078800              END-IF                                                      
078900           END-PERFORM                                                    
079000           MOVE ZERO                     TO MOD-IDFKNGRP-FOM-HI           
079100        END-IF                                                            
079200     END-IF                                                               
079300     .                                                                    
079400     EJECT                                                                
079500* IMS SEKTIONER                                                           
079600     SKIP3                                                                
079700 IMS-GET-MSG SECTION.                                                     
079800                                                                          
079900     MOVE '  QC' TO GODK-STATUSKODER                                      
080000     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
080100     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
080200     PERFORM IMS-STATUSKONTROLL                                           
080300     SKIP3                                                                
080400     .                                                                    
080500 IMS-INSERT-MSG SECTION.                                                  
080600                                                                          
080700     IF NOT ENGLISH-TEXT                                                  
080800       MOVE '0' TO MFS-KDHUVOMR                                           
080900     END-IF                                                               
081000     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
081100     MOVE SPACE TO GODK-STATUSKODER                                       
081200     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
081300     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
081400     PERFORM IMS-STATUSKONTROLL                                           
081500     EJECT                                                                
081600     .                                                                    
081700 IMS-GU-XXAS01 SECTION.                                                   
081800                                                                          
081900     STRING 'WLXXAS01(WDGXKEY  =' W-1135-KEY-X ')'                        
082000            DELIMITED BY SIZE INTO SSA1                                   
082100     MOVE '  GE' TO GODK-STATUSKODER                                      
082200     CALL CBLTDLI USING GU XXAS-PCB DLI-IO-AREA SSA1                      
082300     MOVE XXAS-STATUS-CODE TO STATUS-WS                                   
082400     PERFORM IMS-STATUSKONTROLL                                           
082500     SKIP3                                                                
082600     .                                                                    
082700 IMS-GHU-XXAS01 SECTION.                                                  
082800                                                                          
082900     STRING 'WLXXAS01(WDGXKEY  =' W-1135-KEY-X ')'                        
083000            DELIMITED BY SIZE INTO SSA1                                   
083100     MOVE '  GE' TO GODK-STATUSKODER                                      
083200     CALL CBLTDLI USING GHU XXAS-PCB DLI-IO-AREA SSA1                     
083300     MOVE XXAS-STATUS-CODE TO STATUS-WS                                   
083400     PERFORM IMS-STATUSKONTROLL                                           
083500     SKIP3                                                                
083600     .                                                                    
083700 IMS-GNP-KEY-XXAS11 SECTION.                                              
083800                                                                          
083900     STRING 'WLXXAS11(WDGXKEY >=' W-1136-KEY-X ')'                        
084000            DELIMITED BY SIZE INTO SSA1                                   
084100     MOVE '  GE' TO GODK-STATUSKODER                                      
084200     CALL CBLTDLI USING GNP XXAS-PCB DLI-IO-AREA SSA1                     
084300     MOVE XXAS-STATUS-CODE TO STATUS-WS                                   
084400     PERFORM IMS-STATUSKONTROLL                                           
084500     SKIP3                                                                
084600     .                                                                    
084700 IMS-GNP-XXAS11 SECTION.                                                  
084800                                                                          
084900     MOVE 'WLXXAS11 '           TO SSA1                                   
085000     MOVE '  GE' TO GODK-STATUSKODER                                      
085100     CALL CBLTDLI USING GNP XXAS-PCB DLI-IO-AREA SSA1                     
085200     MOVE XXAS-STATUS-CODE TO STATUS-WS                                   
085300     PERFORM IMS-STATUSKONTROLL                                           
085400     SKIP3                                                                
085500     .                                                                    
085600 IMS-GHU-XXAS11 SECTION.                                                  
085700                                                                          
085800     STRING 'WLXXAS01(WDGXKEY  =' W-1135-KEY-X ')'                        
085900            DELIMITED BY SIZE INTO SSA1                                   
086000     STRING 'WLXXAS11(WDGXKEY  =' W-1136-KEY-X ')'                        
086100            DELIMITED BY SIZE INTO SSA2                                   
086200     MOVE '  GE' TO GODK-STATUSKODER                                      
086300     CALL CBLTDLI USING GHU XXAS-PCB DLI-IO-AREA SSA1 SSA2                
086400     MOVE XXAS-STATUS-CODE TO STATUS-WS                                   
086500     PERFORM IMS-STATUSKONTROLL                                           
086600     SKIP3                                                                
086700     .                                                                    
086800 IMS-ISRT-XXAS11 SECTION.                                                 
086900                                                                          
087000     MOVE   'WLXXAS11 '            TO SSA2                                
087100     MOVE '  ' TO GODK-STATUSKODER                                        
087200     CALL CBLTDLI USING ISRT XXAS-PCB DLI-IO-AREA SSA1 SSA2               
087300     MOVE XXAS-STATUS-CODE TO STATUS-WS                                   
087400     PERFORM IMS-STATUSKONTROLL                                           
087500     SKIP3                                                                
087600     .                                                                    
087700 IMS-DLET-XXAS11 SECTION.                                                 
087800                                                                          
087900     MOVE '  ' TO GODK-STATUSKODER                                        
088000     CALL CBLTDLI USING DLET XXAS-PCB DLI-IO-AREA                         
088100     MOVE XXAS-STATUS-CODE TO STATUS-WS                                   
088200     PERFORM IMS-STATUSKONTROLL                                           
088300     SKIP3                                                                
088400     .                                                                    
088500 IMS-REPL-XXAS11 SECTION.                                                 
088600                                                                          
088700     MOVE '  ' TO GODK-STATUSKODER                                        
088800     CALL CBLTDLI USING REPL XXAS-PCB DLI-IO-AREA                         
088900     MOVE XXAS-STATUS-CODE TO STATUS-WS                                   
089000     PERFORM IMS-STATUSKONTROLL                                           
089100     SKIP3                                                                
089200     .                                                                    
089300 IMS-STATUSKONTROLL SECTION.                                              
089400                                                                          
089500     SET STATUS-IX TO 1                                                   
089600     SEARCH GODK-STATUS                                                   
089610       AT END                                                             
089620         CALL FELLOG                                                      
089700       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS NEXT SENTENCE.            
089800 IMS-STATUSKONTROLL-EXIT. EXIT.                                           
