000200 ID DIVISION.                                                             
000300     SKIP2                                                                
000400 PROGRAM-ID.     W2015200.                                                
000500 AUTHOR.         PETER D.                                                 
000600 DATE-WRITTEN.   APR   88.                                                
000700                                                                          
000800*    REMARKS.                                                             
000900*                                                                         
001000*    FUNKTION.                                                            
001100*     PROGRAMMET LÄSER OCH UPPDATERAR ON-LINE. MATA IN ETT PRODUKT        
001200*     SLAG SÅ VISAS FUNKTIONSITERVALL FÖR RESP ANSKAFFARE                 
001300*     DESSA KAN ÄVEN UPPPDATERAS                                          
001400*                                                                         
001500*    INDATA.                                                              
001600*        TRANSAKTION: W2T152                                              
001700*        MID:         W2I15201                                            
001800*                                                                         
001900*    UTDATA.                                                              
002000*        MOD:         W2O15201                                            
002100     SKIP3                                                                
002200 ENVIRONMENT DIVISION.                                                    
002300     SKIP2                                                                
002400 DATA DIVISION.                                                           
002500     EJECT                                                                
002600 WORKING-STORAGE SECTION.                                                 
002601                                                                          
002610*    -- CHECKED BY WY2000                                                 
002700 77  PROGRAM-NAMN                PIC X(8) VALUE 'W2015200'.               
002800 77  WS-KDPRODSL                 PIC X(2).                                
002900 77  WS-IDFKNGRP-FOM-IN          PIC X(4).                                
003000 01  WS-IDFKNGRP-TOM-IN          PIC X(4).                                
003100 01  FILLER REDEFINES WS-IDFKNGRP-TOM-IN.                                 
003200   03  WSS-IDFKNGRP-TOM-IN       PIC 9(4).                                
003300 01  WS-IDANSK-IN                PIC X(3).                                
003400 01  FILLER REDEFINES WS-IDANSK-IN.                                       
003500   03  WSS-IDANSK-IN             PIC 9(3).                                
003600 77  WS-IDUSER-IN                PIC X(8).                                
003700 77  WS-IDFKNGRP-FOM-JFR         PIC 9(5) VALUE ZERO.                     
003800 77  WS-IDFKNGRP-TOM-JFR         PIC 9(5) VALUE ZERO.                     
003900 77  WS-IDFKNGRP-FOM-SPAR        PIC 9(5) VALUE ZERO.                     
004000 77  WS-IDFKNGRP-TOM-SPAR        PIC 9(5) VALUE ZERO.                     
004100 77  WS-IDFKNGRP-FOM-HI          PIC 9(4) VALUE ZERO.                     
004200 77  W-XXAT11-1138-IDFKNGRP-FOM  PIC X(5) VALUE HIGH-VALUE.               
004300 77  W-XXAT11-1138-IDFKNGRP-TOM  PIC X(5) VALUE HIGH-VALUE.               
004400 77  JA                          PIC X       VALUE 'J'.                   
004500 77  NEJ                         PIC X       VALUE 'N'.                   
004600 77  SPRAK-IX                    PIC S9(9)   VALUE +0   COMP SYNC.        
004700 77  INDX                        PIC S9(9)   VALUE +0   COMP SYNC.        
004800 77  RAD-INDX                    PIC S9(9)   VALUE +0   COMP SYNC.        
004900 77  COL-INDX                    PIC S9(9)   VALUE +0   COMP SYNC.        
005000 77  MAX-RAD-MINUS-1             PIC S9(9)   VALUE +10  COMP SYNC.        
005100 77  MAX-COL                     PIC S9(9)   VALUE  +2  COMP SYNC.        
005200 77  MAX-MOD-LAENGD              PIC S9(4)  VALUE  +801 COMP SYNC.        
005210                                                                          
005220 01  DYNAMISKA-SUBPROGRAM.                                                
005230     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
005240     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
005250                                                                          
005300 01  SW-NYCKLAR-OK               PIC X.                                   
005400    88  NYCKLAR-OK                           VALUE 'J'.                   
005500 01  SW-INDATA-OK                PIC X.                                   
005600    88  INDATA-OK                            VALUE 'J'.                   
005700 01  WS-IDTRANS                  PIC X(4).                                
005800    88  GODKAEND-BILD                        VALUE '2152'.                
005900    88  EGEN-BILD                            VALUE '2152'.                
006000 01  W-KDSVAR                    PIC X.                                   
006100    88  GODKAENT-SVAR                        VALUE 'B' 'Ä' 'N'            
006110                                                   'D' 'C'.               
006200    88  BORTTAG                              VALUE 'B' 'D'.               
006300    88  AENDRING                             VALUE 'Ä' 'C'.               
006400    88  NYUPPLAEGG                           VALUE 'N'.                   
006500     EJECT                                                                
006600 01  NYCKLAR-TILL-DLI.                                                    
006700   03  FILLER                    PIC X(16)   VALUE                        
006800                                            'NYCKLAR-TILL-DLI'.           
006900   03  W-1137-KEY-X.                                                      
007000     05  W-IDHTYP                PIC   X(4)  VALUE '1137'.                
007100     05  W-KDPRODSL              PIC  S9(3)  VALUE ZERO  COMP-3.          
007200     05  W-LOWVALUE              PIC  X(24)  VALUE LOW-VALUE.             
007300   03  W-1138-KEY-X.                                                      
007400     05  W-IDFKNGRP-FOM          PIC S9(5)   VALUE ZERO  COMP-3.          
007500     05  W-ILOWVALUE             PIC  X(2)   VALUE LOW-VALUE.             
007600     EJECT                                                                
007700 01  MEDDELANDE.                                                          
007800   03  FILLER                    PIC X(16)   VALUE 'MEDDELANDE'.          
007900   03  FEL1.                                                              
008000     05 FILLER                   PIC X(40)                                
008100          VALUE 'UPPLYSTA FÄLT FEL'.                                      
008200     05 FILLER                   PIC X(40)                                
008300          VALUE 'HIGHLIGHTED FIELDS ARE WRONG'.                           
008400   03  FILLER REDEFINES FEL1.                                             
008500     05  FEL-1                   PIC X(40)   OCCURS 2.                    
008600                                                                          
008700   03  FEL2.                                                              
008800     05 FILLER                   PIC X(40)                                
008900          VALUE 'NYCKLAR EJ GODKÄNDA '.                                   
009000     05 FILLER                   PIC X(40)                                
009100          VALUE 'KEYS NOT VALID  '.                                       
009200   03  FILLER REDEFINES FEL2.                                             
009300     05  FEL-2                   PIC X(40)   OCCURS 2.                    
009400                                                                          
009500   03  FEL3.                                                              
009600     05 FILLER                   PIC X(40)                                
009700          VALUE 'NYCKLAR SAKNAS'.                                         
009800     05 FILLER                   PIC X(40)                                
009900          VALUE 'KEYS NOT FOUND  '.                                       
010000   03  FILLER REDEFINES FEL3.                                             
010100     05  FEL-3                   PIC X(40)   OCCURS 2.                    
010200                                                                          
010300   03  MED1.                                                              
010400     05 FILLER                   PIC X(40)                                
010500          VALUE 'UPPLYST RAD UPPDATERAD'.                                 
010600     05 FILLER                   PIC X(40)                                
010700          VALUE 'HIGHLIGHTED FIELDS ARE UPDATED'.                         
010800   03  FILLER REDEFINES MED1.                                             
010900     05  MED-1                   PIC X(40)   OCCURS 2.                    
011000                                                                          
011100   03  MED2.                                                              
011200     05 FILLER                   PIC X(40)                                
011300          VALUE 'TRYCK PF8 FÖR FLERA RADER'.                              
011400     05 FILLER                   PIC X(40)                                
011500          VALUE 'PRESS PF8 FOR MORE LINES'.                               
011600   03  FILLER REDEFINES MED2.                                             
011700     05  MED-2                   PIC X(40)   OCCURS 2.                    
011800                                                                          
011900   03  MED3.                                                              
012000     05 FILLER                   PIC X(40)                                
012100          VALUE 'DETTA ÄR FÖRSTA SIDAN'.                                  
012200     05 FILLER                   PIC X(40)                                
012300          VALUE 'THIS IS THE FIRST PAGE'.                                 
012400   03  FILLER REDEFINES MED3.                                             
012500     05  MED-3                   PIC X(40)   OCCURS 2.                    
012600                                                                          
012700   03  MED4.                                                              
012800     05 FILLER                   PIC X(40)                                
012900          VALUE ' VID ÄNDR AV INTERV ANV FUNK B OCH N '.                  
013000     05 FILLER                   PIC X(40)                                
013100          VALUE 'CH INTERV BY USING FUNC D AND N      '.                  
013200   03  FILLER REDEFINES MED4.                                             
013300     05  MED-4                   PIC X(40)   OCCURS 2.                    
013400                                                                          
013500   03  MED5.                                                              
013600     05 FILLER                   PIC X(40)                                
013700          VALUE 'MISSL UPPD. DEL AV INTERV FINNS REDAN'.                  
013800     05 FILLER                   PIC X(40)                                
013900          VALUE 'ISRT FAIL. PART OF INTERV ALRDY EXIST'.                  
014000   03  FILLER REDEFINES MED5.                                             
014100     05  MED-5                   PIC X(40)   OCCURS 2.                    
014200                                                                          
014300   03  MED6.                                                              
014400     05 FILLER                   PIC X(40)                                
014500          VALUE 'UPPLYSTA FÄLT MÅSTE VARA NUMERISKA'.                     
014600     05 FILLER                   PIC X(40)                                
014700          VALUE 'HIGHLIGHTED FIELDS MUST BE NUMERIC'.                     
014800   03  FILLER REDEFINES MED6.                                             
014900     05  MED-6                   PIC X(40)   OCCURS 2.                    
015000                                                                          
015100   03  MED7.                                                              
015200     05 FILLER                   PIC X(40)                                
015300          VALUE 'UPPLYSTA FÄLT MÅSTE VARA IFYLLDA'.                       
015400     05 FILLER                   PIC X(40)                                
015500          VALUE 'HIGHLIGHTED FIELDS MUST BE FILLED IN'.                   
015600   03  FILLER REDEFINES MED7.                                             
015700     05  MED-7                   PIC X(40)   OCCURS 2.                    
015800                                                                          
015900   03  MED8.                                                              
016000     05 FILLER                   PIC X(40)                                
016100          VALUE 'FOM > TOM'.                                              
016200     05 FILLER                   PIC X(40)                                
016300          VALUE 'FOM > TOM'.                                              
016400   03  FILLER REDEFINES MED8.                                             
016500     05  MED-8                   PIC X(40)   OCCURS 2.                    
016600                                                                          
016700   03  MED9.                                                              
016800     05 FILLER                   PIC X(40)                                
016900          VALUE 'TRYCK PF11 VID UPPDATERING'.                             
017000     05 FILLER                   PIC X(40)                                
017100          VALUE 'PRESS PF11 WHEN UPDATE'.                                 
017200   03  FILLER REDEFINES MED9.                                             
017300     05  MED-9                   PIC X(40)   OCCURS 2.                    
017400                                                                          
017500   03  MED10.                                                             
017600     05 FILLER                   PIC X(40)                                
017700          VALUE 'UPPDATERING GJORD'.                                      
017800     05 FILLER                   PIC X(40)                                
017900          VALUE 'UPDATE HAS BEEN DONE'.                                   
018000   03  FILLER REDEFINES MED10.                                            
018100     05  MED-10                  PIC X(40)   OCCURS 2.                    
018200                                                                          
018300   03  MED11.                                                             
018400     05 FILLER                   PIC X(40)                                
018500          VALUE 'FUNKTIONS-INTERVALL SAKNAS'.                             
018600     05 FILLER                   PIC X(40)                                
018700          VALUE 'FUNC. INTERV. IS MISSING'.                               
018800   03  FILLER REDEFINES MED11.                                            
018900     05  MED-11                  PIC X(40)   OCCURS 2.                    
019000                                                                          
019100     EJECT                                                                
019200******************************************************************        
019300*                                                                         
019400*                AREOR FÖR MFS OCH SKÄRMHANTERING                         
019500*                                                                         
019600     SKIP3                                                                
019700 01  FILLER                      PIC X(16)   VALUE 'MID-COPY-WS'.         
019800*01  MID -COPY W2I15201                                                   
020000     EJECT                                                                
020100 01  FILLER                      PIC X(16)   VALUE 'MSG-COPY-WS'.         
020200*01  -COPY WMSGAREA                                                       
020400     EJECT                                                                
020500*  03  MOD -COPY W2O15201           -RED MSG-AREA.                        
020700     EJECT                                                                
020800 01  FILLER                      PIC X(16)   VALUE 'MFS-COPY-WS'.         
020900*01  -COPY WMFSAREA                                                       
021100     EJECT                                                                
021200******************************************************************        
021300*                                                                         
021400*        ARBETS-AREOR TILL IMS-SEKTIONERNA                                
021500*                                                                         
021600 01  IMS-WS.                                                              
021700   03  FILLER                    PIC X(16)   VALUE 'IMS-WS     '.         
021800     SKIP3                                                                
021900*                        **** STATUS-KOD FRÅN IMS                         
022000   03  STATUS-WS                 PIC XX.                                  
022100     88  SEGMENT-FINNS                       VALUE '  '.                  
022200     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
022300     SKIP3                                                                
022400   03  GODK-STATUSKODER.                                                  
022500     05  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
022600     SKIP3                                                                
022700 01    SSA1                      PIC X(64).                               
022800 01    SSA2                      PIC X(64).                               
022900     EJECT                                                                
023000*                            IMS FUNKTIONSKODER                           
023100*01    -COPY W0003                                                        
023300     EJECT                                                                
023400************************     DLI INPUT-OUTPUT AREA ***************        
023500 01  FILLER                    PIC X(16)   VALUE 'DLI-IO-AREA'.           
023600 01  DLI-IO-AREA.                                                         
023700   03  IO-AREA                   PIC X(100)  VALUE SPACE.                 
023800     SKIP3                                                                
023900*03  WLXXAT01    -COPY WDGX1137  -PRE XXAT01-  -RED IO-AREA.              
024100     EJECT                                                                
024200*03  WLXXAT11    -COPY WDGX1138  -PRE XXAT11-  -RED IO-AREA.              
024400     EJECT                                                                
024500 LINKAGE SECTION.                                                         
024600*01  -COPY W0009     -PRE MSG-                                            
024800     SKIP2                                                                
024900*01  -COPY W0008     -PRE XXAT-                                           
025100     05  FILLER                  PIC X.                                   
025200     EJECT                                                                
025300 PROCEDURE DIVISION USING MSG-PCB XXAT-PCB.                               
025400     ENTRY 'DLITCBL' USING MSG-PCB XXAT-PCB.                              
025500                                                                          
025600     PERFORM IMS-GET-MSG                                                  
025700     IF SEGMENT-FINNS                                                     
025800        PERFORM A-INIT                                                    
025900        PERFORM B-GOR-IORDNING-NYCKLAR                                    
026000        IF NYCKLAR-OK                                                     
026100           IF MFS-UPDATE                                                  
026200              PERFORM C-KOLLA-INDATA                                      
026300              IF INDATA-OK                                                
026400                 PERFORM D-UPPDATERA                                      
026500                 IF BORTTAG                                               
026600                    MOVE MED-10 (SPRAK-IX)    TO MOD-TEMFSINF             
026700                 ELSE                                                     
026800                    MOVE MED-1 (SPRAK-IX)       TO MOD-TEMFSINF           
026900                 END-IF                                                   
027000                 PERFORM G-VISA-SIDAN                                     
027100              ELSE                                                        
027200                 PERFORM E-MFS-ROER-EJ-FAELT                              
027300              END-IF                                                      
027400           ELSE                                                           
027500              PERFORM G-VISA-SIDAN                                        
027600           END-IF                                                         
027700        ELSE                                                              
027800           PERFORM F-FELHANTERING                                         
027900        END-IF                                                            
028000        MOVE MAX-MOD-LAENGD TO MSG-KVLL                                   
028100        PERFORM IMS-INSERT-MSG                                            
028200     END-IF                                                               
028300     MOVE ZERO                            TO RETURN-CODE                  
028400     GOBACK                                                               
028500     .                                                                    
028600     EJECT                                                                
028700 A-INIT SECTION.                                                          
028800     SKIP2                                                                
028900     IF MSG-DUBBLA-TRANSKODER                                             
029000        MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W2I15201                
029100        MOVE MSG-IDTRANS-2                 TO MFS-IDTRANS                 
029200        MOVE MSG-KDMFSFOR-2                TO MFS-KDMFSFOR                
029300        MOVE MSG-KDTRTYP                   TO MFS-KDTRTYP                 
029400        MOVE MSG-IDPFK                     TO MFS-IDPFK                   
029500     ELSE                                                                 
029600        MOVE MSG-INDATA-MINUS-1-TRANSKOD   TO MID-W2I15201                
029700        MOVE MSG-IDTRANS-1                 TO MFS-IDTRANS                 
029800        MOVE MSG-KDMFSFOR-1                TO MFS-KDMFSFOR                
029900        MOVE SPACE                         TO MFS-KDTRTYP                 
030000                                              MFS-IDPFK                   
030100     END-IF                                                               
030200                                                                          
030300     MOVE LOW-VALUE                        TO MSG-AREA                    
030400     MOVE 'W2O15201'                       TO MFS-IDMOD                   
030500     MOVE '2152'                           TO MOD-IDTRANS                 
030600     MOVE MFS-IDTRANS                      TO WS-IDTRANS                  
030700     MOVE MFS-RENSA-FAELT                  TO MOD-TEMFSFEL                
030800                                              MOD-TEMFSINF                
030900                                              MOD-KDPRODSL-IN             
031000*                                                                         
031100     IF EGEN-BILD                                                         
031200        CONTINUE                                                          
031300     ELSE                                                                 
031400        MOVE SPACE                         TO MFS-KDTRTYP                 
031500        MOVE '7'                           TO MFS-IDPFK                   
031600     END-IF                                                               
031700     IF ENGLISH-TEXT                                                      
031800        MOVE +2                            TO SPRAK-IX                    
031900     ELSE                                                                 
032000        MOVE +1                            TO SPRAK-IX                    
032100     END-IF                                                               
032200     .                                                                    
032300     EJECT                                                                
032400 B-GOR-IORDNING-NYCKLAR SECTION.                                          
032500     SKIP3                                                                
032600     MOVE JA                         TO SW-NYCKLAR-OK                     
032700     SKIP2                                                                
032800     IF MID-KDPRODSL-IN = ALL '+'                                         
032900        MOVE MID-KDPRODSL-UT         TO WS-KDPRODSL                       
033000     ELSE                                                                 
033100        MOVE MID-KDPRODSL-IN         TO WS-KDPRODSL                       
033200        MOVE '7'                     TO MFS-IDPFK                         
033300        MOVE SPACE                   TO MFS-KDTRTYP                       
033400     END-IF                                                               
033500     INSPECT WS-KDPRODSL    REPLACING LEADING SPACE                       
033600                                             BY ZERO                      
033700     IF WS-KDPRODSL   NUMERIC                                             
033800        MOVE WS-KDPRODSL              TO  W-KDPRODSL                      
033900     ELSE                                                                 
034000        MOVE NEJ                     TO SW-NYCKLAR-OK                     
034100        MOVE FEL-2   (SPRAK-IX)      TO MOD-TEMFSFEL                      
034200     END-IF                                                               
034300     MOVE WS-KDPRODSL                TO MOD-KDPRODSL-UT                   
034400     INSPECT MOD-KDPRODSL-UT REPLACING LEADING ZERO                       
034500                                           BY SPACE                       
034600*                                                                         
034700     IF NYCKLAR-OK                                                        
034800        PERFORM IMS-GU-XXAT01                                             
034900        IF SEGMENT-FINNS                                                  
035000           PERFORM BA-KOLLA-TANGENT                                       
035100        ELSE                                                              
035200           MOVE NEJ                     TO SW-NYCKLAR-OK                  
035300           MOVE FEL-3   (SPRAK-IX)      TO MOD-TEMFSFEL                   
035400        END-IF                                                            
035500     END-IF                                                               
035600     .                                                                    
035700     EJECT                                                                
035800 BA-KOLLA-TANGENT SECTION.                                                
035900     SKIP2                                                                
036000     IF MFS-UPDATE                                                        
036100        CONTINUE                                                          
036200     ELSE                                                                 
036300        IF EGEN-BILD                                                      
036400           IF MID-IDFKNGRP-FOM-IN    = ALL '+' AND                        
036500              MID-IDFKNGRP-TOM-IN    = ALL '+' AND                        
036600              MID-IDANSK-IN          = ALL '+' AND                        
036700              MID-IDUSER-IN          = ALL '+' AND                        
036800              MID-KDSVAR-IN          = ALL '+'                            
036900              MOVE MFS-RENSA-FAELT          TO                            
037000                                           MOD-IDFKNGRP-FOM-IN            
037100                                           MOD-IDFKNGRP-TOM-IN            
037200                                           MOD-IDANSK-IN                  
037300                                           MOD-IDUSER-IN                  
037400                                           MOD-KDSVAR-IN                  
037500              MOVE MFS-NUM-FAELT-RAETT     TO                             
037600                                      MOD-IDFKNGRP-FOM-IN-ATTR            
037700                                      MOD-IDFKNGRP-TOM-IN-ATTR            
037800                                      MOD-IDANSK-IN-ATTR                  
037900              MOVE MFS-ALFA-FAELT-RAETT    TO                             
038000                                           MOD-IDUSER-IN-ATTR             
038100                                           MOD-KDSVAR-IN-ATTR             
038200              IF MFS-IDPFK = '8'                                          
038300                 MOVE MID-IDFKNGRP-FOM-HI      TO                         
038400                                            WS-IDFKNGRP-FOM-IN            
038500                 INSPECT WS-IDFKNGRP-FOM-IN REPLACING LEADING             
038600                                                SPACE BY ZERO             
038700                 IF WS-IDFKNGRP-FOM-IN NUMERIC                            
038800                    IF WS-IDFKNGRP-FOM-IN = ZERO                          
038900                       MOVE '7'                TO MFS-IDPFK               
039000                       MOVE SPACE              TO                         
039100                                              MFS-KDTRTYP                 
039200                       MOVE MED-3 (SPRAK-IX)   TO MOD-TEMFSFEL            
039300                       MOVE ZERO               TO W-IDFKNGRP-FOM          
039400                    ELSE                                                  
039500                       MOVE WS-IDFKNGRP-FOM-IN  TO                        
039600                                                 W-IDFKNGRP-FOM           
039700                    END-IF                                                
039800                 ELSE                                                     
039900                    MOVE ZERO                  TO                         
040000                                             MOD-IDFKNGRP-FOM-HI          
040100                    MOVE MED-3 (SPRAK-IX)      TO MOD-TEMFSFEL            
040200                    MOVE '7'                   TO MFS-IDPFK               
040300                    MOVE SPACE                 TO                         
040400                                              MFS-KDTRTYP                 
040500                    MOVE ZERO                  TO W-IDFKNGRP-FOM          
040600                 END-IF                                                   
040700              ELSE                                                        
040800                 IF MFS-IDPFK = '7'                                       
040900                    MOVE MED-3 (SPRAK-IX)        TO MOD-TEMFSFEL          
041000                    MOVE ZERO                    TO                       
041100                                                 W-IDFKNGRP-FOM           
041200                 ELSE                                                     
041300                    MOVE MID-IDFKNGRP-FOM-LO   TO                         
041400                                              WS-IDFKNGRP-FOM-IN          
041500                    INSPECT WS-IDFKNGRP-FOM-IN REPLACING LEADING          
041600                                                   SPACE BY ZERO          
041700                    IF WS-IDFKNGRP-FOM-IN NUMERIC                         
041800                       MOVE WS-IDFKNGRP-FOM-IN     TO                     
041900                                                   W-IDFKNGRP-FOM         
042000                    ELSE                                                  
042100                       MOVE ZERO               TO                         
042200                                             MOD-IDFKNGRP-FOM-HI          
042300                                             W-IDFKNGRP-FOM               
042400                    END-IF                                                
042500                 END-IF                                                   
042600              END-IF                                                      
042700           ELSE                                                           
042800              MOVE MFS-NUM-FAELT-RAETT       TO                           
042900                                 MOD-IDFKNGRP-FOM-IN-ATTR                 
043000                                 MOD-IDFKNGRP-TOM-IN-ATTR                 
043100                                 MOD-IDANSK-IN-ATTR                       
043200              MOVE MFS-ALFA-FAELT-RAETT     TO                            
043300                                      MOD-IDUSER-IN-ATTR                  
043400                                      MOD-KDSVAR-IN-ATTR                  
043500              MOVE MFS-ROER-EJ-FAELT        TO                            
043600                                      MOD-IDFKNGRP-FOM-IN                 
043700                                      MOD-IDFKNGRP-TOM-IN                 
043800                                      MOD-IDANSK-IN                       
043900                                      MOD-IDUSER-IN                       
044000                                      MOD-KDSVAR-IN                       
044100              MOVE MID-IDFKNGRP-FOM-LO   TO                               
044200                                        WS-IDFKNGRP-FOM-IN                
044300              INSPECT WS-IDFKNGRP-FOM-IN REPLACING LEADING                
044400                                             SPACE BY ZERO                
044500              IF WS-IDFKNGRP-FOM-IN NUMERIC                               
044600                 MOVE WS-IDFKNGRP-FOM-IN   TO                             
044700                                           W-IDFKNGRP-FOM                 
044800              ELSE                                                        
044900                 MOVE ZERO                 TO                             
045000                                       MOD-IDFKNGRP-FOM-HI                
045100                                            W-IDFKNGRP-FOM                
045200              END-IF                                                      
045300              MOVE MED-9 (SPRAK-IX)      TO MOD-TEMFSFEL                  
045400           END-IF                                                         
045500        ELSE                                                              
045600           MOVE MED-3 (SPRAK-IX)            TO MOD-TEMFSFEL               
045700           MOVE ZERO                        TO W-IDFKNGRP-FOM             
045800        END-IF                                                            
045900     END-IF                                                               
046000     .                                                                    
046100     EJECT                                                                
046200 C-KOLLA-INDATA SECTION.                                                  
046300     MOVE JA                         TO SW-INDATA-OK                      
046400     IF MID-IDFKNGRP-FOM-IN = ALL '+'                                     
046500        MOVE MFS-RENSA-FAELT         TO MOD-IDFKNGRP-FOM-IN               
046600        MOVE MFS-NUM-FAELT-FEL       TO MOD-IDFKNGRP-FOM-IN-ATTR          
046700        MOVE NEJ                     TO SW-INDATA-OK                      
046800        MOVE FEL-1 (SPRAK-IX)        TO MOD-TEMFSFEL                      
046900        MOVE MED-7 (SPRAK-IX)        TO MOD-TEMFSINF                      
047000     ELSE                                                                 
047100        MOVE MID-IDFKNGRP-FOM-IN     TO WS-IDFKNGRP-FOM-IN                
047200        INSPECT  WS-IDFKNGRP-FOM-IN REPLACING LEADING SPACE               
047300                                                    BY ZERO               
047400        MOVE MFS-ROER-EJ-FAELT       TO MOD-IDFKNGRP-FOM-IN               
047500        IF WS-IDFKNGRP-FOM-IN NUMERIC                                     
047600           MOVE MFS-NUM-FAELT-RAETT TO MOD-IDFKNGRP-FOM-IN-ATTR           
047700        ELSE                                                              
047800           MOVE MFS-NUM-FAELT-FEL    TO MOD-IDFKNGRP-FOM-IN-ATTR          
047900           MOVE NEJ                  TO SW-INDATA-OK                      
048000           MOVE FEL-1 (SPRAK-IX)     TO MOD-TEMFSFEL                      
048100           MOVE MED-6 (SPRAK-IX)     TO MOD-TEMFSINF                      
048200        END-IF                                                            
048300     END-IF                                                               
048400*                                                                         
048500     IF MID-IDFKNGRP-TOM-IN = ALL '+'                                     
048600        MOVE MFS-RENSA-FAELT         TO MOD-IDFKNGRP-TOM-IN               
048700        MOVE MFS-NUM-FAELT-FEL       TO MOD-IDFKNGRP-TOM-IN-ATTR          
048800        MOVE NEJ                     TO SW-INDATA-OK                      
048900        MOVE FEL-1 (SPRAK-IX)        TO MOD-TEMFSFEL                      
049000        MOVE MED-7 (SPRAK-IX)        TO MOD-TEMFSINF                      
049100     ELSE                                                                 
049200        MOVE MID-IDFKNGRP-TOM-IN     TO WS-IDFKNGRP-TOM-IN                
049300        INSPECT  WS-IDFKNGRP-TOM-IN REPLACING LEADING SPACE               
049400                                                BY ZERO                   
049500        MOVE MFS-ROER-EJ-FAELT    TO MOD-IDFKNGRP-TOM-IN                  
049600        IF WS-IDFKNGRP-TOM-IN NUMERIC                                     
049700           MOVE MFS-NUM-FAELT-RAETT TO MOD-IDFKNGRP-TOM-IN-ATTR           
049800        ELSE                                                              
049900           MOVE MFS-NUM-FAELT-FEL    TO MOD-IDFKNGRP-TOM-IN-ATTR          
050000           MOVE NEJ                  TO SW-INDATA-OK                      
050100           MOVE FEL-1 (SPRAK-IX)     TO MOD-TEMFSFEL                      
050200           MOVE MED-6 (SPRAK-IX)     TO MOD-TEMFSINF                      
050300        END-IF                                                            
050400     END-IF                                                               
050500*                                                                         
050600     IF MID-KDSVAR-IN = ALL '+'                                           
050700        MOVE MFS-RENSA-FAELT            TO MOD-KDSVAR-IN                  
050800        MOVE MFS-ALFA-FAELT-FEL         TO MOD-KDSVAR-IN-ATTR             
050900        MOVE NEJ                        TO SW-INDATA-OK                   
051000        MOVE FEL-1 (SPRAK-IX)           TO MOD-TEMFSFEL                   
051100        MOVE MED-7 (SPRAK-IX)           TO MOD-TEMFSINF                   
051200     ELSE                                                                 
051300        MOVE MID-KDSVAR-IN              TO W-KDSVAR                       
051400        MOVE MFS-ROER-EJ-FAELT          TO MOD-KDSVAR-IN                  
051500        IF GODKAENT-SVAR                                                  
051600           MOVE MFS-ALFA-FAELT-RAETT    TO MOD-KDSVAR-IN-ATTR             
051700        ELSE                                                              
051800           MOVE MFS-ALFA-FAELT-FEL      TO MOD-KDSVAR-IN-ATTR             
051900           MOVE NEJ                     TO SW-INDATA-OK                   
052000           MOVE FEL-1 (SPRAK-IX)        TO MOD-TEMFSFEL                   
052100        END-IF                                                            
052200     END-IF                                                               
052300*                                                                         
052400     IF BORTTAG                                                           
052500        MOVE MFS-RENSA-FAELT         TO MOD-IDANSK-IN                     
052600                                        MOD-IDUSER-IN                     
052700        MOVE MFS-NUM-FAELT-RAETT     TO MOD-IDANSK-IN-ATTR                
052800        MOVE MFS-ALFA-FAELT-RAETT    TO MOD-IDUSER-IN-ATTR                
052900     ELSE                                                                 
053000        IF MID-IDANSK-IN = ALL '+' AND MID-IDUSER-IN = ALL '+'            
053100           MOVE MFS-RENSA-FAELT         TO MOD-IDANSK-IN                  
053200           MOVE MFS-RENSA-FAELT         TO MOD-IDUSER-IN                  
053300           MOVE MFS-NUM-FAELT-FEL       TO MOD-IDANSK-IN-ATTR             
053400           MOVE MFS-ALFA-FAELT-FEL      TO MOD-IDUSER-IN-ATTR             
053500           MOVE NEJ                     TO SW-INDATA-OK                   
053600           MOVE FEL-1 (SPRAK-IX)        TO MOD-TEMFSFEL                   
053700           MOVE MED-7 (SPRAK-IX)        TO MOD-TEMFSINF                   
053800        ELSE                                                              
053900           IF MID-IDANSK-IN = ALL '+'                                     
054000              MOVE SPACE                TO WS-IDANSK-IN                   
054100           ELSE                                                           
054200              MOVE MID-IDANSK-IN        TO WS-IDANSK-IN                   
054300           END-IF                                                         
054400           INSPECT  WS-IDANSK-IN   REPLACING LEADING SPACE                
054500                                                   BY ZERO                
054600           MOVE MFS-ROER-EJ-FAELT    TO MOD-IDANSK-IN                     
054700           IF WS-IDANSK-IN       NUMERIC                                  
054800              MOVE MFS-NUM-FAELT-RAETT TO MOD-IDANSK-IN-ATTR              
054900           ELSE                                                           
055000              MOVE MFS-NUM-FAELT-FEL    TO MOD-IDANSK-IN-ATTR             
055100              MOVE NEJ                  TO SW-INDATA-OK                   
055200              MOVE FEL-1 (SPRAK-IX)     TO MOD-TEMFSFEL                   
055300              MOVE MED-6 (SPRAK-IX)     TO MOD-TEMFSINF                   
055400           END-IF                                                         
055500           IF MID-IDUSER-IN = ALL '+'                                     
055600              MOVE SPACE                TO WS-IDUSER-IN                   
055700           ELSE                                                           
055800              MOVE MID-IDUSER-IN        TO WS-IDUSER-IN                   
055900           END-IF                                                         
056000           MOVE MFS-ROER-EJ-FAELT       TO MOD-IDUSER-IN                  
056100           MOVE MFS-ALFA-FAELT-RAETT    TO MOD-IDUSER-IN-ATTR             
056200        END-IF                                                            
056300     END-IF                                                               
056400*                                                                         
056500*                                                                         
056600     IF INDATA-OK                                                         
056700        IF WS-IDFKNGRP-FOM-IN > WS-IDFKNGRP-TOM-IN                        
056800           MOVE MFS-NUM-FAELT-FEL      TO                                 
056900                                       MOD-IDFKNGRP-FOM-IN-ATTR           
057000                                       MOD-IDFKNGRP-TOM-IN-ATTR           
057100           MOVE NEJ                    TO SW-INDATA-OK                    
057200           MOVE FEL-1 (SPRAK-IX)       TO MOD-TEMFSFEL                    
057300           MOVE MED-8 (SPRAK-IX)       TO MOD-TEMFSINF                    
057400        END-IF                                                            
057500     END-IF                                                               
057600     IF INDATA-OK                                                         
057700        MOVE WS-IDFKNGRP-FOM-IN         TO WS-IDFKNGRP-FOM-JFR            
057800        MOVE WS-IDFKNGRP-TOM-IN         TO WS-IDFKNGRP-TOM-JFR            
057900        PERFORM CA-KOLLA-MOT-BAS                                          
058000     END-IF                                                               
058100     .                                                                    
058200     EJECT                                                                
058300 CA-KOLLA-MOT-BAS SECTION.                                                
058400     SKIP2                                                                
058500     IF AENDRING                                                          
058600        MOVE WS-IDFKNGRP-FOM-IN        TO W-IDFKNGRP-FOM                  
058700        PERFORM IMS-GHU-XXAT11                                            
058800        IF SEGMENT-FINNS                                                  
058900           IF XXAT11-1138-IDFKNGRP-TOM = WSS-IDFKNGRP-TOM-IN              
059000              IF WS-IDANSK-IN = ZERO                                      
059100                 MOVE WS-IDUSER-IN        TO                              
059200                                         XXAT11-1138-IDUSER               
059300              ELSE                                                        
059400                 MOVE WS-IDANSK-IN        TO                              
059500                                         XXAT11-1138-IDANSK               
059600                 IF WS-IDUSER-IN = SPACE                                  
059700                    CONTINUE                                              
059800                 ELSE                                                     
059900                    MOVE WS-IDUSER-IN        TO                           
060000                                         XXAT11-1138-IDUSER               
060100                 END-IF                                                   
060200              END-IF                                                      
060300           ELSE                                                           
060400              MOVE MFS-NUM-FAELT-FEL   TO                                 
060500                                   MOD-IDFKNGRP-TOM-IN-ATTR               
060600              MOVE NEJ                 TO SW-INDATA-OK                    
060700              MOVE FEL-1 (SPRAK-IX)    TO MOD-TEMFSFEL                    
060800              MOVE MED-4 (SPRAK-IX)    TO MOD-TEMFSINF                    
060900           END-IF                                                         
061000        ELSE                                                              
061100           MOVE MFS-NUM-FAELT-FEL       TO                                
061200                                   MOD-IDFKNGRP-FOM-IN-ATTR               
061300           MOVE NEJ                    TO SW-INDATA-OK                    
061400           MOVE FEL-1 (SPRAK-IX)       TO MOD-TEMFSFEL                    
061500           MOVE MED-4 (SPRAK-IX)       TO MOD-TEMFSINF                    
061600        END-IF                                                            
061700     ELSE                                                                 
061800        IF BORTTAG                                                        
061900           MOVE WS-IDFKNGRP-FOM-IN  TO W-IDFKNGRP-FOM                     
062000           PERFORM IMS-GHU-XXAT11                                         
062100           IF SEGMENT-FINNS                                               
062200              IF XXAT11-1138-IDFKNGRP-TOM =                               
062300                                          WSS-IDFKNGRP-TOM-IN             
062400                 CONTINUE                                                 
062500              ELSE                                                        
062600                 MOVE MFS-NUM-FAELT-FEL   TO                              
062700                                    MOD-IDFKNGRP-TOM-IN-ATTR              
062800                 MOVE FEL-1 (SPRAK-IX)     TO MOD-TEMFSFEL                
062900                 MOVE MED-11 (SPRAK-IX)    TO MOD-TEMFSINF                
063000                 MOVE NEJ                  TO SW-INDATA-OK                
063100              END-IF                                                      
063200           ELSE                                                           
063300              MOVE MFS-NUM-FAELT-FEL TO                                   
063400                                    MOD-IDFKNGRP-FOM-IN-ATTR              
063500              MOVE NEJ                TO SW-INDATA-OK                     
063600              MOVE FEL-1 (SPRAK-IX)   TO MOD-TEMFSFEL                     
063700              MOVE MED-11 (SPRAK-IX)  TO MOD-TEMFSINF                     
063800           END-IF                                                         
063900        ELSE                                                              
064000           IF WS-IDANSK-IN = ZERO                                         
064100              MOVE MFS-NUM-FAELT-FEL TO                                   
064200                                      MOD-IDANSK-IN-ATTR                  
064300              MOVE NEJ                TO SW-INDATA-OK                     
064400              MOVE FEL-1 (SPRAK-IX)   TO MOD-TEMFSFEL                     
064500              MOVE MED-7 (SPRAK-IX)   TO MOD-TEMFSINF                     
064600           ELSE                                                           
064700              MOVE MFS-NUM-FAELT-RAETT TO                                 
064800                                      MOD-IDANSK-IN-ATTR                  
064900           END-IF                                                         
065000           IF WS-IDUSER-IN = SPACE                                        
065100              MOVE NEJ                TO SW-INDATA-OK                     
065200              MOVE FEL-1 (SPRAK-IX)   TO MOD-TEMFSFEL                     
065300              MOVE MED-7 (SPRAK-IX)   TO MOD-TEMFSINF                     
065400              MOVE MFS-ALFA-FAELT-FEL TO                                  
065500                                      MOD-IDUSER-IN-ATTR                  
065600           ELSE                                                           
065700              MOVE MFS-ALFA-FAELT-RAETT         TO                        
065800                                      MOD-IDUSER-IN-ATTR                  
065900           END-IF                                                         
066000           IF INDATA-OK                                                   
066100              PERFORM CAA-KOLLA-BAS-VID-NYUPP                             
066200           END-IF                                                         
066300        END-IF                                                            
066400     END-IF                                                               
066500     .                                                                    
066600     EJECT                                                                
066700 CAA-KOLLA-BAS-VID-NYUPP SECTION.                                         
066800     PERFORM IMS-GNP-XXAT11                                               
066900     IF SEGMENT-FINNS                                                     
067000        MOVE XXAT11-1138-IDFKNGRP-FOM   TO                                
067100                         W-XXAT11-1138-IDFKNGRP-FOM                       
067200                         WS-IDFKNGRP-FOM-SPAR                             
067300        MOVE XXAT11-1138-IDFKNGRP-TOM   TO                                
067400                         W-XXAT11-1138-IDFKNGRP-TOM                       
067500                         WS-IDFKNGRP-TOM-SPAR                             
067600     ELSE                                                                 
067700        MOVE HIGH-VALUE                 TO                                
067800                         W-XXAT11-1138-IDFKNGRP-FOM                       
067900                         W-XXAT11-1138-IDFKNGRP-TOM                       
068000     END-IF                                                               
068100     PERFORM UNTIL  WS-IDFKNGRP-FOM-JFR <                                 
068200      W-XXAT11-1138-IDFKNGRP-TOM  OR SEGMENT-SAKNAS                       
068300          MOVE   XXAT11-1138-IDFKNGRP-FOM   TO                            
068400                                      WS-IDFKNGRP-FOM-SPAR                
068500          MOVE   XXAT11-1138-IDFKNGRP-TOM   TO                            
068600                                      WS-IDFKNGRP-TOM-SPAR                
068700          PERFORM IMS-GNP-XXAT11                                          
068800          IF SEGMENT-FINNS                                                
068900             MOVE XXAT11-1138-IDFKNGRP-FOM   TO                           
069000                              W-XXAT11-1138-IDFKNGRP-FOM                  
069100             MOVE XXAT11-1138-IDFKNGRP-TOM   TO                           
069200                              W-XXAT11-1138-IDFKNGRP-TOM                  
069300          ELSE                                                            
069400             MOVE HIGH-VALUE                 TO                           
069500                              W-XXAT11-1138-IDFKNGRP-FOM                  
069600                              W-XXAT11-1138-IDFKNGRP-TOM                  
069700          END-IF                                                          
069800     END-PERFORM                                                          
069900     IF SEGMENT-FINNS                                                     
070000        IF WS-IDFKNGRP-FOM-JFR = WS-IDFKNGRP-TOM-SPAR                     
070100           MOVE MFS-NUM-FAELT-FEL       TO                                
070200                                      MOD-IDFKNGRP-FOM-IN-ATTR            
070300                                      MOD-IDFKNGRP-TOM-IN-ATTR            
070400           MOVE NEJ                     TO SW-INDATA-OK                   
070500           MOVE FEL-1 (SPRAK-IX)        TO MOD-TEMFSFEL                   
070600           MOVE MED-5 (SPRAK-IX)        TO MOD-TEMFSINF                   
070700        ELSE                                                              
070800           IF WS-IDFKNGRP-TOM-JFR < W-XXAT11-1138-IDFKNGRP-FOM            
070900              CONTINUE                                                    
071000           ELSE                                                           
071100              MOVE MFS-NUM-FAELT-FEL      TO                              
071200                                      MOD-IDFKNGRP-FOM-IN-ATTR            
071300                                      MOD-IDFKNGRP-TOM-IN-ATTR            
071400              MOVE NEJ                    TO SW-INDATA-OK                 
071500              MOVE FEL-1 (SPRAK-IX)       TO MOD-TEMFSFEL                 
071600              MOVE MED-5 (SPRAK-IX)       TO MOD-TEMFSINF                 
071700           END-IF                                                         
071800        END-IF                                                            
071900     ELSE                                                                 
072000        IF WS-IDFKNGRP-TOM-SPAR > ZERO                                    
072100           IF WS-IDFKNGRP-TOM-SPAR = WS-IDFKNGRP-FOM-JFR                  
072200              MOVE MFS-NUM-FAELT-FEL      TO                              
072300                                      MOD-IDFKNGRP-FOM-IN-ATTR            
072400                                      MOD-IDFKNGRP-TOM-IN-ATTR            
072500              MOVE NEJ                    TO SW-INDATA-OK                 
072600              MOVE FEL-1 (SPRAK-IX)       TO MOD-TEMFSFEL                 
072700              MOVE MED-5 (SPRAK-IX)       TO MOD-TEMFSINF                 
072800           END-IF                                                         
072900        END-IF                                                            
073000     END-IF                                                               
073100     .                                                                    
073200     EJECT                                                                
073300 D-UPPDATERA SECTION.                                                     
073400     IF NYUPPLAEGG                                                        
073500        PERFORM IMS-GHU-XXAT01                                            
073600        MOVE WS-IDFKNGRP-FOM-IN   TO XXAT11-1138-IDFKNGRP-FOM             
073700        MOVE LOW-VALUE            TO XXAT11-1138-LOWVALUE                 
073800        MOVE WS-IDFKNGRP-TOM-IN   TO XXAT11-1138-IDFKNGRP-TOM             
073900        MOVE WS-IDANSK-IN         TO XXAT11-1138-IDANSK                   
074000        MOVE WS-IDUSER-IN         TO XXAT11-1138-IDUSER                   
074100        PERFORM IMS-ISRT-XXAT11                                           
074200        MOVE WS-IDFKNGRP-FOM-IN   TO W-IDFKNGRP-FOM                       
074300     ELSE                                                                 
074400        IF BORTTAG                                                        
074500           PERFORM IMS-DLET-XXAT11                                        
074600        ELSE                                                              
074700           PERFORM IMS-REPL-XXAT11                                        
074800        END-IF                                                            
074900     END-IF                                                               
075000     .                                                                    
075100     EJECT                                                                
075200 E-MFS-ROER-EJ-FAELT SECTION.                                             
075300     MOVE MFS-ROER-EJ-FAELT          TO                                   
075400                         MOD-IDFKNGRP-FOM-LO                              
075500                         MOD-IDFKNGRP-FOM-HI                              
075600     MOVE +1                         TO COL-INDX                          
075700     MOVE +1                         TO RAD-INDX                          
075800     PERFORM UNTIL COL-INDX > MAX-COL                                     
075900        MOVE MFS-ROER-EJ-FAELT       TO                                   
076000                         MOD-IDFKNGRP-FOM (COL-INDX , RAD-INDX)           
076100                         MOD-IDFKNGRP-TOM (COL-INDX , RAD-INDX)           
076200                         MOD-IDANSK       (COL-INDX , RAD-INDX)           
076300                         MOD-IDUSER       (COL-INDX , RAD-INDX)           
076400        IF RAD-INDX > MAX-RAD-MINUS-1                                     
076500           ADD +1                       TO COL-INDX                       
076600           MOVE +1                      TO RAD-INDX                       
076700        ELSE                                                              
076800           ADD +1                       TO RAD-INDX                       
076900        END-IF                                                            
077000     END-PERFORM                                                          
077100     .                                                                    
077200     EJECT                                                                
077300 F-FELHANTERING SECTION.                                                  
077400     MOVE +1                         TO COL-INDX                          
077500     MOVE +1                         TO RAD-INDX                          
077600     PERFORM UNTIL COL-INDX > MAX-COL                                     
077700        MOVE MFS-RENSA-FAELT         TO                                   
077800                     MOD-IDFKNGRP-FOM (COL-INDX , RAD-INDX)               
077900                     MOD-IDFKNGRP-TOM (COL-INDX , RAD-INDX)               
078000                     MOD-IDANSK       (COL-INDX , RAD-INDX)               
078100                     MOD-IDUSER       (COL-INDX , RAD-INDX)               
078200        IF RAD-INDX > MAX-RAD-MINUS-1                                     
078300           ADD +1                       TO COL-INDX                       
078400           MOVE +1                      TO RAD-INDX                       
078500        ELSE                                                              
078600           ADD +1                       TO RAD-INDX                       
078700        END-IF                                                            
078800     END-PERFORM                                                          
078900     MOVE MFS-RENSA-FAELT               TO                                
079000                                        MOD-IDFKNGRP-FOM-IN               
079100                                        MOD-IDFKNGRP-TOM-IN               
079200                                        MOD-IDANSK-IN                     
079300                                        MOD-IDUSER-IN                     
079400                                        MOD-KDSVAR-IN                     
079500     .                                                                    
079600     EJECT                                                                
079700 G-VISA-SIDAN SECTION.                                                    
079800     MOVE +1                          TO RAD-INDX                         
079900     MOVE +1                          TO COL-INDX                         
080000     PERFORM IMS-GU-XXAT01                                                
080100     IF SEGMENT-FINNS                                                     
080200        PERFORM IMS-GNP-KEY-XXAT11                                        
080300        PERFORM UNTIL COL-INDX > MAX-COL   OR SEGMENT-SAKNAS              
080400           MOVE XXAT11-1138-IDFKNGRP-FOM TO                               
080500                   MOD-IDFKNGRP-FOM (COL-INDX , RAD-INDX)                 
080600           MOVE XXAT11-1138-IDFKNGRP-TOM TO                               
080700                   MOD-IDFKNGRP-TOM (COL-INDX , RAD-INDX)                 
080800           MOVE XXAT11-1138-IDANSK       TO                               
080900                   MOD-IDANSK       (COL-INDX , RAD-INDX)                 
081000           MOVE XXAT11-1138-IDUSER       TO                               
081100                   MOD-IDUSER       (COL-INDX , RAD-INDX)                 
081200           IF RAD-INDX > MAX-RAD-MINUS-1                                  
081300              MOVE +1                    TO RAD-INDX                      
081400              ADD  +1                    TO COL-INDX                      
081500           ELSE                                                           
081600              ADD +1                     TO RAD-INDX                      
081700           END-IF                                                         
081800           PERFORM IMS-GNP-XXAT11                                         
081900        END-PERFORM                                                       
082000        IF RAD-INDX > +1 OR COL-INDX > +1                                 
082100           MOVE MOD-IDFKNGRP-FOM (1 , 1) TO MOD-IDFKNGRP-FOM-LO           
082200        ELSE                                                              
082300           MOVE ZERO                     TO MOD-IDFKNGRP-FOM-LO           
082400        END-IF                                                            
082500        IF MFS-UPDATE                                                     
082600           IF BORTTAG                                                     
082700              CONTINUE                                                    
082800           ELSE                                                           
082900              MOVE MFS-ADD-LYS-UPP-FAELT TO                               
083000                                         MOD-RAD-ATTR (1 , 1)             
083100           END-IF                                                         
083200           MOVE MFS-RENSA-FAELT          TO                               
083300                                          MOD-IDFKNGRP-FOM-IN             
083400                                          MOD-IDFKNGRP-TOM-IN             
083500                                          MOD-IDANSK-IN                   
083600                                          MOD-IDUSER-IN                   
083700                                          MOD-KDSVAR-IN                   
083800        END-IF                                                            
083900        IF SEGMENT-FINNS                                                  
084000           MOVE XXAT11-1138-IDFKNGRP-FOM TO WS-IDFKNGRP-FOM-HI            
084100           MOVE WS-IDFKNGRP-FOM-HI       TO MOD-IDFKNGRP-FOM-HI           
084200           IF MFS-UPDATE                                                  
084300              CONTINUE                                                    
084400           ELSE                                                           
084500              MOVE MED-2 (SPRAK-IX)      TO MOD-TEMFSINF                  
084600           END-IF                                                         
084700        ELSE                                                              
084800           PERFORM UNTIL COL-INDX > MAX-COL                               
084900              MOVE MFS-RENSA-FAELT       TO                               
085000                         MOD-IDFKNGRP-FOM (COL-INDX , RAD-INDX)           
085100                         MOD-IDFKNGRP-TOM (COL-INDX , RAD-INDX)           
085200                         MOD-IDANSK       (COL-INDX , RAD-INDX)           
085300                         MOD-IDUSER       (COL-INDX , RAD-INDX)           
085400              IF RAD-INDX > MAX-RAD-MINUS-1                               
085500                 MOVE +1                 TO RAD-INDX                      
085600                 ADD  +1                 TO COL-INDX                      
085700              ELSE                                                        
085800                 ADD +1                  TO RAD-INDX                      
085900              END-IF                                                      
086000           END-PERFORM                                                    
086100           MOVE ZERO                     TO MOD-IDFKNGRP-FOM-HI           
086200        END-IF                                                            
086300     ELSE                                                                 
086400        MOVE FEL-3 (SPRAK-IX)            TO MOD-TEMFSFEL                  
086500     END-IF                                                               
086600     .                                                                    
086700     EJECT                                                                
086800* IMS SEKTIONER                                                           
086900     SKIP3                                                                
087000 IMS-GET-MSG SECTION.                                                     
087100                                                                          
087200     MOVE '  QC' TO GODK-STATUSKODER                                      
087300     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
087400     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
087500     PERFORM IMS-STATUSKONTROLL                                           
087600     SKIP3                                                                
087700     .                                                                    
087800 IMS-INSERT-MSG SECTION.                                                  
087900                                                                          
088000     IF ENGLISH-TEXT                                                      
088100       MOVE 'N' TO MFS-KDHUVOMR                                           
088200     END-IF                                                               
088300     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
088400     MOVE SPACE TO GODK-STATUSKODER                                       
088500     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
088600     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
088700     PERFORM IMS-STATUSKONTROLL                                           
088800     EJECT                                                                
088900     .                                                                    
089000 IMS-GU-XXAT01 SECTION.                                                   
089100                                                                          
089200     STRING 'WLXXAT01(WDGXKEY  =' W-1137-KEY-X ')'                        
089300            DELIMITED BY SIZE INTO SSA1                                   
089400     MOVE '  GE' TO GODK-STATUSKODER                                      
089500     CALL CBLTDLI USING GU XXAT-PCB DLI-IO-AREA SSA1                      
089600     MOVE XXAT-STATUS-CODE TO STATUS-WS                                   
089700     PERFORM IMS-STATUSKONTROLL                                           
089800     SKIP3                                                                
089900     .                                                                    
090000 IMS-GHU-XXAT01 SECTION.                                                  
090100                                                                          
090200     STRING 'WLXXAT01(WDGXKEY  =' W-1137-KEY-X ')'                        
090300            DELIMITED BY SIZE INTO SSA1                                   
090400     MOVE '  GE' TO GODK-STATUSKODER                                      
090500     CALL CBLTDLI USING GHU XXAT-PCB DLI-IO-AREA SSA1                     
090600     MOVE XXAT-STATUS-CODE TO STATUS-WS                                   
090700     PERFORM IMS-STATUSKONTROLL                                           
090800     SKIP3                                                                
090900     .                                                                    
091000 IMS-GNP-KEY-XXAT11 SECTION.                                              
091100                                                                          
091200     STRING 'WLXXAT11(WDGXKEY >=' W-1138-KEY-X ')'                        
091300            DELIMITED BY SIZE INTO SSA1                                   
091400     MOVE '  GE' TO GODK-STATUSKODER                                      
091500     CALL CBLTDLI USING GNP XXAT-PCB DLI-IO-AREA SSA1                     
091600     MOVE XXAT-STATUS-CODE TO STATUS-WS                                   
091700     PERFORM IMS-STATUSKONTROLL                                           
091800     SKIP3                                                                
091900     .                                                                    
092000 IMS-GNP-XXAT11 SECTION.                                                  
092100                                                                          
092200     MOVE 'WLXXAT11 '           TO SSA1                                   
092300     MOVE '  GE' TO GODK-STATUSKODER                                      
092400     CALL CBLTDLI USING GNP XXAT-PCB DLI-IO-AREA SSA1                     
092500     MOVE XXAT-STATUS-CODE TO STATUS-WS                                   
092600     PERFORM IMS-STATUSKONTROLL                                           
092700     SKIP3                                                                
092800     .                                                                    
092900 IMS-GHU-XXAT11 SECTION.                                                  
093000                                                                          
093100     STRING 'WLXXAT01(WDGXKEY  =' W-1137-KEY-X ')'                        
093200            DELIMITED BY SIZE INTO SSA1                                   
093300     STRING 'WLXXAT11(WDGXKEY  =' W-1138-KEY-X ')'                        
093400            DELIMITED BY SIZE INTO SSA2                                   
093500     MOVE '  GE' TO GODK-STATUSKODER                                      
093600     CALL CBLTDLI USING GHU XXAT-PCB DLI-IO-AREA SSA1 SSA2                
093700     MOVE XXAT-STATUS-CODE TO STATUS-WS                                   
093800     PERFORM IMS-STATUSKONTROLL                                           
093900     SKIP3                                                                
094000     .                                                                    
094100 IMS-ISRT-XXAT11 SECTION.                                                 
094200                                                                          
094300     MOVE   'WLXXAT11 '            TO SSA2                                
094400     MOVE '  ' TO GODK-STATUSKODER                                        
094500     CALL CBLTDLI USING ISRT XXAT-PCB DLI-IO-AREA SSA1 SSA2               
094600     MOVE XXAT-STATUS-CODE TO STATUS-WS                                   
094700     PERFORM IMS-STATUSKONTROLL                                           
094800     SKIP3                                                                
094900     .                                                                    
095000 IMS-DLET-XXAT11 SECTION.                                                 
095100                                                                          
095200     MOVE '  ' TO GODK-STATUSKODER                                        
095300     CALL CBLTDLI USING DLET XXAT-PCB DLI-IO-AREA                         
095400     MOVE XXAT-STATUS-CODE TO STATUS-WS                                   
095500     PERFORM IMS-STATUSKONTROLL                                           
095600     SKIP3                                                                
095700     .                                                                    
095800 IMS-REPL-XXAT11 SECTION.                                                 
095900                                                                          
096000     MOVE '  ' TO GODK-STATUSKODER                                        
096100     CALL CBLTDLI USING REPL XXAT-PCB DLI-IO-AREA                         
096200     MOVE XXAT-STATUS-CODE TO STATUS-WS                                   
096300     PERFORM IMS-STATUSKONTROLL                                           
096400     SKIP3                                                                
096500     .                                                                    
096600 IMS-STATUSKONTROLL SECTION.                                              
096700                                                                          
096800     SET STATUS-IX TO 1                                                   
096900     SEARCH GODK-STATUS                                                   
096910       AT END                                                             
096920         CALL FELLOG                                                      
097000       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS NEXT SENTENCE.            
097100 IMS-STATUSKONTROLL-EXIT. EXIT.                                           
