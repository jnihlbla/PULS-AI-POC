000100     SKIP3                                                                
000200 ID DIVISION.                                                             
000300     SKIP2                                                                
000400 PROGRAM-ID.     W2014100.                                                
000500 AUTHOR.         PETER D.                                                 
000600 DATE-WRITTEN.   MAJ   88.                                                
000700                                                                          
000800     REMARKS.                                                             
000900*                                                                         
001000*    FUNKTION.                                                            
001100*        PROGRAMMET LÄSER OCH UPPDATERAR WLARTG01-BASEN                   
001200*        PROGRAMMET VISAR ANTAL ARTIKLAR, 'PISKADE' ARTIKLAR              
001300*        SAMT PASSERADE INLEVERANSER                                      
001400*   OBS! SKALL VISAS ENDAST NÄR HELA BASEN HAR LÄSTS                      
001500*        DETTA STYRS MED HJÄLP AV SWITCHARNA HELA-BASEN-LAEST             
001600*        OCH SW-RAEKNA-UPP                                                
001700****************************************************************          
001800*        VISAR EJ ARTIKLAR MED FÖRSTA INLEVERANSDATUM 9999                
001900*                                           89-10-19/ANN J                
002000****************************************************************          
002100****************************************************************          
002200* 2006-10-20                                                              
002300* VISA FÖRST ALLA ARTIKLAR MEDFL PISK = J MED SEQ-B NYCKEL SEN            
002400* MED FL PISK = N                                                         
002500****************************************************************          
002600*    INDATA.                                                              
002700*        TRANSAKTION: W2T141                                              
002800*        MID:         W2I14101                                            
002900*                                                                         
003000*    UTDATA.                                                              
003100*        MOD:         W2O14101                                            
003200     SKIP3                                                                
003300 ENVIRONMENT DIVISION.                                                    
003400     SKIP2                                                                
003500 DATA DIVISION.                                                           
003600     EJECT                                                                
003700 WORKING-STORAGE SECTION.                                                 
003800*    -COPY WY2000W3                                                       
003900     SKIP3                                                                
004000***** ARBETSFÄLT FÖR DATUM FINNS VID WDATKONV.********************        
004100 77  PROGRAM-NAMN                PIC X(8) VALUE 'W2014100'.               
004200 77  WS-IDPROJ                   PIC X(4) VALUE SPACE.                    
004300 77  WS-LAES-FLPISK              PIC X(1) VALUE SPACE.                    
004400 77  WS-MAX-RAD-ANT-ARTIKLAR     PIC S9(5) VALUE ZERO  COMP SYNC.         
004500 77  WS-MAX-RAD-ANT-PASS-INLV    PIC S9(5) VALUE ZERO  COMP SYNC.         
004600 77  WS-MAX-RAD-ANT-PISK         PIC S9(5) VALUE ZERO  COMP SYNC.         
004700 77  WS-ANT-ARTIKLAR             PIC S9(5) VALUE ZERO  COMP SYNC.         
004800 77  WS-ANT-PASS-INLV            PIC S9(5) VALUE ZERO  COMP SYNC.         
004900 77  WS-ANT-PISK                 PIC S9(5) VALUE ZERO  COMP SYNC.         
005000 77  WS-HELA-BASEN-LAEST         PIC X(1) VALUE SPACE.                    
005100 77  WS-SW-RAEKNA-UPP            PIC X(1) VALUE SPACE.                    
005200 77  WS-IDANSK-FROM              PIC X(3) VALUE SPACE.                    
005300 77  WS-IDANSK-TOM               PIC X(3) VALUE SPACE.                    
005400 77  WS-FLPISK                   PIC X(1) VALUE SPACE.                    
005500 77  JA                          PIC X       VALUE 'J'.                   
005600 77  NEJ                         PIC X       VALUE 'N'.                   
005700 77  INDX                        PIC S9(9)   VALUE +0   COMP SYNC.        
005800 77  RAD-INDX                    PIC S9(9)   VALUE +0   COMP SYNC.        
005900 77  SPRAK-IX                    PIC S9(9)   VALUE +0   COMP SYNC.        
006000 77  MAX-RAD                     PIC S9(9)   VALUE +12  COMP SYNC.        
006100 77  MAX-RAD-PLUS-1              PIC S9(9)   VALUE +13  COMP SYNC.        
006200 77  MAX-RAD-PLUS-2              PIC S9(9)   VALUE +14  COMP SYNC.        
006300 77  MAX-ANT-LAESN               PIC S9(9)   VALUE +200 COMP SYNC.        
006400 77  MAX-ANT-LAESN-PLUS-1        PIC S9(9)   VALUE +201 COMP SYNC.        
006500 77  MAX-MOD-LAENGD              PIC S9(4)  VALUE +1026 COMP SYNC.        
006600 01  SW-NYCKLAR-OK               PIC X.                                   
006700    88  NYCKLAR-OK                           VALUE 'J'.                   
006800 01  SW-INDATA-OK                PIC X.                                   
006900    88  INDATA-OK                            VALUE 'J'.                   
007000 01  SW-INDATA-IFYLLT-EJ-UPPDATE PIC X.                                   
007100    88  INDATA-IFYLLT-EJ-UPPDATE             VALUE 'J'.                   
007200 01  WS-IDTRANS                  PIC X(4).                                
007300    88  GODKAEND-BILD                        VALUE '2141'                 
007400                                                   '2133'.                
007500    88  EGEN-BILD                            VALUE '2141'.                
007600    88  2133-BILDEN                          VALUE '2133'.                
007700     EJECT                                                                
007800 01  DYNAMISKA-SUBPROGRAM.                                                
007900   03  WDATKONV                  PIC X(8)    VALUE 'WDATKONV'.            
008000   03  CBLTDLI                   PIC X(8)    VALUE 'CBLTDLI '.            
008100   03  FELLOG                    PIC X(8)    VALUE 'FELLOG  '.            
008200   03  W005INIT                  PIC X(8)    VALUE 'W005INIT'.            
008300*----------------------------------PARAMETRAR TILL DATUMKORT              
008400     SKIP2                                                                
008500*01  -COPY WDATAREA                                                       
008600                                                                          
008700*    --- PARAMETRAR TILL SUBPROGRAM W005INIT                              
008800*                                                                         
008900 01  FILLER                      PIC X(16)   VALUE 'WMSGINIT'.            
009000     SKIP3                                                                
009100*01 -COPY WMSGINIT                                                        
009200     EJECT                                                                
009300 77  WS-TIFINLEV                 PIC  9(6).                               
009400 01  W-AAVV-NUM                  PIC  9(4).                               
009500 01  W-AAVV REDEFINES W-AAVV-NUM.                                         
009600   03  W-AA                      PIC  9(2).                               
009700   03  W-VV                      PIC  9(2).                               
009800 01  WS-DAGENS-AAVV              PIC 9(4).                                
009900 01  WS-DAGENS-AAVV-GRP REDEFINES WS-DAGENS-AAVV.                         
010000   03  WS-DAGENS-AA              PIC  9(2).                               
010100   03  WS-DAGENS-VV              PIC  9(2).                               
010200     EJECT                                                                
010300 01  FILLER                      PIC X(16)   VALUE 'MEDDELANDE'.          
010400 01  MEDDELANDE.                                                          
010500   03  FEL1.                                                              
010600     05 FILLER                   PIC X(40)                                
010700          VALUE 'UPPLYSTA FÄLT FEL'.                                      
010800     05 FILLER                   PIC X(40)                                
010900          VALUE 'HIGHLIGHTED FIELDS ARE WRONG'.                           
011000   03  FILLER REDEFINES FEL1.                                             
011100     05  FEL-1                   PIC X(40)   OCCURS 2.                    
011200                                                                          
011300   03  FEL2.                                                              
011400     05 FILLER                   PIC X(40)                                
011500          VALUE 'NYCKLAR FEL'.                                            
011600     05 FILLER                   PIC X(40)                                
011700          VALUE 'KEYS ARE WRONG'.                                         
011800   03  FILLER REDEFINES FEL2.                                             
011900     05  FEL-2                   PIC X(40)   OCCURS 2.                    
012000                                                                          
012100   03  FEL3.                                                              
012200     05 FILLER                   PIC X(40)                                
012300          VALUE 'NYCKLAR SAKNAS'.                                         
012400     05 FILLER                   PIC X(40)                                
012500          VALUE 'KEYS NOT FOUND  '.                                       
012600   03  FILLER REDEFINES FEL3.                                             
012700     05  FEL-3                   PIC X(40)   OCCURS 2.                    
012800                                                                          
012900   03  FEL4.                                                              
013000     05 FILLER                   PIC X(40)                                
013100          VALUE 'TRYCK PF11 VID UPPDATERING'.                             
013200     05 FILLER                   PIC X(40)                                
013300          VALUE 'PRESS PF11 WHEN UPDATE'.                                 
013400   03  FILLER REDEFINES FEL4.                                             
013500     05  FEL-4                   PIC X(40)   OCCURS 2.                    
013600                                                                          
013700   03  MED1.                                                              
013800     05 FILLER                   PIC X(40)                                
013900          VALUE 'UPPDATERING GJORD        '.                              
014000     05 FILLER                   PIC X(40)                                
014100          VALUE 'UPDATE HAS BEEN DONE          '.                         
014200   03  FILLER REDEFINES MED1.                                             
014300     05  MED-1                   PIC X(40)   OCCURS 2.                    
014400                                                                          
014500   03  MED2.                                                              
014600     05 FILLER                   PIC X(40)                                
014700          VALUE 'TRYCK PF8 FÖR FLERA RADER'.                              
014800     05 FILLER                   PIC X(40)                                
014900          VALUE 'PRESS PF8 FOR MORE LINES'.                               
015000   03  FILLER REDEFINES MED2.                                             
015100     05  MED-2                   PIC X(40)   OCCURS 2.                    
015200                                                                          
015300   03  MED3.                                                              
015400     05 FILLER                   PIC X(40)                                
015500          VALUE 'DETTA ÄR FÖRSTA SIDAN'.                                  
015600     05 FILLER                   PIC X(40)                                
015700          VALUE 'THIS IS THE FIRST PAGE'.                                 
015800   03  FILLER REDEFINES MED3.                                             
015900     05  MED-3                   PIC X(40)   OCCURS 2.                    
016000                                                                          
016100   03  MED4.                                                              
016200     05 FILLER                   PIC X(40)                                
016300          VALUE 'UPPDATERING GJORD'.                                      
016400     05 FILLER                   PIC X(40)                                
016500          VALUE 'SUCCESSFUL UPDATE'.                                      
016600   03  FILLER REDEFINES MED4.                                             
016700     05  MED-4                   PIC X(40)   OCCURS 2.                    
016800                                                                          
016900   03  MED5.                                                              
017000     05 FILLER                   PIC X(40)                                
017100          VALUE 'SISTA SIDAN '.                                           
017200     05 FILLER                   PIC X(40)                                
017300          VALUE 'LAST PAGE'.                                              
017400   03  FILLER REDEFINES MED5.                                             
017500     05  MED-5                   PIC X(40)   OCCURS 2.                    
017600                                                                          
017700     EJECT                                                                
017800******************************************************************        
017900*                                                                         
018000*                AREOR FÖR MFS OCH SKÄRMHANTERING                         
018100*                                                                         
018200 01  FILLER                      PIC X(16)   VALUE 'MID-WS'.              
018300     SKIP3                                                                
018400*01  MID -COPY W2I14101                                                   
018500     SKIP3                                                                
018600*01  MID -COPY W2I13301 -PRE 2133-.                                       
018700     EJECT                                                                
018800 01  FILLER                      PIC X(16)   VALUE 'MSG-WS'.              
018900*01  -COPY WMSGAREA                                                       
019000     EJECT                                                                
019100*  03  MOD -COPY W2O14101           -RED MSG-AREA.                        
019200     EJECT                                                                
019300 01  FILLER                      PIC X(16)   VALUE 'MFS-WS'.              
019400*01  -COPY WMFSAREA                                                       
019500     EJECT                                                                
019600                                                                          
019700 01  FILLER                      PIC X(16)   VALUE 'SPAR-AREA'.           
019800 01  SPAR-AREA.                                                           
019900     03  SPAR-IDTRANS            PIC X(4)    VALUE SPACE.                 
020000     03  SPAR-LAES-NYCKEL        PIC X       VALUE SPACE.                 
020100                                                                          
020200     EJECT                                                                
020300 01  FILLER                      PIC X(16)   VALUE                        
020400                                            'NYCKLAR-TILL-DLI'.           
020500 01  NYCKLAR-TILL-DLI.                                                    
020600*                                                                         
020700   03  W-WDD2B1KY-GU.                                                     
020800     05  W-FLPISK-GU             PIC  X(1).                               
020900     05  W-IDANSK-GU             PIC S9(3)  COMP-3.                       
021000     05  W-DAFINLEV-GU           PIC  9(8).                               
021100     05  W-IDAO-GU               PIC  X(10).                              
021200     05  W-IDPROJ-GU             PIC  X(4).                               
021300     05  W-IDARTNR-GU            PIC S9(9)  COMP-3.                       
021400                                                                          
021500   03  W-WDD2B1KY-MIN.                                                    
021600     05  W-FLPISK-MIN-B          PIC  X(1).                               
021700     05  W-IDANSK-MIN-B          PIC S9(3)  COMP-3.                       
021800     05  W-DAFINLEV-MIN-B        PIC  9(8).                               
021900     05  W-IDAO-MIN-B            PIC  X(10).                              
022000     05  W-IDPROJ-MIN-B          PIC  X(4).                               
022100     05  W-IDARTNR-MIN-B         PIC S9(9)  COMP-3.                       
022200*                                                                         
022300   03  W-WDD2B1KY-MAX.                                                    
022400     05  W-FLPISK-MAX-B          PIC  X(1)   VALUE HIGH-VALUE.            
022500     05  W-IDANSK-MAX-B          PIC S9(3)   COMP-3.                      
022600     05  W-DAFINLEV-MAX-B        PIC  9(8)   VALUE 99999999.              
022700     05  W-IDAO-MAX-B            PIC  X(10)  VALUE HIGH-VALUE.            
022800     05  W-IDPROJ-MAX-B          PIC  X(4).                               
022900     05  W-IDARTNR-MAX-B         PIC S9(9)               COMP-3           
023000                               VALUE +999999999.                          
023100*                                                                         
023200   03  W-IDARTNR-X.                                                       
023300     05  W-IDARTNR               PIC S9(9)   VALUE ZERO  COMP-3.          
023400   03  W-KDSEGKEY-X.                                                      
023500       05  W-KDSEGKEY            PIC X       VALUE '1'.                   
023600   03  W-KDANSKQ-X             PIC  X(1)   VALUE '1'.                     
023700* SMIN & SMAX STÅR FÖR SÖKBEGREPP-MIN OCH MAX                             
023800   03  W-IDPROJ-SMIN           PIC  X(4)   VALUE SPACE.                   
023900   03  W-IDPROJ-SMAX           PIC  X(4)   VALUE SPACE.                   
024000     EJECT                                                                
024100******************************************************************        
024200*                                                                         
024300*        ARBETS-AREOR TILL IMS-SEKTIONERNA                                
024400*                                                                         
024500 01  FILLER                     PIC X(16)   VALUE 'IMS-WS     '.          
024600 01  IMS-WS.                                                              
024700     SKIP3                                                                
024800*                        **** STATUS-KOD FRÅN IMS                         
024900   03  STATUS-WS                 PIC XX.                                  
025000     88  SEGMENT-FINNS                       VALUE '  '.                  
025100     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
025200     88  BASEN-SLUT                          VALUE 'GB'.                  
025300     SKIP3                                                                
025400   03  GODK-STATUSKODER.                                                  
025500     05  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
025600     SKIP3                                                                
025700 01    SSA1                      PIC X(192).                              
025800 01    SSA2                      PIC X(192).                              
025900     EJECT                                                                
026000*                            IMS FUNKTIONSKODER                           
026100*01    -COPY W0003                                                        
026200     EJECT                                                                
026300*                            DLI INPUT-OUTPUT AREA                        
026400 01  FILLER                    PIC X(16)   VALUE 'DLI-IO-AREA'.           
026500 01  DLI-IO-AREA.                                                         
026600   03  IO-AREA                   PIC X(550)   VALUE SPACE.                
026700     SKIP3                                                                
026800*03  WLARTG01    -COPY WDD201  -PRE ARTG01-  -RED IO-AREA.                
026900     SKIP3                                                                
027000*03  WLARTI01    -COPY WDD2B1  -PRE ARTI01-  -RED IO-AREA.                
027100     EJECT                                                                
027200 01  FILLER                 PIC X(16)   VALUE 'DLI-IO-AREA-01'.           
027300 01  DLI-IO-AREA-01.                                                      
027400   03  IO-AREA-01                PIC X(200)   VALUE SPACE.                
027500     SKIP3                                                                
027600*03  WLARTC01    -COPY WDK601  -RED IO-AREA-01.                           
027700     EJECT                                                                
027800 01  FILLER                     PIC X(16) VALUE 'WDK611-AREA'.            
027900 01  DLI-IO-WDK611.                                                       
028000*  03  -COPY WDK611.                                                      
028100 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDD2SEQ'.                     
028200 01  DLI-IO-WDD2SEQ.                                                      
028300*    03  -COPY WDD2B1   -PRE SEQ-                                         
028400     EJECT                                                                
028500 LINKAGE SECTION.                                                         
028600*01  -COPY W0009     -PRE MSG-                                            
028700     SKIP2                                                                
028800*01  -COPY W0008     -PRE WDP7-                                           
028900     05  FILLER                  PIC X.                                   
029000*01  -COPY W0008     -PRE ARTI-                                           
029100     05  FILLER                  PIC X.                                   
029200     SKIP2                                                                
029300*01  -COPY W0008     -PRE ARTC-                                           
029400     05  FILLER                  PIC X.                                   
029500     SKIP2                                                                
029600*01  -COPY W0008     -PRE ARTG-                                           
029700     05  FILLER                  PIC X.                                   
029800*01  -COPY W0008     -PRE WDD2B-                                          
029900     05  FILLER                  PIC X.                                   
030000     EJECT                                                                
030100 PROCEDURE DIVISION   USING  MSG-PCB WDP7-PCB                             
030200                             ARTC-PCB ARTG-PCB                            
030300                             WDD2B-PCB.                                   
030400      ENTRY 'DLITCBL' USING  MSG-PCB WDP7-PCB                             
030500                             ARTC-PCB ARTG-PCB                            
030600                             WDD2B-PCB.                                   
030700                                                                          
030800     PERFORM IMS-GET-MSG                                                  
030900     IF SEGMENT-FINNS                                                     
031000        PERFORM A-INIT                                                    
031100        IF GODKAEND-BILD                                                  
031200           PERFORM B-KOLLA-NYCKLAR                                        
031300           IF NYCKLAR-OK                                                  
031400              IF MFS-UPDATE                                               
031500                 PERFORM C-KOLLA-INDATA                                   
031600                 IF INDATA-OK                                             
031700                    PERFORM D-UPPDATERA                                   
031800                    MOVE MED-1 (SPRAK-IX)    TO MOD-TEMFSINF              
031900                    PERFORM I-FORMATETS-ATTR                              
032000                    PERFORM S01-LAES-WDD2B                                
032100                    IF SEGMENT-FINNS                                      
032200                       PERFORM G-VISA-SIDAN                               
032300                    ELSE                                                  
032400                       MOVE FEL-3 (SPRAK-IX) TO MOD-TEMFSFEL              
032500                       PERFORM F-RENSA-FAELT                              
032600                    END-IF                                                
032700                 ELSE                                                     
032800                    MOVE FEL-1 (SPRAK-IX)    TO MOD-TEMFSFEL              
032900                    PERFORM E-MFS-ROER-EJ-FAELT                           
033000                 END-IF                                                   
033100              ELSE                                                        
033200                 IF MFS-IDPFK = ' '  OR MFS-IDPFK = '8'                   
033300                   PERFORM IMS-GU-WDD2B01                                 
033400                 ELSE                                                     
033500                   PERFORM S01-LAES-WDD2B                                 
033600                 END-IF                                                   
033700                 IF SEGMENT-FINNS                                         
033800                    MOVE NEJ                 TO                           
033900                                  SW-INDATA-IFYLLT-EJ-UPPDATE             
034000                    IF EGEN-BILD                                          
034100                       IF MFS-IDPFK = ' '                                 
034200                          PERFORM H-KOLLA-ATT-INDATA-EJ-IFYLLD            
034300                       END-IF                                             
034400                    END-IF                                                
034500                    IF INDATA-IFYLLT-EJ-UPPDATE                           
034600                       PERFORM E-MFS-ROER-EJ-FAELT                        
034700                       MOVE FEL-4 (SPRAK-IX) TO MOD-TEMFSFEL              
034800                    ELSE                                                  
034900                       PERFORM G-VISA-SIDAN                               
035000                    END-IF                                                
035100                 ELSE                                                     
035200                    MOVE FEL-3 (SPRAK-IX)    TO MOD-TEMFSFEL              
035300                    PERFORM F-RENSA-FAELT                                 
035400                 END-IF                                                   
035500              END-IF                                                      
035600           ELSE                                                           
035700              PERFORM F-RENSA-FAELT                                       
035800           END-IF                                                         
035900        ELSE                                                              
036000           PERFORM F-RENSA-FAELT                                          
036100           PERFORM K-RENSA-INMATNINGSFAELT                                
036200        END-IF                                                            
036300        MOVE MAX-MOD-LAENGD                  TO MSG-KVLL                  
036400        PERFORM IMS-INSERT-MSG                                            
036500     END-IF                                                               
036600     MOVE ZERO                               TO RETURN-CODE               
036700     GOBACK                                                               
036800     .                                                                    
036900     EJECT                                                                
037000 A-INIT SECTION.                                                          
037100     SKIP2                                                                
037200     IF MSG-DUBBLA-TRANSKODER                                             
037300        MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W2I14101                
037400        MOVE MSG-IDTRANS-2                 TO MFS-IDTRANS                 
037500        MOVE MSG-KDMFSFOR-2                TO MFS-KDMFSFOR                
037600        MOVE MSG-KDTRTYP                   TO MFS-KDTRTYP                 
037700        MOVE MSG-IDPFK                     TO MFS-IDPFK                   
037800     ELSE                                                                 
037900        MOVE MSG-INDATA-MINUS-1-TRANSKOD   TO MID-W2I14101                
038000        MOVE MSG-IDTRANS-1                 TO MFS-IDTRANS                 
038100        MOVE MSG-KDMFSFOR-1                TO MFS-KDMFSFOR                
038200        MOVE SPACE                         TO MFS-KDTRTYP                 
038300                                              MFS-IDPFK                   
038400     END-IF                                                               
038500     MOVE LOW-VALUE                        TO MSG-AREA                    
038600     MOVE 'W2O14101'                       TO MFS-IDMOD                   
038700     MOVE '2141'                           TO MOD-IDTRANS                 
038800     MOVE MFS-RENSA-FAELT                  TO MOD-TEMFSFEL                
038900                                              MOD-TEMFSINF                
039000                                              MOD-IDANSK-FROM-IN          
039100                                              MOD-IDANSK-TOM-IN           
039200                                              MOD-IDPROJ-IN               
039300     IF MFS-IDTRANS = '2133'                                              
039400        MOVE MID-W2I14101                  TO 2133-MID-W2I13301           
039500     END-IF                                                               
039600     MOVE MFS-IDTRANS                      TO WS-IDTRANS                  
039700                                                                          
039800     MOVE ALL '+'           TO MSGI-WMSGINIT                              
039900     MOVE '001'             TO MSGI-KDCALL                                
040000     MOVE MSG-LTERM-NAME    TO MSGI-IDLTERM-USER                          
040100     MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                                
040200     MOVE '2141'            TO MSGI-IDTRANS                               
040300     CALL W005INIT USING MSGI-WMSGINIT WDP7-PCB                           
040400                                                                          
040500     MOVE MSGI-SPAR-AREA TO SPAR-AREA                                     
040600                                                                          
040700     IF EGEN-BILD                                                         
040800        IF SPAR-LAES-NYCKEL = 'J' OR 'N'                                  
040900          MOVE SPAR-LAES-NYCKEL TO WS-LAES-FLPISK                         
041000                                   W-FLPISK-MIN-B                         
041100                                   W-FLPISK-MAX-B                         
041200        ELSE                                                              
041300          MOVE 'J'              TO WS-LAES-FLPISK                         
041400                                   W-FLPISK-MIN-B                         
041500                                   W-FLPISK-MAX-B                         
041600        END-IF                                                            
041700        IF  MID-ANT-ARTIKLAR    NUMERIC                                   
041800        AND MID-ANT-PASS-INLV   NUMERIC                                   
041900        AND MID-ANT-PISK        NUMERIC                                   
042000          MOVE MID-ANT-ARTIKLAR          TO WS-ANT-ARTIKLAR               
042100          MOVE MID-ANT-PASS-INLV         TO WS-ANT-PASS-INLV              
042200          MOVE MID-ANT-PISK              TO WS-ANT-PISK                   
042300          MOVE MID-HELA-BASEN-LAEST      TO WS-HELA-BASEN-LAEST           
042400       END-IF                                                             
042500     ELSE                                                                 
042600        MOVE 'J'                      TO WS-LAES-FLPISK                   
042700                                         W-FLPISK-MIN-B                   
042800                                         W-FLPISK-MAX-B                   
042900        MOVE SPACE                    TO MFS-KDTRTYP                      
043000        MOVE '7'                      TO MFS-IDPFK                        
043100        MOVE NEJ                      TO MID-HELA-BASEN-LAEST             
043200        IF 2133-BILDEN                                                    
043300           MOVE 2133-MID-IDANSK-FOM   TO MID-IDANSK-FROM-IN               
043400           MOVE 2133-MID-IDANSK-TOM   TO MID-IDANSK-TOM-IN                
043500           MOVE 2133-MID-IDPROJ-VALT  TO MID-IDPROJ-IN                    
043600           MOVE SPAR-LAES-NYCKEL      TO WS-LAES-FLPISK                   
043700                                         W-FLPISK-MIN-B                   
043800                                         W-FLPISK-MAX-B                   
043900        END-IF                                                            
044000     END-IF                                                               
044100     MOVE 'IDAG  '                           TO DAT-KDDATFORM             
044200     PERFORM S99-CALL-WDATKONV                                            
044300     IF DAT-KDSVAR-OK                                                     
044400        MOVE  DAT-TIAA-VECKA               TO WS-DAGENS-AA                
044500        MOVE  DAT-TIVV                     TO WS-DAGENS-VV                
044600     END-IF                                                               
044700     IF ENGLISH-TEXT                                                      
044800        MOVE +2                        TO SPRAK-IX                        
044900     ELSE                                                                 
045000        MOVE +1                        TO SPRAK-IX                        
045100     END-IF                                                               
045200     .                                                                    
045300     EJECT                                                                
045400 B-KOLLA-NYCKLAR SECTION.                                                 
045500     SKIP3                                                                
045600     MOVE JA                          TO SW-NYCKLAR-OK                    
045700     SKIP2                                                                
045800     IF MID-IDANSK-FROM-IN = ALL '+'                                      
045900        IF MID-IDANSK-TOM-IN = ALL '+'                                    
046000           IF MID-IDPROJ-IN = ALL '+'                                     
046100              PERFORM BH-BEHANDLA-GAMLA-NYCKLAR                           
046200           ELSE                                                           
046300              PERFORM  BG-NYTT-PROJ                                       
046400           END-IF                                                         
046500        ELSE                                                              
046600           PERFORM  BB-NY-ANSK-TOM                                        
046700           PERFORM  BE-KOLLA-PROJ                                         
046800        END-IF                                                            
046900     ELSE                                                                 
047000        PERFORM BC-NY-ANSK-FROM                                           
047100        PERFORM BE-KOLLA-PROJ                                             
047200     END-IF                                                               
047300     PERFORM BA-BEHANDLA-WS-FAELT                                         
047400     IF NYCKLAR-OK                                                        
047500        IF W-IDANSK-MIN-B > W-IDANSK-MAX-B                                
047600           MOVE NEJ                        TO SW-NYCKLAR-OK               
047700           MOVE FEL-2 (SPRAK-IX)           TO MOD-TEMFSFEL                
047800        ELSE                                                              
047900           PERFORM BD-KOLLA-TANGENT-TRYCK                                 
048000        END-IF                                                            
048100     ELSE                                                                 
048200        MOVE FEL-2 (SPRAK-IX)           TO MOD-TEMFSFEL                   
048300     END-IF                                                               
048400     .                                                                    
048500     EJECT                                                                
048600 BA-BEHANDLA-WS-FAELT   SECTION.                                          
048700     IF WS-IDANSK-FROM NUMERIC                                            
048800        MOVE WS-IDANSK-FROM       TO W-IDANSK-MIN-B                       
048900                                                                          
049000     ELSE                                                                 
049100        MOVE NEJ                  TO SW-NYCKLAR-OK                        
049200     END-IF                                                               
049300     IF WS-IDANSK-TOM NUMERIC                                             
049400        MOVE WS-IDANSK-TOM        TO W-IDANSK-MAX-B                       
049500                                                                          
049600     ELSE                                                                 
049700        MOVE NEJ                  TO SW-NYCKLAR-OK                        
049800     END-IF                                                               
049900     MOVE WS-IDANSK-FROM          TO MOD-IDANSK-FROM-UT                   
050000     MOVE WS-IDANSK-TOM           TO MOD-IDANSK-TOM-UT                    
050100     INSPECT MOD-IDANSK-FROM-UT REPLACING LEADING ZERO BY SPACE           
050200     INSPECT MOD-IDANSK-TOM-UT REPLACING LEADING ZERO BY SPACE            
050300     IF MOD-IDANSK-FROM-UT = SPACE                                        
050400        MOVE '  0'                TO MOD-IDANSK-FROM-UT                   
050500     END-IF                                                               
050600     IF MOD-IDANSK-TOM-UT  = SPACE                                        
050700        MOVE '  0'                TO MOD-IDANSK-TOM-UT                    
050800     END-IF                                                               
050900     IF WS-IDPROJ = SPACE                                                 
051000        MOVE LOW-VALUE               TO W-IDPROJ-MIN-B                    
051100                                        W-IDPROJ-SMIN                     
051200        MOVE HIGH-VALUE              TO W-IDPROJ-MAX-B                    
051300                                        W-IDPROJ-SMAX                     
051400     ELSE                                                                 
051500        MOVE WS-IDPROJ               TO W-IDPROJ-MIN-B                    
051600                                        W-IDPROJ-SMIN                     
051700                                        W-IDPROJ-MAX-B                    
051800                                        W-IDPROJ-SMAX                     
051900     END-IF                                                               
052000     MOVE WS-IDPROJ                  TO MOD-IDPROJ-UT                     
052100     .                                                                    
052200     EJECT                                                                
052300 BB-NY-ANSK-TOM         SECTION.                                          
052400     MOVE '7'                     TO MFS-IDPFK                            
052500     INSPECT MID-IDANSK-FROM-UT REPLACING LEADING SPACE BY ZERO           
052600     MOVE MID-IDANSK-FROM-UT      TO WS-IDANSK-FROM                       
052700     MOVE MID-IDANSK-TOM-IN       TO WS-IDANSK-TOM                        
052800     .                                                                    
052900     EJECT                                                                
053000 BC-NY-ANSK-FROM        SECTION.                                          
053100     MOVE '7'                        TO MFS-IDPFK                         
053200     IF MID-IDANSK-TOM-IN = ALL '+'                                       
053300        MOVE MID-IDANSK-FROM-IN      TO WS-IDANSK-FROM                    
053400                                        WS-IDANSK-TOM                     
053500     ELSE                                                                 
053600        MOVE MID-IDANSK-FROM-IN      TO WS-IDANSK-FROM                    
053700        MOVE MID-IDANSK-TOM-IN       TO WS-IDANSK-TOM                     
053800     END-IF                                                               
053900     .                                                                    
054000     EJECT                                                                
054100 BD-KOLLA-TANGENT-TRYCK SECTION.                                          
054200     SKIP2                                                                
054300***  WS-SW-RAEKNA-UPP                                                     
054400***  ANVÄNDS FÖR ATT RÄKNA UPP ANTAL ARTIKLAR,                            
054500***  PASSERADE INLEVERANSER OCH PISK-MÄRKTA                               
054600     SKIP2                                                                
054700     IF MFS-IDPFK = '8'                                                   
054800        IF MID-IDARTNR-HI NUMERIC AND MID-IDANSK-HI NUMERIC               
054900                                AND MID-TIFINLEV-HI NUMERIC               
055000           IF MID-IDARTNR-HI ZERO                                         
055100              MOVE '7'                  TO MFS-IDPFK                      
055200           ELSE                                                           
055300              MOVE MID-IDANSK-HI        TO W-IDANSK-GU                    
055400                                                                          
055500              MOVE MID-IDPROJ-HI        TO W-IDPROJ-GU                    
055600                                                                          
055700              MOVE MID-FLPISK-HI        TO W-FLPISK-GU                    
055800                                                                          
055900              MOVE MID-TIFINLEV-HI      TO DAT-I-TIDATUM                  
056000              MOVE 'AAMMDD'             TO DAT-KDDATFORM                  
056100              PERFORM S99-CALL-WDATKONV                                   
056200              IF DAT-KDSVAR-OK                                            
056300                 MOVE DAT-TISEKEL       TO W-DAFINLEV-GU    (1:2)         
056400                                                                          
056500                 MOVE DAT-TIAAMMDD      TO W-DAFINLEV-GU    (3:6)         
056600                                                                          
056700              END-IF                                                      
056800              MOVE MID-IDAO-HI          TO W-IDAO-GU                      
056900                                                                          
057000                                                                          
057100              MOVE MID-IDARTNR-HI       TO W-IDARTNR-GU                   
057200                                                                          
057300                                                                          
057400              MOVE JA                   TO WS-SW-RAEKNA-UPP               
057500           END-IF                                                         
057600        ELSE                                                              
057700           MOVE '7'                     TO MFS-IDPFK                      
057800        END-IF                                                            
057900     ELSE                                                                 
058000        IF MFS-IDPFK = '7'                                                
058100           CONTINUE                                                       
058200        ELSE                                                              
058300            MOVE NEJ                    TO WS-SW-RAEKNA-UPP               
058400            IF MID-IDARTNR-LO NUMERIC AND MID-IDANSK-LO NUMERIC           
058500                                    AND MID-TIFINLEV-LO NUMERIC           
058600               MOVE MID-IDANSK-LO       TO W-IDANSK-GU                    
058700                                                                          
058800               MOVE MID-IDPROJ-LO       TO W-IDPROJ-GU                    
058900                                                                          
059000               MOVE MID-FLPISK-LO       TO W-FLPISK-GU                    
059100                                           W-FLPISK-MIN-B                 
059200                                           W-FLPISK-MAX-B                 
059300               MOVE MID-TIFINLEV-LO     TO DAT-I-TIDATUM                  
059400               MOVE 'AAMMDD'            TO DAT-KDDATFORM                  
059500               PERFORM S99-CALL-WDATKONV                                  
059600               IF DAT-KDSVAR-OK                                           
059700                 MOVE DAT-TISEKEL       TO W-DAFINLEV-GU    (1:2)         
059800                                                                          
059900                 MOVE DAT-TIAAMMDD      TO W-DAFINLEV-GU    (3:6)         
060000                                                                          
060100               END-IF                                                     
060200               MOVE MID-IDAO-LO         TO W-IDAO-GU                      
060300                                                                          
060400               MOVE MID-IDARTNR-LO      TO W-IDARTNR-GU                   
060500                                                                          
060600            ELSE                                                          
060700               MOVE '7'                 TO MFS-IDPFK                      
060800            END-IF                                                        
060900        END-IF                                                            
061000     END-IF                                                               
061100     IF MFS-IDPFK = '7'                                                   
061200        MOVE SPACE                  TO MFS-KDTRTYP                        
061300        MOVE JA                     TO WS-SW-RAEKNA-UPP                   
061400        MOVE NEJ                    TO WS-HELA-BASEN-LAEST                
061500        MOVE ZERO                   TO WS-ANT-ARTIKLAR                    
061600                                       WS-ANT-PASS-INLV                   
061700                                       WS-ANT-PISK                        
061800                                       W-DAFINLEV-MIN-B                   
061900                                       W-IDARTNR-MIN-B                    
062000        MOVE LOW-VALUE              TO W-IDAO-MIN-B                       
062100                                                                          
062200        MOVE 'J'                    TO W-FLPISK-MIN-B                     
062300                                       W-FLPISK-MAX-B                     
062400                                                                          
062500                                                                          
062600        MOVE MED-3 (SPRAK-IX)       TO MOD-TEMFSFEL                       
062700     END-IF                                                               
062800     .                                                                    
062900     EJECT                                                                
063000 BE-KOLLA-PROJ SECTION.                                                   
063100     SKIP2                                                                
063200     IF MID-IDPROJ-IN = ALL '+'                                           
063300        MOVE MID-IDPROJ-UT         TO WS-IDPROJ                           
063400     ELSE                                                                 
063500        MOVE MID-IDPROJ-IN         TO WS-IDPROJ                           
063600     END-IF                                                               
063700     .                                                                    
063800     EJECT                                                                
063900 BG-NYTT-PROJ   SECTION.                                                  
064000     SKIP2                                                                
064100     MOVE '7'                     TO MFS-IDPFK                            
064200     MOVE MID-IDPROJ-IN           TO WS-IDPROJ                            
064300     MOVE ZERO                    TO WS-IDANSK-FROM                       
064400     MOVE 999                     TO WS-IDANSK-TOM                        
064500     .                                                                    
064600     EJECT                                                                
064700 BH-BEHANDLA-GAMLA-NYCKLAR    SECTION.                                    
064800     SKIP2                                                                
064900     INSPECT MID-IDANSK-FROM-UT REPLACING LEADING SPACE BY ZERO           
065000     INSPECT MID-IDANSK-TOM-UT REPLACING  LEADING SPACE BY ZERO           
065100     MOVE MID-IDANSK-FROM-UT TO WS-IDANSK-FROM                            
065200                                W-IDANSK-MIN-B                            
065300     MOVE MID-IDANSK-TOM-UT  TO WS-IDANSK-TOM                             
065400                                W-IDANSK-MAX-B                            
065500     MOVE MID-IDPROJ-UT      TO WS-IDPROJ                                 
065600                                W-IDPROJ-MIN-B                            
065700                                W-IDPROJ-MAX-B                            
065800     .                                                                    
065900     EJECT                                                                
066000 C-KOLLA-INDATA SECTION.                                                  
066100     MOVE JA                              TO SW-INDATA-OK                 
066200     MOVE +1                              TO RAD-INDX                     
066300     PERFORM UNTIL RAD-INDX > MAX-RAD                                     
066400        IF MID-NY-IDANSK (RAD-INDX) = ALL '+'                             
066500           CONTINUE                                                       
066600        ELSE                                                              
066700           IF MID-NY-IDANSK (RAD-INDX) NUMERIC                            
066800              MOVE MFS-NUM-FAELT-RAETT       TO                           
066900                              MOD-NY-IDANSK-ATTR (RAD-INDX)               
067000              INSPECT MID-IDARTNR (RAD-INDX) REPLACING LEADING            
067100                                                 SPACE BY ZERO            
067200              IF MID-IDARTNR (RAD-INDX) NUMERIC                           
067300                 MOVE MID-IDARTNR (RAD-INDX)    TO                        
067400                                                   W-IDARTNR              
067500                 PERFORM IMS-GU-ARTG01                                    
067600                 IF SEGMENT-FINNS                                         
067700                    MOVE MFS-NUM-FAELT-RAETT     TO                       
067800                                    MOD-IDARTNR-ATTR   (RAD-INDX)         
067900                 ELSE                                                     
068000                    MOVE MFS-NUM-FAELT-FEL      TO                        
068100                                 MOD-IDARTNR-ATTR   (RAD-INDX)            
068200                    MOVE NEJ                    TO SW-INDATA-OK           
068300                 END-IF                                                   
068400              ELSE                                                        
068500                 MOVE MFS-NUM-FAELT-FEL            TO                     
068600                                    MOD-IDARTNR-ATTR   (RAD-INDX)         
068700                 MOVE NEJ                          TO SW-INDATA-OK        
068800              END-IF                                                      
068900           ELSE                                                           
069000              MOVE NEJ                       TO SW-INDATA-OK              
069100              MOVE MFS-NUM-FAELT-FEL         TO                           
069200                              MOD-NY-IDANSK-ATTR (RAD-INDX)               
069300              MOVE MFS-NUM-FAELT-RAETT       TO                           
069400                              MOD-IDARTNR-ATTR   (RAD-INDX)               
069500           END-IF                                                         
069600        END-IF                                                            
069700        ADD +1                          TO RAD-INDX                       
069800     END-PERFORM                                                          
069900     .                                                                    
070000     EJECT                                                                
070100 D-UPPDATERA SECTION.                                                     
070200     SKIP2                                                                
070300     MOVE +1                              TO RAD-INDX                     
070400     PERFORM UNTIL RAD-INDX > MAX-RAD                                     
070500        IF MID-NY-IDANSK (RAD-INDX) = ALL '+'                             
070600           CONTINUE                                                       
070700        ELSE                                                              
070800           MOVE MID-IDARTNR (RAD-INDX)          TO                        
070900                                               W-IDARTNR                  
071000           PERFORM IMS-GHU-ARTG01                                         
071100           MOVE MID-NY-IDANSK (RAD-INDX)        TO                        
071200                                        ARTG01-ART-IDANSK                 
071300           PERFORM IMS-REPL-ARTG01                                        
071400        END-IF                                                            
071500        ADD +1                                  TO RAD-INDX               
071600     END-PERFORM                                                          
071700     .                                                                    
071800     EJECT                                                                
071900 E-MFS-ROER-EJ-FAELT SECTION.                                             
072000     MOVE MFS-ROER-EJ-FAELT          TO                                   
072100                                        MOD-ANT-ARTIKLAR                  
072200                                        MOD-ANT-PASS-INLV                 
072300                                        MOD-ANT-PISK                      
072400                                        MOD-HELA-BASEN-LAEST              
072500                                        MOD-IDANSK-LO                     
072600                                        MOD-IDANSK-HI                     
072700                                        MOD-IDPROJ-LO                     
072800                                        MOD-IDPROJ-HI                     
072900                                        MOD-FLPISK-LO                     
073000                                        MOD-FLPISK-HI                     
073100                                        MOD-TIFINLEV-LO                   
073200                                        MOD-TIFINLEV-HI                   
073300                                        MOD-IDAO-LO                       
073400                                        MOD-IDAO-HI                       
073500                                        MOD-VISA-ANT-ARTIKLAR             
073600                                        MOD-VISA-ANT-PASS-INLV            
073700                                        MOD-VISA-ANT-PISK                 
073800                                        MOD-IDARTNR-LO                    
073900                                        MOD-IDARTNR-HI                    
074000     MOVE +1                         TO RAD-INDX                          
074100     PERFORM UNTIL RAD-INDX > MAX-RAD                                     
074200        MOVE MFS-ROER-EJ-FAELT       TO                                   
074300                                        MOD-IDARTNR   (RAD-INDX)          
074400                                        MOD-IDPROJ    (RAD-INDX)          
074500                                        MOD-IDAO      (RAD-INDX)          
074600                                        MOD-TIFINLEV  (RAD-INDX)          
074700                                        MOD-FLPISK    (RAD-INDX)          
074800                                        MOD-TIREGDAT  (RAD-INDX)          
074900                                        MOD-IDANSK    (RAD-INDX)          
075000                                        MOD-NY-IDANSK (RAD-INDX)          
075010                                        MOD-FLCN      (RAD-INDX)          
075100        ADD +1                       TO RAD-INDX                          
075200     END-PERFORM                                                          
075300     .                                                                    
075400     EJECT                                                                
075500 F-RENSA-FAELT SECTION.                                                   
075600     MOVE MFS-RENSA-FAELT              TO MOD-VISA-ANT-ARTIKLAR           
075700                                          MOD-VISA-ANT-PASS-INLV          
075800                                          MOD-VISA-ANT-PISK               
075900     MOVE +1                           TO RAD-INDX                        
076000     PERFORM UNTIL RAD-INDX > MAX-RAD                                     
076100           MOVE MFS-RENSA-FAELT        TO                                 
076200                                      MOD-IDARTNR   (RAD-INDX)            
076300                                      MOD-IDPROJ    (RAD-INDX)            
076400                                      MOD-IDAO      (RAD-INDX)            
076500                                      MOD-TIFINLEV  (RAD-INDX)            
076600                                      MOD-FLPISK    (RAD-INDX)            
076700                                      MOD-TIREGDAT  (RAD-INDX)            
076800                                      MOD-IDANSK    (RAD-INDX)            
076900                                      MOD-NY-IDANSK (RAD-INDX)            
077000                                      MOD-FLCN      (RAD-INDX)            
077100           ADD +1                      TO RAD-INDX                        
077200     END-PERFORM                                                          
077300     .                                                                    
077400     EJECT                                                                
077500 G-VISA-SIDAN SECTION.                                                    
077600     MOVE +1                            TO RAD-INDX                       
077700**        WS-HELA-BASEN-LAEST FÅR SITT VÄRDE I A-INIT                     
077800     IF WS-HELA-BASEN-LAEST = JA                                          
077900        MOVE NEJ                        TO WS-SW-RAEKNA-UPP               
078000     END-IF                                                               
078100     PERFORM UNTIL RAD-INDX > MAX-ANT-LAESN                               
078200        IF SEGMENT-FINNS                                                  
078300           IF SEQ-SEQB-DAFINLEV = +99999999                               
078400           OR SEQ-SEQB-TIMOTSI > ZERO                                     
078500              CONTINUE                                                    
078600           ELSE                                                           
078700              IF RAD-INDX > MAX-RAD-PLUS-1                                
078800                 IF WS-SW-RAEKNA-UPP = JA                                 
078900                    PERFORM GD-RAEKNA-UPP                                 
079000                 END-IF                                                   
079100              ELSE                                                        
079200                 IF MAX-RAD-PLUS-1  =  RAD-INDX                           
079300                    PERFORM GE-BYT-HI-NYCKLAR                             
079400                 ELSE                                                     
079500                    PERFORM GA-FL-T-BILD                                  
079600                    MOVE WS-ANT-ARTIKLAR      TO                          
079700                                        WS-MAX-RAD-ANT-ARTIKLAR           
079800                    MOVE WS-ANT-PASS-INLV     TO                          
079900                                        WS-MAX-RAD-ANT-PASS-INLV          
080000                    MOVE WS-ANT-PISK          TO                          
080100                                        WS-MAX-RAD-ANT-PISK               
080200                 END-IF                                                   
080300                 IF WS-SW-RAEKNA-UPP = JA                                 
080400                    PERFORM GF-RAEKNA-UPP                                 
080500                 END-IF                                                   
080600              END-IF                                                      
080700              ADD +1                       TO RAD-INDX                    
080800           END-IF                                                         
080900           PERFORM S01-LAES-WDD2B                                         
081000        ELSE                                                              
081100           IF WDD2B-STATUS-CODE = 'GB' OR 'GE'                            
081200             IF RAD-INDX <= MAX-RAD-PLUS-1                                
081300                MOVE MED-5 (SPRAK-IX)       TO MOD-TEMFSINF               
081400             END-IF                                                       
081500           END-IF                                                         
081600           IF RAD-INDX < MAX-RAD-PLUS-2                                   
081700              PERFORM GB-RENSA-SIDAN                                      
081800           END-IF                                                         
081900           MOVE  MAX-ANT-LAESN-PLUS-1      TO RAD-INDX                    
082000        END-IF                                                            
082100     END-PERFORM                                                          
082200     PERFORM GC-BEHANDLA-UPPRAEKNBARA-FAELT                               
082300     MOVE WS-LAES-FLPISK TO SPAR-LAES-NYCKEL                              
082400     MOVE '002'      TO MSGI-KDCALL                                       
082500     MOVE '2141'     TO SPAR-IDTRANS                                      
082600     MOVE SPAR-AREA  TO MSGI-SPAR-AREA                                    
082700     CALL W005INIT USING MSGI-WMSGINIT WDP7-PCB                           
082800     .                                                                    
082900     EJECT                                                                
083000 GA-FL-T-BILD SECTION.                                                    
083100     SKIP2                                                                
083200                                                                          
083300     MOVE SEQ-SEQB-IDARTNR            TO W-IDARTNR                        
083400     PERFORM IMS-GU-WDK611                                                
083500     IF SEGMENT-FINNS                                                     
083600** VISA INTE ERSATTA ARTIKLAR ELLER                                       
083610**           ARTIKEL KOPPLADE TILL ANSK 210 ELLER 310                     
083700       IF (CLAG-KDERS > 19)                                               
083712       OR (SEQ-SEQB-IDANSK = +210 OR +310)                                
083717         COMPUTE RAD-INDX = RAD-INDX - 1                                  
083720       ELSE                                                               
083800         IF RAD-INDX = 1                                                  
083900            MOVE SEQ-SEQB-IDANSK        TO MOD-IDANSK-LO                  
084000            MOVE SEQ-SEQB-IDPROJ        TO MOD-IDPROJ-LO                  
084100            MOVE SEQ-SEQB-FLPISK        TO MOD-FLPISK-LO                  
084200            MOVE SEQ-SEQB-DAFINLEV(3:6) TO MOD-TIFINLEV-LO                
084300            MOVE SEQ-SEQB-IDAO          TO MOD-IDAO-LO                    
084400            MOVE SEQ-SEQB-IDARTNR       TO MOD-IDARTNR-LO                 
084500         END-IF                                                           
084600         MOVE SEQ-SEQB-IDARTNR          TO MOD-IDARTNR (RAD-INDX)         
084700         MOVE SEQ-SEQB-IDPROJ           TO MOD-IDPROJ  (RAD-INDX)         
084800         MOVE SEQ-SEQB-IDAO             TO MOD-IDAO    (RAD-INDX)         
084900         MOVE SEQ-SEQB-FLPISK           TO MOD-FLPISK  (RAD-INDX)         
085000                                           WS-FLPISK                      
085100         MOVE SEQ-SEQB-IDANSK           TO MOD-IDANSK  (RAD-INDX)         
085101* TO BE DECIDED                                                           
085110         MOVE SPACES                    TO MOD-FLCN    (RAD-INDX)         
085120*                                                                         
085200         MOVE 'AAMMDD'                  TO DAT-KDDATFORM                  
085300         MOVE SEQ-SEQB-DAFINLEV (3:6)   TO DAT-I-TIDATUM                  
085400                                           WS-TIFINLEV                    
085500         PERFORM S99-CALL-WDATKONV                                        
085600         IF DAT-KDSVAR-OK                                                 
085700            MOVE DAT-TIAA-VECKA         TO W-AA                           
085800            MOVE DAT-TIVV               TO W-VV                           
085900            MOVE W-AAVV                 TO MOD-TIFINLEV (RAD-INDX)        
086000         END-IF                                                           
086100                                                                          
086200         MOVE SEQ-SEQB-IDARTNR          TO W-IDARTNR                      
086300         PERFORM IMS-GU-ARTC01                                            
086400         MOVE ART-IDFKNGRP              TO MOD-IDFKNGRP (RAD-INDX)        
086500         MOVE 'AAMMDD'                  TO DAT-KDDATFORM                  
086600         MOVE ART-TIREGDAT              TO DAT-I-TIDATUM                  
086700         PERFORM S99-CALL-WDATKONV                                        
086800         IF DAT-KDSVAR-OK                                                 
086900            MOVE DAT-TIAA-VECKA         TO W-AA                           
087000            MOVE DAT-TIVV               TO W-VV                           
087100            MOVE W-AAVV                 TO MOD-TIREGDAT (RAD-INDX)        
087200         END-IF                                                           
087500       END-IF                                                             
087600     ELSE                                                                 
087700       COMPUTE RAD-INDX = RAD-INDX - 1                                    
087800     END-IF                                                               
087900     .                                                                    
088000     EJECT                                                                
088100 GB-RENSA-SIDAN SECTION.                                                  
088200     PERFORM UNTIL RAD-INDX > MAX-RAD                                     
088300           MOVE MFS-RENSA-FAELT   TO MOD-IDARTNR    (RAD-INDX)            
088400                                     MOD-IDPROJ     (RAD-INDX)            
088500                                     MOD-IDAO       (RAD-INDX)            
088600                                     MOD-TIFINLEV   (RAD-INDX)            
088700                                     MOD-FLPISK     (RAD-INDX)            
088800                                     MOD-TIREGDAT   (RAD-INDX)            
088900                                     MOD-IDANSK     (RAD-INDX)            
088910                                     MOD-FLCN       (RAD-INDX)            
089000           ADD +1                 TO                 RAD-INDX             
089100     END-PERFORM                                                          
089200     MOVE ZERO                    TO MOD-IDARTNR-HI                       
089300     .                                                                    
089400     EJECT                                                                
089500 GC-BEHANDLA-UPPRAEKNBARA-FAELT  SECTION.                                 
089600     IF WS-SW-RAEKNA-UPP = JA                                             
089700        IF SEGMENT-FINNS                                                  
089800           MOVE NEJ                      TO WS-HELA-BASEN-LAEST           
089900           MOVE WS-MAX-RAD-ANT-ARTIKLAR  TO MOD-ANT-ARTIKLAR              
090000           MOVE WS-MAX-RAD-ANT-PASS-INLV TO MOD-ANT-PASS-INLV             
090100           MOVE WS-MAX-RAD-ANT-PISK      TO MOD-ANT-PISK                  
090200        ELSE                                                              
090300           MOVE JA                       TO WS-HELA-BASEN-LAEST           
090400           MOVE WS-ANT-ARTIKLAR          TO MOD-ANT-ARTIKLAR              
090500           MOVE WS-ANT-PASS-INLV         TO MOD-ANT-PASS-INLV             
090600           MOVE WS-ANT-PISK              TO MOD-ANT-PISK                  
090700        END-IF                                                            
090800     ELSE                                                                 
090900        MOVE WS-ANT-ARTIKLAR             TO MOD-ANT-ARTIKLAR              
091000        MOVE WS-ANT-PASS-INLV            TO MOD-ANT-PASS-INLV             
091100        MOVE WS-ANT-PISK                 TO MOD-ANT-PISK                  
091200     END-IF                                                               
091300     IF WS-HELA-BASEN-LAEST = JA                                          
091400        MOVE WS-ANT-ARTIKLAR             TO MOD-VISA-ANT-ARTIKLAR         
091500        MOVE WS-ANT-PASS-INLV            TO MOD-VISA-ANT-PASS-INLV        
091600        MOVE WS-ANT-PISK                 TO MOD-VISA-ANT-PISK             
091700     ELSE                                                                 
091800        MOVE MFS-BLANKA-UT-FAELT         TO MOD-VISA-ANT-ARTIKLAR         
091900                                            MOD-VISA-ANT-PASS-INLV        
092000                                            MOD-VISA-ANT-PISK             
092100     END-IF                                                               
092200     MOVE WS-HELA-BASEN-LAEST            TO MOD-HELA-BASEN-LAEST          
092300     .                                                                    
092400     EJECT                                                                
092500 GD-RAEKNA-UPP SECTION.                                                   
092600     SKIP2                                                                
092700     ADD +1                       TO WS-ANT-ARTIKLAR                      
092800     MOVE SEQ-SEQB-DAFINLEV (3:6) TO DAT-I-TIDATUM                        
092900     MOVE 'AAMMDD'                TO DAT-KDDATFORM                        
093000     PERFORM S99-CALL-WDATKONV                                            
093100     IF DAT-KDSVAR-OK                                                     
093200        MOVE DAT-TIAA-VECKA       TO W-AA                                 
093300        MOVE DAT-TIVV             TO W-VV                                 
093400        MOVE WS-DAGENS-AAVV   TO TMP1-YYWW                                
093500        MOVE W-AAVV-NUM       TO TMP2-YYWW                                
093600        PERFORM WY2000P3                                                  
093700        IF TMP1-YYWW > TMP2-YYWW                                          
093800           ADD +1                 TO WS-ANT-PASS-INLV                     
093900        END-IF                                                            
094000     END-IF                                                               
094100     IF SEQ-SEQB-FLPISK = JA                                              
094200        ADD +1                 TO WS-ANT-PISK                             
094300     END-IF                                                               
094400     .                                                                    
094500     EJECT                                                                
094600 GE-BYT-HI-NYCKLAR            SECTION.                                    
094700     SKIP2                                                                
094800     MOVE SEQ-SEQB-IDANSK         TO MOD-IDANSK-HI                        
094900     MOVE SEQ-SEQB-IDPROJ         TO MOD-IDPROJ-HI                        
095000     MOVE SEQ-SEQB-FLPISK         TO MOD-FLPISK-HI                        
095100                                     WS-FLPISK                            
095200     MOVE SEQ-SEQB-DAFINLEV (3:6) TO MOD-TIFINLEV-HI                      
095300                                        WS-TIFINLEV                       
095400     MOVE SEQ-SEQB-IDAO           TO MOD-IDAO-HI                          
095500     MOVE SEQ-SEQB-IDARTNR        TO MOD-IDARTNR-HI                       
095600     IF MFS-UPDATE                                                        
095700        CONTINUE                                                          
095800     ELSE                                                                 
095900        MOVE MED-2 (SPRAK-IX)     TO MOD-TEMFSINF                         
096000     END-IF                                                               
096100     IF WS-SW-RAEKNA-UPP = NEJ                                            
096200        MOVE MAX-ANT-LAESN-PLUS-1 TO RAD-INDX                             
096300     END-IF                                                               
096400     .                                                                    
096500     EJECT                                                                
096600 GF-RAEKNA-UPP  SECTION.                                                  
096700     ADD +1                       TO WS-ANT-ARTIKLAR                      
096800     MOVE WS-TIFINLEV             TO DAT-I-TIDATUM                        
096900     MOVE 'AAMMDD'                TO DAT-KDDATFORM                        
097000     PERFORM S99-CALL-WDATKONV                                            
097100     IF DAT-KDSVAR-OK                                                     
097200        MOVE DAT-TIAA-VECKA       TO W-AA                                 
097300        MOVE DAT-TIVV             TO W-VV                                 
097400        MOVE WS-DAGENS-AAVV   TO TMP1-YYWW                                
097500        MOVE W-AAVV-NUM       TO TMP2-YYWW                                
097600        PERFORM WY2000P3                                                  
097700        IF TMP1-YYWW > TMP2-YYWW                                          
097800           ADD +1                 TO WS-ANT-PASS-INLV                     
097900        END-IF                                                            
098000     END-IF                                                               
098100     IF WS-FLPISK = JA                                                    
098200        ADD +1                 TO WS-ANT-PISK                             
098300     END-IF                                                               
098400     .                                                                    
098500     EJECT                                                                
098600 H-KOLLA-ATT-INDATA-EJ-IFYLLD SECTION.                                    
098700     SKIP2                                                                
098800     MOVE +1                             TO RAD-INDX                      
098900     PERFORM UNTIL RAD-INDX > MAX-RAD                                     
099000        IF MID-NY-IDANSK (RAD-INDX) = ALL '+'                             
099100           CONTINUE                                                       
099200        ELSE                                                              
099300           MOVE JA TO SW-INDATA-IFYLLT-EJ-UPPDATE                         
099400           MOVE MFS-NUM-FAELT-RAETT     TO                                
099500                                MOD-NY-IDANSK-ATTR    (RAD-INDX)          
099600        END-IF                                                            
099700        ADD +1                           TO RAD-INDX                      
099800     END-PERFORM                                                          
099900     .                                                                    
100000     EJECT                                                                
100100 I-FORMATETS-ATTR  SECTION.                                               
100200     SKIP2                                                                
100300     MOVE +1                                  TO RAD-INDX                 
100400     PERFORM UNTIL RAD-INDX > MAX-RAD                                     
100500           MOVE MFS-FORMATETS-ATTR            TO                          
100600                                  MOD-IDARTNR-ATTR   (RAD-INDX)           
100700                                  MOD-NY-IDANSK-ATTR (RAD-INDX)           
100800           ADD +1                 TO  RAD-INDX                            
100900     END-PERFORM                                                          
101000     .                                                                    
101100     EJECT                                                                
101200 K-RENSA-INMATNINGSFAELT  SECTION.                                        
101300     SKIP2                                                                
101400     MOVE MFS-RENSA-FAELT          TO  MOD-IDANSK-FROM-IN                 
101500                                       MOD-IDANSK-FROM-UT                 
101600                                       MOD-IDANSK-TOM-IN                  
101700                                       MOD-IDANSK-TOM-UT                  
101800                                       MOD-IDPROJ-IN                      
101900                                       MOD-IDPROJ-UT                      
102000     .                                                                    
102100     EJECT                                                                
102200 S99-CALL-WDATKONV   SECTION.                                             
102300     SKIP2                                                                
102400     CALL WDATKONV           USING  DAT-KDDATFORM                         
102500                                    DAT-I-TIDATUM                         
102600                                    DAT-O-TIDATUM                         
102700                                    DAT-KDSVAR                            
102800     .                                                                    
102900     EJECT                                                                
103000 S01-LAES-WDD2B SECTION.                                                  
103100     PERFORM IMS-GN-WDD2B01                                               
103200     IF SEGMENT-SAKNAS OR BASEN-SLUT                                      
103300       IF W-FLPISK-MIN-B = 'J'                                            
103400         MOVE 'N' TO W-FLPISK-MIN-B                                       
103500                     W-FLPISK-MAX-B                                       
103600                     WS-LAES-FLPISK                                       
103700         PERFORM IMS-GN-WDD2B01                                           
103800       END-IF                                                             
103900     END-IF                                                               
104000     .                                                                    
104100     EJECT                                                                
104200* IMS SEKTIONER                                                           
104300     SKIP3                                                                
104400 IMS-GET-MSG SECTION.                                                     
104500                                                                          
104600     MOVE '  QC' TO GODK-STATUSKODER                                      
104700     CALL  CBLTDLI  USING GU MSG-PCB MSG-IO-AREA                          
104800     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
104900     PERFORM IMS-STATUSKONTROLL                                           
105000     SKIP3                                                                
105100     .                                                                    
105200 IMS-INSERT-MSG SECTION.                                                  
105300                                                                          
105400     IF ENGLISH-TEXT                                                      
105500       MOVE 'N' TO MFS-KDHUVOMR                                           
105600     END-IF                                                               
105700     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
105800     MOVE SPACE TO GODK-STATUSKODER                                       
105900     CALL  CBLTDLI  USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD              
106000     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
106100     PERFORM IMS-STATUSKONTROLL                                           
106200     EJECT                                                                
106300     .                                                                    
106400*                                                                         
106500*IMS-GN-ARTI01  SECTION.                                                  
106600*    SKIP1                                                                
106700*    STRING 'WLARTI01(WDD2B1KY>=' W-WDD2B1KY-MIN                          
106800*                   '&WDD2B1KY<=' W-WDD2B1KY-MAX                          
106900*                   '&IDPROJ  >=' W-IDPROJ-SMIN                           
107000*                   '&IDPROJ  <=' W-IDPROJ-SMAX                           
107100*                   '&KDANSKQ  =' W-KDANSKQ-X ')'                         
107200*           DELIMITED BY SIZE INTO SSA1                                   
107300*    MOVE '  GE' TO GODK-STATUSKODER                                      
107400*    CALL  CBLTDLI  USING GN ARTI-PCB DLI-IO-AREA SSA1                    
107500*    MOVE ARTI-STATUS-CODE TO STATUS-WS                                   
107600*    PERFORM IMS-STATUSKONTROLL                                           
107700*    SKIP3                                                                
107800*    .                                                                    
107900 IMS-GN-WDD2B01 SECTION.                                                  
108000     SKIP1                                                                
108100     STRING 'WDD2B1  (WDD2B1KY>=' W-WDD2B1KY-MIN                          
108200                    '&WDD2B1KY<=' W-WDD2B1KY-MAX                          
108300                    '&IDPROJ  >=' W-IDPROJ-SMIN                           
108400                    '&IDPROJ  <=' W-IDPROJ-SMAX                           
108500                    '&KDANSKQ  =' W-KDANSKQ-X ')'                         
108600            DELIMITED BY SIZE INTO SSA1                                   
108700     MOVE '  GEGB' TO GODK-STATUSKODER                                    
108800     CALL  CBLTDLI  USING GN WDD2B-PCB DLI-IO-WDD2SEQ SSA1                
108900     MOVE WDD2B-STATUS-CODE TO STATUS-WS                                  
109000     PERFORM IMS-STATUSKONTROLL                                           
109100     SKIP3                                                                
109200     .                                                                    
109300 IMS-GU-WDD2B01 SECTION.                                                  
109400     SKIP1                                                                
109500     STRING 'WDD2B1  (WDD2B1KY =' W-WDD2B1KY-GU                           
109600                    '&KDANSKQ  =' W-KDANSKQ-X ')'                         
109700            DELIMITED BY SIZE INTO SSA1                                   
109800     MOVE '  GEGB' TO GODK-STATUSKODER                                    
109900     CALL  CBLTDLI  USING GU WDD2B-PCB DLI-IO-WDD2SEQ SSA1                
110000     MOVE WDD2B-STATUS-CODE TO STATUS-WS                                  
110100     PERFORM IMS-STATUSKONTROLL                                           
110200     SKIP3                                                                
110300     .                                                                    
110400 IMS-GU-ARTG01 SECTION.                                                   
110500     SKIP1                                                                
110600     STRING 'WLARTG01(IDARTNR  =' W-IDARTNR-X ')'                         
110700            DELIMITED BY SIZE INTO SSA1                                   
110800     MOVE '  GE' TO GODK-STATUSKODER                                      
110900     CALL  CBLTDLI  USING GU ARTG-PCB DLI-IO-AREA SSA1                    
111000     MOVE ARTG-STATUS-CODE TO STATUS-WS                                   
111100     PERFORM IMS-STATUSKONTROLL                                           
111200     SKIP3                                                                
111300     .                                                                    
111400 IMS-GHU-ARTG01 SECTION.                                                  
111500                                                                          
111600     STRING 'WLARTG01(IDARTNR  =' W-IDARTNR-X ')'                         
111700            DELIMITED BY SIZE INTO SSA1                                   
111800     MOVE '  ' TO GODK-STATUSKODER                                        
111900     CALL  CBLTDLI  USING GHU ARTG-PCB DLI-IO-AREA SSA1                   
112000     MOVE ARTG-STATUS-CODE TO STATUS-WS                                   
112100     PERFORM IMS-STATUSKONTROLL                                           
112200     SKIP3                                                                
112300     .                                                                    
112400 IMS-GU-ARTC01 SECTION.                                                   
112500                                                                          
112600     STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-X ')'                         
112700            DELIMITED BY SIZE INTO SSA1                                   
112800     MOVE '  ' TO GODK-STATUSKODER                                        
112900     CALL  CBLTDLI  USING GU ARTC-PCB DLI-IO-AREA-01 SSA1                 
113000     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
113100     PERFORM IMS-STATUSKONTROLL                                           
113200     SKIP3                                                                
113300     .                                                                    
113400 IMS-GU-WDK611 SECTION.                                                   
113500                                                                          
113600     STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-X ')'                         
113700            DELIMITED BY SIZE INTO SSA1                                   
113800     STRING 'WLARTC11(KDSEGKEY =' W-KDSEGKEY-X ')'                        
113900            DELIMITED BY SIZE INTO SSA2                                   
114000     MOVE '  GE' TO GODK-STATUSKODER                                      
114100     CALL  CBLTDLI  USING GU ARTC-PCB DLI-IO-WDK611  SSA1 SSA2            
114200     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
114300     PERFORM IMS-STATUSKONTROLL                                           
114400     SKIP3                                                                
114500     .                                                                    
114600 IMS-REPL-ARTG01 SECTION.                                                 
114700                                                                          
114800     MOVE '  ' TO GODK-STATUSKODER                                        
114900     CALL  CBLTDLI  USING REPL ARTG-PCB DLI-IO-AREA                       
115000     MOVE ARTG-STATUS-CODE TO STATUS-WS                                   
115100     PERFORM IMS-STATUSKONTROLL                                           
115200     SKIP3                                                                
115300     .                                                                    
115400 IMS-STATUSKONTROLL SECTION.                                              
115500                                                                          
115600     SET STATUS-IX TO 1                                                   
115700     SEARCH GODK-STATUS AT END CALL  FELLOG                               
115800       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
115900       CONTINUE                                                           
116000     END-SEARCH                                                           
116100     .                                                                    
116200     EJECT                                                                
116300*    -COPY WY2000P3                                                       
