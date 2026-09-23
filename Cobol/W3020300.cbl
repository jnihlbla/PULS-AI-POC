000100     SKIP3                                                                
000200 ID DIVISION.                                                             
000300     SKIP2                                                                
000400 PROGRAM-ID.     W3020300.                                                
000500 AUTHOR.         PETER D.                                                 
000600 DATE-WRITTEN.   SEP   89.                                                
000700                                                                          
000800     REMARKS.                                                             
000900*                                                                         
001000*    FUNKTION.                                                            
001100*        PROGRAMMET LÄSER OCH UPPDATERAR ON-LINE.                         
001200*                                                                         
001300*                                                                         
001400*                                                                         
001500*    INDATA.                                                              
001600*        TRANSAKTION: W3T203                                              
001700*        MID:         W3I20301                                            
001800*                                                                         
001900*    UTDATA.                                                              
002000*        MOD:         W3O20301                                            
002100     SKIP3                                                                
002200 ENVIRONMENT DIVISION.                                                    
002300     SKIP2                                                                
002400 DATA DIVISION.                                                           
002500     EJECT                                                                
002600 WORKING-STORAGE SECTION.                                                 
002601*    -COPY WY2000W3                                                       
002602     SKIP3                                                                
002700 77  PROGRAM-NAMN                PIC X(8) VALUE 'W3020300'.               
002800 77  JA                          PIC X       VALUE 'J'.                   
002900 77  NEJ                         PIC X       VALUE 'N'.                   
003000 77  SPRAK-IX                    PIC X(3)    VALUE SPACE.                 
003100 77  ANTAL-ARTIKLAR-BAS          PIC S9(9)   VALUE +0   COMP SYNC.        
003200 77  ANTAL-ATT-TA-BORT           PIC S9(9)   VALUE +0   COMP SYNC.        
003300 77  RAD-INDX                    PIC S9(9)   VALUE +0   COMP SYNC.        
003400 77  MAX-MOD-LAENGD              PIC S9(4)  VALUE  +783 COMP SYNC.        
003500 77  WS-IDFSGURV                 PIC  X(8)   VALUE SPACE.                 
003600 01  WS-TIREGTID.                                                         
003700   03  WS-TIREGTID-HHMMSS        PIC 9(6)    VALUE ZERO.                  
003800   03  FILLER                    PIC 9(2)    VALUE ZERO.                  
003900 01  SW-IFYLLT             PIC X.                                         
004000    88  INGET-IFYLLT                         VALUE 'N'.                   
004100*KONFLIKT TÄNDS OM MER ÄN ETT URVAL AV KONCERN MARKNAD ELER               
004200*DISTRIKT ÄR VALDA                                                        
004300 01  SW-KONFLIKT                 PIC X.                                   
004400    88  KONFLIKT                             VALUE 'J'.                   
004500 01  SW-KONCERN                  PIC X.                                   
004600    88  KONCERN-EJ-IFYLLD                    VALUE 'N'.                   
004700 01  SW-DISTRIKT                 PIC X.                                   
004800    88  DISTRIKT-EJ-IFYLLD                   VALUE 'N'.                   
004900 01  SW-MARKNAD                  PIC X.                                   
005000    88  MARKNAD-EJ-IFYLLD                    VALUE 'N'.                   
005100 01  SW-NYCKLAR-OK               PIC X.                                   
005200    88  NYCKLAR-OK                           VALUE 'J'.                   
005300 01  SW-INDATA-OK                PIC X.                                   
005400    88  INDATA-OK                            VALUE 'J'.                   
005500 01  WS-IDTRANS                  PIC X(4).                                
005600    88  EGEN-BILD                            VALUE '3203'.                
005700    88  3201-BILD                            VALUE '3201'.                
005800    88  GODKAEND-BILD                        VALUE '3201'                 
005900                                                   '3202'                 
006000                                                   '3204'                 
006100                                                   '3203'.                
006200     EJECT                                                                
006300 01  DATUMKORT.                                                           
006400   03  FILLER                    PIC X(16)   VALUE                        
006500                                            'DATUMKORT       '.           
006600   03  WS-AAVV.                                                           
006700     05  WS-AA                   PIC 9(2)    VALUE ZERO.                  
006800     05  WS-VV                   PIC 9(2)    VALUE ZERO.                  
006810   03  WS-AAVV-N REDEFINES WS-AAVV                                        
006820                                 PIC 9(4).                                
006900   03  WS-DAGENS-AAVV.                                                    
007000     05  WS-DAGENS-AA            PIC 9(2)    VALUE ZERO.                  
007100     05  WS-DAGENS-VV            PIC 9(2)    VALUE ZERO.                  
007110   03  WS-DAGENS-AAVV-N REDEFINES WS-DAGENS-AAVV                          
007120                                 PIC 9(4).                                
007200   03  WS-DAGENS-AAVV-MINUS-2-AA.                                         
007300     05  WS-DAGENS-AA-MINUS-2-AA PIC 9(2)    VALUE ZERO.                  
007400     05  WS-DAGENS-VV-MINUS-2-AA PIC 9(2)    VALUE ZERO.                  
007410   03  WS-DAGENS-AAVV-MINUS-2-AA-N REDEFINES                              
007420       WS-DAGENS-AAVV-MINUS-2-AA PIC 9(4).                                
007500*03 -COPY WDATAREA                                                        
007700     EJECT                                                                
007800 01  DYNAMISKA-SUBPROGRAM.                                                
007900   03  WDATKONV                  PIC X(8) VALUE 'WDATKONV'.               
008000   03  WMEDKONV                  PIC X(8) VALUE 'WMEDKONV'.               
008100     EJECT                                                                
008200 01  NYCKLAR-TILL-DLI.                                                    
008300   03  FILLER                    PIC X(16)   VALUE                        
008400                                            'NYCKLAR-TILL-DLI'.           
008500   03  W-WDM301KY-SPAR.                                                   
008600     05  W-IDUSER-SPAR           PIC  X(8)   VALUE SPACE.                 
008700     05  W-DAREGDAT-SPAR         PIC 9(8)    VALUE ZERO.                  
008800     05  W-TIREGTID-SPAR         PIC S9(7)   VALUE ZERO  COMP-3.          
008900   03  W-WDM301KY-X.                                                      
009000     05  W-IDUSER                PIC  X(8)   VALUE SPACE.                 
009100     05  W-DAREGDAT              PIC 9(8)    VALUE ZERO.                  
009200     05  W-TIREGTID              PIC S9(7)   VALUE ZERO  COMP-3.          
009300   03  W-WDM3A1KY-MIN.                                                    
009400     05  W-IDUSER-MIN            PIC  X(8)   VALUE SPACE.                 
009500     05  W-IDFSGURV-MIN          PIC  X(8)   VALUE SPACE.                 
009600     05  W-IDTRANS-MIN           PIC  X(4)   VALUE SPACE.                 
009700     05  FILLER                  PIC  X(12)   VALUE LOW-VALUE.            
009800   03  W-WDM3A1KY-MAX.                                                    
009900     05  W-IDUSER-MAX            PIC  X(8)   VALUE SPACE.                 
010000     05  W-IDFSGURV-MAX          PIC  X(8)   VALUE SPACE.                 
010100     05  W-IDTRANS-MAX           PIC  X(4)   VALUE SPACE.                 
010200     05  FILLER                  PIC  X(12)   VALUE HIGH-VALUE.           
010300   03  W-KDSEGKEY-X.                                                      
010400     05  W-KDSEGKEY              PIC  X(1)   VALUE '1'.                   
010500   03  W-IDARTNR-X.                                                       
010600     05  W-IDARTNR               PIC  S9(9)  VALUE ZERO  COMP-3.          
010700   03  W-IDARTNR-SPAR-X.                                                  
010800     05  W-IDARTNR-SPAR          PIC  S9(9)  VALUE ZERO  COMP-3.          
010900     EJECT                                                                
011000 01  DYNAMISKA-SUBPROGRAM.                                                
011100   03  FELLOG                    PIC X(8)    VALUE 'FELLOG  '.            
011200   03  CBLTDLI                   PIC X(8)    VALUE 'CBLTDLI '.            
011300 01  MEDDELANDE.                                                          
011400   03  FILLER                    PIC X(16)   VALUE 'MEDDELANDE'.          
011500   03  FEL1.                                                              
011600     05 FILLER                   PIC X(40)                                
011700          VALUE 'UPPLYSTA FÄLT FEL'.                                      
011800     05 FILLER                   PIC X(40)                                
011900          VALUE 'HIGHLIGHTED FIELDS ARE WRONG'.                           
012000   03  FILLER REDEFINES FEL1.                                             
012100     05  FEL-1                   PIC X(40)   OCCURS 2.                    
012200                                                                          
012300   03  FEL2.                                                              
012400     05 FILLER                   PIC X(40)                                
012500          VALUE 'KONFLIKT                                '.               
012600     05 FILLER                   PIC X(40)                                
012700          VALUE 'CONFLICT                                '.               
012800   03  FILLER REDEFINES FEL2.                                             
012900     05  FEL-2                   PIC X(40)   OCCURS 2.                    
013000                                                                          
013100   03  FEL3.                                                              
013200     05 FILLER                   PIC X(40)                                
013300          VALUE 'TRYCK PF11 VID UPPDATERING'.                             
013400     05 FILLER                   PIC X(40)                                
013500          VALUE 'PRESS PF11 WHEN UPDATE'.                                 
013600   03  FILLER REDEFINES FEL3.                                             
013700     05  FEL-3                   PIC X(40)   OCCURS 2.                    
013800                                                                          
013900   03  FEL4.                                                              
014000     05 FILLER                   PIC X(40)                                
014100          VALUE 'ANGE ETT AV NEDANSTÅENDE URVAL'.                         
014200     05 FILLER                   PIC X(40)                                
014300          VALUE 'SPECIFY ONE CHOISE (SEE BELOW)'.                         
014400   03  FILLER REDEFINES FEL4.                                             
014500     05  FEL-4                   PIC X(40)   OCCURS 2.                    
014600                                                                          
014700   03  FEL5.                                                              
014800     05 FILLER                   PIC X(40)                                
014900          VALUE 'URVAL SAKNAS          '.                                 
015000     05 FILLER                   PIC X(40)                                
015100          VALUE 'KEYS ARE MISSING  '.                                     
015200   03  FILLER REDEFINES FEL5.                                             
015300     05  FEL-5                   PIC X(40)   OCCURS 2.                    
015400    SKIP1                                                                 
015500                                                                          
015600   03  FEL6.                                                              
015700     05 FILLER                   PIC X(40)                                
015800          VALUE 'DETTA ÄR FÖRSTA SIDAN '.                                 
015900     05 FILLER                   PIC X(40)                                
016000          VALUE 'THIS IS THE FIRST PAGE'.                                 
016100   03  FILLER REDEFINES FEL6.                                             
016200     05  FEL-6                   PIC X(40)   OCCURS 2.                    
016300    SKIP1                                                                 
016400                                                                          
016500   03  MED1.                                                              
016600     05 FILLER                   PIC X(40)                                
016700          VALUE 'UPPDATERING GJORD     '.                                 
016800     05 FILLER                   PIC X(40)                                
016900          VALUE 'FIELDS ARE UPDATED'.                                     
017000   03  FILLER REDEFINES MED1.                                             
017100     05  MED-1                   PIC X(40)   OCCURS 2.                    
017200    SKIP1                                                                 
017300   03  MED2.                                                              
017400     05 FILLER                   PIC X(60)                                
017500         VALUE 'KONCERN             DISTRIKT             MARKNAD'.        
017600     05 FILLER                   PIC X(60)                                
017700          VALUE '                                        '.               
017800   03  FILLER REDEFINES MED2.                                             
017900     05  MED-2                   PIC X(60)   OCCURS 2.                    
018000    SKIP1                                                                 
018100   03  MED3.                                                              
018200     05 FILLER                   PIC X(60)                                
018300         VALUE 'ARTIKEL SAKNAS PÅ BASEN                         '.        
018400     05 FILLER                   PIC X(60)                                
018500          VALUE 'PARTNUMBER IS MISSING ON THE BASE       '.               
018600   03  FILLER REDEFINES MED3.                                             
018700     05  MED-3                   PIC X(60)   OCCURS 2.                    
018800    SKIP1                                                                 
018900   03  MED4.                                                              
019000     05 FILLER                   PIC X(60)                                
019100         VALUE 'FLER SIDOR FINNS                                '.        
019200     05 FILLER                   PIC X(60)                                
019300          VALUE 'THERE ARE MORE SIDES                    '.               
019400   03  FILLER REDEFINES MED4.                                             
019500     05  MED-4                   PIC X(60)   OCCURS 2.                    
019600                                                                          
019700   03  MED5.                                                              
019800     05 FILLER                   PIC X(60)                                
019900         VALUE 'MINST ETT ARTIKELNUMMER MÅSTE FINNAS KVAR       '.        
020000     05 FILLER                   PIC X(60)                                
020100          VALUE 'YOU ARE TRYING TO DELETE TOO MANY PARTS '.               
020200   03  FILLER REDEFINES MED5.                                             
020300     05  MED-5                   PIC X(60)   OCCURS 2.                    
020400                                                                          
020500   03  MED6.                                                              
020600     05 FILLER                   PIC X(60)                                
020700         VALUE 'BORTTAG OCH UPPATERING SAMTIDIGT GÅR EJ         '.        
020800     05 FILLER                   PIC X(60)                                
020900          VALUE 'DEL AND UPDATE AT THE SAME TIME IS NOT ALLOWED'.         
021000   03  FILLER REDEFINES MED6.                                             
021100     05  MED-6                   PIC X(60)   OCCURS 2.                    
021200                                                                          
021300                                                                          
021400   03  MED8.                                                              
021500     05 FILLER                   PIC X(60)                                
021600         VALUE 'ENBART URVAL UNDER EGET USERID FÅR TAS BORT     '.        
021700     05 FILLER                   PIC X(60)                                
021800          VALUE 'YOU ARE NOT ALLOWED TO DELETE OTHERS USERID S '.         
021900   03  FILLER REDEFINES MED8.                                             
022000     05  MED-8                   PIC X(60)   OCCURS 2.                    
022100                                                                          
022200     EJECT                                                                
022300******************************************************************        
022400*                                                                         
022500*                AREOR FÖR MFS OCH SKÄRMHANTERING                         
022600*                                                                         
022700     SKIP3                                                                
022800 01  FILLER                      PIC X(16)   VALUE 'MID-COPY-WS'.         
022900*01  MID -COPY W3I20301                                                   
023100     EJECT                                                                
023200 01  FILLER                      PIC X(16)   VALUE 'MSG-COPY-WS'.         
023300*01  -COPY WMSGAREA                                                       
023500     EJECT                                                                
023600*  03  MOD -COPY W3O20301           -RED MSG-AREA.                        
023800     EJECT                                                                
023900 01  FILLER                      PIC X(16)   VALUE 'MFS-COPY-WS'.         
024000*01  -COPY WMFSAREA                                                       
024200     EJECT                                                                
024300 01  FILLER                      PIC X(16)   VALUE 'WMEDAREA   '.         
024400*01  -COPY WMEDAREA                                                       
024600     EJECT                                                                
024700*01  WLFSGA01    -COPY WDM301 -PRE WS-.                                   
024900     EJECT                                                                
025000*01  WLFSGA13    -COPY WDM313 -PRE WS-.                                   
025200     EJECT                                                                
025300*01  WLFSGA14    -COPY WDM314 -PRE WS-.                                   
025500     EJECT                                                                
025600******************************************************************        
025700*                                                                         
025800*        ARBETS-AREOR TILL IMS-SEKTIONERNA                                
025900*                                                                         
026000 01  IMS-WS.                                                              
026100   03  FILLER                    PIC X(16)   VALUE 'IMS-WS     '.         
026200     SKIP3                                                                
026300*                        **** STATUS-KOD FRÅN IMS                         
026400   03  STATUS-WS                 PIC XX.                                  
026500     88  SEGMENT-FINNS                       VALUE '  '.                  
026600     88  SEGMENT-SAKNAS                      VALUE 'GE' 'GB'.             
026700     SKIP3                                                                
026800   03  GODK-STATUSKODER.                                                  
026900     05  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
027000     SKIP3                                                                
027100 01    SSA1                      PIC X(128).                              
027200 01    SSA2                      PIC X(64).                               
027300     EJECT                                                                
027400*                            IMS FUNKTIONSKODER                           
027500*01    -COPY W0003                                                        
027700     EJECT                                                                
027800************************     DLI INPUT-OUTPUT AREA ***************        
027900 01  FILLER                    PIC X(16)   VALUE 'DLI-IO-AREA'.           
028000 01  DLI-IO-AREA.                                                         
028100   03  IO-AREA                   PIC X(100)  VALUE SPACE.                 
028200     SKIP3                                                                
028300*03  WLFSGA01    -COPY WDM301   -RED IO-AREA.                             
028500     EJECT                                                                
028600*03  WLFSGA13    -COPY WDM313   -RED IO-AREA.                             
028800     EJECT                                                                
028900*03  WLFSGA14    -COPY WDM314   -RED IO-AREA.                             
029100     EJECT                                                                
029200*03  WLFSGB01    -COPY WDM3A1   -RED IO-AREA.                             
029400     EJECT                                                                
029500 01  FILLER                    PIC X(16)   VALUE 'DLI-IO-AREA-2'.         
029600 01  DLI-IO-AREA-2.                                                       
029700   03  IO-AREA-2                 PIC X(100)  VALUE SPACE.                 
029800     SKIP3                                                                
029900*03  WLFSGA01    -COPY WDM301   -PRE SPAR- -RED IO-AREA-2.                
030100     EJECT                                                                
030200*03  WLFSGA14    -COPY WDM314   -PRE SPAR- -RED IO-AREA-2.                
030400     EJECT                                                                
030500 LINKAGE SECTION.                                                         
030600*01  -COPY W0009     -PRE MSG-                                            
030800     SKIP2                                                                
030900*01  -COPY W0008     -PRE FSGA-                                           
031100     05  FILLER                  PIC X.                                   
031200     SKIP2                                                                
031300*01  -COPY W0008     -PRE FSGA1-                                          
031500     05  FILLER                  PIC X.                                   
031600     SKIP2                                                                
031700*01  -COPY W0008     -PRE FSGB-                                           
031900     05  FILLER                  PIC X.                                   
032000     EJECT                                                                
032100 PROCEDURE DIVISION USING MSG-PCB FSGA-PCB FSGA1-PCB FSGB-PCB.            
032200     ENTRY 'DLITCBL' USING MSG-PCB FSGA-PCB FSGA1-PCB FSGB-PCB.           
032300                                                                          
032400     PERFORM IMS-GET-MSG                                                  
032500     IF SEGMENT-FINNS                                                     
032600        PERFORM A-INIT                                                    
032700        IF GODKAEND-BILD                                                  
032800           PERFORM B-GOR-IORDN-NYCK-PFTRY                                 
032900           IF MFS-UPDATE                                                  
033000              IF  W-DAREGDAT = ZERO                                       
033100                 PERFORM C-KOLLA-INDATA                                   
033200              ELSE                                                        
033300                 PERFORM D-KOLLA-INDATA                                   
033400              END-IF                                                      
033500              IF INDATA-OK                                                
033600                 PERFORM E-UPPDATERA-VISA-SIDAN                           
033700              ELSE                                                        
033800                 IF KONFLIKT                                              
033900                    PERFORM S04-SAETT-NUMFAELT-FEL                        
034000                 END-IF                                                   
034100                                                                          
034200                 MOVE '001' TO MED-IDMFSFEL                               
034300                 MOVE SPRAK-IX TO MED-IDSKYLT                             
034400                 CALL WMEDKONV USING MED-WMEDAREA                         
034500                 MOVE MED-MFSFEL TO MOD-TEMFSFEL                          
034600                                                                          
034700                 PERFORM S06-MFS-ROER-EJ-FAELT                            
034800              END-IF                                                      
034900           ELSE                                                           
035000              MOVE NEJ                    TO SW-IFYLLT                    
035100              IF MFS-IDPFK = '7'                                          
035200              OR MID-IDARTNR-LO = 999999999                               
035300                 MOVE ZERO                TO W-IDARTNR                    
035400                 MOVE ZERO                TO MID-IDARTNR-LO               
035500              ELSE                                                        
035600                 PERFORM S08-KOLLA-ATT-INGET-IFYLLT                       
035700              END-IF                                                      
035800              IF INGET-IFYLLT                                             
035900                IF MID-IDFSGURV-IN = ALL '+'                              
036000                   PERFORM IMS-GU-FSGA01                                  
036100                   IF SEGMENT-FINNS                                       
036200                      PERFORM S07-VISA-SIDAN                              
036300                   ELSE                                                   
036400                      PERFORM F-LAES-FSGB-FSGA                            
036500                   END-IF                                                 
036600                ELSE                                                      
036700                   PERFORM F-LAES-FSGB-FSGA                               
036800                END-IF                                                    
036900              ELSE                                                        
037000                                                                          
037100                 MOVE '003' TO MED-IDMFSFEL                               
037200                 MOVE SPRAK-IX TO MED-IDSKYLT                             
037300                 CALL WMEDKONV USING MED-WMEDAREA                         
037400                 MOVE MED-MFSFEL TO MOD-TEMFSFEL                          
037500                                                                          
037600                 PERFORM S06-MFS-ROER-EJ-FAELT                            
037700                 PERFORM S09-LAES-IN-IGEN                                 
037800              END-IF                                                      
037900           END-IF                                                         
038000        ELSE                                                              
038100           MOVE MFS-RENSA-FAELT            TO MOD-IDFSGURV-UT             
038200                                              MOD-IDUSER-UT               
038300           PERFORM S01-RENSA-HELA-SIDAN                                   
038400        END-IF                                                            
038500        MOVE MAX-MOD-LAENGD TO MSG-KVLL                                   
038600        PERFORM IMS-INSERT-MSG                                            
038700     END-IF                                                               
038800     MOVE ZERO                                  TO RETURN-CODE            
038900     GOBACK                                                               
039000     .                                                                    
039100     EJECT                                                                
039200 A-INIT SECTION.                                                          
039300     SKIP2                                                                
039400     IF MSG-DUBBLA-TRANSKODER                                             
039500        MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W3I20301                
039600        MOVE MSG-IDTRANS-2                 TO MFS-IDTRANS                 
039700        MOVE MSG-KDMFSFOR-2                TO MFS-KDMFSFOR                
039800        MOVE MSG-KDTRTYP                   TO MFS-KDTRTYP                 
039900        MOVE MSG-IDPFK                     TO MFS-IDPFK                   
040000     ELSE                                                                 
040100        MOVE MSG-INDATA-MINUS-1-TRANSKOD   TO MID-W3I20301                
040200        MOVE MSG-IDTRANS-1                 TO MFS-IDTRANS                 
040300        MOVE MSG-KDMFSFOR-1                TO MFS-KDMFSFOR                
040400        MOVE SPACE                         TO MFS-KDTRTYP                 
040500                                              MFS-IDPFK                   
040600     END-IF                                                               
040700                                                                          
040800     MOVE LOW-VALUE                        TO MSG-AREA                    
040900     MOVE 'W3O203N1'                       TO MFS-IDMOD                   
041000     MOVE '3203'                           TO MOD-IDTRANS                 
041100     MOVE MFS-IDTRANS                      TO WS-IDTRANS                  
041200     MOVE MFS-RENSA-FAELT                  TO MOD-TEMFSFEL                
041300                                              MOD-TEMFSINF                
041400                                              MOD-IDFSGURV-IN             
041500                                              MOD-IDUSER-IN               
041600*                                                                         
041700     IF EGEN-BILD                                                         
041800        CONTINUE                                                          
041900     ELSE                                                                 
042000        MOVE SPACE                         TO MFS-KDTRTYP                 
042100        MOVE '7'                           TO MFS-IDPFK                   
042200     END-IF                                                               
042300     IF ENGLISH-TEXT                                                      
042400        MOVE 'GB '                         TO SPRAK-IX                    
042500     ELSE                                                                 
042600        MOVE 'S  '                         TO SPRAK-IX                    
042700     END-IF                                                               
042800     MOVE 'AAMMDD'                         TO DAT-KDDATFORM               
042900     ACCEPT DAT-I-TIDATUM FROM DATE                                       
043000     PERFORM S99-CALL-WDATKONV                                            
043100     IF DAT-KDSVAR-OK                                                     
043200        MOVE DAT-TIAA-VECKA                 TO WS-DAGENS-AA               
043300        MOVE DAT-TIVV                       TO WS-DAGENS-VV               
043400        MOVE WS-DAGENS-AAVV                 TO                            
043500                                   WS-DAGENS-AAVV-MINUS-2-AA              
043510        IF WS-DAGENS-AA-MINUS-2-AA = 00                                   
043520          MOVE 98 TO WS-DAGENS-AA-MINUS-2-AA                              
043530        ELSE                                                              
043540          IF WS-DAGENS-AA-MINUS-2-AA = 01                                 
043550            MOVE 99 TO WS-DAGENS-AA-MINUS-2-AA                            
043560          ELSE                                                            
043600            SUBTRACT 2 FROM WS-DAGENS-AA-MINUS-2-AA                       
043610          END-IF                                                          
043620        END-IF                                                            
043700     END-IF                                                               
043800     INITIALIZE WS-WLFSGA13                                               
043900     .                                                                    
044000     EJECT                                                                
044100 B-GOR-IORDN-NYCK-PFTRY SECTION.                                          
044200     SKIP3                                                                
044300     IF MID-IDFSGURV-IN = ALL '+'                                         
044400        MOVE MID-IDFSGURV-UT         TO WS-IDFSGURV                       
044500     ELSE                                                                 
044600        MOVE MID-IDFSGURV-IN         TO WS-IDFSGURV                       
044700        MOVE SPACE                   TO MFS-KDTRTYP                       
044800        MOVE '7'                     TO MFS-IDPFK                         
044900     END-IF                                                               
045000     MOVE WS-IDFSGURV                TO MOD-IDFSGURV-UT                   
045100                                        W-IDFSGURV-MIN                    
045200                                        W-IDFSGURV-MAX                    
045300     IF MFS-UPDATE                                                        
045400        MOVE MID-IDUSER-UT           TO W-IDUSER                          
045500     ELSE                                                                 
045600        IF MID-IDUSER-IN = ALL '+'                                        
045700           MOVE MID-IDUSER-UT        TO W-IDUSER                          
045800        ELSE                                                              
045900           MOVE MID-IDUSER-IN        TO W-IDUSER                          
046000           MOVE '7'                  TO MFS-IDPFK                         
046100        END-IF                                                            
046200     END-IF                                                               
046300     IF W-IDUSER = SPACE                                                  
046400        MOVE MSG-SIGNON-USERID       TO W-IDUSER                          
046500     END-IF                                                               
046600     MOVE W-IDUSER                   TO MOD-IDUSER-UT                     
046700                                        W-IDUSER-MIN                      
046800                                        W-IDUSER-MAX                      
046900     MOVE  3203                      TO W-IDTRANS-MIN                     
047000                                        W-IDTRANS-MAX                     
047100     PERFORM BA-KOLLA-PF-TRYCK                                            
047200     .                                                                    
047300     EJECT                                                                
047400 BA-KOLLA-PF-TRYCK  SECTION.                                              
047500     SKIP2                                                                
047600* TIREGTID OCH DAREGDAT    DOLDA    FÄLT GES VÄRDE I A-INIT               
047700* D-VISA SAMT E-UPPDATERA                                                 
047800     IF  MID-IDARTNR-LO NUMERIC                                           
047900     AND MID-IDARTNR-HI NUMERIC                                           
048000     AND MID-DAREGDAT-DOLD NUMERIC                                        
048100     AND MID-TIREGTID-DOLD NUMERIC                                        
048200        MOVE MID-DAREGDAT-DOLD         TO W-DAREGDAT                      
048210*---Y2K-FIX********                                                       
048220        IF MID-DAREGDAT-DOLD NOT = ZERO                                   
048230          IF MID-DAREGDAT-DOLD < 1000000                                  
048240*           MOVE 20         TO W-DAREGDAT(1:2)                            
048250*         ELSE                                                            
048260*           IF MID-DAREGDAT-DOLD < 999999                                 
048270              MOVE 19       TO W-DAREGDAT(1:2)                            
048280*           ELSE                                                          
048290*             MOVE 99999999 TO W-DAREGDAT                                 
048291*           END-IF                                                        
048292          END-IF                                                          
048293        END-IF                                                            
048300        MOVE MID-TIREGTID-DOLD         TO W-TIREGTID                      
048400     ELSE                                                                 
048500        MOVE '7'                       TO MFS-IDPFK                       
048600        MOVE SPACE                     TO MFS-KDTRTYP                     
048700        MOVE ZERO                      TO W-TIREGTID                      
048800                                          W-DAREGDAT                      
048900     END-IF                                                               
049000     IF MFS-IDPFK = '8'                                                   
049100     AND MID-IDARTNR-HI  > ZERO                                           
049200        MOVE MID-IDARTNR-HI            TO W-IDARTNR                       
049300     ELSE                                                                 
049400        IF MFS-IDPFK = ' '                                                
049500           MOVE MID-IDARTNR-LO         TO W-IDARTNR                       
049600        ELSE                                                              
049700                                                                          
049800           MOVE '006' TO MED-IDMFSFEL                                     
049900           MOVE SPRAK-IX TO MED-IDSKYLT                                   
050000           CALL WMEDKONV USING MED-WMEDAREA                               
050100           MOVE MED-MFSFEL TO MOD-TEMFSFEL                                
050200                                                                          
050300           MOVE ZERO                   TO W-IDARTNR                       
050400        END-IF                                                            
050500     END-IF                                                               
050600     .                                                                    
050700     EJECT                                                                
050800 C-KOLLA-INDATA SECTION.                                                  
050900     SKIP2                                                                
051000     MOVE JA                             TO SW-INDATA-OK                  
051100     IF MID-KDBORT = 'B' OR 'D' OR 'J'                                    
051200        MOVE MFS-ALFA-FAELT-RAETT        TO MOD-KDBORT-ATTR               
051300     ELSE                                                                 
051400        PERFORM CA-KOLLA-VECKA                                            
051500        PERFORM CB-KOLLA-SPAR-BORT-LIST-PRIS                              
051600        PERFORM CC-KOLLA-IDARTNR                                          
051700        MOVE NEJ                         TO SW-KONFLIKT                   
051800* SW-KONFLIKT HÅLLER REDA PÅ ATT ENBART ETT URVAL AV                      
051900* KONCNR KDMARK OCH IDDISTR ANGES                                         
052000* OBS! FEL-FLAGGAN TÄNDS EJ UTAN BEHANDLAS SOM FEL I HUVUDSLINGAN         
052100        PERFORM CD-KOLLA-IDKONCNR                                         
052200        PERFORM CE-KOLLA-KDMARK                                           
052300        PERFORM CF-KOLLA-IDDISTR                                          
052400        IF MID-IDPTYP = 'PPV'                                             
052500          PERFORM CG-KOLLA-PPV                                            
052600        END-IF                                                            
052700     END-IF                                                               
052800     .                                                                    
052900     EJECT                                                                
053000 CA-KOLLA-VECKA      SECTION.                                             
053100     SKIP2                                                                
053200     IF  MID-TIFSGVV-FOM = ALL '+'                                        
053300     AND MID-TIFSGVV-TOM = ALL '+'                                        
053400        CONTINUE                                                          
053500     ELSE                                                                 
053600        IF  MID-TIFSGVV-FOM NUMERIC                                       
053700        AND MID-TIFSGVV-TOM NUMERIC                                       
053701           MOVE MID-TIFSGVV-TOM   TO TMP1-YYWW                            
053702           MOVE MID-TIFSGVV-FOM   TO TMP2-YYWW                            
053710           PERFORM WY2000P3                                               
053800           IF  TMP1-YYWW >= TMP2-YYWW                                     
053900              MOVE 'AAVV  '             TO DAT-KDDATFORM                  
054000              MOVE MID-TIFSGVV-FOM      TO DAT-I-TIDATUM                  
054100              PERFORM S99-CALL-WDATKONV                                   
054200              IF DAT-KDSVAR-OK                                            
054300                 MOVE DAT-TIAA-VECKA        TO WS-AA                      
054400                 MOVE DAT-TIVV              TO WS-VV                      
054401                 MOVE WS-AAVV-N                   TO TMP1-YYWW            
054402                 MOVE WS-DAGENS-AAVV-MINUS-2-AA-N TO TMP2-YYWW            
054410                 PERFORM WY2000P3                                         
054500                 IF TMP1-YYWW >= TMP2-YYWW                                
054600                    MOVE MFS-NUM-FAELT-RAETT  TO                          
054700                                          MOD-TIFSGVV-FOM-ATTR            
054800                    MOVE MID-TIFSGVV-FOM      TO                          
054900                                  WS-URV2-TIFSGVV-FOM                     
055000                 ELSE                                                     
055100                    MOVE NEJ                  TO SW-INDATA-OK             
055200                    MOVE MFS-NUM-FAELT-FEL    TO                          
055300                                          MOD-TIFSGVV-FOM-ATTR            
055400                 END-IF                                                   
055500              ELSE                                                        
055600                 MOVE NEJ                  TO SW-INDATA-OK                
055700                 MOVE MFS-NUM-FAELT-FEL    TO                             
055800                                       MOD-TIFSGVV-FOM-ATTR               
055900              END-IF                                                      
056000              MOVE 'AAVV  '             TO DAT-KDDATFORM                  
056100              MOVE MID-TIFSGVV-TOM      TO DAT-I-TIDATUM                  
056200              PERFORM S99-CALL-WDATKONV                                   
056300              IF DAT-KDSVAR-OK                                            
056400                 MOVE DAT-TIAA-VECKA        TO WS-AA                      
056500                 MOVE DAT-TIVV              TO WS-VV                      
056501                 MOVE WS-AAVV-N        TO TMP1-YYWW                       
056502                 MOVE WS-DAGENS-AAVV-N TO TMP2-YYWW                       
056510                 PERFORM WY2000P3                                         
056600                 IF TMP1-YYWW < TMP2-YYWW                                 
056700                    MOVE MFS-NUM-FAELT-RAETT  TO                          
056800                                          MOD-TIFSGVV-TOM-ATTR            
056900                    MOVE MID-TIFSGVV-TOM      TO                          
057000                                  WS-URV2-TIFSGVV-TOM                     
057100                 ELSE                                                     
057200                    IF MID-IDPTYP = 'VA1' OR 'VA2' AND                    
057300                       WS-AAVV = WS-DAGENS-AAVV                           
057400                       MOVE MID-TIFSGVV-TOM      TO                       
057500                                          WS-URV2-TIFSGVV-TOM             
057600                    ELSE                                                  
057700                       MOVE NEJ                  TO SW-INDATA-OK          
057800                       MOVE MFS-NUM-FAELT-FEL    TO                       
057900                                          MOD-TIFSGVV-TOM-ATTR            
058000                    END-IF                                                
058100                 END-IF                                                   
058200              ELSE                                                        
058300                 MOVE NEJ                  TO SW-INDATA-OK                
058400                 MOVE MFS-NUM-FAELT-FEL    TO                             
058500                                          MOD-TIFSGVV-TOM-ATTR            
058600              END-IF                                                      
058700           ELSE                                                           
058800              MOVE NEJ                  TO SW-INDATA-OK                   
058900              MOVE MFS-NUM-FAELT-FEL    TO MOD-TIFSGVV-FOM-ATTR           
059000                                           MOD-TIFSGVV-TOM-ATTR           
059100           END-IF                                                         
059200        ELSE                                                              
059300           MOVE NEJ                      TO SW-INDATA-OK                  
059400           IF  MID-TIFSGVV-FOM      = ALL '+'                             
059500              MOVE MFS-NUM-FAELT-FEL    TO MOD-TIFSGVV-TOM-ATTR           
059600           ELSE                                                           
059700              IF  MID-TIFSGVV-TOM      = ALL '+'                          
059800                 MOVE MFS-NUM-FAELT-FEL TO MOD-TIFSGVV-FOM-ATTR           
059900              ELSE                                                        
060000                 MOVE MFS-NUM-FAELT-FEL TO MOD-TIFSGVV-FOM-ATTR           
060100                                           MOD-TIFSGVV-TOM-ATTR           
060200              END-IF                                                      
060300           END-IF                                                         
060400        END-IF                                                            
060500     END-IF                                                               
060600     .                                                                    
060700     EJECT                                                                
060800 CB-KOLLA-SPAR-BORT-LIST-PRIS   SECTION.                                  
060900     SKIP2                                                                
061000     IF MID-IDFSGURV = ALL '+'                                            
061100        CONTINUE                                                          
061200     ELSE                                                                 
061300        MOVE MFS-ALFA-FAELT-RAETT        TO MOD-IDFSGURV-ATTR             
061400     END-IF                                                               
061500     IF MID-KDBORT   = ALL '+'                                            
061600        CONTINUE                                                          
061700     ELSE                                                                 
061800        MOVE NEJ                         TO SW-INDATA-OK                  
061900        MOVE MFS-ALFA-FAELT-FEL          TO MOD-KDBORT-ATTR               
062000     END-IF                                                               
062100     IF MID-KDCMD    = ALL '+'                                            
062200        CONTINUE                                                          
062300     ELSE                                                                 
062400        MOVE NEJ                         TO SW-INDATA-OK                  
062500        MOVE MFS-ALFA-FAELT-FEL          TO MOD-KDCMD-ATTR                
062600     END-IF                                                               
062700     IF MID-IDPTYP = ALL '+'                                              
062800        MOVE NEJ                         TO SW-INDATA-OK                  
062900        MOVE MFS-ALFA-FAELT-FEL          TO MOD-IDPTYP-ATTR               
063000     ELSE                                                                 
063010                                                                          
063011                                                                          
063020                                                                          
063100        IF MID-IDPTYP = 'P1' OR 'PPV' OR 'A1' OR 'A2'                     
063200           IF  MID-TIFSGVV-FOM = ALL '+'                                  
063300              MOVE MFS-ALFA-FAELT-RAETT     TO MOD-IDPTYP-ATTR            
063400              MOVE MID-IDPTYP               TO WS-URV2-IDPTYP             
063500           ELSE                                                           
063600              MOVE NEJ                      TO SW-INDATA-OK               
063700              MOVE MFS-ALFA-FAELT-FEL       TO MOD-IDPTYP-ATTR            
063800           END-IF                                                         
063900        ELSE                                                              
064000           IF MID-IDPTYP = 'VA1' OR 'VA2'                                 
064100              IF  MID-TIFSGVV-FOM = ALL '+'                               
064200                 MOVE NEJ                   TO SW-INDATA-OK               
064300                 MOVE MFS-ALFA-FAELT-FEL    TO MOD-IDPTYP-ATTR            
064400              ELSE                                                        
064500                 MOVE MFS-ALFA-FAELT-RAETT  TO MOD-IDPTYP-ATTR            
064600                 MOVE MID-IDPTYP            TO WS-URV2-IDPTYP             
064700              END-IF                                                      
064800           ELSE                                                           
064900             MOVE NEJ                  TO SW-INDATA-OK                    
065000             MOVE MFS-ALFA-FAELT-FEL   TO MOD-IDPTYP-ATTR                 
065100           END-IF                                                         
065200        END-IF                                                            
065300     END-IF                                                               
065400     IF MID-KDPRTYPG = ALL '+'                                            
065500        CONTINUE                                                          
065600     ELSE                                                                 
065700        IF MID-KDPRTYPG = 'S' OR 'F' OR 'M' OR 'K' OR 'R' OR '-'          
065800           IF  MID-TIFSGVV-FOM = ALL '+'                                  
065900              MOVE NEJ                      TO SW-INDATA-OK               
066000              MOVE MFS-ALFA-FAELT-FEL       TO                            
066100                                             MOD-KDPRTYPG-ATTR            
066200           ELSE                                                           
066300              MOVE MID-KDPRTYPG             TO WS-URV2-KDPRTYP            
066400              MOVE MFS-ALFA-FAELT-RAETT     TO                            
066500                                             MOD-KDPRTYPG-ATTR            
066600           END-IF                                                         
066700        ELSE                                                              
066800           MOVE NEJ                      TO SW-INDATA-OK                  
066900           MOVE MFS-ALFA-FAELT-FEL       TO MOD-KDPRTYPG-ATTR             
067000        END-IF                                                            
067100     END-IF                                                               
067200     .                                                                    
067300     EJECT                                                                
067400 CC-KOLLA-IDARTNR            SECTION.                                     
067500     SKIP2                                                                
067600     MOVE +1                          TO RAD-INDX                         
067700     IF  MID-IDARTNR (RAD-INDX) = ALL '+'                                 
067800        MOVE NEJ                      TO SW-INDATA-OK                     
067900        MOVE MFS-NUM-FAELT-FEL        TO                                  
068000                           MOD-IDARTNR-ATTR (RAD-INDX)                    
068100     END-IF                                                               
068200     PERFORM UNTIL RAD-INDX > 35                                          
068300           IF  MID-IDARTNR (RAD-INDX) = ALL '+'                           
068400              CONTINUE                                                    
068500           ELSE                                                           
068600              IF MID-IDARTNR  (RAD-INDX) NUMERIC                          
068700                 IF MID-IDARTNR  (RAD-INDX) > ZERO                        
068800                    MOVE MFS-NUM-FAELT-RAETT TO                           
068900                           MOD-IDARTNR-ATTR (RAD-INDX)                    
069000                 ELSE                                                     
069100                    MOVE NEJ                TO SW-INDATA-OK               
069200                    MOVE MFS-NUM-FAELT-FEL  TO                            
069300                              MOD-IDARTNR-ATTR (RAD-INDX)                 
069400                 END-IF                                                   
069500              ELSE                                                        
069600                 MOVE NEJ                TO SW-INDATA-OK                  
069700                 MOVE MFS-NUM-FAELT-FEL  TO                               
069800                           MOD-IDARTNR-ATTR (RAD-INDX)                    
069900              END-IF                                                      
070000           END-IF                                                         
070100           ADD +1                           TO RAD-INDX                   
070200     END-PERFORM                                                          
070300     .                                                                    
070400     EJECT                                                                
070500 CD-KOLLA-IDKONCNR          SECTION.                                      
070600     MOVE NEJ                            TO SW-KONCERN                    
070700     MOVE +1                             TO RAD-INDX                      
070800     PERFORM UNTIL RAD-INDX > 8                                           
070900        IF MID-IDKONCNR (RAD-INDX) = ALL '+'                              
071000           CONTINUE                                                       
071100        ELSE                                                              
071200           MOVE JA                       TO SW-KONCERN                    
071300           IF MID-IDKONCNR (RAD-INDX) NUMERIC                             
071400              MOVE MFS-NUM-FAELT-RAETT   TO                               
071500                                    MOD-IDKONCNR-ATTR (RAD-INDX)          
071600              MOVE MID-IDKONCNR (RAD-INDX) TO                             
071700                                     WS-URV2-IDKONCNR (RAD-INDX)          
071800           ELSE                                                           
071900              MOVE NEJ                   TO SW-INDATA-OK                  
072000              MOVE MFS-NUM-FAELT-FEL     TO                               
072100                                    MOD-IDKONCNR-ATTR (RAD-INDX)          
072200           END-IF                                                         
072300        END-IF                                                            
072400        ADD +1                           TO RAD-INDX                      
072500     END-PERFORM                                                          
072600     .                                                                    
072700     EJECT                                                                
072800 CE-KOLLA-KDMARK            SECTION.                                      
072900     SKIP2                                                                
073000     MOVE NEJ                            TO SW-MARKNAD                    
073100     MOVE +1                             TO RAD-INDX                      
073200     PERFORM UNTIL RAD-INDX > 8                                           
073300        IF  MID-KDMARK-FOM (RAD-INDX) = ALL '+'                           
073400        AND MID-KDMARK-TOM (RAD-INDX) = ALL '+'                           
073500           CONTINUE                                                       
073600        ELSE                                                              
073700           MOVE JA                        TO SW-MARKNAD                   
073800           IF SW-KONCERN = JA                                             
073900              MOVE JA                     TO SW-KONFLIKT                  
074000           END-IF                                                         
074100           IF  MID-KDMARK-FOM (RAD-INDX) NUMERIC                          
074200           AND MID-KDMARK-FOM (RAD-INDX) > ZERO                           
074210           AND MID-KDMARK-FOM (RAD-INDX) < 100                            
074220           AND MID-KDMARK-TOM (RAD-INDX) NUMERIC                          
074230           AND MID-KDMARK-TOM (RAD-INDX) > ZERO                           
074240           AND MID-KDMARK-TOM (RAD-INDX) < 100                            
074300              IF MID-KDMARK-TOM (RAD-INDX) NOT <                          
074400              MID-KDMARK-FOM (RAD-INDX)                                   
074500                 MOVE MFS-NUM-FAELT-RAETT TO                              
074600                           MOD-KDMARK-FOM-ATTR (RAD-INDX)                 
074700                           MOD-KDMARK-TOM-ATTR (RAD-INDX)                 
074800                 MOVE MID-KDMARK-FOM (RAD-INDX) TO                        
074900                       WS-URV2-KDMARK-BUDG-FOM (RAD-INDX)                 
075000                 MOVE MID-KDMARK-TOM (RAD-INDX) TO                        
075100                       WS-URV2-KDMARK-BUDG-TOM (RAD-INDX)                 
075200              ELSE                                                        
075300                 MOVE NEJ                 TO SW-INDATA-OK                 
075400                 MOVE MFS-NUM-FAELT-FEL   TO                              
075500                           MOD-KDMARK-FOM-ATTR (RAD-INDX)                 
075600                           MOD-KDMARK-TOM-ATTR (RAD-INDX)                 
075700              END-IF                                                      
075800           ELSE                                                           
075900              MOVE NEJ                    TO SW-INDATA-OK                 
076000              IF MID-KDMARK-FOM (RAD-INDX)   = ALL '+'                    
076100                 MOVE MFS-NUM-FAELT-FEL   TO                              
076200                                 MOD-KDMARK-TOM-ATTR (RAD-INDX)           
076300              ELSE                                                        
076400                 IF MID-KDMARK-TOM (RAD-INDX) = ALL '+'                   
076500                    MOVE MFS-NUM-FAELT-FEL TO                             
076600                                 MOD-KDMARK-FOM-ATTR (RAD-INDX)           
076700                 ELSE                                                     
076800                    MOVE MFS-NUM-FAELT-FEL TO                             
076900                                 MOD-KDMARK-FOM-ATTR (RAD-INDX)           
077000                                 MOD-KDMARK-TOM-ATTR (RAD-INDX)           
077100                 END-IF                                                   
077200              END-IF                                                      
077300           END-IF                                                         
077400        END-IF                                                            
077500        ADD +1                           TO RAD-INDX                      
077600     END-PERFORM                                                          
077700     .                                                                    
077800     EJECT                                                                
077900 CF-KOLLA-IDDISTR           SECTION.                                      
078000     SKIP2                                                                
078100     MOVE NEJ                            TO SW-DISTRIKT                   
078200     MOVE +1                             TO RAD-INDX                      
078300     PERFORM UNTIL RAD-INDX > 4                                           
078400        IF  MID-IDDISTR-FOM (RAD-INDX) = ALL '+'                          
078500        AND MID-IDDISTR-TOM (RAD-INDX) = ALL '+'                          
078600           CONTINUE                                                       
078700        ELSE                                                              
078800           MOVE JA                        TO SW-DISTRIKT                  
078900           IF SW-KONCERN = JA                                             
079000           OR SW-MARKNAD = JA                                             
079100              MOVE JA                     TO SW-KONFLIKT                  
079200           END-IF                                                         
079300           IF MID-IDDISTR-FOM  (RAD-INDX) NUMERIC                         
079400           AND MID-IDDISTR-TOM (RAD-INDX) NUMERIC                         
079500              IF MID-IDDISTR-TOM (RAD-INDX) NOT <                         
079600              MID-IDDISTR-FOM (RAD-INDX)                                  
079700                 MOVE MFS-NUM-FAELT-RAETT TO                              
079800                           MOD-IDDISTR-FOM-ATTR (RAD-INDX)                
079900                           MOD-IDDISTR-TOM-ATTR (RAD-INDX)                
080000                 MOVE MID-IDDISTR-FOM (RAD-INDX) TO                       
080100                                  WS-URV2-IDDISTR-FOM (RAD-INDX)          
080200                 MOVE MID-IDDISTR-TOM (RAD-INDX) TO                       
080300                                  WS-URV2-IDDISTR-TOM (RAD-INDX)          
080400              ELSE                                                        
080500                 MOVE NEJ                 TO SW-INDATA-OK                 
080600                 MOVE MFS-NUM-FAELT-FEL  TO                               
080700                          MOD-IDDISTR-FOM-ATTR (RAD-INDX)                 
080800                          MOD-IDDISTR-TOM-ATTR (RAD-INDX)                 
080900              END-IF                                                      
081000           ELSE                                                           
081100              MOVE NEJ                    TO SW-INDATA-OK                 
081200              IF MID-IDDISTR-FOM (RAD-INDX)   = ALL '+'                   
081300                 MOVE MFS-NUM-FAELT-FEL   TO                              
081400                                 MOD-IDDISTR-TOM-ATTR (RAD-INDX)          
081500              ELSE                                                        
081600                 IF MID-IDDISTR-TOM (RAD-INDX) = ALL '+'                  
081700                    MOVE MFS-NUM-FAELT-FEL TO                             
081800                                 MOD-IDDISTR-FOM-ATTR (RAD-INDX)          
081900                 ELSE                                                     
082000                    MOVE MFS-NUM-FAELT-FEL TO                             
082100                                 MOD-IDDISTR-FOM-ATTR (RAD-INDX)          
082200                                 MOD-IDDISTR-TOM-ATTR (RAD-INDX)          
082300                 END-IF                                                   
082400              END-IF                                                      
082500           END-IF                                                         
082600        END-IF                                                            
082700        ADD +1                           TO RAD-INDX                      
082800     END-PERFORM                                                          
082900     .                                                                    
083000     EJECT                                                                
083110 CG-KOLLA-PPV SECTION.                                                    
083200     SKIP2                                                                
083201                                                                          
083210                                                                          
083300     IF SW-KONCERN = NEJ AND SW-DISTRIKT = NEJ                            
083400        AND SW-MARKNAD = NEJ                                              
083500        CONTINUE                                                          
083600     ELSE                                                                 
083700        MOVE NEJ                      TO SW-INDATA-OK                     
083800        MOVE MFS-ALFA-FAELT-FEL       TO MOD-IDPTYP-ATTR                  
083900     END-IF                                                               
084000     .                                                                    
084100 D-KOLLA-INDATA SECTION.                                                  
084200     SKIP2                                                                
084300     MOVE JA                             TO SW-INDATA-OK                  
084400     IF MID-KDBORT = 'B' OR 'D' OR 'J'                                    
084700        MOVE MFS-ALFA-FAELT-RAETT        TO MOD-KDBORT-ATTR               
084800        PERFORM S08-KOLLA-ATT-INGET-IFYLLT                                
084900        IF  INGET-IFYLLT                                                  
085000           IF  MID-IDUSER-UT = MSG-SIGNON-USERID                          
085100              CONTINUE                                                    
085200           ELSE                                                           
085300              MOVE NEJ                      TO SW-INDATA-OK               
085400              MOVE MFS-ALFA-FAELT-FEL       TO MOD-KDBORT-ATTR            
085500*----MEDDELANDE 8 ENLIGT GAMLA MEDDELANDE "SETET".                        
085600              MOVE '000' TO MED-IDMFSINF                                  
085700              MOVE SPRAK-IX TO MED-IDSKYLT                                
085800              CALL WMEDKONV USING MED-WMEDAREA                            
085900              MOVE MED-MFSINF TO MOD-TEMFSINF                             
086000                                                                          
086100              PERFORM S09-LAES-IN-IGEN                                    
086200           END-IF                                                         
086300        ELSE                                                              
086400           MOVE NEJ                      TO SW-INDATA-OK                  
086500           MOVE MFS-ALFA-FAELT-FEL       TO MOD-KDBORT-ATTR               
086600*-----  MED-6 ENLIGT GAMLA MEDDELANDE "SETET".                            
086700           MOVE '000' TO MED-IDMFSINF                                     
086800           MOVE SPRAK-IX TO MED-IDSKYLT                                   
086900           CALL WMEDKONV USING MED-WMEDAREA                               
087000           MOVE MED-MFSINF TO MOD-TEMFSINF                                
087100                                                                          
087200           PERFORM S09-LAES-IN-IGEN                                       
087300        END-IF                                                            
087400     ELSE                                                                 
087500        MOVE NEJ                         TO SW-KONFLIKT                   
087600        PERFORM IMS-GU-FSGA01                                             
087700        IF SEGMENT-FINNS                                                  
087800           PERFORM IMS-GNP-FSGA13                                         
087900           IF SEGMENT-FINNS                                               
088000* SW-KONFLIKT HÅLLER REDA PÅ ATT ENBART ETT URVAL AV                      
088100* KONCNR KDMARK OCH IDDISTR ANGES                                         
088200* OBS! FEL-FLAGGAN TÄNDS EJ UTAN BEHANDLAS SOM FEL I HUVUDSLINGAN         
088300              MOVE WLFSGA13              TO WS-WLFSGA13                   
088400              PERFORM DA-KOLLA-VECKA                                      
088500              PERFORM DB-KOLLA-SPAR-BORT-LIST-PRIS                        
088600              PERFORM DC-KOLLA-IDARTNR                                    
088700              PERFORM DD-KOLLA-IDKONCNR                                   
088800              PERFORM DE-KOLLA-KDMARK                                     
088900              PERFORM DF-KOLLA-IDDISTR                                    
089000              IF WS-URV2-IDPTYP =  'PPV'                                  
089100                PERFORM DG-KOLLA-PPV                                      
089200              END-IF                                                      
089300              PERFORM S05-KOLLA-BORTTAG-IDARTNR                           
089400           END-IF                                                         
089500        END-IF                                                            
089600     END-IF                                                               
089700     .                                                                    
089800     EJECT                                                                
089900 DA-KOLLA-VECKA      SECTION.                                             
090000     SKIP2                                                                
090100     IF  MID-TIFSGVV-FOM = ALL '+'                                        
090200     AND MID-TIFSGVV-TOM = ALL '+'                                        
090300        CONTINUE                                                          
090400     ELSE                                                                 
090500        IF  MID-TIFSGVV-FOM = ALL '+'                                     
090600           CONTINUE                                                       
090700        ELSE                                                              
090800           MOVE MFS-NUM-FAELT-RAETT     TO                                
090900                                       MOD-TIFSGVV-FOM-ATTR               
091000           IF  MID-TIFSGVV-FOM = ZERO                                     
091100              MOVE MID-TIFSGVV-FOM      TO                                
091200                                       WS-URV2-TIFSGVV-FOM                
091300           ELSE                                                           
091400              MOVE 'AAVV  '             TO DAT-KDDATFORM                  
091500              MOVE MID-TIFSGVV-FOM      TO DAT-I-TIDATUM                  
091600              PERFORM S99-CALL-WDATKONV                                   
091700              IF DAT-KDSVAR-OK                                            
091800                 MOVE DAT-TIAA-VECKA        TO WS-AA                      
091900                 MOVE DAT-TIVV              TO WS-VV                      
091901                 MOVE WS-AAVV-N                   TO TMP1-YYWW            
091902                 MOVE WS-DAGENS-AAVV-MINUS-2-AA-N TO TMP2-YYWW            
091910                 PERFORM WY2000P3                                         
092000                 IF TMP1-YYWW >= TMP2-YYWW                                
092100                    MOVE MID-TIFSGVV-FOM      TO                          
092200                                       WS-URV2-TIFSGVV-FOM                
092300                 ELSE                                                     
092400                    MOVE NEJ                  TO SW-INDATA-OK             
092500                    MOVE MFS-NUM-FAELT-FEL    TO                          
092600                                       MOD-TIFSGVV-FOM-ATTR               
092700                 END-IF                                                   
092800              ELSE                                                        
092900                 MOVE NEJ                  TO SW-INDATA-OK                
093000                 MOVE MFS-NUM-FAELT-FEL    TO                             
093100                                    MOD-TIFSGVV-FOM-ATTR                  
093200              END-IF                                                      
093300           END-IF                                                         
093400        END-IF                                                            
093500        IF  MID-TIFSGVV-TOM = ALL '+'                                     
093600           CONTINUE                                                       
093700        ELSE                                                              
093800           MOVE MFS-NUM-FAELT-RAETT     TO                                
093900                                      MOD-TIFSGVV-TOM-ATTR                
094000           IF  MID-TIFSGVV-TOM = ZERO                                     
094100              MOVE MID-TIFSGVV-TOM      TO                                
094200                                       WS-URV2-TIFSGVV-TOM                
094300           ELSE                                                           
094400              MOVE 'AAVV  '             TO DAT-KDDATFORM                  
094500              MOVE MID-TIFSGVV-TOM      TO DAT-I-TIDATUM                  
094600              PERFORM S99-CALL-WDATKONV                                   
094700              IF DAT-KDSVAR-OK                                            
094800                 MOVE DAT-TIAA-VECKA        TO WS-AA                      
094900                 MOVE DAT-TIVV              TO WS-VV                      
094901                 MOVE WS-AAVV-N        TO TMP1-YYWW                       
094902                 MOVE WS-DAGENS-AAVV-N TO TMP2-YYWW                       
094910                 PERFORM WY2000P3                                         
095000                 IF TMP1-YYWW < TMP2-YYWW                                 
095100                    MOVE MID-TIFSGVV-TOM      TO                          
095200                                       WS-URV2-TIFSGVV-TOM                
095300                 ELSE                                                     
095400                    IF WS-URV2-IDPTYP = 'VA1' OR 'VA2'                    
095500                       AND WS-AAVV = WS-DAGENS-AAVV                       
095600                       MOVE MID-TIFSGVV-TOM      TO                       
095700                                          WS-URV2-TIFSGVV-TOM             
095800                    ELSE                                                  
095900                       MOVE NEJ                  TO SW-INDATA-OK          
096000                       MOVE MFS-NUM-FAELT-FEL    TO                       
096100                                          MOD-TIFSGVV-TOM-ATTR            
096200                    END-IF                                                
096300                 END-IF                                                   
096400              ELSE                                                        
096500                 MOVE NEJ                  TO SW-INDATA-OK                
096600                 MOVE MFS-NUM-FAELT-FEL    TO                             
096700                                    MOD-TIFSGVV-TOM-ATTR                  
096800              END-IF                                                      
096900           END-IF                                                         
097000        END-IF                                                            
097100        IF INDATA-OK                                                      
097101           MOVE WS-URV2-TIFSGVV-TOM   TO TMP1-YYWW                        
097102           MOVE WS-URV2-TIFSGVV-FOM   TO TMP2-YYWW                        
097110           PERFORM WY2000P3                                               
097200           IF  TMP1-YYWW >= TMP2-YYWW                                     
097300              IF WS-URV2-TIFSGVV-FOM = ZERO                               
097400                 IF WS-URV2-TIFSGVV-TOM = ZERO                            
097500                    CONTINUE                                              
097600                 ELSE                                                     
097700                    MOVE NEJ                   TO SW-INDATA-OK            
097800                    PERFORM DAE-SAETT-MFS-FEL                             
097900                 END-IF                                                   
098000              END-IF                                                      
098100           ELSE                                                           
098200              MOVE NEJ                   TO SW-INDATA-OK                  
098300              PERFORM DAE-SAETT-MFS-FEL                                   
098400           END-IF                                                         
098500        END-IF                                                            
098600     END-IF                                                               
098700     .                                                                    
098800     EJECT                                                                
098900 DAE-SAETT-MFS-FEL    SECTION.                                            
099000     SKIP2                                                                
099100     IF MID-TIFSGVV-FOM = ALL '+'                                         
099200        CONTINUE                                                          
099300     ELSE                                                                 
099400        MOVE MFS-NUM-FAELT-FEL    TO                                      
099500                           MOD-TIFSGVV-FOM-ATTR                           
099600     END-IF                                                               
099700     IF MID-TIFSGVV-TOM = ALL '+'                                         
099800        CONTINUE                                                          
099900     ELSE                                                                 
100000        MOVE MFS-NUM-FAELT-FEL    TO                                      
100100                           MOD-TIFSGVV-TOM-ATTR                           
100200     END-IF                                                               
100300     .                                                                    
100400     EJECT                                                                
100500 DB-KOLLA-SPAR-BORT-LIST-PRIS   SECTION.                                  
100600     SKIP2                                                                
100700     IF MID-IDFSGURV = ALL '+'                                            
100800        CONTINUE                                                          
100900     ELSE                                                                 
101000        MOVE MFS-ALFA-FAELT-RAETT        TO MOD-IDFSGURV-ATTR             
101100     END-IF                                                               
101200     IF MID-KDBORT   = ALL '+'                                            
101300        CONTINUE                                                          
101400     ELSE                                                                 
101500        MOVE NEJ                         TO SW-INDATA-OK                  
101600        MOVE MFS-ALFA-FAELT-FEL          TO MOD-KDBORT-ATTR               
101700     END-IF                                                               
101800     IF MID-KDCMD    = ALL '+'                                            
101900        CONTINUE                                                          
102000     ELSE                                                                 
102100        IF MID-KDCMD    = ALL 'B' OR 'D'                                  
102200           MOVE MFS-ALFA-FAELT-RAETT     TO MOD-KDCMD-ATTR                
102300        ELSE                                                              
102400           MOVE NEJ                      TO SW-INDATA-OK                  
102500           MOVE MFS-ALFA-FAELT-FEL       TO MOD-KDCMD-ATTR                
102600        END-IF                                                            
102700     END-IF                                                               
102710                                                                          
102800     IF MID-IDPTYP = ALL '+'                                              
102900     AND MID-TIFSGVV-FOM = ALL '+'                                        
103000     AND MID-TIFSGVV-TOM = ALL '+'                                        
103100        CONTINUE                                                          
103200     ELSE                                                                 
103300        IF MID-IDPTYP     = ALL '+'                                       
103400           CONTINUE                                                       
103500        ELSE                                                              
103600           MOVE MFS-ALFA-FAELT-RAETT        TO MOD-IDPTYP-ATTR            
103700           MOVE MID-IDPTYP                  TO WS-URV2-IDPTYP             
103800        END-IF                                                            
103900        IF WS-URV2-IDPTYP = 'VA1' OR 'VA2'                                
104000           IF WS-URV2-TIFSGVV-FOM > ZERO                                  
104100              CONTINUE                                                    
104200           ELSE                                                           
104300              MOVE NEJ                      TO SW-INDATA-OK               
104400              PERFORM DBA-BEHANDLA-FEL-IDPTYP                             
104500           END-IF                                                         
104600        ELSE                                                              
104610                                                                          
104611                                                                          
104620                                                                          
104710           IF WS-URV2-IDPTYP = 'P1' OR 'A1' OR 'A2' OR 'PPV'              
104800              IF WS-URV2-TIFSGVV-FOM = ZERO                               
104900                 CONTINUE                                                 
105000              ELSE                                                        
105100                 MOVE NEJ                   TO SW-INDATA-OK               
105200                 PERFORM DBA-BEHANDLA-FEL-IDPTYP                          
105300              END-IF                                                      
105400           ELSE                                                           
105500              MOVE NEJ                      TO SW-INDATA-OK               
105600              MOVE MFS-ALFA-FAELT-FEL       TO MOD-IDPTYP-ATTR            
105700           END-IF                                                         
105800        END-IF                                                            
105900     END-IF                                                               
106000     IF MID-KDPRTYPG = ALL '+'                                            
106100        CONTINUE                                                          
106200     ELSE                                                                 
106300        MOVE MFS-ALFA-FAELT-RAETT           TO  MOD-KDPRTYPG-ATTR         
106400        MOVE MID-KDPRTYPG                   TO WS-URV2-KDPRTYP            
106500     END-IF                                                               
106600     IF WS-URV2-KDPRTYP =                                                 
106700                       'S' OR 'F' OR 'M' OR 'K' OR 'R' OR '-'             
106800        IF WS-URV2-TIFSGVV-FOM > ZERO                                     
106900           CONTINUE                                                       
107000        ELSE                                                              
107100           MOVE NEJ                      TO SW-INDATA-OK                  
107200           PERFORM DBB-BEHANDLA-FEL-KDPRTYPG                              
107300        END-IF                                                            
107400     ELSE                                                                 
107500        IF WS-URV2-KDPRTYP = SPACE                                        
107600           CONTINUE                                                       
107700        ELSE                                                              
107800           MOVE NEJ                      TO SW-INDATA-OK                  
107900           MOVE MFS-ALFA-FAELT-FEL       TO MOD-KDPRTYPG-ATTR             
108000        END-IF                                                            
108100     END-IF                                                               
108200     .                                                                    
108300     EJECT                                                                
108400 DBA-BEHANDLA-FEL-IDPTYP SECTION.                                         
108500     SKIP2                                                                
108600     IF MID-IDPTYP = ALL '+'                                              
108700        IF MID-TIFSGVV-FOM = ALL '+'                                      
108800           CONTINUE                                                       
108900        ELSE                                                              
109000           MOVE MFS-NUM-FAELT-FEL    TO                                   
109100                             MOD-TIFSGVV-FOM-ATTR                         
109200        END-IF                                                            
109300        IF MID-TIFSGVV-TOM = ALL '+'                                      
109400           CONTINUE                                                       
109500        ELSE                                                              
109600            MOVE MFS-NUM-FAELT-FEL    TO                                  
109700                              MOD-TIFSGVV-TOM-ATTR                        
109800        END-IF                                                            
109900     ELSE                                                                 
110000        MOVE MFS-ALFA-FAELT-FEL  TO                                       
110100                          MOD-IDPTYP-ATTR                                 
110200     END-IF                                                               
110300     .                                                                    
110400     EJECT                                                                
110500 DBB-BEHANDLA-FEL-KDPRTYPG    SECTION.                                    
110600     SKIP2                                                                
110700     IF MID-KDPRTYPG = ALL '+'                                            
110800        IF MID-TIFSGVV-FOM = ALL '+'                                      
110900           CONTINUE                                                       
111000        ELSE                                                              
111100           MOVE MFS-NUM-FAELT-FEL    TO                                   
111200                             MOD-TIFSGVV-FOM-ATTR                         
111300        END-IF                                                            
111400        IF MID-TIFSGVV-TOM = ALL '+'                                      
111500           CONTINUE                                                       
111600        ELSE                                                              
111700            MOVE MFS-NUM-FAELT-FEL    TO                                  
111800                              MOD-TIFSGVV-TOM-ATTR                        
111900        END-IF                                                            
112000     ELSE                                                                 
112100        MOVE MFS-ALFA-FAELT-FEL  TO                                       
112200                          MOD-KDPRTYPG-ATTR                               
112300     END-IF                                                               
112400     .                                                                    
112500     EJECT                                                                
112600 DC-KOLLA-IDARTNR            SECTION.                                     
112700     SKIP2                                                                
112800     MOVE +1                          TO RAD-INDX                         
112900     PERFORM UNTIL RAD-INDX > 35                                          
113000        IF  MID-IDARTNR (RAD-INDX) = ALL '+'                              
113100           CONTINUE                                                       
113200        ELSE                                                              
113300           IF MID-IDARTNR  (RAD-INDX) NUMERIC                             
113400              IF MID-IDARTNR  (RAD-INDX) > ZERO                           
113500                 MOVE MFS-NUM-FAELT-RAETT TO                              
113600                        MOD-IDARTNR-ATTR (RAD-INDX)                       
113700              ELSE                                                        
113800                 MOVE NEJ                TO SW-INDATA-OK                  
113900                 MOVE MFS-NUM-FAELT-FEL  TO                               
114000                           MOD-IDARTNR-ATTR (RAD-INDX)                    
114100              END-IF                                                      
114200           ELSE                                                           
114300              MOVE NEJ                TO SW-INDATA-OK                     
114400              MOVE MFS-NUM-FAELT-FEL  TO                                  
114500                        MOD-IDARTNR-ATTR (RAD-INDX)                       
114600           END-IF                                                         
114700        END-IF                                                            
114800        ADD +1                           TO RAD-INDX                      
114900     END-PERFORM                                                          
115000     .                                                                    
115100     EJECT                                                                
115200 DD-KOLLA-IDKONCNR          SECTION.                                      
115300     MOVE NEJ                            TO SW-KONCERN                    
115400     MOVE +1                             TO RAD-INDX                      
115500     PERFORM UNTIL RAD-INDX > 8                                           
115600        IF MID-IDKONCNR (RAD-INDX) = ALL '+'                              
115700        AND WS-URV2-IDKONCNR (RAD-INDX) = ZERO                            
115800           CONTINUE                                                       
115900        ELSE                                                              
116000           IF MID-IDKONCNR (RAD-INDX) = ALL '+'                           
116100              MOVE JA                    TO SW-KONCERN                    
116200           ELSE                                                           
116300              IF MID-IDKONCNR (RAD-INDX) = ZERO                           
116400                 MOVE MFS-NUM-FAELT-RAETT   TO                            
116500                                    MOD-IDKONCNR-ATTR (RAD-INDX)          
116600                 MOVE MID-IDKONCNR (RAD-INDX) TO                          
116700                                     WS-URV2-IDKONCNR (RAD-INDX)          
116800              ELSE                                                        
116900                 MOVE JA                       TO SW-KONCERN              
117000                 IF MID-IDKONCNR (RAD-INDX) NUMERIC                       
117100                    MOVE MFS-NUM-FAELT-RAETT   TO                         
117200                                    MOD-IDKONCNR-ATTR (RAD-INDX)          
117300                    MOVE MID-IDKONCNR (RAD-INDX) TO                       
117400                                     WS-URV2-IDKONCNR (RAD-INDX)          
117500                 ELSE                                                     
117600                    MOVE NEJ                   TO SW-INDATA-OK            
117700                    MOVE MFS-NUM-FAELT-FEL     TO                         
117800                                    MOD-IDKONCNR-ATTR (RAD-INDX)          
117900                 END-IF                                                   
118000              END-IF                                                      
118100           END-IF                                                         
118200        END-IF                                                            
118300        ADD +1                           TO RAD-INDX                      
118400     END-PERFORM                                                          
118500     .                                                                    
118600     EJECT                                                                
118700 DE-KOLLA-KDMARK            SECTION.                                      
118800     SKIP2                                                                
118900     MOVE NEJ                            TO SW-MARKNAD                    
119000     MOVE +1                             TO RAD-INDX                      
119100     PERFORM UNTIL RAD-INDX > 8                                           
119200        IF  MID-KDMARK-FOM (RAD-INDX) = ALL '+'                           
119300        AND MID-KDMARK-TOM (RAD-INDX) = ALL '+'                           
119400        AND WS-URV2-KDMARK-BUDG-FOM (RAD-INDX) = ZERO                     
119500           CONTINUE                                                       
119600        ELSE                                                              
119700           IF  MID-KDMARK-FOM (RAD-INDX) = ALL '+'                        
119800           AND MID-KDMARK-TOM (RAD-INDX) = ALL '+'                        
119900              MOVE JA                     TO SW-MARKNAD                   
120000              IF SW-KONCERN = JA                                          
120100                 MOVE JA                  TO SW-KONFLIKT                  
120200              END-IF                                                      
120300           ELSE                                                           
120400              IF  MID-KDMARK-FOM (RAD-INDX) = ZERO                        
120500              AND MID-KDMARK-TOM (RAD-INDX) = ZERO                        
120600                 MOVE MFS-NUM-FAELT-RAETT TO                              
120700                                 MOD-KDMARK-FOM-ATTR (RAD-INDX)           
120800                                 MOD-KDMARK-TOM-ATTR (RAD-INDX)           
120900                 MOVE ZERO                      TO                        
121000                             WS-URV2-KDMARK-BUDG-FOM (RAD-INDX)           
121100                             WS-URV2-KDMARK-BUDG-TOM (RAD-INDX)           
121200              ELSE                                                        
121300                 MOVE JA                     TO SW-MARKNAD                
121400                 IF SW-KONCERN = JA                                       
121500                    MOVE JA                  TO SW-KONFLIKT               
121600                 END-IF                                                   
121700                 IF  MID-KDMARK-FOM (RAD-INDX) = ALL '+'                  
121800                    CONTINUE                                              
121900                 ELSE                                                     
122000                    IF  MID-KDMARK-FOM (RAD-INDX) NUMERIC                 
122100                       MOVE MFS-NUM-FAELT-RAETT TO                        
122200                                 MOD-KDMARK-FOM-ATTR (RAD-INDX)           
122300                       MOVE MID-KDMARK-FOM (RAD-INDX) TO                  
122400                             WS-URV2-KDMARK-BUDG-FOM (RAD-INDX)           
122500                    ELSE                                                  
122600                       MOVE NEJ                 TO SW-INDATA-OK           
122700                       MOVE MFS-NUM-FAELT-FEL   TO                        
122800                                 MOD-KDMARK-FOM-ATTR (RAD-INDX)           
122900                    END-IF                                                
123000                 END-IF                                                   
123100                 IF  MID-KDMARK-TOM (RAD-INDX) = ALL '+'                  
123200                    CONTINUE                                              
123300                 ELSE                                                     
123400                    IF  MID-KDMARK-TOM (RAD-INDX) NUMERIC                 
123500                       MOVE MFS-NUM-FAELT-RAETT TO                        
123600                                 MOD-KDMARK-TOM-ATTR (RAD-INDX)           
123700                       MOVE MID-KDMARK-TOM (RAD-INDX) TO                  
123800                             WS-URV2-KDMARK-BUDG-TOM (RAD-INDX)           
123900                    ELSE                                                  
124000                       MOVE NEJ                 TO SW-INDATA-OK           
124100                       MOVE MFS-NUM-FAELT-FEL   TO                        
124200                                 MOD-KDMARK-TOM-ATTR (RAD-INDX)           
124300                    END-IF                                                
124400                 END-IF                                                   
124500                 IF WS-URV2-KDMARK-BUDG-TOM (RAD-INDX) NOT <              
124600                 WS-URV2-KDMARK-BUDG-FOM (RAD-INDX)                       
124700                    IF WS-URV2-KDMARK-BUDG-FOM (RAD-INDX) = ZERO          
124800                       IF WS-URV2-KDMARK-BUDG-TOM (RAD-INDX)              
124900                                                     = ZERO               
125000                          CONTINUE                                        
125100                       ELSE                                               
125200                          MOVE NEJ           TO SW-INDATA-OK              
125300                          PERFORM DEA-FEL-KDMARK                          
125400                       END-IF                                             
125500                    END-IF                                                
125600                 ELSE                                                     
125700                    MOVE NEJ                 TO SW-INDATA-OK              
125800                    PERFORM DEA-FEL-KDMARK                                
125900                 END-IF                                                   
126000              END-IF                                                      
126100           END-IF                                                         
126200        END-IF                                                            
126300        ADD +1                           TO RAD-INDX                      
126400     END-PERFORM                                                          
126500     .                                                                    
126600     EJECT                                                                
126700 DEA-FEL-KDMARK        SECTION.                                           
126800     SKIP2                                                                
126900     IF  MID-KDMARK-FOM (RAD-INDX) = ALL '+'                              
127000        CONTINUE                                                          
127100     ELSE                                                                 
127200        MOVE MFS-NUM-FAELT-FEL      TO                                    
127300                     MOD-KDMARK-FOM-ATTR (RAD-INDX)                       
127400     END-IF                                                               
127500     IF  MID-KDMARK-TOM (RAD-INDX) = ALL '+'                              
127600        CONTINUE                                                          
127700     ELSE                                                                 
127800        MOVE MFS-NUM-FAELT-FEL      TO                                    
127900                     MOD-KDMARK-TOM-ATTR (RAD-INDX)                       
128000     END-IF                                                               
128100     .                                                                    
128200     EJECT                                                                
128300 DF-KOLLA-IDDISTR           SECTION.                                      
128400     SKIP2                                                                
128500     MOVE NEJ                            TO SW-DISTRIKT                   
128600     MOVE +1                             TO RAD-INDX                      
128700     PERFORM UNTIL RAD-INDX > 4                                           
128800        IF  MID-IDDISTR-FOM (RAD-INDX) = ALL '+'                          
128900        AND MID-IDDISTR-TOM (RAD-INDX) = ALL '+'                          
129000        AND WS-URV2-IDDISTR-FOM (RAD-INDX) = ZERO                         
129100           CONTINUE                                                       
129200        ELSE                                                              
129300           IF  MID-IDDISTR-FOM (RAD-INDX) = ALL '+'                       
129400           AND MID-IDDISTR-TOM (RAD-INDX) = ALL '+'                       
129500              MOVE JA                        TO SW-DISTRIKT               
129600              IF SW-KONCERN = JA                                          
129700              OR SW-MARKNAD = JA                                          
129800                 MOVE JA                     TO SW-KONFLIKT               
129900              END-IF                                                      
130000           ELSE                                                           
130100              IF MID-IDDISTR-FOM  (RAD-INDX) = ZERO                       
130200              AND MID-IDDISTR-TOM (RAD-INDX) = ZERO                       
130300                 MOVE MFS-NUM-FAELT-RAETT TO                              
130400                                 MOD-IDDISTR-FOM-ATTR (RAD-INDX)          
130500                                 MOD-IDDISTR-TOM-ATTR (RAD-INDX)          
130600                 MOVE ZERO                       TO                       
130700                                  WS-URV2-IDDISTR-FOM (RAD-INDX)          
130800                                  WS-URV2-IDDISTR-TOM (RAD-INDX)          
130900              ELSE                                                        
131000                 MOVE JA                        TO SW-DISTRIKT            
131100                 IF SW-KONCERN = JA                                       
131200                 OR SW-MARKNAD = JA                                       
131300                    MOVE JA                     TO SW-KONFLIKT            
131400                 END-IF                                                   
131500                 IF MID-IDDISTR-FOM  (RAD-INDX) = ALL '+'                 
131600                    CONTINUE                                              
131700                 ELSE                                                     
131800                    IF MID-IDDISTR-FOM  (RAD-INDX) NUMERIC                
131900                       MOVE MFS-NUM-FAELT-RAETT TO                        
132000                                 MOD-IDDISTR-FOM-ATTR (RAD-INDX)          
132100                       MOVE MID-IDDISTR-FOM (RAD-INDX) TO                 
132200                                  WS-URV2-IDDISTR-FOM (RAD-INDX)          
132300                    ELSE                                                  
132400                       MOVE NEJ                 TO SW-INDATA-OK           
132500                       MOVE MFS-NUM-FAELT-FEL  TO                         
132600                          MOD-IDDISTR-FOM-ATTR (RAD-INDX)                 
132700                    END-IF                                                
132800                 END-IF                                                   
132900                 IF MID-IDDISTR-TOM  (RAD-INDX) = ALL '+'                 
133000                    CONTINUE                                              
133100                 ELSE                                                     
133200                    IF MID-IDDISTR-TOM  (RAD-INDX) NUMERIC                
133300                       MOVE MFS-NUM-FAELT-RAETT TO                        
133400                                 MOD-IDDISTR-TOM-ATTR (RAD-INDX)          
133500                       MOVE MID-IDDISTR-TOM (RAD-INDX) TO                 
133600                                  WS-URV2-IDDISTR-TOM (RAD-INDX)          
133700                    ELSE                                                  
133800                       MOVE NEJ                 TO SW-INDATA-OK           
133900                       MOVE MFS-NUM-FAELT-FEL  TO                         
134000                          MOD-IDDISTR-TOM-ATTR (RAD-INDX)                 
134100                    END-IF                                                
134200                 END-IF                                                   
134300                 IF WS-URV2-IDDISTR-TOM (RAD-INDX) NOT <                  
134400                 WS-URV2-IDDISTR-FOM (RAD-INDX)                           
134500                    CONTINUE                                              
134600                 ELSE                                                     
134700                    MOVE NEJ                 TO SW-INDATA-OK              
134800                    IF MID-IDDISTR-FOM  (RAD-INDX) = ALL '+'              
134900                       CONTINUE                                           
135000                    ELSE                                                  
135100                       MOVE MFS-NUM-FAELT-FEL  TO                         
135200                               MOD-IDDISTR-FOM-ATTR (RAD-INDX)            
135300                    END-IF                                                
135400                    IF MID-IDDISTR-TOM  (RAD-INDX) = ALL '+'              
135500                       CONTINUE                                           
135600                    ELSE                                                  
135700                       MOVE MFS-NUM-FAELT-FEL  TO                         
135800                               MOD-IDDISTR-TOM-ATTR (RAD-INDX)            
135900                    END-IF                                                
136000                 END-IF                                                   
136100              END-IF                                                      
136200           END-IF                                                         
136300        END-IF                                                            
136400        ADD +1                           TO RAD-INDX                      
136500     END-PERFORM                                                          
136600     .                                                                    
136700     EJECT                                                                
136810 DG-KOLLA-PPV SECTION.                                                    
136900     SKIP2                                                                
137000     IF SW-KONCERN = NEJ AND SW-DISTRIKT = NEJ                            
137100        AND SW-MARKNAD = NEJ                                              
137200        CONTINUE                                                          
137300     ELSE                                                                 
137400        MOVE NEJ                      TO SW-INDATA-OK                     
137500        MOVE MFS-ALFA-FAELT-FEL       TO MOD-IDPTYP-ATTR                  
137600     END-IF                                                               
137700     .                                                                    
137800 E-UPPDATERA-VISA-SIDAN SECTION.                                          
137900     SKIP1                                                                
138000     IF MID-KDBORT = 'B' OR 'D' OR 'J'                                    
138100        PERFORM EB-TA-BORT-POST                                           
138200                                                                          
138300        MOVE '101' TO MED-IDMFSINF                                        
138400        MOVE SPRAK-IX TO MED-IDSKYLT                                      
138500        CALL WMEDKONV USING MED-WMEDAREA                                  
138600        MOVE MED-MFSINF TO MOD-TEMFSINF                                   
138700                                                                          
138800        PERFORM S01-RENSA-HELA-SIDAN                                      
138900     ELSE                                                                 
139000         IF KONFLIKT                                                      
139100            PERFORM S04-SAETT-NUMFAELT-FEL                                
139200            PERFORM S06-MFS-ROER-EJ-FAELT                                 
139300                                                                          
139400            MOVE '002' TO MED-IDMFSFEL                                    
139500            MOVE SPRAK-IX TO MED-IDSKYLT                                  
139600            CALL WMEDKONV USING MED-WMEDAREA                              
139700            MOVE MED-MFSFEL TO MOD-TEMFSFEL                               
139800                                                                          
139900            MOVE '102' TO MED-IDMFSINF                                    
140000            MOVE SPRAK-IX TO MED-IDSKYLT                                  
140100            CALL WMEDKONV USING MED-WMEDAREA                              
140200            MOVE MED-MFSINF TO MOD-TEMFSINF                               
140300                                                                          
140400         ELSE                                                             
141300            MOVE '101' TO MED-IDMFSINF                                    
141400            MOVE SPRAK-IX TO MED-IDSKYLT                                  
141500            CALL WMEDKONV USING MED-WMEDAREA                              
141600            MOVE MED-MFSINF TO MOD-TEMFSINF                               
141800            PERFORM EA-LAGG-TILL-VISA-SIDAN                               
142000         END-IF                                                           
142100     END-IF                                                               
142200     .                                                                    
142300     EJECT                                                                
142400 EA-LAGG-TILL-VISA-SIDAN SECTION.                                         
142500     SKIP2                                                                
142600     IF W-DAREGDAT > ZERO                                                 
142700        MOVE W-IDUSER                TO W-IDUSER-SPAR                     
142800        MOVE W-DAREGDAT              TO W-DAREGDAT-SPAR                   
142900        MOVE W-TIREGTID              TO W-TIREGTID-SPAR                   
143000        MOVE ZERO                    TO W-IDARTNR-SPAR                    
143100        IF MID-IDFSGURV = ALL '+'                                         
143200           CONTINUE                                                       
143300        ELSE                                                              
143400           MOVE MID-IDFSGURV         TO WS-IDFSGURV                       
143500        END-IF                                                            
143600        IF MID-IDUSER-IN = ALL '+'                                        
143700           IF W-IDUSER = MSG-SIGNON-USERID                                
143800              CONTINUE                                                    
143900           ELSE                                                           
144000              IF MID-IDFSGURV = ALL '+'                                   
144100                 MOVE SPACE          TO WS-IDFSGURV                       
144200              END-IF                                                      
144300           END-IF                                                         
144400        ELSE                                                              
144500           MOVE MID-IDUSER-IN        TO W-IDUSER                          
144600                                        MOD-IDUSER-UT                     
144700        END-IF                                                            
144800        MOVE WS-IDFSGURV             TO USER-IDFSGURV                     
144900                                        MOD-IDFSGURV                      
145000                                        MOD-IDFSGURV-UT                   
145100        PERFORM EAA-SKAPA-NYTT-01-13-SEGM                                 
145200        PERFORM EAB-ISRTA-INMATADE-ARTIKLAR                               
145300        PERFORM IMS-GU-FSGA01-SPAR                                        
145400        IF SEGMENT-FINNS                                                  
145500           PERFORM IMS-GNP-FSGA14-SPAR                                    
145600           IF SEGMENT-FINNS                                               
145700              PERFORM EAC-KOPIERA-ARTIKLAR                                
145800              IF MID-KDCMD = 'B' OR 'D'                                   
145900                 PERFORM EAD-TA-BORT-IDARTNR                              
146000              END-IF                                                      
146100              IF MID-IDARTNR (35)  = ALL '+'                              
146200                 PERFORM IMS-GU-FSGA01                                    
146300                 IF SEGMENT-FINNS                                         
146400                    PERFORM S03-VISA-ARTNR-FSGA14                         
146500                 END-IF                                                   
146600              ELSE                                                        
146700                 PERFORM EAF-RENSA-ARTIKLAR                               
146800              END-IF                                                      
146900           END-IF                                                         
147000           IF  MID-IDFSGURV  = ALL '+'                                    
147100           AND MID-IDUSER-IN = ALL '+'                                    
147200           AND W-IDUSER-SPAR = MSG-SIGNON-USERID                          
147300              PERFORM IMS-GHU-FSGA01-SPAR                                 
147400              IF SEGMENT-FINNS                                            
147500                 PERFORM IMS-DLET-FSGA-SPAR                               
147600              END-IF                                                      
147700           END-IF                                                         
147800        END-IF                                                            
147900     ELSE                                                                 
148000        IF MID-IDFSGURV = ALL '+'                                         
148100           MOVE SPACE                TO USER-IDFSGURV                     
148200                                        MOD-IDFSGURV                      
148300                                        MOD-IDFSGURV-UT                   
148400        ELSE                                                              
148500           MOVE MID-IDFSGURV         TO USER-IDFSGURV                     
148600                                        MOD-IDFSGURV                      
148700                                        MOD-IDFSGURV-UT                   
148800        END-IF                                                            
148900        PERFORM EAA-SKAPA-NYTT-01-13-SEGM                                 
149000        PERFORM EAB-ISRTA-INMATADE-ARTIKLAR                               
149100        IF MID-IDARTNR (35)  = ALL '+'                                    
149200           PERFORM IMS-GU-FSGA01                                          
149300           IF SEGMENT-FINNS                                               
149400              PERFORM S03-VISA-ARTNR-FSGA14                               
149500           END-IF                                                         
149600        ELSE                                                              
149700           PERFORM EAF-RENSA-ARTIKLAR                                     
149800        END-IF                                                            
149900     END-IF                                                               
150000     PERFORM EAE-FORMATETS-ATTR                                           
150100     .                                                                    
150200     EJECT                                                                
150300 EAA-SKAPA-NYTT-01-13-SEGM SECTION.                                       
150400     SKIP2                                                                
150500     MOVE FUNCTION CURRENT-DATE(1:8) TO  W-DAREGDAT                       
150600     ACCEPT WS-TIREGTID  FROM TIME                                        
150700     MOVE WS-TIREGTID-HHMMSS   TO W-TIREGTID                              
150800     MOVE W-IDUSER             TO USER-IDUSER                             
150900     MOVE W-DAREGDAT           TO USER-DAREGDAT                           
151000     MOVE W-DAREGDAT           TO MOD-DAREGDAT-DOLD                       
151100     MOVE W-TIREGTID           TO USER-TIREGTID                           
151200                                  MOD-TIREGTID-DOLD                       
151300     MOVE '3203'               TO USER-IDTRANS                            
151400     IF USER-IDFSGURV = 'STOPPAD '                                        
151500        MOVE 'N'               TO USER-FLLISTA                            
151600     ELSE                                                                 
151700        MOVE 'J'               TO USER-FLLISTA                            
151800     END-IF                                                               
151900     PERFORM IMS-ISRT-FSGA01                                              
152000     MOVE '1'                  TO WS-URV2-KDSEGKEY                        
152100     MOVE WS-WLFSGA13          TO WLFSGA13                                
152200     PERFORM S02-VISA-FSGA13                                              
152300     PERFORM IMS-ISRT-FSGA13                                              
152400     .                                                                    
152500     EJECT                                                                
152600 EAB-ISRTA-INMATADE-ARTIKLAR   SECTION.                                   
152700     SKIP2                                                                
152800     MOVE +1                   TO RAD-INDX                                
152900     PERFORM UNTIL RAD-INDX > 35                                          
153000        IF MID-IDARTNR (RAD-INDX) = ALL '+'                               
153100           CONTINUE                                                       
153200        ELSE                                                              
153300           MOVE MID-IDARTNR(RAD-INDX)    TO ART-IDARTNR                   
153400           PERFORM IMS-ISRT-FSGA14                                        
153500        END-IF                                                            
153600        ADD +1                 TO RAD-INDX                                
153700     END-PERFORM                                                          
153800     .                                                                    
153900     EJECT                                                                
154000 EAC-KOPIERA-ARTIKLAR          SECTION.                                   
154100     SKIP2                                                                
154200     PERFORM UNTIL SEGMENT-SAKNAS                                         
154300        MOVE SPAR-ART-IDARTNR       TO ART-IDARTNR                        
154400        PERFORM IMS-ISRT-FSGA14                                           
154500        PERFORM IMS-GNP-FSGA14-SPAR                                       
154600     END-PERFORM                                                          
154700     .                                                                    
154800     EJECT                                                                
154900 EAD-TA-BORT-IDARTNR   SECTION.                                           
155000     SKIP1                                                                
155100     MOVE MFS-RENSA-FAELT           TO MOD-KDCMD                          
155200     MOVE +1                        TO RAD-INDX                           
155300     PERFORM UNTIL RAD-INDX > 7                                           
155400        IF MID-IDARTNR-IN (RAD-INDX) = ALL '+'                            
155500           CONTINUE                                                       
155600        ELSE                                                              
155700           MOVE MID-IDARTNR-IN (RAD-INDX) TO W-IDARTNR                    
155800           PERFORM IMS-GHU-FSGA14                                         
155900           IF SEGMENT-FINNS                                               
156000              PERFORM IMS-DLET-FSGA                                       
156100           END-IF                                                         
156200           MOVE MFS-RENSA-FAELT     TO MOD-IDARTNR-IN (RAD-INDX)          
156300        END-IF                                                            
156400        ADD +1                      TO RAD-INDX                           
156500     END-PERFORM                                                          
156600     .                                                                    
156700     EJECT                                                                
156800 EAE-FORMATETS-ATTR  SECTION.                                             
156900     SKIP2                                                                
157000     MOVE MFS-FORMATETS-ATTR      TO MOD-TIFSGVV-FOM-ATTR                 
157100                                     MOD-IDFSGURV-ATTR                    
157200                                     MOD-KDBORT-ATTR                      
157300                                     MOD-IDPTYP-ATTR                      
157400                                     MOD-TIFSGVV-TOM-ATTR                 
157500                                     MOD-KDPRTYPG-ATTR                    
157600                                     MOD-KDCMD-ATTR                       
157700     MOVE +1                      TO RAD-INDX                             
157800     PERFORM UNTIL RAD-INDX > 35                                          
157900        IF RAD-INDX > 8                                                   
158000           MOVE MFS-FORMATETS-ATTR   TO MOD-IDARTNR-ATTR(RAD-INDX)        
158100        ELSE                                                              
158200           IF RAD-INDX > 7                                                
158300              MOVE MFS-FORMATETS-ATTR TO                                  
158400                                     MOD-IDARTNR-ATTR   (RAD-INDX)        
158500                                     MOD-IDKONCNR-ATTR  (RAD-INDX)        
158600                                     MOD-KDMARK-FOM-ATTR(RAD-INDX)        
158700                                     MOD-KDMARK-TOM-ATTR(RAD-INDX)        
158800           ELSE                                                           
158900              IF RAD-INDX > 4                                             
159000                 MOVE MFS-FORMATETS-ATTR TO                               
159100                                     MOD-IDARTNR-ATTR   (RAD-INDX)        
159200                                     MOD-IDKONCNR-ATTR  (RAD-INDX)        
159300                                     MOD-IDARTNR-IN-ATTR(RAD-INDX)        
159400                                     MOD-KDMARK-FOM-ATTR(RAD-INDX)        
159500                                     MOD-KDMARK-TOM-ATTR(RAD-INDX)        
159600              ELSE                                                        
159700                 MOVE MFS-FORMATETS-ATTR TO                               
159800                                     MOD-IDARTNR-ATTR   (RAD-INDX)        
159900                                     MOD-IDKONCNR-ATTR  (RAD-INDX)        
160000                                     MOD-IDARTNR-IN-ATTR(RAD-INDX)        
160100                                    MOD-KDMARK-FOM-ATTR (RAD-INDX)        
160200                                    MOD-KDMARK-TOM-ATTR (RAD-INDX)        
160300                                    MOD-IDDISTR-FOM-ATTR(RAD-INDX)        
160400                                    MOD-IDDISTR-TOM-ATTR(RAD-INDX)        
160500              END-IF                                                      
160600           END-IF                                                         
160700        END-IF                                                            
160800        ADD +1                       TO RAD-INDX                          
160900     END-PERFORM                                                          
161000     .                                                                    
161100     EJECT                                                                
161200 EAF-RENSA-ARTIKLAR  SECTION.                                             
161300     SKIP2                                                                
161400     MOVE MFS-ROER-EJ-FAELT       TO MOD-IDARTNR-LO                       
161500                                     MOD-IDARTNR-HI                       
161600     MOVE +1                      TO RAD-INDX                             
161700     PERFORM UNTIL RAD-INDX > 35                                          
161800        MOVE MFS-RENSA-FAELT         TO MOD-IDARTNR(RAD-INDX)             
161900        ADD +1                       TO RAD-INDX                          
162000     END-PERFORM                                                          
162100     .                                                                    
162200     EJECT                                                                
162300 EB-TA-BORT-POST   SECTION.                                               
162400     SKIP1                                                                
162500     PERFORM IMS-GHU-FSGA01                                               
162600     IF SEGMENT-FINNS                                                     
162700        PERFORM IMS-DLET-FSGA                                             
162800     END-IF                                                               
162900     .                                                                    
163000     EJECT                                                                
163100 F-LAES-FSGB-FSGA              SECTION.                                   
163200     SKIP1                                                                
163300     PERFORM IMS-GN-FSGB01                                                
163400     IF SEGMENT-FINNS                                                     
163500        MOVE SEQA-DAREGDAT    TO W-DAREGDAT                               
163600        MOVE SEQA-TIREGTID    TO W-TIREGTID                               
163700        PERFORM IMS-GU-FSGA01                                             
163800        IF SEGMENT-FINNS                                                  
163900           PERFORM S07-VISA-SIDAN                                         
164000        END-IF                                                            
164100     ELSE                                                                 
164200        MOVE '005' TO MED-IDMFSFEL                                        
164300        MOVE SPRAK-IX TO MED-IDSKYLT                                      
164400        CALL WMEDKONV USING MED-WMEDAREA                                  
164500        MOVE MED-MFSFEL TO MOD-TEMFSFEL                                   
164600                                                                          
164700        PERFORM S01-RENSA-HELA-SIDAN                                      
164800     END-IF                                                               
164900     .                                                                    
165000     EJECT                                                                
165100 S01-RENSA-HELA-SIDAN SECTION.                                            
165200     SKIP2                                                                
165300     MOVE ZERO                    TO MOD-DAREGDAT-DOLD                    
165400                                     MOD-TIREGTID-DOLD                    
165500                                     MOD-IDARTNR-LO                       
165600                                     MOD-IDARTNR-HI                       
165700     MOVE MFS-RENSA-FAELT         TO MOD-TIFSGVV-FOM                      
165800                                     MOD-IDFSGURV                         
165900                                     MOD-KDBORT                           
166000                                     MOD-IDPTYP                           
166100                                     MOD-TIFSGVV-TOM                      
166200                                     MOD-KDPRTYPG                         
166300                                     MOD-KDCMD                            
166400     MOVE +1                      TO RAD-INDX                             
166500     PERFORM UNTIL RAD-INDX > 35                                          
166600        IF RAD-INDX > 8                                                   
166700           MOVE MFS-RENSA-FAELT         TO MOD-IDARTNR  (RAD-INDX)        
166800        ELSE                                                              
166900           IF RAD-INDX > 7                                                
167000              MOVE MFS-RENSA-FAELT TO MOD-IDKONCNR (RAD-INDX)             
167100                                      MOD-IDARTNR  (RAD-INDX)             
167200                                      MOD-KDMARK-FOM (RAD-INDX)           
167300                                      MOD-KDMARK-TOM (RAD-INDX)           
167400           ELSE                                                           
167500              IF RAD-INDX > 4                                             
167600                 MOVE MFS-RENSA-FAELT TO MOD-IDKONCNR (RAD-INDX)          
167700                                         MOD-IDARTNR  (RAD-INDX)          
167800                                         MOD-IDARTNR-IN(RAD-INDX)         
167900                                         MOD-KDMARK-FOM (RAD-INDX)        
168000                                         MOD-KDMARK-TOM (RAD-INDX)        
168100              ELSE                                                        
168200                 MOVE MFS-RENSA-FAELT TO MOD-IDKONCNR (RAD-INDX)          
168300                                       MOD-IDARTNR    (RAD-INDX)          
168400                                       MOD-IDARTNR-IN (RAD-INDX)          
168500                                       MOD-KDMARK-FOM (RAD-INDX)          
168600                                       MOD-KDMARK-TOM (RAD-INDX)          
168700                                       MOD-IDDISTR-FOM(RAD-INDX)          
168800                                       MOD-IDDISTR-TOM(RAD-INDX)          
168900              END-IF                                                      
169000           END-IF                                                         
169100        END-IF                                                            
169200        ADD +1                    TO RAD-INDX                             
169300     END-PERFORM                                                          
169400     .                                                                    
169500     EJECT                                                                
169600 S02-VISA-FSGA13 SECTION.                                                 
169700     SKIP2                                                                
169800     IF URV2-TIFSGVV-FOM = ZERO                                           
169900        MOVE MFS-RENSA-FAELT      TO MOD-TIFSGVV-FOM                      
170000     ELSE                                                                 
170100        MOVE URV2-TIFSGVV-FOM     TO MOD-TIFSGVV-FOM                      
170200     END-IF                                                               
170300     MOVE URV2-IDPTYP             TO MOD-IDPTYP                           
170400     IF URV2-TIFSGVV-TOM = ZERO                                           
170500        MOVE MFS-RENSA-FAELT      TO MOD-TIFSGVV-TOM                      
170600     ELSE                                                                 
170700        MOVE URV2-TIFSGVV-TOM     TO MOD-TIFSGVV-TOM                      
170800     END-IF                                                               
170900     MOVE URV2-KDPRTYP            TO MOD-KDPRTYPG                         
171000     MOVE +1                      TO RAD-INDX                             
171100     PERFORM UNTIL RAD-INDX > 8                                           
171200       IF RAD-INDX > 4                                                    
171300          IF URV2-IDKONCNR(RAD-INDX) = ZERO                               
171400             MOVE MFS-RENSA-FAELT                TO                       
171500                                        MOD-IDKONCNR (RAD-INDX)           
171600          ELSE                                                            
171700             MOVE URV2-IDKONCNR(RAD-INDX)        TO                       
171800                                        MOD-IDKONCNR (RAD-INDX)           
171900          END-IF                                                          
172000          IF URV2-KDMARK-BUDG-FOM (RAD-INDX) = ZERO                       
172100             MOVE MFS-RENSA-FAELT                     TO                  
172200                                 MOD-KDMARK-FOM (RAD-INDX)                
172300          ELSE                                                            
172400             MOVE URV2-KDMARK-BUDG-FOM (RAD-INDX)     TO                  
172500                                 MOD-KDMARK-FOM (RAD-INDX)                
172600          END-IF                                                          
172700          IF URV2-KDMARK-BUDG-TOM (RAD-INDX) = ZERO                       
172800             MOVE MFS-RENSA-FAELT                     TO                  
172900                                 MOD-KDMARK-TOM (RAD-INDX)                
173000          ELSE                                                            
173100             MOVE URV2-KDMARK-BUDG-TOM (RAD-INDX)     TO                  
173200                                 MOD-KDMARK-TOM (RAD-INDX)                
173300          END-IF                                                          
173400       ELSE                                                               
173500          IF URV2-IDKONCNR(RAD-INDX) = ZERO                               
173600             MOVE MFS-RENSA-FAELT                TO                       
173700                                        MOD-IDKONCNR (RAD-INDX)           
173800          ELSE                                                            
173900             MOVE URV2-IDKONCNR(RAD-INDX)        TO                       
174000                                        MOD-IDKONCNR (RAD-INDX)           
174100          END-IF                                                          
174200          IF URV2-KDMARK-BUDG-FOM (RAD-INDX) = ZERO                       
174300             MOVE MFS-RENSA-FAELT                     TO                  
174400                                 MOD-KDMARK-FOM (RAD-INDX)                
174500          ELSE                                                            
174600             MOVE URV2-KDMARK-BUDG-FOM (RAD-INDX)     TO                  
174700                                 MOD-KDMARK-FOM (RAD-INDX)                
174800          END-IF                                                          
174900          IF URV2-KDMARK-BUDG-TOM (RAD-INDX) = ZERO                       
175000             MOVE MFS-RENSA-FAELT                     TO                  
175100                                 MOD-KDMARK-TOM (RAD-INDX)                
175200          ELSE                                                            
175300             MOVE URV2-KDMARK-BUDG-TOM (RAD-INDX)     TO                  
175400                                 MOD-KDMARK-TOM (RAD-INDX)                
175500          END-IF                                                          
175600          IF URV2-IDDISTR-FOM (RAD-INDX) = ZERO                           
175700             MOVE MFS-RENSA-FAELT                TO                       
175800                                     MOD-IDDISTR-FOM (RAD-INDX)           
175900          ELSE                                                            
176000             MOVE URV2-IDDISTR-FOM (RAD-INDX)    TO                       
176100                                     MOD-IDDISTR-FOM (RAD-INDX)           
176200          END-IF                                                          
176300          IF URV2-IDDISTR-TOM (RAD-INDX) = ZERO                           
176400             MOVE MFS-RENSA-FAELT                TO                       
176500                                     MOD-IDDISTR-TOM (RAD-INDX)           
176600          ELSE                                                            
176700             MOVE URV2-IDDISTR-TOM (RAD-INDX)    TO                       
176800                                     MOD-IDDISTR-TOM (RAD-INDX)           
176900          END-IF                                                          
177000       END-IF                                                             
177100       ADD +1                                 TO RAD-INDX                 
177200     END-PERFORM                                                          
177300     .                                                                    
177400     EJECT                                                                
177500 S03-VISA-ARTNR-FSGA14  SECTION.                                          
177600     SKIP1                                                                
177700     MOVE +1                      TO RAD-INDX                             
177800     IF MFS-UPDATE                                                        
177900        IF  MID-IDFSGURV  = ALL '+'                                       
178000        AND MID-IDUSER-IN = ALL '+'                                       
178100           MOVE MID-IDARTNR-LO        TO W-IDARTNR                        
178200        ELSE                                                              
178300           MOVE ZERO                  TO W-IDARTNR                        
178400        END-IF                                                            
178500     END-IF                                                               
178600     PERFORM IMS-GNP-FSGA14                                               
178700     IF SEGMENT-FINNS                                                     
178800        MOVE ART-IDARTNR          TO MOD-IDARTNR-LO                       
178900        PERFORM UNTIL RAD-INDX > 35                                       
179000           IF SEGMENT-FINNS                                               
179100              MOVE ART-IDARTNR         TO                                 
179200                                     MOD-IDARTNR    (RAD-INDX)            
179300              PERFORM IMS-GNP-FSGA14                                      
179400           ELSE                                                           
179500              MOVE MFS-RENSA-FAELT     TO                                 
179600                               MOD-IDARTNR (RAD-INDX)                     
179700           END-IF                                                         
179800           IF RAD-INDX < 8                                                
179900              MOVE MFS-RENSA-FAELT     TO                                 
180000                               MOD-IDARTNR-IN (RAD-INDX)                  
180100           END-IF                                                         
180200           ADD +1                    TO RAD-INDX                          
180300        END-PERFORM                                                       
180400     ELSE                                                                 
180500        MOVE ZERO                    TO MOD-IDARTNR-LO                    
180600     END-IF                                                               
180700     IF SEGMENT-FINNS                                                     
180800        MOVE ART-IDARTNR                TO MOD-IDARTNR-HI                 
180900        IF MFS-UPDATE                                                     
181000           CONTINUE                                                       
181100        ELSE                                                              
181200                                                                          
181300           MOVE '105' TO MED-IDMFSINF                                     
181400           MOVE SPRAK-IX TO MED-IDSKYLT                                   
181500           CALL WMEDKONV USING MED-WMEDAREA                               
181600           MOVE MED-MFSINF TO MOD-TEMFSINF                                
181700                                                                          
181800        END-IF                                                            
181900     ELSE                                                                 
182000        MOVE ZERO                       TO MOD-IDARTNR-HI                 
182100     END-IF                                                               
182200     .                                                                    
182300     EJECT                                                                
182400 S04-SAETT-NUMFAELT-FEL SECTION.                                          
182500     SKIP1                                                                
182600     MOVE +1                        TO RAD-INDX                           
182700     PERFORM UNTIL RAD-INDX > 8                                           
182800        IF RAD-INDX > 4                                                   
182900           IF MID-IDKONCNR (RAD-INDX) = ALL '+'                           
183000              CONTINUE                                                    
183100           ELSE                                                           
183200              MOVE MFS-NUM-FAELT-FEL TO                                   
183300                               MOD-IDKONCNR-ATTR (RAD-INDX)               
183400           END-IF                                                         
183500           IF MID-KDMARK-FOM (RAD-INDX) = ALL '+'                         
183600              CONTINUE                                                    
183700           ELSE                                                           
183800              MOVE MFS-NUM-FAELT-FEL TO                                   
183900                               MOD-KDMARK-FOM-ATTR (RAD-INDX)             
184000           END-IF                                                         
184100           IF MID-KDMARK-TOM (RAD-INDX) = ALL '+'                         
184200              CONTINUE                                                    
184300           ELSE                                                           
184400              MOVE MFS-NUM-FAELT-FEL TO                                   
184500                               MOD-KDMARK-TOM-ATTR (RAD-INDX)             
184600           END-IF                                                         
184700        ELSE                                                              
184800           IF MID-IDKONCNR (RAD-INDX) = ALL '+'                           
184900              CONTINUE                                                    
185000           ELSE                                                           
185100              MOVE MFS-NUM-FAELT-FEL TO                                   
185200                               MOD-IDKONCNR-ATTR (RAD-INDX)               
185300           END-IF                                                         
185400           IF MID-KDMARK-FOM (RAD-INDX) = ALL '+'                         
185500              CONTINUE                                                    
185600           ELSE                                                           
185700              MOVE MFS-NUM-FAELT-FEL TO                                   
185800                               MOD-KDMARK-FOM-ATTR (RAD-INDX)             
185900           END-IF                                                         
186000           IF MID-KDMARK-TOM (RAD-INDX) = ALL '+'                         
186100              CONTINUE                                                    
186200           ELSE                                                           
186300              MOVE MFS-NUM-FAELT-FEL TO                                   
186400                               MOD-KDMARK-TOM-ATTR (RAD-INDX)             
186500           END-IF                                                         
186600           IF MID-IDDISTR-FOM (RAD-INDX) = ALL '+'                        
186700              CONTINUE                                                    
186800           ELSE                                                           
186900              MOVE MFS-NUM-FAELT-FEL TO                                   
187000                               MOD-IDDISTR-FOM-ATTR (RAD-INDX)            
187100           END-IF                                                         
187200           IF MID-IDDISTR-TOM (RAD-INDX) = ALL '+'                        
187300              CONTINUE                                                    
187400           ELSE                                                           
187500              MOVE MFS-NUM-FAELT-FEL TO                                   
187600                               MOD-IDDISTR-TOM-ATTR (RAD-INDX)            
187700           END-IF                                                         
187800        END-IF                                                            
187900        ADD +1                      TO RAD-INDX                           
188000     END-PERFORM                                                          
188100     .                                                                    
188200     EJECT                                                                
188300 S05-KOLLA-BORTTAG-IDARTNR  SECTION.                                      
188400     SKIP1                                                                
188500     MOVE ZERO                      TO ANTAL-ATT-TA-BORT                  
188600     MOVE +1                        TO RAD-INDX                           
188700     PERFORM UNTIL RAD-INDX > 7                                           
188800        IF MID-IDARTNR-IN (RAD-INDX) = ALL '+'                            
188900           CONTINUE                                                       
189000        ELSE                                                              
189100           ADD  +1                  TO ANTAL-ATT-TA-BORT                  
189200           IF MID-IDARTNR-IN (RAD-INDX) NUMERIC                           
189300              MOVE MID-IDARTNR-IN (RAD-INDX) TO W-IDARTNR                 
189400              PERFORM IMS-GU-FSGA14                                       
189500              IF SEGMENT-FINNS                                            
189600                 MOVE MFS-NUM-FAELT-RAETT  TO                             
189700                            MOD-IDARTNR-IN-ATTR (RAD-INDX)                
189800              ELSE                                                        
189900                 MOVE NEJ               TO SW-INDATA-OK                   
190000                 MOVE MFS-NUM-FAELT-FEL TO                                
190100                            MOD-IDARTNR-IN-ATTR (RAD-INDX)                
190200                                                                          
190300                 MOVE '017' TO MED-IDMFSINF                               
190400                 MOVE SPRAK-IX TO MED-IDSKYLT                             
190500                 CALL WMEDKONV USING MED-WMEDAREA                         
190600                 MOVE MED-MFSINF TO MOD-TEMFSINF                          
190700                                                                          
190800              END-IF                                                      
190900           ELSE                                                           
191000              MOVE NEJ               TO SW-INDATA-OK                      
191100              MOVE MFS-NUM-FAELT-FEL TO                                   
191200                            MOD-IDARTNR-IN-ATTR (RAD-INDX)                
191300           END-IF                                                         
191400        END-IF                                                            
191500        ADD +1                      TO RAD-INDX                           
191600     END-PERFORM                                                          
191700     IF ANTAL-ATT-TA-BORT > ZERO                                          
191800     AND INDATA-OK                                                        
191900        IF MID-KDCMD = ALL '+'                                            
192000           MOVE NEJ                 TO SW-INDATA-OK                       
192100           MOVE MFS-ALFA-FAELT-FEL  TO MOD-KDCMD-ATTR                     
192200        ELSE                                                              
192300           IF INDATA-OK                                                   
192400              MOVE ZERO                   TO W-IDARTNR                    
192500              MOVE ZERO                   TO ANTAL-ARTIKLAR-BAS           
192600              PERFORM IMS-GU-FSGA01                                       
192700              PERFORM IMS-GNP-FSGA14                                      
192800              PERFORM UNTIL ANTAL-ARTIKLAR-BAS > ANTAL-ATT-TA-BORT        
192900                                                 OR SEGMENT-SAKNAS        
193000                 ADD  +1                  TO ANTAL-ARTIKLAR-BAS           
193100                 PERFORM IMS-GNP-FSGA14                                   
193200              END-PERFORM                                                 
193300              IF ANTAL-ARTIKLAR-BAS > ANTAL-ATT-TA-BORT                   
193400                 CONTINUE                                                 
193500              ELSE                                                        
193600                 MOVE NEJ                 TO SW-INDATA-OK                 
193700                 MOVE MFS-ALFA-FAELT-FEL  TO MOD-KDCMD-ATTR               
193800                                                                          
193900                 MOVE '104' TO MED-IDMFSINF                               
194000                 MOVE SPRAK-IX TO MED-IDSKYLT                             
194100                 CALL WMEDKONV USING MED-WMEDAREA                         
194200                 MOVE MED-MFSINF TO MOD-TEMFSINF                          
194300                                                                          
194400              END-IF                                                      
194500           END-IF                                                         
194600        END-IF                                                            
194700     END-IF                                                               
194800     .                                                                    
194900     EJECT                                                                
195000 S06-MFS-ROER-EJ-FAELT SECTION.                                           
195100     SKIP2                                                                
195200     IF MFS-UPDATE                                                        
195300        IF MID-IDUSER-IN = ALL '+'                                        
195400           CONTINUE                                                       
195500        ELSE                                                              
195600           MOVE MFS-ROER-EJ-FAELT TO MOD-IDUSER-IN                        
195700        END-IF                                                            
195800     END-IF                                                               
195900     MOVE MFS-ROER-EJ-FAELT       TO MOD-TIFSGVV-FOM                      
196000                                     MOD-IDFSGURV                         
196100                                     MOD-KDBORT                           
196200                                     MOD-IDPTYP                           
196300                                     MOD-TIFSGVV-TOM                      
196400                                     MOD-KDPRTYPG                         
196500                                     MOD-KDCMD                            
196600                                     MOD-IDARTNR-LO                       
196700                                     MOD-IDARTNR-HI                       
196800                                     MOD-DAREGDAT-DOLD                    
196900                                     MOD-TIREGTID-DOLD                    
197000     MOVE +1                      TO RAD-INDX                             
197100     PERFORM UNTIL RAD-INDX > 35                                          
197200        IF RAD-INDX > 8                                                   
197300           MOVE MFS-ROER-EJ-FAELT TO    MOD-IDARTNR     (RAD-INDX)        
197400        ELSE                                                              
197500           IF RAD-INDX > 7                                                
197600              MOVE MFS-ROER-EJ-FAELT TO MOD-IDKONCNR    (RAD-INDX)        
197700                                        MOD-IDARTNR     (RAD-INDX)        
197800                                        MOD-KDMARK-FOM  (RAD-INDX)        
197900                                        MOD-KDMARK-TOM  (RAD-INDX)        
198000           ELSE                                                           
198100              IF RAD-INDX > 4                                             
198200                 MOVE MFS-ROER-EJ-FAELT TO                                
198300                                        MOD-IDKONCNR    (RAD-INDX)        
198400                                        MOD-IDARTNR     (RAD-INDX)        
198500                                        MOD-IDARTNR-IN  (RAD-INDX)        
198600                                        MOD-KDMARK-FOM  (RAD-INDX)        
198700                                        MOD-KDMARK-TOM  (RAD-INDX)        
198800              ELSE                                                        
198900                 MOVE MFS-ROER-EJ-FAELT TO                                
199000                                       MOD-IDKONCNR     (RAD-INDX)        
199100                                       MOD-IDARTNR      (RAD-INDX)        
199200                                       MOD-IDARTNR-IN   (RAD-INDX)        
199300                                       MOD-KDMARK-FOM   (RAD-INDX)        
199400                                       MOD-KDMARK-TOM   (RAD-INDX)        
199500                                       MOD-IDDISTR-FOM  (RAD-INDX)        
199600                                       MOD-IDDISTR-TOM  (RAD-INDX)        
199700              END-IF                                                      
199800           END-IF                                                         
199900        END-IF                                                            
200000        ADD +1                       TO RAD-INDX                          
200100     END-PERFORM                                                          
200200     .                                                                    
200300     EJECT                                                                
200400 S07-VISA-SIDAN SECTION.                                                  
200500     SKIP1                                                                
200600     MOVE W-DAREGDAT                  TO MOD-DAREGDAT-DOLD                
200700     MOVE W-TIREGTID                  TO MOD-TIREGTID-DOLD                
200800     IF SEGMENT-FINNS                                                     
200900        MOVE USER-IDFSGURV               TO MOD-IDFSGURV                  
201000        PERFORM IMS-GNP-FSGA13                                            
201100        IF SEGMENT-FINNS                                                  
201200           MOVE MFS-RENSA-FAELT          TO MOD-KDBORT                    
201300                                            MOD-KDPRTYPG                  
201400                                            MOD-KDCMD                     
201500           MOVE +1                       TO RAD-INDX                      
201600           PERFORM UNTIL RAD-INDX > 7                                     
201700              MOVE MFS-RENSA-FAELT       TO                               
201800                                       MOD-IDARTNR-IN (RAD-INDX)          
201900              ADD +1                     TO RAD-INDX                      
202000           END-PERFORM                                                    
202100           PERFORM S02-VISA-FSGA13                                        
202200           PERFORM S03-VISA-ARTNR-FSGA14                                  
202300        ELSE                                                              
202400           PERFORM S01-RENSA-HELA-SIDAN                                   
202500        END-IF                                                            
202600     ELSE                                                                 
202700        PERFORM S01-RENSA-HELA-SIDAN                                      
202800     END-IF                                                               
202900     .                                                                    
203000     EJECT                                                                
203100 S08-KOLLA-ATT-INGET-IFYLLT SECTION.                                      
203200     SKIP2                                                                
203300     MOVE NEJ                        TO SW-IFYLLT                         
203400     IF MFS-UPDATE                                                        
203500        IF MID-IDUSER-IN = ALL '+'                                        
203600        OR SPACE                                                          
203700           CONTINUE                                                       
203800        ELSE                                                              
203900           MOVE JA                TO SW-IFYLLT                            
204000        END-IF                                                            
204100     ELSE                                                                 
204200        IF MID-KDBORT = ALL '+'                                           
204300           CONTINUE                                                       
204400        ELSE                                                              
204500           MOVE JA                TO SW-IFYLLT                            
204600        END-IF                                                            
204700     END-IF                                                               
204800     IF  MID-TIFSGVV-FOM    = ALL '+'                                     
204900     AND MID-IDFSGURV       = ALL '+'                                     
205000     AND MID-IDPTYP         = ALL '+'                                     
205100     AND MID-TIFSGVV-TOM    = ALL '+'                                     
205200     AND MID-KDPRTYPG       = ALL '+'                                     
205300     AND MID-KDCMD          = ALL '+'                                     
205400        CONTINUE                                                          
205500     ELSE                                                                 
205600        MOVE JA                   TO SW-IFYLLT                            
205700     END-IF                                                               
205800     MOVE +1                      TO RAD-INDX                             
205900     PERFORM UNTIL RAD-INDX > 35                                          
206000     OR            SW-IFYLLT = JA                                         
206100        IF RAD-INDX > 8                                                   
206200           IF MID-IDARTNR (RAD-INDX) = ALL '+'                            
206300              CONTINUE                                                    
206400           ELSE                                                           
206500              MOVE JA             TO SW-IFYLLT                            
206600           END-IF                                                         
206700        ELSE                                                              
206800           IF RAD-INDX > 7                                                
206900              IF  MID-IDARTNR       (RAD-INDX) = ALL '+'                  
207000              AND MID-IDKONCNR      (RAD-INDX) = ALL '+'                  
207100              AND MID-KDMARK-FOM    (RAD-INDX) = ALL '+'                  
207200              AND MID-KDMARK-TOM    (RAD-INDX) = ALL '+'                  
207300                 CONTINUE                                                 
207400              ELSE                                                        
207500                 MOVE JA          TO SW-IFYLLT                            
207600              END-IF                                                      
207700           ELSE                                                           
207800              IF RAD-INDX > 4                                             
207900                 IF  MID-IDARTNR       (RAD-INDX) = ALL '+'               
208000                 AND MID-IDKONCNR      (RAD-INDX) = ALL '+'               
208100                 AND MID-IDARTNR-IN    (RAD-INDX) = ALL '+'               
208200                 AND MID-KDMARK-FOM    (RAD-INDX) = ALL '+'               
208300                 AND MID-KDMARK-TOM    (RAD-INDX) = ALL '+'               
208400                    CONTINUE                                              
208500                 ELSE                                                     
208600                    MOVE JA          TO SW-IFYLLT                         
208700                 END-IF                                                   
208800              ELSE                                                        
208900                 IF  MID-IDARTNR        (RAD-INDX) = ALL '+'              
209000                 AND MID-IDARTNR-IN     (RAD-INDX) = ALL '+'              
209100                 AND MID-IDKONCNR       (RAD-INDX) = ALL '+'              
209200                 AND MID-KDMARK-FOM     (RAD-INDX) = ALL '+'              
209300                 AND MID-KDMARK-TOM     (RAD-INDX) = ALL '+'              
209400                 AND MID-IDDISTR-FOM    (RAD-INDX) = ALL '+'              
209500                 AND MID-IDDISTR-TOM    (RAD-INDX) = ALL '+'              
209600                    CONTINUE                                              
209700                 ELSE                                                     
209800                    MOVE JA          TO SW-IFYLLT                         
209900                 END-IF                                                   
210000              END-IF                                                      
210100           END-IF                                                         
210200        END-IF                                                            
210300        ADD +1                    TO RAD-INDX                             
210400     END-PERFORM                                                          
210500     .                                                                    
210600     EJECT                                                                
210700 S09-LAES-IN-IGEN    SECTION.                                             
210800     SKIP2                                                                
210900     IF MFS-UPDATE                                                        
211000        IF MID-IDUSER-IN = ALL '+'                                        
211100           CONTINUE                                                       
211200        ELSE                                                              
211300           MOVE MFS-ROER-EJ-FAELT     TO MOD-IDUSER-IN                    
211400        END-IF                                                            
211500     ELSE                                                                 
211600       IF MID-KDBORT = ALL '+'                                            
211700          CONTINUE                                                        
211800       ELSE                                                               
211900          MOVE MFS-ADD-LAES-IN-FAELT   TO MOD-KDBORT-ATTR                 
212000       END-IF                                                             
212100     END-IF                                                               
212200     IF MID-TIFSGVV-FOM = ALL '+'                                         
212300        CONTINUE                                                          
212400     ELSE                                                                 
212500        MOVE MFS-ADD-LAES-IN-FAELT   TO MOD-TIFSGVV-FOM-ATTR              
212600     END-IF                                                               
212700     IF MID-IDFSGURV = ALL '+'                                            
212800        CONTINUE                                                          
212900     ELSE                                                                 
213000        MOVE MFS-ADD-LAES-IN-FAELT   TO MOD-IDFSGURV-ATTR                 
213100     END-IF                                                               
213200     IF MID-IDPTYP = ALL '+'                                              
213300        CONTINUE                                                          
213400     ELSE                                                                 
213500        MOVE MFS-ADD-LAES-IN-FAELT   TO MOD-IDPTYP-ATTR                   
213600     END-IF                                                               
213700     IF MID-TIFSGVV-TOM = ALL '+'                                         
213800        CONTINUE                                                          
213900     ELSE                                                                 
214000        MOVE MFS-ADD-LAES-IN-FAELT   TO MOD-TIFSGVV-TOM-ATTR              
214100     END-IF                                                               
214200     IF MID-KDPRTYPG = ALL '+'                                            
214300        CONTINUE                                                          
214400     ELSE                                                                 
214500        MOVE MFS-ADD-LAES-IN-FAELT   TO MOD-KDPRTYPG-ATTR                 
214600     END-IF                                                               
214700     IF MID-KDCMD = ALL '+'                                               
214800        CONTINUE                                                          
214900     ELSE                                                                 
215000        MOVE MFS-ADD-LAES-IN-FAELT   TO MOD-KDCMD-ATTR                    
215100     END-IF                                                               
215200     MOVE +1                      TO RAD-INDX                             
215300     PERFORM UNTIL RAD-INDX > 35                                          
215400        IF RAD-INDX > 8                                                   
215500           IF MID-IDARTNR (RAD-INDX) = ALL '+'                            
215600              CONTINUE                                                    
215700           ELSE                                                           
215800              MOVE MFS-ADD-LAES-IN-FAELT TO                               
215900                                   MOD-IDARTNR-ATTR   (RAD-INDX)          
216000           END-IF                                                         
216100        ELSE                                                              
216200           IF RAD-INDX > 7                                                
216300              IF MID-IDARTNR (RAD-INDX) = ALL '+'                         
216400                 CONTINUE                                                 
216500              ELSE                                                        
216600                 MOVE MFS-ADD-LAES-IN-FAELT TO                            
216700                                  MOD-IDARTNR-ATTR    (RAD-INDX)          
216800              END-IF                                                      
216900              IF MID-IDKONCNR (RAD-INDX) = ALL '+'                        
217000                 CONTINUE                                                 
217100              ELSE                                                        
217200                 MOVE MFS-ADD-LAES-IN-FAELT TO                            
217300                                  MOD-IDKONCNR-ATTR   (RAD-INDX)          
217400              END-IF                                                      
217500              IF MID-KDMARK-FOM (RAD-INDX) = ALL '+'                      
217600                 CONTINUE                                                 
217700              ELSE                                                        
217800                 MOVE MFS-ADD-LAES-IN-FAELT TO                            
217900                               MOD-KDMARK-FOM-ATTR (RAD-INDX)             
218000              END-IF                                                      
218100              IF MID-KDMARK-TOM (RAD-INDX) = ALL '+'                      
218200                 CONTINUE                                                 
218300              ELSE                                                        
218400                 MOVE MFS-ADD-LAES-IN-FAELT TO                            
218500                               MOD-KDMARK-TOM-ATTR (RAD-INDX)             
218600              END-IF                                                      
218700           ELSE                                                           
218800              IF RAD-INDX > 4                                             
218900                 IF MID-IDARTNR (RAD-INDX) = ALL '+'                      
219000                    CONTINUE                                              
219100                 ELSE                                                     
219200                    MOVE MFS-ADD-LAES-IN-FAELT TO                         
219300                                  MOD-IDARTNR-ATTR    (RAD-INDX)          
219400                 END-IF                                                   
219500                 IF MID-IDKONCNR (RAD-INDX) = ALL '+'                     
219600                    CONTINUE                                              
219700                 ELSE                                                     
219800                    MOVE MFS-ADD-LAES-IN-FAELT TO                         
219900                                  MOD-IDKONCNR-ATTR   (RAD-INDX)          
220000                 END-IF                                                   
220100                 IF MID-IDARTNR-IN (RAD-INDX) = ALL '+'                   
220200                    CONTINUE                                              
220300                 ELSE                                                     
220400                    MOVE MFS-ADD-LAES-IN-FAELT TO                         
220500                                  MOD-IDARTNR-IN-ATTR (RAD-INDX)          
220600                 END-IF                                                   
220700                 IF MID-KDMARK-FOM (RAD-INDX) = ALL '+'                   
220800                    CONTINUE                                              
220900                 ELSE                                                     
221000                    MOVE MFS-ADD-LAES-IN-FAELT TO                         
221100                                  MOD-KDMARK-FOM-ATTR (RAD-INDX)          
221200                 END-IF                                                   
221300                 IF MID-KDMARK-TOM (RAD-INDX) = ALL '+'                   
221400                    CONTINUE                                              
221500                 ELSE                                                     
221600                    MOVE MFS-ADD-LAES-IN-FAELT TO                         
221700                                  MOD-KDMARK-TOM-ATTR (RAD-INDX)          
221800                 END-IF                                                   
221900              ELSE                                                        
222000                 IF MID-IDARTNR (RAD-INDX) = ALL '+'                      
222100                    CONTINUE                                              
222200                 ELSE                                                     
222300                    MOVE MFS-ADD-LAES-IN-FAELT TO                         
222400                                  MOD-IDARTNR-ATTR   (RAD-INDX)           
222500                 END-IF                                                   
222600                 IF MID-IDKONCNR (RAD-INDX) = ALL '+'                     
222700                    CONTINUE                                              
222800                 ELSE                                                     
222900                    MOVE MFS-ADD-LAES-IN-FAELT TO                         
223000                                  MOD-IDKONCNR-ATTR   (RAD-INDX)          
223100                 END-IF                                                   
223200                 IF MID-IDARTNR-IN (RAD-INDX) = ALL '+'                   
223300                    CONTINUE                                              
223400                 ELSE                                                     
223500                    MOVE MFS-ADD-LAES-IN-FAELT TO                         
223600                                  MOD-IDARTNR-IN-ATTR (RAD-INDX)          
223700                 END-IF                                                   
223800                 IF MID-KDMARK-FOM (RAD-INDX) = ALL '+'                   
223900                    CONTINUE                                              
224000                 ELSE                                                     
224100                    MOVE MFS-ADD-LAES-IN-FAELT TO                         
224200                                  MOD-KDMARK-FOM-ATTR (RAD-INDX)          
224300                 END-IF                                                   
224400                 IF MID-KDMARK-TOM (RAD-INDX) = ALL '+'                   
224500                    CONTINUE                                              
224600                 ELSE                                                     
224700                    MOVE MFS-ADD-LAES-IN-FAELT TO                         
224800                                  MOD-KDMARK-TOM-ATTR (RAD-INDX)          
224900                 END-IF                                                   
225000                 IF MID-IDDISTR-FOM (RAD-INDX) = ALL '+'                  
225100                    CONTINUE                                              
225200                 ELSE                                                     
225300                    MOVE MFS-ADD-LAES-IN-FAELT TO                         
225400                                  MOD-IDDISTR-FOM-ATTR (RAD-INDX)         
225500                 END-IF                                                   
225600                 IF MID-IDDISTR-TOM (RAD-INDX) = ALL '+'                  
225700                    CONTINUE                                              
225800                 ELSE                                                     
225900                    MOVE MFS-ADD-LAES-IN-FAELT TO                         
226000                                  MOD-IDDISTR-TOM-ATTR (RAD-INDX)         
226100                 END-IF                                                   
226200              END-IF                                                      
226300           END-IF                                                         
226400        END-IF                                                            
226500        ADD +1                    TO RAD-INDX                             
226600     END-PERFORM                                                          
226700     .                                                                    
226800     EJECT                                                                
226900 S99-CALL-WDATKONV  SECTION.                                              
227000     SKIP1                                                                
227100     CALL WDATKONV             USING DAT-KDDATFORM                        
227200                                     DAT-I-TIDATUM                        
227300                                     DAT-O-TIDATUM                        
227400                                     DAT-KDSVAR                           
227500     .                                                                    
227600     EJECT                                                                
227700 IMS-GET-MSG SECTION.                                                     
227800     SKIP1                                                                
227900     MOVE '  QC' TO GODK-STATUSKODER                                      
228000     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
228100     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
228200     PERFORM IMS-STATUSKONTROLL                                           
228300     .                                                                    
228400     SKIP3                                                                
228500 IMS-INSERT-MSG SECTION.                                                  
228600     SKIP1                                                                
228700     IF NOT ENGLISH-TEXT                                                  
228800       MOVE '0' TO MFS-KDHUVOMR                                           
228900     END-IF                                                               
229000     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
229100     MOVE SPACE TO GODK-STATUSKODER                                       
229200     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
229300     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
229400     PERFORM IMS-STATUSKONTROLL                                           
229500     .                                                                    
229600     SKIP3                                                                
229700 IMS-GN-FSGB01 SECTION.                                                   
229800     SKIP1                                                                
229900     STRING 'WLFSGB01(WDM3A1KY>=' W-WDM3A1KY-MIN                          
230000                    '&WDM3A1KY<=' W-WDM3A1KY-MAX ')'                      
230100            DELIMITED BY SIZE INTO SSA1                                   
230200     MOVE '  GE' TO GODK-STATUSKODER                                      
230300     CALL CBLTDLI USING GN FSGB-PCB DLI-IO-AREA SSA1                      
230400     MOVE FSGB-STATUS-CODE TO STATUS-WS                                   
230500     PERFORM IMS-STATUSKONTROLL                                           
230600     .                                                                    
230700     SKIP3                                                                
230800 IMS-GHU-FSGA01 SECTION.                                                  
230900     SKIP1                                                                
231000     STRING 'WLFSGA01(WDM301KY =' W-WDM301KY-X ')'                        
231100            DELIMITED BY SIZE INTO SSA1                                   
231200     MOVE '  GE' TO GODK-STATUSKODER                                      
231300     CALL CBLTDLI USING GHU FSGA-PCB DLI-IO-AREA SSA1                     
231400     MOVE FSGA-STATUS-CODE TO STATUS-WS                                   
231500     PERFORM IMS-STATUSKONTROLL                                           
231600     .                                                                    
231700     SKIP3                                                                
231800 IMS-GU-FSGA01 SECTION.                                                   
231900     SKIP1                                                                
232000     STRING 'WLFSGA01(WDM301KY =' W-WDM301KY-X ')'                        
232100            DELIMITED BY SIZE INTO SSA1                                   
232200     MOVE '  GE' TO GODK-STATUSKODER                                      
232300     CALL CBLTDLI USING GU FSGA-PCB DLI-IO-AREA SSA1                      
232400     MOVE FSGA-STATUS-CODE TO STATUS-WS                                   
232500     PERFORM IMS-STATUSKONTROLL                                           
232600     .                                                                    
232700     SKIP3                                                                
232800 IMS-GNP-FSGA13 SECTION.                                                  
232900     SKIP1                                                                
233000     STRING 'WLFSGA01(WDM301KY =' W-WDM301KY-X ')'                        
233100            DELIMITED BY SIZE INTO SSA1                                   
233200     STRING 'WLFSGA13(KDSEGKEY>=' W-KDSEGKEY-X ')'                        
233300            DELIMITED BY SIZE INTO SSA2                                   
233400     MOVE '  GE' TO GODK-STATUSKODER                                      
233500     CALL CBLTDLI USING GNP FSGA-PCB DLI-IO-AREA SSA1 SSA2                
233600     MOVE FSGA-STATUS-CODE TO STATUS-WS                                   
233700     PERFORM IMS-STATUSKONTROLL                                           
233800     .                                                                    
233900     SKIP3                                                                
234000 IMS-GNP-FSGA14 SECTION.                                                  
234100     SKIP1                                                                
234200     STRING 'WLFSGA01(WDM301KY =' W-WDM301KY-X ')'                        
234300            DELIMITED BY SIZE INTO SSA1                                   
234400     STRING 'WLFSGA14(IDARTNR >=' W-IDARTNR-X ')'                         
234500            DELIMITED BY SIZE INTO SSA2                                   
234600     MOVE '  GE' TO GODK-STATUSKODER                                      
234700     CALL CBLTDLI USING GNP FSGA-PCB DLI-IO-AREA SSA1 SSA2                
234800     MOVE FSGA-STATUS-CODE TO STATUS-WS                                   
234900     PERFORM IMS-STATUSKONTROLL                                           
235000     .                                                                    
235100     SKIP3                                                                
235200 IMS-ISRT-FSGA01 SECTION.                                                 
235300     SKIP1                                                                
235400     MOVE   'WLFSGA01 '            TO SSA1                                
235500     MOVE '  ' TO GODK-STATUSKODER                                        
235600     CALL CBLTDLI USING ISRT FSGA-PCB DLI-IO-AREA SSA1                    
235700     MOVE FSGA-STATUS-CODE TO STATUS-WS                                   
235800     PERFORM IMS-STATUSKONTROLL                                           
235900     .                                                                    
236000     SKIP3                                                                
236100 IMS-ISRT-FSGA13 SECTION.                                                 
236200     SKIP1                                                                
236300     STRING 'WLFSGA01(WDM301KY =' W-WDM301KY-X ')'                        
236400            DELIMITED BY SIZE INTO SSA1                                   
236500     MOVE   'WLFSGA13 '            TO SSA2                                
236600     MOVE '  ' TO GODK-STATUSKODER                                        
236700     CALL CBLTDLI USING ISRT FSGA-PCB DLI-IO-AREA SSA1 SSA2               
236800     MOVE FSGA-STATUS-CODE TO STATUS-WS                                   
236900     PERFORM IMS-STATUSKONTROLL                                           
237000     .                                                                    
237100     SKIP3                                                                
237200 IMS-GHU-FSGA14 SECTION.                                                  
237300     SKIP1                                                                
237400     STRING 'WLFSGA01(WDM301KY =' W-WDM301KY-X ')'                        
237500            DELIMITED BY SIZE INTO SSA1                                   
237600     STRING 'WLFSGA14(IDARTNR  =' W-IDARTNR-X ')'                         
237700            DELIMITED BY SIZE INTO SSA2                                   
237800     MOVE '  GE' TO GODK-STATUSKODER                                      
237900     CALL CBLTDLI USING GHU FSGA-PCB DLI-IO-AREA SSA1 SSA2                
238000     MOVE FSGA-STATUS-CODE TO STATUS-WS                                   
238100     PERFORM IMS-STATUSKONTROLL                                           
238200     .                                                                    
238300     SKIP3                                                                
238400 IMS-GU-FSGA14 SECTION.                                                   
238500     SKIP1                                                                
238600     STRING 'WLFSGA01(WDM301KY =' W-WDM301KY-X ')'                        
238700            DELIMITED BY SIZE INTO SSA1                                   
238800     STRING 'WLFSGA14(IDARTNR  =' W-IDARTNR-X ')'                         
238900            DELIMITED BY SIZE INTO SSA2                                   
239000     MOVE '  GE' TO GODK-STATUSKODER                                      
239100     CALL CBLTDLI USING GU FSGA-PCB DLI-IO-AREA SSA1 SSA2                 
239200     MOVE FSGA-STATUS-CODE TO STATUS-WS                                   
239300     PERFORM IMS-STATUSKONTROLL                                           
239400     .                                                                    
239500     SKIP3                                                                
239600 IMS-ISRT-FSGA14 SECTION.                                                 
239700     SKIP1                                                                
239800     STRING 'WLFSGA01(WDM301KY =' W-WDM301KY-X ')'                        
239900            DELIMITED BY SIZE INTO SSA1                                   
240000     MOVE   'WLFSGA14 '            TO SSA2                                
240100     MOVE '  II' TO GODK-STATUSKODER                                      
240200     CALL CBLTDLI USING ISRT FSGA-PCB DLI-IO-AREA SSA1 SSA2               
240300     MOVE FSGA-STATUS-CODE TO STATUS-WS                                   
240400     PERFORM IMS-STATUSKONTROLL                                           
240500     .                                                                    
240600     SKIP3                                                                
240700 IMS-GHU-FSGA01-SPAR SECTION.                                             
240800     SKIP1                                                                
240900     STRING 'WLFSGA01(WDM301KY =' W-WDM301KY-SPAR ')'                     
241000            DELIMITED BY SIZE INTO SSA1                                   
241100     MOVE '  GE' TO GODK-STATUSKODER                                      
241200     CALL CBLTDLI USING GHU FSGA1-PCB DLI-IO-AREA-2 SSA1                  
241300     MOVE FSGA1-STATUS-CODE TO STATUS-WS                                  
241400     PERFORM IMS-STATUSKONTROLL                                           
241500     .                                                                    
241600     SKIP3                                                                
241700 IMS-GU-FSGA01-SPAR SECTION.                                              
241800     SKIP1                                                                
241900     STRING 'WLFSGA01(WDM301KY =' W-WDM301KY-SPAR ')'                     
242000            DELIMITED BY SIZE INTO SSA1                                   
242100     MOVE '  GE' TO GODK-STATUSKODER                                      
242200     CALL CBLTDLI USING GU FSGA1-PCB DLI-IO-AREA-2 SSA1                   
242300     MOVE FSGA1-STATUS-CODE TO STATUS-WS                                  
242400     PERFORM IMS-STATUSKONTROLL                                           
242500     .                                                                    
242600     SKIP3                                                                
242700 IMS-GNP-FSGA14-SPAR SECTION.                                             
242800     SKIP1                                                                
242900     STRING 'WLFSGA01(WDM301KY =' W-WDM301KY-SPAR ')'                     
243000            DELIMITED BY SIZE INTO SSA1                                   
243100     STRING 'WLFSGA14(IDARTNR >=' W-IDARTNR-SPAR-X ')'                    
243200            DELIMITED BY SIZE INTO SSA2                                   
243300     MOVE '  GE' TO GODK-STATUSKODER                                      
243400     CALL CBLTDLI USING GNP FSGA1-PCB DLI-IO-AREA-2 SSA1 SSA2             
243500     MOVE FSGA1-STATUS-CODE TO STATUS-WS                                  
243600     PERFORM IMS-STATUSKONTROLL                                           
243700     .                                                                    
243800     SKIP3                                                                
243900 IMS-DLET-FSGA-SPAR   SECTION.                                            
244000     MOVE '  ' TO GODK-STATUSKODER                                        
244100     CALL CBLTDLI USING DLET FSGA1-PCB DLI-IO-AREA-2                      
244200     MOVE FSGA1-STATUS-CODE TO STATUS-WS                                  
244300     PERFORM IMS-STATUSKONTROLL                                           
244400     .                                                                    
244500     SKIP3                                                                
244600 IMS-DLET-FSGA   SECTION.                                                 
244700     MOVE '  ' TO GODK-STATUSKODER                                        
244800     CALL CBLTDLI USING DLET FSGA-PCB DLI-IO-AREA                         
244900     MOVE FSGA-STATUS-CODE TO STATUS-WS                                   
245000     PERFORM IMS-STATUSKONTROLL                                           
245100     .                                                                    
245200     SKIP3                                                                
245300 IMS-STATUSKONTROLL SECTION.                                              
245400     SKIP1                                                                
245500     SET STATUS-IX TO 1                                                   
245600     SEARCH GODK-STATUS AT END CALL FELLOG                                
245700       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS NEXT SENTENCE             
245800     END-SEARCH                                                           
245900     .                                                                    
245910     EJECT                                                                
246100*    -COPY WY2000P3                                                       
