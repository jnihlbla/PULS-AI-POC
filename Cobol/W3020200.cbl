000100     SKIP3                                                                
000200 ID DIVISION.                                                             
000300     SKIP2                                                                
000400 PROGRAM-ID.     W3020200.                                                
000500 AUTHOR.         PETER D.                                                 
000600 DATE-WRITTEN.   NOV   89.                                                
000700                                                                          
000800     REMARKS.                                                             
000900*                                                                         
001000*    FUNKTION.                                                            
001100*        PROGRAMMET LÄSER OCH UPPDATERAR ON-LINE.                         
001200*                                                                         
001300*                                                                         
001400*    INDATA.                                                              
001500*        TRANSAKTION: W3T202                                              
001600*        MID:         W3I20201                                            
001700*                                                                         
001800*    UTDATA.                                                              
001900*        MOD:         W3O20201                                            
002000     SKIP3                                                                
002100 ENVIRONMENT DIVISION.                                                    
002200     SKIP2                                                                
002300 DATA DIVISION.                                                           
002400     EJECT                                                                
002500 WORKING-STORAGE SECTION.                                                 
002600*    -COPY WY2000W3                                                       
002700     SKIP3                                                                
002800 77  PROGRAM-NAMN                PIC X(8) VALUE 'W3020200'.               
002900 77  JA                          PIC X       VALUE 'J'.                   
003000 77  NEJ                         PIC X       VALUE 'N'.                   
003100 77  SPRAK-IX                    PIC S9(9)   VALUE +0   COMP SYNC.        
003200 77  ANTAL-IDFKNGRP-BAS          PIC S9(9)   VALUE +0   COMP SYNC.        
003300 77  ANTAL-ATT-TA-BORT           PIC S9(9)   VALUE +0   COMP SYNC.        
003400 77  RAD-INDX                    PIC S9(9)   VALUE +0   COMP SYNC.        
003500 77  MAX-MOD-LAENGD              PIC S9(4)  VALUE  +733 COMP SYNC.        
003600 77  WS-IDFSGURV                 PIC  X(8)   VALUE SPACE.                 
003700 01  WS-TIREGTID.                                                         
003800   03  WS-TIREGTID-HHMMSS        PIC 9(6)    VALUE ZERO.                  
003900   03  FILLER                    PIC 9(2)    VALUE ZERO.                  
004000 01  SW-IFYLLT             PIC X.                                         
004100    88  INGET-IFYLLT                         VALUE 'N'.                   
004200*KONFLIKT TÄNDS OM MER ÄN ETT URVAL AV KONCERN MARKNAD ELER               
004300*DISTRIKT ÄR VALDA                                                        
004400 01  SW-KONFLIKT                 PIC X.                                   
004500    88  KONFLIKT                             VALUE 'J'.                   
004600 01  SW-PRODSL                   PIC X.                                   
004700    88  PRODSL-EJ-IFYLLD                     VALUE 'N'.                   
004800 01  SW-KONCERN                  PIC X.                                   
004900    88  KONCERN-EJ-IFYLLD                    VALUE 'N'.                   
005000 01  SW-DISTRIKT                 PIC X.                                   
005100    88  DISTRIKT-EJ-IFYLLD                   VALUE 'N'.                   
005200 01  SW-MARKNAD                  PIC X.                                   
005300    88  MARKNAD-EJ-IFYLLD                    VALUE 'N'.                   
005400 01  SW-NYCKLAR-OK               PIC X.                                   
005500    88  NYCKLAR-OK                           VALUE 'J'.                   
005600 01  SW-INDATA-OK                PIC X.                                   
005700    88  INDATA-OK                            VALUE 'J'.                   
005800 01  WS-IDTRANS                  PIC X(4).                                
005900    88  EGEN-BILD                            VALUE '3202'.                
006000    88  3201-BILD                            VALUE '3201'.                
006100    88  GODKAEND-BILD                        VALUE '3201'                 
006200                                                   '3202'                 
006300                                                   '3204'                 
006400                                                   '3203'.                
006500                                                                          
006600*01  -COPY WWPRODSL                                                       
006700                                                                          
006800     EJECT                                                                
006900 01  DATUMKORT.                                                           
007000   03  FILLER                    PIC X(16)   VALUE                        
007100                                            'DATUMKORT       '.           
007200   03  WS-AAVV.                                                           
007300     05  WS-AA                   PIC 9(2)    VALUE ZERO.                  
007400     05  WS-VV                   PIC 9(2)    VALUE ZERO.                  
007500   03  WS-AAVV-N REDEFINES WS-AAVV                                        
007600                                 PIC 9(4).                                
007700   03  WS-DAGENS-AAVV.                                                    
007800     05  WS-DAGENS-AA            PIC 9(2)    VALUE ZERO.                  
007900     05  WS-DAGENS-VV            PIC 9(2)    VALUE ZERO.                  
008000   03  WS-DAGENS-AAVV-N REDEFINES WS-DAGENS-AAVV                          
008100                                 PIC 9(4).                                
008200   03  WS-DAGENS-AAVV-MINUS-2-AA.                                         
008300     05  WS-DAGENS-AA-MINUS-2-AA PIC 9(2)    VALUE ZERO.                  
008400     05  WS-DAGENS-VV-MINUS-2-AA PIC 9(2)    VALUE ZERO.                  
008500   03  WS-DAGENS-AAVV-MINUS-2-AA-N REDEFINES                              
008600       WS-DAGENS-AAVV-MINUS-2-AA PIC 9(4).                                
008700*03 -COPY WDATAREA                                                        
008800     EJECT                                                                
008900 01  DYNAMISKA-SUBPROGRAM.                                                
009000   03  WDATKONV                  PIC X(8) VALUE 'WDATKONV'.               
009100     EJECT                                                                
009200 01  NYCKLAR-TILL-DLI.                                                    
009300   03  FILLER                    PIC X(16)   VALUE                        
009400                                            'NYCKLAR-TILL-DLI'.           
009500   03  W-WDM301KY-SPAR.                                                   
009600     05  W-IDUSER-SPAR           PIC  X(8)   VALUE SPACE.                 
009700     05  W-DAREGDAT-SPAR         PIC 9(8)    VALUE ZERO.                  
009800     05  W-TIREGTID-SPAR         PIC S9(7)   VALUE ZERO  COMP-3.          
009900   03  W-WDM301KY-X.                                                      
010000     05  W-IDUSER                PIC  X(8)   VALUE SPACE.                 
010100     05  W-DAREGDAT              PIC 9(8)    VALUE ZERO.                  
010200     05  W-TIREGTID              PIC S9(7)   VALUE ZERO  COMP-3.          
010300   03  W-WDM3A1KY-MIN.                                                    
010400     05  W-IDUSER-MIN            PIC  X(8)   VALUE SPACE.                 
010500     05  W-IDFSGURV-MIN          PIC  X(8)   VALUE SPACE.                 
010600     05  W-IDTRANS-MIN           PIC  X(4)   VALUE SPACE.                 
010700     05  FILLER                  PIC  X(12)   VALUE LOW-VALUE.            
010800   03  W-WDM3A1KY-MAX.                                                    
010900     05  W-IDUSER-MAX            PIC  X(8)   VALUE SPACE.                 
011000     05  W-IDFSGURV-MAX          PIC  X(8)   VALUE SPACE.                 
011100     05  W-IDTRANS-MAX           PIC  X(4)   VALUE SPACE.                 
011200     05  FILLER                  PIC  X(12)   VALUE HIGH-VALUE.           
011300   03  W-KDSEGKEY-X.                                                      
011400     05  W-KDSEGKEY              PIC  X(1)   VALUE '1'.                   
011500   03  W-IDFKNGRP-X.                                                      
011600     05  W-IDFKNGRP-FOM          PIC  S9(5)  VALUE ZERO  COMP-3.          
011700     05  W-IDFKNGRP-TOM          PIC  S9(5)  VALUE ZERO  COMP-3.          
011800   03  W-IDFKNGRP-SPAR-X.                                                 
011900     05  FILLER                  PIC  X(3)  VALUE LOW-VALUE.              
012000     05  FILLER                  PIC  X(3)  VALUE HIGH-VALUE.             
012100     EJECT                                                                
012200 01  DYNAMISKA-SUBPROGRAM.                                                
012300   03  FELLOG                    PIC X(8)    VALUE 'FELLOG  '.            
012400   03  CBLTDLI                   PIC X(8)    VALUE 'CBLTDLI '.            
012500   03  WMEDKONV                  PIC X(8)    VALUE 'WMEDKONV'.            
012600                                                                          
012700*01 -COPY WMEDAREA                                                        
012800*01  MEDDELANDE.                                                          
012900*  03  FILLER                    PIC X(16)   VALUE 'MEDDELANDE'.          
013000*  03  FEL1.                                                              
013100*    05 FILLER                   PIC X(40)                                
013200*         VALUE 'UPPLYSTA FÄLT FEL'.                                      
013300*    05 FILLER                   PIC X(40)                                
013400*         VALUE 'HIGHLIGHTED FIELDS ARE WRONG'.                           
013500*  03  FILLER REDEFINES FEL1.                                             
013600*    05  FEL-1                   PIC X(40)   OCCURS 2.                    
013700*                                                                         
013800*  03  FEL2.                                                              
013900*    05 FILLER                   PIC X(40)                                
014000*         VALUE 'KONFLIKT                                '.               
014100*    05 FILLER                   PIC X(40)                                
014200*         VALUE 'CONFLICT                                '.               
014300*  03  FILLER REDEFINES FEL2.                                             
014400*    05  FEL-2                   PIC X(40)   OCCURS 2.                    
014500*                                                                         
014600*  03  FEL3.                                                              
014700*    05 FILLER                   PIC X(40)                                
014800*         VALUE 'TRYCK PF11 VID UPPDATERING'.                             
014900*    05 FILLER                   PIC X(40)                                
015000*         VALUE 'PRESS PF11 WHEN UPDATE'.                                 
015100*  03  FILLER REDEFINES FEL3.                                             
015200*    05  FEL-3                   PIC X(40)   OCCURS 2.                    
015300*                                                                         
015400*  03  FEL4.                                                              
015500*    05 FILLER                   PIC X(40)                                
015600*         VALUE 'ANGE ETT AV NEDANSTÅENDE URVAL'.                         
015700*    05 FILLER                   PIC X(40)                                
015800*         VALUE 'SPECIFY ONE CHOISE (SEE BELOW)'.                         
015900*  03  FILLER REDEFINES FEL4.                                             
016000*    05  FEL-4                   PIC X(40)   OCCURS 2.                    
016100*                                                                         
016200*  03  FEL5.                                                              
016300*    05 FILLER                   PIC X(40)                                
016400*         VALUE 'URVAL SAKNAS          '.                                 
016500*    05 FILLER                   PIC X(40)                                
016600*         VALUE 'KEYS ARE MISSING  '.                                     
016700*  03  FILLER REDEFINES FEL5.                                             
016800*    05  FEL-5                   PIC X(40)   OCCURS 2.                    
016900*   SKIP1                                                                 
017000*                                                                         
017100*  03  FEL6.                                                              
017200*    05 FILLER                   PIC X(40)                                
017300*         VALUE 'DETTA ÄR FÖRSTA SIDAN '.                                 
017400*    05 FILLER                   PIC X(40)                                
017500*         VALUE 'THIS IS THE FIRST PAGE'.                                 
017600*  03  FILLER REDEFINES FEL6.                                             
017700*    05  FEL-6                   PIC X(40)   OCCURS 2.                    
017800*   SKIP1                                                                 
017900*                                                                         
018000*  03  MED1.                                                              
018100*    05 FILLER                   PIC X(40)                                
018200*         VALUE 'UPPDATERING GJORD     '.                                 
018300*    05 FILLER                   PIC X(40)                                
018400*         VALUE 'FIELDS ARE UPDATED'.                                     
018500*  03  FILLER REDEFINES MED1.                                             
018600*    05  MED-1                   PIC X(40)   OCCURS 2.                    
018700*   SKIP1                                                                 
018800*  03  MED2.                                                              
018900*    05 FILLER                   PIC X(60)                                
019000*        VALUE 'KONCERN             DISTRIKT             MARKNAD'.        
019100*    05 FILLER                   PIC X(60)                                
019200*         VALUE '                                        '.               
019300*  03  FILLER REDEFINES MED2.                                             
019400*    05  MED-2                   PIC X(60)   OCCURS 2.                    
019500*   SKIP1                                                                 
019600*  03  MED3.                                                              
019700*    05 FILLER                   PIC X(60)                                
019800*        VALUE 'FUNKTIONSGRUPP SAKNAS PÅ BASEN           '.               
019900*    05 FILLER                   PIC X(60)                                
020000*         VALUE ' MISSING ON THE BASE                    '.               
020100*  03  FILLER REDEFINES MED3.                                             
020200*    05  MED-3                   PIC X(60)   OCCURS 2.                    
020300*   SKIP1                                                                 
020400*  03  MED4.                                                              
020500*    05 FILLER                   PIC X(60)                                
020600*        VALUE 'FLER SIDOR FINNS                                '.        
020700*    05 FILLER                   PIC X(60)                                
020800*         VALUE 'THERE ARE MORE SIDES                    '.               
020900*  03  FILLER REDEFINES MED4.                                             
021000*    05  MED-4                   PIC X(60)   OCCURS 2.                    
021100                                                                          
021200   03  MED5.                                                              
021300     05 FILLER                   PIC X(60)                                
021400         VALUE 'MINST EN FUNKTIONSGRP   MÅSTE FINNAS KVAR       '.        
021500     05 FILLER                   PIC X(60)                                
021600          VALUE 'YOU ARE TRYING TO DELETE TOO MANY PARTS '.               
021700   03  FILLER REDEFINES MED5.                                             
021800     05  MED-5                   PIC X(60)   OCCURS 2.                    
021900                                                                          
022000   03  MED6.                                                              
022100     05 FILLER                   PIC X(60)                                
022200         VALUE 'MINST ETT PRODUKTSLAG MÅSTE ANGES               '.        
022300     05 FILLER                   PIC X(60)                                
022400          VALUE 'AT LEAST ONE PRODSL MUST BE GIVEN       '.               
022500   03  FILLER REDEFINES MED6.                                             
022600     05  MED-6                   PIC X(60)   OCCURS 2.                    
022700                                                                          
022800   03  MED7.                                                              
022900     05 FILLER                   PIC X(60)                                
023000         VALUE 'BORTTAG OCH UPPDATERING SAMTIDIGT               '.        
023100     05 FILLER                   PIC X(60)                                
023200          VALUE 'DELETE AND UPDATE AT SAME TIME          '.               
023300   03  FILLER REDEFINES MED7.                                             
023400     05  MED-7                   PIC X(60)   OCCURS 2.                    
023500                                                                          
023600   03  MED8.                                                              
023700     05 FILLER                   PIC X(60)                                
023800         VALUE 'ENBART URVAL UNDER EGET USERID FÅR TAS BORT     '.        
023900     05 FILLER                   PIC X(60)                                
024000          VALUE 'YOU CAN NOT DELETE OTHERS USERID S      '.               
024100   03  FILLER REDEFINES MED8.                                             
024200     05  MED-8                   PIC X(60)   OCCURS 2.                    
024300                                                                          
024400     EJECT                                                                
024500******************************************************************        
024600*                                                                         
024700*                AREOR FÖR MFS OCH SKÄRMHANTERING                         
024800*                                                                         
024900     SKIP3                                                                
025000 01  FILLER                      PIC X(16)   VALUE 'MID-COPY-WS'.         
025100*01  MID -COPY W3I20201                                                   
025200     EJECT                                                                
025300 01  FILLER                      PIC X(16)   VALUE 'MSG-COPY-WS'.         
025400*01  -COPY WMSGAREA                                                       
025500     EJECT                                                                
025600*  03  MOD -COPY W3O20201           -RED MSG-AREA.                        
025700     EJECT                                                                
025800 01  FILLER                      PIC X(16)   VALUE 'MFS-COPY-WS'.         
025900*01  -COPY WMFSAREA                                                       
026000     EJECT                                                                
026100*01  WLFSGA01    -COPY WDM301 -PRE WS-.                                   
026200     EJECT                                                                
026300*01  WLFSGA11    -COPY WDM311 -PRE WS-.                                   
026400     EJECT                                                                
026500*01  WLFSGA12    -COPY WDM312 -PRE WS-.                                   
026600     EJECT                                                                
026700******************************************************************        
026800*                                                                         
026900*        ARBETS-AREOR TILL IMS-SEKTIONERNA                                
027000*                                                                         
027100 01  IMS-WS.                                                              
027200   03  FILLER                    PIC X(16)   VALUE 'IMS-WS     '.         
027300     SKIP3                                                                
027400*                        **** STATUS-KOD FRÅN IMS                         
027500   03  STATUS-WS                 PIC XX.                                  
027600     88  SEGMENT-FINNS                       VALUE '  '.                  
027700     88  SEGMENT-SAKNAS                      VALUE 'GE' 'GB'.             
027800     SKIP3                                                                
027900   03  GODK-STATUSKODER.                                                  
028000     05  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
028100     SKIP3                                                                
028200 01    SSA1                      PIC X(128).                              
028300 01    SSA2                      PIC X(64).                               
028400     EJECT                                                                
028500*                            IMS FUNKTIONSKODER                           
028600*01    -COPY W0003                                                        
028700     EJECT                                                                
028800************************     DLI INPUT-OUTPUT AREA ***************        
028900 01  FILLER                    PIC X(16)   VALUE 'DLI-IO-AREA'.           
029000 01  DLI-IO-AREA.                                                         
029100   03  IO-AREA                   PIC X(200)  VALUE SPACE.                 
029200     SKIP3                                                                
029300*03  WLFSGA01    -COPY WDM301   -RED IO-AREA.                             
029400     EJECT                                                                
029500*03  WLFSGA11    -COPY WDM311   -RED IO-AREA.                             
029600     EJECT                                                                
029700*03  WLFSGA12    -COPY WDM312   -RED IO-AREA.                             
029800     EJECT                                                                
029900*03  WLFSGB01    -COPY WDM3A1   -RED IO-AREA.                             
030000     EJECT                                                                
030100 01  FILLER                    PIC X(16)   VALUE 'DLI-IO-AREA-2'.         
030200 01  DLI-IO-AREA-2.                                                       
030300   03  IO-AREA-2                 PIC X(100)  VALUE SPACE.                 
030400     SKIP3                                                                
030500*03  WLFSGA01    -COPY WDM301   -PRE SPAR- -RED IO-AREA-2.                
030600     EJECT                                                                
030700*03  WLFSGA12    -COPY WDM312   -PRE SPAR- -RED IO-AREA-2.                
030800     EJECT                                                                
030900 LINKAGE SECTION.                                                         
031000*01  -COPY W0009     -PRE MSG-                                            
031100     SKIP2                                                                
031200*01  -COPY W0008     -PRE FSGA-                                           
031300     05  FILLER                  PIC X.                                   
031400     SKIP2                                                                
031500*01  -COPY W0008     -PRE FSGA1-                                          
031600     05  FILLER                  PIC X.                                   
031700     SKIP2                                                                
031800*01  -COPY W0008     -PRE FSGB-                                           
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
034100*                MOVE FEL-1   (SPRAK-IX)  TO MOD-TEMFSFEL                 
034200                 MOVE '001'               TO MED-IDMFSFEL                 
034300                 CALL WMEDKONV            USING MED-WMEDAREA              
034400                 MOVE MED-MFSFEL          TO MOD-TEMFSFEL                 
034500                 PERFORM F-MFS-ROER-EJ-FAELT                              
034600              END-IF                                                      
034700           ELSE                                                           
034800              MOVE NEJ                        TO SW-IFYLLT                
034900              IF MFS-IDPFK = '7'                                          
035000              OR MID-IDFKNGRP-FOM-HI = +9999                              
035100                 MOVE ZERO                TO W-IDFKNGRP-FOM               
035200              ELSE                                                        
035300                 PERFORM S09-KOLLA-ATT-INGET-IFYLLT                       
035400              END-IF                                                      
035500              IF INGET-IFYLLT                                             
035600                IF MID-IDFSGURV-IN = ALL '+'                              
035700                   PERFORM IMS-GU-FSGA01                                  
035800                   IF SEGMENT-FINNS                                       
035900                      PERFORM S07-VISA-SIDAN                              
036000                   ELSE                                                   
036100                      PERFORM H-LAES-FSGB-FSGA                            
036200                   END-IF                                                 
036300                ELSE                                                      
036400                   PERFORM H-LAES-FSGB-FSGA                               
036500                END-IF                                                    
036600              ELSE                                                        
036700*                MOVE FEL-3 (SPRAK-IX)       TO MOD-TEMFSFEL              
036800                 MOVE '003'                  TO MED-IDMFSFEL              
036900                 CALL WMEDKONV        USING MED-WMEDAREA                  
037000                 MOVE MED-MFSFEL             TO MOD-TEMFSFEL              
037100                 PERFORM S10-LAES-IN-IGEN                                 
037200                 PERFORM S06-MFS-ROER-EJ-FAELT                            
037300              END-IF                                                      
037400           END-IF                                                         
037500        ELSE                                                              
037600           MOVE MFS-RENSA-FAELT            TO MOD-IDFSGURV-UT             
037700                                              MOD-IDUSER-UT               
037800           PERFORM S01-RENSA-HELA-SIDAN                                   
037900        END-IF                                                            
038000        MOVE MAX-MOD-LAENGD TO MSG-KVLL                                   
038100        PERFORM IMS-INSERT-MSG                                            
038200     END-IF                                                               
038300     MOVE ZERO                                  TO RETURN-CODE            
038400     GOBACK                                                               
038500     .                                                                    
038600     EJECT                                                                
038700 A-INIT SECTION.                                                          
038800     SKIP2                                                                
038900     IF MSG-DUBBLA-TRANSKODER                                             
039000        MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W3I20201                
039100        MOVE MSG-IDTRANS-2                 TO MFS-IDTRANS                 
039200        MOVE MSG-KDMFSFOR-2                TO MFS-KDMFSFOR                
039300        MOVE MSG-KDTRTYP                   TO MFS-KDTRTYP                 
039400        MOVE MSG-IDPFK                     TO MFS-IDPFK                   
039500     ELSE                                                                 
039600        MOVE MSG-INDATA-MINUS-1-TRANSKOD   TO MID-W3I20201                
039700        MOVE MSG-IDTRANS-1                 TO MFS-IDTRANS                 
039800        MOVE MSG-KDMFSFOR-1                TO MFS-KDMFSFOR                
039900        MOVE SPACE                         TO MFS-KDTRTYP                 
040000                                              MFS-IDPFK                   
040100     END-IF                                                               
040200                                                                          
040300     MOVE LOW-VALUE                        TO MSG-AREA                    
040400     MOVE 'W3O202N1'                       TO MFS-IDMOD                   
040500     MOVE '3202'                           TO MOD-IDTRANS                 
040600     MOVE MFS-IDTRANS                      TO WS-IDTRANS                  
040700     MOVE MFS-RENSA-FAELT                  TO MOD-TEMFSFEL                
040800                                              MOD-TEMFSINF                
040900                                              MOD-IDFSGURV-IN             
041000                                              MOD-IDUSER-IN               
041100*                                                                         
041200     IF EGEN-BILD                                                         
041300        CONTINUE                                                          
041400     ELSE                                                                 
041500        MOVE SPACE                         TO MFS-KDTRTYP                 
041600        MOVE '7'                           TO MFS-IDPFK                   
041700     END-IF                                                               
041800     IF ENGLISH-TEXT                                                      
041900        MOVE +2                            TO SPRAK-IX                    
042000        MOVE 'GB '                         TO MED-IDSKYLT                 
042100     ELSE                                                                 
042200        MOVE +1                            TO SPRAK-IX                    
042300        MOVE 'S  '                         TO MED-IDSKYLT                 
042400     END-IF                                                               
042500     MOVE 'AAMMDD'                         TO DAT-KDDATFORM               
042600     ACCEPT DAT-I-TIDATUM FROM DATE                                       
042700     PERFORM S99-CALL-WDATKONV                                            
042800     IF DAT-KDSVAR-OK                                                     
042900        MOVE DAT-TIAA-VECKA                 TO WS-DAGENS-AA               
043000        MOVE DAT-TIVV                       TO WS-DAGENS-VV               
043100        MOVE WS-DAGENS-AAVV                 TO                            
043200                                   WS-DAGENS-AAVV-MINUS-2-AA              
043300        IF WS-DAGENS-AA-MINUS-2-AA = 00                                   
043400          MOVE 98 TO WS-DAGENS-AA-MINUS-2-AA                              
043500        ELSE                                                              
043600          IF WS-DAGENS-AA-MINUS-2-AA = 01                                 
043700            MOVE 99 TO WS-DAGENS-AA-MINUS-2-AA                            
043800          ELSE                                                            
043900            SUBTRACT 2 FROM WS-DAGENS-AA-MINUS-2-AA                       
044000          END-IF                                                          
044100        END-IF                                                            
044200     END-IF                                                               
044300     INITIALIZE WS-WLFSGA11                                               
044400     .                                                                    
044500     EJECT                                                                
044600 B-GOR-IORDN-NYCK-PFTRY SECTION.                                          
044700     SKIP3                                                                
044800     IF MID-IDFSGURV-IN = ALL '+'                                         
044900        MOVE MID-IDFSGURV-UT         TO WS-IDFSGURV                       
045000     ELSE                                                                 
045100        MOVE MID-IDFSGURV-IN         TO WS-IDFSGURV                       
045200        MOVE SPACE                   TO MFS-KDTRTYP                       
045300        MOVE '7'                     TO MFS-IDPFK                         
045400     END-IF                                                               
045500     MOVE WS-IDFSGURV                TO MOD-IDFSGURV-UT                   
045600                                        W-IDFSGURV-MIN                    
045700                                        W-IDFSGURV-MAX                    
045800     IF MFS-UPDATE                                                        
045900        MOVE MID-IDUSER-UT           TO W-IDUSER                          
046000     ELSE                                                                 
046100        IF MID-IDUSER-IN = ALL '+'                                        
046200           MOVE MID-IDUSER-UT        TO W-IDUSER                          
046300        ELSE                                                              
046400           MOVE MID-IDUSER-IN        TO W-IDUSER                          
046500           MOVE '7'                  TO MFS-IDPFK                         
046600        END-IF                                                            
046700     END-IF                                                               
046800     IF W-IDUSER = SPACE                                                  
046900        MOVE MSG-SIGNON-USERID       TO W-IDUSER                          
047000     END-IF                                                               
047100     MOVE W-IDUSER                   TO MOD-IDUSER-UT                     
047200                                        W-IDUSER-MIN                      
047300                                        W-IDUSER-MAX                      
047400     MOVE '3202'                     TO W-IDTRANS-MIN                     
047500                                        W-IDTRANS-MAX                     
047600     PERFORM BA-KOLLA-PF-TRYCK                                            
047700     .                                                                    
047800     EJECT                                                                
047900 BA-KOLLA-PF-TRYCK  SECTION.                                              
048000     SKIP2                                                                
048100* TIREGTID OCH DAREGDAT    DOLDA    FÄLT GES VÄRDE I A-INIT               
048200* D-VISA SAMT E-UPPDATERA                                                 
048300     IF  MID-IDFKNGRP-FOM-LO NUMERIC                                      
048400     AND MID-IDFKNGRP-TOM-LO NUMERIC                                      
048500     AND MID-IDFKNGRP-FOM-HI NUMERIC                                      
048600     AND MID-IDFKNGRP-TOM-HI NUMERIC                                      
048700     AND MID-DAREGDAT-DOLD NUMERIC                                        
048800     AND MID-TIREGTID-DOLD NUMERIC                                        
048900        MOVE MID-DAREGDAT-DOLD         TO W-DAREGDAT                      
049000*---Y2K-FIX********                                                       
049100        IF MID-DAREGDAT-DOLD NOT = ZERO                                   
049200          IF MID-DAREGDAT-DOLD < 1000000                                  
049300*           MOVE 20         TO W-DAREGDAT(1:2)                            
049400*         ELSE                                                            
049500*           IF MID-DAREGDAT-DOLD < 999999                                 
049600              MOVE 19       TO W-DAREGDAT(1:2)                            
049700*           ELSE                                                          
049800*             MOVE 99999999 TO W-DAREGDAT                                 
049900*           END-IF                                                        
050000          END-IF                                                          
050100        END-IF                                                            
050200        MOVE MID-TIREGTID-DOLD         TO W-TIREGTID                      
050300     ELSE                                                                 
050400        MOVE '7'                       TO MFS-IDPFK                       
050500        MOVE SPACE                     TO MFS-KDTRTYP                     
050600        MOVE ZERO                      TO W-TIREGTID                      
050700                                          W-DAREGDAT                      
050800     END-IF                                                               
050900     IF MFS-IDPFK = '8'                                                   
051000     AND MID-IDFKNGRP-FOM-HI > ZERO                                       
051100        MOVE MID-IDFKNGRP-FOM-HI       TO W-IDFKNGRP-FOM                  
051200        MOVE MID-IDFKNGRP-TOM-HI       TO W-IDFKNGRP-TOM                  
051300     ELSE                                                                 
051400        IF MFS-IDPFK = ' '                                                
051500           MOVE MID-IDFKNGRP-FOM-LO    TO W-IDFKNGRP-FOM                  
051600           MOVE MID-IDFKNGRP-TOM-LO    TO W-IDFKNGRP-TOM                  
051700        ELSE                                                              
051800*          MOVE FEL-6 (SPRAK-IX)       TO MOD-TEMFSFEL                    
051900           MOVE '006'                  TO MED-IDMFSFEL                    
052000           CALL WMEDKONV        USING MED-WMEDAREA                        
052100           MOVE MED-MFSFEL             TO MOD-TEMFSFEL                    
052200           MOVE +99999                 TO W-IDFKNGRP-TOM                  
052300        END-IF                                                            
052400     END-IF                                                               
052500     .                                                                    
052600     EJECT                                                                
052700 C-KOLLA-INDATA SECTION.                                                  
052800     SKIP2                                                                
052900     MOVE JA                             TO SW-INDATA-OK                  
053000     IF MID-KDBORT = 'B' OR 'D' OR 'J'                                    
053100        MOVE MFS-ALFA-FAELT-RAETT        TO MOD-KDBORT-ATTR               
053200     ELSE                                                                 
053300        PERFORM CA-KOLLA-VECKA                                            
053400        PERFORM CC-KOLLA-IDFKNGRP                                         
053500* SW-KONFLIKT HÅLLER REDA PÅ ATT ENBART ETT URVAL AV                      
053600* KONCNR KDMARK OCH IDDISTR ANGES                                         
053700* OBS! FEL-FLAGGAN TÄNDS EJ UTAN BEHANDLAS SOM FEL I HUVUDSLINGAN         
053800        MOVE NEJ                     TO SW-PRODSL                         
053900        MOVE NEJ                     TO SW-KONFLIKT                       
054000        MOVE NEJ                     TO SW-KONCERN                        
054100        MOVE NEJ                     TO SW-MARKNAD                        
054200        MOVE NEJ                     TO SW-DISTRIKT                       
054300        MOVE +1                      TO RAD-INDX                          
054400        PERFORM UNTIL RAD-INDX > 8                                        
054500           IF RAD-INDX > 7                                                
054600              PERFORM CD-KOLLA-IDKONCNR                                   
054700              PERFORM CE-KOLLA-IDLEVNR                                    
054800              PERFORM CF-KOLLA-IDLKTO                                     
054900              PERFORM CI-KOLLA-KDMARK                                     
055000              PERFORM S08-KOLLA-IDFKNGRP                                  
055100           ELSE                                                           
055200              IF RAD-INDX > 5                                             
055300                 PERFORM CD-KOLLA-IDKONCNR                                
055400                 PERFORM CE-KOLLA-IDLEVNR                                 
055500                 PERFORM CF-KOLLA-IDLKTO                                  
055600                 PERFORM CH-KOLLA-KDPRODSL                                
055700                 PERFORM CI-KOLLA-KDMARK                                  
055800                 PERFORM S08-KOLLA-IDFKNGRP                               
055900              ELSE                                                        
056000                 IF RAD-INDX > 4                                          
056100                    PERFORM CD-KOLLA-IDKONCNR                             
056200                    PERFORM CE-KOLLA-IDLEVNR                              
056300                    PERFORM CF-KOLLA-IDLKTO                               
056400                    PERFORM CG-KOLLA-KDVVKL                               
056500                    PERFORM CH-KOLLA-KDPRODSL                             
056600                    PERFORM CI-KOLLA-KDMARK                               
056700                    PERFORM S08-KOLLA-IDFKNGRP                            
056800                 ELSE                                                     
056900                    PERFORM CD-KOLLA-IDKONCNR                             
057000                    PERFORM CE-KOLLA-IDLEVNR                              
057100                    PERFORM CF-KOLLA-IDLKTO                               
057200                    PERFORM CG-KOLLA-KDVVKL                               
057300                    PERFORM CH-KOLLA-KDPRODSL                             
057400                    PERFORM CI-KOLLA-KDMARK                               
057500                    PERFORM CJ-KOLLA-IDDISTR                              
057600                    PERFORM CK-KOLLA-IDANSK                               
057700                    PERFORM S08-KOLLA-IDFKNGRP                            
057800                 END-IF                                                   
057900              END-IF                                                      
058000           END-IF                                                         
058100           ADD  +1                      TO RAD-INDX                       
058200        END-PERFORM                                                       
058300        PERFORM CB-KOLLA-SPAR-BORT-LIST-PRIS                              
058400     END-IF                                                               
058500     .                                                                    
058600     EJECT                                                                
058700 CA-KOLLA-VECKA      SECTION.                                             
058800     SKIP2                                                                
058900     IF  MID-TIFSGVV-FOM = ALL '+'                                        
059000     AND MID-TIFSGVV-TOM = ALL '+'                                        
059100        CONTINUE                                                          
059200     ELSE                                                                 
059300        IF  MID-TIFSGVV-FOM NUMERIC                                       
059400        AND MID-TIFSGVV-TOM NUMERIC                                       
059500           MOVE MID-TIFSGVV-TOM   TO TMP1-YYWW                            
059600           MOVE MID-TIFSGVV-FOM   TO TMP2-YYWW                            
059700           PERFORM WY2000P3                                               
059800           IF  TMP1-YYWW >= TMP2-YYWW                                     
059900              MOVE 'AAVV  '             TO DAT-KDDATFORM                  
060000              MOVE MID-TIFSGVV-FOM      TO DAT-I-TIDATUM                  
060100              PERFORM S99-CALL-WDATKONV                                   
060200              IF DAT-KDSVAR-OK                                            
060300                 MOVE DAT-TIAA-VECKA        TO WS-AA                      
060400                 MOVE DAT-TIVV              TO WS-VV                      
060500                 MOVE WS-AAVV-N                   TO TMP1-YYWW            
060600                 MOVE WS-DAGENS-AAVV-MINUS-2-AA-N TO TMP2-YYWW            
060700                 PERFORM WY2000P3                                         
060800                 IF TMP1-YYWW >= TMP2-YYWW                                
060900                    MOVE MFS-NUM-FAELT-RAETT  TO                          
061000                                          MOD-TIFSGVV-FOM-ATTR            
061100                    MOVE MID-TIFSGVV-FOM      TO                          
061200                                  WS-URV1-TIFSGVV-FOM                     
061300                 ELSE                                                     
061400                    MOVE NEJ                  TO SW-INDATA-OK             
061500                    MOVE MFS-NUM-FAELT-FEL    TO                          
061600                                          MOD-TIFSGVV-FOM-ATTR            
061700                 END-IF                                                   
061800              ELSE                                                        
061900                 MOVE NEJ                  TO SW-INDATA-OK                
062000                 MOVE MFS-NUM-FAELT-FEL    TO                             
062100                                       MOD-TIFSGVV-FOM-ATTR               
062200              END-IF                                                      
062300              MOVE 'AAVV  '             TO DAT-KDDATFORM                  
062400              MOVE MID-TIFSGVV-TOM      TO DAT-I-TIDATUM                  
062500              PERFORM S99-CALL-WDATKONV                                   
062600              IF DAT-KDSVAR-OK                                            
062700                 MOVE DAT-TIAA-VECKA        TO WS-AA                      
062800                 MOVE DAT-TIVV              TO WS-VV                      
062900                 MOVE WS-AAVV-N        TO TMP1-YYWW                       
063000                 MOVE WS-DAGENS-AAVV-N TO TMP2-YYWW                       
063100                 PERFORM WY2000P3                                         
063200                 IF TMP1-YYWW < TMP2-YYWW                                 
063300                    MOVE MFS-NUM-FAELT-RAETT  TO                          
063400                                          MOD-TIFSGVV-TOM-ATTR            
063500                    MOVE MID-TIFSGVV-TOM      TO                          
063600                                  WS-URV1-TIFSGVV-TOM                     
063700                 ELSE                                                     
063800                    IF MID-IDPTYP = 'VP1' OR 'VA1' OR 'VA2'               
063900                    AND WS-AAVV = WS-DAGENS-AAVV                          
064000                       MOVE MID-TIFSGVV-TOM      TO                       
064100                                          WS-URV1-TIFSGVV-TOM             
064200                    ELSE                                                  
064300                       MOVE NEJ                  TO SW-INDATA-OK          
064400                       MOVE MFS-NUM-FAELT-FEL    TO                       
064500                                         MOD-TIFSGVV-TOM-ATTR             
064600                    END-IF                                                
064700                 END-IF                                                   
064800              ELSE                                                        
064900                 MOVE NEJ                  TO SW-INDATA-OK                
065000                 MOVE MFS-NUM-FAELT-FEL    TO                             
065100                                          MOD-TIFSGVV-TOM-ATTR            
065200              END-IF                                                      
065300           ELSE                                                           
065400              MOVE NEJ                  TO SW-INDATA-OK                   
065500              MOVE MFS-NUM-FAELT-FEL    TO MOD-TIFSGVV-FOM-ATTR           
065600                                           MOD-TIFSGVV-TOM-ATTR           
065700           END-IF                                                         
065800        ELSE                                                              
065900           MOVE NEJ                      TO SW-INDATA-OK                  
066000           IF  MID-TIFSGVV-FOM      = ALL '+'                             
066100              MOVE MFS-NUM-FAELT-FEL    TO MOD-TIFSGVV-TOM-ATTR           
066200           ELSE                                                           
066300              IF  MID-TIFSGVV-TOM      = ALL '+'                          
066400                 MOVE MFS-NUM-FAELT-FEL TO MOD-TIFSGVV-FOM-ATTR           
066500              ELSE                                                        
066600                 MOVE MFS-NUM-FAELT-FEL TO MOD-TIFSGVV-FOM-ATTR           
066700                                           MOD-TIFSGVV-TOM-ATTR           
066800              END-IF                                                      
066900           END-IF                                                         
067000        END-IF                                                            
067100     END-IF                                                               
067200     .                                                                    
067300     EJECT                                                                
067400 CB-KOLLA-SPAR-BORT-LIST-PRIS   SECTION.                                  
067500     SKIP2                                                                
067600     IF MID-IDFSGURV = ALL '+'                                            
067700        CONTINUE                                                          
067800     ELSE                                                                 
067900        MOVE MFS-ALFA-FAELT-RAETT        TO MOD-IDFSGURV-ATTR             
068000     END-IF                                                               
068100     IF MID-KDBORT   = ALL '+'                                            
068200        CONTINUE                                                          
068300     ELSE                                                                 
068400        MOVE NEJ                         TO SW-INDATA-OK                  
068500        MOVE MFS-ALFA-FAELT-FEL          TO MOD-KDBORT-ATTR               
068600     END-IF                                                               
068700     IF MID-KDCMD    = ALL '+'                                            
068800        CONTINUE                                                          
068900     ELSE                                                                 
069000        MOVE NEJ                         TO SW-INDATA-OK                  
069100        MOVE MFS-ALFA-FAELT-FEL          TO MOD-KDCMD-ATTR                
069200     END-IF                                                               
069300     IF MID-IDPTYP = ALL '+'                                              
069400        MOVE NEJ                         TO SW-INDATA-OK                  
069500        MOVE MFS-ALFA-FAELT-FEL          TO MOD-IDPTYP-ATTR               
069600     ELSE                                                                 
069700        IF MID-IDPTYP = 'P1' OR 'A1' OR 'A2'                              
069800                                                                          
069900                                                                          
070000                                                                          
070100                                                                          
070200           IF  MID-TIFSGVV-FOM = ALL '+'                                  
070300              MOVE MFS-ALFA-FAELT-RAETT     TO MOD-IDPTYP-ATTR            
070400              MOVE MID-IDPTYP               TO WS-URV1-IDPTYP             
070500           ELSE                                                           
070600              MOVE NEJ                      TO SW-INDATA-OK               
070700              MOVE MFS-ALFA-FAELT-FEL       TO MOD-IDPTYP-ATTR            
070800           END-IF                                                         
070900        ELSE                                                              
071000           IF MID-IDPTYP = 'VP1' OR 'VA1' OR 'VA2'                        
071100              IF  MID-TIFSGVV-FOM = ALL '+'                               
071200                 MOVE NEJ                   TO SW-INDATA-OK               
071300                 MOVE MFS-ALFA-FAELT-FEL    TO MOD-IDPTYP-ATTR            
071400              ELSE                                                        
071500                 MOVE MFS-ALFA-FAELT-RAETT  TO MOD-IDPTYP-ATTR            
071600                 MOVE MID-IDPTYP            TO WS-URV1-IDPTYP             
071700              END-IF                                                      
071800           ELSE                                                           
071900                                                                          
072000                                                                          
072100                                                                          
072200                                                                          
072300             IF MID-IDPTYP = 'PPV'                                        
072400               AND KONCERN-EJ-IFYLLD AND DISTRIKT-EJ-IFYLLD               
072500               AND MARKNAD-EJ-IFYLLD                                      
072600               AND MID-TIFSGVV-FOM = ALL '+'                              
072700                MOVE MFS-ALFA-FAELT-RAETT     TO MOD-IDPTYP-ATTR          
072800                MOVE MID-IDPTYP               TO WS-URV1-IDPTYP           
072900             ELSE                                                         
073000                MOVE NEJ                      TO SW-INDATA-OK             
073100                MOVE MFS-ALFA-FAELT-FEL       TO MOD-IDPTYP-ATTR          
073200             END-IF                                                       
073300           END-IF                                                         
073400        END-IF                                                            
073500        IF MID-IDPTYP = 'A1' OR 'A2' OR 'VA1' OR 'VA2'                    
073600           IF SW-PRODSL = NEJ                                             
073700              MOVE MFS-ALFA-FAELT-FEL       TO MOD-IDPTYP-ATTR            
073800              MOVE NEJ                      TO SW-INDATA-OK               
073900              MOVE MED-6 (SPRAK-IX) TO MOD-TEMFSINF                       
074000           END-IF                                                         
074100        END-IF                                                            
074200     END-IF                                                               
074300     IF MID-KDPRTYPG = ALL '+'                                            
074400        CONTINUE                                                          
074500     ELSE                                                                 
074600        IF MID-KDPRTYPG = 'S' OR 'F' OR 'M' OR 'K' OR 'R' OR '-'          
074700           IF  MID-TIFSGVV-FOM = ALL '+'                                  
074800              MOVE NEJ                      TO SW-INDATA-OK               
074900              MOVE MFS-ALFA-FAELT-FEL       TO                            
075000                                             MOD-KDPRTYPG-ATTR            
075100           ELSE                                                           
075200              MOVE MID-KDPRTYPG             TO WS-URV1-KDPRTYP            
075300              MOVE MFS-ALFA-FAELT-RAETT     TO                            
075400                                             MOD-KDPRTYPG-ATTR            
075500           END-IF                                                         
075600        ELSE                                                              
075700           MOVE NEJ                      TO SW-INDATA-OK                  
075800           MOVE MFS-ALFA-FAELT-FEL       TO MOD-KDPRTYPG-ATTR             
075900        END-IF                                                            
076000     END-IF                                                               
076100     IF MID-KDNIVA = ALL '+'                                              
076200        MOVE +4                          TO WS-URV1-KDNIVA                
076300     ELSE                                                                 
076400        IF MID-KDNIVA =  0  OR  2  OR  4                                  
076500           MOVE MID-KDNIVA               TO WS-URV1-KDNIVA                
076600           MOVE MFS-ALFA-FAELT-RAETT     TO MOD-KDNIVA-ATTR               
076700        ELSE                                                              
076800           MOVE NEJ                      TO SW-INDATA-OK                  
076900           MOVE MFS-ALFA-FAELT-FEL       TO MOD-KDNIVA-ATTR               
077000        END-IF                                                            
077100     END-IF                                                               
077200     .                                                                    
077300     EJECT                                                                
077400 CC-KOLLA-IDFKNGRP       SECTION.                                         
077500     SKIP2                                                                
077600     IF  MID-IDFKNGRP-FOM (1) = ALL '+'                                   
077700        MOVE NEJ                      TO SW-INDATA-OK                     
077800        MOVE MFS-NUM-FAELT-FEL        TO                                  
077900                        MOD-IDFKNGRP-FOM-ATTR (1)                         
078000                        MOD-IDFKNGRP-TOM-ATTR (1)                         
078100     END-IF                                                               
078200     .                                                                    
078300     EJECT                                                                
078400 CD-KOLLA-IDKONCNR          SECTION.                                      
078500        IF MID-IDKONCNR (RAD-INDX) = ALL '+'                              
078600           CONTINUE                                                       
078700        ELSE                                                              
078800           MOVE JA                       TO SW-KONCERN                    
078900           IF MID-IDKONCNR (RAD-INDX) NUMERIC                             
079000              MOVE MFS-NUM-FAELT-RAETT   TO                               
079100                                    MOD-IDKONCNR-ATTR (RAD-INDX)          
079200              MOVE MID-IDKONCNR (RAD-INDX) TO                             
079300                                     WS-URV1-IDKONCNR (RAD-INDX)          
079400           ELSE                                                           
079500              MOVE NEJ                   TO SW-INDATA-OK                  
079600              MOVE MFS-NUM-FAELT-FEL     TO                               
079700                                    MOD-IDKONCNR-ATTR (RAD-INDX)          
079800           END-IF                                                         
079900        END-IF                                                            
080000     .                                                                    
080100     EJECT                                                                
080200 CE-KOLLA-IDLEVNR           SECTION.                                      
080300        IF MID-IDLEVNR (RAD-INDX) = ALL '+'                               
080400           CONTINUE                                                       
080500        ELSE                                                              
080600              MOVE MFS-ALFA-FAELT-RAETT   TO                              
080700                                    MOD-IDLEVNR-ATTR (RAD-INDX)           
080800              MOVE MID-IDLEVNR (RAD-INDX) TO                              
080900                                     WS-URV1-IDLEVNR (RAD-INDX)           
081000        END-IF                                                            
081100     .                                                                    
081200     EJECT                                                                
081300 CF-KOLLA-IDLKTO            SECTION.                                      
081400        IF MID-IDLKTO (RAD-INDX) = ALL '+'                                
081500           CONTINUE                                                       
081600        ELSE                                                              
081700           IF MID-IDLKTO (RAD-INDX) NUMERIC                               
081800              MOVE MFS-NUM-FAELT-RAETT   TO                               
081900                                    MOD-IDLKTO-ATTR (RAD-INDX)            
082000              MOVE MID-IDLKTO (RAD-INDX) TO                               
082100                                     WS-URV1-IDLKTO (RAD-INDX)            
082200           ELSE                                                           
082300              MOVE NEJ                   TO SW-INDATA-OK                  
082400              MOVE MFS-NUM-FAELT-FEL     TO                               
082500                                    MOD-IDLKTO-ATTR (RAD-INDX)            
082600           END-IF                                                         
082700        END-IF                                                            
082800     .                                                                    
082900     EJECT                                                                
083000 CG-KOLLA-KDVVKL            SECTION.                                      
083100        IF MID-KDVVKL (RAD-INDX) = ALL '+'                                
083200           CONTINUE                                                       
083300        ELSE                                                              
083400           IF MID-KDVVKL (RAD-INDX) NUMERIC                               
083500              MOVE MFS-NUM-FAELT-RAETT   TO                               
083600                                    MOD-KDVVKL-ATTR (RAD-INDX)            
083700              MOVE MID-KDVVKL (RAD-INDX) TO                               
083800                                     WS-URV1-KDVVKL (RAD-INDX)            
083900           ELSE                                                           
084000              MOVE NEJ                   TO SW-INDATA-OK                  
084100              MOVE MFS-NUM-FAELT-FEL     TO                               
084200                                    MOD-KDVVKL-ATTR (RAD-INDX)            
084300           END-IF                                                         
084400        END-IF                                                            
084500     .                                                                    
084600     EJECT                                                                
084700 CH-KOLLA-KDPRODSL          SECTION.                                      
084800*--KDSVAR VISAR OM MAN ÖNSKAR SAMMANSLAGNING AV PS.                       
084900     IF MID-KDSVAR = 'J'                                                  
085000       MOVE MFS-ALFA-FAELT-RAETT TO MOD-KDSVAR-ATTR                       
085100       MOVE MID-KDSVAR          TO WS-URV1-KDSVAR                         
085200     ELSE                                                                 
085300       MOVE MFS-ALFA-FAELT-RAETT TO MOD-KDSVAR-ATTR                       
085400       MOVE 'N'                 TO WS-URV1-KDSVAR                         
085500     END-IF                                                               
085600        IF MID-KDPRODSL (RAD-INDX) = ALL '+'                              
085700           CONTINUE                                                       
085800        ELSE                                                              
085900           IF MID-KDPRODSL (RAD-INDX) NUMERIC                             
086000              MOVE MID-KDPRODSL (RAD-INDX)                                
086100                                 TO TEST-KDPRODSL                         
086200              IF KDPRODSL-VOLVO-BIMA OR                                   
086300                 TEST-KDPRODSL = ZEROES                                   
086400                 MOVE MFS-NUM-FAELT-RAETT   TO                            
086500                                    MOD-KDPRODSL-ATTR (RAD-INDX)          
086600                 MOVE MID-KDPRODSL (RAD-INDX) TO                          
086700                                     WS-URV1-KDPRODSL (RAD-INDX)          
086800                 MOVE JA                       TO SW-PRODSL               
086900              ELSE                                                        
087000                 MOVE NEJ                   TO SW-INDATA-OK               
087100                 MOVE MFS-NUM-FAELT-FEL     TO                            
087200                                    MOD-KDPRODSL-ATTR (RAD-INDX)          
087300              END-IF                                                      
087400           ELSE                                                           
087500              MOVE NEJ                   TO SW-INDATA-OK                  
087600              MOVE MFS-NUM-FAELT-FEL     TO                               
087700                                    MOD-KDPRODSL-ATTR (RAD-INDX)          
087800           END-IF                                                         
087900        END-IF                                                            
088000     .                                                                    
088100     EJECT                                                                
088200 CI-KOLLA-KDMARK            SECTION.                                      
088300     SKIP2                                                                
088400        IF  MID-KDMARK-FOM (RAD-INDX) = ALL '+'                           
088500        AND MID-KDMARK-TOM (RAD-INDX) = ALL '+'                           
088600           CONTINUE                                                       
088700        ELSE                                                              
088800           MOVE JA                        TO SW-MARKNAD                   
088900           IF SW-KONCERN = JA                                             
089000              MOVE JA                     TO SW-KONFLIKT                  
089100           END-IF                                                         
089200           IF  MID-KDMARK-FOM (RAD-INDX) NUMERIC                          
089300           AND MID-KDMARK-FOM (RAD-INDX) > ZERO                           
089400           AND MID-KDMARK-FOM (RAD-INDX) < 100                            
089500           AND MID-KDMARK-TOM (RAD-INDX) NUMERIC                          
089600           AND MID-KDMARK-TOM (RAD-INDX) > ZERO                           
089700           AND MID-KDMARK-TOM (RAD-INDX) < 100                            
089800              IF MID-KDMARK-TOM (RAD-INDX) NOT <                          
089900              MID-KDMARK-FOM (RAD-INDX)                                   
090000                 MOVE MFS-NUM-FAELT-RAETT TO                              
090100                           MOD-KDMARK-FOM-ATTR (RAD-INDX)                 
090200                           MOD-KDMARK-TOM-ATTR (RAD-INDX)                 
090300                 MOVE MID-KDMARK-FOM (RAD-INDX) TO                        
090400                       WS-URV1-KDMARK-FOM (RAD-INDX)                      
090500                 MOVE MID-KDMARK-TOM (RAD-INDX) TO                        
090600                       WS-URV1-KDMARK-TOM (RAD-INDX)                      
090700              ELSE                                                        
090800                 MOVE NEJ                 TO SW-INDATA-OK                 
090900                 MOVE MFS-NUM-FAELT-FEL   TO                              
091000                           MOD-KDMARK-FOM-ATTR (RAD-INDX)                 
091100                           MOD-KDMARK-TOM-ATTR (RAD-INDX)                 
091200              END-IF                                                      
091300           ELSE                                                           
091400              MOVE NEJ                    TO SW-INDATA-OK                 
091500              IF MID-KDMARK-FOM (RAD-INDX)   = ALL '+'                    
091600                 MOVE MFS-NUM-FAELT-FEL   TO                              
091700                                 MOD-KDMARK-TOM-ATTR (RAD-INDX)           
091800              ELSE                                                        
091900                 IF MID-KDMARK-TOM (RAD-INDX) = ALL '+'                   
092000                    MOVE MFS-NUM-FAELT-FEL TO                             
092100                                 MOD-KDMARK-FOM-ATTR (RAD-INDX)           
092200                 ELSE                                                     
092300                    MOVE MFS-NUM-FAELT-FEL TO                             
092400                                 MOD-KDMARK-FOM-ATTR (RAD-INDX)           
092500                                 MOD-KDMARK-TOM-ATTR (RAD-INDX)           
092600                 END-IF                                                   
092700              END-IF                                                      
092800           END-IF                                                         
092900        END-IF                                                            
093000     .                                                                    
093100     EJECT                                                                
093200 CJ-KOLLA-IDDISTR           SECTION.                                      
093300     SKIP2                                                                
093400        IF  MID-IDDISTR-FOM (RAD-INDX) = ALL '+'                          
093500        AND MID-IDDISTR-TOM (RAD-INDX) = ALL '+'                          
093600           CONTINUE                                                       
093700        ELSE                                                              
093800           MOVE JA                        TO SW-DISTRIKT                  
093900           IF SW-KONCERN = JA                                             
094000           OR SW-MARKNAD = JA                                             
094100              MOVE JA                     TO SW-KONFLIKT                  
094200           END-IF                                                         
094300           IF MID-IDDISTR-FOM  (RAD-INDX) NUMERIC                         
094400           AND MID-IDDISTR-TOM (RAD-INDX) NUMERIC                         
094500              IF MID-IDDISTR-TOM (RAD-INDX) NOT <                         
094600              MID-IDDISTR-FOM (RAD-INDX)                                  
094700                 MOVE MFS-NUM-FAELT-RAETT TO                              
094800                           MOD-IDDISTR-FOM-ATTR (RAD-INDX)                
094900                           MOD-IDDISTR-TOM-ATTR (RAD-INDX)                
095000                 MOVE MID-IDDISTR-FOM (RAD-INDX) TO                       
095100                                  WS-URV1-IDDISTR-FOM (RAD-INDX)          
095200                 MOVE MID-IDDISTR-TOM (RAD-INDX) TO                       
095300                                  WS-URV1-IDDISTR-TOM (RAD-INDX)          
095400              ELSE                                                        
095500                 MOVE NEJ                 TO SW-INDATA-OK                 
095600                 MOVE MFS-NUM-FAELT-FEL  TO                               
095700                          MOD-IDDISTR-FOM-ATTR (RAD-INDX)                 
095800                          MOD-IDDISTR-TOM-ATTR (RAD-INDX)                 
095900              END-IF                                                      
096000           ELSE                                                           
096100              MOVE NEJ                    TO SW-INDATA-OK                 
096200              IF MID-IDDISTR-FOM (RAD-INDX)   = ALL '+'                   
096300                 MOVE MFS-NUM-FAELT-FEL   TO                              
096400                                 MOD-IDDISTR-TOM-ATTR (RAD-INDX)          
096500              ELSE                                                        
096600                 IF MID-IDDISTR-TOM (RAD-INDX) = ALL '+'                  
096700                    MOVE MFS-NUM-FAELT-FEL TO                             
096800                                 MOD-IDDISTR-FOM-ATTR (RAD-INDX)          
096900                 ELSE                                                     
097000                    MOVE MFS-NUM-FAELT-FEL TO                             
097100                                 MOD-IDDISTR-FOM-ATTR (RAD-INDX)          
097200                                 MOD-IDDISTR-TOM-ATTR (RAD-INDX)          
097300                 END-IF                                                   
097400              END-IF                                                      
097500           END-IF                                                         
097600        END-IF                                                            
097700     .                                                                    
097800     EJECT                                                                
097900 CK-KOLLA-IDANSK            SECTION.                                      
098000     SKIP2                                                                
098100        IF  MID-IDANSK-FOM (RAD-INDX) = ALL '+'                           
098200        AND MID-IDANSK-TOM (RAD-INDX) = ALL '+'                           
098300           CONTINUE                                                       
098400        ELSE                                                              
098500           IF MID-IDANSK-FOM   (RAD-INDX) NUMERIC                         
098600           AND MID-IDANSK-TOM (RAD-INDX) NUMERIC                          
098700              IF MID-IDANSK-TOM (RAD-INDX) NOT <                          
098800              MID-IDANSK-FOM (RAD-INDX)                                   
098900                 MOVE MFS-NUM-FAELT-RAETT TO                              
099000                           MOD-IDANSK-FOM-ATTR (RAD-INDX)                 
099100                           MOD-IDANSK-TOM-ATTR (RAD-INDX)                 
099200                 MOVE MID-IDANSK-FOM (RAD-INDX) TO                        
099300                                  WS-URV1-IDANSK-FOM (RAD-INDX)           
099400                 MOVE MID-IDANSK-TOM (RAD-INDX) TO                        
099500                                  WS-URV1-IDANSK-TOM (RAD-INDX)           
099600              ELSE                                                        
099700                 MOVE NEJ                 TO SW-INDATA-OK                 
099800                 MOVE MFS-NUM-FAELT-FEL  TO                               
099900                          MOD-IDANSK-FOM-ATTR (RAD-INDX)                  
100000                          MOD-IDANSK-TOM-ATTR (RAD-INDX)                  
100100              END-IF                                                      
100200           ELSE                                                           
100300              MOVE NEJ                    TO SW-INDATA-OK                 
100400              IF MID-IDANSK-FOM (RAD-INDX)    = ALL '+'                   
100500                 MOVE MFS-NUM-FAELT-FEL   TO                              
100600                                 MOD-IDANSK-TOM-ATTR (RAD-INDX)           
100700              ELSE                                                        
100800                 IF MID-IDANSK-TOM (RAD-INDX) = ALL '+'                   
100900                    MOVE MFS-NUM-FAELT-FEL TO                             
101000                                 MOD-IDANSK-FOM-ATTR (RAD-INDX)           
101100                 ELSE                                                     
101200                    MOVE MFS-NUM-FAELT-FEL TO                             
101300                                 MOD-IDANSK-FOM-ATTR (RAD-INDX)           
101400                                 MOD-IDANSK-TOM-ATTR (RAD-INDX)           
101500                 END-IF                                                   
101600              END-IF                                                      
101700           END-IF                                                         
101800        END-IF                                                            
101900     .                                                                    
102000     EJECT                                                                
102100 D-KOLLA-INDATA SECTION.                                                  
102200     SKIP2                                                                
102300     MOVE JA                             TO SW-INDATA-OK                  
102400     IF MID-KDBORT = 'B' OR 'D' OR 'J'                                    
102500        MOVE NEJ                         TO SW-IFYLLT                     
102600        PERFORM S09-KOLLA-ATT-INGET-IFYLLT                                
102700        IF  INGET-IFYLLT                                                  
102800           IF  MID-IDUSER-UT = MSG-SIGNON-USERID                          
102900              MOVE MFS-ALFA-FAELT-RAETT     TO MOD-KDBORT-ATTR            
103000           ELSE                                                           
103100              MOVE NEJ                      TO SW-INDATA-OK               
103200              MOVE MFS-ALFA-FAELT-FEL       TO MOD-KDBORT-ATTR            
103300              MOVE MED-8 (SPRAK-IX) TO MOD-TEMFSINF                       
103400              PERFORM S10-LAES-IN-IGEN                                    
103500           END-IF                                                         
103600        ELSE                                                              
103700           MOVE NEJ                      TO SW-INDATA-OK                  
103800           MOVE MFS-ALFA-FAELT-FEL       TO MOD-KDBORT-ATTR               
103900           MOVE MED-7 (SPRAK-IX) TO MOD-TEMFSINF                          
104000           PERFORM S10-LAES-IN-IGEN                                       
104100        END-IF                                                            
104200     ELSE                                                                 
104300        MOVE NEJ                         TO SW-KONFLIKT                   
104400        PERFORM IMS-GU-FSGA01                                             
104500        IF SEGMENT-FINNS                                                  
104600           PERFORM IMS-GNP-FSGA11                                         
104700           IF SEGMENT-FINNS                                               
104800* SW-KONFLIKT HÅLLER REDA PÅ ATT ENBART ETT URVAL AV                      
104900* KONCNR KDMARK OCH IDDISTR ANGES                                         
105000* OBS! FEL-FLAGGAN TÄNDS EJ UTAN BEHANDLAS SOM FEL I HUVUDSLINGAN         
105100              MOVE WLFSGA11              TO WS-WLFSGA11                   
105200              PERFORM DA-KOLLA-VECKA                                      
105300              MOVE ZERO             TO ANTAL-ATT-TA-BORT                  
105400              MOVE NEJ                   TO SW-PRODSL                     
105500              MOVE NEJ                   TO SW-MARKNAD                    
105600              MOVE NEJ                   TO SW-KONCERN                    
105700              MOVE NEJ                   TO SW-DISTRIKT                   
105800              MOVE +1                    TO RAD-INDX                      
105900              PERFORM UNTIL RAD-INDX > 8                                  
106000                 IF RAD-INDX > 7                                          
106100                    PERFORM DD-KOLLA-IDKONCNR                             
106200                    PERFORM DE-KOLLA-IDLEVNR                              
106300                    PERFORM DF-KOLLA-IDLKTO                               
106400                    PERFORM DI-KOLLA-KDMARK                               
106500                    PERFORM S08-KOLLA-IDFKNGRP                            
106600                 ELSE                                                     
106700                    IF RAD-INDX > 5                                       
106800                       PERFORM DD-KOLLA-IDKONCNR                          
106900                       PERFORM DE-KOLLA-IDLEVNR                           
107000                       PERFORM DF-KOLLA-IDLKTO                            
107100                       PERFORM DH-KOLLA-KDPRODSL                          
107200                       PERFORM DI-KOLLA-KDMARK                            
107300                       PERFORM S08-KOLLA-IDFKNGRP                         
107400                    ELSE                                                  
107500                       IF RAD-INDX > 4                                    
107600                          PERFORM DD-KOLLA-IDKONCNR                       
107700                          PERFORM DE-KOLLA-IDLEVNR                        
107800                          PERFORM DF-KOLLA-IDLKTO                         
107900                          PERFORM DG-KOLLA-KDVVKL                         
108000                          PERFORM DH-KOLLA-KDPRODSL                       
108100                          PERFORM DI-KOLLA-KDMARK                         
108200                          PERFORM S08-KOLLA-IDFKNGRP                      
108300                       ELSE                                               
108400                          PERFORM DD-KOLLA-IDKONCNR                       
108500                          PERFORM DE-KOLLA-IDLEVNR                        
108600                          PERFORM DF-KOLLA-IDLKTO                         
108700                          PERFORM DG-KOLLA-KDVVKL                         
108800                          PERFORM DH-KOLLA-KDPRODSL                       
108900                          PERFORM DI-KOLLA-KDMARK                         
109000                          PERFORM DJ-KOLLA-IDDISTR                        
109100                          PERFORM DK-KOLLA-IDANSK                         
109200                          PERFORM S05-KOLLA-BORTTAG-IDFKNGRP              
109300                          PERFORM S08-KOLLA-IDFKNGRP                      
109400                       END-IF                                             
109500                    END-IF                                                
109600                 END-IF                                                   
109700                 ADD  +1                      TO RAD-INDX                 
109800              END-PERFORM                                                 
109900              PERFORM DB-KOLLA-SPAR-BORT-LIST-PRIS                        
110000              IF ANTAL-ATT-TA-BORT > ZERO                                 
110100                 PERFORM DC-KOLLA-ANTA-BAS                                
110200                 IF ANTAL-IDFKNGRP-BAS > ANTAL-ATT-TA-BORT                
110300                    PERFORM DL-KOLLA-IDFKNGRP-FINNS                       
110400                 ELSE                                                     
110500                    MOVE NEJ                 TO SW-INDATA-OK              
110600                    MOVE MFS-ALFA-FAELT-FEL  TO MOD-KDCMD-ATTR            
110700                    MOVE MED-5   (SPRAK-IX)  TO MOD-TEMFSINF              
110800                 END-IF                                                   
110900              END-IF                                                      
111000           END-IF                                                         
111100        END-IF                                                            
111200     END-IF                                                               
111300     .                                                                    
111400     EJECT                                                                
111500 DA-KOLLA-VECKA      SECTION.                                             
111600     SKIP2                                                                
111700*--KDSVAR VISAR OM MAN ÖNSKAR SAMMANSLAGNING AV PS.                       
111800     IF MID-KDSVAR = '+'                                                  
111900       CONTINUE                                                           
112000     ELSE                                                                 
112100       IF MID-KDSVAR = 'J'                                                
112200         MOVE MFS-ALFA-FAELT-RAETT TO MOD-KDSVAR-ATTR                     
112300         MOVE MID-KDSVAR          TO WS-URV1-KDSVAR                       
112400       ELSE                                                               
112500         MOVE MFS-ALFA-FAELT-RAETT TO MOD-KDSVAR-ATTR                     
112600         MOVE 'N'                 TO WS-URV1-KDSVAR                       
112700       END-IF                                                             
112800     END-IF                                                               
112900                                                                          
113000     IF  MID-TIFSGVV-FOM = ALL '+'                                        
113100     AND MID-TIFSGVV-TOM = ALL '+'                                        
113200        CONTINUE                                                          
113300     ELSE                                                                 
113400        IF  MID-TIFSGVV-FOM = ALL '+'                                     
113500           CONTINUE                                                       
113600        ELSE                                                              
113700           MOVE MFS-NUM-FAELT-RAETT  TO                                   
113800                                       MOD-TIFSGVV-FOM-ATTR               
113900           IF  MID-TIFSGVV-FOM = ZERO                                     
114000              MOVE MID-TIFSGVV-FOM      TO                                
114100                                       WS-URV1-TIFSGVV-FOM                
114200           ELSE                                                           
114300              MOVE 'AAVV  '             TO DAT-KDDATFORM                  
114400              MOVE MID-TIFSGVV-FOM      TO DAT-I-TIDATUM                  
114500              PERFORM S99-CALL-WDATKONV                                   
114600              IF DAT-KDSVAR-OK                                            
114700                 MOVE DAT-TIAA-VECKA        TO WS-AA                      
114800                 MOVE DAT-TIVV              TO WS-VV                      
114900                 MOVE WS-AAVV-N                   TO TMP1-YYWW            
115000                 MOVE WS-DAGENS-AAVV-MINUS-2-AA-N TO TMP2-YYWW            
115100                 PERFORM WY2000P3                                         
115200                 IF TMP1-YYWW >= TMP2-YYWW                                
115300                    MOVE MID-TIFSGVV-FOM      TO                          
115400                                       WS-URV1-TIFSGVV-FOM                
115500                 ELSE                                                     
115600                    MOVE NEJ                  TO SW-INDATA-OK             
115700                    MOVE MFS-NUM-FAELT-FEL    TO                          
115800                                       MOD-TIFSGVV-FOM-ATTR               
115900                 END-IF                                                   
116000              ELSE                                                        
116100                 MOVE NEJ                  TO SW-INDATA-OK                
116200                 MOVE MFS-NUM-FAELT-FEL    TO                             
116300                                    MOD-TIFSGVV-FOM-ATTR                  
116400              END-IF                                                      
116500           END-IF                                                         
116600        END-IF                                                            
116700        IF  MID-TIFSGVV-TOM = ALL '+'                                     
116800           CONTINUE                                                       
116900        ELSE                                                              
117000           MOVE MFS-NUM-FAELT-RAETT     TO                                
117100                                      MOD-TIFSGVV-TOM-ATTR                
117200           IF  MID-TIFSGVV-TOM = ZERO                                     
117300              MOVE MID-TIFSGVV-TOM      TO                                
117400                                       WS-URV1-TIFSGVV-TOM                
117500           ELSE                                                           
117600              MOVE 'AAVV  '             TO DAT-KDDATFORM                  
117700              MOVE MID-TIFSGVV-TOM      TO DAT-I-TIDATUM                  
117800              PERFORM S99-CALL-WDATKONV                                   
117900              IF DAT-KDSVAR-OK                                            
118000                 MOVE DAT-TIAA-VECKA        TO WS-AA                      
118100                 MOVE DAT-TIVV              TO WS-VV                      
118200                 MOVE WS-AAVV-N        TO TMP1-YYWW                       
118300                 MOVE WS-DAGENS-AAVV-N TO TMP2-YYWW                       
118400                 PERFORM WY2000P3                                         
118500                 IF TMP1-YYWW < TMP2-YYWW                                 
118600                    MOVE MID-TIFSGVV-TOM      TO                          
118700                                        WS-URV1-TIFSGVV-TOM               
118800                 ELSE                                                     
118900                    IF WS-URV1-IDPTYP = 'VP1'                             
119000                                      OR 'VA1' OR 'VA2'                   
119100                      AND WS-AAVV = WS-DAGENS-AAVV                        
119200                       MOVE MID-TIFSGVV-TOM      TO                       
119300                                          WS-URV1-TIFSGVV-TOM             
119400                    ELSE                                                  
119500                       MOVE NEJ                  TO SW-INDATA-OK          
119600                       MOVE MFS-NUM-FAELT-FEL    TO                       
119700                                         MOD-TIFSGVV-TOM-ATTR             
119800                    END-IF                                                
119900                 END-IF                                                   
120000              ELSE                                                        
120100                 MOVE NEJ                  TO SW-INDATA-OK                
120200                 MOVE MFS-NUM-FAELT-FEL    TO                             
120300                                    MOD-TIFSGVV-TOM-ATTR                  
120400              END-IF                                                      
120500           END-IF                                                         
120600        END-IF                                                            
120700        IF INDATA-OK                                                      
120800           MOVE WS-URV1-TIFSGVV-TOM   TO TMP1-YYWW                        
120900           MOVE WS-URV1-TIFSGVV-FOM   TO TMP2-YYWW                        
121000           PERFORM WY2000P3                                               
121100           IF  TMP1-YYWW >= TMP2-YYWW                                     
121200              IF  WS-URV1-TIFSGVV-FOM = ZERO                              
121300                 IF  WS-URV1-TIFSGVV-TOM = ZERO                           
121400                    CONTINUE                                              
121500                 ELSE                                                     
121600                    MOVE NEJ             TO SW-INDATA-OK                  
121700                    PERFORM DAE-SAETT-MFS-FEL                             
121800                 END-IF                                                   
121900              END-IF                                                      
122000           ELSE                                                           
122100              MOVE NEJ                   TO SW-INDATA-OK                  
122200              PERFORM DAE-SAETT-MFS-FEL                                   
122300           END-IF                                                         
122400        END-IF                                                            
122500     END-IF                                                               
122600     .                                                                    
122700     EJECT                                                                
122800 DAE-SAETT-MFS-FEL  SECTION.                                              
122900     SKIP2                                                                
123000     IF MID-TIFSGVV-FOM = ALL '+'                                         
123100        CONTINUE                                                          
123200     ELSE                                                                 
123300        MOVE MFS-NUM-FAELT-FEL           TO                               
123400                                       MOD-TIFSGVV-FOM-ATTR               
123500     END-IF                                                               
123600     IF MID-TIFSGVV-TOM = ALL '+'                                         
123700        CONTINUE                                                          
123800     ELSE                                                                 
123900        MOVE MFS-NUM-FAELT-FEL           TO                               
124000                                       MOD-TIFSGVV-TOM-ATTR               
124100     END-IF                                                               
124200     .                                                                    
124300     EJECT                                                                
124400 DB-KOLLA-SPAR-BORT-LIST-PRIS   SECTION.                                  
124500     SKIP2                                                                
124600     IF MID-IDFSGURV = ALL '+'                                            
124700        CONTINUE                                                          
124800     ELSE                                                                 
124900        MOVE MFS-ALFA-FAELT-RAETT        TO MOD-IDFSGURV-ATTR             
125000     END-IF                                                               
125100     IF MID-KDBORT = ALL '+'                                              
125200        CONTINUE                                                          
125300     ELSE                                                                 
125400        MOVE NEJ                         TO SW-INDATA-OK                  
125500        MOVE MFS-ALFA-FAELT-FEL          TO MOD-KDBORT-ATTR               
125600     END-IF                                                               
125700     IF MID-KDCMD    = ALL '+'                                            
125800        CONTINUE                                                          
125900     ELSE                                                                 
126000        IF MID-KDCMD    = ALL 'B' OR 'D'                                  
126100           MOVE MFS-ALFA-FAELT-RAETT     TO MOD-KDCMD-ATTR                
126200        ELSE                                                              
126300           MOVE NEJ                      TO SW-INDATA-OK                  
126400           MOVE MFS-ALFA-FAELT-FEL       TO MOD-KDCMD-ATTR                
126500        END-IF                                                            
126600     END-IF                                                               
126700     IF MID-IDPTYP = ALL '+'                                              
126800     AND MID-TIFSGVV-FOM = ALL '+'                                        
126900     AND MID-TIFSGVV-TOM = ALL '+'                                        
127000        CONTINUE                                                          
127100     ELSE                                                                 
127200        IF MID-IDPTYP     = ALL '+'                                       
127300           CONTINUE                                                       
127400        ELSE                                                              
127500           MOVE MFS-ALFA-FAELT-RAETT        TO MOD-IDPTYP-ATTR            
127600           MOVE MID-IDPTYP                  TO WS-URV1-IDPTYP             
127700        END-IF                                                            
127800        IF WS-URV1-IDPTYP = 'VP1' OR 'VA1' OR 'VA2'                       
127900           IF WS-URV1-TIFSGVV-FOM > ZERO                                  
128000              CONTINUE                                                    
128100           ELSE                                                           
128200              MOVE NEJ                      TO SW-INDATA-OK               
128300              PERFORM DBA-BEHANDLA-FEL-IDPTYP                             
128400           END-IF                                                         
128500        ELSE                                                              
128600                                                                          
128700                                                                          
128800                                                                          
128900           IF WS-URV1-IDPTYP = 'P1' OR 'A1' OR 'A2'                       
129000              IF WS-URV1-TIFSGVV-FOM = ZERO                               
129100                 CONTINUE                                                 
129200              ELSE                                                        
129300                 MOVE NEJ                   TO SW-INDATA-OK               
129400                 PERFORM DBA-BEHANDLA-FEL-IDPTYP                          
129500              END-IF                                                      
129600           ELSE                                                           
129700                                                                          
129800                                                                          
129900              IF WS-URV1-IDPTYP =  'PPV'                                  
130000                 PERFORM DM-KOLLA-P2                                      
130100              ELSE                                                        
130200                 MOVE NEJ                   TO SW-INDATA-OK               
130300                 MOVE MFS-ALFA-FAELT-FEL    TO MOD-IDPTYP-ATTR            
130400              END-IF                                                      
130500           END-IF                                                         
130600        END-IF                                                            
130700     END-IF                                                               
130800     IF WS-URV1-IDPTYP = 'A1' OR 'A2' OR 'VA1' OR 'VA2'                   
130900        IF SW-PRODSL = NEJ                                                
131000           MOVE NEJ                      TO SW-INDATA-OK                  
131100           MOVE MFS-ALFA-FAELT-FEL       TO MOD-IDPTYP-ATTR               
131200           MOVE MED-6 (SPRAK-IX) TO MOD-TEMFSINF                          
131300        END-IF                                                            
131400     END-IF                                                               
131500     IF MID-KDPRTYPG = ALL '+'                                            
131600        CONTINUE                                                          
131700     ELSE                                                                 
131800        MOVE MFS-ALFA-FAELT-RAETT        TO                               
131900                                          MOD-KDPRTYPG-ATTR               
132000        MOVE MID-KDPRTYPG                   TO WS-URV1-KDPRTYP            
132100     END-IF                                                               
132200     IF WS-URV1-KDPRTYP =                                                 
132300                       'S' OR 'F' OR 'M' OR 'K' OR 'R' OR '-'             
132400        IF WS-URV1-TIFSGVV-FOM > ZERO                                     
132500           CONTINUE                                                       
132600        ELSE                                                              
132700           MOVE NEJ                      TO SW-INDATA-OK                  
132800           PERFORM DBB-BEHANDLA-FEL-KDPRTYPG                              
132900        END-IF                                                            
133000     ELSE                                                                 
133100        IF WS-URV1-KDPRTYP = SPACE                                        
133200           CONTINUE                                                       
133300        ELSE                                                              
133400           MOVE NEJ                      TO SW-INDATA-OK                  
133500           MOVE MFS-ALFA-FAELT-FEL       TO MOD-KDPRTYPG-ATTR             
133600        END-IF                                                            
133700     END-IF                                                               
133800     IF MID-KDNIVA = ALL '+'                                              
133900        CONTINUE                                                          
134000     ELSE                                                                 
134100        IF MID-KDNIVA =  0  OR  2  OR  4                                  
134200           MOVE MFS-NUM-FAELT-RAETT      TO  MOD-KDNIVA-ATTR              
134300           MOVE MID-KDNIVA               TO WS-URV1-KDNIVA                
134400        ELSE                                                              
134500           MOVE NEJ                      TO SW-INDATA-OK                  
134600           MOVE MFS-NUM-FAELT-FEL        TO MOD-KDNIVA-ATTR               
134700        END-IF                                                            
134800     END-IF                                                               
134900     .                                                                    
135000     EJECT                                                                
135100 DBA-BEHANDLA-FEL-IDPTYP SECTION.                                         
135200     SKIP2                                                                
135300     IF MID-IDPTYP = ALL '+'                                              
135400        IF MID-TIFSGVV-FOM = ALL '+'                                      
135500           CONTINUE                                                       
135600        ELSE                                                              
135700           MOVE MFS-NUM-FAELT-FEL       TO MOD-TIFSGVV-FOM-ATTR           
135800        END-IF                                                            
135900        IF MID-TIFSGVV-TOM = ALL '+'                                      
136000           CONTINUE                                                       
136100        ELSE                                                              
136200           MOVE MFS-NUM-FAELT-FEL       TO MOD-TIFSGVV-TOM-ATTR           
136300        END-IF                                                            
136400     ELSE                                                                 
136500        MOVE MFS-ALFA-FAELT-FEL         TO MOD-IDPTYP-ATTR                
136600     END-IF                                                               
136700     .                                                                    
136800     EJECT                                                                
136900 DBB-BEHANDLA-FEL-KDPRTYPG SECTION.                                       
137000     SKIP2                                                                
137100     IF MID-KDPRTYPG = ALL '+'                                            
137200        IF MID-TIFSGVV-FOM = ALL '+'                                      
137300           CONTINUE                                                       
137400        ELSE                                                              
137500           MOVE MFS-NUM-FAELT-FEL       TO MOD-TIFSGVV-FOM-ATTR           
137600        END-IF                                                            
137700        IF MID-TIFSGVV-TOM = ALL '+'                                      
137800           CONTINUE                                                       
137900        ELSE                                                              
138000           MOVE MFS-NUM-FAELT-FEL       TO MOD-TIFSGVV-TOM-ATTR           
138100        END-IF                                                            
138200     ELSE                                                                 
138300        MOVE MFS-ALFA-FAELT-FEL         TO MOD-KDPRTYPG-ATTR              
138400     END-IF                                                               
138500     .                                                                    
138600     EJECT                                                                
138700 DC-KOLLA-ANTA-BAS SECTION.                                               
138800     SKIP2                                                                
138900     IF MID-KDCMD = ALL '+'                                               
139000        MOVE NEJ                 TO SW-INDATA-OK                          
139100        MOVE MFS-ALFA-FAELT-FEL  TO MOD-KDCMD-ATTR                        
139200     ELSE                                                                 
139300        IF INDATA-OK                                                      
139400           MOVE ZERO                   TO W-IDFKNGRP-FOM                  
139500           MOVE +99999                 TO W-IDFKNGRP-TOM                  
139600           MOVE ZERO                   TO ANTAL-IDFKNGRP-BAS              
139700           PERFORM IMS-GNP-FSGA12                                         
139800           PERFORM UNTIL ANTAL-IDFKNGRP-BAS > ANTAL-ATT-TA-BORT           
139900                                              OR SEGMENT-SAKNAS           
140000              ADD  +1                  TO ANTAL-IDFKNGRP-BAS              
140100              PERFORM IMS-GNP-FSGA12                                      
140200           END-PERFORM                                                    
140300        END-IF                                                            
140400     END-IF                                                               
140500     .                                                                    
140600     EJECT                                                                
140700 DD-KOLLA-IDKONCNR          SECTION.                                      
140800     SKIP2                                                                
140900     IF MID-IDKONCNR (RAD-INDX) = ALL '+'                                 
141000        IF  WS-URV1-IDKONCNR (RAD-INDX) > ZERO                            
141100           MOVE JA                    TO SW-KONCERN                       
141200        END-IF                                                            
141300     ELSE                                                                 
141400        IF MID-IDKONCNR (RAD-INDX) NUMERIC                                
141500           MOVE MFS-NUM-FAELT-RAETT   TO                                  
141600                                 MOD-IDKONCNR-ATTR (RAD-INDX)             
141700           MOVE MID-IDKONCNR (RAD-INDX) TO                                
141800                                  WS-URV1-IDKONCNR (RAD-INDX)             
141900           IF  WS-URV1-IDKONCNR (RAD-INDX) > ZERO                         
142000              MOVE JA                    TO SW-KONCERN                    
142100           END-IF                                                         
142200        ELSE                                                              
142300           MOVE NEJ                   TO SW-INDATA-OK                     
142400           MOVE MFS-NUM-FAELT-FEL     TO                                  
142500                                 MOD-IDKONCNR-ATTR (RAD-INDX)             
142600        END-IF                                                            
142700     END-IF                                                               
142800     .                                                                    
142900     EJECT                                                                
143000 DE-KOLLA-IDLEVNR           SECTION.                                      
143100     SKIP2                                                                
143200     IF MID-IDLEVNR (RAD-INDX) = ALL '+'                                  
143300        CONTINUE                                                          
143400     ELSE                                                                 
143500           MOVE MID-IDLEVNR (RAD-INDX) TO                                 
143600                            WS-URV1-IDLEVNR (RAD-INDX)                    
143700           MOVE MFS-ALFA-FAELT-RAETT   TO                                 
143800                                 MOD-IDLEVNR-ATTR (RAD-INDX)              
143900     END-IF                                                               
144000     .                                                                    
144100     EJECT                                                                
144200 DF-KOLLA-IDLKTO            SECTION.                                      
144300     SKIP2                                                                
144400     IF MID-IDLKTO (RAD-INDX) = ALL '+'                                   
144500        CONTINUE                                                          
144600     ELSE                                                                 
144700        IF MID-IDLKTO (RAD-INDX) NUMERIC                                  
144800           MOVE MID-IDLKTO (RAD-INDX) TO                                  
144900                            WS-URV1-IDLKTO (RAD-INDX)                     
145000           MOVE MFS-NUM-FAELT-RAETT   TO                                  
145100                                 MOD-IDLKTO-ATTR (RAD-INDX)               
145200        ELSE                                                              
145300           MOVE NEJ                   TO SW-INDATA-OK                     
145400           MOVE MFS-NUM-FAELT-FEL     TO                                  
145500                                 MOD-IDLKTO-ATTR (RAD-INDX)               
145600        END-IF                                                            
145700     END-IF                                                               
145800     .                                                                    
145900     EJECT                                                                
146000 DG-KOLLA-KDVVKL            SECTION.                                      
146100     SKIP2                                                                
146200     IF MID-KDVVKL (RAD-INDX) = ALL '+'                                   
146300        CONTINUE                                                          
146400     ELSE                                                                 
146500        IF MID-KDVVKL (RAD-INDX) NUMERIC                                  
146600           MOVE MID-KDVVKL (RAD-INDX) TO                                  
146700                            WS-URV1-KDVVKL (RAD-INDX)                     
146800           MOVE MFS-NUM-FAELT-RAETT   TO                                  
146900                                 MOD-KDVVKL-ATTR (RAD-INDX)               
147000        ELSE                                                              
147100           MOVE NEJ                   TO SW-INDATA-OK                     
147200           MOVE MFS-NUM-FAELT-FEL     TO                                  
147300                                 MOD-KDVVKL-ATTR (RAD-INDX)               
147400        END-IF                                                            
147500     END-IF                                                               
147600     .                                                                    
147700     EJECT                                                                
147800 DH-KOLLA-KDPRODSL          SECTION.                                      
147900     SKIP2                                                                
148000     IF MID-KDPRODSL (RAD-INDX) = ALL '+'                                 
148100        IF WS-URV1-KDPRODSL (RAD-INDX) > ZERO                             
148200           MOVE JA                    TO SW-PRODSL                        
148300        END-IF                                                            
148400     ELSE                                                                 
148500        IF MID-KDPRODSL (RAD-INDX) NUMERIC                                
148600           MOVE MID-KDPRODSL (RAD-INDX)                                   
148700                                 TO TEST-KDPRODSL                         
148800           IF KDPRODSL-VOLVO-BIMA OR                                      
148900              TEST-KDPRODSL = ZEROES                                      
149000              MOVE MFS-NUM-FAELT-RAETT   TO                               
149100                                 MOD-KDPRODSL-ATTR (RAD-INDX)             
149200              MOVE MID-KDPRODSL (RAD-INDX) TO                             
149300                                  WS-URV1-KDPRODSL (RAD-INDX)             
149400              IF WS-URV1-KDPRODSL (RAD-INDX) > ZERO                       
149500                 MOVE JA                    TO SW-PRODSL                  
149600              END-IF                                                      
149700           ELSE                                                           
149800              MOVE NEJ                   TO SW-INDATA-OK                  
149900              MOVE MFS-NUM-FAELT-FEL     TO                               
150000                                 MOD-KDPRODSL-ATTR (RAD-INDX)             
150100           END-IF                                                         
150200        ELSE                                                              
150300           MOVE NEJ                   TO SW-INDATA-OK                     
150400           MOVE MFS-NUM-FAELT-FEL     TO                                  
150500                                 MOD-KDPRODSL-ATTR (RAD-INDX)             
150600        END-IF                                                            
150700     END-IF                                                               
150800     .                                                                    
150900     EJECT                                                                
151000 DI-KOLLA-KDMARK            SECTION.                                      
151100     SKIP2                                                                
151200     IF  MID-KDMARK-FOM (RAD-INDX) = ALL '+'                              
151300     AND MID-KDMARK-TOM (RAD-INDX) = ALL '+'                              
151400        PERFORM DIA-KOLLA-OM-KONFLIKT                                     
151500     ELSE                                                                 
151600        IF  MID-KDMARK-FOM (RAD-INDX) = ALL '+'                           
151700           CONTINUE                                                       
151800        ELSE                                                              
151900           MOVE MFS-NUM-FAELT-RAETT    TO                                 
152000                        MOD-KDMARK-FOM-ATTR (RAD-INDX)                    
152100           IF  MID-KDMARK-FOM (RAD-INDX) NUMERIC                          
152200           MOVE JA                     TO SW-MARKNAD                      
152300              MOVE MID-KDMARK-FOM (RAD-INDX) TO                           
152400                          WS-URV1-KDMARK-FOM (RAD-INDX)                   
152500           ELSE                                                           
152600              MOVE NEJ                 TO SW-INDATA-OK                    
152700              MOVE MFS-NUM-FAELT-FEL   TO                                 
152800                        MOD-KDMARK-FOM-ATTR (RAD-INDX)                    
152900           END-IF                                                         
153000        END-IF                                                            
153100        IF  MID-KDMARK-TOM (RAD-INDX) = ALL '+'                           
153200           CONTINUE                                                       
153300        ELSE                                                              
153400           IF  MID-KDMARK-TOM (RAD-INDX) NUMERIC                          
153500              MOVE MFS-NUM-FAELT-RAETT    TO                              
153600                        MOD-KDMARK-TOM-ATTR (RAD-INDX)                    
153700              MOVE MID-KDMARK-TOM (RAD-INDX) TO                           
153800                          WS-URV1-KDMARK-TOM (RAD-INDX)                   
153900           ELSE                                                           
154000              MOVE NEJ                 TO SW-INDATA-OK                    
154100              MOVE MFS-NUM-FAELT-FEL   TO                                 
154200                        MOD-KDMARK-TOM-ATTR (RAD-INDX)                    
154300           END-IF                                                         
154400        END-IF                                                            
154500        IF INDATA-OK                                                      
154600           PERFORM DIA-KOLLA-OM-KONFLIKT                                  
154700           IF WS-URV1-KDMARK-TOM (RAD-INDX) NOT <                         
154800           WS-URV1-KDMARK-FOM (RAD-INDX)                                  
154900              CONTINUE                                                    
155000           ELSE                                                           
155100              MOVE NEJ                 TO SW-INDATA-OK                    
155200              IF  MID-KDMARK-FOM (RAD-INDX) = ALL '+'                     
155300                 CONTINUE                                                 
155400              ELSE                                                        
155500                 MOVE MFS-NUM-FAELT-FEL   TO                              
155600                        MOD-KDMARK-FOM-ATTR (RAD-INDX)                    
155700              END-IF                                                      
155800              IF  MID-KDMARK-TOM (RAD-INDX) = ALL '+'                     
155900                 CONTINUE                                                 
156000              ELSE                                                        
156100                 MOVE MFS-NUM-FAELT-FEL   TO                              
156200                        MOD-KDMARK-TOM-ATTR (RAD-INDX)                    
156300              END-IF                                                      
156400           END-IF                                                         
156500        END-IF                                                            
156600     END-IF                                                               
156700     .                                                                    
156800     EJECT                                                                
156900 DIA-KOLLA-OM-KONFLIKT   SECTION.                                         
157000     SKIP2                                                                
157100     IF WS-URV1-KDMARK-TOM (RAD-INDX) > ZERO                              
157200        MOVE JA                     TO SW-MARKNAD                         
157300        IF SW-KONCERN = JA                                                
157400           MOVE JA                  TO SW-KONFLIKT                        
157500        END-IF                                                            
157600     END-IF                                                               
157700     .                                                                    
157800     EJECT                                                                
157900 DJ-KOLLA-IDDISTR           SECTION.                                      
158000     SKIP2                                                                
158100     IF  MID-IDDISTR-FOM (RAD-INDX) = ALL '+'                             
158200     AND MID-IDDISTR-TOM (RAD-INDX) = ALL '+'                             
158300        PERFORM DJA-KOLLA-OM-KONFLIKT                                     
158400     ELSE                                                                 
158500        IF MID-IDDISTR-FOM  (RAD-INDX) = ALL '+'                          
158600           CONTINUE                                                       
158700        ELSE                                                              
158800           IF MID-IDDISTR-FOM  (RAD-INDX) NUMERIC                         
158900              MOVE MFS-NUM-FAELT-RAETT TO                                 
159000                       MOD-IDDISTR-FOM-ATTR (RAD-INDX)                    
159100              MOVE MID-IDDISTR-FOM (RAD-INDX) TO                          
159200                         WS-URV1-IDDISTR-FOM (RAD-INDX)                   
159300              MOVE JA                   TO SW-DISTRIKT                    
159400           ELSE                                                           
159500              MOVE NEJ                 TO SW-INDATA-OK                    
159600              MOVE MFS-NUM-FAELT-FEL  TO                                  
159700                       MOD-IDDISTR-FOM-ATTR (RAD-INDX)                    
159800           END-IF                                                         
159900        END-IF                                                            
160000        IF MID-IDDISTR-TOM  (RAD-INDX) = ALL '+'                          
160100           CONTINUE                                                       
160200        ELSE                                                              
160300           IF MID-IDDISTR-TOM  (RAD-INDX) NUMERIC                         
160400              MOVE MFS-NUM-FAELT-RAETT TO                                 
160500                       MOD-IDDISTR-TOM-ATTR (RAD-INDX)                    
160600              MOVE MID-IDDISTR-TOM (RAD-INDX) TO                          
160700                         WS-URV1-IDDISTR-TOM (RAD-INDX)                   
160800           ELSE                                                           
160900              MOVE NEJ                 TO SW-INDATA-OK                    
161000              MOVE MFS-NUM-FAELT-FEL  TO                                  
161100                       MOD-IDDISTR-TOM-ATTR (RAD-INDX)                    
161200           END-IF                                                         
161300        END-IF                                                            
161400        IF INDATA-OK                                                      
161500           PERFORM DJA-KOLLA-OM-KONFLIKT                                  
161600           IF WS-URV1-IDDISTR-TOM (RAD-INDX) NOT <                        
161700           WS-URV1-IDDISTR-FOM (RAD-INDX)                                 
161800              CONTINUE                                                    
161900           ELSE                                                           
162000              MOVE NEJ                 TO SW-INDATA-OK                    
162100              IF MID-IDDISTR-FOM (RAD-INDX) = ALL '+'                     
162200                 CONTINUE                                                 
162300              ELSE                                                        
162400                 MOVE MFS-NUM-FAELT-FEL  TO                               
162500                       MOD-IDDISTR-FOM-ATTR (RAD-INDX)                    
162600              END-IF                                                      
162700              IF MID-IDDISTR-TOM (RAD-INDX) = ALL '+'                     
162800                 CONTINUE                                                 
162900              ELSE                                                        
163000                 MOVE MFS-NUM-FAELT-FEL  TO                               
163100                       MOD-IDDISTR-TOM-ATTR (RAD-INDX)                    
163200              END-IF                                                      
163300           END-IF                                                         
163400        END-IF                                                            
163500     END-IF                                                               
163600     .                                                                    
163700     EJECT                                                                
163800 DJA-KOLLA-OM-KONFLIKT   SECTION.                                         
163900     SKIP2                                                                
164000     IF WS-URV1-IDDISTR-TOM (RAD-INDX) > ZERO                             
164100        MOVE JA                        TO SW-DISTRIKT                     
164200        IF SW-KONCERN = JA                                                
164300        OR SW-MARKNAD = JA                                                
164400           MOVE JA                     TO SW-KONFLIKT                     
164500        END-IF                                                            
164600     END-IF                                                               
164700     .                                                                    
164800     EJECT                                                                
164900 DK-KOLLA-IDANSK            SECTION.                                      
165000     SKIP2                                                                
165100     IF  MID-IDANSK-FOM (RAD-INDX) = ALL '+'                              
165200        CONTINUE                                                          
165300     ELSE                                                                 
165400        IF MID-IDANSK-FOM   (RAD-INDX) NUMERIC                            
165500           MOVE MFS-NUM-FAELT-RAETT    TO                                 
165600                               WS-URV1-IDANSK-FOM (RAD-INDX)              
165700           MOVE MID-IDANSK-FOM (RAD-INDX)     TO                          
165800                               WS-URV1-IDANSK-FOM (RAD-INDX)              
165900        ELSE                                                              
166000           MOVE NEJ                    TO SW-INDATA-OK                    
166100           MOVE MFS-NUM-FAELT-FEL      TO                                 
166200                               WS-URV1-IDANSK-FOM (RAD-INDX)              
166300        END-IF                                                            
166400     END-IF                                                               
166500     IF  MID-IDANSK-TOM (RAD-INDX) = ALL '+'                              
166600        CONTINUE                                                          
166700     ELSE                                                                 
166800        IF MID-IDANSK-TOM   (RAD-INDX) NUMERIC                            
166900           MOVE MFS-NUM-FAELT-RAETT    TO                                 
167000                               WS-URV1-IDANSK-TOM (RAD-INDX)              
167100           MOVE MID-IDANSK-TOM (RAD-INDX)     TO                          
167200                               WS-URV1-IDANSK-TOM (RAD-INDX)              
167300        ELSE                                                              
167400           MOVE NEJ                    TO SW-INDATA-OK                    
167500           MOVE MFS-NUM-FAELT-FEL      TO                                 
167600                               WS-URV1-IDANSK-TOM (RAD-INDX)              
167700        END-IF                                                            
167800     END-IF                                                               
167900     IF INDATA-OK                                                         
168000        IF WS-URV1-IDANSK-TOM (RAD-INDX) NOT <                            
168100        WS-URV1-IDANSK-FOM (RAD-INDX)                                     
168200           CONTINUE                                                       
168300        ELSE                                                              
168400           MOVE NEJ                 TO SW-INDATA-OK                       
168500           IF  MID-IDANSK-FOM (RAD-INDX) = ALL '+'                        
168600              CONTINUE                                                    
168700           ELSE                                                           
168800              MOVE MFS-NUM-FAELT-FEL   TO                                 
168900                    MOD-IDANSK-FOM-ATTR (RAD-INDX)                        
169000           END-IF                                                         
169100           IF  MID-IDANSK-TOM (RAD-INDX) = ALL '+'                        
169200              CONTINUE                                                    
169300           ELSE                                                           
169400              MOVE MFS-NUM-FAELT-FEL   TO                                 
169500                    MOD-IDANSK-TOM-ATTR (RAD-INDX)                        
169600           END-IF                                                         
169700        END-IF                                                            
169800     END-IF                                                               
169900     .                                                                    
170000     EJECT                                                                
170100 DL-KOLLA-IDFKNGRP-FINNS SECTION.                                         
170200     SKIP1                                                                
170300     MOVE +1                        TO RAD-INDX                           
170400     PERFORM UNTIL RAD-INDX > +4                                          
170500        IF MID-IDFKNGRP-FOM-IN (RAD-INDX) = ALL '+'                       
170600        AND MID-IDFKNGRP-TOM-IN (RAD-INDX) = ALL '+'                      
170700           CONTINUE                                                       
170800        ELSE                                                              
170900           MOVE MID-IDFKNGRP-FOM-IN (RAD-INDX) TO                         
171000                                        W-IDFKNGRP-FOM                    
171100           MOVE MID-IDFKNGRP-TOM-IN (RAD-INDX) TO                         
171200                                        W-IDFKNGRP-TOM                    
171300           PERFORM IMS-GU-FSGA12                                          
171400           IF SEGMENT-FINNS                                               
171500              CONTINUE                                                    
171600           ELSE                                                           
171700              MOVE NEJ                       TO SW-INDATA-OK              
171800              MOVE MFS-NUM-FAELT-FEL TO                                   
171900                            MOD-IDFKNGRP-FOM-IN-ATTR (RAD-INDX)           
172000                            MOD-IDFKNGRP-TOM-IN-ATTR (RAD-INDX)           
172100*             MOVE MED-3 (SPRAK-IX)          TO MOD-TEMFSINF              
172200              MOVE '110'            TO MED-IDMFSINF                       
172300              CALL WMEDKONV     USING MED-WMEDAREA                        
172400              MOVE MED-MFSINF       TO MOD-TEMFSINF                       
172500           END-IF                                                         
172600        END-IF                                                            
172700        ADD  +1                        TO RAD-INDX                        
172800     END-PERFORM                                                          
172900     .                                                                    
173000     EJECT                                                                
173100 DM-KOLLA-P2      SECTION.                                                
173200     SKIP1                                                                
173300     IF SW-KONCERN = NEJ AND SW-DISTRIKT = NEJ                            
173400        AND SW-MARKNAD = NEJ                                              
173500        CONTINUE                                                          
173600     ELSE                                                                 
173700        MOVE NEJ                    TO SW-INDATA-OK                       
173800        MOVE MFS-ALFA-FAELT-FEL     TO MOD-IDPTYP-ATTR                    
173900     END-IF                                                               
174000     .                                                                    
174100     SKIP2                                                                
174200 E-UPPDATERA-VISA-SIDAN SECTION.                                          
174300     SKIP1                                                                
174400     IF MID-KDBORT = 'B' OR 'D' OR 'J'                                    
174500        PERFORM EB-TA-BORT-POST                                           
174600*       MOVE MED-1 (SPRAK-IX) TO MOD-TEMFSINF                             
174700        MOVE '101'            TO MED-IDMFSINF                             
174800        CALL WMEDKONV     USING MED-WMEDAREA                              
174900        MOVE MED-MFSINF       TO MOD-TEMFSINF                             
175000        PERFORM S01-RENSA-HELA-SIDAN                                      
175100     ELSE                                                                 
175200         IF KONFLIKT                                                      
175300            PERFORM S04-SAETT-NUMFAELT-FEL                                
175400            PERFORM S06-MFS-ROER-EJ-FAELT                                 
175500*           MOVE FEL-2 (SPRAK-IX) TO MOD-TEMFSFEL                         
175600            MOVE '002'            TO MED-IDMFSFEL                         
175700            CALL WMEDKONV     USING MED-WMEDAREA                          
175800            MOVE MED-MFSFEL       TO MOD-TEMFSFEL                         
175900*           MOVE MED-2 (SPRAK-IX) TO MOD-TEMFSINF                         
176000            MOVE '102'            TO MED-IDMFSINF                         
176100            CALL WMEDKONV     USING MED-WMEDAREA                          
176200            MOVE MED-MFSINF       TO MOD-TEMFSINF                         
176300         ELSE                                                             
176400            PERFORM EA-LAGG-TILL-VISA-SIDAN                               
176500*           MOVE MED-1 (SPRAK-IX) TO MOD-TEMFSINF                         
176600            MOVE '101'            TO MED-IDMFSINF                         
176700            CALL WMEDKONV     USING MED-WMEDAREA                          
176800            MOVE MED-MFSINF       TO MOD-TEMFSINF                         
176900         END-IF                                                           
177000     END-IF                                                               
177100     .                                                                    
177200     EJECT                                                                
177300 EA-LAGG-TILL-VISA-SIDAN SECTION.                                         
177400     SKIP2                                                                
177500     IF W-DAREGDAT > ZERO                                                 
177600        MOVE W-IDUSER                TO W-IDUSER-SPAR                     
177700        MOVE W-DAREGDAT              TO W-DAREGDAT-SPAR                   
177800        MOVE W-TIREGTID              TO W-TIREGTID-SPAR                   
177900        IF MID-IDFSGURV = ALL '+'                                         
178000           CONTINUE                                                       
178100        ELSE                                                              
178200           MOVE MID-IDFSGURV         TO WS-IDFSGURV                       
178300        END-IF                                                            
178400        IF MID-IDUSER-IN = ALL '+'                                        
178500           IF W-IDUSER = MSG-SIGNON-USERID                                
178600              CONTINUE                                                    
178700           ELSE                                                           
178800              IF MID-IDFSGURV = ALL '+'                                   
178900                 MOVE SPACE             TO WS-IDFSGURV                    
179000              END-IF                                                      
179100           END-IF                                                         
179200        ELSE                                                              
179300           MOVE MID-IDUSER-IN        TO W-IDUSER                          
179400                                        MOD-IDUSER-UT                     
179500        END-IF                                                            
179600        MOVE WS-IDFSGURV             TO USER-IDFSGURV                     
179700                                        MOD-IDFSGURV                      
179800                                        MOD-IDFSGURV-UT                   
179900        PERFORM EAA-SKAPA-NYTT-01-11-SEGM                                 
180000        PERFORM EAB-ISRTA-INMATADE-IDFKNGRP                               
180100        PERFORM IMS-GU-FSGA01-SPAR                                        
180200        IF SEGMENT-FINNS                                                  
180300           PERFORM IMS-GNP-FSGA12-SPAR                                    
180400           IF SEGMENT-FINNS                                               
180500              PERFORM EAC-KOPIERA-IDFKNGRP                                
180600              IF MID-KDCMD = 'B' OR 'D'                                   
180700                 PERFORM EAD-TA-BORT-IDFKNGRP                             
180800              END-IF                                                      
180900              IF MID-IDFKNGRP-FOM (8) = ALL '+'                           
181000                 PERFORM IMS-GU-FSGA01                                    
181100                 IF SEGMENT-FINNS                                         
181200                    PERFORM S03-VISA-FKNGRP-FSGA12                        
181300                 END-IF                                                   
181400              ELSE                                                        
181500                 PERFORM EAF-RENSA-IDFKNGRP                               
181600              END-IF                                                      
181700           END-IF                                                         
181800           IF  MID-IDFSGURV  = ALL '+'                                    
181900           AND MID-IDUSER-IN = ALL '+'                                    
182000           AND W-IDUSER-SPAR = MSG-SIGNON-USERID                          
182100              PERFORM IMS-GHU-FSGA01-SPAR                                 
182200              IF SEGMENT-FINNS                                            
182300                 PERFORM IMS-DLET-FSGA-SPAR                               
182400              END-IF                                                      
182500           END-IF                                                         
182600        END-IF                                                            
182700     ELSE                                                                 
182800        IF MID-IDFSGURV = ALL '+'                                         
182900           MOVE SPACE             TO USER-IDFSGURV                        
183000                                     MOD-IDFSGURV                         
183100                                     MOD-IDFSGURV-UT                      
183200        ELSE                                                              
183300           MOVE MID-IDFSGURV      TO USER-IDFSGURV                        
183400                                     MOD-IDFSGURV                         
183500                                     MOD-IDFSGURV-UT                      
183600        END-IF                                                            
183700        PERFORM EAA-SKAPA-NYTT-01-11-SEGM                                 
183800        PERFORM EAB-ISRTA-INMATADE-IDFKNGRP                               
183900        IF MID-IDFKNGRP-FOM (8) = ALL '+'                                 
184000           PERFORM IMS-GU-FSGA01                                          
184100           IF SEGMENT-FINNS                                               
184200              PERFORM S03-VISA-FKNGRP-FSGA12                              
184300           END-IF                                                         
184400        ELSE                                                              
184500           PERFORM EAF-RENSA-IDFKNGRP                                     
184600        END-IF                                                            
184700     END-IF                                                               
184800     PERFORM EAE-FORMATETS-ATTR                                           
184900     .                                                                    
185000     EJECT                                                                
185100 EAA-SKAPA-NYTT-01-11-SEGM SECTION.                                       
185200     SKIP2                                                                
185300     MOVE FUNCTION CURRENT-DATE(1:8) TO W-DAREGDAT                        
185400     ACCEPT WS-TIREGTID  FROM TIME                                        
185500     MOVE WS-TIREGTID-HHMMSS   TO W-TIREGTID                              
185600     MOVE W-IDUSER             TO USER-IDUSER                             
185700     MOVE W-DAREGDAT           TO USER-DAREGDAT                           
185800     MOVE W-DAREGDAT           TO MOD-DAREGDAT-DOLD                       
185900     MOVE W-TIREGTID           TO USER-TIREGTID                           
186000                                  MOD-TIREGTID-DOLD                       
186100     MOVE '3202'               TO USER-IDTRANS                            
186200     IF  USER-IDFSGURV = 'STOPPAD '                                       
186300        MOVE 'N'               TO USER-FLLISTA                            
186400     ELSE                                                                 
186500        MOVE 'J'               TO USER-FLLISTA                            
186600     END-IF                                                               
186700     PERFORM IMS-ISRT-FSGA01                                              
186800     MOVE '1'                  TO WS-URV1-KDSEGKEY                        
186900     MOVE WS-WLFSGA11          TO WLFSGA11                                
187000     PERFORM S02-VISA-FSGA11                                              
187100     PERFORM IMS-ISRT-FSGA11                                              
187200     .                                                                    
187300     EJECT                                                                
187400 EAB-ISRTA-INMATADE-IDFKNGRP   SECTION.                                   
187500     SKIP2                                                                
187600     MOVE +1                   TO RAD-INDX                                
187700     PERFORM UNTIL RAD-INDX > 8                                           
187800        IF MID-IDFKNGRP-FOM (RAD-INDX) = ALL '+'                          
187900           CONTINUE                                                       
188000        ELSE                                                              
188100           MOVE MID-IDFKNGRP-FOM(RAD-INDX) TO URV1-IDFKNGRP-FOM           
188200           MOVE MID-IDFKNGRP-TOM(RAD-INDX) TO URV1-IDFKNGRP-TOM           
188300           PERFORM IMS-ISRT-FSGA12                                        
188400        END-IF                                                            
188500        ADD +1                 TO RAD-INDX                                
188600     END-PERFORM                                                          
188700     .                                                                    
188800     EJECT                                                                
188900 EAC-KOPIERA-IDFKNGRP          SECTION.                                   
189000     SKIP2                                                                
189100     PERFORM UNTIL SEGMENT-SAKNAS                                         
189200        MOVE SPAR-URV1-IDFKNGRP-FOM TO URV1-IDFKNGRP-FOM                  
189300        MOVE SPAR-URV1-IDFKNGRP-TOM TO URV1-IDFKNGRP-TOM                  
189400        PERFORM IMS-ISRT-FSGA12                                           
189500        PERFORM IMS-GNP-FSGA12-SPAR                                       
189600     END-PERFORM                                                          
189700     .                                                                    
189800     EJECT                                                                
189900 EAD-TA-BORT-IDFKNGRP  SECTION.                                           
190000     SKIP1                                                                
190100     MOVE MFS-RENSA-FAELT           TO MOD-KDCMD                          
190200     MOVE +1                        TO RAD-INDX                           
190300     PERFORM UNTIL RAD-INDX > 4                                           
190400        IF MID-IDFKNGRP-FOM-IN (RAD-INDX) = ALL '+'                       
190500           CONTINUE                                                       
190600        ELSE                                                              
190700           MOVE MID-IDFKNGRP-FOM-IN (RAD-INDX) TO W-IDFKNGRP-FOM          
190800           MOVE MID-IDFKNGRP-TOM-IN (RAD-INDX) TO W-IDFKNGRP-TOM          
190900           PERFORM IMS-GHU-FSGA12                                         
191000           IF SEGMENT-FINNS                                               
191100              PERFORM IMS-DLET-FSGA                                       
191200           END-IF                                                         
191300           MOVE MFS-RENSA-FAELT     TO                                    
191400                               MOD-IDFKNGRP-FOM-IN (RAD-INDX)             
191500                               MOD-IDFKNGRP-TOM-IN (RAD-INDX)             
191600        END-IF                                                            
191700        ADD +1                      TO RAD-INDX                           
191800     END-PERFORM                                                          
191900     .                                                                    
192000     EJECT                                                                
192100 EAE-FORMATETS-ATTR  SECTION.                                             
192200     SKIP2                                                                
192300        MOVE MFS-FORMATETS-ATTR      TO MOD-TIFSGVV-FOM-ATTR              
192400                                        MOD-IDUSER-IN-ATTR                
192500                                        MOD-IDFSGURV-ATTR                 
192600                                        MOD-KDBORT-ATTR                   
192700                                        MOD-IDPTYP-ATTR                   
192800                                        MOD-TIFSGVV-TOM-ATTR              
192900                                        MOD-KDPRTYPG-ATTR                 
193000                                        MOD-KDNIVA-ATTR                   
193100                                        MOD-KDSVAR-ATTR                   
193200                                        MOD-KDCMD-ATTR                    
193300     MOVE +1                      TO RAD-INDX                             
193400     PERFORM UNTIL RAD-INDX > 8                                           
193500        IF RAD-INDX > 7                                                   
193600           MOVE MFS-FORMATETS-ATTR      TO                                
193700                            MOD-IDKONCNR-ATTR     (RAD-INDX)              
193800                            MOD-IDLEVNR-ATTR      (RAD-INDX)              
193900                            MOD-IDLKTO-ATTR       (RAD-INDX)              
194000                            MOD-KDMARK-FOM-ATTR   (RAD-INDX)              
194100                            MOD-KDMARK-TOM-ATTR   (RAD-INDX)              
194200                            MOD-IDFKNGRP-FOM-ATTR (RAD-INDX)              
194300                            MOD-IDFKNGRP-TOM-ATTR (RAD-INDX)              
194400        ELSE                                                              
194500           IF RAD-INDX > 5                                                
194600              MOVE MFS-FORMATETS-ATTR   TO                                
194700                               MOD-KDPRODSL-ATTR  (RAD-INDX)              
194800                               MOD-IDKONCNR-ATTR  (RAD-INDX)              
194900                               MOD-IDLEVNR-ATTR   (RAD-INDX)              
195000                               MOD-IDLKTO-ATTR    (RAD-INDX)              
195100                               MOD-KDMARK-FOM-ATTR (RAD-INDX)             
195200                               MOD-KDMARK-TOM-ATTR (RAD-INDX)             
195300                               MOD-IDFKNGRP-FOM-ATTR (RAD-INDX)           
195400                               MOD-IDFKNGRP-TOM-ATTR (RAD-INDX)           
195500           ELSE                                                           
195600              IF RAD-INDX > 4                                             
195700                 MOVE MFS-FORMATETS-ATTR   TO                             
195800                               MOD-KDVVKL-ATTR    (RAD-INDX)              
195900                               MOD-KDPRODSL-ATTR  (RAD-INDX)              
196000                               MOD-IDKONCNR-ATTR  (RAD-INDX)              
196100                               MOD-IDLEVNR-ATTR   (RAD-INDX)              
196200                               MOD-IDLKTO-ATTR    (RAD-INDX)              
196300                               MOD-KDMARK-FOM-ATTR (RAD-INDX)             
196400                               MOD-KDMARK-TOM-ATTR (RAD-INDX)             
196500                               MOD-IDFKNGRP-FOM-ATTR (RAD-INDX)           
196600                               MOD-IDFKNGRP-TOM-ATTR (RAD-INDX)           
196700              ELSE                                                        
196800                 MOVE MFS-FORMATETS-ATTR   TO                             
196900                               MOD-KDVVKL-ATTR    (RAD-INDX)              
197000                               MOD-KDPRODSL-ATTR  (RAD-INDX)              
197100                               MOD-IDKONCNR-ATTR  (RAD-INDX)              
197200                               MOD-IDLEVNR-ATTR   (RAD-INDX)              
197300                               MOD-IDLKTO-ATTR    (RAD-INDX)              
197400                               MOD-KDMARK-FOM-ATTR (RAD-INDX)             
197500                               MOD-KDMARK-TOM-ATTR (RAD-INDX)             
197600                               MOD-IDDISTR-FOM-ATTR (RAD-INDX)            
197700                               MOD-IDDISTR-TOM-ATTR (RAD-INDX)            
197800                               MOD-IDANSK-FOM-ATTR (RAD-INDX)             
197900                               MOD-IDANSK-TOM-ATTR (RAD-INDX)             
198000                               MOD-IDFKNGRP-FOM-ATTR (RAD-INDX)           
198100                               MOD-IDFKNGRP-TOM-ATTR (RAD-INDX)           
198200                            MOD-IDFKNGRP-FOM-IN-ATTR (RAD-INDX)           
198300                            MOD-IDFKNGRP-TOM-IN-ATTR (RAD-INDX)           
198400              END-IF                                                      
198500           END-IF                                                         
198600        END-IF                                                            
198700        ADD +1                    TO RAD-INDX                             
198800     END-PERFORM                                                          
198900     .                                                                    
199000     EJECT                                                                
199100 EAF-RENSA-IDFKNGRP  SECTION.                                             
199200     SKIP2                                                                
199300     MOVE MFS-ROER-EJ-FAELT       TO MOD-IDFKNGRP-FOM-LO                  
199400                                     MOD-IDFKNGRP-TOM-LO                  
199500                                     MOD-IDFKNGRP-FOM-HI                  
199600                                     MOD-IDFKNGRP-TOM-HI                  
199700     MOVE +1                      TO RAD-INDX                             
199800     PERFORM UNTIL RAD-INDX > 8                                           
199900        MOVE MFS-RENSA-FAELT         TO MOD-IDFKNGRP-FOM(RAD-INDX)        
200000        ADD +1                       TO RAD-INDX                          
200100     END-PERFORM                                                          
200200     .                                                                    
200300     EJECT                                                                
200400 EB-TA-BORT-POST   SECTION.                                               
200500     SKIP1                                                                
200600     PERFORM IMS-GHU-FSGA01                                               
200700     IF SEGMENT-FINNS                                                     
200800        PERFORM IMS-DLET-FSGA                                             
200900     END-IF                                                               
201000     .                                                                    
201100     EJECT                                                                
201200 F-MFS-ROER-EJ-FAELT SECTION.                                             
201300     SKIP2                                                                
201400     MOVE MFS-ROER-EJ-FAELT       TO MOD-TIFSGVV-FOM                      
201500                                     MOD-KDCMD                            
201600                                     MOD-KDBORT                           
201700                                     MOD-KDPRTYPG                         
201800                                     MOD-IDFSGURV                         
201900                                     MOD-IDPTYP                           
202000                                     MOD-TIFSGVV-TOM                      
202100                                     MOD-KDNIVA                           
202200                                     MOD-KDSVAR                           
202300                                     MOD-IDFKNGRP-FOM-LO                  
202400                                     MOD-IDFKNGRP-TOM-LO                  
202500                                     MOD-IDFKNGRP-FOM-HI                  
202600                                     MOD-IDFKNGRP-TOM-HI                  
202700                                     MOD-DAREGDAT-DOLD                    
202800                                     MOD-TIREGTID-DOLD                    
202900                                     MOD-IDUSER-IN                        
203000     MOVE +1                      TO RAD-INDX                             
203100     PERFORM UNTIL RAD-INDX > 8                                           
203200        IF RAD-INDX > 7                                                   
203300           MOVE MFS-ROER-EJ-FAELT    TO                                   
203400                        MOD-IDKONCNR             (RAD-INDX)               
203500                        MOD-IDLEVNR              (RAD-INDX)               
203600                        MOD-IDLKTO               (RAD-INDX)               
203700                        MOD-KDMARK-FOM           (RAD-INDX)               
203800                        MOD-KDMARK-TOM           (RAD-INDX)               
203900           IF  MID-IDFKNGRP-FOM (RAD-INDX) = ALL '+'                      
204000           AND MID-IDFKNGRP-TOM (RAD-INDX) = ALL '+'                      
204100              MOVE MFS-ROER-EJ-FAELT TO                                   
204200                        MOD-IDFKNGRP-FOM         (RAD-INDX)               
204300                        MOD-IDFKNGRP-TOM         (RAD-INDX)               
204400           ELSE                                                           
204500              IF  MID-IDFKNGRP-FOM (RAD-INDX) = ALL '+'                   
204600                 MOVE MFS-RENSA-FAELT   TO                                
204700                        MOD-IDFKNGRP-FOM         (RAD-INDX)               
204800              ELSE                                                        
204900                 MOVE MFS-ROER-EJ-FAELT TO                                
205000                        MOD-IDFKNGRP-FOM         (RAD-INDX)               
205100              END-IF                                                      
205200              IF  MID-IDFKNGRP-TOM (RAD-INDX) = ALL '+'                   
205300                 MOVE MFS-RENSA-FAELT   TO                                
205400                        MOD-IDFKNGRP-TOM         (RAD-INDX)               
205500              ELSE                                                        
205600                 MOVE MFS-ROER-EJ-FAELT TO                                
205700                        MOD-IDFKNGRP-TOM         (RAD-INDX)               
205800              END-IF                                                      
205900           END-IF                                                         
206000        ELSE                                                              
206100           IF RAD-INDX > 5                                                
206200              MOVE MFS-ROER-EJ-FAELT TO                                   
206300                           MOD-KDPRODSL          (RAD-INDX)               
206400                           MOD-IDKONCNR          (RAD-INDX)               
206500                           MOD-IDLEVNR           (RAD-INDX)               
206600                           MOD-IDLKTO            (RAD-INDX)               
206700                           MOD-KDMARK-FOM        (RAD-INDX)               
206800                           MOD-KDMARK-TOM        (RAD-INDX)               
206900              IF MID-IDFKNGRP-FOM (RAD-INDX) = ALL '+'                    
207000              AND MID-IDFKNGRP-TOM (RAD-INDX) = ALL '+'                   
207100                 MOVE MFS-ROER-EJ-FAELT TO                                
207200                           MOD-IDFKNGRP-FOM      (RAD-INDX)               
207300                           MOD-IDFKNGRP-TOM      (RAD-INDX)               
207400              ELSE                                                        
207500                 IF MID-IDFKNGRP-FOM (RAD-INDX) = ALL '+'                 
207600                    MOVE MFS-RENSA-FAELT TO                               
207700                           MOD-IDFKNGRP-FOM      (RAD-INDX)               
207800                 ELSE                                                     
207900                    MOVE MFS-ROER-EJ-FAELT TO                             
208000                           MOD-IDFKNGRP-FOM      (RAD-INDX)               
208100                 END-IF                                                   
208200                 IF MID-IDFKNGRP-TOM (RAD-INDX) = ALL '+'                 
208300                    MOVE MFS-RENSA-FAELT TO                               
208400                           MOD-IDFKNGRP-TOM      (RAD-INDX)               
208500                 ELSE                                                     
208600                    MOVE MFS-ROER-EJ-FAELT TO                             
208700                           MOD-IDFKNGRP-TOM      (RAD-INDX)               
208800                 END-IF                                                   
208900              END-IF                                                      
209000           ELSE                                                           
209100              IF RAD-INDX > 4                                             
209200                 MOVE MFS-ROER-EJ-FAELT TO                                
209300                           MOD-KDVVKL            (RAD-INDX)               
209400                           MOD-KDPRODSL          (RAD-INDX)               
209500                           MOD-IDKONCNR          (RAD-INDX)               
209600                           MOD-IDLEVNR           (RAD-INDX)               
209700                           MOD-IDLKTO            (RAD-INDX)               
209800                           MOD-KDMARK-FOM        (RAD-INDX)               
209900                           MOD-KDMARK-TOM        (RAD-INDX)               
210000                 IF MID-IDFKNGRP-FOM (RAD-INDX) = ALL '+'                 
210100                 AND MID-IDFKNGRP-TOM (RAD-INDX) = ALL '+'                
210200                    MOVE MFS-ROER-EJ-FAELT TO                             
210300                              MOD-IDFKNGRP-FOM      (RAD-INDX)            
210400                              MOD-IDFKNGRP-TOM      (RAD-INDX)            
210500                 ELSE                                                     
210600                    IF MID-IDFKNGRP-FOM (RAD-INDX) = ALL '+'              
210700                       MOVE MFS-RENSA-FAELT TO                            
210800                              MOD-IDFKNGRP-FOM      (RAD-INDX)            
210900                    ELSE                                                  
211000                       MOVE MFS-ROER-EJ-FAELT TO                          
211100                              MOD-IDFKNGRP-FOM      (RAD-INDX)            
211200                    END-IF                                                
211300                    IF MID-IDFKNGRP-TOM (RAD-INDX) = ALL '+'              
211400                       MOVE MFS-RENSA-FAELT TO                            
211500                              MOD-IDFKNGRP-TOM      (RAD-INDX)            
211600                    ELSE                                                  
211700                       MOVE MFS-ROER-EJ-FAELT TO                          
211800                              MOD-IDFKNGRP-TOM      (RAD-INDX)            
211900                    END-IF                                                
212000                 END-IF                                                   
212100              ELSE                                                        
212200                 MOVE MFS-ROER-EJ-FAELT TO                                
212300                           MOD-KDVVKL            (RAD-INDX)               
212400                           MOD-KDPRODSL          (RAD-INDX)               
212500                           MOD-IDKONCNR          (RAD-INDX)               
212600                           MOD-IDLEVNR           (RAD-INDX)               
212700                           MOD-IDLKTO            (RAD-INDX)               
212800                           MOD-KDMARK-FOM        (RAD-INDX)               
212900                           MOD-KDMARK-TOM        (RAD-INDX)               
213000                           MOD-IDDISTR-FOM       (RAD-INDX)               
213100                           MOD-IDDISTR-TOM       (RAD-INDX)               
213200                           MOD-IDANSK-FOM        (RAD-INDX)               
213300                           MOD-IDANSK-TOM        (RAD-INDX)               
213400                           MOD-IDFKNGRP-FOM-IN   (RAD-INDX)               
213500                           MOD-IDFKNGRP-TOM-IN   (RAD-INDX)               
213600                 IF MID-IDFKNGRP-FOM (RAD-INDX) = ALL '+'                 
213700                 AND MID-IDFKNGRP-TOM (RAD-INDX) = ALL '+'                
213800                    MOVE MFS-ROER-EJ-FAELT TO                             
213900                              MOD-IDFKNGRP-FOM      (RAD-INDX)            
214000                              MOD-IDFKNGRP-TOM      (RAD-INDX)            
214100                 ELSE                                                     
214200                    IF MID-IDFKNGRP-FOM (RAD-INDX) = ALL '+'              
214300                       MOVE MFS-RENSA-FAELT TO                            
214400                              MOD-IDFKNGRP-FOM      (RAD-INDX)            
214500                    ELSE                                                  
214600                       MOVE MFS-ROER-EJ-FAELT TO                          
214700                              MOD-IDFKNGRP-FOM      (RAD-INDX)            
214800                    END-IF                                                
214900                    IF MID-IDFKNGRP-TOM (RAD-INDX) = ALL '+'              
215000                       MOVE MFS-RENSA-FAELT TO                            
215100                              MOD-IDFKNGRP-TOM      (RAD-INDX)            
215200                    ELSE                                                  
215300                       MOVE MFS-ROER-EJ-FAELT TO                          
215400                              MOD-IDFKNGRP-TOM      (RAD-INDX)            
215500                    END-IF                                                
215600                 END-IF                                                   
215700              END-IF                                                      
215800           END-IF                                                         
215900        END-IF                                                            
216000        ADD +1                    TO RAD-INDX                             
216100     END-PERFORM                                                          
216200     .                                                                    
216300     EJECT                                                                
216400 H-LAES-FSGB-FSGA              SECTION.                                   
216500     SKIP1                                                                
216600     PERFORM IMS-GN-FSGB01                                                
216700     IF SEGMENT-FINNS                                                     
216800        MOVE SEQA-DAREGDAT    TO W-DAREGDAT                               
216900        MOVE SEQA-TIREGTID    TO W-TIREGTID                               
217000        PERFORM IMS-GU-FSGA01                                             
217100        IF SEGMENT-FINNS                                                  
217200           PERFORM S07-VISA-SIDAN                                         
217300        END-IF                                                            
217400     ELSE                                                                 
217500*       MOVE FEL-5 (SPRAK-IX) TO MOD-TEMFSFEL                             
217600        MOVE '005'            TO MED-IDMFSFEL                             
217700        CALL WMEDKONV     USING MED-WMEDAREA                              
217800        MOVE MED-MFSFEL       TO MOD-TEMFSFEL                             
217900        PERFORM S01-RENSA-HELA-SIDAN                                      
218000     END-IF                                                               
218100     .                                                                    
218200     EJECT                                                                
218300 S01-RENSA-HELA-SIDAN SECTION.                                            
218400     SKIP2                                                                
218500     MOVE ZERO                    TO MOD-DAREGDAT-DOLD                    
218600                                     MOD-TIREGTID-DOLD                    
218700                                     MOD-IDFKNGRP-FOM-LO                  
218800                                     MOD-IDFKNGRP-TOM-LO                  
218900                                     MOD-IDFKNGRP-FOM-HI                  
219000                                     MOD-IDFKNGRP-TOM-HI                  
219100     MOVE MFS-RENSA-FAELT         TO MOD-TIFSGVV-FOM                      
219200                                     MOD-IDUSER-IN                        
219300                                     MOD-IDFSGURV                         
219400                                     MOD-KDBORT                           
219500                                     MOD-IDPTYP                           
219600                                     MOD-TIFSGVV-TOM                      
219700                                     MOD-KDPRTYPG                         
219800                                     MOD-KDNIVA                           
219900                                     MOD-KDSVAR                           
220000                                     MOD-KDCMD                            
220100     MOVE +1                      TO RAD-INDX                             
220200     PERFORM UNTIL RAD-INDX > 8                                           
220300        IF RAD-INDX > 7                                                   
220400           MOVE MFS-RENSA-FAELT      TO                                   
220500                        MOD-IDKONCNR             (RAD-INDX)               
220600                        MOD-IDLEVNR              (RAD-INDX)               
220700                        MOD-IDLKTO               (RAD-INDX)               
220800                        MOD-KDMARK-FOM           (RAD-INDX)               
220900                        MOD-KDMARK-TOM           (RAD-INDX)               
221000                        MOD-IDFKNGRP-FOM         (RAD-INDX)               
221100                        MOD-IDFKNGRP-TOM         (RAD-INDX)               
221200        ELSE                                                              
221300           IF RAD-INDX > 5                                                
221400              MOVE MFS-RENSA-FAELT   TO                                   
221500                           MOD-KDPRODSL          (RAD-INDX)               
221600                           MOD-IDKONCNR          (RAD-INDX)               
221700                           MOD-IDLEVNR           (RAD-INDX)               
221800                           MOD-IDLKTO            (RAD-INDX)               
221900                           MOD-KDMARK-FOM        (RAD-INDX)               
222000                           MOD-KDMARK-TOM        (RAD-INDX)               
222100                           MOD-IDFKNGRP-FOM      (RAD-INDX)               
222200                           MOD-IDFKNGRP-TOM      (RAD-INDX)               
222300           ELSE                                                           
222400              IF RAD-INDX > 4                                             
222500                 MOVE MFS-RENSA-FAELT TO                                  
222600                           MOD-KDVVKL            (RAD-INDX)               
222700                           MOD-KDPRODSL          (RAD-INDX)               
222800                           MOD-IDKONCNR          (RAD-INDX)               
222900                           MOD-IDLEVNR           (RAD-INDX)               
223000                           MOD-IDLKTO            (RAD-INDX)               
223100                           MOD-KDMARK-FOM        (RAD-INDX)               
223200                           MOD-KDMARK-TOM        (RAD-INDX)               
223300                           MOD-IDFKNGRP-FOM      (RAD-INDX)               
223400                           MOD-IDFKNGRP-TOM      (RAD-INDX)               
223500              ELSE                                                        
223600                 MOVE MFS-RENSA-FAELT TO                                  
223700                           MOD-KDVVKL            (RAD-INDX)               
223800                           MOD-KDPRODSL          (RAD-INDX)               
223900                           MOD-IDKONCNR          (RAD-INDX)               
224000                           MOD-IDLEVNR           (RAD-INDX)               
224100                           MOD-IDLKTO            (RAD-INDX)               
224200                           MOD-KDMARK-FOM        (RAD-INDX)               
224300                           MOD-KDMARK-TOM        (RAD-INDX)               
224400                           MOD-IDDISTR-FOM       (RAD-INDX)               
224500                           MOD-IDDISTR-TOM       (RAD-INDX)               
224600                           MOD-IDANSK-FOM        (RAD-INDX)               
224700                           MOD-IDANSK-TOM        (RAD-INDX)               
224800                           MOD-IDFKNGRP-FOM      (RAD-INDX)               
224900                           MOD-IDFKNGRP-TOM      (RAD-INDX)               
225000                           MOD-IDFKNGRP-FOM-IN   (RAD-INDX)               
225100                           MOD-IDFKNGRP-TOM-IN   (RAD-INDX)               
225200              END-IF                                                      
225300           END-IF                                                         
225400        END-IF                                                            
225500        ADD +1                    TO RAD-INDX                             
225600     END-PERFORM                                                          
225700     .                                                                    
225800     EJECT                                                                
225900 S02-VISA-FSGA11 SECTION.                                                 
226000     SKIP2                                                                
226100     IF URV1-TIFSGVV-FOM = ZERO                                           
226200        MOVE MFS-RENSA-FAELT      TO MOD-TIFSGVV-FOM                      
226300     ELSE                                                                 
226400        MOVE URV1-TIFSGVV-FOM     TO MOD-TIFSGVV-FOM                      
226500     END-IF                                                               
226600     MOVE URV1-IDPTYP             TO MOD-IDPTYP                           
226700     IF URV1-TIFSGVV-TOM = ZERO                                           
226800        MOVE MFS-RENSA-FAELT      TO MOD-TIFSGVV-TOM                      
226900     ELSE                                                                 
227000        MOVE URV1-TIFSGVV-TOM     TO MOD-TIFSGVV-TOM                      
227100     END-IF                                                               
227200     MOVE URV1-KDPRTYP            TO MOD-KDPRTYPG                         
227300     MOVE URV1-KDNIVA             TO MOD-KDNIVA                           
227400     MOVE URV1-KDSVAR             TO MOD-KDSVAR                           
227500     MOVE +1                      TO RAD-INDX                             
227600     PERFORM UNTIL RAD-INDX > 8                                           
227700       IF RAD-INDX > 7                                                    
227800          IF URV1-IDKONCNR(RAD-INDX) = ZERO                               
227900             MOVE MFS-RENSA-FAELT                TO                       
228000                                     MOD-IDKONCNR (RAD-INDX)              
228100          ELSE                                                            
228200             MOVE URV1-IDKONCNR(RAD-INDX)        TO                       
228300                                     MOD-IDKONCNR (RAD-INDX)              
228400          END-IF                                                          
228500          IF URV1-IDLEVNR (RAD-INDX) = SPACE                              
228600             MOVE MFS-RENSA-FAELT                TO                       
228700                                     MOD-IDLEVNR (RAD-INDX)               
228800          ELSE                                                            
228900             MOVE URV1-IDLEVNR(RAD-INDX)         TO                       
229000                                     MOD-IDLEVNR (RAD-INDX)               
229100          END-IF                                                          
229200          IF URV1-IDLKTO (RAD-INDX) = ZERO                                
229300             MOVE MFS-RENSA-FAELT                TO                       
229400                                     MOD-IDLKTO (RAD-INDX)                
229500          ELSE                                                            
229600             MOVE URV1-IDLKTO(RAD-INDX)          TO                       
229700                                     MOD-IDLKTO (RAD-INDX)                
229800          END-IF                                                          
229900          IF URV1-KDMARK-FOM (RAD-INDX) = ZERO                            
230000             MOVE MFS-RENSA-FAELT             TO                          
230100                               MOD-KDMARK-FOM (RAD-INDX)                  
230200          ELSE                                                            
230300             MOVE URV1-KDMARK-FOM(RAD-INDX)   TO                          
230400                               MOD-KDMARK-FOM (RAD-INDX)                  
230500          END-IF                                                          
230600          IF URV1-KDMARK-TOM (RAD-INDX) = ZERO                            
230700             MOVE MFS-RENSA-FAELT             TO                          
230800                               MOD-KDMARK-TOM (RAD-INDX)                  
230900          ELSE                                                            
231000             MOVE URV1-KDMARK-TOM(RAD-INDX)   TO                          
231100                               MOD-KDMARK-TOM (RAD-INDX)                  
231200          END-IF                                                          
231300       ELSE                                                               
231400          IF RAD-INDX > 5                                                 
231500             IF URV1-IDKONCNR(RAD-INDX) = ZERO                            
231600                MOVE MFS-RENSA-FAELT             TO                       
231700                                     MOD-IDKONCNR (RAD-INDX)              
231800             ELSE                                                         
231900                MOVE URV1-IDKONCNR(RAD-INDX)     TO                       
232000                                     MOD-IDKONCNR (RAD-INDX)              
232100             END-IF                                                       
232200             IF URV1-IDLEVNR (RAD-INDX) = SPACE                           
232300                MOVE MFS-RENSA-FAELT             TO                       
232400                                     MOD-IDLEVNR (RAD-INDX)               
232500             ELSE                                                         
232600                MOVE URV1-IDLEVNR(RAD-INDX)      TO                       
232700                                     MOD-IDLEVNR (RAD-INDX)               
232800             END-IF                                                       
232900             IF URV1-IDLKTO (RAD-INDX) = ZERO                             
233000                MOVE MFS-RENSA-FAELT             TO                       
233100                                     MOD-IDLKTO (RAD-INDX)                
233200             ELSE                                                         
233300                MOVE URV1-IDLKTO(RAD-INDX)       TO                       
233400                                     MOD-IDLKTO (RAD-INDX)                
233500             END-IF                                                       
233600             IF URV1-KDMARK-FOM (RAD-INDX) = ZERO                         
233700                MOVE MFS-RENSA-FAELT             TO                       
233800                                  MOD-KDMARK-FOM (RAD-INDX)               
233900             ELSE                                                         
234000                MOVE URV1-KDMARK-FOM(RAD-INDX)   TO                       
234100                                  MOD-KDMARK-FOM (RAD-INDX)               
234200             END-IF                                                       
234300             IF URV1-KDMARK-TOM (RAD-INDX) = ZERO                         
234400                MOVE MFS-RENSA-FAELT             TO                       
234500                                  MOD-KDMARK-TOM (RAD-INDX)               
234600             ELSE                                                         
234700                MOVE URV1-KDMARK-TOM(RAD-INDX)   TO                       
234800                                  MOD-KDMARK-TOM (RAD-INDX)               
234900             END-IF                                                       
235000             IF URV1-KDPRODSL (RAD-INDX) = ZERO                           
235100                MOVE MFS-RENSA-FAELT                TO                    
235200                                     MOD-KDPRODSL (RAD-INDX)              
235300             ELSE                                                         
235400                MOVE URV1-KDPRODSL(RAD-INDX)        TO                    
235500                                     MOD-KDPRODSL (RAD-INDX)              
235600             END-IF                                                       
235700          ELSE                                                            
235800             IF RAD-INDX > 4                                              
235900                IF URV1-IDKONCNR(RAD-INDX) = ZERO                         
236000                   MOVE MFS-RENSA-FAELT             TO                    
236100                                     MOD-IDKONCNR (RAD-INDX)              
236200                ELSE                                                      
236300                   MOVE URV1-IDKONCNR(RAD-INDX)     TO                    
236400                                     MOD-IDKONCNR (RAD-INDX)              
236500                END-IF                                                    
236600                IF URV1-IDLEVNR (RAD-INDX) = SPACE                        
236700                   MOVE MFS-RENSA-FAELT             TO                    
236800                                     MOD-IDLEVNR (RAD-INDX)               
236900                ELSE                                                      
237000                   MOVE URV1-IDLEVNR(RAD-INDX)      TO                    
237100                                     MOD-IDLEVNR (RAD-INDX)               
237200                END-IF                                                    
237300                IF URV1-IDLKTO (RAD-INDX) = ZERO                          
237400                   MOVE MFS-RENSA-FAELT             TO                    
237500                                     MOD-IDLKTO (RAD-INDX)                
237600                ELSE                                                      
237700                   MOVE URV1-IDLKTO(RAD-INDX)       TO                    
237800                                     MOD-IDLKTO (RAD-INDX)                
237900                END-IF                                                    
238000                IF URV1-KDMARK-FOM (RAD-INDX) = ZERO                      
238100                   MOVE MFS-RENSA-FAELT             TO                    
238200                                     MOD-KDMARK-FOM (RAD-INDX)            
238300                ELSE                                                      
238400                   MOVE URV1-KDMARK-FOM(RAD-INDX)   TO                    
238500                                     MOD-KDMARK-FOM (RAD-INDX)            
238600                END-IF                                                    
238700                IF URV1-KDMARK-TOM (RAD-INDX) = ZERO                      
238800                   MOVE MFS-RENSA-FAELT             TO                    
238900                                     MOD-KDMARK-TOM (RAD-INDX)            
239000                ELSE                                                      
239100                   MOVE URV1-KDMARK-TOM(RAD-INDX)   TO                    
239200                                     MOD-KDMARK-TOM (RAD-INDX)            
239300                END-IF                                                    
239400                IF URV1-KDPRODSL (RAD-INDX) = ZERO                        
239500                   MOVE MFS-RENSA-FAELT             TO                    
239600                                     MOD-KDPRODSL (RAD-INDX)              
239700                ELSE                                                      
239800                   MOVE URV1-KDPRODSL(RAD-INDX)     TO                    
239900                                     MOD-KDPRODSL (RAD-INDX)              
240000                END-IF                                                    
240100                IF URV1-KDVVKL (RAD-INDX) = ZERO                          
240200                   MOVE MFS-RENSA-FAELT             TO                    
240300                                     MOD-KDVVKL (RAD-INDX)                
240400                ELSE                                                      
240500                   MOVE URV1-KDVVKL(RAD-INDX)       TO                    
240600                                     MOD-KDVVKL (RAD-INDX)                
240700                END-IF                                                    
240800             ELSE                                                         
240900                IF URV1-IDKONCNR(RAD-INDX) = ZERO                         
241000                   MOVE MFS-RENSA-FAELT             TO                    
241100                                     MOD-IDKONCNR (RAD-INDX)              
241200                ELSE                                                      
241300                   MOVE URV1-IDKONCNR(RAD-INDX)     TO                    
241400                                     MOD-IDKONCNR (RAD-INDX)              
241500                END-IF                                                    
241600                IF URV1-IDLEVNR (RAD-INDX) = SPACE                        
241700                   MOVE MFS-RENSA-FAELT             TO                    
241800                                     MOD-IDLEVNR (RAD-INDX)               
241900                ELSE                                                      
242000                   MOVE URV1-IDLEVNR(RAD-INDX)      TO                    
242100                                     MOD-IDLEVNR (RAD-INDX)               
242200                END-IF                                                    
242300                IF URV1-IDLKTO (RAD-INDX) = ZERO                          
242400                   MOVE MFS-RENSA-FAELT             TO                    
242500                                     MOD-IDLKTO (RAD-INDX)                
242600                ELSE                                                      
242700                   MOVE URV1-IDLKTO(RAD-INDX)       TO                    
242800                                     MOD-IDLKTO (RAD-INDX)                
242900                END-IF                                                    
243000                IF URV1-KDPRODSL (RAD-INDX) = ZERO                        
243100                   MOVE MFS-RENSA-FAELT             TO                    
243200                                     MOD-KDPRODSL (RAD-INDX)              
243300                ELSE                                                      
243400                   MOVE URV1-KDPRODSL(RAD-INDX)     TO                    
243500                                     MOD-KDPRODSL (RAD-INDX)              
243600                END-IF                                                    
243700                IF URV1-KDVVKL (RAD-INDX) = ZERO                          
243800                   MOVE MFS-RENSA-FAELT             TO                    
243900                                     MOD-KDVVKL (RAD-INDX)                
244000                ELSE                                                      
244100                   MOVE URV1-KDVVKL(RAD-INDX)       TO                    
244200                                     MOD-KDVVKL (RAD-INDX)                
244300                END-IF                                                    
244400                IF URV1-IDKONCNR (RAD-INDX) = ZERO                        
244500                   MOVE MFS-RENSA-FAELT             TO                    
244600                                     MOD-IDKONCNR (RAD-INDX)              
244700                ELSE                                                      
244800                   MOVE URV1-IDKONCNR(RAD-INDX)     TO                    
244900                                     MOD-IDKONCNR (RAD-INDX)              
245000                END-IF                                                    
245100                IF URV1-KDMARK-FOM (RAD-INDX) = ZERO                      
245200                   MOVE MFS-RENSA-FAELT             TO                    
245300                                     MOD-KDMARK-FOM (RAD-INDX)            
245400                ELSE                                                      
245500                   MOVE URV1-KDMARK-FOM(RAD-INDX)   TO                    
245600                                     MOD-KDMARK-FOM (RAD-INDX)            
245700                END-IF                                                    
245800                IF URV1-KDMARK-TOM (RAD-INDX) = ZERO                      
245900                   MOVE MFS-RENSA-FAELT             TO                    
246000                                     MOD-KDMARK-TOM (RAD-INDX)            
246100                ELSE                                                      
246200                   MOVE URV1-KDMARK-TOM(RAD-INDX)   TO                    
246300                                     MOD-KDMARK-TOM (RAD-INDX)            
246400                END-IF                                                    
246500                IF URV1-IDDISTR-FOM (RAD-INDX) = ZERO                     
246600                   MOVE MFS-RENSA-FAELT             TO                    
246700                                     MOD-IDDISTR-FOM (RAD-INDX)           
246800                ELSE                                                      
246900                   MOVE URV1-IDDISTR-FOM(RAD-INDX)  TO                    
247000                                     MOD-IDDISTR-FOM (RAD-INDX)           
247100                END-IF                                                    
247200                IF URV1-IDDISTR-TOM (RAD-INDX) = ZERO                     
247300                   MOVE MFS-RENSA-FAELT             TO                    
247400                                     MOD-IDDISTR-TOM (RAD-INDX)           
247500                ELSE                                                      
247600                   MOVE URV1-IDDISTR-TOM(RAD-INDX)  TO                    
247700                                     MOD-IDDISTR-TOM (RAD-INDX)           
247800                END-IF                                                    
247900                IF URV1-IDANSK-FOM (RAD-INDX) = ZERO                      
248000                   MOVE MFS-RENSA-FAELT             TO                    
248100                                     MOD-IDANSK-FOM (RAD-INDX)            
248200                ELSE                                                      
248300                   MOVE URV1-IDANSK-FOM(RAD-INDX)   TO                    
248400                                     MOD-IDANSK-FOM (RAD-INDX)            
248500                END-IF                                                    
248600                IF URV1-IDANSK-TOM (RAD-INDX) = ZERO                      
248700                   MOVE MFS-RENSA-FAELT             TO                    
248800                                     MOD-IDANSK-TOM (RAD-INDX)            
248900                ELSE                                                      
249000                   MOVE URV1-IDANSK-TOM(RAD-INDX)   TO                    
249100                                        MOD-IDANSK-TOM (RAD-INDX)         
249200                END-IF                                                    
249300             END-IF                                                       
249400          END-IF                                                          
249500       END-IF                                                             
249600       ADD +1                                 TO RAD-INDX                 
249700     END-PERFORM                                                          
249800     .                                                                    
249900     EJECT                                                                
250000 S03-VISA-FKNGRP-FSGA12 SECTION.                                          
250100     SKIP1                                                                
250200     MOVE +1                      TO RAD-INDX                             
250300     IF MFS-UPDATE                                                        
250400        IF  MID-IDFSGURV  = ALL '+'                                       
250500        AND MID-IDUSER-IN = ALL '+'                                       
250600           MOVE MID-IDFKNGRP-FOM-LO   TO W-IDFKNGRP-FOM                   
250700           MOVE MID-IDFKNGRP-TOM-LO   TO W-IDFKNGRP-TOM                   
250800        ELSE                                                              
250900           MOVE ZERO                  TO W-IDFKNGRP-FOM                   
251000           MOVE +99999                TO W-IDFKNGRP-TOM                   
251100        END-IF                                                            
251200     END-IF                                                               
251300     PERFORM IMS-GNP-FSGA12                                               
251400     IF SEGMENT-FINNS                                                     
251500        MOVE URV1-IDFKNGRP-FOM    TO MOD-IDFKNGRP-FOM-LO                  
251600        MOVE URV1-IDFKNGRP-TOM    TO MOD-IDFKNGRP-TOM-LO                  
251700        PERFORM UNTIL RAD-INDX > 8                                        
251800           IF SEGMENT-FINNS                                               
251900              MOVE URV1-IDFKNGRP-FOM   TO                                 
252000                               MOD-IDFKNGRP-FOM (RAD-INDX)                
252100              MOVE URV1-IDFKNGRP-TOM   TO                                 
252200                               MOD-IDFKNGRP-TOM (RAD-INDX)                
252300              PERFORM IMS-GNP-FSGA12                                      
252400           ELSE                                                           
252500              MOVE MFS-RENSA-FAELT     TO                                 
252600                               MOD-IDFKNGRP-FOM (RAD-INDX)                
252700                               MOD-IDFKNGRP-TOM (RAD-INDX)                
252800           END-IF                                                         
252900           IF RAD-INDX < 5                                                
253000              MOVE MFS-RENSA-FAELT     TO                                 
253100                            MOD-IDFKNGRP-FOM-IN (RAD-INDX)                
253200                            MOD-IDFKNGRP-TOM-IN (RAD-INDX)                
253300           END-IF                                                         
253400           ADD +1                    TO RAD-INDX                          
253500        END-PERFORM                                                       
253600     ELSE                                                                 
253700        MOVE ZERO                    TO MOD-IDFKNGRP-FOM-LO               
253800        MOVE +99999                  TO MOD-IDFKNGRP-TOM-LO               
253900     END-IF                                                               
254000     IF SEGMENT-FINNS                                                     
254100        MOVE URV1-IDFKNGRP-FOM       TO MOD-IDFKNGRP-FOM-HI               
254200        MOVE URV1-IDFKNGRP-TOM       TO MOD-IDFKNGRP-TOM-HI               
254300        IF MFS-UPDATE                                                     
254400           CONTINUE                                                       
254500        ELSE                                                              
254600*          MOVE MED-4 (SPRAK-IX)     TO MOD-TEMFSINF                      
254700           MOVE '105'            TO MED-IDMFSINF                          
254800           CALL WMEDKONV     USING MED-WMEDAREA                           
254900           MOVE MED-MFSINF       TO MOD-TEMFSINF                          
255000        END-IF                                                            
255100     ELSE                                                                 
255200        MOVE ZERO                    TO MOD-IDFKNGRP-FOM-HI               
255300        MOVE +99999                  TO MOD-IDFKNGRP-TOM-HI               
255400     END-IF                                                               
255500     .                                                                    
255600     EJECT                                                                
255700 S04-SAETT-NUMFAELT-FEL SECTION.                                          
255800     SKIP1                                                                
255900     MOVE +1                        TO RAD-INDX                           
256000     PERFORM UNTIL RAD-INDX > 8                                           
256100        IF RAD-INDX > 4                                                   
256200           IF MID-IDKONCNR (RAD-INDX) = ALL '+'                           
256300              CONTINUE                                                    
256400           ELSE                                                           
256500              MOVE MFS-NUM-FAELT-FEL TO                                   
256600                               MOD-IDKONCNR-ATTR (RAD-INDX)               
256700           END-IF                                                         
256800           IF MID-KDMARK-FOM (RAD-INDX) = ALL '+'                         
256900              CONTINUE                                                    
257000           ELSE                                                           
257100              MOVE MFS-NUM-FAELT-FEL TO                                   
257200                               MOD-KDMARK-FOM-ATTR (RAD-INDX)             
257300           END-IF                                                         
257400           IF MID-KDMARK-TOM (RAD-INDX) = ALL '+'                         
257500              CONTINUE                                                    
257600           ELSE                                                           
257700              MOVE MFS-NUM-FAELT-FEL TO                                   
257800                               MOD-KDMARK-TOM-ATTR (RAD-INDX)             
257900           END-IF                                                         
258000        ELSE                                                              
258100           IF MID-IDKONCNR (RAD-INDX) = ALL '+'                           
258200              CONTINUE                                                    
258300           ELSE                                                           
258400              MOVE MFS-NUM-FAELT-FEL TO                                   
258500                               MOD-IDKONCNR-ATTR (RAD-INDX)               
258600           END-IF                                                         
258700           IF MID-KDMARK-FOM (RAD-INDX) = ALL '+'                         
258800              CONTINUE                                                    
258900           ELSE                                                           
259000              MOVE MFS-NUM-FAELT-FEL TO                                   
259100                               MOD-KDMARK-FOM-ATTR (RAD-INDX)             
259200           END-IF                                                         
259300           IF MID-KDMARK-TOM (RAD-INDX) = ALL '+'                         
259400              CONTINUE                                                    
259500           ELSE                                                           
259600              MOVE MFS-NUM-FAELT-FEL TO                                   
259700                               MOD-KDMARK-TOM-ATTR (RAD-INDX)             
259800           END-IF                                                         
259900           IF MID-IDDISTR-FOM (RAD-INDX) = ALL '+'                        
260000              CONTINUE                                                    
260100           ELSE                                                           
260200              MOVE MFS-NUM-FAELT-FEL TO                                   
260300                               MOD-IDDISTR-FOM-ATTR (RAD-INDX)            
260400           END-IF                                                         
260500           IF MID-IDDISTR-TOM (RAD-INDX) = ALL '+'                        
260600              CONTINUE                                                    
260700           ELSE                                                           
260800              MOVE MFS-NUM-FAELT-FEL TO                                   
260900                               MOD-IDDISTR-TOM-ATTR (RAD-INDX)            
261000           END-IF                                                         
261100        END-IF                                                            
261200        ADD +1                      TO RAD-INDX                           
261300     END-PERFORM                                                          
261400     .                                                                    
261500     EJECT                                                                
261600 S05-KOLLA-BORTTAG-IDFKNGRP SECTION.                                      
261700     SKIP1                                                                
261800        IF MID-IDFKNGRP-FOM-IN (RAD-INDX) = ALL '+'                       
261900        AND MID-IDFKNGRP-TOM-IN (RAD-INDX) = ALL '+'                      
262000           CONTINUE                                                       
262100        ELSE                                                              
262200           ADD  +1                  TO ANTAL-ATT-TA-BORT                  
262300           IF MID-IDFKNGRP-FOM-IN (RAD-INDX) NUMERIC                      
262400              MOVE MFS-NUM-FAELT-RAETT  TO                                
262500                            MOD-IDFKNGRP-FOM-IN-ATTR (RAD-INDX)           
262600           ELSE                                                           
262700              MOVE NEJ               TO SW-INDATA-OK                      
262800              MOVE MFS-NUM-FAELT-FEL TO                                   
262900                            MOD-IDFKNGRP-FOM-IN-ATTR (RAD-INDX)           
263000           END-IF                                                         
263100           IF MID-IDFKNGRP-TOM-IN (RAD-INDX) NUMERIC                      
263200              MOVE MFS-NUM-FAELT-RAETT  TO                                
263300                         MOD-IDFKNGRP-TOM-IN-ATTR (RAD-INDX)              
263400           ELSE                                                           
263500              MOVE NEJ               TO SW-INDATA-OK                      
263600              MOVE MFS-NUM-FAELT-FEL TO                                   
263700                         MOD-IDFKNGRP-TOM-IN-ATTR (RAD-INDX)              
263800           END-IF                                                         
263900        END-IF                                                            
264000     .                                                                    
264100     EJECT                                                                
264200 S06-MFS-ROER-EJ-FAELT SECTION.                                           
264300     SKIP2                                                                
264400     MOVE MFS-ROER-EJ-FAELT       TO MOD-TIFSGVV-FOM                      
264500                                     MOD-IDUSER-IN                        
264600                                     MOD-IDFSGURV                         
264700                                     MOD-KDBORT                           
264800                                     MOD-IDPTYP                           
264900                                     MOD-TIFSGVV-TOM                      
265000                                     MOD-KDPRTYPG                         
265100                                     MOD-KDNIVA                           
265200                                     MOD-KDSVAR                           
265300                                     MOD-KDCMD                            
265400                                     MOD-IDFKNGRP-FOM-LO                  
265500                                     MOD-IDFKNGRP-TOM-LO                  
265600                                     MOD-IDFKNGRP-FOM-HI                  
265700                                     MOD-IDFKNGRP-TOM-HI                  
265800                                     MOD-DAREGDAT-DOLD                    
265900                                     MOD-TIREGTID-DOLD                    
266000     MOVE +1                      TO RAD-INDX                             
266100     PERFORM UNTIL RAD-INDX > 8                                           
266200        IF RAD-INDX > 7                                                   
266300           MOVE MFS-ROER-EJ-FAELT    TO                                   
266400                        MOD-IDKONCNR             (RAD-INDX)               
266500                        MOD-IDLEVNR              (RAD-INDX)               
266600                        MOD-IDLKTO               (RAD-INDX)               
266700                        MOD-KDMARK-FOM           (RAD-INDX)               
266800                        MOD-KDMARK-TOM           (RAD-INDX)               
266900                        MOD-IDFKNGRP-FOM         (RAD-INDX)               
267000                        MOD-IDFKNGRP-TOM         (RAD-INDX)               
267100        ELSE                                                              
267200           IF RAD-INDX > 5                                                
267300              MOVE MFS-ROER-EJ-FAELT TO                                   
267400                           MOD-KDPRODSL          (RAD-INDX)               
267500                           MOD-IDKONCNR          (RAD-INDX)               
267600                           MOD-IDLEVNR           (RAD-INDX)               
267700                           MOD-IDLKTO            (RAD-INDX)               
267800                           MOD-KDMARK-FOM        (RAD-INDX)               
267900                           MOD-KDMARK-TOM        (RAD-INDX)               
268000                           MOD-IDFKNGRP-FOM      (RAD-INDX)               
268100                           MOD-IDFKNGRP-TOM      (RAD-INDX)               
268200           ELSE                                                           
268300              IF RAD-INDX > 4                                             
268400                 MOVE MFS-ROER-EJ-FAELT TO                                
268500                           MOD-KDVVKL            (RAD-INDX)               
268600                           MOD-KDPRODSL          (RAD-INDX)               
268700                           MOD-IDKONCNR          (RAD-INDX)               
268800                           MOD-IDLEVNR           (RAD-INDX)               
268900                           MOD-IDLKTO            (RAD-INDX)               
269000                           MOD-KDMARK-FOM        (RAD-INDX)               
269100                           MOD-KDMARK-TOM        (RAD-INDX)               
269200                           MOD-IDFKNGRP-FOM      (RAD-INDX)               
269300                           MOD-IDFKNGRP-TOM      (RAD-INDX)               
269400              ELSE                                                        
269500                 MOVE MFS-ROER-EJ-FAELT TO                                
269600                           MOD-KDVVKL            (RAD-INDX)               
269700                           MOD-KDPRODSL          (RAD-INDX)               
269800                           MOD-IDKONCNR          (RAD-INDX)               
269900                           MOD-IDLEVNR           (RAD-INDX)               
270000                           MOD-IDLKTO            (RAD-INDX)               
270100                           MOD-KDMARK-FOM        (RAD-INDX)               
270200                           MOD-KDMARK-TOM        (RAD-INDX)               
270300                           MOD-IDDISTR-FOM       (RAD-INDX)               
270400                           MOD-IDDISTR-TOM       (RAD-INDX)               
270500                           MOD-IDANSK-FOM        (RAD-INDX)               
270600                           MOD-IDANSK-TOM        (RAD-INDX)               
270700                           MOD-IDFKNGRP-FOM      (RAD-INDX)               
270800                           MOD-IDFKNGRP-TOM      (RAD-INDX)               
270900                           MOD-IDFKNGRP-FOM-IN   (RAD-INDX)               
271000                           MOD-IDFKNGRP-TOM-IN   (RAD-INDX)               
271100              END-IF                                                      
271200           END-IF                                                         
271300        END-IF                                                            
271400        ADD +1                    TO RAD-INDX                             
271500     END-PERFORM                                                          
271600     .                                                                    
271700     EJECT                                                                
271800 S07-VISA-SIDAN SECTION.                                                  
271900     SKIP1                                                                
272000     MOVE W-DAREGDAT                  TO MOD-DAREGDAT-DOLD                
272100     MOVE W-TIREGTID                  TO MOD-TIREGTID-DOLD                
272200     IF SEGMENT-FINNS                                                     
272300        MOVE USER-IDFSGURV               TO MOD-IDFSGURV                  
272400        PERFORM IMS-GNP-FSGA11                                            
272500        IF SEGMENT-FINNS                                                  
272600           MOVE MFS-RENSA-FAELT          TO MOD-KDBORT                    
272700                                            MOD-KDPRTYPG                  
272800                                            MOD-KDBORT                    
272900                                            MOD-KDCMD                     
273000           MOVE +1                       TO RAD-INDX                      
273100           PERFORM UNTIL RAD-INDX > 4                                     
273200              MOVE MFS-RENSA-FAELT       TO                               
273300                                 MOD-IDFKNGRP-FOM-IN (RAD-INDX)           
273400              ADD +1                     TO RAD-INDX                      
273500           END-PERFORM                                                    
273600           PERFORM S02-VISA-FSGA11                                        
273700           PERFORM S03-VISA-FKNGRP-FSGA12                                 
273800        ELSE                                                              
273900           PERFORM S01-RENSA-HELA-SIDAN                                   
274000        END-IF                                                            
274100     ELSE                                                                 
274200        PERFORM S01-RENSA-HELA-SIDAN                                      
274300     END-IF                                                               
274400     .                                                                    
274500     EJECT                                                                
274600 S08-KOLLA-IDFKNGRP    SECTION.                                           
274700     SKIP2                                                                
274800     IF  MID-IDFKNGRP-FOM (RAD-INDX) = ALL '+'                            
274900     AND MID-IDFKNGRP-TOM (RAD-INDX) = ALL '+'                            
275000        CONTINUE                                                          
275100     ELSE                                                                 
275200        IF  MID-IDFKNGRP-FOM (RAD-INDX) = ALL '+'                         
275300        OR  ZERO                                                          
275400           MOVE NEJ                       TO SW-INDATA-OK                 
275500           MOVE MFS-NUM-FAELT-FEL         TO                              
275600                        MOD-IDFKNGRP-FOM-ATTR (RAD-INDX)                  
275700        ELSE                                                              
275800           IF MID-IDFKNGRP-FOM (RAD-INDX) NUMERIC                         
275900              MOVE MFS-NUM-FAELT-RAETT    TO                              
276000                        MOD-IDFKNGRP-FOM-ATTR (RAD-INDX)                  
276100           ELSE                                                           
276200              MOVE NEJ                       TO SW-INDATA-OK              
276300              MOVE MFS-NUM-FAELT-FEL         TO                           
276400                        MOD-IDFKNGRP-FOM-ATTR (RAD-INDX)                  
276500           END-IF                                                         
276600        END-IF                                                            
276700        IF  MID-IDFKNGRP-TOM (RAD-INDX) = ALL '+'                         
276800           MOVE NEJ                       TO SW-INDATA-OK                 
276900           MOVE MFS-NUM-FAELT-FEL         TO                              
277000                        MOD-IDFKNGRP-TOM-ATTR (RAD-INDX)                  
277100        ELSE                                                              
277200           IF MID-IDFKNGRP-TOM (RAD-INDX) NUMERIC                         
277300              MOVE MFS-NUM-FAELT-RAETT    TO                              
277400                        MOD-IDFKNGRP-TOM-ATTR (RAD-INDX)                  
277500           ELSE                                                           
277600              MOVE NEJ                       TO SW-INDATA-OK              
277700              MOVE MFS-NUM-FAELT-FEL         TO                           
277800                        MOD-IDFKNGRP-TOM-ATTR (RAD-INDX)                  
277900           END-IF                                                         
278000        END-IF                                                            
278100        IF INDATA-OK                                                      
278200           IF MID-IDFKNGRP-TOM (RAD-INDX) NOT <                           
278300           MID-IDFKNGRP-FOM (RAD-INDX)                                    
278400              MOVE MFS-NUM-FAELT-RAETT       TO                           
278500                        MOD-IDFKNGRP-FOM-ATTR (RAD-INDX)                  
278600                        MOD-IDFKNGRP-TOM-ATTR (RAD-INDX)                  
278700           ELSE                                                           
278800              MOVE NEJ                TO SW-INDATA-OK                     
278900              MOVE MFS-NUM-FAELT-FEL  TO                                  
279000                        MOD-IDFKNGRP-FOM-ATTR (RAD-INDX)                  
279100                        MOD-IDFKNGRP-TOM-ATTR (RAD-INDX)                  
279200           END-IF                                                         
279300        END-IF                                                            
279400     END-IF                                                               
279500     .                                                                    
279600     EJECT                                                                
279700 S09-KOLLA-ATT-INGET-IFYLLT SECTION.                                      
279800     SKIP2                                                                
279900     IF  MFS-UPDATE                                                       
280000           CONTINUE                                                       
280100        IF MID-IDUSER-IN = ALL '+'                                        
280200           CONTINUE                                                       
280300        ELSE                                                              
280400           MOVE JA                TO SW-IFYLLT                            
280500        END-IF                                                            
280600     ELSE                                                                 
280700        IF MID-KDBORT = ALL '+'                                           
280800           CONTINUE                                                       
280900        ELSE                                                              
281000           MOVE JA                TO SW-IFYLLT                            
281100        END-IF                                                            
281200     END-IF                                                               
281300     IF  MID-TIFSGVV-FOM    = ALL '+'                                     
281400     AND MID-IDFSGURV       = ALL '+'                                     
281500     AND MID-IDPTYP         = ALL '+'                                     
281600     AND MID-TIFSGVV-TOM    = ALL '+'                                     
281700     AND MID-KDPRTYPG       = ALL '+'                                     
281800     AND MID-KDCMD          = ALL '+'                                     
281900     AND MID-KDNIVA         = ALL '+'                                     
282000     AND MID-KDSVAR         = ALL '+'                                     
282100        CONTINUE                                                          
282200     ELSE                                                                 
282300        MOVE JA                   TO SW-IFYLLT                            
282400     END-IF                                                               
282500     MOVE +1                      TO RAD-INDX                             
282600     PERFORM UNTIL RAD-INDX > 8                                           
282700     OR            SW-IFYLLT = JA                                         
282800        IF RAD-INDX > 7                                                   
282900           IF  MID-IDKONCNR      (RAD-INDX) = ALL '+'                     
283000           AND MID-IDLEVNR       (RAD-INDX) = ALL '+'                     
283100           AND MID-IDLKTO        (RAD-INDX) = ALL '+'                     
283200           AND MID-KDMARK-FOM    (RAD-INDX) = ALL '+'                     
283300           AND MID-KDMARK-TOM    (RAD-INDX) = ALL '+'                     
283400           AND MID-IDFKNGRP-FOM  (RAD-INDX) = ALL '+'                     
283500           AND MID-IDFKNGRP-TOM  (RAD-INDX) = ALL '+'                     
283600              CONTINUE                                                    
283700           ELSE                                                           
283800              MOVE JA          TO SW-IFYLLT                               
283900           END-IF                                                         
284000        ELSE                                                              
284100           IF RAD-INDX > 5                                                
284200              IF MID-IDKONCNR    (RAD-INDX) = ALL '+'                     
284300              AND MID-KDPRODSL   (RAD-INDX) = ALL '+'                     
284400              AND MID-IDLEVNR    (RAD-INDX) = ALL '+'                     
284500              AND MID-IDLKTO     (RAD-INDX) = ALL '+'                     
284600              AND MID-KDMARK-FOM  (RAD-INDX) = ALL '+'                    
284700              AND MID-KDMARK-TOM  (RAD-INDX) = ALL '+'                    
284800              AND MID-IDFKNGRP-FOM (RAD-INDX) = ALL '+'                   
284900              AND MID-IDFKNGRP-TOM (RAD-INDX) = ALL '+'                   
285000                 CONTINUE                                                 
285100              ELSE                                                        
285200                 MOVE JA       TO SW-IFYLLT                               
285300              END-IF                                                      
285400           ELSE                                                           
285500              IF RAD-INDX > 4                                             
285600                 IF MID-KDVVKL      (RAD-INDX) = ALL '+'                  
285700                 AND MID-KDPRODSL   (RAD-INDX) = ALL '+'                  
285800                 AND MID-IDKONCNR   (RAD-INDX) = ALL '+'                  
285900                 AND MID-IDLEVNR    (RAD-INDX) = ALL '+'                  
286000                 AND MID-IDLKTO     (RAD-INDX) = ALL '+'                  
286100                 AND MID-KDMARK-FOM  (RAD-INDX) = ALL '+'                 
286200                 AND MID-KDMARK-TOM  (RAD-INDX) = ALL '+'                 
286300                 AND MID-IDFKNGRP-FOM (RAD-INDX) = ALL '+'                
286400                 AND MID-IDFKNGRP-TOM (RAD-INDX) = ALL '+'                
286500                    CONTINUE                                              
286600                 ELSE                                                     
286700                    MOVE JA       TO SW-IFYLLT                            
286800                 END-IF                                                   
286900              ELSE                                                        
287000                 IF MID-KDVVKL       (RAD-INDX) = ALL '+'                 
287100                 AND MID-KDPRODSL    (RAD-INDX) = ALL '+'                 
287200                 AND MID-IDKONCNR    (RAD-INDX) = ALL '+'                 
287300                 AND MID-IDLEVNR     (RAD-INDX) = ALL '+'                 
287400                 AND MID-IDLKTO      (RAD-INDX) = ALL '+'                 
287500                 AND MID-KDMARK-FOM  (RAD-INDX) = ALL '+'                 
287600                 AND MID-KDMARK-TOM  (RAD-INDX) = ALL '+'                 
287700                 AND MID-IDDISTR-FOM (RAD-INDX) = ALL '+'                 
287800                 AND MID-IDDISTR-TOM (RAD-INDX) = ALL '+'                 
287900                 AND MID-IDANSK-FOM  (RAD-INDX) = ALL '+'                 
288000                 AND MID-IDANSK-TOM  (RAD-INDX) = ALL '+'                 
288100                 AND MID-IDFKNGRP-FOM (RAD-INDX) = ALL '+'                
288200                 AND MID-IDFKNGRP-TOM (RAD-INDX) = ALL '+'                
288300                 AND MID-IDFKNGRP-FOM-IN (RAD-INDX) = ALL '+'             
288400                 AND MID-IDFKNGRP-TOM-IN (RAD-INDX) = ALL '+'             
288500                    CONTINUE                                              
288600                 ELSE                                                     
288700                    MOVE JA       TO SW-IFYLLT                            
288800                 END-IF                                                   
288900              END-IF                                                      
289000           END-IF                                                         
289100        END-IF                                                            
289200        ADD +1                    TO RAD-INDX                             
289300     END-PERFORM                                                          
289400     .                                                                    
289500     EJECT                                                                
289600 S10-LAES-IN-IGEN    SECTION.                                             
289700     SKIP2                                                                
289800     IF MFS-UPDATE                                                        
289900        IF MID-IDUSER-IN = ALL '+'                                        
290000           CONTINUE                                                       
290100        ELSE                                                              
290200           MOVE MFS-ADD-LAES-IN-FAELT  TO MOD-IDUSER-IN-ATTR              
290300        END-IF                                                            
290400     ELSE                                                                 
290500        IF MID-KDBORT = ALL '+'                                           
290600           CONTINUE                                                       
290700        ELSE                                                              
290800           MOVE MFS-ADD-LAES-IN-FAELT   TO MOD-KDBORT-ATTR                
290900        END-IF                                                            
291000     END-IF                                                               
291100     IF MID-TIFSGVV-FOM = ALL '+'                                         
291200        CONTINUE                                                          
291300     ELSE                                                                 
291400        MOVE MFS-ADD-LAES-IN-FAELT   TO MOD-TIFSGVV-FOM-ATTR              
291500     END-IF                                                               
291600     IF MID-IDFSGURV = ALL '+'                                            
291700        CONTINUE                                                          
291800     ELSE                                                                 
291900        MOVE MFS-ADD-LAES-IN-FAELT   TO MOD-IDFSGURV-ATTR                 
292000     END-IF                                                               
292100     IF MID-IDPTYP = ALL '+'                                              
292200        CONTINUE                                                          
292300     ELSE                                                                 
292400        MOVE MFS-ADD-LAES-IN-FAELT   TO MOD-IDPTYP-ATTR                   
292500     END-IF                                                               
292600     IF MID-TIFSGVV-TOM = ALL '+'                                         
292700        CONTINUE                                                          
292800     ELSE                                                                 
292900        MOVE MFS-ADD-LAES-IN-FAELT   TO MOD-TIFSGVV-TOM-ATTR              
293000     END-IF                                                               
293100     IF MID-KDPRTYPG = ALL '+'                                            
293200        CONTINUE                                                          
293300     ELSE                                                                 
293400        MOVE MFS-ADD-LAES-IN-FAELT   TO MOD-KDPRTYPG-ATTR                 
293500     END-IF                                                               
293600     IF MID-KDNIVA = ALL '+'                                              
293700        CONTINUE                                                          
293800     ELSE                                                                 
293900        MOVE MFS-ADD-LAES-IN-FAELT   TO MOD-KDNIVA-ATTR                   
294000     END-IF                                                               
294100     IF MID-KDSVAR = ALL '+'                                              
294200        CONTINUE                                                          
294300     ELSE                                                                 
294400        MOVE MFS-ADD-LAES-IN-FAELT   TO MOD-KDSVAR-ATTR                   
294500     END-IF                                                               
294600     IF MID-KDCMD = ALL '+'                                               
294700        CONTINUE                                                          
294800     ELSE                                                                 
294900        MOVE MFS-ADD-LAES-IN-FAELT   TO MOD-KDCMD-ATTR                    
295000     END-IF                                                               
295100     MOVE +1                      TO RAD-INDX                             
295200     PERFORM UNTIL RAD-INDX > 8                                           
295300        IF RAD-INDX > 7                                                   
295400           IF MID-IDKONCNR (RAD-INDX) = ALL '+'                           
295500              CONTINUE                                                    
295600           ELSE                                                           
295700              MOVE MFS-ADD-LAES-IN-FAELT TO                               
295800                            MOD-IDKONCNR-ATTR   (RAD-INDX)                
295900           END-IF                                                         
296000           IF MID-IDLEVNR (RAD-INDX) = ALL '+'                            
296100              CONTINUE                                                    
296200           ELSE                                                           
296300              MOVE MFS-ADD-LAES-IN-FAELT TO                               
296400                                   MOD-IDLEVNR-ATTR   (RAD-INDX)          
296500           END-IF                                                         
296600           IF MID-IDLKTO (RAD-INDX) = ALL '+'                             
296700              CONTINUE                                                    
296800           ELSE                                                           
296900              MOVE MFS-ADD-LAES-IN-FAELT TO                               
297000                            MOD-IDLKTO-ATTR (RAD-INDX)                    
297100           END-IF                                                         
297200           IF MID-KDMARK-FOM (RAD-INDX) = ALL '+'                         
297300              CONTINUE                                                    
297400           ELSE                                                           
297500              MOVE MFS-ADD-LAES-IN-FAELT TO                               
297600                          MOD-KDMARK-FOM-ATTR (RAD-INDX)                  
297700           END-IF                                                         
297800           IF MID-KDMARK-TOM (RAD-INDX) = ALL '+'                         
297900              CONTINUE                                                    
298000           ELSE                                                           
298100              MOVE MFS-ADD-LAES-IN-FAELT TO                               
298200                          MOD-KDMARK-TOM-ATTR (RAD-INDX)                  
298300           END-IF                                                         
298400           IF MID-IDFKNGRP-FOM (RAD-INDX) = ALL '+'                       
298500              CONTINUE                                                    
298600           ELSE                                                           
298700              MOVE MFS-ADD-LAES-IN-FAELT TO                               
298800                            MOD-IDFKNGRP-FOM-ATTR (RAD-INDX)              
298900           END-IF                                                         
299000           IF MID-IDFKNGRP-TOM (RAD-INDX) = ALL '+'                       
299100              CONTINUE                                                    
299200           ELSE                                                           
299300              MOVE MFS-ADD-LAES-IN-FAELT TO                               
299400                            MOD-IDFKNGRP-TOM-ATTR (RAD-INDX)              
299500           END-IF                                                         
299600        ELSE                                                              
299700           IF RAD-INDX > 5                                                
299800              IF MID-KDPRODSL (RAD-INDX) = ALL '+'                        
299900                 CONTINUE                                                 
300000              ELSE                                                        
300100                 MOVE MFS-ADD-LAES-IN-FAELT TO                            
300200                               MOD-KDPRODSL-ATTR (RAD-INDX)               
300300              END-IF                                                      
300400              IF MID-IDKONCNR (RAD-INDX) = ALL '+'                        
300500                 CONTINUE                                                 
300600              ELSE                                                        
300700                 MOVE MFS-ADD-LAES-IN-FAELT TO                            
300800                               MOD-IDKONCNR-ATTR (RAD-INDX)               
300900              END-IF                                                      
301000              IF MID-IDLEVNR (RAD-INDX) = ALL '+'                         
301100                 CONTINUE                                                 
301200              ELSE                                                        
301300                 MOVE MFS-ADD-LAES-IN-FAELT TO                            
301400                                      MOD-IDLEVNR-ATTR (RAD-INDX)         
301500              END-IF                                                      
301600              IF MID-IDLKTO (RAD-INDX) = ALL '+'                          
301700                 CONTINUE                                                 
301800              ELSE                                                        
301900                 MOVE MFS-ADD-LAES-IN-FAELT TO                            
302000                               MOD-IDLKTO-ATTR (RAD-INDX)                 
302100              END-IF                                                      
302200              IF MID-KDMARK-FOM (RAD-INDX) = ALL '+'                      
302300                 CONTINUE                                                 
302400              ELSE                                                        
302500                 MOVE MFS-ADD-LAES-IN-FAELT TO                            
302600                             MOD-KDMARK-FOM-ATTR (RAD-INDX)               
302700              END-IF                                                      
302800              IF MID-KDMARK-TOM (RAD-INDX) = ALL '+'                      
302900                 CONTINUE                                                 
303000              ELSE                                                        
303100                 MOVE MFS-ADD-LAES-IN-FAELT TO                            
303200                             MOD-KDMARK-TOM-ATTR (RAD-INDX)               
303300              END-IF                                                      
303400              IF MID-IDFKNGRP-FOM (RAD-INDX) = ALL '+'                    
303500                 CONTINUE                                                 
303600              ELSE                                                        
303700                 MOVE MFS-ADD-LAES-IN-FAELT TO                            
303800                               MOD-IDFKNGRP-FOM-ATTR (RAD-INDX)           
303900              END-IF                                                      
304000              IF MID-IDFKNGRP-TOM (RAD-INDX) = ALL '+'                    
304100                 CONTINUE                                                 
304200              ELSE                                                        
304300                 MOVE MFS-ADD-LAES-IN-FAELT TO                            
304400                               MOD-IDFKNGRP-TOM-ATTR (RAD-INDX)           
304500              END-IF                                                      
304600           ELSE                                                           
304700              IF RAD-INDX > 4                                             
304800                 IF MID-KDVVKL (RAD-INDX) = ALL '+'                       
304900                    CONTINUE                                              
305000                 ELSE                                                     
305100                    MOVE MFS-ADD-LAES-IN-FAELT TO                         
305200                                  MOD-KDVVKL-ATTR  (RAD-INDX)             
305300                 END-IF                                                   
305400                 IF MID-KDPRODSL (RAD-INDX) = ALL '+'                     
305500                    CONTINUE                                              
305600                 ELSE                                                     
305700                    MOVE MFS-ADD-LAES-IN-FAELT TO                         
305800                                  MOD-KDPRODSL-ATTR (RAD-INDX)            
305900                 END-IF                                                   
306000                 IF MID-IDKONCNR (RAD-INDX) = ALL '+'                     
306100                    CONTINUE                                              
306200                 ELSE                                                     
306300                    MOVE MFS-ADD-LAES-IN-FAELT TO                         
306400                                  MOD-IDKONCNR-ATTR (RAD-INDX)            
306500                 END-IF                                                   
306600                 IF MID-IDLEVNR (RAD-INDX) = ALL '+'                      
306700                    CONTINUE                                              
306800                 ELSE                                                     
306900                    MOVE MFS-ADD-LAES-IN-FAELT TO                         
307000                                    MOD-IDLEVNR-ATTR (RAD-INDX)           
307100                 END-IF                                                   
307200                 IF MID-IDLKTO (RAD-INDX) = ALL '+'                       
307300                    CONTINUE                                              
307400                 ELSE                                                     
307500                    MOVE MFS-ADD-LAES-IN-FAELT TO                         
307600                                  MOD-IDLKTO-ATTR (RAD-INDX)              
307700                 END-IF                                                   
307800                 IF MID-KDMARK-FOM (RAD-INDX) = ALL '+'                   
307900                    CONTINUE                                              
308000                 ELSE                                                     
308100                    MOVE MFS-ADD-LAES-IN-FAELT TO                         
308200                                MOD-KDMARK-FOM-ATTR (RAD-INDX)            
308300                 END-IF                                                   
308400                 IF MID-KDMARK-TOM (RAD-INDX) = ALL '+'                   
308500                    CONTINUE                                              
308600                 ELSE                                                     
308700                    MOVE MFS-ADD-LAES-IN-FAELT TO                         
308800                                MOD-KDMARK-TOM-ATTR (RAD-INDX)            
308900                 END-IF                                                   
309000                 IF MID-IDFKNGRP-FOM (RAD-INDX) = ALL '+'                 
309100                    CONTINUE                                              
309200                 ELSE                                                     
309300                    MOVE MFS-ADD-LAES-IN-FAELT TO                         
309400                                MOD-IDFKNGRP-FOM-ATTR (RAD-INDX)          
309500                 END-IF                                                   
309600                 IF MID-IDFKNGRP-TOM (RAD-INDX) = ALL '+'                 
309700                    CONTINUE                                              
309800                 ELSE                                                     
309900                    MOVE MFS-ADD-LAES-IN-FAELT TO                         
310000                                MOD-IDFKNGRP-TOM-ATTR (RAD-INDX)          
310100                 END-IF                                                   
310200              ELSE                                                        
310300                 IF MID-KDVVKL (RAD-INDX) = ALL '+'                       
310400                    CONTINUE                                              
310500                 ELSE                                                     
310600                    MOVE MFS-ADD-LAES-IN-FAELT TO                         
310700                                  MOD-KDVVKL-ATTR  (RAD-INDX)             
310800                 END-IF                                                   
310900                 IF MID-KDPRODSL (RAD-INDX) = ALL '+'                     
311000                    CONTINUE                                              
311100                 ELSE                                                     
311200                    MOVE MFS-ADD-LAES-IN-FAELT TO                         
311300                                  MOD-KDPRODSL-ATTR (RAD-INDX)            
311400                 END-IF                                                   
311500                 IF MID-IDKONCNR (RAD-INDX) = ALL '+'                     
311600                    CONTINUE                                              
311700                 ELSE                                                     
311800                    MOVE MFS-ADD-LAES-IN-FAELT TO                         
311900                                  MOD-IDKONCNR-ATTR (RAD-INDX)            
312000                 END-IF                                                   
312100                 IF MID-IDLEVNR (RAD-INDX) = ALL '+'                      
312200                    CONTINUE                                              
312300                 ELSE                                                     
312400                    MOVE MFS-ADD-LAES-IN-FAELT TO                         
312500                                    MOD-IDLEVNR-ATTR (RAD-INDX)           
312600                 END-IF                                                   
312700                 IF MID-IDLKTO (RAD-INDX) = ALL '+'                       
312800                    CONTINUE                                              
312900                 ELSE                                                     
313000                    MOVE MFS-ADD-LAES-IN-FAELT TO                         
313100                                  MOD-IDLKTO-ATTR (RAD-INDX)              
313200                 END-IF                                                   
313300                 IF MID-KDMARK-FOM (RAD-INDX) = ALL '+'                   
313400                    CONTINUE                                              
313500                 ELSE                                                     
313600                    MOVE MFS-ADD-LAES-IN-FAELT TO                         
313700                                MOD-KDMARK-FOM-ATTR (RAD-INDX)            
313800                 END-IF                                                   
313900                 IF MID-KDMARK-TOM (RAD-INDX) = ALL '+'                   
314000                    CONTINUE                                              
314100                 ELSE                                                     
314200                    MOVE MFS-ADD-LAES-IN-FAELT TO                         
314300                                MOD-KDMARK-TOM-ATTR (RAD-INDX)            
314400                 END-IF                                                   
314500                 IF MID-IDDISTR-FOM (RAD-INDX) = ALL '+'                  
314600                    CONTINUE                                              
314700                 ELSE                                                     
314800                    MOVE MFS-ADD-LAES-IN-FAELT TO                         
314900                                MOD-IDDISTR-FOM-ATTR (RAD-INDX)           
315000                 END-IF                                                   
315100                 IF MID-IDDISTR-TOM (RAD-INDX) = ALL '+'                  
315200                    CONTINUE                                              
315300                 ELSE                                                     
315400                    MOVE MFS-ADD-LAES-IN-FAELT TO                         
315500                                MOD-IDDISTR-TOM-ATTR (RAD-INDX)           
315600                 END-IF                                                   
315700                 IF MID-IDANSK-FOM (RAD-INDX) = ALL '+'                   
315800                    CONTINUE                                              
315900                 ELSE                                                     
316000                    MOVE MFS-ADD-LAES-IN-FAELT TO                         
316100                                MOD-IDANSK-FOM-ATTR (RAD-INDX)            
316200                 END-IF                                                   
316300                 IF MID-IDANSK-TOM (RAD-INDX) = ALL '+'                   
316400                    CONTINUE                                              
316500                 ELSE                                                     
316600                    MOVE MFS-ADD-LAES-IN-FAELT TO                         
316700                                MOD-IDANSK-TOM-ATTR (RAD-INDX)            
316800                 END-IF                                                   
316900                 IF MID-IDFKNGRP-FOM (RAD-INDX) = ALL '+'                 
317000                    CONTINUE                                              
317100                 ELSE                                                     
317200                    MOVE MFS-ADD-LAES-IN-FAELT TO                         
317300                                MOD-IDFKNGRP-FOM-ATTR (RAD-INDX)          
317400                 END-IF                                                   
317500                 IF MID-IDFKNGRP-TOM (RAD-INDX) = ALL '+'                 
317600                    CONTINUE                                              
317700                 ELSE                                                     
317800                    MOVE MFS-ADD-LAES-IN-FAELT TO                         
317900                                MOD-IDFKNGRP-TOM-ATTR (RAD-INDX)          
318000                 END-IF                                                   
318100                 IF MID-IDFKNGRP-FOM-IN (RAD-INDX) = ALL '+'              
318200                    CONTINUE                                              
318300                 ELSE                                                     
318400                    MOVE MFS-ADD-LAES-IN-FAELT TO                         
318500                           MOD-IDFKNGRP-FOM-IN-ATTR (RAD-INDX)            
318600                 END-IF                                                   
318700                 IF MID-IDFKNGRP-TOM-IN (RAD-INDX) = ALL '+'              
318800                    CONTINUE                                              
318900                 ELSE                                                     
319000                    MOVE MFS-ADD-LAES-IN-FAELT TO                         
319100                           MOD-IDFKNGRP-TOM-IN-ATTR (RAD-INDX)            
319200                 END-IF                                                   
319300              END-IF                                                      
319400           END-IF                                                         
319500         END-IF                                                           
319600        ADD +1                    TO RAD-INDX                             
319700     END-PERFORM                                                          
319800     .                                                                    
319900     EJECT                                                                
320000 S99-CALL-WDATKONV  SECTION.                                              
320100     SKIP1                                                                
320200     CALL WDATKONV             USING DAT-KDDATFORM                        
320300                                     DAT-I-TIDATUM                        
320400                                     DAT-O-TIDATUM                        
320500                                     DAT-KDSVAR                           
320600     .                                                                    
320700     EJECT                                                                
320800 IMS-GET-MSG SECTION.                                                     
320900     SKIP1                                                                
321000     MOVE '  QC' TO GODK-STATUSKODER                                      
321100     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
321200     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
321300     PERFORM IMS-STATUSKONTROLL                                           
321400     .                                                                    
321500     SKIP3                                                                
321600 IMS-INSERT-MSG SECTION.                                                  
321700     SKIP1                                                                
321800     IF NOT ENGLISH-TEXT                                                  
321900       MOVE '0' TO MFS-KDHUVOMR                                           
322000     END-IF                                                               
322100     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
322200     MOVE SPACE TO GODK-STATUSKODER                                       
322300     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
322400     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
322500     PERFORM IMS-STATUSKONTROLL                                           
322600     .                                                                    
322700     SKIP3                                                                
322800 IMS-GN-FSGB01 SECTION.                                                   
322900     SKIP1                                                                
323000     STRING 'WLFSGB01(WDM3A1KY>=' W-WDM3A1KY-MIN                          
323100                    '&WDM3A1KY<=' W-WDM3A1KY-MAX ')'                      
323200            DELIMITED BY SIZE INTO SSA1                                   
323300     MOVE '  GE' TO GODK-STATUSKODER                                      
323400     CALL CBLTDLI USING GN FSGB-PCB DLI-IO-AREA SSA1                      
323500     MOVE FSGB-STATUS-CODE TO STATUS-WS                                   
323600     PERFORM IMS-STATUSKONTROLL                                           
323700     .                                                                    
323800     SKIP3                                                                
323900 IMS-GHU-FSGA01 SECTION.                                                  
324000     SKIP1                                                                
324100     STRING 'WLFSGA01(WDM301KY =' W-WDM301KY-X ')'                        
324200            DELIMITED BY SIZE INTO SSA1                                   
324300     MOVE '  GE' TO GODK-STATUSKODER                                      
324400     CALL CBLTDLI USING GHU FSGA-PCB DLI-IO-AREA SSA1                     
324500     MOVE FSGA-STATUS-CODE TO STATUS-WS                                   
324600     PERFORM IMS-STATUSKONTROLL                                           
324700     .                                                                    
324800     SKIP3                                                                
324900 IMS-GU-FSGA01 SECTION.                                                   
325000     SKIP1                                                                
325100     STRING 'WLFSGA01(WDM301KY =' W-WDM301KY-X ')'                        
325200            DELIMITED BY SIZE INTO SSA1                                   
325300     MOVE '  GE' TO GODK-STATUSKODER                                      
325400     CALL CBLTDLI USING GU FSGA-PCB DLI-IO-AREA SSA1                      
325500     MOVE FSGA-STATUS-CODE TO STATUS-WS                                   
325600     PERFORM IMS-STATUSKONTROLL                                           
325700     .                                                                    
325800     SKIP3                                                                
325900 IMS-GNP-FSGA11 SECTION.                                                  
326000     SKIP1                                                                
326100     STRING 'WLFSGA01(WDM301KY =' W-WDM301KY-X ')'                        
326200            DELIMITED BY SIZE INTO SSA1                                   
326300     STRING 'WLFSGA11(KDSEGKEY>=' W-KDSEGKEY-X ')'                        
326400            DELIMITED BY SIZE INTO SSA2                                   
326500     MOVE '  GE' TO GODK-STATUSKODER                                      
326600     CALL CBLTDLI USING GNP FSGA-PCB DLI-IO-AREA SSA1 SSA2                
326700     MOVE FSGA-STATUS-CODE TO STATUS-WS                                   
326800     PERFORM IMS-STATUSKONTROLL                                           
326900     .                                                                    
327000     SKIP3                                                                
327100 IMS-GNP-FSGA12 SECTION.                                                  
327200     SKIP1                                                                
327300     STRING 'WLFSGA01(WDM301KY =' W-WDM301KY-X ')'                        
327400            DELIMITED BY SIZE INTO SSA1                                   
327500     STRING 'WLFSGA12(WDM312KY>=' W-IDFKNGRP-X ')'                        
327600            DELIMITED BY SIZE INTO SSA2                                   
327700     MOVE '  GE' TO GODK-STATUSKODER                                      
327800     CALL CBLTDLI USING GNP FSGA-PCB DLI-IO-AREA SSA1 SSA2                
327900     MOVE FSGA-STATUS-CODE TO STATUS-WS                                   
328000     PERFORM IMS-STATUSKONTROLL                                           
328100     .                                                                    
328200     SKIP3                                                                
328300 IMS-ISRT-FSGA01 SECTION.                                                 
328400     SKIP1                                                                
328500     MOVE   'WLFSGA01 '            TO SSA1                                
328600     MOVE '  ' TO GODK-STATUSKODER                                        
328700     CALL CBLTDLI USING ISRT FSGA-PCB DLI-IO-AREA SSA1                    
328800     MOVE FSGA-STATUS-CODE TO STATUS-WS                                   
328900     PERFORM IMS-STATUSKONTROLL                                           
329000     .                                                                    
329100     SKIP3                                                                
329200 IMS-ISRT-FSGA11 SECTION.                                                 
329300     SKIP1                                                                
329400     STRING 'WLFSGA01(WDM301KY =' W-WDM301KY-X ')'                        
329500            DELIMITED BY SIZE INTO SSA1                                   
329600     MOVE   'WLFSGA11 '            TO SSA2                                
329700     MOVE '  ' TO GODK-STATUSKODER                                        
329800     CALL CBLTDLI USING ISRT FSGA-PCB DLI-IO-AREA SSA1 SSA2               
329900     MOVE FSGA-STATUS-CODE TO STATUS-WS                                   
330000     PERFORM IMS-STATUSKONTROLL                                           
330100     .                                                                    
330200     SKIP3                                                                
330300 IMS-GU-FSGA12 SECTION.                                                   
330400     SKIP1                                                                
330500     STRING 'WLFSGA01(WDM301KY =' W-WDM301KY-X ')'                        
330600            DELIMITED BY SIZE INTO SSA1                                   
330700     STRING 'WLFSGA12(WDM312KY =' W-IDFKNGRP-X ')'                        
330800            DELIMITED BY SIZE INTO SSA2                                   
330900     MOVE '  GE' TO GODK-STATUSKODER                                      
331000     CALL CBLTDLI USING GU FSGA-PCB DLI-IO-AREA SSA1 SSA2                 
331100     MOVE FSGA-STATUS-CODE TO STATUS-WS                                   
331200     PERFORM IMS-STATUSKONTROLL                                           
331300     .                                                                    
331400     SKIP3                                                                
331500 IMS-GHU-FSGA12 SECTION.                                                  
331600     SKIP1                                                                
331700     STRING 'WLFSGA01(WDM301KY =' W-WDM301KY-X ')'                        
331800            DELIMITED BY SIZE INTO SSA1                                   
331900     STRING 'WLFSGA12(WDM312KY =' W-IDFKNGRP-X ')'                        
332000            DELIMITED BY SIZE INTO SSA2                                   
332100     MOVE '  GE' TO GODK-STATUSKODER                                      
332200     CALL CBLTDLI USING GHU FSGA-PCB DLI-IO-AREA SSA1 SSA2                
332300     MOVE FSGA-STATUS-CODE TO STATUS-WS                                   
332400     PERFORM IMS-STATUSKONTROLL                                           
332500     .                                                                    
332600     SKIP3                                                                
332700 IMS-ISRT-FSGA12 SECTION.                                                 
332800     SKIP1                                                                
332900     STRING 'WLFSGA01(WDM301KY =' W-WDM301KY-X ')'                        
333000            DELIMITED BY SIZE INTO SSA1                                   
333100     MOVE   'WLFSGA12 '            TO SSA2                                
333200     MOVE '  II' TO GODK-STATUSKODER                                      
333300     CALL CBLTDLI USING ISRT FSGA-PCB DLI-IO-AREA SSA1 SSA2               
333400     MOVE FSGA-STATUS-CODE TO STATUS-WS                                   
333500     PERFORM IMS-STATUSKONTROLL                                           
333600     .                                                                    
333700     SKIP3                                                                
333800 IMS-GHU-FSGA01-SPAR SECTION.                                             
333900     SKIP1                                                                
334000     STRING 'WLFSGA01(WDM301KY =' W-WDM301KY-SPAR ')'                     
334100            DELIMITED BY SIZE INTO SSA1                                   
334200     MOVE '  GE' TO GODK-STATUSKODER                                      
334300     CALL CBLTDLI USING GHU FSGA1-PCB DLI-IO-AREA-2 SSA1                  
334400     MOVE FSGA1-STATUS-CODE TO STATUS-WS                                  
334500     PERFORM IMS-STATUSKONTROLL                                           
334600     .                                                                    
334700     SKIP3                                                                
334800 IMS-GU-FSGA01-SPAR SECTION.                                              
334900     SKIP1                                                                
335000     STRING 'WLFSGA01(WDM301KY =' W-WDM301KY-SPAR ')'                     
335100            DELIMITED BY SIZE INTO SSA1                                   
335200     MOVE '  GE' TO GODK-STATUSKODER                                      
335300     CALL CBLTDLI USING GU FSGA1-PCB DLI-IO-AREA-2 SSA1                   
335400     MOVE FSGA1-STATUS-CODE TO STATUS-WS                                  
335500     PERFORM IMS-STATUSKONTROLL                                           
335600     .                                                                    
335700     SKIP3                                                                
335800 IMS-GNP-FSGA12-SPAR SECTION.                                             
335900     SKIP1                                                                
336000     STRING 'WLFSGA01(WDM301KY =' W-WDM301KY-SPAR ')'                     
336100            DELIMITED BY SIZE INTO SSA1                                   
336200     STRING 'WLFSGA12(WDM312KY>=' W-IDFKNGRP-SPAR-X ')'                   
336300            DELIMITED BY SIZE INTO SSA2                                   
336400     MOVE '  GE' TO GODK-STATUSKODER                                      
336500     CALL CBLTDLI USING GNP FSGA1-PCB DLI-IO-AREA-2 SSA1 SSA2             
336600     MOVE FSGA1-STATUS-CODE TO STATUS-WS                                  
336700     PERFORM IMS-STATUSKONTROLL                                           
336800     .                                                                    
336900     SKIP3                                                                
337000 IMS-DLET-FSGA-SPAR   SECTION.                                            
337100     MOVE '  ' TO GODK-STATUSKODER                                        
337200     CALL CBLTDLI USING DLET FSGA1-PCB DLI-IO-AREA-2                      
337300     MOVE FSGA1-STATUS-CODE TO STATUS-WS                                  
337400     PERFORM IMS-STATUSKONTROLL                                           
337500     .                                                                    
337600     SKIP3                                                                
337700 IMS-DLET-FSGA   SECTION.                                                 
337800     MOVE '  ' TO GODK-STATUSKODER                                        
337900     CALL CBLTDLI USING DLET FSGA-PCB DLI-IO-AREA                         
338000     MOVE FSGA-STATUS-CODE TO STATUS-WS                                   
338100     PERFORM IMS-STATUSKONTROLL                                           
338200     .                                                                    
338300     SKIP3                                                                
338400 IMS-STATUSKONTROLL SECTION.                                              
338500     SKIP1                                                                
338600     SET STATUS-IX TO 1                                                   
338700     SEARCH GODK-STATUS AT END CALL FELLOG                                
338800       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS NEXT SENTENCE             
338900     END-SEARCH                                                           
339000     .                                                                    
339100     EJECT                                                                
339200*    -COPY WY2000P3                                                       
