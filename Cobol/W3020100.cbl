000100     SKIP3                                                                
000200 ID DIVISION.                                                             
000300     SKIP2                                                                
000400 PROGRAM-ID.     W3020100.                                                
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
001600*        TRANSAKTION: W3T201                                              
001700*        MID:         W3I20101                                            
001800*                                                                         
001900*    UTDATA.                                                              
002000*        MOD:         W3O20101                                            
002100     SKIP3                                                                
002200 ENVIRONMENT DIVISION.                                                    
002300     SKIP2                                                                
002400 DATA DIVISION.                                                           
002500     EJECT                                                                
002600 WORKING-STORAGE SECTION.                                                 
002601                                                                          
002610*    -- CHECKED BY WY2000                                                 
002700 77  PROGRAM-NAMN                PIC X(8) VALUE 'W3020100'.               
002800 77  JA                          PIC X       VALUE 'J'.                   
002900 77  NEJ                         PIC X       VALUE 'N'.                   
003000 77  SPRAK-IX                    PIC S9(9)   VALUE +0   COMP SYNC.        
003100 77  RAD-INDX                    PIC S9(9)   VALUE +0   COMP SYNC.        
003200 77  MAX-RAD                     PIC S9(9)   VALUE +28  COMP SYNC.        
003300 77  MAX-MOD-LAENGD              PIC S9(4)  VALUE +1185 COMP SYNC.        
003400 77  WS-IDFSGURV                 PIC  X(8)   VALUE SPACE.                 
003500 01  WS-TIREGTID.                                                         
003600   03  WS-TIREGTID-HHMMSS        PIC 9(6)    VALUE ZERO.                  
003700   03  FILLER                    PIC 9(2)    VALUE ZERO.                  
003800 01  WS-IDTRANS                  PIC X(4).                                
003900    88  EGEN-BILD                            VALUE '3201'.                
004000    88  3201-BILD                            VALUE '3201'.                
004100    88  GODKAEND-BILD                        VALUE '3201'                 
004200                                                   '3202'                 
004300                                                   '3204'                 
004400                                                   '3203'.                
004500 01  SW-IFYLLT                   PIC X(1).                                
004600    88  INGET-IFYLLT                         VALUE 'N'.                   
004700     EJECT                                                                
004800 01  DATUMKORT.                                                           
004900   03  FILLER                    PIC X(16)   VALUE                        
005000                                            'DATUMKORT       '.           
005100   03  WS-AAVV.                                                           
005200     05  WS-AA                   PIC 9(2)    VALUE ZERO.                  
005300     05  WS-VV                   PIC 9(2)    VALUE ZERO.                  
005400   03  WS-DAGENS-AAVV.                                                    
005500     05  WS-DAGENS-AA            PIC 9(2)    VALUE ZERO.                  
005600     05  WS-DAGENS-VV            PIC 9(2)    VALUE ZERO.                  
005700   03  WS-DAGENS-AAVV-MINUS-2-AA.                                         
005800     05  WS-DAGENS-AA-MINUS-2-AA PIC 9(2)    VALUE ZERO.                  
005900     05  WS-DAGENS-VV-MINUS-2-AA PIC 9(2)    VALUE ZERO.                  
006000*03 -COPY WDATAREA                                                        
006200     EJECT                                                                
006300 01  DYNAMISKA-SUBPROGRAM.                                                
006400   03  WDATKONV                  PIC X(8) VALUE 'WDATKONV'.               
006500     EJECT                                                                
006600 01  NYCKLAR-TILL-DLI.                                                    
006700   03  FILLER                    PIC X(16)   VALUE                        
006800                                            'NYCKLAR-TILL-DLI'.           
006900   03  W-WDM3A1KY-MIN.                                                    
007000     05  W-IDUSER-MIN            PIC  X(8)   VALUE SPACE.                 
007100     05  W-IDFSGURV-MIN          PIC  X(8)   VALUE SPACE.                 
007200     05  W-IDTRANS-MIN           PIC  X(4)   VALUE SPACE.                 
007300     05  W-DAREGDAT-MIN          PIC  9(8)   VALUE ZERO.                  
007400     05  W-TIREGTID-MIN          PIC S9(7)   VALUE ZERO COMP-3.           
007500   03  W-WDM3A1KY-MAX.                                                    
007600     05  W-IDUSER-MAX            PIC  X(8)   VALUE SPACE.                 
007700     05  W-IDFSGURV-MAX          PIC  X(8)   VALUE HIGH-VALUE.            
007800     05  W-IDTRANS-MAX           PIC  X(4)   VALUE '3204'.                
007900     05  FILLER                  PIC  X(12)   VALUE HIGH-VALUE.           
008000     EJECT                                                                
008100 01  DYNAMISKA-SUBPROGRAM.                                                
008200   03  FELLOG                    PIC X(8)    VALUE 'FELLOG  '.            
008300   03  CBLTDLI                   PIC X(8)    VALUE 'CBLTDLI '.            
008400   03  WMEDKONV                  PIC X(8)    VALUE 'WMEDKONV'.            
008500* COPYTEXT FÖR MDDELANDE-MODUL                                            
008600 01    FILLER                    PIC X(8)    VALUE 'WMEDAREA'.            
008700*01   -COPY WMEDAREA.                                                     
008900 01  MEDDELANDE.                                                          
009000   03  FILLER                    PIC X(16)   VALUE 'MEDDELANDE'.          
009100   03  FEL1.                                                              
009200     05 FILLER                   PIC X(40)                                
009300          VALUE 'UPPLYSTA FÄLT FEL'.                                      
009400     05 FILLER                   PIC X(40)                                
009500          VALUE 'HIGHLIGHTED FIELDS ARE WRONG'.                           
009600   03  FILLER REDEFINES FEL1.                                             
009700     05  FEL-1                   PIC X(40)   OCCURS 2.                    
009800                                                                          
009900   03  FEL2.                                                              
010000     05 FILLER                   PIC X(40)                                
010100          VALUE 'KONFLIKT                                '.               
010200     05 FILLER                   PIC X(40)                                
010300          VALUE 'CONFLICT                                '.               
010400   03  FILLER REDEFINES FEL2.                                             
010500     05  FEL-2                   PIC X(40)   OCCURS 2.                    
010600                                                                          
010700   03  FEL3.                                                              
010800     05 FILLER                   PIC X(40)                                
010900          VALUE 'TRYCK PF11 VID UPPDATERING'.                             
011000     05 FILLER                   PIC X(40)                                
011100          VALUE 'PRESS PF11 WHEN UPDATE'.                                 
011200   03  FILLER REDEFINES FEL3.                                             
011300     05  FEL-3                   PIC X(40)   OCCURS 2.                    
011400                                                                          
011500   03  FEL4.                                                              
011600     05 FILLER                   PIC X(40)                                
011700          VALUE 'ANGE ETT AV NEDANSTÅENDE URVAL'.                         
011800     05 FILLER                   PIC X(40)                                
011900          VALUE 'SPECIFY ONE CHOISE (SEE BELOW)'.                         
012000   03  FILLER REDEFINES FEL4.                                             
012100     05  FEL-4                   PIC X(40)   OCCURS 2.                    
012200                                                                          
012300   03  FEL5.                                                              
012400     05 FILLER                   PIC X(40)                                
012500          VALUE 'URVAL SAKNAS          '.                                 
012600     05 FILLER                   PIC X(40)                                
012700          VALUE 'KEYS ARE MISSING  '.                                     
012800   03  FILLER REDEFINES FEL5.                                             
012900     05  FEL-5                   PIC X(40)   OCCURS 2.                    
013000    SKIP1                                                                 
013100                                                                          
013200   03  FEL6.                                                              
013300     05 FILLER                   PIC X(40)                                
013400          VALUE 'DETTA ÄR FÖRSTA SIDAN '.                                 
013500     05 FILLER                   PIC X(40)                                
013600          VALUE 'THIS IS THE FIRST PAGE'.                                 
013700   03  FILLER REDEFINES FEL6.                                             
013800     05  FEL-6                   PIC X(40)   OCCURS 2.                    
013900    SKIP1                                                                 
014000                                                                          
014100   03  MED1.                                                              
014200     05 FILLER                   PIC X(40)                                
014300          VALUE 'TRYCK PF 9 VID SELEKTERING AV BILD'.                     
014400     05 FILLER                   PIC X(40)                                
014500          VALUE 'PRESS PF9         '.                                     
014600   03  FILLER REDEFINES MED1.                                             
014700     05  MED-1                   PIC X(40)   OCCURS 2.                    
014800    SKIP1                                                                 
014900   03  MED2.                                                              
015000     05 FILLER                   PIC X(60)                                
015100         VALUE 'KONCERN             DISTRIKT             MARKNAD'.        
015200     05 FILLER                   PIC X(60)                                
015300          VALUE '                                        '.               
015400   03  FILLER REDEFINES MED2.                                             
015500     05  MED-2                   PIC X(60)   OCCURS 2.                    
015600    SKIP1                                                                 
015700   03  MED3.                                                              
015800     05 FILLER                   PIC X(60)                                
015900         VALUE 'TILL 3201-BILDEN                                '.        
016000     05 FILLER                   PIC X(60)                                
016100          VALUE '                                        '.               
016200   03  FILLER REDEFINES MED3.                                             
016300     05  MED-3                   PIC X(60)   OCCURS 2.                    
016400    SKIP1                                                                 
016500   03  MED4.                                                              
016600     05 FILLER                   PIC X(60)                                
016700         VALUE 'FLER SIDOR FINNS                                '.        
016800     05 FILLER                   PIC X(60)                                
016900          VALUE 'THERE ARE MORE SIDES                    '.               
017000   03  FILLER REDEFINES MED4.                                             
017100     05  MED-4                   PIC X(60)   OCCURS 2.                    
017200                                                                          
017300   03  MED5.                                                              
017400     05 FILLER                   PIC X(60)                                
017500         VALUE 'MINST ETT ARTIKELNUMMER MÅSTE FINNAS KVAR       '.        
017600     05 FILLER                   PIC X(60)                                
017700          VALUE 'YOU ARE TRYING TO DELETE TOO MANY PARTS '.               
017800   03  FILLER REDEFINES MED5.                                             
017900     05  MED-5                   PIC X(60)   OCCURS 2.                    
017901   03  MED6.                                                              
017902     05 FILLER                   PIC X(60)                                
017903         VALUE 'FÖRBJUDEN ÅTGÄRD                                '.        
017904     05 FILLER                   PIC X(60)                                
017905          VALUE 'FORBIDDEN ACTION                        '.               
017910   03  FILLER REDEFINES MED6.                                             
017920     05  MED-6                   PIC X(40)   OCCURS 2.                    
018000                                                                          
018100     EJECT                                                                
018200******************************************************************        
018300*                                                                         
018400*                AREOR FÖR MFS OCH SKÄRMHANTERING                         
018500*                                                                         
018600     SKIP3                                                                
018700 01  FILLER                      PIC X(16)   VALUE 'MID-COPY-WS'.         
018800*01  MID -COPY W3I20101                                                   
019000     EJECT                                                                
019100 01  FILLER                      PIC X(16)   VALUE 'MSG-COPY-WS'.         
019200*01  -COPY WMSGAREA                                                       
019400     EJECT                                                                
019500*  03  MOD -COPY W3O20101           -RED MSG-AREA.                        
019700     EJECT                                                                
019800 01  FILLER                      PIC X(16)   VALUE 'ALT-AREA'.            
019900 01  W-PROG-TO-PROG-SW.                                                   
020000   03  M-SW-LL                   PIC S9(4)  VALUE +404 COMP SYNC.         
020100   03  M-SW-Z1-Z2                PIC  X(2)   VALUE LOW-VALUE.             
020200   03  M-SW-KDTRANS              PIC  X(8)   VALUE 'W3T202  '.            
020300   03  M-SW-IDTRANS              PIC  X(4)   VALUE '3202'.                
020400   03  M-SW-KDMFSTYP             PIC  X(1)   VALUE '1'.                   
020500*03  -COPY W3I20201           -PRE 3202-                                  
020700    EJECT                                                                 
020800 01  FILLER                      PIC X(16)   VALUE 'ALT-AREA1'.           
020900 01  W1-PROG-TO-PROG-SW.                                                  
021000   03  M1-SW-LL                  PIC S9(4)  VALUE +517 COMP SYNC.         
021100   03  M1-SW-Z1-Z2               PIC  X(2)   VALUE LOW-VALUE.             
021200   03  M1-SW-KDTRANS             PIC  X(8)   VALUE 'W3T203  '.            
021300   03  M1-SW-IDTRANS             PIC  X(4)   VALUE '3203'.                
021400   03  M1-SW-KDMFSTYP            PIC  X(1)   VALUE '1'.                   
021500*  03  -COPY W3I20301           -PRE 3203-                                
021700    EJECT                                                                 
021800 01  FILLER                      PIC X(16)   VALUE 'MFS-COPY-WS'.         
021900*01  -COPY WMFSAREA                                                       
022100     EJECT                                                                
022200*01  WLFSGB01    -COPY WDM3A1 -PRE WS-.                                   
022400     EJECT                                                                
022500******************************************************************        
022600*                                                                         
022700*        ARBETS-AREOR TILL IMS-SEKTIONERNA                                
022800*                                                                         
022900 01  IMS-WS.                                                              
023000   03  FILLER                    PIC X(16)   VALUE 'IMS-WS     '.         
023100     SKIP3                                                                
023200*                        **** STATUS-KOD FRÅN IMS                         
023300   03  STATUS-WS                 PIC XX.                                  
023400     88  SEGMENT-FINNS                       VALUE '  '.                  
023500     88  SEGMENT-SAKNAS                      VALUE 'GE' 'GB'.             
023600     SKIP3                                                                
023700   03  GODK-STATUSKODER.                                                  
023800     05  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
023900     SKIP3                                                                
024000 01    SSA1                      PIC X(128).                              
024100 01    SSA2                      PIC X(64).                               
024200     EJECT                                                                
024300*                            IMS FUNKTIONSKODER                           
024400*01    -COPY W0003                                                        
024600     EJECT                                                                
024700************************     DLI INPUT-OUTPUT AREA ***************        
024800 01  FILLER                    PIC X(16)   VALUE 'DLI-IO-AREA'.           
024900 01  DLI-IO-AREA.                                                         
025000   03  IO-AREA                   PIC X(100)  VALUE SPACE.                 
025100     SKIP3                                                                
025200*03  WLFSGB01    -COPY WDM3A1   -RED IO-AREA.                             
025400     EJECT                                                                
025500 LINKAGE SECTION.                                                         
025600*01  -COPY W0009     -PRE MSG-                                            
025800     SKIP2                                                                
025900*01  -COPY W0009     -PRE ALT-                                            
026100     SKIP2                                                                
026200*01  -COPY W0009     -PRE ALT1-                                           
026400     SKIP2                                                                
026500*01  -COPY W0008     -PRE FSGB-                                           
026700     05  FILLER                  PIC X.                                   
026800     EJECT                                                                
026900 PROCEDURE DIVISION USING MSG-PCB ALT-PCB ALT1-PCB FSGB-PCB.              
027000     ENTRY 'DLITCBL' USING MSG-PCB ALT-PCB ALT1-PCB FSGB-PCB.             
027100                                                                          
027200     PERFORM IMS-GET-MSG                                                  
027300     IF SEGMENT-FINNS                                                     
027400        PERFORM A-INIT                                                    
027500        IF GODKAEND-BILD                                                  
027600           CONTINUE                                                       
027700        ELSE                                                              
027800           MOVE SPACE                     TO MID-IDFSGURV-IN              
027900                                             MID-IDUSER-IN                
028000        END-IF                                                            
028100        PERFORM B-GOR-IORDN-NYCK-PFTRY                                    
028200        IF MFS-IDPFK = '7'                                                
028300           PERFORM S07-VISA-SIDAN                                         
028400        ELSE                                                              
028500           PERFORM C-KOLLA-INDATA                                         
028600        END-IF                                                            
028700     END-IF                                                               
028800     MOVE ZERO                                  TO RETURN-CODE            
028900     GOBACK                                                               
029000     .                                                                    
029100     EJECT                                                                
029200 A-INIT SECTION.                                                          
029300     SKIP2                                                                
029400     IF MSG-DUBBLA-TRANSKODER                                             
029500        MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W3I20101                
029600        MOVE MSG-IDTRANS-2                 TO MFS-IDTRANS                 
029700        MOVE MSG-KDMFSFOR-2                TO MFS-KDMFSFOR                
029800        MOVE MSG-KDTRTYP                   TO MFS-KDTRTYP                 
029900        MOVE MSG-IDPFK                     TO MFS-IDPFK                   
030000     ELSE                                                                 
030100        MOVE MSG-INDATA-MINUS-1-TRANSKOD   TO MID-W3I20101                
030200        MOVE MSG-IDTRANS-1                 TO MFS-IDTRANS                 
030300        MOVE MSG-KDMFSFOR-1                TO MFS-KDMFSFOR                
030400        MOVE SPACE                         TO MFS-KDTRTYP                 
030500                                              MFS-IDPFK                   
030600     END-IF                                                               
030700                                                                          
030800     MOVE LOW-VALUE                        TO MSG-AREA                    
030900     MOVE 'W3O201N1'                       TO MFS-IDMOD                   
031000     MOVE '3201'                           TO MOD-IDTRANS                 
031100     MOVE MFS-IDTRANS                      TO WS-IDTRANS                  
031200     MOVE MFS-RENSA-FAELT                  TO MOD-TEMFSFEL                
031300                                              MOD-TEMFSINF                
031400                                              MOD-IDFSGURV-IN             
031500                                              MOD-IDUSER-IN               
031600*                                                                         
031700     IF EGEN-BILD                                                         
031800        CONTINUE                                                          
031900     ELSE                                                                 
032000        MOVE SPACE                         TO MFS-KDTRTYP                 
032100        MOVE '7'                           TO MFS-IDPFK                   
032200     END-IF                                                               
032300     IF ENGLISH-TEXT                                                      
032400        MOVE +2                            TO SPRAK-IX                    
032500        MOVE 'GB '                         TO MED-IDSKYLT                 
032600     ELSE                                                                 
032700        MOVE +1                            TO SPRAK-IX                    
032800        MOVE 'S  '                         TO MED-IDSKYLT                 
032900     END-IF                                                               
033000     .                                                                    
033100     EJECT                                                                
033200 B-GOR-IORDN-NYCK-PFTRY SECTION.                                          
033300     SKIP3                                                                
033400     IF MID-IDFSGURV-IN = ALL '+'                                         
033500        MOVE MID-IDFSGURV-UT         TO WS-IDFSGURV                       
033600     ELSE                                                                 
033700        MOVE MID-IDFSGURV-IN         TO WS-IDFSGURV                       
033800        MOVE SPACE                   TO MFS-KDTRTYP                       
033900        MOVE '7'                     TO MFS-IDPFK                         
034000     END-IF                                                               
034100     MOVE WS-IDFSGURV                TO MOD-IDFSGURV-UT                   
034200                                        W-IDFSGURV-MIN                    
034300     IF MID-IDUSER-IN = ALL '+'                                           
034400        MOVE MID-IDUSER-UT           TO W-IDUSER-MIN                      
034500     ELSE                                                                 
034600        MOVE MID-IDUSER-IN           TO W-IDUSER-MIN                      
034700        MOVE '7'                     TO MFS-IDPFK                         
034800     END-IF                                                               
034900     IF W-IDUSER-MIN = SPACE                                              
035000        MOVE MSG-SIGNON-USERID       TO W-IDUSER-MIN                      
035100     END-IF                                                               
035200     MOVE W-IDUSER-MIN               TO MOD-IDUSER-UT                     
035300                                        W-IDUSER-MAX                      
035400     PERFORM BA-KOLLA-PF-TRYCK                                            
035500     .                                                                    
035600     EJECT                                                                
035700 BA-KOLLA-PF-TRYCK  SECTION.                                              
035800     SKIP2                                                                
035900     IF  MID-DAREGDAT-LO NUMERIC                                          
036000     AND MID-TIREGTID-LO NUMERIC                                          
036100     AND MID-DAREGDAT-HI NUMERIC                                          
036200     AND MID-TIREGTID-HI NUMERIC                                          
036300        CONTINUE                                                          
036400     ELSE                                                                 
036500        MOVE '7'                       TO MFS-IDPFK                       
036600        MOVE SPACE                     TO MFS-KDTRTYP                     
036700     END-IF                                                               
036800     IF MFS-IDPFK = '8'                                                   
036900     AND MID-DAREGDAT-HI  > ZERO                                          
037000     AND MID-TIREGTID-HI  > ZERO                                          
037100        MOVE MID-IDTRANS-HI            TO W-IDTRANS-MIN                   
037200        MOVE MID-DAREGDAT-HI           TO W-DAREGDAT-MIN                  
037201*---Y2K-FIX************                                                   
037210        IF MID-DAREGDAT-HI NOT = ZERO                                     
037220           IF MID-DAREGDAT-HI < 1000000                                   
037221*            MOVE 20         TO W-DAREGDAT-MIN(1:2)                       
037222*          ELSE                                                           
037223*            IF MID-DAREGDAT-HI < 999999                                  
037224               MOVE 19         TO W-DAREGDAT-MIN(1:2)                     
037225*            ELSE                                                         
037226*              MOVE 99999999   TO W-DAREGDAT-MIN                          
037227*            END-IF                                                       
037228           END-IF                                                         
037229        END-IF                                                            
037300        MOVE MID-TIREGTID-HI           TO W-TIREGTID-MIN                  
037400        MOVE MID-IDFSGURV-HI           TO W-IDFSGURV-MIN                  
037500     ELSE                                                                 
037600        IF MFS-IDPFK = '7'                                                
037700*          MOVE FEL-6 (SPRAK-IX)       TO MOD-TEMFSFEL                    
037800           MOVE '006'                  TO MED-IDMFSFEL                    
037900           CALL WMEDKONV               USING MED-WMEDAREA                 
038000           MOVE MED-MFSFEL             TO MOD-TEMFSFEL                    
038100           MOVE LOW-VALUE              TO W-IDTRANS-MIN                   
038200           MOVE ZERO                   TO W-DAREGDAT-MIN                  
038300                                          W-TIREGTID-MIN                  
038400        ELSE                                                              
038410           IF MFS-IDPFK = '9'                                             
038411              MOVE MED-6 (SPRAK-IX)     TO MOD-TEMFSFEL                   
038420           ELSE                                                           
038500              MOVE MID-IDTRANS-LO       TO W-IDTRANS-MIN                  
038600              MOVE MID-DAREGDAT-LO      TO W-DAREGDAT-MIN                 
038610              IF MID-DAREGDAT-LO NOT = ZERO                               
038620                IF MID-DAREGDAT-LO < 1000000                              
038630*                 MOVE 20               TO W-DAREGDAT-MIN (1:2)           
038640*               ELSE                                                      
038650*                 IF MID-DAREGDAT-LO < 999999                             
038660                    MOVE 19             TO W-DAREGDAT-MIN (1:2)           
038670*                 ELSE                                                    
038680*                   MOVE 99999999       TO W-DAREGDAT-MIN                 
038690*                 END-IF                                                  
038691                END-IF                                                    
038692              END-IF                                                      
038700              MOVE MID-TIREGTID-LO      TO W-TIREGTID-MIN                 
038800              MOVE MID-IDFSGURV-LO      TO W-IDFSGURV-MIN                 
038810           END-IF                                                         
038900        END-IF                                                            
039000     END-IF                                                               
039100     .                                                                    
039200     EJECT                                                                
039300 C-KOLLA-INDATA SECTION.                                                  
039400     SKIP2                                                                
039500     MOVE +1                             TO RAD-INDX                      
039600     MOVE NEJ                            TO SW-IFYLLT                     
039700     PERFORM UNTIL RAD-INDX > MAX-RAD                                     
039800        IF MID-SELECT-URVAL (RAD-INDX) = ALL '+'                          
039900           CONTINUE                                                       
040000        ELSE                                                              
040100           MOVE JA                       TO SW-IFYLLT                     
040200           IF MFS-IDPFK = '9'                                             
040300              PERFORM CA-BEHANDL-MID-OCH-SKICKA                           
040400           ELSE                                                           
040500              MOVE MED-1 (SPRAK-IX)      TO MOD-TEMFSFEL                  
040600*             MOVE '001'                 TO MED-IDMFSFEL                  
040700*             CALL WMEDKONV              USING MED-WMEDAREA               
040800*             MOVE MED-MFSFEL            TO MOD-TEMFSFEL                  
040900              PERFORM S08-ROER-EJ-FAELT                                   
041000           END-IF                                                         
041100           ADD MAX-RAD                   TO RAD-INDX                      
041200        END-IF                                                            
041300        ADD +1                           TO RAD-INDX                      
041400     END-PERFORM                                                          
041500     IF SW-IFYLLT = NEJ                                                   
041600        PERFORM S07-VISA-SIDAN                                            
041700     END-IF                                                               
041800     .                                                                    
041900     EJECT                                                                
042000 CA-BEHANDL-MID-OCH-SKICKA  SECTION.                                      
042100     SKIP2                                                                
042200     IF MID-IDTRANS-VISA (RAD-INDX) = '3202'                              
042300        MOVE LOW-VALUE                  TO 3202-MID-W3I20201              
042400        MOVE MID-DAREGDAT-VISA (RAD-INDX) TO                              
042500                           3202-MID-DAREGDAT-DOLD                         
042600        MOVE MID-TIREGTID-VISA (RAD-INDX) TO                              
042700                           3202-MID-TIREGTID-DOLD                         
042800        MOVE MID-IDFSGURV-VISA (RAD-INDX) TO                              
042900                           3202-MID-IDFSGURV-UT                           
043000        MOVE MID-IDUSER-UT                TO                              
043100                           3202-MID-IDUSER-UT                             
043200        MOVE '++++++++'                   TO                              
043300                           3202-MID-IDUSER-IN                             
043400                           3202-MID-IDFSGURV-IN                           
043500        MOVE ZERO                         TO                              
043600                           3202-MID-IDFKNGRP-FOM-LO                       
043700                           3202-MID-IDFKNGRP-TOM-LO                       
043800        MOVE +99999                       TO                              
043900                           3202-MID-IDFKNGRP-FOM-HI                       
044000                           3202-MID-IDFKNGRP-TOM-HI                       
044100        PERFORM IMS-ISRT-ALT-MSG                                          
044200     ELSE                                                                 
044300        IF MID-IDTRANS-VISA (RAD-INDX) = '3203'                           
044400           MOVE LOW-VALUE               TO 3203-MID-W3I20301              
044500           MOVE MID-DAREGDAT-VISA (RAD-INDX) TO                           
044600                              3203-MID-DAREGDAT-DOLD                      
044700           MOVE MID-TIREGTID-VISA (RAD-INDX) TO                           
044800                              3203-MID-TIREGTID-DOLD                      
044900           MOVE MID-IDFSGURV-VISA (RAD-INDX) TO                           
045000                              3203-MID-IDFSGURV-UT                        
045100           MOVE MID-IDUSER-UT                TO                           
045200                              3203-MID-IDUSER-UT                          
045300           MOVE '++++++++'                   TO                           
045400                              3203-MID-IDUSER-IN                          
045500                              3203-MID-IDFSGURV-IN                        
045600           MOVE 999999999                    TO                           
045700                              3203-MID-IDARTNR-LO                         
045800           MOVE ZERO                         TO                           
045900                              3203-MID-IDARTNR-HI                         
046000           PERFORM IMS-ISRT-ALT1-MSG                                      
046100        ELSE                                                              
046200           PERFORM S07-VISA-SIDAN                                         
046300        END-IF                                                            
046400     END-IF                                                               
046500     .                                                                    
046600     EJECT                                                                
046700 S07-VISA-SIDAN SECTION.                                                  
046800     SKIP1                                                                
046900     MOVE +1                             TO RAD-INDX                      
047000     PERFORM IMS-GN-FSGB01                                                
047100     IF SEGMENT-FINNS                                                     
047200        MOVE SEQA-IDTRANS                TO MOD-IDTRANS-LO                
047300        MOVE SEQA-DAREGDAT               TO MOD-DAREGDAT-LO               
047400        MOVE SEQA-TIREGTID               TO MOD-TIREGTID-LO               
047500        MOVE SEQA-IDFSGURV               TO MOD-IDFSGURV-LO               
047600     ELSE                                                                 
047700*       MOVE FEL-5 (SPRAK-IX)            TO MOD-TEMFSFEL                  
047800        MOVE '005'                       TO MED-IDMFSFEL                  
047900        CALL WMEDKONV                    USING MED-WMEDAREA               
048000        MOVE MED-MFSFEL                  TO MOD-TEMFSFEL                  
048100        MOVE SPACE                       TO MOD-IDTRANS-LO                
048200        MOVE ZERO                        TO MOD-DAREGDAT-LO               
048300                                            MOD-TIREGTID-LO               
048400     END-IF                                                               
048500     PERFORM UNTIL RAD-INDX > MAX-RAD                                     
048600        IF SEGMENT-FINNS                                                  
048700           MOVE SEQA-IDFSGURV      TO MOD-IDFSGURV-VISA (RAD-INDX)        
048800           MOVE SEQA-DAREGDAT      TO MOD-DAREGDAT-VISA (RAD-INDX)        
048900           MOVE SEQA-TIREGTID      TO MOD-TIREGTID-VISA (RAD-INDX)        
049000           MOVE SEQA-IDTRANS       TO MOD-IDTRANS-VISA  (RAD-INDX)        
049100           MOVE MFS-RENSA-FAELT    TO MOD-SELECT-URVAL  (RAD-INDX)        
049200           PERFORM IMS-GN-FSGB01                                          
049300        ELSE                                                              
049400           MOVE MFS-RENSA-FAELT  TO MOD-SELECT-URVAL  (RAD-INDX)          
049500                                    MOD-IDFSGURV-VISA (RAD-INDX)          
049600                                    MOD-DAREGDAT-VISA (RAD-INDX)          
049700                                    MOD-TIREGTID-VISA (RAD-INDX)          
049800                                    MOD-IDTRANS-VISA  (RAD-INDX)          
049900        END-IF                                                            
050000        ADD +1                           TO RAD-INDX                      
050100     END-PERFORM                                                          
050200     IF SEGMENT-FINNS                                                     
050300*       MOVE MED-4 (SPRAK-IX)            TO MOD-TEMFSINF                  
050400        MOVE '105'                       TO MED-IDMFSINF                  
050500        CALL WMEDKONV                    USING MED-WMEDAREA               
050600        MOVE MED-MFSINF                  TO MOD-TEMFSINF                  
050700        MOVE SEQA-IDTRANS                TO MOD-IDTRANS-HI                
050800        MOVE SEQA-DAREGDAT               TO MOD-DAREGDAT-HI               
050900        MOVE SEQA-TIREGTID               TO MOD-TIREGTID-HI               
051000        MOVE SEQA-IDFSGURV               TO MOD-IDFSGURV-HI               
051100     ELSE                                                                 
051200        MOVE ZERO                        TO MOD-DAREGDAT-HI               
051300                                            MOD-TIREGTID-HI               
051400     END-IF                                                               
051500     MOVE MAX-MOD-LAENGD TO MSG-KVLL                                      
051600     PERFORM IMS-INSERT-MSG                                               
051700     .                                                                    
051800     EJECT                                                                
051900 S08-ROER-EJ-FAELT  SECTION.                                              
052000     SKIP1                                                                
052100     MOVE +1                             TO RAD-INDX                      
052200     PERFORM UNTIL RAD-INDX > MAX-RAD                                     
052300           MOVE MFS-ADD-LAES-IN-FAELT    TO                               
052400                               MOD-SELECT-URVAL-ATTR  (RAD-INDX)          
052500                               MOD-IDFSGURV-VISA-ATTR (RAD-INDX)          
052600                               MOD-DAREGDAT-VISA-ATTR (RAD-INDX)          
052700                               MOD-TIREGTID-VISA-ATTR (RAD-INDX)          
052800                               MOD-IDTRANS-VISA-ATTR  (RAD-INDX)          
052900           MOVE MFS-ROER-EJ-FAELT TO MOD-IDFSGURV-VISA(RAD-INDX)          
053000                                    MOD-DAREGDAT-VISA (RAD-INDX)          
053100                                    MOD-TIREGTID-VISA (RAD-INDX)          
053200                                    MOD-IDTRANS-VISA  (RAD-INDX)          
053300                                    MOD-SELECT-URVAL  (RAD-INDX)          
053400        ADD +1                           TO RAD-INDX                      
053500     END-PERFORM                                                          
053600     MOVE MFS-ROER-EJ-FAELT          TO  MOD-IDTRANS-HI                   
053700                                         MOD-DAREGDAT-HI                  
053800                                         MOD-TIREGTID-HI                  
053900                                         MOD-IDFSGURV-HI                  
054000                                                                          
054100                                         MOD-IDTRANS-LO                   
054200                                         MOD-DAREGDAT-LO                  
054300                                         MOD-TIREGTID-LO                  
054400                                         MOD-IDFSGURV-LO                  
054500     MOVE MAX-MOD-LAENGD TO MSG-KVLL                                      
054600     PERFORM IMS-INSERT-MSG                                               
054700     .                                                                    
054800     EJECT                                                                
054900 IMS-GET-MSG SECTION.                                                     
055000     SKIP1                                                                
055100     MOVE '  QC' TO GODK-STATUSKODER                                      
055200     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
055300     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
055400     PERFORM IMS-STATUSKONTROLL                                           
055500     .                                                                    
055600     SKIP3                                                                
055700 IMS-INSERT-MSG SECTION.                                                  
055800     SKIP1                                                                
055900     IF NOT ENGLISH-TEXT                                                  
056000       MOVE '0' TO MFS-KDHUVOMR                                           
056100     END-IF                                                               
056200     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
056300     MOVE SPACE TO GODK-STATUSKODER                                       
056400     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
056500     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
056600     PERFORM IMS-STATUSKONTROLL                                           
056700     .                                                                    
056800     SKIP3                                                                
056900 IMS-ISRT-ALT-MSG SECTION.                                                
057000     MOVE SPACE  TO GODK-STATUSKODER                                      
057100     CALL CBLTDLI USING ISRT ALT-PCB W-PROG-TO-PROG-SW                    
057200     MOVE ALT-STATUS-CODE TO STATUS-WS                                    
057300     PERFORM IMS-STATUSKONTROLL                                           
057400     .                                                                    
057500     SKIP3                                                                
057600 IMS-ISRT-ALT1-MSG SECTION.                                               
057700     MOVE SPACE  TO GODK-STATUSKODER                                      
057800     CALL CBLTDLI USING ISRT ALT1-PCB W1-PROG-TO-PROG-SW                  
057900     MOVE ALT1-STATUS-CODE TO STATUS-WS                                   
058000     PERFORM IMS-STATUSKONTROLL                                           
058100     .                                                                    
058200     SKIP3                                                                
058300 IMS-GN-FSGB01 SECTION.                                                   
058400     SKIP1                                                                
058500     STRING 'WLFSGB01(WDM3A1KY>=' W-WDM3A1KY-MIN                          
058600                    '&WDM3A1KY< ' W-WDM3A1KY-MAX ')'                      
058700            DELIMITED BY SIZE INTO SSA1                                   
058800     MOVE '  GE' TO GODK-STATUSKODER                                      
058900     CALL CBLTDLI USING GN FSGB-PCB DLI-IO-AREA SSA1                      
059000     MOVE FSGB-STATUS-CODE TO STATUS-WS                                   
059100     PERFORM IMS-STATUSKONTROLL                                           
059200     .                                                                    
059300     SKIP3                                                                
059400 IMS-STATUSKONTROLL SECTION.                                              
059500     SKIP1                                                                
059600     SET STATUS-IX TO 1                                                   
059700     SEARCH GODK-STATUS AT END CALL FELLOG                                
059800       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS NEXT SENTENCE             
059900     END-SEARCH                                                           
060000     .                                                                    
