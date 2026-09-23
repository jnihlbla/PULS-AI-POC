000100 PROCESS DYNAM                                                            
000200 ID DIVISION.                                                             
000300 PROGRAM-ID.     W9042800.                                                
000400 AUTHOR.         PETER D.                                                 
000500 DATE-WRITTEN.   MAR   89.                                                
000600                                                                          
000700*                                                                         
000800*    FUNKTION                                                             
000900*    FRÅGEPROGRAM DB2 OCH DL1                                             
001000*    INMATNINGSFÄLT ÄR BYTESNR (ARTNR)                                    
001100*    PROGRAMMET LÄSER  WLLEVA, WLBENA OCH WLARTC-BASEN                    
001200*    SAMT TABELLERNA BYART BYPRO OCH BYLEV                                
001300*                                                                         
001400*                                                                         
001500*    FRÅN PULS:         TRANSAKTION W90428T                               
001600*                       MID         W90428I1                              
001700*                       MOD         W90428O1                              
001800*                                                                         
001900*    FRÅN VOLVO VISION: TRANSAKTION W30151T                               
002000*                       MID         W3I151V1                              
002100*                       MOD         W3O151V1                              
002200*                                                                         
002300 ENVIRONMENT DIVISION.                                                    
002400     SKIP2                                                                
002500 DATA DIVISION.                                                           
002600     EJECT                                                                
002700 WORKING-STORAGE SECTION.                                                 
002800                                                                          
002900*    -- CHECKED BY WY2000                                                 
003000 77  PROGRAM-NAMN                PIC X(8) VALUE 'W9042800'.               
003100 01  WS-IDARTNR                  PIC X(9) VALUE ZERO.                     
003200 01  FILLER REDEFINES WS-IDARTNR.                                         
003300   03  FILLER                    PIC 9(5).                                
003400   03  WS-ARTSIFFRA              PIC 9(1).                                
003500     88  ART-0                   VALUE 6.                                 
003600     88  ART-1                   VALUE 4  7.                              
003700     88  ART-2                   VALUE 5  8.                              
003800     88  ART-3                   VALUE 9.                                 
003900   03  FILLER                    PIC 9(5).                                
004000                                                                          
004100 77  FLAGGA-TRANS-TYP            PIC X       VALUE 'J'.                   
004200     88  PULS-TRANS                          VALUE 'J'.                   
004300     88  VOLVO-VISION-TRANS                  VALUE 'N'.                   
004400                                                                          
004500 77  W-IDPRODNR                  PIC S9(9) COMP-3 VALUE ZERO.             
004600 77  W-BELEV                     PIC X(30) VALUE SPACE.                   
004700 01  WS-IDDISTR-RENOV            PIC 9(5) VALUE ZERO.                     
004800 01  FILLER REDEFINES WS-IDDISTR-RENOV.                                   
004900    03  IDDISTR-RENOV-WS         PIC X(5).                                
005000                                                                          
005100 01  WS-IDDISTR-NDC              PIC 9(5) VALUE ZERO.                     
005200 01  FILLER REDEFINES WS-IDDISTR-NDC.                                     
005300    03  IDDISTR-NDC-WS           PIC X(5).                                
005400                                                                          
005500 01  WS-IDDISTR-PAC              PIC 9(5) VALUE ZERO.                     
005600 01  FILLER REDEFINES WS-IDDISTR-PAC.                                     
005700    03  IDDISTR-PAC-WS           PIC X(5).                                
005800                                                                          
005900 01  WS-IDDISTR-CAN              PIC 9(5) VALUE ZERO.                     
006000 01  FILLER REDEFINES WS-IDDISTR-CAN.                                     
006100    03  IDDISTR-CAN-WS           PIC X(5).                                
006200                                                                          
006300 01  WS-IDDISTR-AUS              PIC 9(5) VALUE ZERO.                     
006400 01  FILLER REDEFINES WS-IDDISTR-AUS.                                     
006500    03  IDDISTR-AUS-WS           PIC X(5).                                
006600                                                                          
006700 01  WS-IDDISTR-CHN              PIC 9(5) VALUE ZERO.                     
006800 01  FILLER REDEFINES WS-IDDISTR-CHN.                                     
006900    03  IDDISTR-CHN-WS           PIC X(5).                                
007000                                                                          
007100 01  WS-IDDISTR-KOR              PIC 9(5) VALUE ZERO.                     
007200 01  FILLER REDEFINES WS-IDDISTR-KOR.                                     
007300    03  IDDISTR-KOR-WS           PIC X(5).                                
007400                                                                          
007491                                                                          
007500 01  WS-IDPRODNR                 PIC 9(8).                                
007600 01  FILLER REDEFINES WS-IDPRODNR.                                        
007700    03  IDPRODNR-WS              PIC X(8).                                
007800 77  JA                          PIC X       VALUE 'J'.                   
007900 77  NEJ                         PIC X       VALUE 'N'.                   
008000 77  RAD-INDX                    PIC S9(9)   VALUE +0   COMP SYNC.        
008100 77  MAX-RAD                     PIC S9(9)   VALUE +3   COMP SYNC.        
008200 77  SPRAK-IX                    PIC S9(9)   VALUE +0   COMP SYNC.        
008300 77  MAX-MOD-LAENGD              PIC S9(4)  VALUE +807  COMP SYNC.        
008400 77  W-IDARTNR-BYT               PIC S9(9) COMP-3 VALUE ZERO.             
008500 01  SW-NYCKLAR-OK               PIC X.                                   
008600   88  NYCKLAR-OK                          VALUE 'J'.                     
008700 01  SW-UPPDATERINGAR            PIC X.                                   
008800   88  INGA-UPPDATERINGAR                  VALUE 'N'.                     
008900 01  SW-INDATA-OK               PIC X.                                    
009000   88  INDATA-OK                          VALUE 'J'.                      
009100 01  SW-INMATAT                 PIC X.                                    
009200   88  INGET-INMATAT                      VALUE 'N'.                      
009300 01  WS-IDTRANS                  PIC X(4).                                
009400   88  EGEN-BILD                           VALUE '9428'.                  
009500 01  FILLER                      PIC X(16)   VALUE                        
009600                                            'NYCKLAR-TILL-DLI'.           
009700 01  NYCKLAR-TILL-DLI.                                                    
009800   03  W-IDARTNR-X.                                                       
009900     05  W-IDARTNR               PIC S9(9) COMP-3 VALUE ZERO.             
010000   03  W-IDSKYLT-X               PIC X(3)         VALUE 'S  '.            
010100     EJECT                                                                
010200 01  DYNAMISKA-SUBPROGRAM.                                                
010300   03  WDATKONV                  PIC X(8)    VALUE 'WDATKONV'.            
010400   03  FELLOG                    PIC X(8)    VALUE 'FELLOG  '.            
010500   03  CBLTDLI                   PIC X(8)    VALUE 'CBLTDLI '.            
010600   03  W005INIT                  PIC X(8)    VALUE 'W005INIT'.            
010700*                     ****  PARAMETRAR TILL W005INIT                      
010800*01  -COPY WMSGINIT                                                       
010900     EJECT                                                                
011000 01  MEDDELANDE.                                                          
011100   03  FEL2.                                                              
011200     05 FILLER                   PIC X(40)                                
011300          VALUE 'BYTESARTIKELNR FEL '.                                    
011400     05 FILLER                   PIC X(40)                                
011500          VALUE 'EXCH. PART.NO WRONG'.                                    
011600   03  FILLER REDEFINES FEL2.                                             
011700     05  FEL-2                   PIC X(40)   OCCURS 2.                    
011800                                                                          
011900   03  FEL3.                                                              
012000     05 FILLER                   PIC X(40)                                
012100          VALUE 'BYTESARTIKELNR SAKNAS'.                                  
012200     05 FILLER                   PIC X(40)                                
012300          VALUE 'EXCH. PART.NO MISSING'.                                  
012400   03  FILLER REDEFINES FEL3.                                             
012500     05  FEL-3                   PIC X(40)   OCCURS 2.                    
012600                                                                          
012700   03  FEL4.                                                              
012800     05 FILLER                   PIC X(40)                                
012900          VALUE 'TRYCK PF11 VID UPPDATERING'.                             
013000     05 FILLER                   PIC X(40)                                
013100          VALUE 'PRESS PF11 WHEN UPDATE'.                                 
013200   03  FILLER REDEFINES FEL4.                                             
013300     05  FEL-4                   PIC X(40)   OCCURS 2.                    
013400                                                                          
013500   03  FEL5.                                                              
013600     05 FILLER                   PIC X(40)                                
013700          VALUE 'INDATA FEL'.                                             
013800     05 FILLER                   PIC X(40)                                
013900          VALUE 'WRONG FIELDS'.                                           
014000   03  FILLER REDEFINES FEL5.                                             
014100     05  FEL-5                   PIC X(40)   OCCURS 2.                    
014200                                                                          
014300   03  FEL6.                                                              
014400     05 FILLER                   PIC X(40)                                
014500          VALUE 'INGET BYTESARTIKELNR'.                                   
014600     05 FILLER                   PIC X(40)                                
014700          VALUE 'NOT AN EXCHANGE NO'.                                     
014800   03  FILLER REDEFINES FEL6.                                             
014900     05  FEL-6                   PIC X(40)   OCCURS 2.                    
015000                                                                          
015100   03  FEL7.                                                              
015200     05 FILLER                   PIC X(40)                                
015300          VALUE 'PRODUKTNR SAKNAS PÅ ARTIKELREGISTRET '.                  
015400     05 FILLER                   PIC X(40)                                
015500          VALUE 'MISSING ON ARTICLE REGISTER'.                            
015600   03  FILLER REDEFINES FEL7.                                             
015700     05  FEL-7                   PIC X(40)   OCCURS 2.                    
015800                                                                          
015900   03  FEL8.                                                              
016000     05 FILLER                   PIC X(40)                                
016100          VALUE 'DB2-TABELL OTILLGÄNGLIG    '.                            
016200     05 FILLER                   PIC X(40)                                
016300          VALUE 'DATABASE UNAVAILABLE       '.                            
016400   03  FILLER REDEFINES FEL8.                                             
016500     05  FEL-8                   PIC X(40)   OCCURS 2.                    
016600                                                                          
016700   03  MED1.                                                              
016800     05 FILLER                   PIC X(40)                                
016900          VALUE 'UPPDATERING GJORD        '.                              
017000     05 FILLER                   PIC X(40)                                
017100          VALUE 'FIELDS ARE UPDATED            '.                         
017200   03  FILLER REDEFINES MED1.                                             
017300     05  MED-1                   PIC X(40)   OCCURS 2.                    
017400                                                                          
017500   03  MED2.                                                              
017600     05 FILLER                   PIC X(40)                                
017700          VALUE 'TRYCK PF8 FÖR FLERA RADER'.                              
017800     05 FILLER                   PIC X(40)                                
017900          VALUE 'PRESS PF8 FOR MORE LINES'.                               
018000   03  FILLER REDEFINES MED2.                                             
018100     05  MED-2                   PIC X(40)   OCCURS 2.                    
018200                                                                          
018300   03  MED3.                                                              
018400     05 FILLER                   PIC X(40)                                
018500          VALUE 'DETTA ÄR FÖRSTA SIDAN'.                                  
018600     05 FILLER                   PIC X(40)                                
018700          VALUE 'THIS IS THE FIRST PAGE'.                                 
018800   03  FILLER REDEFINES MED3.                                             
018900     05  MED-3                   PIC X(40)   OCCURS 2.                    
019000                                                                          
019100   03  MED4.                                                              
019200     05 FILLER                   PIC X(40)                                
019300          VALUE 'FÖRSÖK SENARE, EV KONTAKTA SYSTANSV'.                    
019400     05 FILLER                   PIC X(40)                                
019500          VALUE 'TRY LATER OR NOTIFY THE DP-DEPARTMENT'.                  
019600   03  FILLER REDEFINES MED4.                                             
019700     05  MED-4                   PIC X(40)   OCCURS 2.                    
019800     EJECT                                                                
019900*- - - - - - - - - - - - - - - - - - - BYTES-ARTIKELTEST                  
020000 01  FILLER                      PIC X(16)   VALUE 'BYTES-ART'.           
020100 01  TEST-IDARTNR                PIC 9(9)   COMP-3.                       
020200*01  FILLER -COPY WWBYT01   -RED TEST-IDARTNR                             
020300*01  FILLER -COPY WWBYT16   -RED TEST-IDARTNR                             
020400     EJECT                                                                
020500******************************************************************        
020600*                                                                         
020700*                AREOR FÖR MFS OCH SKÄRMHANTERING                         
020800*                                                                         
020900 01  FILLER                      PIC X(16)   VALUE 'MFS-WS'.              
021000 01  FILLER                      PIC X(16)  VALUE 'MID-AREA PULS'.        
021100*01  MID -COPY W90428I1                                                   
021200     EJECT                                                                
021300 01  FILLER                      PIC X(16)  VALUE 'MID-VOLVISION'.        
021400*01  MID -COPY W3I151V1   -PRE MID2-                                      
021500     EJECT                                                                
021600 01  FILLER                      PIC X(16)  VALUE 'MSG AREA     '.        
021700*01  -COPY WMSGAREA                                                       
021800     EJECT                                                                
021900*  03  MOD -COPY W90428O1  -RED MSG-AREA.                                 
022000     EJECT                                                                
022100*  03  MOD -COPY W3O151V1 -PRE MOD2- -RED MSG-AREA.                       
022200     EJECT                                                                
022300 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
022400*01  -COPY WMFSAREA                                                       
022500     EJECT                                                                
022600******************************************************************        
022700*                                                                         
022800*        ARBETS-AREOR TILL DB2- OCH IMS-SEKTIONERNA                       
022900*                                                                         
023000 01  FILLER                  PIC X(16)   VALUE 'DB2-WS     '.             
023100*01  -COPY BYART -PRE BYART-                                              
023200     EJECT                                                                
023300*01  -COPY BYPRO -PRE BYPRO-                                              
023400     EJECT                                                                
023500*01  -COPY BYLEV -PRE BYLEV-                                              
023600     EJECT                                                                
023700 01  FILLER                  PIC X(16) VALUE 'BYART-AREA'.                
023800       EXEC SQL INCLUDE BYART END-EXEC.                                   
023900     SKIP3                                                                
024000 01  FILLER                  PIC X(16) VALUE 'BYPRO-AREA'.                
024100       EXEC SQL INCLUDE BYPRO END-EXEC.                                   
024200     SKIP3                                                                
024300 01  FILLER                  PIC X(16) VALUE 'BYLEV-AREA'.                
024400       EXEC SQL INCLUDE BYLEV END-EXEC.                                   
024500     SKIP3                                                                
024600 01  FILLER                  PIC X(16) VALUE 'SQLCA-AREA'.                
024700       EXEC SQL INCLUDE SQLCA END-EXEC.                                   
024800*                        **** STATUS-KOD FRÅN DB2                         
024900 01  FILLER                     PIC X(16) VALUE 'SQLCODE-WS'.             
025000 01  DB2-WS.                                                              
025100   03  SQLCODE-WS                PIC 9(3) VALUE ZERO.                     
025200     88  CURSOR-OK                           VALUE 000.                   
025300     88  RADER-FINNS                         VALUE 000.                   
025400     88  RADER-SAKNAS                        VALUE 100.                   
025500     88  904-KOD                             VALUE 904.                   
025600     SKIP1                                                                
025700   03  GODK-SQLCODESKODER.                                                
025800     05  GODK-SQLCODE OCCURS 5                                            
025900         INDEXED BY SQLCODE-IX PIC 999.                                   
026000     EJECT                                                                
026100 01  FILLER                    PIC X(16)   VALUE 'IMS-WS     '.           
026200 01  IMS-WS.                                                              
026300     SKIP3                                                                
026400*                        **** STATUS-KOD FRÅN IMS                         
026500   03  STATUS-WS                 PIC XX.                                  
026600     88  SEGMENT-FINNS                       VALUE '  '.                  
026700     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
026800     SKIP3                                                                
026900   03  GODK-STATUSKODER.                                                  
027000     05  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
027100     SKIP3                                                                
027200 01    SSA1                      PIC X(64).                               
027300 01    SSA2                      PIC X(64).                               
027400 01    SSA3                      PIC X(64).                               
027500*                            IMS FUNKTIONSKODER                           
027600*01    -COPY W0003                                                        
027700     EJECT                                                                
027800 01  FILLER                      PIC X(16) VALUE 'DLI-IO-AREA'.           
027900 01  DLI-IO-AREA.                                                         
028000   03  IO-AREA                   PIC X(128)   VALUE SPACE.                
028100*   03 ARTC01  -COPY WDK601   -PRE ARTC01-   -RED IO-AREA                 
028200     EJECT                                                                
028300*   03 LEVA01  -COPY WDF101   -PRE LEVA01-   -RED IO-AREA                 
028400     EJECT                                                                
028500*   03 BENA11  -COPY WDD311   -PRE BENA11-   -RED IO-AREA                 
028600     EJECT                                                                
028700 LINKAGE SECTION.                                                         
028800*01  -COPY W0009     -PRE MSG-                                            
028900     SKIP2                                                                
029000*01  -COPY W0008     -PRE USEA-                                           
029100     05  FILLER                  PIC X.                                   
029200     SKIP2                                                                
029300*01  -COPY W0008     -PRE ARTC-                                           
029400     05  FILLER                  PIC X.                                   
029500     SKIP2                                                                
029600*01  -COPY W0008     -PRE BENA-                                           
029700     05  FILLER                  PIC X.                                   
029800     SKIP2                                                                
029900*01  -COPY W0008     -PRE LEVA-                                           
030000     05  FILLER                  PIC X.                                   
030100     EJECT                                                                
030200 PROCEDURE DIVISION   USING  MSG-PCB USEA-PCB BENA-PCB LEVA-PCB           
030300                                                       ARTC-PCB.          
030400      ENTRY 'DLITCBL' USING  MSG-PCB USEA-PCB BENA-PCB LEVA-PCB           
030500                                                       ARTC-PCB.          
030600     PERFORM IMS-GET-MSG                                                  
030700     IF SEGMENT-FINNS                                                     
030800        PERFORM A-INIT-KOLLA-NYCKLAR                                      
030900        IF NYCKLAR-OK                                                     
031000           IF PULS-TRANS                                                  
031100              PERFORM B-KOLLA-PF-TRYCK                                    
031200              IF MFS-UPDATE                                               
031300                 PERFORM E-MFS-ROER-EJ-FAELT                              
031400                 PERFORM F-KOLLA-INDATA-EV-UPPDAT                         
031500              ELSE                                                        
031600                 IF MFS-IDPFK = ' '                                       
031700                    PERFORM G-KOLLA-ATT-INGET-IFYLLT                      
031800                    IF INGET-INMATAT                                      
031900                       PERFORM I-VISA-SIDAN                               
032000                    ELSE                                                  
032100                       PERFORM E-MFS-ROER-EJ-FAELT                        
032200                       MOVE FEL-4 (SPRAK-IX) TO MOD-TEMFSFEL              
032300                    END-IF                                                
032400                 ELSE                                                     
032500                    PERFORM I-VISA-SIDAN                                  
032600                 END-IF                                                   
032700              END-IF                                                      
032800           ELSE                                                           
032900*               (VOLVO-VISION-TRANS)                                      
033000              PERFORM I-VISA-SIDAN                                        
033100           END-IF                                                         
033200        ELSE                                                              
033300           IF PULS-TRANS                                                  
033400              PERFORM S05-RENSA-SIDAN                                     
033500           END-IF                                                         
033600        END-IF                                                            
033700                                                                          
033800        IF PULS-TRANS                                                     
033900           COMPUTE MSG-KVLL = LENGTH OF MOD-W90428O1 + 4                  
034000        ELSE                                                              
034100*            (VOLVO-VISION-TRANS)                                         
034200           COMPUTE MSG-KVLL = LENGTH OF MOD2-W3O151V1 + 4                 
034300        END-IF                                                            
034400        PERFORM IMS-INSERT-MSG                                            
034500     END-IF                                                               
034600     MOVE ZERO                            TO RETURN-CODE                  
034700     GOBACK                                                               
034800     .                                                                    
034900     EJECT                                                                
035000 A-INIT-KOLLA-NYCKLAR SECTION.                                            
035100     SKIP2                                                                
035200***                                                                       
035300***  FÖRUTSÄTTER ATT DET ALLTID ÄR SPIE2-TRANS                            
035400***  VOLVO VISION TRANSEN ANVÄNDS EJ I W90428-VERSIONEN                   
035500***                                                                       
035600*    IF MSG-KDTRANS-1 (3:1) = 'T'                                         
035700       MOVE JA             TO FLAGGA-TRANS-TYP                            
035800*    ELSE                                                                 
035900*      MOVE NEJ            TO FLAGGA-TRANS-TYP                            
036000*    END-IF                                                               
036100                                                                          
036200     IF PULS-TRANS                                                        
036300                                                                          
036400        IF MSG-DUBBLA-TRANSKODER                                          
036500           MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W90428I1             
036600           MOVE MSG-IDTRANS-2              TO MFS-IDTRANS                 
036700           MOVE MSG-KDMFSFOR-2             TO MFS-KDMFSFOR                
036800           MOVE MSG-KDTRTYP                TO MFS-KDTRTYP                 
036900           MOVE MSG-IDPFK                  TO MFS-IDPFK                   
037000        ELSE                                                              
037100           MOVE MSG-INDATA-MINUS-1-TRANSKOD TO MID-W90428I1               
037200           MOVE MSG-IDTRANS-1              TO MFS-IDTRANS                 
037300           MOVE MSG-KDMFSFOR-1             TO MFS-KDMFSFOR                
037400           MOVE SPACE                      TO MFS-KDTRTYP                 
037500                                              MFS-IDPFK                   
037600        END-IF                                                            
037700        MOVE LOW-VALUE                     TO MSG-AREA                    
037800        MOVE 'W90428O1'                    TO MFS-IDMOD                   
037900        MOVE '9428'                        TO MOD-IDTRANS                 
038000        MOVE MFS-IDTRANS                   TO WS-IDTRANS                  
038100        MOVE MFS-RENSA-FAELT               TO MOD-TEMFSFEL                
038200                                                 MOD-TEMFSINF             
038300                                                 MOD-IDARTNR-IN           
038400        IF EGEN-BILD                                                      
038500           CONTINUE                                                       
038600        ELSE                                                              
038700           MOVE SPACE                      TO MFS-KDTRTYP                 
038800           MOVE '7'                        TO MFS-IDPFK                   
038900        END-IF                                                            
039000        IF ENGLISH-TEXT                                                   
039100           MOVE +2                         TO SPRAK-IX                    
039200           MOVE 'GB '                      TO W-IDSKYLT-X                 
039300        ELSE                                                              
039400           MOVE +1                         TO SPRAK-IX                    
039500        END-IF                                                            
039600     ELSE                                                                 
039700*         (VOLVO-VISION-TRANS)                                            
039800        MOVE MSG-INDATA-MINUS-1-TRANSKOD TO MID2-W3I151V1                 
039900        MOVE MSG-IDTRANS-1                  TO MFS-IDTRANS                
040000        MOVE MSG-KDMFSFOR-1                 TO MFS-KDMFSFOR               
040100        MOVE MSG-KDTRTYP                    TO MFS-KDTRTYP                
040200        MOVE MSG-IDPFK                      TO MFS-IDPFK                  
040300                                                                          
040400        MOVE LOW-VALUE                     TO MSG-AREA                    
040500        MOVE SPACE                         TO MFS-IDMOD                   
040600        MOVE '9428'                        TO MOD2-IDTRANS                
040700        MOVE MFS-IDTRANS                   TO WS-IDTRANS                  
040800     END-IF                                                               
040900                                                                          
041000     INITIALIZE GODK-SQLCODESKODER                                        
041100     PERFORM AA-KOLLA-NYCKLAR                                             
041200     .                                                                    
041300     EJECT                                                                
041400 AA-KOLLA-NYCKLAR  SECTION.                                               
041500     SKIP3                                                                
041600     MOVE JA                         TO SW-NYCKLAR-OK                     
041700                                                                          
041800     IF PULS-TRANS                                                        
041900        MOVE ALL '+' TO MSGI-WMSGINIT                                     
042000        MOVE '001'          TO MSGI-KDCALL                                
042100        MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                             
042200                               MSGI-IDLTERM-USER                          
042300        MOVE '9428'         TO MSGI-IDTRANS                               
042400        IF MFS-IDTRANS = '9428'                                           
042500        OR (MID-IDARTNR-IN NUMERIC                                        
042600        AND MID-IDARTNR-IN > ZERO)                                        
042700            MOVE MID-IDARTNR-IN TO MSGI-IDARTNR                           
042800        END-IF                                                            
042900        CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                        
043000        MOVE MSGI-IDARTNR TO WS-IDARTNR                                   
043100        INSPECT WS-IDARTNR REPLACING ALL SPACE BY ZERO                    
043200                                                                          
043300        IF MID-IDARTNR-IN = ALL '+'                                       
043400          CONTINUE                                                        
043500        ELSE                                                              
043600          MOVE '7'      TO MFS-IDPFK                                      
043700          MOVE SPACE    TO MFS-KDTRTYP                                    
043800        END-IF                                                            
043900                                                                          
044000*       MOVE WS-IDARTNR              TO MOD-IDARTNR-UT                    
044100*       INSPECT MOD-IDARTNR-UT REPLACING LEADING ZERO BY SPACE            
044200        IF WS-IDARTNR NUMERIC                                             
044300           MOVE WS-IDARTNR TO TEST-IDARTNR                                
044400           IF BYT01-BYTES                                                 
044500              IF BYT16-RADIO                                              
044600                 MOVE 3                    TO WS-ARTSIFFRA                
044700              ELSE                                                        
044800                 IF ART-0                                                 
044900                    MOVE 0                   TO WS-ARTSIFFRA              
045000                 ELSE                                                     
045100                   IF ART-1                                               
045200                      MOVE 1                 TO WS-ARTSIFFRA              
045300                   ELSE                                                   
045400                      IF ART-2                                            
045500                         MOVE 2              TO WS-ARTSIFFRA              
045600                      ELSE                                                
045700                         IF ART-3                                         
045800                            MOVE 3           TO WS-ARTSIFFRA              
045900                         END-IF                                           
046000                      END-IF                                              
046100                   END-IF                                                 
046200                 END-IF                                                   
046300              END-IF                                                      
046400              MOVE WS-IDARTNR           TO W-IDARTNR-BYT                  
046500                                              W-IDARTNR                   
046600              PERFORM IMS-GU-ARTC01                                       
046700              IF SEGMENT-FINNS                                            
046800                 CONTINUE                                                 
046900              ELSE                                                        
047000                 MOVE NEJ               TO SW-NYCKLAR-OK                  
047100                 MOVE FEL-3 (SPRAK-IX)  TO MOD-TEMFSFEL                   
047200              END-IF                                                      
047300           ELSE                                                           
047400              MOVE NEJ               TO SW-NYCKLAR-OK                     
047500              MOVE FEL-6 (SPRAK-IX)  TO MOD-TEMFSFEL                      
047600           END-IF                                                         
047700        ELSE                                                              
047800           MOVE NEJ                  TO SW-NYCKLAR-OK                     
047900           MOVE FEL-2 (SPRAK-IX)     TO MOD-TEMFSFEL                      
048000        END-IF                                                            
048100     ELSE                                                                 
048200*         (VOLVO-VISION-TRANS)                                            
048300        MOVE ZERO                       TO MOD2-IDMFSFEL                  
048400        MOVE MID2-IDARTNR-BYT           TO WS-IDARTNR                     
048500        INSPECT WS-IDARTNR REPLACING ALL SPACE BY ZERO                    
048600        IF WS-IDARTNR NUMERIC                                             
048700           MOVE WS-IDARTNR              TO TEST-IDARTNR                   
048800                                           MOD2-IDARTNR-BYT-INPUT         
048900           IF BYT01-BYTES                                                 
049000              IF BYT16-RADIO                                              
049100                 MOVE 3                 TO WS-ARTSIFFRA                   
049200              ELSE                                                        
049300                 IF ART-0                                                 
049400                    MOVE 0                TO WS-ARTSIFFRA                 
049500                 ELSE                                                     
049600                   IF ART-1                                               
049700                      MOVE 1              TO WS-ARTSIFFRA                 
049800                   ELSE                                                   
049900                      IF ART-2                                            
050000                         MOVE 2           TO WS-ARTSIFFRA                 
050100                      ELSE                                                
050200                         IF ART-3                                         
050300                            MOVE 3        TO WS-ARTSIFFRA                 
050400                         END-IF                                           
050500                      END-IF                                              
050600                   END-IF                                                 
050700                 END-IF                                                   
050800              END-IF                                                      
050900              MOVE WS-IDARTNR           TO W-IDARTNR-BYT                  
051000                                              W-IDARTNR                   
051100              PERFORM IMS-GU-ARTC01                                       
051200              IF SEGMENT-FINNS                                            
051300                 CONTINUE                                                 
051400              ELSE                                                        
051500                 MOVE NEJ         TO SW-NYCKLAR-OK                        
051600                 MOVE 'B10'       TO MOD2-IDMFSFEL                        
051700              END-IF                                                      
051800           ELSE                                                           
051900              MOVE NEJ            TO SW-NYCKLAR-OK                        
052000              MOVE 'B10'          TO MOD2-IDMFSFEL                        
052100           END-IF                                                         
052200        ELSE                                                              
052300           MOVE NEJ               TO SW-NYCKLAR-OK                        
052400           MOVE 'B10'             TO MOD2-IDMFSFEL                        
052500        END-IF                                                            
052600     END-IF                                                               
052700     .                                                                    
052800     EJECT                                                                
052900 B-KOLLA-PF-TRYCK  SECTION.                                               
053000     SKIP2                                                                
053100     IF  MID-IDPRODNR-LO NUMERIC                                          
053200     AND MID-IDPRODNR-HI NUMERIC                                          
053300        CONTINUE                                                          
053400     ELSE                                                                 
053500        MOVE ZERO                     TO MID-IDPRODNR-LO                  
053600                                         MID-IDPRODNR-HI                  
053700     END-IF                                                               
053800     IF MFS-IDPFK = '8'                                                   
053900        IF  MID-IDPRODNR-HI = ZERO                                        
054000        AND MID-BELEV-HI  = SPACE                                         
054100           MOVE '7'                   TO MFS-IDPFK                        
054200        END-IF                                                            
054300     END-IF                                                               
054400     IF MFS-IDPFK = '8'                                                   
054500        IF  MID-IDPRODNR-HI = ZERO                                        
054600           MOVE MID-IDPRODNR-LO       TO W-IDPRODNR                       
054700        ELSE                                                              
054800           MOVE MID-IDPRODNR-HI       TO W-IDPRODNR                       
054900        END-IF                                                            
055000        IF  MID-BELEV-HI  = SPACE                                         
055100           MOVE MID-BELEV-LO          TO W-BELEV                          
055200        ELSE                                                              
055300           MOVE MID-BELEV-HI          TO W-BELEV                          
055400        END-IF                                                            
055500     ELSE                                                                 
055600        IF MFS-IDPFK = ' '                                                
055700           MOVE MID-BELEV-LO          TO W-BELEV                          
055800           MOVE MID-IDPRODNR-LO       TO W-IDPRODNR                       
055900        ELSE                                                              
056000            MOVE LOW-VALUE            TO W-BELEV                          
056100            MOVE ZERO                 TO W-IDPRODNR                       
056200            MOVE MED-3 (SPRAK-IX)     TO MOD-TEMFSFEL                     
056300        END-IF                                                            
056400     END-IF                                                               
056500     .                                                                    
056600     EJECT                                                                
056700 E-MFS-ROER-EJ-FAELT  SECTION.                                            
056800     SKIP2                                                                
056900     MOVE MFS-ROER-EJ-FAELT          TO                                   
057000                                        MOD-IDPRODNR-LO                   
057100                                        MOD-IDPRODNR-HI                   
057200                                        MOD-BELEV-LO                      
057300                                        MOD-BELEV-HI                      
057400                                        MOD-BEART-SVE                     
057500                                        MOD-BETFLEV                       
057600                                        MOD-UPPDATERINGSSORT              
057700                                        MOD-IDPRODNR-IN                   
057800                                        MOD-IDDISTR-RENOV-IN              
057900                                        MOD-IDDISTR-NDC-IN                
058000                                        MOD-IDDISTR-PAC-IN                
058100                                        MOD-IDDISTR-CAN-IN                
058200                                        MOD-IDDISTR-AUS-IN                
058300                                        MOD-IDDISTR-CHN-IN                
058400                                        MOD-IDDISTR-KOR-IN                
058500                                        MOD-BELEV-IN                      
058600     MOVE +1                         TO RAD-INDX                          
058700     PERFORM UNTIL RAD-INDX > +3                                          
058800        MOVE MFS-ROER-EJ-FAELT       TO MOD-TEBYTNOT (RAD-INDX)           
058900        ADD +1                       TO RAD-INDX                          
059000     END-PERFORM                                                          
059100     MOVE MFS-ROER-EJ-FAELT       TO MOD-IDDISTR-RENOV                    
059200     MOVE MFS-ROER-EJ-FAELT       TO MOD-IDDISTR-NDC                      
059300     MOVE MFS-ROER-EJ-FAELT       TO MOD-IDDISTR-PAC                      
059400     MOVE MFS-ROER-EJ-FAELT       TO MOD-IDDISTR-CAN                      
059500     MOVE MFS-ROER-EJ-FAELT       TO MOD-IDDISTR-AUS                      
059600     MOVE MFS-ROER-EJ-FAELT       TO MOD-IDDISTR-CHN                      
059700     MOVE MFS-ROER-EJ-FAELT       TO MOD-IDDISTR-KOR                      
059800     MOVE MFS-ROER-EJ-FAELT       TO MOD-TEBYTNOT(4)                      
059900     MOVE +1                         TO RAD-INDX                          
060000     PERFORM UNTIL RAD-INDX > +18                                         
060100        MOVE MFS-ROER-EJ-FAELT       TO MOD-IDARTNR (RAD-INDX)            
060200        ADD +1                       TO RAD-INDX                          
060300     END-PERFORM                                                          
060400     MOVE +1                         TO RAD-INDX                          
060500     PERFORM UNTIL RAD-INDX > +6                                          
060600        MOVE MFS-ROER-EJ-FAELT       TO MOD-BELEV   (RAD-INDX)            
060700        ADD +1                       TO RAD-INDX                          
060800     END-PERFORM                                                          
060900     .                                                                    
061000     EJECT                                                                
061100 F-KOLLA-INDATA-EV-UPPDAT  SECTION.                                       
061200     SKIP2                                                                
061300     IF  MID-UPPDATERINGSSORT = ALL '+'                                   
061400     AND MID-IDPRODNR-IN = ALL '+'                                        
061500     AND MID-BELEV = ALL '+'                                              
061600        PERFORM FH-EV-UPPD-BYTNOT-BETFLEV                                 
061700     ELSE                                                                 
061800        MOVE JA                           TO SW-INDATA-OK                 
061900        IF MID-UPPDATERINGSSORT = 'N'                                     
062000           MOVE MFS-ALFA-FAELT-RAETT      TO                              
062100                                   MOD-UPPDATERINGSSORT-ATTR              
062200           PERFORM FD-KOLLA-INDATA-NYUPPL                                 
062300        ELSE                                                              
062400           IF MID-UPPDATERINGSSORT = 'D' OR 'B'                           
062500              MOVE MFS-ALFA-FAELT-RAETT   TO                              
062600                                   MOD-UPPDATERINGSSORT-ATTR              
062700              PERFORM FA-KOLLA-INDATA-BORTTAG                             
062800           ELSE                                                           
062900              MOVE NEJ                    TO SW-INDATA-OK                 
063000              MOVE MFS-ALFA-FAELT-FEL     TO                              
063100                                      MOD-UPPDATERINGSSORT-ATTR           
063200              PERFORM FG-LAES-IN-IGEN                                     
063300           END-IF                                                         
063400        END-IF                                                            
063500        IF INDATA-OK                                                      
063600           MOVE MED-1 (SPRAK-IX)     TO MOD-TEMFSINF                      
063700           MOVE W-IDARTNR-BYT        TO W-IDARTNR                         
063800           IF MID-UPPDATERINGSSORT = 'N'                                  
063900              PERFORM DB2-SELECT-BYART                                    
064000              IF SQLCODE = ZERO                                           
064100                 PERFORM FE-UPPDATERA-BYART                               
064200                 PERFORM FC-SKAP-BYLEV-BYPRO-RENSA-INM                    
064300              ELSE                                                        
064400                 IF 904-KOD                                               
064500                    MOVE FEL-8 (SPRAK-IX)    TO MOD-TEMFSFEL              
064600                    MOVE MED-4 (SPRAK-IX)    TO MOD-TEMFSINF              
064700                 ELSE                                                     
064800                    PERFORM IMS-GU-BENA11                                 
064900                    IF SEGMENT-FINNS                                      
065000                       MOVE BENA11-TEXT-BEART   TO MOD-BEART-SVE          
065100                    END-IF                                                
065200                    PERFORM FF-SKAPA-BYART                                
065300                    PERFORM FC-SKAP-BYLEV-BYPRO-RENSA-INM                 
065400                 END-IF                                                   
065500              END-IF                                                      
065600           ELSE                                                           
065700              PERFORM FB-TA-BORT                                          
065800              PERFORM S04-RENSA-INMATAT                                   
065900           END-IF                                                         
066000        ELSE                                                              
066100           MOVE FEL-5 (SPRAK-IX)     TO MOD-TEMFSFEL                      
066200        END-IF                                                            
066300     END-IF                                                               
066400     .                                                                    
066500     EJECT                                                                
066600 FA-KOLLA-INDATA-BORTTAG SECTION.                                         
066700     SKIP2                                                                
066800     IF MID-IDPRODNR-IN = ALL '+'                                         
066900        CONTINUE                                                          
067000     ELSE                                                                 
067100        IF MID-IDPRODNR-IN NUMERIC                                        
067200           MOVE MFS-NUM-FAELT-RAETT       TO MOD-IDPRODNR-IN-ATTR         
067300        ELSE                                                              
067400           MOVE NEJ                       TO SW-INDATA-OK                 
067500           MOVE MFS-NUM-FAELT-FEL         TO MOD-IDPRODNR-IN-ATTR         
067600        END-IF                                                            
067700     END-IF                                                               
067800     IF MID-IDDISTR-RENOV-IN = ALL '+'                                    
067900        CONTINUE                                                          
068000     ELSE                                                                 
068100        IF MID-IDDISTR-RENOV-IN NUMERIC                                   
068200           MOVE MFS-NUM-FAELT-RAETT  TO MOD-IDDISTR-RENOV-IN-ATTR         
068300        ELSE                                                              
068400           MOVE NEJ                       TO SW-INDATA-OK                 
068500           MOVE MFS-NUM-FAELT-FEL    TO MOD-IDDISTR-RENOV-IN-ATTR         
068600        END-IF                                                            
068700     END-IF                                                               
068800     IF MID-IDDISTR-NDC-IN = ALL '+'                                      
068900        CONTINUE                                                          
069000     ELSE                                                                 
069100        IF MID-IDDISTR-NDC-IN NUMERIC                                     
069200           MOVE MFS-NUM-FAELT-RAETT  TO MOD-IDDISTR-NDC-IN-ATTR           
069300        ELSE                                                              
069400           MOVE NEJ                       TO SW-INDATA-OK                 
069500           MOVE MFS-NUM-FAELT-FEL    TO MOD-IDDISTR-NDC-IN-ATTR           
069600        END-IF                                                            
069700     END-IF                                                               
069800     IF MID-IDDISTR-PAC-IN = ALL '+'                                      
069900        CONTINUE                                                          
070000     ELSE                                                                 
070100        IF MID-IDDISTR-PAC-IN NUMERIC                                     
070200           MOVE MFS-NUM-FAELT-RAETT  TO MOD-IDDISTR-PAC-IN-ATTR           
070300        ELSE                                                              
070400           MOVE NEJ                       TO SW-INDATA-OK                 
070500           MOVE MFS-NUM-FAELT-FEL    TO MOD-IDDISTR-PAC-IN-ATTR           
070600        END-IF                                                            
070700     END-IF                                                               
070800                                                                          
070900     IF MID-IDDISTR-CAN-IN = ALL '+'                                      
071000        CONTINUE                                                          
071100     ELSE                                                                 
071200        IF MID-IDDISTR-CAN-IN NUMERIC                                     
071300           MOVE MFS-NUM-FAELT-RAETT  TO MOD-IDDISTR-CAN-IN-ATTR           
071400        ELSE                                                              
071500           MOVE NEJ                       TO SW-INDATA-OK                 
071600           MOVE MFS-NUM-FAELT-FEL    TO MOD-IDDISTR-CAN-IN-ATTR           
071700        END-IF                                                            
071800     END-IF                                                               
071900                                                                          
072000     IF MID-IDDISTR-AUS-IN = ALL '+'                                      
072100        CONTINUE                                                          
072200     ELSE                                                                 
072300        IF MID-IDDISTR-AUS-IN NUMERIC                                     
072400           MOVE MFS-NUM-FAELT-RAETT  TO MOD-IDDISTR-AUS-IN-ATTR           
072500        ELSE                                                              
072600           MOVE NEJ                       TO SW-INDATA-OK                 
072700           MOVE MFS-NUM-FAELT-FEL    TO MOD-IDDISTR-AUS-IN-ATTR           
072800        END-IF                                                            
072900     END-IF                                                               
073000                                                                          
073100     IF MID-IDDISTR-CHN-IN = ALL '+'                                      
073200        CONTINUE                                                          
073300     ELSE                                                                 
073400        IF MID-IDDISTR-CHN-IN NUMERIC                                     
073500           MOVE MFS-NUM-FAELT-RAETT  TO MOD-IDDISTR-CHN-IN-ATTR           
073600        ELSE                                                              
073700           MOVE NEJ                       TO SW-INDATA-OK                 
073800           MOVE MFS-NUM-FAELT-FEL    TO MOD-IDDISTR-CHN-IN-ATTR           
073900        END-IF                                                            
074000     END-IF                                                               
074100                                                                          
074200     IF MID-IDDISTR-KOR-IN = ALL '+'                                      
074300        CONTINUE                                                          
074400     ELSE                                                                 
074500        IF MID-IDDISTR-KOR-IN NUMERIC                                     
074600           MOVE MFS-NUM-FAELT-RAETT  TO MOD-IDDISTR-KOR-IN-ATTR           
074700        ELSE                                                              
074800           MOVE NEJ                       TO SW-INDATA-OK                 
074900           MOVE MFS-NUM-FAELT-FEL    TO MOD-IDDISTR-KOR-IN-ATTR           
075000        END-IF                                                            
075100     END-IF                                                               
075200                                                                          
075300     IF MID-BELEV = ALL '+'                                               
075400        CONTINUE                                                          
075500     ELSE                                                                 
075600        MOVE MID-BELEV                 TO W-BELEV                         
075700        PERFORM DB2-SELECT-BYLEV                                          
075800        IF CURSOR-OK                                                      
075900           IF BYLEV-IDARTNR-BYT = W-IDARTNR-BYT                           
076000              MOVE MFS-ALFA-FAELT-RAETT TO                                
076100                                       MOD-BELEV-IN-ATTR                  
076200           ELSE                                                           
076300              MOVE NEJ                 TO SW-INDATA-OK                    
076400              MOVE MFS-ALFA-FAELT-FEL  TO                                 
076500                                       MOD-BELEV-IN-ATTR                  
076600           END-IF                                                         
076700        ELSE                                                              
076800           MOVE MFS-ALFA-FAELT-RAETT   TO                                 
076900                                       MOD-BELEV-IN-ATTR                  
077000        END-IF                                                            
077100     END-IF                                                               
077200     .                                                                    
077300     EJECT                                                                
077400 FB-TA-BORT    SECTION.                                                   
077500     SKIP3                                                                
077600     IF  MID-TEBYTNOT(1)      = ALL '+'                                   
077700     AND MID-TEBYTNOT(2)      = ALL '+'                                   
077800     AND MID-TEBYTNOT(3)      = ALL '+'                                   
077900     AND MID-TEBYTNOT(4)      = ALL '+'                                   
078000     AND MID-BETFLEV          = ALL '+'                                   
078100     AND MID-IDDISTR-RENOV-IN = ALL '+'                                   
078200     AND MID-IDDISTR-NDC-IN   = ALL '+'                                   
078300     AND MID-IDDISTR-PAC-IN   = ALL '+'                                   
078400     AND MID-IDDISTR-CAN-IN   = ALL '+'                                   
078500     AND MID-IDDISTR-AUS-IN   = ALL '+'                                   
078600     AND MID-IDDISTR-CHN-IN   = ALL '+'                                   
078700     AND MID-IDDISTR-KOR-IN   = ALL '+'                                   
078800        IF  MID-BELEV = ALL '+'                                           
078900        AND MID-IDPRODNR-IN = ALL '+'                                     
079000           PERFORM DB2-DELETE-BYART                                       
079100           IF 904-KOD                                                     
079200              MOVE FEL-8 (SPRAK-IX)    TO MOD-TEMFSFEL                    
079300              MOVE MED-4 (SPRAK-IX)    TO MOD-TEMFSINF                    
079400           ELSE                                                           
079500              PERFORM S05-RENSA-SIDAN                                     
079600           END-IF                                                         
079700        ELSE                                                              
079800           PERFORM FBB-EV-TABORT-BYPRO-BYLEV                              
079900        END-IF                                                            
080000     ELSE                                                                 
080100        PERFORM FBA-TABORT-ELEMENT-BYART                                  
080200        PERFORM FBB-EV-TABORT-BYPRO-BYLEV                                 
080300     END-IF                                                               
080400     .                                                                    
080500     EJECT                                                                
080600 FBA-TABORT-ELEMENT-BYART SECTION.                                        
080700     SKIP1                                                                
080800     PERFORM DB2-SELECT-BYART                                             
080900     IF SQLCODE = ZERO                                                    
081000        MOVE +1                         TO RAD-INDX                       
081100        PERFORM UNTIL RAD-INDX > 4                                        
081200           IF MID-TEBYTNOT (RAD-INDX) = ALL '+'                           
081300              ADD +1                    TO RAD-INDX                       
081400           ELSE                                                           
081500              PERFORM FBAA-BEHANDLA-BYTNOT                                
081600           END-IF                                                         
081700        END-PERFORM                                                       
081800                                                                          
081900        IF MID-IDDISTR-PAC-IN = ALL '+'                                   
082000           CONTINUE                                                       
082100        ELSE                                                              
082200           PERFORM FBAD-TA-BORT-IDDISTR-PAC                               
082300        END-IF                                                            
082400                                                                          
082500        IF MID-IDDISTR-CAN-IN = ALL '+'                                   
082600           CONTINUE                                                       
082700        ELSE                                                              
082800           PERFORM FBAF-TA-BORT-IDDISTR-CAN                               
082900        END-IF                                                            
083000                                                                          
083100        IF MID-IDDISTR-AUS-IN = ALL '+'                                   
083200           CONTINUE                                                       
083300        ELSE                                                              
083400           PERFORM FBAG-TA-BORT-IDDISTR-AUS                               
083500        END-IF                                                            
083600                                                                          
083700        IF MID-IDDISTR-CHN-IN = ALL '+'                                   
083800           CONTINUE                                                       
083900        ELSE                                                              
084000           PERFORM FBAH-TA-BORT-IDDISTR-CHN                               
084100        END-IF                                                            
084200                                                                          
084300        IF MID-IDDISTR-KOR-IN = ALL '+'                                   
084400           CONTINUE                                                       
084500        ELSE                                                              
084600           PERFORM FBAI-TA-BORT-IDDISTR-KOR                               
084700        END-IF                                                            
084800                                                                          
084900        IF MID-IDDISTR-NDC-IN = ALL '+'                                   
085000           CONTINUE                                                       
085100        ELSE                                                              
085200           PERFORM FBAE-TA-BORT-IDDISTR-NDC                               
085300        END-IF                                                            
085400                                                                          
085500        IF MID-IDDISTR-RENOV-IN = ALL '+'                                 
085600           CONTINUE                                                       
085700        ELSE                                                              
085800           PERFORM FBAC-TA-BORT-IDDISTR-RENOV                             
085900        END-IF                                                            
086000                                                                          
086100        IF MID-BETFLEV = ALL '+'                                          
086200           CONTINUE                                                       
086300        ELSE                                                              
086400           MOVE SPACE                 TO BYART-BETFLEV                    
086500           MOVE MFS-FORMATETS-ATTR    TO MOD-BETFLEV-ATTR                 
086600        END-IF                                                            
086700        PERFORM S01-FLYTT-BYART-T-BILD                                    
086800        PERFORM DB2-UPDATE-BYART                                          
086900     ELSE                                                                 
087000        IF 904-KOD                                                        
087100           MOVE FEL-8 (SPRAK-IX)    TO MOD-TEMFSFEL                       
087200           MOVE MED-4 (SPRAK-IX)    TO MOD-TEMFSINF                       
087300        END-IF                                                            
087400     END-IF                                                               
087500     .                                                                    
087600     EJECT                                                                
087700 FBAA-BEHANDLA-BYTNOT  SECTION.                                           
087800     SKIP3                                                                
087900     PERFORM UNTIL RAD-INDX > 4                                           
088000        IF MID-TEBYTNOT (RAD-INDX) = ALL '+'                              
088100           CONTINUE                                                       
088200        ELSE                                                              
088300           IF RAD-INDX = 1                                                
088400              MOVE SPACE          TO BYART-TEBYTNOT1                      
088500           ELSE                                                           
088600              IF RAD-INDX = 2                                             
088700                 MOVE SPACE          TO BYART-TEBYTNOT2                   
088800              ELSE                                                        
088900                IF RAD-INDX = 3                                           
089000                   MOVE SPACE          TO BYART-TEBYTNOT3                 
089100                ELSE                                                      
089200                  MOVE MID-IDDISTR-RENOV                                  
089300                           TO IDDISTR-RENOV-WS                            
089400                  INSPECT  IDDISTR-RENOV-WS REPLACING                     
089500                           LEADING SPACE BY ZERO                          
089600                  IF  WS-IDDISTR-RENOV = BYART-IDDISTR-RENOV              
089700                      MOVE SPACE          TO BYART-TEBYTNOT4              
089800                  END-IF                                                  
089900                END-IF                                                    
090000              END-IF                                                      
090100           END-IF                                                         
090200           MOVE MFS-FORMATETS-ATTR       TO                               
090300                                      MOD-TEBYTNOT-ATTR(RAD-INDX)         
090400        END-IF                                                            
090500        ADD +1                                TO RAD-INDX                 
090600     END-PERFORM                                                          
090700     .                                                                    
090800     EJECT                                                                
090900 FBAC-TA-BORT-IDDISTR-RENOV SECTION.                                      
091000     SKIP2                                                                
091100     MOVE MID-IDDISTR-RENOV-IN TO IDDISTR-RENOV-WS                        
091200     IF  WS-IDDISTR-RENOV = BYART-IDDISTR-RENOV                           
091300        MOVE ZERO              TO BYART-IDDISTR-RENOV                     
091400     END-IF                                                               
091500     MOVE MFS-FORMATETS-ATTR   TO MOD-IDDISTR-RENOV-IN-ATTR               
091600     .                                                                    
091700     EJECT                                                                
091800 FBAD-TA-BORT-IDDISTR-PAC   SECTION.                                      
091900     SKIP2                                                                
092000     MOVE MID-IDDISTR-PAC-IN TO IDDISTR-PAC-WS                            
092100     IF  WS-IDDISTR-PAC = BYART-IDDISTR-RENOV-PAC                         
092200        MOVE ZERO              TO BYART-IDDISTR-RENOV-PAC                 
092300     END-IF                                                               
092400     MOVE MFS-FORMATETS-ATTR   TO MOD-IDDISTR-PAC-IN-ATTR                 
092500     .                                                                    
092600     EJECT                                                                
092700 FBAE-TA-BORT-IDDISTR-NDC   SECTION.                                      
092800     SKIP2                                                                
092900     MOVE MID-IDDISTR-NDC-IN TO IDDISTR-NDC-WS                            
093000     IF  WS-IDDISTR-NDC = BYART-IDDISTR-RENOV-NDC                         
093100        MOVE ZERO              TO BYART-IDDISTR-RENOV-NDC                 
093200     END-IF                                                               
093300     MOVE MFS-FORMATETS-ATTR   TO MOD-IDDISTR-NDC-IN-ATTR                 
093400     .                                                                    
093500     EJECT                                                                
093600 FBAF-TA-BORT-IDDISTR-CAN   SECTION.                                      
093700     SKIP2                                                                
093800     MOVE MID-IDDISTR-CAN-IN TO IDDISTR-CAN-WS                            
093900     IF  WS-IDDISTR-CAN = BYART-IDDISTR-RENOV-CAN                         
094000        MOVE ZERO              TO BYART-IDDISTR-RENOV-CAN                 
094100     END-IF                                                               
094200     MOVE MFS-FORMATETS-ATTR   TO MOD-IDDISTR-CAN-IN-ATTR                 
094300     .                                                                    
094400     EJECT                                                                
094500 FBAG-TA-BORT-IDDISTR-AUS   SECTION.                                      
094600     SKIP2                                                                
094700     MOVE MID-IDDISTR-AUS-IN TO IDDISTR-AUS-WS                            
094800     IF  WS-IDDISTR-AUS = BYART-IDDISTR-RENOV-AUS                         
094900        MOVE ZERO              TO BYART-IDDISTR-RENOV-AUS                 
095000     END-IF                                                               
095100     MOVE MFS-FORMATETS-ATTR   TO MOD-IDDISTR-AUS-IN-ATTR                 
095200     .                                                                    
095300     EJECT                                                                
095400 FBAH-TA-BORT-IDDISTR-CHN   SECTION.                                      
095500     SKIP2                                                                
095600     MOVE MID-IDDISTR-CHN-IN TO IDDISTR-CHN-WS                            
095700     IF  WS-IDDISTR-CHN = BYART-IDDISTR-RENOV-CHN                         
095800        MOVE ZERO              TO BYART-IDDISTR-RENOV-CHN                 
095900     END-IF                                                               
096000     MOVE MFS-FORMATETS-ATTR   TO MOD-IDDISTR-CHN-IN-ATTR                 
096100     .                                                                    
096200     EJECT                                                                
096300 FBAI-TA-BORT-IDDISTR-KOR   SECTION.                                      
096400     SKIP2                                                                
096500     MOVE MID-IDDISTR-KOR-IN TO IDDISTR-KOR-WS                            
096600     IF  WS-IDDISTR-KOR = BYART-IDDISTR-RENOV-KOR                         
096700        MOVE ZERO              TO BYART-IDDISTR-RENOV-KOR                 
096800     END-IF                                                               
096900     MOVE MFS-FORMATETS-ATTR   TO MOD-IDDISTR-KOR-IN-ATTR                 
097000     .                                                                    
097100     EJECT                                                                
097200 FBB-EV-TABORT-BYPRO-BYLEV SECTION.                                       
097300     SKIP2                                                                
097400     IF MID-IDPRODNR-IN = ALL '+'                                         
097500        CONTINUE                                                          
097600     ELSE                                                                 
097700        MOVE MID-IDPRODNR-IN        TO W-IDPRODNR                         
097800        PERFORM DB2-DLET-BYPRO                                            
097900        IF 904-KOD                                                        
098000           MOVE FEL-8 (SPRAK-IX)    TO MOD-TEMFSFEL                       
098100           MOVE MED-4 (SPRAK-IX)    TO MOD-TEMFSINF                       
098200        ELSE                                                              
098300           PERFORM S02-FLYTT-BYPRO-T-BILD                                 
098400           MOVE MFS-FORMATETS-ATTR     TO MOD-IDPRODNR-IN-ATTR            
098500        END-IF                                                            
098600     END-IF                                                               
098700     IF MID-BELEV = ALL '+'                                               
098800        CONTINUE                                                          
098900     ELSE                                                                 
099000        PERFORM DB2-DLET-BYLEV                                            
099100        IF 904-KOD                                                        
099200           MOVE FEL-8 (SPRAK-IX)    TO MOD-TEMFSFEL                       
099300           MOVE MED-4 (SPRAK-IX)    TO MOD-TEMFSINF                       
099400        ELSE                                                              
099500           PERFORM S03-FLYTT-BYLEV-T-BILD                                 
099600           MOVE MFS-FORMATETS-ATTR     TO MOD-BELEV-IN-ATTR               
099700        END-IF                                                            
099800     END-IF                                                               
099900     .                                                                    
100000     EJECT                                                                
100100 FC-SKAP-BYLEV-BYPRO-RENSA-INM   SECTION.                                 
100200     SKIP2                                                                
100300     IF MID-BELEV = ALL '+'                                               
100400        CONTINUE                                                          
100500     ELSE                                                                 
100600        PERFORM DB2-ISRT-BYLEV                                            
100700        PERFORM S03-FLYTT-BYLEV-T-BILD                                    
100800        MOVE MFS-FORMATETS-ATTR           TO MOD-BELEV-IN-ATTR            
100900     END-IF                                                               
101000     IF MID-IDPRODNR-IN = ALL '+'                                         
101100        CONTINUE                                                          
101200     ELSE                                                                 
101300        MOVE MID-IDPRODNR-IN             TO W-IDPRODNR                    
101400        PERFORM DB2-ISRT-BYPRO                                            
101500        PERFORM S02-FLYTT-BYPRO-T-BILD                                    
101600        MOVE MFS-FORMATETS-ATTR           TO MOD-IDPRODNR-IN-ATTR         
101700     END-IF                                                               
101800     PERFORM S04-RENSA-INMATAT                                            
101900     .                                                                    
102000     EJECT                                                                
102100 FD-KOLLA-INDATA-NYUPPL SECTION.                                          
102200     SKIP2                                                                
102300     IF MID-IDPRODNR-IN = ALL '+'                                         
102400        CONTINUE                                                          
102500     ELSE                                                                 
102600        IF MID-IDPRODNR-IN NUMERIC                                        
102700           MOVE MID-IDPRODNR-IN           TO W-IDPRODNR                   
102800                                             W-IDARTNR                    
102900           PERFORM IMS-GU-ARTC01                                          
103000           MOVE MFS-NUM-FAELT-RAETT       TO MOD-IDPRODNR-IN-ATTR         
103100           IF SEGMENT-SAKNAS                                              
103200              MOVE FEL-7 (SPRAK-IX)       TO MOD-TEMFSFEL                 
103300           END-IF                                                         
103400        ELSE                                                              
103500           MOVE NEJ                       TO SW-INDATA-OK                 
103600           MOVE MFS-NUM-FAELT-FEL         TO MOD-IDPRODNR-IN-ATTR         
103700        END-IF                                                            
103800     END-IF                                                               
103900                                                                          
104000     IF MID-BELEV = ALL '+'                                               
104100        CONTINUE                                                          
104200     ELSE                                                                 
104300        MOVE MID-BELEV                 TO W-BELEV                         
104400        PERFORM DB2-SELECT-BYLEV                                          
104500        IF CURSOR-OK                                                      
104600           MOVE NEJ                    TO SW-INDATA-OK                    
104700           MOVE MFS-ALFA-FAELT-FEL     TO                                 
104800                                       MOD-BELEV-IN-ATTR                  
104900        ELSE                                                              
105000           MOVE MFS-ALFA-FAELT-RAETT   TO                                 
105100                                       MOD-BELEV-IN-ATTR                  
105200        END-IF                                                            
105300     END-IF                                                               
105400     .                                                                    
105500     EJECT                                                                
105600 FE-UPPDATERA-BYART SECTION.                                              
105700     SKIP3                                                                
105800     IF  MID-TEBYTNOT(1)      = ALL '+'                                   
105900     AND MID-TEBYTNOT(2)      = ALL '+'                                   
106000     AND MID-TEBYTNOT(3)      = ALL '+'                                   
106100     AND MID-TEBYTNOT(4)      = ALL '+'                                   
106200     AND MID-BETFLEV          = ALL '+'                                   
106300     AND MID-IDDISTR-RENOV-IN = ALL '+'                                   
106400     AND MID-IDDISTR-NDC-IN   = ALL '+'                                   
106500     AND MID-IDDISTR-PAC-IN   = ALL '+'                                   
106600     AND MID-IDDISTR-CAN-IN   = ALL '+'                                   
106700     AND MID-IDDISTR-AUS-IN   = ALL '+'                                   
106800     AND MID-IDDISTR-CHN-IN   = ALL '+'                                   
106900     AND MID-IDDISTR-KOR-IN   = ALL '+'                                   
107000        CONTINUE                                                          
107100     ELSE                                                                 
107200        PERFORM FEA-BEHANDLA-BYTNOT                                       
107300        IF MID-BETFLEV = ALL '+'                                          
107400           CONTINUE                                                       
107500        ELSE                                                              
107600           MOVE MID-BETFLEV          TO BYART-BETFLEV                     
107700           MOVE MFS-FORMATETS-ATTR   TO MOD-BETFLEV-ATTR                  
107800        END-IF                                                            
107900        IF MID-IDDISTR-RENOV-IN = ALL '+'                                 
108000           CONTINUE                                                       
108100        ELSE                                                              
108200           MOVE MID-IDDISTR-RENOV-IN  TO IDDISTR-RENOV-WS                 
108300           MOVE WS-IDDISTR-RENOV      TO BYART-IDDISTR-RENOV              
108400           MOVE MFS-FORMATETS-ATTR TO MOD-IDDISTR-RENOV-IN-ATTR           
108500        END-IF                                                            
108600        IF MID-IDDISTR-NDC-IN = ALL '+'                                   
108700           CONTINUE                                                       
108800        ELSE                                                              
108900           MOVE MID-IDDISTR-NDC-IN    TO IDDISTR-NDC-WS                   
109000           MOVE WS-IDDISTR-NDC        TO BYART-IDDISTR-RENOV-NDC          
109100           MOVE MFS-FORMATETS-ATTR TO MOD-IDDISTR-NDC-IN-ATTR             
109200        END-IF                                                            
109300        IF MID-IDDISTR-PAC-IN = ALL '+'                                   
109400           CONTINUE                                                       
109500        ELSE                                                              
109600           MOVE MID-IDDISTR-PAC-IN    TO IDDISTR-PAC-WS                   
109700           MOVE WS-IDDISTR-PAC        TO BYART-IDDISTR-RENOV-PAC          
109800           MOVE MFS-FORMATETS-ATTR TO MOD-IDDISTR-PAC-IN-ATTR             
109900        END-IF                                                            
110000                                                                          
110100        IF MID-IDDISTR-CAN-IN = ALL '+'                                   
110200           CONTINUE                                                       
110300        ELSE                                                              
110400           MOVE MID-IDDISTR-CAN-IN    TO IDDISTR-CAN-WS                   
110500           MOVE WS-IDDISTR-CAN        TO BYART-IDDISTR-RENOV-CAN          
110600           MOVE MFS-FORMATETS-ATTR TO MOD-IDDISTR-CAN-IN-ATTR             
110700        END-IF                                                            
110800                                                                          
110900        IF MID-IDDISTR-AUS-IN = ALL '+'                                   
111000           CONTINUE                                                       
111100        ELSE                                                              
111200           MOVE MID-IDDISTR-AUS-IN    TO IDDISTR-AUS-WS                   
111300           MOVE WS-IDDISTR-AUS        TO BYART-IDDISTR-RENOV-AUS          
111400           MOVE MFS-FORMATETS-ATTR TO MOD-IDDISTR-AUS-IN-ATTR             
111500        END-IF                                                            
111600                                                                          
111700        IF MID-IDDISTR-CHN-IN = ALL '+'                                   
111800           CONTINUE                                                       
111900        ELSE                                                              
112000           MOVE MID-IDDISTR-CHN-IN    TO IDDISTR-CHN-WS                   
112100           MOVE WS-IDDISTR-CHN        TO BYART-IDDISTR-RENOV-CHN          
112200           MOVE MFS-FORMATETS-ATTR TO MOD-IDDISTR-CHN-IN-ATTR             
112300        END-IF                                                            
112400                                                                          
112500        IF MID-IDDISTR-KOR-IN = ALL '+'                                   
112600           CONTINUE                                                       
112700        ELSE                                                              
112800           MOVE MID-IDDISTR-KOR-IN    TO IDDISTR-KOR-WS                   
112900           MOVE WS-IDDISTR-KOR        TO BYART-IDDISTR-RENOV-KOR          
113000           MOVE MFS-FORMATETS-ATTR TO MOD-IDDISTR-KOR-IN-ATTR             
113100        END-IF                                                            
113200                                                                          
113300        PERFORM S01-FLYTT-BYART-T-BILD                                    
113400        PERFORM DB2-UPDATE-BYART                                          
113500     END-IF                                                               
113600     .                                                                    
113700     EJECT                                                                
113800 FEA-BEHANDLA-BYTNOT  SECTION.                                            
113900     SKIP3                                                                
114000     MOVE                                  +1 TO RAD-INDX                 
114100     PERFORM UNTIL RAD-INDX > 4                                           
114200        IF MID-TEBYTNOT (RAD-INDX) = ALL '+'                              
114300           CONTINUE                                                       
114400        ELSE                                                              
114500           IF RAD-INDX = 1                                                
114600              MOVE MID-TEBYTNOT(RAD-INDX)  TO BYART-TEBYTNOT1             
114700              MOVE MFS-FORMATETS-ATTR    TO MOD-TEBYTNOT-ATTR(1)          
114800           ELSE                                                           
114900              IF RAD-INDX = 2                                             
115000                 MOVE MID-TEBYTNOT(RAD-INDX)  TO                          
115100                                              BYART-TEBYTNOT2             
115200                 MOVE MFS-FORMATETS-ATTR  TO MOD-TEBYTNOT-ATTR(2)         
115300              ELSE                                                        
115400                 IF RAD-INDX = 3                                          
115500                    MOVE MID-TEBYTNOT(RAD-INDX)  TO                       
115600                                              BYART-TEBYTNOT3             
115700                    MOVE MFS-FORMATETS-ATTR  TO                           
115800                                         MOD-TEBYTNOT-ATTR(3)             
115900                 ELSE                                                     
116000                    MOVE MID-TEBYTNOT(RAD-INDX)  TO                       
116100                                              BYART-TEBYTNOT4             
116200                    MOVE MFS-FORMATETS-ATTR  TO                           
116300                                         MOD-TEBYTNOT-ATTR(4)             
116400                 END-IF                                                   
116500              END-IF                                                      
116600           END-IF                                                         
116700        END-IF                                                            
116800        ADD +1                                TO RAD-INDX                 
116900     END-PERFORM                                                          
117000     .                                                                    
117100     EJECT                                                                
117200 FF-SKAPA-BYART   SECTION.                                                
117300     SKIP2                                                                
117400     INITIALIZE BYART-BYART                                               
117500     IF MID-TEBYTNOT(1) = ALL '+'                                         
117600        CONTINUE                                                          
117700     ELSE                                                                 
117800        MOVE MID-TEBYTNOT(1)              TO BYART-TEBYTNOT1              
117900        MOVE MFS-FORMATETS-ATTR           TO MOD-TEBYTNOT-ATTR(1)         
118000     END-IF                                                               
118100     IF MID-TEBYTNOT(2) = ALL '+'                                         
118200        CONTINUE                                                          
118300     ELSE                                                                 
118400        MOVE MID-TEBYTNOT(2)              TO BYART-TEBYTNOT2              
118500        MOVE MFS-FORMATETS-ATTR           TO MOD-TEBYTNOT-ATTR(2)         
118600     END-IF                                                               
118700     IF MID-TEBYTNOT(3) = ALL '+'                                         
118800        CONTINUE                                                          
118900     ELSE                                                                 
119000        MOVE MID-TEBYTNOT(3)              TO BYART-TEBYTNOT3              
119100        MOVE MFS-FORMATETS-ATTR           TO MOD-TEBYTNOT-ATTR(3)         
119200     END-IF                                                               
119300     IF MID-TEBYTNOT(4) = ALL '+'                                         
119400        CONTINUE                                                          
119500     ELSE                                                                 
119600        MOVE MID-TEBYTNOT(4)              TO BYART-TEBYTNOT4              
119700        MOVE MFS-FORMATETS-ATTR           TO MOD-TEBYTNOT-ATTR(4)         
119800     END-IF                                                               
119900     IF MID-BETFLEV = ALL '+'                                             
120000        CONTINUE                                                          
120100     ELSE                                                                 
120200        MOVE MID-BETFLEV                  TO BYART-BETFLEV                
120300        MOVE MFS-FORMATETS-ATTR           TO MOD-BETFLEV-ATTR             
120400     END-IF                                                               
120500     IF MID-IDDISTR-RENOV-IN = ALL '+'                                    
120600        CONTINUE                                                          
120700     ELSE                                                                 
120800        MOVE MID-IDDISTR-RENOV-IN    TO IDDISTR-RENOV-WS                  
120900        MOVE  WS-IDDISTR-RENOV       TO BYART-IDDISTR-RENOV               
121000        MOVE MFS-FORMATETS-ATTR      TO MOD-IDDISTR-RENOV-IN-ATTR         
121100     END-IF                                                               
121200     IF MID-IDDISTR-NDC-IN = ALL '+'                                      
121300        CONTINUE                                                          
121400     ELSE                                                                 
121500        MOVE MID-IDDISTR-NDC-IN      TO IDDISTR-NDC-WS                    
121600        MOVE  WS-IDDISTR-NDC         TO BYART-IDDISTR-RENOV-NDC           
121700        MOVE MFS-FORMATETS-ATTR      TO MOD-IDDISTR-NDC-IN-ATTR           
121800     END-IF                                                               
121900                                                                          
122000     IF MID-IDDISTR-PAC-IN = ALL '+'                                      
122100        CONTINUE                                                          
122200     ELSE                                                                 
122300        MOVE MID-IDDISTR-PAC-IN      TO IDDISTR-PAC-WS                    
122400        MOVE  WS-IDDISTR-PAC         TO BYART-IDDISTR-RENOV-PAC           
122500        MOVE MFS-FORMATETS-ATTR      TO MOD-IDDISTR-PAC-IN-ATTR           
122600     END-IF                                                               
122700                                                                          
122800     IF MID-IDDISTR-CAN-IN = ALL '+'                                      
122900        CONTINUE                                                          
123000     ELSE                                                                 
123100        MOVE MID-IDDISTR-CAN-IN      TO IDDISTR-CAN-WS                    
123200        MOVE  WS-IDDISTR-CAN         TO BYART-IDDISTR-RENOV-CAN           
123300        MOVE MFS-FORMATETS-ATTR      TO MOD-IDDISTR-CAN-IN-ATTR           
123400     END-IF                                                               
123500                                                                          
123600     IF MID-IDDISTR-AUS-IN = ALL '+'                                      
123700        CONTINUE                                                          
123800     ELSE                                                                 
123900        MOVE MID-IDDISTR-AUS-IN      TO IDDISTR-AUS-WS                    
124000        MOVE  WS-IDDISTR-AUS         TO BYART-IDDISTR-RENOV-AUS           
124100        MOVE MFS-FORMATETS-ATTR      TO MOD-IDDISTR-AUS-IN-ATTR           
124200     END-IF                                                               
124300                                                                          
124400     IF MID-IDDISTR-CHN-IN = ALL '+'                                      
124500        CONTINUE                                                          
124600     ELSE                                                                 
124700        MOVE MID-IDDISTR-CHN-IN      TO IDDISTR-CHN-WS                    
124800        MOVE  WS-IDDISTR-CHN         TO BYART-IDDISTR-RENOV-CHN           
124900        MOVE MFS-FORMATETS-ATTR      TO MOD-IDDISTR-CHN-IN-ATTR           
125000     END-IF                                                               
125100                                                                          
125200     IF MID-IDDISTR-KOR-IN = ALL '+'                                      
125300        CONTINUE                                                          
125400     ELSE                                                                 
125500        MOVE MID-IDDISTR-KOR-IN      TO IDDISTR-KOR-WS                    
125600        MOVE  WS-IDDISTR-KOR         TO BYART-IDDISTR-RENOV-KOR           
125700        MOVE MFS-FORMATETS-ATTR      TO MOD-IDDISTR-KOR-IN-ATTR           
125800     END-IF                                                               
125900                                                                          
126000     PERFORM S01-FLYTT-BYART-T-BILD                                       
126100     MOVE ZERO TO BYART-KVLS-MAXCORE                                      
126200     PERFORM DB2-ISRT-BYART                                               
126300     .                                                                    
126400     EJECT                                                                
126500 FG-LAES-IN-IGEN SECTION.                                                 
126600     SKIP2                                                                
126700     MOVE 1                              TO RAD-INDX                      
126800     PERFORM UNTIL RAD-INDX > 4                                           
126900        IF MID-TEBYTNOT(RAD-INDX) = ALL '+'                               
127000           CONTINUE                                                       
127100        ELSE                                                              
127200           MOVE MFS-ADD-LAES-IN-FAELT  TO                                 
127300                                      MOD-TEBYTNOT-ATTR(RAD-INDX)         
127400        END-IF                                                            
127500        ADD +1                         TO RAD-INDX                        
127600     END-PERFORM                                                          
127700     IF MID-BETFLEV = ALL '+'                                             
127800        CONTINUE                                                          
127900     ELSE                                                                 
128000        MOVE MFS-ADD-LAES-IN-FAELT        TO MOD-BETFLEV-ATTR             
128100     END-IF                                                               
128200     IF MID-IDPRODNR-IN = ALL '+'                                         
128300        CONTINUE                                                          
128400     ELSE                                                                 
128500        MOVE MFS-ADD-LAES-IN-FAELT        TO MOD-IDPRODNR-IN-ATTR         
128600     END-IF                                                               
128700     IF MID-IDDISTR-RENOV-IN = ALL '+'                                    
128800        CONTINUE                                                          
128900     ELSE                                                                 
129000        MOVE MFS-ADD-LAES-IN-FAELT   TO MOD-IDDISTR-RENOV-IN-ATTR         
129100     END-IF                                                               
129200     IF MID-IDDISTR-NDC-IN = ALL '+'                                      
129300        CONTINUE                                                          
129400     ELSE                                                                 
129500        MOVE MFS-ADD-LAES-IN-FAELT   TO MOD-IDDISTR-NDC-IN-ATTR           
129600     END-IF                                                               
129700     IF MID-IDDISTR-PAC-IN = ALL '+'                                      
129800        CONTINUE                                                          
129900     ELSE                                                                 
130000        MOVE MFS-ADD-LAES-IN-FAELT   TO MOD-IDDISTR-PAC-IN-ATTR           
130100     END-IF                                                               
130200                                                                          
130300     IF MID-IDDISTR-CAN-IN = ALL '+'                                      
130400        CONTINUE                                                          
130500     ELSE                                                                 
130600        MOVE MFS-ADD-LAES-IN-FAELT   TO MOD-IDDISTR-CAN-IN-ATTR           
130700     END-IF                                                               
130800                                                                          
130900     IF MID-IDDISTR-AUS-IN = ALL '+'                                      
131000        CONTINUE                                                          
131100     ELSE                                                                 
131200        MOVE MFS-ADD-LAES-IN-FAELT   TO MOD-IDDISTR-AUS-IN-ATTR           
131300     END-IF                                                               
131400                                                                          
131500     IF MID-IDDISTR-CHN-IN = ALL '+'                                      
131600        CONTINUE                                                          
131700     ELSE                                                                 
131800        MOVE MFS-ADD-LAES-IN-FAELT   TO MOD-IDDISTR-CHN-IN-ATTR           
131900     END-IF                                                               
132000                                                                          
132100     IF MID-IDDISTR-KOR-IN = ALL '+'                                      
132200        CONTINUE                                                          
132300     ELSE                                                                 
132400        MOVE MFS-ADD-LAES-IN-FAELT   TO MOD-IDDISTR-KOR-IN-ATTR           
132500     END-IF                                                               
132600                                                                          
132700     IF MID-BELEV = ALL '+'                                               
132800        CONTINUE                                                          
132900     ELSE                                                                 
133000        MOVE MFS-ADD-LAES-IN-FAELT        TO MOD-BELEV-IN-ATTR            
133100     END-IF                                                               
133200     .                                                                    
133300     EJECT                                                                
133400 FH-EV-UPPD-BYTNOT-BETFLEV SECTION.                                       
133500     SKIP2                                                                
133600     MOVE 1                              TO RAD-INDX                      
133700     IF MID-BETFLEV = ALL '+'                                             
133800        PERFORM UNTIL RAD-INDX > 4                                        
133900           IF MID-TEBYTNOT(RAD-INDX) = ALL '+'                            
134000              CONTINUE                                                    
134100           ELSE                                                           
134200              PERFORM DB2-SELECT-BYART                                    
134300              IF RADER-FINNS                                              
134400                 PERFORM FHA-UPPD-BYTNOT-BETFLEV                          
134500              ELSE                                                        
134600                 IF 904-KOD                                               
134700                    MOVE FEL-8 (SPRAK-IX)    TO MOD-TEMFSFEL              
134800                    MOVE MED-4 (SPRAK-IX)    TO MOD-TEMFSINF              
134900                    ADD +4                   TO RAD-INDX                  
135000                 END-IF                                                   
135100              END-IF                                                      
135200           END-IF                                                         
135300           ADD +1                        TO RAD-INDX                      
135400        END-PERFORM                                                       
135500     ELSE                                                                 
135600        PERFORM DB2-SELECT-BYART                                          
135700        IF RADER-FINNS                                                    
135800           MOVE MID-BETFLEV           TO BYART-BETFLEV                    
135900           PERFORM FHA-UPPD-BYTNOT-BETFLEV                                
136000        ELSE                                                              
136100           IF 904-KOD                                                     
136200              MOVE FEL-8 (SPRAK-IX)    TO MOD-TEMFSFEL                    
136300              MOVE MED-4 (SPRAK-IX)    TO MOD-TEMFSINF                    
136400           END-IF                                                         
136500        END-IF                                                            
136600     END-IF                                                               
136700     .                                                                    
136800     EJECT                                                                
136900 FHA-UPPD-BYTNOT-BETFLEV SECTION.                                         
137000     SKIP2                                                                
137100     PERFORM UNTIL RAD-INDX > 4                                           
137200        IF MID-TEBYTNOT(RAD-INDX) = ALL '+'                               
137300           CONTINUE                                                       
137400        ELSE                                                              
137500           IF RAD-INDX = 1                                                
137600              MOVE MID-TEBYTNOT(RAD-INDX)  TO BYART-TEBYTNOT1             
137700              MOVE MFS-FORMATETS-ATTR    TO MOD-TEBYTNOT-ATTR(1)          
137800           ELSE                                                           
137900              IF RAD-INDX = 2                                             
138000                 MOVE MID-TEBYTNOT(RAD-INDX)  TO                          
138100                                              BYART-TEBYTNOT2             
138200                 MOVE MFS-FORMATETS-ATTR  TO MOD-TEBYTNOT-ATTR(2)         
138300              ELSE                                                        
138400                 IF RAD-INDX = 3                                          
138500                    MOVE MID-TEBYTNOT(RAD-INDX)  TO                       
138600                                              BYART-TEBYTNOT3             
138700                    MOVE MFS-FORMATETS-ATTR  TO                           
138800                                         MOD-TEBYTNOT-ATTR(3)             
138900                 ELSE                                                     
139000                    MOVE MID-TEBYTNOT(RAD-INDX)  TO                       
139100                                              BYART-TEBYTNOT4             
139200                    MOVE MFS-FORMATETS-ATTR  TO                           
139300                                         MOD-TEBYTNOT-ATTR(4)             
139400                 END-IF                                                   
139500              END-IF                                                      
139600           END-IF                                                         
139700        END-IF                                                            
139800        ADD +1                        TO RAD-INDX                         
139900     END-PERFORM                                                          
140000     PERFORM S01-FLYTT-BYART-T-BILD                                       
140100     PERFORM DB2-UPDATE-BYART                                             
140200     MOVE MED-1 (SPRAK-IX)            TO MOD-TEMFSINF                     
140300     .                                                                    
140400     EJECT                                                                
140500 G-KOLLA-ATT-INGET-IFYLLT  SECTION.                                       
140600     SKIP2                                                                
140700     MOVE NEJ                         TO SW-INMATAT                       
140800     MOVE +1                             TO RAD-INDX                      
140900     PERFORM UNTIL RAD-INDX > +4                                          
141000        IF MID-TEBYTNOT(RAD-INDX) = ALL '+'                               
141100           CONTINUE                                                       
141200        ELSE                                                              
141300           MOVE JA                     TO SW-INMATAT                      
141400           MOVE MFS-ADD-LAES-IN-FAELT  TO                                 
141500                                      MOD-TEBYTNOT-ATTR(RAD-INDX)         
141600        END-IF                                                            
141700        ADD +1                         TO RAD-INDX                        
141800     END-PERFORM                                                          
141900     IF MID-BETFLEV = ALL '+'                                             
142000        CONTINUE                                                          
142100     ELSE                                                                 
142200        MOVE JA                           TO SW-INMATAT                   
142300        MOVE MFS-ADD-LAES-IN-FAELT        TO MOD-BETFLEV-ATTR             
142400     END-IF                                                               
142500     IF MID-UPPDATERINGSSORT = ALL '+'                                    
142600        CONTINUE                                                          
142700     ELSE                                                                 
142800        MOVE JA                           TO SW-INMATAT                   
142900        MOVE MFS-ADD-LAES-IN-FAELT        TO                              
143000                                       MOD-UPPDATERINGSSORT-ATTR          
143100     END-IF                                                               
143200     IF MID-IDPRODNR-IN = ALL '+'                                         
143300        CONTINUE                                                          
143400     ELSE                                                                 
143500        MOVE JA                           TO SW-INMATAT                   
143600        MOVE MFS-ADD-LAES-IN-FAELT        TO MOD-IDPRODNR-IN-ATTR         
143700     END-IF                                                               
143800     IF MID-IDDISTR-RENOV-IN = ALL '+'                                    
143900        CONTINUE                                                          
144000     ELSE                                                                 
144100        MOVE JA                      TO SW-INMATAT                        
144200        MOVE MFS-ADD-LAES-IN-FAELT   TO MOD-IDDISTR-RENOV-IN-ATTR         
144300     END-IF                                                               
144400     IF MID-IDDISTR-NDC-IN = ALL '+'                                      
144500        CONTINUE                                                          
144600     ELSE                                                                 
144700        MOVE JA                      TO SW-INMATAT                        
144800        MOVE MFS-ADD-LAES-IN-FAELT   TO MOD-IDDISTR-NDC-IN-ATTR           
144900     END-IF                                                               
145000     IF MID-IDDISTR-PAC-IN = ALL '+'                                      
145100        CONTINUE                                                          
145200     ELSE                                                                 
145300        MOVE JA                      TO SW-INMATAT                        
145400        MOVE MFS-ADD-LAES-IN-FAELT   TO MOD-IDDISTR-PAC-IN-ATTR           
145500     END-IF                                                               
145600                                                                          
145700     IF MID-IDDISTR-CAN-IN = ALL '+'                                      
145800        CONTINUE                                                          
145900     ELSE                                                                 
146000        MOVE JA                      TO SW-INMATAT                        
146100        MOVE MFS-ADD-LAES-IN-FAELT   TO MOD-IDDISTR-CAN-IN-ATTR           
146200     END-IF                                                               
146300                                                                          
146400     IF MID-IDDISTR-AUS-IN = ALL '+'                                      
146500        CONTINUE                                                          
146600     ELSE                                                                 
146700        MOVE JA                      TO SW-INMATAT                        
146800        MOVE MFS-ADD-LAES-IN-FAELT   TO MOD-IDDISTR-AUS-IN-ATTR           
146900     END-IF                                                               
147000                                                                          
147100     IF MID-IDDISTR-CHN-IN = ALL '+'                                      
147200        CONTINUE                                                          
147300     ELSE                                                                 
147400        MOVE JA                      TO SW-INMATAT                        
147500        MOVE MFS-ADD-LAES-IN-FAELT   TO MOD-IDDISTR-CHN-IN-ATTR           
147600     END-IF                                                               
147700                                                                          
147800     IF MID-IDDISTR-KOR-IN = ALL '+'                                      
147900        CONTINUE                                                          
148000     ELSE                                                                 
148100        MOVE JA                      TO SW-INMATAT                        
148200        MOVE MFS-ADD-LAES-IN-FAELT   TO MOD-IDDISTR-KOR-IN-ATTR           
148300     END-IF                                                               
148400                                                                          
148500     IF MID-BELEV = ALL '+'                                               
148600        CONTINUE                                                          
148700     ELSE                                                                 
148800        MOVE JA                           TO SW-INMATAT                   
148900        MOVE MFS-ADD-LAES-IN-FAELT        TO MOD-BELEV-IN-ATTR            
149000     END-IF                                                               
149100     .                                                                    
149200     EJECT                                                                
149300 I-VISA-SIDAN SECTION.                                                    
149400     SKIP2                                                                
149500     IF PULS-TRANS                                                        
149600        PERFORM DB2-SELECT-BYART                                          
149700        IF RADER-FINNS                                                    
149800           PERFORM IMS-GU-BENA11                                          
149900           IF SEGMENT-FINNS                                               
150000              MOVE BENA11-TEXT-BEART TO                                   
150100                                       MOD-BEART-SVE                      
150200           END-IF                                                         
150300           PERFORM S01-FLYTT-BYART-T-BILD                                 
150400           PERFORM S02-FLYTT-BYPRO-T-BILD                                 
150500           PERFORM S03-FLYTT-BYLEV-T-BILD                                 
150600           PERFORM S04-RENSA-INMATAT                                      
150700        ELSE                                                              
150800           PERFORM S05-RENSA-SIDAN                                        
150900           IF 904-KOD                                                     
151000              MOVE FEL-8 (SPRAK-IX) TO MOD-TEMFSFEL                       
151100              MOVE MED-4 (SPRAK-IX) TO MOD-TEMFSINF                       
151200           ELSE                                                           
151300              MOVE FEL-3 (SPRAK-IX) TO MOD-TEMFSFEL                       
151400              MOVE SPACE            TO MOD-TEMFSINF                       
151500           END-IF                                                         
151600        END-IF                                                            
151700     ELSE                                                                 
151800*         (VOLVO-VISION-TRANS)                                            
151900        MOVE MID2-IDARTNR-NEXT      TO W-IDPRODNR                         
152000        PERFORM S02-FLYTT-BYPRO-T-BILD                                    
152100     END-IF                                                               
152200     .                                                                    
152300     EJECT                                                                
152400 S01-FLYTT-BYART-T-BILD SECTION.                                          
152500     SKIP2                                                                
152600     MOVE BYART-BETFLEV            TO MOD-BETFLEV                         
152700     MOVE BYART-TEBYTNOT1          TO MOD-TEBYTNOT (1)                    
152800     MOVE BYART-TEBYTNOT2          TO MOD-TEBYTNOT (2)                    
152900     MOVE BYART-TEBYTNOT3          TO MOD-TEBYTNOT (3)                    
153000     MOVE BYART-TEBYTNOT4          TO MOD-TEBYTNOT (4)                    
153100     MOVE BYART-IDDISTR-RENOV      TO MOD-IDDISTR-RENOV                   
153200     MOVE BYART-IDDISTR-RENOV-NDC  TO MOD-IDDISTR-NDC                     
153300     MOVE BYART-IDDISTR-RENOV-PAC  TO MOD-IDDISTR-PAC                     
153400     MOVE BYART-IDDISTR-RENOV-CAN  TO MOD-IDDISTR-CAN                     
153500     MOVE BYART-IDDISTR-RENOV-AUS  TO MOD-IDDISTR-AUS                     
153600     MOVE BYART-IDDISTR-RENOV-CHN  TO MOD-IDDISTR-CHN                     
153700     MOVE BYART-IDDISTR-RENOV-KOR  TO MOD-IDDISTR-KOR                     
153800     .                                                                    
153900     EJECT                                                                
154000 S02-FLYTT-BYPRO-T-BILD SECTION.                                          
154100     SKIP1                                                                
154200     PERFORM DB2-DCL-OPN-CRS-BYPRO                                        
154300     PERFORM DB2-FETCH-BYPRO                                              
154400     IF RADER-FINNS                                                       
154500        PERFORM S02A-FLYTTA-FRA-BYPRO                                     
154600        PERFORM DB2-CLOSE-BYPRO-CRS                                       
154700     ELSE                                                                 
154800        PERFORM S02B-RENSA-BYPRO                                          
154900     END-IF                                                               
155000     .                                                                    
155100     EJECT                                                                
155200 S02A-FLYTTA-FRA-BYPRO SECTION.                                           
155300     SKIP2                                                                
155400     IF PULS-TRANS                                                        
155500        MOVE BYPRO-IDARTNR            TO MOD-IDPRODNR-LO                  
155600        MOVE +1                       TO RAD-INDX                         
155700        PERFORM UNTIL NOT RADER-FINNS                                     
155800        OR RAD-INDX > +18                                                 
155900           MOVE BYPRO-IDARTNR         TO MOD-IDARTNR (RAD-INDX)           
156000           PERFORM DB2-FETCH-BYPRO                                        
156100           ADD +1                     TO RAD-INDX                         
156200        END-PERFORM                                                       
156300        IF RADER-FINNS                                                    
156400           IF MFS-UPDATE                                                  
156500              CONTINUE                                                    
156600           ELSE                                                           
156700              MOVE MED-2 (SPRAK-IX)   TO MOD-TEMFSINF                     
156800           END-IF                                                         
156900           MOVE BYPRO-IDARTNR         TO MOD-IDPRODNR-HI                  
157000        ELSE                                                              
157100           MOVE ZERO                  TO MOD-IDPRODNR-HI                  
157200           PERFORM UNTIL RAD-INDX > +18                                   
157300              MOVE MFS-RENSA-FAELT    TO MOD-IDARTNR (RAD-INDX)           
157400              ADD +1                  TO RAD-INDX                         
157500           END-PERFORM                                                    
157600        END-IF                                                            
157700     ELSE                                                                 
157800*         (VOLVO-VISION-TRANS)                                            
157900        MOVE +1                       TO RAD-INDX                         
158000        PERFORM UNTIL NOT RADER-FINNS                                     
158100        OR RAD-INDX > +13                                                 
158200           MOVE BYPRO-IDARTNR         TO MOD2-IDARTNR (RAD-INDX)          
158300           PERFORM DB2-FETCH-BYPRO                                        
158400           ADD +1                     TO RAD-INDX                         
158500        END-PERFORM                                                       
158600        SUBTRACT +1 FROM RAD-INDX GIVING MOD2-COUNTER                     
158700        IF RADER-FINNS                                                    
158800           MOVE BYPRO-IDARTNR         TO MOD2-IDARTNR-NEXT                
158900        ELSE                                                              
159000           MOVE ZERO                  TO MOD2-IDARTNR-NEXT                
159100           PERFORM UNTIL RAD-INDX > +13                                   
159200              MOVE ZERO               TO MOD2-IDARTNR (RAD-INDX)          
159300              ADD +1                  TO RAD-INDX                         
159400           END-PERFORM                                                    
159500        END-IF                                                            
159600     END-IF                                                               
159700     .                                                                    
159800     EJECT                                                                
159900 S02B-RENSA-BYPRO SECTION.                                                
160000     SKIP2                                                                
160100     IF PULS-TRANS                                                        
160200        MOVE ZERO                     TO MOD-IDPRODNR-LO                  
160300                                            MOD-IDPRODNR-HI               
160400        MOVE +1                       TO RAD-INDX                         
160500        PERFORM UNTIL RAD-INDX > +18                                      
160600           MOVE MFS-RENSA-FAELT       TO MOD-IDARTNR (RAD-INDX)           
160700           ADD +1                     TO RAD-INDX                         
160800        END-PERFORM                                                       
160900     ELSE                                                                 
161000*         (VOLVO-VISION-TRANS)                                            
161100        MOVE 'B10'                    TO MOD2-IDMFSFEL                    
161200        MOVE ZERO                     TO MOD2-IDARTNR-NEXT                
161300                                         MOD2-COUNTER                     
161400        MOVE +1                       TO RAD-INDX                         
161500        PERFORM UNTIL RAD-INDX > +13                                      
161600           MOVE ZERO                  TO MOD2-IDARTNR (RAD-INDX)          
161700           ADD +1                     TO RAD-INDX                         
161800        END-PERFORM                                                       
161900     END-IF                                                               
162000     .                                                                    
162100     EJECT                                                                
162200 S03-FLYTT-BYLEV-T-BILD SECTION.                                          
162300     SKIP1                                                                
162400     PERFORM DB2-DCL-OPN-CRS-BYLEV                                        
162500     PERFORM DB2-FETCH-BYLEV                                              
162600     IF RADER-FINNS                                                       
162700        PERFORM S03A-FLYTTA-FRA-BYLEV                                     
162800        PERFORM DB2-CLOSE-BYLEV-CRS                                       
162900     ELSE                                                                 
163000        PERFORM S03B-RENSA-BYLEV                                          
163100     END-IF                                                               
163200     .                                                                    
163300     EJECT                                                                
163400 S03A-FLYTTA-FRA-BYLEV SECTION.                                           
163500     SKIP2                                                                
163600     MOVE BYLEV-BELEV                 TO MOD-BELEV-LO                     
163700     MOVE +1                          TO RAD-INDX                         
163800     PERFORM UNTIL NOT RADER-FINNS                                        
163900     OR RAD-INDX > +6                                                     
164000        MOVE BYLEV-BELEV              TO MOD-BELEV (RAD-INDX)             
164100        PERFORM DB2-FETCH-BYLEV                                           
164200        ADD +1                        TO RAD-INDX                         
164300     END-PERFORM                                                          
164400     IF RADER-FINNS                                                       
164500        IF MFS-UPDATE                                                     
164600           CONTINUE                                                       
164700        ELSE                                                              
164800           MOVE MED-2 (SPRAK-IX)      TO MOD-TEMFSINF                     
164900        END-IF                                                            
165000        MOVE BYLEV-BELEV              TO MOD-BELEV-HI                     
165100     ELSE                                                                 
165200        MOVE SPACE                    TO MOD-BELEV-HI                     
165300        PERFORM UNTIL RAD-INDX > +6                                       
165400           MOVE MFS-RENSA-FAELT       TO MOD-BELEV (RAD-INDX)             
165500           ADD +1                     TO RAD-INDX                         
165600        END-PERFORM                                                       
165700     END-IF                                                               
165800     .                                                                    
165900     EJECT                                                                
166000 S03B-RENSA-BYLEV SECTION.                                                
166100     SKIP2                                                                
166200     MOVE SPACE                       TO MOD-BELEV-LO                     
166300                                         MOD-BELEV-HI                     
166400     MOVE +1                          TO RAD-INDX                         
166500     PERFORM UNTIL RAD-INDX > +6                                          
166600        MOVE MFS-RENSA-FAELT          TO MOD-BELEV (RAD-INDX)             
166700        ADD +1                        TO RAD-INDX                         
166800     END-PERFORM                                                          
166900     .                                                                    
167000     EJECT                                                                
167100 S04-RENSA-INMATAT SECTION.                                               
167200     SKIP2                                                                
167300     MOVE MFS-RENSA-FAELT          TO                                     
167400                                      MOD-UPPDATERINGSSORT                
167500                                      MOD-IDPRODNR-IN                     
167600                                      MOD-IDDISTR-RENOV-IN                
167700                                      MOD-IDDISTR-NDC-IN                  
167800                                      MOD-IDDISTR-PAC-IN                  
167900                                      MOD-IDDISTR-CAN-IN                  
168000                                      MOD-IDDISTR-AUS-IN                  
168100                                      MOD-IDDISTR-CHN-IN                  
168200                                      MOD-IDDISTR-KOR-IN                  
168300                                      MOD-BELEV-IN                        
168400     .                                                                    
168500     EJECT                                                                
168600 S05-RENSA-SIDAN SECTION.                                                 
168700     SKIP2                                                                
168800     MOVE MFS-RENSA-FAELT            TO                                   
168900                                        MOD-BEART-SVE                     
169000                                        MOD-BETFLEV                       
169100                                        MOD-UPPDATERINGSSORT              
169200                                        MOD-IDPRODNR-IN                   
169300                                        MOD-IDDISTR-RENOV-IN              
169400                                        MOD-IDDISTR-NDC-IN                
169500                                        MOD-IDDISTR-PAC-IN                
169600                                        MOD-IDDISTR-CAN-IN                
169700                                        MOD-IDDISTR-AUS-IN                
169800                                        MOD-IDDISTR-CHN-IN                
169900                                        MOD-IDDISTR-KOR-IN                
170000                                        MOD-BELEV-IN                      
170100     MOVE +1                         TO RAD-INDX                          
170200     PERFORM UNTIL RAD-INDX > +3                                          
170300        MOVE MFS-RENSA-FAELT         TO MOD-TEBYTNOT (RAD-INDX)           
170400        ADD +1                       TO RAD-INDX                          
170500     END-PERFORM                                                          
170600     MOVE MFS-RENSA-FAELT         TO MOD-IDDISTR-RENOV                    
170700     MOVE MFS-RENSA-FAELT         TO MOD-IDDISTR-NDC                      
170800     MOVE MFS-RENSA-FAELT         TO MOD-IDDISTR-PAC                      
170900     MOVE MFS-RENSA-FAELT         TO MOD-IDDISTR-CAN                      
171000     MOVE MFS-RENSA-FAELT         TO MOD-IDDISTR-AUS                      
171100     MOVE MFS-RENSA-FAELT         TO MOD-IDDISTR-CHN                      
171200     MOVE MFS-RENSA-FAELT         TO MOD-IDDISTR-KOR                      
171300     MOVE MFS-RENSA-FAELT            TO MOD-TEBYTNOT (4)                  
171400     MOVE +1                         TO RAD-INDX                          
171500     PERFORM UNTIL RAD-INDX > +18                                         
171600        MOVE MFS-RENSA-FAELT         TO MOD-IDARTNR (RAD-INDX)            
171700        ADD +1                       TO RAD-INDX                          
171800     END-PERFORM                                                          
171900     MOVE +1                         TO RAD-INDX                          
172000     PERFORM UNTIL RAD-INDX > +6                                          
172100        MOVE MFS-RENSA-FAELT         TO MOD-BELEV   (RAD-INDX)            
172200        ADD +1                       TO RAD-INDX                          
172300     END-PERFORM                                                          
172400     .                                                                    
172500     EJECT                                                                
172600* IMS SEKTIONER                                                           
172700     SKIP3                                                                
172800 IMS-GET-MSG SECTION.                                                     
172900     SKIP1                                                                
173000     MOVE '  QC' TO GODK-STATUSKODER                                      
173100     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
173200     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
173300     PERFORM IMS-STATUSKONTROLL                                           
173400     SKIP3                                                                
173500     .                                                                    
173600 IMS-INSERT-MSG SECTION.                                                  
173700     SKIP1                                                                
173800*     IF MSGI-IDLAND-SPR NOT = 'GB'                                       
173900*      MOVE '0' TO MFS-KDHUVOMR                                           
174000*    END-IF                                                               
174100     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
174200     MOVE SPACE TO GODK-STATUSKODER                                       
174300     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
174400     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
174500     PERFORM IMS-STATUSKONTROLL                                           
174600     EJECT                                                                
174700     .                                                                    
174800 IMS-GU-ARTC01 SECTION.                                                   
174900     SKIP1                                                                
175000     STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-X ')'                         
175100            DELIMITED BY SIZE INTO SSA1                                   
175200     MOVE '  GE' TO GODK-STATUSKODER                                      
175300     CALL CBLTDLI USING GU ARTC-PCB DLI-IO-AREA SSA1                      
175400     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
175500     PERFORM IMS-STATUSKONTROLL                                           
175600     SKIP3                                                                
175700     .                                                                    
175800 IMS-GU-BENA11 SECTION.                                                   
175900     SKIP1                                                                
176000     STRING 'WLBENA01(WDD3BSEQ =' W-IDARTNR-X ')'                         
176100            DELIMITED BY SIZE INTO SSA1                                   
176200     STRING 'WLBENA11(IDSKYLT  =' W-IDSKYLT-X ')'                         
176300            DELIMITED BY SIZE INTO SSA2                                   
176400     MOVE '  GE' TO GODK-STATUSKODER                                      
176500     CALL CBLTDLI USING GU BENA-PCB DLI-IO-AREA SSA1 SSA2                 
176600     MOVE BENA-STATUS-CODE TO STATUS-WS                                   
176700     PERFORM IMS-STATUSKONTROLL                                           
176800     SKIP3                                                                
176900     .                                                                    
177000 IMS-STATUSKONTROLL SECTION.                                              
177100     SKIP1                                                                
177200     SET STATUS-IX TO 1                                                   
177300     SEARCH GODK-STATUS                                                   
177400       AT END                                                             
177500         CALL FELLOG                                                      
177600        WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                          
177700           CONTINUE                                                       
177800     END-SEARCH                                                           
177900     .                                                                    
178000     EJECT                                                                
178100 DB2-DCL-OPN-CRS-BYPRO SECTION.                                           
178200* OBS!!! DECLARE GER INGEN SQLCODE I RETUR                                
178300     EXEC SQL DECLARE BYPRO-CRS CURSOR FOR                                
178400              SELECT IDARTNR                                              
178500              FROM BYPRO                                                  
178600              WHERE IDARTNR >= :W-IDPRODNR                                
178700              AND IDARTNR_BYT = :W-IDARTNR-BYT                            
178800              ORDER BY IDARTNR                                            
178900     END-EXEC                                                             
179000     MOVE 000               TO GODK-SQLCODESKODER                         
179100     EXEC SQL OPEN BYPRO-CRS END-EXEC                                     
179200     MOVE SQLCODE           TO SQLCODE-WS                                 
179300     PERFORM DB2-STATUSKONTROLL                                           
179400     .                                                                    
179500     EJECT                                                                
179600 DB2-DCL-OPN-CRS-BYLEV SECTION.                                           
179700     EXEC SQL DECLARE BYLEV-CRS CURSOR FOR                                
179800              SELECT BELEV                                                
179900              FROM BYLEV                                                  
180000              WHERE BELEV >= :W-BELEV                                     
180100              AND IDARTNR_BYT = :W-IDARTNR-BYT                            
180200              ORDER BY BELEV                                              
180300     END-EXEC                                                             
180400     MOVE 000               TO GODK-SQLCODESKODER                         
180500     EXEC SQL OPEN BYLEV-CRS END-EXEC                                     
180600     MOVE SQLCODE           TO SQLCODE-WS                                 
180700     PERFORM DB2-STATUSKONTROLL                                           
180800     .                                                                    
180900     EJECT                                                                
181000 DB2-FETCH-BYPRO SECTION.                                                 
181100     MOVE 000100            TO GODK-SQLCODESKODER                         
181200     EXEC SQL FETCH BYPRO-CRS INTO                                        
181300            :BYPRO-IDARTNR                                                
181400     END-EXEC                                                             
181500     MOVE SQLCODE           TO SQLCODE-WS                                 
181600     PERFORM DB2-STATUSKONTROLL                                           
181700     .                                                                    
181800 DB2-FETCH-BYLEV SECTION.                                                 
181900     MOVE 000100            TO GODK-SQLCODESKODER                         
182000     EXEC SQL FETCH BYLEV-CRS INTO                                        
182100            :BYLEV-BELEV                                                  
182200     END-EXEC                                                             
182300     MOVE SQLCODE           TO SQLCODE-WS                                 
182400     PERFORM DB2-STATUSKONTROLL                                           
182500     .                                                                    
182600     EJECT                                                                
182700 DB2-SELECT-BYART SECTION.                                                
182800     MOVE 000100904         TO GODK-SQLCODESKODER                         
182900     EXEC SQL SELECT                                                      
183000                  IDARTNR_BYT,                                            
183100                  BETFLEV, TEBYTNOT1,                                     
183200                  TEBYTNOT2, TEBYTNOT3,                                   
183300                  TEBYTNOT4,                                              
183400                  IDDISTR_RENOV,                                          
183500                  IDDISTR_RENOV_NDC,                                      
183600                  IDDISTR_RENOV_PAC,                                      
183700                  IDDISTR_RENOV_CAN,                                      
183800                  IDDISTR_RENOV_AUS,                                      
183900                  IDDISTR_RENOV_CHN,                                      
184000                  IDDISTR_RENOV_KOR                                       
184100              INTO                                                        
184200                  :BYART-IDARTNR-BYT,                                     
184300                  :BYART-BETFLEV, :BYART-TEBYTNOT1,                       
184400                  :BYART-TEBYTNOT2, :BYART-TEBYTNOT3,                     
184500                  :BYART-TEBYTNOT4,                                       
184600                  :BYART-IDDISTR-RENOV,                                   
184700                  :BYART-IDDISTR-RENOV-NDC,                               
184800                  :BYART-IDDISTR-RENOV-PAC,                               
184900                  :BYART-IDDISTR-RENOV-CAN,                               
185000                  :BYART-IDDISTR-RENOV-AUS,                               
185100                  :BYART-IDDISTR-RENOV-CHN,                               
185200                  :BYART-IDDISTR-RENOV-KOR                                
185300            FROM BYART                                                    
185400            WHERE IDARTNR_BYT = :W-IDARTNR-BYT                            
185500     END-EXEC                                                             
185600     MOVE SQLCODE           TO SQLCODE-WS                                 
185700     PERFORM DB2-STATUSKONTROLL                                           
185800     .                                                                    
185900 DB2-SELECT-BYLEV SECTION.                                                
186000     MOVE 000100904         TO GODK-SQLCODESKODER                         
186100     EXEC SQL SELECT                                                      
186200                   BELEV,                                                 
186300                   IDARTNR_BYT                                            
186400              INTO                                                        
186500                   :BYLEV-BELEV,                                          
186600                   :BYLEV-IDARTNR-BYT                                     
186700              FROM BYLEV                                                  
186800              WHERE BELEV = :W-BELEV                                      
186900     END-EXEC                                                             
187000     MOVE SQLCODE           TO SQLCODE-WS                                 
187100     PERFORM DB2-STATUSKONTROLL                                           
187200     .                                                                    
187300     EJECT                                                                
187400 DB2-DLET-BYPRO    SECTION.                                               
187500     SKIP2                                                                
187600     MOVE 000100904         TO GODK-SQLCODESKODER                         
187700     EXEC SQL DELETE FROM BYPRO                                           
187800        WHERE IDARTNR_BYT = :W-IDARTNR-BYT                                
187900        AND   IDARTNR     = :W-IDPRODNR                                   
188000     END-EXEC                                                             
188100     MOVE SQLCODE           TO SQLCODE-WS                                 
188200     PERFORM DB2-STATUSKONTROLL                                           
188300     .                                                                    
188400     EJECT                                                                
188500 DB2-DLET-BYLEV    SECTION.                                               
188600     SKIP2                                                                
188700     MOVE 000803904         TO GODK-SQLCODESKODER                         
188800     EXEC SQL DELETE FROM BYLEV                                           
188900        WHERE BELEV = :W-BELEV                                            
189000        AND   IDARTNR_BYT = :W-IDARTNR-BYT                                
189100     END-EXEC                                                             
189200     .                                                                    
189300     EJECT                                                                
189400 DB2-UPDATE-BYART    SECTION.                                             
189500     SKIP2                                                                
189600     MOVE 000               TO GODK-SQLCODESKODER                         
189700     EXEC SQL UPDATE BYART                                                
189800        SET BETFLEV     = :BYART-BETFLEV,                                 
189900            TEBYTNOT1   = :BYART-TEBYTNOT1,                               
190000            TEBYTNOT2   = :BYART-TEBYTNOT2,                               
190100            TEBYTNOT3   = :BYART-TEBYTNOT3,                               
190200            TEBYTNOT4   = :BYART-TEBYTNOT4,                               
190300            IDDISTR_RENOV  = :BYART-IDDISTR-RENOV,                        
190400            IDDISTR_RENOV_NDC  = :BYART-IDDISTR-RENOV-NDC,                
190500            IDDISTR_RENOV_PAC  = :BYART-IDDISTR-RENOV-PAC,                
190600            IDDISTR_RENOV_CAN  = :BYART-IDDISTR-RENOV-CAN,                
190700            IDDISTR_RENOV_AUS  = :BYART-IDDISTR-RENOV-AUS,                
190800            IDDISTR_RENOV_CHN  = :BYART-IDDISTR-RENOV-CHN,                
190900            IDDISTR_RENOV_KOR  = :BYART-IDDISTR-RENOV-KOR                 
191000     WHERE IDARTNR_BYT  = :W-IDARTNR-BYT                                  
191100     END-EXEC                                                             
191200     MOVE SQLCODE           TO SQLCODE-WS                                 
191300     PERFORM DB2-STATUSKONTROLL                                           
191400     .                                                                    
191500     EJECT                                                                
191600 DB2-DELETE-BYART SECTION.                                                
191700     SKIP2                                                                
191800     MOVE 000904            TO GODK-SQLCODESKODER                         
191900     EXEC SQL DELETE FROM BYART                                           
192000        WHERE IDARTNR_BYT = :W-IDARTNR-BYT                                
192100     END-EXEC                                                             
192200     MOVE SQLCODE           TO SQLCODE-WS                                 
192300     PERFORM DB2-STATUSKONTROLL                                           
192400     .                                                                    
192500     EJECT                                                                
192600 DB2-ISRT-BYART    SECTION.                                               
192700     MOVE 000               TO GODK-SQLCODESKODER                         
192800     SKIP2                                                                
192900     EXEC SQL INSERT INTO BYART                                           
193000           (IDARTNR_BYT,                                                  
193100            BETFLEV,                                                      
193200            TEBYTNOT1,                                                    
193300            TEBYTNOT2,                                                    
193400            TEBYTNOT3,                                                    
193500            TEBYTNOT4,                                                    
193600            IDDISTR_RENOV,                                                
193700            ADLAGOMR,                                                     
193800            ADGANG,                                                       
193900            ADPLATS,                                                      
194000            ADBYTOMR,                                                     
194100            ADBYTGANG,                                                    
194200            ADBYTPL,                                                      
194300            FLBYTKTL,                                                     
194400            KVBYTPKO,                                                     
194500            TEBYTKVA1,                                                    
194600            TEBYTKVA2,                                                    
194700            IDDISTR_RENOV_NDC,                                            
194800            IDDISTR_RENOV_PAC,                                            
194900            IDDISTR_RENOV_CAN,                                            
195000            IDDISTR_RENOV_AUS,                                            
195100            KVLS_MAXCORE,                                                 
195200            IDDISTR_RENOV_CHN,                                            
195300            TEBYTKVA3,                                                    
195400            TEBYTKVA4,                                                    
195500            IDDISTR_RENOV_KOR,                                            
195510            IDDISTR_RENOV_MY,                                             
195520            IDDISTR_RENOV_TW,                                             
195530            IDDISTR_RENOV_TH)                                             
195600        VALUES (:W-IDARTNR-BYT,                                           
195700           :BYART-BETFLEV,                                                
195800           :BYART-TEBYTNOT1,                                              
195900           :BYART-TEBYTNOT2,                                              
196000           :BYART-TEBYTNOT3,                                              
196100           :BYART-TEBYTNOT4,                                              
196200           :BYART-IDDISTR-RENOV,                                          
196300           :BYART-ADLAGOMR,                                               
196400           :BYART-ADGANG,                                                 
196500           :BYART-ADPLATS,                                                
196600           :BYART-ADBYTOMR,                                               
196700           :BYART-ADBYTGANG,                                              
196800           :BYART-ADBYTPL,                                                
196900           :BYART-FLBYTKTL,                                               
197000           :BYART-KVBYTPKO,                                               
197100           :BYART-TEBYTKVA1,                                              
197200           :BYART-TEBYTKVA2,                                              
197300           :BYART-IDDISTR-RENOV-NDC,                                      
197400           :BYART-IDDISTR-RENOV-PAC,                                      
197500           :BYART-IDDISTR-RENOV-CAN,                                      
197600           :BYART-IDDISTR-RENOV-AUS,                                      
197700           :BYART-KVLS-MAXCORE,                                           
197800           :BYART-IDDISTR-RENOV-CHN,                                      
197900           :BYART-TEBYTKVA3,                                              
198000           :BYART-TEBYTKVA4,                                              
198100           :BYART-IDDISTR-RENOV-KOR,                                      
198110           :BYART-IDDISTR-RENOV-MY,                                       
198120           :BYART-IDDISTR-RENOV-TW,                                       
198130           :BYART-IDDISTR-RENOV-TH)                                       
198200     END-EXEC                                                             
198300     MOVE SQLCODE           TO SQLCODE-WS                                 
198400     PERFORM DB2-STATUSKONTROLL                                           
198500     .                                                                    
198600     EJECT                                                                
198700 DB2-ISRT-BYPRO    SECTION.                                               
198800     SKIP2                                                                
198900     MOVE 000803            TO GODK-SQLCODESKODER                         
199000     EXEC SQL INSERT INTO BYPRO (IDARTNR_BYT, IDARTNR)                    
199100        VALUES (:W-IDARTNR-BYT, :W-IDPRODNR)                              
199200     END-EXEC                                                             
199300     MOVE SQLCODE           TO SQLCODE-WS                                 
199400     PERFORM DB2-STATUSKONTROLL                                           
199500     .                                                                    
199600     EJECT                                                                
199700 DB2-ISRT-BYLEV    SECTION.                                               
199800     SKIP2                                                                
199900     MOVE 000803            TO GODK-SQLCODESKODER                         
200000     EXEC SQL INSERT INTO BYLEV (BELEV, IDARTNR_BYT)                      
200100        VALUES (:W-BELEV, :W-IDARTNR-BYT)                                 
200200     END-EXEC                                                             
200300     MOVE SQLCODE           TO SQLCODE-WS                                 
200400     PERFORM DB2-STATUSKONTROLL                                           
200500     .                                                                    
200600     EJECT                                                                
200700 DB2-CLOSE-BYLEV-CRS SECTION.                                             
200800     SKIP2                                                                
200900     EXEC SQL CLOSE BYLEV-CRS END-EXEC                                    
201000     .                                                                    
201100     EJECT                                                                
201200 DB2-CLOSE-BYPRO-CRS SECTION.                                             
201300     SKIP2                                                                
201400     EXEC SQL CLOSE BYPRO-CRS END-EXEC                                    
201500     .                                                                    
201600     EJECT                                                                
201700 DB2-STATUSKONTROLL SECTION.                                              
201800     SKIP2                                                                
201900     SET SQLCODE-IX          TO 1                                         
202000     SEARCH GODK-SQLCODE                                                  
202100       AT END                                                             
202200         CALL FELLOG                                                      
202300        WHEN GODK-SQLCODE (SQLCODE-IX) = SQLCODE-WS                       
202400           CONTINUE                                                       
202500     END-SEARCH                                                           
202600     .                                                                    
