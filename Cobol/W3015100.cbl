000100 PROCESS DYNAM                                                            
000200 ID DIVISION.                                                             
000300 PROGRAM-ID.     W3015100.                                                
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
001500*    FRÅN PULS:         TRANSAKTION W3T151                                
001510*    FROM TCPLM                     W3T151X                               
001520*                                   W0T693X                               
001600*                       MID         W3I15101                              
001700*                       MOD         W3O15101                              
001800*                                                                         
001900*    FRÅN VOLVO VISION: TRANSAKTION W30151T                               
002000*                       MID         W3I151V1                              
002100*                       MOD         W3O151V1                              
002200*                                                                         
002300*    CHANGE LOG:                                                          
002400*                                                                         
002500*      YY/MM/DD - NAME            - CHANGE DESCRIPTION                    
002600*      ----------------------------------------------------------         
002700*      14/11/04 - REDDY RAHUL     - CHANGES FOR CHINA EXCHANGE            
002800*                                   E'TRACKER 10242148                    
002900*                                                                         
003000 ENVIRONMENT DIVISION.                                                    
003100     SKIP2                                                                
003200 DATA DIVISION.                                                           
003300     EJECT                                                                
003400 WORKING-STORAGE SECTION.                                                 
003500                                                                          
003600*    -- CHECKED BY WY2000                                                 
003700 77  PROGRAM-NAMN                PIC X(8) VALUE 'W3015100'.               
003800 01  WS-IDARTNR                  PIC X(9) VALUE ZERO.                     
003900 01  FILLER REDEFINES WS-IDARTNR.                                         
004000   03  FILLER                    PIC 9(5).                                
004100   03  WS-ARTSIFFRA              PIC 9(1).                                
004200     88  ART-0                   VALUE 6.                                 
004300     88  ART-1                   VALUE 4  7.                              
004400     88  ART-2                   VALUE 5  8.                              
004500     88  ART-3                   VALUE 9.                                 
004600   03  FILLER                    PIC 9(5).                                
004700                                                                          
004800 77  FLAGGA-TRANS-TYP            PIC X       VALUE 'J'.                   
004900     88  PULS-TRANS                          VALUE 'J'.                   
005000     88  VOLVO-VISION-TRANS                  VALUE 'N'.                   
005100                                                                          
005200 77  W-IDPRODNR                  PIC S9(9) COMP-3 VALUE ZERO.             
005300 77  W-BELEV                     PIC X(30) VALUE SPACE.                   
005400 01  WS-IDDISTR-RENOV            PIC 9(5) VALUE ZERO.                     
005500 01  FILLER REDEFINES WS-IDDISTR-RENOV.                                   
005600    03  IDDISTR-RENOV-WS         PIC X(5).                                
005700                                                                          
005800 01  WS-IDDISTR-NDC              PIC 9(5) VALUE ZERO.                     
005900 01  FILLER REDEFINES WS-IDDISTR-NDC.                                     
006000    03  IDDISTR-NDC-WS           PIC X(5).                                
006100                                                                          
006200 01  WS-IDDISTR-PAC              PIC 9(5) VALUE ZERO.                     
006300 01  FILLER REDEFINES WS-IDDISTR-PAC.                                     
006400    03  IDDISTR-PAC-WS           PIC X(5).                                
006500                                                                          
006600 01  WS-IDDISTR-CAN              PIC 9(5) VALUE ZERO.                     
006700 01  FILLER REDEFINES WS-IDDISTR-CAN.                                     
006800    03  IDDISTR-CAN-WS           PIC X(5).                                
006900                                                                          
007000 01  WS-IDDISTR-AUS              PIC 9(5) VALUE ZERO.                     
007100 01  FILLER REDEFINES WS-IDDISTR-AUS.                                     
007200    03  IDDISTR-AUS-WS           PIC X(5).                                
007300                                                                          
007400 01  WS-IDDISTR-CHN              PIC 9(5) VALUE ZERO.                     
007500 01  FILLER REDEFINES WS-IDDISTR-CHN.                                     
007600    03  IDDISTR-CHN-WS           PIC X(5).                                
007700                                                                          
007800 01  WS-IDDISTR-KOR              PIC 9(5) VALUE ZERO.                     
007900 01  FILLER REDEFINES WS-IDDISTR-KOR.                                     
008000    03  IDDISTR-KOR-WS           PIC X(5).                                
008100                                                                          
008110 01  WS-IDDISTR-MY               PIC 9(5) VALUE ZERO.                     
008120 01  FILLER REDEFINES WS-IDDISTR-MY.                                      
008130    03  IDDISTR-MY-WS           PIC X(5).                                 
008131                                                                          
008140 01  WS-IDDISTR-TW               PIC 9(5) VALUE ZERO.                     
008150 01  FILLER REDEFINES WS-IDDISTR-TW.                                      
008160    03  IDDISTR-TW-WS           PIC X(5).                                 
008161                                                                          
008170 01  WS-IDDISTR-TH               PIC 9(5) VALUE ZERO.                     
008180 01  FILLER REDEFINES WS-IDDISTR-TH.                                      
008190    03  IDDISTR-TH-WS           PIC X(5).                                 
008191                                                                          
008200 01  WS-IDPRODNR                 PIC 9(8).                                
008300 01  FILLER REDEFINES WS-IDPRODNR.                                        
008400    03  IDPRODNR-WS              PIC X(8).                                
008500 77  JA                          PIC X       VALUE 'J'.                   
008600 77  NEJ                         PIC X       VALUE 'N'.                   
008700 77  RAD-INDX                    PIC S9(9)   VALUE +0   COMP SYNC.        
008800 77  MAX-RAD                     PIC S9(9)   VALUE +3   COMP SYNC.        
008900 77  SPRAK-IX                    PIC S9(9)   VALUE +2   COMP SYNC.        
009000 77  MAX-MOD-LAENGD              PIC S9(4)  VALUE +807  COMP SYNC.        
009100 77  W-IDARTNR-BYT               PIC S9(9) COMP-3 VALUE ZERO.             
009110 77  WS-ERROR-UPDX               PIC X(3)    VALUE SPACE.                 
009200 01  SW-NYCKLAR-OK               PIC X.                                   
009300   88  NYCKLAR-OK                          VALUE 'J'.                     
009400 01  SW-UPPDATERINGAR            PIC X.                                   
009500   88  INGA-UPPDATERINGAR                  VALUE 'N'.                     
009600 01  SW-INDATA-OK               PIC X.                                    
009700   88  INDATA-OK                          VALUE 'J'.                      
009800 01  SW-INMATAT                 PIC X.                                    
009900   88  INGET-INMATAT                      VALUE 'N'.                      
010000 01  WS-IDTRANS                  PIC X(4).                                
010100   88  EGEN-BILD                           VALUE '3151'.                  
010200 01  FILLER                      PIC X(16)   VALUE                        
010300                                            'NYCKLAR-TILL-DLI'.           
010400 01  NYCKLAR-TILL-DLI.                                                    
010500   03  W-IDARTNR-X.                                                       
010600     05  W-IDARTNR               PIC S9(9) COMP-3 VALUE ZERO.             
010700   03  W-IDSKYLT-X               PIC X(3)         VALUE 'S  '.            
010800                                                                          
010900   03   W-IDDC-X.                                                         
011000     05 W-IDDC                   PIC X(2) VALUE '91'.                     
011100                                                                          
011200     EJECT                                                                
011300 01  DYNAMISKA-SUBPROGRAM.                                                
011400   03  WDATKONV                  PIC X(8)    VALUE 'WDATKONV'.            
011500   03  FELLOG                    PIC X(8)    VALUE 'FELLOG  '.            
011600   03  CBLTDLI                   PIC X(8)    VALUE 'CBLTDLI '.            
011700   03  W005INIT                  PIC X(8)    VALUE 'W005INIT'.            
011800*                     ****  PARAMETRAR TILL W005INIT                      
011900*01  -COPY WMSGINIT                                                       
012000     EJECT                                                                
012100 01  MEDDELANDE.                                                          
012200   03  FEL2.                                                              
012300     05 FILLER                   PIC X(40)                                
012400          VALUE 'BYTESARTIKELNR FEL '.                                    
012500     05 FILLER                   PIC X(40)                                
012600          VALUE 'EXCH. PART.NO WRONG'.                                    
012700   03  FILLER REDEFINES FEL2.                                             
012800     05  FEL-2                   PIC X(40)   OCCURS 2.                    
012900                                                                          
013000   03  FEL3.                                                              
013100     05 FILLER                   PIC X(40)                                
013200          VALUE 'BYTESARTIKELNR SAKNAS'.                                  
013300     05 FILLER                   PIC X(40)                                
013400          VALUE 'EXCH. PART.NO MISSING'.                                  
013500   03  FILLER REDEFINES FEL3.                                             
013600     05  FEL-3                   PIC X(40)   OCCURS 2.                    
013700                                                                          
013800   03  FEL4.                                                              
013900     05 FILLER                   PIC X(40)                                
014000          VALUE 'TRYCK PF11 VID UPPDATERING'.                             
014100     05 FILLER                   PIC X(40)                                
014200          VALUE 'PRESS PF11 WHEN UPDATE'.                                 
014300   03  FILLER REDEFINES FEL4.                                             
014400     05  FEL-4                   PIC X(40)   OCCURS 2.                    
014500                                                                          
014600   03  FEL5.                                                              
014700     05 FILLER                   PIC X(40)                                
014800          VALUE 'INDATA FEL'.                                             
014900     05 FILLER                   PIC X(40)                                
015000          VALUE 'WRONG FIELDS'.                                           
015100   03  FILLER REDEFINES FEL5.                                             
015200     05  FEL-5                   PIC X(40)   OCCURS 2.                    
015300                                                                          
015400   03  FEL6.                                                              
015500     05 FILLER                   PIC X(40)                                
015600          VALUE 'INGET BYTESARTIKELNR'.                                   
015700     05 FILLER                   PIC X(40)                                
015800          VALUE 'NOT AN EXCHANGE NO'.                                     
015900   03  FILLER REDEFINES FEL6.                                             
016000     05  FEL-6                   PIC X(40)   OCCURS 2.                    
016100                                                                          
016200   03  FEL7.                                                              
016300     05 FILLER                   PIC X(40)                                
016400          VALUE 'PRODUKTNR SAKNAS PÅ ARTIKELREGISTRET '.                  
016500     05 FILLER                   PIC X(40)                                
016600          VALUE 'MISSING ON ARTICLE REGISTER'.                            
016700   03  FILLER REDEFINES FEL7.                                             
016800     05  FEL-7                   PIC X(40)   OCCURS 2.                    
016900                                                                          
017000   03  FEL8.                                                              
017100     05 FILLER                   PIC X(40)                                
017200          VALUE 'DB2-TABELL OTILLGÄNGLIG    '.                            
017300     05 FILLER                   PIC X(40)                                
017400          VALUE 'DATABASE UNAVAILABLE       '.                            
017500   03  FILLER REDEFINES FEL8.                                             
017600     05  FEL-8                   PIC X(40)   OCCURS 2.                    
017700                                                                          
017800   03  MED1.                                                              
017900     05 FILLER                   PIC X(40)                                
018000          VALUE 'UPPDATERING GJORD        '.                              
018100     05 FILLER                   PIC X(40)                                
018200          VALUE 'FIELDS ARE UPDATED            '.                         
018300   03  FILLER REDEFINES MED1.                                             
018400     05  MED-1                   PIC X(40)   OCCURS 2.                    
018500                                                                          
018600   03  MED2.                                                              
018700     05 FILLER                   PIC X(40)                                
018800          VALUE 'TRYCK PF8 FÖR FLERA RADER'.                              
018900     05 FILLER                   PIC X(40)                                
019000          VALUE 'PRESS PF8 FOR MORE LINES'.                               
019100   03  FILLER REDEFINES MED2.                                             
019200     05  MED-2                   PIC X(40)   OCCURS 2.                    
019300                                                                          
019400   03  MED3.                                                              
019500     05 FILLER                   PIC X(40)                                
019600          VALUE 'DETTA ÄR FÖRSTA SIDAN'.                                  
019700     05 FILLER                   PIC X(40)                                
019800          VALUE 'THIS IS THE FIRST PAGE'.                                 
019900   03  FILLER REDEFINES MED3.                                             
020000     05  MED-3                   PIC X(40)   OCCURS 2.                    
020100                                                                          
020200   03  MED4.                                                              
020300     05 FILLER                   PIC X(40)                                
020400          VALUE 'FÖRSÖK SENARE, EV KONTAKTA SYSTANSV'.                    
020500     05 FILLER                   PIC X(40)                                
020600          VALUE 'TRY LATER OR NOTIFY THE DP-DEPARTMENT'.                  
020700   03  FILLER REDEFINES MED4.                                             
020800     05  MED-4                   PIC X(40)   OCCURS 2.                    
020900                                                                          
021000   03  MED5.                                                              
021100     05 FILLER                   PIC X(40)                                
021200          VALUE 'PF11 OCH INGET INMATAT'.                                 
021300     05 FILLER                   PIC X(40)                                
021400          VALUE 'PF11 AND NO INPUT'.                                      
021500   03  FILLER REDEFINES MED5.                                             
021600     05  MED-5                   PIC X(40)   OCCURS 2.                    
021700     EJECT                                                                
021800*- - - - - - - - - - - - - - - - - - - BYTES-ARTIKELTEST                  
021900 01  FILLER                      PIC X(16)   VALUE 'BYTES-ART'.           
022000 01  TEST-IDARTNR                PIC 9(9)   COMP-3.                       
022100*01  FILLER -COPY WWBYT01   -RED TEST-IDARTNR                             
022200*01  FILLER -COPY WWBYT16   -RED TEST-IDARTNR                             
022300     EJECT                                                                
022400******************************************************************        
022500*                                                                         
022600*                AREOR FÖR MFS OCH SKÄRMHANTERING                         
022700*                                                                         
022800 01  FILLER                      PIC X(16)   VALUE 'MFS-WS'.              
022900 01  FILLER                      PIC X(16)  VALUE 'MID-AREA PULS'.        
023000*01  MID -COPY W3I15101                                                   
023100     EJECT                                                                
023200 01  FILLER                      PIC X(16)  VALUE 'MID-VOLVISION'.        
023300*01  MID -COPY W3I151V1   -PRE MID2-                                      
023400     EJECT                                                                
023500 01  FILLER                      PIC X(16)  VALUE 'MSG AREA     '.        
023600*01  -COPY WMSGAREA                                                       
023700     EJECT                                                                
023800*  03  MOD -COPY W3O15101  -RED MSG-AREA.                                 
023900     EJECT                                                                
024000*  03  MOD -COPY W3O151V1 -PRE MOD2- -RED MSG-AREA.                       
024100     EJECT                                                                
024200 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
024300*01  -COPY WMFSAREA                                                       
024400     EJECT                                                                
024401* --- GENERAL  IO-COMMUNICATION  FOR DISPATCHER                           
024402*01    -COPY WMSGKOM                                                      
024403    EJECT                                                                 
024410 01  MSG-KOM-MESSAGE-CODES.                                               
024420     03  FEL-ERR-FIELD           PIC X(3)    VALUE '020'.                 
024430     03  FEL-ERR-IDDC            PIC X(3)    VALUE '028'.                 
024431     03  FEL-ERR-IDARTNR         PIC X(3)    VALUE '768'.                 
024432     03  FEL-ERR-IDPRODNR        PIC X(3)    VALUE '98J'.                 
024433     03  FEL-ERR-BELEV           PIC X(3)    VALUE '092'.                 
024434     03  FEL-ERR-IDDISTR         PIC X(3)    VALUE '747'.                 
024435     03  FEL-ERR-TEBYTNOT        PIC X(3)    VALUE '98K'.                 
024436     03  FEL-ERR-BETFLEV         PIC X(3)    VALUE '092'.                 
024437     03  FEL-ERR-KVLS            PIC X(3)    VALUE '94H'.                 
024438     03  FEL-ERR-UPDATE          PIC X(3)    VALUE '955'.                 
024440     03  OK-GODKANT-FEL          PIC X(3)    VALUE '114'.                 
024450     03  OK-BEHANDLAD            PIC X(3)    VALUE '101'.                 
024500******************************************************************        
024600*                                                                         
024700*        ARBETS-AREOR TILL DB2- OCH IMS-SEKTIONERNA                       
024800*                                                                         
024900 01  FILLER                  PIC X(16)   VALUE 'DB2-WS     '.             
025000*01  -COPY BYART -PRE BYART-                                              
025100     EJECT                                                                
025200*01  -COPY BYPRO -PRE BYPRO-                                              
025300     EJECT                                                                
025400*01  -COPY BYLEV -PRE BYLEV-                                              
025500     EJECT                                                                
025600 01  FILLER                  PIC X(16) VALUE 'BYART-AREA'.                
025700       EXEC SQL INCLUDE BYART END-EXEC.                                   
025800     SKIP3                                                                
025900 01  FILLER                  PIC X(16) VALUE 'BYPRO-AREA'.                
026000       EXEC SQL INCLUDE BYPRO END-EXEC.                                   
026100     SKIP3                                                                
026200 01  FILLER                  PIC X(16) VALUE 'BYLEV-AREA'.                
026300       EXEC SQL INCLUDE BYLEV END-EXEC.                                   
026400     SKIP3                                                                
026500 01  FILLER                  PIC X(16) VALUE 'SQLCA-AREA'.                
026600       EXEC SQL INCLUDE SQLCA END-EXEC.                                   
026700*                        **** STATUS-KOD FRÅN DB2                         
026800 01  FILLER                     PIC X(16) VALUE 'SQLCODE-WS'.             
026900 01  DB2-WS.                                                              
027000   03  SQLCODE-WS                PIC 9(3) VALUE ZERO.                     
027100     88  CURSOR-OK                           VALUE 000.                   
027200     88  RADER-FINNS                         VALUE 000.                   
027300     88  RADER-SAKNAS                        VALUE 100.                   
027400     88  904-KOD                             VALUE 904.                   
027500     SKIP1                                                                
027600   03  GODK-SQLCODESKODER.                                                
027700     05  GODK-SQLCODE OCCURS 5                                            
027800         INDEXED BY SQLCODE-IX PIC 999.                                   
027900     EJECT                                                                
028000 01  FILLER                    PIC X(16)   VALUE 'IMS-WS     '.           
028100 01  IMS-WS.                                                              
028200     SKIP3                                                                
028300*                        **** STATUS-KOD FRÅN IMS                         
028400   03  STATUS-WS                 PIC XX.                                  
028500     88  SEGMENT-FINNS                       VALUE '  '.                  
028600     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
028700     SKIP3                                                                
028800   03  GODK-STATUSKODER.                                                  
028900     05  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
029000     SKIP3                                                                
029100 01    SSA1                      PIC X(64).                               
029200 01    SSA2                      PIC X(64).                               
029300 01    SSA3                      PIC X(64).                               
029400*                            IMS FUNKTIONSKODER                           
029500*01    -COPY W0003                                                        
029600     EJECT                                                                
029700 01  FILLER                      PIC X(16) VALUE 'DLI-IO-AREA'.           
029800 01  DLI-IO-AREA.                                                         
029900   03  IO-AREA                   PIC X(128)   VALUE SPACE.                
030000*   03 ARTC01  -COPY WDK601   -PRE ARTC01-   -RED IO-AREA                 
030100     EJECT                                                                
030200*   03 LEVA01  -COPY WDF101   -PRE LEVA01-   -RED IO-AREA                 
030300     EJECT                                                                
030400*   03 BENA11  -COPY WDD311   -PRE BENA11-   -RED IO-AREA                 
030500     EJECT                                                                
030600 01  DLI-IO-WDK711.                                                       
030700*    03  -COPY WDK711                                                     
030800     EJECT                                                                
030900 LINKAGE SECTION.                                                         
031000*01  -COPY W0009     -PRE MSG-                                            
031100     SKIP2                                                                
031410*01  -COPY W0008     -PRE MSGKOM-                                         
031420     05  FILLER                  PIC X.                                   
031430     SKIP2                                                                
031440*01  -COPY W0008     -PRE USEA-                                           
031450     05  FILLER                  PIC X.                                   
031460     SKIP2                                                                
031500*01  -COPY W0008     -PRE ARTC-                                           
031600     05  FILLER                  PIC X.                                   
031700     SKIP2                                                                
031800*01  -COPY W0008     -PRE BENA-                                           
031900     05  FILLER                  PIC X.                                   
032000     SKIP2                                                                
032100*01  -COPY W0008     -PRE LEVA-                                           
032200     05  FILLER                  PIC X.                                   
032300     SKIP2                                                                
032400*01  -COPY W0008     -PRE WDK7-                                           
032500     05  FILLER                  PIC X.                                   
032600     EJECT                                                                
032700 PROCEDURE DIVISION   USING  MSG-PCB MSGKOM-PCB                           
032701                             USEA-PCB BENA-PCB LEVA-PCB                   
032710                             ARTC-PCB WDK7-PCB.                           
032900      ENTRY 'DLITCBL' USING  MSG-PCB MSGKOM-PCB                           
032901                             USEA-PCB BENA-PCB LEVA-PCB                   
032910                             ARTC-PCB WDK7-PCB.                           
033100     PERFORM IMS-GET-MSG                                                  
033200     IF SEGMENT-FINNS                                                     
033300        PERFORM IMS-GET-WMSGKOM-MSG                                       
033310        PERFORM A-INIT-KOLLA-NYCKLAR                                      
033400        IF NYCKLAR-OK                                                     
033500           IF PULS-TRANS                                                  
033600              PERFORM B-KOLLA-PF-TRYCK                                    
033700              IF MFS-UPDATE OR MFS-UPD-X                                  
033800                 PERFORM E-MFS-ROER-EJ-FAELT                              
033900                 PERFORM F-KOLLA-INDATA-EV-UPPDAT                         
034000              ELSE                                                        
034100                 IF MFS-IDPFK = ' '                                       
034200                    PERFORM G-KOLLA-ATT-INGET-IFYLLT                      
034300                    IF INGET-INMATAT                                      
034400                       PERFORM I-VISA-SIDAN                               
034500                    ELSE                                                  
034600                       PERFORM E-MFS-ROER-EJ-FAELT                        
034700                       MOVE FEL-4 (SPRAK-IX) TO MOD-TEMFSFEL              
034800                    END-IF                                                
034900                 ELSE                                                     
035000                    PERFORM I-VISA-SIDAN                                  
035100                 END-IF                                                   
035200              END-IF                                                      
035300           ELSE                                                           
035400*               (VOLVO-VISION-TRANS)                                      
035500              PERFORM I-VISA-SIDAN                                        
035600           END-IF                                                         
035700        ELSE                                                              
035800           IF PULS-TRANS                                                  
035900              PERFORM S05-RENSA-SIDAN                                     
036000           END-IF                                                         
036100        END-IF                                                            
036200        IF PULS-TRANS                                                     
036310          IF MFS-UPD-X                                                    
036320*            X-TRANS FRÅN DISPATCHERN SKALL INTE SVARA EN SKÄRM           
036330             IF WS-ERROR-UPDX = SPACE                                     
036340               MOVE OK-BEHANDLAD TO MSG-KOM-IDMFSMED                      
036350               PERFORM IMS-INSERT-WMSGKOM-MSG                             
036360             ELSE                                                         
036370               MOVE WS-ERROR-UPDX TO MSG-KOM-IDMFSMED                     
036380               MOVE '1'       TO MSG-KOM-KDSVAR                           
036390               PERFORM IMS-INSERT-WMSGKOM-MSG                             
036391             END-IF                                                       
036392          ELSE                                                            
036393             COMPUTE MSG-KVLL = LENGTH OF MOD-W3O15101 + 4                
036394             PERFORM IMS-INSERT-MSG                                       
036395          END-IF                                                          
036400        ELSE                                                              
036500*            (VOLVO-VISION-TRANS)                                         
036600           COMPUTE MSG-KVLL = LENGTH OF MOD2-W3O151V1 + 4                 
036610           PERFORM IMS-INSERT-MSG                                         
036700        END-IF                                                            
036900     END-IF                                                               
037000     MOVE ZERO                            TO RETURN-CODE                  
037100     GOBACK                                                               
037200     .                                                                    
037300     EJECT                                                                
037400 A-INIT-KOLLA-NYCKLAR SECTION.                                            
037500     SKIP2                                                                
037600     IF MSG-KDTRANS-1 (3:1) = 'T'                                         
037700       MOVE JA             TO FLAGGA-TRANS-TYP                            
037800     ELSE                                                                 
037900       MOVE NEJ            TO FLAGGA-TRANS-TYP                            
038000     END-IF                                                               
038100                                                                          
038200     IF PULS-TRANS                                                        
038300                                                                          
038400        IF MSG-DUBBLA-TRANSKODER                                          
038500           MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W3I15101             
038600           MOVE MSG-IDTRANS-2              TO MFS-IDTRANS                 
038700           MOVE MSG-KDMFSFOR-2             TO MFS-KDMFSFOR                
038800           MOVE MSG-KDTRTYP                TO MFS-KDTRTYP                 
038900           MOVE MSG-IDPFK                  TO MFS-IDPFK                   
039000        ELSE                                                              
039100           MOVE MSG-INDATA-MINUS-1-TRANSKOD TO MID-W3I15101               
039200           MOVE MSG-IDTRANS-1              TO MFS-IDTRANS                 
039300           MOVE MSG-KDMFSFOR-1             TO MFS-KDMFSFOR                
039400           MOVE SPACE                      TO MFS-KDTRTYP                 
039500                                              MFS-IDPFK                   
039600        END-IF                                                            
040500        MOVE MSG-KDTRTYP                     TO MFS-KDTRTYP               
040500        MOVE MSG-IDPFK                       TO MFS-IDPFK                 
040000        MOVE MFS-IDTRANS                     TO WS-IDTRANS                
040400        IF EGEN-BILD OR MFS-UPD-X                                         
040600           CONTINUE                                                       
040600        ELSE                                                              
040700           MOVE SPACE                      TO MFS-KDTRTYP                 
040800           MOVE '7'                        TO MFS-IDPFK                   
040900        END-IF                                                            
039700        MOVE LOW-VALUE                     TO MSG-AREA                    
039800        MOVE 'W3O151N1'                    TO MFS-IDMOD                   
039900        MOVE '3151'                        TO MOD-IDTRANS                 
040000        MOVE MFS-IDTRANS                   TO WS-IDTRANS                  
040100        MOVE MFS-RENSA-FAELT               TO MOD-TEMFSFEL                
040200                                              MOD-TEMFSINF                
040300                                              MOD-IDARTNR-IN              
041600     ELSE                                                                 
041700*         (VOLVO-VISION-TRANS)                                            
041800        MOVE MSG-INDATA-MINUS-1-TRANSKOD TO MID2-W3I151V1                 
041900        MOVE MSG-IDTRANS-1                  TO MFS-IDTRANS                
042000        MOVE MSG-KDMFSFOR-1                 TO MFS-KDMFSFOR               
042100        MOVE MSG-KDTRTYP                    TO MFS-KDTRTYP                
042200        MOVE MSG-IDPFK                      TO MFS-IDPFK                  
042300                                                                          
042400        MOVE LOW-VALUE                     TO MSG-AREA                    
042500        MOVE SPACE                         TO MFS-IDMOD                   
042600        MOVE '3151'                        TO MOD2-IDTRANS                
042700        MOVE MFS-IDTRANS                   TO WS-IDTRANS                  
042800     END-IF                                                               
042900                                                                          
043000     INITIALIZE GODK-SQLCODESKODER                                        
043100     PERFORM AA-KOLLA-NYCKLAR                                             
043200     .                                                                    
043300     EJECT                                                                
043400 AA-KOLLA-NYCKLAR  SECTION.                                               
043500     SKIP3                                                                
043600     MOVE JA                         TO SW-NYCKLAR-OK                     
043700                                                                          
043800     IF PULS-TRANS                                                        
043811       IF MFS-UPD-X                                                       
043812****************    DISPATCHER CALL                                       
043813         IF MID-IDARTNR-IN NUMERIC AND MID-IDARTNR-IN > ZERO              
043814            MOVE MID-IDARTNR-IN TO WS-IDARTNR                             
043815            MOVE WS-IDARTNR TO MOD-IDARTNR-UT                             
043816            INSPECT MOD-IDARTNR-UT REPLACING LEADING ZERO BY SPACE        
043817         END-IF                                                           
043860       ELSE                                                               
043900        MOVE ALL '+' TO MSGI-WMSGINIT                                     
044000        MOVE '001'          TO MSGI-KDCALL                                
044100        MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                             
044200        MOVE MSG-LTERM-NAME TO MSGI-IDLTERM-USER                          
044300        MOVE '3151'         TO MSGI-IDTRANS                               
044400        IF MFS-IDTRANS = '3151'                                           
044500        OR (MID-IDARTNR-IN NUMERIC                                        
044600        AND MID-IDARTNR-IN > ZERO)                                        
044700            MOVE MID-IDARTNR-IN TO MSGI-IDARTNR                           
044800        END-IF                                                            
044900        CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                        
045000        MOVE MSGI-IDARTNR TO WS-IDARTNR                                   
045100        INSPECT WS-IDARTNR REPLACING ALL SPACE BY ZERO                    
045200                                                                          
045300        IF MID-IDARTNR-IN = ALL '+'                                       
045400          CONTINUE                                                        
045500        ELSE                                                              
045500          IF MFS-UPD-X                                                    
045500             CONTINUE                                                     
045500          ELSE                                                            
045600            MOVE '7'    TO MFS-IDPFK                                      
045700            MOVE SPACE  TO MFS-KDTRTYP                                    
045800          END-IF                                                          
045800        END-IF                                                            
045900       END-IF                                                             
046000        MOVE WS-IDARTNR              TO MOD-IDARTNR-UT                    
046100        INSPECT MOD-IDARTNR-UT REPLACING LEADING ZERO BY SPACE            
046200        IF WS-IDARTNR NUMERIC                                             
046300           MOVE WS-IDARTNR TO TEST-IDARTNR                                
046400           IF BYT01-BYTES                                                 
046500              IF BYT16-RADIO                                              
046600                IF BYT16-RADIO-EXTRA                                      
046700                  MOVE 0                   TO WS-ARTSIFFRA                
046800                ELSE                                                      
046900                  MOVE 3                   TO WS-ARTSIFFRA                
047000                END-IF                                                    
047100              ELSE                                                        
047200                 IF ART-0                                                 
047300                    MOVE 0                 TO WS-ARTSIFFRA                
047400                 ELSE                                                     
047500                    IF ART-1                                              
047600                       MOVE 1              TO WS-ARTSIFFRA                
047700                    ELSE                                                  
047800                       IF ART-2                                           
047900                          MOVE 2           TO WS-ARTSIFFRA                
048000                       ELSE                                               
048100                         IF ART-3                                         
048200                            MOVE 3         TO WS-ARTSIFFRA                
048300                         END-IF                                           
048400                       END-IF                                             
048500                    END-IF                                                
048600                 END-IF                                                   
048700              END-IF                                                      
048800              MOVE WS-IDARTNR           TO W-IDARTNR-BYT                  
048900                                              W-IDARTNR                   
049000              PERFORM IMS-GU-ARTC01                                       
049100              IF SEGMENT-FINNS                                            
049200                 CONTINUE                                                 
049300              ELSE                                                        
049400                 MOVE NEJ               TO SW-NYCKLAR-OK                  
049410                 MOVE FEL-ERR-IDARTNR   TO WS-ERROR-UPDX                  
049500                 MOVE FEL-3 (SPRAK-IX)  TO MOD-TEMFSFEL                   
049600              END-IF                                                      
049700           ELSE                                                           
049800              MOVE NEJ               TO SW-NYCKLAR-OK                     
049810              MOVE FEL-ERR-IDARTNR   TO WS-ERROR-UPDX                     
049900              MOVE FEL-6 (SPRAK-IX)  TO MOD-TEMFSFEL                      
050000           END-IF                                                         
050100        ELSE                                                              
050200           MOVE NEJ                  TO SW-NYCKLAR-OK                     
050210           MOVE FEL-ERR-IDARTNR   TO WS-ERROR-UPDX                        
050300           MOVE FEL-2 (SPRAK-IX)     TO MOD-TEMFSFEL                      
050400        END-IF                                                            
050500     ELSE                                                                 
050600*         (VOLVO-VISION-TRANS)                                            
050700        MOVE ZERO                       TO MOD2-IDMFSFEL                  
050800        MOVE MID2-IDARTNR-BYT           TO WS-IDARTNR                     
050900        INSPECT WS-IDARTNR REPLACING ALL SPACE BY ZERO                    
051000        IF WS-IDARTNR NUMERIC                                             
051100           MOVE WS-IDARTNR              TO TEST-IDARTNR                   
051200                                           MOD2-IDARTNR-BYT-INPUT         
051300           IF BYT01-BYTES                                                 
051400              IF BYT16-RADIO                                              
051500                IF BYT16-RADIO-EXTRA                                      
051600                  MOVE 0                TO WS-ARTSIFFRA                   
051700                ELSE                                                      
051800                  MOVE 3                TO WS-ARTSIFFRA                   
051900                END-IF                                                    
052000              ELSE                                                        
052100                 IF ART-1                                                 
052200                    MOVE 1              TO WS-ARTSIFFRA                   
052300                 ELSE                                                     
052400                    IF ART-2                                              
052500                       MOVE 2           TO WS-ARTSIFFRA                   
052600                    ELSE                                                  
052700                       IF ART-3                                           
052800                          MOVE 3        TO WS-ARTSIFFRA                   
052900                       END-IF                                             
053000                    END-IF                                                
053100                 END-IF                                                   
053200              END-IF                                                      
053300              MOVE WS-IDARTNR           TO W-IDARTNR-BYT                  
053400                                              W-IDARTNR                   
053500              PERFORM IMS-GU-ARTC01                                       
053600              IF SEGMENT-FINNS                                            
053700                 CONTINUE                                                 
053800              ELSE                                                        
053900                 MOVE NEJ         TO SW-NYCKLAR-OK                        
053910                 MOVE FEL-ERR-IDARTNR   TO WS-ERROR-UPDX                  
054000                 MOVE 'B10'       TO MOD2-IDMFSFEL                        
054100              END-IF                                                      
054200           ELSE                                                           
054300              MOVE NEJ            TO SW-NYCKLAR-OK                        
054310              MOVE FEL-ERR-IDARTNR   TO WS-ERROR-UPDX                     
054400              MOVE 'B10'          TO MOD2-IDMFSFEL                        
054500           END-IF                                                         
054600        ELSE                                                              
054700           MOVE NEJ               TO SW-NYCKLAR-OK                        
054710           MOVE FEL-ERR-IDARTNR   TO WS-ERROR-UPDX                        
054800           MOVE 'B10'             TO MOD2-IDMFSFEL                        
054900        END-IF                                                            
055000     END-IF                                                               
055100     .                                                                    
055200     EJECT                                                                
055300 B-KOLLA-PF-TRYCK  SECTION.                                               
055400     SKIP2                                                                
055500     IF  MID-IDPRODNR-LO NUMERIC                                          
055600     AND MID-IDPRODNR-HI NUMERIC                                          
055700        CONTINUE                                                          
055800     ELSE                                                                 
055900        MOVE ZERO                     TO MID-IDPRODNR-LO                  
056000                                         MID-IDPRODNR-HI                  
056100     END-IF                                                               
056200     IF MFS-IDPFK = '8'                                                   
056300        IF  MID-IDPRODNR-HI = ZERO                                        
056400        AND MID-BELEV-HI  = SPACE                                         
056500           MOVE '7'                   TO MFS-IDPFK                        
056600        END-IF                                                            
056700     END-IF                                                               
056800     IF MFS-IDPFK = '8'                                                   
056900        IF  MID-IDPRODNR-HI = ZERO                                        
057000           MOVE MID-IDPRODNR-LO       TO W-IDPRODNR                       
057100        ELSE                                                              
057200           MOVE MID-IDPRODNR-HI       TO W-IDPRODNR                       
057300        END-IF                                                            
057400        IF  MID-BELEV-HI  = SPACE                                         
057500           MOVE MID-BELEV-LO          TO W-BELEV                          
057600        ELSE                                                              
057700           MOVE MID-BELEV-HI          TO W-BELEV                          
057800        END-IF                                                            
057900     ELSE                                                                 
058000        IF MFS-IDPFK = ' '                                                
058100           MOVE MID-BELEV-LO          TO W-BELEV                          
058200           MOVE MID-IDPRODNR-LO       TO W-IDPRODNR                       
058300        ELSE                                                              
058400            MOVE LOW-VALUE            TO W-BELEV                          
058500            MOVE ZERO                 TO W-IDPRODNR                       
058600            MOVE MED-3 (SPRAK-IX)     TO MOD-TEMFSFEL                     
058700        END-IF                                                            
058800     END-IF                                                               
058900     .                                                                    
059000     EJECT                                                                
059100 E-MFS-ROER-EJ-FAELT  SECTION.                                            
059200     SKIP2                                                                
059300     MOVE MFS-ROER-EJ-FAELT          TO MOD-IDPRODNR-LO                   
059400                                        MOD-IDPRODNR-HI                   
059500                                        MOD-BELEV-LO                      
059600                                        MOD-BELEV-HI                      
059700                                        MOD-BEART-SVE                     
059800                                        MOD-BETFLEV                       
059900                                        MOD-UPPDATERINGSSORT              
060000                                        MOD-IDPRODNR-IN                   
060100                                        MOD-IDDISTR-RENOV-IN              
060200                                        MOD-IDDISTR-NDC-IN                
060300                                        MOD-IDDISTR-PAC-IN                
060400                                        MOD-IDDISTR-CAN-IN                
060500                                        MOD-IDDISTR-AUS-IN                
060600                                        MOD-IDDISTR-CHN-IN                
060700                                        MOD-IDDISTR-KOR-IN                
060710                                        MOD-IDDISTR-MY-IN                 
060720                                        MOD-IDDISTR-TW-IN                 
060730                                        MOD-IDDISTR-TH-IN                 
060800                                        MOD-BELEV-IN                      
060900     MOVE +1                         TO RAD-INDX                          
061000     PERFORM UNTIL RAD-INDX > +3                                          
061100        MOVE MFS-ROER-EJ-FAELT       TO MOD-TEBYTNOT (RAD-INDX)           
061200        ADD +1                       TO RAD-INDX                          
061300     END-PERFORM                                                          
061400     MOVE MFS-ROER-EJ-FAELT       TO MOD-IDDISTR-RENOV                    
061500     MOVE MFS-ROER-EJ-FAELT       TO MOD-IDDISTR-NDC                      
061600     MOVE MFS-ROER-EJ-FAELT       TO MOD-IDDISTR-PAC                      
061700     MOVE MFS-ROER-EJ-FAELT       TO MOD-IDDISTR-CAN                      
061800     MOVE MFS-ROER-EJ-FAELT       TO MOD-IDDISTR-AUS                      
061900     MOVE MFS-ROER-EJ-FAELT       TO MOD-IDDISTR-CHN                      
062000     MOVE MFS-ROER-EJ-FAELT       TO MOD-IDDISTR-KOR                      
062010     MOVE MFS-ROER-EJ-FAELT       TO MOD-IDDISTR-MY                       
062020     MOVE MFS-ROER-EJ-FAELT       TO MOD-IDDISTR-TW                       
062030     MOVE MFS-ROER-EJ-FAELT       TO MOD-IDDISTR-TH                       
062100     MOVE MFS-ROER-EJ-FAELT       TO MOD-KVLS                             
062200     MOVE MFS-ROER-EJ-FAELT       TO MOD-KVLS-MAXCORE-IN                  
062300     MOVE MFS-ROER-EJ-FAELT       TO MOD-KVLS-MAXCORE-UT                  
062400     MOVE MFS-ROER-EJ-FAELT       TO MOD-TEBYTNOT(4)                      
062500     MOVE +1                         TO RAD-INDX                          
062600     PERFORM UNTIL RAD-INDX > +18                                         
062700        MOVE MFS-ROER-EJ-FAELT       TO MOD-IDARTNR (RAD-INDX)            
062800        ADD +1                       TO RAD-INDX                          
062900     END-PERFORM                                                          
063000     MOVE +1                         TO RAD-INDX                          
063100     PERFORM UNTIL RAD-INDX > +6                                          
063200        MOVE MFS-ROER-EJ-FAELT       TO MOD-BELEV   (RAD-INDX)            
063300        ADD +1                       TO RAD-INDX                          
063400     END-PERFORM                                                          
063500     .                                                                    
063600     EJECT                                                                
063700 F-KOLLA-INDATA-EV-UPPDAT  SECTION.                                       
063800     SKIP2                                                                
063900     IF  MID-UPPDATERINGSSORT = ALL '+'                                   
064000     AND MID-IDPRODNR-IN = ALL '+'                                        
064100     AND MID-BELEV = ALL '+'                                              
064200        PERFORM FH-EV-UPPD-BYTNOT-BETFLEV                                 
064300     ELSE                                                                 
064400        MOVE JA                           TO SW-INDATA-OK                 
064500        IF MID-UPPDATERINGSSORT = 'N'                                     
064600           MOVE MFS-ALFA-FAELT-RAETT      TO                              
064700                                   MOD-UPPDATERINGSSORT-ATTR              
064800           PERFORM FD-KOLLA-INDATA-NYUPPL                                 
064900        ELSE                                                              
065000           IF MID-UPPDATERINGSSORT = 'D' OR 'B'                           
065100              MOVE MFS-ALFA-FAELT-RAETT   TO                              
065200                                   MOD-UPPDATERINGSSORT-ATTR              
065300              PERFORM FA-KOLLA-INDATA-BORTTAG                             
065400           ELSE                                                           
065500              MOVE NEJ                    TO SW-INDATA-OK                 
065600              MOVE MFS-ALFA-FAELT-FEL     TO                              
065700                                      MOD-UPPDATERINGSSORT-ATTR           
065800              PERFORM FG-LAES-IN-IGEN                                     
065900           END-IF                                                         
066000        END-IF                                                            
066100        IF INDATA-OK                                                      
066200           MOVE MED-1 (SPRAK-IX)     TO MOD-TEMFSINF                      
066300           MOVE W-IDARTNR-BYT        TO W-IDARTNR                         
066400           IF MID-UPPDATERINGSSORT = 'N'                                  
066500              PERFORM DB2-SELECT-BYART                                    
066600              IF SQLCODE = ZERO                                           
066700                 PERFORM FE-UPPDATERA-BYART                               
066800                 PERFORM FC-SKAP-BYLEV-BYPRO-RENSA-INM                    
066900              ELSE                                                        
067000                 IF 904-KOD                                               
067100                    MOVE FEL-8 (SPRAK-IX)    TO MOD-TEMFSFEL              
067200                    MOVE MED-4 (SPRAK-IX)    TO MOD-TEMFSINF              
067300                 ELSE                                                     
067400                    PERFORM IMS-GU-BENA11                                 
067500                    IF SEGMENT-FINNS                                      
067600                       MOVE BENA11-TEXT-BEART   TO MOD-BEART-SVE          
067700                    END-IF                                                
067800                    PERFORM FF-SKAPA-BYART                                
067900                    PERFORM FC-SKAP-BYLEV-BYPRO-RENSA-INM                 
068000                 END-IF                                                   
068100              END-IF                                                      
068200           ELSE                                                           
068300              PERFORM FB-TA-BORT                                          
068400              PERFORM S04-RENSA-INMATAT                                   
068500           END-IF                                                         
068600        ELSE                                                              
068700           MOVE FEL-5 (SPRAK-IX)     TO MOD-TEMFSFEL                      
068800        END-IF                                                            
068900     END-IF                                                               
069000     .                                                                    
069100     EJECT                                                                
069200 FA-KOLLA-INDATA-BORTTAG SECTION.                                         
069300     SKIP2                                                                
069400     IF MID-IDPRODNR-IN = ALL '+'                                         
069500        CONTINUE                                                          
069600     ELSE                                                                 
069700        IF MID-IDPRODNR-IN NUMERIC                                        
069800           MOVE MFS-NUM-FAELT-RAETT       TO MOD-IDPRODNR-IN-ATTR         
069900        ELSE                                                              
070000           MOVE NEJ                       TO SW-INDATA-OK                 
070100           MOVE FEL-ERR-IDPRODNR          TO WS-ERROR-UPDX                
070100           MOVE MFS-NUM-FAELT-FEL         TO MOD-IDPRODNR-IN-ATTR         
070200        END-IF                                                            
070300     END-IF                                                               
070400     IF MID-IDDISTR-RENOV-IN = ALL '+'                                    
070500        CONTINUE                                                          
070600     ELSE                                                                 
070700        IF MID-IDDISTR-RENOV-IN NUMERIC                                   
070800           MOVE MFS-NUM-FAELT-RAETT  TO MOD-IDDISTR-RENOV-IN-ATTR         
070900        ELSE                                                              
071000           MOVE NEJ                       TO SW-INDATA-OK                 
071100           MOVE MFS-NUM-FAELT-FEL    TO MOD-IDDISTR-RENOV-IN-ATTR         
071200        END-IF                                                            
071300     END-IF                                                               
071400     IF MID-IDDISTR-NDC-IN = ALL '+'                                      
071500        CONTINUE                                                          
071600     ELSE                                                                 
071700        IF MID-IDDISTR-NDC-IN NUMERIC                                     
071800           MOVE MFS-NUM-FAELT-RAETT  TO MOD-IDDISTR-NDC-IN-ATTR           
071900        ELSE                                                              
072000           MOVE NEJ                       TO SW-INDATA-OK                 
072100           MOVE MFS-NUM-FAELT-FEL    TO MOD-IDDISTR-NDC-IN-ATTR           
072200        END-IF                                                            
072300     END-IF                                                               
072400     IF MID-IDDISTR-PAC-IN = ALL '+'                                      
072500        CONTINUE                                                          
072600     ELSE                                                                 
072700        IF MID-IDDISTR-PAC-IN NUMERIC                                     
072800           MOVE MFS-NUM-FAELT-RAETT  TO MOD-IDDISTR-PAC-IN-ATTR           
072900        ELSE                                                              
073000           MOVE NEJ                       TO SW-INDATA-OK                 
073100           MOVE MFS-NUM-FAELT-FEL    TO MOD-IDDISTR-PAC-IN-ATTR           
073200        END-IF                                                            
073300     END-IF                                                               
073400                                                                          
073500     IF MID-IDDISTR-CAN-IN = ALL '+'                                      
073600        CONTINUE                                                          
073700     ELSE                                                                 
073800        IF MID-IDDISTR-CAN-IN NUMERIC                                     
073900           MOVE MFS-NUM-FAELT-RAETT  TO MOD-IDDISTR-CAN-IN-ATTR           
074000        ELSE                                                              
074100           MOVE NEJ                       TO SW-INDATA-OK                 
074200           MOVE MFS-NUM-FAELT-FEL    TO MOD-IDDISTR-CAN-IN-ATTR           
074300        END-IF                                                            
074400     END-IF                                                               
074500                                                                          
074600     IF MID-KVLS-MAXCORE-IN  = ALL '+'                                    
074700        CONTINUE                                                          
074800     ELSE                                                                 
074900        IF MID-KVLS-MAXCORE-IN NUMERIC                                    
075000           MOVE MFS-NUM-FAELT-RAETT  TO MOD-KVLS-MAXCORE-IN-ATTR          
075100        ELSE                                                              
075200           MOVE NEJ                  TO SW-INDATA-OK                      
075300           MOVE MFS-NUM-FAELT-FEL    TO MOD-KVLS-MAXCORE-IN-ATTR          
075400           MOVE FEL-5 (SPRAK-IX)     TO MOD-TEMFSFEL                      
075400           MOVE FEL-ERR-FIELD        TO WS-ERROR-UPDX                     
075500        END-IF                                                            
075600     END-IF                                                               
075700                                                                          
075800     IF MID-IDDISTR-AUS-IN = ALL '+'                                      
075900        CONTINUE                                                          
076000     ELSE                                                                 
076100        IF MID-IDDISTR-AUS-IN NUMERIC                                     
076200           MOVE MFS-NUM-FAELT-RAETT  TO MOD-IDDISTR-AUS-IN-ATTR           
076300        ELSE                                                              
076400           MOVE NEJ                       TO SW-INDATA-OK                 
076500           MOVE MFS-NUM-FAELT-FEL    TO MOD-IDDISTR-AUS-IN-ATTR           
076600        END-IF                                                            
076700     END-IF                                                               
076800                                                                          
076900     IF MID-IDDISTR-CHN-IN = ALL '+'                                      
077000        CONTINUE                                                          
077100     ELSE                                                                 
077200        IF MID-IDDISTR-CHN-IN NUMERIC                                     
077300           MOVE MFS-NUM-FAELT-RAETT  TO MOD-IDDISTR-CHN-IN-ATTR           
077400        ELSE                                                              
077500           MOVE NEJ                       TO SW-INDATA-OK                 
077600           MOVE MFS-NUM-FAELT-FEL    TO MOD-IDDISTR-CHN-IN-ATTR           
077700        END-IF                                                            
077800     END-IF                                                               
077900                                                                          
078000     IF MID-IDDISTR-KOR-IN = ALL '+'                                      
078100        CONTINUE                                                          
078200     ELSE                                                                 
078300        IF MID-IDDISTR-KOR-IN NUMERIC                                     
078400           MOVE MFS-NUM-FAELT-RAETT  TO MOD-IDDISTR-KOR-IN-ATTR           
078500        ELSE                                                              
078600           MOVE NEJ                       TO SW-INDATA-OK                 
078700           MOVE MFS-NUM-FAELT-FEL    TO MOD-IDDISTR-KOR-IN-ATTR           
078800        END-IF                                                            
078900     END-IF                                                               
079000                                                                          
079010     IF MID-IDDISTR-MY-IN = ALL '+'                                       
079020        CONTINUE                                                          
079030     ELSE                                                                 
079040        IF MID-IDDISTR-MY-IN NUMERIC                                      
079050           MOVE MFS-NUM-FAELT-RAETT  TO MOD-IDDISTR-MY-IN-ATTR            
079060        ELSE                                                              
079070           MOVE NEJ                       TO SW-INDATA-OK                 
079080           MOVE MFS-NUM-FAELT-FEL    TO MOD-IDDISTR-MY-IN-ATTR            
079090        END-IF                                                            
079091     END-IF                                                               
079092                                                                          
079093     IF MID-IDDISTR-TW-IN = ALL '+'                                       
079094        CONTINUE                                                          
079095     ELSE                                                                 
079096        IF MID-IDDISTR-TW-IN NUMERIC                                      
079097           MOVE MFS-NUM-FAELT-RAETT  TO MOD-IDDISTR-TW-IN-ATTR            
079098        ELSE                                                              
079099           MOVE NEJ                       TO SW-INDATA-OK                 
079100           MOVE MFS-NUM-FAELT-FEL    TO MOD-IDDISTR-TW-IN-ATTR            
079101        END-IF                                                            
079102     END-IF                                                               
079103                                                                          
079104     IF MID-IDDISTR-TH-IN = ALL '+'                                       
079105        CONTINUE                                                          
079106     ELSE                                                                 
079107        IF MID-IDDISTR-TH-IN NUMERIC                                      
079108           MOVE MFS-NUM-FAELT-RAETT  TO MOD-IDDISTR-TH-IN-ATTR            
079109        ELSE                                                              
079110           MOVE NEJ                       TO SW-INDATA-OK                 
079111           MOVE MFS-NUM-FAELT-FEL    TO MOD-IDDISTR-TH-IN-ATTR            
079112        END-IF                                                            
079113     END-IF                                                               
079114                                                                          
079120     IF MID-BELEV = ALL '+'                                               
079200        CONTINUE                                                          
079300     ELSE                                                                 
079400        MOVE MID-BELEV                 TO W-BELEV                         
079500        PERFORM DB2-SELECT-BYLEV                                          
079600        IF CURSOR-OK                                                      
079700           IF BYLEV-IDARTNR-BYT = W-IDARTNR-BYT                           
079800              MOVE MFS-ALFA-FAELT-RAETT TO                                
079900                                       MOD-BELEV-IN-ATTR                  
080000           ELSE                                                           
080100              MOVE NEJ                 TO SW-INDATA-OK                    
080200              MOVE MFS-ALFA-FAELT-FEL  TO                                 
080300                                       MOD-BELEV-IN-ATTR                  
080400           END-IF                                                         
080500        ELSE                                                              
080600           MOVE MFS-ALFA-FAELT-RAETT   TO                                 
080700                                       MOD-BELEV-IN-ATTR                  
080800        END-IF                                                            
080900     END-IF                                                               
081000     .                                                                    
081100     EJECT                                                                
081200 FB-TA-BORT    SECTION.                                                   
081300     SKIP3                                                                
081400     IF  MID-TEBYTNOT(1)      = ALL '+'                                   
081500     AND MID-TEBYTNOT(2)      = ALL '+'                                   
081600     AND MID-TEBYTNOT(3)      = ALL '+'                                   
081700     AND MID-TEBYTNOT(4)      = ALL '+'                                   
081800     AND MID-BETFLEV          = ALL '+'                                   
081900     AND MID-IDDISTR-RENOV-IN = ALL '+'                                   
082000     AND MID-IDDISTR-NDC-IN   = ALL '+'                                   
082100     AND MID-IDDISTR-PAC-IN   = ALL '+'                                   
082200     AND MID-IDDISTR-CAN-IN   = ALL '+'                                   
082300     AND MID-IDDISTR-AUS-IN   = ALL '+'                                   
082400     AND MID-IDDISTR-CHN-IN   = ALL '+'                                   
082500     AND MID-IDDISTR-KOR-IN   = ALL '+'                                   
082510     AND MID-IDDISTR-MY-IN    = ALL '+'                                   
082520     AND MID-IDDISTR-TW-IN    = ALL '+'                                   
082530     AND MID-IDDISTR-TH-IN    = ALL '+'                                   
082600        IF  MID-BELEV = ALL '+'                                           
082700        AND MID-IDPRODNR-IN = ALL '+'                                     
082800           MOVE MED-5 (SPRAK-IX)       TO MOD-TEMFSFEL                    
082900           MOVE MFS-RENSA-FAELT        TO MOD-TEMFSINF                    
083000        ELSE                                                              
083100           PERFORM FBB-EV-TABORT-BYPRO-BYLEV                              
083200        END-IF                                                            
083300     ELSE                                                                 
083400        PERFORM FBA-TABORT-ELEMENT-BYART                                  
083500        PERFORM FBB-EV-TABORT-BYPRO-BYLEV                                 
083600     END-IF                                                               
083700     .                                                                    
083800     EJECT                                                                
083900 FBA-TABORT-ELEMENT-BYART SECTION.                                        
084000     SKIP1                                                                
084100     PERFORM DB2-SELECT-BYART                                             
084200     IF SQLCODE = ZERO                                                    
084300        MOVE +1                         TO RAD-INDX                       
084400        PERFORM UNTIL RAD-INDX > 4                                        
084500           IF MID-TEBYTNOT (RAD-INDX) = ALL '+'                           
084600              ADD +1                    TO RAD-INDX                       
084700           ELSE                                                           
084800              PERFORM FBAA-BEHANDLA-BYTNOT                                
084900           END-IF                                                         
085000        END-PERFORM                                                       
085100                                                                          
085200        IF MID-IDDISTR-CHN-IN = ALL '+'                                   
085300           CONTINUE                                                       
085400        ELSE                                                              
085500           PERFORM FBAB-TA-BORT-IDDISTR-CHN                               
085600        END-IF                                                            
085700                                                                          
085800        IF MID-IDDISTR-KOR-IN = ALL '+'                                   
085900           CONTINUE                                                       
086000        ELSE                                                              
086100           PERFORM FBAH-TA-BORT-IDDISTR-KOR                               
086200        END-IF                                                            
086300                                                                          
086310        IF MID-IDDISTR-MY-IN = ALL '+'                                    
086320           CONTINUE                                                       
086330        ELSE                                                              
086340           PERFORM FBAI-TA-BORT-IDDISTR-MY                                
086350        END-IF                                                            
086360                                                                          
086370        IF MID-IDDISTR-TW-IN = ALL '+'                                    
086380           CONTINUE                                                       
086390        ELSE                                                              
086391           PERFORM FBAJ-TA-BORT-IDDISTR-TW                                
086392        END-IF                                                            
086393                                                                          
086394        IF MID-IDDISTR-TH-IN = ALL '+'                                    
086395           CONTINUE                                                       
086396        ELSE                                                              
086397           PERFORM FBAK-TA-BORT-IDDISTR-TH                                
086398        END-IF                                                            
086399                                                                          
086400        IF MID-IDDISTR-PAC-IN = ALL '+'                                   
086500           CONTINUE                                                       
086600        ELSE                                                              
086700           PERFORM FBAD-TA-BORT-IDDISTR-PAC                               
086800        END-IF                                                            
086900                                                                          
087000        IF MID-IDDISTR-CAN-IN = ALL '+'                                   
087100           CONTINUE                                                       
087200        ELSE                                                              
087300           PERFORM FBAF-TA-BORT-IDDISTR-CAN                               
087400        END-IF                                                            
087500                                                                          
087600        IF MID-IDDISTR-AUS-IN = ALL '+'                                   
087700           CONTINUE                                                       
087800        ELSE                                                              
087900           PERFORM FBAG-TA-BORT-IDDISTR-AUS                               
088000        END-IF                                                            
088100                                                                          
088200        IF MID-IDDISTR-NDC-IN = ALL '+'                                   
088300           CONTINUE                                                       
088400        ELSE                                                              
088500           PERFORM FBAE-TA-BORT-IDDISTR-NDC                               
088600        END-IF                                                            
088700                                                                          
088800        IF MID-IDDISTR-RENOV-IN = ALL '+'                                 
088900           CONTINUE                                                       
089000        ELSE                                                              
089100           PERFORM FBAC-TA-BORT-IDDISTR-RENOV                             
089200        END-IF                                                            
089300                                                                          
089400        IF MID-BETFLEV = ALL '+'                                          
089500           CONTINUE                                                       
089600        ELSE                                                              
089700           MOVE SPACE                 TO BYART-BETFLEV                    
089800           MOVE MFS-FORMATETS-ATTR    TO MOD-BETFLEV-ATTR                 
089900        END-IF                                                            
090000        PERFORM S01-FLYTT-BYART-T-BILD                                    
090100        PERFORM DB2-UPDATE-BYART                                          
090200     ELSE                                                                 
090300        IF 904-KOD                                                        
090400           MOVE FEL-8 (SPRAK-IX)    TO MOD-TEMFSFEL                       
090500           MOVE MED-4 (SPRAK-IX)    TO MOD-TEMFSINF                       
090600        END-IF                                                            
090700     END-IF                                                               
090800     .                                                                    
090900     EJECT                                                                
091000 FBAA-BEHANDLA-BYTNOT  SECTION.                                           
091100     SKIP3                                                                
091200     PERFORM UNTIL RAD-INDX > 4                                           
091300        IF MID-TEBYTNOT (RAD-INDX) = ALL '+'                              
091400           CONTINUE                                                       
091500        ELSE                                                              
091600           IF RAD-INDX = 1                                                
091700              MOVE SPACE          TO BYART-TEBYTNOT1                      
091800           ELSE                                                           
091900              IF RAD-INDX = 2                                             
092000                 MOVE SPACE          TO BYART-TEBYTNOT2                   
092100              ELSE                                                        
092200                IF RAD-INDX = 3                                           
092300                   MOVE SPACE          TO BYART-TEBYTNOT3                 
092400                ELSE                                                      
092500                  MOVE MID-IDDISTR-RENOV                                  
092600                           TO IDDISTR-RENOV-WS                            
092700                  INSPECT  IDDISTR-RENOV-WS REPLACING                     
092800                           LEADING SPACE BY ZERO                          
092900                  IF  WS-IDDISTR-RENOV = BYART-IDDISTR-RENOV              
093000                      MOVE SPACE          TO BYART-TEBYTNOT4              
093100                  END-IF                                                  
093200                END-IF                                                    
093300              END-IF                                                      
093400           END-IF                                                         
093500           MOVE MFS-FORMATETS-ATTR       TO                               
093600                                      MOD-TEBYTNOT-ATTR(RAD-INDX)         
093700        END-IF                                                            
093800        ADD +1                                TO RAD-INDX                 
093900     END-PERFORM                                                          
094000     .                                                                    
094100     EJECT                                                                
094200 FBAB-TA-BORT-IDDISTR-CHN   SECTION.                                      
094300     SKIP2                                                                
094400     MOVE MID-IDDISTR-CHN-IN TO IDDISTR-CHN-WS                            
094500     IF  WS-IDDISTR-CHN = BYART-IDDISTR-RENOV-CHN                         
094600        MOVE ZERO              TO BYART-IDDISTR-RENOV-CHN                 
094700     END-IF                                                               
094800     MOVE MFS-FORMATETS-ATTR   TO MOD-IDDISTR-CHN-IN-ATTR                 
094900     .                                                                    
095000     EJECT                                                                
095100 FBAC-TA-BORT-IDDISTR-RENOV SECTION.                                      
095200     SKIP2                                                                
095300     MOVE MID-IDDISTR-RENOV-IN TO IDDISTR-RENOV-WS                        
095400     IF  WS-IDDISTR-RENOV = BYART-IDDISTR-RENOV                           
095500        MOVE ZERO              TO BYART-IDDISTR-RENOV                     
095600     END-IF                                                               
095700     MOVE MFS-FORMATETS-ATTR   TO MOD-IDDISTR-RENOV-IN-ATTR               
095800     .                                                                    
095900     EJECT                                                                
096000 FBAD-TA-BORT-IDDISTR-PAC   SECTION.                                      
096100     SKIP2                                                                
096200     MOVE MID-IDDISTR-PAC-IN TO IDDISTR-PAC-WS                            
096300     IF  WS-IDDISTR-PAC = BYART-IDDISTR-RENOV-PAC                         
096400        MOVE ZERO              TO BYART-IDDISTR-RENOV-PAC                 
096500     END-IF                                                               
096600     MOVE MFS-FORMATETS-ATTR   TO MOD-IDDISTR-PAC-IN-ATTR                 
096700     .                                                                    
096800     EJECT                                                                
096900 FBAE-TA-BORT-IDDISTR-NDC   SECTION.                                      
097000     SKIP2                                                                
097100     MOVE MID-IDDISTR-NDC-IN TO IDDISTR-NDC-WS                            
097200     IF  WS-IDDISTR-NDC = BYART-IDDISTR-RENOV-NDC                         
097300        MOVE ZERO              TO BYART-IDDISTR-RENOV-NDC                 
097400     END-IF                                                               
097500     MOVE MFS-FORMATETS-ATTR   TO MOD-IDDISTR-NDC-IN-ATTR                 
097600     .                                                                    
097700     EJECT                                                                
097800 FBAF-TA-BORT-IDDISTR-CAN   SECTION.                                      
097900     SKIP2                                                                
098000     MOVE MID-IDDISTR-CAN-IN TO IDDISTR-CAN-WS                            
098100     IF  WS-IDDISTR-CAN = BYART-IDDISTR-RENOV-CAN                         
098200        MOVE ZERO              TO BYART-IDDISTR-RENOV-CAN                 
098300     END-IF                                                               
098400     MOVE MFS-FORMATETS-ATTR   TO MOD-IDDISTR-CAN-IN-ATTR                 
098500     .                                                                    
098600     EJECT                                                                
098700 FBAG-TA-BORT-IDDISTR-AUS   SECTION.                                      
098800     SKIP2                                                                
098900     MOVE MID-IDDISTR-AUS-IN TO IDDISTR-AUS-WS                            
099000     IF  WS-IDDISTR-AUS = BYART-IDDISTR-RENOV-AUS                         
099100        MOVE ZERO              TO BYART-IDDISTR-RENOV-AUS                 
099200     END-IF                                                               
099300     MOVE MFS-FORMATETS-ATTR   TO MOD-IDDISTR-AUS-IN-ATTR                 
099400     .                                                                    
099500     EJECT                                                                
099600 FBAH-TA-BORT-IDDISTR-KOR   SECTION.                                      
099700     SKIP2                                                                
099800     MOVE MID-IDDISTR-KOR-IN TO IDDISTR-KOR-WS                            
099900     IF  WS-IDDISTR-KOR = BYART-IDDISTR-RENOV-KOR                         
100000        MOVE ZERO              TO BYART-IDDISTR-RENOV-KOR                 
100100     END-IF                                                               
100200     MOVE MFS-FORMATETS-ATTR   TO MOD-IDDISTR-KOR-IN-ATTR                 
100300     .                                                                    
100400     EJECT                                                                
100410 FBAI-TA-BORT-IDDISTR-MY    SECTION.                                      
100420     SKIP2                                                                
100430     MOVE MID-IDDISTR-MY-IN TO IDDISTR-MY-WS                              
100440     IF  WS-IDDISTR-MY = BYART-IDDISTR-RENOV-MY                           
100450        MOVE ZERO              TO BYART-IDDISTR-RENOV-MY                  
100460     END-IF                                                               
100470     MOVE MFS-FORMATETS-ATTR   TO MOD-IDDISTR-MY-IN-ATTR                  
100480     .                                                                    
100490     EJECT                                                                
100491 FBAJ-TA-BORT-IDDISTR-TW    SECTION.                                      
100492     SKIP2                                                                
100493     MOVE MID-IDDISTR-TW-IN TO IDDISTR-TW-WS                              
100494     IF  WS-IDDISTR-TW = BYART-IDDISTR-RENOV-TW                           
100495        MOVE ZERO              TO BYART-IDDISTR-RENOV-TW                  
100496     END-IF                                                               
100497     MOVE MFS-FORMATETS-ATTR   TO MOD-IDDISTR-TW-IN-ATTR                  
100498     .                                                                    
100499     EJECT                                                                
100500 FBAK-TA-BORT-IDDISTR-TH    SECTION.                                      
100501     SKIP2                                                                
100502     MOVE MID-IDDISTR-TH-IN TO IDDISTR-TH-WS                              
100503     IF  WS-IDDISTR-TH = BYART-IDDISTR-RENOV-TH                           
100504        MOVE ZERO              TO BYART-IDDISTR-RENOV-TH                  
100505     END-IF                                                               
100506     MOVE MFS-FORMATETS-ATTR   TO MOD-IDDISTR-TH-IN-ATTR                  
100507     .                                                                    
100508     EJECT                                                                
100510 FBB-EV-TABORT-BYPRO-BYLEV SECTION.                                       
100600     SKIP2                                                                
100700     IF MID-IDPRODNR-IN = ALL '+'                                         
100800        CONTINUE                                                          
100900     ELSE                                                                 
101000        MOVE MID-IDPRODNR-IN        TO W-IDPRODNR                         
101100        PERFORM DB2-DLET-BYPRO                                            
101200        IF 904-KOD                                                        
101300           MOVE FEL-8 (SPRAK-IX)    TO MOD-TEMFSFEL                       
101400           MOVE MED-4 (SPRAK-IX)    TO MOD-TEMFSINF                       
101500        ELSE                                                              
101600           PERFORM S02-FLYTT-BYPRO-T-BILD                                 
101700           MOVE MFS-FORMATETS-ATTR     TO MOD-IDPRODNR-IN-ATTR            
101800        END-IF                                                            
101900     END-IF                                                               
102000     IF MID-BELEV = ALL '+'                                               
102100        CONTINUE                                                          
102200     ELSE                                                                 
102300        PERFORM DB2-DLET-BYLEV                                            
102400        IF 904-KOD                                                        
102500           MOVE FEL-8 (SPRAK-IX)    TO MOD-TEMFSFEL                       
102600           MOVE MED-4 (SPRAK-IX)    TO MOD-TEMFSINF                       
102700        ELSE                                                              
102800           PERFORM S03-FLYTT-BYLEV-T-BILD                                 
102900           MOVE MFS-FORMATETS-ATTR     TO MOD-BELEV-IN-ATTR               
103000        END-IF                                                            
103100     END-IF                                                               
103200     .                                                                    
103300     EJECT                                                                
103400 FC-SKAP-BYLEV-BYPRO-RENSA-INM   SECTION.                                 
103500     SKIP2                                                                
103600     IF MID-BELEV = ALL '+'                                               
103700        CONTINUE                                                          
103800     ELSE                                                                 
103900        PERFORM DB2-ISRT-BYLEV                                            
104000        PERFORM S03-FLYTT-BYLEV-T-BILD                                    
104100        MOVE MFS-FORMATETS-ATTR           TO MOD-BELEV-IN-ATTR            
104200     END-IF                                                               
104300     IF MID-IDPRODNR-IN = ALL '+'                                         
104400        CONTINUE                                                          
104500     ELSE                                                                 
104600        MOVE MID-IDPRODNR-IN             TO W-IDPRODNR                    
104700        PERFORM DB2-ISRT-BYPRO                                            
104800        PERFORM S02-FLYTT-BYPRO-T-BILD                                    
104900        MOVE MFS-FORMATETS-ATTR           TO MOD-IDPRODNR-IN-ATTR         
105000     END-IF                                                               
105100     PERFORM S04-RENSA-INMATAT                                            
105200     .                                                                    
105300     EJECT                                                                
105400 FD-KOLLA-INDATA-NYUPPL SECTION.                                          
105500     SKIP2                                                                
105600     IF MID-IDPRODNR-IN = ALL '+'                                         
105700        CONTINUE                                                          
105800     ELSE                                                                 
105900        IF MID-IDPRODNR-IN NUMERIC                                        
106000           MOVE MID-IDPRODNR-IN           TO W-IDPRODNR                   
106100                                             W-IDARTNR                    
106200           PERFORM IMS-GU-ARTC01                                          
106300           MOVE MFS-NUM-FAELT-RAETT       TO MOD-IDPRODNR-IN-ATTR         
106400           IF SEGMENT-SAKNAS                                              
106500              MOVE FEL-7 (SPRAK-IX)       TO MOD-TEMFSFEL                 
106600           END-IF                                                         
106700        ELSE                                                              
106800           MOVE NEJ                       TO SW-INDATA-OK                 
106900           MOVE FEL-ERR-IDPRODNR          TO WS-ERROR-UPDX                
106900           MOVE MFS-NUM-FAELT-FEL         TO MOD-IDPRODNR-IN-ATTR         
107000        END-IF                                                            
107100     END-IF                                                               
107200**                                                                        
107300     IF MID-KVLS-MAXCORE-IN  = ALL '+'                                    
107400        CONTINUE                                                          
107500     ELSE                                                                 
107600        IF MID-KVLS-MAXCORE-IN NUMERIC                                    
107700           MOVE MFS-NUM-FAELT-RAETT  TO MOD-KVLS-MAXCORE-IN-ATTR          
107800        ELSE                                                              
107900           MOVE NEJ                  TO SW-INDATA-OK                      
108000           MOVE MFS-NUM-FAELT-FEL    TO MOD-KVLS-MAXCORE-IN-ATTR          
108100           MOVE FEL-5 (SPRAK-IX)     TO MOD-TEMFSFEL                      
101300           MOVE FEL-ERR-FIELD       TO WS-ERROR-UPDX                      
108200        END-IF                                                            
108300     END-IF                                                               
108400**                                                                        
108500                                                                          
108600     IF MID-BELEV = ALL '+'                                               
108700        CONTINUE                                                          
108800     ELSE                                                                 
108900        MOVE MID-BELEV                 TO W-BELEV                         
109000        PERFORM DB2-SELECT-BYLEV                                          
109100        IF CURSOR-OK                                                      
109200           MOVE NEJ                    TO SW-INDATA-OK                    
109300           MOVE MFS-ALFA-FAELT-FEL     TO                                 
109400                                       MOD-BELEV-IN-ATTR                  
109500        ELSE                                                              
109600           MOVE MFS-ALFA-FAELT-RAETT   TO                                 
109700                                       MOD-BELEV-IN-ATTR                  
109800        END-IF                                                            
109900     END-IF                                                               
110000     .                                                                    
110100     EJECT                                                                
110200 FE-UPPDATERA-BYART SECTION.                                              
110300     SKIP3                                                                
110400     IF  MID-TEBYTNOT(1)      = ALL '+'                                   
110500     AND MID-TEBYTNOT(2)      = ALL '+'                                   
110600     AND MID-TEBYTNOT(3)      = ALL '+'                                   
110700     AND MID-TEBYTNOT(4)      = ALL '+'                                   
110800     AND MID-BETFLEV          = ALL '+'                                   
110900     AND MID-IDDISTR-RENOV-IN = ALL '+'                                   
111000     AND MID-IDDISTR-NDC-IN   = ALL '+'                                   
111100     AND MID-IDDISTR-PAC-IN   = ALL '+'                                   
111200     AND MID-IDDISTR-CAN-IN   = ALL '+'                                   
111300     AND MID-IDDISTR-AUS-IN   = ALL '+'                                   
111400     AND MID-IDDISTR-CHN-IN   = ALL '+'                                   
111500     AND MID-IDDISTR-KOR-IN   = ALL '+'                                   
111510     AND MID-IDDISTR-MY-IN    = ALL '+'                                   
111520     AND MID-IDDISTR-TW-IN    = ALL '+'                                   
111530     AND MID-IDDISTR-TH-IN    = ALL '+'                                   
111600     AND MID-KVLS-MAXCORE-IN  = ALL '+'                                   
111700        CONTINUE                                                          
111800     ELSE                                                                 
111900        PERFORM FEA-BEHANDLA-BYTNOT                                       
112000        IF MID-BETFLEV = ALL '+'                                          
112100           CONTINUE                                                       
112200        ELSE                                                              
112300           MOVE MID-BETFLEV          TO BYART-BETFLEV                     
112400           MOVE MFS-FORMATETS-ATTR   TO MOD-BETFLEV-ATTR                  
112500        END-IF                                                            
112600        IF MID-IDDISTR-RENOV-IN = ALL '+'                                 
112700           CONTINUE                                                       
112800        ELSE                                                              
112900           MOVE MID-IDDISTR-RENOV-IN  TO IDDISTR-RENOV-WS                 
113000           MOVE WS-IDDISTR-RENOV      TO BYART-IDDISTR-RENOV              
113100           MOVE MFS-FORMATETS-ATTR TO MOD-IDDISTR-RENOV-IN-ATTR           
113200        END-IF                                                            
113300        IF MID-IDDISTR-NDC-IN = ALL '+'                                   
113400           CONTINUE                                                       
113500        ELSE                                                              
113600           MOVE MID-IDDISTR-NDC-IN    TO IDDISTR-NDC-WS                   
113700           MOVE WS-IDDISTR-NDC        TO BYART-IDDISTR-RENOV-NDC          
113800           MOVE MFS-FORMATETS-ATTR TO MOD-IDDISTR-NDC-IN-ATTR             
113900        END-IF                                                            
114000        IF MID-IDDISTR-PAC-IN = ALL '+'                                   
114100           CONTINUE                                                       
114200        ELSE                                                              
114300           MOVE MID-IDDISTR-PAC-IN    TO IDDISTR-PAC-WS                   
114400           MOVE WS-IDDISTR-PAC        TO BYART-IDDISTR-RENOV-PAC          
114500           MOVE MFS-FORMATETS-ATTR TO MOD-IDDISTR-PAC-IN-ATTR             
114600        END-IF                                                            
114700                                                                          
114800        IF MID-IDDISTR-CAN-IN = ALL '+'                                   
114900           CONTINUE                                                       
115000        ELSE                                                              
115100           MOVE MID-IDDISTR-CAN-IN    TO IDDISTR-CAN-WS                   
115200           MOVE WS-IDDISTR-CAN        TO BYART-IDDISTR-RENOV-CAN          
115300           MOVE MFS-FORMATETS-ATTR TO MOD-IDDISTR-CAN-IN-ATTR             
115400        END-IF                                                            
115500                                                                          
115600        IF MID-IDDISTR-AUS-IN = ALL '+'                                   
115700           CONTINUE                                                       
115800        ELSE                                                              
115900           MOVE MID-IDDISTR-AUS-IN    TO IDDISTR-AUS-WS                   
116000           MOVE WS-IDDISTR-AUS        TO BYART-IDDISTR-RENOV-AUS          
116100           MOVE MFS-FORMATETS-ATTR TO MOD-IDDISTR-AUS-IN-ATTR             
116200        END-IF                                                            
116300                                                                          
116400        IF MID-IDDISTR-CHN-IN = ALL '+'                                   
116500           CONTINUE                                                       
116600        ELSE                                                              
116700           MOVE MID-IDDISTR-CHN-IN    TO IDDISTR-CHN-WS                   
116800           MOVE WS-IDDISTR-CHN        TO BYART-IDDISTR-RENOV-CHN          
116900           MOVE MFS-FORMATETS-ATTR TO MOD-IDDISTR-CHN-IN-ATTR             
117000        END-IF                                                            
117100                                                                          
117200        IF MID-IDDISTR-KOR-IN = ALL '+'                                   
117300           CONTINUE                                                       
117400        ELSE                                                              
117500           MOVE MID-IDDISTR-KOR-IN    TO IDDISTR-KOR-WS                   
117600           MOVE WS-IDDISTR-KOR        TO BYART-IDDISTR-RENOV-KOR          
117700           MOVE MFS-FORMATETS-ATTR TO MOD-IDDISTR-KOR-IN-ATTR             
117800        END-IF                                                            
117900                                                                          
117910        IF MID-IDDISTR-MY-IN = ALL '+'                                    
117920           CONTINUE                                                       
117930        ELSE                                                              
117940           MOVE MID-IDDISTR-MY-IN    TO IDDISTR-MY-WS                     
117950           MOVE WS-IDDISTR-MY         TO BYART-IDDISTR-RENOV-MY           
117960           MOVE MFS-FORMATETS-ATTR TO MOD-IDDISTR-MY-IN-ATTR              
117970        END-IF                                                            
117980                                                                          
117990        IF MID-IDDISTR-TW-IN = ALL '+'                                    
117991           CONTINUE                                                       
117992        ELSE                                                              
117993           MOVE MID-IDDISTR-TW-IN    TO IDDISTR-TW-WS                     
117994           MOVE WS-IDDISTR-TW         TO BYART-IDDISTR-RENOV-TW           
117995           MOVE MFS-FORMATETS-ATTR TO MOD-IDDISTR-TW-IN-ATTR              
117996        END-IF                                                            
117997                                                                          
117998        IF MID-IDDISTR-TH-IN = ALL '+'                                    
117999           CONTINUE                                                       
118000        ELSE                                                              
118001           MOVE MID-IDDISTR-TH-IN    TO IDDISTR-TH-WS                     
118002           MOVE WS-IDDISTR-TH         TO BYART-IDDISTR-RENOV-TH           
118003           MOVE MFS-FORMATETS-ATTR TO MOD-IDDISTR-TH-IN-ATTR              
118004        END-IF                                                            
118005                                                                          
118010        IF MID-KVLS-MAXCORE-IN = ALL '+'                                  
118100           CONTINUE                                                       
118200        ELSE                                                              
118300           MOVE MID-KVLS-MAXCORE-IN   TO BYART-KVLS-MAXCORE               
118400           MOVE MFS-FORMATETS-ATTR    TO MOD-KVLS-MAXCORE-IN-ATTR         
118500        END-IF                                                            
118600                                                                          
118700        PERFORM S01-FLYTT-BYART-T-BILD                                    
118800        PERFORM DB2-UPDATE-BYART                                          
118900     END-IF                                                               
119000     .                                                                    
119100     EJECT                                                                
119200 FEA-BEHANDLA-BYTNOT  SECTION.                                            
119300     SKIP3                                                                
119400     MOVE                                  +1 TO RAD-INDX                 
119500     PERFORM UNTIL RAD-INDX > 4                                           
119600        IF MID-TEBYTNOT (RAD-INDX) = ALL '+'                              
119700           CONTINUE                                                       
119800        ELSE                                                              
119900           IF RAD-INDX = 1                                                
120000              MOVE MID-TEBYTNOT(RAD-INDX)  TO BYART-TEBYTNOT1             
120100              MOVE MFS-FORMATETS-ATTR    TO MOD-TEBYTNOT-ATTR(1)          
120200           ELSE                                                           
120300              IF RAD-INDX = 2                                             
120400                 MOVE MID-TEBYTNOT(RAD-INDX)  TO                          
120500                                              BYART-TEBYTNOT2             
120600                 MOVE MFS-FORMATETS-ATTR  TO MOD-TEBYTNOT-ATTR(2)         
120700              ELSE                                                        
120800                 IF RAD-INDX = 3                                          
120900                    MOVE MID-TEBYTNOT(RAD-INDX)  TO                       
121000                                              BYART-TEBYTNOT3             
121100                    MOVE MFS-FORMATETS-ATTR  TO                           
121200                                         MOD-TEBYTNOT-ATTR(3)             
121300                 ELSE                                                     
121400                    MOVE MID-TEBYTNOT(RAD-INDX)  TO                       
121500                                              BYART-TEBYTNOT4             
121600                    MOVE MFS-FORMATETS-ATTR  TO                           
121700                                         MOD-TEBYTNOT-ATTR(4)             
121800                 END-IF                                                   
121900              END-IF                                                      
122000           END-IF                                                         
122100        END-IF                                                            
122200        ADD +1                                TO RAD-INDX                 
122300     END-PERFORM                                                          
122400     .                                                                    
122500     EJECT                                                                
122600 FF-SKAPA-BYART   SECTION.                                                
122700     SKIP2                                                                
122800     INITIALIZE BYART-BYART                                               
122900     IF MID-TEBYTNOT(1) = ALL '+'                                         
123000        CONTINUE                                                          
123100     ELSE                                                                 
123200        MOVE MID-TEBYTNOT(1)              TO BYART-TEBYTNOT1              
123300        MOVE MFS-FORMATETS-ATTR           TO MOD-TEBYTNOT-ATTR(1)         
123400     END-IF                                                               
123500     IF MID-TEBYTNOT(2) = ALL '+'                                         
123600        CONTINUE                                                          
123700     ELSE                                                                 
123800        MOVE MID-TEBYTNOT(2)              TO BYART-TEBYTNOT2              
123900        MOVE MFS-FORMATETS-ATTR           TO MOD-TEBYTNOT-ATTR(2)         
124000     END-IF                                                               
124100     IF MID-TEBYTNOT(3) = ALL '+'                                         
124200        CONTINUE                                                          
124300     ELSE                                                                 
124400        MOVE MID-TEBYTNOT(3)              TO BYART-TEBYTNOT3              
124500        MOVE MFS-FORMATETS-ATTR           TO MOD-TEBYTNOT-ATTR(3)         
124600     END-IF                                                               
124700     IF MID-TEBYTNOT(4) = ALL '+'                                         
124800        CONTINUE                                                          
124900     ELSE                                                                 
125000        MOVE MID-TEBYTNOT(4)              TO BYART-TEBYTNOT4              
125100        MOVE MFS-FORMATETS-ATTR           TO MOD-TEBYTNOT-ATTR(4)         
125200     END-IF                                                               
125300     IF MID-BETFLEV = ALL '+'                                             
125400        CONTINUE                                                          
125500     ELSE                                                                 
125600        MOVE MID-BETFLEV                  TO BYART-BETFLEV                
125700        MOVE MFS-FORMATETS-ATTR           TO MOD-BETFLEV-ATTR             
125800     END-IF                                                               
125900     IF MID-IDDISTR-RENOV-IN = ALL '+'                                    
126000        CONTINUE                                                          
126100     ELSE                                                                 
126200        MOVE MID-IDDISTR-RENOV-IN    TO IDDISTR-RENOV-WS                  
126300        MOVE  WS-IDDISTR-RENOV       TO BYART-IDDISTR-RENOV               
126400        MOVE MFS-FORMATETS-ATTR      TO MOD-IDDISTR-RENOV-IN-ATTR         
126500     END-IF                                                               
126600     IF MID-IDDISTR-NDC-IN = ALL '+'                                      
126700        CONTINUE                                                          
126800     ELSE                                                                 
126900        MOVE MID-IDDISTR-NDC-IN      TO IDDISTR-NDC-WS                    
127000        MOVE  WS-IDDISTR-NDC         TO BYART-IDDISTR-RENOV-NDC           
127100        MOVE MFS-FORMATETS-ATTR      TO MOD-IDDISTR-NDC-IN-ATTR           
127200     END-IF                                                               
127300                                                                          
127400     IF MID-IDDISTR-PAC-IN = ALL '+'                                      
127500        CONTINUE                                                          
127600     ELSE                                                                 
127700        MOVE MID-IDDISTR-PAC-IN      TO IDDISTR-PAC-WS                    
127800        MOVE  WS-IDDISTR-PAC         TO BYART-IDDISTR-RENOV-PAC           
127900        MOVE MFS-FORMATETS-ATTR      TO MOD-IDDISTR-PAC-IN-ATTR           
128000     END-IF                                                               
128100                                                                          
128200     IF MID-IDDISTR-CAN-IN = ALL '+'                                      
128300        CONTINUE                                                          
128400     ELSE                                                                 
128500        MOVE MID-IDDISTR-CAN-IN      TO IDDISTR-CAN-WS                    
128600        MOVE  WS-IDDISTR-CAN         TO BYART-IDDISTR-RENOV-CAN           
128700        MOVE MFS-FORMATETS-ATTR      TO MOD-IDDISTR-CAN-IN-ATTR           
128800     END-IF                                                               
128900                                                                          
129000     IF MID-IDDISTR-AUS-IN = ALL '+'                                      
129100        CONTINUE                                                          
129200     ELSE                                                                 
129300        MOVE MID-IDDISTR-AUS-IN      TO IDDISTR-AUS-WS                    
129400        MOVE  WS-IDDISTR-AUS         TO BYART-IDDISTR-RENOV-AUS           
129500        MOVE MFS-FORMATETS-ATTR      TO MOD-IDDISTR-AUS-IN-ATTR           
129600     END-IF                                                               
129700                                                                          
129800     IF MID-IDDISTR-CHN-IN = ALL '+'                                      
129900        CONTINUE                                                          
130000     ELSE                                                                 
130100        MOVE MID-IDDISTR-CHN-IN      TO IDDISTR-CHN-WS                    
130200        MOVE  WS-IDDISTR-CHN         TO BYART-IDDISTR-RENOV-CHN           
130300        MOVE MFS-FORMATETS-ATTR      TO MOD-IDDISTR-CHN-IN-ATTR           
130400     END-IF                                                               
130500                                                                          
130600     IF MID-IDDISTR-KOR-IN = ALL '+'                                      
130700        CONTINUE                                                          
130800     ELSE                                                                 
130900        MOVE MID-IDDISTR-KOR-IN      TO IDDISTR-KOR-WS                    
131000        MOVE  WS-IDDISTR-KOR         TO BYART-IDDISTR-RENOV-KOR           
131100        MOVE MFS-FORMATETS-ATTR      TO MOD-IDDISTR-KOR-IN-ATTR           
131200     END-IF                                                               
131300                                                                          
131310     IF MID-IDDISTR-MY-IN = ALL '+'                                       
131320        CONTINUE                                                          
131330     ELSE                                                                 
131340        MOVE MID-IDDISTR-MY-IN      TO IDDISTR-MY-WS                      
131350        MOVE  WS-IDDISTR-MY          TO BYART-IDDISTR-RENOV-MY            
131360        MOVE MFS-FORMATETS-ATTR      TO MOD-IDDISTR-MY-IN-ATTR            
131370     END-IF                                                               
131380                                                                          
131390     IF MID-IDDISTR-TW-IN = ALL '+'                                       
131391        CONTINUE                                                          
131392     ELSE                                                                 
131393        MOVE MID-IDDISTR-TW-IN      TO IDDISTR-TW-WS                      
131394        MOVE  WS-IDDISTR-TW          TO BYART-IDDISTR-RENOV-TW            
131395        MOVE MFS-FORMATETS-ATTR      TO MOD-IDDISTR-TW-IN-ATTR            
131396     END-IF                                                               
131397                                                                          
131398     IF MID-IDDISTR-TH-IN = ALL '+'                                       
131399        CONTINUE                                                          
131400     ELSE                                                                 
131401        MOVE MID-IDDISTR-TH-IN      TO IDDISTR-TH-WS                      
131402        MOVE  WS-IDDISTR-TH          TO BYART-IDDISTR-RENOV-TH            
131403        MOVE MFS-FORMATETS-ATTR      TO MOD-IDDISTR-TH-IN-ATTR            
131404     END-IF                                                               
131405                                                                          
131410     IF MID-KVLS-MAXCORE-IN = ALL '+'                                     
131500        CONTINUE                                                          
131600     ELSE                                                                 
131700        MOVE MID-KVLS-MAXCORE-IN   TO BYART-KVLS-MAXCORE                  
131800        MOVE MFS-FORMATETS-ATTR    TO MOD-KVLS-MAXCORE-IN-ATTR            
131900     END-IF                                                               
132000                                                                          
132100     PERFORM S01-FLYTT-BYART-T-BILD                                       
132200     PERFORM DB2-ISRT-BYART                                               
132300     .                                                                    
132400     EJECT                                                                
132500 FG-LAES-IN-IGEN SECTION.                                                 
132600     SKIP2                                                                
132700     MOVE 1                              TO RAD-INDX                      
132800     PERFORM UNTIL RAD-INDX > 4                                           
132900        IF MID-TEBYTNOT(RAD-INDX) = ALL '+'                               
133000           CONTINUE                                                       
133100        ELSE                                                              
133200           MOVE MFS-ADD-LAES-IN-FAELT  TO                                 
133300                                      MOD-TEBYTNOT-ATTR(RAD-INDX)         
133400        END-IF                                                            
133500        ADD +1                         TO RAD-INDX                        
133600     END-PERFORM                                                          
133700     IF MID-BETFLEV = ALL '+'                                             
133800        CONTINUE                                                          
133900     ELSE                                                                 
134000        MOVE MFS-ADD-LAES-IN-FAELT        TO MOD-BETFLEV-ATTR             
134100     END-IF                                                               
134200     IF MID-IDPRODNR-IN = ALL '+'                                         
134300        CONTINUE                                                          
134400     ELSE                                                                 
134500        MOVE MFS-ADD-LAES-IN-FAELT        TO MOD-IDPRODNR-IN-ATTR         
134600     END-IF                                                               
134700     IF MID-IDDISTR-RENOV-IN = ALL '+'                                    
134800        CONTINUE                                                          
134900     ELSE                                                                 
135000        MOVE MFS-ADD-LAES-IN-FAELT   TO MOD-IDDISTR-RENOV-IN-ATTR         
135100     END-IF                                                               
135200     IF MID-IDDISTR-NDC-IN = ALL '+'                                      
135300        CONTINUE                                                          
135400     ELSE                                                                 
135500        MOVE MFS-ADD-LAES-IN-FAELT   TO MOD-IDDISTR-NDC-IN-ATTR           
135600     END-IF                                                               
135700     IF MID-IDDISTR-PAC-IN = ALL '+'                                      
135800        CONTINUE                                                          
135900     ELSE                                                                 
136000        MOVE MFS-ADD-LAES-IN-FAELT   TO MOD-IDDISTR-PAC-IN-ATTR           
136100     END-IF                                                               
136200                                                                          
136300     IF MID-IDDISTR-CAN-IN = ALL '+'                                      
136400        CONTINUE                                                          
136500     ELSE                                                                 
136600        MOVE MFS-ADD-LAES-IN-FAELT   TO MOD-IDDISTR-CAN-IN-ATTR           
136700     END-IF                                                               
136800                                                                          
136900     IF MID-IDDISTR-AUS-IN = ALL '+'                                      
137000        CONTINUE                                                          
137100     ELSE                                                                 
137200        MOVE MFS-ADD-LAES-IN-FAELT   TO MOD-IDDISTR-AUS-IN-ATTR           
137300     END-IF                                                               
137400                                                                          
137500     IF MID-IDDISTR-CHN-IN = ALL '+'                                      
137600        CONTINUE                                                          
137700     ELSE                                                                 
137800        MOVE MFS-ADD-LAES-IN-FAELT   TO MOD-IDDISTR-CHN-IN-ATTR           
137900     END-IF                                                               
138000                                                                          
138100     IF MID-IDDISTR-KOR-IN = ALL '+'                                      
138200        CONTINUE                                                          
138300     ELSE                                                                 
138400        MOVE MFS-ADD-LAES-IN-FAELT   TO MOD-IDDISTR-KOR-IN-ATTR           
138500     END-IF                                                               
138600                                                                          
138610     IF MID-IDDISTR-MY-IN = ALL '+'                                       
138620        CONTINUE                                                          
138630     ELSE                                                                 
138640        MOVE MFS-ADD-LAES-IN-FAELT   TO MOD-IDDISTR-MY-IN-ATTR            
138650     END-IF                                                               
138660                                                                          
138670     IF MID-IDDISTR-TW-IN = ALL '+'                                       
138680        CONTINUE                                                          
138690     ELSE                                                                 
138691        MOVE MFS-ADD-LAES-IN-FAELT   TO MOD-IDDISTR-TW-IN-ATTR            
138692     END-IF                                                               
138693                                                                          
138694     IF MID-IDDISTR-TH-IN = ALL '+'                                       
138695        CONTINUE                                                          
138696     ELSE                                                                 
138697        MOVE MFS-ADD-LAES-IN-FAELT   TO MOD-IDDISTR-TH-IN-ATTR            
138698     END-IF                                                               
138699                                                                          
138700     IF MID-BELEV = ALL '+'                                               
138800        CONTINUE                                                          
138900     ELSE                                                                 
139000        MOVE MFS-ADD-LAES-IN-FAELT        TO MOD-BELEV-IN-ATTR            
139100     END-IF                                                               
139200     .                                                                    
139300     EJECT                                                                
139400 FH-EV-UPPD-BYTNOT-BETFLEV SECTION.                                       
139500     SKIP2                                                                
139600     PERFORM FHB-CHECK-UPDATE-MAXCORE                                     
139700     IF INDATA-OK                                                         
139800       MOVE 1                              TO RAD-INDX                    
139900       IF MID-BETFLEV = ALL '+'                                           
140000          PERFORM UNTIL RAD-INDX > 4                                      
140100             IF MID-TEBYTNOT(RAD-INDX) = ALL '+'                          
140200                CONTINUE                                                  
140300             ELSE                                                         
140400                PERFORM DB2-SELECT-BYART                                  
140500                IF RADER-FINNS                                            
140600                   PERFORM FHA-UPPD-BYTNOT-BETFLEV                        
140700                ELSE                                                      
140800                   IF 904-KOD                                             
140900                      MOVE FEL-8 (SPRAK-IX)    TO MOD-TEMFSFEL            
141000                      MOVE MED-4 (SPRAK-IX)    TO MOD-TEMFSINF            
141100                      ADD +4                   TO RAD-INDX                
141200                   END-IF                                                 
141300                END-IF                                                    
141400             END-IF                                                       
141500             ADD +1                        TO RAD-INDX                    
141600          END-PERFORM                                                     
141700       ELSE                                                               
141800          PERFORM DB2-SELECT-BYART                                        
141900          IF RADER-FINNS                                                  
142000             MOVE MID-BETFLEV           TO BYART-BETFLEV                  
142100             PERFORM FHA-UPPD-BYTNOT-BETFLEV                              
142200          ELSE                                                            
142300             IF 904-KOD                                                   
142400                MOVE FEL-8 (SPRAK-IX)    TO MOD-TEMFSFEL                  
142500                MOVE MED-4 (SPRAK-IX)    TO MOD-TEMFSINF                  
142600             END-IF                                                       
142700          END-IF                                                          
142800       END-IF                                                             
142900     END-IF                                                               
143000     .                                                                    
143100     EJECT                                                                
143200 FHA-UPPD-BYTNOT-BETFLEV SECTION.                                         
143300     SKIP2                                                                
143400     PERFORM UNTIL RAD-INDX > 4                                           
143500        IF MID-TEBYTNOT(RAD-INDX) = ALL '+'                               
143600           CONTINUE                                                       
143700        ELSE                                                              
143800           IF RAD-INDX = 1                                                
143900              MOVE MID-TEBYTNOT(RAD-INDX)  TO BYART-TEBYTNOT1             
144000              MOVE MFS-FORMATETS-ATTR    TO MOD-TEBYTNOT-ATTR(1)          
144100           ELSE                                                           
144200              IF RAD-INDX = 2                                             
144300                 MOVE MID-TEBYTNOT(RAD-INDX)  TO                          
144400                                              BYART-TEBYTNOT2             
144500                 MOVE MFS-FORMATETS-ATTR  TO MOD-TEBYTNOT-ATTR(2)         
144600              ELSE                                                        
144700                 IF RAD-INDX = 3                                          
144800                    MOVE MID-TEBYTNOT(RAD-INDX)  TO                       
144900                                              BYART-TEBYTNOT3             
145000                    MOVE MFS-FORMATETS-ATTR  TO                           
145100                                         MOD-TEBYTNOT-ATTR(3)             
145200                 ELSE                                                     
145300                    MOVE MID-TEBYTNOT(RAD-INDX)  TO                       
145400                                              BYART-TEBYTNOT4             
145500                    MOVE MFS-FORMATETS-ATTR  TO                           
145600                                         MOD-TEBYTNOT-ATTR(4)             
145700                 END-IF                                                   
145800              END-IF                                                      
145900           END-IF                                                         
146000        END-IF                                                            
146100        ADD +1                        TO RAD-INDX                         
146200     END-PERFORM                                                          
146300     PERFORM S01-FLYTT-BYART-T-BILD                                       
146400     PERFORM DB2-UPDATE-BYART                                             
146500     MOVE MED-1 (SPRAK-IX)            TO MOD-TEMFSINF                     
146600     .                                                                    
146700     EJECT                                                                
146800 FHB-CHECK-UPDATE-MAXCORE  SECTION.                                       
146900     MOVE JA                         TO SW-INDATA-OK                      
147000     IF MID-KVLS-MAXCORE-IN = ALL '+'                                     
147100       CONTINUE                                                           
147200     ELSE                                                                 
147300       IF MID-KVLS-MAXCORE-IN IS NUMERIC                                  
147400         MOVE MFS-NUM-FAELT-RAETT    TO MOD-KVLS-MAXCORE-IN-ATTR          
147500         PERFORM DB2-SELECT-BYART                                         
147600         IF RADER-FINNS                                                   
147700           MOVE MID-KVLS-MAXCORE-IN  TO BYART-KVLS-MAXCORE                
147800                                        MOD-KVLS-MAXCORE-UT               
147900           PERFORM DB2-UPDATE-BYART                                       
148000           MOVE MED-1 (SPRAK-IX)     TO MOD-TEMFSINF                      
148100           MOVE MFS-RENSA-FAELT      TO MOD-KVLS-MAXCORE-IN               
148200         ELSE                                                             
148300           IF 904-KOD                                                     
148400             MOVE FEL-8 (SPRAK-IX)   TO MOD-TEMFSFEL                      
148500             MOVE MED-4 (SPRAK-IX)   TO MOD-TEMFSINF                      
148600           END-IF                                                         
148700         END-IF                                                           
148800       ELSE                                                               
148900         MOVE NEJ                    TO SW-INDATA-OK                      
149000         MOVE MFS-NUM-FAELT-FEL      TO MOD-KVLS-MAXCORE-IN-ATTR          
149100         MOVE FEL-5 (SPRAK-IX)       TO MOD-TEMFSFEL                      
149100         MOVE FEL-ERR-FIELD          TO WS-ERROR-UPDX                     
149200       END-IF                                                             
149300     END-IF                                                               
149400     .                                                                    
149500     EJECT                                                                
149600 G-KOLLA-ATT-INGET-IFYLLT  SECTION.                                       
149700     SKIP2                                                                
149800     MOVE NEJ                         TO SW-INMATAT                       
149900     MOVE +1                             TO RAD-INDX                      
150000     PERFORM UNTIL RAD-INDX > +4                                          
150100        IF MID-TEBYTNOT(RAD-INDX) = ALL '+'                               
150200           CONTINUE                                                       
150300        ELSE                                                              
150400           MOVE JA                     TO SW-INMATAT                      
150500           MOVE MFS-ADD-LAES-IN-FAELT  TO                                 
150600                                      MOD-TEBYTNOT-ATTR(RAD-INDX)         
150700        END-IF                                                            
150800        ADD +1                         TO RAD-INDX                        
150900     END-PERFORM                                                          
151000     IF MID-BETFLEV = ALL '+'                                             
151100        CONTINUE                                                          
151200     ELSE                                                                 
151300        MOVE JA                           TO SW-INMATAT                   
151400        MOVE MFS-ADD-LAES-IN-FAELT        TO MOD-BETFLEV-ATTR             
151500     END-IF                                                               
151600     IF MID-UPPDATERINGSSORT = ALL '+'                                    
151700        CONTINUE                                                          
151800     ELSE                                                                 
151900        MOVE JA                           TO SW-INMATAT                   
152000        MOVE MFS-ADD-LAES-IN-FAELT        TO                              
152100                                       MOD-UPPDATERINGSSORT-ATTR          
152200     END-IF                                                               
152300     IF MID-IDPRODNR-IN = ALL '+'                                         
152400        CONTINUE                                                          
152500     ELSE                                                                 
152600        MOVE JA                           TO SW-INMATAT                   
152700        MOVE MFS-ADD-LAES-IN-FAELT        TO MOD-IDPRODNR-IN-ATTR         
152800     END-IF                                                               
152900     IF MID-IDDISTR-RENOV-IN = ALL '+'                                    
153000        CONTINUE                                                          
153100     ELSE                                                                 
153200        MOVE JA                      TO SW-INMATAT                        
153300        MOVE MFS-ADD-LAES-IN-FAELT   TO MOD-IDDISTR-RENOV-IN-ATTR         
153400     END-IF                                                               
153500     IF MID-IDDISTR-NDC-IN = ALL '+'                                      
153600        CONTINUE                                                          
153700     ELSE                                                                 
153800        MOVE JA                      TO SW-INMATAT                        
153900        MOVE MFS-ADD-LAES-IN-FAELT   TO MOD-IDDISTR-NDC-IN-ATTR           
154000     END-IF                                                               
154100     IF MID-IDDISTR-PAC-IN = ALL '+'                                      
154200        CONTINUE                                                          
154300     ELSE                                                                 
154400        MOVE JA                      TO SW-INMATAT                        
154500        MOVE MFS-ADD-LAES-IN-FAELT   TO MOD-IDDISTR-PAC-IN-ATTR           
154600     END-IF                                                               
154700                                                                          
154800     IF MID-IDDISTR-CAN-IN = ALL '+'                                      
154900        CONTINUE                                                          
155000     ELSE                                                                 
155100        MOVE JA                      TO SW-INMATAT                        
155200        MOVE MFS-ADD-LAES-IN-FAELT   TO MOD-IDDISTR-CAN-IN-ATTR           
155300     END-IF                                                               
155400                                                                          
155500     IF MID-IDDISTR-AUS-IN = ALL '+'                                      
155600        CONTINUE                                                          
155700     ELSE                                                                 
155800        MOVE JA                      TO SW-INMATAT                        
155900        MOVE MFS-ADD-LAES-IN-FAELT   TO MOD-IDDISTR-AUS-IN-ATTR           
156000     END-IF                                                               
156100                                                                          
156200     IF MID-IDDISTR-CHN-IN = ALL '+'                                      
156300        CONTINUE                                                          
156400     ELSE                                                                 
156500        MOVE JA                      TO SW-INMATAT                        
156600        MOVE MFS-ADD-LAES-IN-FAELT   TO MOD-IDDISTR-CHN-IN-ATTR           
156700     END-IF                                                               
156800                                                                          
156900     IF MID-IDDISTR-KOR-IN = ALL '+'                                      
157000        CONTINUE                                                          
157100     ELSE                                                                 
157200        MOVE JA                      TO SW-INMATAT                        
157300        MOVE MFS-ADD-LAES-IN-FAELT   TO MOD-IDDISTR-KOR-IN-ATTR           
157400     END-IF                                                               
157500                                                                          
157510     IF MID-IDDISTR-MY-IN = ALL '+'                                       
157520        CONTINUE                                                          
157530     ELSE                                                                 
157540        MOVE JA                      TO SW-INMATAT                        
157550        MOVE MFS-ADD-LAES-IN-FAELT   TO MOD-IDDISTR-MY-IN-ATTR            
157560     END-IF                                                               
157570                                                                          
157580     IF MID-IDDISTR-TW-IN = ALL '+'                                       
157590        CONTINUE                                                          
157591     ELSE                                                                 
157592        MOVE JA                      TO SW-INMATAT                        
157593        MOVE MFS-ADD-LAES-IN-FAELT   TO MOD-IDDISTR-TW-IN-ATTR            
157594     END-IF                                                               
157595                                                                          
157596     IF MID-IDDISTR-TH-IN = ALL '+'                                       
157597        CONTINUE                                                          
157598     ELSE                                                                 
157599        MOVE JA                      TO SW-INMATAT                        
157600        MOVE MFS-ADD-LAES-IN-FAELT   TO MOD-IDDISTR-TH-IN-ATTR            
157601     END-IF                                                               
157602                                                                          
157610     IF MID-BELEV = ALL '+'                                               
157700        CONTINUE                                                          
157800     ELSE                                                                 
157900        MOVE JA                           TO SW-INMATAT                   
158000        MOVE MFS-ADD-LAES-IN-FAELT        TO MOD-BELEV-IN-ATTR            
158100     END-IF                                                               
158200                                                                          
158300     IF MID-KVLS-MAXCORE-IN = ALL '+'                                     
158400        CONTINUE                                                          
158500     ELSE                                                                 
158600        MOVE JA                      TO SW-INMATAT                        
158700        MOVE MFS-ADD-LAES-IN-FAELT   TO MOD-KVLS-MAXCORE-IN-ATTR          
158800     END-IF                                                               
158900     .                                                                    
159000     EJECT                                                                
159100 I-VISA-SIDAN SECTION.                                                    
159200     SKIP2                                                                
159300     IF PULS-TRANS                                                        
159400        PERFORM DB2-SELECT-BYART                                          
159500        IF RADER-FINNS                                                    
159600           PERFORM IMS-GU-BENA11                                          
159700           IF SEGMENT-FINNS                                               
159800              MOVE BENA11-TEXT-BEART TO                                   
159900                                       MOD-BEART-SVE                      
160000           END-IF                                                         
160100           MOVE W-IDARTNR-BYT TO W-IDARTNR                                
160200           IF BYT16-RADIO                                                 
160300             ADD +1000 TO W-IDARTNR                                       
160400           ELSE                                                           
160500             ADD +6000 TO W-IDARTNR                                       
160600           END-IF                                                         
160700           PERFORM IMS-GU-WDK711                                          
160800           IF SEGMENT-FINNS                                               
160900             MOVE SLAG-KVLS TO MOD-KVLS                                   
161000           END-IF                                                         
161100           PERFORM S01-FLYTT-BYART-T-BILD                                 
161200           PERFORM S02-FLYTT-BYPRO-T-BILD                                 
161300           PERFORM S03-FLYTT-BYLEV-T-BILD                                 
161400           PERFORM S04-RENSA-INMATAT                                      
161500        ELSE                                                              
161600           PERFORM S05-RENSA-SIDAN                                        
161700           IF 904-KOD                                                     
161800              MOVE FEL-8 (SPRAK-IX) TO MOD-TEMFSFEL                       
161900              MOVE MED-4 (SPRAK-IX) TO MOD-TEMFSINF                       
162000           ELSE                                                           
162100              MOVE FEL-3 (SPRAK-IX) TO MOD-TEMFSFEL                       
162200              MOVE SPACE            TO MOD-TEMFSINF                       
162300           END-IF                                                         
162400        END-IF                                                            
162500     ELSE                                                                 
162600*         (VOLVO-VISION-TRANS)                                            
162700        MOVE MID2-IDARTNR-NEXT      TO W-IDPRODNR                         
162800        PERFORM S02-FLYTT-BYPRO-T-BILD                                    
162900     END-IF                                                               
163000     .                                                                    
163100     EJECT                                                                
163200 S01-FLYTT-BYART-T-BILD SECTION.                                          
163300     SKIP2                                                                
163400     MOVE BYART-BETFLEV            TO MOD-BETFLEV                         
163500     MOVE BYART-TEBYTNOT1          TO MOD-TEBYTNOT (1)                    
163600     MOVE BYART-TEBYTNOT2          TO MOD-TEBYTNOT (2)                    
163700     MOVE BYART-TEBYTNOT3          TO MOD-TEBYTNOT (3)                    
163800     MOVE BYART-TEBYTNOT4          TO MOD-TEBYTNOT (4)                    
163900     MOVE BYART-IDDISTR-RENOV      TO MOD-IDDISTR-RENOV                   
164000     MOVE BYART-IDDISTR-RENOV-NDC  TO MOD-IDDISTR-NDC                     
164100     MOVE BYART-IDDISTR-RENOV-PAC  TO MOD-IDDISTR-PAC                     
164200     MOVE BYART-IDDISTR-RENOV-CAN  TO MOD-IDDISTR-CAN                     
164300     MOVE BYART-IDDISTR-RENOV-AUS  TO MOD-IDDISTR-AUS                     
164400     MOVE BYART-IDDISTR-RENOV-CHN  TO MOD-IDDISTR-CHN                     
164500     MOVE BYART-IDDISTR-RENOV-KOR  TO MOD-IDDISTR-KOR                     
164510     MOVE BYART-IDDISTR-RENOV-MY   TO MOD-IDDISTR-MY                      
164520     MOVE BYART-IDDISTR-RENOV-TW   TO MOD-IDDISTR-TW                      
164530     MOVE BYART-IDDISTR-RENOV-TH   TO MOD-IDDISTR-TH                      
164600     MOVE BYART-KVLS-MAXCORE       TO MOD-KVLS-MAXCORE-UT                 
164700     .                                                                    
164800     EJECT                                                                
164900 S02-FLYTT-BYPRO-T-BILD SECTION.                                          
165000     SKIP1                                                                
165100     PERFORM DB2-DCL-OPN-CRS-BYPRO                                        
165200     PERFORM DB2-FETCH-BYPRO                                              
165300     IF RADER-FINNS                                                       
165400        PERFORM S02A-FLYTTA-FRA-BYPRO                                     
165500        PERFORM DB2-CLOSE-BYPRO-CRS                                       
165600     ELSE                                                                 
165700        PERFORM S02B-RENSA-BYPRO                                          
165800     END-IF                                                               
165900     .                                                                    
166000     EJECT                                                                
166100 S02A-FLYTTA-FRA-BYPRO SECTION.                                           
166200     SKIP2                                                                
166300     IF PULS-TRANS                                                        
166400        MOVE BYPRO-IDARTNR            TO MOD-IDPRODNR-LO                  
166500        MOVE +1                       TO RAD-INDX                         
166600        PERFORM UNTIL NOT RADER-FINNS                                     
166700        OR RAD-INDX > +18                                                 
166800           MOVE BYPRO-IDARTNR         TO MOD-IDARTNR (RAD-INDX)           
166900           PERFORM DB2-FETCH-BYPRO                                        
167000           ADD +1                     TO RAD-INDX                         
167100        END-PERFORM                                                       
167200        IF RADER-FINNS                                                    
167300           IF MFS-UPDATE                                                  
167400              CONTINUE                                                    
167500           ELSE                                                           
167600              MOVE MED-2 (SPRAK-IX)   TO MOD-TEMFSINF                     
167700           END-IF                                                         
167800           MOVE BYPRO-IDARTNR         TO MOD-IDPRODNR-HI                  
167900        ELSE                                                              
168000           MOVE ZERO                  TO MOD-IDPRODNR-HI                  
168100           PERFORM UNTIL RAD-INDX > +18                                   
168200              MOVE MFS-RENSA-FAELT    TO MOD-IDARTNR (RAD-INDX)           
168300              ADD +1                  TO RAD-INDX                         
168400           END-PERFORM                                                    
168500        END-IF                                                            
168600     ELSE                                                                 
168700*         (VOLVO-VISION-TRANS)                                            
168800        MOVE +1                       TO RAD-INDX                         
168900        PERFORM UNTIL NOT RADER-FINNS                                     
169000        OR RAD-INDX > +13                                                 
169100           MOVE BYPRO-IDARTNR         TO MOD2-IDARTNR (RAD-INDX)          
169200           PERFORM DB2-FETCH-BYPRO                                        
169300           ADD +1                     TO RAD-INDX                         
169400        END-PERFORM                                                       
169500        SUBTRACT +1 FROM RAD-INDX GIVING MOD2-COUNTER                     
169600        IF RADER-FINNS                                                    
169700           MOVE BYPRO-IDARTNR         TO MOD2-IDARTNR-NEXT                
169800        ELSE                                                              
169900           MOVE ZERO                  TO MOD2-IDARTNR-NEXT                
170000           PERFORM UNTIL RAD-INDX > +13                                   
170100              MOVE ZERO               TO MOD2-IDARTNR (RAD-INDX)          
170200              ADD +1                  TO RAD-INDX                         
170300           END-PERFORM                                                    
170400        END-IF                                                            
170500     END-IF                                                               
170600     .                                                                    
170700     EJECT                                                                
170800 S02B-RENSA-BYPRO SECTION.                                                
170900     SKIP2                                                                
171000     IF PULS-TRANS                                                        
171100        MOVE ZERO                     TO MOD-IDPRODNR-LO                  
171200                                            MOD-IDPRODNR-HI               
171300        MOVE +1                       TO RAD-INDX                         
171400        PERFORM UNTIL RAD-INDX > +18                                      
171500           MOVE MFS-RENSA-FAELT       TO MOD-IDARTNR (RAD-INDX)           
171600           ADD +1                     TO RAD-INDX                         
171700        END-PERFORM                                                       
171800     ELSE                                                                 
171900*         (VOLVO-VISION-TRANS)                                            
172000        MOVE 'B10'                    TO MOD2-IDMFSFEL                    
172100        MOVE ZERO                     TO MOD2-IDARTNR-NEXT                
172200                                         MOD2-COUNTER                     
172300        MOVE +1                       TO RAD-INDX                         
172400        PERFORM UNTIL RAD-INDX > +13                                      
172500           MOVE ZERO                  TO MOD2-IDARTNR (RAD-INDX)          
172600           ADD +1                     TO RAD-INDX                         
172700        END-PERFORM                                                       
172800     END-IF                                                               
172900     .                                                                    
173000     EJECT                                                                
173100 S03-FLYTT-BYLEV-T-BILD SECTION.                                          
173200     SKIP1                                                                
173300     PERFORM DB2-DCL-OPN-CRS-BYLEV                                        
173400     PERFORM DB2-FETCH-BYLEV                                              
173500     IF RADER-FINNS                                                       
173600        PERFORM S03A-FLYTTA-FRA-BYLEV                                     
173700        PERFORM DB2-CLOSE-BYLEV-CRS                                       
173800     ELSE                                                                 
173900        PERFORM S03B-RENSA-BYLEV                                          
174000     END-IF                                                               
174100     .                                                                    
174200     EJECT                                                                
174300 S03A-FLYTTA-FRA-BYLEV SECTION.                                           
174400     SKIP2                                                                
174500     MOVE BYLEV-BELEV                 TO MOD-BELEV-LO                     
174600     MOVE +1                          TO RAD-INDX                         
174700     PERFORM UNTIL NOT RADER-FINNS                                        
174800     OR RAD-INDX > +6                                                     
174900        MOVE BYLEV-BELEV              TO MOD-BELEV (RAD-INDX)             
175000        PERFORM DB2-FETCH-BYLEV                                           
175100        ADD +1                        TO RAD-INDX                         
175200     END-PERFORM                                                          
175300     IF RADER-FINNS                                                       
175400        IF MFS-UPDATE                                                     
175500           CONTINUE                                                       
175600        ELSE                                                              
175700           MOVE MED-2 (SPRAK-IX)      TO MOD-TEMFSINF                     
175800        END-IF                                                            
175900        MOVE BYLEV-BELEV              TO MOD-BELEV-HI                     
176000     ELSE                                                                 
176100        MOVE SPACE                    TO MOD-BELEV-HI                     
176200        PERFORM UNTIL RAD-INDX > +6                                       
176300           MOVE MFS-RENSA-FAELT       TO MOD-BELEV (RAD-INDX)             
176400           ADD +1                     TO RAD-INDX                         
176500        END-PERFORM                                                       
176600     END-IF                                                               
176700     .                                                                    
176800     EJECT                                                                
176900 S03B-RENSA-BYLEV SECTION.                                                
177000     SKIP2                                                                
177100     MOVE SPACE                       TO MOD-BELEV-LO                     
177200                                         MOD-BELEV-HI                     
177300     MOVE +1                          TO RAD-INDX                         
177400     PERFORM UNTIL RAD-INDX > +6                                          
177500        MOVE MFS-RENSA-FAELT          TO MOD-BELEV (RAD-INDX)             
177600        ADD +1                        TO RAD-INDX                         
177700     END-PERFORM                                                          
177800     .                                                                    
177900     EJECT                                                                
178000 S04-RENSA-INMATAT SECTION.                                               
178100     SKIP2                                                                
178200     MOVE MFS-RENSA-FAELT          TO MOD-UPPDATERINGSSORT                
178300                                      MOD-IDPRODNR-IN                     
178400                                      MOD-IDDISTR-RENOV-IN                
178500                                      MOD-IDDISTR-NDC-IN                  
178600                                      MOD-IDDISTR-PAC-IN                  
178700                                      MOD-IDDISTR-CAN-IN                  
178800                                      MOD-IDDISTR-AUS-IN                  
178900                                      MOD-IDDISTR-CHN-IN                  
179000                                      MOD-IDDISTR-KOR-IN                  
179100                                      MOD-IDDISTR-MY-IN                   
179110                                      MOD-IDDISTR-TW-IN                   
179120                                      MOD-IDDISTR-TH-IN                   
179130                                      MOD-BELEV-IN                        
179200                                      MOD-KVLS-MAXCORE-IN                 
179300     .                                                                    
179400     EJECT                                                                
179500 S05-RENSA-SIDAN SECTION.                                                 
179600     SKIP2                                                                
179700     MOVE MFS-RENSA-FAELT            TO MOD-BEART-SVE                     
179800                                        MOD-BETFLEV                       
179900                                        MOD-UPPDATERINGSSORT              
180000                                        MOD-IDPRODNR-IN                   
180100                                        MOD-IDDISTR-RENOV-IN              
180200                                        MOD-IDDISTR-NDC-IN                
180300                                        MOD-IDDISTR-PAC-IN                
180400                                        MOD-IDDISTR-CAN-IN                
180500                                        MOD-IDDISTR-AUS-IN                
180600                                        MOD-IDDISTR-CHN-IN                
180700                                        MOD-IDDISTR-KOR-IN                
180710                                        MOD-IDDISTR-MY-IN                 
180720                                        MOD-IDDISTR-TW-IN                 
180730                                        MOD-IDDISTR-TH-IN                 
180800                                        MOD-BELEV-IN                      
180900                                        MOD-KVLS-MAXCORE-IN               
181000     MOVE +1                         TO RAD-INDX                          
181100     PERFORM UNTIL RAD-INDX > +3                                          
181200        MOVE MFS-RENSA-FAELT         TO MOD-TEBYTNOT (RAD-INDX)           
181300        ADD +1                       TO RAD-INDX                          
181400     END-PERFORM                                                          
181500     MOVE MFS-RENSA-FAELT         TO MOD-IDDISTR-RENOV                    
181600     MOVE MFS-RENSA-FAELT         TO MOD-IDDISTR-NDC                      
181700     MOVE MFS-RENSA-FAELT         TO MOD-IDDISTR-PAC                      
181800     MOVE MFS-RENSA-FAELT         TO MOD-IDDISTR-CAN                      
181900     MOVE MFS-RENSA-FAELT         TO MOD-IDDISTR-AUS                      
182000     MOVE MFS-RENSA-FAELT         TO MOD-IDDISTR-CHN                      
182100     MOVE MFS-RENSA-FAELT         TO MOD-IDDISTR-KOR                      
182110     MOVE MFS-RENSA-FAELT         TO MOD-IDDISTR-MY                       
182120     MOVE MFS-RENSA-FAELT         TO MOD-IDDISTR-TW                       
182130     MOVE MFS-RENSA-FAELT         TO MOD-IDDISTR-TH                       
182200     MOVE MFS-RENSA-FAELT            TO MOD-TEBYTNOT (4)                  
182300     MOVE +1                         TO RAD-INDX                          
182400     PERFORM UNTIL RAD-INDX > +18                                         
182500        MOVE MFS-RENSA-FAELT         TO MOD-IDARTNR (RAD-INDX)            
182600        ADD +1                       TO RAD-INDX                          
182700     END-PERFORM                                                          
182800     MOVE +1                         TO RAD-INDX                          
182900     PERFORM UNTIL RAD-INDX > +6                                          
183000        MOVE MFS-RENSA-FAELT         TO MOD-BELEV   (RAD-INDX)            
183100        ADD +1                       TO RAD-INDX                          
183200     END-PERFORM                                                          
183300     .                                                                    
183400     EJECT                                                                
183500* IMS SEKTIONER                                                           
183600     SKIP3                                                                
183700 IMS-GET-MSG SECTION.                                                     
183800     SKIP1                                                                
183900     MOVE '  QC' TO GODK-STATUSKODER                                      
184000     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
184100     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
184200     PERFORM IMS-STATUSKONTROLL                                           
184300     SKIP3                                                                
184400     .                                                                    
184500 IMS-INSERT-MSG SECTION.                                                  
184600     SKIP1                                                                
185000     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
185100     MOVE SPACE TO GODK-STATUSKODER                                       
185200     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
185300     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
185400     PERFORM IMS-STATUSKONTROLL                                           
185500     EJECT                                                                
185600     .                                                                    
185610 IMS-GET-WMSGKOM-MSG SECTION.                                             
185620                                                                          
185630     MOVE '  QD'   TO GODK-STATUSKODER                                    
185640     CALL CBLTDLI USING GN MSG-PCB MSG-KOM-WMSGKOM                        
185650     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
185660     PERFORM IMS-STATUSKONTROLL                                           
185670     SKIP3                                                                
185671     .                                                                    
185680 IMS-INSERT-WMSGKOM-MSG SECTION.                                          
185690                                                                          
185691     MOVE '  '  TO GODK-STATUSKODER                                       
185692     CALL CBLTDLI USING ISRT MSGKOM-PCB MSG-KOM-WMSGKOM                   
185693     MOVE MSGKOM-STATUS-CODE TO STATUS-WS                                 
185694     PERFORM IMS-STATUSKONTROLL                                           
185695     SKIP3                                                                
185696     .                                                                    
185700 IMS-GU-ARTC01 SECTION.                                                   
185800     SKIP1                                                                
185900     STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-X ')'                         
186000            DELIMITED BY SIZE INTO SSA1                                   
186100     MOVE '  GE' TO GODK-STATUSKODER                                      
186200     CALL CBLTDLI USING GU ARTC-PCB DLI-IO-AREA SSA1                      
186300     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
186400     PERFORM IMS-STATUSKONTROLL                                           
186500     SKIP3                                                                
186600     .                                                                    
186700 IMS-GU-BENA11 SECTION.                                                   
186800     SKIP1                                                                
186900     STRING 'WLBENA01(WDD3BSEQ =' W-IDARTNR-X ')'                         
187000            DELIMITED BY SIZE INTO SSA1                                   
187100     STRING 'WLBENA11(IDSKYLT  =' W-IDSKYLT-X ')'                         
187200            DELIMITED BY SIZE INTO SSA2                                   
187300     MOVE '  GE' TO GODK-STATUSKODER                                      
187400     CALL CBLTDLI USING GU BENA-PCB DLI-IO-AREA SSA1 SSA2                 
187500     MOVE BENA-STATUS-CODE TO STATUS-WS                                   
187600     PERFORM IMS-STATUSKONTROLL                                           
187700     SKIP3                                                                
187800     .                                                                    
187900 IMS-GU-WDK711 SECTION.                                                   
188000     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
188100     DELIMITED BY SIZE INTO SSA1                                          
188200     STRING 'WDK711  (IDDC     =' W-IDDC-X    ')'                         
188300     DELIMITED BY SIZE INTO SSA2                                          
188400     MOVE '  GE' TO GODK-STATUSKODER                                      
188500     CALL CBLTDLI USING GU WDK7-PCB DLI-IO-WDK711   SSA1 SSA2             
188600     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
188700     PERFORM IMS-STATUSKONTROLL                                           
188800     .                                                                    
188900     SKIP2                                                                
189000 IMS-STATUSKONTROLL SECTION.                                              
189100     SKIP1                                                                
189200     SET STATUS-IX TO 1                                                   
189300     SEARCH GODK-STATUS                                                   
189400       AT END                                                             
189500         CALL FELLOG                                                      
189600        WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                          
189700           CONTINUE                                                       
189800     END-SEARCH                                                           
189900     .                                                                    
190000     EJECT                                                                
190100 DB2-DCL-OPN-CRS-BYPRO SECTION.                                           
190200* OBS!!! DECLARE GER INGEN SQLCODE I RETUR                                
190300     EXEC SQL DECLARE BYPRO-CRS CURSOR FOR                                
190400              SELECT IDARTNR                                              
190500              FROM BYPRO                                                  
190600              WHERE IDARTNR >= :W-IDPRODNR                                
190700              AND IDARTNR_BYT = :W-IDARTNR-BYT                            
190800              ORDER BY IDARTNR                                            
190900     END-EXEC                                                             
191000     MOVE 000               TO GODK-SQLCODESKODER                         
191100     EXEC SQL OPEN BYPRO-CRS END-EXEC                                     
191200     MOVE SQLCODE           TO SQLCODE-WS                                 
191300     PERFORM DB2-STATUSKONTROLL                                           
191400     .                                                                    
191500     EJECT                                                                
191600 DB2-DCL-OPN-CRS-BYLEV SECTION.                                           
191700     EXEC SQL DECLARE BYLEV-CRS CURSOR FOR                                
191800              SELECT BELEV                                                
191900              FROM BYLEV                                                  
192000              WHERE BELEV >= :W-BELEV                                     
192100              AND IDARTNR_BYT = :W-IDARTNR-BYT                            
192200              ORDER BY BELEV                                              
192300     END-EXEC                                                             
192400     MOVE 000               TO GODK-SQLCODESKODER                         
192500     EXEC SQL OPEN BYLEV-CRS END-EXEC                                     
192600     MOVE SQLCODE           TO SQLCODE-WS                                 
192700     PERFORM DB2-STATUSKONTROLL                                           
192800     .                                                                    
192900     EJECT                                                                
193000 DB2-FETCH-BYPRO SECTION.                                                 
193100     MOVE 000100            TO GODK-SQLCODESKODER                         
193200     EXEC SQL FETCH BYPRO-CRS INTO                                        
193300            :BYPRO-IDARTNR                                                
193400     END-EXEC                                                             
193500     MOVE SQLCODE           TO SQLCODE-WS                                 
193600     PERFORM DB2-STATUSKONTROLL                                           
193700     .                                                                    
193800 DB2-FETCH-BYLEV SECTION.                                                 
193900     MOVE 000100            TO GODK-SQLCODESKODER                         
194000     EXEC SQL FETCH BYLEV-CRS INTO                                        
194100            :BYLEV-BELEV                                                  
194200     END-EXEC                                                             
194300     MOVE SQLCODE           TO SQLCODE-WS                                 
194400     PERFORM DB2-STATUSKONTROLL                                           
194500     .                                                                    
194600     EJECT                                                                
194700 DB2-SELECT-BYART SECTION.                                                
194800     MOVE 000100904         TO GODK-SQLCODESKODER                         
194900     EXEC SQL SELECT                                                      
195000                  IDARTNR_BYT,                                            
195100                  BETFLEV, TEBYTNOT1,                                     
195200                  TEBYTNOT2, TEBYTNOT3,                                   
195300                  TEBYTNOT4,                                              
195400                  IDDISTR_RENOV,                                          
195500                  IDDISTR_RENOV_NDC,                                      
195600                  IDDISTR_RENOV_PAC,                                      
195700                  IDDISTR_RENOV_CAN,                                      
195800                  IDDISTR_RENOV_AUS,                                      
195900                  IDDISTR_RENOV_CHN,                                      
196000                  IDDISTR_RENOV_KOR,                                      
196010                  IDDISTR_RENOV_MY,                                       
196020                  IDDISTR_RENOV_TW,                                       
196030                  IDDISTR_RENOV_TH,                                       
196100                  KVLS_MAXCORE                                            
196200              INTO                                                        
196300                  :BYART-IDARTNR-BYT,                                     
196400                  :BYART-BETFLEV, :BYART-TEBYTNOT1,                       
196500                  :BYART-TEBYTNOT2, :BYART-TEBYTNOT3,                     
196600                  :BYART-TEBYTNOT4,                                       
196700                  :BYART-IDDISTR-RENOV,                                   
196800                  :BYART-IDDISTR-RENOV-NDC,                               
196900                  :BYART-IDDISTR-RENOV-PAC,                               
197000                  :BYART-IDDISTR-RENOV-CAN,                               
197100                  :BYART-IDDISTR-RENOV-AUS,                               
197200                  :BYART-IDDISTR-RENOV-CHN,                               
197300                  :BYART-IDDISTR-RENOV-KOR,                               
197310                  :BYART-IDDISTR-RENOV-MY,                                
197320                  :BYART-IDDISTR-RENOV-TW,                                
197330                  :BYART-IDDISTR-RENOV-TH,                                
197400                  :BYART-KVLS-MAXCORE                                     
197500            FROM BYART                                                    
197600            WHERE IDARTNR_BYT = :W-IDARTNR-BYT                            
197700     END-EXEC                                                             
197800     MOVE SQLCODE           TO SQLCODE-WS                                 
197900     PERFORM DB2-STATUSKONTROLL                                           
198000     .                                                                    
198100 DB2-SELECT-BYLEV SECTION.                                                
198200     MOVE 000100904         TO GODK-SQLCODESKODER                         
198300     EXEC SQL SELECT                                                      
198400                   BELEV,                                                 
198500                   IDARTNR_BYT                                            
198600              INTO                                                        
198700                   :BYLEV-BELEV,                                          
198800                   :BYLEV-IDARTNR-BYT                                     
198900              FROM BYLEV                                                  
199000              WHERE BELEV = :W-BELEV                                      
199100     END-EXEC                                                             
199200     MOVE SQLCODE           TO SQLCODE-WS                                 
199300     PERFORM DB2-STATUSKONTROLL                                           
199400     .                                                                    
199500     EJECT                                                                
199600 DB2-DLET-BYPRO    SECTION.                                               
199700     SKIP2                                                                
199800     MOVE 000100904         TO GODK-SQLCODESKODER                         
199900     EXEC SQL DELETE FROM BYPRO                                           
200000        WHERE IDARTNR_BYT = :W-IDARTNR-BYT                                
200100        AND   IDARTNR     = :W-IDPRODNR                                   
200200     END-EXEC                                                             
200300     MOVE SQLCODE           TO SQLCODE-WS                                 
200400     PERFORM DB2-STATUSKONTROLL                                           
200500     .                                                                    
200600     EJECT                                                                
200700 DB2-DLET-BYLEV    SECTION.                                               
200800     SKIP2                                                                
200900     MOVE 000803904         TO GODK-SQLCODESKODER                         
201000     EXEC SQL DELETE FROM BYLEV                                           
201100        WHERE BELEV = :W-BELEV                                            
201200        AND   IDARTNR_BYT = :W-IDARTNR-BYT                                
201300     END-EXEC                                                             
201400     .                                                                    
201500     EJECT                                                                
201600 DB2-UPDATE-BYART    SECTION.                                             
201700     SKIP2                                                                
201800     MOVE 000               TO GODK-SQLCODESKODER                         
201900     EXEC SQL UPDATE BYART                                                
202000        SET BETFLEV     = :BYART-BETFLEV,                                 
202100            TEBYTNOT1   = :BYART-TEBYTNOT1,                               
202200            TEBYTNOT2   = :BYART-TEBYTNOT2,                               
202300            TEBYTNOT3   = :BYART-TEBYTNOT3,                               
202400            TEBYTNOT4   = :BYART-TEBYTNOT4,                               
202500            IDDISTR_RENOV  = :BYART-IDDISTR-RENOV,                        
202600            IDDISTR_RENOV_NDC  = :BYART-IDDISTR-RENOV-NDC,                
202700            IDDISTR_RENOV_PAC  = :BYART-IDDISTR-RENOV-PAC,                
202800            IDDISTR_RENOV_CAN  = :BYART-IDDISTR-RENOV-CAN,                
202900            IDDISTR_RENOV_AUS  = :BYART-IDDISTR-RENOV-AUS,                
203000            IDDISTR_RENOV_CHN  = :BYART-IDDISTR-RENOV-CHN,                
203100            IDDISTR_RENOV_KOR  = :BYART-IDDISTR-RENOV-KOR,                
203110            IDDISTR_RENOV_MY   = :BYART-IDDISTR-RENOV-MY,                 
203120            IDDISTR_RENOV_TW   = :BYART-IDDISTR-RENOV-TW,                 
203130            IDDISTR_RENOV_TH   = :BYART-IDDISTR-RENOV-TH,                 
203200            KVLS_MAXCORE       = :BYART-KVLS-MAXCORE                      
203300     WHERE IDARTNR_BYT  = :W-IDARTNR-BYT                                  
203400     END-EXEC                                                             
203500     MOVE SQLCODE           TO SQLCODE-WS                                 
203600     PERFORM DB2-STATUSKONTROLL                                           
203700     .                                                                    
203800     EJECT                                                                
203900 DB2-ISRT-BYART    SECTION.                                               
204000     MOVE 000               TO GODK-SQLCODESKODER                         
204100     SKIP2                                                                
204200     EXEC SQL INSERT INTO BYART                                           
204300           (IDARTNR_BYT,                                                  
204400            BETFLEV,                                                      
204500            TEBYTNOT1,                                                    
204600            TEBYTNOT2,                                                    
204700            TEBYTNOT3,                                                    
204800            TEBYTNOT4,                                                    
204900            IDDISTR_RENOV,                                                
205000            ADLAGOMR,                                                     
205100            ADGANG,                                                       
205200            ADPLATS,                                                      
205300            ADBYTOMR,                                                     
205400            ADBYTGANG,                                                    
205500            ADBYTPL,                                                      
205600            FLBYTKTL,                                                     
205700            KVBYTPKO,                                                     
205800            TEBYTKVA1,                                                    
205900            TEBYTKVA2,                                                    
206000            IDDISTR_RENOV_NDC,                                            
206100            IDDISTR_RENOV_PAC,                                            
206200            IDDISTR_RENOV_CAN,                                            
206300            IDDISTR_RENOV_AUS,                                            
206400            KVLS_MAXCORE,                                                 
206500            IDDISTR_RENOV_CHN,                                            
206600            IDDISTR_RENOV_KOR,                                            
206610            IDDISTR_RENOV_MY,                                             
206620            IDDISTR_RENOV_TW,                                             
206630            IDDISTR_RENOV_TH,                                             
206700            TEBYTKVA3,                                                    
206800            TEBYTKVA4)                                                    
206900        VALUES (:W-IDARTNR-BYT,                                           
207000           :BYART-BETFLEV,                                                
207100           :BYART-TEBYTNOT1,                                              
207200           :BYART-TEBYTNOT2,                                              
207300           :BYART-TEBYTNOT3,                                              
207400           :BYART-TEBYTNOT4,                                              
207500           :BYART-IDDISTR-RENOV,                                          
207600           :BYART-ADLAGOMR,                                               
207700           :BYART-ADGANG,                                                 
207800           :BYART-ADPLATS,                                                
207900           :BYART-ADBYTOMR,                                               
208000           :BYART-ADBYTGANG,                                              
208100           :BYART-ADBYTPL,                                                
208200           :BYART-FLBYTKTL,                                               
208300           :BYART-KVBYTPKO,                                               
208400           :BYART-TEBYTKVA1,                                              
208500           :BYART-TEBYTKVA2,                                              
208600           :BYART-IDDISTR-RENOV-NDC,                                      
208700           :BYART-IDDISTR-RENOV-PAC,                                      
208800           :BYART-IDDISTR-RENOV-CAN,                                      
208900           :BYART-IDDISTR-RENOV-AUS,                                      
209000           :BYART-KVLS-MAXCORE,                                           
209100           :BYART-IDDISTR-RENOV-CHN,                                      
209200           :BYART-IDDISTR-RENOV-KOR,                                      
209210           :BYART-IDDISTR-RENOV-MY,                                       
209220           :BYART-IDDISTR-RENOV-TW,                                       
209230           :BYART-IDDISTR-RENOV-TH,                                       
209300           :BYART-TEBYTKVA3,                                              
209400           :BYART-TEBYTKVA4)                                              
209500     END-EXEC                                                             
209600     MOVE SQLCODE           TO SQLCODE-WS                                 
209700     PERFORM DB2-STATUSKONTROLL                                           
209800     .                                                                    
209900     EJECT                                                                
210000 DB2-ISRT-BYPRO    SECTION.                                               
210100     SKIP2                                                                
210200     MOVE 000803            TO GODK-SQLCODESKODER                         
210300     EXEC SQL INSERT INTO BYPRO (IDARTNR_BYT, IDARTNR)                    
210400        VALUES (:W-IDARTNR-BYT, :W-IDPRODNR)                              
210500     END-EXEC                                                             
210600     MOVE SQLCODE           TO SQLCODE-WS                                 
210700     PERFORM DB2-STATUSKONTROLL                                           
210800     .                                                                    
210900     EJECT                                                                
211000 DB2-ISRT-BYLEV    SECTION.                                               
211100     SKIP2                                                                
211200     MOVE 000803            TO GODK-SQLCODESKODER                         
211300     EXEC SQL INSERT INTO BYLEV (BELEV, IDARTNR_BYT)                      
211400        VALUES (:W-BELEV, :W-IDARTNR-BYT)                                 
211500     END-EXEC                                                             
211600     MOVE SQLCODE           TO SQLCODE-WS                                 
211700     PERFORM DB2-STATUSKONTROLL                                           
211800     .                                                                    
211900     EJECT                                                                
212000 DB2-CLOSE-BYLEV-CRS SECTION.                                             
212100     SKIP2                                                                
212200     EXEC SQL CLOSE BYLEV-CRS END-EXEC                                    
212300     .                                                                    
212400     EJECT                                                                
212500 DB2-CLOSE-BYPRO-CRS SECTION.                                             
212600     SKIP2                                                                
212700     EXEC SQL CLOSE BYPRO-CRS END-EXEC                                    
212800     .                                                                    
212900     EJECT                                                                
213000 DB2-STATUSKONTROLL SECTION.                                              
213100     SKIP2                                                                
213200     SET SQLCODE-IX          TO 1                                         
213300     SEARCH GODK-SQLCODE                                                  
213400       AT END                                                             
213500         CALL FELLOG                                                      
213600        WHEN GODK-SQLCODE (SQLCODE-IX) = SQLCODE-WS                       
213700           CONTINUE                                                       
214000     END-SEARCH                                                           
220000     .                                                                    
