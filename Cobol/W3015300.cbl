000100 PROCESS DYNAM                                                            
000200 ID DIVISION.                                                             
000300 PROGRAM-ID.     W3015300.                                                
000400 AUTHOR.         RONNY S.                                                 
000500 DATE-WRITTEN.   MAR   92.                                                
000600                                                                          
000700*                                                                         
000800*    FUNKTION                                                             
000900*    UPPDATERING AV DIV FÄLT I BYART                                      
001000*    INMATNINGSFÄLT ÄR BYTESNR (ARTNR)                                    
001100*    PROGRAMMET LÄSER ARTC OCH BENA                                       
001200*    SAMT TABELLEN BYART                                                  
001300*    OBSERVERA ATT DET FINNS 2 * 6 OLIKA ADRESSER. EN FÖR VANLIGA         
001400*    OBJEKT OCH EN ADRESS FÖR GARANTI-OBJEKT.                             
001500*                                                                         
001600*    INDATA                                                               
001700*    TRANSAKTION  W3T153                                                  
001800*    MID          W3I15301                                                
001900*    MOD          W3O15301                                                
002000*    CHANGE LOG:                                                          
002100*                                                                         
002200*      YY/MM/DD - NAME            - CHANGE DESCRIPTION                    
002300*      ----------------------------------------------------------         
002400*      14/11/04 - REDDY RAHUL     - CHANGES FOR CHINA EXCHANGE            
002500*                                   E'TRACKER 10242148                    
002600*                                                                         
002700*      16/01/05 - REDDY RAHUL     - INCREASE LENGTH AND NUMBER OF         
002800*                                   LINES FOR EXCHANGE NOTES.             
002900*                                   REMOVE UNUSED FIELDS.                 
003000*                                   E'TRACKER 10251642                    
003100*                                                                         
003200 ENVIRONMENT DIVISION.                                                    
003300     SKIP2                                                                
003400 DATA DIVISION.                                                           
003500     EJECT                                                                
003600 WORKING-STORAGE SECTION.                                                 
003700                                                                          
003800*    -- CHECKED BY WY2000                                                 
003900 77  PROGRAM-NAMN                PIC X(8) VALUE 'W3015300'.               
004000 01  WS-IDARTNR                  PIC X(9) VALUE ZERO.                     
004100 01  FILLER REDEFINES WS-IDARTNR.                                         
004200   03  FILLER                    PIC 9(5).                                
004300   03  WS-ARTSIFFRA              PIC 9(1).                                
004400     88  ART-0                   VALUE 6.                                 
004500     88  ART-1                   VALUE 4  7.                              
004600     88  ART-2                   VALUE 5  8.                              
004700     88  ART-3                   VALUE 9.                                 
004800   03  FILLER                    PIC 9(3).                                
004900 77  W-IDPRODNR                  PIC S9(9) COMP-3 VALUE ZERO.             
005000 77  W-BELEV                     PIC X(30) VALUE SPACE.                   
005100                                                                          
005200 01  WS-IDLEVNR                  PIC 9(5).                                
005300 01  FILLER REDEFINES WS-IDLEVNR.                                         
005400    03  IDLEVNR-WS               PIC X(5).                                
005500                                                                          
005600 01  WS-IDDISTR-RENOV            PIC 9(5) VALUE ZERO.                     
005700 01  FILLER REDEFINES WS-IDDISTR-RENOV.                                   
005800    03  IDDISTR-RENOV-WS         PIC X(5).                                
005900                                                                          
006000 01  WS-IDDISTR-RENOV-NDC        PIC 9(5) VALUE ZERO.                     
006100 01  FILLER REDEFINES WS-IDDISTR-RENOV-NDC.                               
006200    03  IDDISTR-RENOV-NDC-WS         PIC X(5).                            
006300                                                                          
006400                                                                          
006500 01  WS-IDDISTR-RENOV-PAC        PIC 9(5) VALUE ZERO.                     
006600 01  FILLER REDEFINES WS-IDDISTR-RENOV-PAC.                               
006700    03  IDDISTR-RENOV-PAC-WS         PIC X(5).                            
006800                                                                          
006900                                                                          
007000 01  WS-IDDISTR-RENOV-AUS        PIC 9(5) VALUE ZERO.                     
007100 01  FILLER REDEFINES WS-IDDISTR-RENOV-AUS.                               
007200    03  IDDISTR-RENOV-AUS-WS         PIC X(5).                            
007300                                                                          
007400                                                                          
007500 01  WS-IDDISTR-RENOV-CHN        PIC 9(5) VALUE ZERO.                     
007600 01  FILLER REDEFINES WS-IDDISTR-RENOV-CHN.                               
007700    03  IDDISTR-RENOV-CHN-WS         PIC X(5).                            
007800                                                                          
007900 01  WS-IDDISTR-RENOV-KOR        PIC 9(5) VALUE ZERO.                     
008000 01  FILLER REDEFINES WS-IDDISTR-RENOV-KOR.                               
008100    03  IDDISTR-RENOV-KOR-WS         PIC X(5).                            
008200                                                                          
008210 01  WS-IDDISTR-RENOV-MY         PIC 9(5) VALUE ZERO.                     
008220 01  FILLER REDEFINES WS-IDDISTR-RENOV-MY.                                
008230    03  IDDISTR-RENOV-MY-WS         PIC X(5).                             
008240                                                                          
008250 01  WS-IDDISTR-RENOV-TW         PIC 9(5) VALUE ZERO.                     
008260 01  FILLER REDEFINES WS-IDDISTR-RENOV-TW.                                
008270    03  IDDISTR-RENOV-TW-WS         PIC X(5).                             
008280                                                                          
008290 01  WS-IDDISTR-RENOV-TH         PIC 9(5) VALUE ZERO.                     
008291 01  FILLER REDEFINES WS-IDDISTR-RENOV-TH.                                
008292    03  IDDISTR-RENOV-TH-WS         PIC X(5).                             
008293                                                                          
008300                                                                          
008400 01  WS-IDDISTR-RENOV-CAN        PIC 9(5) VALUE ZERO.                     
008500 01  FILLER REDEFINES WS-IDDISTR-RENOV-CAN.                               
008600    03  IDDISTR-RENOV-CAN-WS         PIC X(5).                            
008700                                                                          
008800 01  WS-IDPRODNR                 PIC 9(8).                                
008900 01  FILLER REDEFINES WS-IDPRODNR.                                        
009000    03  IDPRODNR-WS              PIC X(8).                                
009100                                                                          
009200 01  WS-KDCLAGER                 PIC 9(1).                                
009300 01  FILLER REDEFINES WS-KDCLAGER.                                        
009400    03  KDCLAGER-WS              PIC X(1).                                
009500                                                                          
009600 01  FILLER                  PIC X(16)   VALUE 'WS-SEKTION'.              
009700 01  WS-SEKTION                  PIC X(30)   VALUE SPACE.                 
009800 01  FILLER                  PIC X(16)   VALUE 'WS-IMS-SEKTION'.          
009900 01  WS-IMS-SEKTION              PIC X(30)   VALUE SPACE.                 
010000                                                                          
010100 77  JA                          PIC X       VALUE 'J'.                   
010200 77  NEJ                         PIC X       VALUE 'N'.                   
010300 77  RAD-INDX                    PIC S9(9)   VALUE +0   COMP SYNC.        
010400 77  MAX-RAD                     PIC S9(9)   VALUE +3   COMP SYNC.        
010500 77  SPRAK-IX                    PIC S9(9)   VALUE +2   COMP SYNC.        
010600 77  MAX-MOD-LAENGD              PIC S9(4)  VALUE +296  COMP SYNC.        
010700 77  W-IDARTNR-BYT               PIC S9(9) COMP-3 VALUE ZERO.             
010800 01  SW-NYCKLAR-OK               PIC X.                                   
010900   88  NYCKLAR-OK                          VALUE 'J'.                     
011000 01  SW-UPPDATERINGAR            PIC X.                                   
011100   88  INGA-UPPDATERINGAR                  VALUE 'N'.                     
011200 01  SW-INDATA-OK               PIC X.                                    
011300   88  INDATA-OK                          VALUE 'J'.                      
011400 01  SW-INMATAT                 PIC X.                                    
011500   88  INGET-INMATAT                      VALUE 'N'.                      
011600 01  WS-IDTRANS                  PIC X(4).                                
011700   88  EGEN-BILD                           VALUE '3153'.                  
011800 01  FILLER                      PIC X(16)   VALUE                        
011900                                            'NYCKLAR-TILL-DLI'.           
012000 01  NYCKLAR-TILL-DLI.                                                    
012100   03  W-IDARTNR-X.                                                       
012200     05  W-IDARTNR               PIC S9(9) COMP-3 VALUE ZERO.             
012300   03  W-IDLEVNR-X.                                                       
012400     05  W-IDLEVNR               PIC S9(5) COMP-3 VALUE ZERO.             
012500   03  W-IDSKYLT-X               PIC X(3)         VALUE 'GB '.            
012600   03  W-KDCLAGER-X.                                                      
012700     05  W-KDCLAGER              PIC S9(1) COMP-3 VALUE ZERO.             
012800     EJECT                                                                
012900 01  DYNAMISKA-SUBPROGRAM.                                                
013000   03  WDATKONV                  PIC X(8)    VALUE 'WDATKONV'.            
013100   03  FELLOG                    PIC X(8)    VALUE 'FELLOG  '.            
013200   03  CBLTDLI                   PIC X(8)    VALUE 'CBLTDLI '.            
013300   03  W005INIT                  PIC X(8)    VALUE 'W005INIT'.            
013400     EJECT                                                                
013500*                     ****   PARAMETRAR TILL W005INIT                     
013600*01  -COPY WMSGINIT                                                       
013700     EJECT                                                                
013800 01  MEDDELANDE.                                                          
013900   03  FEL2.                                                              
014000     05 FILLER                   PIC X(40)                                
014100          VALUE 'BYTESARTIKELNR FEL '.                                    
014200     05 FILLER                   PIC X(40)                                
014300          VALUE 'EXCH. PART.NO WRONG'.                                    
014400   03  FILLER REDEFINES FEL2.                                             
014500     05  FEL-2                   PIC X(40)   OCCURS 2.                    
014600                                                                          
014700   03  FEL3.                                                              
014800     05 FILLER                   PIC X(40)                                
014900          VALUE 'BYTESARTIKELNR SAKNAS'.                                  
015000     05 FILLER                   PIC X(40)                                
015100          VALUE 'EXCH. PART.NO MISSING'.                                  
015200   03  FILLER REDEFINES FEL3.                                             
015300     05  FEL-3                   PIC X(40)   OCCURS 2.                    
015400                                                                          
015500   03  FEL4.                                                              
015600     05 FILLER                   PIC X(40)                                
015700          VALUE 'TRYCK PF11 VID UPPDATERING'.                             
015800     05 FILLER                   PIC X(40)                                
015900          VALUE 'PRESS PF11 WHEN UPDATE'.                                 
016000   03  FILLER REDEFINES FEL4.                                             
016100     05  FEL-4                   PIC X(40)   OCCURS 2.                    
016200                                                                          
016300   03  FEL5.                                                              
016400     05 FILLER                   PIC X(40)                                
016500          VALUE 'INDATA FEL'.                                             
016600     05 FILLER                   PIC X(40)                                
016700          VALUE 'WRONG FIELDS'.                                           
016800   03  FILLER REDEFINES FEL5.                                             
016900     05  FEL-5                   PIC X(40)   OCCURS 2.                    
017000                                                                          
017100   03  FEL6.                                                              
017200     05 FILLER                   PIC X(40)                                
017300          VALUE 'INGET BYTESARTIKELNR'.                                   
017400     05 FILLER                   PIC X(40)                                
017500          VALUE 'NOT AN EXCHANGE NO'.                                     
017600   03  FILLER REDEFINES FEL6.                                             
017700     05  FEL-6                   PIC X(40)   OCCURS 2.                    
017800                                                                          
017900   03  FEL7.                                                              
018000     05 FILLER                   PIC X(40)                                
018100          VALUE 'PRODUKTNR SAKNAS PÅ ARTIKELREGISTRET '.                  
018200     05 FILLER                   PIC X(40)                                
018300          VALUE 'MISSING ON ARTICLE REGISTER'.                            
018400   03  FILLER REDEFINES FEL7.                                             
018500     05  FEL-7                   PIC X(40)   OCCURS 2.                    
018600                                                                          
018700   03  FEL8.                                                              
018800     05 FILLER                   PIC X(40)                                
018900          VALUE 'DB2-TABELL OTILLGÄNGLIG    '.                            
019000     05 FILLER                   PIC X(40)                                
019100          VALUE 'DATABASE UNAVAILABLE       '.                            
019200   03  FILLER REDEFINES FEL8.                                             
019300     05  FEL-8                   PIC X(40)   OCCURS 2.                    
019400                                                                          
019500   03  FEL9.                                                              
019600     05 FILLER                   PIC X(40)                                
019700          VALUE 'INGET ÄNDRAT, UPPDATERING EJ UTFÖRD'.                    
019800     05 FILLER                   PIC X(40)                                
019900          VALUE 'NO CHANGE, UPDATE HAS NOT BEEN DONE'.                    
020000   03  FILLER REDEFINES FEL9.                                             
020100     05  FEL-9                   PIC X(40)   OCCURS 2.                    
020200                                                                          
020300   03  MED1.                                                              
020400     05 FILLER                   PIC X(40)                                
020500          VALUE 'UPPDATERING GJORD        '.                              
020600     05 FILLER                   PIC X(40)                                
020700          VALUE 'FIELDS ARE UPDATED            '.                         
020800   03  FILLER REDEFINES MED1.                                             
020900     05  MED-1                   PIC X(40)   OCCURS 2.                    
021000                                                                          
021100   03  MED2.                                                              
021200     05 FILLER                   PIC X(40)                                
021300          VALUE 'TRYCK PF8 FÖR FLERA RADER'.                              
021400     05 FILLER                   PIC X(40)                                
021500          VALUE 'PRESS PF8 FOR MORE LINES'.                               
021600   03  FILLER REDEFINES MED2.                                             
021700     05  MED-2                   PIC X(40)   OCCURS 2.                    
021800                                                                          
021900   03  MED3.                                                              
022000     05 FILLER                   PIC X(40)                                
022100          VALUE 'DETTA ÄR FÖRSTA SIDAN'.                                  
022200     05 FILLER                   PIC X(40)                                
022300          VALUE 'THIS IS THE FIRST PAGE'.                                 
022400   03  FILLER REDEFINES MED3.                                             
022500     05  MED-3                   PIC X(40)   OCCURS 2.                    
022600                                                                          
022700   03  MED4.                                                              
022800     05 FILLER                   PIC X(40)                                
022900          VALUE 'FÖRSÖK SENARE, EV KONTAKTA SYSTANSV'.                    
023000     05 FILLER                   PIC X(40)                                
023100          VALUE 'TRY LATER OR NOTIFY THE DP-DEPARTMENT'.                  
023200   03  FILLER REDEFINES MED4.                                             
023300     05  MED-4                   PIC X(40)   OCCURS 2.                    
023400     EJECT                                                                
023500*- - - - - - - - - - - - - - - - - - - BYTES-ARTIKELTEST                  
023600 01  FILLER                      PIC X(16)   VALUE 'BYTES-ART'.           
023700 01  TEST-IDARTNR                PIC 9(9)   COMP-3.                       
023800*01  FILLER -COPY WWBYT01   -RED TEST-IDARTNR                             
023900*01  FILLER -COPY WWBYT16   -RED TEST-IDARTNR                             
024000     EJECT                                                                
024100******************************************************************        
024200*                                                                         
024300*                AREOR FÖR MFS OCH SKÄRMHANTERING                         
024400*                                                                         
024500 01  FILLER                      PIC X(16)   VALUE 'MFS-WS'.              
024600*01  MID -COPY W3I15301                                                   
024700*01  -COPY WMSGAREA                                                       
024800     EJECT                                                                
024900*  03  MOD -COPY W3O15301  -RED MSG-AREA.                                 
025000     EJECT                                                                
025100*01  -COPY WMFSAREA                                                       
025200     EJECT                                                                
025300******************************************************************        
025400*                                                                         
025500*        ARBETS-AREOR TILL DB2- OCH IMS-SEKTIONERNA                       
025600*                                                                         
025700 01  FILLER                  PIC X(16)   VALUE 'DB2-WS     '.             
025800*01  -COPY BYART -PRE BYART-                                              
025900     EJECT                                                                
026000*01  -COPY BYPRO -PRE BYPRO-                                              
026100     EJECT                                                                
026200*01  -COPY BYLEV -PRE BYLEV-                                              
026300     EJECT                                                                
026400 01  FILLER                  PIC X(16) VALUE 'BYART-AREA'.                
026500       EXEC SQL INCLUDE BYART END-EXEC.                                   
026600     SKIP3                                                                
026700 01  FILLER                  PIC X(16) VALUE 'BYPRO-AREA'.                
026800       EXEC SQL INCLUDE BYPRO END-EXEC.                                   
026900     SKIP3                                                                
027000 01  FILLER                  PIC X(16) VALUE 'BYLEV-AREA'.                
027100       EXEC SQL INCLUDE BYLEV END-EXEC.                                   
027200     SKIP3                                                                
027300 01  FILLER                  PIC X(16) VALUE 'SQLCA-AREA'.                
027400       EXEC SQL INCLUDE SQLCA END-EXEC.                                   
027500*                        **** STATUS-KOD FRÅN DB2                         
027600 01  FILLER                     PIC X(16) VALUE 'SQLCODE-WS'.             
027700 01  DB2-WS.                                                              
027800   03  SQLCODE-WS                PIC 9(3) VALUE ZERO.                     
027900     88  CURSOR-OK                           VALUE 000.                   
028000     88  RADER-FINNS                         VALUE 000.                   
028100     88  RADER-SAKNAS                        VALUE 100.                   
028200     88  904-KOD                             VALUE 904.                   
028300     SKIP1                                                                
028400   03  GODK-SQLCODESKODER.                                                
028500     05  GODK-SQLCODE OCCURS 5                                            
028600         INDEXED BY SQLCODE-IX PIC 999.                                   
028700     EJECT                                                                
028800 01  FILLER                    PIC X(16)   VALUE 'IMS-WS     '.           
028900 01  IMS-WS.                                                              
029000     SKIP3                                                                
029100*                        **** STATUS-KOD FRÅN IMS                         
029200   03  STATUS-WS                 PIC XX.                                  
029300     88  SEGMENT-FINNS                       VALUE '  '.                  
029400     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
029500     SKIP3                                                                
029600   03  GODK-STATUSKODER.                                                  
029700     05  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
029800     SKIP3                                                                
029900 01    SSA1                      PIC X(64).                               
030000 01    SSA2                      PIC X(64).                               
030100 01    SSA3                      PIC X(64).                               
030200*                            IMS FUNKTIONSKODER                           
030300*01    -COPY W0003                                                        
030400     EJECT                                                                
030500 01  FILLER                      PIC X(16) VALUE 'DLI-IO-AREA'.           
030600 01  DLI-IO-AREA.                                                         
030700   03  IO-AREA                   PIC X(128)   VALUE SPACE.                
030800*   03 ARTC01  -COPY WDK601   -PRE ARTC01-   -RED IO-AREA                 
030900     EJECT                                                                
031000*   03 LEVA01  -COPY WDF101   -PRE LEVA01-   -RED IO-AREA                 
031100     EJECT                                                                
031200*   03 BENA11  -COPY WDD311   -PRE BENA11-   -RED IO-AREA                 
031300     EJECT                                                                
031400 LINKAGE SECTION.                                                         
031500*01  -COPY W0009     -PRE MSG-                                            
031600     SKIP2                                                                
031700*01  -COPY W0008     -PRE USEA-                                           
031800     05  FILLER                  PIC X.                                   
031900     SKIP2                                                                
032000*01  -COPY W0008     -PRE ARTC-                                           
032100     05  FILLER                  PIC X.                                   
032200     SKIP2                                                                
032300*01  -COPY W0008     -PRE BENA-                                           
032400     05  FILLER                  PIC X.                                   
032500     SKIP2                                                                
032600*01  -COPY W0008     -PRE LEVA-                                           
032700     05  FILLER                  PIC X.                                   
032800     EJECT                                                                
032900 PROCEDURE DIVISION   USING  MSG-PCB USEA-PCB BENA-PCB LEVA-PCB           
033000                                                       ARTC-PCB.          
033100      ENTRY 'DLITCBL' USING  MSG-PCB USEA-PCB BENA-PCB LEVA-PCB           
033200                                                       ARTC-PCB.          
033300     PERFORM IMS-GET-MSG                                                  
033400     IF SEGMENT-FINNS                                                     
033500        PERFORM A-INIT-KOLLA-NYCKLAR                                      
033600        IF NYCKLAR-OK                                                     
033700           IF MFS-UPDATE                                                  
033800              PERFORM B-MFS-ROER-EJ-FAELT                                 
033900              PERFORM C-KOLLA-INDATA-EV-UPPDAT                            
034000              IF INDATA-OK                                                
034100                 PERFORM E-VISA-SIDAN                                     
034200              END-IF                                                      
034300           ELSE                                                           
034400              IF MFS-IDPFK = ' '                                          
034500                 PERFORM D-KOLLA-ATT-INGET-IFYLLT                         
034600                 IF INGET-INMATAT                                         
034700                    PERFORM E-VISA-SIDAN                                  
034800                 ELSE                                                     
034900                    PERFORM B-MFS-ROER-EJ-FAELT                           
035000                    MOVE FEL-4 (SPRAK-IX) TO MOD-TEMFSFEL                 
035100                 END-IF                                                   
035200              ELSE                                                        
035300                 PERFORM E-VISA-SIDAN                                     
035400              END-IF                                                      
035500           END-IF                                                         
035600        ELSE                                                              
035700           PERFORM S03-RENSA-SIDAN                                        
035800        END-IF                                                            
035900        COMPUTE MSG-KVLL = LENGTH OF MOD-W3O15301 + 4                     
036000******  MOVE MAX-MOD-LAENGD TO MSG-KVLL                                   
036100        PERFORM IMS-INSERT-MSG                                            
036200     END-IF                                                               
036300     MOVE ZERO                            TO RETURN-CODE                  
036400     GOBACK                                                               
036500     .                                                                    
036600     EJECT                                                                
036700 A-INIT-KOLLA-NYCKLAR SECTION.                                            
036800     MOVE 'A-INIT-KOLLA-NYCKLAR' TO WS-SEKTION                            
036900*    DISPLAY WS-SEKTION                                                   
037000     SKIP2                                                                
037100     IF MSG-DUBBLA-TRANSKODER                                             
037200        MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W3I15301                
037300        MOVE MSG-IDTRANS-2                 TO MFS-IDTRANS                 
037400        MOVE MSG-KDMFSFOR-2                TO MFS-KDMFSFOR                
037500        MOVE MSG-KDTRTYP                   TO MFS-KDTRTYP                 
037600        MOVE MSG-IDPFK                     TO MFS-IDPFK                   
037700     ELSE                                                                 
037800        MOVE MSG-INDATA-MINUS-1-TRANSKOD   TO MID-W3I15301                
037900        MOVE MSG-IDTRANS-1                 TO MFS-IDTRANS                 
038000        MOVE MSG-KDMFSFOR-1                TO MFS-KDMFSFOR                
038100        MOVE SPACE                         TO MFS-KDTRTYP                 
038200                                              MFS-IDPFK                   
038300     END-IF                                                               
038400     MOVE LOW-VALUE                        TO MSG-AREA                    
038500     MOVE 'W3O153N1'                       TO MFS-IDMOD                   
038600     MOVE '3153'                           TO MOD-IDTRANS                 
038700     MOVE MFS-IDTRANS                      TO WS-IDTRANS                  
038800     MOVE MFS-RENSA-FAELT                  TO MOD-TEMFSFEL                
038900                                              MOD-TEMFSINF                
039000                                              MOD-IDARTNR-IN              
039100     IF EGEN-BILD                                                         
039200        CONTINUE                                                          
039300     ELSE                                                                 
039400        MOVE SPACE                         TO MFS-KDTRTYP                 
039500        MOVE '7'                           TO MFS-IDPFK                   
039600     END-IF                                                               
039700     INITIALIZE GODK-SQLCODESKODER                                        
039800     PERFORM AA-KOLLA-NYCKLAR                                             
039900     .                                                                    
040000     EJECT                                                                
040100 AA-KOLLA-NYCKLAR  SECTION.                                               
040200     MOVE 'AA-KOLLA-NYCKLAR' TO WS-SEKTION                                
040300*    DISPLAY WS-SEKTION                                                   
040400     SKIP3                                                                
040500     MOVE JA                         TO SW-NYCKLAR-OK                     
040600                                                                          
040700     MOVE ALL '+' TO MSGI-WMSGINIT                                        
040800     MOVE '001'             TO MSGI-KDCALL                                
040900     MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                                
041000     MOVE MSG-LTERM-NAME    TO MSGI-IDLTERM-USER                          
041100     MOVE '3153'            TO MSGI-IDTRANS                               
041200     IF MFS-IDTRANS = '3153'                                              
041300     OR (MID-IDARTNR-IN NUMERIC                                           
041400     AND MID-IDARTNR-IN > ZERO)                                           
041500         MOVE MID-IDARTNR-IN TO MSGI-IDARTNR                              
041600     END-IF                                                               
041700     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
041800     MOVE MSGI-IDARTNR TO WS-IDARTNR                                      
041900     INSPECT WS-IDARTNR REPLACING ALL SPACE BY ZERO                       
042000                                                                          
042100     IF MID-IDARTNR-IN = ALL '+'                                          
042200       CONTINUE                                                           
042300     ELSE                                                                 
042400       MOVE '7'         TO MFS-IDPFK                                      
042500       MOVE SPACE       TO MFS-KDTRTYP                                    
042600     END-IF                                                               
042700                                                                          
042800     MOVE WS-IDARTNR                 TO MOD-IDARTNR-UT                    
042900     INSPECT MOD-IDARTNR-UT REPLACING LEADING ZERO BY SPACE               
043000     IF WS-IDARTNR    NUMERIC                                             
043100        MOVE WS-IDARTNR TO TEST-IDARTNR                                   
043200        IF BYT01-BYTES                                                    
043300           IF BYT16-RADIO                                                 
043400              MOVE 3                       TO WS-ARTSIFFRA                
043500           ELSE                                                           
043600              IF ART-0                                                    
043700                 MOVE 0                    TO WS-ARTSIFFRA                
043800              ELSE                                                        
043900                 IF ART-1                                                 
044000                    MOVE 1                 TO WS-ARTSIFFRA                
044100                 ELSE                                                     
044200                    IF ART-2                                              
044300                       MOVE 2              TO WS-ARTSIFFRA                
044400                    ELSE                                                  
044500                       IF ART-3                                           
044600                          MOVE 3           TO WS-ARTSIFFRA                
044700                       END-IF                                             
044800                    END-IF                                                
044900                 END-IF                                                   
045000              END-IF                                                      
045100           END-IF                                                         
045200                                                                          
045300           MOVE WS-IDARTNR              TO W-IDARTNR-BYT                  
045400                                           W-IDARTNR                      
045500           PERFORM IMS-GU-ARTC01                                          
045600           IF SEGMENT-FINNS                                               
045700              CONTINUE                                                    
045800           ELSE                                                           
045900              MOVE NEJ                  TO SW-NYCKLAR-OK                  
046000              MOVE FEL-3 (SPRAK-IX)     TO MOD-TEMFSFEL                   
046100           END-IF                                                         
046200        ELSE                                                              
046300           MOVE NEJ                  TO SW-NYCKLAR-OK                     
046400           MOVE FEL-6 (SPRAK-IX)     TO MOD-TEMFSFEL                      
046500        END-IF                                                            
046600     ELSE                                                                 
046700        MOVE NEJ                     TO SW-NYCKLAR-OK                     
046800        MOVE FEL-2 (SPRAK-IX)        TO MOD-TEMFSFEL                      
046900     END-IF                                                               
047000     .                                                                    
047100     EJECT                                                                
047200 B-MFS-ROER-EJ-FAELT  SECTION.                                            
047300     MOVE 'B-MFS-ROER-EJ-FAELT' TO WS-SEKTION                             
047400*    DISPLAY WS-SEKTION                                                   
047500     SKIP2                                                                
047600     MOVE MFS-ROER-EJ-FAELT          TO MOD-BEART-SVE                     
047700                                        MOD-ADLAGOMR-UT                   
047800                                        MOD-ADGANG-UT                     
047900                                        MOD-ADPLATS-UT                    
048000                                        MOD-IDDISTR-RENOV-IN              
048100                                        MOD-IDDISTR-RENOV-NDC-IN          
048200                                        MOD-IDDISTR-RENOV-PAC-IN          
048300                                        MOD-IDDISTR-RENOV-CAN-IN          
048400                                        MOD-IDDISTR-RENOV-AUS-IN          
048500                                        MOD-IDDISTR-RENOV-CHN-IN          
048600                                        MOD-IDDISTR-RENOV-KOR-IN          
048610                                        MOD-IDDISTR-RENOV-MY-IN           
048620                                        MOD-IDDISTR-RENOV-TW-IN           
048630                                        MOD-IDDISTR-RENOV-TH-IN           
048700                                        MOD-IDDISTR-RENOV-UT              
048800                                        MOD-IDDISTR-RENOV-NDC-UT          
048900                                        MOD-IDDISTR-RENOV-PAC-UT          
049000                                        MOD-IDDISTR-RENOV-CAN-UT          
049100                                        MOD-IDDISTR-RENOV-AUS-UT          
049200                                        MOD-IDDISTR-RENOV-CHN-UT          
049300                                        MOD-IDDISTR-RENOV-KOR-UT          
049310                                        MOD-IDDISTR-RENOV-MY-UT           
049311                                        MOD-IDDISTR-RENOV-TW-UT           
049320                                        MOD-IDDISTR-RENOV-TH-UT           
049400                                        MOD-KVBYTPKO-IN                   
049500                                        MOD-KVBYTPKO-UT                   
049600                                        MOD-TEBYTKVA1                     
049700                                        MOD-TEBYTKVA2                     
049800                                        MOD-TEBYTKVA3                     
049900                                        MOD-TEBYTKVA4                     
050000     .                                                                    
050100     EJECT                                                                
050200 C-KOLLA-INDATA-EV-UPPDAT  SECTION.                                       
050300     MOVE 'C-KOLLA-INDATA-EV-UPPDAT' TO WS-SEKTION                        
050400*    DISPLAY WS-SEKTION                                                   
050500     SKIP2                                                                
050600     IF  MID-IDDISTR-RENOV     = ALL '+'                                  
050700     AND MID-IDDISTR-RENOV-NDC = ALL '+'                                  
050800     AND MID-IDDISTR-RENOV-PAC = ALL '+'                                  
050900     AND MID-IDDISTR-RENOV-CAN = ALL '+'                                  
051000     AND MID-IDDISTR-RENOV-AUS = ALL '+'                                  
051100     AND MID-IDDISTR-RENOV-CHN = ALL '+'                                  
051200     AND MID-IDDISTR-RENOV-KOR = ALL '+'                                  
051210     AND MID-IDDISTR-RENOV-MY  = ALL '+'                                  
051220     AND MID-IDDISTR-RENOV-TW  = ALL '+'                                  
051230     AND MID-IDDISTR-RENOV-TH  = ALL '+'                                  
051300     AND MID-KVBYTPKO          = ALL '+'                                  
051400     AND MID-TEBYTKVA1         = ALL '+'                                  
051500     AND MID-TEBYTKVA2         = ALL '+'                                  
051600     AND MID-TEBYTKVA3         = ALL '+'                                  
051700     AND MID-TEBYTKVA4         = ALL '+'                                  
051800        CONTINUE                                                          
051900        MOVE FEL-9 (SPRAK-IX)    TO MOD-TEMFSFEL                          
052000     ELSE                                                                 
052100        MOVE JA                           TO SW-INDATA-OK                 
052200******KOLLAR ATT ART FINNS ********                                       
052300        PERFORM DB2-SELECT-BYART                                          
052400        IF RADER-FINNS                                                    
052500           MOVE JA                        TO SW-INDATA-OK                 
052600        ELSE                                                              
052700           MOVE NEJ                           TO SW-INDATA-OK             
052800           PERFORM S03-RENSA-SIDAN                                        
052900           IF 904-KOD                                                     
053000              MOVE FEL-8 (SPRAK-IX)    TO MOD-TEMFSFEL                    
053100              MOVE MED-4 (SPRAK-IX)    TO MOD-TEMFSINF                    
053200           ELSE                                                           
053300              MOVE FEL-3 (SPRAK-IX)    TO MOD-TEMFSFEL                    
053400              MOVE SPACE               TO MOD-TEMFSINF                    
053500           END-IF                                                         
053600        END-IF                                                            
053700*************************                                                 
053800        IF INDATA-OK                                                      
053900          PERFORM CA-KOLLA-INDATA-NYUPPL                                  
054000          IF INDATA-OK                                                    
054100             MOVE MED-1 (SPRAK-IX)     TO MOD-TEMFSINF                    
054200             MOVE W-IDARTNR-BYT        TO W-IDARTNR                       
054300             PERFORM CB-UPPDATERA-BYART                                   
054400             PERFORM S02-RENSA-INMATAT                                    
054500          ELSE                                                            
054600             MOVE FEL-5 (SPRAK-IX)     TO MOD-TEMFSFEL                    
054700          END-IF                                                          
054800        END-IF                                                            
054900     END-IF                                                               
055000     .                                                                    
055100     EJECT                                                                
055200 CA-KOLLA-INDATA-NYUPPL SECTION.                                          
055300     MOVE 'CA-KOLLA-INDATA-NYUPPL' TO WS-SEKTION                          
055400*    DISPLAY WS-SEKTION                                                   
055500*OBSERVERA ATT DET FINNS 2 * 6 OLIKA ADRESSER. EN FÖR VANLIGA             
055600*OBJEKT OCH EN ADRESS FÖR GARANTI-OBJEKT.                                 
055700     SKIP2                                                                
055800     IF MID-IDDISTR-RENOV = ALL '+'                                       
055900        CONTINUE                                                          
056000     ELSE                                                                 
056100        IF MID-IDDISTR-RENOV NUMERIC                                      
056200           MOVE MFS-NUM-FAELT-RAETT TO MOD-IDDISTR-RENOV-IN-ATTR          
056300        ELSE                                                              
056400           MOVE NEJ                 TO SW-INDATA-OK                       
056500           MOVE MFS-NUM-FAELT-FEL   TO MOD-IDDISTR-RENOV-IN-ATTR          
056600        END-IF                                                            
056700     END-IF                                                               
056800     IF MID-IDDISTR-RENOV-NDC = ALL '+'                                   
056900        CONTINUE                                                          
057000     ELSE                                                                 
057100        IF MID-IDDISTR-RENOV-NDC NUMERIC                                  
057200           MOVE MFS-NUM-FAELT-RAETT TO                                    
057300                           MOD-IDDISTR-RENOV-NDC-IN-ATTR                  
057400        ELSE                                                              
057500           MOVE NEJ                 TO SW-INDATA-OK                       
057600           MOVE MFS-NUM-FAELT-FEL   TO                                    
057700                           MOD-IDDISTR-RENOV-NDC-IN-ATTR                  
057800        END-IF                                                            
057900     END-IF                                                               
058000     IF MID-IDDISTR-RENOV-PAC = ALL '+'                                   
058100        CONTINUE                                                          
058200     ELSE                                                                 
058300        IF MID-IDDISTR-RENOV-PAC NUMERIC                                  
058400           MOVE MFS-NUM-FAELT-RAETT TO                                    
058500                           MOD-IDDISTR-RENOV-PAC-IN-ATTR                  
058600        ELSE                                                              
058700           MOVE NEJ                 TO SW-INDATA-OK                       
058800           MOVE MFS-NUM-FAELT-FEL   TO                                    
058900                           MOD-IDDISTR-RENOV-PAC-IN-ATTR                  
059000        END-IF                                                            
059100     END-IF                                                               
059200     IF MID-IDDISTR-RENOV-CAN = ALL '+'                                   
059300        CONTINUE                                                          
059400     ELSE                                                                 
059500        IF MID-IDDISTR-RENOV-CAN NUMERIC                                  
059600           MOVE MFS-NUM-FAELT-RAETT TO                                    
059700                           MOD-IDDISTR-RENOV-CAN-IN-ATTR                  
059800        ELSE                                                              
059900           MOVE NEJ                 TO SW-INDATA-OK                       
060000           MOVE MFS-NUM-FAELT-FEL   TO                                    
060100                           MOD-IDDISTR-RENOV-CAN-IN-ATTR                  
060200        END-IF                                                            
060300     END-IF                                                               
060400     IF MID-IDDISTR-RENOV-AUS = ALL '+'                                   
060500        CONTINUE                                                          
060600     ELSE                                                                 
060700        IF MID-IDDISTR-RENOV-AUS NUMERIC                                  
060800           MOVE MFS-NUM-FAELT-RAETT TO                                    
060900                           MOD-IDDISTR-RENOV-AUS-IN-ATTR                  
061000        ELSE                                                              
061100           MOVE NEJ                 TO SW-INDATA-OK                       
061200           MOVE MFS-NUM-FAELT-FEL   TO                                    
061300                           MOD-IDDISTR-RENOV-AUS-IN-ATTR                  
061400        END-IF                                                            
061500     END-IF                                                               
061600     IF MID-IDDISTR-RENOV-CHN = ALL '+'                                   
061700        CONTINUE                                                          
061800     ELSE                                                                 
061900        IF MID-IDDISTR-RENOV-CHN NUMERIC                                  
062000           MOVE MFS-NUM-FAELT-RAETT TO                                    
062100                           MOD-IDDISTR-RENOV-CHN-IN-ATTR                  
062200        ELSE                                                              
062300           MOVE NEJ                 TO SW-INDATA-OK                       
062400           MOVE MFS-NUM-FAELT-FEL   TO                                    
062500                           MOD-IDDISTR-RENOV-CHN-IN-ATTR                  
062600        END-IF                                                            
062700     END-IF                                                               
062801     IF MID-IDDISTR-RENOV-KOR = ALL '+'                                   
062901        CONTINUE                                                          
063001     ELSE                                                                 
063101        IF MID-IDDISTR-RENOV-KOR NUMERIC                                  
063201           MOVE MFS-NUM-FAELT-RAETT TO                                    
063301                           MOD-IDDISTR-RENOV-KOR-IN-ATTR                  
063401        ELSE                                                              
063501           MOVE NEJ                 TO SW-INDATA-OK                       
063601           MOVE MFS-NUM-FAELT-FEL   TO                                    
063701                           MOD-IDDISTR-RENOV-KOR-IN-ATTR                  
063801        END-IF                                                            
063901     END-IF                                                               
063902     IF MID-IDDISTR-RENOV-MY  = ALL '+'                                   
063903        CONTINUE                                                          
063904     ELSE                                                                 
063905        IF MID-IDDISTR-RENOV-MY  NUMERIC                                  
063906           MOVE MFS-NUM-FAELT-RAETT TO                                    
063907                           MOD-IDDISTR-RENOV-MY-IN-ATTR                   
063908        ELSE                                                              
063909           MOVE NEJ                 TO SW-INDATA-OK                       
063910           MOVE MFS-NUM-FAELT-FEL   TO                                    
063920                           MOD-IDDISTR-RENOV-MY-IN-ATTR                   
063930        END-IF                                                            
063940     END-IF                                                               
063950     IF MID-IDDISTR-RENOV-TW  = ALL '+'                                   
063960        CONTINUE                                                          
063970     ELSE                                                                 
063980        IF MID-IDDISTR-RENOV-TW  NUMERIC                                  
063990           MOVE MFS-NUM-FAELT-RAETT TO                                    
064000                           MOD-IDDISTR-RENOV-TW-IN-ATTR                   
064001        ELSE                                                              
064002           MOVE NEJ                 TO SW-INDATA-OK                       
064003           MOVE MFS-NUM-FAELT-FEL   TO                                    
064004                           MOD-IDDISTR-RENOV-TW-IN-ATTR                   
064005        END-IF                                                            
064006     END-IF                                                               
064007     IF MID-IDDISTR-RENOV-TH  = ALL '+'                                   
064008        CONTINUE                                                          
064009     ELSE                                                                 
064010        IF MID-IDDISTR-RENOV-TH  NUMERIC                                  
064011           MOVE MFS-NUM-FAELT-RAETT TO                                    
064012                           MOD-IDDISTR-RENOV-TH-IN-ATTR                   
064013        ELSE                                                              
064014           MOVE NEJ                 TO SW-INDATA-OK                       
064015           MOVE MFS-NUM-FAELT-FEL   TO                                    
064016                           MOD-IDDISTR-RENOV-TH-IN-ATTR                   
064017        END-IF                                                            
064018     END-IF                                                               
064020     IF MID-KVBYTPKO  = ALL '+'                                           
064101        CONTINUE                                                          
064201     ELSE                                                                 
064301        IF MID-KVBYTPKO NUMERIC                                           
064401           MOVE MFS-NUM-FAELT-RAETT    TO MOD-KVBYTPKO-IN-ATTR            
064501        ELSE                                                              
064601           MOVE NEJ                    TO SW-INDATA-OK                    
064701           MOVE MFS-NUM-FAELT-FEL      TO MOD-KVBYTPKO-IN-ATTR            
064801        END-IF                                                            
064901     END-IF                                                               
065001     IF MID-TEBYTKVA1 = ALL '+'                                           
065101        CONTINUE                                                          
065201     ELSE                                                                 
065301        MOVE MFS-ALFA-FAELT-RAETT   TO MOD-TEBYTKVA1-ATTR                 
065401     END-IF                                                               
065501     IF MID-TEBYTKVA2 = ALL '+'                                           
065601        CONTINUE                                                          
065701     ELSE                                                                 
065801        MOVE MFS-ALFA-FAELT-RAETT   TO MOD-TEBYTKVA2-ATTR                 
065901     END-IF                                                               
066001     IF MID-TEBYTKVA3 = ALL '+'                                           
066101        CONTINUE                                                          
066201     ELSE                                                                 
066301        MOVE MFS-ALFA-FAELT-RAETT   TO MOD-TEBYTKVA3-ATTR                 
066401     END-IF                                                               
066501     IF MID-TEBYTKVA4 = ALL '+'                                           
066601        CONTINUE                                                          
066701     ELSE                                                                 
066801        MOVE MFS-ALFA-FAELT-RAETT   TO MOD-TEBYTKVA4-ATTR                 
066901     END-IF                                                               
067001     .                                                                    
067101     EJECT                                                                
067201 CB-UPPDATERA-BYART SECTION.                                              
067301     MOVE 'CB-UPPDATERA-BYART' TO WS-SEKTION                              
067401*    DISPLAY WS-SEKTION                                                   
067501*OBSERVERA ATT DET FINNS 6 * 2 OLIKA ADRESSER. EN FÖR VANLIGA             
067601*OBJEKT OCH EN ADRESS FÖR GARANTI-OBJEKT.                                 
067701     SKIP3                                                                
067801     IF  MID-IDDISTR-RENOV     = ALL '+'                                  
067901     AND MID-IDDISTR-RENOV-NDC = ALL '+'                                  
068001     AND MID-IDDISTR-RENOV-PAC = ALL '+'                                  
068101     AND MID-IDDISTR-RENOV-CAN = ALL '+'                                  
068201     AND MID-IDDISTR-RENOV-AUS = ALL '+'                                  
068301     AND MID-IDDISTR-RENOV-CHN = ALL '+'                                  
068401     AND MID-IDDISTR-RENOV-KOR = ALL '+'                                  
068402     AND MID-IDDISTR-RENOV-MY  = ALL '+'                                  
068403     AND MID-IDDISTR-RENOV-TW  = ALL '+'                                  
068404     AND MID-IDDISTR-RENOV-TH  = ALL '+'                                  
068501     AND MID-KVBYTPKO          = ALL '+'                                  
068601     AND MID-TEBYTKVA1         = ALL '+'                                  
068701     AND MID-TEBYTKVA2         = ALL '+'                                  
068801     AND MID-TEBYTKVA3         = ALL '+'                                  
068901     AND MID-TEBYTKVA4         = ALL '+'                                  
069001        CONTINUE                                                          
069101     ELSE                                                                 
069201        PERFORM DB2-SELECT-BYART                                          
069301        IF MID-IDDISTR-RENOV = ALL '+'                                    
069401           CONTINUE                                                       
069501        ELSE                                                              
069601           MOVE  MID-IDDISTR-RENOV   TO WS-IDDISTR-RENOV                  
069701           MOVE  WS-IDDISTR-RENOV    TO BYART-IDDISTR-RENOV               
069801           MOVE MFS-FORMATETS-ATTR   TO MOD-IDDISTR-RENOV-IN-ATTR         
069901        END-IF                                                            
070001                                                                          
070101        IF MID-IDDISTR-RENOV-NDC = ALL '+'                                
070201           CONTINUE                                                       
070301        ELSE                                                              
070401           MOVE MID-IDDISTR-RENOV-NDC  TO WS-IDDISTR-RENOV-NDC            
070501           MOVE WS-IDDISTR-RENOV-NDC   TO                                 
070601                              BYART-IDDISTR-RENOV-NDC                     
070701           MOVE MFS-FORMATETS-ATTR   TO                                   
070801                              MOD-IDDISTR-RENOV-NDC-IN-ATTR               
070901        END-IF                                                            
071001                                                                          
071101        IF MID-IDDISTR-RENOV-PAC = ALL '+'                                
071201           CONTINUE                                                       
071301        ELSE                                                              
071401           MOVE MID-IDDISTR-RENOV-PAC  TO WS-IDDISTR-RENOV-PAC            
071501           MOVE WS-IDDISTR-RENOV-PAC   TO                                 
071601                              BYART-IDDISTR-RENOV-PAC                     
071701           MOVE MFS-FORMATETS-ATTR   TO                                   
071801                              MOD-IDDISTR-RENOV-PAC-IN-ATTR               
071901        END-IF                                                            
072001                                                                          
072101                                                                          
072201        IF MID-IDDISTR-RENOV-CAN = ALL '+'                                
072301           CONTINUE                                                       
072401        ELSE                                                              
072501           MOVE MID-IDDISTR-RENOV-CAN  TO WS-IDDISTR-RENOV-CAN            
072601           MOVE WS-IDDISTR-RENOV-CAN   TO                                 
072701                              BYART-IDDISTR-RENOV-CAN                     
072801           MOVE MFS-FORMATETS-ATTR   TO                                   
072901                              MOD-IDDISTR-RENOV-CAN-IN-ATTR               
073001        END-IF                                                            
073101                                                                          
073201                                                                          
073301        IF MID-IDDISTR-RENOV-AUS = ALL '+'                                
073401           CONTINUE                                                       
073501        ELSE                                                              
073601           MOVE MID-IDDISTR-RENOV-AUS  TO WS-IDDISTR-RENOV-AUS            
073701           MOVE WS-IDDISTR-RENOV-AUS   TO                                 
073801                              BYART-IDDISTR-RENOV-AUS                     
073901           MOVE MFS-FORMATETS-ATTR   TO                                   
074001                              MOD-IDDISTR-RENOV-AUS-IN-ATTR               
074101        END-IF                                                            
074201                                                                          
074301        IF MID-IDDISTR-RENOV-CHN = ALL '+'                                
074401           CONTINUE                                                       
074501        ELSE                                                              
074601           MOVE MID-IDDISTR-RENOV-CHN  TO WS-IDDISTR-RENOV-CHN            
074701           MOVE WS-IDDISTR-RENOV-CHN   TO                                 
074801                              BYART-IDDISTR-RENOV-CHN                     
074901           MOVE MFS-FORMATETS-ATTR   TO                                   
075001                              MOD-IDDISTR-RENOV-CHN-IN-ATTR               
075101        END-IF                                                            
075201                                                                          
075301        IF MID-IDDISTR-RENOV-KOR = ALL '+'                                
075401           CONTINUE                                                       
075501        ELSE                                                              
075601           MOVE MID-IDDISTR-RENOV-KOR  TO WS-IDDISTR-RENOV-KOR            
075701           MOVE WS-IDDISTR-RENOV-KOR   TO                                 
075801                              BYART-IDDISTR-RENOV-KOR                     
075901           MOVE MFS-FORMATETS-ATTR   TO                                   
076001                              MOD-IDDISTR-RENOV-KOR-IN-ATTR               
076101        END-IF                                                            
076201                                                                          
076202        IF MID-IDDISTR-RENOV-MY  = ALL '+'                                
076203           CONTINUE                                                       
076204        ELSE                                                              
076205           MOVE MID-IDDISTR-RENOV-MY   TO WS-IDDISTR-RENOV-MY             
076206           MOVE WS-IDDISTR-RENOV-MY    TO                                 
076207                              BYART-IDDISTR-RENOV-MY                      
076208           MOVE MFS-FORMATETS-ATTR   TO                                   
076209                              MOD-IDDISTR-RENOV-MY-IN-ATTR                
076210        END-IF                                                            
076220                                                                          
076230        IF MID-IDDISTR-RENOV-TW  = ALL '+'                                
076240           CONTINUE                                                       
076250        ELSE                                                              
076260           MOVE MID-IDDISTR-RENOV-TW   TO WS-IDDISTR-RENOV-TW             
076270           MOVE WS-IDDISTR-RENOV-TW    TO                                 
076280                              BYART-IDDISTR-RENOV-TW                      
076290           MOVE MFS-FORMATETS-ATTR   TO                                   
076300                              MOD-IDDISTR-RENOV-TW-IN-ATTR                
076301        END-IF                                                            
076302                                                                          
076303        IF MID-IDDISTR-RENOV-TH  = ALL '+'                                
076304           CONTINUE                                                       
076305        ELSE                                                              
076306           MOVE MID-IDDISTR-RENOV-TH   TO WS-IDDISTR-RENOV-TH             
076307           MOVE WS-IDDISTR-RENOV-TH    TO                                 
076308                              BYART-IDDISTR-RENOV-TH                      
076309           MOVE MFS-FORMATETS-ATTR   TO                                   
076310                              MOD-IDDISTR-RENOV-TH-IN-ATTR                
076311        END-IF                                                            
076312                                                                          
076320        IF MID-KVBYTPKO = ALL '+'                                         
076401           CONTINUE                                                       
076501        ELSE                                                              
076601           MOVE MID-KVBYTPKO        TO BYART-KVBYTPKO                     
076701           MOVE MFS-FORMATETS-ATTR   TO MOD-KVBYTPKO-IN-ATTR              
076801        END-IF                                                            
076901                                                                          
077001        IF  MID-TEBYTKVA1 = ALL '+'                                       
077101           CONTINUE                                                       
077201        ELSE                                                              
077301           MOVE MID-TEBYTKVA1        TO BYART-TEBYTKVA1                   
077401           MOVE MFS-FORMATETS-ATTR   TO MOD-TEBYTKVA1-ATTR                
077501        END-IF                                                            
077601        IF  MID-TEBYTKVA2 = ALL '+'                                       
077701           CONTINUE                                                       
077801        ELSE                                                              
077901           MOVE MID-TEBYTKVA2        TO BYART-TEBYTKVA2                   
078001           MOVE MFS-FORMATETS-ATTR   TO MOD-TEBYTKVA2-ATTR                
078101        END-IF                                                            
078201        IF  MID-TEBYTKVA3 = ALL '+'                                       
078301           CONTINUE                                                       
078401        ELSE                                                              
078501           MOVE MID-TEBYTKVA3        TO BYART-TEBYTKVA3                   
078601           MOVE MFS-FORMATETS-ATTR   TO MOD-TEBYTKVA3-ATTR                
078701        END-IF                                                            
078801        IF  MID-TEBYTKVA4 = ALL '+'                                       
078901           CONTINUE                                                       
079001        ELSE                                                              
079101           MOVE MID-TEBYTKVA4        TO BYART-TEBYTKVA4                   
079201           MOVE MFS-FORMATETS-ATTR   TO MOD-TEBYTKVA4-ATTR                
079301        END-IF                                                            
079401*       PERFORM S01-FLYTT-BYART-T-BILD                                    
079501        PERFORM DB2-UPDATE-BYART                                          
079601     END-IF                                                               
079701     .                                                                    
079801     EJECT                                                                
079901 D-KOLLA-ATT-INGET-IFYLLT  SECTION.                                       
080001     MOVE 'D-KOLLA-ATT-INGET-IFYLLT' TO WS-SEKTION                        
080101*    DISPLAY WS-SEKTION                                                   
080201     SKIP2                                                                
080301     MOVE NEJ                         TO SW-INMATAT                       
080401                                                                          
080501     IF  MID-IDDISTR-RENOV     = ALL '+'                                  
080601        CONTINUE                                                          
080701     ELSE                                                                 
080801        MOVE JA                     TO SW-INMATAT                         
080901        MOVE MFS-ADD-LAES-IN-FAELT  TO                                    
081001                          MOD-IDDISTR-RENOV-IN-ATTR                       
081101                                                                          
081201     END-IF                                                               
081301                                                                          
081401     IF  MID-IDDISTR-RENOV-NDC = ALL '+'                                  
081501        CONTINUE                                                          
081601     ELSE                                                                 
081701        MOVE JA                     TO SW-INMATAT                         
081801        MOVE MFS-ADD-LAES-IN-FAELT  TO                                    
081901                          MOD-IDDISTR-RENOV-NDC-IN-ATTR                   
082001                                                                          
082101     END-IF                                                               
082201                                                                          
082301                                                                          
082401     IF  MID-IDDISTR-RENOV-PAC = ALL '+'                                  
082501        CONTINUE                                                          
082601     ELSE                                                                 
082701        MOVE JA                     TO SW-INMATAT                         
082801        MOVE MFS-ADD-LAES-IN-FAELT  TO                                    
082901                          MOD-IDDISTR-RENOV-PAC-IN-ATTR                   
083001                                                                          
083101     END-IF                                                               
083201                                                                          
083301     IF  MID-IDDISTR-RENOV-CAN = ALL '+'                                  
083401        CONTINUE                                                          
083501     ELSE                                                                 
083601        MOVE JA                     TO SW-INMATAT                         
083701        MOVE MFS-ADD-LAES-IN-FAELT  TO                                    
083801                          MOD-IDDISTR-RENOV-CAN-IN-ATTR                   
083901     END-IF                                                               
084001                                                                          
084101     IF  MID-IDDISTR-RENOV-AUS = ALL '+'                                  
084201        CONTINUE                                                          
084301     ELSE                                                                 
084401        MOVE JA                     TO SW-INMATAT                         
084501        MOVE MFS-ADD-LAES-IN-FAELT  TO                                    
084601                          MOD-IDDISTR-RENOV-AUS-IN-ATTR                   
084701     END-IF                                                               
084801                                                                          
084901     IF  MID-IDDISTR-RENOV-CHN = ALL '+'                                  
085001        CONTINUE                                                          
085101     ELSE                                                                 
085201        MOVE JA                     TO SW-INMATAT                         
085301        MOVE MFS-ADD-LAES-IN-FAELT  TO                                    
085401                          MOD-IDDISTR-RENOV-CHN-IN-ATTR                   
085501     END-IF                                                               
085601                                                                          
085701     IF  MID-IDDISTR-RENOV-KOR = ALL '+'                                  
085801        CONTINUE                                                          
085901     ELSE                                                                 
086001        MOVE JA                     TO SW-INMATAT                         
086101        MOVE MFS-ADD-LAES-IN-FAELT  TO                                    
086201                          MOD-IDDISTR-RENOV-KOR-IN-ATTR                   
086301     END-IF                                                               
086401                                                                          
086402     IF  MID-IDDISTR-RENOV-MY  = ALL '+'                                  
086403        CONTINUE                                                          
086404     ELSE                                                                 
086405        MOVE JA                     TO SW-INMATAT                         
086406        MOVE MFS-ADD-LAES-IN-FAELT  TO                                    
086407                          MOD-IDDISTR-RENOV-MY-IN-ATTR                    
086408     END-IF                                                               
086409                                                                          
086410     IF  MID-IDDISTR-RENOV-TW  = ALL '+'                                  
086420        CONTINUE                                                          
086430     ELSE                                                                 
086440        MOVE JA                     TO SW-INMATAT                         
086450        MOVE MFS-ADD-LAES-IN-FAELT  TO                                    
086460                          MOD-IDDISTR-RENOV-TW-IN-ATTR                    
086470     END-IF                                                               
086480                                                                          
086490     IF  MID-IDDISTR-RENOV-TH  = ALL '+'                                  
086500        CONTINUE                                                          
086501     ELSE                                                                 
086502        MOVE JA                     TO SW-INMATAT                         
086503        MOVE MFS-ADD-LAES-IN-FAELT  TO                                    
086504                          MOD-IDDISTR-RENOV-TH-IN-ATTR                    
086505     END-IF                                                               
086506                                                                          
086507     IF MID-KVBYTPKO = ALL '+'                                            
086601        CONTINUE                                                          
086701     ELSE                                                                 
086801        MOVE JA                     TO SW-INMATAT                         
086901        MOVE MFS-ADD-LAES-IN-FAELT  TO                                    
087001                                    MOD-KVBYTPKO-IN-ATTR                  
087101     END-IF                                                               
087201                                                                          
087301     IF  MID-TEBYTKVA1 = ALL '+'                                          
087401        CONTINUE                                                          
087501     ELSE                                                                 
087601        MOVE JA                     TO SW-INMATAT                         
087701        MOVE MFS-ADD-LAES-IN-FAELT  TO                                    
087801                                   MOD-TEBYTKVA1-ATTR                     
087901     END-IF                                                               
088001     IF  MID-TEBYTKVA2 = ALL '+'                                          
088101        CONTINUE                                                          
088201     ELSE                                                                 
088301        MOVE JA                     TO SW-INMATAT                         
088401        MOVE MFS-ADD-LAES-IN-FAELT  TO                                    
088501                                   MOD-TEBYTKVA2-ATTR                     
088601     END-IF                                                               
088701     IF  MID-TEBYTKVA3 = ALL '+'                                          
088801        CONTINUE                                                          
088901     ELSE                                                                 
089001        MOVE JA                     TO SW-INMATAT                         
089101        MOVE MFS-ADD-LAES-IN-FAELT  TO                                    
089201                                   MOD-TEBYTKVA3-ATTR                     
089301     END-IF                                                               
089401     IF  MID-TEBYTKVA4 = ALL '+'                                          
089501        CONTINUE                                                          
089601     ELSE                                                                 
089701        MOVE JA                     TO SW-INMATAT                         
089801        MOVE MFS-ADD-LAES-IN-FAELT  TO                                    
089901                                   MOD-TEBYTKVA4-ATTR                     
090001     END-IF                                                               
090101     .                                                                    
090201     EJECT                                                                
090301 E-VISA-SIDAN SECTION.                                                    
090401     MOVE 'E-VISA-SIDAN' TO WS-SEKTION                                    
090501*    DISPLAY WS-SEKTION                                                   
090601     SKIP2                                                                
090701     PERFORM DB2-SELECT-BYART                                             
090801     IF RADER-FINNS                                                       
090901        PERFORM IMS-GU-BENA11                                             
091001        IF SEGMENT-FINNS                                                  
091101           MOVE BENA11-TEXT-BEART   TO                                    
091201                                    MOD-BEART-SVE                         
091301        END-IF                                                            
091401        PERFORM S01-FLYTT-BYART-T-BILD                                    
091501        PERFORM S02-RENSA-INMATAT                                         
091601     ELSE                                                                 
091701        PERFORM S03-RENSA-SIDAN                                           
091801        IF 904-KOD                                                        
091901           MOVE FEL-8 (SPRAK-IX)    TO MOD-TEMFSFEL                       
092001           MOVE MED-4 (SPRAK-IX)    TO MOD-TEMFSINF                       
092101        ELSE                                                              
092201           MOVE FEL-3 (SPRAK-IX)    TO MOD-TEMFSFEL                       
092301           MOVE SPACE               TO MOD-TEMFSINF                       
092401        END-IF                                                            
092501     END-IF                                                               
092601                                                                          
092701     .                                                                    
092801     EJECT                                                                
092901 S01-FLYTT-BYART-T-BILD SECTION.                                          
093001     MOVE 'S01-FLYTT-BYART-T-BILD' TO WS-SEKTION                          
093101*    DISPLAY WS-SEKTION                                                   
093201     SKIP2                                                                
093301     MOVE BYART-ADLAGOMR          TO MOD-ADLAGOMR-UT                      
093401     MOVE BYART-ADGANG            TO MOD-ADGANG-UT                        
093501     MOVE BYART-ADPLATS           TO MOD-ADPLATS-UT                       
093601     MOVE BYART-IDDISTR-RENOV     TO MOD-IDDISTR-RENOV-UT                 
093701     MOVE BYART-IDDISTR-RENOV-NDC TO MOD-IDDISTR-RENOV-NDC-UT             
093801     MOVE BYART-IDDISTR-RENOV-PAC TO MOD-IDDISTR-RENOV-PAC-UT             
093901     MOVE BYART-IDDISTR-RENOV-CAN TO MOD-IDDISTR-RENOV-CAN-UT             
094001     MOVE BYART-IDDISTR-RENOV-AUS TO MOD-IDDISTR-RENOV-AUS-UT             
094101     MOVE BYART-IDDISTR-RENOV-CHN TO MOD-IDDISTR-RENOV-CHN-UT             
094201     MOVE BYART-IDDISTR-RENOV-KOR TO MOD-IDDISTR-RENOV-KOR-UT             
094202     MOVE BYART-IDDISTR-RENOV-MY  TO MOD-IDDISTR-RENOV-MY-UT              
094203     MOVE BYART-IDDISTR-RENOV-TW  TO MOD-IDDISTR-RENOV-TW-UT              
094204     MOVE BYART-IDDISTR-RENOV-TH  TO MOD-IDDISTR-RENOV-TH-UT              
094301     MOVE BYART-KVBYTPKO          TO MOD-KVBYTPKO-UT                      
094401     MOVE BYART-TEBYTKVA1         TO MOD-TEBYTKVA1                        
094501     MOVE BYART-TEBYTKVA2         TO MOD-TEBYTKVA2                        
094601     MOVE BYART-TEBYTKVA3         TO MOD-TEBYTKVA3                        
094701     MOVE BYART-TEBYTKVA4         TO MOD-TEBYTKVA4                        
094801                                                                          
094901     .                                                                    
095001     EJECT                                                                
095101 S02-RENSA-INMATAT SECTION.                                               
095201     MOVE 'S02-RENSA-INMATAT' TO WS-SEKTION                               
095301*    DISPLAY WS-SEKTION                                                   
095401     SKIP2                                                                
095501     MOVE MFS-RENSA-FAELT          TO MOD-IDDISTR-RENOV-IN                
095601                                      MOD-IDDISTR-RENOV-NDC-IN            
095701                                      MOD-IDDISTR-RENOV-CAN-IN            
095801                                      MOD-IDDISTR-RENOV-PAC-IN            
095901                                      MOD-IDDISTR-RENOV-AUS-IN            
096001                                      MOD-IDDISTR-RENOV-CHN-IN            
096101                                      MOD-IDDISTR-RENOV-KOR-IN            
096102                                      MOD-IDDISTR-RENOV-MY-IN             
096103                                      MOD-IDDISTR-RENOV-TW-IN             
096104                                      MOD-IDDISTR-RENOV-TH-IN             
096201                                      MOD-KVBYTPKO-IN                     
096301     .                                                                    
096401     EJECT                                                                
096501 S03-RENSA-SIDAN SECTION.                                                 
096601     MOVE 'S03-RENSA-SIDAN' TO WS-SEKTION                                 
096701*    DISPLAY WS-SEKTION                                                   
096801     SKIP2                                                                
096901     MOVE MFS-RENSA-FAELT          TO MOD-ADLAGOMR-UT                     
097001                                      MOD-ADGANG-UT                       
097101                                      MOD-ADPLATS-UT                      
097201                                      MOD-IDDISTR-RENOV-UT                
097301                                      MOD-IDDISTR-RENOV-NDC-UT            
097401                                      MOD-IDDISTR-RENOV-CAN-UT            
097501                                      MOD-IDDISTR-RENOV-PAC-UT            
097601                                      MOD-IDDISTR-RENOV-AUS-UT            
097701                                      MOD-IDDISTR-RENOV-CHN-UT            
097801                                      MOD-IDDISTR-RENOV-KOR-UT            
097802                                      MOD-IDDISTR-RENOV-MY-UT             
097803                                      MOD-IDDISTR-RENOV-TW-UT             
097804                                      MOD-IDDISTR-RENOV-TH-UT             
097901                                      MOD-KVBYTPKO-UT                     
098001                                      MOD-TEBYTKVA1                       
098101                                      MOD-TEBYTKVA2                       
098201                                      MOD-TEBYTKVA3                       
098301                                      MOD-TEBYTKVA4                       
098401                                      MOD-BEART-SVE                       
098501     .                                                                    
098601     EJECT                                                                
098701* IMS SEKTIONER                                                           
098801     SKIP3                                                                
098901 IMS-GET-MSG SECTION.                                                     
099001     MOVE 'IMS-GET-MSG'     TO WS-IMS-SEKTION                             
099101*    DISPLAY WS-SEKTION                                                   
099201     SKIP1                                                                
099301     MOVE '  QC' TO GODK-STATUSKODER                                      
099401     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
099501     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
099601     PERFORM IMS-STATUSKONTROLL                                           
099701     SKIP3                                                                
099801     .                                                                    
099901 IMS-INSERT-MSG SECTION.                                                  
100001     MOVE 'IMS-INSERT-MSG'     TO WS-IMS-SEKTION                          
100101     SKIP1                                                                
100201     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
100301     MOVE SPACE TO GODK-STATUSKODER                                       
100401     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
100501     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
100601     PERFORM IMS-STATUSKONTROLL                                           
100701     EJECT                                                                
100801     .                                                                    
100901 IMS-GU-ARTC01 SECTION.                                                   
101001     MOVE 'IMS-GU-ARTC01'     TO WS-IMS-SEKTION                           
101101     SKIP1                                                                
101201     STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-X ')'                         
101301            DELIMITED BY SIZE INTO SSA1                                   
101401     MOVE '  GE' TO GODK-STATUSKODER                                      
101501     CALL CBLTDLI USING GU ARTC-PCB DLI-IO-AREA SSA1                      
101601     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
101701     PERFORM IMS-STATUSKONTROLL                                           
101801     SKIP3                                                                
101901     .                                                                    
102001 IMS-GU-BENA11 SECTION.                                                   
102101     MOVE 'IMS-GU-BENA11'     TO WS-IMS-SEKTION                           
102201     SKIP1                                                                
102301     STRING 'WLBENA01(WDD3BSEQ =' W-IDARTNR-X ')'                         
102401            DELIMITED BY SIZE INTO SSA1                                   
102501     STRING 'WLBENA11(IDSKYLT  =' W-IDSKYLT-X ')'                         
102601            DELIMITED BY SIZE INTO SSA2                                   
102701     MOVE '  GE' TO GODK-STATUSKODER                                      
102801     CALL CBLTDLI USING GU BENA-PCB DLI-IO-AREA SSA1 SSA2                 
102901     MOVE BENA-STATUS-CODE TO STATUS-WS                                   
103001     PERFORM IMS-STATUSKONTROLL                                           
103101     SKIP3                                                                
103201     .                                                                    
103301 IMS-STATUSKONTROLL SECTION.                                              
103401     SKIP1                                                                
103501     SET STATUS-IX TO 1                                                   
103601     SEARCH GODK-STATUS                                                   
103701       AT END                                                             
103801         CALL FELLOG                                                      
103901        WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                          
104001           CONTINUE                                                       
104101     END-SEARCH                                                           
104201     .                                                                    
104301     EJECT                                                                
104401 DB2-SELECT-BYART SECTION.                                                
104501     MOVE 'DB2-SELECT-BYART'     TO WS-IMS-SEKTION                        
104601     MOVE 000100904         TO GODK-SQLCODESKODER                         
104701     EXEC SQL SELECT                                                      
104801                  IDARTNR_BYT,                                            
104901                  IDDISTR_RENOV,                                          
105001                  ADLAGOMR, ADGANG,                                       
105101                  ADPLATS,                                                
105201                  KVBYTPKO,                                               
105301                  TEBYTKVA1, TEBYTKVA2,                                   
105401                  TEBYTKVA3, TEBYTKVA4,                                   
105501                  IDDISTR_RENOV_NDC,                                      
105601                  IDDISTR_RENOV_PAC,                                      
105701                  IDDISTR_RENOV_CAN,                                      
105801                  IDDISTR_RENOV_AUS,                                      
105901                  IDDISTR_RENOV_CHN,                                      
106001                  IDDISTR_RENOV_KOR,                                      
106002                  IDDISTR_RENOV_MY,                                       
106003                  IDDISTR_RENOV_TW,                                       
106004                  IDDISTR_RENOV_TH                                        
106101              INTO                                                        
106201                  :BYART-IDARTNR-BYT,                                     
106301                  :BYART-IDDISTR-RENOV,                                   
106401                  :BYART-ADLAGOMR,                                        
106501                  :BYART-ADGANG,                                          
106601                  :BYART-ADPLATS,                                         
106701                  :BYART-KVBYTPKO,                                        
106801                  :BYART-TEBYTKVA1,                                       
106901                  :BYART-TEBYTKVA2,                                       
107001                  :BYART-TEBYTKVA3,                                       
107101                  :BYART-TEBYTKVA4,                                       
107201                  :BYART-IDDISTR-RENOV-NDC,                               
107301                  :BYART-IDDISTR-RENOV-PAC,                               
107401                  :BYART-IDDISTR-RENOV-CAN,                               
107501                  :BYART-IDDISTR-RENOV-AUS,                               
107601                  :BYART-IDDISTR-RENOV-CHN,                               
107701                  :BYART-IDDISTR-RENOV-KOR,                               
107702                  :BYART-IDDISTR-RENOV-MY,                                
107703                  :BYART-IDDISTR-RENOV-TW,                                
107704                  :BYART-IDDISTR-RENOV-TH                                 
107801            FROM BYART                                                    
107901            WHERE IDARTNR_BYT = :W-IDARTNR-BYT                            
108001     END-EXEC                                                             
108101     MOVE SQLCODE           TO SQLCODE-WS                                 
108201     PERFORM DB2-STATUSKONTROLL                                           
108301     .                                                                    
108401 DB2-UPDATE-BYART    SECTION.                                             
108501     MOVE 'DB2-UPDATE-BYART'     TO WS-IMS-SEKTION                        
108601     SKIP2                                                                
108701     MOVE 000               TO GODK-SQLCODESKODER                         
108801     EXEC SQL UPDATE BYART                                                
108901        SET IDDISTR_RENOV     = :BYART-IDDISTR-RENOV,                     
109001            KVBYTPKO          = :BYART-KVBYTPKO,                          
109101            TEBYTKVA1         = :BYART-TEBYTKVA1,                         
109201            TEBYTKVA2         = :BYART-TEBYTKVA2,                         
109301            TEBYTKVA3         = :BYART-TEBYTKVA3,                         
109401            TEBYTKVA4         = :BYART-TEBYTKVA4,                         
109501            IDDISTR_RENOV_NDC = :BYART-IDDISTR-RENOV-NDC,                 
109601            IDDISTR_RENOV_PAC = :BYART-IDDISTR-RENOV-PAC,                 
109701            IDDISTR_RENOV_CAN = :BYART-IDDISTR-RENOV-CAN,                 
109801            IDDISTR_RENOV_AUS = :BYART-IDDISTR-RENOV-AUS,                 
109901            IDDISTR_RENOV_CHN = :BYART-IDDISTR-RENOV-CHN,                 
110001            IDDISTR_RENOV_KOR = :BYART-IDDISTR-RENOV-KOR,                 
110002            IDDISTR_RENOV_MY  = :BYART-IDDISTR-RENOV-MY,                  
110003            IDDISTR_RENOV_TW  = :BYART-IDDISTR-RENOV-TW,                  
110004            IDDISTR_RENOV_TH  = :BYART-IDDISTR-RENOV-TH                   
110101     WHERE IDARTNR_BYT        = :W-IDARTNR-BYT                            
110201     END-EXEC                                                             
110301     MOVE SQLCODE           TO SQLCODE-WS                                 
110401     PERFORM DB2-STATUSKONTROLL                                           
110501     .                                                                    
110601     EJECT                                                                
110701 DB2-STATUSKONTROLL SECTION.                                              
110801     SKIP2                                                                
110901     SET SQLCODE-IX          TO 1                                         
111001     SEARCH GODK-SQLCODE                                                  
111101       AT END                                                             
111201         CALL FELLOG                                                      
111301        WHEN GODK-SQLCODE (SQLCODE-IX) = SQLCODE-WS                       
111401           CONTINUE                                                       
112001     END-SEARCH                                                           
120001     .                                                                    
