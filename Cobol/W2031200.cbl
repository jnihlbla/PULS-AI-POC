000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.     W2031200.                                                
000400 AUTHOR.         BERT ANDERSSON.                                          
000500 DATE-WRITTEN.   90/10/25.                                                
000600     REMARKS.                                                             
000700*                                                                         
000800*    FUNKTION.                                                            
000900*        PROGRAMMET ÄR ETT FRÅGE-/UPPDATERINGS-PROGRAM MOT                
001000*        MARKNADSREG.(XXKS) OCH ANTALSREG.(XXKT).                         
001100*        PROG. UPPGIFT ÄR ATT UTFÖRA NYREG., ÄNDRING OCH BORTTAG          
001200*        AV DE KUNDER SOM FÅR BESTÄLLA EN ARTIKEL I EN KAMPANJ.           
001300*        BILDEN VISAR ÄVEN HUR MYCKET AV EN ARTIKEL SOM VARJE             
001400*        KUND FÅR BESTÄLLA.                                               
001500*        EN KAMPANJREF. KAN ENDAST REGISTRERAS PÅ XXKS OM                 
001600*        ANGIVEN ARTIKEL FINNS PÅ XXKT SAMT IDDISTRIKT                    
001700*        REGISTRERAS SAMTIDIGT PÅ XXKS.                                   
001800*                                                                         
001900*        I PROGRAMMET FINNS MÖJLIGHET ATT:                                
002000*        - SÖKA PÅ KAMPANJREFERENS.                                       
002100*        - ÄNDRA/UPPDATERA OCH NYREGISTRERA KAMPANJREFERENS OCH           
002200*          ARTIKLAR INGÅENDE I KAMPANJREFERENSEN.                         
002300*        - BLÄDDRA GENOM IDDISTRIKT.                                      
002400*        PROGRAMMET ÄR EN UPPDATERINGS-MPP                                
002500*        PROGRAMMET UPPDATERAR WDM2                                       
002700*                                                                         
002800*    INDATA.                                                              
002900*        TRANSAKTION: W2T312                                              
003000*        MID:         W2I31201                                            
003100*                                                                         
003200*    UTDATA.                                                              
003300*        MOD:         W2O31201                                            
003400*                                                                         
003500* ETRACK 1436917                                                          
003510* ETRACK 10228562 2016 ÄNDRAT FRÅN WLXXKR/XXKT/XXKS TILL WDM2             
003600                                                                          
003700     SKIP3                                                                
003800 ENVIRONMENT DIVISION.                                                    
003900     EJECT                                                                
004000 DATA DIVISION.                                                           
004100 WORKING-STORAGE SECTION.                                                 
004200*    -- CHECKED BY WY2000                                                 
004310 77  IDPGM                       PIC X(08)   VALUE 'W2031200'.            
004400                                                                          
004500 77  CURRENT-SECTION             PIC X(80)   VALUE SPACE.                 
004600 77  CURRENT-IMS-SECTION         PIC X(80)   VALUE SPACE.                 
004700                                                                          
004800 77  JA                          PIC X       VALUE 'J'.                   
004900 77  YES                         PIC X       VALUE 'Y'.                   
005000 77  NEJ                         PIC X       VALUE 'N'.                   
005100                                                                          
005200*    --- INDEX FÖR BLÄDDRINGSRADER                                        
005300 77  INDX                        PIC S9(4)  VALUE +0    COMP SYNC.        
005400 77  WS-IX                       PIC S9(4)  VALUE +0    COMP SYNC.        
005500 77  MAX-INDX                    PIC S9(4)  VALUE +012  COMP SYNC.        
005600                                                                          
005700 77  SPRAK-IX                    PIC S9(9)  VALUE +0    COMP SYNC.        
005800 77  MAX-MOD-LAENGD              PIC S9(4)  VALUE +868  COMP SYNC.        
005900                                                                          
006000*********************************************************                 
006100*    SAVE-AREA                                                            
006200*           ANVÄNDS FÖR ATT SPARA PÅ NYCKELDATABASEN WDP7                 
006300*           (I MSGI-SPAR-AREA)                                            
006400*********************************************************                 
006500 01  SAVE-AREA.                                                           
006600    10 SAVE-IDTRANS              PIC X(4)    VALUE '2312'.                
006601    10 SAVE-KEY.                                                          
006610      15 SAVE-IDKAMPRF-KEY       PIC X(7)    VALUE SPACE.                 
006620      15 SAVE-IDDC-KEY           PIC X(2)    VALUE SPACE.                 
006630      15 SAVE-IDARTNR-KEY        PIC X(9)    VALUE SPACE.                 
006700    10 SAVE-KEY-ENTER.                                                    
006820      15 SAVE-IDDISTR-ENTER-FOM  PIC 9(4)    VALUE ZERO.                  
006900      15 SAVE-IDDISTR-ENTER-TOM  PIC 9(4)    VALUE ZERO.                  
007000      15 SAVE-IDKUNDNR-ENTER-FOM PIC 9(6)    VALUE ZERO.                  
007100      15 SAVE-IDKUNDNR-ENTER-TOM PIC 9(6)    VALUE ZERO.                  
007200    10 SAVE-KEY-NEXT.                                                     
007300      15 SAVE-IDDISTR-FOM-NEXT   PIC 9(4)    VALUE ZERO.                  
007400      15 SAVE-IDDISTR-TOM-NEXT   PIC 9(4)    VALUE ZERO.                  
007500      15 SAVE-IDKUNDNR-FOM-NEXT  PIC 9(6)    VALUE ZERO.                  
007600      15 SAVE-IDKUNDNR-TOM-NEXT  PIC 9(6)    VALUE ZERO.                  
007900                                                                          
008000 77  WS-KAMPRF                   PIC X(7).                                
008100 77  WS-KAMPRF-COPY              PIC X(7).                                
008200 77  WS-IDDC-COPY                PIC X(2).                                
008300 77  WS-IDDC                     PIC X(2).                                
008400 77  WS-IDARTNR                  PIC X(9).                                
008500 77  WS-IDARTNR-COPY             PIC X(9).                                
008600 77  WS-IDDISTR                  PIC 9(4)  VALUE ZERO.                    
008700 77  WS-IDDISTR-TOM              PIC 9(4)  VALUE ZERO.                    
008800 77  WS-IDDISTR-FROM             PIC S9(4) VALUE ZERO COMP-3.             
008900 77  WS-KVBEART-KAMP             PIC 9(6)  VALUE ZERO.                    
009000 77  WS-KVBEART-REM              PIC S9(7) VALUE ZERO COMP-3.             
009100 77  WS-KVBEART-KAMP-1           PIC S9(7) VALUE ZERO COMP-3.             
009200 77  WS-KVBEART-KAMP-2           PIC S9(7) VALUE ZERO COMP-3.             
009300 77  WS-KVKVARFD-1               PIC S9(7) VALUE ZERO COMP-3.             
009400 77  WS-KVBEART-REM-1            PIC S9(7) VALUE ZERO COMP-3.             
009500 77  WS-KVBEART-KUND-1           PIC S9(7) VALUE ZERO COMP-3.             
009600 77  ACC-KMRK-KVBEART-KAMP       PIC 9(6)  VALUE ZERO.                    
009700 77  ACC-KART-KVBEART-KAMP       PIC 9(6)  VALUE ZERO.                    
009800 77  WS-KVBEART-KUND             PIC 9(6)  VALUE ZERO.                    
009900 77  SPAR-KMRK-KVBEART-KAMP      PIC 9(6)  VALUE ZERO.                    
010000 77  SPAR-KMRK-KVBEART-KUND      PIC 9(6)  VALUE ZERO.                    
010100 77  WS-KVKVARFD                 PIC 9(7)  VALUE ZERO.                    
010200 77  WS-FLER-XXKS11-FINNS        PIC X(1).                                
010300 77  TODAYS-DATE                 PIC S9(7) VALUE ZERO COMP-3.             
010400                                                                          
010500 01  WS-INFO                     PIC X(55)  VALUE SPACE.                  
010600 01  SPRAKNR                     PIC S9(9).                               
010700 01  WS-FILLER      REDEFINES  SPRAKNR.                                   
010800     03  FILLER                      PIC 9(8).                            
010900     03  WS-SPRAK                    PIC 9.                               
011000                                                                          
011100 01  WS-IDKUNDNR7                PIC S9(7)  VALUE ZERO.                   
011200 01  WS-FILLER      REDEFINES  WS-IDKUNDNR7.                              
011300     03  FILLER                      PIC 9.                               
011400     03  WS-IDKUNDNR                 PIC 9(6).                            
011500 01  WS-FILLER      REDEFINES  WS-IDKUNDNR7.                              
011600     03  FILLER                      PIC 9.                               
011700     03  WS-EDKUNDNR                 PIC Z(5)9.                           
011800                                                                          
011900 01  WS-IDKUNDNR7-TOM            PIC S9(7)  VALUE ZERO.                   
012000 01  WS-FILLER      REDEFINES  WS-IDKUNDNR7-TOM.                          
012100     03  FILLER                      PIC 9.                               
012200     03  WS-IDKUNDNR-TOM             PIC 9(6).                            
012300 01  WS-FILLER      REDEFINES  WS-IDKUNDNR7-TOM.                          
012400     03  FILLER                      PIC 9.                               
012500     03  WS-EDKUNDNR-TOM             PIC Z(5)9.                           
012600                                                                          
012700 01  WS-ED                       PIC Z(5)9.                               
012800 01  FILLER         REDEFINES  WS-ED.                                     
012900     03  WS-NUM9                     PIC 9(6).                            
013000                                                                          
013100 77  WDM211-UPDATE               PIC X.                                   
013200     88  WDM211-AENDRING                     VALUE '1'.                   
013300     88  WDM211-NYUPPLAEGG                   VALUE '2'.                   
013400     88  WDM211-NO-UPDATE                    VALUE '3'.                   
013500                                                                          
013600 77  INDATA-SW                   PIC X       VALUE 'J'.                   
013700     88  INDATA-OK                           VALUE 'J'.                   
013800     88  INDATA-FEL                          VALUE 'N'.                   
013900                                                                          
014000 77  WDM221-SW                   PIC X       VALUE 'N'.                   
014100     88  WDM221-FINNS                        VALUE 'J'.                   
014200     88  WDM221-SAKNAS                       VALUE 'N'.                   
014300                                                                          
014400 77  ART-REG-PA-KAMPANJ-SW       PIC X       VALUE 'J'.                   
014500     88  ART-REG-PA-KAMPANJ                  VALUE 'J'.                   
014600                                                                          
014700 77  INDATA-EXISTS-SW            PIC X       VALUE 'J'.                   
014800     88  INDATA-EXISTS                       VALUE 'J'.                   
014900                                                                          
015000 77  UPDATE-SW                   PIC X       VALUE 'J'.                   
015100     88  UPDATE-OK                           VALUE 'J'.                   
015200                                                                          
015300 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
015400     88  NYCKLAR-OK                          VALUE 'J'.                   
015500     88  NYCKLAR-FEL                         VALUE 'N'.                   
015600                                                                          
015700 77  INTERVALL-SW                PIC X       VALUE 'J'.                   
015800     88  INTERVALL-OVERLAPP                  VALUE 'N'.                   
015900     88  INTERVALL-OUTSIDE                   VALUE 'J'.                   
016000                                                                          
016100 77  NEW-INSERT-SW               PIC X       VALUE 'N'.                   
016200     88  NO-TO-NEW-INSERT                    VALUE 'N'.                   
016300     88  YES-TO-NEW-INSERT                   VALUE 'J'.                   
016400                                                                          
016500 77  COPY-CAMP-ART-SW            PIC X       VALUE 'N'.                   
016600     88  NO-TO-COPY                          VALUE 'N'.                   
016700     88  YES-TO-COPY                         VALUE 'J'.                   
016800                                                                          
016900 77  UPDATE-DELETE-SW            PIC X       VALUE 'N'.                   
017000     88  NO-TO-UPDATE-DELETE                 VALUE 'N'.                   
017100     88  YES-TO-UPDATE-DELETE                VALUE 'J'.                   
017200                                                                          
017300 77  NYA-NYCKLAR-SW              PIC X       VALUE 'N'.                   
017400     88  NYA-NYCKLAR                         VALUE 'J'.                   
017500                                                                          
017600 77  ALLT-SW                     PIC X       VALUE 'J'.                   
017700     88  ALLT-OK                             VALUE 'J'.                   
017800                                                                          
017900 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
018000     88  EGEN-MID                            VALUE '2312'.                
018010     88  2317-MID                            VALUE '2317'.                
018100     88  GODK-MID                            VALUE '2311' '2312'          
018200                                                   '2317'.                
018300 77  VISA-ERR-MED-SW             PIC X       VALUE 'N'.                   
018400     88  ERR-MED-SKALL-VISAS                 VALUE 'J'.                   
018500                                                                          
018600     EJECT                                                                
018700*      --- VALID IDDC CODES                                               
018800*                                                                         
018900*01    -COPY WWDCKONS                                                     
019000       EJECT                                                              
019100*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
019200 01  GENERELLA-SUBPROGRAM.                                                
019300     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
019400     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
019500     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
019600     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
019700     EJECT                                                                
019800*   -COPY WMEDAREAC                                                       
019900     EJECT                                                                
020000 01  MESSAGE-CODES.                                                       
020100     03  ERR-HIGHLITED-FIELDS-WRONG   PIC X(3)    VALUE '001'.            
020200     03  ERR-ONLY-ONE-ALLOWED         PIC X(3)    VALUE '004'.            
020300     03  ERR-KEYS-ARE-MISSING         PIC X(3)    VALUE '005'.            
020400     03  ERR-UPDATE-FORBIDDEN         PIC X(3)    VALUE '007'.            
020500     03  ERR-KAMPANJ-STARTAD          PIC X(3)    VALUE '007'.            
020600     03  ERR-MISSING-REGISTER         PIC X(3)    VALUE '010'.            
020700     03  ERR-PF11-AND-NO-DATA         PIC X(3)    VALUE '011'.            
020800     03  ERR-EJ-NUMERISK              PIC X(3)    VALUE '020'.            
020900     03  ERR-NO-UPDATE                PIC X(3)    VALUE '034'.            
021000     03  ERR-QUANT-ERROR              PIC X(3)    VALUE '181'.            
021100     03  ERR-LINE-EXIST               PIC X(3)    VALUE '245'.            
021200     03  ERR-WRONG-KEY                PIC X(3)    VALUE '401'.            
021300     03  ERR-QUANT-TO-BIG             PIC X(3)    VALUE '725'.            
021400     03  ERR-INTERVALL-WRONG          PIC X(3)    VALUE '738'.            
021500     03  INF-PRESS-PF11               PIC X(3)    VALUE '003'.            
021600     03  INF-FIRST-PAGE               PIC X(3)    VALUE '006'.            
021700     03  INF-FINNS-EJ-PA-REGISTER     PIC X(3)    VALUE '010'.            
021800     03  INF-ARTIKEL-SAKNAS           PIC X(3)    VALUE '017'.            
021900     03  INF-KAMPANJ-SAKNAS           PIC X(3)    VALUE '100'.            
022000     03  INF-UPDATE-DONE              PIC X(3)    VALUE '101'.            
022100     03  INF-MORE-INFO-EXISTS         PIC X(3)    VALUE '105'.            
022200     03  INF-LAST-PAGE                PIC X(3)    VALUE '106'.            
022300     EJECT                                                                
022400*                        ****   PARAMETRAR TILL W005INIT                  
022500*01      -COPY WMSGINIT                                                   
022600     EJECT                                                                
022700*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
022800*                                                                         
022900 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
023000     SKIP3                                                                
023100*01  MID -COPY W2I31201                                                   
023200     EJECT                                                                
023300 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
023400     SKIP3                                                                
023500*01  -COPY WMSGAREA                                                       
023600     EJECT                                                                
023700     03  MOD REDEFINES MSG-AREA.                                          
023800*      05  -COPY W2O31201                                                 
023900     EJECT                                                                
024000 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
024100     SKIP3                                                                
024200*01  -COPY WMFSAREA                                                       
024300     EJECT                                                                
024400*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
024500*                                                                         
024600 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
024700     SKIP3                                                                
024800 01 NYCKLAR-TILL-DLI.                                                     
024900    03 W-WDM201-X.                                                        
025000       05 W-KAMP-IDKAMPRF         PIC S9(7) VALUE ZERO COMP-3.            
025100       05 W-KAMP-IDDC             PIC  X(2) VALUE SPACE.                  
025200                                                                          
025300    03 W-WDM201-COPY-X.                                                   
025400       05 W-COPY-IDKAMPRF         PIC S9(7) VALUE ZERO COMP-3.            
025500       05 W-COPY-IDDC             PIC  X(2) VALUE SPACE.                  
025600                                                                          
025700    03 W-WDM211-X.                                                        
025800       05 W-KART-IDARTNR          PIC S9(09) VALUE ZERO COMP-3.           
025900                                                                          
026000    03 W-WDM211-COPY-X.                                                   
026100       05 W-COPY-IDARTNR          PIC S9(09) VALUE ZERO COMP-3.           
026200                                                                          
026300    03 W-WDM221-X.                                                        
026400       05 W-KMRK-IDDISTR-FOM      PIC S9(05) VALUE ZERO COMP-3.           
026500       05 W-KMRK-IDDISTR-TOM      PIC S9(05) VALUE ZERO COMP-3.           
026600       05 W-KMRK-IDKUNDNR-FOM     PIC S9(07) VALUE ZERO COMP-3.           
026700       05 W-KMRK-IDKUNDNR-TOM     PIC S9(07) VALUE ZERO COMP-3.           
026800                                                                          
026810    03 W-WDM221-MAX-X.                                                    
026820       05 W-KMRK-IDDISTR-FOM-MAX  PIC S9(05) VALUE ZERO COMP-3.           
026830       05 W-KMRK-IDDISTR-TOM-MAX  PIC S9(05) VALUE ZERO COMP-3.           
026840       05 W-KMRK-IDKUNDNR-FOM-MAX PIC S9(07) VALUE ZERO COMP-3.           
026850       05 W-KMRK-IDKUNDNR-TOM-MAX PIC S9(07) VALUE ZERO COMP-3.           
026860                                                                          
026900    03 W-WDM221-COPY-X.                                                   
027000       05 W-COPY-IDDISTR-FOM   PIC S9(05) VALUE ZERO COMP-3.              
027100       05 W-COPY-IDDISTR-TOM   PIC S9(05) VALUE ZERO COMP-3.              
027200       05 W-COPY-IDKUNDNR-FOM  PIC S9(07) VALUE ZERO COMP-3.              
027300       05 W-COPY-IDKUNDNR-TOM  PIC S9(07) VALUE ZERO COMP-3.              
027400                                                                          
029800*    --- STATUS-KOD FRÅN IMS                                              
029900 01  STATUS-WS                   PIC XX.                                  
030000     88  SEGMENT-FINNS                       VALUE '  '.                  
030100     88  SEGMENT-ALREADY-EXISTS              VALUE 'II'.                  
030200     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
030300     88  END-OF-DATA                         VALUE 'GB'.                  
030400     88  PARENT-MISSING                      VALUE 'GP'.                  
030500     SKIP2                                                                
030600 01  GODK-STATUSKODER.                                                    
030700     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
030800     SKIP3                                                                
030900 01  SSA1                        PIC X(200).                              
031000 01  SSA2                        PIC X(128).                              
031100 01  SSA3                        PIC X(128).                              
031200     EJECT                                                                
031300*    --- IMS FUNKTIONSKODER                                               
031400*01  -COPY W0003                                                          
031500     EJECT                                                                
031600*    ---  DLI INPUT-OUTPUT AREA                                           
031700 01  FILLER                    PIC X(16)   VALUE 'DLI-IO-WDM201'.         
031800 01  DLI-IO-WDM201.                                                       
031900*    03 -COPY WDM201                                                      
032000     EJECT                                                                
032100 01  FILLER                    PIC X(16)   VALUE 'DLI-IO-WDM211'.         
032200 01  DLI-IO-WDM211.                                                       
032300*    03 -COPY WDM211                                                      
032400     EJECT                                                                
032500 01  FILLER                    PIC X(16)   VALUE 'DLI-IO-WDM221'.         
032600 01  DLI-IO-WDM221.                                                       
032700*    03 -COPY WDM221                                                      
032800     EJECT                                                                
032900                                                                          
033000 01  FILLER                 PIC X(16)   VALUE 'DLI-IO-WDM201-CP'.         
033100 01  DLI-IO-WDM201-COPY.                                                  
033200*    03 -COPY WDM201 -PRE COPY-                                           
033300                                                                          
033310 01  FILLER                 PIC X(16)   VALUE 'DLI-IO-WDM211-CP'.         
033320 01  DLI-IO-WDM211-COPY.                                                  
033330*    03 -COPY WDM211 -PRE COPY-                                           
033340                                                                          
033400 01  FILLER                 PIC X(16)   VALUE 'DLI-IO-WDM221-CP'.         
033500 01  DLI-IO-WDM221-COPY.                                                  
033600*    03 -COPY WDM221 -PRE COPY-                                           
033700     EJECT                                                                
033800                                                                          
033900 LINKAGE SECTION.                                                         
034000*01  -COPY W0009      -PRE MSG-                                           
034100     EJECT                                                                
034200*01  -COPY W0008      -PRE USEA-                                          
034300     05  FILLER                  PIC X.                                   
034400     EJECT                                                                
034500*01  -COPY W0008      -PRE WDM2-                                          
034600     05  FILLER                  PIC X.                                   
034700     EJECT                                                                
035100 PROCEDURE DIVISION  USING MSG-PCB USEA-PCB                               
035200                                   WDM2-PCB.                              
035300     ENTRY 'DLITCBL' USING MSG-PCB USEA-PCB                               
035400                                   WDM2-PCB.                              
035500                                                                          
035600     PERFORM IMS-GET-MSG                                                  
035700     IF SEGMENT-FINNS                                                     
035800       PERFORM A-INIT                                                     
035900       PERFORM B-NYCKEL-KONTROLL                                          
036000       IF NYCKLAR-OK                                                      
036100         PERFORM S01-COUNT-KVBEART-KAMP                                   
036200         IF NYA-NYCKLAR                                                   
036300           CONTINUE                                                       
036400         ELSE                                                             
036500           IF MFS-UPDATE                                                  
036600             PERFORM D-CONTROL-INSERT                                     
036700             IF INDATA-OK                                                 
036800               PERFORM D-CONTROL-UPDATE                                   
036900             END-IF                                                       
037000             IF INDATA-OK                                                 
037100               PERFORM D-CONTROL-COPY                                     
037200             END-IF                                                       
037300             IF INDATA-OK                                                 
037400               IF YES-TO-NEW-INSERT                                       
037500                 PERFORM E-NEW-INSERT                                     
037600               END-IF                                                     
037700               IF YES-TO-UPDATE-DELETE                                    
037800                 PERFORM E-UPPDATERA                                      
037900               END-IF                                                     
038000               IF YES-TO-COPY                                             
038100                 PERFORM E-COPY-CAMP-DC-ART                               
038200               END-IF                                                     
038300             END-IF                                                       
038400           ELSE                                                           
038500             PERFORM F-MFS-IDPFK-KONTROLL                                 
038600           END-IF                                                         
038700         END-IF                                                           
038800         IF ALLT-OK                                                       
038900           PERFORM G-LAES-VISA-INFO                                       
039000         END-IF                                                           
039100       END-IF                                                             
039200                                                                          
040300       PERFORM S04-ADD-TO-MID                                             
040400       COMPUTE MSG-KVLL = LENGTH OF MOD-W2O31201-CTX + 4                  
040500       PERFORM IMS-INSERT-MSG                                             
040600     END-IF                                                               
040700                                                                          
040800     MOVE ZERO TO RETURN-CODE                                             
040900     GOBACK                                                               
041000     .                                                                    
041100     EJECT                                                                
041200                                                                          
041300 A-INIT SECTION.                                                          
041400     MOVE 'A-INIT                       ' TO CURRENT-SECTION              
041500                                                                          
041600     IF MSG-DUBBLA-TRANSKODER                                             
041700       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W2I31201-CTX             
041800       MOVE MSG-IDTRANS-2                 TO MFS-IDTRANS                  
041900       MOVE MSG-KDMFSFOR-2                TO MFS-KDMFSFOR                 
042000     ELSE                                                                 
042100       MOVE MSG-INDATA-MINUS-1-TRANSKOD   TO MID-W2I31201-CTX             
042200       MOVE MSG-IDTRANS-1                 TO MFS-IDTRANS                  
042300       MOVE MSG-KDMFSFOR-1                TO MFS-KDMFSFOR                 
042400     END-IF                                                               
042500                                                                          
042600     MOVE MSG-KDTRTYP                     TO MFS-KDTRTYP                  
042700     MOVE MSG-IDPFK                       TO MFS-IDPFK                    
042800     MOVE MFS-IDTRANS                     TO W-IDTRANS                    
042900                                                                          
043000     MOVE LOW-VALUE                       TO MSG-AREA                     
043100                                                                          
043200     MOVE 'W2O312N1'                      TO MFS-IDMOD                    
043300     MOVE '2312'                          TO MOD-IDTRANS                  
043400     MOVE MFS-RENSA-FAELT                 TO MOD-TEMFSFEL                 
043500                                             MOD-TEMFSINF                 
043600                                                                          
043700     ACCEPT TODAYS-DATE                 FROM DATE                         
043800                                                                          
043900     IF NOT EGEN-MID                                                      
044000       MOVE SPACE                         TO MFS-KDTRTYP                  
044100       MOVE '7'                           TO MFS-IDPFK                    
044200     END-IF                                                               
044700     .                                                                    
044800     EJECT                                                                
044900                                                                          
045000 B-NYCKEL-KONTROLL SECTION.                                               
045100     MOVE 'B-NYCKEL-KONTROLL            ' TO CURRENT-SECTION              
045200                                                                          
045300     MOVE JA                    TO NYCKLAR-SW ALLT-SW                     
045400                                   INDATA-SW                              
045500     MOVE NEJ                   TO UPDATE-SW                              
045600                                   NYA-NYCKLAR-SW                         
045700                                                                          
046500     MOVE ALL '+'               TO MSGI-WMSGINIT                          
046600     MOVE '001'                 TO MSGI-KDCALL                            
046700     MOVE MSG-SIGNON-USERID     TO MSGI-IDUSER                            
046800     MOVE MSG-LTERM-NAME        TO MSGI-IDLTERM-USER                      
046900     MOVE '2312'                TO MSGI-IDTRANS                           
047000     IF EGEN-MID OR 2317-MID                                              
047200        MOVE MID-IDKAMPRF-IN    TO MSGI-IDKAMPRF                          
047300        MOVE MID-IDDC-IN        TO MSGI-IDDC-KEY                          
047301        MOVE MID-IDARTNR-IN     TO MSGI-IDARTNR                           
047600     END-IF                                                               
047610                                                                          
047700     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
047800                                                                          
048000     IF MSGI-SPAR-AREA(1:4) = '2312'                                      
048100       MOVE MSGI-SPAR-AREA      TO SAVE-AREA                              
048210     END-IF                                                               
048211                                                                          
048220*    - LANGUAGE TO BE USED BY MEDKONV                                     
048230     MOVE MSGI-IDLAND-SPR       TO MED-IDSKYLT                            
048300                                                                          
048310*    -- KONTROLL IDKAMPRF                                                 
048320     MOVE MFS-RENSA-FAELT       TO MOD-IDKAMPRF-IN                        
048400     IF MID-IDKAMPRF-IN NOT = ALL '+'                                     
048900       MOVE '7'                 TO MFS-IDPFK                              
049000       MOVE SPACE               TO MFS-KDTRTYP                            
049100     END-IF                                                               
049200     INSPECT MSGI-IDKAMPRF REPLACING LEADING SPACE BY ZERO                
049300     IF MSGI-IDKAMPRF NUMERIC                                             
049400        MOVE MSGI-IDKAMPRF      TO WS-KAMPRF                              
049500        MOVE WS-KAMPRF          TO W-KAMP-IDKAMPRF                        
049600     ELSE                                                                 
049700       MOVE NEJ                 TO NYCKLAR-SW                             
049800     END-IF                                                               
050500                                                                          
050510*    -- KONTROLL AV IDDC                                                  
050520     MOVE MFS-RENSA-FAELT       TO MOD-IDDC-IN                            
050600     IF MID-IDDC-IN NOT = ALL '+'                                         
050654       MOVE '7'                 TO MFS-IDPFK                              
050655       MOVE SPACE               TO MFS-KDTRTYP                            
050660     END-IF                                                               
050661     MOVE MSGI-IDDC-KEY         TO WS-IDDC                                
050662     MOVE MSGI-IDDC-KEY         TO W-KAMP-IDDC                            
050664                                                                          
050665*    -- KONTROLL AV IDARTNR                                               
050666     MOVE MFS-RENSA-FAELT       TO MOD-IDARTNR-IN                         
052310     IF MID-IDARTNR-IN NOT = ALL '+'                                      
052393       MOVE '7'                 TO MFS-IDPFK                              
052394       MOVE SPACE               TO MFS-KDTRTYP                            
052396     END-IF                                                               
052397                                                                          
052398     INSPECT MSGI-IDARTNR REPLACING LEADING SPACE BY ZERO                 
052399     IF MSGI-IDARTNR NUMERIC                                              
052401        MOVE MSGI-IDARTNR       TO WS-IDARTNR                             
052402        MOVE WS-IDARTNR         TO W-KART-IDARTNR                         
052404     ELSE                                                                 
052405       MOVE NEJ                 TO NYCKLAR-SW                             
052406     END-IF                                                               
052900                                                                          
053800                                                                          
053900     IF GODK-MID OR NYCKLAR-OK                                            
054010       MOVE MSGI-IDKAMPRF       TO MOD-IDKAMPRF-UT                        
054020                                   SAVE-IDKAMPRF-KEY                      
054100       INSPECT MOD-IDKAMPRF-UT REPLACING LEADING ZERO BY SPACE            
054302                                                                          
054310       MOVE MSGI-IDDC-KEY       TO MOD-IDDC-UT                            
054311                                   SAVE-IDDC-KEY                          
054312                                                                          
054320       MOVE MSGI-IDARTNR        TO MOD-IDARTNR-UT                         
054330       INSPECT MOD-IDARTNR-UT REPLACING LEADING ZERO BY SPACE             
054350       MOVE MSGI-IDARTNR        TO SAVE-IDARTNR-KEY                       
054400     END-IF                                                               
054500                                                                          
054600     IF NYCKLAR-FEL                                                       
054700       MOVE ERR-WRONG-KEY       TO MED-IDMFSFEL                           
054800       PERFORM S01-ERR-RUTINE                                             
054810       PERFORM MFS-RENSA-FAELT-IN                                         
054820       PERFORM MFS-RENSA-FAELT-UT                                         
054900     END-IF                                                               
055000     .                                                                    
055100     EJECT                                                                
055200                                                                          
055300 D-CONTROL-INSERT  SECTION.                                               
055400     MOVE 'D-CONTROL-INSERT             ' TO CURRENT-SECTION              
055500                                                                          
055600     IF ART-REG-PA-KAMPANJ                                                
055700       PERFORM DA-INFAELT-KONTROLL                                        
055800       IF INDATA-EXISTS                                                   
055900         IF YES-TO-NEW-INSERT                                             
056000           PERFORM DB-GENERELL-KONTROLL                                   
056100           IF INDATA-FEL                                                  
056200             MOVE NEJ TO ALLT-SW                                          
056300             IF ERR-MED-SKALL-VISAS                                       
056400               PERFORM S01-ERR-RUTINE                                     
056500             END-IF                                                       
056600             PERFORM MFS-ROER-EJ-FAELT-IN                                 
056700             PERFORM MFS-ROER-EJ-FAELT-UT                                 
056800             PERFORM MFS-LAS-IN-IGEN                                      
056900           ELSE                                                           
057000             MOVE JA        TO UPDATE-SW                                  
057100           END-IF                                                         
057200         END-IF                                                           
057300       END-IF                                                             
057400     ELSE                                                                 
057500       MOVE NEJ         TO INDATA-SW ALLT-SW                              
057600       MOVE ERR-UPDATE-FORBIDDEN TO MED-IDMFSFEL                          
057700       PERFORM S01-ERR-RUTINE                                             
057800       MOVE INF-ARTIKEL-SAKNAS   TO  MED-IDMFSINF                         
057900       PERFORM S02-INF-RUTINE                                             
058000       PERFORM MFS-ROER-EJ-FAELT-IN                                       
058100       PERFORM MFS-ROER-EJ-FAELT-UT                                       
058200       PERFORM MFS-LAS-IN-IGEN                                            
058300     END-IF                                                               
058400     .                                                                    
058500     EJECT                                                                
058600                                                                          
058700 D-CONTROL-UPDATE  SECTION.                                               
058800     MOVE 'D-CONTROL-UPDATE             ' TO CURRENT-SECTION              
058900                                                                          
059000     IF ART-REG-PA-KAMPANJ                                                
059100       MOVE +1           TO WS-IX                                         
059200       MOVE ZERO         TO WS-KVKVARFD-1                                 
059300       COMPUTE WS-KVKVARFD-1 =                                            
059400              (ACC-KART-KVBEART-KAMP -                                    
059500               ACC-KMRK-KVBEART-KAMP)                                     
059600       PERFORM UNTIL WS-IX > MAX-INDX OR INDATA-FEL                       
059700*          IF MID-KVBEART-KAMP(WS-IX) > ZERO                              
059800             MOVE MID-KVBEART-KAMP(WS-IX) TO WS-KVBEART-KAMP-1            
059900*          ELSE                                                           
060000*            MOVE ZERO TO WS-KVBEART-KAMP-1                               
060100*          END-IF                                                         
060200           MOVE MID-KVBEART-REM(WS-IX)  TO WS-KVBEART-REM-1               
060300           MOVE MID-KVBEART-KUND(WS-IX) TO WS-KVBEART-KUND-1              
060400           IF MID-CMD-UPD(WS-IX) = 'U'                                    
060500             MOVE MID-IDDISTR-FOM(WS-IX) TO WS-IDDISTR-FROM               
060600*            IF MID-IDDISTR-FOM(WS-IX) > ZERO                             
060700             IF WS-IDDISTR-FROM       > ZERO                              
060800               PERFORM DA-COUNT-KVBEART                                   
060900               IF WS-KVKVARFD-1 < ZERO                                    
061000                 MOVE MFS-NUM-FAELT-FEL TO                                
061100                                      MOD-KVBEART-KAMP-ATTR(WS-IX)        
061200                 MOVE ERR-QUANT-TO-BIG TO MED-IDMFSFEL                    
061300                 PERFORM S01-ERR-RUTINE                                   
061400                 MOVE JA              TO VISA-ERR-MED-SW                  
061500                 MOVE NEJ             TO INDATA-SW ALLT-SW                
061600                 PERFORM MFS-ROER-EJ-FAELT-IN                             
061700                 PERFORM MFS-ROER-EJ-FAELT-UT                             
061800                 PERFORM MFS-LAS-IN-IGEN                                  
061900               ELSE                                                       
062000                 IF WS-KVBEART-KAMP-1 < WS-KVBEART-KUND-1                 
062100                   MOVE ERR-QUANT-ERROR TO MED-IDMFSFEL                   
062200                   PERFORM S01-ERR-RUTINE                                 
062300                   MOVE JA              TO VISA-ERR-MED-SW                
062400                   MOVE NEJ             TO INDATA-SW ALLT-SW              
062500                   PERFORM MFS-ROER-EJ-FAELT-IN                           
062600                   PERFORM MFS-ROER-EJ-FAELT-UT                           
062700                   PERFORM MFS-LAS-IN-IGEN                                
062800                 ELSE                                                     
062900                   MOVE JA    TO UPDATE-SW                                
063000                 END-IF                                                   
063100               END-IF                                                     
063200             ELSE                                                         
063300               MOVE MFS-NUM-FAELT-FEL TO                                  
063400                                MOD-KVBEART-KAMP-ATTR(WS-IX)              
063500               MOVE ERR-PF11-AND-NO-DATA TO MED-IDMFSFEL                  
063600               PERFORM S01-ERR-RUTINE                                     
063700               MOVE JA              TO VISA-ERR-MED-SW                    
063800               MOVE NEJ             TO INDATA-SW ALLT-SW                  
063900               PERFORM MFS-ROER-EJ-FAELT-IN                               
064000               PERFORM MFS-ROER-EJ-FAELT-UT                               
064100               PERFORM MFS-LAS-IN-IGEN                                    
064200             END-IF                                                       
064300             MOVE JA TO UPDATE-DELETE-SW                                  
064400           END-IF                                                         
064500           IF MID-CMD-UPD(WS-IX) = 'D'                                    
064600             MOVE MID-IDDISTR-FOM(WS-IX) TO WS-IDDISTR-FROM               
064700*            IF MID-IDDISTR-FOM(WS-IX) > ZERO                             
064800             IF WS-IDDISTR-FROM       > ZERO                              
064900               IF WS-KVBEART-KUND-1 > ZERO                                
065000                 MOVE MFS-NUM-FAELT-FEL TO                                
065100                                      MOD-KVBEART-KAMP-ATTR(WS-IX)        
065200                 MOVE ERR-KAMPANJ-STARTAD TO MED-IDMFSFEL                 
065300                 PERFORM S01-ERR-RUTINE                                   
065400                 MOVE JA              TO VISA-ERR-MED-SW                  
065500                 MOVE NEJ             TO INDATA-SW ALLT-SW                
065600                 PERFORM MFS-ROER-EJ-FAELT-IN                             
065700                 PERFORM MFS-ROER-EJ-FAELT-UT                             
065800                 PERFORM MFS-LAS-IN-IGEN                                  
065900               ELSE                                                       
066000*                IF MID-IDDISTR-FOM(WS-IX) > ZERO                         
066100                 IF WS-IDDISTR-FROM       > ZERO                          
066200                   MOVE JA    TO UPDATE-SW                                
066300                 ELSE                                                     
066400                   MOVE MFS-NUM-FAELT-FEL TO                              
066500                                    MOD-KVBEART-KAMP-ATTR(WS-IX)          
066600                   MOVE ERR-PF11-AND-NO-DATA TO MED-IDMFSFEL              
066700                   PERFORM S01-ERR-RUTINE                                 
066800                   MOVE JA              TO VISA-ERR-MED-SW                
066900                   MOVE NEJ             TO INDATA-SW ALLT-SW              
067000                   PERFORM MFS-ROER-EJ-FAELT-IN                           
067100                   PERFORM MFS-ROER-EJ-FAELT-UT                           
067200                   PERFORM MFS-LAS-IN-IGEN                                
067300                 END-IF                                                   
067400               END-IF                                                     
067500             ELSE                                                         
067600               MOVE MFS-NUM-FAELT-FEL TO                                  
067700                                MOD-KVBEART-KAMP-ATTR(WS-IX)              
067800*              MOVE ERR-PF11-AND-NO-DATA TO MED-IDMFSFEL                  
067900                   MOVE ERR-KAMPANJ-STARTAD TO MED-IDMFSFEL               
068000               PERFORM S01-ERR-RUTINE                                     
068100               MOVE JA              TO VISA-ERR-MED-SW                    
068200               MOVE NEJ             TO INDATA-SW ALLT-SW                  
068300               PERFORM MFS-ROER-EJ-FAELT-IN                               
068400               PERFORM MFS-ROER-EJ-FAELT-UT                               
068500               PERFORM MFS-LAS-IN-IGEN                                    
068600             END-IF                                                       
068700             MOVE JA TO UPDATE-DELETE-SW                                  
068800           END-IF                                                         
068900           ADD +1 TO WS-IX                                                
069000       END-PERFORM                                                        
069100     ELSE                                                                 
069200       MOVE NEJ                  TO INDATA-SW ALLT-SW                     
069300       MOVE ERR-UPDATE-FORBIDDEN TO MED-IDMFSFEL                          
069400       PERFORM S01-ERR-RUTINE                                             
069500       MOVE INF-ARTIKEL-SAKNAS   TO  MED-IDMFSINF                         
069600       PERFORM S02-INF-RUTINE                                             
069700       PERFORM MFS-ROER-EJ-FAELT-IN                                       
069800       PERFORM MFS-ROER-EJ-FAELT-UT                                       
069900       PERFORM MFS-LAS-IN-IGEN                                            
070000     END-IF                                                               
070100     .                                                                    
070200     EJECT                                                                
070300                                                                          
070400 D-CONTROL-COPY    SECTION.                                               
070500     MOVE 'D-CONTROL-COPY               ' TO CURRENT-SECTION              
070600                                                                          
070700     IF ART-REG-PA-KAMPANJ                                                
070800       IF YES-TO-COPY AND YES-TO-NEW-INSERT                               
070900         MOVE NEJ                  TO INDATA-SW ALLT-SW                   
071000         MOVE ERR-ONLY-ONE-ALLOWED TO MED-IDMFSFEL                        
071100         PERFORM S01-ERR-RUTINE                                           
071200         MOVE JA                   TO VISA-ERR-MED-SW                     
071300         MOVE NEJ                  TO INDATA-SW ALLT-SW                   
071400         PERFORM MFS-ROER-EJ-FAELT-IN                                     
071500         PERFORM MFS-ROER-EJ-FAELT-UT                                     
071600         PERFORM MFS-LAS-IN-IGEN                                          
071700       END-IF                                                             
071800       IF YES-TO-COPY AND YES-TO-UPDATE-DELETE                            
071900         MOVE NEJ                  TO INDATA-SW ALLT-SW                   
072000         MOVE ERR-ONLY-ONE-ALLOWED TO MED-IDMFSFEL                        
072100         PERFORM S01-ERR-RUTINE                                           
072200         MOVE JA                   TO VISA-ERR-MED-SW                     
072300         MOVE NEJ                  TO INDATA-SW ALLT-SW                   
072400         PERFORM MFS-ROER-EJ-FAELT-IN                                     
072500         PERFORM MFS-ROER-EJ-FAELT-UT                                     
072600         PERFORM MFS-LAS-IN-IGEN                                          
072700       END-IF                                                             
072800       IF YES-TO-NEW-INSERT AND YES-TO-UPDATE-DELETE                      
072900         MOVE NEJ                  TO INDATA-SW ALLT-SW                   
073000         MOVE ERR-ONLY-ONE-ALLOWED TO MED-IDMFSFEL                        
073100         PERFORM S01-ERR-RUTINE                                           
073200         MOVE JA                   TO VISA-ERR-MED-SW                     
073300         MOVE NEJ                  TO INDATA-SW ALLT-SW                   
073400         PERFORM MFS-ROER-EJ-FAELT-IN                                     
073500         PERFORM MFS-ROER-EJ-FAELT-UT                                     
073600         PERFORM MFS-LAS-IN-IGEN                                          
073700       END-IF                                                             
073800                                                                          
073900       IF ALLT-OK AND YES-TO-COPY                                         
074000         MOVE MID-IDKAMPRF-COPY     TO WS-KAMPRF-COPY                     
074100         INSPECT WS-KAMPRF-COPY REPLACING LEADING SPACE BY ZERO           
074200         IF WS-KAMPRF-COPY NUMERIC AND WS-KAMPRF-COPY > ZERO              
074300           MOVE WS-KAMPRF-COPY      TO W-COPY-IDKAMPRF                    
074400         ELSE                                                             
074500           MOVE MFS-NUM-FAELT-FEL   TO                                    
074600                                       MOD-IDKAMPRF-COPY-ATTR             
074700           MOVE ERR-EJ-NUMERISK     TO MED-IDMFSFEL                       
074800           PERFORM S01-ERR-RUTINE                                         
074900           MOVE JA                  TO VISA-ERR-MED-SW                    
075000           MOVE NEJ                 TO INDATA-SW ALLT-SW                  
075100           PERFORM MFS-ROER-EJ-FAELT-IN                                   
075200           PERFORM MFS-ROER-EJ-FAELT-UT                                   
075300           PERFORM MFS-LAS-IN-IGEN                                        
075400         END-IF                                                           
075500                                                                          
075600         MOVE MID-IDDC-COPY         TO WS-IDDC-COPY                       
075700         IF WS-IDDC-COPY NOT = SPACE                                      
075800           MOVE WS-IDDC-COPY        TO W-COPY-IDDC                        
075900         ELSE                                                             
076000           MOVE MFS-ALFA-FAELT-FEL  TO                                    
076100                                 MOD-IDDC-COPY-ATTR                       
076200           MOVE ERR-HIGHLITED-FIELDS-WRONG                                
076300                                    TO MED-IDMFSFEL                       
076400           PERFORM S01-ERR-RUTINE                                         
076500           MOVE JA                  TO VISA-ERR-MED-SW                    
076600           MOVE NEJ                 TO INDATA-SW ALLT-SW                  
076700           PERFORM MFS-ROER-EJ-FAELT-IN                                   
076800           PERFORM MFS-ROER-EJ-FAELT-UT                                   
076900           PERFORM MFS-LAS-IN-IGEN                                        
077000         END-IF                                                           
077100                                                                          
077200         MOVE MID-IDARTNR-COPY      TO WS-IDARTNR-COPY                    
077300         INSPECT WS-IDARTNR-COPY REPLACING LEADING SPACE BY ZERO          
077400         IF WS-IDARTNR-COPY NUMERIC AND WS-IDARTNR-COPY > ZERO            
077500           MOVE WS-IDARTNR-COPY     TO W-COPY-IDARTNR                     
077600                                                                          
077700           MOVE JA        TO UPDATE-SW                                    
077800         ELSE                                                             
077900           MOVE MFS-NUM-FAELT-FEL   TO                                    
078000                                 MOD-IDARTNR-COPY-ATTR                    
078100           MOVE ERR-EJ-NUMERISK     TO MED-IDMFSFEL                       
078200           PERFORM S01-ERR-RUTINE                                         
078300           MOVE JA                  TO VISA-ERR-MED-SW                    
078400           MOVE NEJ                 TO INDATA-SW ALLT-SW                  
078500           PERFORM MFS-ROER-EJ-FAELT-IN                                   
078600           PERFORM MFS-ROER-EJ-FAELT-UT                                   
078700           PERFORM MFS-LAS-IN-IGEN                                        
078800         END-IF                                                           
078900       END-IF                                                             
079000     ELSE                                                                 
079100       MOVE NEJ         TO INDATA-SW ALLT-SW                              
079200       MOVE ERR-UPDATE-FORBIDDEN TO MED-IDMFSFEL                          
079300       PERFORM S01-ERR-RUTINE                                             
079400       MOVE INF-ARTIKEL-SAKNAS   TO  MED-IDMFSINF                         
079500       PERFORM S02-INF-RUTINE                                             
079600       PERFORM MFS-ROER-EJ-FAELT-IN                                       
079700       PERFORM MFS-ROER-EJ-FAELT-UT                                       
079800       PERFORM MFS-LAS-IN-IGEN                                            
079900     END-IF                                                               
080000     .                                                                    
080100     EJECT                                                                
080200                                                                          
080300 DA-COUNT-KVBEART    SECTION.                                             
080400     MOVE 'DA-COUNT-KVBEART             ' TO CURRENT-SECTION              
080500                                                                          
080510     MOVE WS-KAMPRF               TO W-KAMP-IDKAMPRF                      
080520     MOVE WS-IDDC                 TO W-KAMP-IDDC                          
080600     MOVE WS-IDARTNR              TO W-KART-IDARTNR                       
080800     MOVE MID-IDDISTR-FOM (WS-IX) TO W-KMRK-IDDISTR-FOM                   
080900     MOVE MID-IDDISTR-TOM (WS-IX) TO W-KMRK-IDDISTR-TOM                   
081000     MOVE MID-IDKUNDNR-FOM(WS-IX) TO W-KMRK-IDKUNDNR-FOM                  
081100     MOVE MID-IDKUNDNR-TOM(WS-IX) TO W-KMRK-IDKUNDNR-TOM                  
081300     PERFORM IMS-GHU-WDM221                                               
081400     IF SEGMENT-FINNS                                                     
081500       MOVE KMRK-KVBEART-KAMP TO WS-KVBEART-KAMP-2                        
081600       IF WS-KVBEART-KAMP-1 > WS-KVBEART-KAMP-2                           
081700         COMPUTE WS-KVKVARFD-1 = WS-KVKVARFD-1 -                          
081800               (WS-KVBEART-KAMP-1 - WS-KVBEART-KAMP-2)                    
081900       ELSE                                                               
082000         COMPUTE WS-KVKVARFD-1 = WS-KVKVARFD-1 +                          
082100               (WS-KVBEART-KAMP-2 - WS-KVBEART-KAMP-1)                    
082200       END-IF                                                             
082300     END-IF                                                               
082400     .                                                                    
082500     EJECT                                                                
082600                                                                          
082700 DA-INFAELT-KONTROLL SECTION.                                             
082800     MOVE 'DA-INFAELT-KONTROLL          ' TO CURRENT-SECTION              
082900                                                                          
083000     IF MID-IDDISTR-FOM-NEW   = ALL '+' AND                               
083100        MID-IDDISTR-TOM-NEW   = ALL '+' AND                               
083200        MID-IDKUNDNR-FOM-NEW  = ALL '+' AND                               
083300        MID-IDKUNDNR-TOM-NEW  = ALL '+' AND                               
083400        MID-KVBEART-KAMP-NEW  = ALL '+' AND                               
083500        MID-IDKAMPRF-COPY     = ALL '+' AND                               
083600        MID-IDDC-COPY         = ALL '+' AND                               
083700        MID-IDARTNR-COPY      = ALL '+'                                   
083800       MOVE NEJ        TO INDATA-EXISTS-SW                                
083900     ELSE                                                                 
084200       MOVE JA       TO INDATA-EXISTS-SW                                  
084300       IF MID-IDKAMPRF-COPY = ALL '+' AND                                 
084400          MID-IDDC-COPY     = ALL '+' AND                                 
084500          MID-IDARTNR-COPY  = ALL '+'                                     
084600         MOVE JA       TO NEW-INSERT-SW                                   
084700         MOVE NEJ      TO COPY-CAMP-ART-SW                                
084800       ELSE                                                               
084900         MOVE JA       TO COPY-CAMP-ART-SW                                
085000         IF MID-IDDISTR-FOM-NEW  = ALL '+' AND                            
085100            MID-IDDISTR-TOM-NEW  = ALL '+' AND                            
085200            MID-IDKUNDNR-FOM-NEW = ALL '+' AND                            
085300            MID-IDKUNDNR-TOM-NEW = ALL '+' AND                            
085400            MID-KVBEART-KAMP-NEW = ALL '+'                                
085500           MOVE NEJ      TO NEW-INSERT-SW                                 
085600         ELSE                                                             
085700           MOVE JA       TO NEW-INSERT-SW                                 
085800           MOVE ERR-ONLY-ONE-ALLOWED TO MED-IDMFSFEL                      
085900           PERFORM S01-ERR-RUTINE                                         
086000           MOVE JA                 TO VISA-ERR-MED-SW                     
086100           MOVE NEJ                TO INDATA-SW ALLT-SW                   
086200           PERFORM MFS-ROER-EJ-FAELT-IN                                   
086300           PERFORM MFS-ROER-EJ-FAELT-UT                                   
086400           PERFORM MFS-LAS-IN-IGEN                                        
086500         END-IF                                                           
086600       END-IF                                                             
087600     END-IF                                                               
087700     .                                                                    
087800     EJECT                                                                
087900                                                                          
088000 DB-GENERELL-KONTROLL SECTION.                                            
088100     MOVE 'DB-GENERELL-KONTROLL         ' TO CURRENT-SECTION              
088200                                                                          
088300     PERFORM DBA-KONTROLL-IDDISTR                                         
088400                                                                          
088500     IF INDATA-OK                                                         
088600       PERFORM DBB-KONTROLL-IDKUNDNR                                      
088700       IF INDATA-OK                                                       
089000         PERFORM DBBA-KONTROLL-INTERVALL                                  
089100         IF INDATA-OK                                                     
089200           PERFORM DBC-KONTROLL-KVBEART                                   
089300         END-IF                                                           
090000       END-IF                                                             
090100     END-IF                                                               
090200     .                                                                    
090300     EJECT                                                                
090400                                                                          
090500 DBA-KONTROLL-IDDISTR SECTION.                                            
090600     MOVE 'DBA-KONTROLL-IDDISTR         ' TO CURRENT-SECTION              
090700                                                                          
090800     IF MID-IDDISTR-FOM-NEW = ALL '+'                                     
090900       MOVE MFS-NUM-FAELT-FEL       TO MOD-IDDISTR-FOM-UPD-ATTR           
091000       MOVE ERR-INTERVALL-WRONG     TO MED-IDMFSFEL                       
091100       MOVE NEJ                     TO INDATA-SW                          
091200       MOVE JA                      TO VISA-ERR-MED-SW                    
091300     ELSE                                                                 
091400       IF MID-IDDISTR-FOM-NEW IS NUMERIC                                  
091500       AND MID-IDDISTR-FOM-NEW > ZERO                                     
091600         MOVE MFS-NUM-FAELT-RAETT   TO MOD-IDDISTR-FOM-UPD-ATTR           
091700         MOVE MID-IDDISTR-FOM-NEW   TO WS-IDDISTR                         
091800                                       W-KMRK-IDDISTR-FOM                 
091900       ELSE                                                               
092000         MOVE MFS-NUM-FAELT-FEL     TO MOD-IDDISTR-FOM-UPD-ATTR           
092100         MOVE ERR-INTERVALL-WRONG   TO MED-IDMFSFEL                       
092200         MOVE NEJ                   TO INDATA-SW                          
092300         MOVE JA                    TO VISA-ERR-MED-SW                    
092400       END-IF                                                             
092500       IF MID-IDDISTR-TOM-NEW = ALL '+'                                   
092600         MOVE MID-IDDISTR-FOM-NEW   TO MID-IDDISTR-TOM-NEW                
092700       END-IF                                                             
092800       IF MID-IDDISTR-TOM-NEW IS NUMERIC                                  
092900       AND MID-IDDISTR-TOM-NEW > ZERO                                     
093000         MOVE MFS-NUM-FAELT-RAETT   TO MOD-IDDISTR-TOM-UPD-ATTR           
093100         MOVE MID-IDDISTR-TOM-NEW   TO WS-IDDISTR-TOM                     
093200                                       W-KMRK-IDDISTR-TOM                 
093300       ELSE                                                               
093400         MOVE MFS-NUM-FAELT-FEL     TO MOD-IDDISTR-TOM-UPD-ATTR           
093500         MOVE ERR-INTERVALL-WRONG   TO MED-IDMFSFEL                       
093600         MOVE NEJ                   TO INDATA-SW                          
093700         MOVE JA                    TO VISA-ERR-MED-SW                    
093800       END-IF                                                             
093900     END-IF                                                               
094000                                                                          
094100     IF INDATA-OK                                                         
094200       IF MID-IDDISTR-FOM-NEW <= MID-IDDISTR-TOM-NEW                      
094300         MOVE MFS-NUM-FAELT-RAETT   TO MOD-IDDISTR-FOM-UPD-ATTR           
094400         MOVE MFS-NUM-FAELT-RAETT   TO MOD-IDDISTR-TOM-UPD-ATTR           
094500       ELSE                                                               
094600         MOVE MFS-NUM-FAELT-FEL     TO MOD-IDDISTR-FOM-UPD-ATTR           
094700         MOVE MFS-NUM-FAELT-FEL     TO MOD-IDDISTR-TOM-UPD-ATTR           
094800         MOVE ERR-INTERVALL-WRONG   TO MED-IDMFSFEL                       
094900         MOVE NEJ                   TO INDATA-SW                          
095000         MOVE JA                    TO VISA-ERR-MED-SW                    
095100       END-IF                                                             
095200     END-IF                                                               
095300     .                                                                    
095400     EJECT                                                                
095500                                                                          
095600 DBB-KONTROLL-IDKUNDNR SECTION.                                           
095700     MOVE 'DBB-KONTROLL-IDKUNDNR        ' TO CURRENT-SECTION              
095800                                                                          
095900     IF MID-IDKUNDNR-FOM-NEW = ALL '+'                                    
096000       MOVE MFS-NUM-FAELT-FEL       TO MOD-IDKUNDNR-FOM-UPD-ATTR          
096100       MOVE ERR-INTERVALL-WRONG     TO MED-IDMFSFEL                       
096200       MOVE NEJ                     TO INDATA-SW                          
096300       MOVE JA                      TO VISA-ERR-MED-SW                    
096400     ELSE                                                                 
096500       IF MID-IDKUNDNR-FOM-NEW IS NUMERIC                                 
096600         MOVE MFS-NUM-FAELT-RAETT   TO MOD-IDKUNDNR-FOM-UPD-ATTR          
096700         MOVE MID-IDKUNDNR-FOM-NEW  TO WS-IDKUNDNR                        
096800                                       W-KMRK-IDKUNDNR-FOM                
096900       ELSE                                                               
097000         MOVE MFS-NUM-FAELT-FEL     TO MOD-IDKUNDNR-FOM-UPD-ATTR          
097100         MOVE ERR-INTERVALL-WRONG   TO MED-IDMFSFEL                       
097200         MOVE NEJ                   TO INDATA-SW                          
097300         MOVE JA                    TO VISA-ERR-MED-SW                    
097400       END-IF                                                             
097500                                                                          
097600       IF MID-IDKUNDNR-TOM-NEW = ALL '+'                                  
097700         MOVE WS-IDKUNDNR           TO MID-IDKUNDNR-TOM-NEW               
097800       END-IF                                                             
097900                                                                          
098000       IF MID-IDKUNDNR-TOM-NEW IS NUMERIC                                 
098100         MOVE MFS-NUM-FAELT-RAETT   TO MOD-IDKUNDNR-TOM-UPD-ATTR          
098200         MOVE MID-IDKUNDNR-TOM-NEW  TO WS-IDKUNDNR-TOM                    
098300                                       W-KMRK-IDKUNDNR-TOM                
098400       ELSE                                                               
098500         MOVE MFS-NUM-FAELT-FEL     TO MOD-IDKUNDNR-TOM-UPD-ATTR          
098600         MOVE ERR-INTERVALL-WRONG   TO MED-IDMFSFEL                       
098700         MOVE NEJ                   TO INDATA-SW                          
098800         MOVE JA                    TO VISA-ERR-MED-SW                    
098900       END-IF                                                             
099000     END-IF                                                               
099100                                                                          
099200     IF INDATA-OK                                                         
099300       IF MID-IDDISTR-FOM-NEW = MID-IDDISTR-TOM-NEW                       
099400         IF MID-IDKUNDNR-FOM-NEW <= MID-IDKUNDNR-TOM-NEW                  
099500           MOVE MFS-NUM-FAELT-RAETT TO MOD-IDKUNDNR-FOM-UPD-ATTR          
099600           MOVE MFS-NUM-FAELT-RAETT TO MOD-IDKUNDNR-TOM-UPD-ATTR          
099700         ELSE                                                             
099800           MOVE MFS-NUM-FAELT-FEL   TO MOD-IDKUNDNR-FOM-UPD-ATTR          
099900           MOVE MFS-NUM-FAELT-FEL   TO MOD-IDKUNDNR-TOM-UPD-ATTR          
100000           MOVE ERR-INTERVALL-WRONG TO MED-IDMFSFEL                       
100100           MOVE NEJ                 TO INDATA-SW                          
100200           MOVE JA                  TO VISA-ERR-MED-SW                    
100300         END-IF                                                           
100400       END-IF                                                             
100500     END-IF                                                               
100600     .                                                                    
100700     EJECT                                                                
100800                                                                          
100900 DBBA-KONTROLL-INTERVALL SECTION.                                         
101000     MOVE 'DBBA-KONTROLL-INTERVALL      ' TO CURRENT-SECTION              
101100                                                                          
101200     MOVE JA  TO INTERVALL-SW                                             
101400     PERFORM IMS-GU-WDM211                                                
101600     PERFORM IMS-GNP-INTERVALL-WDM221                                     
101700     IF SEGMENT-FINNS                                                     
101800       PERFORM UNTIL SEGMENT-SAKNAS OR PARENT-MISSING                     
101900         IF  MID-IDDISTR-FOM-NEW  >  KMRK-IDDISTR-TOM                     
102000         OR  MID-IDDISTR-TOM-NEW  <  KMRK-IDDISTR-FOM                     
102100           CONTINUE                                                       
102200         ELSE                                                             
102300           IF  MID-IDDISTR-FOM-NEW  =  KMRK-IDDISTR-TOM                   
102400           AND MID-IDKUNDNR-FOM-NEW >  KMRK-IDKUNDNR-TOM                  
102500             CONTINUE                                                     
102600           ELSE                                                           
102700             IF  MID-IDDISTR-TOM-NEW  =  KMRK-IDDISTR-FOM                 
102800             AND MID-IDKUNDNR-TOM-NEW <  KMRK-IDKUNDNR-FOM                
102900               CONTINUE                                                   
103000             ELSE                                                         
103100               MOVE NEJ TO INTERVALL-SW                                   
103200             END-IF                                                       
103300           END-IF                                                         
103400         END-IF                                                           
103600         PERFORM IMS-GNP-INTERVALL-WDM221                                 
103700       END-PERFORM                                                        
103810     END-IF                                                               
103900     IF INTERVALL-OVERLAPP                                                
104000       MOVE ERR-INTERVALL-WRONG TO MED-IDMFSFEL                           
104100       MOVE NEJ                 TO INDATA-SW                              
104200       MOVE JA                  TO VISA-ERR-MED-SW                        
104300     END-IF                                                               
104400     .                                                                    
104500     EJECT                                                                
104600                                                                          
104700 DBC-KONTROLL-KVBEART SECTION.                                            
104800     MOVE 'DBC-KONTROLL-KVBEART         ' TO CURRENT-SECTION              
104900                                                                          
105000     IF MID-KVBEART-KAMP-NEW = ALL '+'                                    
105100       MOVE MFS-NUM-FAELT-FEL       TO MOD-KVBEART-KAMP-UPD-ATTR          
105200       MOVE NEJ                     TO INDATA-SW                          
105300     ELSE                                                                 
105400       IF MID-KVBEART-KAMP-NEW IS NUMERIC                                 
105500         PERFORM IMS-GU-WDM211                                            
105600         IF SEGMENT-FINNS                                                 
105700           IF KART-KVBEART-KUND > ZERO AND WDM221-SAKNAS                  
105800             PERFORM DBCA-KONTROLL-AV-KVBEART-KAMP                        
105900           ELSE                                                           
106000             PERFORM DBCA-KONTROLL-AV-KVBEART-KAMP                        
106100           END-IF                                                         
106200                                                                          
106300           PERFORM IMS-GU-WDM221                                          
106400           MOVE NEJ                 TO WDM221-SW                          
106500           IF SEGMENT-FINNS                                               
106600             MOVE KMRK-KVBEART-KAMP TO SPAR-KMRK-KVBEART-KAMP             
106700             MOVE KMRK-KVBEART-KUND TO SPAR-KMRK-KVBEART-KUND             
106800             MOVE JA                TO WDM221-SW                          
106900           ELSE                                                           
107000             MOVE ZERO              TO SPAR-KMRK-KVBEART-KAMP             
107100                                       SPAR-KMRK-KVBEART-KUND             
107200           END-IF                                                         
107300         ELSE                                                             
107400           MOVE INF-ARTIKEL-SAKNAS  TO MED-IDMFSINF                       
107500           MOVE NEJ                 TO  INDATA-SW VISA-ERR-MED-SW         
107600           PERFORM S02-INF-RUTINE                                         
107700         END-IF                                                           
107800         MOVE MID-KVBEART-KAMP-NEW  TO WS-KVBEART-KAMP                    
107900       ELSE                                                               
108000         MOVE MFS-NUM-FAELT-FEL     TO MOD-KVBEART-KAMP-UPD-ATTR          
108100         MOVE NEJ TO INDATA-SW                                            
108200       END-IF                                                             
108300     END-IF                                                               
108400     .                                                                    
108500     EJECT                                                                
108600                                                                          
108700 DBCA-KONTROLL-AV-KVBEART-KAMP SECTION.                                   
108800     MOVE 'DBCA-KONTROLL-AV-KVBEART-KAMP' TO CURRENT-SECTION              
108900                                                                          
109000     IF MID-KVBEART-KAMP-NEW >= SPAR-KMRK-KVBEART-KAMP AND                
109100       ACC-KART-KVBEART-KAMP >= ACC-KMRK-KVBEART-KAMP -                   
109200       SPAR-KMRK-KVBEART-KAMP + MID-KVBEART-KAMP-NEW                      
109300      MOVE MFS-NUM-FAELT-RAETT TO MOD-KVBEART-KAMP-UPD-ATTR               
109400     ELSE                                                                 
109500       MOVE MFS-NUM-FAELT-FEL   TO MOD-KVBEART-KAMP-UPD-ATTR              
109600       MOVE ERR-QUANT-TO-BIG    TO MED-IDMFSFEL                           
109700       MOVE JA                  TO VISA-ERR-MED-SW                        
109800       MOVE NEJ                 TO INDATA-SW                              
109900     END-IF                                                               
110000     .                                                                    
110100     EJECT                                                                
110200                                                                          
110300 E-NEW-INSERT SECTION.                                                    
110400     MOVE 'E-NEW-INSERT                 ' TO CURRENT-SECTION              
110500                                                                          
110900     PERFORM IMS-GHU-WDM221                                               
111000     IF SEGMENT-SAKNAS                                                    
111100        PERFORM EB-NYREGISTRERING                                         
111300     END-IF                                                               
111400                                                                          
111500     IF ALLT-OK                                                           
111600       MOVE WS-IDDISTR              TO W-KMRK-IDDISTR-FOM                 
111700       MOVE WS-IDDISTR-TOM          TO W-KMRK-IDDISTR-TOM                 
111800       MOVE WS-IDKUNDNR7            TO W-KMRK-IDKUNDNR-FOM                
111900       MOVE WS-IDKUNDNR7-TOM        TO W-KMRK-IDKUNDNR-TOM                
112000     END-IF                                                               
112100     .                                                                    
112200     EJECT                                                                
112300                                                                          
112400 E-COPY-CAMP-DC-ART  SECTION.                                             
112500     MOVE 'E-COPY-CAMP-DC-ART           ' TO CURRENT-SECTION              
112600                                                                          
112800     IF MID-IDARTNR-COPY NOT = ALL '+'                                    
113204       PERFORM IMS-GU-WDM211-COPY                                         
113205       IF SEGMENT-FINNS                                                   
113728          PERFORM IMS-GNP-WDM221-COPY                                     
113729          IF SEGMENT-SAKNAS                                               
113730            MOVE NEJ                       TO UPDATE-SW                   
113731            MOVE ERR-MISSING-REGISTER      TO MED-IDMFSFEL                
113732            PERFORM S01-ERR-RUTINE                                        
113733          ELSE                                                            
113734            PERFORM UNTIL SEGMENT-SAKNAS                                  
113739              PERFORM EA-CHECK-INTERVALL-OVERLAPP                         
113740              IF INTERVALL-OUTSIDE                                        
113754                MOVE COPY-KMRK-IDDISTR-FOM  TO KMRK-IDDISTR-FOM           
113755                MOVE COPY-KMRK-IDDISTR-TOM  TO KMRK-IDDISTR-TOM           
113756                MOVE COPY-KMRK-IDKUNDNR-FOM TO KMRK-IDKUNDNR-FOM          
113758                MOVE COPY-KMRK-IDKUNDNR-TOM TO KMRK-IDKUNDNR-TOM          
113760                MOVE ZERO                   TO KMRK-KVBEART-KAMP          
113761                MOVE ZERO                   TO KMRK-KVBEART-KUND          
113762                PERFORM IMS-ISRT-WDM221                                   
113763              END-IF                                                      
113764                                                                          
113768              MOVE COPY-KMRK-IDDISTR-FOM    TO W-COPY-IDDISTR-FOM         
113770              MOVE COPY-KMRK-IDDISTR-TOM    TO W-COPY-IDDISTR-TOM         
113772              MOVE COPY-KMRK-IDKUNDNR-FOM   TO W-COPY-IDKUNDNR-FOM        
113774              MOVE COPY-KMRK-IDKUNDNR-TOM   TO W-COPY-IDKUNDNR-TOM        
113776              PERFORM IMS-GU-WDM211-COPY                                  
113778              PERFORM IMS-GNP-WDM221-COPY                                 
113780            END-PERFORM                                                   
113781          END-IF                                                          
113786       ELSE                                                               
113787         MOVE NEJ                        TO UPDATE-SW                     
113788         MOVE ERR-MISSING-REGISTER       TO MED-IDMFSFEL                  
113789         PERFORM S01-ERR-RUTINE                                           
113790       END-IF                                                             
113800     END-IF                                                               
114500     .                                                                    
114600     EJECT                                                                
114610 EA-CHECK-INTERVALL-OVERLAPP SECTION.                                     
114620     MOVE 'EA-CHECK-INTERVALL-OVERLAPP'   TO CURRENT-SECTION              
114630                                                                          
114640     MOVE JA            TO INTERVALL-SW                                   
114650     PERFORM IMS-GU-WDM211                                                
114660     PERFORM IMS-GNP-INTERVALL-WDM221                                     
114670     IF SEGMENT-FINNS                                                     
114680       PERFORM UNTIL SEGMENT-SAKNAS OR PARENT-MISSING                     
114690         IF  COPY-KMRK-IDDISTR-FOM >  KMRK-IDDISTR-TOM                    
114691         OR  COPY-KMRK-IDDISTR-TOM <  KMRK-IDDISTR-FOM                    
114692           CONTINUE                                                       
114693         ELSE                                                             
114694           IF  COPY-KMRK-IDDISTR-FOM  = KMRK-IDDISTR-TOM                  
114695           AND COPY-KMRK-IDKUNDNR-FOM > KMRK-IDKUNDNR-TOM                 
114696             CONTINUE                                                     
114697           ELSE                                                           
114698             IF  COPY-KMRK-IDDISTR-TOM  = KMRK-IDDISTR-FOM                
114699             AND COPY-KMRK-IDKUNDNR-TOM < KMRK-IDKUNDNR-FOM               
114700               CONTINUE                                                   
114710             ELSE                                                         
114720               MOVE NEJ TO INTERVALL-SW                                   
114730             END-IF                                                       
114740           END-IF                                                         
114750         END-IF                                                           
114760         PERFORM IMS-GNP-INTERVALL-WDM221                                 
114770       END-PERFORM                                                        
114780     END-IF                                                               
114795     .                                                                    
114796     EJECT                                                                
114800                                                                          
114820 E-UPPDATERA     SECTION.                                                 
114900     MOVE 'E-UPPDATERA                  ' TO CURRENT-SECTION              
115000                                                                          
115100     MOVE +1 TO WS-IX                                                     
115200     PERFORM UNTIL WS-IX > MAX-INDX                                       
115300       IF MID-CMD-UPD(WS-IX) = 'U'                                        
115400         MOVE WS-IDARTNR              TO W-KART-IDARTNR                   
115500         MOVE MID-IDDISTR-FOM(WS-IX)  TO W-KMRK-IDDISTR-FOM               
115600         MOVE MID-IDDISTR-TOM(WS-IX)  TO W-KMRK-IDDISTR-TOM               
115700         MOVE MID-IDKUNDNR-FOM(WS-IX) TO W-KMRK-IDKUNDNR-FOM              
115800         MOVE MID-IDKUNDNR-TOM(WS-IX) TO W-KMRK-IDKUNDNR-TOM              
116000         PERFORM IMS-GHU-WDM221                                           
116100         IF SEGMENT-FINNS                                                 
116200           MOVE WS-IDARTNR              TO KART-IDARTNR                   
116300           MOVE MID-IDDISTR-FOM(WS-IX)  TO KMRK-IDDISTR-FOM               
116400           MOVE MID-IDDISTR-TOM(WS-IX)  TO KMRK-IDDISTR-TOM               
116500           MOVE MID-IDKUNDNR-FOM(WS-IX) TO KMRK-IDKUNDNR-FOM              
116600           MOVE MID-IDKUNDNR-TOM(WS-IX) TO KMRK-IDKUNDNR-TOM              
116700           MOVE MID-KVBEART-KAMP(WS-IX) TO KMRK-KVBEART-KAMP              
116800           MOVE MID-KVBEART-KUND(WS-IX) TO KMRK-KVBEART-KUND              
117000           PERFORM IMS-REPL-WDM221                                        
117100         ELSE                                                             
117200           CONTINUE                                                       
117300         END-IF                                                           
117400       END-IF                                                             
117500       IF MID-CMD-UPD(WS-IX) = 'D'                                        
117600         PERFORM EC-BORTTAG                                               
117700       END-IF                                                             
117800       ADD +1 TO WS-IX                                                    
117900     END-PERFORM                                                          
117901                                                                          
117910     MOVE SAVE-IDDISTR-ENTER-FOM    TO W-KMRK-IDDISTR-FOM                 
117920     MOVE SAVE-IDDISTR-ENTER-TOM    TO W-KMRK-IDDISTR-TOM                 
117930     MOVE SAVE-IDKUNDNR-ENTER-FOM   TO W-KMRK-IDKUNDNR-FOM                
117940     MOVE SAVE-IDKUNDNR-ENTER-TOM   TO W-KMRK-IDKUNDNR-TOM                
118000     .                                                                    
118100     EJECT                                                                
118200                                                                          
118300 EB-COPY-CAMP-ART  SECTION.                                               
118400     MOVE 'EB-COPY-CAMP-ART             ' TO CURRENT-SECTION              
118500                                                                          
118700     MOVE WS-IDDC                    TO W-KAMP-IDDC                       
118800     MOVE WS-IDARTNR                 TO W-KART-IDARTNR                    
118900     PERFORM IMS-GU-WDM211-COPY                                           
118910     IF SEGMENT-FINNS                                                     
119130       PERFORM IMS-GNP-WDM221-COPY                                        
119400       PERFORM UNTIL SEGMENT-SAKNAS OR END-OF-DATA                        
119500         MOVE KMRK-IDDISTR-FOM  TO W-KMRK-IDDISTR-FOM                     
119600         MOVE KMRK-IDDISTR-TOM  TO W-KMRK-IDDISTR-TOM                     
119700         MOVE KMRK-IDKUNDNR-FOM TO W-KMRK-IDKUNDNR-FOM                    
119800         MOVE KMRK-IDKUNDNR-TOM TO W-KMRK-IDKUNDNR-TOM                    
120000         PERFORM IMS-GHU-WDM221                                           
120100         IF SEGMENT-FINNS                                                 
120200           CONTINUE                                                       
120300         ELSE                                                             
120600           PERFORM EBB-COPY-TO-IO-AREA-WDM221                             
120800           PERFORM IMS-ISRT-COPY-WDM221                                   
120900         END-IF                                                           
121001         PERFORM IMS-GNP-WDM221-COPY                                      
121200       END-PERFORM                                                        
121210     END-IF                                                               
121300                                                                          
121400     IF PARENT-MISSING                                                    
121500       MOVE NEJ       TO UPDATE-SW                                        
121600       MOVE ERR-MISSING-REGISTER TO MED-IDMFSFEL                          
121700       PERFORM S01-ERR-RUTINE                                             
121800     ELSE                                                                 
121900       MOVE NEJ       TO UPDATE-SW                                        
122000       MOVE ERR-LINE-EXIST       TO MED-IDMFSFEL                          
122100       PERFORM S01-ERR-RUTINE                                             
122300     END-IF                                                               
122400     .                                                                    
122500     EJECT                                                                
122600                                                                          
122700 EB-NYREGISTRERING SECTION.                                               
122800     MOVE 'EB-NYREGISTRERING            ' TO CURRENT-SECTION              
122900                                                                          
123200     PERFORM EBB-MOVE-TO-IO-AREA-WDM221                                   
123400     PERFORM IMS-ISRT-WDM221                                              
123500     .                                                                    
123600     EJECT                                                                
125100                                                                          
125200 EBB-MOVE-TO-IO-AREA-WDM221 SECTION.                                      
125300     MOVE 'EBB-MOVE-TO-IO-AREA-WDM221' TO CURRENT-SECTION                 
125400                                                                          
125500     MOVE WS-IDARTNR                TO KART-IDARTNR                       
125600     MOVE WS-IDDISTR                TO KMRK-IDDISTR-FOM                   
125700     MOVE WS-IDDISTR-TOM            TO KMRK-IDDISTR-TOM                   
125800     MOVE WS-IDKUNDNR               TO KMRK-IDKUNDNR-FOM                  
125900     MOVE WS-IDKUNDNR-TOM           TO KMRK-IDKUNDNR-TOM                  
126000     MOVE WS-KVBEART-KAMP           TO KMRK-KVBEART-KAMP                  
126100     MOVE WS-KVBEART-KUND           TO KMRK-KVBEART-KUND                  
126110                                                                          
126111     MOVE KMRK-IDDISTR-FOM          TO W-KMRK-IDDISTR-FOM                 
126112     MOVE KMRK-IDDISTR-TOM          TO W-KMRK-IDDISTR-TOM                 
126113     MOVE KMRK-IDKUNDNR-FOM         TO W-KMRK-IDKUNDNR-FOM                
126114     MOVE KMRK-IDKUNDNR-TOM         TO W-KMRK-IDKUNDNR-TOM                
126200     .                                                                    
126300     EJECT                                                                
126400                                                                          
126500 EBB-COPY-TO-IO-AREA-WDM221 SECTION.                                      
126600     MOVE 'EBB-COPY-TO-IO-AREA-WDM221' TO CURRENT-SECTION                 
126700                                                                          
126800     MOVE WS-IDARTNR                TO KART-IDARTNR                       
126900     MOVE COPY-KMRK-IDDISTR-FOM     TO KMRK-IDDISTR-FOM                   
127000     MOVE COPY-KMRK-IDDISTR-TOM     TO KMRK-IDDISTR-TOM                   
127100     MOVE COPY-KMRK-IDKUNDNR-FOM    TO KMRK-IDKUNDNR-FOM                  
127200     MOVE COPY-KMRK-IDKUNDNR-TOM    TO KMRK-IDKUNDNR-TOM                  
127300     MOVE ZERO                      TO KMRK-KVBEART-KAMP                  
127400     MOVE ZERO                      TO KMRK-KVBEART-KUND                  
127500     .                                                                    
127600     EJECT                                                                
127700                                                                          
127800 EC-BORTTAG SECTION.                                                      
127900     MOVE 'EC-BORTTAG                   ' TO CURRENT-SECTION              
128000                                                                          
128200     MOVE WS-IDARTNR              TO W-KART-IDARTNR                       
128300     MOVE MID-IDDISTR-FOM(WS-IX)  TO W-KMRK-IDDISTR-FOM                   
128400     MOVE MID-IDDISTR-TOM(WS-IX)  TO W-KMRK-IDDISTR-TOM                   
128500     MOVE MID-IDKUNDNR-FOM(WS-IX) TO W-KMRK-IDKUNDNR-FOM                  
128600     MOVE MID-IDKUNDNR-TOM(WS-IX) TO W-KMRK-IDKUNDNR-TOM                  
128800     PERFORM IMS-GHU-WDM221                                               
128900     IF SEGMENT-SAKNAS                                                    
129000       CONTINUE                                                           
129100     END-IF                                                               
129300     PERFORM IMS-DLET-WDM221                                              
130400     .                                                                    
130500     EJECT                                                                
130600                                                                          
130700 F-MFS-IDPFK-KONTROLL SECTION.                                            
130800     MOVE 'F-MFS-IDPFK-KONTROLL         ' TO CURRENT-SECTION              
130900                                                                          
131000     IF MFS-FIRST                                                         
131100       PERFORM FA-FOERSTA-SIDA                                            
131200     ELSE                                                                 
131300       IF MFS-NEXT                                                        
131400         PERFORM FB-NAESTA-SIDA                                           
131500       ELSE                                                               
131600         PERFORM FC-SAMMA-SIDA                                            
131700       END-IF                                                             
131800     END-IF                                                               
131900     .                                                                    
132000     EJECT                                                                
132100                                                                          
132200 FA-FOERSTA-SIDA SECTION.                                                 
132300     MOVE 'FA-FOERSTA-SIDA              ' TO CURRENT-SECTION              
132400                                                                          
132500     MOVE INF-FIRST-PAGE TO MED-IDMFSINF                                  
132600     PERFORM S02-INF-RUTINE                                               
132700*    --- BLANKA/NOLLA UT BLÄDDRINGSNYCKEL                                 
132800     MOVE ZERO                   TO W-KMRK-IDDISTR-FOM                    
132900                                    W-KMRK-IDKUNDNR-FOM                   
133000     MOVE ZERO                   TO W-KMRK-IDDISTR-TOM                    
133100                                    W-KMRK-IDKUNDNR-TOM                   
133200     MOVE JA                     TO ALLT-SW                               
133300     .                                                                    
133400     EJECT                                                                
133500                                                                          
133600 FB-NAESTA-SIDA SECTION.                                                  
133700     MOVE 'FB-NAESTA-SIDA               ' TO CURRENT-SECTION              
133800                                                                          
133810     MOVE SAVE-IDKAMPRF-KEY      TO W-KAMP-IDKAMPRF                       
133820     MOVE SAVE-IDDC-KEY          TO W-KAMP-IDDC                           
133830     MOVE SAVE-IDARTNR-KEY       TO W-KART-IDARTNR                        
133900     MOVE SAVE-IDDISTR-FOM-NEXT  TO W-KMRK-IDDISTR-FOM                    
134000     MOVE SAVE-IDDISTR-TOM-NEXT  TO W-KMRK-IDDISTR-TOM                    
134100     MOVE SAVE-IDKUNDNR-FOM-NEXT TO W-KMRK-IDKUNDNR-FOM                   
134200     MOVE SAVE-IDKUNDNR-TOM-NEXT TO W-KMRK-IDKUNDNR-TOM                   
134290                                                                          
134300     MOVE JA TO ALLT-SW                                                   
134400     .                                                                    
134500     EJECT                                                                
134600                                                                          
134700 FC-SAMMA-SIDA SECTION.                                                   
134800     MOVE 'FC-SAMMA-SIDA                ' TO CURRENT-SECTION              
134900                                                                          
135000     PERFORM FCA-INFAELT-KONTROLL                                         
135100     IF INDATA-EXISTS                                                     
135200       MOVE NEJ                     TO ALLT-SW                            
135300       PERFORM FCB-LAES-IN-IGEN                                           
135400       MOVE INF-PRESS-PF11 TO MED-IDMFSINF                                
135500       PERFORM S02-INF-RUTINE                                             
135600       PERFORM MFS-ROER-EJ-FAELT-UT                                       
135700       PERFORM MFS-ROER-EJ-FAELT-IN                                       
135800       PERFORM MFS-LAS-IN-IGEN                                            
135900     ELSE                                                                 
136000       MOVE SAVE-IDKAMPRF-KEY       TO W-KAMP-IDKAMPRF                    
136001       MOVE SAVE-IDDC-KEY           TO W-KAMP-IDDC                        
136002       MOVE SAVE-IDARTNR-KEY        TO W-KART-IDARTNR                     
136010       MOVE SAVE-IDDISTR-ENTER-FOM  TO W-KMRK-IDDISTR-FOM                 
136100       MOVE SAVE-IDDISTR-ENTER-TOM  TO W-KMRK-IDDISTR-TOM                 
136200       MOVE SAVE-IDKUNDNR-ENTER-FOM TO W-KMRK-IDKUNDNR-FOM                
136300       MOVE SAVE-IDKUNDNR-ENTER-TOM TO W-KMRK-IDKUNDNR-TOM                
136400     END-IF                                                               
136500     .                                                                    
136600     EJECT                                                                
136700                                                                          
136800 FCA-INFAELT-KONTROLL SECTION.                                            
136900     MOVE 'FCA-INFAELT-KONTROLL         ' TO CURRENT-SECTION              
137000                                                                          
137100     IF MID-IDDISTR-FOM-NEW   = ALL '+' AND                               
137200        MID-IDDISTR-TOM-NEW   = ALL '+' AND                               
137300        MID-IDKUNDNR-FOM-NEW  = ALL '+' AND                               
137400        MID-IDKUNDNR-TOM-NEW  = ALL '+' AND                               
137500        MID-KVBEART-KAMP-NEW  = ALL '+' AND                               
137700        MID-IDKAMPRF-COPY     = ALL '+' AND                               
137800        MID-IDDC-COPY         = ALL '+' AND                               
137900        MID-IDARTNR-COPY      = ALL '+'                                   
138000       MOVE NEJ        TO INDATA-EXISTS-SW                                
138100     ELSE                                                                 
138200       MOVE JA         TO INDATA-EXISTS-SW                                
138300     END-IF                                                               
138400     .                                                                    
138500     EJECT                                                                
138600                                                                          
138700 FCB-LAES-IN-IGEN SECTION.                                                
138800     MOVE 'FCB-LAES-IN-IGEN             ' TO CURRENT-SECTION              
138900                                                                          
139000     IF MID-IDDISTR-FOM-NEW NOT = ALL '+'                                 
139100       MOVE MFS-ADD-LAES-IN-FAELT   TO MOD-IDDISTR-FOM-UPD-ATTR           
139200     END-IF                                                               
139300                                                                          
139400     IF MID-IDDISTR-TOM-NEW NOT = ALL '+'                                 
139500       MOVE MFS-ADD-LAES-IN-FAELT   TO MOD-IDDISTR-TOM-UPD-ATTR           
139600     END-IF                                                               
139700                                                                          
139800     IF MID-IDKUNDNR-FOM-NEW NOT = ALL '+'                                
139900       MOVE MFS-ADD-LAES-IN-FAELT   TO MOD-IDKUNDNR-FOM-UPD-ATTR          
140000     END-IF                                                               
140100                                                                          
140200     IF MID-IDKUNDNR-TOM-NEW NOT = ALL '+'                                
140300       MOVE MFS-ADD-LAES-IN-FAELT   TO MOD-IDKUNDNR-TOM-UPD-ATTR          
140400     END-IF                                                               
140500                                                                          
140600     IF MID-KVBEART-KAMP-NEW NOT = ALL '+'                                
140700       MOVE MFS-ADD-LAES-IN-FAELT   TO MOD-KVBEART-KAMP-UPD-ATTR          
140800     END-IF                                                               
140900                                                                          
141400     IF MID-IDKAMPRF-COPY NOT = ALL '+'                                   
141500       MOVE MFS-ADD-LAES-IN-FAELT   TO MOD-IDKAMPRF-COPY-ATTR             
141600     END-IF                                                               
141700                                                                          
141800     IF MID-IDDC-COPY NOT = ALL '+'                                       
141900       MOVE MFS-ADD-LAES-IN-FAELT   TO MOD-IDDC-COPY-ATTR                 
142000     END-IF                                                               
142100                                                                          
142200     IF MID-IDARTNR-COPY NOT = ALL '+'                                    
142300       MOVE MFS-ADD-LAES-IN-FAELT   TO MOD-IDARTNR-COPY-ATTR              
142400     END-IF                                                               
142500     .                                                                    
142600     EJECT                                                                
142700                                                                          
142800 G-LAES-VISA-INFO SECTION.                                                
142900     MOVE 'G-LAES-VISA-INFO             ' TO CURRENT-SECTION              
143091                                                                          
143320     PERFORM IMS-GU-WDM211                                                
143400     IF SEGMENT-SAKNAS                                                    
143500       MOVE ERR-KEYS-ARE-MISSING  TO MED-IDMFSFEL                         
143600       PERFORM S01-ERR-RUTINE                                             
143700       PERFORM MFS-RENSA-FAELT-UT                                         
143710       PERFORM MFS-STAENG-FAELT-IN                                        
143800     ELSE                                                                 
143860       MOVE W-KAMP-IDKAMPRF       TO SAVE-IDKAMPRF-KEY                    
143870       MOVE W-KAMP-IDDC           TO SAVE-IDDC-KEY                        
143880       MOVE W-KART-IDARTNR        TO SAVE-IDARTNR-KEY                     
143900       PERFORM IMS-GNP-WDM221                                             
144000       IF SEGMENT-FINNS                                                   
144110        MOVE KMRK-IDDISTR-FOM     TO SAVE-IDDISTR-ENTER-FOM               
144200        MOVE KMRK-IDDISTR-TOM     TO SAVE-IDDISTR-ENTER-TOM               
144210        MOVE KMRK-IDKUNDNR-FOM    TO SAVE-IDKUNDNR-ENTER-FOM              
144220        MOVE KMRK-IDKUNDNR-TOM    TO SAVE-IDKUNDNR-ENTER-TOM              
144300        MOVE +1                   TO INDX                                 
144400                                                                          
144500        PERFORM UNTIL INDX > MAX-INDX                                     
144600          IF SEGMENT-FINNS                                                
144700            PERFORM GC-MOVE-RADDATA-TO-MOD                                
144800            MOVE MFS-ADD-LAES-IN-FAELT                                    
144900                                  TO MOD-KVBEART-KAMP-ATTR(INDX)          
145000            PERFORM IMS-GNP-WDM221                                        
145100          ELSE                                                            
145200            MOVE MFS-STAENG-FAELT TO MOD-CMD-UPD-ATTR     (INDX)          
145300            MOVE MFS-STAENG-FAELT TO MOD-KVBEART-KAMP-ATTR(INDX)          
145400            MOVE MFS-RENSA-FAELT  TO MOD-CMD-UPD          (INDX)          
145500                                     MOD-KVBEART-KAMP     (INDX)          
145600            PERFORM MFS-RENSA-RAD-FAELT-UT                                
145700          END-IF                                                          
145800          ADD +1                  TO INDX                                 
145900        END-PERFORM                                                       
146000                                                                          
146100        PERFORM GD-VISA-DIV-INFO-TEXTER                                   
146200       ELSE                                                               
146300        MOVE ZERO                 TO SAVE-IDDISTR-ENTER-FOM               
146400        MOVE ZERO                 TO SAVE-IDDISTR-ENTER-TOM               
146410        MOVE ZERO                 TO SAVE-IDKUNDNR-ENTER-FOM              
146420        MOVE ZERO                 TO SAVE-IDKUNDNR-ENTER-TOM              
146440        MOVE +1                   TO INDX                                 
146450        PERFORM UNTIL INDX > MAX-INDX                                     
146460          MOVE MFS-STAENG-FAELT   TO MOD-CMD-UPD-ATTR (INDX)              
146461          MOVE MFS-STAENG-FAELT   TO MOD-KVBEART-KAMP-ATTR(INDX)          
146470          ADD 1                   TO INDX                                 
146480        END-PERFORM                                                       
146500       END-IF                                                             
146600     END-IF                                                               
146601     PERFORM GA-VISA-KVKVARFD                                             
146607                                                                          
146610*   ---  UPPDATERA MSGI-SPAR-AREA                                         
146620     MOVE '002'                     TO MSGI-KDCALL                        
146630     MOVE '2312'                    TO MSGI-IDTRANS                       
146640     MOVE SAVE-AREA                 TO MSGI-SPAR-AREA                     
146650     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
146700     PERFORM MFS-RENSA-FAELT-IN                                           
146800     .                                                                    
146900     EJECT                                                                
147000                                                                          
147100 GA-VISA-KVKVARFD SECTION.                                                
147200     MOVE 'GA-VISA-KVKVARFD             ' TO CURRENT-SECTION              
147300                                                                          
147400     IF MFS-UPDATE                                                        
147600       IF MID-IDARTNR-COPY NOT = ALL '+'                                  
147700       AND YES-TO-NEW-INSERT                                              
147800         MOVE WS-IDDISTR       TO W-KMRK-IDDISTR-FOM                      
147900         MOVE WS-IDDISTR-TOM   TO W-KMRK-IDDISTR-TOM                      
148000         MOVE WS-IDKUNDNR7     TO W-KMRK-IDKUNDNR-FOM                     
148100         MOVE WS-IDKUNDNR7-TOM TO W-KMRK-IDKUNDNR-TOM                     
148200         COMPUTE WS-KVKVARFD = (ACC-KART-KVBEART-KAMP) -                  
148300           (ACC-KMRK-KVBEART-KAMP - SPAR-KMRK-KVBEART-KAMP +              
148400            MID-KVBEART-KAMP-NEW)                                         
148500         END-COMPUTE                                                      
148600       ELSE                                                               
148700         PERFORM S01-COUNT-KVBEART-KAMP                                   
148800         COMPUTE WS-KVKVARFD = ACC-KART-KVBEART-KAMP -                    
148900                               ACC-KMRK-KVBEART-KAMP                      
149000       END-IF                                                             
149100     ELSE                                                                 
149200*      PERFORM S01-COUNT-KVBEART-KAMP                                     
149300       COMPUTE WS-KVKVARFD = ACC-KART-KVBEART-KAMP -                      
149400                             ACC-KMRK-KVBEART-KAMP                        
149500       END-COMPUTE                                                        
149600     END-IF                                                               
149700     MOVE WS-KVKVARFD        TO MOD-KVKVARFD                              
149800     .                                                                    
149900     EJECT                                                                
150000                                                                          
150100 GC-MOVE-RADDATA-TO-MOD SECTION.                                          
150200     MOVE 'GC-MOVE-RADDATA-TO-MOD       ' TO CURRENT-SECTION              
150300                                                                          
150400     MOVE KMRK-IDDISTR-FOM      TO WS-NUM9                                
150500     MOVE WS-ED                 TO MOD-IDDISTR-FOM(INDX)                  
150600     MOVE KMRK-IDDISTR-TOM      TO WS-NUM9                                
150700     MOVE WS-ED                 TO MOD-IDDISTR-TOM(INDX)                  
150800     MOVE KMRK-IDKUNDNR-FOM     TO WS-IDKUNDNR                            
150900     MOVE WS-EDKUNDNR           TO MOD-IDKUNDNR-FOM(INDX)                 
151000     MOVE KMRK-IDKUNDNR-TOM     TO WS-IDKUNDNR-TOM                        
151100     MOVE WS-EDKUNDNR-TOM       TO MOD-IDKUNDNR-TOM(INDX)                 
151200     MOVE KMRK-KVBEART-KAMP     TO WS-NUM9                                
151300     MOVE WS-ED                 TO MOD-KVBEART-KAMP(INDX)                 
151400     MOVE KMRK-KVBEART-KUND     TO WS-NUM9                                
151500     MOVE WS-ED                 TO MOD-KVBEART-KUND(INDX)                 
151600     COMPUTE WS-KVBEART-REM = KMRK-KVBEART-KAMP -                         
151700                              KMRK-KVBEART-KUND                           
151800     MOVE WS-KVBEART-REM        TO WS-NUM9                                
151900     MOVE WS-ED                 TO MOD-KVBEART-REM(INDX)                  
152000     .                                                                    
152100     EJECT                                                                
152200                                                                          
152300 GD-VISA-DIV-INFO-TEXTER SECTION.                                         
152400     MOVE 'GD-VISA-DIV-INFO-TEXTER      ' TO CURRENT-SECTION              
152500                                                                          
152600     IF SEGMENT-FINNS                                                     
152700       MOVE KMRK-IDDISTR-FOM        TO SAVE-IDDISTR-FOM-NEXT              
152800       MOVE KMRK-IDKUNDNR-FOM       TO SAVE-IDKUNDNR-FOM-NEXT             
152900       MOVE KMRK-IDDISTR-TOM        TO SAVE-IDDISTR-TOM-NEXT              
153000       MOVE KMRK-IDKUNDNR-TOM       TO SAVE-IDKUNDNR-TOM-NEXT             
153100       MOVE INF-MORE-INFO-EXISTS    TO MED-IDMFSINF                       
153200       PERFORM S02-INF-RUTINE                                             
153300     ELSE                                                                 
153400       MOVE SAVE-IDDISTR-ENTER-FOM  TO SAVE-IDDISTR-FOM-NEXT              
153500       MOVE SAVE-IDKUNDNR-ENTER-FOM TO SAVE-IDKUNDNR-FOM-NEXT             
153600       MOVE SAVE-IDDISTR-ENTER-TOM  TO SAVE-IDDISTR-TOM-NEXT              
153700       MOVE SAVE-IDKUNDNR-ENTER-TOM TO SAVE-IDKUNDNR-TOM-NEXT             
153800       IF MFS-FIRST                                                       
153900         MOVE INF-FIRST-PAGE        TO MED-IDMFSINF                       
154000         PERFORM S02-INF-RUTINE                                           
154100       ELSE                                                               
154200         IF MFS-NEXT                                                      
154300           MOVE INF-LAST-PAGE       TO MED-IDMFSINF                       
154400           PERFORM S02-INF-RUTINE                                         
154500         END-IF                                                           
154600       END-IF                                                             
154700     END-IF                                                               
154800                                                                          
154900     IF UPDATE-OK AND ALLT-OK                                             
155000       MOVE INF-UPDATE-DONE         TO MED-IDMFSINF                       
155100       PERFORM S02-INF-RUTINE                                             
155200       PERFORM MFS-RENSA-FAELT-IN                                         
155300     END-IF                                                               
155400     .                                                                    
155500     EJECT                                                                
155600                                                                          
155700 S01-COUNT-KVBEART-KAMP SECTION.                                          
155800     MOVE 'S01-COUNT-KVBEART-KAMP       ' TO CURRENT-SECTION              
155900                                                                          
156000     MOVE ZERO                 TO ACC-KMRK-KVBEART-KAMP                   
156100                                  ACC-KART-KVBEART-KAMP                   
156110                                                                          
156120     MOVE WS-KAMPRF            TO W-KAMP-IDKAMPRF                         
156130     MOVE WS-IDDC              TO W-KAMP-IDDC                             
156140     MOVE WS-IDARTNR           TO W-KART-IDARTNR                          
156700                                                                          
156710     MOVE LOW-VALUE                        TO W-WDM221-X                  
156720     MOVE HIGH-VALUE                       TO W-WDM221-MAX-X              
156730                                                                          
156800     PERFORM IMS-GU-WDM211                                                
156900     IF SEGMENT-FINNS                                                     
157000        MOVE KART-KVBEART-KAMP   TO ACC-KART-KVBEART-KAMP                 
157100                                                                          
157200        PERFORM IMS-GNP-WDM221                                            
157300        PERFORM UNTIL SEGMENT-SAKNAS OR END-OF-DATA                       
157400          ADD KMRK-KVBEART-KAMP  TO ACC-KMRK-KVBEART-KAMP                 
157500          PERFORM IMS-GNP-WDM221                                          
157600       END-PERFORM                                                        
157700     ELSE                                                                 
157701       MOVE NEJ                  TO ART-REG-PA-KAMPANJ-SW                 
157710     END-IF                                                               
157800                                                                          
157900     .                                                                    
158000     EJECT                                                                
158100                                                                          
158200 S01-ERR-RUTINE SECTION.                                                  
158300     MOVE 'S01-ERR-RUTINE               ' TO CURRENT-SECTION              
158400                                                                          
158500     CALL WMEDKONV USING MED-WMEDAREA                                     
158600     MOVE MED-MFSFEL                TO MOD-TEMFSFEL                       
158700     .                                                                    
158800     SKIP2                                                                
158900                                                                          
159000 S02-INF-RUTINE SECTION.                                                  
159100     MOVE 'S02-INF-RUTINE               ' TO CURRENT-SECTION              
159200                                                                          
159300     CALL WMEDKONV USING MED-WMEDAREA                                     
159400     MOVE MED-MFSINF              TO MOD-TEMFSINF                         
159500     .                                                                    
159600     SKIP2                                                                
159700                                                                          
159800 S04-ADD-TO-MID SECTION.                                                  
159900     MOVE 'S04-ADD-TO-MID               ' TO CURRENT-SECTION              
160000                                                                          
160100     MOVE MFS-ADD-LAES-IN-FAELT TO MOD-KVKVARFD-ATTR                      
160200     MOVE +1 TO WS-IX                                                     
160300     PERFORM UNTIL WS-IX > MAX-INDX                                       
160400       MOVE MFS-ADD-LAES-IN-FAELT TO MOD-IDDISTR-FOM-ATTR(WS-IX)          
160500                                     MOD-IDDISTR-TOM-ATTR(WS-IX)          
160600                                     MOD-IDKUNDNR-FOM-ATTR(WS-IX)         
160700                                     MOD-IDKUNDNR-TOM-ATTR(WS-IX)         
160800*                                    MOD-KVBEART-KAMP-ATTR(WS-IX)         
160900                                     MOD-KVBEART-KUND-ATTR(WS-IX)         
161000                                     MOD-KVBEART-REM-ATTR(WS-IX)          
161100       ADD +1 TO WS-IX                                                    
161200     END-PERFORM                                                          
161300     MOVE 1 TO WS-IX                                                      
161400     .                                                                    
161500     SKIP2                                                                
161600                                                                          
161700 MFS-RENSA-FAELT-UT SECTION.                                              
162400     MOVE MFS-RENSA-FAELT           TO MOD-IDKAMPRF-COPY                  
162500                                       MOD-IDDC-COPY                      
162600                                       MOD-IDARTNR-COPY                   
162700     MOVE +1                        TO INDX                               
162800     PERFORM UNTIL INDX > MAX-INDX                                        
162900       PERFORM MFS-RENSA-RAD-FAELT-UT                                     
163000     ADD  +1                        TO INDX                               
163100     END-PERFORM                                                          
163200     .                                                                    
163300     SKIP2                                                                
163400                                                                          
163500 MFS-RENSA-RAD-FAELT-UT SECTION.                                          
163600     MOVE MFS-RENSA-FAELT TO MOD-IDDISTR-FOM  (INDX)                      
163700                             MOD-IDDISTR-TOM  (INDX)                      
163800                             MOD-IDKUNDNR-FOM (INDX)                      
163900                             MOD-IDKUNDNR-TOM (INDX)                      
164000                             MOD-KVBEART-KAMP (INDX)                      
164100                             MOD-KVBEART-KUND (INDX)                      
164200                             MOD-KVBEART-REM  (INDX)                      
164300                             MOD-CMD-UPD      (INDX)                      
164400     .                                                                    
164500     SKIP2                                                                
164600                                                                          
164700 MFS-RENSA-FAELT-IN SECTION.                                              
164800     MOVE MFS-RENSA-FAELT TO MOD-IDKAMPRF-IN                              
164900                             MOD-IDDC-IN                                  
165000                             MOD-IDARTNR-IN                               
165100                             MOD-IDDISTR-FOM-UPD                          
165200                             MOD-IDDISTR-TOM-UPD                          
165300                             MOD-IDKUNDNR-FOM-UPD                         
165400                             MOD-IDKUNDNR-TOM-UPD                         
165500                             MOD-KVBEART-KAMP-UPD                         
165700                             MOD-IDKAMPRF-COPY                            
165800                             MOD-IDDC-COPY                                
165900                             MOD-IDARTNR-COPY                             
166000     .                                                                    
166100     EJECT                                                                
166200                                                                          
166300 MFS-ROER-EJ-FAELT-UT SECTION.                                            
166900     MOVE MFS-ROER-EJ-FAELT TO MOD-IDARTNR-UT                             
167100                               MOD-IDKAMPRF-COPY                          
167200                               MOD-IDDC-COPY                              
167300                               MOD-IDARTNR-COPY                           
167400     MOVE +1 TO INDX                                                      
167500     PERFORM UNTIL INDX > MAX-INDX                                        
167600       PERFORM MFS-ROER-EJ-RAD-FAELT-UT                                   
167700       ADD +1 TO INDX                                                     
167800     END-PERFORM                                                          
167900     .                                                                    
168000     SKIP2                                                                
168100                                                                          
168200 MFS-ROER-EJ-RAD-FAELT-UT SECTION.                                        
168300     MOVE MFS-ROER-EJ-FAELT TO MOD-IDDISTR-FOM  (INDX)                    
168400                               MOD-IDDISTR-TOM  (INDX)                    
168500                               MOD-IDKUNDNR-FOM (INDX)                    
168600                               MOD-IDKUNDNR-TOM (INDX)                    
168700                               MOD-KVBEART-KAMP (INDX)                    
168800                               MOD-KVBEART-KUND (INDX)                    
168900                               MOD-KVBEART-REM  (INDX)                    
169000                               MOD-CMD-UPD      (INDX)                    
169100     .                                                                    
169200     SKIP2                                                                
169300                                                                          
169400 MFS-ROER-EJ-FAELT-IN SECTION.                                            
169500     MOVE MFS-ROER-EJ-FAELT TO MOD-IDDISTR-FOM-UPD                        
169600                               MOD-IDDISTR-TOM-UPD                        
169700                               MOD-IDKUNDNR-FOM-UPD                       
169800                               MOD-IDKUNDNR-TOM-UPD                       
169900                               MOD-KVBEART-KAMP-UPD                       
170100                               MOD-IDKAMPRF-COPY                          
170200                               MOD-IDDC-COPY                              
170300                               MOD-IDARTNR-COPY                           
170400     .                                                                    
170500     SKIP2                                                                
170600                                                                          
170700 MFS-LAS-IN-IGEN      SECTION.                                            
170800     MOVE MFS-ADD-LAES-IN-FAELT TO MOD-IDDISTR-FOM-UPD-ATTR               
170900                                   MOD-IDDISTR-TOM-UPD-ATTR               
171000                                   MOD-IDKUNDNR-FOM-UPD-ATTR              
171100                                   MOD-IDKUNDNR-TOM-UPD-ATTR              
171200                                   MOD-KVBEART-KAMP-UPD-ATTR              
171400                                   MOD-IDKAMPRF-COPY-ATTR                 
171500                                   MOD-IDDC-COPY-ATTR                     
171600                                   MOD-IDARTNR-COPY-ATTR                  
171700     .                                                                    
171701                                                                          
171710 MFS-STAENG-FAELT-IN SECTION.                                             
171720       MOVE +1                    TO INDX                                 
171730       PERFORM UNTIL INDX > MAX-INDX                                      
171740         MOVE MFS-STAENG-FAELT    TO MOD-CMD-UPD-ATTR (INDX)              
171750         MOVE MFS-STAENG-FAELT    TO MOD-KVBEART-KAMP-ATTR(INDX)          
171760         ADD 1                    TO INDX                                 
171770       END-PERFORM                                                        
171780      MOVE MFS-STAENG-FAELT      TO MOD-IDDISTR-FOM-UPD-ATTR              
171790                                    MOD-IDDISTR-TOM-UPD-ATTR              
171791                                    MOD-IDKUNDNR-FOM-UPD-ATTR             
171792                                    MOD-IDKUNDNR-TOM-UPD-ATTR             
171793                                    MOD-KVBEART-KAMP-UPD-ATTR             
171794                                    MOD-IDKAMPRF-COPY-ATTR                
171795                                    MOD-IDDC-COPY-ATTR                    
171796                                    MOD-IDARTNR-COPY-ATTR                 
171797     .                                                                    
171798                                                                          
171800     SKIP2                                                                
171900* --- IMS SEKTIONER ---                                                   
172000     SKIP3                                                                
172100                                                                          
172200 IMS-GET-MSG SECTION.                                                     
172300     MOVE '  QC' TO GODK-STATUSKODER                                      
172400     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
172500     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
172600     PERFORM IMS-STATUSKONTROLL                                           
172700     .                                                                    
172800     SKIP3                                                                
172900                                                                          
173000 IMS-INSERT-MSG SECTION.                                                  
173100     IF ENGLISH-TEXT                                                      
173200       MOVE 'N' TO MFS-KDHUVOMR                                           
173300     END-IF                                                               
173400     IF MSGI-IDLAND-SPR = 'GB'                                            
173500        MOVE 'N' TO MFS-KDHUVOMR                                          
173600     END-IF                                                               
173700     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
173800     MOVE SPACE TO GODK-STATUSKODER                                       
173900     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
174000     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
174100     PERFORM IMS-STATUSKONTROLL                                           
174200     .                                                                    
174300     EJECT                                                                
174310 IMS-GU-WDM201 SECTION.                                                   
174320     MOVE 'IMS-GU-WDM201            ' TO CURRENT-IMS-SECTION              
174330                                                                          
174340     STRING 'WDM201  (WDM201KY =' W-WDM201-X ')'                          
174350          DELIMITED BY SIZE INTO SSA1                                     
174360     MOVE '  GE'              TO GODK-STATUSKODER                         
174370     CALL CBLTDLI USING GU WDM2-PCB DLI-IO-WDM201 SSA1                    
174380     MOVE WDM2-STATUS-CODE    TO STATUS-WS                                
174390     PERFORM IMS-STATUSKONTROLL                                           
174391     .                                                                    
174392                                                                          
174393 IMS-GNP-WDM211 SECTION.                                                  
174394     MOVE 'IMS-GNP-WDM211           ' TO CURRENT-IMS-SECTION              
174395                                                                          
174398     STRING 'WDM211  (IDARTNR  =' W-WDM211-X ')'                          
174399          DELIMITED BY SIZE INTO SSA1                                     
174400     MOVE '  GE'              TO GODK-STATUSKODER                         
174401     CALL CBLTDLI USING GNP WDM2-PCB DLI-IO-WDM211 SSA1                   
174402     MOVE WDM2-STATUS-CODE    TO STATUS-WS                                
174403     PERFORM IMS-STATUSKONTROLL                                           
174420     .                                                                    
174430                                                                          
180627 IMS-GU-WDM211 SECTION.                                                   
180628     MOVE 'IMS-GU-WDM211       ' TO CURRENT-IMS-SECTION                   
180629                                                                          
180630     STRING 'WDM201  (WDM201KY =' W-WDM201-X ')'                          
180631          DELIMITED BY SIZE INTO SSA1                                     
180632     STRING 'WDM211  (IDARTNR  =' W-WDM211-X ')'                          
180633          DELIMITED BY SIZE INTO SSA2                                     
180634     MOVE '  GE'              TO GODK-STATUSKODER                         
180635     CALL CBLTDLI USING GU WDM2-PCB DLI-IO-WDM211 SSA1 SSA2               
180636     MOVE WDM2-STATUS-CODE    TO STATUS-WS                                
180637     PERFORM IMS-STATUSKONTROLL                                           
180638     .                                                                    
180639                                                                          
180692 IMS-GNP-INTERVALL-WDM221 SECTION.                                        
180693     MOVE 'IMS-GNP-INTERVALL-WDM221' TO CURRENT-IMS-SECTION               
180694                                                                          
180698     MOVE 'WDM221  '          TO SSA1                                     
180699     MOVE '    GE'            TO GODK-STATUSKODER                         
180700     CALL CBLTDLI USING GNP WDM2-PCB DLI-IO-WDM221 SSA1                   
180701     MOVE WDM2-STATUS-CODE    TO STATUS-WS                                
180702     PERFORM IMS-STATUSKONTROLL                                           
180703     .                                                                    
180704                                                                          
180705 IMS-GNP-WDM221 SECTION.                                                  
180706     MOVE 'IMS-GNP-WDM221             ' TO CURRENT-IMS-SECTION            
180707                                                                          
180708     STRING 'WDM221  (WDM221KY>=' W-WDM221-X                              
180709                    '&WDM221KY<=' W-WDM221-MAX-X ')'                      
180710          DELIMITED BY SIZE INTO SSA1                                     
180711     MOVE '    GE'            TO GODK-STATUSKODER                         
180712     CALL CBLTDLI USING GNP WDM2-PCB DLI-IO-WDM221 SSA1                   
180713     MOVE WDM2-STATUS-CODE    TO STATUS-WS                                
180714     PERFORM IMS-STATUSKONTROLL                                           
180715     .                                                                    
180716                                                                          
180720 IMS-GU-WDM221 SECTION.                                                   
180800     MOVE 'IMS-GU-WDM221       ' TO CURRENT-IMS-SECTION                   
180900                                                                          
181000     STRING 'WDM201  (WDM201KY =' W-WDM201-X ')'                          
181100          DELIMITED BY SIZE INTO SSA1                                     
181200     STRING 'WDM211  (IDARTNR  =' W-WDM211-X ')'                          
181300          DELIMITED BY SIZE INTO SSA2                                     
181400     STRING 'WDM221  (WDM221KY =' W-WDM221-X ')'                          
181500          DELIMITED BY SIZE INTO SSA3                                     
181600     MOVE '  GE' TO GODK-STATUSKODER                                      
181700     CALL CBLTDLI USING GU WDM2-PCB DLI-IO-WDM221 SSA1 SSA2 SSA3          
181800     MOVE WDM2-STATUS-CODE    TO STATUS-WS                                
181900     PERFORM IMS-STATUSKONTROLL                                           
182000     .                                                                    
182100                                                                          
182200 IMS-GHU-WDM221 SECTION.                                                  
182300     MOVE 'IMS-GHU-WDM221      ' TO CURRENT-IMS-SECTION                   
182400                                                                          
182500     STRING 'WDM201  (WDM201KY =' W-WDM201-X ')'                          
182600          DELIMITED BY SIZE INTO SSA1                                     
182700     STRING 'WDM211  (IDARTNR  =' W-WDM211-X ')'                          
182800          DELIMITED BY SIZE INTO SSA2                                     
182801     STRING 'WDM221  (WDM221KY =' W-WDM221-X ')'                          
182802          DELIMITED BY SIZE INTO SSA3                                     
182803     MOVE '  GEGP' TO GODK-STATUSKODER                                    
182804     CALL CBLTDLI USING GHU WDM2-PCB DLI-IO-WDM221 SSA1 SSA2 SSA3         
182805     MOVE WDM2-STATUS-CODE TO STATUS-WS                                   
182806     PERFORM IMS-STATUSKONTROLL                                           
182807     .                                                                    
182808                                                                          
182829 IMS-ISRT-WDM211 SECTION.                                                 
182830     MOVE 'IMS-ISRT-WDM211     ' TO CURRENT-IMS-SECTION                   
182831                                                                          
182832     STRING 'WDM201  (WDM201KY =' W-WDM201-X ')'                          
182833          DELIMITED BY SIZE INTO SSA1                                     
182834     MOVE 'WDM211 '           TO SSA2                                     
182835     MOVE '  II'              TO GODK-STATUSKODER                         
182836     CALL CBLTDLI USING ISRT WDM2-PCB DLI-IO-WDM211 SSA1 SSA2             
182837     MOVE WDM2-STATUS-CODE    TO STATUS-WS                                
182838     PERFORM IMS-STATUSKONTROLL                                           
182839     .                                                                    
182840                                                                          
182841 IMS-ISRT-WDM221 SECTION.                                                 
182842     MOVE 'IMS-ISRT-WDM221     ' TO CURRENT-IMS-SECTION                   
182843                                                                          
182844     STRING 'WDM201  (WDM201KY =' W-WDM201-X ')'                          
182845          DELIMITED BY SIZE INTO SSA1                                     
182846     STRING 'WDM211  (IDARTNR  =' W-WDM211-X ')'                          
182847          DELIMITED BY SIZE INTO SSA2                                     
182848     MOVE 'WDM221  '          TO SSA3                                     
182849     MOVE '  II'              TO GODK-STATUSKODER                         
182850     CALL CBLTDLI USING ISRT WDM2-PCB DLI-IO-WDM221 SSA1                  
182851                                                    SSA2                  
182852                                                    SSA3                  
182853     MOVE WDM2-STATUS-CODE    TO STATUS-WS                                
182854     PERFORM IMS-STATUSKONTROLL                                           
182855     .                                                                    
182856                                                                          
182857 IMS-REPL-WDM221 SECTION.                                                 
182858     MOVE 'IMS-REPL-WDM221     ' TO CURRENT-IMS-SECTION                   
182859                                                                          
182860     MOVE '  '             TO GODK-STATUSKODER                            
182861     CALL CBLTDLI USING REPL WDM2-PCB DLI-IO-WDM221                       
182862     MOVE WDM2-STATUS-CODE TO STATUS-WS                                   
182863     PERFORM IMS-STATUSKONTROLL                                           
182864     .                                                                    
182874                                                                          
182875 IMS-DLET-WDM221 SECTION.                                                 
182876     MOVE 'IMS-DLET-WDM221     ' TO CURRENT-IMS-SECTION                   
182877                                                                          
182878     MOVE '  '             TO GODK-STATUSKODER                            
182879     CALL CBLTDLI USING DLET WDM2-PCB DLI-IO-WDM221                       
182880     MOVE WDM2-STATUS-CODE TO STATUS-WS                                   
182881     PERFORM IMS-STATUSKONTROLL                                           
182882     .                                                                    
182883                                                                          
182896 IMS-GU-WDM211-COPY SECTION.                                              
182897     MOVE 'IMS-GU-WDM211-COPY' TO CURRENT-IMS-SECTION                     
182898                                                                          
182899     STRING 'WDM201  (WDM201KY =' W-WDM201-COPY-X ')'                     
182900          DELIMITED BY SIZE INTO SSA1                                     
182901     STRING 'WDM211  (IDARTNR  =' W-WDM211-COPY-X ')'                     
182902          DELIMITED BY SIZE INTO SSA2                                     
182903     MOVE '  GE'              TO GODK-STATUSKODER                         
182904     CALL CBLTDLI USING GU WDM2-PCB DLI-IO-WDM211-COPY SSA1 SSA2          
182905     MOVE WDM2-STATUS-CODE    TO STATUS-WS                                
182906     PERFORM IMS-STATUSKONTROLL                                           
182907     .                                                                    
182908                                                                          
182909 IMS-GU-WDM221-COPY SECTION.                                              
182910     MOVE 'IMS-GU-WDM221-COPY  ' TO CURRENT-IMS-SECTION                   
182911                                                                          
182912     STRING 'WDM201  (WDM201KY =' W-WDM201-COPY-X ')'                     
182913          DELIMITED BY SIZE INTO SSA1                                     
182920     STRING 'WDM211  (IDARTNR  =' W-WDM211-COPY-X ')'                     
183000          DELIMITED BY SIZE INTO SSA2                                     
183100     STRING 'WDM221  (WDM221KY =' W-WDM221-X ')'                          
183200          DELIMITED BY SIZE INTO SSA3                                     
183300     MOVE '  GE' TO GODK-STATUSKODER                                      
183400     CALL CBLTDLI USING GU WDM2-PCB DLI-IO-WDM221-COPY SSA1               
183410                                                       SSA2               
183420                                                       SSA3               
183500     MOVE WDM2-STATUS-CODE    TO STATUS-WS                                
183600     PERFORM IMS-STATUSKONTROLL                                           
183700     .                                                                    
183800                                                                          
197800 IMS-GNP-WDM221-COPY SECTION.                                             
197900     MOVE 'IMS-GNP-WDM221-COPY'       TO CURRENT-IMS-SECTION              
198000                                                                          
198100     STRING 'WDM211  (IDARTNR  =' W-WDM211-COPY-X ')'                     
198200          DELIMITED BY SIZE INTO SSA1                                     
198401     STRING 'WDM221  (WDM221KY >' W-WDM221-COPY-X ')'                     
198402          DELIMITED BY SIZE INTO SSA2                                     
198403     MOVE '  GE'              TO GODK-STATUSKODER                         
199200     CALL CBLTDLI USING GNP WDM2-PCB DLI-IO-WDM221-COPY SSA1              
199300                                                        SSA2              
199500     MOVE WDM2-STATUS-CODE TO STATUS-WS                                   
199600     PERFORM IMS-STATUSKONTROLL                                           
199700     .                                                                    
199800                                                                          
207300 IMS-ISRT-COPY-WDM221 SECTION.                                            
207400     MOVE 'IMS-ISRT-COPY-WDM221' TO CURRENT-IMS-SECTION                   
207500                                                                          
207600     STRING 'WDM201  (WDM201KY =' W-WDM201-COPY-X ')'                     
207700          DELIMITED BY SIZE INTO SSA1                                     
207800     STRING 'WDM211  (IDARTNR  =' W-WDM211-COPY-X ')'                     
207900          DELIMITED BY SIZE INTO SSA2                                     
208000     MOVE 'WDM221  '          TO SSA3                                     
208100     MOVE '  II' TO GODK-STATUSKODER                                      
208200     CALL CBLTDLI USING ISRT WDM2-PCB DLI-IO-WDM221-COPY SSA1             
208300                                                         SSA2             
208400                                                         SSA3             
208500     MOVE WDM2-STATUS-CODE    TO STATUS-WS                                
208600     PERFORM IMS-STATUSKONTROLL                                           
208700     .                                                                    
208800                                                                          
213800 IMS-STATUSKONTROLL SECTION.                                              
213900     SET STATUS-IX TO 1                                                   
214000     SEARCH GODK-STATUS                                                   
214100       AT END CALL FELLOG                                                 
214200       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                  
214300     END-SEARCH                                                           
214400     .                                                                    
