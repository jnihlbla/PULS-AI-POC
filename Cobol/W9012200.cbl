000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.     W9012200.                                                
000400 AUTHOR.          GUNNEL E.                                               
000500 DATE-WRITTEN.   JUNI  88.                                                
000600                                                                          
000700*************************************************************             
000800*    REMARKS.                                                             
000900*                                                                         
001000*    COPY-TEXTERNA WWGODKAF,                                              
001100*                  WWGODKCB,                                              
001200*                  WWGODIDV                                               
001300*                  FINNS I PROGRAM-SOURCE-BIBLIOTEKET                     
001400*                                                                         
001500*    FUNKTION.                                                            
001600*    KVANTITETS OCH TEXT UPDATERING                                       
001700*   KVANTITETS-UPPDATERING SKER MED HJÄLP AV PROCENTUELL FÖRDELNIN        
001800*   (KVBASLM),  ELLER EN VISS SUMMA PER DISTRIKT (KVBASLMD).              
001900*   OM MARKNADEN HAR PROCENTUELL FÖRDELNING (EX USA), KAN DEN             
002000*   ÅSIDOSÄTTAS VID UPPDATERINGEN.                                        
002100*   JUSTERING TILL DEN PROCENTUELLA FÖRDELNINGEN SKER MED AVRUNDNI        
002200*   UPPÅT                                                                 
002300*************************************************************             
002400*  ÄNDRING AUG -91/C.E.                                                   
002500*  I SEKTION CA-WSECURIT TILLAGT LÄSNING AV XXAP12-TIPROJSTO              
002600*  OCH ARTG11-TISTOMREG. HÄNSYN SKALL TAS TILL MARKNADSSPECIFIKA          
002700*  PROJEKTSTOPP OCH MARKNADSREGISTRERINGSSTOPP.                           
002800*  TILLAGT KONTR-SIFFR PÅ ARTIKELNUMMER.                                  
002900*************************************************************             
003000*  I CA- FINNS INLAGT REMARKS DÄR FIX LÄGGS IN FÖR                        
003100*  OSPÄRRAD TILLGÅNG FÖR KVANTUPPDATERING. (FIND STARTFIX)                
003200*************************************************************             
003300*    INDATA.                                                              
003400*        TRANSAKTION: W9T122                                              
003500*                     ALT W9T121                                          
003600     EJECT                                                                
003700 ENVIRONMENT DIVISION.                                                    
003800     SKIP3                                                                
003900 DATA DIVISION.                                                           
004000     EJECT                                                                
004100 WORKING-STORAGE SECTION.                                                 
004200*    -COPY WY2000W1                                                       
004300     SKIP3                                                                
004400 77  PROGRAM-NAMN                PIC X(08)   VALUE 'W9012200'.            
004500 77  JA                          PIC X       VALUE 'J'.                   
004600 77  NEJ                         PIC X       VALUE 'N'.                   
004700 77  DAGENS-DATUM                PIC S9(7)   VALUE +0 COMP-3.             
004800 77  SPRAK-IX                    PIC S9(9)   VALUE +0   COMP SYNC.        
004900 77  INDX                        PIC S9(9)   VALUE +0   COMP SYNC.        
005000 77  MAX-MOD-LAENGD              PIC S9(4)   VALUE +512 COMP SYNC.        
005100 77  MAX-RAD                     PIC S9(2)   VALUE +13  COMP SYNC.        
005200 77  IDPROJ-WS                   PIC X(4)    VALUE SPACE.                 
005300 77  KDBASLM-WS                  PIC X(6)    VALUE SPACE.                 
005400 77  IDSKYLT-WS                  PIC X(3)    VALUE SPACE.                 
005500 77  IDARTNR-WS                  PIC X(9)    VALUE SPACE.                 
005600 77  IDDISTR-WS                  PIC 9(4)    VALUE ZERO.                  
005700 77  KDBPSR-MIN-WS               PIC 9(1)    VALUE ZERO.                  
005800 77  KDBPSR-MAX-WS               PIC 9(1)    VALUE 9.                     
005900 77  WS-ARTC01-KDPRODSL          PIC S9(3)   VALUE ZERO COMP-3.           
006000 77  WS-ARTG01-TISTOMREG         PIC S9(7)   VALUE ZERO COMP-3.           
006100 77  WS-ARTG11-TISTOMREG         PIC S9(7)   VALUE ZERO COMP-3.           
006200 77  WS-XXAP11-TIPROJSTO         PIC S9(7)   VALUE ZERO COMP-3.           
006300 77  WS-XXAP12-TIPROJSTO         PIC S9(7)   VALUE ZERO COMP-3.           
006400 77  WS-REKSIFFR                 PIC  X(1)   VALUE SPACE.                 
006500 77  KDRESBED-WS                 PIC  X(1)   VALUE SPACE.                 
006600 77  KVBASLM-WS                  PIC S9(7)   VALUE +0.                    
006700 77  KVBASLM-AVRUNDAT-WS         PIC S9(7)   VALUE +0.                    
006800 77  KVBASLM-GAMLA-VARDET-WS     PIC S9(7)   VALUE +0.                    
006900 77  KVBASLKIT-WS                PIC S9(7)   VALUE +0   COMP-3.           
007000 01  KVBASLMD-GRP-WS.                                                     
007100   03  KVBASLMD-WS  OCCURS 6       PIC S9(7) COMP-3.                      
007200                                                                          
007300 01  UPPDAT-FALT.                                                         
007400    03  KVANT-UPPDAT-FALT.                                                
007500       05  UPPDAT-KVBASLM              PIC  X(7) VALUE SPACE.             
007600       05  UPPDAT-RAD  OCCURS 6.                                          
007700          07  UPPDAT-KVBASLMD          PIC  X(7).                         
007800       05  UPPDAT-KVBASLKIT            PIC  X(7) VALUE SPACE.             
007900    03  TEXT-UPPDAT-FALT.                                                 
008000       05  UPPDAT-TEARTNOT-MARK        PIC  X(40) VALUE SPACE.            
008100       05  UPPDAT-KDDEALER             PIC  X(1) VALUE SPACE.             
008200                                                                          
008300 77  INDATA-SW                   PIC X.                                   
008400   88  INDATA-OK                             VALUE 'J'.                   
008500   88  INDATA-FEL                            VALUE 'N'.                   
008600     SKIP3                                                                
008700*01      -COPY WWGODKAF                                                   
008800     SKIP3                                                                
008900*01      -COPY WWPRODSL                                                   
009000     SKIP3                                                                
009100*01      -COPY WWGODKID        -PRE GODK-                                 
009200     SKIP3                                                                
009300*01      -COPY WWKDDEAL        -PRE GODK-                                 
009400     SKIP3                                                                
009500 01  DYNAMISK-SUBMODUL.                                                   
009600       03  WSECURIT                PIC X(8) VALUE 'WSECURIT'.             
009700       03  CBLTDLI                 PIC X(8) VALUE 'CBLTDLI '.             
009800       03  FELLOG                  PIC X(8) VALUE 'FELLOG  '.             
009900       03  W005INIT                PIC X(8) VALUE 'W005INIT'.             
010000     SKIP3                                                                
010100*                       ****   PARAMETRAR TILL W005INIT                   
010200*01      -COPY WMSGINIT                                                   
010300     EJECT                                                                
010400*01      -COPY WSECAREA                                                   
010500     EJECT                                                                
010600 01  NYCKLAR-TILL-DLI.                                                    
010700                                                                          
010800   03  W-WDD7A1KY-MIN.                                                    
010900     05  W-IDARTNR-MIN           PIC S9(9)   COMP-3 VALUE ZERO.           
011000     05  FILLER                  PIC S9(9)   COMP-3 VALUE ZERO.           
011100     05  FILLER                  PIC S9(3)   COMP-3 VALUE ZERO.           
011200                                                                          
011300   03  W-WDD7A1KY-MAX.                                                    
011400     05  W-IDARTNR-MAX           PIC S9(9)   COMP-3 VALUE ZERO.           
011500     05  FILLER            PIC S9(9) COMP-3 VALUE   +999999999.           
011600     05  FILLER            PIC S9(3) COMP-3 VALUE   +999.                 
011700                                                                          
011800   03  W-WDD2D1KY-MIN.                                                    
011900     05  W-IDPROJ-MIN            PIC  X(4)   VALUE SPACE.                 
012000     05  W-KDBASLM-MIN           PIC X(6)    VALUE SPACE.                 
012100     05  W-IDFKNGRP-MIN          PIC S9(5)   COMP-3 VALUE ZERO.           
012200     05  W-IDARTNR               PIC S9(9)   COMP-3 VALUE ZERO.           
012300                                                                          
012400   03  W-WDD2D1KY-MAX.                                                    
012500     05  W-IDPROJ-MAX        PIC  X(4)   VALUE SPACE.                     
012600     05  W-KDBASLM-MAX       PIC X(6)    VALUE SPACE.                     
012700     05  FILLER              PIC S9(5) COMP-3 VALUE +99999.               
012800     05  FILLER              PIC S9(9) COMP-3 VALUE +999999999.           
012900                                                                          
013000   03  W-IDARTNR-X.                                                       
013100      05 WX-IDARTNR            PIC S9(9)  VALUE ZERO COMP-3.              
013200                                                                          
013300   03  W-KDBASLM-X.                                                       
013400     05  W-KDBASLM          PIC  X(6)   VALUE SPACE.                      
013500                                                                          
013600   03  W-KDBPSR-MIN-X.                                                    
013700     05  W-KDBPSR-MIN            PIC S9     VALUE +0 COMP-3.              
013800                                                                          
013900   03  W-KDBPSR-MAX-X.                                                    
014000     05  W-KDBPSR-MAX            PIC S9     VALUE +9 COMP-3.              
014100                                                                          
014200   03  W-IDSKYLT-X.                                                       
014300     05  W-IDSKYLT               PIC  X(3)   VALUE SPACE.                 
014400                                                                          
014500   03  W-KDCLAGER-X.                                                      
014600     05  FILLER                  PIC S9(1)   COMP-3 VALUE +1.             
014700                                                                          
014800   03  W-KDNOTTYP-X.                                                      
014900     05  W-KDNOTTYP              PIC S9      COMP-3 VALUE +6.             
015000                                                                          
015100   03  W-KDSEGKEY-X.                                                      
015200     05  FILLER                  PIC  X(1)   VALUE '1'.                   
015300                                                                          
015400   03  W-1123-KEY-X.                                                      
015500     05  FILLER                  PIC  X(4)   VALUE '1123'.                
015600     05  W-1123-KDPRODSL         PIC S9(3)   COMP-3 VALUE ZERO.           
015700     05  W-1123-IDPROJ           PIC  X(4)   VALUE SPACE.                 
015800     05  FILLER                  PIC  X(20)  VALUE LOW-VALUE.             
015900                                                                          
016000   03  W-1124-KEY-X.                                                      
016100     05  FILLER                  PIC  X(1)   VALUE '1'.                   
016200                                                                          
016300   03  W-1126-KEY-X.                                                      
016400     05  W-1126-KDBASLM          PIC  X(6)   VALUE SPACE.                 
016500     05  FILLER                  PIC  X(09)  VALUE LOW-VALUE.             
016600                                                                          
016700   03  W-IDARTNR-MOTSV-X.                                                 
016800     05  W-IDARTNR-MOTSV         PIC S9(9)   COMP-3 VALUE ZERO.           
016900     EJECT                                                                
017000                                                                          
017100 01  MEDDELANDE.                                                          
017200   03  FEL1.                                                              
017300     05 FILLER                   PIC X(40)                                
017400          VALUE 'ARTIKEL SAKNAS   '.                                      
017500     05 FILLER                   PIC X(40)                                
017600          VALUE 'PARTNO  MISSING           '.                             
017700   03  FILLER REDEFINES FEL1.                                             
017800     05  FEL-1                   PIC X(40)   OCCURS 2.                    
017900   03  FEL2.                                                              
018000     05 FILLER                   PIC X(40)                                
018100          VALUE 'ARTIKEL SAKNAS PÅ MARKNAD'.                              
018200     05 FILLER                   PIC X(40)                                
018300          VALUE 'PARTNO MISSING ON THIS MARKET'.                          
018400   03  FILLER REDEFINES FEL2.                                             
018500     05  FEL-2                   PIC X(40)   OCCURS 2.                    
018600   03  FEL3.                                                              
018700     05 FILLER                   PIC X(40)                                
018800          VALUE 'NYCKLAR FELAKTIGA'.                                      
018900     05 FILLER                   PIC X(40)                                
019000          VALUE 'KEYS NOT OK '.                                           
019100   03  FILLER REDEFINES FEL3.                                             
019200     05  FEL-3                   PIC X(40)   OCCURS 2.                    
019300                                                                          
019400   03  FEL4.                                                              
019500     05 FILLER                   PIC X(40)                                
019600          VALUE 'UPPLYSTA FÄLT FEL'.                                      
019700     05 FILLER                   PIC X(40)                                
019800          VALUE 'HIGHLIGHTED FIELDS WRONG'.                               
019900   03  FILLER REDEFINES FEL4.                                             
020000     05  FEL-4                   PIC X(40)   OCCURS 2.                    
020100                                                                          
020200   03  FEL5.                                                              
020300     05 FILLER                   PIC X(40)                                
020400          VALUE 'TRYCK PF11 FÖR UPPDATERING'.                             
020500     05 FILLER                   PIC X(40)                                
020600          VALUE 'PRESS PF11 FOR UPDATE'.                                  
020700   03  FILLER REDEFINES FEL5.                                             
020800     05  FEL-5                   PIC X(40)   OCCURS 2.                    
020900                                                                          
021000   03  FEL6.                                                              
021100     05 FILLER                   PIC X(40)                                
021200          VALUE 'FÅR EJ BYTA NYCKLAR VID UPPDATERING'.                    
021300     05 FILLER                   PIC X(40)                                
021400          VALUE 'DO NOT CHANGE KEYS WHEN UPDATE'.                         
021500   03  FILLER REDEFINES FEL6.                                             
021600     05  FEL-6                   PIC X(40)   OCCURS 2.                    
021700                                                                          
021800   03  FEL7.                                                              
021900     05 FILLER                   PIC X(40)                                
022000          VALUE 'FYLL I INMATNINGSFÄLT VID UPPDATERING'.                  
022100     05 FILLER                   PIC X(40)                                
022200          VALUE 'FILL IN DATA FOR UPDATE'.                                
022300   03  FILLER REDEFINES FEL7.                                             
022400     05  FEL-7                   PIC X(40)   OCCURS 2.                    
022500                                                                          
022600   03  FEL8.                                                              
022700     05 FILLER                   PIC X(40)                                
022800          VALUE ' UPPDATERING EJ TILLÅTEN '.                              
022900     05 FILLER                   PIC X(40)                                
023000          VALUE ' UPDATE NOT ALLOWED '.                                   
023100   03  FILLER REDEFINES FEL8.                                             
023200     05  FEL-8                   PIC X(40)   OCCURS 2.                    
023300                                                                          
023400   03  FEL9.                                                              
023500     05 FILLER                   PIC X(40)                                
023600          VALUE ' FELAKTIGT PRODUKTSLAG'.                                 
023700     05 FILLER                   PIC X(40)                                
023800          VALUE ' PROD.GROUP  WRONG'.                                     
023900   03  FILLER REDEFINES FEL9.                                             
024000     05  FEL-9                   PIC X(40)   OCCURS 2.                    
024100                                                                          
024200   03  FEL10.                                                             
024300     05 FILLER                   PIC X(40)                                
024400          VALUE ' FELAKTIGT PROJEKT    '.                                 
024500     05 FILLER                   PIC X(40)                                
024600          VALUE ' WRONG PROJECT    '.                                     
024700   03  FILLER REDEFINES FEL10.                                            
024800     05  FEL-10                  PIC X(40)   OCCURS 2.                    
024900                                                                          
025000   03  FEL11.                                                             
025100     05 FILLER                   PIC X(40)                                
025200          VALUE ' UPPDAT SPÄRRAD FÖR DETTA DISTRIKT      '.               
025300     05 FILLER                   PIC X(40)                                
025400          VALUE ' UPDATE IS PROHIBITED FOR THIS DISTRICT '.               
025500   03  FILLER REDEFINES FEL11.                                            
025600     05  FEL-11                  PIC X(40)   OCCURS 2.                    
025700*--------------------------------------                                   
025800   03  MED1.                                                              
025900     05 FILLER                   PIC X(40)                                
026000          VALUE 'UPPDATERING UTFÖRD'.                                     
026100     05 FILLER                   PIC X(40)                                
026200          VALUE 'UPDATE O.K.'.                                            
026300   03  FILLER REDEFINES MED1.                                             
026400     05  MED-1                   PIC X(40)   OCCURS 2.                    
026500   03  MED2.                                                              
026600     05 FILLER                   PIC X(40)                                
026700          VALUE 'MARKNADS-KVANT JUST. EFTER %-FÖRDELN'.                   
026800     05 FILLER                   PIC X(40)                                
026900          VALUE 'MARK.QTY ADJUSTED TO DISTR-%'.                           
027000   03  FILLER REDEFINES MED2.                                             
027100     05  MED-2                   PIC X(40)   OCCURS 2.                    
027200   03  MED3.                                                              
027300     05 FILLER                   PIC X(40)                                
027400          VALUE 'PASSIV ART: FÖRBR.FÖRV. EJ FÖRSTA ÅRET'.                 
027500     05 FILLER                   PIC X(40)                                
027600          VALUE 'PASSIVE ART:NO CONSUMPT.EXPECT.1:ST YEAR'.               
027700   03  FILLER REDEFINES MED3.                                             
027800     05  MED-3                   PIC X(40)   OCCURS 2.                    
027900     EJECT                                                                
028000******************************************************************        
028100*                                                                         
028200*                AREOR FÖR MFS OCH SKÄRMHANTERING                         
028300*                                                                         
028400 01  FILLER                      PIC X(16)   VALUE 'MFS-WS'.              
028500     SKIP3                                                                
028600*01  MID -COPY W9I12101        -PRE 9121-                                 
028700     EJECT                                                                
028800*01  MID -COPY W9I12201                                                   
028900     EJECT                                                                
029000*01  -COPY WMSGAREA                                                       
029100     EJECT                                                                
029200*03  MOD -COPY W9O12201     -RED MSG-AREA.                                
029300     EJECT                                                                
029400*01  -COPY WMFSAREA                                                       
029500     EJECT                                                                
029600******************************************************************        
029700*                                                                         
029800*        ARBETS-AREOR TILL IMS-SEKTIONERNA                                
029900*                                                                         
030000 01  IMS-WS.                                                              
030100   03  FILLER                    PIC X(16)   VALUE 'IMS-WS     '.         
030200     SKIP3                                                                
030300*                        **** STATUS-KOD FRÅN IMS                         
030400   03  STATUS-WS                 PIC XX.                                  
030500     88  SEGMENT-FINNS                       VALUE '  '.                  
030600     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
030700     SKIP3                                                                
030800   03  GODK-STATUSKODER.                                                  
030900     05  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
031000     SKIP3                                                                
031100 01    SSA1                      PIC X(96).                               
031200 01    SSA2                      PIC X(64).                               
031300     EJECT                                                                
031400*                            IMS FUNKTIONSKODER                           
031500*01    -COPY W0003                                                        
031600     EJECT                                                                
031700*                            DLI INPUT-OUTPUT AREA                        
031800 01  DLI-IO-AREA2.                                                        
031900     03  IO-AREA2             PIC X(600) VALUE SPACE.                     
032000     SKIP3                                                                
032100*    03   ARTG01  -COPY WDD201 -RED IO-AREA2 -PRE ARTG01-                 
032200*    EJECT                                                                
032300*    03   ARTG11  -COPY WDD211 -RED IO-AREA2 -PRE ARTG11-                 
032400     EJECT                                                                
032500 01  DLI-IO-AREA.                                                         
032600     03  IO-AREA              PIC X(900) VALUE SPACE.                     
032700     SKIP3                                                                
032800*    03   ARTC01  -COPY WDK601  -RED IO-AREA                              
032900     EJECT                                                                
033000*    03   ARTC11  -COPY WDK611  -RED IO-AREA                              
033100     EJECT                                                                
033200*    03   ARTC22  -COPY WDK625  -RED IO-AREA                              
033300     EJECT                                                                
033400*    03   ERSB01  -COPY WDD7A1  -RED IO-AREA -PRE ERSB01-                 
033500     EJECT                                                                
033600*    03   XXAP01 -COPY WDGX1123 -RED IO-AREA -PRE XXAP01-                 
033700     EJECT                                                                
033800*    03   XXAP11 -COPY WDGX1124 -RED IO-AREA -PRE XXAP11-                 
033900     EJECT                                                                
034000*    03   XXAP12 -COPY WDGX1126 -RED IO-AREA -PRE XXAP12-                 
034100     EJECT                                                                
034200*    03   BENA11  -COPY WDD311  -RED IO-AREA -PRE BENA11-                 
034300     EJECT                                                                
034400*    03   ARTK01  -COPY WDD2D1  -RED IO-AREA -PRE ARTK01-                 
034500     EJECT                                                                
034600 LINKAGE SECTION.                                                         
034700*01  -COPY W0009     -PRE MSG-                                            
034800     EJECT                                                                
034900*01  -COPY W0008     -PRE USEA-                                           
035000     05  FILLER                  PIC X.                                   
035100     EJECT                                                                
035200*01  -COPY W0008     -PRE ARTG-                                           
035300     05  FILLER                  PIC X.                                   
035400     EJECT                                                                
035500*01  -COPY W0008     -PRE ARTC-                                           
035600     05  FILLER                  PIC X.                                   
035700     EJECT                                                                
035800*01  -COPY W0008     -PRE ERSB-                                           
035900     05  FILLER                  PIC X.                                   
036000     EJECT                                                                
036100*01  -COPY W0008     -PRE XXAP-                                           
036200     05  FILLER                  PIC X.                                   
036300     EJECT                                                                
036400*01  -COPY W0008     -PRE BENA-                                           
036500     05  FILLER                  PIC X.                                   
036600     EJECT                                                                
036700*01  -COPY W0008     -PRE ARTK-                                           
036800     05  FILLER                  PIC X.                                   
036900     EJECT                                                                
037000 PROCEDURE DIVISION USING   MSG-PCB USEA-PCB                              
037100                                    ARTG-PCB ARTC-PCB ERSB-PCB            
037200                                    XXAP-PCB BENA-PCB ARTK-PCB.           
037300     ENTRY 'DLITCBL' USING  MSG-PCB USEA-PCB                              
037400                                    ARTG-PCB ARTC-PCB ERSB-PCB            
037500                                    XXAP-PCB BENA-PCB ARTK-PCB.           
037600     PERFORM IMS-GET-MSG                                                  
037700     IF SEGMENT-FINNS                                                     
037800       PERFORM A-INIT                                                     
037900       PERFORM B-KTR-NYCKLAR                                              
038000       IF INDATA-OK                                                       
038100         IF MFS-UPDATE                                                    
038200           PERFORM C-KTR-INMAT-FAELT                                      
038300           IF INDATA-OK                                                   
038400             PERFORM D-UPPDATERA                                          
038500             PERFORM E-LAES-SAMMA                                         
038600             MOVE +02 TO MOD-CURSOR-RAD                                   
038700             MOVE +55 TO MOD-CURSOR-KOL                                   
038800           END-IF                                                         
038900         ELSE                                                             
039000           IF MFS-IDPFK = '7'                                             
039100             PERFORM E-LAES-SAMMA                                         
039200           ELSE                                                           
039300             IF MFS-IDPFK = '8'                                           
039400               IF MID-IDFKNGRP-MIN NUMERIC                                
039500                 MOVE MID-IDFKNGRP-MIN TO  W-IDFKNGRP-MIN                 
039600               ELSE                                                       
039700                 MOVE ZERO TO W-IDFKNGRP-MIN                              
039800               END-IF                                                     
039900               PERFORM F-LAES-NAESTA                                      
040000             ELSE                                                         
040100               PERFORM E-LAES-SAMMA                                       
040200             END-IF                                                       
040300           END-IF                                                         
040400           MOVE +07 TO MOD-CURSOR-RAD                                     
040500           MOVE +74 TO MOD-CURSOR-KOL                                     
040600         END-IF                                                           
040700       END-IF                                                             
040800       MOVE MAX-MOD-LAENGD TO MSG-KVLL                                    
040900       PERFORM IMS-INSERT-MSG                                             
041000     END-IF                                                               
041100                                                                          
041200     MOVE ZERO TO RETURN-CODE                                             
041300     GOBACK                                                               
041400     .                                                                    
041500     EJECT                                                                
041600 A-INIT SECTION.                                                          
041700                                                                          
041800     IF MSG-DUBBLA-TRANSKODER                                             
041900       MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W9I12201                 
042000       MOVE MSG-IDTRANS-2 TO MFS-IDTRANS                                  
042100       MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                                
042200       MOVE MSG-KDTRTYP          TO MFS-KDTRTYP                           
042300       MOVE MSG-IDPFK TO MFS-IDPFK                                        
042400     ELSE                                                                 
042500       MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W9I12201                  
042600       MOVE MSG-IDTRANS-1 TO MFS-IDTRANS                                  
042700       MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                
042800       MOVE SPACE TO MFS-KDTRTYP        MFS-IDPFK                         
042900     END-IF                                                               
043000                                                                          
043100     MOVE ALL '+' TO MSGI-WMSGINIT                                        
043200     MOVE '001'             TO MSGI-KDCALL                                
043300     MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                                
043400     MOVE '9122'            TO MSGI-IDTRANS                               
043500     MOVE MSG-LTERM-NAME    TO MSGI-IDLTERM-USER                          
043600                                                                          
043700     IF MFS-IDTRANS = '9122'                                              
043800       MOVE MID-IDARTNR-IN TO MSGI-IDARTNR                                
043900     END-IF                                                               
044000     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
044100                                                                          
044200     IF MFS-IDTRANS NOT = '9122' AND '0551'                               
044300       MOVE SPACE TO MFS-KDTRTYP                                          
044400       MOVE '7' TO MFS-IDPFK                                              
044500       IF MFS-IDTRANS = '9121'                                            
044600         MOVE MID-W9I12201        TO 9121-MID-W9I12101                    
044700         MOVE 9121-MID-IDPROJ-UT TO MID-IDPROJ-IN                         
044800         MOVE 9121-MID-KDBASLM-UT TO MID-KDBASLM-IN                       
044900         MOVE 9121-MID-IDSKYLT-UT TO MID-IDSKYLT-IN                       
045000         MOVE 9121-MID-KDPRODSL-UT TO MID-KDPRODSL-UT                     
045100         MOVE +1 TO INDX                                                  
045200         PERFORM UNTIL INDX > MAX-RAD                                     
045300           IF 9121-MID-KDSVAR(INDX) NOT = ALL '+'                         
045400             MOVE 9121-MID-IDARTNR(INDX) TO MID-IDARTNR-IN                
045500             MOVE +14 TO INDX                                             
045600           ELSE                                                           
045700             ADD +1 TO INDX                                               
045800           END-IF                                                         
045900         END-PERFORM                                                      
046000         INSPECT MID-IDARTNR-IN REPLACING LEADING SPACE BY ZERO           
046100         IF MID-IDARTNR-IN NOT NUMERIC                                    
046200           MOVE ZERO TO MID-IDARTNR-IN                                    
046300         END-IF                                                           
046400         MOVE 9121-MID-KDBPSR-1-UT TO MID-KDBPSR-MIN                      
046500         MOVE 9121-MID-KDBPSR-2-UT TO MID-KDBPSR-MAX                      
046600       ELSE                                                               
046700         PERFORM MFS-RENSA-MID-NYCKLAR-IN                                 
046800       END-IF                                                             
046900     END-IF                                                               
047000     IF MFS-KDMFSFOR = '2'                                                
047100       MOVE +2 TO SPRAK-IX                                                
047200     ELSE                                                                 
047300       MOVE +1 TO SPRAK-IX                                                
047400     END-IF                                                               
047500     MOVE LOW-VALUE TO MSG-AREA                                           
047600     MOVE 'W9O12201' TO MFS-IDMOD                                         
047700     MOVE '9122' TO MOD-IDTRANS                                           
047800     PERFORM MFS-RENSA-MOD-INPUT-FALT                                     
047900     .                                                                    
048000     EJECT                                                                
048100 B-KTR-NYCKLAR SECTION.                                                   
048200                                                                          
048300     MOVE JA TO INDATA-SW                                                 
048400     PERFORM BA-FIXA-INMATN-KTR                                           
048500                                                                          
048600     IF  MID-IDPROJ-IN   = ALL '+'                                        
048700     AND MID-KDBASLM-IN  = ALL '+'                                        
048800     AND MID-IDSKYLT-IN  = ALL '+'                                        
048900     AND MID-IDARTNR-IN  = ALL '+'                                        
049000       IF UPPDAT-FALT = ALL '+'                                           
049100         CONTINUE                                                         
049200       ELSE                                                               
049300         IF MFS-IDPFK = SPACE AND MFS-KDTRTYP      = SPACE                
049400           MOVE NEJ TO INDATA-SW                                          
049500           MOVE FEL-5(SPRAK-IX) TO MOD-TEMFSFEL                           
049600           PERFORM MFS-ROR-EJ-FAELT-UT                                    
049700           PERFORM MFS-ROR-EJ-FAELT-IN                                    
049800           PERFORM MFS-LAS-IN-IGEN                                        
049900         END-IF                                                           
050000       END-IF                                                             
050100     ELSE                                                                 
050200       IF MFS-UPDATE                                                      
050300         MOVE NEJ TO INDATA-SW                                            
050400         MOVE FEL-6(SPRAK-IX) TO MOD-TEMFSFEL                             
050500         PERFORM MFS-FELHANTERING                                         
050600       END-IF                                                             
050700     END-IF                                                               
050800                                                                          
050900     IF MID-IDPROJ-IN = ALL '+'                                           
051000       MOVE MID-IDPROJ-UT TO    IDPROJ-WS                                 
051100     ELSE                                                                 
051200       MOVE MID-IDPROJ-IN TO IDPROJ-WS                                    
051300       MOVE '7'             TO MFS-IDPFK                                  
051400       MOVE SPACE           TO MFS-KDTRTYP                                
051500     END-IF                                                               
051600                                                                          
051700     IF MID-KDBASLM-IN = ALL '+'                                          
051800       MOVE MID-KDBASLM-UT TO  KDBASLM-WS                                 
051900     ELSE                                                                 
052000       MOVE MID-KDBASLM-IN  TO KDBASLM-WS                                 
052100       MOVE '7'             TO MFS-IDPFK                                  
052200       MOVE SPACE           TO MFS-KDTRTYP                                
052300     END-IF                                                               
052400                                                                          
052500     IF MID-IDSKYLT-IN = ALL '+'                                          
052600       MOVE MID-IDSKYLT-UT TO  IDSKYLT-WS                                 
052700     ELSE                                                                 
052800       MOVE MID-IDSKYLT-IN  TO IDSKYLT-WS                                 
052900       MOVE '7'             TO MFS-IDPFK                                  
053000       MOVE SPACE           TO MFS-KDTRTYP                                
053100     END-IF                                                               
053200                                                                          
053300     MOVE IDSKYLT-WS TO  GODK-IDSKYLT                                     
053400     IF  GODK-IDSKYLT-VAERDEN                                             
053500       CONTINUE                                                           
053600     ELSE                                                                 
053700       IF IDSKYLT-WS = SPACE                                              
053800         IF SPRAK-IX = +2                                                 
053900           MOVE 'GB ' TO IDSKYLT-WS                                       
054000         ELSE                                                             
054100           MOVE 'S  ' TO IDSKYLT-WS                                       
054200         END-IF                                                           
054300       ELSE                                                               
054400         MOVE NEJ TO INDATA-SW                                            
054500         MOVE FEL-3(SPRAK-IX) TO MOD-TEMFSFEL                             
054600         PERFORM MFS-FELHANTERING                                         
054700       END-IF                                                             
054800     END-IF                                                               
054900                                                                          
055000     EJECT                                                                
055100     IF MID-IDARTNR-IN = ALL '+'                                          
055200       MOVE MID-IDARTNR-UT TO  IDARTNR-WS                                 
055300     ELSE                                                                 
055400       MOVE MID-IDARTNR-IN  TO IDARTNR-WS                                 
055500       MOVE '7'             TO MFS-IDPFK                                  
055600       MOVE SPACE           TO MFS-KDTRTYP                                
055700     END-IF                                                               
055800     INSPECT IDARTNR-WS REPLACING LEADING SPACE BY ZERO                   
055900                                                                          
056000     IF MID-KDBPSR-MIN = ALL '+'                                          
056100       MOVE +0 TO KDBPSR-MIN-WS                                           
056200     ELSE                                                                 
056300       MOVE MID-KDBPSR-MIN TO KDBPSR-MIN-WS                               
056400     END-IF                                                               
056500     INSPECT KDBPSR-MIN-WS REPLACING LEADING  SPACE BY ZERO               
056600                                                                          
056700     IF MID-KDBPSR-MAX = ALL '+'                                          
056800       MOVE +9 TO KDBPSR-MAX-WS                                           
056900     ELSE                                                                 
057000       MOVE MID-KDBPSR-MAX TO KDBPSR-MAX-WS                               
057100     END-IF                                                               
057200     INSPECT KDBPSR-MAX-WS REPLACING LEADING SPACE BY ZERO                
057300                                                                          
057400     MOVE IDPROJ-WS       TO MOD-IDPROJ-UT                                
057500     MOVE KDBASLM-WS      TO MOD-KDBASLM-UT                               
057600     MOVE IDSKYLT-WS      TO MOD-IDSKYLT-UT                               
057700     MOVE IDARTNR-WS      TO MOD-IDARTNR-UT                               
057800     INSPECT MOD-IDARTNR-UT REPLACING LEADING ZERO BY SPACE               
057900     MOVE KDBPSR-MIN-WS   TO MOD-KDBPSR-MIN                               
058000     MOVE KDBPSR-MAX-WS   TO MOD-KDBPSR-MAX                               
058100                                                                          
058200     MOVE MID-KDPRODSL-UT TO  MOD-KDPRODSL-UT                             
058300                                                                          
058400     PERFORM MFS-RENSA-MOD-NYCKLAR-IN                                     
058500                                                                          
058600     IF INDATA-OK                                                         
058700       PERFORM BC-FEL-KTR-NYCKLAR                                         
058800     END-IF                                                               
058900                                                                          
059000     .                                                                    
059100     EJECT                                                                
059200 BA-FIXA-INMATN-KTR SECTION.                                              
059300                                                                          
059400* KVANT-UPPDAT                                                            
059500                                                                          
059600     MOVE MID-KVBASLM  TO UPPDAT-KVBASLM                                  
059700     MOVE MID-KVBASLMD(1) TO UPPDAT-KVBASLMD(1)                           
059800     MOVE MID-KVBASLMD(2) TO UPPDAT-KVBASLMD(2)                           
059900     MOVE MID-KVBASLMD(3) TO UPPDAT-KVBASLMD(3)                           
060000     MOVE MID-KVBASLMD(4) TO UPPDAT-KVBASLMD(4)                           
060100     MOVE MID-KVBASLMD(5) TO UPPDAT-KVBASLMD(5)                           
060200     MOVE MID-KVBASLMD(6) TO UPPDAT-KVBASLMD(6)                           
060300     MOVE MID-KVBASLKIT   TO UPPDAT-KVBASLKIT                             
060400                                                                          
060500* TEXT-UPPDAT                                                             
060600                                                                          
060700     MOVE MID-TEARTNOT-MARK TO UPPDAT-TEARTNOT-MARK                       
060800     MOVE MID-KDDEALER TO UPPDAT-KDDEALER                                 
060900                            GODK-KDDEALER                                 
061000     .                                                                    
061100     EJECT                                                                
061200 BC-FEL-KTR-NYCKLAR SECTION.                                              
061300     SKIP2                                                                
061400     IF IDPROJ-WS = SPACE                                                 
061500       MOVE NEJ TO INDATA-SW                                              
061600     END-IF                                                               
061700     SKIP2                                                                
061800     IF KDBASLM-WS = SPACE OR                                             
061900       KDBASLM-WS NUMERIC                                                 
062000       MOVE NEJ TO INDATA-SW                                              
062100     END-IF                                                               
062200     SKIP2                                                                
062300     IF IDARTNR-WS NOT NUMERIC                                            
062400       MOVE NEJ TO INDATA-SW                                              
062500     END-IF                                                               
062600     SKIP2                                                                
062700     IF KDBPSR-MIN-WS NOT NUMERIC                                         
062800       MOVE +0  TO KDBPSR-MIN-WS                                          
062900     END-IF                                                               
063000     SKIP2                                                                
063100     IF KDBPSR-MAX-WS NOT NUMERIC                                         
063200       MOVE +9  TO KDBPSR-MAX-WS                                          
063300     END-IF                                                               
063400     SKIP2                                                                
063500                                                                          
063600     IF INDATA-OK                                                         
063700       MOVE IDPROJ-WS     TO W-1123-IDPROJ                                
063800                             W-IDPROJ-MIN                                 
063900                             W-IDPROJ-MAX                                 
064000       MOVE KDBASLM-WS    TO W-1126-KDBASLM                               
064100                             W-KDBASLM                                    
064200                             W-KDBASLM-MIN                                
064300                             W-KDBASLM-MAX                                
064400                             ATERFORS-MARKNADER                           
064500       MOVE IDSKYLT-WS    TO W-IDSKYLT                                    
064600       MOVE IDARTNR-WS    TO W-IDARTNR                                    
064700                             W-IDARTNR-MIN                                
064800                             W-IDARTNR-MAX                                
064900                             WX-IDARTNR                                   
065000       MOVE KDBPSR-MIN-WS TO W-KDBPSR-MIN                                 
065100       MOVE KDBPSR-MAX-WS TO W-KDBPSR-MAX                                 
065200     ELSE                                                                 
065300       PERFORM MFS-FELHANTERING                                           
065400       MOVE FEL-3(SPRAK-IX) TO MOD-TEMFSFEL                               
065500     END-IF                                                               
065600     .                                                                    
065700     EJECT                                                                
065800 C-KTR-INMAT-FAELT SECTION.                                               
065900                                                                          
066000     IF UPPDAT-FALT  = ALL '+'                                            
066100       MOVE FEL-7(SPRAK-IX) TO MOD-TEMFSFEL                               
066200       PERFORM MFS-ROR-EJ-FAELT-UT                                        
066300       PERFORM MFS-ROR-EJ-FAELT-IN                                        
066400       MOVE NEJ TO INDATA-SW                                              
066500     ELSE                                                                 
066600       PERFORM CA-WSECURIT                                                
066700       IF INDATA-OK                                                       
066800         IF MID-KVBASLM NOT =  ALL '+'                                    
066900           IF MID-KVBASLM NUMERIC                                         
067000             MOVE MFS-NUM-FAELT-RAETT TO MOD-KVBASLM-IN-ATTR              
067100             MOVE MID-KVBASLM TO KVBASLM-WS                               
067200           ELSE                                                           
067300             MOVE MFS-NUM-FAELT-FEL   TO MOD-KVBASLM-IN-ATTR              
067400             MOVE NEJ TO INDATA-SW                                        
067500           END-IF                                                         
067600         END-IF                                                           
067700                                                                          
067800         IF MID-KDDEALER NOT = ALL '+'                                    
067900           IF GODK-KDDEALER-VAERDEN                                       
068000             MOVE MFS-ALFA-FAELT-RAETT TO MOD-KDDEALER-IN-ATTR            
068100           ELSE                                                           
068200             MOVE MFS-ALFA-FAELT-FEL   TO MOD-KDDEALER-IN-ATTR            
068300             MOVE NEJ TO INDATA-SW                                        
068400           END-IF                                                         
068500         END-IF                                                           
068600                                                                          
068700         MOVE +1 TO INDX                                                  
068800         PERFORM UNTIL INDX > 6                                           
068900           IF MID-KVBASLMD(INDX) NOT = ALL '+'                            
069000             IF MID-KVBASLMD(INDX) NUMERIC                                
069100               MOVE MFS-NUM-FAELT-RAETT TO                                
069200                                      MOD-KVBASLMD-IN-ATTR(INDX)          
069300               MOVE MID-KVBASLMD(INDX) TO KVBASLMD-WS(INDX)               
069400             ELSE                                                         
069500               MOVE MFS-NUM-FAELT-FEL TO                                  
069600                                      MOD-KVBASLMD-IN-ATTR(INDX)          
069700               MOVE NEJ TO INDATA-SW                                      
069800             END-IF                                                       
069900           END-IF                                                         
070000           ADD +1 TO INDX                                                 
070100         END-PERFORM                                                      
070200                                                                          
070300         IF MID-KVBASLKIT NOT = ALL '+'                                   
070400           IF MID-KVBASLKIT NUMERIC                                       
070500             MOVE MFS-NUM-FAELT-RAETT TO MOD-KVBASLKIT-IN-ATTR            
070600             MOVE MID-KVBASLKIT       TO KVBASLKIT-WS                     
070700           ELSE                                                           
070800             MOVE MFS-NUM-FAELT-FEL   TO MOD-KVBASLKIT-IN-ATTR            
070900             MOVE NEJ TO INDATA-SW                                        
071000           END-IF                                                         
071100         END-IF                                                           
071200                                                                          
071300         IF MID-TEARTNOT-MARK NOT =  ALL '+'                              
071400           MOVE MFS-ALFA-FAELT-RAETT TO MOD-TEARTNOT-MARK-ATTR            
071500         END-IF                                                           
071600                                                                          
071700         MOVE +1 TO INDX                                                  
071800         IF MID-KVBASLM NOT = ALL '+'                                     
071900           PERFORM UNTIL INDX > 6                                         
072000             IF MID-KVBASLMD(INDX) NOT = ALL '+'                          
072100               MOVE MFS-NUM-FAELT-FEL TO MOD-KVBASLM-IN-ATTR              
072200               MOVE MFS-NUM-FAELT-FEL TO                                  
072300                                     MOD-KVBASLMD-IN-ATTR(INDX)           
072400               MOVE NEJ TO INDATA-SW                                      
072500             END-IF                                                       
072600             ADD +1 TO INDX                                               
072700           END-PERFORM                                                    
072800         END-IF                                                           
072900                                                                          
073000         IF INDATA-FEL                                                    
073100           MOVE FEL-4(SPRAK-IX)   TO MOD-TEMFSFEL                         
073200           PERFORM MFS-ROR-EJ-FAELT-UT                                    
073300           PERFORM MFS-ROR-EJ-FAELT-IN                                    
073400         END-IF                                                           
073500       ELSE                                                               
073600         PERFORM MFS-FELHANTERING                                         
073700       END-IF                                                             
073800     END-IF                                                               
073900     .                                                                    
074000     EJECT                                                                
074100 CA-WSECURIT SECTION.                                                     
074200     SKIP2                                                                
074300     ACCEPT DAGENS-DATUM FROM DATE                                        
074400     MOVE MSG-SIGNON-USERID       TO SEC-IDUSER                           
074500     MOVE '9122'                  TO SEC-IDTRANS                          
074600     PERFORM IMS-GHU-ARTG01                                               
074700     IF SEGMENT-SAKNAS                                                    
074800       MOVE NEJ TO INDATA-SW                                              
074900       MOVE FEL-1(SPRAK-IX) TO MOD-TEMFSFEL                               
075000     ELSE                                                                 
075100       MOVE ARTG01-ART-TISTOMREG TO WS-ARTG01-TISTOMREG                   
075200       PERFORM IMS-GHNP-ARTG11                                            
075300       IF SEGMENT-SAKNAS                                                  
075400         MOVE NEJ TO INDATA-SW                                            
075500         MOVE FEL-2(SPRAK-IX) TO MOD-TEMFSFEL                             
075600       ELSE                                                               
075700         MOVE ARTG11-ART-TISTOMREG  TO WS-ARTG11-TISTOMREG                
075800         MOVE ARTG11-ART-IDDISTR(1) TO IDDISTR-WS                         
075900         MOVE IDDISTR-WS            TO SEC-IDKEY                          
076000         CALL WSECURIT USING SEC-IDUSER                                   
076100                             SEC-IDTRANS                                  
076200                             SEC-IDKEY                                    
076300                             SEC-KDSVAR                                   
076400         IF SEC-KDSVAR = 'F'                                              
076500           MOVE NEJ TO INDATA-SW                                          
076600           MOVE FEL-11(SPRAK-IX) TO MOD-TEMFSFEL                          
076700         ELSE                                                             
076800           PERFORM IMS-GU-ARTC01                                          
076900*          KONTROLL ATT ARTIKELNS PRODUKTSLAG TILLHÖR PV                  
077000           PERFORM S02-KTR-PRODSL                                         
077100           IF INDATA-OK                                                   
077200             PERFORM IMS-GU-XXAP01                                        
077300             IF SEGMENT-FINNS                                             
077400               IF SEC-KDSVAR = SPACE                                      
077500**********        SEC-KDSVAR = SPACE BETYDER 'VISA ALLT'                  
077600                 PERFORM IMS-GNP-XXAP12                                   
077700                 IF SEGMENT-FINNS                                         
077800                   IF XXAP12-1126-TIBASORD > ZERO                         
077900                     MOVE NEJ TO INDATA-SW                                
078000                     MOVE FEL-8(SPRAK-IX) TO MOD-TEMFSFEL                 
078100                   ELSE                                                   
078200                     IF XXAP12-1126-TIMARKORD = ZERO                      
078300                       PERFORM IMS-GNP-XXAP11                             
078400                       MOVE XXAP11-1124-TIGENORD   TO TMP1-YYMMDD         
078500                       MOVE DAGENS-DATUM           TO TMP2-YYMMDD         
078600                       PERFORM WY2000P1                                   
078700                       IF TMP1-YYMMDD < TMP2-YYMMDD                       
078800                         MOVE NEJ TO INDATA-SW                            
078900                         MOVE FEL-8(SPRAK-IX) TO MOD-TEMFSFEL             
079000                       END-IF                                             
079100                     ELSE                                                 
079200                       MOVE XXAP12-1126-TIMARKORD   TO TMP1-YYMMDD        
079300                       MOVE DAGENS-DATUM            TO TMP2-YYMMDD        
079400                       PERFORM WY2000P1                                   
079500                       IF TMP1-YYMMDD < TMP2-YYMMDD                       
079600                         MOVE NEJ TO INDATA-SW                            
079700                         MOVE FEL-8(SPRAK-IX) TO MOD-TEMFSFEL             
079800                       END-IF                                             
079900                     END-IF                                               
080000                   END-IF                                                 
080100                 ELSE                                                     
080200                   MOVE NEJ TO INDATA-SW                                  
080300                   MOVE FEL-2(SPRAK-IX) TO MOD-TEMFSFEL                   
080400                 END-IF                                                   
080500               ELSE                                                       
080600                 IF SEC-KDSVAR NUMERIC                                    
080700*                                      INNEBÄR  'IMPORTÖRSNIVÅ'           
080800                   MOVE WS-ARTC01-KDPRODSL      TO TEST-KDPRODSL          
080900                   IF KDPRODSL-ACC OR                                     
081000                      KDPRODSL-WHEELS                                     
081100                     PERFORM IMS-GNP-XXAP11                               
081200                     MOVE XXAP11-1124-TIPROJSTO TO                        
081300                                          WS-XXAP11-TIPROJSTO             
081400                     PERFORM IMS-GNP-XXAP12                               
081500                     IF SEGMENT-FINNS                                     
081600                       MOVE XXAP12-1126-TIPROJSTO TO                      
081700                                          WS-XXAP12-TIPROJSTO             
081800                     ELSE                                                 
081900                       MOVE +0 TO         WS-XXAP12-TIPROJSTO             
082000                     END-IF                                               
082100                     IF WS-XXAP12-TIPROJSTO > ZERO                        
082200                       MOVE DAGENS-DATUM          TO TMP1-YYMMDD          
082300                       MOVE WS-XXAP12-TIPROJSTO   TO TMP2-YYMMDD          
082400                       PERFORM WY2000P1                                   
082500                       IF TMP1-YYMMDD > TMP2-YYMMDD                       
082600                         MOVE NEJ TO INDATA-SW                            
082700                         MOVE FEL-8(SPRAK-IX) TO MOD-TEMFSFEL             
082800                       END-IF                                             
082900                     ELSE                                                 
083000                       MOVE DAGENS-DATUM          TO TMP1-YYMMDD          
083100                       MOVE WS-XXAP11-TIPROJSTO   TO TMP2-YYMMDD          
083200                       PERFORM WY2000P1                                   
083300                       IF TMP1-YYMMDD > TMP2-YYMMDD                       
083400                         MOVE NEJ TO INDATA-SW                            
083500                         MOVE FEL-8(SPRAK-IX) TO MOD-TEMFSFEL             
083600                       END-IF                                             
083700                     END-IF                                               
083800                   ELSE                                                   
083900                     IF WS-ARTG11-TISTOMREG > ZERO                        
084000                       MOVE DAGENS-DATUM          TO TMP1-YYMMDD          
084100                       MOVE WS-ARTG11-TISTOMREG   TO TMP2-YYMMDD          
084200                       PERFORM WY2000P1                                   
084300                       IF TMP1-YYMMDD > TMP2-YYMMDD                       
084400                         MOVE NEJ TO INDATA-SW                            
084500                         MOVE FEL-8(SPRAK-IX) TO MOD-TEMFSFEL             
084600                       ELSE                                               
084700                         CONTINUE                                         
084800                       END-IF                                             
084900                     ELSE                                                 
085000                       MOVE DAGENS-DATUM          TO TMP1-YYMMDD          
085100                       MOVE WS-ARTG01-TISTOMREG   TO TMP2-YYMMDD          
085200                       PERFORM WY2000P1                                   
085300                       IF TMP1-YYMMDD > TMP2-YYMMDD                       
085400                         MOVE NEJ TO INDATA-SW                            
085500                         MOVE FEL-8(SPRAK-IX) TO MOD-TEMFSFEL             
085600                       ELSE                                               
085700                         CONTINUE                                         
085800                       END-IF                                             
085900                     END-IF                                               
086000                   END-IF                                                 
086100                 ELSE                                                     
086200                   MOVE NEJ TO INDATA-SW                                  
086300                   MOVE FEL-8(SPRAK-IX) TO MOD-TEMFSFEL                   
086400                 END-IF                                                   
086500               END-IF                                                     
086600             ELSE                                                         
086700               MOVE NEJ TO INDATA-SW                                      
086800               MOVE FEL-2(SPRAK-IX) TO MOD-TEMFSFEL                       
086900             END-IF                                                       
087000           ELSE                                                           
087100             MOVE FEL-9(SPRAK-IX) TO MOD-TEMFSFEL                         
087200           END-IF                                                         
087300         END-IF                                                           
087400       END-IF                                                             
087500     END-IF                                                               
087600     .                                                                    
087700     EJECT                                                                
087800 D-UPPDATERA   SECTION.                                                   
087900                                                                          
088000     PERFORM MFS-ROR-EJ-FAELT-UT                                          
088100                                                                          
088200     IF TEXT-UPPDAT-FALT = ALL '+'                                        
088300       PERFORM D01-UPPDAT-KVANT                                           
088400     ELSE                                                                 
088500       IF KVANT-UPPDAT-FALT = ALL '+'                                     
088600         PERFORM D02-UPPDAT-TEXT                                          
088700         PERFORM IMS-REPL-ARTG                                            
088800         MOVE MED-1(SPRAK-IX) TO MOD-TEMFSINF                             
088900       ELSE                                                               
089000         PERFORM D02-UPPDAT-TEXT                                          
089100         PERFORM D01-UPPDAT-KVANT                                         
089200       END-IF                                                             
089300     END-IF                                                               
089400     .                                                                    
089500     EJECT                                                                
089600                                                                          
089700 D01-UPPDAT-KVANT SECTION.                                                
089800                                                                          
089900*===> FLERA DISTRIKT                                                      
090000     IF ARTG11-ART-IDDISTR(2) > ZERO                                      
090100       MOVE +1 TO INDX                                                    
090200       IF MID-KVBASLM NOT = ALL '+'                                       
090300         PERFORM UNTIL INDX > 6                                           
090400           IF ARTG11-ART-IDDISTR(INDX) = ZERO                             
090500             ADD +6 TO INDX                                               
090600           ELSE                                                           
090700              MOVE ZERO TO ARTG11-ART-KVBASLMD(INDX)                      
090800              COMPUTE                                                     
090900              KVBASLMD-WS(INDX) =                                         
091000              KVBASLM-WS * MID-REBLFORD(INDX) / 100 + 0.5                 
091100              COMPUTE                                                     
091200              KVBASLM-AVRUNDAT-WS = KVBASLM-AVRUNDAT-WS +                 
091300                                    KVBASLMD-WS(INDX)                     
091400              ADD +1 TO INDX                                              
091500           END-IF                                                         
091600         END-PERFORM                                                      
091700         IF KVBASLM-AVRUNDAT-WS NOT = KVBASLM-WS                          
091800           MOVE KVBASLM-AVRUNDAT-WS TO KVBASLM-WS                         
091900           MOVE MED-2(SPRAK-IX) TO MOD-TEMFSFEL                           
092000         END-IF                                                           
092100       ELSE                                                               
092200         PERFORM UNTIL INDX > 6                                           
092300           IF ARTG11-ART-IDDISTR(INDX) = ZERO                             
092400             ADD +6 TO INDX                                               
092500           ELSE                                                           
092600             IF MID-KVBASLMD(INDX) NOT = ALL '+'                          
092700               MOVE KVBASLMD-WS(INDX) TO                                  
092800                     ARTG11-ART-KVBASLMD(INDX)                            
092900             ELSE                                                         
093000               MOVE ARTG11-ART-KVBASLMD(INDX) TO                          
093100               KVBASLMD-WS(INDX)                                          
093200             END-IF                                                       
093300             COMPUTE                                                      
093400             KVBASLM-WS =                                                 
093500             KVBASLM-WS + KVBASLMD-WS(INDX)                               
093600             ADD +1 TO INDX                                               
093700           END-IF                                                         
093800         END-PERFORM                                                      
093900       END-IF                                                             
094000     ELSE                                                                 
094100                                                                          
094200*===>  ENDAST 1 DISTRIKT                                                  
094300       IF GODK-ATERFORS-MARK                                              
094400         IF MID-KVBASLKIT NOT = ALL '+'                                   
094500           MOVE KVBASLKIT-WS TO MOD-KVBASLKIT-UT                          
094600                                ARTG11-ART-KVBASLKIT                      
094700           COMPUTE                                                        
094800           KVBASLM-WS = KVBASLKIT-WS * MID-KVBLKIT                        
094900         END-IF                                                           
095000       ELSE                                                               
095100         IF MID-KVBASLMD(1) NOT = ALL '+'                                 
095200            MOVE MID-KVBASLMD(1) TO KVBASLMD-WS(1)                        
095300            MOVE KVBASLMD-WS(1) TO ARTG11-ART-KVBASLMD(1)                 
095400         ELSE                                                             
095500            MOVE ARTG11-ART-KVBASLMD(1) TO KVBASLMD-WS(1)                 
095600         END-IF                                                           
095700         IF MID-KVBASLKIT  NOT = ALL '+'                                  
095800*           MOVE MID-KVBASLKIT TO KVBASLKIT-WS                            
095900            MOVE KVBASLKIT-WS  TO ARTG11-ART-KVBASLKIT                    
096000         ELSE                                                             
096100            MOVE ARTG11-ART-KVBASLKIT TO KVBASLKIT-WS                     
096200         END-IF                                                           
096300         COMPUTE                                                          
096400         KVBASLM-WS = KVBASLMD-WS(1) +                                    
096500         MID-KVBLKIT * KVBASLKIT-WS                                       
096600       END-IF                                                             
096700     END-IF                                                               
096800                                                                          
096900     MOVE DAGENS-DATUM TO ARTG11-ART-TIBASLM                              
097000     MOVE JA           TO ARTG11-ART-FLBLMQ                               
097100     MOVE ARTG11-ART-KVBASLM TO KVBASLM-GAMLA-VARDET-WS                   
097200     MOVE KVBASLM-WS   TO ARTG11-ART-KVBASLM                              
097300                                                                          
097400* REPL ARTG11                                                             
097500     PERFORM IMS-REPL-ARTG                                                
097600     MOVE MED-1(SPRAK-IX) TO MOD-TEMFSINF                                 
097700                                                                          
097800* REPL ARTG01                                                             
097900     IF KVBASLM-GAMLA-VARDET-WS NOT = ARTG11-ART-KVBASLM                  
098000       PERFORM IMS-GHU-ARTG01                                             
098100       COMPUTE                                                            
098200       ARTG01-ART-KVBASL =                                                
098300       ARTG01-ART-KVBASL +                                                
098400       ( KVBASLM-WS - KVBASLM-GAMLA-VARDET-WS )                           
098500       PERFORM IMS-REPL-ARTG                                              
098600     END-IF                                                               
098700                                                                          
098800     .                                                                    
098900     EJECT                                                                
099000                                                                          
099100 D02-UPPDAT-TEXT  SECTION.                                                
099200                                                                          
099300     IF MID-KDDEALER NOT = ALL '+'                                        
099400       MOVE MID-KDDEALER TO ARTG11-ART-KDDEALER                           
099500     END-IF                                                               
099600                                                                          
099700     IF MID-TEARTNOT-MARK NOT = ALL '+'                                   
099800       MOVE MID-TEARTNOT-MARK TO                                          
099900            ARTG11-ART-TEARTNOT-MARK                                      
100000     END-IF                                                               
100100                                                                          
100200     .                                                                    
100300     EJECT                                                                
100400 E-LAES-SAMMA     SECTION.                                                
100500     PERFORM IMS-GHU-ARTG01                                               
100600                                                                          
100700     IF SEGMENT-FINNS                                                     
100800        PERFORM S01-LAES-VISA-INFO                                        
100900     ELSE                                                                 
101000        MOVE FEL-1(SPRAK-IX) TO MOD-TEMFSFEL                              
101100        PERFORM MFS-FELHANTERING                                          
101200     END-IF                                                               
101300     .                                                                    
101400 F-LAES-NAESTA    SECTION.                                                
101500     PERFORM IMS-GN-ARTK01                                                
101600     IF SEGMENT-FINNS                                                     
101700        MOVE ARTK01-SEQD-IDARTNR TO W-IDARTNR                             
101800                                    W-IDARTNR-MIN                         
101900                                    W-IDARTNR-MAX                         
102000                                    WX-IDARTNR                            
102100                                   MOD-IDARTNR-UT                         
102200        INSPECT MOD-IDARTNR-UT REPLACING LEADING ZERO BY SPACE            
102300        PERFORM IMS-GHU-ARTG01                                            
102400        IF SEGMENT-FINNS                                                  
102500           PERFORM S01-LAES-VISA-INFO                                     
102600        ELSE                                                              
102700           MOVE FEL-1(SPRAK-IX) TO MOD-TEMFSFEL                           
102800           PERFORM  MFS-FELHANTERING                                      
102900        END-IF                                                            
103000     ELSE                                                                 
103100        MOVE FEL-1(SPRAK-IX) TO MOD-TEMFSFEL                              
103200        PERFORM MFS-FELHANTERING                                          
103300     END-IF                                                               
103400     EJECT                                                                
103500     .                                                                    
103600 S01-LAES-VISA-INFO SECTION.                                              
103700                                                                          
103800     IF   ARTG01-ART-IDPROJ NOT = IDPROJ-WS                               
103900        MOVE FEL-10(SPRAK-IX) TO MOD-TEMFSFEL                             
104000        MOVE NEJ TO INDATA-SW                                             
104100     ELSE                                                                 
104200        MOVE MFS-FORMATETS-ATTR TO MOD-TEARTNOT-MARK-ATTR                 
104300                                                                          
104400        IF ARTG01-ART-IDARTNR-MOTSV NOT = ZERO                            
104500          MOVE ARTG01-ART-IDARTNR-MOTSV TO MOD-IDARTNR-MOTSV              
104600                                             W-IDARTNR-MOTSV              
104700        END-IF                                                            
104800        MOVE ARTG01-ART-TEARTNOT-BASL TO MOD-TEARTNOT-BASL                
104900        MOVE ARTG01-ART-KVARTVAGN     TO MOD-KVARTVAGN                    
105000        MOVE ARTG01-ART-KDRESBED      TO KDRESBED-WS                      
105100        PERFORM IMS-GHNP-ARTG11                                           
105200        IF SEGMENT-FINNS                                                  
105300            MOVE ARTG11-ART-IDFKNGRP TO MOD-IDFKNGRP-MIN                  
105400            PERFORM IMS-GU-ARTC01                                         
105500            PERFORM S02-KTR-PRODSL                                        
105600            IF INDATA-OK                                                  
105700              MOVE ART-REKSIFFR TO MOD-REKSIFFR-UT                        
105800                                   WS-REKSIFFR                            
105900              MOVE '-'          TO MOD-STRECK-UT                          
106000              MOVE ART-KDPRODSL TO MOD-KDPRODSL                           
106100              MOVE ART-IDFKNGRP TO MOD-IDFKNGRP                           
106200              PERFORM IMS-GET-ARTC11                                      
106300              IF SEGMENT-FINNS                                            
106400                 MOVE CLAG-KDBPSR   TO MOD-KDBPSR                         
106500              END-IF                                                      
106600              PERFORM IMS-GU-XXAP01                                       
106700              IF SEGMENT-FINNS                                            
106800                  PERFORM IMS-GNP-XXAP12                                  
106900                  IF SEGMENT-FINNS                                        
107000                    PERFORM  S01A-KTR-ANTAL-DISTR                         
107100                    MOVE ARTG11-ART-KDDEALER TO MOD-KDDEALER-UT           
107200                    MOVE ARTG11-ART-TEARTNOT-MARK TO                      
107300                                MOD-TEARTNOT-MARK                         
107400                    MOVE ARTG11-ART-TIBASLM   TO MOD-TIBASLM              
107500                    PERFORM S01B-KTR-ERSAETTNING                          
107600                    PERFORM IMS-GET-BENA11                                
107700                    MOVE BENA11-TEXT-BEART TO                             
107800                         MOD-TEXT-BEART                                   
107900                  ELSE                                                    
108000                    MOVE FEL-2(SPRAK-IX) TO MOD-TEMFSFEL                  
108100                    MOVE NEJ TO INDATA-SW                                 
108200                  END-IF                                                  
108300              ELSE                                                        
108400                  MOVE FEL-9(SPRAK-IX) TO MOD-TEMFSFEL                    
108500                  MOVE NEJ TO INDATA-SW                                   
108600              END-IF                                                      
108700            ELSE                                                          
108800              MOVE FEL-9(SPRAK-IX) TO MOD-TEMFSFEL                        
108900            END-IF                                                        
109000        ELSE                                                              
109100          MOVE FEL-2(SPRAK-IX) TO MOD-TEMFSFEL                            
109200          MOVE NEJ TO INDATA-SW                                           
109300        END-IF                                                            
109400        IF INDATA-FEL                                                     
109500          PERFORM MFS-FELHANTERING                                        
109600        END-IF                                                            
109700     END-IF                                                               
109800     .                                                                    
109900     EJECT                                                                
110000 S01A-KTR-ANTAL-DISTR SECTION.                                            
110100     MOVE +1 TO INDX                                                      
110200                                                                          
110300*===> KONTROLL OM FLERA DISTRIKT FINNS                                    
110400     IF ARTG11-ART-IDDISTR(2) > ZERO                                      
110500        MOVE MFS-OEPPNA-NUM-FAELT TO MOD-KVBASLM-IN-ATTR                  
110600                                                                          
110700*===> KONTROLL %-FÖRDELNING ÄR ÅSIDOSATT                                  
110800       PERFORM UNTIL INDX > 6                                             
110900         IF ARTG11-ART-KVBASLMD(INDX) > ZERO                              
111000            MOVE +8 TO INDX                                               
111100         END-IF                                                           
111200         ADD +1 TO INDX                                                   
111300       END-PERFORM                                                        
111400*===>  %-FÖRDELNING ÄR ÅSIDOSATT                                          
111500       IF INDX = +9                                                       
111600         MOVE ARTG11-ART-KVBASLM   TO MOD-KVBASLM-UT                      
111700         MOVE +1 TO INDX                                                  
111800         PERFORM UNTIL INDX > 6                                           
111900           IF ARTG11-ART-IDDISTR(INDX) = ZERO                             
112000             MOVE MFS-RENSA-FAELT TO MOD-IDDISTR(INDX)                    
112100                                     MOD-REBLFORD(INDX)                   
112200                                     MOD-KVBASLMD-UT(INDX)                
112300           ELSE                                                           
112400              MOVE ARTG11-ART-IDDISTR(INDX) TO                            
112500                          MOD-IDDISTR(INDX)                               
112600              MOVE MFS-OEPPNA-NUM-FAELT TO                                
112700              MOD-KVBASLMD-IN-ATTR(INDX)                                  
112800                          MOD-KVBASLMD-UT(INDX)                           
112900              MOVE ARTG11-ART-KVBASLMD(INDX) TO                           
113000                          MOD-KVBASLMD-UT(INDX)                           
113100              INSPECT MOD-KVBASLMD-UT(INDX)                               
113200              REPLACING LEADING ZERO BY SPACE                             
113300              MOVE XXAP12-1126-REBLFORD(INDX) TO                          
113400                           MOD-REBLFORD(INDX)                             
113500           END-IF                                                         
113600           ADD +1 TO INDX                                                 
113700         END-PERFORM                                                      
113800                                                                          
113900*==> %-FÖRDELNINGEN GÄLLER, VÄRDEN RÄKNAS UT VARJE GÅNG                   
114000       ELSE                                                               
114100         MOVE ZERO TO KVBASLM-WS                                          
114200         MOVE +1 TO INDX                                                  
114300         PERFORM UNTIL INDX > 6                                           
114400           IF ARTG11-ART-IDDISTR(INDX) = ZERO                             
114500              MOVE MFS-RENSA-FAELT TO MOD-IDDISTR(INDX)                   
114600                                      MOD-REBLFORD(INDX)                  
114700                                      MOD-KVBASLMD-UT(INDX)               
114800           ELSE                                                           
114900              MOVE ARTG11-ART-IDDISTR(INDX) TO                            
115000                          MOD-IDDISTR(INDX)                               
115100              MOVE XXAP12-1126-REBLFORD(INDX) TO                          
115200                           MOD-REBLFORD(INDX)                             
115300              MOVE MFS-OEPPNA-NUM-FAELT TO                                
115400                             MOD-KVBASLMD-IN-ATTR(INDX)                   
115500             COMPUTE                                                      
115600             KVBASLMD-WS(INDX) = ARTG11-ART-KVBASLM                       
115700                  * XXAP12-1126-REBLFORD(INDX) / 100 + 0.5                
115800             MOVE KVBASLMD-WS(INDX) TO MOD-KVBASLMD-UT(INDX)              
115900             INSPECT MOD-KVBASLMD-UT(INDX)                                
116000             REPLACING LEADING ZERO BY SPACE                              
116100             COMPUTE                                                      
116200             KVBASLM-WS = KVBASLM-WS +                                    
116300                          KVBASLMD-WS(INDX)                               
116400           END-IF                                                         
116500           ADD +1 TO INDX                                                 
116600         END-PERFORM                                                      
116700         MOVE KVBASLM-WS TO MOD-KVBASLM-UT                                
116800       END-IF                                                             
116900     ELSE                                                                 
117000     EJECT                                                                
117100                                                                          
117200                                                                          
117300*===> ENDAST 1 DISTRIKT                                                   
117400                                                                          
117500       MOVE ARTG11-ART-KVBASLKIT TO MOD-KVBASLKIT-UT                      
117600       MOVE XXAP12-1126-KVBLKIT  TO MOD-KVBLKIT                           
117700       IF XXAP12-1126-KVBLKIT > ZERO                                      
117800          MOVE MFS-OEPPNA-NUM-FAELT TO MOD-KVBASLKIT-IN-ATTR              
117900       ELSE                                                               
118000          MOVE MFS-STAENG-FAELT TO MOD-KVBASLKIT-IN-ATTR                  
118100       END-IF                                                             
118200       IF GODK-ATERFORS-MARK                                              
118300         COMPUTE                                                          
118400         KVBASLM-WS = ARTG11-ART-KVBASLKIT *                              
118500                      XXAP12-1126-KVBLKIT                                 
118600         MOVE KVBASLM-WS TO MOD-KVBASLM-UT                                
118700         MOVE XXAP12-1126-IDDISTR(INDX) TO MOD-IDDISTR(INDX)              
118800       ELSE                                                               
118900         MOVE ARTG11-ART-KVBASLMD(1) TO                                   
119000                             MOD-KVBASLMD-UT(1)                           
119100         INSPECT MOD-KVBASLMD-UT(1)                                       
119200             REPLACING LEADING ZERO BY SPACE                              
119300         MOVE MFS-OEPPNA-NUM-FAELT TO                                     
119400                             MOD-KVBASLMD-IN-ATTR(1)                      
119500         COMPUTE                                                          
119600         KVBASLM-WS = ARTG11-ART-KVBASLMD(1) +                            
119700         XXAP12-1126-KVBLKIT * ARTG11-ART-KVBASLKIT                       
119800         MOVE KVBASLM-WS TO MOD-KVBASLM-UT                                
119900         MOVE XXAP12-1126-IDDISTR(INDX) TO MOD-IDDISTR(INDX)              
120000       END-IF                                                             
120100     END-IF                                                               
120200* FIXMOVE MFS-ADD-SAETT-CURSOR TO    MOD-KDDEALER-IN-ATTR FIX TA B        
120300     .                                                                    
120400     EJECT                                                                
120500 S01B-KTR-ERSAETTNING SECTION.                                            
120600     IF W-IDARTNR-MOTSV NOT = ZERO                                        
120700       PERFORM IMS-GU-ARTC11-MOTSV                                        
120800       IF SEGMENT-FINNS                                                   
120900         MOVE WS-REKSIFFR     TO MOD-REKSIFFR-MOTSV                       
121000         MOVE '-'             TO MOD-STRECK-MOTSV                         
121100         IF CLAG-PRARTBTO-EXP NOT = ZERO                                  
121200           MOVE CLAG-PRARTBTO-EXP TO MOD-PRARTBTO-EXP-CP                  
121300         ELSE                                                             
121400           MOVE MFS-RENSA-FAELT  TO MOD-PRARTBTO-EXP-CP                   
121500         END-IF                                                           
121600       END-IF                                                             
121700     END-IF                                                               
121800     PERFORM IMS-GU-ERSB01                                                
121900     IF SEGMENT-FINNS                                                     
122000       IF ERSB01-ERS-IDARTNR NOT = ZERO                                   
122100         MOVE ERSB01-ERS-IDARTNR TO MOD-ERS-IDARTNR                       
122200                                      W-IDARTNR-MOTSV                     
122300         PERFORM IMS-GU-ARTC11-MOTSV                                      
122400         IF SEGMENT-FINNS                                                 
122500           MOVE WS-REKSIFFR     TO MOD-REKSIFFR-ERS                       
122600           MOVE '-'             TO MOD-STRECK-ERS                         
122700           IF CLAG-PRARTBTO-EXP NOT = ZERO                                
122800             MOVE CLAG-PRARTBTO-EXP TO MOD-PRARTBTO-EXP-DEL               
122900           ELSE                                                           
123000             MOVE MFS-RENSA-FAELT  TO MOD-PRARTBTO-EXP-DEL                
123100           END-IF                                                         
123200         ELSE                                                             
123300           MOVE MFS-RENSA-FAELT  TO MOD-PRARTBTO-EXP-DEL                  
123400         END-IF                                                           
123500       ELSE                                                               
123600         MOVE MFS-RENSA-FAELT    TO MOD-ERS-IDARTNR                       
123700                                    MOD-PRARTBTO-EXP-DEL                  
123800       END-IF                                                             
123900     ELSE                                                                 
124000         MOVE MFS-RENSA-FAELT    TO MOD-ERS-IDARTNR                       
124100                                    MOD-PRARTBTO-EXP-DEL                  
124200         MOVE ZERO               TO   W-IDARTNR-MOTSV                     
124300     END-IF                                                               
124400     EJECT                                                                
124500     PERFORM IMS-GU-ARTC01                                                
124600     IF SEGMENT-FINNS                                                     
124700       IF KDRESBED-WS = 'E' OR 'U'                                        
124800           IF ART-KDERS-UTG = ZERO                                        
124900             PERFORM IMS-GET-ARTC11                                       
125000             IF SEGMENT-FINNS                                             
125100               MOVE CLAG-KDERS TO MOD-KDERS                               
125200               IF CLAG-KDUART = 'P'                                       
125300                  MOVE MED-3(SPRAK-IX)  TO MOD-TEMFSINF                   
125400               END-IF                                                     
125500             ELSE                                                         
125600               MOVE ZERO  TO MOD-KDERS                                    
125700             END-IF                                                       
125800           ELSE                                                           
125900             MOVE ART-KDERS-UTG TO MOD-KDERS                              
126000           END-IF                                                         
126100       ELSE                                                               
126200          MOVE ZERO  TO MOD-KDERS                                         
126300       END-IF                                                             
126400       PERFORM IMS-GET-ARTC25                                             
126500       IF SEGMENT-FINNS                                                   
126600         MOVE NOT-TEARTNOT TO MOD-TEARTNOT                                
126700       END-IF                                                             
126800     ELSE                                                                 
126900       MOVE ZERO TO MOD-KDERS                                             
127000     END-IF                                                               
127100     .                                                                    
127200     EJECT                                                                
127300 S02-KTR-PRODSL       SECTION.                                            
127400     MOVE ART-KDPRODSL TO TEST-KDPRODSL                                   
127500                          WS-ARTC01-KDPRODSL                              
127600     IF KDPRODSL-VOLVO-UTAN-EMB                                           
127700        MOVE WC-KDPRODSL-VCC-PARTS                                        
127800                 TO W-1123-KDPRODSL                                       
127900     ELSE                                                                 
128000        MOVE NEJ TO INDATA-SW                                             
128100     END-IF                                                               
128200     .                                                                    
128300     EJECT                                                                
128400 MFS-RENSA-MID-NYCKLAR-IN SECTION.                                        
128500     MOVE MFS-RENSA-FAELT  TO      MID-IDPROJ-IN                          
128600                                   MID-KDBASLM-IN                         
128700                                   MID-IDSKYLT-IN                         
128800                                   MID-IDARTNR-IN                         
128900     .                                                                    
129000     SKIP2                                                                
129100 MFS-RENSA-MOD-NYCKLAR-IN SECTION.                                        
129200     MOVE MFS-RENSA-FAELT  TO      MOD-IDPROJ-IN                          
129300                                   MOD-KDBASLM-IN                         
129400                                   MOD-IDSKYLT-IN                         
129500                                   MOD-IDARTNR-IN                         
129600     .                                                                    
129700     SKIP2                                                                
129800 MFS-RENSA-MOD-INPUT-FALT SECTION.                                        
129900       MOVE MFS-RENSA-FAELT TO MOD-TEMFSFEL MOD-TEMFSINF                  
130000                                 MOD-KVBASLM-IN                           
130100                                 MOD-KDDEALER-IN                          
130200                                 MOD-KVBASLKIT-IN                         
130300                                 MOD-TEARTNOT-MARK                        
130400                                                                          
130500       MOVE +1 TO INDX                                                    
130600       PERFORM UNTIL INDX > 6                                             
130700         MOVE MFS-RENSA-FAELT TO MOD-KVBASLMD-IN(INDX)                    
130800         ADD +1 TO INDX                                                   
130900       END-PERFORM                                                        
131000     .                                                                    
131100     SKIP2                                                                
131200 MFS-FELHANTERING SECTION.                                                
131300                                                                          
131400     MOVE MFS-RENSA-FAELT    TO  MOD-TEXT-BEART                           
131500                                 MOD-KDPRODSL                             
131600                                 MOD-IDFKNGRP                             
131700                                 MOD-KDERS                                
131800                                 MOD-TIBASLM                              
131900                                 MOD-TEARTNOT                             
132000                                 MOD-TEARTNOT-BASL                        
132100                                 MOD-KVARTVAGN                            
132200                                 MOD-ERS-IDARTNR                          
132300                                 MOD-IDARTNR-MOTSV                        
132400                                 MOD-PRARTBTO-EXP-DEL                     
132500                                 MOD-PRARTBTO-EXP-CP                      
132600                                 MOD-KVBASLM-UT                           
132700                                 MOD-KDDEALER-UT                          
132800                                 MOD-KDBPSR                               
132900                                 MOD-KVBLKIT                              
133000                                 MOD-KVBASLKIT-UT                         
133100                                 MOD-TEARTNOT-MARK                        
133200     MOVE +1 TO INDX                                                      
133300     PERFORM UNTIL INDX > 6                                               
133400       MOVE MFS-RENSA-FAELT  TO  MOD-IDDISTR(INDX)                        
133500                                 MOD-REBLFORD(INDX)                       
133600                                 MOD-KVBASLMD-UT(INDX)                    
133700       ADD +1 TO INDX                                                     
133800     END-PERFORM                                                          
133900     PERFORM MFS-FORM-ATTR                                                
134000     .                                                                    
134100     EJECT                                                                
134200 MFS-ROR-EJ-FAELT-UT  SECTION.                                            
134300     MOVE MFS-ROER-EJ-FAELT   TO MOD-IDPROJ-UT                            
134400                                 MOD-KDBASLM-UT                           
134500                                 MOD-IDSKYLT-UT                           
134600                                 MOD-IDARTNR-UT                           
134700                                 MOD-KDPRODSL-UT                          
134800                                 MOD-KDBPSR-MIN                           
134900                                 MOD-KDBPSR-MAX                           
135000                                 MOD-IDFKNGRP-MIN                         
135100                                 MOD-TEXT-BEART                           
135200                                 MOD-KDPRODSL                             
135300                                 MOD-IDFKNGRP                             
135400                                 MOD-KDERS                                
135500                                 MOD-TIBASLM                              
135600                                 MOD-TEARTNOT                             
135700                                 MOD-TEARTNOT-BASL                        
135800                                 MOD-KVARTVAGN                            
135900                                 MOD-ERS-IDARTNR                          
136000                                 MOD-IDARTNR-MOTSV                        
136100                                 MOD-PRARTBTO-EXP-DEL                     
136200                                 MOD-PRARTBTO-EXP-CP                      
136300                                 MOD-KVBASLM-UT                           
136400                                 MOD-KDDEALER-UT                          
136500                                 MOD-KDBPSR                               
136600                                 MOD-KVBLKIT                              
136700                                 MOD-KVBASLKIT-UT                         
136800                                 MOD-TEARTNOT-MARK                        
136900     MOVE +1 TO INDX                                                      
137000     PERFORM UNTIL INDX > 6                                               
137100       MOVE MFS-ROER-EJ-FAELT TO MOD-IDDISTR(INDX)                        
137200                                 MOD-REBLFORD(INDX)                       
137300                                 MOD-KVBASLMD-UT(INDX)                    
137400       ADD +1 TO INDX                                                     
137500     END-PERFORM                                                          
137600     .                                                                    
137700     EJECT                                                                
137800 MFS-ROR-EJ-FAELT-IN  SECTION.                                            
137900     MOVE MFS-ROER-EJ-FAELT   TO MOD-KVBASLM-IN                           
138000                                 MOD-KDDEALER-IN                          
138100                                 MOD-KVBASLKIT-IN                         
138200                                                                          
138300     MOVE +1 TO INDX                                                      
138400     PERFORM UNTIL INDX > 6                                               
138500       MOVE MFS-ROER-EJ-FAELT TO MOD-KVBASLMD-IN(INDX)                    
138600       ADD +1 TO INDX                                                     
138700     END-PERFORM                                                          
138800     .                                                                    
138900     SKIP2                                                                
139000 MFS-LAS-IN-IGEN SECTION.                                                 
139100     MOVE MFS-ADD-LAES-IN-FAELT TO MOD-KVBASLM-IN-ATTR                    
139200                                   MOD-KDDEALER-IN-ATTR                   
139300                                   MOD-KVBASLKIT-IN-ATTR                  
139400                                   MOD-TEARTNOT-MARK-ATTR                 
139500     MOVE +1 TO INDX                                                      
139600     PERFORM UNTIL INDX > 6                                               
139700       MOVE MFS-ADD-LAES-IN-FAELT TO  MOD-KVBASLMD-IN-ATTR(INDX)          
139800       ADD +1 TO INDX                                                     
139900     END-PERFORM                                                          
140000     .                                                                    
140100     SKIP2                                                                
140200 MFS-FORM-ATTR SECTION.                                                   
140300                                                                          
140400     MOVE MFS-FORMATETS-ATTR TO MOD-KVBASLM-UT-ATTR                       
140500                                MOD-KVBASLM-IN-ATTR                       
140600                                MOD-KDDEALER-UT-ATTR                      
140700                                MOD-KDDEALER-IN-ATTR                      
140800                                MOD-KVBASLKIT-UT-ATTR                     
140900                                MOD-KVBASLKIT-IN-ATTR                     
141000                                MOD-TEARTNOT-MARK-ATTR                    
141100     MOVE +1 TO INDX                                                      
141200     PERFORM UNTIL INDX > 6                                               
141300       MOVE MFS-FORMATETS-ATTR TO  MOD-KVBASLMD-IN-ATTR(INDX)             
141400                                   MOD-KVBASLMD-UT-ATTR(INDX)             
141500       ADD +1 TO INDX                                                     
141600     END-PERFORM                                                          
141700     .                                                                    
141800     EJECT                                                                
141900* IMS SEKTIONER                                                           
142000     SKIP3                                                                
142100 IMS-GET-MSG SECTION.                                                     
142200                                                                          
142300     MOVE '  QC' TO GODK-STATUSKODER                                      
142400     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
142500     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
142600     PERFORM IMS-STATUSKONTROLL                                           
142700     SKIP3                                                                
142800     .                                                                    
142900 IMS-INSERT-MSG SECTION.                                                  
143000                                                                          
143100     IF ENGLISH-TEXT                                                      
143200       MOVE 'N' TO MFS-KDHUVOMR                                           
143300     END-IF                                                               
143400     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
143500     MOVE SPACE TO GODK-STATUSKODER                                       
143600     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
143700     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
143800     PERFORM IMS-STATUSKONTROLL                                           
143900     EJECT                                                                
144000                                                                          
144100     .                                                                    
144200 IMS-GHU-ARTG01  SECTION.                                                 
144300                                                                          
144400     STRING 'WLARTG01(IDARTNR  =' W-IDARTNR-X ')'                         
144500          DELIMITED BY SIZE INTO SSA1                                     
144600     MOVE '  GE' TO GODK-STATUSKODER                                      
144700     CALL CBLTDLI USING GHU ARTG-PCB DLI-IO-AREA2    SSA1                 
144800     MOVE ARTG-STATUS-CODE TO STATUS-WS                                   
144900     PERFORM IMS-STATUSKONTROLL                                           
145000     SKIP1                                                                
145100     .                                                                    
145200 IMS-GHNP-ARTG11  SECTION.                                                
145300                                                                          
145400     STRING 'WLARTG11(KDBASLM  =' W-KDBASLM-X ')'                         
145500          DELIMITED BY SIZE INTO SSA1                                     
145600     MOVE '  GE' TO GODK-STATUSKODER                                      
145700     CALL CBLTDLI USING GHNP ARTG-PCB DLI-IO-AREA2 SSA1                   
145800     MOVE ARTG-STATUS-CODE TO STATUS-WS                                   
145900     PERFORM IMS-STATUSKONTROLL                                           
146000     SKIP1                                                                
146100     .                                                                    
146200 IMS-REPL-ARTG          SECTION.                                          
146300                                                                          
146400     MOVE '  ' TO GODK-STATUSKODER                                        
146500     CALL CBLTDLI USING REPL ARTG-PCB DLI-IO-AREA2                        
146600     MOVE ARTG-STATUS-CODE TO STATUS-WS                                   
146700     PERFORM IMS-STATUSKONTROLL                                           
146800     .                                                                    
146900     EJECT                                                                
147000 IMS-GU-ARTC11-MOTSV SECTION.                                             
147100                                                                          
147200     STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-MOTSV-X ')'                   
147300          DELIMITED BY SIZE INTO SSA1                                     
147400     MOVE 'WLARTC11 '         TO SSA2                                     
147500     MOVE '  GE' TO GODK-STATUSKODER                                      
147600     CALL CBLTDLI USING GU ARTC-PCB DLI-IO-AREA    SSA1 SSA2              
147700     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
147800     PERFORM IMS-STATUSKONTROLL                                           
147900     .                                                                    
148000 IMS-GN-ARTK01  SECTION.                                                  
148100                                                                          
148200     STRING 'WLARTK01(WDD2D1KY >' W-WDD2D1KY-MIN                          
148300                    '&WDD2D1KY=<' W-WDD2D1KY-MAX                          
148400                    '&KDBPSR  =>' W-KDBPSR-MIN-X                          
148500                    '&KDBPSR  =<' W-KDBPSR-MAX-X ')'                      
148600          DELIMITED BY SIZE INTO SSA1                                     
148700     MOVE '  GEGB' TO GODK-STATUSKODER                                    
148800     CALL CBLTDLI USING GN ARTK-PCB DLI-IO-AREA    SSA1                   
148900     MOVE ARTK-STATUS-CODE TO STATUS-WS                                   
149000     PERFORM IMS-STATUSKONTROLL                                           
149100     .                                                                    
149200     EJECT                                                                
149300 IMS-GU-ARTC01  SECTION.                                                  
149400                                                                          
149500     STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-X ')'                         
149600          DELIMITED BY SIZE INTO SSA1                                     
149700     MOVE '  GE' TO GODK-STATUSKODER                                      
149800     CALL CBLTDLI USING GU ARTC-PCB DLI-IO-AREA    SSA1                   
149900     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
150000     PERFORM IMS-STATUSKONTROLL                                           
150100     SKIP1                                                                
150200     .                                                                    
150300 IMS-GET-ARTC25  SECTION.                                                 
150400                                                                          
150500     STRING 'WLARTC11*F(KDSEGKEY =' W-KDSEGKEY-X ')'                      
150600          DELIMITED BY SIZE INTO SSA1                                     
150700     STRING 'WLARTC25(KDNOTTYP =' W-KDNOTTYP-X ')'                        
150800          DELIMITED BY SIZE INTO SSA2                                     
150900     MOVE '  GE' TO GODK-STATUSKODER                                      
151000     CALL CBLTDLI USING GNP ARTC-PCB DLI-IO-AREA    SSA1 SSA2             
151100     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
151200     PERFORM IMS-STATUSKONTROLL                                           
151300     SKIP1                                                                
151400     .                                                                    
151500 IMS-GET-ARTC11  SECTION.                                                 
151600                                                                          
151700     STRING 'WLARTC11*F(KDSEGKEY =' W-KDSEGKEY-X ')'                      
151800          DELIMITED BY SIZE INTO SSA1                                     
151900     MOVE '  GE' TO GODK-STATUSKODER                                      
152000     CALL CBLTDLI USING GNP ARTC-PCB DLI-IO-AREA    SSA1                  
152100     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
152200     PERFORM IMS-STATUSKONTROLL                                           
152300     EJECT                                                                
152400     .                                                                    
152500 IMS-GU-ERSB01  SECTION.                                                  
152600                                                                          
152700     STRING 'WLERSB01(WDD7A1KY=>' W-WDD7A1KY-MIN                          
152800                    '&WDD7A1KY=<' W-WDD7A1KY-MAX ')'                      
152900          DELIMITED BY SIZE INTO SSA1                                     
153000     MOVE '  GE' TO GODK-STATUSKODER                                      
153100     CALL CBLTDLI USING GU    ERSB-PCB DLI-IO-AREA  SSA1                  
153200     MOVE ERSB-STATUS-CODE TO STATUS-WS                                   
153300     PERFORM IMS-STATUSKONTROLL                                           
153400     EJECT                                                                
153500     .                                                                    
153600 IMS-GU-XXAP01  SECTION.                                                  
153700                                                                          
153800     STRING 'WLXXAP01(WDGXKEY  =' W-1123-KEY-X ')'                        
153900          DELIMITED BY SIZE INTO SSA1                                     
154000     MOVE '  GE' TO GODK-STATUSKODER                                      
154100     CALL CBLTDLI USING GU    XXAP-PCB DLI-IO-AREA  SSA1                  
154200     MOVE XXAP-STATUS-CODE TO STATUS-WS                                   
154300     PERFORM IMS-STATUSKONTROLL                                           
154400     SKIP1                                                                
154500     .                                                                    
154600 IMS-GNP-XXAP11  SECTION.                                                 
154700                                                                          
154800     MOVE 'WLXXAP11*F' TO SSA1                                            
154900     MOVE '  ' TO GODK-STATUSKODER                                        
155000     CALL CBLTDLI USING GNP XXAP-PCB DLI-IO-AREA    SSA1                  
155100     MOVE XXAP-STATUS-CODE TO STATUS-WS                                   
155200     PERFORM IMS-STATUSKONTROLL                                           
155300     SKIP1                                                                
155400     .                                                                    
155500 IMS-GNP-XXAP12  SECTION.                                                 
155600                                                                          
155700     STRING 'WLXXAP12(WDGXKEY  =' W-1126-KEY-X ')'                        
155800          DELIMITED BY SIZE INTO SSA1                                     
155900     MOVE '  GE' TO GODK-STATUSKODER                                      
156000     CALL CBLTDLI USING GNP XXAP-PCB DLI-IO-AREA    SSA1                  
156100     MOVE XXAP-STATUS-CODE TO STATUS-WS                                   
156200     PERFORM IMS-STATUSKONTROLL                                           
156300     SKIP1                                                                
156400     .                                                                    
156500     EJECT                                                                
156600 IMS-GET-BENA11  SECTION.                                                 
156700                                                                          
156800     STRING 'WLBENA01(WDD3BSEQ =' W-IDARTNR-X ')'                         
156900          DELIMITED BY SIZE INTO SSA1                                     
157000     STRING 'WLBENA11(IDSKYLT  =' W-IDSKYLT ')'                           
157100          DELIMITED BY SIZE INTO SSA2                                     
157200     MOVE '  ' TO GODK-STATUSKODER                                        
157300     CALL CBLTDLI USING GU    BENA-PCB DLI-IO-AREA SSA1 SSA2              
157400     MOVE BENA-STATUS-CODE TO STATUS-WS                                   
157500     PERFORM IMS-STATUSKONTROLL                                           
157600     SKIP1                                                                
157700     .                                                                    
157800     EJECT                                                                
157900 IMS-STATUSKONTROLL SECTION.                                              
158000                                                                          
158100     SET STATUS-IX TO 1                                                   
158200     SEARCH GODK-STATUS AT END CALL FELLOG                                
158300       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                  
158400     END-SEARCH                                                           
158500     .                                                                    
158600     EJECT                                                                
158700*    -COPY WY2000P1                                                       
