000100 ID DIVISION.                                                             
000200                                                                          
000300 PROGRAM-ID.         W2150200.                                            
000400 AUTHOR.             IDK, GÖTEBORG.                                       
000500 DATE-WRITTEN.            1978.                                           
000600     SKIP2                                                                
000700     REMARKS.                                                             
000800*                                                                         
000900*    FUNCTION.                                                            
001000*            PROGRAMMET LÄSER ALLA 1002-SATSER. FÖR VARJE SATS            
001100*            UTFÖRES FÖLJANDE HUVUDFUNKTIONER.                            
001200*            - STÄDNING AV SATS. HEL SATS ELLER INGÅENDE                  
001300*              ARTIKLAR UPPDATERAS. VARNINGAR SKRIVES                     
001400*              TILL ANSKAFFARE                                            
001500*            - STÄDNING AV LEVERANSPLAN FÖR SATS. UPPDATERING,            
001600*              DELETE AV AVROP M.M.                                       
001700*            - EVENTUELL GENERERING AV BEGÄRAN OM ORDERNR FRÅN            
001800*              SATSORDER FÖR ETT AVROP                                    
001900*                                                                         
002000*    960619  TAGIT BORT SAMTLIGA DATABASUPPDATERINGAR OCH SKRIVER         
002100*            ISTÄLLET DESSA PÅ FILEN W21510. UPPDATERINGARNA GÖRS         
002200*            I PROGRAM W21503 (BMP)                                       
002300*                                                                         
002400*                                                                         
002500*    SUBPROGRAM:                                                          
002600*            DATKORT                                                      
002700*            POSTSUM                                                      
002800*            ABEND                                                        
002900     EJECT                                                                
003000 ENVIRONMENT DIVISION.                                                    
003100 INPUT-OUTPUT SECTION.                                                    
003200 FILE-CONTROL.                                                            
003300     SKIP2                                                                
003400*                            *** FIL MED TRANSAR TILL                     
003500*                            *** SATSORDERSYSTEMET                        
003600*                            *** OUTPUT                                   
003700     SELECT W21502 ASSIGN UT-S-W21502D1.                                  
003800*                            *** INFO OM SATSER                ***        
003900*                            *** PRINTAS I W215P002            ***        
004000*                            *** OUTPUT                        ***        
004100     SELECT W21507 ASSIGN UT-S-W21502D2.                                  
004200*                            *** UPPDATERINGSPOSTER                       
004300     SELECT W21510 ASSIGN UT-S-W21502D3.                                  
004400     EJECT                                                                
004500 DATA DIVISION.                                                           
004600 FILE SECTION.                                                            
004700     SKIP3                                                                
004800 FD  W21502                                                               
004900     RECORDING F                                                          
005000     BLOCK 0                                                              
005100     LABEL RECORD STANDARD.                                               
005200*01  POST    -COPY W21502     -PRE W21502- -L.                            
005300     SKIP3                                                                
005400 FD  W21507                                                               
005500     RECORDING V                                                          
005600     BLOCK 0                                                              
005700     LABEL RECORD STANDARD.                                               
005800*01  POST    -COPY W215LI01    -PRE U07LI1- -L.                           
005900*01  POST    -COPY W215LI02    -PRE U07LI2- -L.                           
006000     SKIP3                                                                
006100 FD  W21510                                                               
006200     RECORDING V                                                          
006300     BLOCK 0                                                              
006400     LABEL RECORD STANDARD.                                               
006500*01  W21510-POST      -COPY W215105 -L.                                   
006600     EJECT                                                                
006700 WORKING-STORAGE SECTION.                                                 
006800                                                                          
006900*    -COPY WY2000W1                                                       
007000     SKIP3                                                                
007100*    -COPY WY2000W9                                                       
007200     SKIP3                                                                
007300 01  FELTEXT.                                                             
007400     03  FILLER              PIC X(8)    VALUE 'FELTEXT'.                 
007500     03  FELTEXT-STR         PIC X(72)   VALUE SPACE.                     
007600     SKIP2                                                                
007700 01  RKOD                    PIC S9(4)   VALUE +0    COMP SYNC.           
007800                                                                          
007900 01  KONSTANTER.                                                          
008000     03  JA                  PIC X       VALUE 'J'.                       
008100     03  NEJ                 PIC X       VALUE 'N'.                       
008200     03  WS-FLIART           PIC X       VALUE SPACE.                     
008300                                                                          
008400     03  INGAENDE-ART        PIC X       VALUE SPACE.                     
008500     03  ERSATTNINGSMARKT-ART PIC X      VALUE 'E'.                       
008600     03  UTGAENDE-ART        PIC X       VALUE 'U'.                       
008700     03  ERSATTANDE-ART      PIC X       VALUE 'T'.                       
008800     03  NYTILLKOMMANDE-ART  PIC X       VALUE 'N'.                       
008900                                                                          
008910*01  -COPY WWDCKONS                                                       
008920                                                                          
009000*----------------------------------------- FELMEDDELANDE FÖR              
009100*                                          LISTFIL W21507                 
009200     03  FEL-TEXTER.                                                      
009300         05  FILLER          PIC X(50)   VALUE                            
009400             'INNEHÅLLER ALTERNATIVT ERSATT ARTIKEL'.                     
009500         05  FILLER          PIC X(50)   VALUE                            
009600             'INNEHÅLLER FÄRRE ÄN 2 ARTIKLAR'.                            
009700         05  FILLER          PIC X(50)   VALUE                            
009800             'STANDARDPRIS = 0 FÖR ARTIKEL   '.                           
009900     SKIP1                                                                
010000     03  FEL-MEDDELANDE  REDEFINES FEL-TEXTER.                            
010100         05  FEL-ANM         OCCURS 3                                     
010200                             PIC X(50).                                   
010300     SKIP3                                                                
010400 01  W-ARBETSFALT.                                                        
010500     SKIP1                                                                
010600     03  W-DAGENS-DATUM-AAVV PIC 9(4).                                    
010700     03  W-DATUM REDEFINES W-DAGENS-DATUM-AAVV.                           
010800         05  W-DAGENS-DATUM-AA                                            
010900                             PIC 9(2).                                    
011000         05  W-DAGENS-DATUM-VV                                            
011100                             PIC 9(2).                                    
011200     03  W-BEORDR-VECKO-GRAENS                                            
011300                             PIC S9(4)               COMP-3.              
011400     03  W-VECKO-DIFFERENS   PIC S9(4)               COMP-3.              
011500     03  W-DATUM-TOM         PIC 9(4).                                    
011600     03  W-DATUM-FROM        PIC 9(4).                                    
011700     03  W-DATUM-AAVV        PIC 9(4).                                    
011800     03  W-DAT               REDEFINES W-DATUM-AAVV.                      
011900         05  W-DATUM-AA      PIC 9(2).                                    
012000         05  W-DATUM-VV      PIC 9(2).                                    
012100     03  W-DIFF-AA           PIC S9(3)               COMP-3.              
012200     SKIP1                                                                
012300     03  WS-DAGENS-DATUM-KOLL.                                            
012400         05  WS-AAR          PIC 9(2).                                    
012500         05  WS-MAANAD       PIC 9(2).                                    
012600         05  WS-DAG          PIC 9(2).                                    
012700     03  WS-DAGENS-DATUM-NY REDEFINES WS-DAGENS-DATUM-KOLL                
012800                             PIC 9(6).                                    
012900     03  WS-DAGENS-DATUM     PIC S9(7)               COMP-3.              
013000     03  DAGENS-DATUM        PIC 9(6).                                    
013100     SKIP1                                                                
013200     03  W-ANT-ARTIKLAR-I-SATS                                            
013300                             PIC S9(5)               COMP-3.              
013400     03  W-TRANSTYP.                                                      
013500         05  W-TRANSTYP-N    PIC 9(4).                                    
013600     03  WS-IDARTNR-SATS     PIC S9(9)      VALUE ZERO COMP-3.            
013700     03  WS-IDANSK           PIC S9(3)      VALUE ZERO COMP-3.            
013800     03  WS-IDDISTR          PIC S9(5)      VALUE ZERO COMP-3.            
013900     03  WS-TISTADAT         PIC S9(7)      VALUE ZERO COMP-3.            
014000     03  WS-TISTODAT         PIC S9(7)      VALUE ZERO COMP-3.            
014100     03  WS-KVAVROP          PIC S9(7)      VALUE ZERO COMP-3.            
014200     03  WS-PRARTSTD         PIC S9(7)V9(2) VALUE ZERO COMP-3.            
014300     03  WS-DAAVROP          PIC 9(6).                                    
014400     03  FILLER  REDEFINES WS-DAAVROP.                                    
014500         05  WS-DAAVROP-SS   PIC 9(2).                                    
014600         05  WS-DAAVROP-AAVV PIC 9(4).                                    
014700 01  WS-TIAAVV               PIC 9(04)   VALUE ZERO.                      
014800     SKIP3                                                                
014900 01  W-INDEX.                                                             
015000     03  IX-FEL              PIC S9(9)   VALUE +0    COMP SYNC.           
015100     SKIP3                                                                
015200 01  SWITCHAR.                                                            
015300     03  SW-ARTREGDATA-LAEST-FOR-SATS                                     
015400                             PIC X       VALUE 'N'.                       
015500     03  SW-SATS-FELAKTIG    PIC X       VALUE 'N'.                       
015600     SKIP3                                                                
015700 01  DYNAMISKA-SUBPROGRAM.                                                
015800     03  DATKORT             PIC X(8)    VALUE 'DATKORT'.                 
015900     03  POSTSUM             PIC X(8)    VALUE 'POSTSUM'.                 
016000     03  ABEND               PIC X(8)    VALUE 'ABEND  '.                 
016100     03  CBLTDLI             PIC X(8)    VALUE 'CBLTDLI '.                
016200     03  FELLOG              PIC X(8)    VALUE 'FELLOG  '.                
016300     03  WDATKONV            PIC X(8)    VALUE 'WDATKONV'.                
016400     EJECT                                                                
016500*01  -COPY WDATAREA                                                       
016600     EJECT                                                                
016700*                            *** PARAMETRAR TILL DATKORT                  
016800 01  PROGRAM-NAMN            PIC X(6)    VALUE 'W21502'.                  
016900 01  DATUMKORT-ID            PIC X(6)    VALUE 'WDATUM'.                  
017000*01  -COPY WDATKORT.                                                      
017100*                            *** PARAMETRAR TILL POSTSUM                  
017200*01  -COPY W0005      -PRE POSTSUM-.                                      
017300     EJECT                                                                
017400*    -PRE W21510-    -COPY W0005                                          
017500     EJECT                                                                
017600*                            *************************************        
017700*                            **  AREA FÖR W21502 - LISTFIL      **        
017800*                            **  BEGÄRAN ORDERNR TILL SATSORDER **        
017900*                            *************************************        
018000*01  AREA  -COPY W21502     -PRE U02-.                                    
018100     EJECT                                                                
018200*                            *************************************        
018300*                            **  AREA FÖR W21507 - LISTFIL      **        
018400*                            **  INFO EJ BEORDRADE SATSER       **        
018500*                            *************************************        
018600*01  AREA  -COPY W215LI02   -PRE U07LI2-.                                 
018700     EJECT                                                                
018800*                            *************************************        
018900*                            **  AREA FÖR W21507 - LISTFIL      **        
019000*                            **  INFO OM BEORDRADE SATSER       **        
019100*                            *************************************        
019200*01  AREA  -COPY W215LI01   -PRE U07LI1-  -RED U07LI2-AREA.               
019300     EJECT                                                                
019400*                                                                         
019500*--- AREOR FÖR UPPDATERINGSFILEN W21510                                   
019600*                                                                         
019700*01  AREA -COPY W215100 -PRE UT-100-                                      
019800     EJECT                                                                
019900*01  AREA -COPY W215101 -PRE UT-101-                                      
020000     EJECT                                                                
020100*01  AREA -COPY W215102 -PRE UT-102-                                      
020200     EJECT                                                                
020300*01  AREA -COPY W215103 -PRE UT-103-                                      
020400     EJECT                                                                
020500*01  AREA -COPY W215104 -PRE UT-104-                                      
020600     EJECT                                                                
020700*01  AREA -COPY W215105 -PRE UT-105-                                      
020800     EJECT                                                                
020900*                            *** ARBETS-AREOR TILL IMS-SEKTIONERNA        
021000 01  FILLER                  PIC X(16)   VALUE 'IMS-WS'.                  
021100                                                                          
021200 01  NYCKLAR-TILL-DLI.                                                    
021300   03  W-IDARTNR-SATS-X.                                                  
021400       05  W-IDARTNR-SATS      PIC S9(9)               COMP-3.            
021500   03  W-IDARTNR-ING-X.                                                   
021600       05  W-IDARTNR-ING       PIC S9(9)               COMP-3.            
021700   03  W-WDD901KY-X.                                                      
021710       05  W-IDARTNR-D9        PIC S9(9)               COMP-3.            
021720       05  W-IDDC-D9           PIC  X(2)    VALUE SPACE.                  
021800   03  W-IDLEVNR-X.                                                       
021900       05  W-IDLEVNR           PIC  X(5)    VALUE SPACE.                  
022000   03  W-WDD905KY-X.                                                      
022100       05  W-DAAVROP-AVS-X.                                               
022200           07  W-DAAVROP-AVS   PIC  9(6)    VALUE ZERO.                   
022300       05  W-TILEVDAG-X.                                                  
022400           07  W-TILEVDAG      PIC  S9      VALUE ZERO COMP-3.            
022500   03  W-KDAVROP-X.                                                       
022600       05  W-KDAVROP           PIC S9(1)               COMP-3.            
022700   03  W-IDLEVNR-STR-X.                                                   
022800       05  W-IDLEVNR-STR       PIC  X(5)    VALUE SPACE.                  
023500   03  W-WDJ1CSEQ-X.                                                      
023600       05  W-IDLEVNR-S         PIC  X(5)  VALUE SPACE.                    
023700       05  W-BELEVART-S        PIC X(30).                                 
023800       05  W-IDARTNR-S         PIC S9(9)               COMP-3.            
023900     SKIP3                                                                
024000*                            *** STATUSKOD FRÅN IMS                       
024100   03    STATUS-WS       PIC XX.                                          
024200     88  SEGMENT-FINNS               VALUE '  '.                          
024300     88  SEGMENT-UPPLAGT             VALUE '  '.                          
024400     88  SEGMENT-SAKNAS              VALUE 'GE'.                          
024500     88  BASEN-SLUT                  VALUE 'GB'.                          
024600     SKIP3                                                                
024700   03    SSA1            PIC X(96).                                       
024800   03    SSA2            PIC X(60).                                       
024900   03    SSA3            PIC X(60).                                       
025200     SKIP3                                                                
025300   03    GODK-STATUSKODER.                                                
025400     05  GODK-STATUS OCCURS 4 INDEXED BY STATUS-IX PIC XX.                
025500     SKIP3                                                                
025600*                            *** IMS FUNKTIONSKODER                       
025700*01      -COPY W0003                                                      
025800     EJECT                                                                
025900*                            *** DLI INPUT-OUTPUT AREA                    
026000 01  DLI-IO-AREA-01      PIC X(200)  VALUE SPACE.                         
026100     SKIP3                                                                
026200*01  WLARTC01  -COPY WDK601  -RED DLI-IO-AREA-01.                         
026300     EJECT                                                                
026400 01  DLI-IO-AREA-11      PIC X(900)  VALUE SPACE.                         
026500     SKIP3                                                                
026600*01  WLARTC11  -COPY WDK611  -RED DLI-IO-AREA-11.                         
026700     EJECT                                                                
026800 01  DLI-IO-AREA         PIC X(400)  VALUE SPACE.                         
026900     SKIP3                                                                
027000*01  WLSATB01 -COPY WDJ101   -PRE SATB-     -RED DLI-IO-AREA.             
027100     EJECT                                                                
027200*01  WLSATB11 -COPY WDJ111   -PRE SATB-     -RED DLI-IO-AREA.             
027300     EJECT                                                                
027400 01  WDJ1CSEQ REDEFINES DLI-IO-AREA.                                      
027500*    03  WLSATB11  -COPY WDJ111  -PRE SATE-                               
027600*    03  WLSATB01  -COPY WDJ101  -PRE SATE-                               
027700     EJECT                                                                
027800*01  WLINLB23  -COPY WDD905  -PRE AVROP-   -RED DLI-IO-AREA.              
027900     EJECT                                                                
028000*01  WLINLB32  -COPY WDD907  -PRE BEORDR-  -RED DLI-IO-AREA.              
028100     EJECT                                                                
029000 LINKAGE SECTION.                                                         
029100     SKIP3                                                                
029200*01  -COPY W0008  -PRE SATB-.                                             
029300        05  FILLER           PIC X.                                       
029400     EJECT                                                                
029500*01  -COPY W0008  -PRE ARTC-.                                             
029600        05  FILLER           PIC X.                                       
029700     EJECT                                                                
030100*01  -COPY W0008  -PRE SATE-.                                             
030200        05  FILLER           PIC X.                                       
030300     EJECT                                                                
030700*01  -COPY W0008  -PRE SATB2-.                                            
030800        05  FILLER           PIC X.                                       
030900     EJECT                                                                
031000*01  -COPY W0008  -PRE INLB-.                                             
031100        05  FILLER           PIC X.                                       
031200     EJECT                                                                
031300 PROCEDURE DIVISION USING SATB-PCB ARTC-PCB  SATE-PCB                     
031400                          SATB2-PCB INLB-PCB.                             
031500     ENTRY 'DLITCBL' USING SATB-PCB ARTC-PCB SATE-PCB                     
031600                           SATB2-PCB INLB-PCB.                            
031700     PERFORM A-INITIERA                                                   
031800                                                                          
031900     PERFORM S01-LAES-NY-SATS                                             
032000                                                                          
032100     PERFORM UNTIL SEGMENT-SAKNAS OR BASEN-SLUT                           
032200       IF SATB-STR-IDARTNR < 100000000                                    
032300         IF SATB-STR-TIBORT = ZERO                                        
032400           MOVE SATB-STR-IDARTNR TO WS-IDARTNR-SATS                       
032500           PERFORM B-STAEDA-SATS-STRUKTUR                                 
032600           PERFORM C-STAED-LEVPL-SKAPA-SATS-TRANS                         
032700         END-IF                                                           
032800       END-IF                                                             
032900       PERFORM S01-LAES-NY-SATS                                           
033000     END-PERFORM                                                          
033100                                                                          
033200     PERFORM Z-FINIT                                                      
033300                                                                          
033400     MOVE ZERO TO RETURN-CODE                                             
033500     GOBACK                                                               
033600     .                                                                    
033700     EJECT                                                                
033800 A-INITIERA SECTION.                                                      
033900******************************************************************        
034000*                                                                *        
034100*    LÄSNING AV DATUMKORT                                        *        
034200*                                                                *        
034300******************************************************************        
034400     SKIP1                                                                
034500     OPEN OUTPUT W21502                                                   
034600                 W21507                                                   
034700                 W21510                                                   
034800                                                                          
034900     MOVE 'W21502'   TO POSTSUM-PROGNAMN                                  
035000     MOVE 'W21510'   TO W21510-PROGNAMN                                   
035100                        W21510-FDNAMN                                     
035200     MOVE 'W21502D3' TO W21510-DDNAMN2                                    
035300                                                                          
035400     ACCEPT DAGENS-DATUM FROM DATE                                        
035500                                                                          
035600     MOVE DAGENS-DATUM TO DAT-I-TIDATUM                                   
035700     MOVE 'AAMMDD'     TO DAT-KDDATFORM                                   
035800     CALL WDATKONV USING DAT-KDDATFORM                                    
035900                         DAT-I-TIDATUM                                    
036000                         DAT-O-TIDATUM                                    
036100                         DAT-KDSVAR                                       
036200     IF DAT-KDSVAR-OK                                                     
036300       MOVE DAT-TIAA       TO W-DAGENS-DATUM-AA                           
036400       MOVE DAT-TIVV       TO W-DAGENS-DATUM-VV                           
036500       MOVE DAT-TIAA       TO WS-AAR                                      
036600       MOVE DAT-TIMM       TO WS-MAANAD                                   
036700       MOVE DAT-TIDD       TO WS-DAG                                      
036800       MOVE WS-DAGENS-DATUM-NY TO WS-DAGENS-DATUM                         
036900     END-IF                                                               
037000                                                                          
037100***  CALL DATKORT USING PROGRAM-NAMN DATUMKORT-ID DATUMKORT               
037200***  MOVE D-AAR              TO W-DAGENS-DATUM-AA                         
037300***  MOVE D-VECKA            TO W-DAGENS-DATUM-VV                         
037400***  MOVE D-AAR              TO WS-AAR                                    
037500***  MOVE D-MAANAD           TO WS-MAANAD                                 
037600***  MOVE D-DAG              TO WS-DAG                                    
037700***  MOVE WS-DAGENS-DATUM-NY TO WS-DAGENS-DATUM                           
037800     .                                                                    
037900     EJECT                                                                
038000 B-STAEDA-SATS-STRUKTUR SECTION.                                          
038100******************************************************************        
038200*                                                                *        
038300*    ALLA ARTIKLAR LÄSES I SATSSTRUKTUREN.                       *        
038400*    STÄDNING SKER BEROENDE PÅ VÄRDET AV ÄNDRINGSKODEN           *        
038500*    (KDISATS).                                                  *        
038600*                                                                *        
038700******************************************************************        
038800                                                                          
038900     MOVE ZERO TO W-ANT-ARTIKLAR-I-SATS                                   
039000                                                                          
039100     MOVE WS-IDARTNR-SATS TO W-IDARTNR-SATS                               
039200     PERFORM IMS-GET-ING-ARTIKEL-SEG                                      
039300                                                                          
039400     PERFORM UNTIL SEGMENT-SAKNAS                                         
039500                                                                          
039600       MOVE SATB-RAD-TISTADAT TO WS-TISTADAT                              
039700       MOVE SATB-RAD-TISTODAT TO WS-TISTODAT                              
039800                                                                          
039900       MOVE WS-DAGENS-DATUM TO TMP1-YYMMDD                                
040000       MOVE WS-TISTODAT     TO TMP2-YYMMDD                                
040100       PERFORM WY2000P1                                                   
040200       IF  SATB-RAD-KDISATS = UTGAENDE-ART                                
040300       AND TMP1-YYMMDD     NOT < TMP2-YYMMDD                              
040400                                                                          
040500         MOVE SATB-RAD-IDARTNR TO W-IDARTNR-ING                           
040600         MOVE INGAENDE-ART     TO UT-101-KDISATS                          
040700*                                                                         
040800*--- SKRIV POST PÅ W21510 FÖR REPL PÅ SATB11                              
040900*                                                                         
041000         PERFORM S11-SKRIV-W21510-101                                     
041100*                                                                         
041200*--- SKRIV POST PÅ W21510 FÖR ISRT PÅ XXBY11                              
041300*                                                                         
041400         PERFORM S13-SKRIV-W21510-103                                     
041500                                                                          
041600         PERFORM BA-KONTROLLERA-FLIART                                    
041700         MOVE WS-IDARTNR-SATS TO W-IDARTNR-SATS                           
041800*                                                                         
041900*--- SKRIV POST PÅ W21510 FÖR REPL PÅ SATB01                              
042000*                                                                         
042100         PERFORM S10-SKRIV-W21510-100                                     
042200       ELSE                                                               
042300         MOVE WS-DAGENS-DATUM TO TMP1-YYMMDD                              
042400         MOVE WS-TISTADAT     TO TMP2-YYMMDD                              
042500         PERFORM WY2000P1                                                 
042600         IF  SATB-RAD-KDISATS = NYTILLKOMMANDE-ART                        
042700         AND TMP1-YYMMDD     >=  TMP2-YYMMDD                              
042800                                                                          
042900           MOVE SATB-RAD-IDARTNR   TO W-IDARTNR-ING                       
043000           PERFORM S11-SKRIV-W21510-101                                   
043100*                                                                         
043200*--- SKRIV POST PÅ W21510 FÖR ISRT PÅ XXBY11                              
043300*                                                                         
043400           PERFORM S13-SKRIV-W21510-103                                   
043500           MOVE WS-IDARTNR-SATS TO W-IDARTNR-SATS                         
043600*                                                                         
043700*--- SKRIV POST PÅ W21510 FÖR REPL PÅ SATB01                              
043800*                                                                         
043900           PERFORM S10-SKRIV-W21510-100                                   
044000         END-IF                                                           
044100       END-IF                                                             
044200                                                                          
044300       MOVE WS-TISTADAT      TO TMP1-YYMMDD                               
044400       MOVE WS-TISTODAT      TO TMP2-YYMMDD                               
044500       MOVE WS-DAGENS-DATUM  TO TMP3-YYMMDD                               
044600       PERFORM WY2000Q1                                                   
044700       IF  TMP1-YYMMDD <= TMP3-YYMMDD                                     
044800       AND TMP2-YYMMDD >  TMP3-YYMMDD                                     
044900         ADD 1 TO W-ANT-ARTIKLAR-I-SATS                                   
045000       END-IF                                                             
045100                                                                          
045200       PERFORM IMS-GET-ING-ARTIKEL-SEG                                    
045300                                                                          
045400     END-PERFORM                                                          
045500     .                                                                    
045600     EJECT                                                                
045700 BA-KONTROLLERA-FLIART SECTION.                                           
045800                                                                          
045900     MOVE SATB-RAD-IDARTNR TO W-IDARTNR-SATS                              
046000                              W-IDARTNR-ING                               
046100                              W-IDARTNR-S                                 
046200     MOVE SPACE            TO W-BELEVART-S                                
046300     MOVE SPACE            TO W-IDLEVNR-S                                 
046400     MOVE NEJ TO WS-FLIART                                                
046500     PERFORM IMS-GET-SATE-CSEQ-FIRST                                      
046600     PERFORM UNTIL SEGMENT-SAKNAS OR WS-FLIART = JA                       
046700       MOVE WS-DAGENS-DATUM   TO TMP1-YYMMDD                              
046800       MOVE SATE-RAD-TISTODAT TO TMP2-YYMMDD                              
046900       PERFORM WY2000P1                                                   
047000       IF  SATE-STR-TIBORT = 0                                            
047100       AND SATE-STR-IDARTNR < 100000000                                   
047200       AND TMP1-YYMMDD < TMP2-YYMMDD                                      
047300         MOVE JA TO WS-FLIART                                             
047400       END-IF                                                             
047500       PERFORM IMS-GET-SATE-CSEQ-NEXT                                     
047600     END-PERFORM                                                          
047700                                                                          
047800     PERFORM IMS-GET-ARTC01                                               
047900     IF SEGMENT-FINNS                                                     
048000       IF WS-FLIART = ART-FLIART                                          
048100         CONTINUE                                                         
048200       ELSE                                                               
048300         MOVE WS-FLIART TO UT-102-FLIART                                  
048400*                                                                         
048500*--- SKRIV POST PÅ W21510 FÖR REPL PÅ ARTC01                              
048600*                                                                         
048700         PERFORM S12-SKRIV-W21510-102                                     
048800       END-IF                                                             
048900     END-IF                                                               
049000     .                                                                    
049100     EJECT                                                                
049200 C-STAED-LEVPL-SKAPA-SATS-TRANS SECTION.                                  
049300******************************************************************        
049400*                                                                *        
049500*    SATSENS AVROP I LEVERANSPLANEN LÄSES FRÅN BÖRJAN.           *        
049600*    FÖR AVROP SOM UPPFYLLER BEORDRINGSVILLKOR SKAPAS            *        
049700*    TRANS TILL SATSORDERSYSTEMET.                               *        
049800*                                                                *        
049900*    SATSORDER INNEBÄR      - TRANS TILL SATSORDERSYSTEMET, BE-  *        
050000*                             GÄRAN OM ORDERNUMMER (W21502)      *        
050100*                           - ORDERINFO TILL ANSKAFFARE (W21507) *        
050300*                                                                *        
050400******************************************************************        
050500     PERFORM S02-LAES-ARTREG-SATS                                         
050600                                                                          
050700     MOVE NEJ TO SW-SATS-FELAKTIG                                         
050800     MOVE '1002 ' TO W-IDLEVNR                                            
050900     MOVE 2       TO W-KDAVROP                                            
050910                                                                          
050920     MOVE W-IDARTNR-SATS TO W-IDARTNR-D9                                  
050930     MOVE WC-CDC-SE      TO W-IDDC-D9                                     
051000     PERFORM IMS-GET-AVROP-SEG-FIRST                                      
051100*                                                                         
051200*  ENDAST GÄLLANDE(GODKÄNDA) AVROP SKALL BEORDRAS, ÖVR. SKIPPAS           
051300*                                                                         
051400     PERFORM UNTIL SEGMENT-SAKNAS OR SW-SATS-FELAKTIG = JA                
051500                                                                          
051600       MOVE AVROP-DAAVROP-AVS TO W-DAAVROP-AVS                            
051700       IF AVROP-KDAVROP = 2                                               
051800         MOVE W-DAGENS-DATUM-AAVV TO W-DATUM-FROM                         
051900         MOVE AVROP-TIAVRDAT-DISP TO DAT-I-TIDATUM                        
052000         MOVE 'AAMMDD'            TO DAT-KDDATFORM                        
052100         CALL WDATKONV USING         DAT-KDDATFORM                        
052200                                     DAT-I-TIDATUM                        
052300                                     DAT-O-TIDATUM                        
052400                                     DAT-KDSVAR                           
052500         IF DAT-KDSVAR-FEL                                                
052600           MOVE 'FEL VID ANROP TILL DATKONV 2' TO FELTEXT                 
052700           CALL FELLOG                                                    
052800         ELSE                                                             
052900           MOVE DAT-TIAAVV-GRP    TO WS-TIAAVV                            
053000           MOVE WS-TIAAVV         TO W-DATUM-TOM                          
053100         END-IF                                                           
053200         PERFORM S06-BERAKNA-VECKODIFFERENS                               
053300                                                                          
053400         IF  W-VECKO-DIFFERENS <= W-BEORDR-VECKO-GRAENS                   
053500           MOVE AVROP-KVAVROP     TO WS-KVAVROP                           
053600           PERFORM IMS-GNP-SATSBEORDR                                     
053700           IF SEGMENT-SAKNAS                                              
053800                                                                          
053900             IF W-ANT-ARTIKLAR-I-SATS  < 2                                
054000               MOVE JA TO SW-SATS-FELAKTIG                                
054100               MOVE 2  TO IX-FEL                                          
054200               PERFORM S03-RED-SKRIV-LISTPOST-W21507                      
054300             ELSE                                                         
054400               IF WS-PRARTSTD > 0                                         
054410                 MOVE W-IDARTNR-SATS TO W-IDARTNR-D9                      
054420                 MOVE WC-CDC-SE      TO W-IDDC-D9                         
054500                 PERFORM IMS-GET-AVROP-UNIKT                              
054600*** OBS FÖR ATT INSERTEN SKA GÅ BRA MÅSTE AVROPET LÄSAS                   
054700***     UNIKT FÖR ATT "POSITIONERA" SIG RÄTT. LÄGG INTE                   
054800***     IN NY KOD HÄR!!!                                                  
054900                 MOVE W-DAGENS-DATUM-AAVV                                 
055000                                       TO BEORDR-TIBEODAT-SATS            
055100                 MOVE ZERO             TO BEORDR-IDORDNSB                 
055200*                                                                         
055300*--- SKRIV POST PÅ W21510 FÖR ISRT PÅ INLB32                              
055400*                                                                         
055500                 PERFORM S15-SKRIV-W21510-105                             
055600                                                                          
055700                 PERFORM CA-SKRIV-ORDERNR-TRANS                           
055800*                                                                         
055900*--- SKRIV POST PÅ W21510 FÖR ISRT PÅ XXBM11                              
056000*                                                                         
056100                 PERFORM S14-SKRIV-W21510-104                             
056200               ELSE                                                       
056300                 MOVE JA TO SW-SATS-FELAKTIG                              
056400                 MOVE 3  TO IX-FEL                                        
056500                 PERFORM S03-RED-SKRIV-LISTPOST-W21507                    
056600               END-IF                                                     
056700             END-IF                                                       
056800           END-IF                                                         
056900         END-IF                                                           
057000       END-IF                                                             
057001                                                                          
057010       MOVE W-IDARTNR-SATS TO W-IDARTNR-D9                                
057020       MOVE WC-CDC-SE      TO W-IDDC-D9                                   
057100       PERFORM IMS-GET-AVROP-SEG                                          
057200     END-PERFORM                                                          
057300     .                                                                    
057400     EJECT                                                                
057500 CA-SKRIV-ORDERNR-TRANS  SECTION.                                         
057600******************************************************************        
057700*                                                                *        
057800*    BEGÄRAN OM ORDERNUMMER SKICKAS TILL SATSORDER               *        
057900*                                                                *        
058000******************************************************************        
058100                                                                          
058200     MOVE WS-IDARTNR-SATS    TO U02-IDARTNR                               
058300     MOVE W-DAAVROP-AVS      TO WS-DAAVROP                                
058400     MOVE WS-DAAVROP-AAVV    TO U02-TIBEHOV                               
058500     MOVE WS-IDANSK          TO U02-IDANSK                                
058600     MOVE WS-IDDISTR         TO U02-IDDISTR                               
058700     MOVE '1002 '            TO U02-IDLEVNR                               
058800     MOVE +1                 TO U02-KDCLAGER                              
058900     MOVE WS-KVAVROP         TO U02-KVBEART                               
059000     PERFORM S05-SKRIV-SATSORD-POST                                       
059100                                                                          
059200     .                                                                    
059300     EJECT                                                                
059400 Z-FINIT SECTION.                                                         
059500******************************************************************        
059600*                                                                *        
059700*    STÄNG FILER                                                 *        
059800*                                                                *        
059900******************************************************************        
060000                                                                          
060100     CLOSE W21502                                                         
060200           W21507                                                         
060300                                                                          
060400     MOVE 'S' TO POSTSUM-OPKOD                                            
060500     CALL POSTSUM USING POSTSUM-PARM                                      
060600     .                                                                    
060700     EJECT                                                                
060800 S01-LAES-NY-SATS SECTION.                                                
060900     SKIP3                                                                
061000     MOVE '1002 ' TO W-IDLEVNR-STR                                        
061100     PERFORM IMS-GET-SATS-ROT-SEG                                         
061200                                                                          
061300     MOVE NEJ TO SW-ARTREGDATA-LAEST-FOR-SATS                             
061400     .                                                                    
061500     EJECT                                                                
061600 S02-LAES-ARTREG-SATS SECTION.                                            
061700******************************************************************        
061800*                                                                *        
061900*    OM ARTREGDATA EJ FINNS FÖR SATS SÅ DUMPAR PROGRAMMET        *        
062000*                                                                *        
062100******************************************************************        
062200                                                                          
062300     IF SW-ARTREGDATA-LAEST-FOR-SATS = NEJ                                
062400       MOVE WS-IDARTNR-SATS TO W-IDARTNR-SATS                             
062500       PERFORM IMS-GET-ARTIKELSATS-ROT-SEG                                
062600       IF ART-IDFTG = 03                                                  
062700         MOVE 44 TO WS-IDDISTR                                            
062800       ELSE                                                               
062900         MOVE 98 TO WS-IDDISTR                                            
063000       END-IF                                                             
063100       PERFORM IMS-GET-CLAG-SEG                                           
063200       IF SEGMENT-FINNS                                                   
063300         MOVE CLAG-IDANSK      TO WS-IDANSK                               
063400         MOVE CLAG-KVVECKOR-LT TO W-BEORDR-VECKO-GRAENS                   
063500         MOVE CLAG-PRARTSTD    TO WS-PRARTSTD                             
063600       END-IF                                                             
063700                                                                          
063800       MOVE JA TO SW-ARTREGDATA-LAEST-FOR-SATS                            
063900     END-IF                                                               
064000     .                                                                    
064100     EJECT                                                                
064200 S03-RED-SKRIV-LISTPOST-W21507 SECTION.                                   
064300******************************************************************        
064400*                                                                *        
064500*    FELINFORMATION TILL ANSKAFFARE.                             *        
064600*        - STANDARDPRIS = 0 FÖR ARTIKEL                          *        
064700*        - ANTAL INGÅENDE ARTIKLAR < 2                           *        
064800*    ARTIKEL-REGISTRET LÄSES FÖR UPPGIFT OM ANSKAFFARE           *        
064900*    OM DET EJ TIDIGARE ÄR GJORT                                 *        
065000*                                                                *        
065100******************************************************************        
065200     SKIP1                                                                
065300     IF SW-ARTREGDATA-LAEST-FOR-SATS = NEJ                                
065400       PERFORM S02-LAES-ARTREG-SATS                                       
065500     END-IF                                                               
065600                                                                          
065700     MOVE WS-IDANSK       TO U07LI1-IDANSK                                
065800     MOVE WS-IDARTNR-SATS TO U07LI1-IDARTNR-SATS                          
065900                                                                          
066000     MOVE +1               TO U07LI2-SORTHELP                             
066100     MOVE FEL-ANM (IX-FEL) TO U07LI2-FELMEDDELANDE                        
066200                                                                          
066300     PERFORM S04-SKRIV-LISTPOST                                           
066400     .                                                                    
066500     EJECT                                                                
066600 S04-SKRIV-LISTPOST SECTION.                                              
066700                                                                          
066800     WRITE U07LI2-POST FROM U07LI2-AREA                                   
066900                                                                          
067000     MOVE 'W21507'        TO POSTSUM-FDNAMN                               
067100     MOVE 'W21502D2'      TO POSTSUM-DDNAMN2                              
067200     MOVE U07LI1-SORTHELP TO W-TRANSTYP-N                                 
067300     MOVE W-TRANSTYP      TO POSTSUM-TRANSTYP                             
067400     CALL POSTSUM      USING POSTSUM-PARM                                 
067500     .                                                                    
067600     EJECT                                                                
067700 S05-SKRIV-SATSORD-POST SECTION.                                          
067800                                                                          
067900     WRITE W21502-POST FROM U02-AREA                                      
068000                                                                          
068100     MOVE 'W21502'   TO POSTSUM-FDNAMN                                    
068200     MOVE 'W21502D1' TO POSTSUM-DDNAMN2                                   
068300     CALL POSTSUM    USING POSTSUM-PARM                                   
068400     .                                                                    
068500     EJECT                                                                
068600 S06-BERAKNA-VECKODIFFERENS SECTION.                                      
068700     SKIP3                                                                
068800     MOVE W-DATUM-TOM TO W-DATUM-AAVV                                     
068900     MOVE W-DATUM-AA  TO W-DIFF-AA                                        
069000     MOVE W-DATUM-VV  TO W-VECKO-DIFFERENS                                
069100                                                                          
069200     MOVE W-DATUM-FROM     TO W-DATUM-AAVV                                
069300                                                                          
069400     MOVE W-DATUM-AA     TO TMP1-YY                                       
069500     MOVE W-DIFF-AA      TO TMP2-YY                                       
069600     PERFORM WY2000P9                                                     
069700     SUBTRACT TMP1-YY    FROM TMP2-YY                                     
069800     MOVE TMP2-YY        TO W-DIFF-AA                                     
069900                                                                          
070000     MULTIPLY 52 BY W-DIFF-AA                                             
070100                                                                          
070200     ADD W-DIFF-AA         TO W-VECKO-DIFFERENS                           
070300     SUBTRACT W-DATUM-VV FROM W-VECKO-DIFFERENS                           
070400     .                                                                    
070500     EJECT                                                                
070600 S10-SKRIV-W21510-100 SECTION.                                            
070700*                                                                         
070800***                                                                       
070900* SKRIVER POST PÅ FILEN W21510 SOM SKA                                    
071000* UPPDATERA SATB01-SEGMENTET (WDJ101)                                     
071100***                                                                       
071200     SKIP3                                                                
071300     MOVE '100'           TO UT-100-IDPTYP                                
071400     MOVE WS-IDARTNR-SATS TO UT-100-IDARTNR-SATS                          
071500                             UT-100-IDARTNR                               
071600     MOVE WS-DAGENS-DATUM TO UT-100-TIUPPDAT                              
071700                                                                          
071800     WRITE W21510-POST FROM UT-100-AREA                                   
071900     MOVE '100'           TO W21510-TRANSTYP                              
072000     CALL POSTSUM USING W21510-PARM                                       
072100     .                                                                    
072200     EJECT                                                                
072300 S11-SKRIV-W21510-101 SECTION.                                            
072400*                                                                         
072500***                                                                       
072600* SKRIVER POST PÅ FILEN W21510 SOM SKA                                    
072700* UPPDATERA SATB11-SEGMENTET (WDJ111)                                     
072800***                                                                       
072900     SKIP3                                                                
073000     MOVE '101'             TO UT-101-IDPTYP                              
073100     MOVE WS-IDARTNR-SATS   TO UT-101-IDARTNR-SATS                        
073200     MOVE SATB-RAD-IDARTNR  TO UT-101-IDARTNR                             
073300     MOVE SATB-RAD-KDSTRRAD TO UT-101-KDSTRRAD                            
073400     MOVE SATB-RAD-IDRADNR  TO UT-101-IDRADNR                             
073500     MOVE INGAENDE-ART      TO UT-101-KDISATS                             
073600                                                                          
073700     WRITE W21510-POST FROM UT-101-AREA                                   
073800     MOVE '101'           TO W21510-TRANSTYP                              
073900     CALL POSTSUM USING W21510-PARM                                       
074000     .                                                                    
074100     EJECT                                                                
074200 S12-SKRIV-W21510-102 SECTION.                                            
074300*                                                                         
074400***                                                                       
074500* SKRIVER POST PÅ FILEN W21510 SOM SKA                                    
074600* UPPDATERA ARTC01-SEGMENTET (WDK601)                                     
074700***                                                                       
074800     SKIP3                                                                
074900     MOVE '102'             TO UT-102-IDPTYP                              
075000     MOVE WS-IDARTNR-SATS   TO UT-102-IDARTNR-SATS                        
075100                                                                          
075200     WRITE W21510-POST FROM UT-102-AREA                                   
075300     MOVE '102'           TO W21510-TRANSTYP                              
075400     CALL POSTSUM USING W21510-PARM                                       
075500     .                                                                    
075600     EJECT                                                                
075700 S13-SKRIV-W21510-103 SECTION.                                            
075800*                                                                         
075900***                                                                       
076000* SKRIVER POST PÅ FILEN W21510 SOM SKA SKAPA                              
076100* ETT XXBY11-SEGMENT (WDG311)                                             
076200***                                                                       
076300     SKIP3                                                                
076400     MOVE '103'            TO UT-103-IDPTYP                               
076500     MOVE WS-IDARTNR-SATS  TO UT-103-IDARTNR-SATS                         
076600     MOVE SATB-RAD-IDARTNR TO UT-103-IDARTNR-ING                          
076700     MOVE UTGAENDE-ART     TO UT-103-KDISATS                              
076800     MOVE ZERO             TO UT-103-KVPB-SEP-TOT                         
076900                              UT-103-REANTPSA-NY                          
077000                              UT-103-REANTPSA-GAMMAL                      
077100                              UT-103-TIBEHDAT                             
077200                                                                          
077300     WRITE W21510-POST FROM UT-103-AREA                                   
077400     MOVE '103'           TO W21510-TRANSTYP                              
077500     CALL POSTSUM USING W21510-PARM                                       
077600     .                                                                    
077700     EJECT                                                                
077800 S14-SKRIV-W21510-104 SECTION.                                            
077900*                                                                         
078000***                                                                       
078100* SKRIVER POST PÅ FILEN W21510 SOM SKA SKAPA                              
078200* ETT XXBM11-SEGMENT (WDG311)                                             
078300***                                                                       
078400     SKIP3                                                                
078500     MOVE '104'            TO UT-104-IDPTYP                               
078600     MOVE WS-IDARTNR-SATS  TO UT-104-IDARTNR-SATS                         
078700     MOVE NEJ              TO UT-104-FLAGGA-LPKNTL-ING                    
078800                                                                          
078900     WRITE W21510-POST FROM UT-104-AREA                                   
079000     MOVE '104'           TO W21510-TRANSTYP                              
079100     CALL POSTSUM USING W21510-PARM                                       
079200     .                                                                    
079300     EJECT                                                                
079400 S15-SKRIV-W21510-105 SECTION.                                            
079500*                                                                         
079600***                                                                       
079700* SKRIVER POST PÅ FILEN W21510 SOM SKA SKAPA                              
079800* ETT INLB32-SEGMENT (WDD932)                                             
079900***                                                                       
080000     SKIP3                                                                
080100     MOVE '105'            TO UT-105-IDPTYP                               
080200     MOVE W-IDARTNR-SATS   TO UT-105-IDARTNR-SATS                         
080300     MOVE W-IDLEVNR        TO UT-105-IDLEVNR                              
080400     MOVE W-DAAVROP-AVS    TO UT-105-DAAVROP-AVS                          
080500     MOVE W-KDAVROP        TO UT-105-KDAVROP                              
080600     MOVE W-DAGENS-DATUM-AAVV                                             
080700                           TO UT-105-TIBEODAT-SATS                        
080800     MOVE ZERO             TO UT-105-IDORDNSB                             
080900                                                                          
081000     WRITE W21510-POST FROM UT-105-AREA                                   
081100     MOVE '105'           TO W21510-TRANSTYP                              
081200     CALL POSTSUM USING W21510-PARM                                       
081300     .                                                                    
081400     EJECT                                                                
081500* IMS-SEKTIONER                                                           
081600     SKIP3                                                                
081700 IMS-GET-SATS-ROT-SEG SECTION.                                            
081800                                                                          
081900     STRING 'WLSATB01(IDLEVNR  =' W-IDLEVNR-STR-X ')'                     
082000            DELIMITED BY SIZE INTO SSA1                                   
082100     MOVE '  GEGB' TO GODK-STATUSKODER                                    
082200     CALL CBLTDLI USING GN SATB-PCB DLI-IO-AREA SSA1                      
082300     MOVE SATB-STATUS-CODE TO STATUS-WS                                   
082400     PERFORM IMS-STATUSKONTROLL                                           
082500     SKIP3                                                                
082600     .                                                                    
082700 IMS-GET-ING-ARTIKEL-SEG  SECTION.                                        
082800                                                                          
082900     MOVE 'WLSATB11 ' TO SSA1                                             
083000     MOVE '  GE' TO GODK-STATUSKODER                                      
083100     CALL CBLTDLI USING GNP SATB-PCB DLI-IO-AREA SSA1                     
083200     MOVE SATB-STATUS-CODE TO STATUS-WS                                   
083300     PERFORM IMS-STATUSKONTROLL                                           
083400     EJECT                                                                
083500     .                                                                    
083600 IMS-GET-SATE-CSEQ-FIRST SECTION.                                         
083700                                                                          
083800     STRING 'WLSATB11*D(WDJ1CSEQ =' W-WDJ1CSEQ-X ')'                      
083900            DELIMITED BY SIZE INTO SSA1                                   
084000     STRING 'WLSATB01(IDLEVNR  =' W-IDLEVNR-STR-X ')'                     
084100            DELIMITED BY SIZE INTO SSA2                                   
084200     MOVE '  GBGE' TO GODK-STATUSKODER                                    
084300     CALL CBLTDLI USING GU SATE-PCB DLI-IO-AREA SSA1 SSA2                 
084400     MOVE SATE-STATUS-CODE TO STATUS-WS                                   
084500     PERFORM IMS-STATUSKONTROLL                                           
084600     SKIP3                                                                
084700     .                                                                    
084800 IMS-GET-SATE-CSEQ-NEXT  SECTION.                                         
084900                                                                          
085000     STRING 'WLSATB11*D(WDJ1CSEQ =' W-WDJ1CSEQ-X ')'                      
085100            DELIMITED BY SIZE INTO SSA1                                   
085200     STRING 'WLSATB01(IDLEVNR  =' W-IDLEVNR-STR-X ')'                     
085300            DELIMITED BY SIZE INTO SSA2                                   
085400     MOVE '  GBGE' TO GODK-STATUSKODER                                    
085500     CALL CBLTDLI USING GN SATE-PCB DLI-IO-AREA SSA1 SSA2                 
085600     MOVE SATE-STATUS-CODE TO STATUS-WS                                   
085700     PERFORM IMS-STATUSKONTROLL                                           
085800     SKIP3                                                                
085900     .                                                                    
086000 IMS-GET-ARTIKELSATS-ROT-SEG SECTION.                                     
086100                                                                          
086200     STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-SATS-X ')'                    
086300            DELIMITED BY SIZE INTO SSA1                                   
086400     MOVE '  ' TO GODK-STATUSKODER                                        
086500     CALL CBLTDLI USING GU ARTC-PCB DLI-IO-AREA-01 SSA1                   
086600     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
086700     PERFORM IMS-STATUSKONTROLL                                           
086800     SKIP3                                                                
086900     .                                                                    
087000 IMS-GET-ARTC01 SECTION.                                                  
087100                                                                          
087200     STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-SATS-X ')'                    
087300            DELIMITED BY SIZE INTO SSA1                                   
087400     MOVE '  GE' TO GODK-STATUSKODER                                      
087500     CALL CBLTDLI USING GU ARTC-PCB DLI-IO-AREA-01 SSA1                   
087600     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
087700     PERFORM IMS-STATUSKONTROLL                                           
087800     SKIP3                                                                
087900     .                                                                    
088000 IMS-GET-CLAG-SEG SECTION.                                                
088100                                                                          
088200     STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-SATS-X ')'                    
088300            DELIMITED BY SIZE INTO SSA1                                   
088400     MOVE 'WLARTC11 ' TO SSA2                                             
088500     MOVE '  GE' TO GODK-STATUSKODER                                      
088600     CALL CBLTDLI USING GN  ARTC-PCB DLI-IO-AREA-11 SSA1 SSA2             
088700     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
088800     PERFORM IMS-STATUSKONTROLL                                           
088900     SKIP3                                                                
089000     .                                                                    
089100 IMS-GET-AVROP-SEG-FIRST SECTION.                                         
089200                                                                          
089300     STRING 'WLINLB01(WDD901KY =' W-WDD901KY-X ')'                        
089400            DELIMITED BY SIZE INTO SSA1                                   
089500     STRING 'WLINLB11(IDLEVNR  =' W-IDLEVNR-X ')'                         
089600            DELIMITED BY SIZE INTO SSA2                                   
089700     STRING 'WLINLB23*F(KDAVROP  =' W-KDAVROP-X ')'                       
089800            DELIMITED BY SIZE INTO SSA3                                   
089900     MOVE '  GE' TO GODK-STATUSKODER                                      
090000     CALL CBLTDLI USING GU  INLB-PCB DLI-IO-AREA SSA1 SSA2 SSA3           
090100     MOVE INLB-STATUS-CODE TO STATUS-WS                                   
090200     PERFORM IMS-STATUSKONTROLL                                           
090300     SKIP3                                                                
090400     .                                                                    
090500 IMS-GET-AVROP-SEG SECTION.                                               
090600                                                                          
090700     STRING 'WLINLB01(WDD901KY =' W-WDD901KY-X ')'                        
090800            DELIMITED BY SIZE INTO SSA1                                   
090900     STRING 'WLINLB11(IDLEVNR  =' W-IDLEVNR-X ')'                         
091000            DELIMITED BY SIZE INTO SSA2                                   
091100     STRING 'WLINLB23(DAAVROP  >' W-DAAVROP-AVS-X                         
091200                    '&KDAVROP  =' W-KDAVROP-X ')'                         
091300            DELIMITED BY SIZE INTO SSA3                                   
091400     MOVE '  GE' TO GODK-STATUSKODER                                      
091500     CALL CBLTDLI USING GU   INLB-PCB DLI-IO-AREA SSA1 SSA2 SSA3          
091600     MOVE INLB-STATUS-CODE TO STATUS-WS                                   
091700     PERFORM IMS-STATUSKONTROLL                                           
091800     EJECT                                                                
091900     .                                                                    
092000 IMS-GET-AVROP-UNIKT SECTION.                                             
092100                                                                          
092200     STRING 'WLINLB01(WDD901KY =' W-WDD901KY-X ')'                        
092300            DELIMITED BY SIZE INTO SSA1                                   
092400     STRING 'WLINLB11(IDLEVNR  =' W-IDLEVNR-X ')'                         
092500            DELIMITED BY SIZE INTO SSA2                                   
092600     STRING 'WLINLB23(DAAVROP  =' W-DAAVROP-AVS-X                         
092700           '&KDAVROP  =' W-KDAVROP-X ')'                                  
092800            DELIMITED BY SIZE INTO SSA3                                   
092900     MOVE '  ' TO GODK-STATUSKODER                                        
093000     CALL CBLTDLI USING GU   INLB-PCB DLI-IO-AREA SSA1 SSA2 SSA3          
093100     MOVE INLB-STATUS-CODE TO STATUS-WS                                   
093200     PERFORM IMS-STATUSKONTROLL                                           
093300     EJECT                                                                
093400     .                                                                    
093500 IMS-GNP-SATSBEORDR SECTION.                                              
093600                                                                          
093700     MOVE 'WLINLB32 ' TO SSA1                                             
093800     MOVE '  GE' TO GODK-STATUSKODER                                      
093900     CALL CBLTDLI USING GNP INLB-PCB DLI-IO-AREA SSA1                     
094000     MOVE INLB-STATUS-CODE TO STATUS-WS                                   
094100     PERFORM IMS-STATUSKONTROLL                                           
094200     EJECT                                                                
094300     .                                                                    
094400 IMS-GET-SATB01 SECTION.                                                  
094500                                                                          
094600     STRING 'WLSATB01(IDARTNR  =' W-IDARTNR-SATS-X ')'                    
094700            DELIMITED BY SIZE INTO SSA1                                   
094800     MOVE '  '  TO GODK-STATUSKODER                                       
094900     CALL CBLTDLI USING GU SATB2-PCB DLI-IO-AREA SSA1                     
095000     MOVE SATB2-STATUS-CODE TO STATUS-WS                                  
095100     PERFORM IMS-STATUSKONTROLL                                           
095200     SKIP3                                                                
095300     .                                                                    
095400 IMS-STATUSKONTROLL SECTION.                                              
095500                                                                          
095600     SET STATUS-IX TO 1                                                   
095700     SEARCH GODK-STATUS AT END CALL FELLOG                                
095800       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
095900       CONTINUE                                                           
096000     END-SEARCH                                                           
096100     .                                                                    
096200     EJECT                                                                
096300*    -COPY WY2000P9                                                       
096400     EJECT                                                                
096500*    -COPY WY2000P1                                                       
096600     EJECT                                                                
096700*    -COPY WY2000Q1                                                       
