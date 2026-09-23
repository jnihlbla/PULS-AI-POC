000100 ID DIVISION.                                                             
000200                                                                          
000300 PROGRAM-ID.     W3712200.                                                
000400 AUTHOR.         RONNY STENHOLM.                                          
000500 DATE-WRITTEN.   92/07/27.                                                
000600 DATE-COMPILED.                                                           
000700                                                                          
000800*    FUNKTION:                                                            
000900*        LÄSER GODKÄNDA RAPPORTER FRÅN WDM6 TILL                          
001000*        BYTES OCH TILL VIPS.                                             
001100*                                                                         
001200*        PROGRAMMET LÄSER      WDM6                                       
001300*                                                                         
001400*    ABENDKODER:                                                          
001500*        U0016 -  . . . .                                                 
001600*        U1000 -  . . . .                                                 
001700*                                                                         
001800                                                                          
001900 ENVIRONMENT DIVISION.                                                    
002000                                                                          
002100 INPUT-OUTPUT SECTION.                                                    
002200                                                                          
002300 FILE-CONTROL.                                                            
002400                                                                          
002500*          --- R56:OR TILL W371 BYTES                                     
002600     SELECT W37104                     ASSIGN TO W37122D1.                
002700                                                                          
002800*          --- FIL TILL VIPS MED GODK/UNDERK RETURER                      
002900*          --- ÄVEN RAPPORTER MED STATUS 3 SKICKAS TILL VIPS              
003000     SELECT W37119                     ASSIGN TO W37122D2.                
003100                                                                          
003200*          --- FIL TILL WDM6 FÖR ATT ÄNDRA STATUS 4 TILL 9.               
003300     SELECT W37118                     ASSIGN TO W37122D3.                
003400                                                                          
003500*          --- FIL TILL ÖVR  TULL                                         
003600     SELECT W37132                     ASSIGN TO W37122D6.                
003700                                                                          
003800*        --- RAPPORTER SOM ÄNDRAR STATUS PÅ KDBYTBEK PÅ BASEN WDM6        
003900     SELECT W37159                     ASSIGN TO W37122D7.                
004000                                                                          
004100     EJECT                                                                
004200 DATA DIVISION.                                                           
004300                                                                          
004400 FILE SECTION.                                                            
004500                                                                          
004600 FD  W37104                                                               
004700     RECORDING       F                                                    
004800     BLOCK CONTAINS  0.                                                   
004900                                                                          
005000*01  POST -COPY W37104 -PRE    BYTES-  -L.                                
005100                                                                          
005200 FD  W37119                                                               
005300     RECORDING       V                                                    
005400     BLOCK CONTAINS  0.                                                   
005500*01  POST -COPY W37119 -PRE  VIPS-  -L.                                   
005600                                                                          
005700 FD  W37118                                                               
005800     RECORDING       F                                                    
005900     BLOCK CONTAINS  0.                                                   
006000*01  POST -COPY W371STAT -PRE    WDM6-  -L.                               
006100                                                                          
006200     EJECT                                                                
006300 FD  W37132                                                               
006400     RECORDING       F                                                    
006500     BLOCK CONTAINS  0.                                                   
006600*01  POST -COPY W37138 -PRE    TULL2- -L.                                 
006700                                                                          
006800 FD  W37159                                                               
006900     RECORDING       F                                                    
007000     BLOCK CONTAINS  0.                                                   
007100                                                                          
007200*01  POST -COPY W37159 -PRE    STA-  -L.                                  
007300                                                                          
007400     EJECT                                                                
007500 WORKING-STORAGE SECTION.                                                 
007600                                                                          
007700*    -- CHECKED BY WY2000                                                 
007800                                                                          
007900 77  IDPGM                       PIC X(8)    VALUE 'W3712200'.            
008000 77  JA                          PIC X       VALUE 'J'.                   
008100 77  NEJ                         PIC X       VALUE 'N'.                   
008200 77  WS-SECTION                  PIC X(10)   VALUE SPACE.                 
008300                                                                          
008400 01  FELTEXT.                                                             
008500     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
008600     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
008700     EJECT                                                                
008800 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
008900 01  FILLER REDEFINES DAGENS-DATUM.                                       
009000     03  DAGENS-DATUM-AAR        PIC 9(2).                                
009100     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
009200     03  DAGENS-DATUM-DAG        PIC 9(2).                                
009300     EJECT                                                                
009400 01  DYNAMISKA-SUBPROGRAM.                                                
009500                                                                          
009600     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
009700     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
009800     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
009900     EJECT                                                                
010000*    --- PARAMETRAR TILL POSTSUM                                          
010100                                                                          
010200*01  -COPY W0005   -PRE  POSTSUM-                                         
010300     EJECT                                                                
010400*    --- VALID IDDC CODES                                                 
010500                                                                          
010600*01  -COPY WWDC99                                                         
010700                                                                          
010710*01  -COPY WWDCKONS                                                       
010800     EJECT                                                                
010900******************************************************************        
011000 01  TEST-IDBYTREF             PIC X(3).                                  
011100*01  FILLER -COPY WWBYT15    -RED TEST-IDBYTREF                           
011200******************************************************************        
011300                                                                          
011400 01  FILLER                      PIC X(24)   VALUE                        
011500                                             'BYTES-AREA-START'.          
011600*01  AREA -COPY W37104     -PRE BYTES-                                    
011700                                                                          
011800     EJECT                                                                
011900 01  FILLER                      PIC X(24)   VALUE                        
012000                                             'STAT-AREA-START'.           
012100*01  AREA -COPY W37159     -PRE STA-                                      
012200                                                                          
012300     EJECT                                                                
012400 01  FILLER                      PIC X(24)   VALUE                        
012500                                             'VIPS-AREA-START'.           
012600*01  AREA -COPY W37119     -PRE VIPS-                                     
012700                                                                          
012800     EJECT                                                                
012900 01  FILLER                      PIC X(24)   VALUE                        
013000                                             'WDM6-AREA-START'.           
013100*01  AREA -COPY W371STAT   -PRE WDM6-                                     
013200                                                                          
013300     EJECT                                                                
013400 01  FILLER                       PIC X(24)   VALUE                       
013500                                             'TULL2-AREA-START '.         
013600*01  AREA -COPY W37138     -PRE TULL2-                                    
013700                                                                          
013800     EJECT                                                                
013900 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
014000                                                                          
014100 01  NYCKLAR-TILL-DLI.                                                    
014200                                                                          
014300     03  W-WDM6ASEQ-MIN.                                                  
014400         05 W-WDM6A-KDBYTSTA-MIN PIC X      VALUE '3'.                    
014500         05 W-WDM6A-IDDC-MIN     PIC X(2)   VALUE SPACE.                  
014600         05 W-WDM6A-DAREGDAT-MIN PIC 9(8)   VALUE ZERO.                   
014700         05 W-WDM6A-IDDISTR-MIN  PIC S9(5)  VALUE ZERO   COMP-3.          
014800         05 W-WDM6A-IDBYTRAP-MIN PIC S9(7)  VALUE ZERO   COMP-3.          
014900                                                                          
015000     03  W-WDM6ASEQ-MAX.                                                  
015100         05 W-WDM6A-KDBYTSTA-MAX PIC X      VALUE '4'.                    
015200         05 W-WDM6A-IDDC-MAX     PIC X(2)   VALUE HIGH-VALUE.             
015300         05 W-WDM6A-DAREGDAT-MAX PIC 9(8)   VALUE 99999999.               
015400         05 W-WDM6A-IDDISTR-MAX  PIC S9(5)  VALUE +99999   COMP-3.        
015500         05 W-WDM6A-IDBYTRAP-MAX PIC S9(7)  VALUE +9999999 COMP-3.        
015600                                                                          
015700     03  W-WDK7-IDARTNR-X.                                                
015800         05 W-WDK7-IDARTNR       PIC S9(9) VALUE ZERO COMP-3.             
015900                                                                          
016000     03  W-WDK7-IDDC-X.                                                   
016100         05 W-WDK7-IDDC          PIC X(2).                                
016200                                                                          
016300*    --- STATUS-KOD FRÅN IMS                                              
016400 01  STATUS-WS                   PIC XX.                                  
016500     88  SEGMENT-FINNS                       VALUE '  '.                  
016600     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
016700     88  SEGMENT-SLUT                        VALUE 'GB'.                  
016800     88  IMS-EJ-OK                           VALUE 'XD'.                  
016900                                                                          
017000 01  GODK-STATUSKODER.                                                    
017100     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
017200                                                                          
017300 01  SSA1                        PIC X(256).                              
017400 01  SSA2                        PIC X(256).                              
017500     EJECT                                                                
017600*    --- IMS FUNKTIONSKODER                                               
017700*01  -COPY W0003                                                          
017800     EJECT                                                                
017900                                                                          
018000*    ---  DLI INPUT-OUTPUT AREA                                           
018100                                                                          
018200 01  FILLER                      PIC X(16)   VALUE 'WDM601-AREA '.        
018300 01  DLI-IO-WDM601.                                                       
018400*    03  -COPY WDM601                                                     
018500     EJECT                                                                
018600                                                                          
018700 01  FILLER                      PIC X(16)   VALUE 'WDM611-AREA '.        
018800 01  DLI-IO-WDM611.                                                       
018900*    03  -COPY WDM611                                                     
019000     EJECT                                                                
019100                                                                          
019200 01  FILLER                      PIC X(16)   VALUE 'WDK711-AREA '.        
019300 01  DLI-IO-WDK711.                                                       
019400*    03  -COPY WDK711                                                     
019500     EJECT                                                                
019600                                                                          
019700 LINKAGE SECTION.                                                         
019800                                                                          
019900*01  -COPY W0008  -PRE WDM6-                                              
020000     05  FILLER                  PIC X.                                   
020100     EJECT                                                                
020200*01  -COPY W0008  -PRE WDK7-                                              
020300     05  FILLER                  PIC X.                                   
020400     EJECT                                                                
020500 PROCEDURE DIVISION  USING  WDM6-PCB WDK7-PCB.                            
020600     ENTRY 'DLITCBL' USING  WDM6-PCB WDK7-PCB.                            
020700                                                                          
020800     PERFORM A-INIT                                                       
020900     PERFORM IMS-GU-WDM601                                                
021000     PERFORM UNTIL NOT SEGMENT-FINNS                                      
021100       PERFORM B-BEHANDLA-WDM601                                          
021200       PERFORM IMS-GNP-WDM611                                             
021300       PERFORM UNTIL NOT SEGMENT-FINNS                                    
021400          PERFORM D-FLYTTA-TILL-UTFILER                                   
021500          PERFORM IMS-GNP-WDM611                                          
021600       END-PERFORM                                                        
021700                                                                          
021800       PERFORM IMS-GN-WDM601                                              
021900     END-PERFORM                                                          
022000                                                                          
022100     PERFORM Z-FINIT                                                      
022200                                                                          
022300     MOVE ZERO                      TO RETURN-CODE                        
022400     GOBACK                                                               
022500     .                                                                    
022600     EJECT                                                                
022700 A-INIT SECTION.                                                          
022800     MOVE 'A'                       TO WS-SECTION                         
022900                                                                          
023000     OPEN OUTPUT W37104                                                   
023100                 W37119                                                   
023200                 W37118                                                   
023300                 W37132                                                   
023400                 W37159                                                   
023500                                                                          
023600     ACCEPT DAGENS-DATUM          FROM DATE                               
023700                                                                          
023800     MOVE IDPGM                     TO POSTSUM-PROGNAMN                   
023900     .                                                                    
024000     EJECT                                                                
024100 B-BEHANDLA-WDM601  SECTION.                                              
024200     MOVE 'B'                       TO WS-SECTION                         
024300                                                                          
024400     MOVE RAPP-IDDC                 TO W-WDK7-IDDC                        
024500                                                                          
024600     MOVE RAPP-IDDISTR              TO BYTES-IDDISTR                      
024700     MOVE RAPP-IDBYTRAP             TO BYTES-IDBYTRAP                     
024800     MOVE RAPP-IDDC                 TO BYTES-IDDC                         
024900     MOVE RAPP-IDKUNDNR             TO BYTES-IDKUNDNR                     
025000     MOVE RAPP-DAREGDAT-GODK (3:6)  TO BYTES-TIREGDAT-GODK                
025100     MOVE 'R56'                     TO BYTES-IDPTYP                       
025200                                                                          
025300     IF RAPP-IDDC = WC-SDC-NL-ET                                          
025400       MOVE WC-SDC-NL               TO VIPS-IDDC                          
025500     ELSE                                                                 
025600       MOVE RAPP-IDDC               TO VIPS-IDDC                          
025700     END-IF                                                               
025800     MOVE RAPP-IDDISTR              TO VIPS-IDDISTR                       
025900     MOVE RAPP-IDDISTR              TO VIPS-SOR0-IDDISTR                  
026000     MOVE RAPP-IDBYTRAP             TO VIPS-IDBYTRAP                      
026100     MOVE RAPP-IDKUNDNR             TO VIPS-IDKUNDNR                      
026200     MOVE RAPP-IDKUNDNR             TO VIPS-SOR0-IDKUNDNR                 
026300     MOVE RAPP-DAREGDAT-GODK (3:6)  TO VIPS-TIREGDAT-GODK                 
026400     MOVE ZERO                      TO VIPS-SOR0-IDRONR                   
026500     MOVE ZERO                      TO VIPS-SOR0-TIRODAT                  
026600     MOVE 'RKG'                     TO VIPS-SOR0-IDPTYP                   
026700     MOVE ZERO                      TO VIPS-SOR0-IDLOPNR                  
026800     MOVE 'RKG'                     TO VIPS-IDPTYP                        
026900                                                                          
027000                                                                          
027100     MOVE RAPP-IDDISTR              TO WDM6-IDDISTR                       
027200     MOVE RAPP-IDBYTRAP             TO WDM6-IDBYTRAP                      
027300     IF RAPP-KDBYTSTA-RAPP = '4'                                          
027400       PERFORM S13-SKRIV-W37118-WDM6                                      
027500     END-IF                                                               
027600                                                                          
027700     MOVE RAPP-IDDC                 TO TULL2-IDDC                         
027800     MOVE RAPP-IDDISTR              TO TULL2-IDDISTR                      
027900     MOVE RAPP-IDBYTRAP             TO TULL2-IDBYTRAP                     
028000     MOVE RAPP-IDKUNDNR             TO TULL2-IDKUNDNR                     
028100     MOVE RAPP-DAREGDAT-GODK (3:6)  TO TULL2-TIREGDAT-GODK                
028200     MOVE RAPP-IDFAKT               TO TULL2-IDFAKT                       
028300                                                                          
028400*...OM KDBYTBEK ÄR LIKA MED ' ' OCH STATUS ÄR 3 PÅ RAPPORTEN              
028500*   DÅ KOMMER KDBYTBEK ATT ÄNDRAS TILL M SOM I MOTTAGEN                   
028600*   OCH UPPDATERA RAPPORTEN I DATABAS WDM6 I PGM W37159                   
028700                                                                          
028800     IF  RAPP-KDBYTBEK = SPACE                                            
028900     AND RAPP-KDBYTSTA-RAPP = '3'                                         
029000       MOVE RAPP-IDDISTR            TO STA-IDDISTR                        
029100       MOVE RAPP-IDBYTRAP           TO STA-IDBYTRAP                       
029200       MOVE 'M'                     TO STA-KDBYTBEK                       
029300       PERFORM S18-SKRIV-W37159-STATUS                                    
029400     END-IF                                                               
029500     .                                                                    
029600     EJECT                                                                
029700 D-FLYTTA-TILL-UTFILER SECTION.                                           
029800     MOVE 'D'                       TO WS-SECTION                         
029900                                                                          
030000     IF RAPP-KDBYTSTA-RAPP = '4'                                          
030100                                                                          
030200       MOVE OBJ-KDBYTREF            TO TEST-IDBYTREF                      
030300                                                                          
030400       IF OBJ-KVRETUR-GODK > ZERO                                         
030500          IF NOT BYT15-KDBYTREF-REMOVE                                    
030600             PERFORM DA-FLYTTA-TILL-BYTES                                 
030700          END-IF                                                          
030800       END-IF                                                             
030900                                                                          
031000       IF VIPS-IDKUNDNR > ZERO                                            
031100*.........OM KUNDNR = 0 SÅ ÄR DET EN ICKE-VIPS4-ANVÄNDARE                 
031200*         OCH DÅ SKALL INTE NÅGON SVARSTRANS SÄNDAS                       
031300          PERFORM DB-FLYTTA-TILL-VIPS                                     
031400          PERFORM S12-SKRIV-W37119-VIPS                                   
031500       END-IF                                                             
031600                                                                          
031700       IF OBJ-KVRETUR-GODK > ZERO                                         
031800         IF CDC OR SDC                                                    
031900            IF RAPP-FLBYGODK = 'N'                                        
032000               PERFORM DD-FLYTTA-TILL-TULL2                               
032100               PERFORM S16-SKRIV-W37132-TULL2                             
032200            END-IF                                                        
032300         END-IF                                                           
032400       END-IF                                                             
032500                                                                          
032600     ELSE                                                                 
032700                                                                          
032800       MOVE OBJ-KDBYTREF            TO TEST-IDBYTREF                      
032900                                                                          
033000       IF RAPP-KDBYTSTA-RAPP = '3'                                        
033100         IF VIPS-IDKUNDNR > ZERO                                          
033200*..........OM KUNDNR = 0 SÅ ÄR DET EN ICKE-VIPS4-ANVÄNDARE                
033300*          OCH DÅ SKALL INTE NÅGON SVARSTRANS SÄNDAS                      
033400           IF RAPP-KDBYTBEK = SPACE                                       
033500******************************************************************        
033600*** OM KDBYTBEK ÄR LIKA MED ' ' OCH STATUS ÄR 3 PÅ RAPPORTEN   ***        
033700*** DÅ KOMMER KDBYTBEK ATT ÄNDRAS TILL M SOM I MOTTAGEN        ***        
033800*** OCH UPPDATERA RAPPORTEN I DATABAS WDM6 I PGM W37159        ***        
033900*** OBS BYTES RAPPORTER SOM ÄR MOTTAGNA OCH HAR STATUS 3       ***        
034000*** SKICKAS ENDAST TILL USA I VÄNTAN PÅ ATT VIPS INSTALLERAR   ***        
034100*** DEN RELEASEN I ÖVRIGA VÄRLDEN                              ***        
034200******************************************************************        
034300*            IF NDC                                                       
034400               IF OBJ-KDBYTSTA-OBJ = 'E'                                  
034500               OR OBJ-KDBYTSTA-OBJ = 'N'                                  
034600                 CONTINUE                                                 
034700               ELSE                                                       
034800                 PERFORM DF-FLYTTA-TILL-VIPS-STATUS-3                     
034900                 PERFORM S12-SKRIV-W37119-VIPS                            
035000               END-IF                                                     
035100*            END-IF                                                       
035200           END-IF                                                         
035300         END-IF                                                           
035400       END-IF                                                             
035500     END-IF                                                               
035600     .                                                                    
035700     EJECT                                                                
035800 DA-FLYTTA-TILL-BYTES  SECTION.                                           
035900     MOVE 'DA'                      TO WS-SECTION                         
036000                                                                          
036100     MOVE OBJ-IDARTNR-OBJ           TO BYTES-IDARTNR-OBJ                  
036200     MOVE OBJ-IDORDER               TO BYTES-IDORDNR7                     
036300     MOVE OBJ-KVRETUR-GODK          TO BYTES-KVRETUR-GODK                 
036400     MOVE OBJ-FLSKROT               TO BYTES-FLSKROT                      
036500                                                                          
036600     IF NOT NDC-NA                                                        
036700       MOVE OBJ-BERADREF            TO BYTES-IDKUNDRF                     
036800     ELSE                                                                 
036900       MOVE SPACE                   TO BYTES-IDKUNDRF                     
037000     END-IF                                                               
037100     MOVE OBJ-KDBYTREF              TO BYTES-KDBYTREF                     
037200                                                                          
037300     PERFORM S11-SKRIV-W37104-BYTES                                       
037400     .                                                                    
037500     EJECT                                                                
037600 DB-FLYTTA-TILL-VIPS   SECTION.                                           
037700     MOVE 'DB'                      TO WS-SECTION                         
037800                                                                          
037900     MOVE OBJ-IDORDER               TO VIPS-IDORDNR7                      
038000     MOVE OBJ-IDARTNR-OBJ           TO VIPS-IDARTNR-OBJ                   
038100     MOVE OBJ-IDTABNR               TO VIPS-IDTABNR                       
038200     MOVE OBJ-KVRETUR-GODK          TO VIPS-KVRETUR-GODK                  
038300     MOVE OBJ-KDBYTSTA-OBJ          TO VIPS-KDBYTSTA-OBJ                  
038400     MOVE OBJ-KDBYTREF              TO VIPS-KDBYTREF                      
038500     MOVE OBJ-IDBYTRAD              TO VIPS-IDBYTRAD                      
038600                                                                          
038700     MOVE OBJ-IDARTNR-OBJ           TO W-WDK7-IDARTNR                     
038800     PERFORM IMS-GU-WDK711                                                
038900     IF SEGMENT-FINNS                                                     
039000        MOVE SLAG-PRAVCOST          TO VIPS-PRAVCOST-CORE                 
039100     ELSE                                                                 
039200        MOVE ZERO                   TO VIPS-PRAVCOST-CORE                 
039300     END-IF                                                               
039400*****************************************************                     
039500*** TILLFÄLLIG ÄNDRING SKA FÖRSVINNA NÄR          ***                     
039600*** DE GAMLA VIPSANVÄNDARNA ÄR BORTA     /INSK    ***                     
039700*****************************************************                     
039800     IF OBJ-IDBYTRAD < 100                                                
039900        IF OBJ-KDBYTSTA-OBJ = 'E'                                         
040000           MOVE 'N'                 TO OBJ-KDBYTSTA-OBJ                   
040100                                       VIPS-KDBYTSTA-OBJ                  
040200        END-IF                                                            
040300     END-IF                                                               
040400     .                                                                    
040500     EJECT                                                                
040600 DD-FLYTTA-TILL-TULL2  SECTION.                                           
040700     MOVE 'DD'                      TO WS-SECTION                         
040800                                                                          
040900     MOVE OBJ-IDORDER               TO TULL2-IDORDNR7                     
041000     MOVE OBJ-IDARTNR-OBJ           TO TULL2-IDARTNR-OBJ                  
041100     MOVE OBJ-KVRETUR-GODK          TO TULL2-KVRETUR-GODK                 
041200     .                                                                    
041300     EJECT                                                                
041400 DF-FLYTTA-TILL-VIPS-STATUS-3 SECTION.                                    
041500     MOVE 'DF'                      TO WS-SECTION                         
041600                                                                          
041700     MOVE OBJ-IDORDER               TO VIPS-IDORDNR7                      
041800     MOVE OBJ-IDARTNR-OBJ           TO VIPS-IDARTNR-OBJ                   
041900     MOVE OBJ-IDTABNR               TO VIPS-IDTABNR                       
042000     MOVE OBJ-KVRETUR-GODK          TO VIPS-KVRETUR-GODK                  
042100     MOVE '3'                       TO VIPS-KDBYTSTA-OBJ                  
042200     MOVE OBJ-KDBYTREF              TO VIPS-KDBYTREF                      
042300     MOVE OBJ-IDBYTRAD              TO VIPS-IDBYTRAD                      
042400                                                                          
042500     MOVE OBJ-IDARTNR-OBJ           TO W-WDK7-IDARTNR                     
042600     PERFORM IMS-GU-WDK711                                                
042700     IF SEGMENT-FINNS                                                     
042800        MOVE SLAG-PRAVCOST          TO VIPS-PRAVCOST-CORE                 
042900     ELSE                                                                 
043000        MOVE ZERO                   TO VIPS-PRAVCOST-CORE                 
043100     END-IF                                                               
043200     .                                                                    
043300     EJECT                                                                
043400 Z-FINIT               SECTION.                                           
043500     MOVE 'Z'                       TO WS-SECTION                         
043600                                                                          
043700     CLOSE W37104                                                         
043800           W37119                                                         
043900           W37118                                                         
044000           W37132                                                         
044100           W37159                                                         
044200                                                                          
044300     MOVE 'S'                       TO POSTSUM-OPKOD                      
044400     CALL POSTSUM                USING POSTSUM-PARM                       
044500     .                                                                    
044600     EJECT                                                                
044700 S11-SKRIV-W37104-BYTES SECTION.                                          
044800     MOVE 'S11'                     TO WS-SECTION                         
044900                                                                          
045000     WRITE BYTES-POST             FROM BYTES-AREA                         
045100                                                                          
045200     MOVE  BYTES-IDPTYP             TO POSTSUM-TRANSTYP                   
045300     MOVE 'W37104 '                 TO POSTSUM-FDNAMN                     
045400     MOVE 'W37122D1'                TO POSTSUM-DDNAMN2                    
045500     CALL POSTSUM                USING POSTSUM-PARM                       
045600     .                                                                    
045700     EJECT                                                                
045800 S12-SKRIV-W37119-VIPS SECTION.                                           
045900     MOVE 'S12'                     TO WS-SECTION                         
046000                                                                          
046100     WRITE VIPS-POST              FROM VIPS-AREA                          
046200                                                                          
046300     MOVE VIPS-IDPTYP               TO POSTSUM-TRANSTYP                   
046400     MOVE 'W37119 '                 TO POSTSUM-FDNAMN                     
046500     MOVE 'W37122D2'                TO POSTSUM-DDNAMN2                    
046600     CALL POSTSUM                USING POSTSUM-PARM                       
046700     .                                                                    
046800     EJECT                                                                
046900 S13-SKRIV-W37118-WDM6 SECTION.                                           
047000     MOVE 'S13'                     TO WS-SECTION                         
047100                                                                          
047200     WRITE WDM6-POST              FROM WDM6-AREA                          
047300                                                                          
047400     MOVE 'WDM6'                    TO POSTSUM-TRANSTYP                   
047500     MOVE 'W37118'                  TO POSTSUM-FDNAMN                     
047600     MOVE 'W37122D3'                TO POSTSUM-DDNAMN2                    
047700     CALL POSTSUM                USING POSTSUM-PARM                       
047800     .                                                                    
047900     EJECT                                                                
048000 S16-SKRIV-W37132-TULL2 SECTION.                                          
048100     MOVE 'S16'                     TO WS-SECTION                         
048200                                                                          
048300     WRITE TULL2-POST             FROM TULL2-AREA                         
048400                                                                          
048500     MOVE 'TUL2'                    TO POSTSUM-TRANSTYP                   
048600     MOVE 'W37132'                  TO POSTSUM-FDNAMN                     
048700     MOVE 'W37122D6'                TO POSTSUM-DDNAMN2                    
048800     CALL POSTSUM                USING POSTSUM-PARM                       
048900     .                                                                    
049000     EJECT                                                                
049100 S18-SKRIV-W37159-STATUS SECTION.                                         
049200     MOVE 'S18'                     TO WS-SECTION                         
049300                                                                          
049400     WRITE STA-POST               FROM STA-AREA                           
049500                                                                          
049600     MOVE  BYTES-IDPTYP             TO POSTSUM-TRANSTYP                   
049700     MOVE 'W37159 '                 TO POSTSUM-FDNAMN                     
049800     MOVE 'W37122D7'                TO POSTSUM-DDNAMN2                    
049900     CALL POSTSUM                USING POSTSUM-PARM                       
050000     .                                                                    
050100     EJECT                                                                
050200* --- IMS SEKTIONER ---                                                   
050300                                                                          
050400 IMS-GU-WDM601 SECTION.                                                   
050500     MOVE 'GU-WDM601'               TO WS-SECTION                         
050600                                                                          
050700     STRING 'WDM601  (WDM6ASEQ>=' W-WDM6ASEQ-MIN                          
050800                    '&WDM6ASEQ<=' W-WDM6ASEQ-MAX ')'                      
050900          DELIMITED BY SIZE INTO SSA1                                     
051000     MOVE '  GE'                    TO GODK-STATUSKODER                   
051100     CALL CBLTDLI USING GU WDM6-PCB DLI-IO-WDM601 SSA1                    
051200     MOVE WDM6-STATUS-CODE          TO STATUS-WS                          
051300     PERFORM IMS-STATUSKONTROLL                                           
051400     .                                                                    
051500     EJECT                                                                
051600 IMS-GN-WDM601 SECTION.                                                   
051700     MOVE 'GN-WDM601'               TO WS-SECTION                         
051800                                                                          
051900     STRING 'WDM601  (WDM6ASEQ>=' W-WDM6ASEQ-MIN                          
052000                    '&WDM6ASEQ<=' W-WDM6ASEQ-MAX ')'                      
052100          DELIMITED BY SIZE INTO SSA1                                     
052200     MOVE '  GEGB'                  TO GODK-STATUSKODER                   
052300     CALL CBLTDLI USING GN WDM6-PCB DLI-IO-WDM601 SSA1                    
052400     MOVE WDM6-STATUS-CODE          TO STATUS-WS                          
052500     PERFORM IMS-STATUSKONTROLL                                           
052600     .                                                                    
052700     EJECT                                                                
052800 IMS-GNP-WDM611 SECTION.                                                  
052900     MOVE 'GNP-WDM611'              TO WS-SECTION                         
053000                                                                          
053100     MOVE   'WDM611'                TO SSA1                               
053200     MOVE '  GEGB'                  TO GODK-STATUSKODER                   
053300     CALL CBLTDLI USING GNP WDM6-PCB DLI-IO-WDM611 SSA1                   
053400     MOVE WDM6-STATUS-CODE          TO STATUS-WS                          
053500     PERFORM IMS-STATUSKONTROLL                                           
053600     .                                                                    
053700     EJECT                                                                
053800 IMS-GU-WDK711 SECTION.                                                   
053900     MOVE 'GU-WDK711'               TO WS-SECTION                         
054000                                                                          
054100     STRING 'WDK701  (IDARTNR  =' W-WDK7-IDARTNR-X ')'                    
054200           DELIMITED BY SIZE INTO SSA1                                    
054300     STRING 'WDK711  (IDDC     =' W-WDK7-IDDC-X ')'                       
054400           DELIMITED BY SIZE INTO SSA2                                    
054500     MOVE '  GE'                    TO GODK-STATUSKODER                   
054600     CALL CBLTDLI USING GU WDK7-PCB DLI-IO-WDK711 SSA1 SSA2               
054700     MOVE WDK7-STATUS-CODE          TO STATUS-WS                          
054800     PERFORM IMS-STATUSKONTROLL                                           
054900     .                                                                    
055000     EJECT                                                                
055100 IMS-STATUSKONTROLL SECTION.                                              
055200                                                                          
055300     SET STATUS-IX TO 1                                                   
055400     SEARCH GODK-STATUS                                                   
055500       AT END                                                             
055600         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
055700           DELIMITED BY SIZE INTO FELTEXT                                 
055800         DISPLAY FELTEXT                                                  
055900         DISPLAY 'SECTION:' WS-SECTION                                    
056000         DISPLAY 'SSA1   :' SSA1                                          
056100         DISPLAY 'SSA2   :' SSA2                                          
056200         CALL FELLOG                                                      
056300       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
056400         CONTINUE                                                         
056500     END-SEARCH                                                           
056600     .                                                                    
