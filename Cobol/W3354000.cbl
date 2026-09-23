000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.     W3354000.                                                
000400*AUTHOR.         RONNY STENHOLM.                                          
000500*DATE-WRITTEN.   93/12/07.                                                
000600                                                                          
000700*    REMARKS                                                              
000800*                                                                         
000900*   USABENÄMNINGAR PÅGÅR                                                  
001000*                                                                         
001100*                                                                         
001200*    FUNKTION:                                                            
001300*        KOMPLETTERAR ARTINFO TILL MB                                     
001400*        HÄMTAR ARTIKELBENÄMNINGENL LEVERANTÖR                            
001500*                                                                         
001600*                                                                         
001700*                                                                         
001800*    ABENDKODER:                                                          
001900*        U0016 -  . . . .                                                 
002000*        U1000 -  . . . .                                                 
002100*                                                                         
002200*060928/EÖ  ETRACKER 3449719 EJ SKICKA LOKALA ARTIKLAR TILL USA.          
002300*           USA SKA LIKA ARTIKLAR SOM EUROPA.                             
002400*                                                                         
002500                                                                          
002600     SKIP3                                                                
002700 ENVIRONMENT DIVISION.                                                    
002800     SKIP2                                                                
002900 INPUT-OUTPUT SECTION.                                                    
003000                                                                          
003100 FILE-CONTROL.                                                            
003200     SKIP2                                                                
003300*          --- TRANSAR SOM SKALL TILL MB                                  
003400     SELECT W33539                     ASSIGN TO W33540D1.                
003500     SKIP2                                                                
003600*          --- INNEH BELEVART                                             
003700     SELECT W91044                     ASSIGN TO W33540D2.                
003800     SKIP2                                                                
003900*          --- INFOFIL TILL MB (ALLA UTOM USA)                            
004000     SELECT W33540                     ASSIGN TO W33540D3.                
004100     SKIP2                                                                
004200*          --- TOT ANTAL ARTIKLAR (EN POST I FILEN, ÖVRIGA)               
004300     SELECT W33541                     ASSIGN TO W33540D4.                
004400*          --- INFOFIL TILL MB I USA                                      
004500     SELECT W33542                     ASSIGN TO W33540D5.                
004600     EJECT                                                                
004700 DATA DIVISION.                                                           
004800     SKIP3                                                                
004900 FILE SECTION.                                                            
005000     SKIP3                                                                
005100 FD  W33539                                                               
005200     RECORDING       V                                                    
005300     BLOCK CONTAINS  0.                                                   
005400     SKIP2                                                                
005500*01  -COPY W335401A     -L.                                               
005600     SKIP2                                                                
005700*01  -COPY W335402A     -L.                                               
005800     SKIP2                                                                
005900*01  -COPY W335405A     -L.                                               
006000     SKIP3                                                                
006100 FD  W91044                                                               
006200     RECORDING       F                                                    
006300     BLOCK CONTAINS  0.                                                   
006400     SKIP2                                                                
006500*01  -COPY W91044L1      -L.                                              
006600     SKIP3                                                                
006700 FD  W33540                                                               
006800     RECORDING       V                                                    
006900     BLOCK CONTAINS  0.                                                   
007000     SKIP2                                                                
007100*01  POST -COPY W335401A -PRE  UT1-  -L.                                  
007200     SKIP2                                                                
007300*01  POST -COPY W335402A -PRE  UT2-  -L.                                  
007400     SKIP2                                                                
007500*01  POST -COPY W335403A -PRE  UT3-  -L.                                  
007600     SKIP3                                                                
007700 FD  W33541                                                               
007800     RECORDING       F                                                    
007900     BLOCK CONTAINS  0.                                                   
008000     SKIP2                                                                
008100*01  POST -COPY W335400A -PRE  ANTAL-     -L.                             
008200     SKIP3                                                                
008300 FD  W33542                                                               
008400     RECORDING       V                                                    
008500     BLOCK CONTAINS  0.                                                   
008600     SKIP2                                                                
008700*01  POST -COPY W335401A -PRE  USA1-  -L.                                 
008800     SKIP2                                                                
008900*01  POST -COPY W335402A -PRE  USA2-  -L.                                 
009000     SKIP2                                                                
009100*01  POST -COPY W335403A -PRE  USA3-  -L.                                 
009200     EJECT                                                                
009300 WORKING-STORAGE SECTION.                                                 
009400     SKIP2                                                                
009500                                                                          
009600*    -- CHECKED BY WY2000                                                 
009700 77  IDPGM                       PIC X(8)    VALUE 'W3354000'.            
009800 77  JA                          PIC X       VALUE 'J'.                   
009900 77  NEJ                         PIC X       VALUE 'N'.                   
010000 01  WS.                                                                  
010100   03 WS-KDRAB                   PIC X(03)   VALUE SPACE.                 
010200   03 WS-PRARTBEL                PIC S9(7)V9(2)      COMP-3.              
010300   03 WS-BELEV                   PIC X(18).                               
010400 77  POST-RAEKNARE               PIC S9(7)   VALUE ZERO.                  
010500                                                                          
010600                                                                          
010700 77  W33539-EOF-SW               PIC X       VALUE 'N'.                   
010800     88  END-OF-W33539                       VALUE 'J'.                   
010900                                                                          
011000 77  W91044-EOF-SW               PIC X       VALUE 'N'.                   
011100     88  END-OF-W91044                       VALUE 'J'.                   
011200     EJECT                                                                
011300 77  PS-RESERVDEL-TEST           PIC S9(3)   VALUE ZERO.                  
011400     88  TILLBEH-PACKN-MTRL        VALUE                                  
011500                                        +15 +16 +17 +18 +19               
011600                                        +25 +26 +27 +28 +29               
011700                                        +35 +36 +37 +38 +39.              
011800 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
011900 01  FILLER REDEFINES DAGENS-DATUM.                                       
012000     03  DAGENS-DATUM-AAR        PIC 9(2).                                
012100     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
012200     03  DAGENS-DATUM-DAG        PIC 9(2).                                
012300     EJECT                                                                
012400*                                                                         
012500*01  -COPY WWPRODSL                                                       
012600*                                                                         
012700 01  DYNAMISKA-SUBPROGRAM.                                                
012800*                                                                         
012900     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
013000     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
013100     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
013200     SKIP2                                                                
013300*    --- PARAMETRAR TILL ABEND                                            
013400                                                                          
013500 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
013600 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
013700     SKIP2                                                                
013800 01  FELTEXT.                                                             
013900     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
014000     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
014100     EJECT                                                                
014200*    --- PARAMETRAR TILL POSTSUM                                          
014300*                                                                         
014400*01  -COPY W0005   -PRE  POSTSUM-                                         
014500     EJECT                                                                
014600 01  INFO-AREA-START             PIC X(24)   VALUE                        
014700                                 'INFO-AREA-START  '.                     
014800     SKIP2                                                                
014900 01  INFO-AREA                  PIC X(400).                               
015000*01  FILLER -COPY W335401A     -PRE INFO1-   -RED  INFO-AREA              
015100     EJECT                                                                
015200*01  FILLER -COPY W335402A     -PRE INFO2-   -RED  INFO-AREA              
015300*01  FILLER -COPY W335405A     -PRE INFO3-   -RED  INFO-AREA              
015400     EJECT                                                                
015500 01  W910-AREA-START             PIC X(24)   VALUE                        
015600                                 'W910-AREA-START  '.                     
015700     SKIP2                                                                
015800                                                                          
015900*01  AREA -COPY W91044L1     -PRE W910-                                   
016000                                                                          
016100     EJECT                                                                
016200 01  ANTAL-AREA-START            PIC X(24)   VALUE                        
016300                                 'ANTAL-AREA-START  '.                    
016400     SKIP2                                                                
016500                                                                          
016600*01  AREA -COPY W335400A    -PRE ANTAL-                                   
016700                                                                          
016800 01  ANTUSA-AREA-START           PIC X(24)   VALUE                        
016900                                 'ANTUSA-AREA-START '.                    
017000     SKIP2                                                                
017100                                                                          
017200*01  AREA -COPY W335400A    -PRE ANTUSA-                                  
017300                                                                          
017400     EJECT                                                                
017500 01  UT-AREA-START               PIC X(24)   VALUE                        
017600                                 'UT-AREA-START  '.                       
017700     SKIP2                                                                
017800 01  UT-AREA.                                                             
017900     03  UT-IDPTYP               PIC X(3).                                
018000     03  FILLER                  PIC X(400).                              
018100*01  FILLER -COPY W335401A     -PRE UT1-   -RED  UT-AREA                  
018200*01  FILLER -COPY W335402A     -PRE UT2-   -RED  UT-AREA                  
018300*01  FILLER -COPY W335403A     -PRE UT3-   -RED  UT-AREA                  
018400 01  USA-AREA-START              PIC X(24)   VALUE                        
018500                                 'USA-AREA-START '.                       
018600     SKIP2                                                                
018700 01  USA-AREA.                                                            
018800     03  UT-IDPTYP               PIC X(3).                                
018900     03  FILLER                  PIC X(400).                              
019000*01  FILLER -COPY W335401A     -PRE USA1-  -RED  USA-AREA                 
019100*01  FILLER -COPY W335402A     -PRE USA2-  -RED  USA-AREA                 
019200*01  FILLER -COPY W335403A     -PRE USA3-  -RED  USA-AREA                 
019300                                                                          
019400 PROCEDURE DIVISION.                                                      
019500                                                                          
019600     SKIP2                                                                
019700     PERFORM A-INIT                                                       
019800     PERFORM S02-LAES-W33539                                              
019900     PERFORM S03-LAES-W91044                                              
020000     PERFORM UNTIL END-OF-W33539                                          
020100                                                                          
020200         IF INFO1-IDPTYP = '401'                                          
020300*           HÄMTA ARTBEN ENL LEVERANTÖREN.                                
020400            PERFORM B-TAG-FRAM-BELEVART                                   
020500*           KOLLA PRODUKTSLAG                                             
020600            PERFORM E-FLYTT-T-UT-SKRIV-401                                
020700* - - - -USA                                                              
020800            MOVE INFO1-KDPRODSL TO TEST-KDPRODSL                          
020900            IF KDPRODSL-LOCAL                                             
021000               CONTINUE                                                   
021100            ELSE                                                          
021200* - - - -USA + ÖVRIGA                                                     
021300               PERFORM S17-SKRIV-W33542-401                               
021400               PERFORM S11-SKRIV-W33540-401                               
021500            END-IF                                                        
021600         ELSE                                                             
021700            PERFORM F-FLYTT-T-UT-SKRIV-402-403                            
021800         END-IF                                                           
021900       PERFORM S02-LAES-W33539                                            
022000     END-PERFORM                                                          
022100     PERFORM S14-SKRIV-W33541                                             
022200                                                                          
022300                                                                          
022400     PERFORM Z-FINIT                                                      
022500                                                                          
022600     MOVE ZERO TO RETURN-CODE                                             
022700     GOBACK                                                               
022800     .                                                                    
022900     EJECT                                                                
023000 A-INIT SECTION.                                                          
023100                                                                          
023200     OPEN INPUT  W33539                                                   
023300                 W91044                                                   
023400                                                                          
023500     OPEN OUTPUT W33540                                                   
023600                 W33541                                                   
023700                 W33542                                                   
023800                                                                          
023900     SKIP2                                                                
024000     ACCEPT DAGENS-DATUM  FROM DATE                                       
024100     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
024200     .                                                                    
024300     EJECT                                                                
024400 B-TAG-FRAM-BELEVART SECTION.                                             
024500     SKIP2                                                                
024600     PERFORM UNTIL W910-IDARTNR > INFO1-IDARTNR                           
024700             OR                                                           
024800                  (W910-IDARTNR = INFO1-IDARTNR AND                       
024900                   W910-IDLEVNR = INFO1-IDLEVNR)                          
025000       PERFORM S03-LAES-W91044                                            
025100     END-PERFORM                                                          
025200     IF (W910-IDARTNR = INFO1-IDARTNR AND                                 
025300         W910-IDLEVNR = INFO1-IDLEVNR)                                    
025400       MOVE W910-BELEV TO UT1-BELEVART                                    
025500                         USA1-BELEVART                                    
025600        MOVE W910-BELEV (1:18) TO WS-BELEV                                
025700     ELSE                                                                 
025800       MOVE SPACE         TO UT1-BELEVART                                 
025900                            USA1-BELEVART                                 
026000                            WS-BELEV                                      
026100     END-IF                                                               
026200     .                                                                    
026300     EJECT                                                                
026400 E-FLYTT-T-UT-SKRIV-401      SECTION.                                     
026500     SKIP2                                                                
026600*    FLYTTA ALLA FÄLT SOM INTE ÄR ÄNDRADE                                 
026700     MOVE INFO1-IDPTYP     TO UT1-IDPTYP                                  
026800                             USA1-IDPTYP                                  
026900     MOVE INFO1-IDVTYP     TO UT1-IDVTYP                                  
027000                             USA1-IDVTYP                                  
027100     MOVE INFO1-IDARTNR    TO UT1-IDARTNR                                 
027200                             USA1-IDARTNR                                 
027300     MOVE INFO1-IDFKNGRP   TO UT1-IDFKNGRP                                
027400                             USA1-IDFKNGRP                                
027500     MOVE INFO1-KDSRA      TO UT1-KDSRA                                   
027600                             USA1-KDSRA                                   
027700     MOVE INFO1-KVQPACK-0  TO UT1-KVQPACK-0                               
027800                             USA1-KVQPACK-0                               
027900     MOVE INFO1-KDARTURS-NUM  TO UT1-KDARTURS-NUM                         
028000                             USA1-KDARTURS-NUM                            
028100     MOVE INFO1-KDPRODSL   TO UT1-KDPRODSL                                
028200                             USA1-KDPRODSL                                
028300*PSLLOC ANVÄNDS BARA I MB M5 VCNA                                         
028400*MEN FINNS I TRANSEN TILL ALLA MB                                         
028500     MOVE INFO1-KDPSLLOC   TO UT1-KDPSLLOC                                
028600                              USA1-KDPSLLOC                               
028700     MOVE INFO1-VLARTNTO   TO UT1-VLARTNTO                                
028800                             USA1-VLARTNTO                                
028900     MOVE INFO1-VKART      TO UT1-VKART                                   
029000                             USA1-VKART                                   
029100     MOVE INFO1-KDVSOP     TO UT1-KDVSOP                                  
029200                             USA1-KDVSOP                                  
029300     MOVE INFO1-IDSTATNR   TO UT1-IDSTATNR                                
029400                             USA1-IDSTATNR                                
029500     MOVE INFO1-KDSORT     TO UT1-KDSORT                                  
029600                             USA1-KDSORT                                  
029700     MOVE INFO1-KDERS      TO UT1-KDERS                                   
029800                             USA1-KDERS                                   
029900     MOVE INFO1-KDBPSR     TO UT1-KDBPSR                                  
030000                             USA1-KDBPSR                                  
030100*KDBBCL BUYBACK ANVÄNDS EJ I NÅGOT MB.                                    
030200     MOVE ZERO             TO UT1-KDBBCL                                  
030300                             USA1-KDBBCL                                  
030400     MOVE INFO1-IDLEVNR    TO UT1-IDLEVNR                                 
030500                             USA1-IDLEVNR                                 
030600     MOVE INFO1-PRARTSJK   TO UT1-PRARTSJK                                
030700                             USA1-PRARTSJK                                
030800     MOVE INFO1-PRARTSTD   TO UT1-PRARTSTD                                
030900                             USA1-PRARTSTD                                
031000     MOVE INFO1-FLIART     TO UT1-FLIART                                  
031100                             USA1-FLIART                                  
031200     MOVE INFO1-IDPROJ     TO UT1-IDPROJ                                  
031300                             USA1-IDPROJ                                  
031400     MOVE INFO1-IDAO(1)    TO UT1-IDAO(1)                                 
031500                             USA1-IDAO(1)                                 
031600     MOVE INFO1-IDAO(2)    TO UT1-IDAO(2)                                 
031700                             USA1-IDAO(2)                                 
031800     MOVE INFO1-TIFINLEV   TO UT1-TIFINLEV                                
031900                             USA1-TIFINLEV                                
032000     MOVE INFO1-IDANSK     TO UT1-IDANSK                                  
032100                             USA1-IDANSK                                  
032200     MOVE INFO1-KVPB       TO UT1-KVPB                                    
032300                             USA1-KVPB                                    
032400     MOVE INFO1-KDVVKL     TO UT1-KDVVKL                                  
032500                             USA1-KDVVKL                                  
032600     MOVE INFO1-KDTIPPR    TO UT1-KDTIPPR                                 
032700                             USA1-KDTIPPR                                 
032800     MOVE INFO1-IDINK      TO UT1-IDINK                                   
032900                             USA1-IDINK                                   
033000     MOVE INFO1-TIREGDAT   TO UT1-TIREGDAT                                
033100                             USA1-TIREGDAT                                
033200     MOVE INFO1-KDUART     TO UT1-KDUART                                  
033300                             USA1-KDUART                                  
033400     MOVE INFO1-IDARTNR-MOTSV TO UT1-IDARTNR-MOTSV                        
033500                                USA1-IDARTNR-MOTSV                        
033600     MOVE INFO1-PRINK      TO UT1-PRINK                                   
033700                             USA1-PRINK                                   
033800     MOVE INFO1-IDRITN     TO UT1-IDRITN                                  
033900                             USA1-IDRITN                                  
034000     MOVE INFO1-PRHANTK    TO UT1-PRHANTK                                 
034100                             USA1-PRHANTK                                 
034200     MOVE INFO1-KDAGE      TO UT1-KDAGE                                   
034300                             USA1-KDAGE                                   
034400     MOVE SPACE            TO UT1-SLAG-IDLEVNR                            
034500                              USA1-SLAG-IDLEVNR                           
034600     MOVE INFO1-IDKAT(1)   TO UT1-IDKAT(1)                                
034700                              USA1-IDKAT(1)                               
034800     MOVE INFO1-IDKAT(2)   TO UT1-IDKAT(2)                                
034900                              USA1-IDKAT(2)                               
035000     MOVE INFO1-IDKAT(3)   TO UT1-IDKAT(3)                                
035100                              USA1-IDKAT(3)                               
035200     MOVE SPACE            TO UT1-KDRAB                                   
035300                              USA1-KDRAB                                  
035400     MOVE ZERO             TO UT1-PRARTBEL                                
035500                              USA1-PRARTBEL                               
035600     MOVE INFO1-FLLSRDEL   TO UT1-FLLSRDEL                                
035700                              USA1-FLLSRDEL                               
035800     MOVE INFO1-IDPROJUP   TO UT1-IDPROJUP                                
035900                              USA1-IDPROJUP                               
036000     MOVE INFO1-FLGEMFMC   TO UT1-FLGEMFMC                                
036100                              USA1-FLGEMFMC                               
036200     MOVE INFO1-TIURPROD   TO UT1-TIURPROD                                
036300                              USA1-TIURPROD                               
036400     .                                                                    
036500     EJECT                                                                
036600 F-FLYTT-T-UT-SKRIV-402-403  SECTION.                                     
036700     SKIP2                                                                
036800     IF INFO1-IDPTYP = '402'                                              
036900       IF KDPRODSL-LOCAL                                                  
037000*- - VCNA SKA EJ LÄNGRE HA LOKAL ARTIKLAR/EÖ 060928                       
037100         CONTINUE                                                         
037200       ELSE                                                               
037300*- - VCNA + ÖVRIGA                                                        
037400         PERFORM S12-SKRIV-W33540-402                                     
037500         PERFORM S15-SKRIV-W33542-402                                     
037600       END-IF                                                             
037700     ELSE                                                                 
037800       IF KDPRODSL-LOCAL                                                  
037900*- - VCNA SKA EJ LÄNGRE HA LOKAL ARTIKLAR/EÖ 060928                       
038000         CONTINUE                                                         
038100       ELSE                                                               
038200*- - VCNA + ÖVRIGA                                                        
038300         PERFORM S13-SKRIV-W33540-403                                     
038400         PERFORM S16-SKRIV-W33542-403                                     
038500       END-IF                                                             
038600     END-IF                                                               
038700     .                                                                    
038800     EJECT                                                                
038900 Z-FINIT SECTION.                                                         
039000     CLOSE W33539                                                         
039100           W91044                                                         
039200           W33540                                                         
039300           W33541                                                         
039400           W33542                                                         
039500                                                                          
039600     SKIP2                                                                
039700     MOVE 'S' TO POSTSUM-OPKOD                                            
039800     CALL POSTSUM USING POSTSUM-PARM                                      
039900     .                                                                    
040000     EJECT                                                                
040100 S02-LAES-W33539  SECTION.                                                
040200     SKIP2                                                                
040300     READ W33539 INTO INFO-AREA                                           
040400     AT END                                                               
040500*       MOVE HIGH-VALUE TO INFO-ID                                        
040600        SET END-OF-W33539 TO TRUE                                         
040700                                                                          
040800     NOT AT END                                                           
040900        MOVE 'W33539' TO POSTSUM-FDNAMN                                   
041000        MOVE 'W33540D1' TO POSTSUM-DDNAMN2                                
041100        MOVE 'INFO'      TO POSTSUM-TRANSTYP                              
041200        CALL POSTSUM USING POSTSUM-PARM                                   
041300     END-READ                                                             
041400     .                                                                    
041500     EJECT                                                                
041600 S03-LAES-W91044  SECTION.                                                
041700     SKIP2                                                                
041800     READ W91044 INTO W910-AREA                                           
041900     AT END                                                               
042000*       MOVE HIGH-VALUE TO W910-ID                                        
042100        MOVE 999999999  TO W910-IDARTNR                                   
042200        SET END-OF-W91044 TO TRUE                                         
042300                                                                          
042400     NOT AT END                                                           
042500        MOVE 'W91044' TO POSTSUM-FDNAMN                                   
042600        MOVE 'W33540D2' TO POSTSUM-DDNAMN2                                
042700        MOVE 'W910'      TO POSTSUM-TRANSTYP                              
042800        CALL POSTSUM USING POSTSUM-PARM                                   
042900     END-READ                                                             
043000     .                                                                    
043100     EJECT                                                                
043200 S11-SKRIV-W33540-401 SECTION.                                            
043300     SKIP2                                                                
043400     WRITE UT1-POST FROM UT-AREA                                          
043500                                                                          
043600     ADD +1 TO POST-RAEKNARE                                              
043700     MOVE '401' TO POSTSUM-TRANSTYP                                       
043800     MOVE 'W33540' TO POSTSUM-FDNAMN                                      
043900     MOVE 'W33540D3' TO POSTSUM-DDNAMN2                                   
044000     CALL POSTSUM USING POSTSUM-PARM                                      
044100     .                                                                    
044200 S12-SKRIV-W33540-402 SECTION.                                            
044300     SKIP2                                                                
044400***                                                                       
044500     MOVE INFO2-IDPTYP   TO UT2-IDPTYP                                    
044600     MOVE INFO2-IDVTYP   TO UT2-IDVTYP                                    
044700     MOVE INFO2-TEORSAK  TO UT2-TEORSAK                                   
044800     MOVE INFO2-TEARTNOT-3 TO UT2-TEARTNOT-3                              
044900     MOVE INFO2-TEARTNOT-7 TO UT2-TEARTNOT-7                              
045000***                                                                       
045100     WRITE UT2-POST FROM UT-AREA                                          
045200                                                                          
045300     ADD +1 TO POST-RAEKNARE                                              
045400     MOVE '402' TO POSTSUM-TRANSTYP                                       
045500     MOVE 'W33540' TO POSTSUM-FDNAMN                                      
045600     MOVE 'W33540D3' TO POSTSUM-DDNAMN2                                   
045700     CALL POSTSUM USING POSTSUM-PARM                                      
045800     .                                                                    
045900 S13-SKRIV-W33540-403 SECTION.                                            
046000     SKIP2                                                                
046100     MOVE INFO3-IDPTYP    TO UT3-IDPTYP                                   
046200     MOVE INFO3-IDVTYP    TO UT3-IDVTYP                                   
046300     MOVE INFO3-BEART(1)  TO UT3-BEART(1)                                 
046400     MOVE INFO3-BEART(2)  TO UT3-BEART(2)                                 
046500     WRITE UT3-POST FROM UT-AREA                                          
046600                                                                          
046700     ADD +1 TO POST-RAEKNARE                                              
046800     MOVE '403' TO POSTSUM-TRANSTYP                                       
046900     MOVE 'W33540' TO POSTSUM-FDNAMN                                      
047000     MOVE 'W33540D3' TO POSTSUM-DDNAMN2                                   
047100     CALL POSTSUM USING POSTSUM-PARM                                      
047200     .                                                                    
047300     EJECT                                                                
047400 S14-SKRIV-W33541 SECTION.                                                
047500     SKIP2                                                                
047600     MOVE '400'          TO ANTAL-IDPTYP                                  
047700     MOVE 'A'            TO ANTAL-IDVTYP                                  
047800     MOVE POST-RAEKNARE  TO ANTAL-KVPOST                                  
047900     WRITE ANTAL-POST FROM ANTAL-AREA                                     
048000                                                                          
048100     MOVE 'ANT' TO POSTSUM-TRANSTYP                                       
048200     MOVE 'W33541' TO POSTSUM-FDNAMN                                      
048300     MOVE 'W33540D4' TO POSTSUM-DDNAMN2                                   
048400     CALL POSTSUM USING POSTSUM-PARM                                      
048500     .                                                                    
048600     EJECT                                                                
048700 S15-SKRIV-W33542-402 SECTION.                                            
048800     SKIP2                                                                
048900***                                                                       
049000     MOVE INFO2-IDPTYP   TO USA2-IDPTYP                                   
049100     MOVE INFO2-IDVTYP   TO USA2-IDVTYP                                   
049200     MOVE INFO2-TEORSAK  TO USA2-TEORSAK                                  
049300     MOVE INFO2-TEARTNOT-3 TO USA2-TEARTNOT-3                             
049400     MOVE INFO2-TEARTNOT-7 TO USA2-TEARTNOT-7                             
049500***                                                                       
049600     WRITE USA2-POST FROM USA-AREA                                        
049700                                                                          
049800     MOVE '402' TO POSTSUM-TRANSTYP                                       
049900     MOVE 'W33542' TO POSTSUM-FDNAMN                                      
050000     MOVE 'W33540D5' TO POSTSUM-DDNAMN2                                   
050100     CALL POSTSUM USING POSTSUM-PARM                                      
050200     .                                                                    
050300 S16-SKRIV-W33542-403 SECTION.                                            
050400     SKIP2                                                                
050500*** BEART ÄR PÅ AMERIKANS ENGELSKA ****                                   
050600     MOVE INFO3-IDPTYP    TO USA3-IDPTYP                                  
050700     MOVE INFO3-IDVTYP    TO USA3-IDVTYP                                  
050800     MOVE INFO3-BEART(1)  TO USA3-BEART(1)                                
050900     MOVE INFO3-BEART(2)  TO USA3-BEART(2)                                
051000     WRITE USA3-POST FROM USA-AREA                                        
051100                                                                          
051200     MOVE '403' TO POSTSUM-TRANSTYP                                       
051300     MOVE 'W33542' TO POSTSUM-FDNAMN                                      
051400     MOVE 'W33540D5' TO POSTSUM-DDNAMN2                                   
051500     CALL POSTSUM USING POSTSUM-PARM                                      
051600     .                                                                    
051700     EJECT                                                                
051800 S17-SKRIV-W33542-401 SECTION.                                            
051900     SKIP2                                                                
052000     WRITE USA1-POST FROM USA-AREA                                        
052100                                                                          
052200     MOVE '401' TO POSTSUM-TRANSTYP                                       
052300     MOVE 'W33542' TO POSTSUM-FDNAMN                                      
052400     MOVE 'W33540D5' TO POSTSUM-DDNAMN2                                   
052500     CALL POSTSUM USING POSTSUM-PARM                                      
052600     .                                                                    
052700     EJECT                                                                
