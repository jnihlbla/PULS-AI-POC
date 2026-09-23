000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W4120300.                                                
000300 AUTHOR.         KJELLSON GÖRAN.                                          
000400 DATE-WRITTEN.   14/09/04.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNKTION:                                                            
000800*        KONSOLIDERING AV PIE SW-ORDER                                    
000900*                                                                         
001000*        PROGRAMMET LÄSER      WDQ2                                       
001100*                              WDB2                                       
001200*                              WDK6                                       
001300*                                                                         
001400*    ABENDKODER:                                                          
001500*        U0016 -  . . . .                                                 
001600*        U1000 -  . . . .                                                 
001700*                                                                         
001800* RECOMPILING PGM FOR ADDING 1090,1958 TO LEVEL DIST36-EXT-WARRANT        
001900*  IN COPYBOOK WWDIST36.                                                  
002000                                                                          
002100 ENVIRONMENT DIVISION.                                                    
002200 INPUT-OUTPUT SECTION.                                                    
002300                                                                          
002400 FILE-CONTROL.                                                            
002500                                                                          
002600*          --- PIE SW-ORDERNUMMER IN-                                     
002700     SELECT W41206I                    ASSIGN TO W41203D1.                
002800                                                                          
002900*          --- PIE SW-ORDERNUMMER UT-                                     
003000     SELECT W41206O                    ASSIGN TO W41203D2.                
003100                                                                          
003200*          --- SW ORDER FRÅN PIE                                          
003300     SELECT W46333                     ASSIGN TO W41203D3.                
003400                                                                          
003500*          --- WDI111 POSTER FÖR UPPDATERING                              
003600     SELECT W41227                     ASSIGN TO W41203D4.                
003700                                                                          
003800                                                                          
003900 DATA DIVISION.                                                           
004000 FILE SECTION.                                                            
004100                                                                          
004200 FD  W41206I                                                              
004300     RECORDING       F                                                    
004400     BLOCK CONTAINS  0.                                                   
004500                                                                          
004600*01  -COPY W41206      -L.                                                
004700                                                                          
004800                                                                          
004900 FD  W41206O                                                              
005000     RECORDING       F                                                    
005100     BLOCK CONTAINS  0.                                                   
005200                                                                          
005300*01  POST -COPY W41206 -PRE  UT-  -L.                                     
005400                                                                          
005500                                                                          
005600 FD  W46333                                                               
005700     RECORDING       V                                                    
005800     BLOCK CONTAINS  0.                                                   
005900                                                                          
006000*01  -COPY W46333       -L.                                               
006100                                                                          
006200                                                                          
006300 FD  W41227                                                               
006400     RECORDING       F                                                    
006500     BLOCK CONTAINS  0.                                                   
006600                                                                          
006700*01  POST -COPY W46333 -PRE  PIE-  -L.                                    
006800                                                                          
006900                                                                          
007000                                                                          
007100 WORKING-STORAGE SECTION.                                                 
007200                                                                          
007300 77  IDPGM                       PIC X(8)    VALUE 'W4120300'.            
007400 77  CURRENT-SECTION             PIC X(16)   VALUE SPACE.                 
007500 77  CURRENT-IMS-SECTION         PIC X(16)   VALUE SPACE.                 
007600 77  CURRENT-MAX-OMST-IDORDNR    PIC 9(5)    VALUE ZERO.                  
007700 77  CURRENT-MAX-IDORDNR         PIC 9(5)    VALUE ZERO.                  
007800 77  JA                          PIC X       VALUE 'J'.                   
007900 77  NEJ                         PIC X       VALUE 'N'.                   
008000 77  BORT                        PIC X       VALUE 'D'.                   
008100 77  EXTENDED-WARRANTY           PIC X(8)    VALUE 'EXTWA   '.            
008200                                                                          
008300 77  W41206-EOF-SW               PIC X       VALUE 'N'.                   
008400     88  END-OF-W41206                       VALUE 'J'.                   
008500                                                                          
008600 77  W46333-EOF-SW               PIC X       VALUE 'N'.                   
008700     88  END-OF-W46333                       VALUE 'J'.                   
008800                                                                          
008900 77  SERVICEAVTAL-SW             PIC X       VALUE 'N'.                   
009000     88  SERVICEAVTAL                        VALUE 'J'.                   
009100     88  SERVICEAVTAL-BORT                   VALUE 'D'.                   
009200                                                                          
009300 77  TEST-IDFKNGRP               PIC S9(5)   COMP-3.                      
009400     88  FKNGRP-SERVICEAGREEMENT             VALUE 1738 1798.             
009500     88  FKNGRP-EXT-WARRANTY                 VALUE 1728 1788.             
009600                                                                          
009700                                                                          
009800 01  DYNAMISKA-SUBPROGRAM.                                                
009900*                                                                         
010000     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
010100     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
010200     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
010300     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
010400     03  W009CIA                 PIC X(8)    VALUE 'W009CIA '.            
010500                                                                          
010600                                                                          
010700*    --- PARAMETRAR TILL ABEND                                            
010800                                                                          
010900 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
011000 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
011100 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
011200                                                                          
011300 01  FELTEXT.                                                             
011400     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
011500     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
011600                                                                          
011700                                                                          
011800                                                                          
011900*    --- PARAMETRAR TILL POSTSUM                                          
012000*                                                                         
012100 01  FILLER                      PIC X(16) VALUE 'POSTSUM '.              
012200*01  -COPY W0005   -PRE  POSTSUM-                                         
012300                                                                          
012400 01  FILLER                      PIC X(16) VALUE 'CIA-AREA'.              
012500*   -COPY W009CIA                                                         
012600                                                                          
012700                                                                          
012800 01  IN-AREA-START               PIC X(24)   VALUE                        
012900                                 '06IN-AREA-START'.                       
013000*01  AREA -COPY W41206     -PRE IN-                                       
013100                                                                          
013200                                                                          
013300 01  UT-AREA-START               PIC X(24)   VALUE                        
013400                                 '06UT-AREA-START'.                       
013500                                                                          
013600*01  AREA -COPY W41206     -PRE UT-                                       
013700                                                                          
013800                                                                          
013900                                                                          
014000 01  PIE-IN-AREA-START           PIC X(24)   VALUE                        
014100                                 'PIEIN-AREA-START'.                      
014200                                                                          
014300*01  AREA -COPY W46333      -PRE PIEIN-                                   
014400                                                                          
014500                                                                          
014600 01  PIE-UT-AREA-START           PIC X(24)   VALUE                        
014700                                 'PIEUT-AREA-START'.                      
014800                                                                          
014900*01  AREA -COPY W46333      -PRE PIEUT-                                   
015000                                                                          
015100                                                                          
015200                                                                          
015300                                                                          
015400*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
015500*                                                                         
015600 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
015700                                                                          
015800 01  NYCKLAR-TILL-DLI.                                                    
015900     03  W-WDQ2CSEQ-X.                                                    
016000         05  W-WDQ2C-IDDISTR      PIC S9(5)   COMP-3 VALUE +0.            
016100         05  W-WDQ2C-IDKUNDNR     PIC S9(7)   COMP-3 VALUE +0.            
016200         05  W-WDQ2C-IDKUNDRF.                                            
016300             07  W-WDQ2C-IDORDNR7 PIC  9(7)          VALUE ZERO.          
016400             07  FILLER           PIC  X(3)          VALUE SPACE.         
016500                                                                          
016600     03  W-IDGMT-X.                                                       
016700         05  W-IDDISTR-B2        PIC S9(5)   VALUE ZERO COMP-3.           
016800         05  W-IDKUNDNR-B2       PIC S9(7)   VALUE ZERO COMP-3.           
016900                                                                          
017000     03  W-IDARTNR-X.                                                     
017100         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
017200                                                                          
017300                                                                          
017400*    --- STATUS-KOD FRÅN IMS                                              
017500 01  STATUS-WS                   PIC XX.                                  
017600     88  SEGMENT-FINNS                       VALUE '  '.                  
017700     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
017800                                                                          
017900 01  GODK-STATUSKODER.                                                    
018000     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
018100                                                                          
018200 01  ALL-SSA.                                                             
018300     03 SSA1                     PIC X(64).                               
018400     03 SSA2                     PIC X(64).                               
018500                                                                          
018600                                                                          
018700                                                                          
018800*    --- IMS FUNKTIONSKODER                                               
018900*01  -COPY W0003                                                          
019000                                                                          
019100*    ---  DLI INPUT-OUTPUT AREA                                           
019200                                                                          
019300 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDQ201'.                      
019400 01  DLI-IO-WDQ201.                                                       
019500*    03  -COPY WDQ201                                                     
019600                                                                          
019700 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDB201'.                      
019800 01  DLI-IO-WDB201.                                                       
019900*    03  -COPY WDB201                                                     
020000                                                                          
020100 01  FILLER                      PIC X(16)   VALUE 'WDK601-AREA'.         
020200 01  DLI-IO-AREA-WDK601.                                                  
020300     03  WDK601.                                                          
020400*        05  -COPY WDK601                                                 
020500                                                                          
020600 01  FILLER                      PIC X(16)   VALUE 'WDK611-AREA'.         
020700 01  DLI-IO-AREA-WDK611.                                                  
020800     03  WDK611.                                                          
020900*        05  -COPY WDK611                                                 
021000                                                                          
021100                                                                          
021200                                                                          
021300 LINKAGE SECTION.                                                         
021400                                                                          
021500*01  -COPY W0008  -PRE WDQ2-                                              
021600     05  FILLER                  PIC X.                                   
021700                                                                          
021800*01  -COPY W0008  -PRE WDB2-                                              
021900     05  FILLER                  PIC X.                                   
022000                                                                          
022100*01  -COPY W0008  -PRE WDK6-                                              
022200     05  FILLER                  PIC X.                                   
022300                                                                          
022400                                                                          
022500                                                                          
022600 PROCEDURE DIVISION  USING WDQ2-PCB WDB2-PCB WDK6-PCB.                    
022700 MAIN SECTION.                                                            
022800     ENTRY 'DLITCBL' USING WDQ2-PCB WDB2-PCB WDK6-PCB.                    
022900                                                                          
023000                                                                          
023100     PERFORM A-INIT                                                       
023200     PERFORM B-HAMTA-ORDERNUMMER                                          
023300                                                                          
023400     PERFORM S02-LAES-W46333                                              
023500                                                                          
023600     PERFORM UNTIL END-OF-W46333                                          
023700                                                                          
023800        PERFORM C-KOLLA-SKRIV-PULS-ORDERRAD                               
023900        PERFORM S02-LAES-W46333                                           
024000     END-PERFORM                                                          
024100                                                                          
024200     PERFORM D-SPARA-ORDERNUMMER                                          
024300     PERFORM Z-FINIT                                                      
024400                                                                          
024500     MOVE ZERO TO RETURN-CODE                                             
024600     GOBACK                                                               
024700     .                                                                    
024800                                                                          
024900                                                                          
025000 A-INIT SECTION.                                                          
025100     MOVE 'A-INIT          ' TO CURRENT-SECTION                           
025200                                                                          
025300     OPEN INPUT  W41206I                                                  
025400                 W46333                                                   
025500                                                                          
025600     OPEN OUTPUT W41206O                                                  
025700                 W41227                                                   
025800                                                                          
025900     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
026000     .                                                                    
026100                                                                          
026200                                                                          
026300 B-HAMTA-ORDERNUMMER SECTION.                                             
026400     MOVE 'B-HAMTA-ORDERNR ' TO CURRENT-SECTION                           
026500                                                                          
026600     PERFORM S01-LAES-W41206                                              
026700     IF END-OF-W41206                                                     
026800        MOVE 'W41206 SAKNAS' TO FELTEXT-STR                               
026900        PERFORM S99-ABEND                                                 
027000     ELSE                                                                 
027100        MOVE IN-IDORDNR-AKT  TO W-WDQ2C-IDORDNR7                          
027200     END-IF                                                               
027300     .                                                                    
027400                                                                          
027500                                                                          
027600 C-KOLLA-SKRIV-PULS-ORDERRAD SECTION.                                     
027700     MOVE 'C-KOLLA-SKRIV   ' TO CURRENT-SECTION                           
027800                                                                          
027900     IF PIEIN-KDSOFT = 'I'                                                
028000        MOVE PIEIN-AREA             TO PIEUT-AREA                         
028100     ELSE                                                                 
028200        MOVE PIEIN-IDARTPRE   TO CIA-IDARTPRE-IN                          
028300        MOVE PIEIN-IDARTBET   TO CIA-IDARTBET-IN                          
028400        CALL W009CIA USING CIA-W009CIA                                    
028500        IF CIA-KDSVAR = 'F'                                               
028600           MOVE NEJ TO SERVICEAVTAL-SW                                    
028700        ELSE                                                              
028800           MOVE CIA-IDARTNR TO W-IDARTNR                                  
028900           PERFORM IMS-GU-WDK601                                          
029000           IF SEGMENT-FINNS                                               
029100              MOVE ART-IDFKNGRP TO TEST-IDFKNGRP                          
029200              IF FKNGRP-EXT-WARRANTY OR FKNGRP-SERVICEAGREEMENT           
029300                IF FKNGRP-SERVICEAGREEMENT                                
029400                   MOVE JA      TO SERVICEAVTAL-SW                        
029500                ELSE                                                      
029600                   MOVE NEJ     TO SERVICEAVTAL-SW                        
029700                END-IF                                                    
029800              ELSE                                                        
029900                MOVE NEJ           TO SERVICEAVTAL-SW                     
030000*               ORDER WILL GO TO PULS                                     
030100              END-IF                                                      
030200           ELSE                                                           
030300              MOVE NEJ           TO SERVICEAVTAL-SW                       
030400*             ORDERN WILL GO TO PULS                                      
030500           END-IF                                                         
030600        END-IF                                                            
030700                                                                          
030800                                                                          
030900        IF PIEIN-KDSOFT = '2'                                             
031000           IF SERVICEAVTAL                                                
031100              CONTINUE                                                    
031200           ELSE                                                           
031300              IF PIEIN-IDDISTR NOT = W-IDDISTR-B2                         
031400                 MOVE PIEIN-IDDISTR TO W-IDDISTR-B2                       
031500                 MOVE PIEIN-IDKUNDNR TO W-IDKUNDNR-B2                     
031600                 PERFORM IMS-GU-WDB201                                    
031700              END-IF                                                      
031800                                                                          
031900              IF GMT-FLSWCONS = NEJ                                       
032000              OR PIEIN-IDDISTR NOT = W-WDQ2C-IDDISTR                      
032100              OR PIEIN-IDKUNDNR NOT = W-WDQ2C-IDKUNDNR                    
032200                 MOVE PIEIN-IDDISTR TO W-WDQ2C-IDDISTR                    
032300                 MOVE PIEIN-IDKUNDNR TO W-WDQ2C-IDKUNDNR                  
032400                 IF GMT-FLSWCONS = JA                                     
032500                    MOVE IN-IDORDNR-AKT TO W-WDQ2C-IDORDNR7               
032600                 ELSE                                                     
032700                    IF PIEIN-IDDISTR = PIEUT-IDDISTR                      
032800                    AND PIEIN-IDKUNDNR = PIEUT-IDKUNDNR                   
032900                       PERFORM S20-NYTT-ORDERNUMMER                       
033000                    ELSE                                                  
033100                       PERFORM CA-SPARA-EV-NYTT-ORDERNR                   
033200                       MOVE IN-IDORDNR-AKT TO W-WDQ2C-IDORDNR7            
033300                    END-IF                                                
033400                 END-IF                                                   
033500                                                                          
033600                 PERFORM CB-KOLLA-Q2-ORDER                                
033700              END-IF                                                      
033800           END-IF                                                         
033900                                                                          
034000           MOVE PIEIN-AREA             TO PIEUT-AREA                      
034100           IF SERVICEAVTAL                                                
034200              MOVE JA                  TO PIEUT-FLSERV                    
034300              MOVE ZERO                TO PIEUT-IDORDNR7                  
034400           ELSE                                                           
034500              MOVE W-WDQ2C-IDORDNR7    TO PIEUT-IDORDNR7                  
034600           END-IF                                                         
034700        ELSE                                                              
034800           MOVE PIEIN-AREA             TO PIEUT-AREA                      
034900           IF SERVICEAVTAL                                                
035000              MOVE JA                  TO PIEUT-FLSERV                    
035100           END-IF                                                         
035200        END-IF                                                            
035300     END-IF                                                               
035400                                                                          
035500     PERFORM S13-SKRIV-W41227                                             
035600     .                                                                    
035700                                                                          
035800                                                                          
035900 CA-SPARA-EV-NYTT-ORDERNR SECTION.                                        
036000     MOVE 'CA-SPARA-EV-NYTT' TO CURRENT-SECTION                           
036100                                                                          
036200     IF CURRENT-MAX-OMST-IDORDNR = ZERO                                   
036300        IF W-WDQ2C-IDORDNR7 < IN-IDORDNR-AKT                              
036400           MOVE W-WDQ2C-IDORDNR7    TO CURRENT-MAX-OMST-IDORDNR           
036500        ELSE                                                              
036600           IF W-WDQ2C-IDORDNR7 > CURRENT-MAX-IDORDNR                      
036700              MOVE W-WDQ2C-IDORDNR7 TO CURRENT-MAX-IDORDNR                
036800           END-IF                                                         
036900        END-IF                                                            
037000     ELSE                                                                 
037100        IF W-WDQ2C-IDORDNR7 < IN-IDORDNR-AKT                              
037200        AND W-WDQ2C-IDORDNR7 > CURRENT-MAX-OMST-IDORDNR                   
037300           MOVE W-WDQ2C-IDORDNR7    TO CURRENT-MAX-OMST-IDORDNR           
037400        END-IF                                                            
037500     END-IF                                                               
037600     .                                                                    
037700                                                                          
037800                                                                          
037900 CB-KOLLA-Q2-ORDER SECTION.                                               
038000     MOVE 'CB-KOLLA-Q2     ' TO CURRENT-SECTION                           
038100                                                                          
038200     PERFORM IMS-GU-WDQ201-CSEQ                                           
038300     PERFORM UNTIL SEGMENT-SAKNAS                                         
038400        PERFORM S20-NYTT-ORDERNUMMER                                      
038500        PERFORM IMS-GU-WDQ201-CSEQ                                        
038600     END-PERFORM                                                          
038700     .                                                                    
038800                                                                          
038900 D-SPARA-ORDERNUMMER SECTION.                                             
039000     MOVE 'D-SPARA-ORDERNR ' TO CURRENT-SECTION                           
039100                                                                          
039200     MOVE IN-IDORDNR-MIN     TO UT-IDORDNR-MIN                            
039300     MOVE IN-IDORDNR-MAX     TO UT-IDORDNR-MAX                            
039400                                                                          
039500     IF CURRENT-MAX-IDORDNR = ZERO                                        
039600*    VI HAR BARA HAFT KOSOLIDERADE DISTRIKT                               
039700        COMPUTE CURRENT-MAX-IDORDNR = IN-IDORDNR-AKT + 1                  
039800     END-IF                                                               
039900                                                                          
040000     IF CURRENT-MAX-OMST-IDORDNR > ZERO                                   
040100        COMPUTE UT-IDORDNR-AKT = CURRENT-MAX-OMST-IDORDNR + 1             
040200     ELSE                                                                 
040300        IF CURRENT-MAX-IDORDNR = IN-IDORDNR-MAX                           
040400           MOVE IN-IDORDNR-MIN TO UT-IDORDNR-AKT                          
040500        ELSE                                                              
040600           COMPUTE UT-IDORDNR-AKT = CURRENT-MAX-IDORDNR + 1               
040700        END-IF                                                            
040800     END-IF                                                               
040900                                                                          
041000     PERFORM S11-SKRIV-W41206                                             
041100     .                                                                    
041200                                                                          
041300                                                                          
041400 Z-FINIT SECTION.                                                         
041500     MOVE 'Z-FINIT         ' TO CURRENT-SECTION                           
041600                                                                          
041700     CLOSE W41206I                                                        
041800           W41206O                                                        
041900           W46333                                                         
042000           W41227                                                         
042100                                                                          
042200     MOVE 'S' TO POSTSUM-OPKOD                                            
042300     CALL POSTSUM USING POSTSUM-PARM                                      
042400     .                                                                    
042500                                                                          
042600                                                                          
042700 S01-LAES-W41206  SECTION.                                                
042800                                                                          
042900     READ W41206I INTO IN-AREA                                            
043000     AT END                                                               
043100        MOVE HIGH-VALUE TO IN-AREA                                        
043200        SET END-OF-W41206 TO TRUE                                         
043300                                                                          
043400     NOT AT END                                                           
043500        MOVE 'W41206'   TO POSTSUM-FDNAMN                                 
043600        MOVE 'W41203D1' TO POSTSUM-DDNAMN2                                
043700        MOVE 'IN'       TO POSTSUM-TRANSTYP                               
043800        CALL POSTSUM USING POSTSUM-PARM                                   
043900     END-READ                                                             
044000     .                                                                    
044100                                                                          
044200                                                                          
044300 S02-LAES-W46333  SECTION.                                                
044400                                                                          
044500     READ W46333 INTO PIEIN-AREA                                          
044600     AT END                                                               
044700        SET END-OF-W46333 TO TRUE                                         
044800                                                                          
044900     NOT AT END                                                           
045000        MOVE 'W46333'   TO POSTSUM-FDNAMN                                 
045100        MOVE 'W41203D3' TO POSTSUM-DDNAMN2                                
045200        MOVE 'PIE'      TO POSTSUM-TRANSTYP                               
045300        CALL POSTSUM USING POSTSUM-PARM                                   
045400     END-READ                                                             
045500     .                                                                    
045600                                                                          
045700                                                                          
045800 S11-SKRIV-W41206 SECTION.                                                
045900                                                                          
046000     WRITE UT-POST FROM UT-AREA                                           
046100                                                                          
046200     MOVE 'UT'       TO POSTSUM-TRANSTYP                                  
046300     MOVE 'W41206'   TO POSTSUM-FDNAMN                                    
046400     MOVE 'W41203D2' TO POSTSUM-DDNAMN2                                   
046500     CALL POSTSUM USING POSTSUM-PARM                                      
046600     .                                                                    
046700                                                                          
046800                                                                          
046900 S13-SKRIV-W41227 SECTION.                                                
047000                                                                          
047100     WRITE PIE-POST FROM PIEUT-AREA                                       
047200                                                                          
047300     MOVE PIEUT-KDSOFT  TO POSTSUM-TRANSTYP                               
047400     MOVE 'W41227'      TO POSTSUM-FDNAMN                                 
047500     MOVE 'W41203D4'    TO POSTSUM-DDNAMN2                                
047600     CALL POSTSUM USING POSTSUM-PARM                                      
047700                                                                          
047800     IF PIEIN-KDSOFT = '1'                                                
047900        MOVE SPACE      TO PIEUT-AREA                                     
048000     END-IF                                                               
048100     .                                                                    
048200                                                                          
048300                                                                          
048400 S20-NYTT-ORDERNUMMER SECTION.                                            
048500     MOVE 'S20-NYTT-ORDERNR' TO CURRENT-SECTION                           
048600                                                                          
048700     IF W-WDQ2C-IDORDNR7 < IN-IDORDNR-MAX                                 
048800        ADD 1 TO W-WDQ2C-IDORDNR7                                         
048900     ELSE                                                                 
049000        MOVE IN-IDORDNR-MIN TO W-WDQ2C-IDORDNR7                           
049100     END-IF                                                               
049200     .                                                                    
049300                                                                          
049400                                                                          
049500 S99-ABEND SECTION.                                                       
049600                                                                          
049700     MOVE 'S' TO POSTSUM-OPKOD                                            
049800     CALL POSTSUM USING POSTSUM-PARM                                      
049900     CALL ABEND USING RKOD-ABEND                                          
050000     .                                                                    
050100                                                                          
050200                                                                          
050300                                                                          
050400* --- IMS SEKTIONER ---                                                   
050500                                                                          
050600 IMS-GU-WDQ201-CSEQ SECTION.                                              
050700     MOVE 'IMS-GU-WDQ201   ' TO CURRENT-IMS-SECTION                       
050800                                                                          
050900     MOVE SPACE               TO ALL-SSA                                  
051000     STRING  'WDQ201  (WDQ2CSEQ =' W-WDQ2CSEQ-X ')'                       
051100             DELIMITED BY SIZE INTO    SSA1                               
051200     MOVE    '  GE'              TO    GODK-STATUSKODER                   
051300     CALL CBLTDLI USING GU WDQ2-PCB    DLI-IO-WDQ201 SSA1                 
051400     MOVE    WDQ2-STATUS-CODE    TO    STATUS-WS                          
051500     PERFORM IMS-STATUSKONTROLL                                           
051600     .                                                                    
051700                                                                          
051800 IMS-GU-WDB201  SECTION.                                                  
051900     MOVE 'IMS-GU-WDB201'    TO CURRENT-IMS-SECTION                       
052000                                                                          
052100     MOVE SPACE               TO ALL-SSA                                  
052200     STRING 'WDB201  (IDGMT    =' W-IDGMT-X ')'                           
052300          DELIMITED BY SIZE INTO SSA1                                     
052400     MOVE '    '              TO GODK-STATUSKODER                         
052500     CALL CBLTDLI USING GU WDB2-PCB DLI-IO-WDB201 SSA1                    
052600     MOVE WDB2-STATUS-CODE    TO STATUS-WS                                
052700     PERFORM IMS-STATUSKONTROLL                                           
052800     .                                                                    
052900                                                                          
053000 IMS-GU-WDK601 SECTION.                                                   
053100     MOVE 'IMS-GU-WDK601'    TO CURRENT-IMS-SECTION                       
053200                                                                          
053300     MOVE SPACE               TO ALL-SSA                                  
053400     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
053500             DELIMITED BY SIZE INTO SSA1                                  
053600     MOVE '  GE'                 TO GODK-STATUSKODER                      
053700     CALL CBLTDLI USING GU WDK6-PCB DLI-IO-AREA-WDK601 SSA1               
053800     MOVE WDK6-STATUS-CODE       TO STATUS-WS                             
053900     PERFORM IMS-STATUSKONTROLL                                           
054000     .                                                                    
054100                                                                          
054200 IMS-GNP-WDK611   SECTION.                                                
054300     MOVE 'IMS-GNP-WDK611'    TO CURRENT-IMS-SECTION                      
054400                                                                          
054500     MOVE SPACE               TO ALL-SSA                                  
054600     MOVE 'WDK611  '          TO SSA1                                     
054700     MOVE '  ' TO GODK-STATUSKODER                                        
054800     CALL CBLTDLI USING GNP WDK6-PCB DLI-IO-AREA-WDK611 SSA1              
054900     MOVE WDK6-STATUS-CODE    TO STATUS-WS                                
055000     PERFORM IMS-STATUSKONTROLL                                           
055100     .                                                                    
055200                                                                          
055300 IMS-STATUSKONTROLL SECTION.                                              
055400                                                                          
055500     SET STATUS-IX TO 1                                                   
055600     SEARCH GODK-STATUS                                                   
055700       AT END                                                             
055800         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
055900           DELIMITED BY SIZE INTO FELTEXT                                 
056000         DISPLAY FELTEXT                                                  
056100         CALL FELLOG                                                      
056200       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
056300         CONTINUE                                                         
056400     END-SEARCH                                                           
056500     .                                                                    
