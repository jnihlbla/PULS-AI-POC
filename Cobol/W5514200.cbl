000100 ID DIVISION.                                                             
000200                                                                          
000300 PROGRAM-ID.     W5514200.                                                
000400 AUTHOR.         THOMAS LARSSON.                                          
000500 DATE-WRITTEN.   94/05/03.                                                
000600 DATE-COMPILED.                                                           
000700                                                                          
000800*    FUNKTION:                                                            
000900*        LÄSER WDK6 MED SB OCH SORTERAR FILEN PÅ ARTIKELNR                
001000*        PROGRAMMET LÄSER      WLARTC (WDK6)                              
001100*                                                                         
001200*    ABENDKODER:                                                          
001300*        U0016 -  FEL I SORTEN                                            
001400                                                                          
001500     SKIP3                                                                
001600 ENVIRONMENT DIVISION.                                                    
001700     SKIP2                                                                
001800 INPUT-OUTPUT SECTION.                                                    
001900                                                                          
002000 FILE-CONTROL.                                                            
002100     SKIP2                                                                
002200*          --- ARTIKELINFO                                                
002300     SELECT W55142                     ASSIGN TO W55142D1.                
002400     SKIP2                                                                
002500*          --- SORTERINGSFIL                                              
002600     SELECT SORTFIL                    ASSIGN TO W55142DS.                
002700     EJECT                                                                
002800 DATA DIVISION.                                                           
002900     SKIP3                                                                
003000 FILE SECTION.                                                            
003100     SKIP3                                                                
003200 FD  W55142                                                               
003300     RECORDING       F                                                    
003400     BLOCK CONTAINS  0.                                                   
003500     SKIP2                                                                
003600*01  POST -COPY W55142 -PRE  SORTWS- -L.                                  
003700     SKIP3                                                                
003800 SD  SORTFIL.                                                             
003900     SKIP2                                                                
004000 01  SRT-POST.                                                            
004100     03  -COPY W55142 -PRE  SRT-                                          
004200     EJECT                                                                
004300 WORKING-STORAGE SECTION.                                                 
004400                                                                          
004500*    -- CHECKED BY WY2000                                                 
004600 77  IDPGM                       PIC X(8)    VALUE 'W5514200'.            
004700 77  JA                          PIC X       VALUE 'J'.                   
004800 77  NEJ                         PIC X       VALUE 'N'.                   
004900 77  INDX                        PIC 9       VALUE ZERO.                  
004910 77  FIRST-INLEV                 PIC X       VALUE 'N'.                   
004920 77  INLEV-SW                    PIC X       VALUE 'N'.                   
004930 77  GODK-SW                     PIC X       VALUE 'N'.                   
004940 77  LOCAL-SW                    PIC X       VALUE 'N'.                   
005000                                                                          
005100 77  SKRIV-SW                    PIC X       VALUE 'N'.                   
005200     88  SKRIV-POST                          VALUE 'J'.                   
005300                                                                          
005310 77  WS-HUVLEV                   PIC X       VALUE 'N'.                   
005320     88  HUVLEV-YES                          VALUE 'J'.                   
005321     88  HUVLEV-NOO                          VALUE 'N'.                   
005330                                                                          
005400 77  SORTFIL-EOF-SW              PIC X       VALUE 'N'.                   
005500     88  END-OF-SORTFIL                      VALUE 'J'.                   
005600                                                                          
005610 01  WS-KDSORT                   PIC X(2)    VALUE SPACE.                 
005620                                                                          
005810 01  WS-PRARTSTD                 PIC S9(7)V9(2) COMP-3.                   
005900                                                                          
006000 01  DATUM-FAELT.                                                         
006100     03  DAGENS-AAAAMMDD         PIC 9(8)    VALUE ZERO.                  
006110     03  WS-DAPRLIST-MAX         PIC 9(8)    VALUE 99999999.              
006200     03  WS-DAPRLIST             PIC 9(8).                                
006300     EJECT                                                                
006400 01  DYNAMISKA-SUBPROGRAM.                                                
006500*                                                                         
006600     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
006700     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
006800     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
006900     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
007000     SKIP2                                                                
007100*    --- PARAMETRAR TILL ABEND                                            
007200                                                                          
007300 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
007400 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
007500     SKIP2                                                                
007600 01  FELTEXT.                                                             
007700     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
007800     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
007900     EJECT                                                                
008000*    --- PARAMETRAR TILL POSTSUM                                          
008100*                                                                         
008200*01  -COPY W0005   -PRE  POSTSUM-                                         
008300     EJECT                                                                
008400 01  UT-AREA-START               PIC X(24)   VALUE                        
008500                                 'UT-AREA-START  '.                       
008600     SKIP2                                                                
008700                                                                          
008800*01  AREA -COPY W55142     -PRE UT-                                       
008900 01  SORT-AREA-START             PIC X(24)   VALUE                        
009000                                 'SORT-AREA-START'.                       
009100     SKIP2                                                                
009200                                                                          
009300*01  AREA -COPY W55142     -PRE SORTWS-                                   
009400 01  SORT-RETURN-X               PIC X(2)  VALUE SPACE.                   
009500     EJECT                                                                
009600*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
009700*                                                                         
009800     EJECT                                                                
009900 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
010000     SKIP3                                                                
010100 01  NYCKLAR-TILL-DLI.                                                    
010200     03  W-IDARTNR-X.                                                     
010300         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
010400     SKIP2                                                                
010500*    --- STATUS-KOD FRÅN IMS                                              
010600 01  STATUS-WS                   PIC XX.                                  
010700     88  SEGMENT-FINNS                       VALUE '  '.                  
010800     88  SEGMENT-SLUT                        VALUE 'GB'.                  
010900     SKIP2                                                                
011000 01  GODK-STATUSKODER.                                                    
011100     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
011200     SKIP3                                                                
011300 01  SSA1                        PIC X(64).                               
011400 01  SSA2                        PIC X(64).                               
011500     EJECT                                                                
011600*    --- IMS FUNKTIONSKODER                                               
011700*01  -COPY W0003                                                          
011800     EJECT                                                                
011900*    ---  DLI INPUT-OUTPUT AREA                                           
012000 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
012100     SKIP3                                                                
012200 01  DLI-IO-AREA.                                                         
012300     03  IO-AREA                 PIC X(900)  VALUE SPACE.                 
012400     SKIP3                                                                
012500     03  WLARTC01 REDEFINES IO-AREA.                                      
012600*        05  -COPY WDK601                                                 
012700     EJECT                                                                
012800     03  WLARTC12 REDEFINES IO-AREA.                                      
012900*        05  -COPY WDK611                                                 
013000     EJECT                                                                
013100     03  WLARTC24 REDEFINES IO-AREA.                                      
013200*        05  -COPY WDK621                                                 
013300     EJECT                                                                
013400 LINKAGE SECTION.                                                         
013500                                                                          
013600     EJECT                                                                
013700*01  -COPY W0008  -PRE ARTC-                                              
013800     05  FILLER                  PIC X.                                   
013900     EJECT                                                                
014000 PROCEDURE DIVISION  USING ARTC-PCB.                                      
014100     ENTRY 'DLITCBL' USING ARTC-PCB.                                      
014200                                                                          
014300                                                                          
014400     PERFORM A-INIT                                                       
014500                                                                          
014600     SORT SORTFIL ASCENDING KEY SRT-IDARTNR                               
014700                  INPUT PROCEDURE B-LAES-INPUT                            
014800                  GIVING W55142                                           
014900                                                                          
015000     IF SORT-RETURN NOT = 0                                               
015100       MOVE SORT-RETURN TO SORT-RETURN-X                                  
015200       STRING 'RETURKOD ' SORT-RETURN-X ' FRÅN SORT'                      
015300           DELIMITED BY SIZE                                              
015400           INTO FELTEXT-STR                                               
015500       DISPLAY FELTEXT                                                    
015600       PERFORM S99-ABEND                                                  
015700     ELSE                                                                 
015800       PERFORM Z-FINIT                                                    
015900                                                                          
016000       MOVE ZERO TO RETURN-CODE                                           
016100       GOBACK                                                             
016200     END-IF                                                               
016300                                                                          
016400     .                                                                    
016500     EJECT                                                                
016600 A-INIT SECTION.                                                          
016700                                                                          
016800     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
016900                                                                          
017000     PERFORM AA-NOLLSTAELL-UTPOST                                         
017100     .                                                                    
017200     EJECT                                                                
017300 AA-NOLLSTAELL-UTPOST SECTION.                                            
017400     SKIP2                                                                
017500     MOVE ZERO  TO SORTWS-IDARTNR                                         
017600                   SORTWS-IDANSK                                          
017700                   SORTWS-KDPRODSL                                        
017800                   SORTWS-IDINK                                           
017900                   SORTWS-IDFKNGRP                                        
018000                   SORTWS-PRINK                                           
018100                   SORTWS-KDVTH                                           
018200                   SORTWS-PRARTSJK                                        
018300                   SORTWS-PRARTBES                                        
018400                   SORTWS-PRDIRLON                                        
018500                   SORTWS-PRDMTRL                                         
018600                   SORTWS-PROVRPAL                                        
018700                   SORTWS-RETULF                                          
018800                   SORTWS-REDIRLEV                                        
018900                   SORTWS-KDERS                                           
019000                   SORTWS-KDHF                                            
019100                   SORTWS-KVAKS                                           
019200                   SORTWS-KVEFRS                                          
019300                   SORTWS-KVLS                                            
019400                   SORTWS-TIPRLIST (1)                                    
019500                   SORTWS-TIPRLIST (2)                                    
019600                   SORTWS-TIPRLIST (3)                                    
019700                   SORTWS-TIPRLIST (4)                                    
019800                   SORTWS-TIPRLIST (5)                                    
019900                   SORTWS-PRARTBES-PR (1)                                 
020000                   SORTWS-PRARTBES-PR (2)                                 
020100                   SORTWS-PRARTBES-PR (3)                                 
020200                   SORTWS-PRARTBES-PR (4)                                 
020300                   SORTWS-PRARTBES-PR (5)                                 
020400                   SORTWS-PRARTBEL-PR (1)                                 
020500                   SORTWS-PRARTBEL-PR (2)                                 
020600                   SORTWS-PRARTBEL-PR (3)                                 
020700                   SORTWS-PRARTBEL-PR (4)                                 
020800                   SORTWS-PRARTBEL-PR (5)                                 
020900                   SORTWS-KDSTATUS-PR (1)                                 
021000                   SORTWS-KDSTATUS-PR (2)                                 
021100                   SORTWS-KDSTATUS-PR (3)                                 
021200                   SORTWS-KDSTATUS-PR (4)                                 
021300                   SORTWS-KDSTATUS-PR (5)                                 
021400                   SORTWS-SUINLEV-PR  (1)                                 
021500                   SORTWS-SUINLEV-PR  (2)                                 
021600                   SORTWS-SUINLEV-PR  (3)                                 
021700                   SORTWS-SUINLEV-PR  (4)                                 
021800                   SORTWS-SUINLEV-PR  (5)                                 
021900                   SORTWS-INLEV-TIPRLIST                                  
021910                   SORTWS-INLEV-PRARTBEL-PR                               
021920                   SORTWS-GODK-TIPRLIST                                   
021930                   SORTWS-GODK-PRARTBEL-PR                                
021940                   SORTWS-LOCAL-TIPRLIST                                  
021950                   SORTWS-LOCAL-PRARTBEL-PR                               
022000                                                                          
022100     MOVE SPACE TO SORTWS-FLIART                                          
022200                   SORTWS-IDLEVNR                                         
022300                   SORTWS-IDLEVNR-PR (1)                                  
022400                   SORTWS-IDLEVNR-PR (2)                                  
022500                   SORTWS-IDLEVNR-PR (3)                                  
022600                   SORTWS-IDLEVNR-PR (4)                                  
022700                   SORTWS-IDLEVNR-PR (5)                                  
022710                   SORTWS-INLEV-IDLEVNR-PR                                
022720                   SORTWS-GODK-IDLEVNR-PR                                 
022730                   SORTWS-LOCAL-IDLEVNR-PR                                
022800     MOVE +1 TO INDX                                                      
022900     PERFORM UNTIL INDX > 5                                               
023000       MOVE SPACE TO SORTWS-KDVALISO (INDX)                               
023100                     SORTWS-KDVALISO (INDX)                               
023200                     SORTWS-KDVALISO (INDX)                               
023300                     SORTWS-KDVALISO (INDX)                               
023400                     SORTWS-KDVALISO (INDX)                               
023500       ADD +1 TO INDX                                                     
023600     END-PERFORM                                                          
023700     .                                                                    
023800     EJECT                                                                
023900 B-LAES-INPUT SECTION.                                                    
024000     SKIP2                                                                
024100     PERFORM IMS-GET-WDK6                                                 
024200     PERFORM UNTIL SEGMENT-SLUT                                           
024300       EVALUATE ARTC-SEG-NAME-FB                                          
024400         WHEN 'WDK601  '                                                  
024900           IF SKRIV-POST                                                  
025000             PERFORM S31-RELEASE-W55142                                   
025100             PERFORM AA-NOLLSTAELL-UTPOST                                 
025200           END-IF                                                         
025300           IF ART-KDERS-UTG = ZERO                                        
025400             IF ART-IDLEVNR NOT = SPACE                                   
025500               MOVE +1 TO INDX                                            
025600               MOVE ART-IDARTNR      TO SORTWS-IDARTNR                    
025700               MOVE ART-IDLEVNR      TO SORTWS-IDLEVNR                    
025800               MOVE ART-KDPRODSL     TO SORTWS-KDPRODSL                   
025900               MOVE ART-IDFKNGRP     TO SORTWS-IDFKNGRP                   
026000               MOVE ART-FLIART       TO SORTWS-FLIART                     
026010               MOVE ART-KDSORT       TO WS-KDSORT                         
026100               MOVE JA TO SKRIV-SW                                        
026200             ELSE                                                         
026300               MOVE NEJ TO SKRIV-SW                                       
026400             END-IF                                                       
026500           ELSE                                                           
026600             MOVE NEJ TO SKRIV-SW                                         
026700           END-IF                                                         
026800         WHEN 'WDK611  '                                                  
026900           IF SKRIV-POST                                                  
027000             IF CLAG-PRINK > ZERO                                         
027100               MOVE CLAG-IDINK       TO SORTWS-IDINK                      
027200               MOVE CLAG-KDHF        TO SORTWS-KDHF                       
027300               MOVE CLAG-IDANSK      TO SORTWS-IDANSK                     
027400               MOVE CLAG-REDIRLEV    TO SORTWS-REDIRLEV                   
027500               MOVE CLAG-PRINK       TO SORTWS-PRINK                      
027600               MOVE CLAG-KDVTH       TO SORTWS-KDVTH                      
027700               MOVE CLAG-PRARTSJK    TO SORTWS-PRARTSJK                   
027800               MOVE ZERO             TO SORTWS-PRARTBES                   
027900               MOVE CLAG-PRDIRLON    TO SORTWS-PRDIRLON                   
028000               MOVE CLAG-PRDMTRL     TO SORTWS-PRDMTRL                    
028100               MOVE CLAG-PROVRPAL    TO SORTWS-PROVRPAL                   
028200               MOVE CLAG-RETULF      TO SORTWS-RETULF                     
028300               MOVE CLAG-KDERS       TO SORTWS-KDERS                      
028400               COMPUTE SORTWS-KVAKS =                                     
028500                 CLAG-KVAKS-CDC + CLAG-KVAKS-PAV + CLAG-KVAKS-T           
028600               MOVE CLAG-KVEFRS      TO SORTWS-KVEFRS                     
028700               MOVE CLAG-KVLS        TO SORTWS-KVLS                       
028710               MOVE CLAG-PRARTSTD    TO WS-PRARTSTD                       
028800             ELSE                                                         
028900               MOVE NEJ TO SKRIV-SW                                       
029000             END-IF                                                       
029100           END-IF                                                         
029200         WHEN 'WDK621  '                                                  
029300           IF SKRIV-POST                                                  
029500             IF INDX < 6                                                  
029600               SUBTRACT PRL-DAPRLIST-9KOMPL FROM WS-DAPRLIST-MAX          
029700               GIVING WS-DAPRLIST                                         
029800               MOVE WS-DAPRLIST(3:6) TO SORTWS-TIPRLIST    (INDX)         
029900               MOVE PRL-IDLEVNR      TO SORTWS-IDLEVNR-PR  (INDX)         
030000               MOVE PRL-PRARTBES-PR  TO SORTWS-PRARTBES-PR (INDX)         
030100               MOVE PRL-PRARTBEL-PR  TO SORTWS-PRARTBEL-PR (INDX)         
030110               IF WS-KDSORT = 'SW'                                        
030120                 MOVE 1              TO SORTWS-KDSTATUS-PR (INDX)         
030130               ELSE                                                       
030200                MOVE PRL-KDSTATUS-PR TO SORTWS-KDSTATUS-PR (INDX)         
030210               END-IF                                                     
030300               MOVE PRL-SUINLEV-PR   TO SORTWS-SUINLEV-PR  (INDX)         
030400               MOVE PRL-KDVALISO     TO SORTWS-KDVALISO    (INDX)         
030401**** IF THERE IS MORE THAN 5 LOCAL PRICEROWS ON THE SAME PART             
030402**** SEE IF THERE ARE ANY MAIN SUPPLIERS PRICEROWS                        
030410               IF INDX = 1                                                
030420                 MOVE NEJ TO WS-HUVLEV                                    
030421                 MOVE NEJ TO INLEV-SW                                     
030422                 MOVE NEJ TO GODK-SW                                      
030423                 MOVE NEJ TO LOCAL-SW                                     
030430               END-IF                                                     
030500               IF SORTWS-IDLEVNR = PRL-IDLEVNR                            
030510                 MOVE JA  TO WS-HUVLEV                                    
030520               END-IF                                                     
030530               IF INDX = 5                                                
030540               AND HUVLEV-NOO                                             
030550                 CONTINUE                                                 
030560               ELSE                                                       
030600                 ADD +1 TO INDX                                           
030610               END-IF                                                     
030620****                                                                      
030700* PRARTBES BORTTAGET FRÅN K611 MEN KVAR PÅ W01160                         
030710* DÄRFÖR HÄMTAS PRARTBES-PR FRÅN K621 TILL CLAG-PRARTBES(FILEN)           
030721               MOVE FUNCTION CURRENT-DATE(1:8) TO DAGENS-AAAAMMDD         
030730               COMPUTE WS-DAPRLIST = 99999999 - DAGENS-AAAAMMDD           
030740                                                                          
030750               IF PRL-DAPRLIST-9KOMPL NOT < WS-DAPRLIST                   
030760                 IF SORTWS-PRARTBES = 0 OR FIRST-INLEV = NEJ              
030770                   IF SORTWS-PRARTBES = 0                                 
030780                     MOVE PRL-PRARTBES-PR                                 
030790                                  TO SORTWS-PRARTBES                      
030791                   END-IF                                                 
030792                   IF PRL-SUINLEV-PR > ZERO                               
030793                     MOVE JA  TO FIRST-INLEV                              
030794                     MOVE PRL-PRARTBES-PR                                 
030795                                  TO SORTWS-PRARTBES                      
030796                   ELSE                                                   
030797                     MOVE NEJ TO FIRST-INLEV                              
030798                   END-IF                                                 
030799                 END-IF                                                   
030800               ELSE                                                       
030801                 MOVE WS-PRARTSTD TO SORTWS-PRARTBES                      
030802               END-IF                                                     
030803             END-IF                                                       
030804             IF PRL-FLHUVLEV = 'N'                                        
030805               IF LOCAL-SW = NEJ                                          
030806                 MOVE JA  TO LOCAL-SW                                     
030807                 MOVE PRL-PRARTBES-PR TO SORTWS-LOCAL-PRARTBEL-PR         
030808                 MOVE PRL-IDLEVNR     TO SORTWS-LOCAL-IDLEVNR-PR          
030809                 SUBTRACT PRL-DAPRLIST-9KOMPL FROM WS-DAPRLIST-MAX        
030810                 GIVING WS-DAPRLIST                                       
030811                 MOVE WS-DAPRLIST(3:6) TO SORTWS-LOCAL-TIPRLIST           
030814               END-IF                                                     
030815             END-IF                                                       
030816             IF PRL-FLHUVLEV = 'J'                                        
030817               IF PRL-SUINLEV-PR > ZERO                                   
030818                 IF INLEV-SW = NEJ                                        
030819                  MOVE JA  TO INLEV-SW                                    
030820                  MOVE PRL-PRARTBES-PR TO SORTWS-INLEV-PRARTBEL-PR        
030821                  MOVE PRL-IDLEVNR     TO SORTWS-INLEV-IDLEVNR-PR         
030823                 SUBTRACT PRL-DAPRLIST-9KOMPL FROM WS-DAPRLIST-MAX        
030825                  GIVING WS-DAPRLIST                                      
030826                  MOVE WS-DAPRLIST(3:6) TO SORTWS-INLEV-TIPRLIST          
030827                 END-IF                                                   
030828               ELSE                                                       
030829                 IF GODK-SW = NEJ                                         
030830                  MOVE JA  TO GODK-SW                                     
030831                  MOVE PRL-PRARTBES-PR TO SORTWS-GODK-PRARTBEL-PR         
030832                  MOVE PRL-IDLEVNR     TO SORTWS-GODK-IDLEVNR-PR          
030833                 SUBTRACT PRL-DAPRLIST-9KOMPL FROM WS-DAPRLIST-MAX        
030834                  GIVING WS-DAPRLIST                                      
030835                  MOVE WS-DAPRLIST(3:6) TO SORTWS-GODK-TIPRLIST           
030836                 END-IF                                                   
030837               END-IF                                                     
030840             END-IF                                                       
030900           END-IF                                                         
031300       END-EVALUATE                                                       
031400       PERFORM IMS-GET-WDK6                                               
031500     END-PERFORM                                                          
031600     IF SKRIV-POST                                                        
031700       PERFORM S31-RELEASE-W55142                                         
031800       PERFORM AA-NOLLSTAELL-UTPOST                                       
031900     END-IF                                                               
032000     .                                                                    
032100     EJECT                                                                
032200 Z-FINIT SECTION.                                                         
032300     SKIP2                                                                
032400     MOVE 'S' TO POSTSUM-OPKOD                                            
032500     CALL POSTSUM USING POSTSUM-PARM                                      
032600     .                                                                    
032700     EJECT                                                                
032800 S31-RELEASE-W55142 SECTION.                                              
032900     SKIP2                                                                
033000     RELEASE SRT-POST FROM SORTWS-AREA                                    
033100                                                                          
033200     MOVE 'WDK6'    TO POSTSUM-TRANSTYP                                   
033300     MOVE 'W55142' TO POSTSUM-FDNAMN                                      
033400     MOVE 'W55142D1' TO POSTSUM-DDNAMN2                                   
033500     CALL POSTSUM USING POSTSUM-PARM                                      
033600     .                                                                    
033700     EJECT                                                                
033800 S99-ABEND SECTION.                                                       
033900     SKIP2                                                                
034000     MOVE 'S' TO POSTSUM-OPKOD                                            
034100     CALL POSTSUM USING POSTSUM-PARM                                      
034200     CALL ABEND USING RKOD-ABEND-UTAN-DUMP                                
034300     .                                                                    
034400     EJECT                                                                
034500* --- IMS SEKTIONER ---                                                   
034600     SKIP3                                                                
034700     EJECT                                                                
034800 IMS-GET-WDK6   SECTION.                                                  
034900     SKIP2                                                                
035000     CALL CBLTDLI USING GN ARTC-PCB DLI-IO-AREA                           
035100     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
035200     MOVE '  GAGKGB' TO GODK-STATUSKODER                                  
035300     PERFORM IMS-STATUSKONTROLL                                           
035400     .                                                                    
035500     EJECT                                                                
035600 IMS-STATUSKONTROLL SECTION.                                              
035700     SKIP2                                                                
035800     SET STATUS-IX TO 1                                                   
035900     SEARCH GODK-STATUS                                                   
036000       AT END                                                             
036100         MOVE 'EJ GODK STATUS' TO FELTEXT-STR                             
036200         DISPLAY FELTEXT                                                  
036300         CALL FELLOG                                                      
036400       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
036500         CONTINUE                                                         
036600     END-SEARCH                                                           
036700     .                                                                    
