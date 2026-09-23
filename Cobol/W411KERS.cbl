000100*COMPOPT STDSUB=YES                                                       
000200 ID DIVISION.                                                             
000300                                                                          
000400 PROGRAM-ID.             W411KERS.                                        
000500 AUTHOR.                 LARS THELL CAP GEMINI LOGIC                      
000600     DATE-WRITTEN.       MAJ 1990.                                        
000700*                                                                         
000800     REMARKS.                                                             
000900*                                                                         
001000*    FUNKTION:                                                            
001100*        KONTROLLERA OM ORDERRADENS ARTIKELNR ÄR ER-                      
001200*        SATT MED NÅGOT ANNAT ARTIKELNR.                                  
001300*                                                                         
001400*        SKAPAR EN TILLKOMMANDE TABELL SOM INNEHÅLLER                     
001500*        ARTIKLAR SOM ERSÄTTER BESTÄLLD ARTIKEL OCH                       
001600*        ARTIKLAR SOM ERSÄTTER ARTIKLAR SOM FINNS I                       
001700*        TABELLEN                                                         
001800*                                                                         
001900*        ERSÄTTNINGARNA KAN VARA ANTINGEN ENTYDIGA ELLER                  
002000*        EJ ENTYDIGA.                                                     
002100*                                                                         
002200*        EX: A ---> B OCH C  ENTYDIGT                                     
002300*            A ---> B ELLER C EJ ENTYDIGT                                 
002400*                                                                         
002500     EJECT                                                                
002600 ENVIRONMENT DIVISION.                                                    
002700                                                                          
002800 DATA DIVISION.                                                           
002900                                                                          
003000 WORKING-STORAGE SECTION.                                                 
003100     SKIP2                                                                
003200*    -COPY WY2000W2                                                       
003300     SKIP3                                                                
003400 77  PROGRAM-NAMN            PIC X(8) VALUE 'W411KERS'.                   
003500     SKIP2                                                                
003600*    ---- KONSTANTER                                                      
003700                                                                          
003800 77  JA                      PIC X       VALUE 'J'.                       
003900 77  NEJ                     PIC X       VALUE 'N'.                       
004000 77  SPEC-FORBI              PIC X       VALUE 'S'.                       
004100 77  RKOD-16                 PIC S9(4)   VALUE +16  COMP.                 
004200 77  IX                      PIC S9(9)   VALUE +0   COMP SYNC.            
004300 77  IX2                     PIC S9(9)   VALUE +0   COMP SYNC.            
004400 77  W-DIERS-KVOT            PIC S9(4)V9(3)         COMP-3.               
004500 77  W-KDERS                 PIC S9(3)              COMP-3.               
004600*                                                                         
004700 01  W-IDARTNR-NUM           PIC 9(9).                                    
004800 01  W-IDARTNR-NUM1 REDEFINES W-IDARTNR-NUM.                              
004900   03  FILLER                PIC 9(2).                                    
005000   03  W-IDARTNR-NUM-3-9     PIC 9(7).                                    
005100                                                                          
005200     SKIP2                                                                
005300 01  AKT-DATUM-PLUS-5-VECKOR     PIC 9(5).                                
005400 01  FILLER REDEFINES AKT-DATUM-PLUS-5-VECKOR.                            
005500   03  AKT-AAR                   PIC 9(2).                                
005600   03  AKT-VECKA                 PIC 9(2).                                
005700   03  AKT-DAG                   PIC 9(1).                                
005800     EJECT                                                                
005900                                                                          
006000 77  AVSLUTA-SW              PIC  X(01)  VALUE 'N'.                       
006100     88 AVSLUTA                          VALUE 'J'.                       
006200                                                                          
006300 77  ART-ERSATT-SW           PIC  X(01)  VALUE 'N'.                       
006400     88 ART-ERSATT                       VALUE 'J'.                       
006500     SKIP2                                                                
006600 77  LAG-SW                  PIC  X(01)  VALUE 'N'.                       
006700 77  LAGE-SW                 PIC  X(01)  VALUE 'N'.                       
006800 01  W-SALDO                 PIC S9(7)          COMP-3.                   
006900*    ---- SUBPROGRAM OCH PARAMETER-AREOR                                  
007000     SKIP2                                                                
007100 01  DYNAMISKA-SUBPROGRAM.                                                
007200   03  W009KSIF              PIC X(8)    VALUE 'W009KSIF'.                
007300   03  WDATKONV              PIC X(8)    VALUE 'WDATKONV'.                
007400   03  CBLTDLI               PIC X(8)    VALUE 'CBLTDLI '.                
007500   03  FELLOG                PIC X(8)    VALUE 'FELLOG  '.                
007600   03  ABEND                 PIC X(8)    VALUE 'ABEND   '.                
007700     SKIP2                                                                
007800 01  W009KSIF-PARM.                                                       
007900   03  FLT                   PIC 9(9).                                    
008000   03  LGD                   PIC 9(1).                                    
008100   03  KSIFF                 PIC 9(1).                                    
008200     EJECT                                                                
008300*01  -COPY WDATAREA.                                                      
008400     EJECT                                                                
008500                                                                          
008600*01  -COPY WWDCKONS                                                       
008700                                                                          
009200 01  FILLER                  PIC X(16) VALUE 'IMS-WS'.                    
009300                                                                          
009400 01  NYCKLAR-TILL-DLI.                                                    
009500                                                                          
009600   03  W-ERSA-IDARTNR-X.                                                  
009700     05  W-ERSA-IDARTNR      PIC S9(9)    COMP-3.                         
009800                                                                          
009900   03  W-ARTC-IDARTNR-X.                                                  
010000     05  W-ARTC-IDARTNR      PIC S9(9)    COMP-3.                         
010100                                                                          
010200   03  W-IDDC-X.                                                          
010300     05  W-IDDC              PIC X(2).                                    
010400                                                                          
010500                                                                          
010600 01  STATUS-WS               PIC XX.                                      
010700     88  SEGMENT-FINNS                    VALUE '  '.                     
010800     88  SEGMENT-SAKNAS                   VALUE 'GE'.                     
010900     88  SEGMENT-FINNS-REDAN              VALUE 'II'.                     
011000     SKIP2                                                                
011100 01  GODK-STATUSKODER.                                                    
011200   03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                  
011300     SKIP2                                                                
011400 01  SSA1                    PIC X(64).                                   
011500     EJECT                                                                
011600*01  -COPY W0003                                                          
011700     EJECT                                                                
011800 01  DLI-IO-AREA.                                                         
011900     SKIP2                                                                
012000 03  FILLER                  PIC X(16) VALUE 'ERSA01-AREA'.               
012100*03  WLERSA01 -COPY WDD701      -PRE ERSA01-                              
012200     EJECT                                                                
012300 03  FILLER                  PIC X(16) VALUE 'ERSA11-AREA'.               
012400*03  WLERSA11 -COPY WDD702      -PRE ERSA11-                              
012500     EJECT                                                                
012600 03  FILLER                  PIC X(16) VALUE 'WDK601-AREA'.               
012700*03  WLARTC01 -COPY WDK601                                                
012800     EJECT                                                                
012900 03  FILLER                  PIC X(16) VALUE 'WDK611-AREA'.               
013000*03  WLARTC11 -COPY WDK611                                                
013100     EJECT                                                                
013200 03  FILLER                  PIC X(16) VALUE 'WDK701-AREA'.               
013300*03  WDK701   -COPY WDK701                                                
013400     EJECT                                                                
013500 03  FILLER                  PIC X(16) VALUE 'WDK711-AREA'.               
013600*03  WDK711   -COPY WDK711                                                
013700     EJECT                                                                
013800 03  FILLER                  PIC X(16) VALUE 'WDK901-AREA'.               
013900*03  WDK901   -COPY WDK901                                                
014000     EJECT                                                                
014100 LINKAGE SECTION.                                                         
014200     SKIP2                                                                
014300*01  -COPY W411KERS                                                       
014400     EJECT                                                                
014500*01  -COPY W411TILK                                                       
014600     EJECT                                                                
014700*01  -COPY W0008      -PRE  ARTC-                                         
014800       05  FILLER                PIC X.                                   
014900     EJECT                                                                
015000*01  -COPY W0008      -PRE  ERSA-                                         
015100       05  FILLER                PIC X.                                   
015200     EJECT                                                                
015300*01  -COPY W0008      -PRE  WDK7-                                         
015400       05  FILLER                PIC X.                                   
015500     EJECT                                                                
015600*01  -COPY W0008      -PRE  WDK9-                                         
015700       05  FILLER                PIC X.                                   
015800     EJECT                                                                
015900 PROCEDURE DIVISION  USING  KERS-W411KERS TILK-W411TILK                   
016000                            ARTC-PCB ERSA-PCB                             
016100                            WDK7-PCB                                      
016200                            WDK9-PCB.                                     
016300 STYR SECTION.                                                            
016400                                                                          
016500     PERFORM A-INIT                                                       
016600                                                                          
016700     IF KERS-FLORDSPE = JA OR KERS-FLOVRLEV = JA                          
016800                           OR KERS-FLFORBI  = JA                          
016900                           OR KERS-FLFORBI  = SPEC-FORBI                  
017100        CONTINUE                                                          
017200     ELSE                                                                 
017300        PERFORM B-KONTROLLERA-ERSATTNINGSKOD                              
017400                                                                          
017500        IF NOT AVSLUTA                                                    
017600            PERFORM C-TILLKOMMANDE-ART                                    
017700                                                                          
017800            IF NOT AVSLUTA                                                
017900                PERFORM D-TILLKOMMANDE-ART-ERSATT                         
018000            END-IF                                                        
018100        END-IF                                                            
019100     END-IF                                                               
019200     GOBACK                                                               
019300     .                                                                    
019400     EJECT                                                                
019500 A-INIT       SECTION.                                                    
019600     SKIP2                                                                
019700     MOVE ZERO                 TO KERS-KDORDBEK                           
019800     PERFORM  S01-TOM-ERS-TILK-TAB                                        
019900                                                                          
020000     MOVE +0                   TO IX                                      
020100     MOVE +1                   TO IX2                                     
020200                                                                          
020300     MOVE NEJ                  TO AVSLUTA-SW                              
020400     MOVE NEJ                  TO ART-ERSATT-SW                           
020500     .                                                                    
020600     EJECT                                                                
020700 B-KONTROLLERA-ERSATTNINGSKOD  SECTION.                                   
020800     SKIP2                                                                
020900     IF (KERS-KDERS     > +10)  OR                                        
021000        (KERS-KDERS     = +01)                                            
021100         MOVE NEJ           TO AVSLUTA-SW                                 
021200     END-IF                                                               
021300                                                                          
021400*    IF (KERS-KDERS     = +0  OR  +1 OR  +2 OR +3 OR +4 OR                
021500     IF (KERS-KDERS     = +0  OR  +2 OR +3 OR +4 OR                       
021600                          +5  OR  +6 OR  +7 OR +8 OR +9 OR                
021700                          +19 OR +29 OR +52)                  OR          
021800        (KERS-KDERS-UTG = +19 OR +29 OR +52)                  OR          
021900        (KERS-KDERS     > +0 AND < +10                        AND         
022000        (KERS-KDTPOTYP  = +1 OR +2                            OR          
022100         KERS-KDUART    = 'P' OR 'S' OR 'M' OR 'L'))          OR          
022200        (KERS-KDERS     > +0 AND < +10 AND KERS-IDKAMPRF > +0)            
022300         MOVE JA            TO AVSLUTA-SW                                 
022400     END-IF                                                               
022500                                                                          
022600     IF KERS-KDERS-UTG > +20  AND  KERS-KDERS = +0                        
022700        MOVE KERS-KDERS-UTG TO KERS-KDERS                                 
022800        MOVE NEJ            TO AVSLUTA-SW                                 
022900     END-IF                                                               
023000     .                                                                    
023100     EJECT                                                                
023200 C-TILLKOMMANDE-ART   SECTION.                                            
023300                                                                          
023400*---- SKAPA EN TILLKOMMANDE TABELL MED                                    
023500*---- ERSÄTTNINGSARTIKLAR FÖR BESTÄLLD ARTIKEL                            
023700     MOVE KERS-IDARTNR         TO W-ERSA-IDARTNR                          
023800     MOVE NEJ                  TO LAG-SW                                  
023900     PERFORM IMS-GET-ERSA-ART-KVAL                                        
024000                                                                          
024100     IF SEGMENT-FINNS                                                     
024200       IF ERSA01-KVKORT      > +20                                        
024300           MOVE 61                   TO KERS-KDORDBEK                     
024400           PERFORM S01-TOM-ERS-TILK-TAB                                   
024500           MOVE JA                   TO AVSLUTA-SW                        
024600       ELSE                                                               
024700                                                                          
024800*        IF KERS-KDERS = +7 OR +27                                        
024900*          TILLFÄLLIG ERSÄTTNING                                          
025000*          PERFORM S07-KOLLA-LAGERSALDO                                   
025100*        END-IF                                                           
025200         PERFORM IMS-GET-ERSA-TLK-OKVAL                                   
025300         PERFORM  UNTIL SEGMENT-SAKNAS OR AVSLUTA                         
025400             ADD +1            TO IX                                      
025500             IF IX             > +20                                      
025600                 MOVE 61                   TO KERS-KDORDBEK               
025700                 PERFORM S01-TOM-ERS-TILK-TAB                             
025800                 MOVE JA                   TO AVSLUTA-SW                  
025900              ELSE                                                        
026000                 PERFORM S03-SKRIV-TILLKOMMANDE-ARTIKEL                   
026100                                                                          
026200                 PERFORM IMS-GET-ERSA-TLK-OKVAL                           
026300             END-IF                                                       
026400         END-PERFORM                                                      
026500         IF NOT AVSLUTA                                                   
026600            MOVE KERS-KDERS       TO W-KDERS                              
026700            PERFORM S05-KONTROLLERA-LAST-KLART                            
026800         END-IF                                                           
026900       END-IF                                                             
027000     ELSE                                                                 
027100         MOVE JA         TO AVSLUTA-SW                                    
027200     END-IF                                                               
027300     .                                                                    
027400     EJECT                                                                
027500 D-TILLKOMMANDE-ART-ERSATT    SECTION.                                    
027600                                                                          
027700*---- GÅ IGENOM ALLA ERSÄTTNINGSARTIKLAR I                                
027800*---- I TILLKOMMANDE TABELLEN OCH KONTROLLERA OM                          
027900*---- EN ERSÄTTNINGSARTIKEL ÄR ERSATT I SIN TUR,                          
028000*---- I SÅDANA FALL, UPPDATERA TILLKOMMANDE TABELLEN                      
028100                                                                          
028200     MOVE +1                   TO IX2                                     
028300     PERFORM UNTIL IX2         > +20  OR                                  
028400       AVSLUTA                        OR                                  
028500       IX2 > IX                                                           
028600                                                                          
028700         IF TILK-KDERS(IX2)        > +10                                  
028800            PERFORM DA-LAS-TILLKOMMANDE-ARTIKLAR                          
028900         END-IF                                                           
029000*? SKA DENNA S05 UTFÖRAS ALLTID ELLER BARA NÄR DA-...                     
029100         MOVE TILK-KDERS(IX2)             TO W-KDERS                      
029200         PERFORM S05-KONTROLLERA-LAST-KLART                               
029300                                                                          
029400         ADD +1                           TO IX2                          
029500     END-PERFORM                                                          
029600     .                                                                    
029700     EJECT                                                                
029800 DA-LAS-TILLKOMMANDE-ARTIKLAR SECTION.                                    
029900                                                                          
030000     MOVE TILK-IDARTNR-TILLK(IX2)     TO W-ERSA-IDARTNR                   
030100     PERFORM IMS-GET-ERSA-ART-KVAL                                        
030200     IF SEGMENT-FINNS                                                     
030300        IF ERSA01-KVKORT      > +20                                       
030400          MOVE 61                   TO KERS-KDORDBEK                      
030500          PERFORM S01-TOM-ERS-TILK-TAB                                    
030600          MOVE JA                   TO AVSLUTA-SW                         
030700        ELSE                                                              
030800          MOVE JA               TO ART-ERSATT-SW                          
030900                                                                          
031000          PERFORM IMS-GET-ERSA-TLK-OKVAL                                  
031100          PERFORM  UNTIL SEGMENT-SAKNAS OR AVSLUTA                        
031200              ADD +1    TO IX                                             
031300              IF IX         > +20                                         
031400                 MOVE 61                   TO KERS-KDORDBEK               
031500                 PERFORM S01-TOM-ERS-TILK-TAB                             
031600                 MOVE JA                   TO AVSLUTA-SW                  
031700              ELSE                                                        
031800                 PERFORM S03-SKRIV-TILLKOMMANDE-ARTIKEL                   
031900                                                                          
032000                 PERFORM IMS-GET-ERSA-TLK-OKVAL                           
032100              END-IF                                                      
032200          END-PERFORM                                                     
032300        END-IF                                                            
032400     END-IF                                                               
032500     .                                                                    
032600     EJECT                                                                
032700 S01-TOM-ERS-TILK-TAB   SECTION.                                          
032800     SKIP2                                                                
032900     MOVE +1                   TO IX                                      
033000     PERFORM UNTIL IX > +20                                               
033100       MOVE ZERO               TO  TILK-IDARTNR(IX)                       
033200       MOVE ZERO               TO  TILK-IDARTNR-TILLK(IX)                 
033300       MOVE SPACE              TO  TILK-BEERS(IX)                         
033400       MOVE ZERO               TO  TILK-DIERS-ERS(IX)                     
033500       MOVE ZERO               TO  TILK-DIERS-TILLK(IX)                   
033600       MOVE JA                 TO  TILK-FLFINLV(IX)                       
033700       MOVE SPACE              TO  TILK-FLPRTILL(IX)                      
033800       MOVE SPACE              TO  TILK-FLTILLK-X(IX)                     
033900       MOVE ZERO               TO  TILK-KDERS(IX)                         
034000       MOVE SPACE              TO  TILK-KDPRTYP(IX)                       
034100       MOVE ZERO               TO  TILK-KVBEART(IX)                       
034200       MOVE ZERO               TO  TILK-PRARTNTO(IX)                      
034300       INITIALIZE                  TILK-DEAL-PR-LINE(IX)                  
034400       MOVE ZERO               TO  TILK-TIPRIS(IX)                        
034500       MOVE ZERO               TO  TILK-REKSIFFR-TILLK(IX)                
034600                                                                          
034700       ADD +1                  TO  IX                                     
034800                                                                          
034900     END-PERFORM                                                          
035000     .                                                                    
035100     EJECT                                                                
035200 S03-SKRIV-TILLKOMMANDE-ARTIKEL  SECTION.                                 
035300     SKIP2                                                                
035400     MOVE ERSA01-DIERS-ERS     TO TILK-DIERS-ERS(IX)                      
035500     MOVE KERS-IDARTNR         TO TILK-IDARTNR(IX)                        
035600                                                                          
035700     IF ERSA11-FLTEXT = JA                                                
035800         MOVE ERSA11-BEERS     TO TILK-BEERS(IX)                          
035900         MOVE JA               TO TILK-FLTILLK-X(IX)                      
036000         MOVE +61              TO KERS-KDORDBEK                           
036100      ELSE                                                                
036200         PERFORM S08-KOLLA-LAGERSALDO                                     
036300         MOVE ERSA11-IDARTNR-TILLK TO W-IDARTNR-NUM                       
036400         MOVE W-IDARTNR-NUM    TO FLT                                     
036500         MOVE 9                TO LGD                                     
036600         PERFORM S06-CALL-W009KSIF                                        
036700         MOVE KSIFF            TO TILK-REKSIFFR-TILLK(IX)                 
036800         MOVE ERSA11-IDARTNR-TILLK TO TILK-IDARTNR-TILLK(IX)              
036900         MOVE ERSA11-DIERS-TILLK   TO TILK-DIERS-TILLK(IX)                
037000         MOVE JA                   TO TILK-FLTILLK-X(IX)                  
037100                                                                          
037200         MOVE TILK-IDARTNR-TILLK(IX) TO W-ARTC-IDARTNR                    
037300         PERFORM IMS-GU-WDK601                                            
037400                                                                          
037500         PERFORM S03A-KOLLA-PUB-VECKA                                     
037600                                                                          
037700         COMPUTE W-DIERS-KVOT  =                                          
037800                   ERSA11-DIERS-TILLK / ERSA01-DIERS-ERS                  
037900         IF W-DIERS-KVOT < +1                                             
038000            MOVE +1             TO W-DIERS-KVOT                           
038100         END-IF                                                           
038500         IF (KERS-FLPRERS      = NEJ)    AND                              
038600            (ERSA01-KVKORT     = +1)     AND                              
038700            (KERS-KDERS        = +1 OR +7 OR +11 OR +17 OR +21 OR         
038800                                 +22 OR +23 OR +27)                       
038900             MOVE KERS-TIPRIS   TO TILK-TIPRIS(IX)                        
039000             MOVE KERS-KDPRTYP  TO TILK-KDPRTYP(IX)                       
039100             MOVE KERS-PRARTNTO TO TILK-PRARTNTO(IX)                      
039200             MOVE KERS-FLPRTILL TO TILK-FLPRTILL(IX)                      
039300         END-IF                                                           
039500     EJECT                                                                
039600         IF ART-ERSATT                                                    
039700             COMPUTE TILK-KVBEART(IX) =                                   
039800                             TILK-KVBEART(IX2) * W-DIERS-KVOT             
039900             MOVE NEJ          TO TILK-FLTILLK-X(IX2)                     
040000             IF ERSA01-KVKORT = +1                                        
040000               IF KERS-FLPRERS = NEJ AND TILK-PRARTNTO(IX) > 0            
040000                 CONTINUE                                                 
040000               ELSE                                                       
040100                 MOVE ZERO     TO TILK-TIPRIS(IX)                         
040200                 MOVE SPACE    TO TILK-KDPRTYP(IX)                        
040300                 MOVE ZERO     TO TILK-PRARTNTO(IX)                       
040400                 MOVE SPACE    TO TILK-FLPRTILL(IX)                       
040000               END-IF                                                     
040500             END-IF                                                       
040600          ELSE                                                            
040700             COMPUTE TILK-KVBEART(IX) =                                   
040800                         KERS-KVBEART * W-DIERS-KVOT                      
040900         END-IF                                                           
041000                                                                          
041100         IF ART-KDERS-UTG > +0                                            
041200            MOVE ART-KDERS-UTG    TO TILK-KDERS(IX)                       
041300         ELSE                                                             
041400            PERFORM IMS-GNP-WDK611                                        
041500            MOVE CLAG-KDERS       TO TILK-KDERS(IX)                       
041600         END-IF                                                           
041700                                                                          
041800         IF TILK-KDERS(IX) = +4 OR +8  OR +14 OR +18 OR                   
041900                            +24 OR +25 OR +26 OR +28                      
042000             MOVE +61          TO KERS-KDORDBEK                           
042100         ELSE                                                             
042200             MOVE +41          TO KERS-KDORDBEK                           
042300         END-IF                                                           
042301*CHECK THE PUB WEEK FOR THE NEW PART                                      
042302         IF KERS-KDERS > +10 AND                                          
042303           (KERS-KDERS NOT = +13 AND                                      
042304                       NOT = +23 AND                                      
042305                       NOT = +16 AND                                      
042306                       NOT = +26)                                         
042310            PERFORM S03B-CHK-SOP-2WEEKS                                   
042320         END-IF                                                           
042400     END-IF                                                               
042500     .                                                                    
042600     EJECT                                                                
042700 S03A-KOLLA-PUB-VECKA SECTION.                                            
042800                                                                          
042900     IF IX = +1                                                           
043000       IF KERS-KDERS = +01                                                
043100                                                                          
043200           MOVE 'IDAG' TO DAT-KDDATFORM                                   
043300           CALL WDATKONV USING DAT-KDDATFORM   DAT-I-TIDATUM              
043400                               DAT-O-TIDATUM   DAT-KDSVAR                 
043500           IF DAT-KDSVAR-OK                                               
043600             MOVE DAT-TIAA TO AKT-AAR                                     
043700             MOVE DAT-TIVV TO AKT-VECKA                                   
043800             MOVE DAT-TID  TO AKT-DAG                                     
043900             ADD 5 TO AKT-VECKA                                           
044000             IF AKT-AAR = 98                                              
044100               IF AKT-VECKA > 53                                          
044200                 SUBTRACT 53 FROM AKT-VECKA                               
044300                 ADD 1 TO AKT-AAR                                         
044400               END-IF                                                     
044500             ELSE                                                         
044600               IF AKT-VECKA > 52                                          
044700                 SUBTRACT 52 FROM AKT-VECKA                               
044800                 ADD 1 TO AKT-AAR                                         
044900               END-IF                                                     
045000             END-IF                                                       
045100           ELSE                                                           
045200             CALL ABEND USING RKOD-16                                     
045300           END-IF                                                         
045400                                                                          
045500           MOVE AKT-DATUM-PLUS-5-VECKOR   TO TMP1-YYWWD                   
045600           MOVE ART-TIFINLV               TO TMP2-YYWWD                   
045700           PERFORM WY2000P2                                               
045800           IF TMP1-YYWWD < TMP2-YYWWD                                     
045900              MOVE NEJ      TO TILK-FLFINLV(IX)                           
045910                               TILK-FLTILLK-X(IX)                         
046000           END-IF                                                         
046100       END-IF                                                             
046200     ELSE                                                                 
046300        IF TILK-FLFINLV(1) = NEJ                                          
046400           MOVE NEJ            TO TILK-FLFINLV(IX)                        
046500        END-IF                                                            
046600     END-IF                                                               
046700     .                                                                    
046800     EJECT                                                                
046810******************************************************************        
046811*1.IF PUB WEEK FOR NEW PART > 2 WEEKS,DONT REPLACE OLD WITH NEW           
046812*  PART.                                                                  
046813*2.IF PUB WEEK FOR NEW PART < 2 WEEKS,REPLACE OLD WITH NEW PART           
046820******************************************************************        
046900 S03B-CHK-SOP-2WEEKS SECTION.                                             
047000     SKIP2                                                                
047001                                                                          
047003     MOVE 'IDAG' TO DAT-KDDATFORM                                         
047004     CALL WDATKONV USING DAT-KDDATFORM   DAT-I-TIDATUM                    
047005                         DAT-O-TIDATUM   DAT-KDSVAR                       
047006* TO PROCESS ORDERS ONLY WITH TIFINLV AFTER 2000                          
047008     IF ART-TIFINLV < 50000                                               
047009       IF DAT-KDSVAR-OK                                                   
047010         ADD 2 TO DAT-TIVV                                                
047012         IF DAT-TIVV > 53                                                 
047013           SUBTRACT 53 FROM DAT-TIVV                                      
047014           ADD 1 TO DAT-TIAA-VECKA                                        
047016         ELSE                                                             
047017           IF DAT-TIVV > 52                                               
047018             SUBTRACT 52 FROM DAT-TIVV                                    
047019             ADD 1 TO DAT-TIAA-VECKA                                      
047021           END-IF                                                         
047022         END-IF                                                           
047023       ELSE                                                               
047024         CALL ABEND USING RKOD-16                                         
047025       END-IF                                                             
047026                                                                          
047029       IF DAT-TIAAVVD < ART-TIFINLV                                       
047031          MOVE JA                   TO  AVSLUTA-SW                        
047032          MOVE ZEROES               TO  KERS-KDORDBEK                     
047033          PERFORM S01-TOM-ERS-TILK-TAB                                    
047034       END-IF                                                             
047035     END-IF                                                               
047036     .                                                                    
047037     EJECT                                                                
047038 S05-KONTROLLERA-LAST-KLART SECTION.                                      
047040     SKIP2                                                                
047100*---- W-KDERS KOMMER IFRÅN IN-AREAN VID ERSÄTTNINGS-                      
047200*---- PRODUKTER PÅ BESTÄLLD ARTIKEL OCH IFRÅN TILL-                       
047300*---- KOMMANDE TABELLEN VID ERSÄTTNINGSPRODUKTER TILL                     
047400*---- PRODUKTER IFRÅN TILLKOMANDE TABELLEN                                
047600     IF W-KDERS                = +1 OR +7                                 
047700         MOVE 41               TO  KERS-KDORDBEK                          
047800         MOVE JA               TO  AVSLUTA-SW                             
047900     END-IF                                                               
048000                                                                          
048610     IF W-KDERS                = +4 OR +8  OR                             
048611                                +14 OR +15 OR +16 OR +18 OR               
048620                                +24 OR +25 OR +26 OR +28                  
048700         MOVE 61               TO  KERS-KDORDBEK                          
048800         MOVE JA               TO  AVSLUTA-SW                             
048900     END-IF                                                               
049000                                                                          
049600     IF W-KDERS                = +11 OR +17 OR                            
049610                                 +21 OR +22 OR +23 OR +27                 
049700         MOVE 41               TO  KERS-KDORDBEK                          
049800     END-IF                                                               
050600     .                                                                    
050700     EJECT                                                                
050800 S06-CALL-W009KSIF        SECTION.                                        
050900     SKIP2                                                                
051000     CALL W009KSIF USING FLT LGD KSIFF                                    
051100     .                                                                    
051200     EJECT                                                                
051300 S07-KOLLA-LAGERSALDO     SECTION.                                        
051400     MOVE NEJ                   TO LAG-SW                                 
051500     MOVE ZERO                  TO W-SALDO                                
051600     MOVE KERS-IDARTNR          TO W-ARTC-IDARTNR                         
051700     MOVE KERS-IDDC             TO W-IDDC                                 
051710                                                                          
051800     IF WC-CDC-SE = W-IDDC                                                
051900       PERFORM IMS-GU-WDK601                                              
052000       PERFORM IMS-GNP-WDK611X                                            
052100       IF SEGMENT-SAKNAS                                                  
052200         MOVE ZERO        TO CLAG-KVLS                                    
052300                             CLAG-KVRESS                                  
052400       END-IF                                                             
052500       PERFORM IMS-GU-WDK901                                              
052600       IF SEGMENT-SAKNAS                                                  
052700         MOVE ZERO        TO ART-KVOKS-BULK                               
052800                             ART-KVOKS-DAG                                
052900                             ART-KVOKS-VOR                                
053000       END-IF                                                             
053100       COMPUTE W-SALDO =  CLAG-KVLS -                                     
053200                          CLAG-KVRESS -                                   
053300                          ART-KVOKS-BULK -                                
053400                          ART-KVOKS-DAG -                                 
053500                          ART-KVOKS-VOR                                   
053600     ELSE                                                                 
053700       MOVE KERS-IDDC           TO W-IDDC                                 
053800       PERFORM IMS-GU-WDK701                                              
053900       IF SEGMENT-FINNS                                                   
054000        PERFORM IMS-GNP-WDK711                                            
054100        IF SEGMENT-FINNS                                                  
054200         COMPUTE W-SALDO = SLAG-KVLS -                                    
054300                           SLAG-KVOKS-DAG -                               
054400                           SLAG-KVOKS-BULK                                
054500        END-IF                                                            
054600       END-IF                                                             
054700     END-IF                                                               
054710                                                                          
054800     IF W-SALDO NOT < KERS-KVBEART                                        
054900       MOVE JA                 TO LAG-SW                                  
055000*      LÄGG IN EN ORDERERKÄNNANDE KOD PÅ URSPRUNGSARTIKELN                
055100*      OCKSÅ                                                              
055200     END-IF                                                               
055300     .                                                                    
055400     EJECT                                                                
055500 S08-KOLLA-LAGERSALDO     SECTION.                                        
055600     MOVE NEJ                   TO LAGE-SW                                
055700     MOVE ZERO                  TO W-SALDO                                
055800     MOVE ERSA11-IDARTNR-TILLK  TO W-ARTC-IDARTNR                         
055900     MOVE KERS-IDDC             TO W-IDDC                                 
056000     IF WC-CDC-SE = W-IDDC                                                
056100       PERFORM IMS-GU-WDK601                                              
056200       PERFORM IMS-GNP-WDK611X                                            
056300       IF SEGMENT-SAKNAS                                                  
056400         MOVE ZERO        TO CLAG-KVLS                                    
056500                             CLAG-KVRESS                                  
056600       END-IF                                                             
056700       PERFORM IMS-GU-WDK901                                              
056800       IF SEGMENT-SAKNAS                                                  
056900         MOVE ZERO        TO ART-KVOKS-BULK                               
057000                             ART-KVOKS-DAG                                
057100                             ART-KVOKS-VOR                                
057200       END-IF                                                             
057300       COMPUTE W-SALDO =  CLAG-KVLS -                                     
057400                          CLAG-KVRESS -                                   
057500                          ART-KVOKS-BULK -                                
057600                          ART-KVOKS-DAG -                                 
057700                          ART-KVOKS-VOR                                   
057800     ELSE                                                                 
057900       MOVE KERS-IDDC           TO W-IDDC                                 
058000       PERFORM IMS-GU-WDK701                                              
058100       IF SEGMENT-FINNS                                                   
058200        PERFORM IMS-GNP-WDK711                                            
058300        IF SEGMENT-FINNS                                                  
058400         COMPUTE W-SALDO = SLAG-KVLS -                                    
058500                           SLAG-KVOKS-DAG -                               
058600                           SLAG-KVOKS-BULK                                
058700        END-IF                                                            
058800       END-IF                                                             
058900     END-IF                                                               
059000     IF W-SALDO NOT < KERS-KVBEART                                        
059100       MOVE JA                 TO LAGE-SW                                 
059200*      LÄGG IN EN ORDERERKÄNNANDE KOD PÅ URSPRUNGSARTIKELN                
059300*      OCKSÅ                                                              
059400     END-IF                                                               
059500     .                                                                    
059600     EJECT                                                                
059700*    ---- IMS SEKTIONER                                                   
059800                                                                          
059900 IMS-GET-ERSA-ART-KVAL SECTION.                                           
060000                                                                          
060100     STRING 'WLERSA01(IDARTNR  =' W-ERSA-IDARTNR-X ')'                    
060200            DELIMITED BY SIZE INTO SSA1                                   
060300     MOVE '  GE'               TO GODK-STATUSKODER                        
060400     CALL CBLTDLI USING GU  ERSA-PCB ERSA01-WLERSA01 SSA1                 
060500     MOVE ERSA-STATUS-CODE     TO STATUS-WS                               
060600     PERFORM IMS-STATUSKONTROLL                                           
060700     .                                                                    
060800     SKIP2                                                                
060900 IMS-GET-ERSA-TLK-OKVAL SECTION.                                          
061000                                                                          
061100     MOVE 'WLERSA11 '          TO SSA1                                    
061200     MOVE '  GE'               TO GODK-STATUSKODER                        
061300     CALL CBLTDLI USING GNP ERSA-PCB ERSA11-WLERSA11 SSA1                 
061400     MOVE ERSA-STATUS-CODE     TO STATUS-WS                               
061500     PERFORM IMS-STATUSKONTROLL                                           
061600     .                                                                    
061700     EJECT                                                                
061800 IMS-GU-WDK601 SECTION.                                                   
061900                                                                          
062000     STRING 'WLARTC01(IDARTNR  =' W-ARTC-IDARTNR-X ')'                    
062100            DELIMITED BY SIZE INTO SSA1                                   
062200     MOVE '  '                 TO GODK-STATUSKODER                        
062300     CALL CBLTDLI USING GU  ARTC-PCB WLARTC01 SSA1                        
062400     MOVE ARTC-STATUS-CODE     TO STATUS-WS                               
062500     PERFORM IMS-STATUSKONTROLL                                           
062600     .                                                                    
062700     SKIP2                                                                
062800 IMS-GNP-WDK611 SECTION.                                                  
062900                                                                          
063000     MOVE   'WLARTC11' TO SSA1                                            
063100     MOVE '  '                 TO GODK-STATUSKODER                        
063200     CALL CBLTDLI USING GNP ARTC-PCB WLARTC11 SSA1                        
063300     MOVE ARTC-STATUS-CODE     TO STATUS-WS                               
063400     PERFORM IMS-STATUSKONTROLL                                           
063500     .                                                                    
063600     SKIP2                                                                
063700 IMS-GNP-WDK611X  SECTION.                                                
063800                                                                          
063900     MOVE   'WLARTC11' TO SSA1                                            
064000     MOVE '  GE'               TO GODK-STATUSKODER                        
064100     CALL CBLTDLI USING GNP ARTC-PCB WLARTC11 SSA1                        
064200     MOVE ARTC-STATUS-CODE     TO STATUS-WS                               
064300     PERFORM IMS-STATUSKONTROLL                                           
064400     .                                                                    
064500     SKIP2                                                                
064600 IMS-GU-WDK701 SECTION.                                                   
064700                                                                          
064800     STRING 'WLARTS01(IDARTNR  =' W-ARTC-IDARTNR-X ')'                    
064900            DELIMITED BY SIZE INTO SSA1                                   
065000     MOVE '  GE'               TO GODK-STATUSKODER                        
065100     CALL CBLTDLI USING GU  WDK7-PCB WDK701 SSA1                          
065200     MOVE WDK7-STATUS-CODE     TO STATUS-WS                               
065300     PERFORM IMS-STATUSKONTROLL                                           
065400     .                                                                    
065500     SKIP2                                                                
065600 IMS-GNP-WDK711 SECTION.                                                  
065700                                                                          
065800     STRING 'WLARTS11(IDDC     =' W-IDDC-X ')'                            
065900            DELIMITED BY SIZE INTO SSA1                                   
066000     MOVE '  GE'               TO GODK-STATUSKODER                        
066100     CALL CBLTDLI USING GNP WDK7-PCB WDK711 SSA1                          
066200     MOVE WDK7-STATUS-CODE     TO STATUS-WS                               
066300     PERFORM IMS-STATUSKONTROLL                                           
066400     .                                                                    
066500     SKIP2                                                                
066600 IMS-GU-WDK901 SECTION.                                                   
066700                                                                          
066800     STRING 'WLARTM01(IDARTNR  =' W-ARTC-IDARTNR-X ')'                    
066900            DELIMITED BY SIZE INTO SSA1                                   
067000     MOVE '  GE'               TO GODK-STATUSKODER                        
067100     CALL CBLTDLI USING GU  WDK9-PCB WDK901 SSA1                          
067200     MOVE WDK9-STATUS-CODE     TO STATUS-WS                               
067300     PERFORM IMS-STATUSKONTROLL                                           
067400     .                                                                    
067500     SKIP2                                                                
067600 IMS-STATUSKONTROLL SECTION.                                              
067700                                                                          
067800     SET STATUS-IX TO 1                                                   
067900     SEARCH GODK-STATUS                                                   
068000       AT END CALL FELLOG                                                 
068100       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                  
068200     END-SEARCH                                                           
068300     .                                                                    
068400     EJECT                                                                
068500*    -COPY WY2000P2                                                       
