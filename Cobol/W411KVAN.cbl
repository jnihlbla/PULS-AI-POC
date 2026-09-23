000100*COMPOPT STDSUB=YES                                                       
000200 ID DIVISION.                                                             
000300     SKIP2                                                                
000400 PROGRAM-ID.     W411KVAN.                                                
000500 AUTHOR.         LASSI OLGRENER.                                          
000600 DATE-WRITTEN.   APRIL -90.                                               
000700                                                                          
000800     REMARKS.                                                             
000900*                                                                         
001000*        PROGRAMMET ÄR EN SUBMODUL TILL ETT MPP-PGM                       
001100*                                                                         
001200*    FUNKTION.                                                            
001300*      - SÄTTER KVBEART-Q PÅ ORDERRADEN.                                  
001400*        KAN ANTINGEN VARA BESTÄLLT ANTAL ELLER BERÄKNAT                  
001500*        KVANTANPASSAT ANTAL.                                             
001600*                                                                         
001700*        LÄNKAREA: W411KVAN                                               
001800*                                                                         
001900*    CHANGE LOG:                                                          
002000*                                                                         
002100                                                                          
002200     SKIP3                                                                
002300 ENVIRONMENT DIVISION.                                                    
002400     SKIP3                                                                
002500                                                                          
002600 DATA DIVISION.                                                           
002700 WORKING-STORAGE SECTION.                                                 
002800                                                                          
002900*    -- CHECKED BY WY2000                                                 
003000 77  IDPGM                       PIC X(08)   VALUE 'W411KVAN'.            
003100 77  JA                          PIC X       VALUE 'J'.                   
003200 77  YES                         PIC X       VALUE 'Y'.                   
003300 77  NEJ                         PIC X       VALUE 'N'.                   
003400 77  SPEC-FORBI                  PIC X       VALUE 'S'.                   
003500 77  WS-FLKVBRYT                 PIC X       VALUE 'N'.                   
003600*                                                                         
003700 01  IDFKNGRP-KVANT              PIC 9(4).                                
003800     88  FKNGRP-KVANT            VALUE 1000 THRU 1999 3513 3514.          
003810                                                                          
003900*                                                                         
004000 01  W-KVBEART                   PIC 9(7)V9(2).                           
004100 01  FILLER REDEFINES W-KVBEART.                                          
004200     03  W-KVBEART-INT           PIC 9(7).                                
004300     03  W-KVBEART-DEC           PIC 9(2).                                
004400*                                                                         
004500 01  GENERELLA-SUBPROGRAM.                                                
004600     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
004700     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
004800                                                                          
004810*    --- QTY ADAPTATION SKIP DIST                                         
004820*01  -COPY WWDIST06                                                       
004900                                                                          
004910*01  -COPY WWDC99                                                         
004920                                                                          
005000 01  TEST-PRODUKTSLAG            PIC 9(3)    COMP-3.                      
005100*01 FILLER    -COPY WWBYT04       -RED  TEST-PRODUKTSLAG.                 
005200     EJECT                                                                
005300 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
005400     SKIP2                                                                
005500*    --- STATUS-KOD FRÅN IMS                                              
005600 01  STATUS-WS                   PIC XX.                                  
005700     88  SEGMENT-FINNS                       VALUE '  '.                  
005800     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
005900     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
006000     SKIP2                                                                
006100 01  GODK-STATUSKODER.                                                    
006200     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
006300     SKIP2                                                                
006400 01  SSA1                        PIC X(64).                               
006500 01  SSA2                        PIC X(64).                               
006600     EJECT                                                                
006700                                                                          
007000     EJECT                                                                
007010*    --- IMS FUNKTIONSKODER                                               
007020*01  -COPY W0003                                                          
007030     EJECT                                                                
007100*                                                                         
007200 01  FILLER                      PIC X(16)                                
007300                                         VALUE 'NYCKLAR-TILL-DLI'.        
007400     SKIP2                                                                
007500 01  NYCKLAR-TILL-DLI.                                                    
007600     03  W-IDGMT-X.                                                       
007700         05  W-IDDISTR           PIC S9(5)  COMP-3.                       
007800         05  W-IDKUNDNR          PIC S9(7)  COMP-3.                       
007900                                                                          
008000     03  W-WDC101KY-X.                                                    
008100         05  W-IDARTNR-C1        PIC S9(9)   VALUE ZERO COMP-3.           
008200         05  W-IDMARKBO-C1       PIC X       VALUE 'B'.                   
008300                                                                          
008400 01  FILLER               PIC X(16)   VALUE 'WDB201 AREA'.                
008500 01   DLI-IO-WDB201.                                                      
008600*     03  -COPY WDB201                                                    
008700                                                                          
008800 01  FILLER               PIC X(16)   VALUE 'WDC101 AREA'.                
008900 01   DLI-IO-WDC101.                                                      
009000*     03  -COPY WDC101                                                    
009100     EJECT                                                                
009200                                                                          
009300 LINKAGE SECTION.                                                         
009400                                                                          
009500*   -COPY W411KVAN                                                        
009600                                                                          
009700*01  -COPY W0008      -PRE WDB2-                                          
009800     05  FILLER                  PIC X.                                   
009900                                                                          
010000*01  -COPY W0008      -PRE WDC1-                                          
010100     05  FILLER                  PIC X.                                   
010200     EJECT                                                                
010300                                                                          
010400 PROCEDURE DIVISION  USING KVAN-W411KVAN WDB2-PCB WDC1-PCB.               
010500                                                                          
010600     MOVE KVAN-KDKVBRYT-IN    TO KVAN-KDKVBRYT-UT                         
010700     MOVE KVAN-KVBEART-IN     TO KVAN-KVBEART-Q-UT                        
010710     MOVE KVAN-IDDC-IN        TO WS-IDDC                                  
010800     MOVE ZERO                TO KVAN-KDORDBEK-UT                         
010900     MOVE ZERO                TO KVAN-KVQPACK-UT                          
011000                                                                          
011100     PERFORM S01-CHECK-LDC                                                
011200                                                                          
011300     IF GMT-FLLDCKND = JA                                                 
011400     AND WS-FLKVBRYT = JA                                                 
011500     AND KVAN-KDORDKL-IN > +0                                             
011600       IF KVAN-KDSORT-IN = 'ST' OR 'PA'                                   
011700          IF KVAN-KVQPACK-1-IN > 0                                        
011800             PERFORM A-KOLLA-BRYTNING-OCH-KVANT                           
011900          ELSE                                                            
012000             IF KVAN-KVQPACK-0-IN > 0                                     
012100                MOVE 1              TO KVAN-KDKVBRYT-UT                   
012200                MOVE KVAN-KVQPACK-0-IN TO KVAN-KVQPACK-UT                 
012300             END-IF                                                       
012400          END-IF                                                          
012500       ELSE                                                               
012600          PERFORM A-KOLLA-BRYTNING-OCH-KVANT                              
012700       END-IF                                                             
012800     ELSE                                                                 
012900       IF KVAN-FLORDSPE-IN = JA OR                                        
013000          KVAN-FLOVRLEV-IN = JA OR                                        
013100          KVAN-FLEMBORD-IN = JA OR                                        
013200          KVAN-KVBEART-IN  = +0 OR                                        
013300         (KVAN-KDORDKL-IN  = +0 AND KVAN-KVQPACK-0-IN > 0) OR             
013400         (KVAN-IDSYSTEM-IN = 'PROF' OR 'OREL')                            
013500                                                                          
013600         CONTINUE                                                         
013700       ELSE                                                               
013800                                                                          
013900          PERFORM A-KOLLA-BRYTNING-OCH-KVANT                              
014000       END-IF                                                             
014100     END-IF                                                               
014200     GOBACK                                                               
014300     .                                                                    
014400     EJECT                                                                
014500                                                                          
014600 A-KOLLA-BRYTNING-OCH-KVANT SECTION.                                      
014700                                                                          
014720     MOVE  KVAN-IDDISTR-IN         TO DIST06-KVANT                        
014800     IF KVAN-KDORDKL-IN = +0  OR                                          
014810        KVAN-FLFORBI-IN = JA  OR                                          
014900        KVAN-FLFORBI-IN = SPEC-FORBI OR                                   
015000        KVAN-IDKAMPRF-IN > +0 OR                                          
015010        (NDC AND KVAN-KDORDKL-IN = 1) OR                                  
015020        (DIST06-KVANT-EJ AND KVAN-KDORDKL-IN = 1)                         
015100                                                                          
015200        MOVE +2 TO KVAN-KDKVBRYT-UT                                       
015300     ELSE                                                                 
015400        MOVE +0 TO KVAN-KDKVBRYT-UT                                       
015500     END-IF                                                               
015600                                                                          
015700     MOVE KVAN-KDPRODSL-IN TO TEST-PRODUKTSLAG                            
015800                                                                          
015900     IF  (KVAN-KVQPACK-0-IN = +0 OR +1)                                   
016000     AND (KVAN-KVQPACK-1-IN = +0 OR +1)                                   
016100       IF KVAN-KDKVBRYT-UT = +1 OR +2                                     
016200         IF NOT BYT04-PRODUKT                                             
016300           MOVE +0 TO KVAN-KDKVBRYT-UT                                    
016400         END-IF                                                           
016500       END-IF                                                             
016600     ELSE                                                                 
016700        IF KVAN-KDSORT-IN = 'KG' OR 'L' OR 'M'                            
016800        OR KVAN-KDKVBRYT-UT = +0                                          
016900           PERFORM AA-KOLLA-KVQPACK                                       
017000        END-IF                                                            
017100     END-IF                                                               
017200                                                                          
017300     IF KVAN-KVBEART-Q-UT NOT = KVAN-KVBEART-IN                           
017400       IF KVAN-KVBEART-IN > KVAN-KVBEART-Q-UT                             
017500         MOVE 44 TO KVAN-KDORDBEK-UT                                      
017600       ELSE                                                               
017700         MOVE 43 TO KVAN-KDORDBEK-UT                                      
017800       END-IF                                                             
017900     END-IF                                                               
018000     .                                                                    
018100     EJECT                                                                
018200                                                                          
018300 AA-KOLLA-KVQPACK SECTION.                                                
018400                                                                          
018500     IF KVAN-KVQPACK-1-IN > 0                                             
018600        IF KVAN-KDORDKL-IN = +0                                           
018820           MOVE KVAN-KVBEART-IN     TO KVAN-KVBEART-Q-UT                  
019200        ELSE                                                              
019300           MOVE KVAN-KVQPACK-1-IN TO KVAN-KVQPACK-UT                      
019400           COMPUTE W-KVBEART = KVAN-KVBEART-IN / KVAN-KVQPACK-1-IN        
019500           IF W-KVBEART-DEC > 50                                          
019600              COMPUTE KVAN-KVBEART-Q-UT =                                 
019700                      (W-KVBEART-INT + 1) * KVAN-KVQPACK-1-IN             
019800           ELSE                                                           
019900              IF W-KVBEART-INT = ZERO                                     
020000                 MOVE KVAN-KVQPACK-1-IN TO KVAN-KVBEART-Q-UT              
020100              ELSE                                                        
020200                 COMPUTE KVAN-KVBEART-Q-UT =                              
020300                         W-KVBEART-INT * KVAN-KVQPACK-1-IN                
020400              END-IF                                                      
020500           END-IF                                                         
020600        END-IF                                                            
020700     ELSE                                                                 
020800        MOVE KVAN-IDARTNR-IN    TO W-IDARTNR-C1                           
020900        PERFORM IMS-GU-WDC101                                             
021000        IF SEGMENT-SAKNAS                                                 
021100          MOVE ZERO             TO ART-PRARTBTO-MARK                      
021200        END-IF                                                            
021300        MOVE KVAN-KVQPACK-0-IN TO KVAN-KVQPACK-UT                         
021400        MOVE KVAN-IDFKNGRP-IN TO IDFKNGRP-KVANT                           
021500* DO QTY ADAPTATION FOR Q0                                                
021510* 1. IF (Q0 * GROSS PRICE)<= 270SEK  AND GROSS PRICE > 0                  
021520* 2. VALID FUNCTION GROUP                                                 
021600        IF  KVAN-KVQPACK-0-IN > 0                                         
021700        AND (ART-PRARTBTO-MARK * KVAN-KVQPACK-0-IN) <= 270                
021710        AND ART-PRARTBTO-MARK > 0                                         
021800**      AND FKNGRP-KVANT                                                  
021900            COMPUTE W-KVBEART = KVAN-KVBEART-IN /                         
022000            KVAN-KVQPACK-0-IN                                             
022100            IF W-KVBEART-DEC > 50                                         
022200              COMPUTE KVAN-KVBEART-Q-UT =                                 
022300                     (W-KVBEART-INT + 1) * KVAN-KVQPACK-0-IN              
022400            ELSE                                                          
022500               IF W-KVBEART-INT = ZERO                                    
022600                  MOVE KVAN-KVQPACK-0-IN TO KVAN-KVBEART-Q-UT             
022700               ELSE                                                       
022800                  COMPUTE KVAN-KVBEART-Q-UT =                             
022900                          W-KVBEART-INT * KVAN-KVQPACK-0-IN               
023000              END-IF                                                      
023100           END-IF                                                         
023200        ELSE                                                              
023300           IF KVAN-KVQPACK-0-IN > 0                                       
024504             MOVE KVAN-KVBEART-IN TO KVAN-KVBEART-Q-UT                    
024505           END-IF                                                         
025020        END-IF                                                            
025100     END-IF                                                               
025200     .                                                                    
025300     EJECT                                                                
025400                                                                          
025500 S01-CHECK-LDC SECTION.                                                   
025600                                                                          
025700     MOVE  NEJ                     TO WS-FLKVBRYT                         
025800     MOVE  KVAN-IDDISTR-IN         TO W-IDDISTR                           
025900     MOVE  KVAN-IDKUNDNR-IN        TO W-IDKUNDNR                          
026000                                                                          
026100     PERFORM IMS-GU-WDB201                                                
026200                                                                          
026300     IF SEGMENT-FINNS                                                     
026400       IF KVAN-KDORDKL-IN = 1                                             
026500         MOVE GMT-FLKVBRYT-ORDKL1       TO WS-FLKVBRYT                    
026600       ELSE                                                               
026700         IF KVAN-KDORDKL-IN = 2                                           
026800           MOVE GMT-FLKVBRYT-ORDKL2     TO WS-FLKVBRYT                    
026900         ELSE                                                             
027000           IF KVAN-KDORDKL-IN = 3                                         
027100             MOVE GMT-FLKVBRYT-ORDKL3   TO WS-FLKVBRYT                    
027200           ELSE                                                           
027300             IF KVAN-KDORDKL-IN = 4                                       
027400               MOVE GMT-FLKVBRYT-ORDKL4 TO WS-FLKVBRYT                    
027500             END-IF                                                       
027600           END-IF                                                         
027700         END-IF                                                           
027800       END-IF                                                             
027900     END-IF                                                               
028000                                                                          
028100                                                                          
028200     .                                                                    
028300     EJECT                                                                
028400* IMS SEKTIONER                                                           
028500                                                                          
028600 IMS-GU-WDB201      SECTION.                                              
028700     STRING 'WDB201  (IDGMT    =' W-IDGMT-X ')'                           
028800            DELIMITED BY SIZE INTO SSA1                                   
028900     MOVE '  GE' TO GODK-STATUSKODER                                      
029000     CALL CBLTDLI USING GU WDB2-PCB DLI-IO-WDB201 SSA1                    
029100     MOVE WDB2-STATUS-CODE TO STATUS-WS                                   
029200     PERFORM IMS-STATUSKONTROLL                                           
029300     SKIP3                                                                
029400     .                                                                    
029500     EJECT                                                                
029600 IMS-GU-WDC101 SECTION.                                                   
029700     STRING 'WDC101  (WDC101KY =' W-WDC101KY-X ')'                        
029800          DELIMITED BY SIZE INTO SSA1                                     
029900     MOVE '  GE' TO GODK-STATUSKODER                                      
030000     CALL CBLTDLI USING GU WDC1-PCB DLI-IO-WDC101 SSA1                    
030100     MOVE WDC1-STATUS-CODE TO STATUS-WS                                   
030200     PERFORM IMS-STATUSKONTROLL                                           
030300     .                                                                    
030400     EJECT                                                                
030500                                                                          
030600 IMS-STATUSKONTROLL SECTION.                                              
030700     SET STATUS-IX TO 1                                                   
030800     SEARCH GODK-STATUS AT END CALL FELLOG                                
030900       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                  
031000     END-SEARCH                                                           
031100     CONTINUE                                                             
031200     .                                                                    
