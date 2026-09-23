000100*COMPOPT STDSUB=YES                                                       
000200 ID DIVISION.                                                             
000300     SKIP2                                                                
000400 PROGRAM-ID.     W413ADRS.                                                
000500 AUTHOR.         LASSE CALAIS.                                            
000600 DATE-WRITTEN.   JUNI -90.                                                
000700                                                                          
000800     REMARKS.                                                             
000900*                                                                         
001000*    DETTA ÄR EN SUBMODUL SOM ANROPAS VID RADBEHANDLING.                  
001100*    MODULEN RÄTTAR TILL ARTIKELNS LAGERADRESS, SÅ ATT DEN KOMMER         
001200*    I PLOCKORDNING (GÄLLER PLATSER < 10000 PÅ CDC).                      
001300*                                                                         
001400*    LAGEROMRÅDE 0 ÄNDRAS ALLTID TILL 1. DETTA PGA AV ATT                 
001500*    LAGEROMRÅDESTABELLEN I WDQ212 ÄR 1 TILL 99. DET FINNS INTE           
001600*    NÅGOT LAGEROMRÅDE NOLL... MEN EFTERSOM MAN MÅSTE LAGRA DEN           
001700*    DATA SOM HÖR TILL DE ARTIKLAR SOM HAR LAGEROMRÅDE NOLL LÄGGS         
001800*    DETTA I LAGEROMRÅDE 1.                                               
001900*                                                                         
002000*    LAGEROMRÅDE SÄTTS TILL 25 OM FÖLJANDE ÄR UPPFYLLT:                   
002100*         - IDDC        = 11                                              
002200*         - DISTRIKT    > 1000                                            
002300*         - ORDERKLASS  = 4                                               
002400*         - LAGEROMRÅDE = 10, 20, 21                                      
002500*         - RADVOLYMEN  > 0.500 M3                                        
002600*         - LAGEROMRÅDE = 35                                              
002700*         - RADVOLYMEN  > 2.500 M3                                        
002800*                                                                         
002900*    ÄNDRAR LAGEROMRÅDE FÖR HF & AK-PLOCK                                 
003000*    GÄLLER ENDAST CDC OCH FÖRBIORDER                                     
003100*                                                                         
003200*    LÄNKAREA :    W413ADRS                                               
003300*                                                                         
003400     EJECT                                                                
003500                                                                          
003600 DATA DIVISION.                                                           
003700 WORKING-STORAGE SECTION.                                                 
003800                                                                          
003900*    -- CHECKED BY WY2000                                                 
004000 01  IDPGM                       PIC X(08)   VALUE 'W413ADRS'.            
004100 01  JA                          PIC X(1)    VALUE 'J'.                   
004200 01  SPEC-FORBI                  PIC X(1)    VALUE 'S'.                   
004300 01  TAB-IX                      PIC S9(9)   VALUE 0 COMP SYNC.           
004400 01  WS-RADVOLYM                 PIC S9(4)V9(3)      COMP-3.              
004500 01  ARB-ADPLATS.                                                         
004600     03 WI-ADPLATS               PIC 9(5)    VALUE ZERO.                  
004700     03 FILLER                   REDEFINES WI-ADPLATS.                    
004800        05 WI-ADPLATS-1-2        PIC 9(2).                                
004900        05 FILLER                PIC X.                                   
005000        05 WI-ADPLATS-4-5        PIC 9(2).                                
005100                                                                          
005200     03 WU-ADPLATS               PIC 9(5)    VALUE ZERO.                  
005300     03 FILLER                   REDEFINES WU-ADPLATS.                    
005400        05 WU-ADPLATS-1          PIC 9.                                   
005500        05 WU-ADPLATS-2-3        PIC 9(2).                                
005600        05 WU-ADPLATS-4-5        PIC 9(2).                                
005700*                                                                         
005800*      --- VALID IDDC CODES                                               
005900*                                                                         
006000*01    -COPY WWDC99                                                       
006100       EJECT                                                              
006200*                                                                         
006300*   -COPY W413WHFA                                                        
006400*                                                                         
006500 01  FILLER                      PIC X(16)  VALUE 'WWDIST02    '.         
006600*   -COPY WWDIST02                                                        
006700*                                                                         
006800 01  FILLER                      PIC X(16)  VALUE 'SKANDINAVIEN'.         
006900*   -COPY WWDIST03                                                        
007000*                                                                         
007100 01  FILLER                      PIC X(16)  VALUE 'REFILLDISTR'.          
007200*   -COPY WWDIST35                                                        
007300*                                                                         
007400     EJECT                                                                
007500 LINKAGE SECTION.                                                         
007600*                                                                         
007700*   -COPY W413ADRS                                                        
007800     EJECT                                                                
007900 PROCEDURE DIVISION  USING ADRS-W413ADRS.                                 
008000                                                                          
008100     PERFORM A-JUSTERA-LAGEROMRADE                                        
008200                                                                          
008300     PERFORM B-JUSTERA-LAGERPLATS                                         
008400                                                                          
008500     PERFORM C-KONTROLLERA-HF-AK-PLOCK                                    
008600                                                                          
008700     GOBACK                                                               
008800     .                                                                    
008900     EJECT                                                                
009000 A-JUSTERA-LAGEROMRADE SECTION.                                           
009100                                                                          
009200     MOVE ADRS-IDDISTR-IN         TO DIST02-IDDISTR                       
009300     MOVE ADRS-IDDISTR-IN         TO DIST03-IDDISTR                       
009400     MOVE ADRS-IDDISTR-IN         TO DIST35-IDDISTR                       
009500     MOVE ADRS-IDDC-IN             TO WS-IDDC                             
009600     IF ADRS-ADLAGOMR-IN = ZERO                                           
009700        MOVE 1 TO ADRS-ADLAGOMR-UT                                        
009800     ELSE                                                                 
009900                                                                          
010000        IF CDC-SE                                                         
010100           AND                                                            
010200           ADRS-ADLAGOMR-IN = 21                                          
010300           AND                                                            
010400          (ADRS-KDORDKL-IN = 0 OR 1 OR 2 OR 3 OR 4)                       
010500           MOVE ADRS-ADLAGOMR-IN TO ADRS-ADLAGOMR-UT                      
010600        ELSE                                                              
010700                                                                          
010800           MOVE ADRS-ADLAGOMR-IN TO ADRS-ADLAGOMR-UT                      
010900        END-IF                                                            
011000                                                                          
011100        IF CDC-SE                                                         
011200           AND                                                            
011300           ADRS-ADLAGOMR-IN = 31                                          
011400           AND                                                            
011500          (ADRS-KDORDKL-IN = 0 OR 1 OR 2 OR 3 OR 4)                       
011600           AND                                                            
011700           DIST03-HELA-NORDEN                                             
011800                                                                          
011900           MOVE 30 TO ADRS-ADLAGOMR-UT                                    
012000        END-IF                                                            
012100     END-IF                                                               
012200                                                                          
012300     IF (CDC-SE                                                           
012400        AND                                                               
012500        ADRS-KDCALL-IN   = 1                                              
012600        AND                                                               
012700        ADRS-KDORDKL-IN  = 4                                              
012800        AND                                                               
012900        ADRS-IDDISTR-IN  > 1200)                                          
013000        OR                                                                
013100       (CDC-SE                                                            
013200        AND                                                               
013300        ADRS-KDCALL-IN   = 1                                              
013400        AND                                                               
013500        (DIST35-CDC-NL-REFILL                                             
013600         OR DIST35-CDC-FR-REFILL                                          
013700         OR DIST35-CDC-GB-REFILL                                          
013800         OR DIST35-CDC-GB-3A-REFILL                                       
013900         OR DIST35-CDC-ES-REFILL                                          
014000         OR DIST35-CDC-IT-REFILL                                          
014100         OR DIST35-CDC-AT-REFILL                                          
014200         OR DIST35-REFILL-JP                                              
014300         OR DIST35-CDC-AU-REFILL                                          
014400         OR DIST35-CDC-1B-REFILL                                          
014500         OR DIST35-REFILL-NA)                                             
014700        AND                                                               
014800        ADRS-KDORDKL-IN = 2)                                              
014900        OR                                                                
015000       (CDC-SE                                                            
015100        AND                                                               
015200        ADRS-KDCALL-IN  = 1                                               
015300        AND                                                               
015400        ADRS-KDORDKL-IN = 2                                               
015500        AND                                                               
015600        ADRS-IDDISTR-IN  > 3000)                                          
015700        OR                                                                
015800       (CDC-SE                                                            
015900        AND                                                               
016000        ADRS-KDCALL-IN  = 1                                               
016100        AND                                                               
016200        ADRS-KDORDKL-IN = 3                                               
016300        AND                                                               
016400        DIST02-TAIWAN)                                                    
016500                                                                          
016600        IF ADRS-ADLAGOMR-IN = 10 OR 13 OR 20 OR 21 OR 22                  
016700                                                                          
016800           COMPUTE WS-RADVOLYM = ((ADRS-KVBEART-Q-IN *                    
016900                                   ADRS-VLARTNTO-IN) / 1000000)           
017000           IF WS-RADVOLYM > 0.500                                         
017100              IF (DIST35-REFILL-JP                                        
017200              OR DIST35-CDC-AU-REFILL)                                    
017300              AND ADRS-ADLAGOMR-IN = 14                                   
017400                CONTINUE                                                  
017500              ELSE                                                        
017600                MOVE ADRS-ADLAGOMR-IN   TO ADRS-ADLAGOMR-UT               
017610*AREA 25 IS NOT NEEDED FOR NOW 190612 USE PRESENT AREA                    
017700****            MOVE 25 TO ADRS-ADLAGOMR-UT                               
017800             END-IF                                                       
017900           END-IF                                                         
018000*       ELSE                                                              
018100*          IF ADRS-ADLAGOMR-IN = 35                                       
018200*                                                                         
018300*             COMPUTE WS-RADVOLYM = ((ADRS-KVBEART-Q-IN *                 
018400*                                     ADRS-VLARTNTO-IN) / 1000000)        
018500*             IF WS-RADVOLYM > 2.500                                      
018600*                MOVE 25 TO ADRS-ADLAGOMR-UT                              
018700*             END-IF                                                      
018800*          END-IF                                                         
018900        END-IF                                                            
019000     END-IF                                                               
019100     .                                                                    
019200     EJECT                                                                
019300 B-JUSTERA-LAGERPLATS SECTION.                                            
019400                                                                          
019500     MOVE ADRS-ADPLATS-IN TO ADRS-ADPLATS-UT                              
019600     .                                                                    
019700     EJECT                                                                
019800 C-KONTROLLERA-HF-AK-PLOCK SECTION.                                       
019900                                                                          
020000     IF CDC-SE                                                            
020100        AND                                                               
020200       (ADRS-FLFORBI-IN  = JA OR ADRS-FLFORBI-IN = SPEC-FORBI)            
020300        AND                                                               
020400        ADRS-BEVARREF-IN NOT = SPACE                                      
020500        MOVE 1 TO TAB-IX                                                  
020600        PERFORM UNTIL TAB-IX > MAX-HFAK-IX                                
020700           IF ADRS-BEVARREF-IN (1:3) = HFAK-BERADREF (TAB-IX)             
020800           OR (ADRS-BEVARREF-IN (1:1) = '#'                               
020900           AND ADRS-BEVARREF-IN (2:2) NOT = SPACE                         
021000           AND HFAK-BERADREF-1 (TAB-IX) = '#')                            
021100              MOVE HFAK-ADLAGOMR (TAB-IX) TO ADRS-ADLAGOMR-UT             
021200              MOVE ADRS-ADPLATS-IN        TO ADRS-ADPLATS-UT              
021300              MOVE 99                     TO TAB-IX                       
021400           ELSE                                                           
021500              ADD  1                      TO TAB-IX                       
021600           END-IF                                                         
021700        END-PERFORM                                                       
021800     END-IF                                                               
021900     .                                                                    
