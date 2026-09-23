000100*COMPOPT STDSUB=YES                                                       
000200 ID DIVISION.                                                             
000300     SKIP2                                                                
000400 PROGRAM-ID.     W411STOR.                                                
000500 AUTHOR.         LARS THELL CAP GEMINI LOCIC.                             
000600 DATE-WRITTEN.   MAJ   -90.                                               
000700                                                                          
000800     REMARKS.                                                             
000900*                                                                         
001000*        PROGRAMMET ÄR EN SUBMODUL TILL ETT MPP-PGM                       
001100*                                                                         
001200*    FUNKTION.                                                            
001300*      - BERÄKNA GRÄNS FÖR STORT UTTAG FÖR AKTUELL                        
001400*        ARTIKEL                                                          
001500*      - BESTÄM OM BESTÄLLD KVANTITET ÄR FÖR STOR.                        
001600*                                                                         
001700*        LÄNKAREA: W411STOR                                               
001800                                                                          
001900     SKIP3                                                                
002000 ENVIRONMENT DIVISION.                                                    
002100     SKIP3                                                                
002200                                                                          
002300 DATA DIVISION.                                                           
002400 WORKING-STORAGE SECTION.                                                 
002500                                                                          
002600*    -- CHECKED BY WY2000                                                 
002700 77  IDPGM                       PIC X(08)   VALUE 'W411STOR'.            
002800 77  JA                          PIC X       VALUE 'J'.                   
002900 77  NEJ                         PIC X       VALUE 'N'.                   
003000 77  SPEC-FORBI                  PIC X       VALUE 'S'.                   
003100                                                                          
003200 77  STORT-UTTAG-SW              PIC X       VALUE 'J'.                   
003300     88 STORT-UTTAG                          VALUE 'J'.                   
003400                                                                          
003410 01  ALL-EXCLUDED-DISTRICT       PIC 9(4).                                
003420     88 EXCLUDED-DISTRICT                    VALUE 2602                   
003421                                                   3160                   
003422                                                   5619 5810              
003423                                                   6200 6251 6589         
003424                                                   7050.                  
003430                                                                          
003500*01  -COPY WWPRODSL                                                       
003600                                                                          
003700 LINKAGE SECTION.                                                         
003800*                                                                         
003900*   -COPY W411STOR                                                        
004000*                                                                         
004100     EJECT                                                                
004200                                                                          
004300 PROCEDURE DIVISION  USING STOR-W411STOR.                                 
004400                                                                          
004500     PERFORM A-INIT                                                       
004600                                                                          
004700     MOVE STOR-KDPRODSL          TO TEST-KDPRODSL                         
004710     MOVE STOR-IDDISTR           TO ALL-EXCLUDED-DISTRICT                 
004800     IF EXCLUDED-DISTRICT             OR                                  
004810        STOR-FLFORBI           = JA   OR                                  
004900        STOR-FLFORBI     = SPEC-FORBI OR                                  
005000        STOR-FLOVRLEV          = JA   OR                                  
005100        STOR-FLORDSPE          = JA   OR                                  
005200        STOR-KDPROTYP          = 'F'  OR                                  
005300        STOR-KVBEART-Q         < +3   OR                                  
005400        STOR-IDKAMPRF          > ZERO OR                                  
005500        STOR-KDERS             > ZERO OR                                  
005600        STOR-IDLEVNR      NOT = SPACE OR                                  
005700       (STOR-IDKUNDRF-RO       NOT = '0000000   ') OR                     
005800        STOR-KDORDKL           = +0   OR                                  
005900        STOR-IDSYSTEM          = 'OREL'            OR                     
006000        STOR-BERADREF          = 'W480      '      OR                     
006100        KDPRODSL-EMB                                                      
006200         CONTINUE                                                         
006300     ELSE                                                                 
006400        PERFORM C-KONTR-KVSLUTKP-RERF-ART                                 
006500                                                                          
006600        IF STORT-UTTAG                                                    
006700                                                                          
006800           PERFORM D-KONTR-KDVVKL-KVBEART-Q                               
006900        END-IF                                                            
007000                                                                          
007100     END-IF                                                               
007200                                                                          
007300     GOBACK                                                               
007400     .                                                                    
007500     EJECT                                                                
007600                                                                          
007700 A-INIT   SECTION.                                                        
007800                                                                          
007900     MOVE ZERO                 TO STOR-KDORDBEK                           
008000     MOVE JA                   TO STORT-UTTAG-SW                          
008100     .                                                                    
008200     EJECT                                                                
008300                                                                          
008400 C-KONTR-KVSLUTKP-RERF-ART SECTION.                                       
008500                                                                          
008600     IF STOR-KVSLUTKP          > +0                                       
008700          MOVE JA              TO STORT-UTTAG-SW                          
008800     ELSE                                                                 
008900        IF STOR-RERF-ART       >  +1.5000                                 
009000            MOVE NEJ           TO STORT-UTTAG-SW                          
009100        ELSE                                                              
009200            MOVE JA            TO STORT-UTTAG-SW                          
009300        END-IF                                                            
009400     END-IF                                                               
009500     .                                                                    
009600     EJECT                                                                
009700                                                                          
009800 D-KONTR-KDVVKL-KVBEART-Q  SECTION.                                       
009900                                                                          
010000     EVALUATE TRUE                                                        
010100     WHEN STOR-KDVVKL          =  +1 AND                                  
010200          STOR-KVBEART-Q       > STOR-KVPB-SEP * 8 / 4.33                 
010300          MOVE 70             TO STOR-KDORDBEK                            
010400                                                                          
010500     WHEN STOR-KDVVKL          = +2 AND                                   
010600          STOR-KVBEART-Q       > STOR-KVPB-SEP * 4 / 4.33                 
010700          MOVE 70             TO STOR-KDORDBEK                            
010800                                                                          
010900     WHEN STOR-KDVVKL          = +3 AND                                   
011000          STOR-KVBEART-Q       > STOR-KVPB-SEP * 2 / 4.33                 
011100          MOVE 70             TO STOR-KDORDBEK                            
011200                                                                          
011300     WHEN STOR-KDVVKL          = +4 AND                                   
011400          STOR-KVBEART-Q       > STOR-KVPB-SEP * 1 / 4.33                 
011500          MOVE 70             TO STOR-KDORDBEK                            
011600                                                                          
011700     WHEN STOR-KDVVKL          = +5 AND                                   
011800          STOR-KVBEART-Q       > STOR-KVPB-SEP * 1 / 4.33                 
011900          MOVE 70             TO STOR-KDORDBEK                            
012000     END-EVALUATE                                                         
012100     .                                                                    
