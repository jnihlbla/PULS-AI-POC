000100*COMPOPT STDSUB=YES                                                       
000200 ID DIVISION.                                                             
000300 PROGRAM-ID.     W100LPC.                                                 
000400 AUTHOR.         BO HAMMARIN.                                             
000500 DATE-WRITTEN.   NOV 2000.                                                
000600                                                                          
000700     REMARKS.                                                             
000800*                                                                         
000900*        PROGRAMMET ÄR EN SUBMODUL                                        
001000*                                                                         
001100*    FUNKTION.                                                            
001200*      - TILLDELAR LOKALT PRODUKTSLAG                                     
001300*        UTIFRÅN INMATADE PARAMETRAR                                      
001400*                                                                         
001500*    LÄNKAREA :    W100LPC                                                
001600*                                                                         
001700                                                                          
001800 ENVIRONMENT DIVISION.                                                    
001900     EJECT                                                                
002000                                                                          
002100 DATA DIVISION.                                                           
002200 WORKING-STORAGE SECTION.                                                 
002300                                                                          
002400*    -- CHECKED BY WY2000                                                 
002500 77  IDPGM                       PIC X(8)    VALUE 'W100LPC '.            
002600 77  JA                          PIC X       VALUE 'J'.                   
002700 77  NEJ                         PIC X       VALUE 'N'.                   
002800 77  SW-TRAEFF                   PIC X(1)    VALUE 'N'.                   
002900 77  TAB-IX                      PIC S9(3)   VALUE ZERO COMP-3.           
003000 77  TAB-IX-MAX                  PIC S9(3)   VALUE +17  COMP-3.           
003100     EJECT                                                                
003110*                                                                         
003120*01  -COPY WWPRODSL                                                       
003200                                                                          
003300*01  -COPY WWBYT03                                                        
003400     EJECT                                                                
003500                                                                          
003600*    --- TABLE FOR                                                        
003700*        1 PRODUCTCODE                                                    
003800*        2 FUNCTIONGROUP FROM                                             
003900*        3 FUNCTIONGROUP TO                                               
004000*        4 LOC. PRODUCT                                                   
004100 01  TABELL-1.                                                            
004200     03  FILLER                  PIC 9(3)    VALUE 011.                   
004300     03  FILLER                  PIC 9(4)    VALUE 3111.                  
004400     03  FILLER                  PIC 9(4)    VALUE 3111.                  
004500     03  FILLER                  PIC 9(2)    VALUE 018.                   
004600     03  FILLER                  PIC 9(3)    VALUE 011.                   
004700     03  FILLER                  PIC 9(4)    VALUE 7702.                  
004800     03  FILLER                  PIC 9(4)    VALUE 7702.                  
004900     03  FILLER                  PIC 9(2)    VALUE 96.                    
005000     03  FILLER                  PIC 9(3)    VALUE 011.                   
005100     03  FILLER                  PIC 9(4)    VALUE 7724.                  
005200     03  FILLER                  PIC 9(4)    VALUE 7724.                  
005300     03  FILLER                  PIC 9(2)    VALUE 96.                    
005400     03  FILLER                  PIC 9(3)    VALUE 011.                   
005500     03  FILLER                  PIC 9(4)    VALUE 7726.                  
005600     03  FILLER                  PIC 9(4)    VALUE 7726.                  
005700     03  FILLER                  PIC 9(2)    VALUE 96.                    
005800     03  FILLER                  PIC 9(3)    VALUE 014.                   
005900     03  FILLER                  PIC 9(4)    VALUE 3471.                  
006000     03  FILLER                  PIC 9(4)    VALUE 3471.                  
006100     03  FILLER                  PIC 9(2)    VALUE 15.                    
006200     03  FILLER                  PIC 9(3)    VALUE 015.                   
006300     03  FILLER                  PIC 9(4)    VALUE 2750.                  
006400     03  FILLER                  PIC 9(4)    VALUE 2759.                  
006500     03  FILLER                  PIC 9(2)    VALUE 15.                    
006600     03  FILLER                  PIC 9(3)    VALUE 016.                   
006700     03  FILLER                  PIC 9(4)    VALUE 7702.                  
006800     03  FILLER                  PIC 9(4)    VALUE 7702.                  
006900     03  FILLER                  PIC 9(2)    VALUE 97.                    
006910     03  FILLER                  PIC 9(3)    VALUE 025.                   
006920     03  FILLER                  PIC 9(4)    VALUE 9910.                  
006930     03  FILLER                  PIC 9(4)    VALUE 9919.                  
006940     03  FILLER                  PIC 9(2)    VALUE 25.                    
007000     03  FILLER                  PIC 9(3)    VALUE 016.                   
007100     03  FILLER                  PIC 9(4)    VALUE 7724.                  
007200     03  FILLER                  PIC 9(4)    VALUE 7724.                  
007300     03  FILLER                  PIC 9(2)    VALUE 96.                    
007400     03  FILLER                  PIC 9(3)    VALUE 016.                   
007500     03  FILLER                  PIC 9(4)    VALUE 3900.                  
007600     03  FILLER                  PIC 9(4)    VALUE 3999.                  
007700     03  FILLER                  PIC 9(2)    VALUE 73.                    
007800     03  FILLER                  PIC 9(3)    VALUE 021.                   
007900     03  FILLER                  PIC 9(4)    VALUE 2841.                  
008000     03  FILLER                  PIC 9(4)    VALUE 2841.                  
008100     03  FILLER                  PIC 9(2)    VALUE 13.                    
008200     03  FILLER                  PIC 9(3)    VALUE 091.                   
008300     03  FILLER                  PIC 9(4)    VALUE 3111.                  
008400     03  FILLER                  PIC 9(4)    VALUE 3111.                  
008500     03  FILLER                  PIC 9(2)    VALUE 11.                    
008600     03  FILLER                  PIC 9(3)    VALUE 091.                   
008700     03  FILLER                  PIC 9(4)    VALUE 7702.                  
008800     03  FILLER                  PIC 9(4)    VALUE 7702.                  
008900     03  FILLER                  PIC 9(2)    VALUE 97.                    
009000     03  FILLER                  PIC 9(3)    VALUE 091.                   
009100     03  FILLER                  PIC 9(4)    VALUE 7724.                  
009200     03  FILLER                  PIC 9(4)    VALUE 7724.                  
009300     03  FILLER                  PIC 9(2)    VALUE 97.                    
009400     03  FILLER                  PIC 9(3)    VALUE 094.                   
009500     03  FILLER                  PIC 9(4)    VALUE 3471.                  
009600     03  FILLER                  PIC 9(4)    VALUE 3471.                  
009700     03  FILLER                  PIC 9(2)    VALUE 16.                    
009800     03  FILLER                  PIC 9(3)    VALUE 095.                   
009900     03  FILLER                  PIC 9(4)    VALUE 3900.                  
010000     03  FILLER                  PIC 9(4)    VALUE 3999.                  
010100     03  FILLER                  PIC 9(2)    VALUE 95.                    
010200     03  FILLER                  PIC 9(3)    VALUE 096.                   
010300     03  FILLER                  PIC 9(4)    VALUE 3900.                  
010400     03  FILLER                  PIC 9(4)    VALUE 3999.                  
010500     03  FILLER                  PIC 9(2)    VALUE 74.                    
010600                                                                          
010700 01  TAB-1 REDEFINES TABELL-1.                                            
010800     03  TAB-RAD OCCURS 17.                                               
010900         05  TAB-KDPRODSL       PIC 9(3).                                 
011000         05  TAB-IDFKNGRP-MIN   PIC 9(4).                                 
011100         05  TAB-IDFKNGRP-MAX   PIC 9(4).                                 
011200         05  TAB-KDPSLLOC       PIC 9(2).                                 
011300     EJECT                                                                
011400                                                                          
011500 LINKAGE SECTION.                                                         
011600*                                                                         
011700*   -COPY W100LPC                                                         
011800*                                                                         
011900     EJECT                                                                
012000                                                                          
012100 PROCEDURE DIVISION  USING LPC-W100LPC.                                   
012200                                                                          
012300     PERFORM A-BEARBETA                                                   
012400                                                                          
012500     GOBACK                                                               
012600     .                                                                    
012700     EJECT                                                                
012800                                                                          
012900 A-BEARBETA SECTION.                                                      
013000     MOVE NEJ                        TO SW-TRAEFF                         
013100     MOVE +1                         TO TAB-IX                            
013200                                                                          
013300     PERFORM UNTIL TAB-IX    >  TAB-IX-MAX OR                             
013400                   SW-TRAEFF  = JA                                        
013500       IF LPC-KDPRODSL-IN     = TAB-KDPRODSL(TAB-IX)                      
013600         IF LPC-IDFKNGRP-IN  >= TAB-IDFKNGRP-MIN(TAB-IX) AND              
013700            LPC-IDFKNGRP-IN  <= TAB-IDFKNGRP-MAX(TAB-IX)                  
013800           MOVE TAB-KDPSLLOC(TAB-IX) TO LPC-KDPSLLOC-UT                   
013900           MOVE JA                   TO SW-TRAEFF                         
014000         END-IF                                                           
014100       END-IF                                                             
014200       ADD +1                        TO TAB-IX                            
014300     END-PERFORM                                                          
014400                                                                          
014500     IF SW-TRAEFF = NEJ                                                   
014510       MOVE LPC-KDPRODSL-IN          TO TEST-KDPRODSL                     
014600       EVALUATE TRUE                                                      
014700         WHEN LPC-KDPRODSL-IN = 11                                        
014800              MOVE 11                TO LPC-KDPSLLOC-UT                   
014900         WHEN LPC-KDPRODSL-IN = 13                                        
015000              MOVE 21                TO LPC-KDPSLLOC-UT                   
015100         WHEN LPC-KDPRODSL-IN = 14                                        
015200              MOVE 13                TO LPC-KDPSLLOC-UT                   
015300         WHEN LPC-KDPRODSL-IN = 15                                        
015400              MOVE 71                TO LPC-KDPSLLOC-UT                   
015500         WHEN LPC-KDPRODSL-IN = 16                                        
015600              MOVE 73                TO LPC-KDPSLLOC-UT                   
015700         WHEN LPC-KDPRODSL-IN = 17                                        
015800              MOVE 17                TO LPC-KDPSLLOC-UT                   
015900         WHEN LPC-KDPRODSL-IN = 18                                        
016000              MOVE 81                TO LPC-KDPSLLOC-UT                   
016100         WHEN LPC-KDPRODSL-IN = 19                                        
016200              MOVE 11                TO LPC-KDPSLLOC-UT                   
016300         WHEN LPC-KDPRODSL-IN = 91                                        
016400              MOVE 12                TO LPC-KDPSLLOC-UT                   
016500         WHEN LPC-KDPRODSL-IN = 93                                        
016600              MOVE 22                TO LPC-KDPSLLOC-UT                   
016700         WHEN LPC-KDPRODSL-IN = 94                                        
016800              MOVE 14                TO LPC-KDPSLLOC-UT                   
016900         WHEN LPC-KDPRODSL-IN = 95                                        
017000              MOVE 72                TO LPC-KDPSLLOC-UT                   
017100         WHEN LPC-KDPRODSL-IN = 96                                        
017200              MOVE 74                TO LPC-KDPSLLOC-UT                   
017300         WHEN LPC-KDPRODSL-IN = 97                                        
017400              MOVE 97                TO LPC-KDPSLLOC-UT                   
017500         WHEN LPC-KDPRODSL-IN = 98                                        
017600              MOVE 82                TO LPC-KDPSLLOC-UT                   
017700         WHEN LPC-KDPRODSL-IN = 99                                        
017800              MOVE 11                TO LPC-KDPSLLOC-UT                   
017900         WHEN KDPRODSL-VCBV                                               
018100              MOVE 11                TO LPC-KDPSLLOC-UT                   
018200         WHEN KDPRODSL-POLESTAR                                           
018400              MOVE LPC-KDPRODSL-IN   TO LPC-KDPSLLOC-UT                   
018410         WHEN KDPRODSL-BIMA                                               
018420              MOVE 11                TO LPC-KDPSLLOC-UT                   
018500         WHEN KDPRODSL-LYNK                                               
018501              MOVE LPC-KDPRODSL-IN   TO LPC-KDPSLLOC-UT                   
018510         WHEN OTHER                                                       
018600              MOVE 11                TO LPC-KDPSLLOC-UT                   
018700       END-EVALUATE                                                       
018800     END-IF                                                               
018900                                                                          
019000     MOVE LPC-IDARTNR-IN             TO BYT03-IDARTNR                     
019100     IF BYT03-OBJEKT                                                      
019200        MOVE 98                      TO LPC-KDPSLLOC-UT                   
019300     END-IF                                                               
019400     .                                                                    
