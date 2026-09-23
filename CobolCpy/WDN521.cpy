000100 01  ART-WDN521.                                                          
000200*                                 KATALOG AVSNITTSREGISTER                
000300*                                 RADINFORMATION                          
000400*                                 DETTA SEGMENT SKALL FINNAS OM           
000500*                                 DET FINNS NÅGOT ÖVRIGT SEGM.            
000600*                                 SÖKBEGREPP: KDSEGKEY                    
000700     03 ART-KDSEGKEY         PIC X.                                       
000800*                                 TEKNISK SEGMENT-NYCKEL                  
000900*                                 TECHNICAL SEGMENT KEY                   
001000     03 ART-KDFBX            PIC X.                                       
001100*                                 FBX-KOD                                 
001200*                                 FBX CODE VALUES:                        
001300*                                 H = MAIN LINE                           
001400*                                 F = CONTINUED LINE (NO BREAK)           
001500*                                 B = PAGE BREAK AFTER THIS LINE          
001600*                                 X = NO PAGE BREAK HERE                  
001700*                                     (MUST BE PRECEDED BY "H")           
001800*                                 SPACE = NORMAL BREAK RULES              
001900     03 ART-IDCATPOS         PIC X(3).                                    
002000*                                 POSITIONSNUMMER                         
002100*                                 FIGURE NUMBER                           
002200     03 ART-IDARTNR          PIC S9(9)           COMP-3.                  
002300*                                 ARTIKELNUMMER                           
002400*                                 PART NUMBER                             
002500     03 ART-KVKOL-GRP.                                                    
002600        05 ART-KVKOL         OCCURS 5 TIMES                               
002700                             PIC X(3).                                    
002800*                                 ANTAL AV ARTIKEL I RESP KOLUMN          
002900*                                 QTY. OF PARTS IN RESP. COLUMN           
003000     03 ART-KDPS             PIC X(2).                                    
003100*                                 ARTIKELSTATUS                           
003200*                                 PART STATUS CODE                        
003300*                                 SPACE = NORMAL STORED PART              
003400*                                 LS= LOCALLY STORED (NOT STORED          
003500*                                 BY C1/C2)                               
003600*                                 NS= NEVER STORED BY VOLVO               
003700*                                 OP= OBSOLETED PART                      
003800*                                 SP= SUPERSEDED PART                     
003900*                                 IK= INCLUDED IN KIT                     
004000*                                 EU= EXCHANGE UNIT                       
004100*                                 KS= PART NOT YET STORED                 
004200*                                 XX= PART DESCR. NOT TRANSLATABL         
004300*                                 E                                       
004400*                                 KN= "IK" AND "NS" TOGETHER              
004500*                                 KL= "IK" AND "LS" TOGETHER              
004600     03 ART-KVPUNKT          PIC S9              COMP-3.                  
004700*                                 ANTAL INDRAGNINGSPUNKTER                
004800*                                 QTY. INDENTING DOTS                     
004900*                                 0 = COMPLETE PART                       
005000*                                 1 = 1ST LEVEL OF INCL. PART             
005100*                                 2 = 2ND LEVEL OF INCL. PART             
005200*                                 3 = 3RD LEVEL OF INCL. PART             
005300*                                 4 = 4TH LEVEL OF INCL. PART             
005400     03 ART-IDTTEXNR         PIC S9(5)           COMP-3.                  
005500*                                 TILLÄGGSTEXT-NR                         
005600*                                 ADDITIONAL TEXT, ID NUMBER              
005700*** END OF VILMAII-COPY LENGTH= 31 BYTES                                  
