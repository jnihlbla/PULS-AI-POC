000100 01  GRP-W6H501.                                                          
000200*                                 KVALITET LAGERINDELNING                 
000300*                                 FYSISK NYCKEL: W6H501KY                 
000400*                                 (IDDC,     IDKVAOMR,                    
000500*                                  IDKVATRG, IDKVAGRP)                    
000600*                                  ADLAGOMR-FOM, ADGANG-FOM,              
000700*                                  ADPLATS-FOM)                           
000800     03 GRP-IDDC             PIC X(2).                                    
000900*                                 IDENTIFIERARE LAGER                     
001000*                                 WAREHOUSE IDENTIFIER                    
001100     03 GRP-IDKVAOMR         PIC X.                                       
001200*                                 KVALITET KONTROLLOMRÅDE                 
001300*                                 QUALITY CONTROL AREA                    
001400     03 GRP-IDKVATRG         PIC 9(2).                                    
001500*                                 KVALITET KONTROLLTORG                   
001600*                                 QUALITY CONTROL AREA                    
001700     03 GRP-IDKVAGRP         PIC 9(3).                                    
001800*                                 KVALITET KONTROLLGRUPP                  
001900*                                 QUALITY CONTROL GROUP                   
002000     03 GRP-ADLAGOMR-FOM     PIC S9(3)           COMP-3.                  
002100*                                 LAGEROMRÅDE FRÅN OCH MED                
002200*                                 AREA ADDRESS FROM                       
002300     03 GRP-ADGANG-FOM       PIC S9(3)           COMP-3.                  
002400*                                 GÅNG FRÅN OCH MED                       
002500*                                 AISLE ADDRESS FROM                      
002600     03 GRP-ADPLATS-FOM      PIC S9(5)           COMP-3.                  
002700*                                 LAGERPLATS FRÅN OCH MED                 
002800*                                 LOCATION ADDRESS FROM                   
002900     03 GRP-ADLAGOMR-TOM     PIC S9(3)           COMP-3.                  
003000*                                 LAGEROMRÅDE TILL OCH MED                
003100*                                 AREA ADDRESS TO                         
003200     03 GRP-ADGANG-TOM       PIC S9(3)           COMP-3.                  
003300*                                 GÅNG TILL OCH MED                       
003400*                                 AISLE ADDRESS TO                        
003500     03 GRP-ADPLATS-TOM      PIC S9(5)           COMP-3.                  
003600*                                 LAGERPLATS TILL OCH MED                 
003700*                                 LOCATION ADDRESS TO                     
003800*** END OF VILMAII-COPY LENGTH= 22 BYTES                                  
