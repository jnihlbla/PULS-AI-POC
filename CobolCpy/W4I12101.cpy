000100 01  MID-W4I12101.                                                        
000200*                                 COPYTEXT FÖR MID W4I121                 
000300*                                                                         
000400     03 MID-IDKVAOMR-IN      PIC X.                                       
000500*                                 KVALITET KONTROLLOMRÅDE                 
000600*                                 QUALITY CONTROL AREA                    
000700     03 MID-IDKVATRG-IN      PIC X(2).                                    
000800*                                 KVALITET KONTROLLTORG                   
000900*                                 QUALITY CONTROL AREA                    
001000     03 MID-IDKVAGRP-IN      PIC X(3).                                    
001100*                                 KVALITET KONTROLLGRUPP                  
001200*                                 QUALITY CONTROL GROUP                   
001300     03 MID-IDDC-IN          PIC X(2).                                    
001400*                                 IDENTIFIERARE LAGER                     
001500*                                 WAREHOUSE IDENTIFIER                    
001600     03 MID-IDKVAOMR-UT      PIC X.                                       
001700*                                 KVALITET KONTROLLOMRÅDE                 
001800*                                 QUALITY CONTROL AREA                    
001900     03 MID-IDKVATRG-UT      PIC X(2).                                    
002000*                                 KVALITET KONTROLLTORG                   
002100*                                 QUALITY CONTROL AREA                    
002200     03 MID-IDKVAGRP-UT      PIC X(3).                                    
002300*                                 KVALITET KONTROLLGRUPP                  
002400*                                 QUALITY CONTROL GROUP                   
002500     03 MID-IDDC-UT          PIC X(2).                                    
002600*                                 IDENTIFIERARE LAGER                     
002700*                                 WAREHOUSE IDENTIFIER                    
002800     03 MID-IDKVAOMR-EN      PIC X.                                       
002900*                                 KVALITET KONTROLLOMRÅDE                 
003000*                                 QUALITY CONTROL AREA                    
003100     03 MID-IDKVATRG-EN      PIC 9(2).                                    
003200*                                 KVALITET KONTROLLTORG                   
003300*                                 QUALITY CONTROL AREA                    
003400     03 MID-IDKVAGRP-EN      PIC 9(3).                                    
003500*                                 KVALITET KONTROLLGRUPP                  
003600*                                 QUALITY CONTROL GROUP                   
003700     03 MID-ADLAGOMR-FOM-EN  PIC 9(2).                                    
003800*                                 LAGEROMRÅDE                             
003900*                                 AREA                                    
004000     03 MID-ADGANG-FOM-EN    PIC 9(2).                                    
004100*                                 GÅNG                                    
004200*                                 AISLE                                   
004300     03 MID-ADPLATS-FOM-EN   PIC 9(5).                                    
004400*                                 LAGERPLATSNUMMER                        
004500*                                 LOCATION                                
004600     03 MID-IDKVAOMR-NX      PIC X.                                       
004700*                                 KVALITET KONTROLLOMRÅDE                 
004800*                                 QUALITY CONTROL AREA                    
004900     03 MID-IDKVATRG-NX      PIC 9(2).                                    
005000*                                 KVALITET KONTROLLTORG                   
005100*                                 QUALITY CONTROL AREA                    
005200     03 MID-IDKVAGRP-NX      PIC 9(3).                                    
005300*                                 KVALITET KONTROLLGRUPP                  
005400*                                 QUALITY CONTROL GROUP                   
005500     03 MID-ADLAGOMR-FOM-NX  PIC 9(2).                                    
005600*                                 LAGEROMRÅDE                             
005700*                                 AREA                                    
005800     03 MID-ADGANG-FOM-NX    PIC 9(2).                                    
005900*                                 GÅNG                                    
006000*                                 AISLE                                   
006100     03 MID-ADPLATS-FOM-NX   PIC 9(5).                                    
006200*                                 LAGERPLATSNUMMER                        
006300*                                 LOCATION                                
006400     03 MID-INPUT.                                                        
006500*                                 INDATA-FÄLT                             
006600*                                 INPUT FIELD                             
006700        05 MID-IDKVAOMR      PIC X.                                       
006800*                                 KVALITET KONTROLLOMRÅDE                 
006900*                                 QUALITY CONTROL AREA                    
007000        05 MID-IDKVATRG      PIC 9(2).                                    
007100*                                 KVALITET KONTROLLTORG                   
007200*                                 QUALITY CONTROL AREA                    
007300        05 MID-IDKVAGRP      PIC 9(3).                                    
007400*                                 KVALITET KONTROLLGRUPP                  
007500*                                 QUALITY CONTROL GROUP                   
007600        05 MID-ADLAGOMR-FOM  PIC 9(2).                                    
007700*                                 LAGEROMRÅDE FRÅN OCH MED                
007800*                                 AREA ADDRESS FROM                       
007900        05 MID-ADGANG-FOM    PIC 9(2).                                    
008000*                                 GÅNG FRÅN OCH MED                       
008100*                                 AISLE ADDRESS FROM                      
008200        05 MID-ADPLATS-FOM   PIC 9(5).                                    
008300*                                 LAGERPLATS FRÅN OCH MED                 
008400*                                 LOCATION ADDRESS FROM                   
008500        05 MID-ADLAGOMR-TOM  PIC 9(2).                                    
008600*                                 LAGEROMRÅDE TILL OCH MED                
008700*                                 AREA ADDRESS TO                         
008800        05 MID-ADGANG-TOM    PIC 9(2).                                    
008900*                                 GÅNG TILL OCH MED                       
009000*                                 AISLE ADDRESS TO                        
009100        05 MID-ADPLATS-TOM   PIC 9(5).                                    
009200*                                 LAGERPLATS TILL OCH MED                 
009300*                                 LOCATION ADDRESS TO                     
009400        05 MID-KDCMD         PIC X.                                       
009500         88 MID-KDCMD-INGENTING                                           
009600                             VALUE ' '.                                   
009700         88 MID-KDCMD-DELETE VALUE 'D'                                    
009800                             'B'.                                         
009900         88 MID-KDCMD-REPLACE                                             
010000                             VALUE 'R'                                    
010100                             'Ä'.                                         
010200         88 MID-KDCMD-INSERT VALUE 'I'                                    
010300                             'N'.                                         
010400*                                 RAD-UPPDATERINGSKOMMANDO                
010500*                                 LINE UPDATE COMMAND                     
010600*** END OF VILMAII-COPY LENGTH= 71 BYTES                                  
