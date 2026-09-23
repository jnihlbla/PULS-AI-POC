000100 01  MOD-W4O12101.                                                        
000200*                                 MOD-COPYTEXT FÖR W4O121                 
000300*                                                                         
000400     03 MOD-IDTRANS          PIC X(4).                                    
000500*                                 BILDNUMMER                              
000600*                                 SCREEN NUMBER                           
000700     03 MOD-TEMFSFEL         PIC X(40).                                   
000800*                                 MFS FELMEDDELANDE                       
000900*                                 MFS ERROR MESSAGE                       
001000     03 MOD-IDKVAOMR-IN      PIC X(2).                                    
001100*                                 MFS BEHANDLING AV INPUTFÄLT             
001200*                                 MFS DISPOSITION OF INPUT FIELD          
001300     03 MOD-IDKVATRG-IN      PIC Z9.                                      
001400*                                 KVALITET KONTROLLTORG                   
001500*                                 QUALITY CONTROL AREA                    
001600     03 MOD-IDKVAGRP-IN      PIC Z(2)9.                                   
001700*                                 KVALITET KONTROLLGRUPP                  
001800*                                 QUALITY CONTROL GROUP                   
001900     03 MOD-IDDC-IN          PIC X(2).                                    
002000*                                 IDENTIFIERARE LAGER                     
002100*                                 WAREHOUSE IDENTIFIER                    
002200     03 MOD-IDKVAOMR-UT      PIC X.                                       
002300*                                 KVALITET KONTROLLOMRÅDE                 
002400*                                 QUALITY CONTROL AREA                    
002500     03 MOD-IDKVATRG-UT      PIC Z9.                                      
002600*                                 KVALITET KONTROLLTORG                   
002700*                                 QUALITY CONTROL AREA                    
002800     03 MOD-IDKVAGRP-UT      PIC Z(2)9.                                   
002900*                                 KVALITET KONTROLLGRUPP                  
003000*                                 QUALITY CONTROL GROUP                   
003100     03 MOD-IDDC-UT          PIC X(2).                                    
003200*                                 IDENTIFIERARE LAGER                     
003300*                                 WAREHOUSE IDENTIFIER                    
003400     03 MOD-IDKVAOMR-EN      PIC X.                                       
003500*                                 KVALITET KONTROLLOMRÅDE                 
003600*                                 QUALITY CONTROL AREA                    
003700     03 MOD-IDKVATRG-EN      PIC 9(2).                                    
003800*                                 KVALITET KONTROLLTORG                   
003900*                                 QUALITY CONTROL AREA                    
004000     03 MOD-IDKVAGRP-EN      PIC 9(3).                                    
004100*                                 KVALITET KONTROLLGRUPP                  
004200*                                 QUALITY CONTROL GROUP                   
004300     03 MOD-ADLAGOMR-FOM-EN  PIC 9(2).                                    
004400*                                 LAGEROMRÅDE FRÅN OCH MED                
004500*                                 AREA ADDRESS FROM                       
004600     03 MOD-ADGANG-FOM-EN    PIC 9(2).                                    
004700*                                 GÅNG FRÅN OCH MED                       
004800*                                 AISLE ADDRESS FROM                      
004900     03 MOD-ADPLATS-FOM-EN   PIC 9(5).                                    
005000*                                 LAGERPLATS FRÅN OCH MED                 
005100*                                 LOCATION ADDRESS FROM                   
005200     03 MOD-IDKVAOMR-NX      PIC X.                                       
005300*                                 KVALITET KONTROLLOMRÅDE                 
005400*                                 QUALITY CONTROL AREA                    
005500     03 MOD-IDKVATRG-NX      PIC 9(2).                                    
005600*                                 KVALITET KONTROLLTORG                   
005700*                                 QUALITY CONTROL AREA                    
005800     03 MOD-IDKVAGRP-NX      PIC 9(3).                                    
005900*                                 KVALITET KONTROLLGRUPP                  
006000*                                 QUALITY CONTROL GROUP                   
006100     03 MOD-ADLAGOMR-FOM-NX  PIC 9(2).                                    
006200*                                 LAGEROMRÅDE FRÅN OCH MED                
006300*                                 AREA ADDRESS FROM                       
006400     03 MOD-ADGANG-FOM-NX    PIC 9(2).                                    
006500*                                 GÅNG FRÅN OCH MED                       
006600*                                 AISLE ADDRESS FROM                      
006700     03 MOD-ADPLATS-FOM-NX   PIC 9(5).                                    
006800*                                 LAGERPLATS FRÅN OCH MED                 
006900*                                 LOCATION ADDRESS FROM                   
007000     03 MOD-RAD              OCCURS 10 TIMES                              
007100                             INDEXED MOD-IX-1.                            
007200*                                  TABELL-RADER                           
007300*                                  TABLE LINES                            
007400        05 MOD-IDKVAOMR-ATTR PIC X(2).                                    
007500*                                 MFS ATTRIBUTFÄLT                        
007600        05 MOD-IDKVAOMR      PIC X.                                       
007700*                                 KVALITET KONTROLLOMRÅDE                 
007800*                                 QUALITY CONTROL AREA                    
007900        05 MOD-IDKVATRG-ATTR PIC X(2).                                    
008000*                                 MFS ATTRIBUTFÄLT                        
008100        05 MOD-IDKVATRG      PIC Z9.                                      
008200*                                 KVALITET KONTROLLTORG                   
008300*                                 QUALITY CONTROL AREA                    
008400        05 MOD-IDKVAGRP-ATTR PIC X(2).                                    
008500*                                 MFS ATTRIBUTFÄLT                        
008600        05 MOD-IDKVAGRP      PIC Z(2)9.                                   
008700*                                 KVALITET KONTROLLGRUPP                  
008800*                                 QUALITY CONTROL GROUP                   
008900        05 MOD-ADLAGOMR-FOM-ATTR                                          
009000                             PIC X(2).                                    
009100*                                 MFS ATTRIBUTFÄLT                        
009200        05 MOD-ADLAGOMR-FOM  PIC Z9.                                      
009300*                                 LAGEROMRÅDE FRÅN OCH MED                
009400*                                 AREA ADDRESS FROM                       
009500        05 MOD-ADGANG-FOM-ATTR                                            
009600                             PIC X(2).                                    
009700*                                 MFS ATTRIBUTFÄLT                        
009800        05 MOD-ADGANG-FOM    PIC Z9.                                      
009900*                                 GÅNG FRÅN OCH MED                       
010000*                                 AISLE ADDRESS FROM                      
010100        05 MOD-ADPLATS-FOM-ATTR                                           
010200                             PIC X(2).                                    
010300*                                 MFS ATTRIBUTFÄLT                        
010400        05 MOD-ADPLATS-FOM   PIC Z(4)9.                                   
010500*                                 LAGERPLATS FRÅN OCH MED                 
010600*                                 LOCATION ADDRESS FROM                   
010700        05 MOD-ADLAGOMR-TOM-ATTR                                          
010800                             PIC X(2).                                    
010900*                                 MFS ATTRIBUTFÄLT                        
011000        05 MOD-ADLAGOMR-TOM  PIC Z9.                                      
011100*                                 LAGEROMRÅDE TILL OCH MED                
011200*                                 AREA ADDRESS TO                         
011300        05 MOD-ADGANG-TOM-ATTR                                            
011400                             PIC X(2).                                    
011500*                                 MFS ATTRIBUTFÄLT                        
011600        05 MOD-ADGANG-TOM    PIC Z9.                                      
011700*                                 GÅNG TILL OCH MED                       
011800*                                 AISLE ADDRESS TO                        
011900        05 MOD-ADPLATS-TOM-ATTR                                           
012000                             PIC X(2).                                    
012100*                                 MFS ATTRIBUTFÄLT                        
012200        05 MOD-ADPLATS-TOM   PIC Z(4)9.                                   
012300*                                 LAGERPLATS TILL OCH MED                 
012400*                                 LOCATION ADDRESS TO                     
012500     03 MOD-IDKVAOMR-ATTR-UPP                                             
012600                             PIC X(2).                                    
012700*                                 MFS ATTRIBUTFÄLT                        
012800     03 MOD-IDKVAOMR-UPP     PIC X.                                       
012900*                                 KVALITET KONTROLLOMRÅDE                 
013000*                                 QUALITY CONTROL AREA                    
013100     03 MOD-IDKVATRG-ATTR-UPP                                             
013200                             PIC X(2).                                    
013300*                                 MFS ATTRIBUTFÄLT                        
013400     03 MOD-IDKVATRG-UPP     PIC Z9.                                      
013500*                                 KVALITET KONTROLLTORG                   
013600*                                 QUALITY CONTROL AREA                    
013700     03 MOD-IDKVAGRP-ATTR-UPP                                             
013800                             PIC X(2).                                    
013900*                                 MFS ATTRIBUTFÄLT                        
014000     03 MOD-IDKVAGRP-UPP     PIC Z(2)9.                                   
014100*                                 KVALITET KONTROLLGRUPP                  
014200*                                 QUALITY CONTROL GROUP                   
014300     03 MOD-ADLAGOMR-FOM-ATTR-UPP                                         
014400                             PIC X(2).                                    
014500*                                 MFS ATTRIBUTFÄLT                        
014600     03 MOD-ADLAGOMR-FOM-UPP PIC Z9.                                      
014700*                                 LAGEROMRÅDE FRÅN OCH MED                
014800*                                 AREA ADDRESS FROM                       
014900     03 MOD-ADGANG-FOM-ATTR-UPP                                           
015000                             PIC X(2).                                    
015100*                                 MFS ATTRIBUTFÄLT                        
015200     03 MOD-ADGANG-FOM-UPP   PIC Z9.                                      
015300*                                 GÅNG FRÅN OCH MED                       
015400*                                 AISLE ADDRESS FROM                      
015500     03 MOD-ADPLATS-FOM-ATTR-UPP                                          
015600                             PIC X(2).                                    
015700*                                 MFS ATTRIBUTFÄLT                        
015800     03 MOD-ADPLATS-FOM-UPP  PIC Z(4)9.                                   
015900*                                 LAGERPLATS FRÅN OCH MED                 
016000*                                 LOCATION ADDRESS FROM                   
016100     03 MOD-ADLAGOMR-TOM-ATTR-UPP                                         
016200                             PIC X(2).                                    
016300*                                 MFS ATTRIBUTFÄLT                        
016400     03 MOD-ADLAGOMR-TOM-UPP PIC Z9.                                      
016500*                                 LAGEROMRÅDE FRÅN OCH MED                
016600*                                 AREA ADDRESS FROM                       
016700     03 MOD-ADGANG-TOM-ATTR-UPP                                           
016800                             PIC X(2).                                    
016900*                                 MFS ATTRIBUTFÄLT                        
017000     03 MOD-ADGANG-TOM-UPP   PIC Z9.                                      
017100*                                 GÅNG FRÅN OCH MED                       
017200*                                 AISLE ADDRESS FROM                      
017300     03 MOD-ADPLATS-TOM-ATTR-UPP                                          
017400                             PIC X(2).                                    
017500*                                 MFS ATTRIBUTFÄLT                        
017600     03 MOD-ADPLATS-TOM-UPP  PIC Z(4)9.                                   
017700*                                 LAGERPLATS FRÅN OCH MED                 
017800*                                 LOCATION ADDRESS FROM                   
017900     03 MOD-KDCMD-ATTR-UPP   PIC X(2).                                    
018000*                                 MFS ATTRIBUTFÄLT                        
018100     03 MOD-KDCMD-UPP        PIC X.                                       
018200*                                 RAD-UPPDATERINGSKOMMANDO                
018300*                                 LINE UPDATE COMMAND                     
018400     03 MOD-TEMFSINF         PIC X(55).                                   
018500*                                 INFORMATIONSMEDDELANDE                  
018600*                                 INFORMATION MESSAGE                     
018700*** END OF VILMAII-COPY LENGTH= 611 BYTES                                 
