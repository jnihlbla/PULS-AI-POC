000100 01  W4260401.                                                            
000200*                                 KVALITET KONTROLLISTA                   
000300*                                 QUALITY CONTROL LIST                    
000400*                                 .                                       
000500     03 IDDC                 PIC X(2).                                    
000600*                                 IDENTIFIERARE LAGER                     
000700*                                 WAREHOUSE IDENTIFIER                    
000800     03 IDKVAOMR             PIC X.                                       
000900*                                 KVALITET KONTROLLOMR≈DE                 
001000*                                 QUALITY CONTROL AREA                    
001100     03 BEKVAOMR             PIC X(10).                                   
001200*                                 KVALITET KONTROLLOMR≈DESNAMN            
001300*                                 QUALITY NAME OF CONTROL AREA            
001400     03 IDKVAGRP             PIC 9(3).                                    
001500*                                 KVALITET KONTROLLGRUPP                  
001600*                                 QUALITY CONTROL GROUP                   
001700     03 TIREGDAT             PIC S9(7)           COMP-3.                  
001800*                                 REGISTRERINGSDATUM (≈≈MMDD)             
001900*                                 REGISTRATION DATE (YYMMDD)              
002000     03 ADLAGOMR-FOM         PIC S9(3)           COMP-3.                  
002100*                                 LAGEROMR≈DE FR≈N OCH MED                
002200*                                 AREA ADDRESS FROM                       
002300     03 ADGANG-FOM           PIC S9(3)           COMP-3.                  
002400*                                 G≈NG FR≈N OCH MED                       
002500*                                 AISLE ADDRESS FROM                      
002600     03 ADPLATS-FOM          PIC S9(5)           COMP-3.                  
002700*                                 LAGERPLATS FR≈N OCH MED                 
002800*                                 LOCATION ADDRESS FROM                   
002900     03 ADLAGOMR-TOM         PIC S9(3)           COMP-3.                  
003000*                                 LAGEROMR≈DE TILL OCH MED                
003100*                                 AREA ADDRESS TO                         
003200     03 ADGANG-TOM           PIC S9(3)           COMP-3.                  
003300*                                 G≈NG TILL OCH MED                       
003400*                                 AISLE ADDRESS TO                        
003500     03 ADPLATS-TOM          PIC S9(5)           COMP-3.                  
003600*                                 LAGERPLATS TILL OCH MED                 
003700*                                 LOCATION ADDRESS TO                     
003800*** END OF VILMAII-COPY LENGTH= 34 BYTES                                  
