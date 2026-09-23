000100 01  SEQA-W6H6A1.                                                         
000200*                                 KVALITET                                
000300*                                 SEKUNDƒRT INDEX TILL W6H601             
000400*                                 OMR≈DE-GRUPP                            
000500*                                 FYSISK NYCKEL: W6H6A1KY                 
000600*                                 (IDDC,     IDKVAOMR, IDKVAGRP           
000700*                                  DAREGDAT, KDKVASTA)                    
000800*                                 SECONDARY NYCKEL: W6H6ASEQ              
000900*                                 (IDDC,     IDKVAOMR, IDKVAGRP           
001000*                                  DAREGDAT, KDKVASTA)                    
001100     03 SEQA-IDDC            PIC X(2).                                    
001200*                                 IDENTIFIERARE LAGER                     
001300*                                 WAREHOUSE IDENTIFIER                    
001400     03 SEQA-IDKVAOMR        PIC X.                                       
001500*                                 KVALITET KONTROLLOMR≈DE                 
001600*                                 QUALITY CONTROL AREA                    
001700     03 SEQA-IDKVAGRP        PIC 9(3).                                    
001800*                                 KVALITET KONTROLLGRUPP                  
001900*                                 QUALITY CONTROL GROUP                   
002000     03 SEQA-DAREGDAT        PIC 9(8).                                    
002100*                                 REGISTRERINGSDATUM (≈≈≈≈MMDD)           
002200*                                 REGISTRATION DATE (YYYYMMDD)            
002300     03 SEQA-KDKVASTA        PIC X.                                       
002400*                                 KVALITET LAGERREVISION STATUS           
002500*                                 QUALITY WAREHOUSE AUDIT STATUS          
002600*** END OF VILMAII-COPY LENGTH= 15 BYTES                                  
