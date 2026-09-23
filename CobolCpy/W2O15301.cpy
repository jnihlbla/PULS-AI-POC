000100 01  MOD-W2O15301.                                                        
000200*                                 COPYTEXT FÖR MOD W2015300               
000300     03 MOD-IDTRANS          PIC X(4).                                    
000400*                                 BILDNUMMER                              
000500     03 MOD-TEMFSFEL         PIC X(40).                                   
000600*                                 MFS FELMEDDELANDE                       
000700     03 MOD-IDFKNGRP-IN      PIC X(4).                                    
000800*                                 FUNKTIONSGRUPP                          
000900     03 MOD-IDFKNGRP-UT      PIC X(4).                                    
001000*                                 FUNKTIONSGRUPP                          
001100     03 MOD-AREA             OCCURS 13 TIMES.                             
001200        05 MOD-KDCMD-UPD-ATTR                                             
001300                             PIC X(2).                                    
001400*                                 MFS ATTRIBUTFÄLT                        
001500        05 MOD-KDCMD-UPD     PIC X.                                       
001600*                                 RAD-UPPDATERINGSKOMMANDO                
001700*                                  BLANK  = INGENTING                     
001800*                                  D , B  = DELETE                        
001900*                                  R , Ä  = REPLACE                       
002000*                                  I,N,A  = INSERT                        
002100*                                  S , V  = SELECT                        
002200*                                  P , P  = PRINT                         
002300*                                  C , K  = COPY                          
002400        05 MOD-IDFKNGRP-FOM  PIC 9(4).                                    
002500*                                 FUNKTIONSGRUPP-FROM                     
002600        05 MOD-BETEXT        PIC X.                                       
002700        05 MOD-IDFKNGRP-TOM  PIC 9(4).                                    
002800*                                 FUNKTIONSGRUPP-TOM                      
002900        05 MOD-KVAARLF       PIC Z9.                                      
003000*                                 ANTAL ÅR LAGERFÖRING EFTER EOP          
003100     03 MOD-IDFKNGRP-FOM-UPD-ATTR                                         
003200                             PIC X(2).                                    
003300*                                 MFS ATTRIBUTFÄLT                        
003400     03 MOD-IDFKNGRP-FOM-UPD PIC 9(4).                                    
003500*                                 FUNKTIONSGRUPP-FROM                     
003600     03 MOD-IDFKNGRP-TOM-UPD-ATTR                                         
003700                             PIC X(2).                                    
003800*                                 MFS ATTRIBUTFÄLT                        
003900     03 MOD-IDFKNGRP-TOM-UPD PIC 9(4).                                    
004000*                                 FUNKTIONSGRUPP-TOM                      
004100     03 MOD-KVAARLF-UPD-ATTR PIC X(2).                                    
004200*                                 MFS ATTRIBUTFÄLT                        
004300     03 MOD-KVAARLF-UPD      PIC 9(2).                                    
004400*                                 ANTAL ÅR LAGERFÖRING EFTER EOP          
004500     03 MOD-TEMFSINF         PIC X(55).                                   
004600*                                 INFORMATIONSMEDDELANDE                  
004700*** END OF VILMAII-COPY LENGTH= 305 BYTES                                 
