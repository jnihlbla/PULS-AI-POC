000100 01  MID-W2I15301.                                                        
000200*                                 COPYTEXT FÖR MID W2I15301               
000300     03 MID-IDFKNGRP-IN      PIC X(4).                                    
000400*                                 FUNKTIONSGRUPP                          
000500     03 MID-LINE-AREA        OCCURS 13 TIMES.                             
000600        05 MID-KDCMD-UPD     PIC X.                                       
000700         88 MID-KDCMD-INGENTING                                           
000800                             VALUE ' '.                                   
000900         88 MID-KDCMD-DELETE VALUE 'D'                                    
001000                             'B'.                                         
001100         88 MID-KDCMD-REPLACE                                             
001200                             VALUE 'R'                                    
001300                             'Ä'.                                         
001400         88 MID-KDCMD-INSERT VALUE 'I'                                    
001500                             'N'                                          
001600                             'A'.                                         
001700         88 MID-KDCMD-SELECT VALUE 'S'                                    
001800                             'V'.                                         
001900         88 MID-KDCMD-PRINT  VALUE 'P'                                    
002000                             'P'.                                         
002100         88 MID-KDCMD-COPY   VALUE 'C'                                    
002200                             'K'.                                         
002300*                                 RAD-UPPDATERINGSKOMMANDO                
002400*                                  BLANK  = INGENTING                     
002500*                                  D , B  = DELETE                        
002600*                                  R , Ä  = REPLACE                       
002700*                                  I,N,A  = INSERT                        
002800*                                  S , V  = SELECT                        
002900*                                  P , P  = PRINT                         
003000*                                  C , K  = COPY                          
003100        05 MID-IDFKNGRP-FOM  PIC 9(4).                                    
003200*                                 FUNKTIONSGRUPP-FROM                     
003300        05 MID-IDFKNGRP-TOM  PIC 9(4).                                    
003400*                                 FUNKTIONSGRUPP-TOM                      
003500        05 MID-KVAARLF       PIC 9(2).                                    
003600*                                 ANTAL ÅR LAGERFÖRING EFTER EOP          
003700     03 MID-UPD-AREA.                                                     
003800        05 MID-IDFKNGRP-FOM-UPD                                           
003900                             PIC 9(4).                                    
004000*                                 FUNKTIONSGRUPP-FROM                     
004100        05 MID-IDFKNGRP-TOM-UPD                                           
004200                             PIC 9(4).                                    
004300*                                 FUNKTIONSGRUPP-TOM                      
004400        05 MID-KVAARLF-UPD   PIC 9(2).                                    
004500*                                 ANTAL ÅR LAGERFÖRING EFTER EOP          
004600*** END OF VILMAII-COPY LENGTH= 157 BYTES                                 
