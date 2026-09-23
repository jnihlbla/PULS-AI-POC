000100 01  MID-W6I10501.                                                        
000200*                                 COPYTEXT FÖR MID                        
000300*                                 W6I10501                                
000400     03 MID-ADINLOMR-IN      PIC X(4).                                    
000500*                                 INLEVERANSOMRÅDE                        
000600     03 MID-ADINLOMR-UT      PIC X(4).                                    
000700*                                 INLEVERANSOMRÅDE                        
000800     03 MID-IDDC-IN          PIC X(2).                                    
000900*                                 IDENTIFIERARE LAGER                     
001000     03 MID-IDDC-UT          PIC X(2).                                    
001100*                                 IDENTIFIERARE LAGER                     
001200     03 MID-IDARTNR-ENTER    PIC X(9).                                    
001300*                                 ARTIKELNUMMER                           
001400     03 MID-IDARTNR-NEXT     PIC X(9).                                    
001500*                                 ARTIKELNUMMER                           
001600     03 MID-IDFKNGRP-ENTER   PIC X(4).                                    
001700*                                 FUNKTIONSGRUPP                          
001800     03 MID-IDFKNGRP-NEXT    PIC X(4).                                    
001900*                                 FUNKTIONSGRUPP                          
002000     03 MID-IDLEVNR-ENTER    PIC X(5).                                    
002100*                                 LEVERANTÖRNUMMER                        
002200     03 MID-IDLEVNR-NEXT     PIC X(5).                                    
002300*                                 LEVERANTÖRNUMMER                        
002400     03 MID-BEFT-ENTER       PIC X(2).                                    
002500*                                 FÖRPACKNINGSTYP                         
002600     03 MID-BEFT-NEXT        PIC X(2).                                    
002700*                                 FÖRPACKNINGSTYP                         
002800     03 MID-RAD              OCCURS 12 TIMES.                             
002900*                                 LINES                                   
003000        05 MID-KDCMD-RAD     PIC X.                                       
003100*                                 RAD-UPPDATERINGSKOMMANDO                
003200*                                  BLANK  = INGENTING                     
003300*                                  D , B  = DELETE                        
003400*                                  R , Ä  = REPLACE                       
003500*                                  I , N  = INSERT                        
003600*                                  S , V  = SELECT                        
003700        05 MID-ADINLOMR-RAD  PIC X(4).                                    
003800*                                 INLEVERANSOMRÅDE                        
003900        05 MID-IDARTNR-FOM   PIC X(8).                                    
004000*                                 ARTIKELNUMMER FRÅN OCH MED              
004100        05 MID-IDARTNR-TOM   PIC X(8).                                    
004200*                                 ARTIKELNUMMER TILL OCH MED              
004300        05 MID-IDFKNGRP-FOM  PIC X(4).                                    
004400*                                 FUNKTIONSGRUPP-FROM                     
004500        05 MID-IDFKNGRP-TOM  PIC X(4).                                    
004600*                                 FUNKTIONSGRUPP-TOM                      
004700        05 MID-IDLEVNR       PIC X(5).                                    
004800*                                 LEVERANTÖRNUMMER                        
004900        05 MID-BEFT-FOM      PIC X(2).                                    
005000*                                 FÖRPACKNINGSTYP FR O M                  
005100        05 MID-BEFT-TOM      PIC X(2).                                    
005200*                                 FÖRPACKNINGSTYP T O M                   
005300     03 MID-INPUT.                                                        
005400*                                 LINES                                   
005500        05 MID-ADINLOMR-INPUT                                             
005600                             PIC X(4).                                    
005700*                                 INLEVERANSOMRÅDE                        
005800        05 MID-IDARTNR-FOM-INPUT                                          
005900                             PIC X(8).                                    
006000*                                 ARTIKELNUMMER FRÅN OCH MED              
006100        05 MID-IDARTNR-TOM-INPUT                                          
006200                             PIC X(8).                                    
006300*                                 ARTIKELNUMMER TILL OCH MED              
006400        05 MID-IDFKNGRP-FOM-INPUT                                         
006500                             PIC X(4).                                    
006600*                                 FUNKTIONSGRUPP-FROM                     
006700        05 MID-IDFKNGRP-TOM-INPUT                                         
006800                             PIC X(4).                                    
006900*                                 FUNKTIONSGRUPP-TOM                      
007000        05 MID-IDLEVNR-INPUT PIC X(5).                                    
007100*                                 LEVERANTÖRNUMMER                        
007200        05 MID-BEFT-FOM-INPUT                                             
007300                             PIC X(2).                                    
007400*                                 FÖRPACKNINGSTYP FR O M                  
007500        05 MID-BEFT-TOM-INPUT                                             
007600                             PIC X(2).                                    
007700*                                 FÖRPACKNINGSTYP T O M                   
007800*** END OF VILMAII-COPY LENGTH= 545 BYTES                                 
