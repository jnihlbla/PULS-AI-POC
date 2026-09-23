000100 01  W11177.                                                              
000200*                                 COPYTEXT TILL FIL W11177                
000300*                                 ERSÄTTNING                              
000400     03 IDPTYP               PIC X(3).                                    
000500*                                 POSTTYP                                 
000600     03 DADATUM              PIC 9(8).                                    
000700*                                 DATUM ENLIGT KDDATFORM                  
000800     03 TIHHMMSS             PIC 9(6).                                    
000900*                                 TIM - MIN - SEK   (HHMMSS)              
001000     03 PARTNER-ID           PIC X(9).                                    
001100     03 IDARTNR-ERS          PIC 9(9).                                    
001200*                                 ERSATT ARTIKELNUMMER                    
001300     03 REKSIFFR-ERS         PIC 9.                                       
001400*                                 KONTROLLSIFFRA                          
001500     03 DIERS-ERS            PIC 9(4)V9(3).                               
001600*                                 ERSATT ARTIKELANTAL                     
001700     03 TIERSDAT             PIC 9(5).                                    
001800*                                 ERSÄTTNINGSDATUM  (ÅÅVVD)               
001900     03 KDERS                PIC 9(3).                                    
002000*                                 ERSÄTTNINGSKOD                          
002100     03 IDKORTNR             PIC 9(3).                                    
002200*                                 KORTNUMMER                              
002300*                                 (RADLÖPNR FÖR ERSÄTTNINGSINFO)          
002400     03 FLTEXT               PIC X.                                       
002500*                                 FINNS TEXTINFORMATION ?                 
002600     03 TYP1.                                                             
002700        05 IDARTNR-TILLK     PIC 9(9).                                    
002800*                                 TILLKOMMANDE ARTIKELNUMMER              
002900        05 REKSIFFR-TILLK    PIC 9.                                       
003000*                                 TILLKOMMANDE KONTROLLSIFFRA             
003100        05 DIERS-TILLK       PIC 9(4)V9(3).                               
003200*                                 TILLKOMMANDE ARTIKELANTAL               
003300*** END OF VILMAII-COPY LENGTH= 72 BYTES                                  
