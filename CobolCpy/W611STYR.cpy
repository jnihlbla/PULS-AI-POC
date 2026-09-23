000100 01  STYR-W611STYR.                                                       
000200*                                 LÄNKAREA TILL W611STYR -                
000300*                                 LÄS STYRTABELL                          
000400*                                 FYLL ALLTID I DC +                      
000500*                                 ANTINGEN :                              
000600*                                 IDARTNR                                 
000700*                                 IDFKNGRP                                
000800*                                 IDLEVNR OCH/ELLER                       
000900*                                 BEFT                                    
001000*                                 DE 3 FÖRSTA GER ALLTID ETT              
001100*                                 VÄRDE I FB OCH DEN SISTA I FP,          
001200*                                 MEN ENDAST OM DEN FINNS.                
001300     03 STYR-IDDC            PIC X(2).                                    
001400*                                 IDENTIFIERARE LAGER                     
001500*                                 WAREHOUSE IDENTIFIER                    
001600     03 STYR-INDATA.                                                      
001700*                                                                         
001800        05 STYR-IDARTNR      PIC S9(9)           COMP-3.                  
001900*                                 ARTIKELNUMMER                           
002000*                                 PART NUMBER                             
002100        05 STYR-IDFKNGRP     PIC S9(5)           COMP-3.                  
002200*                                 FUNKTIONSGRUPP                          
002300*                                 FUNCTION GROUP                          
002400        05 STYR-IDLEVNR      PIC X(5).                                    
002500*                                 LEVERANTÖRNUMMER                        
002600*                                 SUPPLIER NUMBER (VENDOR NUMBER)         
002700        05 STYR-BEFT         PIC S9(3)           COMP-3.                  
002800*                                 FÖRPACKNINGSTYP                         
002900*                                 PACKAGING TYPE                          
003000     03 STYR-UTDATA.                                                      
003100*                                                                         
003200        05 STYR-ADINLOMR-FB  PIC X(4).                                    
003300*                                 FÖRBEHANDLINGSOMRÅDE.                   
003400*                                 HANDLING AREA                           
003500        05 STYR-ADINLOMR-FP  PIC X(4).                                    
003600*                                 FÖRPACKNINGSOMRÅDE.                     
003700*                                 HANDLING AREA                           
003800        05 STYR-KDSVAR       PIC X.                                       
003900         88 STYR-KDSVAR-OK   VALUE ' '.                                   
004000         88 STYR-KDSVAR-FEL  VALUE 'F'.                                   
004100*                                                       KDSVAR-88         
004200*                                 SVARSKOD FRÅN SUBPROGRAM                
004300*                                                       KDSVAR-88         
004400*                                 RETURN CODE FROM SUBPROGRAM             
004500*** END OF VILMAII-COPY LENGTH= 26 BYTES                                  
