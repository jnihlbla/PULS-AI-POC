000100 01  MID-W4I39501.                                                        
000200*                                 MID-COPYTEXT FÖR W40395                 
000300     03 MID-IDPRODNR-IN      PIC X(7).                                    
000400*                                 PRODUKTIONSNUMMER                       
000500     03 MID-IDPRODNR-UT      PIC X(7).                                    
000600*                                 PRODUKTIONSNUMMER                       
000700     03 MID-IDDISTR-UT       PIC X(4).                                    
000800*                                 DISTRIKTNUMMER                          
000900     03 MID-IDKUNDNR-UT      PIC X(6).                                    
001000*                                 KUNDNUMMER                              
001100     03 MID-KDFRAKT-UT       PIC X(2).                                    
001200*                                 FRAKTSÄTT DC TILL KUND                  
001300     03 MID-PRTVAL-ADRESSFL  PIC X(2).                                    
001400*                                 PRINTER-VAL KOD                         
001500     03 MID-IDORDNR-UT       PIC X(5).                                    
001600*                                 ORDERNUMMER UTGÅR PD90                  
001700     03 MID-KDORDKL-UT       PIC X.                                       
001800*                                 ORDERKLASS                              
001900     03 MID-IDDC-UT          PIC X(2).                                    
002000*                                 IDENTIFIERARE LAGER                     
002100     03 MID-FLMAN-RAPP       PIC X.                                       
002200*                                 ALLMÄN FLAGGA                           
002300     03 MID-FLSIDA1          PIC X.                                       
002400*                                 ALLMÄN FLAGGA                           
002500     03 MID-IDRADNR-SENAST   PIC X(4).                                    
002600*                                 RADNUMMER                               
002700     03 MID-SUM              PIC 9(6).                                    
002800*                                 LEVERERAT ANTAL STYCK                   
002900     03 MID-RAPP-RAD.                                                     
003000*                                 MID-COPYTEXT FÖR W40395                 
003100        05 MID-IDRADNR       PIC 9(4).                                    
003200*                                 RADNUMMER                               
003300        05 MID-KVLEVART      PIC 9(6).                                    
003400*                                 LEVERERAT ANTAL STYCK                   
003500        05 MID-KVLEVART-DELAT1                                            
003600                             PIC 9(6).                                    
003700*                                 LEVERERAT ANTAL STYCK                   
003800        05 MID-IDKOLLI-FOM   PIC 9(5).                                    
003900*                                 KOLLINUMMER                             
004000        05 MID-IDKOLLI-TOM   PIC 9(5).                                    
004100*                                 KOLLINUMMER                             
004200        05 MID-RADER.                                                     
004300*                                 MID-COPYTEXT FÖR W40395                 
004400           07 MID-RAD        OCCURS 13 TIMES.                             
004500*                                 MID-COPYTEXT FÖR W40395                 
004600              09 MID-KVLEVART-DELAT2                                      
004700                             PIC 9(6).                                    
004800*                                 LEVERERAT ANTAL STYCK                   
004900              09 MID-IDKOLLI PIC 9(5).                                    
005000*                                 KOLLINUMMER                             
005100*** END OF VILMAII-COPY LENGTH= 217 BYTES                                 
