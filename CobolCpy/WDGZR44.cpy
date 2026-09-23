000100 01  R44-WDGZR44.                                                         
000200*                                 ANVÄNDS FÖR ATT LOGGA TP-               
000300*                                 TRANSEN FÖR FAKTURARELEASE.             
000400*                                 TRANSAR SOM LIKNAR R44:AN               
000500*                                 BILDAS FÖR UTSKRIFT I                   
000600*                                 KONTROLLISTAN.                          
000700     03 R44-IDPTYP           PIC X(3).                                    
000800*                                 POSTTYP                                 
000900     03 R44-IDDISTR          PIC 9(4).                                    
001000*                                 DISTRIKTNUMMER                          
001100     03 R44-IDKUNDNR         PIC 9(6).                                    
001200*                                 KUNDNUMMER                              
001300     03 R44-IDDC             PIC X(2).                                    
001400*                                 IDENTIFIERARE LAGER                     
001500     03 R44-KDFRAKT          PIC 9(2).                                    
001600*                                 FRAKTSÄTT C1-C2 TILL KUND               
001700     03 R44-FILLER           OCCURS 4 TIMES.                              
001800*                                 NEDAN ANGES OBRUTNA INTERVALL           
001900*                                 FÖR ORDERNUMMER/KOLLI                   
002000        05 R44-IDPRODNR      PIC 9(6).                                    
002100*                                 PRODUKTIONSNUMMER                       
002200        05 R44-IDKOLLI-FOM   PIC 9(4).                                    
002300*                                 KOLLINUMMER         IDKOLLI-003         
002400        05 R44-IDKOLLI-TOM   PIC 9(4).                                    
002500*                                 KOLLINUMMER         IDKOLLI-003         
002600     03 R44-IDFAKT           PIC 9(7).                                    
002700*                                 FAKTURANUMMER                           
002800     03 R44-IDSKEPPN         PIC 9(7).                                    
002900*                                 SKEPPNINGSNUMMER                        
003000     03 R44-FILLER           PIC X(3).                                    
003100*** END OF VILMAII-COPY LENGTH= 90 BYTES                                  
