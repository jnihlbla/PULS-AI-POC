000100 01  MID-W9I34301.                                                        
000200*                                 MIDCOPYTEXT TILL W9034300.              
000300*                                 ORDER CONFIRMATION INQUIRY.             
000400*                                 FRÅGA ORDERBEKRÄFTELSE.                 
000500     03 MID-IDDISTR          PIC X(4).                                    
000600*                                 DISTRIKTNUMMER                          
000700     03 MID-IDKUNDNR         PIC X(6).                                    
000800*                                 KUNDNUMMER                              
000900     03 MID-IDORDNR7         PIC X(7).                                    
001000*                                 ORDERNUMMER                             
001100     03 MID-IDARTNR          PIC X(9).                                    
001200*                                 ARTIKELNUMMER                           
001300     03 MID-KDORDKL          PIC X.                                       
001400*                                 ORDERKLASS                              
001500     03 MID-TIORDREG         PIC X(6).                                    
001600*                                 ORDERREGISTRERINGSDATUM  ÅÅMMDD         
001700     03 MID-IDVTYP           PIC X.                                       
001800*                                 POSTTYPSVERSION                         
001900     03 MID-TIORDREG-NEXT    PIC 9(6).                                    
002000*                                 ORDERREGISTRERINGSDATUM  ÅÅMMDD         
002100     03 MID-KDFRAKT-NEXT     PIC 9(2).                                    
002200*                                 FRAKTSÄTT DC TILL KUND                  
002300     03 MID-KDORDKL-NEXT     PIC 9.                                       
002400*                                 ORDERKLASS                              
002500     03 MID-IDORDNR7-NEXT    PIC 9(7).                                    
002600*                                 ORDERNUMMER                             
002700     03 MID-IDARTNR-NEXT     PIC 9(9).                                    
002800*                                 ARTIKELNUMMER                           
002900     03 MID-IDLOPNR-NEXT     PIC 9(3).                                    
003000*                                 LÖPNUMMER                               
003100     03 MID-IDSEKVNR-NEXT    PIC 9(3).                                    
003200*                                 GENERELLT SEKVENSNUMMER                 
003300     03 MID-IDDC-NEXT        PIC X(2).                                    
003400*                                 IDENTIFIERARE LAGER                     
003500     03 MID-IDORDER-NEXT     PIC 9(7).                                    
003600*                                 VOLVO PARTS ORDERNUMMER                 
003700     03 MID-KDORDBEK-NEXT    PIC 9(2).                                    
003800*                                 ORDERBEKRÄFTELSEKOD                     
003900*** END OF VILMAII-COPY LENGTH= 76 BYTES                                  
