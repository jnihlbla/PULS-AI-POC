000100 01  402-W402TACD.                                                        
000200*                                 LAYOUT FÖR ORDERBEKRÄFTELSE             
000300*                                 TILL TACDIS                             
000400*                                 ANVÄNDS I W40293 SOM SKICKAR            
000500*                                 ORDERBEKR. TILL TACDIS VIA              
000600*                                 D & P                                   
000700     03 402-IDPTYP           PIC X(3).                                    
000800*                                 POSTTYP                                 
000900     03 402-IDVTYP-TACDIS    PIC 9(2).                                    
001000*                                 POSTTYPSVERSION                         
001100     03 402-DAREGDAT         PIC 9(8).                                    
001200*                                 REGISTRERINGSDATUM (ÅÅÅÅMMDD)           
001300     03 402-IDDISTR          PIC 9(4).                                    
001400*                                 DISTRIKTNUMMER                          
001500     03 402-IDKUNDNR         PIC 9(6).                                    
001600*                                 KUNDNUMMER                              
001700     03 402-IDORDNR7         PIC 9(7).                                    
001800*                                 ORDERNUMMER                             
001900     03 402-IDARTBET         PIC X(17).                                   
002000*                                 ARTIKELBETECKNING EFTERMARKNAD          
002100     03 402-KDORDBEK         PIC 9(2).                                    
002200*                                 ORDERBEKRÄFTELSEKOD                     
002300     03 402-KVBEART          PIC 9(6).                                    
002400*                                 BESTÄLLT ANTAL STYCKEN                  
002500     03 402-IDSEKVNR         PIC 9(2).                                    
002600*                                 GENERELLT SEKVENSNUMMER                 
002700     03 402-IDARTBET-TILLK   PIC X(17).                                   
002800*                                 TILLK ARTIKELBET EFTERMARKNAD           
002900     03 402-KVLEVART         PIC 9(6).                                    
003000*                                 LEVERERAT ANTAL STYCK                   
003100     03 402-IDDC             PIC X(2).                                    
003200*                                 IDENTIFIERARE LAGER                     
003300     03 402-DADLEVDAT        PIC 9(8).                                    
003400*                                 OMRÄKNAT DATUM FÖR DDGS-ORDER           
003500*** END OF VILMAII-COPY LENGTH= 90 BYTES                                  
