000100 01  RESP-WL0129O1.                                                       
000200*                                 RESPONS FROM PGM WL0129                 
000300     03 RESP-L129-IDDC-KEY   PIC X(2).                                    
000400*                                 IDENTIFIERARE LAGER                     
000500     03 RESP-L129-IDDISTR-KEY                                             
000600                             PIC Z(3)9.                                   
000700*                                 DISTRIKTNUMMER                          
000800     03 RESP-L129-IDKUNDNR-KEY                                            
000900                             PIC Z(5)9.                                   
001000*                                 KUNDNUMMER                              
001100     03 RESP-L129-IDORDNR-KEY                                             
001200                             PIC Z(4)9.                                   
001300*                                 ORDERNUMMER UTGÅR PD90                  
001400     03 RESP-L129-IDKOLLI-KEY                                             
001500                             PIC Z(4)9.                                   
001600*                                 KOLLINUMMER                             
001700     03 RESP-L129-IDKOLLI-TOM-KEY                                         
001800                             PIC Z(4)9.                                   
001900*                                 KOLLINUMMER                             
002000*** END OF VILMAII-COPY LENGTH= 27 BYTES                                  
