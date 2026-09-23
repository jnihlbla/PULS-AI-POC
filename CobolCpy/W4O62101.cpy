000100 01  MOD-W4O62101.                                                        
000200*                                 MODCOPYTEXT TILL W40621.                
000300     03 MOD-IDTRANS          PIC X(4).                                    
000400*                                 BILDNUMMER                              
000500*                                 SCREEN NUMBER                           
000600     03 MOD-TEMFSFEL         PIC X(40).                                   
000700*                                 MFS FELMEDDELANDE                       
000800*                                 MFS ERROR MESSAGE                       
000900     03 MOD-IDSHIPM-IN       PIC Z(7).                                    
001000*                                 SKEPPNINGSNUMMER                        
001100*                                 SHIPMENT NO                             
001200     03 MOD-IDSHIPM-UT       PIC Z(7).                                    
001300*                                 SKEPPNINGSNUMMER                        
001400*                                 SHIPMENT NO                             
001500     03 MOD-RADER            OCCURS 14 TIMES.                             
001600*                                                                         
001700        05 MOD-IDDISTR       PIC Z(3)9.                                   
001800*                                 DISTRIKTNUMMER                          
001900*                                 DISTRICT NUMBER                         
002000        05 MOD-IDKUNDNR      PIC Z(5)9.                                   
002100*                                 KUNDNUMMER                              
002200*                                 CUSTOMER NO                             
002300        05 MOD-IDORDNR5      PIC Z(4)9.                                   
002400*                                 ORDERNUMMER                             
002500*                                 ORDER NUMBER                            
002600        05 MOD-IDKOLLI       PIC Z(4)9.                                   
002700*                                 KOLLINUMMER                             
002800*                                 CASE NUMBER                             
002900     03 MOD-TEMFSINF         PIC X(55).                                   
003000*                                 INFORMATIONSMEDDELANDE                  
003100*                                 INFORMATION MESSAGE                     
003200*** END OF VILMAII-COPY LENGTH= 393 BYTES                                 
