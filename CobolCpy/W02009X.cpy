000100 01  W02009X.                                                             
000200*                                 SRS - COPYTEXT TILL FIL W02009X         
000300*                                                                         
000400     03 IDDC                 PIC X(2).                                    
000500*                                 IDENTIFIERARE LAGER                     
000600     03 IDDISTR              PIC Z(3)9.                                   
000700*                                 DISTRIKTNUMMER                          
000800     03 IDKUNDNR             PIC Z(5)9.                                   
000900*                                 KUNDNUMMER                              
001000     03 IDARTNR              PIC Z(7)9.                                   
001100*                                 ARTIKELNUMMER                           
001200     03 KDORDKL              PIC 9.                                       
001300*                                 ORDERKLASS                              
001400     03 IDDC-PRIM            PIC X(2).                                    
001500*                                 PRIMÄRT LEVERERANDE LAGER               
001600     03 KDPRODSL             PIC Z9.                                      
001700*                                 PRODUKTSLAG                             
001800     03 REINKORD             PIC -9.                                      
001900*                                 INKOMMEN ORDERRAD                       
002000     03 REFYSAVV             PIC -9.9(2).                                 
002100*                                 FYSISK AVVIKELSE                        
002200     03 REAVBRAD             PIC -9.9(2).                                 
002300*                                 AVBOKAD MÄNGD (DEL AV RAD)              
002400     03 RELAGERB             PIC -9.                                      
002500*                                 INKOMMEN ORDERRAD                       
002600     03 RESORTB              PIC -9.                                      
002700*                                 INKOMMEN ORDERRAD                       
002800     03 KVBEART              PIC -(6)9.                                   
002900*                                 BESTÄLLT ANTAL STYCKEN                  
003000*** END OF VILMAII-COPY LENGTH= 48 BYTES                                  
