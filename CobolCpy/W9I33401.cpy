000100 01  MID-W9I33401.                                                        
000200*                                 MIDCOPYTEXT TILL W9033400.              
000300*                                 BO/TPO INQUIRY.                         
000400*                                 FRÅGA RO/TPO.                           
000500     03 MID-IDDISTR          PIC X(4).                                    
000600*                                 DISTRIKTNUMMER                          
000700     03 MID-IDKUNDNR         PIC X(6).                                    
000800*                                 KUNDNUMMER                              
000900     03 MID-IDORDNR7         PIC X(7).                                    
001000*                                 ORDERNUMMER                             
001100     03 MID-IDARTNR          PIC X(9).                                    
001200*                                 ARTIKELNUMMER                           
001300     03 MID-KDTPOTYP         PIC X.                                       
001400*                                 TYP AV TIDPLANERAD ORDER                
001500     03 MID-KDSTARAD         PIC X.                                       
001600*                                 RADSTATUSKOD                            
001700     03 MID-IDVTYP           PIC X.                                       
001800*                                 POSTTYPSVERSION                         
001900     03 MID-IDORDNR7-NEXT    PIC 9(7).                                    
002000*                                 ORDERNUMMER                             
002100     03 MID-IDARTNR-NEXT     PIC 9(9).                                    
002200*                                 ARTIKELNUMMER                           
002300     03 MID-IDLOPNR-NEXT     PIC 9(3).                                    
002400*                                 LÖPNUMMER                               
002500*** END OF VILMAII-COPY LENGTH= 48 BYTES                                  
