000100 01  W411510.                                                             
000200*                                 TYP = 510, LÄNGD = 80                   
000300*                                                                         
000400     03 IDTYP                PIC X(3).                                    
000500*                                 POSTTYP                                 
000600     03 IDDISTR              PIC 9(4).                                    
000700*                                 DISTRIKTNUMMER                          
000800     03 IDKUNDNR             PIC 9(6).                                    
000900*                                 KUNDNUMMER                              
001000     03 KDCLAGER             PIC 9.                                       
001100*                                 CENTRALLAGERKOD                         
001200     03 KDFRAKT              PIC 9(2).                                    
001300*                                 FRAKTSÄTT C1-C2 TILL KUND               
001400     03 IDORDNR              PIC 9(5).                                    
001500*                                 ORDERNUMMER                             
001600     03 BEKOPARE             PIC X(54).                                   
001700*                                 KÖPARNAMN                               
001800     03 BEGODSM REDEFINES BEKOPARE                                        
001900                             PIC X(54).                                   
002000*                                 GODSMOTTAGARNAMN                        
002100     03 FILLER               PIC X(5).                                    
002200*** END COPY W411510CC0  LENGTH=80                                        
