000100 01  W411530.                                                             
000200*                                 TYP = 530, LÄNGD = 80                   
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
001600     03 IDDIVORD             PIC 9(3).                                    
001700*                                 DIVERSEORDERNUMMER                      
001800     03 FILLER               PIC X(56).                                   
001900*** END COPY W411530CC0  LENGTH=80                                        
