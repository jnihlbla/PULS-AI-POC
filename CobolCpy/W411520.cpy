000100 01  W411520.                                                             
000200*                                 TYP = 520,LÄNGD = 80                    
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
001600     03 ADKOPARE             PIC X(54).                                   
001700*                                 KÖPARADRESS                             
001800     03 ADGODSMK REDEFINES ADKOPARE                                       
001900                             PIC X(54).                                   
002000*                                 GODSMOTTAGARADRESS                      
002100     03 FILLER REDEFINES ADKOPARE.                                        
002200        05 FILLER            PIC X(19).                                   
002300        05 IDARTNR           PIC 9(8).                                    
002400*                                 ARTIKELNUMMER                           
002500        05 FILLER            PIC X(18).                                   
002600        05 KVORDRAD          PIC 9(3).                                    
002700*                                 ANTAL ORDERRRADER  KVORDRAD-002         
002800        05 KVANTART          PIC 9(6).                                    
002900*                                 ANTAL ARTIKLAR                          
003000     03 FILLER               PIC X(5).                                    
003100*** END COPY W411520CC0  LENGTH=80                                        
