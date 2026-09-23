000100 01  W092W001.                                                            
000200     03 IDPTYP               PIC X(3).                                    
000300*                                 POSTTYP                                 
000400     03 IDDISTR              PIC S9(4).                                   
000500*                                 DISTRIKTNUMMER                          
000600     03 IDKUNDNR             PIC S9(6).                                   
000700*                                 KUNDNUMMER                              
000800     03 KDCLAGER             PIC S9.                                      
000900*                                 CENTRALLAGERKOD                         
001000     03 KDFRAKT              PIC S9(2).                                   
001100*                                 FRAKTSÄTT C1-C2 TILL KUND               
001200     03 KDFRAKT-X REDEFINES KDFRAKT                                       
001300                             PIC X(2).                                    
001400*                                 FRAKTSÄTT C1-C2 TILL KUND               
001500     03 IDORDNR              PIC S9(5).                                   
001600*                                 ORDERNUMMER                             
001700     03 IDORDNR-X REDEFINES IDORDNR                                       
001800                             PIC X(5).                                    
001900*                                 ORDERNUMMER                             
002000     03 KDORDKL              PIC S9.                                      
002100*                                 ORDERKLASS                              
002200     03 KDSORT1 REDEFINES KDORDKL                                         
002300                             PIC S9.                                      
002400*                                 SORTKOD-ENSTÄLLIG                       
002500     03 SORTBGP              PIC S9(8).                                   
002600*                                 DIVERSE INNEHÅLLSORT-BEGREPP            
002700     03 SORTBGP-X REDEFINES SORTBGP                                       
002800                             PIC X(8).                                    
002900*                                 DIVERSE INNEHÅLLSORT-BEGREPP            
003000     03 KDFELMRK             PIC S9.                                      
003100*                                 FELMARKERINGSKOD                        
003200     03 IDFELKODX            PIC X(3).                                    
003300*                                 FELKOD                                  
003400     03 FILLER2              PIC X(2).                                    
003500*** END COPY W092W001C0  LENGTH=36                                        
