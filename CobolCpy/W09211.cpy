000100 01  W09211.                                                              
000200     03 SORTARG.                                                          
000300        05 IDFTG             PIC 9(2).                                    
000400*                                 FÖRETAGSID EKONOM REDOVISNING           
000500        05 KDLISTAD          PIC X(2).                                    
000600*                                 LISTADRESS                              
000700        05 KDLISTSO          PIC 9(2).                                    
000800*                                 LISTSORTERING                           
000900     03 TRANS.                                                            
001000        05 IDPTYP            PIC X(3).                                    
001100*                                 POSTTYP                                 
001200        05 IDDISTR           PIC 9(4).                                    
001300*                                 DISTRIKTNUMMER                          
001400        05 IDKUNDNR          PIC 9(6).                                    
001500*                                 KUNDNUMMER                              
001600        05 KDCLAGER          PIC 9.                                       
001700*                                 CENTRALLAGERKOD                         
001800        05 KDFRAKT           PIC 9(2).                                    
001900*                                 FRAKTSÄTT C1-C2 TILL KUND               
002000        05 KDFRAKT-X REDEFINES KDFRAKT                                    
002100                             PIC X(2).                                    
002200*                                 FRAKTSÄTT C1-C2 TILL KUND               
002300        05 IDORDNR           PIC 9(5).                                    
002400*                                 ORDERNUMMER                             
002500        05 IDORDNR-X REDEFINES IDORDNR                                    
002600                             PIC X(5).                                    
002700*                                 ORDERNUMMER                             
002800        05 KDORDKL           PIC 9.                                       
002900*                                 ORDERKLASS                              
003000        05 KDSORT1 REDEFINES KDORDKL                                      
003100                             PIC 9.                                       
003200*                                 SORTERINGSKOD                           
003300        05 SORTBGP           PIC S9(8).                                   
003400*                                 DIVERSE INNEHÅLLSORT-BEGREPP            
003500        05 SORTBGP-X REDEFINES SORTBGP                                    
003600                             PIC X(8).                                    
003700*                                 DIVERSE INNEHÅLLSORT-BEGREPP            
003800        05 KDFELMRK          PIC 9.                                       
003900*                                 FELMARKERINGSKOD                        
004000        05 IDFELKODX         PIC X(3).                                    
004100*                                 FELKOD                                  
004200        05 FILLER            PIC X(2).                                    
004300        05 FELTEXT           PIC X(80).                                   
004400*** END COPY W09211      LENGTH=122                                       
