000100 01  W211FEL.                                                             
000200*                                 FELPOSTER OCH MEDDELANDEN               
000300     03 SORT-FLT.                                                         
000400        05 W092P001.                                                      
000500*                                 SORTERINGS-BEGREPP GEMENSAMT            
000600*                                 FÖR ALLA TRANSAR UR TRATTEN             
000700           07 IDPTYP-S       PIC X(3).                                    
000800*                                 POSTTYP                                 
000900           07 IDORDER-S.                                                  
001000              09 IDDISTR-S   PIC S9(4).                                   
001100*                                 DISTRIKTNUMMER                          
001200              09 IDKUNDNR-S  PIC S9(6).                                   
001300*                                 KUNDNUMMER                              
001400              09 KDCLAGER-S  PIC S9.                                      
001500*                                 CENTRALLAGERKOD                         
001600              09 KDFRAKT-S   PIC X(2).                                    
001700*                                 FRAKTSÄTT C1-C2 TILL KUND               
001800              09 KDFRAKT-X REDEFINES KDFRAKT-S                            
001900                             PIC X(2).                                    
002000*                                 FRAKTSÄTT C1-C2 TILL KUND               
002100              09 IDORDNR-S   PIC S9(5).                                   
002200*                                 ORDERNUMMER                             
002300              09 IDORDNR-X REDEFINES IDORDNR-S                            
002400                             PIC X(5).                                    
002500*                                 ORDERNUMMER                             
002600           07 PDATA.                                                      
002700              09 KDORDKL     PIC S9.                                      
002800*                                 ORDERKLASS                              
002900              09 KDSORT1 REDEFINES KDORDKL                                
003000                             PIC S9.                                      
003100*                                 SORTERINGSKOD                           
003200              09 SORTBGP     PIC S9(8).                                   
003300*                                 DIVERSE INNEHÅLLSORT-BEGREPP            
003400              09 KDFELMRK    PIC S9.                                      
003500*                                 FELMARKERINGSKOD                        
003600              09 IDFELKOD    PIC 9(3).                                    
003700*                                 FELKOD                                  
003800              09 IDFELKODX REDEFINES IDFELKOD                             
003900                             PIC X(3).                                    
004000*                                 FELKOD                                  
004100              09 FILLER2     PIC X(2).                                    
004200     03 FELMED               PIC X(80).                                   
004300*** END COPY W211FELCC0  LENGTH=116                                       
