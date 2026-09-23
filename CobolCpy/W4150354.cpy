000100 01  W4150354.                                                            
000200*                                                                         
000300*                                 TYP = 654                               
000400*                                                                         
000500     03 POST-IDENTITET.                                                   
000600        05 W092P001.                                                      
000700*                                 SORTERINGS-BEGREPP GEMENSAMT            
000800*                                 FÖR ALLA TRANSAR UR TRATTEN             
000900           07 IDPTYP-S       PIC X(3).                                    
001000*                                 POSTTYP                                 
001100           07 IDORDER-S.                                                  
001200              09 IDDISTR-S   PIC S9(4).                                   
001300*                                 DISTRIKTNUMMER                          
001400              09 IDKUNDNR-S  PIC S9(6).                                   
001500*                                 KUNDNUMMER                              
001600              09 KDCLAGER-S  PIC S9.                                      
001700*                                 CENTRALLAGERKOD                         
001800              09 KDFRAKT-S   PIC 9(2).                                    
001900*                                 FRAKTSÄTT C1-C2 TILL KUND               
002000              09 KDFRAKT-X REDEFINES KDFRAKT-S                            
002100                             PIC X(2).                                    
002200*                                 FRAKTSÄTT C1-C2 TILL KUND               
002300              09 IDORDNR-S   PIC S9(5).                                   
002400*                                 ORDERNUMMER                             
002500              09 IDORDNR-X REDEFINES IDORDNR-S                            
002600                             PIC X(5).                                    
002700*                                 ORDERNUMMER                             
002800           07 PDATA.                                                      
002900              09 KDORDKL     PIC S9.                                      
003000*                                 ORDERKLASS                              
003100              09 KDSORT1 REDEFINES KDORDKL                                
003200                             PIC S9.                                      
003300*                                 SORTKOD-ENSTÄLLIG                       
003400              09 SORTBGP     PIC S9(8).                                   
003500*                                 DIVERSE INNEHÅLLSORT-BEGREPP            
003600              09 KDFELMRK    PIC S9.                                      
003700*                                 FELMARKERINGSKOD                        
003800              09 IDFELKOD    PIC 9(3).                                    
003900*                                 FELKOD                                  
004000              09 IDFELKODX REDEFINES IDFELKOD                             
004100                             PIC X(3).                                    
004200*                                 FELKOD                                  
004300              09 FILLER2     PIC X(2).                                    
004400     03 POST-BILD.                                                        
004500        05 IDPTYP            PIC X(3).                                    
004600*                                 POSTTYP                                 
004700        05 IDDISTR           PIC 9(4).                                    
004800*                                 DISTRIKTNUMMER                          
004900        05 IDKUNDNR          PIC 9(6).                                    
005000*                                 KUNDNUMMER                              
005100        05 KDCLAGER          PIC 9.                                       
005200*                                 CENTRALLAGERKOD                         
005300        05 KDFRAKT           PIC 9(2).                                    
005400*                                 FRAKTSÄTT C1-C2 TILL KUND               
005500        05 IDORDNR           PIC 9(5).                                    
005600*                                 ORDERNUMMER                             
005700        05 IDKOLLI           PIC 9(6).                                    
005800*                                 KOLLINUMMER         IDKOLLI-002         
005900        05 IDORDNR-TOM       PIC 9(5).                                    
006000*                                 ORDERNUMMER                             
006100        05 IDKOLLI-TOM       PIC 9(6).                                    
006200*                                 KOLLINUMMER         IDKOLLI-002         
006300        05 IDLASTBN          PIC 9(2).                                    
006400*                                 LASTBÄRARE NUMMER                       
006500        05 KDLBTYP           PIC 9(2).                                    
006600*                                 LASTBÄRARTYP                            
006700        05 BELBBET           PIC X(12).                                   
006800*                                 LASTBÄRARBETECKNING                     
006900        05 IDLBSIGL          PIC X(9).                                    
007000*                                 LASTBÄRARE SIGILL                       
007100        05 FILLER            PIC X(6).                                    
007200        05 TIPACKAF          PIC 9(2).                                    
007300*                                 PACKN.-ÅR FAKTISKT                      
007400        05 TIPACKDF          PIC 9(3).                                    
007500*                                 PACKN.-DAG FAKTISK                      
007600        05 FILLER            PIC X(6).                                    
007700*** END COPY W4150354C0  LENGTH=116                                       
