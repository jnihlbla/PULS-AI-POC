000100 01  W474001.                                                             
000200*                                 DISTRIBUTION 80   LASTNING              
000300*                                 MELLANPOST FÖR UTSKR AV                 
000400*                                 STAT.LISTA UNDER VECKAN UTFÖRDA         
000500*                                 LASTNINGAR                              
000600*                                                                         
000700     03 IDPTYP               PIC X(3).                                    
000800*                                 POSTTYP                                 
000900     03 IDDC                 PIC X(2).                                    
001000*                                 IDENTIFIERARE LAGER                     
001100     03 IDDISTR              PIC S9(5)           COMP-3.                  
001200*                                 DISTRIKTNUMMER                          
001300     03 KDSORT1              PIC X.                                       
001400*                                 SORTERINGSKOD                           
001500     03 KDFRAKT              PIC S9(3)           COMP-3.                  
001600*                                 FRAKTSÄTT C1-C2 TILL KUND               
001700     03 IDLASTN              PIC S9(7)           COMP-3.                  
001800*                                 LASTNINGSNUMMER                         
001900     03 KVLASTN              PIC S9(3)           COMP-3.                  
002000*                                 ANTAL LASTNINGAR                        
002100     03 KVKOLLI              PIC S9(5)           COMP-3.                  
002200*                                 ANTAL KOLLI                             
002300     03 VKORDBTO             PIC S9(6)V9(1)      COMP-3.                  
002400*                                 ORDERVIKT BRUTTO (KG)                   
002500     03 VLORDBTO             PIC S9(4)V9(3)      COMP-3.                  
002600*                                 ORDERVOLYM BRUTTO (M3)                  
002700*** END COPY W474001     LENGTH=28                                        
