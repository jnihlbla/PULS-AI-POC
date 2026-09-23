000100 01  W474003.                                                             
000200*                                 DISTRIBUTION 80   LASTNING              
000300*                                 STATISTIKPOST FÖR UTSKR AV              
000400*                                 STAT.LISTA UNDER VECKAN OCH             
000500*                                 ÅRET UTFÖRDA LASTNINGAR                 
000600*                                                                         
000700     03 IDDC                 PIC X(2).                                    
000800*                                 IDENTIFIERARE LAGER                     
000900     03 IDDISTR              PIC S9(5)           COMP-3.                  
001000*                                 DISTRIKTNUMMER                          
001100     03 KDSORT1              PIC X.                                       
001200*                                 SORTERINGSKOD                           
001300     03 KDFRAKT              PIC S9(3)           COMP-3.                  
001400*                                 FRAKTSÄTT C1-C2 TILL KUND               
001500     03 IDLASTN              PIC S9(7)           COMP-3.                  
001600*                                 LASTNINGSNUMMER                         
001700     03 KVLASTN-VKA          PIC S9(3)           COMP-3.                  
001800*                                 ANTAL LASTNINGAR                        
001900     03 KVKOLLI-VKA          PIC S9(5)           COMP-3.                  
002000*                                 ANTAL KOLLI                             
002100     03 VKORDBTO-VKA         PIC S9(6)V9(1)      COMP-3.                  
002200*                                 ORDERVIKT BRUTTO (KG)                   
002300     03 VLORDBTO-VKA         PIC S9(4)V9(3)      COMP-3.                  
002400*                                 ORDERVOLYM BRUTTO (M3)                  
002500     03 KVLASTN-AR           PIC S9(3)           COMP-3.                  
002600*                                 ANTAL LASTNINGAR                        
002700     03 KVKOLLI-AR           PIC S9(5)           COMP-3.                  
002800*                                 ANTAL KOLLI                             
002900     03 VKORDBTO-AR          PIC S9(6)V9(1)      COMP-3.                  
003000*                                 ORDERVIKT BRUTTO (KG)                   
003100     03 VLORDBTO-AR          PIC S9(4)V9(3)      COMP-3.                  
003200*                                 ORDERVOLYM BRUTTO (M3)                  
003300*** END COPY W474003     LENGTH=38                                        
