000100 01  W474002.                                                             
000200*                                 DISTRIBUTION 80   LASTNING              
000300*                                 MELLANPOST FÖR UTSKR AV LISTA           
000400*                                 LASTNINGAR ÄLDRE ÄN 2 VECKOR            
000500*                                                                         
000600     03 IDPTYP               PIC X(3).                                    
000700*                                 POSTTYP                                 
000800     03 IDDC                 PIC X(2).                                    
000900*                                 IDENTIFIERARE LAGER                     
001000     03 KDPERSON             PIC S9(3)           COMP-3.                  
001100*                                 PERSONKOD                               
001200     03 IDDISTR              PIC S9(5)           COMP-3.                  
001300*                                 DISTRIKTNUMMER                          
001400     03 IDKUNDNR             PIC S9(7)           COMP-3.                  
001500*                                 KUNDNUMMER                              
001600     03 KDFRAKT              PIC S9(3)           COMP-3.                  
001700*                                 FRAKTSÄTT C1-C2 TILL KUND               
001800     03 IDSKEPPN             PIC S9(7)           COMP-3.                  
001900*                                 SKEPPNINGSNUMMER                        
002000     03 IDLASTN              PIC S9(7)           COMP-3.                  
002100*                                 LASTNINGSNUMMER                         
002200     03 KVKOLLI              PIC S9(5)           COMP-3.                  
002300*                                 ANTAL KOLLI                             
002400     03 VKORDBTO             PIC S9(6)V9(1)      COMP-3.                  
002500*                                 ORDERVIKT BRUTTO (KG)                   
002600     03 VLORDBTO             PIC S9(4)V9(3)      COMP-3.                  
002700*                                 ORDERVOLYM BRUTTO (M3)                  
002800*** END COPY W474002     LENGTH=35                                        
