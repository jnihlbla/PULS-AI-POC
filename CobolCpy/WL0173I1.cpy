000100 01  REQU-WL0173I1.                                                       
000200*                                 REQUEST TO PGM WL0173                   
000300     03 REQU-IDDC-L173       PIC X(2).                                    
000400*                                 IDENTIFIERARE LAGER                     
000500     03 REQU-ART-PRINT-GRP.                                               
000600*                                 GRUPPNIVÅ ARTIKEL-PRINT                 
000700        05 REQU-ART-PRINT    OCCURS 10 TIMES.                             
000800*                                 ARTIKEL-PRINT GRUPP                     
000900           07 REQU-IDARTNR-L173                                           
001000                             PIC 9(8).                                    
001100*                                 ARTIKELNUMMER                           
001200           07 REQU-KDINVPRIO-L173                                         
001300                             PIC 9.                                       
001400*                                 INVENTERING PRIORITET                   
001500           07 REQU-KDINVKAT-L173                                          
001600                             PIC 9(2).                                    
001700*                                 INVENTERINGSKATEGORI                    
001800     03 REQU-SINGLE-ART REDEFINES REQU-ART-PRINT-GRP.                     
001900*                                 SINGLE-ART GRUPP                        
002000        05 REQU-IDARTNR-L173-A                                            
002100                             PIC 9(8).                                    
002200*                                 ARTIKELNUMMER                           
002300        05 REQU-KDINVPRIO-L173-A                                          
002400                             PIC 9.                                       
002500*                                 INVENTERING PRIORITET                   
002600        05 REQU-KDINVKAT-L173-A                                           
002700                             PIC 9(2).                                    
002800*                                 INVENTERINGSKATEGORI                    
002900        05 REQU-FILLER       PIC X(99).                                   
003000*** END OF VILMAII-COPY LENGTH= 112 BYTES                                 
