000100 01  MID-W4I38801.                                                        
000200*                                 COPYTEXT FÖR MID                        
000300*                                 W4I38801                                
000400     03 MID-KDPRCGRP-IN      PIC X(5).                                    
000500*                                 PRODUKTIONSKANALSGRUPP                  
000600     03 MID-KDPRCGRP-UT      PIC X(5).                                    
000700*                                 PRODUKTIONSKANALSGRUPP                  
000800     03 MID-IDDC-IN          PIC X(2).                                    
000900*                                 IDENTIFIERARE LAGER                     
001000     03 MID-IDDC-UT          PIC X(2).                                    
001100*                                 IDENTIFIERARE LAGER                     
001200     03 MID-KDPRODKL         PIC X.                                       
001300*                                 PRODUKTIONSKLASS                        
001400     03 MID-IDPRC.                                                        
001500*                                 PRODUKTIONSKANAL                        
001600        05 MID-IDPRCBAS      PIC X(3).                                    
001700*                                 PRC-BAS                                 
001800        05 MID-IDPRCVAR      PIC X.                                       
001900*                                 PRC-VARIANT                             
002000     03 MID-IDLISTTYP-ENTER  PIC X(6).                                    
002100*                                 LISTTYP                                 
002200     03 MID-IDLISTA-ENTER    PIC X(3).                                    
002300*                                 LISTNUMMER                              
002400     03 MID-IDLISTTYP-NEXT   PIC X(6).                                    
002500*                                 LISTTYP                                 
002600     03 MID-IDLISTA-NEXT     PIC X(3).                                    
002700*                                 LISTNUMMER                              
002800     03 MID-INPUT.                                                        
002900*                                 001-GRP FÖR MID                         
003000*                                 W4I38801                                
003100*                                                                         
003200        05 MID-IDLISTTYP-IN  PIC X(6).                                    
003300*                                 LISTTYP                                 
003400        05 MID-IDLISTA-IN    PIC X(3).                                    
003500*                                 LISTNUMMER                              
003600        05 MID-FLBEST-IN     PIC X.                                       
003700*                                 LISTA BESTÄLLD                          
003800        05 MID-BELISTA-IN    PIC X(25).                                   
003900*                                 TYP AV LISTNING                         
004000        05 MID-FLBORT-IN     PIC X.                                       
004100*                                 BORTTAGNINGSFLAGGA                      
