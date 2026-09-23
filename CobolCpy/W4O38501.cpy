000100 01  MOD-W4O38501.                                                        
000200*                                 COPYTEXT FOR MOD W4O38501               
000300     03 MOD-IDTRANS          PIC X(4).                                    
000400*                                 BILDNUMMER                              
000500     03 MOD-TEMFSFEL         PIC X(40).                                   
000600*                                 MFS FELMEDDELANDE                       
000700     03 MOD-PFI-IDTRP.                                                    
000800*                                 TRANSPORTIDENTITET                      
000900        05 MOD-IDTRPLOS      PIC X(3).                                    
001000*                                 TRANSPORTLÖSNING                        
001100        05 MOD-IDTRPVAR      PIC X(2).                                    
001200*                                 TRANSPORTLÖSNINGSGRUPP                  
001300     03 MOD-PFI-IDDC         PIC X(2).                                    
001400*                                 IDENTIFIERARE LAGER                     
001500     03 MOD-PF7-IDTRP.                                                    
001600*                                 TRANSPORTIDENTITET                      
001700        05 MOD-IDTRPLOS      PIC X(3).                                    
001800*                                 TRANSPORTLÖSNING                        
001900        05 MOD-IDTRPVAR      PIC X(2).                                    
002000*                                 TRANSPORTLÖSNINGSGRUPP                  
002100     03 MOD-PF7-IDDC         PIC X(2).                                    
002200*                                 IDENTIFIERARE LAGER                     
002300     03 MOD-PFE-IDTRP.                                                    
002400*                                 TRANSPORTIDENTITET                      
002500        05 MOD-IDTRPLOS      PIC X(3).                                    
002600*                                 TRANSPORTLÖSNING                        
002700        05 MOD-IDTRPVAR      PIC X(2).                                    
002800*                                 TRANSPORTLÖSNINGSGRUPP                  
002900     03 MOD-PFE-IDORDER      PIC 9(7).                                    
003000*                                 VOLVO PARTS ORDERNUMMER                 
003100     03 MOD-PF8-IDTRP.                                                    
003200*                                 TRANSPORTIDENTITET                      
003300        05 MOD-IDTRPLOS      PIC X(3).                                    
003400*                                 TRANSPORTLÖSNING                        
003500        05 MOD-IDTRPVAR      PIC X(2).                                    
003600*                                 TRANSPORTLÖSNINGSGRUPP                  
003700     03 MOD-PF8-IDORDER      PIC 9(7).                                    
003800*                                 VOLVO PARTS ORDERNUMMER                 
003900     03 MOD-RESULT.                                                       
004000*                                 TABLE-LINES                             
004100        05 MOD-VLORDNTO-TOT-ATTR                                          
004200                             PIC X(2).                                    
004300*                                 MFS ATTRIBUTFÄLT                        
004400        05 MOD-VLORDNTO-TOT  PIC Z(3)9.9(3).                              
004500*                                 ORDERVOLYM NETTO (M3)                   
004600        05 MOD-VKORDNTO-TOT-ATTR                                          
004700                             PIC X(2).                                    
004800*                                 MFS ATTRIBUTFÄLT                        
004900        05 MOD-VKORDNTO-TOT  PIC Z(5)9.9.                                 
005000*                                 ORDERVIKT NETTO (KG)                    
005100     03 MOD-RAD              OCCURS 14 TIMES.                             
005200*                                 TABLE-LINES                             
005300        05 MOD-IDDISTR-RAD-ATTR                                           
005400                             PIC X(2).                                    
005500*                                 MFS ATTRIBUTFÄLT                        
005600        05 MOD-IDDISTR-RAD   PIC Z(3)9.                                   
005700*                                 DISTRIKTNUMMER                          
005800        05 MOD-IDKUNDNR-RAD-ATTR                                          
005900                             PIC X(2).                                    
006000*                                 MFS ATTRIBUTFÄLT                        
006100        05 MOD-IDKUNDNR-RAD  PIC Z(5)9.                                   
006200*                                 KUNDNUMMER                              
006300        05 MOD-IDORDER-RAD-ATTR                                           
006400                             PIC X(2).                                    
006500*                                 MFS ATTRIBUTFÄLT                        
006600        05 MOD-IDORDER-RAD   PIC Z(6)9.                                   
006700*                                 VOLVO PARTS ORDERNUMMER                 
006800        05 MOD-VLORDNTO-RAD-ATTR                                          
006900                             PIC X(2).                                    
007000*                                 MFS ATTRIBUTFÄLT                        
007100        05 MOD-VLORDNTO-RAD  PIC Z(3)9.9(3).                              
007200*                                 ORDERVOLYM NETTO (M3)                   
007300        05 MOD-VKORDNTO-RAD-ATTR                                          
007400                             PIC X(2).                                    
007500*                                 MFS ATTRIBUTFÄLT                        
007600        05 MOD-VKORDNTO-RAD  PIC Z(5)9.9.                                 
007700*                                 ORDERVIKT NETTO (KG)                    
007800        05 MOD-TIRFS-RAD-ATTR                                             
007900                             PIC X(2).                                    
008000*                                 MFS ATTRIBUTFÄLT                        
008100        05 MOD-TIRFS-RAD     PIC 9(6)B9(4).                               
008200*                                 KLART FÖR TRANSPORT ÅÅMMDDTTMM          
008300        05 MOD-KDFRAKT-RAD-ATTR                                           
008400                             PIC X(2).                                    
008500*                                 MFS ATTRIBUTFÄLT                        
008600        05 MOD-KDORDSTA-RAD  PIC X(2).                                    
008700     03 MOD-TEMFSINF         PIC X(55).                                   
008800*                                 INFORMATIONSMEDDELANDE                  
