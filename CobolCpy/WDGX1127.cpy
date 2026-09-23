000100 01  1127-WDGX1127.                                                       
000200*                                 BASLAGER                                
000300*                                 ORDERGENERERING                         
000400*                                 FYSISK NYCKEL: WDGXKEY                  
000500*                                 (IDHTYP + KDPRODSL                      
000600*                                  + IDPROJ + KDBASLM                     
000700*                                  + LOWVALUE)                            
000800     03 1127-IDHTYP          PIC X(4).                                    
000900*                                 HÄNDELSETYP                             
001000     03 1127-KDPRODSL        PIC S9(3)           COMP-3.                  
001100*                                 PRODUKTSLAG                             
001200*                                 TYPE OF ASSORTMENT                      
001300     03 1127-IDPROJ          PIC X(4).                                    
001400*                                 PARTS PROJEKTIDENTITET                  
001500*                                 PARTS PROJECT IDENTITY                  
001600     03 1127-KDBASLM         PIC X(6).                                    
001700*                                 BASLAGERMARKNAD                         
001800*                                 BASIC STOCK MARKET                      
001900     03 1127-LOWVALUE        PIC X(14).                                   
002000*** END COPY WDGX1127C0  LENGTH=30                                        
