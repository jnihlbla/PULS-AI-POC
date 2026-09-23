000100 01  1130-WDGX1130.                                                       
000200*                                 BASLAGER                                
000300*                                 ORDERGENERERING                         
000400*                                 ÅTERSTARTSREGISTER                      
000500*                                 FYSISK NYCKEL: KDSEGKEY                 
000600*                                 SKALL VARA  = "1"                       
000700     03 1130-KDSEGKEY        PIC X.                                       
000800*                                 TEKNISK SEGMENT-NYCKEL                  
000900*                                 TECHNICAL SEGMENT KEY                   
001000     03 1130-KVPOST          PIC S9(7)           COMP-3.                  
001100*                                 RÄKNARE, ANTAL POSTER                   
001200*                                 RECORD COUNTER                          
001300     03 1130-KDPRODSL        PIC S9(3)           COMP-3.                  
001400*                                 PRODUKTSLAG                             
001500*                                 TYPE OF ASSORTMENT                      
001600     03 1130-KDBASLM         PIC X(6).                                    
001700*                                 BASLAGERMARKNAD                         
001800*                                 BASIC STOCK MARKET                      
001900     03 1130-IDPROJ          PIC X(4).                                    
002000*                                 PARTS PROJEKTIDENTITET                  
002100*                                 PARTS PROJECT IDENTITY                  
002200     03 1130-IDORDNR         PIC S9(5)           COMP-3.                  
002300*                                 ORDERNUMMER                             
002400*                                 ORDER NUMBER                            
002500     03 FILLER               PIC X(20).                                   
002600*** END COPY WDGX1130C0  LENGTH=40                                        
