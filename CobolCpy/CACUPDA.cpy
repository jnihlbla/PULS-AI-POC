000100* GENERATION OF COBOL HOST STRUCTURE FROM CACUPDA-TAB                     
000200  01 CACUPDA.                                                             
000300*              SALES CAMPAIGN TARGET DEALER                               
000400   03 IDLANDX2                          PIC X(2).                         
000500*              2-STÄLLIG LANDSBETECKNINGSKOD                              
000600   03 IDUSER                            PIC X(10).                        
000700*              ANVÄNDARENS SÄKERHETS ID                                   
000800   03 IDFSGKAM                          PIC S9(3) COMP-3.                 
000900*              CAMPAIGN NUMBER                                            
001000   03 IDDEALER                          PIC X(6).                         
001100*              DEALER KUNDNUMMER                                          
001200   03 IDARTNR20                         PIC X(20).                        
001300*              20-STÄLLIGT ARTIKELNUMMER FÖR AS400 (VIPS)                 
001400*              FORMATET ÄR HÖGERJUSTERAT MED INLEDANDE                    
001500*              BLANKTECKEN, OCH UTAN INLEDANDE NOLLOR.                    
001600   03 SUDLRNET                          PIC S9(13)V9(2) COMP-3.           
001700*              VÄRDE TILL DEALER NET                                      
001800   03 KVFSGTAR                          PIC S9(9) COMP-3.                 
001900*              SÄLJMÅL KVANTITET                                          
002000   03 REFSGRAB                          PIC S9(3)V9(2) COMP-3.            
002100*              FÖRSÄLJNINGSRABATT KAMPANJ                                 
002200   03 KVFSGBE                           PIC S9(9) COMP-3.                 
002300*              BREAL EVEN KVANTITET                                       
002400*                                                                         
002500*** END OF VILMAII-COPY LENGTH= 61 OLD LENGTH=                            
