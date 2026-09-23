000100* GENERATION OF COBOL HOST STRUCTURE FROM CACUPA-TAB                      
000200  01 CACUPA.                                                              
000300*              SALES CAMPAIGN TARGET DEALER                               
000400   03 IDLANDX2                          PIC X(2).                         
000500*              2-STÄLLIG LANDSBETECKNINGSKOD                              
000600   03 IDUSER                            PIC X(10).                        
000700*              ANVÄNDARENS SÄKERHETS ID                                   
000800   03 IDFSGKAM                          PIC S9(3) COMP-3.                 
000900*              CAMPAIGN NUMBER                                            
001000   03 IDARTNR20                         PIC X(20).                        
001100*              20-STÄLLIGT ARTIKELNUMMER FÖR AS400 (VIPS)                 
001200*              FORMATET ÄR HÖGERJUSTERAT MED INLEDANDE                    
001300*              BLANKTECKEN, OCH UTAN INLEDANDE NOLLOR.                    
001400   03 SUDLRNET                          PIC S9(13)V9(2) COMP-3.           
001500*              VÄRDE TILL DEALER NET                                      
001600   03 KVFSGTAR                          PIC S9(9) COMP-3.                 
001700*              SÄLJMÅL KVANTITET                                          
001800   03 REFSGRAB                          PIC S9(3)V9(2) COMP-3.            
001900*              FÖRSÄLJNINGSRABATT KAMPANJ                                 
002000   03 KVFSGBE                           PIC S9(9) COMP-3.                 
002100*              BREAL EVEN KVANTITET                                       
002200*                                                                         
002300*** END OF VILMAII-COPY LENGTH= 55 OLD LENGTH=                            
