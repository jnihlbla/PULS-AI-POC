000100* GENERATION OF COBOL HOST STRUCTURE FROM CACUP-TAB                       
000200  01 CACUP.                                                               
000300*              SALES CAMPAIGN HEADER                                      
000400   03 IDLANDX2                          PIC X(2).                         
000500*              2-STÄLLIG LANDSBETECKNINGSKOD                              
000600   03 IDUSER                            PIC X(10).                        
000700*              ANVÄNDARENS SÄKERHETS ID                                   
000800   03 IDFSGKAM                          PIC S9(3) COMP-3.                 
000900*              CAMPAIGN NUMBER                                            
001000   03 TEFSGKAM                          PIC X(30).                        
001100*              TEXT FÖRSÄLJNINGSKAMPANJ                                   
001200   03 DAKAMVV-FIRST                     PIC S9(7) COMP-3.                 
001300*              FÖRSTA FÖRSÄLJNINGSKAMPANJ VECKA                           
001400   03 DAKAMVV-LAST                      PIC S9(7) COMP-3.                 
001500*              SISTA FÖRSÄLJNINGSKAMPANJ VECKA                            
001600   03 DAUPPDAT                          PIC X(8).                         
001700*              UPPDATERINGSDATUM  (ÅÅÅÅMMDD)                              
001800*                                                                         
001900*** END OF VILMAII-COPY LENGTH= 60 OLD LENGTH=                            
