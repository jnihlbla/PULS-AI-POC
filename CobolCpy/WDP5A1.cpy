000100 01  SEQA-WDP5A1.                                                         
000200*                                 SEKUNDÄRT INDEX TILL WDP501             
000300*                                 INFORMATIONS TEXT REGISTER              
000400*                                 FYSISK NYCKEL: WDP5A1KY                 
000500*                                  (IDSKYLT + IDDOKTYP + IDDOK)           
000600*                                 SEKUNDÄR NYCKEL: WDP5ASEQ               
000700*                                  (IDSKYLT + IDDOKTYP)                   
000800     03 SEQA-IDSKYLT         PIC X(3).                                    
000900*                                 NATIONALITETSTECKEN                     
001000*                                 NATIONALITY SIGN                        
001100     03 SEQA-IDDOKTYP        PIC X(8).                                    
001200*                                 DOKUMENTATIONSTYP                       
001300*                                 TYPE OF DOCUMENTATION                   
001400     03 SEQA-IDDOK           PIC X(8).                                    
001500*                                 DOKUMENTATIONSIDENTITET                 
001600*                                 DOCUMENTATION IDENTITY                  
001700     03 SEQA-TIREGDAT        PIC S9(7)           COMP-3.                  
001800*                                 REGISTRERINGSDATUM (ÅÅMMDD)             
001900*                                 REGISTRATION DATE (YYMMDD)              
002000     03 SEQA-TIREGTID        PIC S9(7)           COMP-3.                  
002100*                                 REGISTRERINGSTID                        
002200     03 SEQA-IDUSER          PIC X(8).                                    
002300*                                 ANVÄNDARIDENTITET I RACF                
002400*                                 USER RACF-IDENTITY                      
002500     03 SEQA-BEDOK           PIC X(25).                                   
002600*                                 DOKUMENTATIONSBESKRIVNING               
002700*                                 DOCUMENTATION DESCRIPTION               
002800*** END COPY WDP5A1CCC0  LENGTH=60                                        
