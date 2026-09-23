000100 01  INFO-WDP501.                                                         
000200*                                 INFORMATIONS TEXT REGISTER              
000300*                                 FYSISK NYCKEL: WDP501KY                 
000400*                                  (IDSKYLT + IDDOKTYP + IDDOK)           
000500     03 INFO-IDSKYLT         PIC X(3).                                    
000600*                                 NATIONALITETSTECKEN                     
000700*                                 SPRÅKIDENTIFIKATION                     
000800*                                 NATIONALITY SIGN                        
000900*                                 LANGUAGE IDENTIFIER                     
001000     03 INFO-IDDOKTYP        PIC X(8).                                    
001100*                                 DOKUMENTATIONSTYP                       
001200*                                 TYPE OF DOCUMENTATION                   
001300     03 INFO-IDDOK           PIC X(8).                                    
001400*                                 DOKUMENTATIONSIDENTITET                 
001500*                                 DOCUMENTATION IDENTITY                  
001600     03 INFO-TIREGDAT        PIC S9(7)           COMP-3.                  
001700*                                 REGISTRERINGSDATUM (ÅÅMMDD)             
001800*                                 REGISTRATION DATE (YYMMDD)              
001900     03 INFO-TIREGTID        PIC S9(7)           COMP-3.                  
002000*                                 REGISTRERINGSTID                        
002100*                                 GENERAL REGISTRATION TIME               
002200     03 INFO-IDUSER          PIC X(8).                                    
002300*                                 ANVÄNDARENS SÄKERHETS ID                
002400*                                 USER SECURITY-IDENTITY                  
002500     03 INFO-BEDOK           PIC X(25).                                   
002600*                                 DOKUMENTATIONSBESKRIVNING               
002700*                                 DOCUMENTATION DESCRIPTION               
002800     03 INFO-FILLER          PIC X.                                       
002900*** END OF VILMAII-COPY LENGTH= 61 BYTES                                  
