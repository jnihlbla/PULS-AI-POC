000010 01  SEQB-WDM7B1.                                                         
000020*                                 TULLSYSTEM                              
000030*                                 SEKUNDÄRT INDEX TILL WDM701             
000040*                                 EXIT: INDEX FINNS NÄR                   
000050*                                 IDTULLNR  > ZERO                        
000060*                                 FYSISK NYCKEL: WDM7B1KY                 
000070*                                 (IDTULL,   IDUSER,                      
000080*                                  IDFAKT,   IDORDNR7,                    
000090*                                  IDKOLLI,  IDPRODNR)                    
000100*                                 SECONDARY NYCKEL: WDM7BSEQ              
000110*                                 (IDTULL,   IDUSER)                      
000120     03 SEQB-IDTULL.                                                      
000130*                                 IDENTITET TULL SÄNDNING                 
000140*                                 IDENTITY CUSTOMS TRANSMISSION           
000150        05 SEQB-IDTULFTG     PIC X(2).                                    
000160*                                 IDENTIFIERARE TULLANDE FÖRETAG          
000170*                                                                         
000180*                                 IDENTIFIER COMPANY TO CUSTOMS           
000190        05 SEQB-IDTULLNR     PIC 9(7).                                    
000200*                                 NUMMERSERIE INGÅENDE I TULLID           
000210*                                                                         
000220*                                 SERIAL NUMBER IN CUSTOMS ID             
000230        05 SEQB-RETULKS      PIC 9.                                       
000240*                                 KONTROLLSIFFRA TULLID                   
000250*                                 CHECK DIGIT CUSTOMS ID                  
000260     03 SEQB-IDUSER          PIC X(8).                                    
000270*                                 ANVÄNDARENS SÄKERHETS ID                
000280*                                 USER SECURITY-IDENTITY                  
000290     03 SEQB-IDWDM701        PIC X(15).                                   
000300*                                 NYCKEL TILL WDM701                      
000310*                                 KEY TO WDM701                           
      *** END COPY WDM7B1      LENGTH=33                                        
