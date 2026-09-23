000100 01  TEXT-WDP512.                                                         
000200*                                 INFORMATIONS TEXT REGISTER              
000300*                                 TEXT SEGMENT                            
000400*                                 FYSISK NYCKEL: IDSID                    
000500     03 TEXT-IDSID           PIC S9(3)           COMP-3.                  
000600*                                 SIDNUMRERING                            
000700*                                 PAGE NUMBER                             
000800     03 TEXT-IDUSER          PIC X(8).                                    
000900*                                 ANVÄNDARENS SÄKERHETS ID                
001000*                                 USER SECURITY-IDENTITY                  
001100     03 TEXT-TIREGDAT        PIC S9(7)           COMP-3.                  
001200*                                 REGISTRERINGSDATUM (ÅÅMMDD)             
001300*                                 REGISTRATION DATE (YYMMDD)              
001400     03 TEXT-TIREGTID        PIC S9(7)           COMP-3.                  
001500*                                 REGISTRERINGSTID                        
001600*                                 GENERAL REGISTRATION TIME               
001700     03 TEXT-TEINFO          PIC X(1200).                                 
001800*                                 ALLMÄN TEXT INFO                        
001900*                                 GENERAL TEXT INFO                       
002000*** END OF VILMAII-COPY LENGTH= 1218 BYTES                                
