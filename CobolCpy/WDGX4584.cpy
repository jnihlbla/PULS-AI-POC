000010 01  4584-WDGX4584.                                                       
000020*                                 ÅTERSTARTS REGISTER TULLSYSTEM          
000030*                                 FYSISK NYCKEL: WDGXKEY                  
000040*                                  (IDUSER + LOW-VALUE)                   
000050     03 4584-IDUSER          PIC X(8).                                    
000060*                                 ANVÄNDARENS SÄKERHETS ID                
000070*                                 USER SECURITY-IDENTITY                  
000080     03 4584-LOW-VALUE       PIC X(2).                                    
000090     03 4584-IDFAKT          PIC S9(7)           COMP-3.                  
000100*                                 FAKTURANUMMER                           
000110*                                 INVOICE NO.                             
000120     03 4584-IDDISTR         PIC S9(5)           COMP-3.                  
000130*                                 DISTRIKTNUMMER                          
000140*                                 DISTRICT NUMBER                         
000150     03 4584-IDKUNDNR        PIC S9(7)           COMP-3.                  
000160*                                 KUNDNUMMER                              
000170*                                 CUSTOMER NO                             
000180     03 4584-KDFRAKT         PIC S9(3)           COMP-3.                  
000190*                                 FRAKTSÄTT C1-C2 TILL KUND               
000200*                                 FREIGHT CODE                            
000210     03 4584-IDSKEPPN        PIC S9(7)           COMP-3.                  
000220*                                 SKEPPNINGSNUMMER                        
000230*                                 SHIPMENT NO                             
000240     03 4584-IDTULL.                                                      
000250*                                 IDENTITET TULL SÄNDNING                 
000260*                                 IDENTITY CUSTOMS TRANSMISSION           
000270        05 4584-IDTULFTG     PIC X(2).                                    
000280*                                 IDENTIFIERARE TULLANDE FÖRETAG          
000290*                                                                         
000300*                                 IDENTIFIER COMPANY TO CUSTOMS           
000310        05 4584-IDTULLNR     PIC 9(7).                                    
000320*                                 NUMMERSERIE INGÅENDE I TULLID           
000330*                                                                         
000340*                                 SERIAL NUMBER IN CUSTOMS ID             
000350        05 4584-RETULKS      PIC 9.                                       
000360*                                 KONTROLLSIFFRA TULLID                   
000370*                                 CHECK DIGIT CUSTOMS ID                  
000380     03 FILLER               PIC X(3).                                    
      *** END COPY WDGX4584    LENGTH=40                                        
