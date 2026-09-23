000100 01  W221W022.                                                            
000200*                                 TABELL   FÖR ANSKAFFNINGEN              
000300*                                 DATASETET INNEHÅLLER                    
000400*                                 ANSKAFFARNUMMER MED TILLHÖRANDE         
000500*                                 NAMN, AVDELNING,                        
000600*                                 TELEFONNUMMER OCH USERID                
000700*                                                                         
000800     03 IDANSK               PIC 9(3).                                    
000900*                                 ANSKAFFARNUMMER                         
001000     03 BENAMN               PIC X(35).                                   
001100*                                 ANSKAFFARNAMN                           
001200*                                                                         
001300     03 IDAVD                PIC 9(5).                                    
001400*                                 DEN ANSTÄLLDES AVDELNING/               
001500*                                 KOSTNADSSTÄLLE                          
001600     03 IDTEL                PIC 9(4).                                    
001700*                                 TELEFON-NUMMER        IDTEL-002         
001800     03 FILLER               PIC X.                                       
001900     03 IDUSER               PIC X(8).                                    
002000*                                 ANVÄNDARENS SÄKERHETS ID                
002100     03 FILLER               PIC X(24).                                   
002200*** END COPY W221W022    LENGTH=80                                        
