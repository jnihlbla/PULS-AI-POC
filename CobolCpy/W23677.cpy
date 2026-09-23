000100 01  W23677.                                                              
000200*                                                                         
000300*                                 INFO ORDERINGÅNG OCH                    
000400*                                 AVBOKADE RADER SAMT                     
000500*                                 ANSKAFFARES NAMN                        
000600*                                                                         
000700     03 IDANSK               PIC S9(3)           COMP-3.                  
000800*                                 ANSKAFFARNUMMER                         
000900     03 KVINORD-SUM          PIC S9(9)           COMP-3.                  
001000*                                 ANTAL INKOMNA ORDERRADER                
001100     03 KVAVBRAD-SUM         PIC S9(9)V9(2)      COMP-3.                  
001200*                                 AVBOKADE RADER                          
001300     03 IDNAMN               PIC X(40).                                   
001400*                                 NAMN                                    
001500     03 IDAVD                PIC S9(5)           COMP-3.                  
001600*                                 DEN ANSTÄLLDES AVDELNING/               
001700*                                 KOSTNADSSTÄLLE                          
001800     03 IDTFN                PIC X(20).                                   
001900*                                 TELEFONNUMMER EXTERNT                   
002000     03 FILLER               PIC X(8).                                    
002100     03 IDMAIL               PIC X(60).                                   
002200*                                 MAIL ADRESS                             
002300*** END OF VILMAII-COPY LENGTH= 144 BYTES                                 
