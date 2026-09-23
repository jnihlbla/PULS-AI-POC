000100 01  RESP-WF0258O1.                                                       
000200*                                 RESPONS-COPYTEXT FÖR PGM WF0258         
000300*                                 PAYMENT TERM MAINTENANCE                
000400     03 RESP-IDLEGSEL-KEY    PIC X(4).                                    
000500*                                 FAKTURERANDE FÖRETAG TEX VCCS           
000600*                                 LEGAL SELLER IDENTITY                   
000700     03 RESP-IDSPRAK-KEY     PIC X(2).                                    
000800*                                 2-STÄLLIG ISO SPRÅKKOD                  
000900*                                 2-LETTER ISO LANGUAGE CODE              
001000     03 RESP-KDBETALV-KEY    PIC X(4).                                    
001100*                                 BETALNINGSVILLKOR KUNDRESKONTRA         
001200*                                 TERMS OF PAYMENT                        
001300     03 RESP-BELEGRAD-1      PIC X(35).                                   
001400*                                 DEL AV LEGAL SELLER NAMN                
001500*                                 PART OF LEGAL SELLER NAME               
001600     03 RESP-BEBETVIL        PIC X(30).                                   
001700*                                 BETALNINGSVILLKORSTEXT                  
001800*                                 TERMS OF PAYMENT TEXT                   
001900     03 RESP-DAREGDAT        PIC Z(8).                                    
002000*                                 REGISTRERINGSDATUM (ÅÅÅÅMMDD)           
002100*                                 REGISTRATION DATE (YYYYMMDD)            
002200     03 RESP-DAUPPDAT        PIC Z(8).                                    
002300*                                 UPPDATERINGSDATUM  (ÅÅÅÅMMDD)           
002400*                                                                         
002500*                                 UPDATING DATE     (YYYYMMDD)            
002600*                                                                         
002700     03 RESP-IDUSER          PIC X(8).                                    
002800*                                 ANVÄNDARENS SÄKERHETS ID                
002900*                                 USER SECURITY-IDENTITY                  
003000*** END OF VILMAII-COPY LENGTH= 99 BYTES                                  
