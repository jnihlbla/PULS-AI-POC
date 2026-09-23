000100 01  REQU-WF0258I1.                                                       
000200*                                 REQUEST-COPYTEXT PGM WF0258             
000300*                                 PAYMENT TERM MAINTENANCE                
000400     03 REQU-IDLEGSEL-KEY    PIC X(4).                                    
000500*                                 FAKTURERANDE FÖRETAG TEX VCCS           
000600*                                 LEGAL SELLER IDENTITY                   
000700     03 REQU-IDSPRAK-KEY     PIC X(2).                                    
000800*                                 2-STÄLLIG ISO SPRÅKKOD                  
000900*                                 2-LETTER ISO LANGUAGE CODE              
001000     03 REQU-KDBETALV-KEY    PIC X(4).                                    
001100*                                 BETALNINGSVILLKOR KUNDRESKONTRA         
001200*                                 TERMS OF PAYMENT                        
001300     03 REQU-BEBETVIL        PIC X(30).                                   
001400*                                 BETALNINGSVILLKORSTEXT                  
001500*                                 TERMS OF PAYMENT TEXT                   
001600*** END OF VILMAII-COPY LENGTH= 40 BYTES                                  
