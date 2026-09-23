000100 01  REQU-WF0271I1.                                                       
000200*                                 REQUEST-COPYTEXT PGM WF0271             
000300*                                 INTERSTATE RULES LOCATE                 
000400     03 REQU-IDLEGSEL-KEY    PIC X(4).                                    
000500*                                 FAKTURERANDE FÖRETAG TEX VCCS           
000600*                                 LEGAL SELLER IDENTITY                   
000700     03 REQU-IDLANDX3-SEND-KEY                                            
000800                             PIC X(3).                                    
000900*                                 3-STÄLLIG LANDSBETECKNINGSKOD           
001000*                                 3-LETTER CODE FOR COUNTRY.              
001100     03 REQU-IDLANDX3-REC-KEY                                             
001200                             PIC X(3).                                    
001300*                                 3-STÄLLIG LANDSBETECKNINGSKOD           
001400*                                 3-LETTER CODE FOR COUNTRY.              
001500*** END OF VILMAII-COPY LENGTH= 10 BYTES                                  
