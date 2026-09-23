000100 01  3152-WDGX3152.                                                       
000200*                                 NES ORDER                               
000300*                                 BESTÄLLARE                              
000400*                                 FYSISK NYCKEL: KY3152                   
000500*                                 (IDUSER-OREG + IDDISTR)                 
000600     03 3152-IDUSER-OREG     PIC X(8).                                    
000700*                                 ANSVARIGT USERID ORDERREG.              
000800*                                 RESPONSIBLE USERID ORDERREG.            
000900     03 3152-IDDISTR         PIC S9(5)           COMP-3.                  
001000*                                 DISTRIKTNUMMER                          
001100*                                 DISTRICT NUMBER                         
001200     03 3152-FLKLAR          PIC X.                                       
001300*                                 AVSLUTNINGSMARKERING                    
001400*                                 FINISHED FLAG                           
001500     03 3152-TIREGDAT        PIC S9(7)           COMP-3.                  
001600*                                 REGISTRERINGSDATUM (ÅÅMMDD)             
001700*                                 REGISTRATION DATE (YYMMDD)              
001800     03 3152-TIKLOCK         PIC S9(9)           COMP-3.                  
001900*                                 KLOCKSLAG (TTMMSSTH)                    
002000*                                 TIME OF DAY (HHMMSSTH)                  
002100*** END OF VILMAII-COPY LENGTH= 21 BYTES                                  
