000100 01  3-W155L033.                                                          
000200*                                 LÄNKAREA TILL PROGRAMMET W15503         
000300     03 3-IDKATRAD           PIC S9(5)           COMP-3.                  
000400*                                 RADNUMMER                               
000500     03 3-TIUPPDAT           PIC S9(7)           COMP-3.                  
000600*                                 UPPDATERINGSDATUM  (ÅÅMMDD)             
000700     03 3-KDRADST            PIC X.                                       
000800*                                 RAD-     L = LÅNAD.                     
000900*                                 STATUS   Ä = ÄNDRAD.                    
001000*                                          N = NYREGISTRERAD.             
001100*                                          C = L,Ä,N EFTER OMBRYT         
001200*                                              FRAM TILL VADGEN.          
001300*                                      SPACE = OFÖRÄNDRAD.                
001400     03 3-IDRUBNR            PIC S9(5)           COMP-3.                  
001500*                                 RUBRIKNUMMER                            
001600     03 3-IDSEGMNR           PIC S9              COMP-3.                  
001700*                                 ORDNINGSFÖLJD PÅ SEGMENTET              
001800     03 3-FLRUBTYP           PIC X.                                       
001900*                                 STYRNING AV RUBRIKUTSKRIFT              
002000*                                 J = LÖPANDE TEXT                        
002100*                                 N = RAD FÖR RAD                         
002200*** END OF VILMAII-COPY LENGTH= 13 BYTES                                  
