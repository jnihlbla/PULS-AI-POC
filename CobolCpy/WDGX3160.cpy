000100 01  3160-WDGX3160.                                                       
000200*                                 REGISTER CORE RECEIVING CODES           
000300*                                 FYSISK NYCKEL: KDBYTREF                 
000400     03 3160-KDBYTREF        PIC X(3).                                    
000500*                                 CENTRAL REFERENS                        
000600*                                 CENTRAL REFERENCE                       
000700     03 3160-DAREGDAT        PIC 9(8).                                    
000800*                                 REGISTRERINGSDATUM (ÅÅÅÅMMDD)           
000900*                                 REGISTRATION DATE (YYYYMMDD)            
001000     03 3160-IDUSER          PIC X(8).                                    
001100*                                 ANVÄNDARENS SÄKERHETS ID                
001200*                                 USER SECURITY-IDENTITY                  
001300     03 3160-KVPOINT         PIC S9(7)           COMP-3.                  
001400*                                 POINT VALUE                             
001500     03 3160-TENOTE          PIC X(40).                                   
001600*                                 NOTERINGSFÄLT                           
001700*                                 NOTE FIELD                              
001800     03 3160-TIKLOCK         PIC S9(9)           COMP-3.                  
001900*                                 KLOCKSLAG (TTMMSSTH)                    
002000*                                 TIME OF DAY (HHMMSSTH)                  
002100*** END OF VILMAII-COPY LENGTH= 68 BYTES                                  
