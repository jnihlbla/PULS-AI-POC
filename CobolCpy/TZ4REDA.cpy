000100* GENERATION OF COBOL HOST STRUCTURE FROM TZ4REDA-TAB                     
000200  01 TZ4REDA.                                                             
000300*              TZ4REDA                                                    
000400   03 IDOUTTYPE                         PIC X(15).                        
000500*              OUTPUTTYP                                                  
000600   03 IDOUTREC                          PIC X(30).                        
000700*              OUTPUTMOTTAGARE                                            
000800   03 IDLIST                            PIC X(10).                        
000900*              LISTIDENTITET                                              
001000   03 TIREGDAT                          PIC S9(7) COMP-3.                 
001100*              REGISTRERINGSDATUM (ÅÅMMDD)                                
001200   03 TIKLOCK                           PIC S9(9) COMP-3.                 
001300*              KLOCKSLAG (TTMMSSTH)                                       
001400   03 IDLOPNR                           PIC S9(3) COMP-3.                 
001500*              LÖPNUMMER                                                  
001600   03 KVPOST                            PIC S9(7) COMP-3.                 
001700*              RÄKNARE, ANTAL POSTER                                      
001800   03 TEOUTDATA.                                                          
001900*              OUTPUTDATA                                                 
002000     49 TEOUTDATA-L                     PIC S9(4) COMP.                   
002100*              OUTPUTDATA                                                 
002200     49 TEOUTDATA-D                     PIC X(3000).                      
002300*              OUTPUTDATA                                                 
002400*                                                                         
002500*** END OF VILMAII-COPY LENGTH= 3072 OLD LENGTH=                          
