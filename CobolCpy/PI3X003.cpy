000010*** EDIT ALLOWED                                                          
000100*    CONSTRUCTED: 970415                                                  
000200*    NAME:        PV INKÖP, VIR II                                        
000300*                                                                         
000400*                                                                         
000500 01  PI30003.                                                             
000600*                    *** KEY FIELDS FROM PI30001 COPYTEXT                 
000700   03 KEYS.                                                               
000800     05  PLANT                                PIC X(1).                   
000900     05  REPTYPE                              PIC X(1).                   
001000     05  REPSERNO                             PIC X(5).                   
001100*                                                                         
001200   03 THE-REST.                                                           
001300*                    *** RECORD TYPE = 3                                  
001400     05  RECTYPE                              PIC X(1).                   
001500*                    *** ATTENTION                                        
001600     05  ATT                                  PIC X(30).                  
001700*                    *** FAX NUMBER TO CONTACT                            
001800     05  ATTFAX                               PIC X(16).                  
001900*                                                                         
001800     05  ATTEMAIL                             PIC X(64).                  
001900*                                                                         
001800     05  FILLER              PIC X(1882) VALUE SPACE.                     
001900*                                                                         
002000*** END OF VILMAII-COPY LENGTH= 2000 OLD LENGTH= 2000                     
