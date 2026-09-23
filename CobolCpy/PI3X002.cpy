000010*** EDIT ALLOWED                                                          
000100*    CONSTRUCTED: 970415                                                  
000200*    NAME:        PV INKÖP, VIR II                                        
000300*                                                                         
000400*                                                                         
000500 01  PI30002.                                                             
000600*                    *** KEY FIELDS FROM PI30001 COPYTEXT                 
000700   03 KEYS.                                                               
000800     05  PLANT                                PIC X(1).                   
000900     05  REPTYPE                              PIC X(1).                   
001000     05  REPSERNO                             PIC X(5).                   
001100*                                                                         
001200   03 THE-REST.                                                           
001300*                    *** RECORD TYPE = 2                                  
001400     05  RECTYPE                              PIC X(1).                   
001500*                    *** PURCHASER NUMBER                                 
001600     05  PURCHNO                              PIC X(3).                   
001700*                    *** PURCHASER NAME                                   
001800     05  PURCHNAME                            PIC X(30).                  
001900*                    *** SUPPLIER NAME                                    
002000     05  SUPPNAME                             PIC X(30).                  
002100*                    *** CAR FAMILY                                       
002200     05  CARFAM                               PIC X(4).                   
002300*                    *** PART NAME                                        
002400     05  PARTNAME                             PIC X(25).                  
002500*                    *** PART NAME IN PALLET 1                            
002600     05  PARTNMP1                             PIC X(25).                  
002700*                    *** PART NAME IN PALLET 2                            
002800     05  PARTNMP2                             PIC X(25).                  
002900*                    *** PART NAME IN PALLET 3                            
003000     05  PARTNMP3                             PIC X(25).                  
003100*                                                                         
001200   03 FILLER                            PIC X(825) VALUE SPACE.           
003200*** END OF VILMAII-COPY LENGTH= 2000 OLD LENGTH= 2000                     
