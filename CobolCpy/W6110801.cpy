000100*                                                                         
000200*                                                                         
000300*    COPYTEXT FÖR VCC INHOUSE FORMAT: 001                                 
000400*    FILE HEADER RECORD                                                   
000500******************************************************************        
000600 01  W6110801.                                                            
000700     03  PT              PIC X(3).                                        
000800*         VALUE '001'       *** RECORD TYPE                               
000900     03  DA-LENGTH       PIC 9(3).                                        
001000*         VALUE 073         *** DATA LENGTH                               
001100     03  SCOMNOD         PIC X(4).                                        
001200*                           *** SENDER NODE                               
001300     03  RCOMNOD         PIC X(4).                                        
001400*                           *** RECEIVER NODE                             
001500     03  VIRTFIL          PIC X(8).                                       
001600*                           *** VIRTUAL FILENAME                          
001700     03  DATE            PIC 9(6).                                        
001800*                           *** VIRTUAL FILE DATE                         
001900     03  TIME            PIC 9(6).                                        
002000*                           *** VIRTUAL FILE TIME                         
002100     03  FILLER          PIC X(45).                                       
002200*                           *** FILLER                                    
002300*** END COPY W6110801    LENGTH=79                                        
