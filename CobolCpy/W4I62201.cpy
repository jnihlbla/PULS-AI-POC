000100 01  MID-W4I62201.                                                        
000200     03 MID-IDTRPTNR-IN      PIC X(3).                                    
000300*                                 TRANSPORT IDENTITY                      
000400     03 MID-IDLBBET-IN       PIC X(12).                                   
000500*                                 TRAILER NUMBER                          
000600     03 MID-TISKEPPN-IN      PIC 9(6).                                    
000700*                                 SHIPPING DATE    (YYMMDD)               
000800     03 MID-IDDC-IN          PIC X(2).                                    
000900*                                 WAREHOUSE IDENTIFIER                    
001000     03 MID-INPUT.                                                        
001100        05 MID-KDCMD         OCCURS 12 TIMES                              
001200                             PIC X.                                       
001300*                                 LINE UPDATE COMMAND                     
001400     03 MID-IDLTERM          PIC X(8).                                    
001500*                                 IDENTITY OF LOGICAL TERMINAL            
001600     03 MID-IDDC-REC         PIC X(2).                                    
001700*                                 RECEIVING WAREHOUSE                     
001800*** END OF VILMAII-COPY LENGTH= 45 BYTES                                  
