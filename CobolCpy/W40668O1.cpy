000100 01  RESP-W40668O1.                                                       
000200*                                 RESP COPYTEXT TILL W40668.              
000300*                                                                         
000400     03 RESP-KVRADER         PIC 9(5).                                    
000500*                                 ANTAL RADER                             
000600     03 RESP-RADER           OCCURS 500 TIMES.                            
000700*                                                                         
000800        05 RESP-KDSVAR-LINE-ATTR                                          
000900                             PIC X(2).                                    
001000*                                 MFS ATTRIBUTFÄLT                        
001100        05 RESP-KDSVAR-LINE  PIC X.                                       
001200*                                 SVAR FRÅN PROGRAM ELLER SKÄRM           
001300        05 RESP-IDTRPTNR-LINE                                             
001400                             PIC Z(2)9.                                   
001500*                                 TRANSPORTIDENTITET                      
001600        05 RESP-IDLBBET-LINE PIC X(12).                                   
001700*                                 LASTBÄRARBETECKNING                     
001800*** END OF VILMAII-COPY LENGTH= 9005 BYTES                                
