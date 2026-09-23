000100 01  MID-W0I55101.                                                        
000200*                                 COPYTEXT FÖR MID W0I55101               
000300     03 MID-TEINFO           PIC X(1200).                                 
000400*                                 ALLMÄN TEXT INFO                        
000500*                                 GENERAL TEXT INFO                       
000600     03 MID-MID-GRP-FILLER REDEFINES MID-TEINFO.                          
000700        05 MID-MID-GRP.                                                   
000800           07 MID-IDDOKTYP-IN                                             
000900                             PIC X(8).                                    
001000*                                 DOKUMENTATIONSTYP                       
001100*                                 TYPE OF DOCUMENTATION                   
001200           07 MID-IDDOKTYP-UT                                             
001300                             PIC X(8).                                    
001400*                                 DOKUMENTATIONSTYP                       
001500*                                 TYPE OF DOCUMENTATION                   
001600           07 MID-IDDOK-IN   PIC X(8).                                    
001700*                                 DOKUMENTATIONSIDENTITET                 
001800*                                 DOCUMENTATION IDENTITY                  
001900           07 MID-IDDOK-UT   PIC X(8).                                    
002000*                                 DOKUMENTATIONSIDENTITET                 
002100*                                 DOCUMENTATION IDENTITY                  
002200           07 MID-IDSID      PIC 9(3).                                    
002300*                                 SIDNUMRERING                            
002400*                                 PAGE NUMBER                             
002500        05 FILLER            PIC X(1165).                                 
002600*** END OF VILMAII-COPY LENGTH= 1200 BYTES                                
