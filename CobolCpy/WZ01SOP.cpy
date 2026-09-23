000100 01  REQU-WZ01SOP.                                                        
000200*                                 TRANSACTION TO PROGRAM W00606           
000300*                                 TO ORDER A PROCESS IN SOP               
000400     03 REQU-FILLER          PIC X(5)                                     
000500                             VALUE SPACES.                                
000600     03 REQU-IDPROCESS       PIC X(10)                                    
000700                             VALUE SPACES.                                
000800*                                 PROCESSNAMN                             
000900*                                 PROCESS NAME                            
001000     03 REQU-KDSOPFUNK       PIC X                                        
001100                             VALUE SPACE.                                 
001200*                                 FUNKTIONSTYP TILL SOP PROGRAM           
001300*                                 ACTION TYPE FOR SOP PROGRAM             
001400     03 REQU-TIORDDAT        PIC 9(6)                                     
001500                             VALUE ZEROS.                                 
001600*                                 ORDERDATUM                              
001700     03 REQU-TESYMBV         PIC X(500)                                   
001800                             VALUE SPACES.                                
001900*** END OF VILMAII-COPY LENGTH= 522 BYTES                                 
