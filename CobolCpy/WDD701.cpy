000100 01  WDD701.                                                              
000200*                                 INFO OM ERSATT ARTIKEL                  
000300*                                 FYSISK NYCKEL IDARTNR                   
000400     03 IDARTNR              PIC S9(9)           COMP-3.                  
000500*                                 ARTIKELNUMMER                           
000600*                                 PART NUMBER                             
000700     03 DIERS-ERS            PIC S9(4)V9(3)      COMP-3.                  
000800*                                 ERSATT ARTIKELANTAL                     
000900*                                 NUMBER OF SUPERSEDED                    
001000     03 FLPUB                PIC X.                                       
001100*                                 PUBLICERINGSKOD                         
001200     03 KVKORT               PIC S9(3)           COMP-3.                  
001300*                                 ANTAL KORT (ERSÄTTNINGS-RADER)          
001400     03 IDUSER               PIC X(8).                                    
001500*                                 ANVÄNDARIDENTITET I RACF                
001600*                                 USER RACF-IDENTITY                      
001700     03 TEARTNOT             PIC X(40).                                   
001800*                                 ARTIKEL NOTERING                        
001900*                                 PARTS NOTIFY                            
002000*** END COPY WDD701CCC0  LENGTH=60                                        
