000100 01  W513.                                                                
000200*                                 POSTTYP RZT                             
000300*                                 LISTPOST TILL W513                      
000400     03 IDPTYP               PIC X(3).                                    
000500*                                 POSTTYP                                 
000600     03 IDARTNR              PIC S9(9)           COMP-3.                  
000700*                                 ARTIKELNUMMER                           
000800     03 KDCLAGER             PIC S9              COMP-3.                  
000900*                                 CENTRALLAGERKOD                         
001000     03 KVCLEAR              PIC S9(7)           COMP-3.                  
001100*                                 KVANTITET ATT CLEARA                    
001200     03 SIGNON-USERID        PIC X(8).                                    
001300*                                 ANVÄNDARIDENTITET I RACF                
001400*** END COPY WDGZRZTCC0  LENGTH=21                                        
