000100 01  4-W473L804.                                                          
000200*                                 CALL4 COPYTEXT TILL SUBPROG.            
000300*                                 W4738310                                
000400*                                                                         
000500     03 4-REST-IDRADNR-ORD-FROM                                           
000600                             PIC S9(5)           COMP-3.                  
000700*                                 RADNUMMER PÅ VOLVOORDER FROM            
000800     03 4-REST-IDRADNR-ORD-TOM                                            
000900                             PIC S9(5)           COMP-3.                  
001000*                                 RADNUMMER PÅ VOLVOORDER TOM             
001100     03 4-FLNOLLJ            PIC X.                                       
001200*                                 UPPDATERAD AV NOLLJAGARE                
001300*** END COPY W473L804C0  LENGTH=7                                         
