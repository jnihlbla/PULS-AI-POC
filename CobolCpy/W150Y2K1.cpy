000010*** EDIT ALLOWED                                                          
000100 S50-Y2K-KDCATPUB-R SECTION .                                             
000200     SKIP2                                                                
000300     MOVE +1 TO Y2K-IX                                                    
000400                                                                          
000500     PERFORM UNTIL Y2K-IX > +4                                            
000600     OR WS-KDCATPUB-R-AVV (1:1) = WS-TIAAAA(Y2K-IX) (4:1)                 
000700       ADD +1 TO Y2K-IX                                                   
000800     END-PERFORM                                                          
000900                                                                          
001000     IF Y2K-IX > +4                                                       
001010*      --- Inget godkänt årtal, tag hand om SPACE efter anropet           
001020       MOVE SPACE                TO WS-KDCATPUB-AAAAVV                    
001400     ELSE                                                                 
001500       MOVE WS-TIAAAA(Y2K-IX)    TO WS-KDCATPUB-AAAAVV                    
001600       MOVE WS-KDCATPUB-R-AVV(2:2) TO WS-KDCATPUB-AAAAVV (5:2)            
001700     END-IF                                                               
001800     .                                                                    
