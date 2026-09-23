000100 01  4522-WDGX4522.                                                       
000200*                                 BESKRIVNING AV                          
000300*                                 ORDERBEKRÄFTELSETEXTER                  
000400*                                 FYSISK NYCKEL WDGXKEY:                  
000500*                                 (KDORDBEK + IDSKYLT)                    
000600     03 4522-KDORDBEK        PIC 9(2).                                    
000700*                                 ORDERBEKRÄFTELSEKOD                     
000800*                                 ORDERCONFIMATIONCODE                    
000900     03 4522-IDSKYLT         PIC X(3).                                    
001000*                                 NATIONALITETSTECKEN                     
001100*                                 NATIONALITY SIGN                        
001200     03 4522-TEORDBEK        PIC X(70).                                   
001300*                                 ORDERBEKRÄFTELSETEXT                    
001400*                                 ORDERCONFIRMATIONTEXT                   
001500     03 4522-FILLER          PIC X(75).                                   
001600*** END COPY WDGX4522C0  LENGTH=150                                       
