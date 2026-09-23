000100 01  BEN-WDD301.                                                          
000200*                                 BENÄMNINGSREGISTER                      
000300*                                 ROTSEGMENT                              
000400*                                 FYSISK NYCKEL IDBENNR                   
000500*                                 SÖKNING KAN SKE ÄVEN GENOM              
000600*                                 WDD3ASEQ OCH WDD3BSEQ                   
000700     03 BEN-IDBENNR          PIC S9(7)           COMP-3.                  
000800*                                 BENÄMNINGSNUMMER                        
000900     03 BEN-KDHOMONYM        PIC S9              COMP-3.                  
001000*                                 HOMONYMKOD                              
001100     03 BEN-TIUPPDAT-STOP    PIC S9(7)           COMP-3.                  
001200*                                 UPPDATERINGSDATUM  (ÅÅMMDD)             
001300     03 BEN-KDBENSTAT        PIC S9              COMP-3.                  
001400*                                 BENÄMNINGENS STATUS PÅ BENREG.          
001500*                                 <2 NAMNLEX, 2 RS-UNIK, 3 RENSAD         
001600     03 BEN-FLAENDR          PIC X.                                       
001700*                                 ÄNDRINGSFLAGGA                          
001800     03 BEN-FILLER           PIC X(4).                                    
001900*** END OF VILMAII-COPY LENGTH= 15 BYTES                                  
