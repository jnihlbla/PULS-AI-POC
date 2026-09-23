000100 01  W61222.                                                              
000200*                                 OUTPUT FILE WITH CONTAINERS             
000300*                                 WHERE PULS WANT PROJECT44               
000400*                                 TO STOP SEND PUSHEVENTS.                
000500     03 IDCONTNR             PIC S9(11)          COMP-3.                  
000600*                                 UNIQUE ID OF CONTAINER FROM             
000700*                                 PROJECT44                               
000800     03 IDSUBSCR             PIC S9(11)          COMP-3.                  
000900*                                 SUBSCRIPTION ID FROM PROJECT44          
001000     03 IDLBBET              PIC X(12).                                   
001100*                                 LASTBÄRARBETECKNING                     
001200     03 DABERANK             PIC 9(8).                                    
001300*                                 BERÄKNAD ANKOMSTDATUM                   
001400     03 IDFAKT               PIC S9(7)           COMP-3.                  
001500*                                 FAKTURANUMMER                           
001600     03 IDDC-REC             PIC X(2).                                    
001700*                                 MOTTAGANDE LAGER                        
001800*** END OF VILMAII-COPY LENGTH= 38 BYTES                                  
