000100 01  W479050.                                                             
000200*                                 FÄRDIGPACKADE RADER                     
000300*                                 (RAD-STATUS = 4)                        
000400*                                                                         
000500     03 IDPTYP               PIC X(3).                                    
000600*                                 POSTTYP                                 
000700     03 IDDISTR              PIC S9(5)           COMP-3.                  
000800*                                 DISTRIKTNUMMER                          
000900     03 IDKUNDNR             PIC S9(7)           COMP-3.                  
001000*                                 KUNDNUMMER                              
001100     03 IDKUNDRF             PIC X(10).                                   
001200*                                 KUNDENS REFERENS                        
001300     03 IDPRODNR             PIC S9(7)           COMP-3.                  
001400*                                 PRODUKTIONSNUMMER                       
001500     03 IDPLKLST             PIC S9(3)           COMP-3.                  
001600*                                 PLOCKLISTNUMMER                         
001700     03 IDARTNR              PIC S9(9)           COMP-3.                  
001800*                                 ARTIKELNUMMER                           
001900     03 IDPURAD              PIC S9(5)           COMP-3.                  
002000*                                 RADNUMMER PÅ PACKUNDERLAG               
002100*** END COPY W479050CC0  LENGTH=34                                        
