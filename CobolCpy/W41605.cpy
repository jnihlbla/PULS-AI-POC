000100 01  W41605.                                                              
000200*                                 ARTIKLAR SOM SKALL LARMAS PGA T         
000300*                                 ID I SATSSYSTEMET                       
000400*                                                                         
000500*                                                                         
000600     03 IDANSK               PIC S9(3)           COMP-3.                  
000700*                                 ANSKAFFARNUMMER                         
000800     03 IDARTNR              PIC S9(9)           COMP-3.                  
000900*                                 ARTIKELNUMMER                           
001000     03 IDORDNST.                                                         
001100*                                 SATSORDERNUMMER-TOTALT                  
001200        05 IDORDNSB          PIC S9(5)           COMP-3.                  
001300*                                 SATSORDERNUMMER-BAS                     
001400        05 IDORDNSS          PIC S9              COMP-3.                  
001500*                                 SATSORDERNUMMER-SUFFIX                  
001600     03 IDPRODNR             PIC S9(7)           COMP-3.                  
001700*                                 PRODUKTIONSNUMMER                       
001800     03 KDSATSTA             PIC X.                                       
001900*                                 STATUS FÖR SATSORDER                    
002000     03 KVBEART              PIC S9(7)           COMP-3.                  
002100*                                 BESTÄLLT ANTAL ARTIKLAR                 
002200     03 TIREGDAT             PIC S9(7)           COMP-3.                  
002300*                                 REGISTRERINGSDATUM (ÅÅMMDD)             
002400     03 TIBEGPAC             PIC S9(7)           COMP-3.                  
002500*                                 BEGÄRD PACKNINGSDAG    (ÅÅMMDD)         
002600     03 IDDISTR              PIC S9(5)           COMP-3.                  
002700*                                 DISTRIKTNUMMER                          
002800*** END COPY W41605      LENGTH=31                                        
