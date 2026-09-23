000100 01  ARTSUM-W488021.                                                      
000200*                                 BUFFERFÖRÄNDRINGAR SUMMERADE            
000300*                                 PER ARTIKEL OCH CLAGER                  
000400     03 ARTSUM-IDPTYP-021    PIC X(3).                                    
000500*                                 POSTTYP                                 
000600     03 ARTSUM-IDLOPNRF      PIC S9(5)           COMP-3.                  
000700*                                 LÖPNUMMER FELTRANS                      
000800     03 ARTSUM-IDARTNR       PIC X(8).                                    
000900*                                 ARTIKELNUMMER                           
001000     03 ARTSUM-KVBUFF-F      PIC S9(7)           COMP-3.                  
001100*                                 FÖRÄDLAT BUFFERSALDO                    
001200     03 ARTSUM-KVBUFF-OF     PIC S9(7)           COMP-3.                  
001300*                                 BUFFERSALDO OFÖRÄDLAT GODS              
001400     03 ARTSUM-KVKOLLI-F     PIC S9(5)           COMP-3.                  
001500*                                 ANTAL FÖRÄDLADE KOLLI I BUFFER          
001600     03 ARTSUM-KVKOLLI-OF    PIC S9(5)           COMP-3.                  
001700*                                 ANTAL OFÖRÄDLADE KOLLI I BUFFER         
001800     03 ARTSUM-IDFELKOD      PIC X(3).                                    
001900*                                 FELKOD                                  
002000*** END COPY W488021CC0  LENGTH=31                                        
