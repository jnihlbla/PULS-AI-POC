000100 01  W488L221.                                                            
000200*                                 LÄNKAREA NR 1 FÖR KOMMUNIKATION         
000300*                                 MELLAN W4882200 OCH DESS SUBPGM         
000400*                                 ANVÄNDS VID ANROPEN:                    
000500*                                         LAS-SALDO-SEG                   
000600*                                         UPPDATERA-SALDO-SEG             
000700     03 IDARTNR              PIC S9(9)           COMP-3.                  
000800*                                 ARTIKELNUMMER                           
000900     03 KVBUFF-F             PIC S9(7)           COMP-3.                  
001000*                                 FÖRÄDLAT BUFFERSALDO                    
001100     03 KVBUFF-OF            PIC S9(7)           COMP-3.                  
001200*                                 BUFFERSALDO OFÖRÄDLAT GODS              
001300     03 KVKOLLI-F            PIC S9(5)           COMP-3.                  
001400*                                 ANTAL FÖRÄDLADE KOLLI I BUFFER          
001500     03 KVKOLLI-OF           PIC S9(5)           COMP-3.                  
001600*                                 ANTAL OFÖRÄDLADE KOLLI I BUFFER         
001700*** END COPY W488L221C0  LENGTH=19                                        
