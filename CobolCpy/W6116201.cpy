000100 01  W6116201.                                                            
000200*                                 INNEHÅLLER ALLA KOLLIN UTAN             
000300*                                 PLATS ELLER MED KVALITETSFEL            
000400     03 IDOKOLLI             PIC 9(9).                                    
000500*                                 ODETTE KOLLINUMMER                      
000600     03 IDLEVNR-KOLLI        PIC X(5).                                    
000700*                                 LEVERANTÖRNUMMER KOLLI                  
000800     03 IDLOPNRM             PIC S9(9)           COMP-3.                  
000900*                                 LÖPNUMMER MOTTAGNINGSRAPPORT            
001000*                                 (0VVDLLLLK)                             
001100     03 IDARTNR              PIC S9(9)           COMP-3.                  
001200*                                 ARTIKELNUMMER                           
001300     03 KVINLART             PIC S9(7)           COMP-3.                  
001400*                                 ANTAL I PARTIRAD                        
001500     03 ADGANG               PIC S9(3)           COMP-3.                  
001600*                                 GÅNG                                    
001700     03 ADLAGOMR             PIC S9(3)           COMP-3.                  
001800*                                 LAGEROMRÅDE                             
001900     03 ADPLATS              PIC S9(5)           COMP-3.                  
002000*                                 LAGERPLATSNUMMER                        
002100     03 FLKVAFEL             PIC X.                                       
002200*                                 FLAGGA KVALITETSFEL                     
002300*** END OF VILMAII-COPY LENGTH= 36 BYTES                                  
