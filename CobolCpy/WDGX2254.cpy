000100 01  2254-WDGX2254.                                                       
000200*                                 BYTE HUVUDLEVERANTÖR                    
000300*                                 SAMT SKEPPANDE DC I KINA                
000400*                                 NYCKEL = KY2254                         
000500*                                 (IDDC + IDARTNR)                        
000600     03 2254-IDDC            PIC X(2).                                    
000700*                                 IDENTIFIERARE LAGER                     
000800     03 2254-IDARTNR         PIC S9(9)           COMP-3.                  
000900*                                 ARTIKELNUMMER                           
001000     03 2254-IDLEVNR-FRAM    PIC X(5).                                    
001100*                                 FRAMTIDA LEVERANTÖRNUMMER               
001200     03 2254-IDLEVNR-SHIP-FRAM                                            
001300                             PIC X(5).                                    
001400*                                 FRAMTIDA SKEPP.LEVERANTÖRNUMMER         
001500     03 2254-TILEVDAT        PIC S9(7)           COMP-3.                  
001600*                                 DATUM FRAMTIDA LEVERANTÖRNUMMER         
001700*** END OF VILMAII-COPY LENGTH= 21 BYTES                                  
