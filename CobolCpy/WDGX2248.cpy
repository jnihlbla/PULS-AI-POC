000100 01  2248-WDGX2248.                                                       
000200*                                 LOCAL PURCHASE ORDER                    
000300*                                 REFILL ARTIKLAR                         
000400*                                 FYSISK NYCKEL: KY2248                   
000500*                                 (IDDC + IDLEVNR + IDARTNR)              
000600     03 2248-IDDC            PIC X(2).                                    
000700*                                 IDENTIFIERARE LAGER                     
000800*                                 WAREHOUSE IDENTIFIER                    
000900     03 2248-IDLEVNR         PIC X(5).                                    
001000*                                 LEVERANTÖRNUMMER                        
001100*                                 SUPPLIER NUMBER (VENDOR NUMBER)         
001200     03 2248-IDARTNR         PIC S9(9)           COMP-3.                  
001300*                                 ARTIKELNUMMER                           
001400*                                 PART NUMBER                             
001500     03 2248-KVDAGAR         PIC S9(3)           COMP-3.                  
001600*                                 ANTAL DAGAR                             
001700     03 2248-KVBEART         PIC S9(7)           COMP-3.                  
001800*                                 BESTÄLLT ANTAL STYCKEN                  
001900*                                 ORDERED QUANTITY                        
002000*** END OF VILMAII-COPY LENGTH= 18 BYTES                                  
