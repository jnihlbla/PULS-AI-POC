000100 01  XLEV-WDF502.                                                         
000200*                                 CROSS-TABLE                             
000300*                                 FYSISK NYCKEL = WDF5KEY                 
000400*                                 (IDLEVNR IDBENR)                        
000500*                                 SÖKBEGREPP = IDLEVNR                    
000600*                                 OCH        = IDBENR                     
000700     03 XLEV-WDF5KEY.                                                     
000800*                                                                         
000900        05 XLEV-IDLEVNR      PIC X(5).                                    
001000*                                 LEVERANTÖRNUMMER                        
001100*                                 SUPPLIER NUMBER (VENDOR NUMBER)         
001200        05 XLEV-IDBENR       PIC S9              COMP-3.                  
001300*                                 BENÄMNINGSNUMMER                        
001400     03 XLEV-FLTLVM          PIC X.                                       
001500*                                 TILLVERKARMÄRKNING                      
001600     03 XLEV-BELEVART        PIC X(30).                                   
001700*                                 LEVERANTÖRENS ARTIKELBENÄMNING          
001800*                                 SUPPLIER PART DESCRIPTION               
001900     03 XLEV-IDLEVART        PIC X(30).                                   
002000*                                 LEVERANTÖRENS ARTNR                     
002100*                                 SUPPLIER PARTNO                         
002200     03 XLEV-FILLER          PIC X(8).                                    
002300*** END OF VILMAII-COPY LENGTH= 75 BYTES                                  
