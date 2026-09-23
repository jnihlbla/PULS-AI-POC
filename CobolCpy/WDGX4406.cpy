000100 01  TRPT-WDGX4406.                                                       
000200*                                 4406 HTR TRANSPORTREGISTER              
000300*                                 NYCKEL: WDGXKEY                         
000400*                                         (IDTPRTNR, IDDC,                
000500*                                          LOWVALUE)                      
000600     03 TRPT-IDTRPTNR        PIC S9(3)           COMP-3.                  
000700*                                 TRANSPORTIDENTITET                      
000800*                                 TRANSPORT IDENTITY                      
000900     03 TRPT-IDDC            PIC X(2).                                    
001000*                                 IDENTIFIERARE LAGER                     
001100*                                 WAREHOUSE IDENTIFIER                    
001200     03 TRPT-LOWVALUE        PIC X(6).                                    
001300     03 TRPT-ADFLGEO         PIC X(3).                                    
001400*                                 GEOGRAFISKT OMRÅDE FÄRDIGLAGER          
001500*                                 GEOGRAPHIC AREA                         
001600     03 TRPT-ADFLOMR         PIC S9(3)           COMP-3.                  
001700*                                 LASTNINGSOMR/STÄLL FÄRDIGLAGER          
001800*                                 DELIVERY AREA                           
001900     03 TRPT-FLUTLAST        PIC X.                                       
002000*                                 KOLLI I UTLASTNINGSLAGER                
002100*                                 CASE IN DISPATCH AREA                   
002200     03 TRPT-FLTOTMS         PIC X.                                       
002300*                                 SEND TO TMS FLAGGA                      
002400*                                 SEND TO TMS FLAG                        
002500*** END OF VILMAII-COPY LENGTH= 17 BYTES                                  
