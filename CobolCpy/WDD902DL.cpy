000100 01  WDD902DL-CTX.                                                        
000200*                                 DELIVERY PLAN REGISTER                  
000300*                                 SUPPLIER INFORMATION                    
000400*                                                                         
000500     03 IDSEGM               PIC X(6).                                    
000600*                                 SEGMENT                                 
000700     03 FILLERX2             PIC X(2).                                    
000800     03 IDARTNR-KEY          PIC S9(9)           COMP-3.                  
000900*                                 ARTIKELNUMMER                           
001000     03 IDDC-KEY             PIC X(2).                                    
001100*                                 IDENTIFIERARE LAGER                     
001200     03 IDLEVNR-KEY          PIC X(5).                                    
001300*                                 LEVERANTÖRNUMMER                        
001400     03 FILLERX26            PIC X(26).                                   
001500     03 WDD902-CTX.                                                       
001600*                                 LEVERANSPLANEREGISTER                   
001700*                                 LEVERANTÖRSINFORMATION                  
001800*                                 FYSISK NYCKEL  IDLEVNR                  
001900        05 IDLEVNR           PIC X(5).                                    
002000*                                 LEVERANTÖRNUMMER                        
002100        05 KVBR              PIC S9(7)           COMP-3.                  
002200*                                 BESTÄLLNINGSREST                        
002300        05 TILEVPL           PIC S9(7)           COMP-3.                  
002400*                                 LEVERANSPLANEDATUM  (ÅÅMMDD)            
002500        05 FILLERX7          PIC X(7).                                    
002600*** END OF VILMAII-COPY LENGTH= 66 BYTES                                  
