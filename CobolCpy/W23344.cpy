000100 01  W23344-CTX.                                                          
000200*                                 PROGNOSER TILL SIPLUS FÖR KINA          
000300*                                 NDC                                     
000400*                                 INOM AKTUELL PERIOD                     
000500     03 TIAAPP-NEXT          PIC S9(5)           COMP-3.                  
000600*                                 ÅR - PLANERINGSPERIOD (ÅÅPP)            
000700*                                 12 PER ÅR                               
000800*                                 NUMERA ÄR DETTA "PV-PERIOD"             
000900     03 IDDC                 PIC X(2).                                    
001000*                                 IDENTIFIERARE LAGER                     
001100     03 IDLEVNR              PIC X(5).                                    
001200*                                 LEVERANTÖRNUMMER                        
001300     03 IDARTNR              PIC 9(9).                                    
001400*                                 ARTIKELNUMMER                           
001500     03 IDLEVNR-DC           PIC X(5).                                    
001600*                                 DC LEVERANTÖR                           
001700     03 KDAVROP              PIC S9              COMP-3.                  
001800*                                 AVROPSKOD                               
001900     03 TIAVRDAT-INL         PIC S9(7)           COMP-3.                  
002000*                                 PLANERAT INLEVERANSDATUM                
002100     03 KVAVROP              PIC S9(7)           COMP-3.                  
002200*                                 AVROPSKVANTITET                         
002300     03 TIAAPP-INL           PIC S9(5)           COMP-3.                  
002400*                                 ÅR - PLANERINGSPERIOD (ÅÅPP)            
002500*                                 12 PER ÅR                               
002600*                                 NUMERA ÄR DETTA "PV-PERIOD"             
002700*** END OF VILMAII-COPY LENGTH= 36 BYTES                                  
