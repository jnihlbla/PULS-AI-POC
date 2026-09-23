000100 01  W5705C.                                                              
000200*                                 ARTIKELSALDO-INFORMATION                
000300*                                 FÖR USA OCH KANADA                      
000400     03 IDARTNR              PIC S9(9)           COMP-3.                  
000500*                                 ARTIKELNUMMER                           
000600     03 IDDC                 PIC X(2).                                    
000700*                                 IDENTIFIERARE LAGER                     
000800     03 DAREGDAT             PIC 9(8).                                    
000900*                                 REGISTRERINGSDATUM (ÅÅÅÅMMDD)           
001000     03 TIKLOCK              PIC S9(9)           COMP-3.                  
001100*                                 KLOCKSLAG (TTMMSSTH)                    
001200     03 KVLS                 PIC S9(7)           COMP-3.                  
001300*                                 LAGERSALDO                              
001400     03 KVEFRS               PIC S9(7)           COMP-3.                  
001500*                                 EJ FAKTURERAT ANTAL STYCK               
001600     03 KVAKS                PIC S9(7)           COMP-3.                  
001700*                                 ANKOMSTSALDO                            
001800     03 KVAKS-PAV            PIC S9(7)           COMP-3.                  
001900*                                 DEL AV AK PÅ VÄG                        
002000     03 PRAVCOST             PIC S9(7)V9(2)      COMP-3.                  
002100*                                 MEDELVÄRDESKOSTNAD I UTL.VALUTA         
002200     03 KDTRADP              PIC X(4).                                    
002300*                                 TRADING PARTNER                         
002400*** END OF VILMAII-COPY LENGTH= 45 BYTES                                  
