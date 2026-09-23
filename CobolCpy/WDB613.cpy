000100 01  PASS-WDB613.                                                         
000200*                                 DC STYRREGISTER                         
000300*                                 STYRTABELL FÖR PASSIVERING              
000400*                                 AV ARTIKLAR PÅ ALLA DC (EJ CDC)         
000500*                                 FYSISK NYCKEL: WDB613KY                 
000600*                                 (5-ITEM + ADLAGOMR(1) ANVÄNDS           
000700*                                  FÖR SORTERING PÅ BILD-2366)            
000800*                                                                         
000900     03 PASS-KVVECKOR-LSALES PIC S9(3)           COMP-3.                  
001000*                                 VECKOR SEDAN SISTA EFTERFRÅGAN          
001100*                                 NN.OF WEEKS SINCE LAST SALES            
001200     03 PASS-KVVECKOR-PUBV   PIC S9(3)           COMP-3.                  
001300*                                 ANTAL VECKOR FRÅN PUBL.VECKAN           
001400*                                 NO. OF WEEKS SINCE PUBL.WEEK            
001500     03 PASS-PRARTSTD        PIC 9(7).                                    
001600*                                 ARTIKELSTANDARDPRIS                     
001700*                                 STANDARD PRICE                          
001800     03 PASS-VLARTNTO        PIC 9(8).                                    
001900*                                 ARTIKELVOLYM NETTO (CM3)                
002000*                                 PART NET VOLUME    (CM3)                
002100     03 PASS-KDPRODSL        PIC S9(3)           COMP-3.                  
002200*                                 PRODUKTSLAG                             
002300*                                 PRODUCT GROUP                           
002400     03 PASS-ADLAGOMR        OCCURS 10 TIMES                              
002500                             PIC S9(3)           COMP-3.                  
002600*                                 LAGEROMRÅDE                             
002700*                                 AREA                                    
002800*** END OF VILMAII-COPY LENGTH= 41 BYTES                                  
