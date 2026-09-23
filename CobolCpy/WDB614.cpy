000100 01  AKT-WDB614.                                                          
000200*                                 DC STYRREGISTER                         
000300*                                 STYRTABELL FÖR AKTIVERING               
000400*                                 AV ARTIKLAR PÅ ALLA DC (EJ CDC)         
000500*                                 FYSISK NYCKEL: WDB614KY                 
000600*                                 (5-ITEM + KDPRODSL(1) ANVÄNDS           
000700*                                  FÖR SORTERING PÅ BILD-2367)            
000800*                                                                         
000900     03 AKT-KVAKT            PIC S9(3)           COMP-3.                  
001000*                                 AKTIVERINGSANTAL                        
001100*                                 LEVEL FOR ACTIVATING A PART             
001200     03 AKT-KVVECKOR-PUBV    PIC S9(3)           COMP-3.                  
001300*                                 ANTAL VECKOR FRÅN PUBL.VECKAN           
001400*                                 NO. OF WEEKS SINCE PUBL.WEEK            
001500     03 AKT-PRARTSTD         PIC 9(7).                                    
001600*                                 ARTIKELSTANDARDPRIS                     
001700*                                 STANDARD PRICE                          
001800     03 AKT-VLARTNTO         PIC 9(8).                                    
001900*                                 ARTIKELVOLYM NETTO (CM3)                
002000*                                 PART NET VOLUME    (CM3)                
002100     03 AKT-KVPB-SEP-REF     PIC S9(7)           COMP-3.                  
002200*                                 SUMMAN AV PB-SEP + REFILL               
002300*                                 SUM OF PB-SEP + REFIL                   
002400     03 AKT-KDPRODSL         OCCURS 11 TIMES                              
002500                             PIC S9(3)           COMP-3.                  
002600*                                 PRODUKTSLAG                             
002700*                                 PRODUCT GROUP                           
002800*** END OF VILMAII-COPY LENGTH= 45 BYTES                                  
