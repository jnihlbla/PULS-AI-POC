000100 01  W51239.                                                              
000200*                                 ANVÄNDS VID                             
000300*                                 VARULAGERVÄRDERING.                     
000400     03 IDARTNR              PIC S9(9)           COMP-3.                  
000500*                                 ARTIKELNUMMER                           
000600     03 IDLEVNR              PIC X(5).                                    
000700*                                 LEVERANTÖRNUMMER                        
000800     03 KDPRODSL             PIC S9(3)           COMP-3.                  
000900*                                 PRODUKTSLAG                             
001000     03 KVAKS                PIC S9(7)           COMP-3.                  
001100*                                 ANKOMSTSALDO                            
001200     03 KVEFRS               PIC S9(7)           COMP-3.                  
001300*                                 EJ FAKTURERAT ANTAL STYCK               
001400     03 KVLS                 PIC S9(7)           COMP-3.                  
001500*                                 LAGERSALDO                              
001600     03 PRINK                PIC S9(7)V9(2)      COMP-3.                  
001700*                                 INKÖPSPRIS                              
001800     03 RETULF               PIC S9(3)V9(4)      COMP-3.                  
001900*                                 TULLFAKTOR                              
002000     03 SUARTVLV-INK         PIC S9(11)V9(2)     COMP-3.                  
002100*                                 SUMMA VARULAGERVÄRDERINGSPRIS           
002200     03 BEST-PRISER          OCCURS 5 TIMES.                              
002300        05 TIPRLIST          PIC S9(7)           COMP-3.                  
002400*                                 PRISLISTEDATUM (AAMMDD)                 
002500        05 SUINLEV-PR        PIC S9(3)           COMP-3.                  
002600*                                 ANTAL INLEV. TILL DETTA PRIS            
002700        05 KVANTMOT          PIC S9(7)           COMP-3.                  
002800*                                 ANTAL MOTTAGET                          
002900        05 PRARTBEL-PR       PIC S9(8)V9(5)      COMP-3.                  
003000*                                 DETTA BESTÄLLNINGSPRIS                  
003100*                                 (I LEVERANTÖRENS VALUTA)                
003200        05 SUARTVLV-PR       PIC S9(11)V9(2)     COMP-3.                  
003300*                                 SUMMA VARULAGERVÄRDERINGSPRIS           
003400        05 KDVALISO          PIC X(3).                                    
003500*                                 VALUTAKOD ENLIGT ISO-STANDARD.          
003600*** END OF VILMAII-COPY LENGTH= 175 BYTES                                 
