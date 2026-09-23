000100 01  W56141.                                                              
000200*                                 FIELDS FROM WDL601 AND WDL611           
000300*                                                                         
000400     03 IDLANDX2             PIC X(2).                                    
000500*                                 2-STÄLLIG LANDSBETECKNINGSKOD           
000600*                                 2-LETTER CODE FOR COUNTRY               
000700     03 IDDC                 PIC X(2).                                    
000800*                                 IDENTIFIERARE LAGER                     
000900*                                 WAREHOUSE IDENTIFIER                    
001000     03 IDARTNR              PIC S9(9)           COMP-3.                  
001100*                                 ARTIKELNUMMER                           
001200*                                 PART NUMBER                             
001300     03 IDLOPNRM             PIC S9(9)           COMP-3.                  
001400*                                 LÖPNUMMER MOTTAGNINGSRAPPORT            
001500*                                 (0VVDLLLLK)                             
001600*                                 SERIAL NO RECEIVING REPORT              
001700*                                 (0WWDLLLLC)                             
001800     03 KVANTMOT             PIC S9(7)           COMP-3.                  
001900*                                 ANTAL MOTTAGET                          
002000*                                 QUANTITY RECEIVED                       
002100     03 KDPRODSL             PIC 9(3).                                    
002200*                                 PRODUKTSLAG                             
002300*                                 PRODUCT GROUP                           
002400     03 KDPSLLOC             PIC 9(2).                                    
002500*                                 PRODUKTSLAG LOKALT                      
002600*                                 PRODUCT GROUP LOCAL                     
002700     03 PRARTBEL-PR          PIC S9(8)V9(5)      COMP-3.                  
002800*                                 DETTA BESTÄLLNINGSPRIS                  
002900*                                 (I LEVERANTÖRENS VALUTA)                
003000*** END OF VILMAII-COPY LENGTH= 30 BYTES                                  
