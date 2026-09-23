000100 01  PCOO-W111PCOO.                                                       
000200*                                 LÄNKAREA VID ANROP AV SUBPGM            
000300*                                 W111PCOO.                               
000400*                                                                         
000500*                                 KDCALL IFYLLS ALLTID.                   
000600*                                 -KDCALL=1 :SÖK MED PCOO-IDARTNR         
000700*                                                   ,PCOO-IDLEVNR         
000800*                                  SVAR: PCOO-FLPCOO                      
000900*                                  MÖJLIGA PCOO-KDSVAR:                   
001000*                                  - 0 => ALLT ÄR OK                      
001100*                                  - 1 => NÅGOT ÄR FEL                    
001200*                                                                         
001300     03 PCOO-KDCALL          PIC S9(3)           COMP-3.                  
001400*                                 ANROPSTYP                               
001500     03 PCOO-IDARTNR         PIC S9(9)           COMP-3.                  
001600*                                 ARTIKELNUMMER                           
001700     03 PCOO-IDLEVNR         PIC X(5).                                    
001800*                                 LEVERANTÖRNUMMER                        
001900     03 PCOO-IDLANDX2        PIC X(2).                                    
002000*                                 2-STÄLLIG LANDSBETECKNINGSKOD           
002100     03 PCOO-KDSVAR          PIC X.                                       
002200*                                 SVAR FRÅN PROGRAM ELLER SKÄRM           
002300     03 PCOO-FLPCOO          PIC X.                                       
002400*                                 FLAGGA OM FÖRMÅNSAVTAL URS.LAND         
002500*** END OF VILMAII-COPY LENGTH= 16 BYTES                                  
