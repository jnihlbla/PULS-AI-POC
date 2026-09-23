000100 01  W212R22K.                                                            
000200*                                 UPPDATERING                             
000300*                                 BESTÄLLNING/ANNULLATION                 
000400*                                                                         
000500     03 IDPTYP               PIC X(3).                                    
000600*                                 POSTTYP                                 
000700     03 IDARTNR              PIC X(8).                                    
000800*                                 ARTIKELNUMMER                           
000900     03 IDBEST               PIC X(12).                                   
001000*                                 BESTÄLLNINGS-ID (PPPBBBBBBSSS)          
001100*                                 PPP   = (PREFIX) INKÖPARNR              
001200*                                 BBBBBB= BESTÄLLARNR                     
001300*                                 SSS   = (SUFFIX) GODSM/PROD.KOD         
001400     03 IDLEVNR-BEST         PIC X(5).                                    
001500*                                 LEVERANTÖR ENL. BESTÄLLNING             
001600     03 TIBEST               PIC X(6).                                    
001700*                                 BESTÄLLNINGSDATUM (ÅÅMMDD)              
001800     03 KVBEST               PIC X(7).                                    
001900*                                 BESTÄLLT ANTAL                          
002000     03 KDBEH-BEST           PIC X.                                       
002100*                                 BEHANDLINGSKOD BESTÄLLNING              
002200     03 TENOT-BESTPRIS       PIC X(20).                                   
002300*                                 NOTERING KÖPVILLKOR                     
002400     03 IDLEVNR-SHIP         PIC X(5).                                    
002500*                                 SKEPPANDE LEVERANTÖR                    
002600*** END OF VILMAII-COPY LENGTH= 67 BYTES                                  
