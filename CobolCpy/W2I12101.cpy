000100 01  MID-W2I12101.                                                        
000200*                                 MID-COPYTEXT FÖR W2012100               
000300     03 MID-IDARTNR-IN       PIC X(9).                                    
000400*                                 ARTIKELNUMMER                           
000500     03 MID-IDARTNR-UT       PIC X(9).                                    
000600*                                 ARTIKELNUMMER                           
000700     03 MID-SW-633           PIC X.                                       
000800     03 MID-IDBEST-SW        PIC 9(12).                                   
000900*                                 BESTÄLLNINGS-ID (PPPBBBBBBSSS)          
001000*                                 PPP   = (PREFIX) INKÖPARNR              
001100*                                 BBBBBB= BESTÄLLARNR                     
001200*                                 SSS   = (SUFFIX) GODSM/PROD.KOD         
001300     03 MID-TEST-AVTNR-1     PIC 9(12).                                   
001400*                                 AVTALSIDENTITET  (PPPBBBBBSSS)          
001500*                                 PPP   = INKÖPARNR (PREFIX)              
001600*                                 BBBBB = BESTÄLLARNR                     
001700*                                 SSS   = SUFFIX                          
001800     03 MID-TEST-AVTNR-2     PIC 9(12).                                   
001900*                                 AVTALSIDENTITET  (PPPBBBBBSSS)          
002000*                                 PPP   = INKÖPARNR (PREFIX)              
002100*                                 BBBBB = BESTÄLLARNR                     
002200*                                 SSS   = SUFFIX                          
002300     03 MID-TEST-IDLEVNR-AVT-1                                            
002400                             PIC X(5).                                    
002500*                                 LEVERANTÖRNUMMER                        
002600     03 MID-TEST-IDLEVNR-AVT-2                                            
002700                             PIC X(5).                                    
002800*                                 LEVERANTÖRNUMMER                        
002900     03 MID-IDBEST-IN        PIC 9(12).                                   
003000     03 MID-IDLEVNR          PIC X(5).                                    
003100*                                 LEVERANTÖRNUMMER                        
003200     03 MID-JUST-KVANT       PIC 9(7).                                    
003300*                                 BESTÄLLT ANTAL                          
003400     03 MID-U-N-A            PIC X.                                       
003500     03 MID-IDAVTAL-AVT-2    PIC X(12).                                   
003600*** END OF VILMAII-COPY LENGTH= 102 BYTES                                 
