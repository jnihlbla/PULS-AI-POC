000100 01  DSTY-WDF118.                                                         
000200*                                 LEVERANTÖRSREGISTER                     
000300*                                 DIREKTLEVERANSSTYRNING                  
000400*                                 FYSISK NYCKEL  WDF118KY                 
000500*                                  (IDDISTR + IDKUNDNR + KDORDKL)         
000600     03 DSTY-IDDISTR         PIC S9(5)           COMP-3.                  
000700*                                 DISTRIKTNUMMER                          
000800*                                 DISTRICT NUMBER                         
000900     03 DSTY-IDKUNDNR        PIC S9(7)           COMP-3.                  
001000*                                 KUNDNUMMER                              
001100*                                 CUSTOMER NO                             
001200     03 DSTY-KDORDKL         PIC S9              COMP-3.                  
001300*                                 ORDERKLASS                              
001400*                                 ORDER CLASS                             
001500     03 DSTY-IDDC            PIC X(2).                                    
001600*                                 IDENTIFIERARE LAGER                     
001700*                                 WAREHOUSE IDENTIFIER                    
001800     03 DSTY-KDVIA           PIC X(2).                                    
001900*                                 KOD FöR LEVERANS VIA                    
002000*                                 CODE FOR DELIVERY VIA                   
002100     03 DSTY-TIMINUT-CUT     PIC S9(5)           COMP-3.                  
002200*                                 BRYTTID     (HH.MM)                     
002300*                                 CUT TIME    (HH.MM)                     
002400     03 DSTY-KVDAGAR-LEV     PIC S9(3)           COMP-3.                  
002500*                                 WORKDAYS TO DELIVERY                    
002600     03 DSTY-KVDAGAR-DIFF    PIC S9(3)           COMP-3.                  
002700*                                 TRANSPORT DAY DIFF. (+/- CONTRA         
002800*                                  CDC)                                   
002900     03 DSTY-KVDAGAR-TPO     PIC S9(3)           COMP-3.                  
003000*                                 ANTAL DAGAR TILL START AV TPO           
003100*                                 WORKDAYS TO DELIVERY OF TPO             
003200*** END OF VILMAII-COPY LENGTH= 21 BYTES                                  
