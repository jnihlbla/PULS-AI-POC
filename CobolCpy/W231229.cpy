000100 01  W231229.                                                             
000200*                                 STATISTIKPOST INNEHÅLLANDE              
000300*                                 UPPGIFT OM ANTAL  PER ARTIKEL           
000400*                                 TILL ELLER FRÅN LAGER                   
000500     03 IDPTYP               PIC X(3).                                    
000600*                                 POSTTYP                                 
000700     03 IDARTNR              PIC S9(9)           COMP-3.                  
000800*                                 ARTIKELNUMMER                           
000900     03 KDCLAGER             PIC S9              COMP-3.                  
001000      88 KDCLAGER-C1         VALUE +1.                                    
001100      88 KDCLAGER-C2         VALUE +2.                                    
001200*                                 CENTRALLAGERKOD                         
001300     03 KDUPPD               PIC X.                                       
001400      88 TILL-LAGER          VALUE '1'.                                   
001500      88 FRAN-LAGER          VALUE '2'.                                   
001600*                                 UPPDATERINGSTYP                         
001700     03 KDRORELS             PIC S9(3)           COMP-3.                  
001800      88 INVENTERING         VALUE +1.                                    
001900      88 CLEARING            VALUE +2.                                    
002000      88 PLAN-INLEV          VALUE +3.                                    
002100      88 OPLAN-INLEV         VALUE +4.                                    
002200      88 LAN-FR-LAGER        VALUE +5.                                    
002300      88 RETUR-FR-LAGER      VALUE +6.                                    
002400      88 SKROTNING           VALUE +7.                                    
002500      88 KUNDORDER           VALUE +8.                                    
002600*                                 RÖRELSE I LAGER                         
002700     03 KVANTAL              PIC S9(7)           COMP-3.                  
002800*                                 ANTAL ALLMÄNT                           
002900*** END COPY W231229CC0  LENGTH=16                                        
