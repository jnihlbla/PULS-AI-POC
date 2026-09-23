000100 01  W33510.                                                              
000200*                                 BETALAREINFORMATION FRÅN                
000300*                                 XBMS                                    
000400     03 IDVTYP               PIC X.                                       
000500*                                 POSTTYPSVERSION                         
000600     03 IDPROMR.                                                          
000700*                                 PRISOMRÅDE (RABATTSTRUKTUR)             
000800        05 IDMARKBO          PIC X.                                       
000900*                                 MARKNADSBOLAGSKOD                       
001000        05 IDPROMRN          PIC X(2).                                    
001100*                                 PRISOMRÅDE LÖPNUMMER                    
001200     03 TISTADAT             PIC S9(7)           COMP-3.                  
001300*                                 GENERELLT STARTDATUM                    
001400     03 001-GRUPP            OCCURS 99 TIMES.                             
001500*                                 RABATTGRUPP  1-99 RABATTER              
001600*                                 MED RABATT FÖR DAGORDER OCH             
001700*                                 MÅNADSORDER                             
001800        05 KDRABATT          PIC 9(3).                                    
001900*                                 RABATTKOD                               
002000        05 REARTRAB-DO       PIC S9(2)V9(2)      COMP-3.                  
002100*                                 ARTIKELRABATT DAGORDER                  
002200        05 REARTRAB-BULK     PIC S9(2)V9(2)      COMP-3.                  
002300*                                 ARTIKELRABATT BULKORDER                 
002400     03 TIKLOCK              PIC 9(8).                                    
002500*                                 KLOCKSLAG (TTMMSSTH)                    
002600*** END OF VILMAII-COPY LENGTH= 907 BYTES                                 
