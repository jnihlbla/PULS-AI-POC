000100 01  W33530.                                                              
000200*                                 BETALAREINFORMATION FRÅN                
000300*                                 MARKNADSBOLAG ARTIKELRABATT             
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
001400     03 TISTODAT             PIC S9(7)           COMP-3.                  
001500*                                 GENERELLT STOPPDATUM                    
001600     03 IDARTNR              PIC S9(9)           COMP-3.                  
001700*                                 ARTIKELNUMMER                           
001800     03 REARTRAB-DO          PIC S9(2)V9(2)      COMP-3.                  
001900*                                 ARTIKELRABATT DAGORDER                  
002000     03 REARTRAB-BULK        PIC S9(2)V9(2)      COMP-3.                  
002100*                                 ARTIKELRABATT BULKORDER                 
002200     03 TIKLOCK              PIC 9(8).                                    
002300*                                 KLOCKSLAG (TTMMSSTH)                    
002400*** END OF VILMAII-COPY LENGTH= 31 BYTES                                  
