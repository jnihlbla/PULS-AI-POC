000100 01  MOD-W3O31501.                                                        
000200*                                 MOD-COPYTEXT                            
000300*                                 FÖR W3O315                              
000400     03 MOD-IDTRANS          PIC X(4).                                    
000500*                                 BILDNUMMER                              
000600     03 MOD-TEMFSFEL         PIC X(40).                                   
000700*                                 MFS FELMEDDELANDE                       
000800     03 MOD-IDPROMR-IN.                                                   
000900*                                 PRISOMRÅDE (RABATTSTRUKTUR)             
001000        05 MOD-IDMARKBO      PIC X.                                       
001100*                                 MARKNADSBOLAGSKOD                       
001200        05 MOD-IDPROMRN      PIC X(2).                                    
001300*                                 PRISOMRÅDE LÖPNUMMER                    
001400     03 MOD-IDPROMR-UT.                                                   
001500*                                 PRISOMRÅDE (RABATTSTRUKTUR)             
001600        05 MOD-IDMARKBO      PIC X.                                       
001700*                                 MARKNADSBOLAGSKOD                       
001800        05 MOD-IDPROMRN      PIC X(2).                                    
001900*                                 PRISOMRÅDE LÖPNUMMER                    
002000     03 MOD-IDDISTR-IN       PIC X(4).                                    
002100*                                 DISTRIKTNUMMER                          
002200     03 MOD-IDDISTR-UT       PIC X(4).                                    
002300*                                 DISTRIKTNUMMER                          
002400     03 MOD-IDPROMR-SPAR.                                                 
002500*                                 PRISOMRÅDE (RABATTSTRUKTUR)             
002600        05 MOD-IDMARKBO      PIC X.                                       
002700*                                 MARKNADSBOLAGSKOD                       
002800        05 MOD-IDPROMRN      PIC X(2).                                    
002900*                                 PRISOMRÅDE LÖPNUMMER                    
003000     03 MOD-KDARTRAB-ENTER   PIC 9(2).                                    
003100*                                 RABATTKOD (ARTIKELPRIS)                 
003200     03 MOD-KDARTRAB-NEXT    PIC 9(2).                                    
003300*                                 RABATTKOD (ARTIKELPRIS)                 
003400     03 MOD-TISTADAT         PIC 9(6).                                    
003500*                                 GENERELLT STARTDATUM                    
003600     03 MOD-W30315           OCCURS 26 TIMES.                             
003700        05 MOD-NOR-RAB       PIC 9(2).                                    
003800*                                 RABATTKOD (ARTIKELPRIS)                 
003900        05 MOD-MO-NOR-RAB    PIC Z9.9(2).                                 
004000*                                 ARTIKELRABATT BULKORDER                 
004100        05 MOD-DO-NOR-RAB    PIC Z9.9(2).                                 
004200*                                 ARTIKELRABATT DAGORDER                  
004300     03 MOD-TEMFSINF         PIC X(55).                                   
004400*                                 INFORMATIONSMEDDELANDE                  
004500*** END OF VILMAII-COPY LENGTH= 438 BYTES                                 
