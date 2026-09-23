000100 01  REQU-WL0122I1.                                                       
000200*                                 REQUEST TO PGM WL0122                   
000300     03 REQU-IDDC-KEY        PIC X(2).                                    
000400*                                 IDENTIFIERARE LAGER                     
000500     03 REQU-IDANSTNR-KEY    PIC 9(5).                                    
000600*                                 ANSTÄLLNINGSNUMMER                      
000700     03 REQU-IDDISTR-KEY     PIC 9(4).                                    
000800*                                 DISTRIKTNUMMER                          
000900     03 REQU-IDKUNDNR-KEY    PIC 9(6).                                    
001000*                                 KUNDNUMMER                              
001100     03 REQU-IDORDNR-KEY     PIC 9(5).                                    
001200*                                 ORDERNUMMER UTGÅR PD90                  
001300     03 REQU-IDKOLLI-KEY     PIC 9(5).                                    
001400*                                 KOLLINUMMER                             
001500     03 REQU-IDPRODNR-KEY    PIC 9(7).                                    
001600*                                 PRODUKTIONSNUMMER                       
001700     03 REQU-KDKOLLI         PIC X(8).                                    
001800*                                 KOLLIKOD                                
001900     03 REQU-VKORDBTO-KOLLI  PIC X(8).                                    
002000*                                 ORDERVIKT BRUTTO (KG)                   
002100     03 REQU-FLSISTAK        PIC X.                                       
002200*                                 SISTA KOLLI I ORDERN?                   
002300     03 REQU-KDEMBTYP        PIC 9.                                       
002400*                                 EMBALLAGETYP                            
002500     03 REQU-DIKOLLIL        PIC 9(4).                                    
002600*                                 KOLLI-LÄNGD                             
002700     03 REQU-DIKOLLIB        PIC 9(3).                                    
002800*                                 KOLLI-BREDD                             
002900     03 REQU-DIKOLLIH        PIC 9(3).                                    
003000*                                 KOLLI-HÖJD                              
003100     03 REQU-ADFLGEO         PIC X(3).                                    
003200*                                 GEOGRAFISKT OMRÅDE FÄRDIGLAGER          
003300     03 REQU-ADFLOMR         PIC 9(3).                                    
003400*                                 LASTNINGSOMR/STÄLL FÄRDIGLAGER          
003500     03 REQU-ADRUTNIV        PIC 9(3).                                    
003600*                                 RUTA/NIVÅ I FÄRDIGLAGRET                
003700     03 REQU-IDKOLLI-FOM     PIC 9(5).                                    
003800*                                 KOLLINUMMER                             
003900     03 REQU-IDKOLLI-TOM     PIC 9(5).                                    
004000*                                 KOLLINUMMER                             
004100     03 REQU-FLSKRIV-CLABEL  PIC X.                                       
004200*                                 J/Y = SKRIV BEGÄRD LISTA                
004300     03 REQU-FLSKRIV-DELNOTE PIC X.                                       
004400*                                 J/Y = SKRIV BEGÄRD LISTA                
004500     03 REQU-IDKOLLI-SAMP    PIC 9(5).                                    
004600*                                 SAMPACKNINGSKOLLINUMMER                 
004700     03 REQU-KDMATT          PIC X.                                       
004800*                                 MÅTTKOD                                 
004900     03 REQU-RAD             OCCURS 200 TIMES.                            
005000        05 REQU-IDRADNR-FOM  PIC 9(4).                                    
005100*                                 RADNUMMER                               
005200        05 REQU-IDRADNR-TOM  PIC 9(4).                                    
005300*                                 RADNUMMER                               
005400        05 REQU-KVLEVART     PIC 9(7).                                    
005500*                                 LEVERERAT ANTAL STYCK                   
005600*** END OF VILMAII-COPY LENGTH= 3089 BYTES                                
