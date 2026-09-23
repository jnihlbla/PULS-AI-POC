000100 01  MID-W4I41501.                                                        
000200*                                                                         
000300     03 MID-IDDC-IN          PIC X(2).                                    
000400*                                 IDENTIFIERARE LAGER                     
000500     03 MID-IDDC-UT          PIC X(2).                                    
000600*                                 IDENTIFIERARE LAGER                     
000700     03 MID-IDDISTR-IN       PIC X(4).                                    
000800*                                 DISTRIKTNUMMER                          
000900     03 MID-IDKUNDNR-IN      PIC X(6).                                    
001000*                                 KUNDNUMMER                              
001100     03 MID-INPUT.                                                        
001200*                                                                         
001300        05 MID-KDCMD-IN      PIC X.                                       
001400*                                 RAD-UPPDATERINGSKOMMANDO                
001500*                                  BLANK  = INGENTING                     
001600*                                  D , B  = DELETE                        
001700*                                  R , Ä  = REPLACE                       
001800*                                  I,N,A  = INSERT                        
001900*                                  S , V  = SELECT                        
002000*                                  P , P  = PRINT                         
002100*                                  C , K  = COPY                          
002200        05 MID-KUNDNR-IN     PIC X(6).                                    
002300*                                 KUNDNUMMER                              
002400        05 MID-IDGMTOMR-IN   PIC X(4).                                    
002500*                                 GODSMOTTAGAREOMRÅDE                     
002600        05 MID-KVDAGAR-TRP-DAY-IN                                         
002700                             PIC 9(2).                                    
002800*                                 TRP DAGAR DC TILL KUND (DAG)            
002900        05 MID-IDPKLTAB-IN   PIC X(2).                                    
003000*                                 PRODUKTIONSKLASSTABELLSID               
003100        05 MID-IDPRCTAB-IN   PIC X(2).                                    
003200*                                 PRCTABELLIDENTITET                      
003300        05 MID-KDGENFRA-VOR-IN                                            
003400                             PIC X(2).                                    
003500*                                 NORMAL FRAKT VOR-ORDER                  
003600        05 MID-KDGENFRA-DO-IN                                             
003700                             PIC X(2).                                    
003800*                                 NORMAL FRAKT DAGORDER                   
003900        05 MID-KDGENFRA-MO-IN                                             
004000                             PIC X(2).                                    
004100*                                 NORMAL FRAKT MÅNADSORDER KL 2-4         
004200        05 MID-KVLEDTIM-0-IN PIC X(6).                                    
004300*                                 LEDTID KL 0                             
004400        05 MID-KVLEDTIM-1-IN PIC X(6).                                    
004500*                                 LEDTID KL 1                             
004600        05 MID-KVLEDTIM-2-IN PIC X(6).                                    
004700*                                 LEDTID KL 2                             
004800        05 MID-KVLEDTIM-3-IN PIC X(6).                                    
004900*                                 LEDTID KL 3                             
005000        05 MID-KVLEDTIM-4-IN PIC X(6).                                    
005100*                                 LEDTID KL 4                             
005200        05 MID-KDFORSKN-IN   PIC X(3).                                    
005300*                                 FÖRSÄKRANSKOD                           
005400        05 MID-KDSPFKTK-IN   PIC X.                                       
005500*                                                    KDSPFKTK-002         
005600*                                 INSTRUKTION SPECIALFAKTURA              
005700        05 MID-KDTULLVE-IN   PIC X.                                       
005800*                                 TYP AV PRIS PÅ TULLFAKTURA              
005900        05 MID-KDMOMSIN-IN   PIC X.                                       
006000*                                 MOMSINSTRUKTION                         
006100        05 MID-KDROPACK-DAG-IN                                            
006200                             PIC X.                                       
006300*                                 FRISLÄPPNINGSKOD RO/DO DAGORDER         
006400        05 MID-KDROPACK-BULK-IN                                           
006500                             PIC X.                                       
006600*                                 FRISLÄPPNINGSKOD RO/DO BULK ORD         
006700*** END OF VILMAII-COPY LENGTH= 75 BYTES                                  
