000100 01  MID-W4I39601.                                                        
000200*                                 MID-COPYTEXT FÖR W40396                 
000300     03 MID-IDPRODNR-IN      PIC X(7).                                    
000400*                                 PRODUKTIONSNUMMER                       
000500     03 MID-IDPRODNR-UT      PIC X(7).                                    
000600*                                 PRODUKTIONSNUMMER                       
000700     03 MID-IDDISTR-UT       PIC X(4).                                    
000800*                                 DISTRIKTNUMMER                          
000900     03 MID-IDKUNDNR-UT      PIC X(6).                                    
001000*                                 KUNDNUMMER                              
001100     03 MID-KDFRAKT-UT       PIC X(2).                                    
001200*                                 FRAKTSÄTT DC TILL KUND                  
001300     03 MID-IDORDNR-UT       PIC X(5).                                    
001400*                                 ORDERNUMMER UTGÅR PD90                  
001500     03 MID-KDORDKL-UT       PIC X.                                       
001600*                                 ORDERKLASS                              
001700     03 MID-IDDC-UT          PIC X(2).                                    
001800*                                 IDENTIFIERARE LAGER                     
001900     03 MID-PRTVAL-ADRESSFL  PIC X(2).                                    
002000*                                 PRINTER-VAL KOD                         
002100     03 MID-IDKOLLI-SENAST   PIC X(5).                                    
002200*                                 KOLLINUMMER                             
002300     03 MID-RAD              OCCURS 13 TIMES.                             
002400*                                 MID-COPYTEXT FÖR W40396                 
002500        05 MID-IDKOLLI       PIC 9(5).                                    
002600*                                 KOLLINUMMER                             
002700        05 MID-KDKOLLI       PIC X(8).                                    
002800*                                 KOLLIKOD                                
002900        05 MID-VKORDBTO-KOLLI                                             
003000                             PIC 9(7).                                    
003100*                                 ORDERVIKT BRUTTO PER KOLLI              
003200        05 MID-ADRUTHYL.                                                  
003300*                                 RUT/HYLL-ADRESS                         
003400           07 MID-ADFLOMR    PIC 9(3).                                    
003500*                                 LASTNINGSOMR/STÄLL FÄRDIGLAGER          
003600           07 MID-ADRUTNIV   PIC 9(3).                                    
003700*                                 RUTA/NIVÅ I FÄRDIGLAGRET                
003800        05 MID-ADFLGEO       PIC X(3).                                    
003900*                                 GEOGRAFISKT OMRÅDE FÄRDIGLAGER          
004000        05 MID-KDEMBTYP      PIC 9.                                       
004100*                                 EMBALLAGETYP                            
004200        05 MID-DIKOLLIL      PIC 9(4).                                    
004300*                                 KOLLI-LÄNGD                             
004400        05 MID-DIKOLLIB      PIC 9(3).                                    
004500*                                 KOLLI-BREDD                             
004600        05 MID-DIKOLLIH      PIC 9(3).                                    
004700*                                 KOLLI-HÖJD                              
004800*** END OF VILMAII-COPY LENGTH= 561 BYTES                                 
