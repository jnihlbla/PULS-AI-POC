000100 01  MID-W4I30201.                                                        
000200*                                 MID-COPYTEXT FÖR W40302                 
000300     03 MID-IDDC-IN          PIC X(2).                                    
000400*                                 IDENTIFIERARE LAGER                     
000500     03 MID-IDDC-UT          PIC X(2).                                    
000600*                                 IDENTIFIERARE LAGER                     
000700     03 MID-PRTVAL-ADRESSFL  PIC X(2).                                    
000800*                                 PRINTER-VAL KOD                         
000900     03 MID-RAD              OCCURS 13 TIMES.                             
001000*                                 MID-COPYTEXT FÖR W40302                 
001100        05 MID-IDDISTR       PIC 9(4).                                    
001200*                                 DISTRIKTNUMMER                          
001300        05 MID-IDPRODNR      PIC 9(7).                                    
001400*                                 PRODUKTIONSNUMMER                       
001500        05 MID-NIV1.                                                      
001600*                                 MID-COPYTEXT FÖR W40302                 
001700           07 MID-FLAVVPACK  PIC X.                                       
001800*                                 AVVIKELSE VID PACKNINGSRAPP?            
001900           07 MID-NIV2.                                                   
002000*                                 MID-COPYTEXT FÖR W40302                 
002100              09 MID-IDKOLLI PIC 9(5).                                    
002200*                                 KOLLINUMMER                             
002300              09 MID-KDKOLLI PIC X(8).                                    
002400*                                 KOLLIKOD                                
002500              09 MID-VKORDBTO-KOLLI                                       
002600                             PIC 9(7).                                    
002700*                                 ORDERVIKT BRUTTO PER KOLLI              
002800              09 MID-ADRUTHYL.                                            
002900*                                 RUT/HYLL-ADRESS                         
003000                 11 MID-ADFLOMR                                           
003100                             PIC 9(3).                                    
003200*                                 LASTNINGSOMR/STÄLL FÄRDIGLAGER          
003300                 11 MID-ADRUTNIV                                          
003400                             PIC 9(3).                                    
003500*                                 RUTA/NIVÅ I FÄRDIGLAGRET                
003600              09 MID-ADFLGEO PIC X(3).                                    
003700*                                 GEOGRAFISKT OMRÅDE FÄRDIGLAGER          
003800              09 MID-KDEMBTYP                                             
003900                             PIC 9.                                       
004000*                                 EMBALLAGETYP                            
004100              09 MID-DIKOLLIL                                             
004200                             PIC 9(4).                                    
004300*                                 KOLLI-LÄNGD                             
004400              09 MID-DIKOLLIB                                             
004500                             PIC 9(3).                                    
004600*                                 KOLLI-BREDD                             
004700              09 MID-DIKOLLIH                                             
004800                             PIC 9(3).                                    
004900*                                 KOLLI-HÖJD                              
005000*** END OF VILMAII-COPY LENGTH= 682 BYTES                                 
