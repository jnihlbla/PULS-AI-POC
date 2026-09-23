000100 01  MID-W4I30601.                                                        
000200*                                 MID-COPYTEXT FÖR W40306                 
000300     03 MID-KVORDRAD-SPAR    PIC 9(5).                                    
000400*                                 ANTAL ORDERRADER                        
000500     03 MID-IDPRODNR-SPAR    PIC 9(7).                                    
000600*                                 PRODUKTIONSNUMMER                       
000700     03 MID-RAPP-RAD.                                                     
000800*                                 MID-COPYTEXT FÖR W40306                 
000900        05 MID-IDDISTR       PIC 9(4).                                    
001000*                                 DISTRIKTNUMMER                          
001100        05 MID-IDPRODNR      PIC 9(7).                                    
001200*                                 PRODUKTIONSNUMMER                       
001300        05 MID-KVMANRAD      PIC 9(5).                                    
001400*                                 ANTAL MANUELLA ORDERRADER               
001500        05 MID-NIV1.                                                      
001600*                                 MID-COPYTEXT FÖR W40306                 
001700           07 MID-FLAVVPACK  PIC X.                                       
001800*                                 AVVIKELSE VID PACKNINGSRAPP?            
001900           07 MID-NIV2.                                                   
002000*                                 MID-COPYTEXT FÖR W40306                 
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
005000*** END COPY W4I30601C0  LENGTH=69                                        
