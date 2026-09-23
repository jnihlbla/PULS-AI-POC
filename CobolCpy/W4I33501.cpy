000100 01  MID-W4I33501.                                                        
000200*                                 MID-COPYTEXT FÖR W4033500               
000300     03 MID-IDDISTR-IN       PIC X(4).                                    
000400*                                 DISTRIKTNUMMER                          
000500     03 MID-IDDISTR-UT       PIC X(4).                                    
000600*                                 DISTRIKTNUMMER                          
000700     03 MID-IDKUNDNR-IN      PIC X(6).                                    
000800*                                 KUNDNUMMER                              
000900     03 MID-IDKUNDNR-UT      PIC X(6).                                    
001000*                                 KUNDNUMMER                              
001100     03 MID-IDORDNR-IN       PIC X(5).                                    
001200*                                 ORDERNUMMER UTGÅR PD90                  
001300     03 MID-IDORDNR-UT       PIC X(5).                                    
001400*                                 ORDERNUMMER UTGÅR PD90                  
001500     03 MID-IDKOLLI-IN       PIC X(5).                                    
001600*                                 KOLLINUMMER                             
001700     03 MID-IDKOLLI-UT       PIC X(5).                                    
001800*                                 KOLLINUMMER                             
001900     03 MID-IDDC-IN          PIC X(2).                                    
002000*                                 IDENTIFIERARE LAGER                     
002100     03 MID-IDDC-UT          PIC X(2).                                    
002200*                                 IDENTIFIERARE LAGER                     
002300     03 MID-KDPRTVAL-IN      PIC X(2).                                    
002400*                                 PRINTER-VAL KOD                         
002500     03 MID-KDPRTVAL-UT      PIC X(2).                                    
002600*                                 PRINTER-VAL KOD                         
002700     03 MID-KDKOLLI-BAER     PIC X(8).                                    
002800*                                 KOLLIKOD                                
002900     03 MID-KDEMBTYP-BAER    PIC X.                                       
003000*                                 EMBALLAGETYP                            
003100     03 MID-DIKOLLIL-BAER    PIC X(4).                                    
003200*                                 KOLLI-LÄNGD                             
003300     03 MID-DIKOLLIB-BAER    PIC X(3).                                    
003400*                                 KOLLI-BREDD                             
003500     03 MID-DIKOLLIH-BAER    PIC X(3).                                    
003600*                                 KOLLI-HÖJD                              
003700     03 MID-W4I33501-001     OCCURS 10 TIMES.                             
003800*                                 MID-COPYTEXT FÖR W4033500               
003900        05 MID-IDKOLLI-RAD   PIC X(5).                                    
004000*                                 KOLLINUMMER                             
004100        05 MID-VKORDBTO-RAD  PIC X(8).                                    
004200*                                 ORDERVIKT BRUTTO (KG)                   
004300        05 MID-KDKOLLI-RAD   PIC X(8).                                    
004400*                                 KOLLIKOD                                
004500        05 MID-KDEMBTYP-RAD  PIC X.                                       
004600*                                 EMBALLAGETYP                            
004700        05 MID-DIKOLLIL-RAD  PIC X(4).                                    
004800*                                 KOLLI-LÄNGD                             
004900        05 MID-DIKOLLIB-RAD  PIC X(3).                                    
005000*                                 KOLLI-BREDD                             
005100        05 MID-DIKOLLIH-RAD  PIC X(3).                                    
005200*                                 KOLLI-HÖJD                              
005300*** END OF VILMAII-COPY LENGTH= 387 BYTES                                 
