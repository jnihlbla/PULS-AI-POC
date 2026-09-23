000100 01  MID-W4I31301.                                                        
000200*                                 MID-COPYTEXT FÖR W40313                 
000300     03 MID-IDDC-IN          PIC X(2).                                    
000400*                                 IDENTIFIERARE LAGER                     
000500     03 MID-IDDC-UT          PIC X(2).                                    
000600*                                 IDENTIFIERARE LAGER                     
000700     03 MID-IDANSTNR         PIC X(5).                                    
000800*                                 ANSTÄLLNINGSNUMMER                      
000900     03 MID-KDPRTVAL-ADR     PIC X(2).                                    
001000*                                 PRINTER-VAL KOD ADRESS FLAGGA           
001100     03 MID-KDPRTVAL-FS      PIC X(2).                                    
001200*                                 PRINTER-VAL KOD FÖLJESEDEL              
001300     03 MID-RAD              OCCURS 13 TIMES.                             
001400*                                 MID-COPYTEXT FÖR W40313                 
001500        05 MID-IDDISTR       PIC X(4).                                    
001600*                                 DISTRIKTNUMMER                          
001700        05 MID-IDPRODNR      PIC X(7).                                    
001800*                                 PRODUKTIONSNUMMER                       
001900        05 MID-IDPLKLST      PIC X(3).                                    
002000*                                 PLOCKLISTNUMMER                         
002100        05 MID-KOLLI-INFO.                                                
002200*                                 MID-COPYTEXT FÖR W40313                 
002300           07 MID-IDKOLLI    PIC X(5).                                    
002400*                                 KOLLINUMMER                             
002500           07 MID-KDKOLLI    PIC X(8).                                    
002600*                                 KOLLIKOD                                
002700           07 MID-VKORDBTO-KOLLI                                          
002800                             PIC X(8).                                    
002900*                                 ORDERVIKT BRUTTO PER KOLLI              
003000           07 MID-KDEMBTYP   PIC X.                                       
003100*                                 EMBALLAGETYP                            
003200           07 MID-DIKOLLIL   PIC X(4).                                    
003300*                                 KOLLI-LÄNGD                             
003400           07 MID-DIKOLLIB   PIC X(3).                                    
003500*                                 KOLLI-BREDD                             
003600           07 MID-DIKOLLIH   PIC X(3).                                    
003700*                                 KOLLI-HÖJD                              
003800           07 MID-KDPRTVAL-ADR-RAD                                        
003900                             PIC X(2).                                    
004000*                                 PRINTER-VAL KOD ADRESS FLAGGA           
004100           07 MID-KDPRTVAL-FS-RAD                                         
004200                             PIC X(2).                                    
004300*                                 PRINTER-VAL KOD FÖLJESEDEL              
004400           07 MID-FLAVSP     PIC X.                                       
004500*                                 ALLMÄN FELFLAGGA                        
004600*** END OF VILMAII-COPY LENGTH= 676 BYTES                                 
