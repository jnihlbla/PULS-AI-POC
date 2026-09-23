000100 01  MID-W6I20401.                                                        
000200*                                 COPYTEXT FÖR MID W6I20401               
000300*                                                                         
000400     03 MID-IDKR-IN          PIC X(5).                                    
000500*                                 KONTROLLRAPPORT NUMMER                  
000600     03 MID-IDKR-UT          PIC X(5).                                    
000700*                                 KONTROLLRAPPORT NUMMER                  
000800     03 MID-IDKOLLINR-IN     PIC X(5).                                    
000900*                                 KOLLINUMMER                             
001000     03 MID-IDKOLLINR-UT     PIC X(5).                                    
001100*                                 KOLLINUMMER                             
001200     03 MID-IDKOLLI-ENTER    PIC X(5).                                    
001300*                                 KOLLINUMMER                             
001400     03 MID-IDKOLLI-NEXT     PIC X(5).                                    
001500*                                 KOLLINUMMER                             
001600     03 MID-INPUT.                                                        
001700*                                                                         
001800        05 MID-KDKOLLI       PIC X(8).                                    
001900*                                 KOLLIKOD                                
002000        05 MID-IDKOLLI-IN    PIC 9(5).                                    
002100*                                 KOLLINUMMER                             
002200        05 MID-VKKOLLIB-IN   PIC X(7).                                    
002300*                                 KOLLI-VIKT-BRUTTO                       
002400        05 MID-DIKOLLIL-IN   PIC 9(4).                                    
002500*                                 KOLLI-LÄNGD                             
002600        05 MID-DIKOLLIB-IN   PIC 9(3).                                    
002700*                                 KOLLI-BREDD                             
002800        05 MID-DIKOLLIH-IN   PIC 9(3).                                    
002900*                                 KOLLI-HÖJD                              
003000        05 MID-KDCMD-IN      PIC X.                                       
003100         88 MID-KDCMD-INGENTING                                           
003200                             VALUE ' '.                                   
003300         88 MID-KDCMD-DELETE VALUE 'D'                                    
003400                             'B'.                                         
003500         88 MID-KDCMD-REPLACE                                             
003600                             VALUE 'R'                                    
003700                             'Ä'.                                         
003800         88 MID-KDCMD-INSERT VALUE 'I'                                    
003900                             'N'.                                         
004000*                                 RAD-UPPDATERINGSKOMMANDO                
004100        05 MID-KDPERSON      PIC 9(3).                                    
004200*                                 PERSONKOD                               
004300        05 MID-BEKRPACK      PIC X(25).                                   
004400*                                 ANSVARIG FÖR PACKNING                   
004500*                                                                         
004600        05 MID-KVKRPACK-IN   PIC X(4).                                    
004700*                                 PACKNINGSTID                            
004800*** END COPY W6I20401    LENGTH=93                                        
