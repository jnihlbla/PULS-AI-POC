000100 01  MID-W4I74201.                                                        
000200*                                 MID-COPYTEXT FÖR W40742                 
000300     03 MID-IDKOLLI-IN       PIC 9(5).                                    
000400*                                 KOLLINUMMER                             
000500     03 MID-IDKOLLI-UT       PIC 9(5).                                    
000600*                                 KOLLINUMMER                             
000700     03 MID-FLVISA-IN        PIC X.                                       
000800*                                 ALLMÄN FLAGGA                           
000900     03 MID-FLVISA-UT        PIC X.                                       
001000*                                 ALLMÄN FLAGGA                           
001100     03 MID-INPUT.                                                        
001200*                                 INMATNINGSFÄLT                          
001300        05 MID-FLNYKLI       PIC X.                                       
001400*                                 ALLMÄN FLAGGA                           
001500        05 MID-KVKOLLI       PIC X(4).                                    
001600*                                 ANTAL KOLLI                             
001700        05 MID-INPUTLINE     OCCURS 10 TIMES.                             
001800*                                 INMATNINGSFÄLT PÅ RADEN                 
001900           07 MID-KDCMD      PIC X.                                       
002000            88 MID-KDCMD-INGENTING                                        
002100                             VALUE ' '.                                   
002200            88 MID-KDCMD-DELETE                                           
002300                             VALUE 'D'                                    
002400                             'B'.                                         
002500            88 MID-KDCMD-REPLACE                                          
002600                             VALUE 'R'                                    
002700                             'Ä'.                                         
002800            88 MID-KDCMD-INSERT                                           
002900                             VALUE 'I'                                    
003000                             'N'.                                         
003100*                                 RAD-UPPDATERINGSKOMMANDO                
003200        05 MID-IDDISTR-IN    PIC X(4).                                    
003300*                                 DISTRIKTNUMMER                          
003400        05 MID-IDKUNDNR-IN   PIC X(6).                                    
003500*                                 KUNDNUMMER                              
003600        05 MID-IDRAPPNR-IN   PIC X(7).                                    
003700*                                 RAPPORT NUMMER                          
003800     03 MID-KEYFIELD.                                                     
003900*                                 NYCKELFÄLT I HUVUDET                    
004000        05 MID-IDKOLLI-FOM   PIC X(5).                                    
004100*                                 KOLLINUMMER                             
004200        05 MID-IDKOLLI-TOM   PIC X(5).                                    
004300*                                 KOLLINUMMER                             
004400        05 MID-KEYFIELD      OCCURS 10 TIMES.                             
004500*                                 NYCKELFÄLT PÅ RADEN                     
004600           07 MID-IDDISTR    PIC X(4).                                    
004700*                                 DISTRIKTNUMMER                          
004800           07 MID-IDKUNDNR   PIC X(6).                                    
004900*                                 KUNDNUMMER                              
005000           07 MID-IDRAPPNR   PIC X(7).                                    
005100*                                 RAPPORT NUMMER                          
005200     03 MID-MODFAELT-IN      PIC X(100).                                  
005300*** END OF VILMAII-COPY LENGTH= 324 BYTES                                 
