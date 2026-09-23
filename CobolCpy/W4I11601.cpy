000100 01  MID-W4I11601.                                                        
000200*                                 COPYTEXT FÖR MID W4I11601               
000300*                                                                         
000400     03 MID-IDKRFELI         PIC X(2).                                    
000500*                                 FELKOD FÖR KONTROLLRAPPORT              
000600     03 MID-IDKRFELU         PIC X(2).                                    
000700*                                 FELKOD FÖR KONTROLLRAPPORT              
000800     03 MID-IDKRFEL-NX       PIC X(2).                                    
000900*                                 FELKOD FÖR KONTROLLRAPPORT              
001000     03 MID-IDKRFEL-EN       PIC X(2).                                    
001100*                                 FELKOD FÖR KONTROLLRAPPORT              
001200     03 MID-INPUT.                                                        
001300*                                 COPYTEXT FOR MID W4O11601 ENDAS         
001400*                                 T INDATA-FÄLT                           
001500        05 MID-IDKRFEL-IN    PIC X(2).                                    
001600*                                 FELKOD FÖR KONTROLLRAPPORT              
001700        05 MID-BEKRFEL-IN    PIC X(63).                                   
001800        05 MID-KDCMD-IN      PIC X.                                       
001900         88 MID-KDCMD-INGENTING                                           
002000                             VALUE ' '.                                   
002100         88 MID-KDCMD-DELETE VALUE 'D'                                    
002200                             'B'.                                         
002300         88 MID-KDCMD-REPLACE                                             
002400                             VALUE 'R'                                    
002500                             'Ä'.                                         
002600         88 MID-KDCMD-INSERT VALUE 'I'                                    
002700                             'N'.                                         
002800*                                 RAD-UPPDATERINGSKOMMANDO                
002900*                                  BLANK  = INGENTING                     
003000*                                  D , B  = DELETE                        
003100*                                  R , Ä  = REPLACE                       
003200*                                  I , N  = INSERT                        
003300*** END OF VILMAII-COPY LENGTH= 74 BYTES                                  
