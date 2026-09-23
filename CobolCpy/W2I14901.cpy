000100 01  MID-W2I14901.                                                        
000200*                                 COPYTEXT FÖR MID W2I14901               
000300     03 MID-IDLEVNR-SHIP-IN  PIC X(5).                                    
000400*                                 SKEPPANDE LEVERANTÖR                    
000500     03 MID-LINE-AREA        OCCURS 10 TIMES.                             
000600        05 MID-KDCMD-UPD     PIC X.                                       
000700         88 MID-KDCMD-INGENTING                                           
000800                             VALUE ' '.                                   
000900         88 MID-KDCMD-DELETE VALUE 'D'                                    
001000                             'B'.                                         
001100         88 MID-KDCMD-REPLACE                                             
001200                             VALUE 'R'                                    
001300                             'Ä'.                                         
001400         88 MID-KDCMD-INSERT VALUE 'I'                                    
001500                             'N'                                          
001600                             'A'.                                         
001700         88 MID-KDCMD-SELECT VALUE 'S'                                    
001800                             'V'.                                         
001900         88 MID-KDCMD-PRINT  VALUE 'P'                                    
002000                             'P'.                                         
002100         88 MID-KDCMD-COPY   VALUE 'C'                                    
002200                             'K'.                                         
002300*                                 RAD-UPPDATERINGSKOMMANDO                
002400*                                  BLANK  = INGENTING                     
002500*                                  D , B  = DELETE                        
002600*                                  R , Ä  = REPLACE                       
002700*                                  I,N,A  = INSERT                        
002800*                                  S , V  = SELECT                        
002900*                                  P , P  = PRINT                         
003000*                                  C , K  = COPY                          
003100        05 MID-IDLEVNR-SHIP  PIC X(5).                                    
003200*                                 SKEPPANDE LEVERANTÖR                    
003300        05 MID-IDANSK-FOM    PIC X(3).                                    
003400*                                 LÄGSTA ANSKAFFARNR I INTERVALL          
003500        05 MID-IDANSK-TOM    PIC X(3).                                    
003600*                                 HÖGSTA ANSKAFFARNR I INTERVALL          
003700        05 MID-DAAVROP-FOM   PIC X(4).                                    
003800*                                 AVSÄNDNINGSVECKA (PLANERAD)             
003900*                                 (ÅÅVV)                                  
004000        05 MID-DAAVROP-TOM   PIC X(4).                                    
004100*                                 AVSÄNDNINGSVECKA (PLANERAD)             
004200*                                 (ÅÅVV)                                  
004300     03 MID-UPD-AREA.                                                     
004400        05 MID-IDLEVNR-SHIP-UPD                                           
004500                             PIC X(5).                                    
004600*                                 SKEPPANDE LEVERANTÖR                    
004700        05 MID-IDANSK-FOM-UPD                                             
004800                             PIC X(3).                                    
004900*                                 LÄGSTA ANSKAFFARNR I INTERVALL          
005000        05 MID-IDANSK-TOM-UPD                                             
005100                             PIC X(3).                                    
005200*                                 HÖGSTA ANSKAFFARNR I INTERVALL          
005300        05 MID-DAAVROP-FOM-UPD                                            
005400                             PIC X(4).                                    
005500*                                 AVSÄNDNINGSVECKA (PLANERAD)             
005600*                                 (ÅÅVV)                                  
005700        05 MID-DAAVROP-TOM-UPD                                            
005800                             PIC X(4).                                    
005900*                                 AVSÄNDNINGSVECKA (PLANERAD)             
006000*                                 (ÅÅVV)                                  
006100        05 MID-DAAVROP-TFOM-UPD                                           
006200                             PIC X(4).                                    
006300*                                 AVSÄNDNINGSVECKA (PLANERAD)             
006400*                                 (ÅÅVV)                                  
006500*** END OF VILMAII-COPY LENGTH= 228 BYTES                                 
