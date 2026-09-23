000100 01  MOD-W3O16701.                                                        
000200*                                 COPYTEXT FÖR MOD                        
000300*                                 W3O16701                                
000400     03 MOD-IDTRANS          PIC X(4).                                    
000500*                                 BILDNUMMER                              
000600     03 MOD-TEMFSFEL         PIC X(40).                                   
000700*                                 MFS FELMEDDELANDE                       
000800     03 MOD-IDFKNGRP-IN      PIC X(2).                                    
000900*                                 MFS BEHANDLING AV INPUTFÄLT             
001000     03 MOD-IDARTNR-IN       PIC X(2).                                    
001100*                                 MFS BEHANDLING AV INPUTFÄLT             
001200     03 MOD-TIAAPP-FOM-IN    PIC X(2).                                    
001300*                                 MFS BEHANDLING AV INPUTFÄLT             
001400     03 MOD-TIAAPP-TOM-IN    PIC X(2).                                    
001500*                                 MFS BEHANDLING AV INPUTFÄLT             
001600     03 MOD-IDFKNGRP-UT      PIC X(4).                                    
001700*                                 FUNKTIONSGRUPP                          
001800     03 MOD-IDARTNR-UT       PIC X(9).                                    
001900*                                 ARTIKELNUMMER                           
002000     03 MOD-TIAAPP-FOM-UT    PIC 9(4).                                    
002100*                                 ÅR - PLANERINGSPERIOD (ÅÅPP)            
002200*                                 12 PER ÅR                               
002300     03 MOD-TIAAPP-TOM-UT    PIC 9(4).                                    
002400*                                 ÅR - PLANERINGSPERIOD (ÅÅPP)            
002500*                                 12 PER ÅR                               
002600     03 MOD-OUTPUTDATA.                                                   
002700        05 MOD-BEART-CORE    PIC X(25).                                   
002800*                                 ENGELSK ARTIKELBENÄMNING                
002900        05 MOD-SUSKROT-DC    PIC Z(8)9.                                   
003000*                                 SUMMA SKROTAT ANTAL                     
003100*                                 AV 1 ARTIKEL                            
003200        05 MOD-SUSKROT-REM   PIC Z(8)9.                                   
003300*                                 SUMMA SKROTAT ANTAL                     
003400*                                 AV 1 ARTIKEL                            
003500        05 MOD-SUMOTT-CP     PIC Z(8)9.                                   
003600*                                 SUMMA MOTTAGET ANTAL                    
003700*                                 AV 1 ARTIKEL                            
003800        05 MOD-SUMOTT-REM    PIC Z(8)9.                                   
003900*                                 SUMMA MOTTAGET ANTAL                    
004000*                                 AV 1 ARTIKEL                            
004100        05 MOD-REPROCENT-DC  PIC Z(2)9.                                   
004200*                                 ALLMÄNT PROCENTTALSFÄLT                 
004300        05 MOD-REPROCENT-REM PIC Z(2)9.                                   
004400*                                 ALLMÄNT PROCENTTALSFÄLT                 
004500     03 MOD-TEMFSINF         PIC X(55).                                   
004600*                                 INFORMATIONSMEDDELANDE                  
004700*** END OF VILMAII-COPY LENGTH= 195 BYTES                                 
