000100 01  MOD-W3O16801.                                                        
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
002900        05 MOD-RECOST        PIC -Z(8)9.9(2).                             
003000*                                 SUMMABELOPP                             
003100        05 MOD-SUINVEST-DC   PIC Z(8)9.                                   
003200*                                 SUMMA INVESTERAT ANTAL                  
003300*                                 AV 1 ARTIKEL                            
003400        05 MOD-SUINVEST-REM  PIC Z(8)9.                                   
003500*                                 SUMMA INVESTERAT ANTAL                  
003600*                                 AV 1 ARTIKEL                            
003700        05 MOD-SUINVEST-TOT  PIC Z(8)9.                                   
003800*                                 SUMMA INVESTERAT ANTAL                  
003900*                                 AV 1 ARTIKEL                            
004000        05 MOD-SULEVANT-DC   PIC Z(8)9.                                   
004100*                                 SUMMA LEVERERAT ANTAL                   
004200*                                 AV 1 ARTIKEL                            
004300        05 MOD-REPROCENT     PIC -(3)9.                                   
004400*                                 ALLMÄNT PROCENTTALSFÄLT                 
004500        05 MOD-SUINVEST-CORE PIC Z(8)9.                                   
004600*                                 SUMMA INVESTERAT ANTAL                  
004700*                                 AV 1 ARTIKEL                            
004800     03 MOD-TEMFSINF         PIC X(55).                                   
004900*                                 INFORMATIONSMEDDELANDE                  
005000*** END OF VILMAII-COPY LENGTH= 215 BYTES                                 
