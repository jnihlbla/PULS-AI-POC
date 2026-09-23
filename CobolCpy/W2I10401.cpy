000100 01  MID-W2I10401.                                                        
000200*                                                                         
000300     03 MID-IDARTNR-IN       PIC X(9).                                    
000400*                                 ARTIKELNUMMER                           
000500     03 MID-IDARTNR-UT       PIC X(9).                                    
000600*                                 ARTIKELNUMMER                           
000700     03 MID-INPUT.                                                        
000800*                                 KVPB OCH FLMPB FÖRÄNDRING               
000900        05 MID-KVPB-C1-IN    PIC X(8).                                    
001000*                                 PERIODBEHOV (PROGNOS)                   
001100        05 MID-KVPB-C2-IN    PIC X(8).                                    
001200*                                 PERIODBEHOV (PROGNOS)                   
001300        05 MID-FLMPB-C1-IN   PIC X.                                       
001400*                                 MASKINELLT UPPDAT PERIODBEHOV ?         
001500        05 MID-FLOREGPB-IN   PIC X.                                       
001600*                                 OREGELBUNDEN PROGNOS (PB) ?             
001700        05 MID-TIPBLOCK-IN   PIC X(6).                                    
001800*                                 FRAMTIDA DATUM FÖR PB-LÅSNING Å         
001900*                                 ÅMMDD                                   
002000*** END OF VILMAII-COPY LENGTH= 42 BYTES                                  
