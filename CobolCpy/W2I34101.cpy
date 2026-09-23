000100 01  MID-W2I34101.                                                        
000200*                                 MID-COPYTEXT FÖR W2034100               
000300     03 MID-IDARTNR-IN       PIC X(9).                                    
000400*                                 ARTIKELNUMMER                           
000500     03 MID-IDARTNR-UT       PIC X(9).                                    
000600*                                 ARTIKELNUMMER                           
000700     03 MID-IDDC-IN          PIC X(2).                                    
000800*                                 IDENTIFIERARE LAGER                     
000900     03 MID-IDDC-UT          PIC X(2).                                    
001000*                                 IDENTIFIERARE LAGER                     
001100     03 MID-INPUT.                                                        
001200*                                 UPPDATERINGSFÄLT                        
001300        05 MID-IDREFTAB      PIC X.                                       
001400*                                 IDENTITET REFILLTABELL                  
001500        05 MID-FLWILSON      PIC X.                                       
001600*                                 WILSONFORMEL                            
001700        05 MID-KVREFPKT      PIC X(7).                                    
001800*                                 BERÄKNAD PÅFYLLNADSPUNKT                
001900        05 MID-TIREFPKT      PIC X(6).                                    
002000*                                 DATUM MANUELL REFILLPUNKT               
002100        05 MID-FLREFILL      PIC X.                                       
002200*                                 REFILLARTIKEL                           
002300        05 MID-KVREFBER      PIC X(7).                                    
002400*                                 BERÄKNAD REFILLINGKVANTITET             
002500        05 MID-FLREFBEO      PIC X.                                       
002600*                                 AUTOMATISK REFILL BEORDRING?            
002700        05 MID-TIREFPAF      PIC X(6).                                    
002800*                                 DATUM MANUELL PÅFYLLNADSKVANT           
002900        05 MID-TIREFSTO      PIC X(6).                                    
003000*                                 BEORDRINGSSTOPPAD T.OM.                 
003100        05 MID-IDPERSON-BUY  PIC X(3).                                    
003200*                                 PERSONKOD REFILLANSVARIG                
003300        05 MID-FLBUYUPD      PIC X.                                       
003400*                                 OM IDPERSONKOD ÄR LÅST                  
003500        05 MID-FLTABUPD      PIC X.                                       
003600*                                 OM REFILLTABELL ÄR LÅST                 
003700        05 MID-FLPB-FLYTT    PIC X.                                       
003800*                                 FLAGGA VID ERSÄTTNING FÖR HÅLLA         
003900*                                  REDA PÅ KOPIERING AV PROGNOS           
004000        05 MID-FLREFNYO      PIC X.                                       
004100*                                 AVVAKTA TILLS NY EFTERFRÅGAN            
004200        05 MID-TEREFMED      PIC X(72).                                   
004300        05 MID-PBJUST-1.                                                  
004400*                                                                         
004500           07 MID-KVPB-JUST-1                                             
004600                             PIC X(8).                                    
004700*                                 PERIODBEHOVSJUSTERING                   
004800           07 MID-TIPBJUST-1 PIC X(4).                                    
004900*                                 DATUM FÖR PB-JUSTERING (ÅÅVV)           
005000        05 MID-PBJUST-2.                                                  
005100*                                                                         
005200           07 MID-KVPB-JUST-2                                             
005300                             PIC X(8).                                    
005400*                                 PERIODBEHOVSJUSTERING                   
005500           07 MID-TIPBJUST-2 PIC X(4).                                    
005600*                                 DATUM FÖR PB-JUSTERING (ÅÅVV)           
005700*** END OF VILMAII-COPY LENGTH= 161 BYTES                                 
