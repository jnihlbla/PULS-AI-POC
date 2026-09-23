000100 01  W27236.                                                              
000200*                                 COPYTEXT FÖR FIL MED ARTIKLAR           
000300*                                 SOM SKALL UPPDATERAS I PGM              
000400*                                 W27237 ERSÄTTNINGAR                     
000500     03 IDARTNR              PIC S9(9)           COMP-3.                  
000600*                                 ARTIKELNUMMER                           
000700     03 FLTILLK              PIC X.                                       
000800*                                 TILLKOMMANDE ARTIKEL ?                  
000900     03 IDPERSON-BUY         PIC S9(3)           COMP-3.                  
001000*                                 PERSONKOD REFILLANSVARIG                
001100     03 FLWILSON             PIC X.                                       
001200*                                 WILSONFORMEL                            
001300     03 KDREFSTA             PIC X.                                       
001400*                                 STATUS REFILLARTIKEL                    
001500     03 TIREFSTO             PIC S9(7)           COMP-3.                  
001600*                                 BEORDRINGSSTOPPAD T.OM.                 
001700     03 FLPB-FLYTT           PIC X.                                       
001800*                                 FLAGGA VID ERSÄTTNING FÖR HÅLLA         
001900*                                  REDA PÅ KOPIERING AV PROGNOS           
002000     03 KVPB-SEP             PIC S9(6)V9(1)      COMP-3.                  
002100*                                 SEPARAT PERIODBEHOV                     
002200     03 FLREFILL             PIC X.                                       
002300*                                 REFILLARTIKEL                           
002400     03 TIPBDAT              PIC S9(5)           COMP-3.                  
002500*                                 DATUM SENASTE PB-ÄNDRING  ÅÅVVD         
002600     03 FLREFBEO             PIC X.                                       
002700*                                 AUTOMATISK REFILL BEORDRING?            
002800     03 DAGENS-DATUM         PIC 9(6).                                    
002900     03 FLFLYG               PIC X.                                       
003000*                                 FLYGARTIKEL                             
003100*** END OF VILMAII-COPY LENGTH= 31 BYTES                                  
