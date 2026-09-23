000100 01  W27136.                                                              
000200*                                 COPYTEXT FÖR FIL MED ARTIKLAR           
000300*                                 SOM SKALL UPPDATERAS I PGM              
000400*                                 W27136                                  
000500     03 IDARTNR              PIC S9(9)           COMP-3.                  
000600*                                 ARTIKELNUMMER                           
000700     03 IDDC                 PIC X(2).                                    
000800*                                 IDENTIFIERARE LAGER                     
000900     03 IDPERSON-BUY         PIC S9(3)           COMP-3.                  
001000*                                 PERSONKOD REFILLANSVARIG                
001100     03 KDERS                PIC S9(3)           COMP-3.                  
001200*                                 ERSÄTTNINGSKOD                          
001300     03 KDREFSTA             PIC X.                                       
001400*                                 STATUS REFILLARTIKEL                    
001500     03 TIREFSTO-CLAG        PIC S9(7)           COMP-3.                  
001600*                                 BEORDRINGSSTOPPAD T.OM.                 
001700     03 TIREFSTO-SLAG        PIC S9(7)           COMP-3.                  
001800*                                 BEORDRINGSSTOPPAD T.OM.                 
001900     03 FLPB-FLYTT           PIC X.                                       
002000*                                 FLAGGA VID ERSÄTTNING FÖR HÅLLA         
002100*                                  REDA PÅ KOPIERING AV PROGNOS           
002200*** END OF VILMAII-COPY LENGTH= 21 BYTES                                  
