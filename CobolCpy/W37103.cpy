000100 01  W37119.                                                              
000200*                                 GODKÄNDA BYTESOBJEKT FRÅN BILD          
000300*                                 3171-3172                               
000400*                                 TILL TULL I BELGIEN.                    
000500     03 IDPTYP               PIC X(3).                                    
000600*                                 POSTTYP                                 
000700     03 IDFTG                PIC 9(2).                                    
000800*                                 FÖRETAGSID EKONOM REDOVISNING           
000900     03 IDDISTR              PIC S9(5)           COMP-3.                  
001000*                                 DISTRIKTNUMMER                          
001100     03 IDKUNDNR             PIC S9(7)           COMP-3.                  
001200*                                 KUNDNUMMER                              
001300     03 IDFAKT               PIC S9(7)           COMP-3.                  
001400*                                 FAKTURANUMMER                           
001500     03 TIREGDAT             PIC S9(7)           COMP-3.                  
001600*                                 REGISTRERINGSDATUM (ÅÅMMDD)             
001700     03 TIREGDAT-GODK        PIC S9(7)           COMP-3.                  
001800*                                 REGISTRERINGSDATUM (ÅÅMMDD)             
001900     03 IDARTNR-OBJ          PIC S9(9)           COMP-3.                  
002000*                                 OBJEKTNUMMER                            
002100     03 KVRETUR-GODK         PIC S9(5)           COMP-3.                  
002200*                                 ANTAL I RETUR                           
002300     03 KVRETUR-URSP         PIC S9(5)           COMP-3.                  
002400*                                 ANTAL I RETUR                           
002500*** END COPY W37103      LENGTH=35                                        
