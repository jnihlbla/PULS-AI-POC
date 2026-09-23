000100 01  MOD-W3O16901.                                                        
000200*                                 MOD-COPYTEXT FÖR BILD 3169              
000300*                                 EXCHANGE POINT INFO                     
000400     03 MOD-IDTRANS          PIC X(4).                                    
000500*                                 BILDNUMMER                              
000600     03 MOD-TEMFSFEL         PIC X(40).                                   
000700*                                 MFS FELMEDDELANDE                       
000800     03 MOD-IDDISTR-FOM-IN   PIC X(2).                                    
000900*                                 MFS BEHANDLING AV INPUTFÄLT             
001000     03 MOD-IDDISTR-TOM-IN   PIC X(2).                                    
001100*                                 MFS BEHANDLING AV INPUTFÄLT             
001200     03 MOD-TIAAVV-FOM-IN    PIC X(2).                                    
001300*                                 MFS BEHANDLING AV INPUTFÄLT             
001400     03 MOD-TIAAVV-TOM-IN    PIC X(2).                                    
001500*                                 MFS BEHANDLING AV INPUTFÄLT             
001600     03 MOD-IDDISTR-FOM-UT   PIC X(4).                                    
001700*                                 DISTRIKTNUMMER                          
001800     03 MOD-IDDISTR-TOM-UT   PIC X(4).                                    
001900*                                 DISTRIKTNUMMER                          
002000     03 MOD-TIAAVV-FOM-UT    PIC X(4).                                    
002100*                                 ÅR - VECKA  (ÅÅVV)                      
002200     03 MOD-TIAAVV-TOM-UT    PIC X(4).                                    
002300*                                 ÅR - VECKA  (ÅÅVV)                      
002400     03 MOD-DATA-UT          OCCURS 15 TIMES.                             
002500*                                 RAPPORTERINGS-FÄLT                      
002600        05 MOD-IDDISTR       PIC Z(3)9.                                   
002700*                                 DISTRIKTNUMMER                          
002800        05 MOD-DATA-UT       OCCURS 8 TIMES.                              
002900*                                 RAPPORTERINGS-FÄLT                      
003000           07 MOD-TIVV       PIC X(2).                                    
003100*                                 VECKA  (VV)                             
003200           07 MOD-KVPOINT    PIC Z(3)-.                                   
003300*                                 POINT VALUE                             
003400     03 MOD-TEMFSINF         PIC X(55).                                   
003500*                                 INFORMATIONSMEDDELANDE                  
003600*** END OF VILMAII-COPY LENGTH= 903 BYTES                                 
