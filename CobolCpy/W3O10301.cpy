000100 01  MOD-W3O10301.                                                        
000200*                                 COPYTEXT FÖR MOD W3O10301               
000300     03 MOD-IDTRANS          PIC X(4).                                    
000400*                                 BILDNUMMER                              
000500     03 MOD-TEMFSFEL         PIC X(40).                                   
000600*                                 MFS FELMEDDELANDE                       
000700     03 MOD-IDARTNR-IN       PIC X(9).                                    
000800*                                 ARTIKELNUMMER                           
000900     03 MOD-IDARTNR-UT       PIC X(9).                                    
001000*                                 ARTIKELNUMMER                           
001100     03 MOD-IDLANDX2-IN      PIC X(2).                                    
001200*                                 2-STÄLLIG LANDSBETECKNINGSKOD           
001300     03 MOD-IDLANDX2-UT      PIC X(2).                                    
001400*                                 2-STÄLLIG LANDSBETECKNINGSKOD           
001500     03 MOD-BEART            PIC X(25).                                   
001600*                                 ARTIKELBENÄMNING                        
001700     03 MOD-TIUPPDAT         PIC 9(6).                                    
001800*                                 UPPDATERINGSDATUM  (ÅÅMMDD)             
001900     03 MOD-RAD              OCCURS 13 TIMES                              
002000                             PIC X(79).                                   
002100     03 MOD-TEMFSINF         PIC X(55).                                   
002200*                                 INFORMATIONSMEDDELANDE                  
002300*** END OF VILMAII-COPY LENGTH= 1179 BYTES                                
