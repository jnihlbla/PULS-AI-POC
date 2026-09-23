000100 01  MOD-W3O16401.                                                        
000200*                                 COPYTEXT FÖR MOD                        
000300*                                 W3O16401                                
000400     03 MOD-IDTRANS          PIC X(4).                                    
000500*                                 BILDNUMMER                              
000600     03 MOD-TEMFSFEL         PIC X(40).                                   
000700*                                 MFS FELMEDDELANDE                       
000800     03 MOD-IDARTNR-IN       PIC X(2).                                    
000900*                                 MFS BEHANDLING AV INPUTFÄLT             
001000     03 MOD-IDARTNR-UT       PIC X(8).                                    
001100*                                 ARTIKELNUMMER                           
001200     03 MOD-BEART-CORE       PIC X(25).                                   
001300*                                 ENGELSK ARTIKELBENÄMNING                
001400     03 MOD-OUTPUTDATA       OCCURS 13 TIMES.                             
001500        05 MOD-IDDISTR       PIC Z(3)9.                                   
001600*                                 DISTRIKTNUMMER                          
001700        05 MOD-DAREGDAT      PIC 9(8).                                    
001800*                                 REGISTRERINGSDATUM (ÅÅÅÅMMDD)           
001900        05 MOD-KVANTAL       PIC Z(6)9.                                   
002000*                                 ANTAL                                   
002100        05 MOD-DAANKDAG      PIC 9(8).                                    
002200*                                 ANKOMSTDAG                              
002300        05 MOD-FLANK         PIC X.                                       
002400*                                 ALLMÄN FLAGGA                           
002500     03 MOD-TEMFSINF         PIC X(55).                                   
002600*                                 INFORMATIONSMEDDELANDE                  
002700*** END OF VILMAII-COPY LENGTH= 498 BYTES                                 
