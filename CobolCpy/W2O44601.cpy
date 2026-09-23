000100 01  MOD-W2O44601.                                                        
000200*                                 MOD-COPYTEXT FÖR W2044600               
000300     03 MOD-IDTRANS          PIC X(4).                                    
000400*                                 BILDNUMMER                              
000500     03 MOD-TEMFSFEL         PIC X(40).                                   
000600*                                 MFS FELMEDDELANDE                       
000700     03 MOD-DAPUBL-FOM-IN    PIC X(2).                                    
000800*                                 MFS BEHANDLING AV INPUTFÄLT             
000900     03 MOD-DAPUBL-FOM-UT    PIC X(5).                                    
001000*                                 ÅR - VECKA - DAG   (ÅÅVVD)              
001100     03 MOD-DAPUBL-TOM-IN    PIC X(2).                                    
001200*                                 MFS BEHANDLING AV INPUTFÄLT             
001300     03 MOD-DAPUBL-TOM-UT    PIC X(5).                                    
001400*                                 ÅR - VECKA - DAG   (ÅÅVVD)              
001500     03 MOD-IDPROJ-IN        PIC X(2).                                    
001600*                                 MFS BEHANDLING AV INPUTFÄLT             
001700     03 MOD-IDPROJ-UT        PIC X(4).                                    
001800*                                 PARTS PROJEKTIDENTITET                  
001900     03 MOD-IDDC-IN          PIC X(2).                                    
002000*                                 MFS BEHANDLING AV INPUTFÄLT             
002100     03 MOD-IDDC-UT          PIC X(2).                                    
002200*                                 IDENTIFIERARE LAGER                     
002300     03 MOD-IDLEVNR-DC-UT    PIC X(5).                                    
002400*                                 DC LEVERANTÖR                           
002500     03 MOD-INFO-RAD         OCCURS 12 TIMES.                             
002600*                                 RADINFORMATION                          
002700        05 MOD-IDARTNR       PIC Z(9).                                    
002800*                                 ARTIKELNUMMER                           
002900        05 MOD-IDPROJ        PIC X(4).                                    
003000*                                 PARTS PROJEKTIDENTITET                  
003100        05 MOD-KDEMBKOD-2    PIC Z(2)9.                                   
003200*                                 EMBALLAGEKOD 2                          
003300        05 MOD-DAPUBL        PIC X(5).                                    
003400*                                 ÅR - VECKA - DAG   (ÅÅVVD)              
003500        05 MOD-FLPISK        PIC X.                                       
003600*                                 PISK ARTIKEL                            
003700        05 MOD-TIREGDAT      PIC 9(6).                                    
003800*                                 REGISTRERINGSDATUM (ÅÅMMDD)             
003900        05 MOD-IDLEVNR-DC    PIC X(5).                                    
004000*                                 DC LEVERANTÖR                           
004100     03 MOD-TEMFSINF         PIC X(55).                                   
004200*                                 INFORMATIONSMEDDELANDE                  
004300*** END OF VILMAII-COPY LENGTH= 524 BYTES                                 
