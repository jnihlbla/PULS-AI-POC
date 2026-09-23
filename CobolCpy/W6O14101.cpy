000010 01  MOD-W6O14101.                                                        
000020*                                 MOD-COPYTEXT FÖR W6014100               
000030     03 MOD-IDTRANS          PIC X(4).                                    
000040*                                 BILDNUMMER                              
000050     03 MOD-TEMFSFEL         PIC X(40).                                   
000060*                                 MFS FELMEDDELANDE                       
000070     03 MOD-IDINLVGN-ATTR    PIC X(2).                                    
000080*                                 MFS ATTRIBUTFÄLT                        
000090     03 MOD-IDINLVGN         PIC X(3).                                    
000100*                                 VAGNSIDENTITET                          
000110     03 MOD-ADINLOMR-ATTR    PIC X(2).                                    
000120*                                 MFS ATTRIBUTFÄLT                        
000130     03 MOD-ADINLOMR         PIC X(4).                                    
000140*                                 INLEVERANSOMRÅDE                        
000150     03 MOD-RADER            OCCURS 12 TIMES.                             
000160*                                 GRP FÖR MOD W6O14101                    
000170        05 MOD-IDLEVNR-KOLLI-ATTR                                         
000180                             PIC X(2).                                    
000190*                                 MFS ATTRIBUTFÄLT                        
000200        05 MOD-IDLEVNR-KOLLI PIC X(5).                                    
000210*                                 LEVERANTÖRNUMMER KOLLI                  
000220        05 MOD-IDOKOLLI-ATTR PIC X(2).                                    
000230*                                 MFS ATTRIBUTFÄLT                        
000240        05 MOD-IDOKOLLI      PIC X(9).                                    
000250*                                 ODETTE KOLLINUMMER                      
000260        05 MOD-TEMFSMED-ATTR PIC X(2).                                    
000270*                                 MFS ATTRIBUTFÄLT                        
000280        05 MOD-TEMFSMED      PIC X(20).                                   
000290*                                 INFO-MEDDELANDE FÖR FÄLT/RAD            
000300     03 MOD-ADINLOMR-PRT-IN-ATTR                                          
000310                             PIC X(2).                                    
000320*                                 MFS ATTRIBUTFÄLT                        
000330     03 MOD-ADINLOMR-PRT-IN  PIC X(4).                                    
000340*                                 PRINTERPLACERING                        
000350     03 MOD-ADINLOMR-PRT-UT-ATTR                                          
000360                             PIC X(2).                                    
000370*                                 MFS ATTRIBUTFÄLT                        
000380     03 MOD-ADINLOMR-PRT-UT  PIC X(4).                                    
000390*                                 PRINTERPLACERING                        
000400     03 MOD-TEMFSINF         PIC X(61).                                   
000410*                                 INFORMATIONSMEDDELANDE                  
      *** END COPY W6O14101    LENGTH=608                                       
