000010 01  MOD-W6O17101.                                                        
000020*                                 MOD-COPYTEXT FÖR W6017100               
000030     03 MOD-IDTRANS          PIC X(4).                                    
000040*                                 BILDNUMMER                              
000050     03 MOD-TEMFSFEL         PIC X(40).                                   
000060*                                 MFS FELMEDDELANDE                       
000070     03 MOD-TABELLRAD        OCCURS 14 TIMES.                             
000080*                                 GRUPP MED TABELLRADER                   
000090        05 MOD-KDCMDVAL-ATTR PIC X(2).                                    
000100*                                 MFS ATTRIBUTFÄLT                        
000110        05 MOD-KDCMDVAL      PIC X(3).                                    
000120*                                 GENERELL KOMMANDOKOD                    
000130        05 MOD-IDARTNR-ATTR  PIC X(2).                                    
000140*                                 MFS ATTRIBUTFÄLT                        
000150        05 MOD-IDARTNR       PIC X(8).                                    
000160*                                 ARTIKELNUMMER                           
000170        05 MOD-BEART-ATTR    PIC X(2).                                    
000180*                                 MFS ATTRIBUTFÄLT                        
000190        05 MOD-BEART         PIC X(20).                                   
000200        05 MOD-KVBUFF-F-ATTR PIC X(2).                                    
000210*                                 MFS ATTRIBUTFÄLT                        
000220        05 MOD-KVBUFF-F      PIC Z(6)9.                                   
000230*                                 FÖRÄDLAT BUFFERSALDO                    
000240        05 MOD-ADBUFPPL-ATTR PIC X(2).                                    
000250*                                 MFS ATTRIBUTFÄLT                        
000260        05 MOD-ADBUFPPL      PIC 9(2).                                    
000270*                                 PALLPLATSNUMMER I BUFFERT               
000280        05 MOD-ADBUFFOMR-ATTR                                             
000290                             PIC X(2).                                    
000300*                                 MFS ATTRIBUTFÄLT                        
000310        05 MOD-ADBUFFOMR     PIC Z9.                                      
000320*                                 BUFFERTOMRÅDE                           
000330        05 MOD-ADBUFFGANG-ATTR                                            
000340                             PIC X(2).                                    
000350*                                 MFS ATTRIBUTFÄLT                        
000360        05 MOD-ADBUFFGANG    PIC Z9.                                      
000370*                                 BUFFERT GÅNG                            
000380        05 MOD-ADBUFFPL-ATTR PIC X(2).                                    
000390*                                 MFS ATTRIBUTFÄLT                        
000400        05 MOD-ADBUFFPL      PIC Z(4)9.                                   
000410*                                 BUFFERPLATSNUMMER                       
000420        05 MOD-TEMFSMED-ATTR PIC X(2).                                    
000430*                                 MFS ATTRIBUTFÄLT                        
000440        05 MOD-TEMFSMED      PIC X(20).                                   
000450*                                 INFO-MEDDELANDE FÖR FÄLT/RAD            
000460     03 MOD-TEMFSINF         PIC X(55).                                   
000470*                                 INFORMATIONSMEDDELANDE                  
      *** END COPY W6O17101    LENGTH=1317                                      
