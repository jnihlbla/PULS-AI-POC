000010 01  MOD-W1O11101.                                                        
000020*                                 MOD COPYTEXT FÖR W1011100               
000030     03 MOD-IDTRANS          PIC X(4).                                    
000040*                                 BILDNUMMER                              
000050     03 MOD-TEMFSFEL         PIC X(40).                                   
000060*                                 MFS FELMEDDELANDE                       
000070     03 MOD-IDARTNR-IN       PIC X(2).                                    
000080*                                 MFS BEHANDLING AV INPUTFÄLT             
000090     03 MOD-IDARTNR-UT       PIC X(9).                                    
000100*                                 ARTIKELNUMMER                           
000110     03 MOD-BEART            PIC X(25).                                   
000120*                                 ARTIKELBENÄMNING                        
000130     03 MOD-KDPRODSL         PIC Z9.                                      
000140*                                 PRODUKTSLAG                             
000150     03 MOD-IDARTNR-MOTSV    PIC Z(9).                                    
000160*                                 MOTSVARANDE ARTIKEL                     
000170     03 MOD-IDFKNGRP         PIC Z(3)9.                                   
000180*                                 FUNKTIONSGRUPP                          
000190     03 MOD-STATNR-UT-GRP    OCCURS 5 TIMES.                              
000200        05 MOD-IDSTATNR-UT-ATTR                                           
000210                             PIC X(2).                                    
000220*                                 MFS ATTRIBUTFÄLT                        
000230        05 MOD-IDSTATNR-UT   PIC Z(8)9.                                   
000240*                                 STATISTISKT NUMMER                      
000250*                                 1 = NORSKT                              
000260*                                 2 = ENGELSKT                            
000270*                                 3 = BELGISKT                            
000280*                                 4 = PERUANSKT                           
000290*                                 5 = SVENSKT                             
000300*                                 6 =                                     
000310     03 MOD-STATNR-IN-GRP    OCCURS 5 TIMES.                              
000320        05 MOD-IDSTATNR-IN-ATTR                                           
000330                             PIC X(2).                                    
000340*                                 MFS ATTRIBUTFÄLT                        
000350        05 MOD-IDSTATNR-IN   PIC X(2).                                    
000360*                                 MFS BEHANDLING AV INPUTFÄLT             
000370     03 MOD-TEMFSINF         PIC X(55).                                   
000380*                                 INFORMATIONSMEDDELANDE                  
      *** END COPY W1O11101    LENGTH=225                                       
