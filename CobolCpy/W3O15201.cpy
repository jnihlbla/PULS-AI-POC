000010 01  MOD-W3O15201.                                                        
000020*                                 MOD-COPYTEXT FÖR W3015200               
000030     03 MOD-IDTRANS          PIC X(4).                                    
000040*                                 BILDNUMMER                              
000050     03 MOD-TEMFSFEL         PIC X(40).                                   
000060*                                 MFS FELMEDDELANDE                       
000070     03 MOD-IDARTNR-IN       PIC X(2).                                    
000080*                                 MFS BEHANDLING AV INPUTFÄLT             
000090     03 MOD-IDARTNR-UT       PIC X(9).                                    
000100*                                 ARTIKELNUMMER                           
000110     03 MOD-INFO-RAD         OCCURS 8 TIMES.                              
000120*                                 RADINFORMATION                          
000130        05 MOD-IDARTNR-BYT   PIC Z(8)9.                                   
000140*                                 ARTIKELNUMMER                           
000150        05 FILLER            PIC X(6).                                    
000160        05 MOD-BEART-SVE     PIC X(25).                                   
000170*                                 SVENSK ARTIKELBENÄMNING                 
000180     03 MOD-TEMFSINF         PIC X(61).                                   
000190*                                 INFORMATIONSMEDDELANDE                  
      *** END COPY W3O15201    LENGTH=436                                       
