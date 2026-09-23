000010 01  MID-W1I29401.                                                        
000020*                                 MID-COPYTEXT FÖR R129400                
000030     03 MID-IDARTNR-IN       PIC X(9).                                    
000040*                                 ARTIKELNUMMER                           
000050     03 MID-IDARTNR-UT       PIC X(9).                                    
000060*                                 ARTIKELNUMMER                           
000070     03 MID-BELEVART-IN      PIC X(30).                                   
000080*                                 LEVERANTÖRENS ARTIKELBENÄMNING          
000090     03 MID-BELEVART-UT      PIC X(30).                                   
000100*                                 LEVERANTÖRENS ARTIKELBENÄMNING          
000110     03 MID-IDLEVNR-IN       PIC X(5).                                    
000120*                                 LEVERANTÖRNUMMER                        
000130     03 MID-IDLEVNR-UT       PIC X(5).                                    
000140*                                 LEVERANTÖRNUMMER                        
000150     03 MID-IDSKYLT-IN-DOLT  PIC X(3).                                    
000160*                                 NATIONALITETSTECKEN                     
000170     03 MID-IDSKYLT-UT-DOLT  PIC X(3).                                    
000180*                                 NATIONALITETSTECKEN                     
000190     03 MID-1002-SATS-IN-DOLT                                             
000200                             PIC X.                                       
000210*                                 ALLMÄN SVARSFLAGGA                      
000220     03 MID-1002-SATS-UT-DOLT                                             
000230                             PIC X.                                       
000240*                                 ALLMÄN SVARSFLAGGA                      
000250     03 MID-KDPRODSL-IN-DOLT PIC X(2).                                    
000260*                                 PRODUKTSLAG                             
000270     03 MID-KDPRODSL-UT-DOLT PIC X(2).                                    
000280*                                 PRODUKTSLAG                             
000290     03 MID-ANTAL-SEGMENT-ENTER                                           
000300                             PIC 9(3).                                    
000310     03 MID-ANTAL-SEGMENT-NEXT                                            
000320                             PIC 9(3).                                    
000330     03 MID-INPUT.                                                        
000340*                                                                         
000350        05 MID-BEART-UTG-ART PIC X(25).                                   
000360*                                 ARTIKELBENÄMNING                        
000370        05 MID-AANGRA        PIC X.                                       
000380*                                 ALLMÄN SVARSFLAGGA                      
000390        05 MID-IDAO          PIC X(10).                                   
000400*                                 ÄNDRINGSORDERNUMMER                     
000410        05 MID-BORTTAG       PIC X.                                       
000420*                                 ALLMÄN SVARSFLAGGA                      
000430        05 MID-TIAAVV        PIC X(4).                                    
000440*                                 ÅR - VECKA  (ÅÅVV)                      
000450        05 MID-RADER         OCCURS 2 TIMES.                              
000460*                                  TILLKOMMANDE ARTIKLAR                  
000470           07 MID-IDLEVNR    PIC X(5).                                    
000480*                                 LEVERANTÖRNUMMER                        
000490           07 MID-BELEVART   PIC X(30).                                   
000500*                                 LEVERANTÖRENS ARTIKELBENÄMNING          
000510           07 MID-IDARTNR    PIC X(9).                                    
000520*                                 ARTIKELNUMMER                           
000530           07 MID-REANTPSA   PIC X(6).                                    
000540*                                 ANTAL PER SATS                          
000550           07 MID-KDSORT     PIC X(2).                                    
000560*                                 SORT-KOD                                
000570           07 MID-BEART      PIC X(25).                                   
000580*                                 ARTIKELBENÄMNING                        
000590           07 MID-KDBENHOM   PIC X.                                       
000600*                                 HOMONYMKOD                              
000610           07 MID-IDSTRTYP   PIC X.                                       
000620*                                 STRUKTURTYP                             
000630           07 MID-TESTRNOT   OCCURS 2 TIMES                               
000640                             PIC X(60).                                   
000650        05 MID-KLAR          PIC X.                                       
000660*                                 ALLMÄN SVARSFLAGGA                      
000670     03 MID-IDUSER           PIC X(8).                                    
000680*                                 ANVÄNDARENS SÄKERHETS ID                
      *** END COPY W1I29401    LENGTH=554                                       
