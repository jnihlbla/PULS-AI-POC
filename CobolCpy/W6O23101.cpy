000010 01  MOD-W6O23101.                                                        
000020*                                 MODCOPYTEXT TILL W60231.                
000030     03 MOD-IDTRANS          PIC X(4).                                    
000040*                                 BILDNUMMER                              
000050     03 MOD-TEMFSFEL         PIC X(40).                                   
000060*                                 MFS FELMEDDELANDE                       
000070     03 MOD-IDPROVPL-IN      PIC X(2).                                    
000080*                                 MFS BEHANDLING AV INPUTFÄLT             
000090     03 MOD-IDPROVPL-UT      PIC X.                                       
000100*                                 PROVTAGNINGSPLAN                        
000110     03 MOD-KDPROVPL-IN      PIC X(2).                                    
000120*                                 MFS BEHANDLING AV INPUTFÄLT             
000130     03 MOD-KDPROVPL-UT      PIC X.                                       
000140*                                 PROVPLAN NORMAL/REDUCERAT UTTAG         
000150     03 MOD-TEXT-UT          PIC X(16).                                   
000160     03 MOD-KVSKPLOT-ATTR    PIC X(2).                                    
000170*                                 MFS ATTRIBUTFÄLT                        
000180     03 MOD-KVSKPLOT-UPD     PIC Z9.                                      
000190*                                 SKIPLOT RÄKNARE                         
000200     03 MOD-KVSKPLOT         PIC Z9.                                      
000210*                                 SKIPLOT RÄKNARE                         
000220     03 MOD-RADER            OCCURS 5 TIMES.                              
000230*                                 RADER                                   
000240        05 MOD-KVAVIS-FOM    PIC Z(6)9.                                   
000250*                                 AVISERAT ANTAL FR O M                   
000260        05 MOD-KVAVIS-TOM    PIC Z(6)9.                                   
000270*                                 AVISERAT ANTAL T O M                    
000280        05 MOD-KVPROVPL      PIC Z(5)9.                                   
000290*                                 KVAL.KONTROLL ANTAL ENL                 
000300*                                 PROVPLAN                                
000310     03 MOD-UPDATE.                                                       
000320*                                 TABELL-UPDATE                           
000330        05 MOD-KVAVIS-FOM-ATTR                                            
000340                             PIC X(2).                                    
000350*                                 MFS ATTRIBUTFÄLT                        
000360        05 MOD-KVAVIS-FOM-UPD                                             
000370                             PIC X(2).                                    
000380*                                 MFS BEHANDLING AV INPUTFÄLT             
000390        05 MOD-KVAVIS-TOM-ATTR                                            
000400                             PIC X(2).                                    
000410*                                 MFS ATTRIBUTFÄLT                        
000420        05 MOD-KVAVIS-TOM-UPD                                             
000430                             PIC X(2).                                    
000440*                                 MFS BEHANDLING AV INPUTFÄLT             
000450        05 MOD-KVPROVPL-ATTR PIC X(2).                                    
000460*                                 MFS ATTRIBUTFÄLT                        
000470        05 MOD-KVPROVPL-UPD  PIC X(2).                                    
000480*                                 MFS BEHANDLING AV INPUTFÄLT             
000490        05 MOD-KDCMD-ATTR    PIC X(2).                                    
000500*                                 MFS ATTRIBUTFÄLT                        
000510        05 MOD-KDCMD-UPD     PIC X(2).                                    
000520*                                 MFS BEHANDLING AV INPUTFÄLT             
000530     03 MOD-TEMFSINF         PIC X(55).                                   
000540*                                 INFORMATIONSMEDDELANDE                  
      *** END COPY W6O23101    LENGTH=243                                       
