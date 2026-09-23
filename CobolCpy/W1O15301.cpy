000010 01  MOD-W1O15301.                                                        
000020*                                 MOD-COPYTEXT FÖR W1015300               
000030     03 MOD-IDTRANS          PIC X(4).                                    
000040*                                 BILDNUMMER                              
000050     03 MOD-TEMFSFEL         PIC X(40).                                   
000060*                                 MFS FELMEDDELANDE                       
000070     03 MOD-KDPRODSL-IN      PIC X(2).                                    
000080*                                 PRODUKTSLAG                             
000090     03 MOD-KDPRODSL-UT      PIC X(2).                                    
000100*                                 PRODUKTSLAG                             
000110     03 MOD-IDPROJ-IN1       PIC X(4).                                    
000120*                                 PARTS PROJEKTIDENTITET                  
000130     03 MOD-IDPROJ-UT1       PIC X(4).                                    
000140*                                 PARTS PROJEKTIDENTITET                  
000150     03 MOD-IDPROJ-1         PIC X(4).                                    
000160*                                 PARTS PROJEKTIDENTITET                  
000170     03 MOD-IDPROJ-11        PIC X(4).                                    
000180*                                 PARTS PROJEKTIDENTITET                  
000190     03 MOD-IDPROJOBJ-1      PIC X(4).                                    
000200*                                 PROJEKTIDENTITET LV OBJEKT              
000210     03 MOD-IDPROJOBJ-11     PIC X(4).                                    
000220*                                 PROJEKTIDENTITET LV OBJEKT              
000230     03 MOD-IDPROJK-1        PIC X(4).                                    
000240*                                 PROJEKTIDENTITET KONSTRUKTION           
000250     03 MOD-IDPROJK-11       PIC X(4).                                    
000260*                                 PROJEKTIDENTITET KONSTRUKTION           
000270     03 MOD-RAD-PROJINFO     OCCURS 10 TIMES                              
000280                             INDEXED MOD-RAD-IND.                         
000290*                                 RADINFORMATION PER PROJEKT              
000300        05 MOD-RAD-IDPROJ-ATTR                                            
000310                             PIC X(2).                                    
000320*                                 MFS ATTRIBUTFÄLT                        
000330        05 MOD-RAD-IDPROJ    PIC X(4).                                    
000340*                                 PARTS PROJEKTIDENTITET                  
000350        05 MOD-RAD-IDPROJOBJ-ATTR                                         
000360                             PIC X(2).                                    
000370*                                 MFS ATTRIBUTFÄLT                        
000380        05 MOD-RAD-IDPROJOBJ PIC X(4).                                    
000390*                                 PROJEKTIDENTITET LV OBJEKT              
000400        05 MOD-RAD-IDPROJK-ATTR                                           
000410                             PIC X(2).                                    
000420*                                 MFS ATTRIBUTFÄLT                        
000430        05 MOD-RAD-IDPROJK   PIC X(4).                                    
000440*                                 PROJEKTIDENTITET KONSTRUKTION           
000450        05 MOD-RAD-KVNYART-ATTR                                           
000460                             PIC X(2).                                    
000470*                                 MFS ATTRIBUTFÄLT                        
000480        05 MOD-RAD-KVNYART   PIC Z(4)9.                                   
000490*                                 PROGNOS NYA ARTIKLAR                    
000500        05 MOD-RAD-KVNYRES-ATTR                                           
000510                             PIC X(2).                                    
000520*                                 MFS ATTRIBUTFÄLT                        
000530        05 MOD-RAD-KVNYRES   PIC Z(4)9.                                   
000540*                                 PROGNOS NYA RESERVDELAR                 
000550        05 MOD-RAD-TIPRODSTA-ATTR                                         
000560                             PIC X(2).                                    
000570*                                 MFS ATTRIBUTFÄLT                        
000580        05 MOD-RAD-TIPRODSTA PIC 9(6).                                    
000590*                                 PRODUKTIONS START AV VAGN               
000600        05 MOD-RAD-TIFINLEV-ATTR                                          
000610                             PIC X(2).                                    
000620*                                 MFS ATTRIBUTFÄLT                        
000630        05 MOD-RAD-TIFINLEV  PIC 9(6).                                    
000640*                                 PUBLICERINGSDATUM  (AAMMDD)             
000650        05 MOD-RAD-TIAAVV-FOM-ATTR                                        
000660                             PIC X(2).                                    
000670*                                 MFS ATTRIBUTFÄLT                        
000680        05 MOD-RAD-TIAAVV-FOM                                             
000690                             PIC 9(4).                                    
000700*                                 ÅR - VECKA  (ÅÅVV)                      
000710        05 MOD-RAD-TIAAVV-TOM-ATTR                                        
000720                             PIC X(2).                                    
000730*                                 MFS ATTRIBUTFÄLT                        
000740        05 MOD-RAD-TIAAVV-TOM                                             
000750                             PIC 9(4).                                    
000760*                                 ÅR - VECKA  (ÅÅVV)                      
000770        05 MOD-RAD-KDAGE-ATTR                                             
000780                             PIC X(2).                                    
000790*                                 MFS ATTRIBUTFÄLT                        
000800        05 MOD-RAD-KDAGE     PIC X.                                       
000810*                                 AGE-CODE                                
000820     03 MOD-KDCMD-IN-ATTR    PIC X(2).                                    
000830*                                 MFS ATTRIBUTFÄLT                        
000840     03 MOD-KDCMD-IN         PIC X(2).                                    
000850*                                 MFS BEHANDLING AV INPUTFÄLT             
000860     03 MOD-IDPROJ-IN2-ATTR  PIC X(2).                                    
000870*                                 MFS ATTRIBUTFÄLT                        
000880     03 MOD-IDPROJ-IN2       PIC X(2).                                    
000890*                                 MFS BEHANDLING AV INPUTFÄLT             
000900     03 MOD-IDPROJOBJ-IN-ATTR                                             
000910                             PIC X(2).                                    
000920*                                 MFS ATTRIBUTFÄLT                        
000930     03 MOD-IDPROJOBJ-IN     PIC X(2).                                    
000940*                                 MFS BEHANDLING AV INPUTFÄLT             
000950     03 MOD-IDPROJK-IN-ATTR  PIC X(2).                                    
000960*                                 MFS ATTRIBUTFÄLT                        
000970     03 MOD-IDPROJK-IN       PIC X(2).                                    
000980*                                 MFS BEHANDLING AV INPUTFÄLT             
000990     03 MOD-KVNYART-IN-ATTR  PIC X(2).                                    
001000*                                 MFS ATTRIBUTFÄLT                        
001010     03 MOD-KVNYART-IN       PIC X(2).                                    
001020*                                 MFS BEHANDLING AV INPUTFÄLT             
001030     03 MOD-KVNYRES-IN-ATTR  PIC X(2).                                    
001040*                                 MFS ATTRIBUTFÄLT                        
001050     03 MOD-KVNYRES-IN       PIC X(2).                                    
001060*                                 MFS BEHANDLING AV INPUTFÄLT             
001070     03 MOD-TIPRODSTA-IN-ATTR                                             
001080                             PIC X(2).                                    
001090*                                 MFS ATTRIBUTFÄLT                        
001100     03 MOD-TIPRODSTA-IN     PIC X(2).                                    
001110*                                 MFS BEHANDLING AV INPUTFÄLT             
001120     03 MOD-TIFINLEV-IN-ATTR PIC X(2).                                    
001130*                                 MFS ATTRIBUTFÄLT                        
001140     03 MOD-TIFINLEV-IN      PIC X(2).                                    
001150*                                 MFS BEHANDLING AV INPUTFÄLT             
001160     03 MOD-TIAAVV-IN-FOM-ATTR                                            
001170                             PIC X(2).                                    
001180*                                 MFS ATTRIBUTFÄLT                        
001190     03 MOD-TIAAVV-IN-FOM    PIC X(2).                                    
001200*                                 MFS BEHANDLING AV INPUTFÄLT             
001210     03 MOD-TIAAVV-IN-TOM-ATTR                                            
001220                             PIC X(2).                                    
001230*                                 MFS ATTRIBUTFÄLT                        
001240     03 MOD-TIAAVV-IN-TOM    PIC X(2).                                    
001250*                                 MFS BEHANDLING AV INPUTFÄLT             
001260     03 MOD-KDAGE-IN-ATTR    PIC X(2).                                    
001270*                                 MFS ATTRIBUTFÄLT                        
001280     03 MOD-KDAGE-IN         PIC X(2).                                    
001290*                                 MFS BEHANDLING AV INPUTFÄLT             
001300     03 MOD-TEMFSINF         PIC X(55).                                   
001310*                                 INFORMATIONSMEDDELANDE                  
      *** END COPY W1O15301    LENGTH=809                                       
