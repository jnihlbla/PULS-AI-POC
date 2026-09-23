000010 01  W42681.                                                              
000020*                                 INLEVERANS HISTORIK                     
000030*                                 310 FRÅN ANDRA LAGRET                   
000040*                                 R30 CLEARING                            
000050*                                 R31 MOTTAGET                            
000060*                                 R32 RAPPORTERAD                         
000070     03 IDPTYP               PIC X(3).                                    
000080*                                 POSTTYP                                 
000090     03 IDLOPNRM             PIC S9(9)           COMP-3.                  
000100*                                 LÖPNUMMER MOTTAGNINGSRAPPORT            
000110*                                 (0VVDLLLLK)                             
000120     03 IDAVINR              PIC S9(7)           COMP-3.                  
000130*                                 AVI-NUMMER                              
000140     03 IDKONTO              PIC S9(11)          COMP-3.                  
000150*                                 KONTO                                   
000160     03 IDLEVNR              PIC S9(5)           COMP-3.                  
000170*                                 LEVERANTÖRNUMMER                        
000180     03 ADLAGOMR             PIC S9(3)           COMP-3.                  
000190*                                 LAGEROMRÅDE                             
000200     03 ADGANG               PIC S9(3)           COMP-3.                  
000210*                                 GÅNG                                    
000220     03 ADPLATS              PIC S9(5)           COMP-3.                  
000230*                                 LAGERPLATSNUMMER                        
000240     03 KDCLAGER             PIC S9              COMP-3.                  
000250*                                 CENTRALLAGERKOD                         
000260     03 KDRT                 PIC S9(3)           COMP-3.                  
000270*                                 REDOVISNINGSTYP                         
000280     03 KDAVVANT             PIC S9              COMP-3.                  
000290*                                 AVVIKELSEANTAL KOD                      
000300*                                 0=INGEN ANM.   1=AVVIKELSE              
000310*                                 2=MAKULERING AV MOTT.RAPPORT            
000320     03 KDAVVKV              PIC S9              COMP-3.                  
000330*                                 KVALITETSAVVIKELSEKOD                   
000340*                                 0=INGEN ANM.  1=AVVIKELSE               
000350*                                 2=AVVIKELSE, RETURNERAS                 
000360     03 KVANTMOT             PIC S9(7)           COMP-3.                  
000370*                                 ANTAL MOTTAGET                          
000380     03 KVAVIS               PIC S9(7)           COMP-3.                  
000390*                                 AVISERAT ANTAL                          
000400     03 KVFORDEL             PIC S9(7)           COMP-3.                  
000410*                                 ANTAL FÖRDELAT                          
000420     03 KVRETUR              PIC S9(7)           COMP-3.                  
000430*                                 ANTAL I RETUR                           
000440     03 KVFORV               PIC S9(7)           COMP-3.                  
000450*                                 FÖRVÄNTAT ANTAL EFTER JUSTERING         
000460     03 TIAVIDAT             PIC S9(7)           COMP-3.                  
000470*                                 AVISERINGSDATUM (ÅÅMMDD)                
000480     03 TIUPPDAT             PIC S9(7)           COMP-3.                  
000490*                                 UPPDATERINGSDATUM  (ÅÅMMDD)             
      *** END COPY W4268101    LENGTH=61                                        
