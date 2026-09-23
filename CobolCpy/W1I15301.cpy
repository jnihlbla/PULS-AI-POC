000010 01  MID-W1I15301.                                                        
000020*                                 MID-COPYTEXT FÖR W1015300               
000030     03 MID-KDPRODSL-IN      PIC X(2).                                    
000040*                                 PRODUKTSLAG                             
000050     03 MID-KDPRODSL-UT      PIC X(2).                                    
000060*                                 PRODUKTSLAG                             
000070     03 MID-IDPROJ-IN        PIC X(4).                                    
000080*                                 PARTS PROJEKTIDENTITET                  
000090     03 MID-IDPROJ-UT        PIC X(4).                                    
000100*                                 PARTS PROJEKTIDENTITET                  
000110     03 MID-IDPROJ-1         PIC X(4).                                    
000120*                                 PARTS PROJEKTIDENTITET                  
000130     03 MID-IDPROJ-11        PIC X(4).                                    
000140*                                 PARTS PROJEKTIDENTITET                  
000150     03 MID-IDPROJOBJ-1      PIC X(4).                                    
000160*                                 PROJEKTIDENTITET LV OBJEKT              
000170     03 MID-IDPROJOBJ-11     PIC X(4).                                    
000180*                                 PROJEKTIDENTITET LV OBJEKT              
000190     03 MID-IDPROJK-1        PIC X(4).                                    
000200*                                 PROJEKTIDENTITET KONSTRUKTION           
000210     03 MID-IDPROJK-11       PIC X(4).                                    
000220*                                 PROJEKTIDENTITET KONSTRUKTION           
000230     03 MID-INFALT.                                                       
000240*                                 RAPPORTERINGSFÄLT 1153-BILDEN           
000250        05 MID-KDCMD         PIC X.                                       
000260         88 MID-KDCMD-INGENTING                                           
000270                             VALUE ' '.                                   
000280         88 MID-KDCMD-DELETE VALUE 'D'                                    
000290                             'B'.                                         
000300         88 MID-KDCMD-REPLACE                                             
000310                             VALUE 'R'                                    
000320                             'Ä'.                                         
000330         88 MID-KDCMD-INSERT VALUE 'I'                                    
000340                             'N'.                                         
000350*                                 RAD-UPPDATERINGSKOMMANDO                
000360        05 MID-IDPROJ        PIC X(4).                                    
000370*                                 PARTS PROJEKTIDENTITET                  
000380        05 MID-IDPROJOBJ     PIC X(4).                                    
000390*                                 PROJEKTIDENTITET LV OBJEKT              
000400        05 MID-IDPROJK       PIC X(4).                                    
000410*                                 PROJEKTIDENTITET KONSTRUKTION           
000420        05 MID-KVNYART       PIC 9(5).                                    
000430*                                 PROGNOS NYA ARTIKLAR                    
000440        05 MID-KVNYRES       PIC 9(5).                                    
000450*                                 PROGNOS NYA RESERVDELAR                 
000460        05 MID-TIPRODSTA     PIC 9(6).                                    
000470*                                 PRODUKTIONS START AV VAGN               
000480        05 MID-TIFINLEV      PIC 9(6).                                    
000490*                                 PUBLICERINGSDATUM  (AAMMDD)             
000500        05 MID-TIAAVV-FOM    PIC 9(4).                                    
000510*                                 ÅR - VECKA  (ÅÅVV)                      
000520        05 MID-TIAAVV-TOM    PIC 9(4).                                    
000530*                                 ÅR - VECKA  (ÅÅVV)                      
000540        05 MID-KDAGE         PIC X.                                       
000550*                                 AGE-CODE                                
      *** END COPY W1I15301    LENGTH=80                                        
