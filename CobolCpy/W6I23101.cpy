000010 01  MID-W6I23101.                                                        
000020*                                 MID-COPYTEXT FÖR W60231                 
000030     03 MID-IDPROVPL-IN      PIC X.                                       
000040*                                 PROVTAGNINGSPLAN                        
000050     03 MID-IDPROVPL-UT      PIC X.                                       
000060*                                 PROVTAGNINGSPLAN                        
000070     03 MID-KDPROVPL-IN      PIC X.                                       
000080*                                 PROVPLAN NORMAL/REDUCERAT UTTAG         
000090     03 MID-KDPROVPL-UT      PIC X.                                       
000100*                                 PROVPLAN NORMAL/REDUCERAT UTTAG         
000110     03 MID-INPUT.                                                        
000120*                                 INDATA FÖR UPPDATERING                  
000130        05 MID-KVSKPLOT-UPD  PIC 9(2).                                    
000140*                                 SKIPLOT RÄKNARE                         
000150        05 MID-KVAVIS-FOM-UPD                                             
000160                             PIC 9(7).                                    
000170*                                 AVISERAT ANTAL FR O M                   
000180        05 MID-KVAVIS-TOM-UPD                                             
000190                             PIC 9(7).                                    
000200*                                 AVISERAT ANTAL T O M                    
000210        05 MID-KVPROVPL-UPD  PIC 9(6).                                    
000220*                                 KVAL.KONTROLL ANTAL ENL                 
000230*                                 PROVPLAN                                
000240        05 MID-KDCMD-UPD     PIC X.                                       
000250         88 MID-KDCMD-INGENTING                                           
000260                             VALUE ' '.                                   
000270         88 MID-KDCMD-DELETE VALUE 'D'                                    
000280                             'B'.                                         
000290         88 MID-KDCMD-REPLACE                                             
000300                             VALUE 'R'                                    
000310                             'Ä'.                                         
000320         88 MID-KDCMD-INSERT VALUE 'I'                                    
000330                             'N'.                                         
000340*                                 RAD-UPPDATERINGSKOMMANDO                
      *** END COPY W6I23101    LENGTH=27                                        
