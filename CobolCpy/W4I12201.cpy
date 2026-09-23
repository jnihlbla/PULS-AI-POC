000010 01  MID-W4I12201.                                                        
000020*                                 COPYTEXT F÷R MID W4I122                 
000030*                                                                         
000040     03 MID-IDKVAOMR-IN      PIC X.                                       
000050*                                 KVALITET KONTROLLOMR≈DE                 
000060*                                 QUALITY CONTROL AREA                    
000070     03 MID-IDKVAGRP-IN      PIC X(3).                                    
000080*                                 KVALITET KONTROLLGRUPP                  
000090*                                 QUALITY CONTROL GROUP                   
000100     03 MID-TIAARP-IN        PIC X(4).                                    
000110*                                 ≈R - REDOVISNINGSPERIOD (≈≈RP)          
000120*                                 12 PER ≈R                               
000130*                                 YEAR - ACCOUNTING PERIOD (YYAP)         
000140*                                 12 PER YEAR                             
000150     03 MID-KDKVASTA-IN      PIC X.                                       
000160*                                 KVALITET LAGERREVISION STATUS           
000170*                                 QUALITY WAREHOUSE AUDIT STATUS          
000180     03 MID-IDDC-IN          PIC X(2).                                    
000190*                                 IDENTIFIERARE LAGER                     
000200*                                 WAREHOUSE IDENTIFIER                    
000210     03 MID-IDKVAOMR-UT      PIC X.                                       
000220*                                 KVALITET KONTROLLOMR≈DE                 
000230*                                 QUALITY CONTROL AREA                    
000240     03 MID-IDKVAGRP-UT      PIC X(3).                                    
000250*                                 KVALITET KONTROLLGRUPP                  
000260*                                 QUALITY CONTROL GROUP                   
000270     03 MID-TIAARP-UT        PIC X(4).                                    
000280*                                 ≈R - REDOVISNINGSPERIOD (≈≈RP)          
000290*                                 12 PER ≈R                               
000300*                                 YEAR - ACCOUNTING PERIOD (YYAP)         
000310*                                 12 PER YEAR                             
000320     03 MID-KDKVASTA-UT      PIC X.                                       
000330*                                 KVALITET LAGERREVISION STATUS           
000340*                                 QUALITY WAREHOUSE AUDIT STATUS          
000350     03 MID-IDDC-UT          PIC X(2).                                    
000360*                                 IDENTIFIERARE LAGER                     
000370*                                 WAREHOUSE IDENTIFIER                    
000380     03 MID-IDKVAOMR-EN      PIC X.                                       
000390*                                 KVALITET KONTROLLOMR≈DE                 
000400*                                 QUALITY CONTROL AREA                    
000410     03 MID-IDKVAGRP-EN      PIC 9(3).                                    
000420*                                 KVALITET KONTROLLGRUPP                  
000430*                                 QUALITY CONTROL GROUP                   
000440     03 MID-TIREGDAT-EN      PIC 9(6).                                    
000450*                                 REGISTRERINGSDATUM (≈≈MMDD)             
000460*                                 REGISTRATION DATE (YYMMDD)              
000470     03 MID-KDKVASTA-EN      PIC X.                                       
000480*                                 KVALITET LAGERREVISION STATUS           
000490*                                 QUALITY WAREHOUSE AUDIT STATUS          
000500     03 MID-IDKVAOMR-NX      PIC X.                                       
000510*                                 KVALITET KONTROLLOMR≈DE                 
000520*                                 QUALITY CONTROL AREA                    
000530     03 MID-IDKVAGRP-NX      PIC 9(3).                                    
000540*                                 KVALITET KONTROLLGRUPP                  
000550*                                 QUALITY CONTROL GROUP                   
000560     03 MID-TIREGDAT-NX      PIC 9(6).                                    
000570*                                 REGISTRERINGSDATUM (≈≈MMDD)             
000580*                                 REGISTRATION DATE (YYMMDD)              
000590     03 MID-KDKVASTA-NX      PIC X.                                       
000600*                                 KVALITET LAGERREVISION STATUS           
000610*                                 QUALITY WAREHOUSE AUDIT STATUS          
000620     03 MID-INPUT            OCCURS 13 TIMES                              
000630                             INDEXED MID-IX-1.                            
000640*                                 COPYTEXT FOR MID W4O12201 ENDAS         
000650*                                 T INDATA-FƒLT                           
000660        05 MID-VALKOD        PIC X.                                       
000670*                                 ƒNDRINGSFLAGGA                          
000680*                                 MARK OF CHANGES                         
000690        05 MID-IDKVAOMR      PIC X.                                       
000700*                                 KVALITET KONTROLLOMR≈DE                 
000710*                                 QUALITY CONTROL AREA                    
000720        05 MID-IDKVAGRP      PIC 9(3).                                    
000730*                                 KVALITET KONTROLLGRUPP                  
000740*                                 QUALITY CONTROL GROUP                   
000750        05 MID-TIREGDAT      PIC 9(6).                                    
000760*                                 REGISTRERINGSDATUM (≈≈MMDD)             
000770*                                 REGISTRATION DATE (YYMMDD)              
000780        05 MID-TIKVAKON      PIC 9(6).                                    
000790*                                 KVALITET DATUM F÷R STATISTIK            
000800*                                 QUALITY DATE FOR STATISTIC              
000810        05 MID-KVART         PIC 9(7).                                    
000820*                                 ANTAL ARTNR PER BRYTBEGREPP             
000830*                                 NO OF PARTNOS PER TYPE                  
000840        05 MID-KDKVASTA      PIC X.                                       
000850*                                 KVALITET LAGERREVISION STATUS           
000860*                                 QUALITY WAREHOUSE AUDIT STATUS          
000870        05 MID-TIUPPDAT      PIC 9(6).                                    
000880*                                 UPPDATERINGSDATUM  (≈≈MMDD)             
000890*                                 UPDATING DATE     (YYMMDD)              
