000010*** POSTTYP 136 FÖR ÖVERFÖRINGSFIL MELLAN PPMS OCH PARTS/SI               
000020*** ERSÄTTNINGS-UPPGIFTER                                                 
000030***                                                                       
000040 01  PPMS-PT136.                                                          
000050     05  POSTTYP         PIC X(3).                                        
000060*                                  *** POSTTYP = 136 ***                  
000070     05  ARTNR           PIC X(8).                                        
000080*                                  *** ARTIKELNUMMER,                     
000090*                                  HÖGERJUST, NOLLUNDERTRYCKT ***         
000100     05  RS-DATUM        PIC X(6).                                        
000110*                                  *** DATUM  (ÅÅMMDD)                    
000120*                                  FÖR SENASTE FÖRÄNDRING  ***            
000130     05  RS-KLOCKSLAG    PIC X(6).                                        
000140*                                  *** KLOCKSLAG  (HHMMSSTH)              
000150*                                  FÖR SENASTE FÖRÄNDRING  ***            
000160     05  RS-KOD          PIC X(2).                                        
000170*                                  *** ERSÄTTNIGSKOD                      
000180*                                  OM NOLL, SKRIVS HÄR SPACE  ***         
000190     05  RS-ANM          PIC X(8).                                        
000200*                                  *** ANMÄRKNING                         
000210*** END COPY PPMS136CC0  LENGTH=33    OLD LENGTH=26                       
