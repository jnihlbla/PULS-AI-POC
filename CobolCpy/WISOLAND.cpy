000010 01  LAND-WISOLAND.                                                       
000020*                                 PARAMETRAR TILL WISOLAND FÖR            
000030*                                 ATT KONVERTERA MELLAN 2- OCH            
000040*                                 3-STÄLLIG KOD.                          
000050*                                                                         
000060*                                 EXEMPEL PÅ ANROP:                       
000070*                                 MOVE "AFG" TO LAND-IDLANDX3             
000080*                                 MOVE "  "  TO LAND-IDLANDX2             
000090*                                 ELLER                                   
000100*                                 MOVE "   " TO LAND-IDLANDX3             
000110*                                 MOVE "AF"  TO LAND-IDLANDX2             
000120*                                 CALL WISOLAND USING                     
000130*                                      LAND-WISOLAND                      
000140*                                                                         
000150*                                 RESULTAT ERHÅLLS I ÖVRIGA FÄLT          
000160*                                 STATUSKOD I LAND-KDSVAR.                
000170*                                 BLANK = RÄTT, F = FEL                   
000180*                                 -------------------------------         
000190*                                 PARAMETERS TO WISOLAND FOR              
000200*                                 CONVERTERING OF 2- OR                   
000210*                                 3-LETTERS COUNTRY CODE                  
000220*                                 EXAMPLE OF CALL:                        
000230*                                 MOVE "AFG" TO LAND-IDLANDX3             
000240*                                 MOVE "  "  TO LAND-IDLANDX2             
000250*                                 OR                                      
000260*                                 MOVE "   " TO LAND-IDLANDX3             
000270*                                 MOVE "AF"  TO LAND-IDLANDX2             
000280*                                 CALL WISOLAND USING                     
000290*                                      LAND-WISOLAND                      
000300*                                                                         
000310*                                 RESULT IS PLACED IN                     
000320*                                 REMAINDING FEILD                        
000330*                                 RETURN CODE IN LAND-KDSVAR.             
000340*                                 BLANK = OK, F = ERROR                   
000350*                                 -------------------------------         
000360     03 LAND-IDLANDX3        PIC X(3).                                    
000370*                                 3-STÄLLIG LANDSBETECKNINGSKOD           
000380*                                 3-LETTER CODE FOR COUNTRY.              
000390     03 LAND-IDLANDX2        PIC X(2).                                    
000400*                                 2-STÄLLIG LANDSBETECKNINGSKOD           
000410*                                 2-LETTER CODE FOR COUNTRY               
000420     03 LAND-BELAND-SVE      PIC X(35).                                   
000430*                                 SVENSK LANDSBETECKNING                  
000440*                                 SWEDISH NAME OF COUNTRY                 
000450     03 LAND-BELAND-ENG      PIC X(35).                                   
000460*                                 ENGELSK LANDSBETECKNING                 
000470*                                 ENGLISH NAME OF COUNTRY                 
000480     03 LAND-WISOVAL         OCCURS 3 TIMES.                              
000490        05 LAND-KDVALISO     PIC X(3).                                    
000500*                                 VALUTAKOD ENLIGT ISO-STANDARD.          
000510*                                 CURRENCY CODE BY ISO-STANDARD.          
000520        05 FILLER            PIC X(7).                                    
000530        05 LAND-BEVALISO     PIC X(25).                                   
000540*                                 BENÄMNING VALUTA ISO-STANDARD.          
000550*                                 DESCRIPTION CURRENCY BY ISO             
000560     03 LAND-KDSVAR          PIC X.                                       
000570*                                 SVAR FRÅN PROGRAM ELLER SKÄRM           
000580*                                 RETURN CODE FROM PROGRAM                
000590*** END COPY WISOLAND  LENGTH=181                                         
