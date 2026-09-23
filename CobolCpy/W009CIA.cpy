000010 01  CIA-W009CIA.                                                         
000020*                                 PARAMETRAR TILL W009CIA                 
000030*                                 (CROSS-INDEX-ARTIKEL)                   
000040*                                 FÖR ATT KONVERTERA MELLAN               
000050*                                 17-STÄLLIGT OCH 8/9 STÄLLIG             
000060*                                 ARTIKEL-IDENTITET                       
000070     03 CIA-IDARTPRE-IN      PIC X(3).                                    
000080*                                 IDENTIFIERARE ARTIKELSORTIMENT          
000090*                                 PARTS RANGE IDENTIFIER                  
000100     03 CIA-IDARTBET-IN.                                                  
000110        05 FILLER            PIC X(17).                                   
000120*                                 ARTIKELBETECKNING EFTERMARKNAD          
000130*                                 AFTERMARKET PARTNUMBER                  
000140     03 CIA-IDARTPRE-UT      PIC X(3).                                    
000150*                                 IDENTIFIERARE ARTIKELSORTIMENT          
000160*                                 PARTS RANGE IDENTIFIER                  
000170     03 CIA-IDARTBET-UT      PIC X(17).                                   
000180*                                 ARTIKELBETECKNING EFTERMARKNAD          
000190*                                 AFTERMARKET PARTNUMBER                  
000200     03 CIA-IDARTBET-MOD     PIC X(17).                                   
000210*                                 ARTIKELBETECKNING EFTERMARKNAD          
000220*                                 AFTERMARKET PARTNUMBER                  
000230     03 CIA-IDARTNR          PIC S9(9)           COMP-3.                  
000240*                                 ARTIKELNUMMER                           
000250*                                 PART NUMBER                             
000260     03 CIA-IDARTN8          PIC 9(8).                                    
000270*                                 VOLVO 8 POS NUMERISKT ARTIKELNR         
000280*                                 VOLVO 8 POS NUMERIC PARTNO              
000290     03 CIA-IDARTX9          PIC X(9).                                    
000300*                                  9 POS ARTNR FÖR MID/MOD                
000310*                                  9 POS PARTNO FOR MID/MOD               
000320     03 CIA-KDSVAR           PIC X.                                       
000330*                                 SVAR FRÅN PROGRAM ELLER SKÄRM           
000340*                                 RETURN CODE FROM PROGRAM                
000350*** END COPY W009CIA   LENGTH=80                                          
