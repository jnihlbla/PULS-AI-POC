000010 01  MOMS-W009MOMS.                                                       
000020*                                 PARAMETRAR TILL W009MOMS FÖR            
000030*                                 ATT TA REDA PÅ VILKEN MOMS              
000040*                                 SOM GÄLLER FÖR ETT GIVET LAND           
000050*                                 OCH MOMSKOD.                            
000060*                                 EXEMPEL PÅ ANROP:                       
000070*                                 MOVE "GB" TO MOMS-IDLANDX2              
000080*                                 MOVE "01" TO MOMS-KDVAT                 
000090*                                 CALL W009MOMS USING                     
000100*                                               MOMS-W009MOMS             
000110*                                                                         
000120*                                 RESULTAT ERHÅLLS I MOMS-KDSVAR          
000130*                                 SPACE = MOMS I FORMEN 9V9999            
000140*                                         DÄR 25% GES SOM 0.2500          
000150*                                  F    = LAND OCH/ELLER MOMSKOD          
000160*                                         SAKNAS I MOMSTABELLEN           
000170*                                         MOMS NOLL GES                   
000180     03 MOMS-IDLANDX2        PIC X(2).                                    
000190*                                 2-STÄLLIG LANDSBETECKNINGSKOD           
000200*                                 2-LETTER CODE FOR COUNTRY               
000210     03 MOMS-KDVAT           PIC X(2).                                    
000220*                                 MOMSKOD                                 
000230*                                 VAT CODE                                
000240     03 MOMS-REVAT           PIC S9V9(4)         COMP-3.                  
000250*                                 MULTIPLIKATIONSFAKTOR FÖR MOMS          
000260*                                 VAT FACTOR                              
000270     03 MOMS-BEVAT           PIC X(2).                                    
000280*                                 MOMSKODSBENÄMNING                       
000290*                                 VAT CODE DESCRIPTION                    
000300     03 MOMS-KDSVAR          PIC X.                                       
000310*                                 SVAR FRÅN PROGRAM ELLER SKÄRM           
000320*                                 RETURN CODE FROM PROGRAM                
000330*** END COPY W009MOMS  LENGTH=10                                          
