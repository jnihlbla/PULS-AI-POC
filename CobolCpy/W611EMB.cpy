000100 01  EMB-W611EMB.                                                         
000200*                                 LÄNKAREA TILL W611EMB  -                
000300*                                 TAR FRAM KDLAGEMB FRÅN                  
000400*                                 INDATA KDEMBISO(IDARTNR-EMBQ3)          
000500*                                 FYLL I   :                              
000600*                                 KDEMBISO                                
000700*                                 SVAR     :                              
000800*                                 KDLAGEMB ELLER SPACE                    
000900     03 EMB-KDEMBISO         PIC 9(3).                                    
001000*                                 ISO EMBALLAGE KOD                       
001100*                                 ISO PACKING MATERIAL CODE               
001200     03 EMB-KDLAGEMB         PIC X(4).                                    
001300*                                 EMBALLAGEBETECKNING                     
001400*                                 PACKINGNOTATION                         
001500     03 EMB-KDSVAR           PIC X.                                       
001600*                                 SVAR FRÅN PROGRAM ELLER SKÄRM           
001700*                                 RETURN CODE FROM PROGRAM                
001800*** END OF VILMAII-COPY LENGTH= 8 BYTES                                   
