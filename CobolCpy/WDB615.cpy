000100 01  LOGG-WDB615.                                                         
000200*                                 DC STYRREGISTER                         
000300*                                 ÄNDRINGSLOGG PER BILD                   
000400*                                 FYSISK NYCKEL: WDB615KY:                
000500*                                 (IDTRANS + IDDCREF)                     
000600     03 LOGG-IDTRANS         PIC X(4).                                    
000700*                                 BILDNUMMER                              
000800*                                 SCREEN NUMBER                           
000900     03 LOGG-IDDC-REF        PIC X(2).                                    
001000*                                 SÄNDANDE LAGER FÖR REFILL               
001100*                                 SENDING WAREHOUSE FOR REFILL            
001200     03 LOGG-IDUSER          PIC X(8).                                    
001300*                                 ANVÄNDARENS SÄKERHETS ID                
001400*                                 USER SECURITY-IDENTITY                  
001500     03 LOGG-TIUPPDAT        PIC 9(6).                                    
001600*                                 UPPDATERINGSDATUM  (ÅÅMMDD)             
001700*                                 UPDATING DATE     (YYMMDD)              
001800*** END OF VILMAII-COPY LENGTH= 20 BYTES                                  
