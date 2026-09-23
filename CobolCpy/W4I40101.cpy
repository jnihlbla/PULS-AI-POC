000100 01  MID-W4I40101.                                                        
000200*                                                                         
000300     03 MID-IDDC-REC-IN      PIC X(2).                                    
000400*                                 MOTTAGANDE LAGER                        
000500*                                 RECEIVING WAREHOUSE                     
000600     03 MID-IDDC-REF-IN      PIC X(2).                                    
000700*                                 SÄNDANDE LAGER FÖR REFILL               
000800*                                 SENDING WAREHOUSE FOR REFILL            
000900     03 MID-WKDAYTABLES      OCCURS 7 TIMES.                              
001000*                                                                         
001100        05 MID-FLREFDAY      PIC X.                                       
001200*                                 VECKO DAGNR FÖR REF. DAGORDER           
001300*                                 WEEKDAY NO. FOR REF. DAY ORDER          
001400        05 MID-FLREFBLK      PIC X.                                       
001500*                                 VECKO DAGNR FÖR REF. BULKORDER          
001600*                                 WEEKDAY NO. FOR REF. BULK ORDER         
001700        05 MID-KDREFDG       PIC X.                                       
001800*                                 VECKO DAGNR FÖR REFILL FG               
001900*                                 WEEK DAY NO. FOR REFILLING DG           
002000     03 MID-FLFRAKDG         PIC X.                                       
002100*                                 FLAGGA FRAKTKOD FARLIGT GODS            
002200*                                 FREIGHT CODE FLAG DANG. GOODS           
002300*** END OF VILMAII-COPY LENGTH= 26 BYTES                                  
