000100 01  MOD-W4O40101.                                                        
000200*                                                                         
000300     03 MOD-IDTRANS          PIC X(4).                                    
000400*                                 BILDNUMMER                              
000500*                                 SCREEN NUMBER                           
000600     03 MOD-TEMFSFEL         PIC X(40).                                   
000700*                                 MFS FELMEDDELANDE                       
000800*                                 MFS ERROR MESSAGE                       
000900     03 MOD-IDDC-REC-IN      PIC X(2).                                    
001000*                                 MOTTAGANDE LAGER                        
001100*                                 RECEIVING WAREHOUSE                     
001200     03 MOD-IDDC-REC-UT      PIC X(2).                                    
001300*                                 MOTTAGANDE LAGER                        
001400*                                 RECEIVING WAREHOUSE                     
001500     03 MOD-IDDC-REF-IN      PIC X(2).                                    
001600*                                 SÄNDANDE LAGER FÖR REFILL               
001700*                                 SENDING WAREHOUSE FOR REFILL            
001800     03 MOD-IDDC-REF-UT      PIC X(2).                                    
001900*                                 SÄNDANDE LAGER FÖR REFILL               
002000*                                 SENDING WAREHOUSE FOR REFILL            
002100     03 MOD-WKDAYTABLES      OCCURS 7 TIMES.                              
002200*                                 RADINFORMATION                          
002300*                                 LINE INFORMATION                        
002400        05 MOD-FLREFDAY-ATTR PIC X(2).                                    
002500*                                 MFS ATTRIBUTFÄLT                        
002600        05 MOD-FLREFDAY      PIC X.                                       
002700*                                 VECKO DAGNR FÖR REF. DAGORDER           
002800*                                 WEEKDAY NO. FOR REF. DAY ORDER          
002900        05 MOD-FLREFBLK-ATTR PIC X(2).                                    
003000*                                 MFS ATTRIBUTFÄLT                        
003100        05 MOD-FLREFBLK      PIC X.                                       
003200*                                 VECKO DAGNR FÖR REF. BULKORDER          
003300*                                 WEEKDAY NO. FOR REF. BULK ORDER         
003400        05 MOD-KDREFDG-ATTR  PIC X(2).                                    
003500*                                 MFS ATTRIBUTFÄLT                        
003600        05 MOD-KDREFDG       PIC X.                                       
003700*                                 VECKO DAGNR FÖR REFILL FG               
003800*                                 WEEK DAY NO. FOR REFILLING DG           
003900     03 MOD-FLFRAKDG-ATTR    PIC X(2).                                    
004000*                                 MFS ATTRIBUTFÄLT                        
004100     03 MOD-FLFRAKDG         PIC X.                                       
004200*                                 FLAGGA FRAKTKOD FARLIGT GODS            
004300*                                 FREIGHT CODE FLAG DANG. GOODS           
004400     03 MOD-TEMFSINF         PIC X(55).                                   
004500*                                 INFORMATIONSMEDDELANDE                  
004600*                                 INFORMATION MESSAGE                     
004700*** END OF VILMAII-COPY LENGTH= 173 BYTES                                 
