000100 01  MOD-W2O32501-CTX.                                                    
000200     03 MOD-IDTRANS          PIC X(4).                                    
000300*                                 BILDNUMMER                              
000400     03 MOD-TEMFSFEL         PIC X(40).                                   
000500*                                 MFS FELMEDDELANDE                       
000600     03 MOD-W2O32501-001-GRP OCCURS 14 TIMES.                             
000700        05 MOD-IDLEVNR-ATTR  PIC X(2).                                    
000800*                                 MFS ATTRIBUTFÄLT                        
000900        05 MOD-IDLEVNR       PIC X(5).                                    
001000*                                 LEVERANTÖRNUMMER                        
001100        05 MOD-IDARTNR-ATTR  PIC X(2).                                    
001200*                                 MFS ATTRIBUTFÄLT                        
001300        05 MOD-IDARTNR       PIC X(9).                                    
001400*                                 ARTIKELNUMMER                           
001500        05 MOD-TIAAMMDD-ATTR PIC X(2).                                    
001600*                                 MFS ATTRIBUTFÄLT                        
001700        05 MOD-TIAAMMDD      PIC 9(6).                                    
001800*                                 ÅR - MÅNAD - DAG  (ÅÅMMDD)              
001900        05 MOD-KVANTMOT-ATTR PIC X(2).                                    
002000*                                 MFS ATTRIBUTFÄLT                        
002100        05 MOD-KVANTMOT      PIC X(7).                                    
002200*                                 ANTAL MOTTAGET                          
002300        05 MOD-KVAVIS-ATTR   PIC X(2).                                    
002400*                                 MFS ATTRIBUTFÄLT                        
002500        05 MOD-KVAVIS        PIC X(7).                                    
002600*                                 AVISERAT ANTAL                          
002700        05 MOD-TYP-ATTR      PIC X(2).                                    
002800*                                 MFS ATTRIBUTFÄLT                        
002900        05 MOD-TYP           PIC X.                                       
003000     03 MOD-TEMFSINF         PIC X(55).                                   
003100*                                 INFORMATIONSMEDDELANDE                  
003200*** END OF VILMAII-COPY LENGTH= 757 BYTES                                 
