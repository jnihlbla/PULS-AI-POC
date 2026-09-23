000100 01  RESP-W60111O1.                                                       
000200*                                 MODCOPYTEXT TILL W60111.                
000300     03 RESP-FLGODK-IN       PIC X.                                       
000400     03 RESP-FLGODK-IN-ATTR  PIC X(2).                                    
000500*                                 MFS ATTRIBUTFÄLT                        
000600     03 RESP-UPDATE          OCCURS 500 TIMES.                            
000700*                                 UPDATE                                  
000800        05 RESP-IDARTNR-LINE PIC X(8).                                    
000900*                                 ARTIKELNUMMER                           
001000*                                 PART NUMBER                             
001100        05 RESP-IDARTNR-LINE-ATTR                                         
001200                             PIC X(2).                                    
001300*                                 MFS ATTRIBUTFÄLT                        
001400        05 RESP-KVAVIS-LINE  PIC X(6).                                    
001500*                                 AVISERAT ANTAL                          
001600*                                 QUANTITY NOTIFIED                       
001700        05 RESP-KVAVIS-LINE-ATTR                                          
001800                             PIC X(2).                                    
001900*                                 MFS ATTRIBUTFÄLT                        
002000*** END OF VILMAII-COPY LENGTH= 9003 BYTES                                
