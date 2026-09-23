000100 01  REQU-W40729I1.                                                       
000200*                                 REQUEST TO PGM W40729                   
000300     03 REQU-IDDC-KEY        PIC X(2).                                    
000400*                                 IDENTIFIERARE LAGER                     
000500     03 REQU-IDDISTR-KEY     PIC 9(5).                                    
000600*                                 DISTRIKTNUMMER                          
000700     03 REQU-IDRAPPNR-KEY    PIC 9(7).                                    
000800*                                 RAPPORT NUMMER                          
000900     03 REQU-SUMINVD-IN      PIC 9(3).                                    
001000*                                 MIN VÄRDE FÖR EN ORDERAD                
001100     03 REQU-REFOBNET-IN     PIC 9(3).                                    
001200*                                 FOBNET I PROCENT                        
001300     03 REQU-IDARTNR-NEXT    PIC 9(8).                                    
001400*                                 ARTIKELNUMMER                           
001500     03 REQU-FLMATCH-NEXT    PIC X.                                       
001600*                                 FLAGGA MATCH                            
001700     03 REQU-FLPRGRNS-NEXT   PIC X.                                       
001800*                                 RAD VÄRDE STÖRRE ÄN PRISGRÄNS           
001900     03 REQU-KVRADER         PIC 9(5).                                    
002000*                                 ANTAL RADER                             
002100*** END OF VILMAII-COPY LENGTH= 35 BYTES                                  
