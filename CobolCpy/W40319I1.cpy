000100 01  REQU-W4I31901.                                                       
000200*                                 REQUEST TO PGM W40319                   
000300     03 REQU-IDDC            PIC X(2).                                    
000400*                                 IDENTIFIERARE LAGER                     
000500     03 REQU-IDANSTNR        PIC 9(5).                                    
000600*                                 ANSTÄLLNINGSNUMMER                      
000700     03 REQU-IDPRCPLK        PIC X(4).                                    
000800*                                 ID FÖR EN PLOCKRUNDA                    
000900     03 REQU-IDLOTNR-PLK     PIC 9(3).                                    
001000*                                 VAGN-NUMMER FÖR PLOCKRUNDA              
001100     03 REQU-KVRADER         PIC 9(5).                                    
001200*                                 ANTAL RADER                             
001300     03 REQU-RAD             OCCURS 500 TIMES.                            
001400        05 REQU-IDPRODNR     PIC 9(7).                                    
001500*                                 PRODUKTIONSNUMMER                       
001600        05 REQU-IDPLKLST     PIC 9(3).                                    
001700*                                 PLOCKLISTNUMMER                         
001800        05 REQU-IDRADNR      PIC 9(4).                                    
001900*                                 RADNUMMER                               
002000        05 REQU-FLSVAR       PIC X.                                       
002100*                                 ALLMÄN SVARSFLAGGA                      
002200*** END OF VILMAII-COPY LENGTH= 7519 BYTES                                
