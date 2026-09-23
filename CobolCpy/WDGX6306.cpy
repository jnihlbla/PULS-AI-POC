000100 01  6306-WDGX6306.                                                       
000200*                                 SDC + NDC INLEV RETUR INFO              
000300*                                 FYSISK NYCKEL: IDFAKT                   
000400*                                 SÖKBEGREPP: FLKLAR                      
000500     03 6306-IDFAKT          PIC S9(7)           COMP-3.                  
000600*                                 FAKTURANUMMER                           
000700*                                 INVOICE NO.                             
000800     03 6306-FLKLAR          PIC X.                                       
000900*                                 AVSLUTNINGSMARKERING                    
001000*                                 FINISHED FLAG                           
001100     03 6306-FLDIRLEV        PIC X.                                       
001200*                                 DIREKTLEVERANS ?                        
001300*                                 DIRECT DELIVERY ?                       
001400     03 6306-IDDC-REC        PIC X(2).                                    
001500*                                 MOTTAGANDE LAGER                        
001600*                                 RECEIVING WAREHOUSE                     
001700     03 6306-IDDC-SEND       PIC X(2).                                    
001800*                                 SÄNDANDE LAGER                          
001900*                                 SENDING WAREHOUSE                       
002000     03 6306-TIFAKT          PIC S9(7)           COMP-3.                  
002100*                                 FAKTURERINGSDATUM (ÅÅMMDD)              
002200*                                 INVOICING DATE   (YYMMDD)               
002300     03 6306-IDDC-LEV        PIC X(2).                                    
002400*                                 LEVERERANDE DC I EXPORTFLÖDET           
002500*                                 DELIVERY DC IN EXPORT FLOW              
002600*** END OF VILMAII-COPY LENGTH= 16 BYTES                                  
