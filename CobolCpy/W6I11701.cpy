000100 01  MID-W6I11701.                                                        
000200*                                 MIDCOPYTEXT TILL W60117.                
000300     03 MID-IDDC-IN          PIC X(2).                                    
000400*                                 IDENTIFIERARE LAGER                     
000500*                                 WAREHOUSE IDENTIFIER                    
000600     03 MID-IDDC-UT          PIC X(2).                                    
000700*                                 IDENTIFIERARE LAGER                     
000800*                                 WAREHOUSE IDENTIFIER                    
000900     03 MID-TIAVIDAT         PIC X(6).                                    
001000*                                 AVISERINGSDATUM (YYMMDD)                
001100*                                 ADVICE NOTE DATE                        
001200     03 MID-R34POST          OCCURS 14 TIMES.                             
001300        05 MID-IDARTNR       PIC X(8).                                    
001400*                                 ARTIKELNUMMER                           
001500*                                 PART NUMBER                             
001600        05 MID-IDLEVNR       PIC X(5).                                    
001700*                                 LEVERANTÖRNUMMER                        
001800*                                 SUPPLIER NUMBER (VENDOR NUMBER)         
001900        05 MID-KDRT          PIC X(2).                                    
002000*                                 REDOVISNINGSTYP                         
002100*                                 TYPE OF ACCOUNTING                      
002200        05 MID-IDKONTO       PIC X(10).                                   
002300*                                 KONTO                                   
002400*                                 ACCOUNT                                 
002500        05 MID-IDAVINR       PIC X(7).                                    
002600*                                 AVI-NUMMER                              
002700*                                 ADVICE NOTE NUMBER                      
002800        05 MID-KVAVIS        PIC X(7).                                    
002900*                                 AVISERAT ANTAL                          
003000*                                 QUANTITY NOTIFIED                       
003100        05 MID-IDARTNR-FROM  PIC X(8).                                    
003200*                                 ARTIKELNUMMER                           
003300*                                 PART NUMBER                             
003400        05 MID-IDANALYS      PIC X(12).                                   
003500*                                 ANALYSNUMMER                            
003600*                                 ANALYSIS NUMBER                         
003700        05 MID-IDKST         PIC X(10).                                   
003800*                                 KOSTNADSSTÄLLE                          
003900*                                 COST CENTRE                             
004000*** END OF VILMAII-COPY LENGTH= 976 BYTES                                 
