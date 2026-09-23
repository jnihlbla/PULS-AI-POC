000100 01  MID-W6I11C01.                                                        
000200*                                 MIDCOPYTEXT TILL W6011C.                
000300     03 MID-IDPGM            PIC X(8).                                    
000400*                                 PROGRAM IDENTITET                       
000500*                                 PROGRAM INTENTITY                       
000600     03 MID-IDDC             PIC X(2).                                    
000700*                                 IDENTIFIERARE LAGER                     
000800*                                 WAREHOUSE IDENTIFIER                    
000900     03 MID-KVPOST           PIC 9(7).                                    
001000*                                 POST ELLER RADRÄKNARE                   
001100*                                 RECORD OR LINE COUNTER                  
001200     03 MID-FLSVS            PIC X.                                       
001300*                                 ANGER GENERELLT OM NÅGOT AVSER          
001400*                                 SVS                                     
001500     03 MID-R34POST          OCCURS 22 TIMES.                             
001600        05 MID-IDLEVNR       PIC X(5).                                    
001700*                                 LEVERANTÖRNUMMER                        
001800*                                 SUPPLIER NUMBER (VENDOR NUMBER)         
001900        05 MID-IDAVINR       PIC 9(7).                                    
002000*                                 AVI-NUMMER                              
002100*                                 ADVICE NOTE NUMBER                      
002200        05 MID-TIAVIDAT      PIC 9(6).                                    
002300*                                 AVISERINGSDATUM (YYMMDD)                
002400*                                 ADVICE NOTE DATE                        
002500        05 MID-IDARTNR       PIC 9(8).                                    
002600*                                 ARTIKELNUMMER                           
002700*                                 PART NUMBER                             
002800        05 MID-KVAVIS        PIC S9(7).                                   
002900*                                 AVISERAT ANTAL                          
003000*                                 QUANTITY NOTIFIED                       
003100        05 MID-KDRT          PIC 9(2).                                    
003200*                                 REDOVISNINGSTYP                         
003300*                                 TYPE OF ACCOUNTING                      
003400        05 MID-IDKONTO       PIC 9(10).                                   
003500*                                 KONTO                                   
003600*                                 ACCOUNT                                 
003700        05 MID-IDARTNR-FROM  PIC 9(8).                                    
003800*                                 ARTIKELNUMMER                           
003900*                                 PART NUMBER                             
004000        05 MID-IDANALYS      PIC X(12).                                   
004100*                                 ANALYSNUMMER                            
004200*                                 ANALYSIS NUMBER                         
004300        05 MID-IDKST         PIC X(10).                                   
004400*                                 KOSTNADSSTÄLLE                          
004500*                                 COST CENTRE                             
004600*** END OF VILMAII-COPY LENGTH= 1668 BYTES                                
