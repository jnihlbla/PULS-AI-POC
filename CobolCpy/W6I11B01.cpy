000100 01  MID-W6I11B01.                                                        
000200*                                 MIDCOPYTEXT TILL W6011B.                
000300     03 MID-IDPGM            PIC X(8).                                    
000400*                                 PROGRAM IDENTITET                       
000500*                                 PROGRAM INTENTITY                       
000600     03 MID-KVPOST           PIC 9(7).                                    
000700*                                 POST ELLER RADRÄKNARE                   
000800*                                 RECORD OR LINE COUNTER                  
000900     03 MID-R33POST          OCCURS 8 TIMES.                              
001000        05 MID-IDLEVNR       PIC X(5).                                    
001100*                                 LEVERANTÖRNUMMER                        
001200*                                 SUPPLIER NUMBER (VENDOR NUMBER)         
001300        05 MID-IDAVINR       PIC 9(7).                                    
001400*                                 AVI-NUMMER                              
001500*                                 ADVICE NOTE NUMBER                      
001600        05 MID-TIAVIDAT      PIC 9(6).                                    
001700*                                 AVISERINGSDATUM (YYMMDD)                
001800*                                 ADVICE NOTE DATE                        
001900        05 MID-IDARTNR       PIC 9(8).                                    
002000*                                 ARTIKELNUMMER                           
002100*                                 PART NUMBER                             
002200        05 MID-KVAVIS        PIC 9(6).                                    
002300*                                 AVISERAT ANTAL                          
002400*                                 QUANTITY NOTIFIED                       
002500        05 MID-KDRT          PIC 9(2).                                    
002600*                                 REDOVISNINGSTYP                         
002700*                                 TYPE OF ACCOUNTING                      
002800        05 MID-IDKONTO       PIC 9(10).                                   
002900*                                 KONTO                                   
003000*                                 ACCOUNT                                 
003100        05 MID-IDKST         PIC X(10).                                   
003200*                                 KOSTNADSSTÄLLE                          
003300*                                 COST CENTRE                             
003400        05 MID-IDANALYS      PIC X(12).                                   
003500*                                 ANALYSNUMMER                            
003600*                                 ANALYSIS NUMBER                         
003700        05 MID-IDDC          PIC X(2).                                    
003800*                                 IDENTIFIERARE LAGER                     
003900*                                 WAREHOUSE IDENTIFIER                    
004000        05 MID-IDDISTR       PIC S9(5).                                   
004100*                                 DISTRIKTNUMMER                          
004200*                                 DISTRICT NUMBER                         
004300        05 MID-IDKUNDNR      PIC S9(7).                                   
004400*                                 KUNDNUMMER                              
004500*                                 CUSTOMER NO                             
004600        05 MID-IDKUNDRF      PIC X(10).                                   
004700*                                 KUNDENS REFERENS (ORDERID)              
004800*                                 CUSTOMER REFERENCE (ORDER ID)           
004900        05 MID-IDPRODNR      PIC 9(7).                                    
005000*                                 PRODUKTIONSNUMMER                       
005100*                                 PRODUCTION NUMBER                       
005200        05 MID-IDFAKT        PIC 9(7).                                    
005300*                                 FAKTURANUMMER                           
005400*                                 INVOICE NO.                             
005500        05 MID-IDSUPREF      PIC X(10).                                   
005600*                                 LEVERANTöRSREF.                         
005700*                                 SUPPLIER REF.                           
005800*** END OF VILMAII-COPY LENGTH= 927 BYTES                                 
