000100 01  3166-WDGX3166.                                                       
000200*                                 BYTES PROFORMAFAKTUROR                  
000300*                                 FAKTURA-SEGMENT                         
000400*                                 FYSISK NYCKEL: IDBYTFAK                 
000500*                                 S÷KBEGREPP: IDDISTR, IDDC,              
000600*                                  KDBYTFAK                               
000700     03 3166-IDBYTFAK        PIC 9(4).                                    
000800*                                 BYTES FAKTURANUMMER                     
000900*                                 EXCHANGE INVOICE NUMBER                 
001000     03 3166-DAREGDAT        PIC 9(8).                                    
001100*                                 REGISTRERINGSDATUM (≈≈≈≈MMDD)           
001200*                                 REGISTRATION DATE (YYYYMMDD)            
001300     03 3166-DAFAKT          PIC 9(8).                                    
001400*                                 FAKTURERINGSDATUM (≈≈≈≈MMDD)            
001500*                                 INVOICING DATE   (YYYYMMDD)             
001600     03 3166-IDDC            PIC X(2).                                    
001700*                                 IDENTIFIERARE LAGER                     
001800*                                 WAREHOUSE IDENTIFIER                    
001900     03 3166-IDDISTR         PIC S9(5)           COMP-3.                  
002000*                                 DISTRIKTNUMMER                          
002100*                                 DISTRICT NUMBER                         
002200     03 3166-IDUSER          PIC X(8).                                    
002300*                                 ANVƒNDARENS SƒKERHETS ID                
002400*                                 USER SECURITY-IDENTITY                  
002500     03 3166-KDBYTFAK        PIC X.                                       
002600*                                 STATUSKOD BYTESFAKTURA                  
002700*                                           1 = P≈G≈R                     
002800*                                           2 = KLAR                      
002900*                                           3 = KLAR,UTSKRIVEN            
003000*                                 STATUSCODE EXCH.INVOICE                 
003100     03 3166-VKORDBTO-FAKT   PIC S9(6)V9(1)      COMP-3.                  
003200*                                 ORDERVIKT BRUTTO PER FAKTURA            
003300*                                 ORDER WEIGHT GROSS PER INVOICE          
003400     03 3166-VLORDBTO-FAKT   PIC S9(4)V9(3)      COMP-3.                  
003500*                                 ORDERVOLYM BRUTTO PER FAKTURA           
003600*                                 ORDER VOLUME GROSS PER INVOICE          
003700*** END OF VILMAII-COPY LENGTH= 42 BYTES                                  
