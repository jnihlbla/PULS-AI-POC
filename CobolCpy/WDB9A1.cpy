000100 01  SEQA-WDB9A1.                                                         
000200*                                 PARAMETERREGISTER TILL DOKUMENT         
000300*                                 UTSKRIFT                                
000400*                                 DISTRIKT INGÅNG                         
000500*                                 SEKUNDÄRT INDEX TILL WDB901             
000600*                                 FYSISK NYCKEL: WDB9A1KY                 
000700*                                 (IDDISTR + IDDC + IDKUND-GRP +          
000800*                                  IDDC-REC)                              
000900*                                 SEKUNDÄR NYCKEL: WDB9ASEQ               
001000*                                 (IDDISTR)                               
001100     03 SEQA-IDDISTR         PIC S9(5)           COMP-3.                  
001200*                                 DISTRIKTNUMMER                          
001300*                                 DISTRICT NUMBER                         
001400     03 SEQA-IDDC            PIC X(2).                                    
001500*                                 IDENTIFIERARE LAGER                     
001600*                                 WAREHOUSE IDENTIFIER                    
001700     03 SEQA-IDKUND-GRP.                                                  
001800*                                 KUNDNUMMER ELLER LEVERANTÖRSNR          
001900*                                 CUSTOMER OR SUPPLIER NO.                
002000        05 SEQA-IDKUND       PIC X(10).                                   
002100*                                 KUND/LEV ID                             
002200*                                 CUSTOMER/SUPPL ID                       
002300        05 SEQA-IDKUNDNR-FILLER REDEFINES SEQA-IDKUND.                    
002400           07 SEQA-IDKUNDNR  PIC 9(6).                                    
002500*                                 KUNDNUMMER                              
002600*                                 CUSTOMER NO                             
002700           07 FILLER         PIC X(4).                                    
002800        05 SEQA-IDLEVNR-FILLER REDEFINES SEQA-IDKUND.                     
002900           07 SEQA-IDLEVNR   PIC X(5).                                    
003000*                                 LEVERANTÖRNUMMER                        
003100*                                 SUPPLIER NUMBER (VENDOR NUMBER)         
003200           07 FILLER         PIC X(5).                                    
003300     03 SEQA-IDDC-REC        PIC X(2).                                    
003400*                                 MOTTAGANDE LAGER                        
003500*                                 RECEIVING WAREHOUSE                     
003600     03 SEQA-KVDAGAR         PIC S9(3)           COMP-3.                  
003700*                                 ANTAL DAGAR                             
003800     03 SEQA-IDWDB901        PIC X(17).                                   
003900*                                 NYCKEL TILL WDB901                      
004000*                                 KEY TO WDB901                           
004100*** END OF VILMAII-COPY LENGTH= 36 BYTES                                  
