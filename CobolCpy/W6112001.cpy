000100 01  W61120.                                                              
000200*                                 URVAL FRÅN W6D1                         
000300*                                 AVISERINGAR                             
000400*                                 TIINLMOT = +0                           
000500*                                                         .               
000600*                                 SELECTED EXTRACT FROM W6D1              
000700*                                 DELIVERY NOTES                          
000800*                                 TIINLMOT = +0                           
000900*                                                         .               
001000     03 IDDC                 PIC X(2).                                    
001100*                                 IDENTIFIERARE LAGER                     
001200*                                 WAREHOUSE IDENTIFIER                    
001300     03 IDLEVNR              PIC X(5).                                    
001400*                                 LEVERANTÖRNUMMER                        
001500*                                 SUPPLIER NUMBER (VENDOR NUMBER)         
001600     03 IDFS                 PIC X(8).                                    
001700*                                 FÖLJESEDELSNUMMER ENL ODETTE            
001800*                                 ADVICE NOTE NUMBER ODETTE               
001900     03 TIAVIDAT             PIC S9(7)           COMP-3.                  
002000*                                 AVISERINGSDATUM (YYMMDD)                
002100*                                 ADVICE NOTE DATE                        
002200     03 KDINL                PIC X(3).                                    
002300*                                 TYP AV INLEVERANS                       
002400*                                 TYPE OF INC.DELIVERY                    
002500     03 TIANKDAG             PIC S9(7)           COMP-3.                  
002600*                                 ANKOMSTDAG                              
002700*                                 RECEIVING DATE                          
002800     03 ADLAGOMR             PIC S9(3)           COMP-3.                  
002900*                                 LAGEROMRÅDE                             
003000*                                 AREA                                    
003100     03 BEFT                 PIC S9(3)           COMP-3.                  
003200*                                 FÖRPACKNINGSTYP                         
003300*                                 PACKAGING TYPE                          
003400     03 IDARTNR              PIC S9(9)           COMP-3.                  
003500*                                 ARTIKELNUMMER                           
003600*                                 PART NUMBER                             
003700     03 FLFEL                PIC X.                                       
003800*                                 ALLMÄN FELFLAGGA                        
003900*                                 GENERAL ERROR FLAG                      
004000     03 KVAVIS               PIC S9(7)           COMP-3.                  
004100*                                 AVISERAT ANTAL                          
004200*                                 QUANTITY NOTIFIED                       
004300     03 ADINLOMR             PIC X(4).                                    
004400*                                 INLEVERANSOMRÅDE                        
004500*                                 RECEIVING AREA                          
004600*** END OF VILMAII-COPY LENGTH= 44 BYTES                                  
