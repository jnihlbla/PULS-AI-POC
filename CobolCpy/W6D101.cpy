000100 01  INL-W6D101.                                                          
000200*                                 INLEVERANSREGISTER                      
000300*                                 SÄNDNINGS SEGMENT                       
000400*                                 FYSISK NYCKEL: W6D101KY:                
000500*                                 (IDDC, IDLEVNR, IDFS                    
000600*                                  TIAVIDAT)                              
000700     03 INL-IDDC             PIC X(2).                                    
000800*                                 IDENTIFIERARE LAGER                     
000900*                                 WAREHOUSE IDENTIFIER                    
001000     03 INL-IDLEVNR          PIC X(5).                                    
001100*                                 LEVERANTÖRNUMMER                        
001200*                                 SUPPLIER NUMBER (VENDOR NUMBER)         
001300     03 INL-IDFS             PIC X(8).                                    
001400*                                 FÖLJESEDELSNUMMER ENL ODETTE            
001500*                                 ADVICE NOTE NUMBER ODETTE               
001600     03 INL-TIAVIDAT         PIC S9(7)           COMP-3.                  
001700*                                 AVISERINGSDATUM (YYMMDD)                
001800*                                 ADVICE NOTE DATE                        
001900     03 INL-FLFEL            PIC X.                                       
002000*                                 ALLMÄN FELFLAGGA                        
002100*                                 GENERAL ERROR FLAG                      
002200     03 INL-IDANALYS         PIC X(12).                                   
002300*                                 ANALYSNUMMER                            
002400*                                 ANALYSIS NUMBER                         
002500     03 INL-IDARTNR          PIC S9(9)           COMP-3.                  
002600*                                 ARTIKELNUMMER                           
002700*                                 PART NUMBER                             
002800     03 INL-IDFTG            PIC 9(2).                                    
002900*                                 FÖRETAGSID EKONOM REDOVISNING           
003000*                                 COMPANY IDENTITY ACCOUNTING             
003100     03 INL-IDKONTO          PIC S9(11)          COMP-3.                  
003200*                                 KONTO                                   
003300*                                 ACCOUNT                                 
003400     03 INL-IDKST            PIC X(10).                                   
003500*                                 KOSTNADSSTÄLLE                          
003600*                                 COST CENTRE                             
003700     03 INL-IDLBBET          PIC X(12).                                   
003800*                                 LASTBÄRARBETECKNING                     
003900*                                 TRAILER NUMBER                          
004000     03 INL-KDINL            PIC X(3).                                    
004100*                                 TYP AV INLEVERANS                       
004200*                                 TYPE OF INC.DELIVERY                    
004300     03 INL-TIANKDAG         PIC S9(7)           COMP-3.                  
004400*                                 ANKOMSTDAG                              
004500*                                 RECEIVING DATE                          
004600     03 INL-TIINLMOT         PIC S9(7)           COMP-3.                  
004700*                                 MOTTAGNINGSDATUM   (ÅÅMMDD)             
004800*                                 RECEIVING DATE    (YYMMDD)              
004900     03 INL-IDSHIPM          PIC 9(7).                                    
005000*                                 SKEPPNINGSNUMMER                        
005100*                                 SHIPMENT NO                             
005200     03 INL-FILLER           PIC X(2).                                    
005300*** END OF VILMAII-COPY LENGTH= 87 BYTES                                  
