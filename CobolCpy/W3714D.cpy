000100 01  W3714D.                                                              
000200*                                 BYTESUPPFÖLJNINGEN, UPPGIFTER           
000300*                                                                         
000400*                                 OM ALLA BYTESRAPPORTER I STATUS         
000500*                                  FYRA                                   
000600*                                                                         
000700     03 IDDC                 PIC X(2).                                    
000800*                                 IDENTIFIERARE LAGER                     
000900*                                 WAREHOUSE IDENTIFIER                    
001000     03 KDBYTSTA             PIC X.                                       
001100*                                 STATUSKOD BYTESOBJEKT                   
001200*                                 STATUSCODE EXCH CORES                   
001300     03 FLBYTGAR             PIC X.                                       
001400*                                 GARANTI RAPPORT FLAGGA                  
001500*                                 Y = GARANTI                             
001600*                                 N = EJ GARANTI                          
001700*                                 WARRANTY FLAG                           
001800     03 KVARBDAG             PIC S9(3)           COMP-3.                  
001900*                                 ANTAL ARBETSDAGAR                       
002000*                                 NUMBER OF WORKING DAYS                  
002100     03 KVRETUR              PIC S9(7)           COMP-3.                  
002200*                                 ANTAL I RETUR                           
002300*                                 QUANTITY IN RETURN                      
002400*** END OF VILMAII-COPY LENGTH= 10 BYTES                                  
