000100 01  REQU-WL0188I1.                                                       
000200*                                 REQUEST TO PGM WL0188                   
000300     03 REQU-IDDC-KEY        PIC X(2).                                    
000400*                                 IDENTIFIERARE LAGER                     
000500*                                 WAREHOUSE IDENTIFIER                    
000600     03 REQU-IDTRPTNR-KEY    PIC 9(3).                                    
000700*                                 TRANSPORTIDENTITET                      
000800*                                 TRANSPORT IDENTITY                      
000900     03 REQU-IDLBBET-KEY     PIC X(12).                                   
001000*                                 LASTBÄRARBETECKNING                     
001100*                                 TRAILER NUMBER                          
001200     03 REQU-KDFARLIG-KEY    PIC X.                                       
001300*                                 FARLIGT GODS-FLAGGA                     
001400*                                 DENGEROUS GOODS FLAG                    
001500     03 REQU-FLSKRIV-NU      PIC X.                                       
001600*                                 J/Y = SKRIV BEGÄRD LISTA                
001700*                                 J/Y = PRINT REPORT NOW                  
001800     03 REQU-IDSHIPM         PIC 9(7).                                    
001900*                                 SKEPPNINGSNUMMER                        
002000*                                 SHIPMENT NO                             
002100     03 REQU-FLAVSLUTA       PIC X.                                       
002200*                                 ALLMÄN FLAGGA                           
002300*                                 GENERAL FLAG                            
002400     03 REQU-KDTRTYP         PIC X.                                       
002500*                                 IMS TRANSAKTIONSTYP                     
002600*                                 IMS TRANSACTION TYPE                    
002700*** END OF VILMAII-COPY LENGTH= 28 BYTES                                  
