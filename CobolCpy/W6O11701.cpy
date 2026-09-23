000100 01  MOD-W6O11701.                                                        
000200*                                 MODCOPYTEXT TILL W60117.                
000300     03 MOD-IDTRANS          PIC X(4).                                    
000400*                                 BILDNUMMER                              
000500*                                 SCREEN NUMBER                           
000600     03 MOD-TEMFSFEL         PIC X(40).                                   
000700*                                 MFS FELMEDDELANDE                       
000800*                                 MFS ERROR MESSAGE                       
000900     03 MOD-IDDC-IN-ATTR     PIC X(2).                                    
001000*                                 MFS ATTRIBUTFÄLT                        
001100     03 MOD-IDDC-IN          PIC X(2).                                    
001200*                                 IDENTIFIERARE LAGER                     
001300*                                 WAREHOUSE IDENTIFIER                    
001400     03 MOD-IDDC-UT-ATTR     PIC X(2).                                    
001500*                                 MFS ATTRIBUTFÄLT                        
001600     03 MOD-IDDC-UT          PIC X(2).                                    
001700*                                 IDENTIFIERARE LAGER                     
001800*                                 WAREHOUSE IDENTIFIER                    
001900     03 MOD-TIAVIDAT-ATTR    PIC X(2).                                    
002000*                                 MFS ATTRIBUTFÄLT                        
002100     03 MOD-TIAVIDAT         PIC X(6).                                    
002200*                                 AVISERINGSDATUM (YYMMDD)                
002300*                                 ADVICE NOTE DATE                        
002400     03 MOD-R34POST          OCCURS 14 TIMES.                             
002500        05 MOD-IDARTNR-ATTR  PIC X(2).                                    
002600*                                 MFS ATTRIBUTFÄLT                        
002700        05 MOD-IDARTNR       PIC X(8).                                    
002800*                                 ARTIKELNUMMER                           
002900*                                 PART NUMBER                             
003000        05 MOD-IDLEVNR-ATTR  PIC X(2).                                    
003100*                                 MFS ATTRIBUTFÄLT                        
003200        05 MOD-IDLEVNR       PIC X(5).                                    
003300*                                 LEVERANTÖRNUMMER                        
003400*                                 SUPPLIER NUMBER (VENDOR NUMBER)         
003500        05 MOD-KDRT-ATTR     PIC X(2).                                    
003600*                                 MFS ATTRIBUTFÄLT                        
003700        05 MOD-KDRT          PIC X(2).                                    
003800*                                 REDOVISNINGSTYP                         
003900*                                 TYPE OF ACCOUNTING                      
004000        05 MOD-IDKONTO-ATTR  PIC X(2).                                    
004100*                                 MFS ATTRIBUTFÄLT                        
004200        05 MOD-IDKONTO       PIC X(10).                                   
004300*                                 KONTO                                   
004400*                                 ACCOUNT                                 
004500        05 MOD-IDAVINR-ATTR  PIC X(2).                                    
004600*                                 MFS ATTRIBUTFÄLT                        
004700        05 MOD-IDAVINR       PIC X(7).                                    
004800*                                 AVI-NUMMER                              
004900*                                 ADVICE NOTE NUMBER                      
005000        05 MOD-KVAVIS-ATTR   PIC X(2).                                    
005100*                                 MFS ATTRIBUTFÄLT                        
005200        05 MOD-KVAVIS        PIC -(6)9.                                   
005300*                                 AVISERAT ANTAL                          
005400*                                 QUANTITY NOTIFIED                       
005500        05 MOD-IDARTNR-FROM-ATTR                                          
005600                             PIC X(2).                                    
005700*                                 MFS ATTRIBUTFÄLT                        
005800        05 MOD-IDARTNR-FROM  PIC 9(8).                                    
005900*                                 ARTIKELNUMMER                           
006000*                                 PART NUMBER                             
006100        05 MOD-IDANALYS-ATTR PIC X(2).                                    
006200*                                 MFS ATTRIBUTFÄLT                        
006300        05 MOD-IDANALYS      PIC X(12).                                   
006400*                                 ANALYSNUMMER                            
006500*                                 ANALYSIS NUMBER                         
006600        05 MOD-IDKST-ATTR    PIC X(2).                                    
006700*                                 MFS ATTRIBUTFÄLT                        
006800        05 MOD-IDKST         PIC X(10).                                   
006900*                                 KOSTNADSSTÄLLE                          
007000*                                 COST CENTRE                             
007100     03 MOD-TEMFSINF         PIC X(55).                                   
007200*                                 INFORMATIONSMEDDELANDE                  
007300*                                 INFORMATION MESSAGE                     
007400*** END OF VILMAII-COPY LENGTH= 1333 BYTES                                
