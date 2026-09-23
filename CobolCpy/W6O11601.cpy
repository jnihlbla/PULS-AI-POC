000100 01  MOD-W6O11601.                                                        
000200*                                 MODCOPYTEXT TILL W60116.                
000300     03 MOD-IDTRANS          PIC X(4).                                    
000400*                                 BILDNUMMER                              
000500*                                 SCREEN NUMBER                           
000600     03 MOD-TEMFSFEL         PIC X(40).                                   
000700*                                 MFS FELMEDDELANDE                       
000800*                                 MFS ERROR MESSAGE                       
000900     03 MOD-IDLEVNR-IN       PIC X(5).                                    
001000*                                 LEVERANTÖRNUMMER                        
001100*                                 SUPPLIER NUMBER (VENDOR NUMBER)         
001200     03 MOD-KDRT-IN          PIC X(2).                                    
001300*                                 REDOVISNINGSTYP                         
001400*                                 TYPE OF ACCOUNTING                      
001500     03 MOD-IDFS-IN          PIC X(8).                                    
001600*                                 FÖLJESEDELSNUMMER ENL ODETTE            
001700*                                 ADVICE NOTE NUMBER ODETTE               
001800     03 MOD-TIAVIDAT-IN      PIC X(6).                                    
001900*                                 AVISERINGSDATUM (YYMMDD)                
002000*                                 ADVICE NOTE DATE                        
002100     03 MOD-ADINLOMR-PRT-IN  PIC X(4).                                    
002200*                                 PRINTERPLACERING                        
002300*                                 PLACE OF A PRINTER                      
002400     03 MOD-IDDC-IN          PIC X(2).                                    
002500*                                 IDENTIFIERARE LAGER                     
002600*                                 WAREHOUSE IDENTIFIER                    
002700     03 MOD-IDLEVNR-UT       PIC X(5).                                    
002800*                                 LEVERANTÖRNUMMER                        
002900*                                 SUPPLIER NUMBER (VENDOR NUMBER)         
003000     03 MOD-KDRT-UT          PIC X(2).                                    
003100*                                 REDOVISNINGSTYP                         
003200*                                 TYPE OF ACCOUNTING                      
003300     03 MOD-IDFS-UT          PIC X(8).                                    
003400*                                 FÖLJESEDELSNUMMER ENL ODETTE            
003500*                                 ADVICE NOTE NUMBER ODETTE               
003600     03 MOD-TIAVIDAT-UT      PIC X(6).                                    
003700*                                 AVISERINGSDATUM (YYMMDD)                
003800*                                 ADVICE NOTE DATE                        
003900     03 MOD-ADINLOMR-PRT-UT  PIC X(4).                                    
004000*                                 PRINTERPLACERING                        
004100*                                 PLACE OF A PRINTER                      
004200     03 MOD-IDDC-UT          PIC X(2).                                    
004300*                                 IDENTIFIERARE LAGER                     
004400*                                 WAREHOUSE IDENTIFIER                    
004500     03 MOD-IDLBBET-IN-ATTR  PIC X(2).                                    
004600     03 MOD-IDLBBET-IN       PIC X(12).                                   
004700*                                 LASTBÄRARBETECKNING                     
004800*                                 TRAILER NUMBER                          
004900     03 MOD-IDFTG-IN-ATTR    PIC X(2).                                    
005000     03 MOD-IDFTG-IN         PIC X(2).                                    
005100*                                 FÖRETAGSID EKONOM REDOVISNING           
005200*                                 COMPANY IDENTITY ACCOUNTING             
005300     03 MOD-IDKONTO-IN-ATTR  PIC X(2).                                    
005400     03 MOD-IDKONTO-IN       PIC Z(9)9.                                   
005500*                                 KONTO                                   
005600*                                 ACCOUNT                                 
005700     03 MOD-IDANALYS-IN-ATTR PIC X(2).                                    
005800     03 MOD-IDANALYS-IN      PIC X(12).                                   
005900*                                 ANALYSNUMMER                            
006000*                                 ANALYSIS NUMBER                         
006100     03 MOD-IDKST-IN-ATTR    PIC X(2).                                    
006200     03 MOD-IDKST-IN         PIC X(10).                                   
006300*                                 KOSTNADSSTÄLLE                          
006400*                                 COST CENTRE                             
006500     03 MOD-FLGODK-IN-ATTR   PIC X(2).                                    
006600     03 MOD-FLGODK-IN        PIC X.                                       
006700     03 MOD-IDLBBET-UT       PIC X(12).                                   
006800*                                 LASTBÄRARBETECKNING                     
006900*                                 TRAILER NUMBER                          
007000     03 MOD-IDFTG-UT         PIC X(2).                                    
007100*                                 FÖRETAGSID EKONOM REDOVISNING           
007200*                                 COMPANY IDENTITY ACCOUNTING             
007300     03 MOD-IDKONTO-UT       PIC Z(9)9.                                   
007400*                                 KONTO                                   
007500*                                 ACCOUNT                                 
007600     03 MOD-IDANALYS-UT      PIC X(12).                                   
007700*                                 ANALYSNUMMER                            
007800*                                 ANALYSIS NUMBER                         
007900     03 MOD-IDKST-UT         PIC X(10).                                   
008000*                                 KOSTNADSSTÄLLE                          
008100*                                 COST CENTRE                             
008200     03 MOD-FLGODK-UT        PIC X.                                       
008300     03 MOD-UPDATE.                                                       
008400*                                 UPDATE                                  
008500        05 MOD-UPDATE        OCCURS 36 TIMES.                             
008600*                                 UPDATE                                  
008700           07 MOD-IDARTNR-IN-ATTR                                         
008800                             PIC X(2).                                    
008900*                                 MFS ATTRIBUTFÄLT                        
009000           07 MOD-IDARTNR-IN PIC X(8).                                    
009100*                                 ARTIKELNUMMER                           
009200*                                 PART NUMBER                             
009300        05 MOD-UPDATE        OCCURS 36 TIMES.                             
009400*                                 UPDATE                                  
009500           07 MOD-KVAVIS-IN-ATTR                                          
009600                             PIC X(2).                                    
009700*                                 MFS ATTRIBUTFÄLT                        
009800           07 MOD-KVAVIS-IN  PIC X(6).                                    
009900*                                 AVISERAT ANTAL                          
010000*                                 QUANTITY NOTIFIED                       
010100     03 MOD-FLKLIVIS-IN      PIC X.                                       
010200*                                 JA/NEJ-FLAGGA                           
010300     03 MOD-FLKLIVIS-UT      PIC X.                                       
010400*                                 JA/NEJ-FLAGGA                           
010500     03 MOD-IDLOPNRM-IN      PIC X(8).                                    
010600*                                 LÖPNUMMER MOTTAGNINGSRAPPORT            
010700*                                 (0VVDLLLLK)                             
010800*                                 SERIAL NO RECEIVING REPORT              
010900*                                 (0WWDLLLLC)                             
011000     03 MOD-IDLOPNRM-UT      PIC X(8).                                    
011100*                                 LÖPNUMMER MOTTAGNINGSRAPPORT            
011200*                                 (0VVDLLLLK)                             
011300*                                 SERIAL NO RECEIVING REPORT              
011400*                                 (0WWDLLLLC)                             
011500     03 MOD-TEMFSINF         PIC X(55).                                   
011600*                                 INFORMATIONSMEDDELANDE                  
011700*                                 INFORMATION MESSAGE                     
011800*** END OF VILMAII-COPY LENGTH= 925 BYTES                                 
