000100 01  MOD-W6O11301.                                                        
000200*                                 MODCOPYTEXT TILL W60113.                
000300     03 MOD-IDTRANS          PIC X(4).                                    
000400*                                 BILDNUMMER                              
000500*                                 SCREEN NUMBER                           
000600     03 MOD-TEMFSFEL         PIC X(40).                                   
000700*                                 MFS FELMEDDELANDE                       
000800*                                 MFS ERROR MESSAGE                       
000900     03 MOD-IDLEVNR-IN       PIC X(5).                                    
001000*                                 LEVERANTÖRNUMMER                        
001100*                                 SUPPLIER NUMBER                         
001200     03 MOD-IDFS-IN          PIC X(8).                                    
001300*                                 FÖLJESEDELSNUMMER ENL ODETTE            
001400*                                 ADVICE NOTE NUMBER ODETTE               
001500     03 MOD-TIAVIDAT-IN      PIC X(6).                                    
001600*                                 AVISERINGSDATUM (YYMMDD)                
001700*                                 ADVICE NOTE DATE                        
001800     03 MOD-ADINLOMR-PRT-IN  PIC X(4).                                    
001900*                                 PRINTERPLACERING                        
002000*                                 PLACE OF A PRINTER                      
002100     03 MOD-IDDC-IN          PIC X(2).                                    
002200*                                 IDENTIFIERARE LAGER                     
002300*                                 WAREHOUSE IDENTIFIER                    
002400     03 MOD-IDLEVNR-UT       PIC X(5).                                    
002500*                                 LEVERANTÖRNUMMER                        
002600*                                 SUPPLIER NUMBER                         
002700     03 MOD-IDFS-UT          PIC X(8).                                    
002800*                                 FÖLJESEDELSNUMMER ENL ODETTE            
002900*                                 ADVICE NOTE NUMBER ODETTE               
003000     03 MOD-TIAVIDAT-UT      PIC X(6).                                    
003100*                                 AVISERINGSDATUM (YYMMDD)                
003200*                                 ADVICE NOTE DATE                        
003300     03 MOD-ADINLOMR-PRT-UT  PIC X(4).                                    
003400*                                 PRINTERPLACERING                        
003500*                                 PLACE OF A PRINTER                      
003600     03 MOD-IDDC-UT          PIC X(2).                                    
003700*                                 IDENTIFIERARE LAGER                     
003800*                                 WAREHOUSE IDENTIFIER                    
003900     03 MOD-IDLBBET-IN-ATTR  PIC X(2).                                    
004000     03 MOD-IDLBBET-IN       PIC X(12).                                   
004100*                                 LASTBÄRARBETECKNING                     
004200*                                 TRAILER NUMBER                          
004300     03 MOD-FLGODK-IN-ATTR   PIC X(2).                                    
004400     03 MOD-FLGODK-IN        PIC X.                                       
004500     03 MOD-IDLBBET-UT       PIC X(12).                                   
004600*                                 LASTBÄRARBETECKNING                     
004700*                                 TRAILER NUMBER                          
004800     03 MOD-FLGODK-UT        PIC X.                                       
004900     03 MOD-UPDATE.                                                       
005000*                                 UPDATE                                  
005100        05 MOD-UPDATE        OCCURS 12 TIMES.                             
005200*                                 UPDATE                                  
005300           07 MOD-IDLEVNR-RAD-ATTR                                        
005400                             PIC X(2).                                    
005500*                                 MFS ATTRIBUTFÄLT                        
005600           07 MOD-IDLEVNR-RAD                                             
005700                             PIC X(5).                                    
005800*                                 LEVERANTÖRNUMMER                        
005900*                                 SUPPLIER NUMBER                         
006000        05 MOD-UPDATE        OCCURS 12 TIMES.                             
006100*                                 UPDATE                                  
006200           07 MOD-KDRT-RAD-ATTR                                           
006300                             PIC X(2).                                    
006400*                                 MFS ATTRIBUTFÄLT                        
006500           07 MOD-KDRT-RAD   PIC X(2).                                    
006600*                                 REDOVISNINGSTYP                         
006700*                                 TYPE OF ACCOUNTING                      
006800        05 MOD-UPDATE        OCCURS 12 TIMES.                             
006900*                                 UPDATE                                  
007000           07 MOD-IDFS-RAD-ATTR                                           
007100                             PIC X(2).                                    
007200*                                 MFS ATTRIBUTFÄLT                        
007300           07 MOD-IDFS-RAD   PIC X(8).                                    
007400*                                 FÖLJESEDELSNUMMER ENL ODETTE            
007500*                                 ADVICE NOTE NUMBER ODETTE               
007600        05 MOD-UPDATE        OCCURS 12 TIMES.                             
007700*                                 UPDATE                                  
007800           07 MOD-TIAVIDAT-RAD-ATTR                                       
007900                             PIC X(2).                                    
008000*                                 MFS ATTRIBUTFÄLT                        
008100           07 MOD-TIAVIDAT-RAD                                            
008200                             PIC X(6).                                    
008300*                                 AVISERINGSDATUM (YYMMDD)                
008400*                                 ADVICE NOTE DATE                        
008500        05 MOD-UPDATE        OCCURS 12 TIMES.                             
008600*                                 UPDATE                                  
008700           07 MOD-IDARTNR-RAD-ATTR                                        
008800                             PIC X(2).                                    
008900*                                 MFS ATTRIBUTFÄLT                        
009000           07 MOD-IDARTNR-RAD                                             
009100                             PIC X(8).                                    
009200*                                 ARTIKELNUMMER                           
009300*                                 PART NUMBER                             
009400        05 MOD-UPDATE        OCCURS 12 TIMES.                             
009500*                                 UPDATE                                  
009600           07 MOD-KVAVIS-RAD-ATTR                                         
009700                             PIC X(2).                                    
009800*                                 MFS ATTRIBUTFÄLT                        
009900           07 MOD-KVAVIS-RAD PIC X(6).                                    
010000*                                 AVISERAT ANTAL                          
010100*                                 QUANTITY NOTIFIED                       
010200     03 MOD-KDRT-IN          PIC X(2).                                    
010300*                                 REDOVISNINGSTYP                         
010400*                                 TYPE OF ACCOUNTING                      
010500     03 MOD-KDRT-UT          PIC X(2).                                    
010600*                                 REDOVISNINGSTYP                         
010700*                                 TYPE OF ACCOUNTING                      
010800     03 MOD-FLKLIVIS-IN      PIC X.                                       
010900*                                 JA/NEJ-FLAGGA                           
011000     03 MOD-FLKLIVIS-UT      PIC X.                                       
011100*                                 JA/NEJ-FLAGGA                           
011200     03 MOD-IDLOPNRM-IN      PIC X(8).                                    
011300*                                 LÖPNUMMER MOTTAGNINGSRAPPORT            
011400*                                 (0VVDLLLLK)                             
011500*                                 SERIAL NO RECEIVING REPORT              
011600*                                 (0WWDLLLLC)                             
011700     03 MOD-IDLOPNRM-UT      PIC X(8).                                    
011800*                                 LÖPNUMMER MOTTAGNINGSRAPPORT            
011900*                                 (0VVDLLLLK)                             
012000*                                 SERIAL NO RECEIVING REPORT              
012100*                                 (0WWDLLLLC)                             
012200     03 MOD-TEMFSINF         PIC X(55).                                   
012300*                                 INFORMATIONSMEDDELANDE                  
012400*                                 INFORMATION MESSAGE                     
