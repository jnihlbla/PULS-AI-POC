000100 01  MOD-W6O11101.                                                        
000200*                                 MODCOPYTEXT TILL W60111.                
000300     03 MOD-IDTRANS          PIC X(4).                                    
000400*                                 BILDNUMMER                              
000500*                                 SCREEN NUMBER                           
000600     03 MOD-TEMFSFEL         PIC X(40).                                   
000700*                                 MFS FELMEDDELANDE                       
000800*                                 MFS ERROR MESSAGE                       
000900     03 MOD-IDLEVNR-IN       PIC X(5).                                    
001000*                                 LEVERANTÖRNUMMER                        
001100*                                 SUPPLIER NUMBER                         
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
002900*                                 SUPPLIER NUMBER                         
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
004900     03 MOD-FLGODK-IN-ATTR   PIC X(2).                                    
005000     03 MOD-FLGODK-IN        PIC X.                                       
005100     03 MOD-IDLBBET-UT       PIC X(12).                                   
005200*                                 LASTBÄRARBETECKNING                     
005300*                                 TRAILER NUMBER                          
005400     03 MOD-FLGODK-UT        PIC X.                                       
005500     03 MOD-UPDATE.                                                       
005600*                                 UPDATE                                  
005700        05 MOD-UPDATE        OCCURS 36 TIMES.                             
005800*                                 UPDATE                                  
005900           07 MOD-IDARTNR-IN-ATTR                                         
006000                             PIC X(2).                                    
006100*                                 MFS ATTRIBUTFÄLT                        
006200           07 MOD-IDARTNR-IN PIC X(8).                                    
006300*                                 ARTIKELNUMMER                           
006400*                                 PART NUMBER                             
006500        05 MOD-UPDATE        OCCURS 36 TIMES.                             
006600*                                 UPDATE                                  
006700           07 MOD-KVAVIS-IN-ATTR                                          
006800                             PIC X(2).                                    
006900*                                 MFS ATTRIBUTFÄLT                        
007000           07 MOD-KVAVIS-IN  PIC X(6).                                    
007100*                                 AVISERAT ANTAL                          
007200*                                 QUANTITY NOTIFIED                       
007300     03 MOD-FLKLIVIS-IN      PIC X.                                       
007400*                                 JA/NEJ-FLAGGA                           
007500     03 MOD-FLKLIVIS-UT      PIC X.                                       
007600*                                 JA/NEJ-FLAGGA                           
007700     03 MOD-IDLOPNRM-IN      PIC X(8).                                    
007800*                                 LÖPNUMMER MOTTAGNINGSRAPPORT            
007900*                                 (0VVDLLLLK)                             
008000*                                 SERIAL NO RECEIVING REPORT              
008100*                                 (0WWDLLLLC)                             
008200     03 MOD-IDLOPNRM-UT      PIC X(8).                                    
008300*                                 LÖPNUMMER MOTTAGNINGSRAPPORT            
008400*                                 (0VVDLLLLK)                             
008500*                                 SERIAL NO RECEIVING REPORT              
008600*                                 (0WWDLLLLC)                             
008700     03 MOD-TEMFSINF         PIC X(55).                                   
008800*                                 INFORMATIONSMEDDELANDE                  
008900*                                 INFORMATION MESSAGE                     
