000100 01  MOD-W6O11501.                                                        
000200*                                 MODCOPYTEXT TILL W60115.                
000300     03 MOD-IDTRANS          PIC X(4).                                    
000400*                                 BILDNUMMER                              
000500*                                 SCREEN NUMBER                           
000600     03 MOD-TEMFSFEL         PIC X(40).                                   
000700*                                 MFS FELMEDDELANDE                       
000800*                                 MFS ERROR MESSAGE                       
000900     03 MOD-IDLEVNR-IN       PIC X(5).                                    
001000*                                 LEVERANTÖRNUMMER                        
001100*                                 SUPPLIER NUMBER (VENDOR NUMBER)         
001200     03 MOD-IDFS-IN          PIC X(8).                                    
001300*                                 FÖLJESEDELSNUMMER ENL ODETTE            
001400*                                 ADVICE NOTE NUMBER ODETTE               
001500     03 MOD-TIAVIDAT-IN      PIC X(6).                                    
001600*                                 AVISERINGSDATUM (YYMMDD)                
001700*                                 ADVICE NOTE DATE                        
001800     03 MOD-IDLBBET-IN       PIC X(12).                                   
001900*                                 LASTBÄRARBETECKNING                     
002000*                                 TRAILER NUMBER                          
002100     03 MOD-ADINLOMR-PRT-IN  PIC X(4).                                    
002200*                                 PRINTERPLACERING                        
002300*                                 PLACE OF A PRINTER                      
002400     03 MOD-IDDC-IN          PIC X(2).                                    
002500*                                 IDENTIFIERARE LAGER                     
002600*                                 WAREHOUSE IDENTIFIER                    
002700     03 MOD-IDLEVNR-UT       PIC X(5).                                    
002800*                                 LEVERANTÖRNUMMER                        
002900*                                 SUPPLIER NUMBER (VENDOR NUMBER)         
003000     03 MOD-IDFS-UT          PIC X(8).                                    
003100*                                 FÖLJESEDELSNUMMER ENL ODETTE            
003200*                                 ADVICE NOTE NUMBER ODETTE               
003300     03 MOD-TIAVIDAT-UT      PIC X(6).                                    
003400*                                 AVISERINGSDATUM (YYMMDD)                
003500*                                 ADVICE NOTE DATE                        
003600     03 MOD-IDLBBET-UT       PIC X(12).                                   
003700*                                 LASTBÄRARBETECKNING                     
003800*                                 TRAILER NUMBER                          
003900     03 MOD-ADINLOMR-PRT-UT  PIC X(4).                                    
004000*                                 PRINTERPLACERING                        
004100*                                 PLACE OF A PRINTER                      
004200     03 MOD-IDDC-UT          PIC X(2).                                    
004300*                                 IDENTIFIERARE LAGER                     
004400*                                 WAREHOUSE IDENTIFIER                    
004500     03 MOD-IDLEVNR-ENTER    PIC X(5).                                    
004600*                                 LEVERANTÖRNUMMER                        
004700*                                 SUPPLIER NUMBER (VENDOR NUMBER)         
004800     03 MOD-IDLEVNR-NEXT     PIC X(5).                                    
004900*                                 LEVERANTÖRNUMMER                        
005000*                                 SUPPLIER NUMBER (VENDOR NUMBER)         
005100     03 MOD-IDFS-ENTER       PIC X(8).                                    
005200*                                 FÖLJESEDELSNUMMER ENL ODETTE            
005300*                                 ADVICE NOTE NUMBER ODETTE               
005400     03 MOD-IDFS-NEXT        PIC X(8).                                    
005500*                                 FÖLJESEDELSNUMMER ENL ODETTE            
005600*                                 ADVICE NOTE NUMBER ODETTE               
005700     03 MOD-TIAVIDAT-ENTER   PIC 9(6).                                    
005800*                                 AVISERINGSDATUM (YYMMDD)                
005900*                                 ADVICE NOTE DATE                        
006000     03 MOD-TIAVIDAT-NEXT    PIC 9(6).                                    
006100*                                 AVISERINGSDATUM (YYMMDD)                
006200*                                 ADVICE NOTE DATE                        
006300     03 MOD-IDLBBET-ENTER    PIC X(12).                                   
006400*                                 LASTBÄRARBETECKNING                     
006500*                                 TRAILER NUMBER                          
006600     03 MOD-IDLBBET-NEXT     PIC X(12).                                   
006700*                                 LASTBÄRARBETECKNING                     
006800*                                 TRAILER NUMBER                          
006900     03 MOD-OMSTART-INDX     PIC 9(2).                                    
007000     03 MOD-UPDATE           OCCURS 10 TIMES.                             
007100*                                 UPDATE                                  
007200        05 MOD-KDCMDVAL-RAD-ATTR                                          
007300                             PIC X(2).                                    
007400*                                 MFS ATTRIBUTFÄLT                        
007500        05 MOD-KDCMDVAL-RAD  PIC X(3).                                    
007600*                                 GENERELL KOMMANDOKOD                    
007700*                                 GENERAL COMMAND-CODE                    
007800     03 MOD-FILLER           OCCURS 10 TIMES.                             
007900*                                 UPDATE                                  
008000        05 MOD-IDLEVNR-RAD-ATTR                                           
008100                             PIC X(2).                                    
008200*                                 MFS ATTRIBUTFÄLT                        
008300        05 MOD-IDLEVNR-RAD   PIC X(5).                                    
008400*                                 LEVERANTÖRNUMMER                        
008500*                                 SUPPLIER NUMBER (VENDOR NUMBER)         
008600     03 MOD-FILLER           OCCURS 10 TIMES.                             
008700*                                 UPDATE                                  
008800        05 MOD-IDFS-RAD-ATTR PIC X(2).                                    
008900*                                 MFS ATTRIBUTFÄLT                        
009000        05 MOD-IDFS-RAD      PIC X(8).                                    
009100*                                 FÖLJESEDELSNUMMER ENL ODETTE            
009200*                                 ADVICE NOTE NUMBER ODETTE               
009300     03 MOD-TIAVIDAT-RAD     OCCURS 10 TIMES                              
009400                             PIC X(6).                                    
009500*                                 AVISERINGSDATUM (YYMMDD)                
009600*                                 ADVICE NOTE DATE                        
009700     03 MOD-IDLBBET-RAD      OCCURS 10 TIMES                              
009800                             PIC X(12).                                   
009900*                                 LASTBÄRARBETECKNING                     
010000*                                 TRAILER NUMBER                          
010100     03 MOD-KVPARTI-RAD      OCCURS 10 TIMES                              
010200                             PIC Z(6)9.                                   
010300     03 MOD-FILLER           OCCURS 10 TIMES.                             
010400*                                 UPDATE                                  
010500        05 MOD-FLFEL-RAD-ATTR                                             
010600                             PIC X(2).                                    
010700*                                 MFS ATTRIBUTFÄLT                        
010800        05 MOD-FLFEL-RAD     PIC X.                                       
010900*                                 ALLMÄN FELFLAGGA                        
011000*                                 GENERAL ERROR FLAG                      
011100     03 MOD-ADINLOMR-FB      OCCURS 10 TIMES                              
011200                             PIC X(4).                                    
011300*                                 INLEVERANSOMRÅDE                        
011400*                                 RECEIVING AREA                          
011500     03 MOD-IDSHIPM-RAD      OCCURS 10 TIMES                              
011600                             PIC Z(7).                                    
011700*                                 SKEPPNINGSNUMMER                        
011800*                                 SHIPMENT NO                             
011900     03 MOD-FILLER           OCCURS 5 TIMES.                              
012000*                                 UPDATE                                  
012100        05 MOD-ADINLOMR-LPL  PIC X(4).                                    
012200*                                 LOSSNINGSPLATS                          
012300*                                 UNLOADING AREA                          
012400        05 MOD-SUPROC-LPL    PIC 9(3).                                    
012500     03 MOD-IDLBBET-UPD-ATTR PIC X(2).                                    
012600*                                 MFS ATTRIBUTFÄLT                        
012700     03 MOD-IDLBBET-UPD      PIC X(12).                                   
012800*                                 LASTBÄRARBETECKNING                     
012900*                                 TRAILER NUMBER                          
013000     03 MOD-FLKLAR-UPD-ATTR  PIC X(2).                                    
013100*                                 MFS ATTRIBUTFÄLT                        
013200     03 MOD-FLKLAR-UPD       PIC X.                                       
013300*                                 AVSLUTNINGSMARKERING                    
013400*                                 FINISHED FLAG                           
013500     03 MOD-ADINLOMR-LPL-UPD-ATTR                                         
013600                             PIC X(2).                                    
013700*                                 MFS ATTRIBUTFÄLT                        
013800     03 MOD-ADINLOMR-LPL-UPD PIC X(4).                                    
013900*                                 LOSSNINGSPLATS                          
014000*                                 UNLOADING AREA                          
014100     03 MOD-KDRT-IN          PIC X(2).                                    
014200*                                 REDOVISNINGSTYP                         
014300*                                 TYPE OF ACCOUNTING                      
014400     03 MOD-KDRT-UT          PIC X(2).                                    
014500*                                 REDOVISNINGSTYP                         
014600*                                 TYPE OF ACCOUNTING                      
014700     03 MOD-FLKLIVIS-IN      PIC X.                                       
014800*                                 JA/NEJ-FLAGGA                           
014900     03 MOD-FLKLIVIS-UT      PIC X.                                       
015000*                                 JA/NEJ-FLAGGA                           
015100     03 MOD-IDLOPNRM-IN      PIC X(8).                                    
015200*                                 LÖPNUMMER MOTTAGNINGSRAPPORT            
015300*                                 (0VVDLLLLK)                             
015400*                                 SERIAL NO RECEIVING REPORT              
015500*                                 (0WWDLLLLC)                             
015600     03 MOD-IDLOPNRM-UT      PIC X(8).                                    
015700*                                 LÖPNUMMER MOTTAGNINGSRAPPORT            
015800*                                 (0VVDLLLLK)                             
015900*                                 SERIAL NO RECEIVING REPORT              
016000*                                 (0WWDLLLLC)                             
016100     03 MOD-TEMFSINF         PIC X(55).                                   
016200*                                 INFORMATIONSMEDDELANDE                  
016300*                                 INFORMATION MESSAGE                     
016400*** END OF VILMAII-COPY LENGTH= 927 BYTES                                 
