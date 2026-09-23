000100 01  MOD-W6O11801.                                                        
000200*                                 MODCOPYTEXT TILL W60118.                
000300     03 MOD-IDTRANS          PIC X(4).                                    
000400*                                 BILDNUMMER                              
000500*                                 SCREEN NUMBER                           
000600     03 MOD-TEMFSFEL         PIC X(40).                                   
000700*                                 MFS FELMEDDELANDE                       
000800*                                 MFS ERROR MESSAGE                       
000900     03 MOD-IDLOPNRM-IN      PIC X(8).                                    
001000*                                 LÖPNUMMER MOTTAGNINGSRAPPORT            
001100*                                 (0VVDLLLLK)                             
001200*                                 SERIAL NO RECEIVING REPORT              
001300*                                 (0WWDLLLLC)                             
001400     03 MOD-IDDC-IN          PIC X(2).                                    
001500*                                 IDENTIFIERARE LAGER                     
001600*                                 WAREHOUSE IDENTIFIER                    
001700     03 MOD-IDLOPNRM-UT      PIC X(8).                                    
001800*                                 LÖPNUMMER MOTTAGNINGSRAPPORT            
001900*                                 (0VVDLLLLK)                             
002000*                                 SERIAL NO RECEIVING REPORT              
002100*                                 (0WWDLLLLC)                             
002200     03 MOD-IDDC-UT          PIC X(2).                                    
002300*                                 IDENTIFIERARE LAGER                     
002400*                                 WAREHOUSE IDENTIFIER                    
002500     03 MOD-IDRADNR-ENTER    PIC 9(4).                                    
002600*                                 RADNUMMER                               
002700*                                 LINE NO                                 
002800     03 MOD-IDRADNR-NEXT     PIC 9(4).                                    
002900*                                 RADNUMMER                               
003000*                                 LINE NO                                 
003100     03 MOD-IDARTNR          PIC Z(7)9.                                   
003200*                                 ARTIKELNUMMER                           
003300*                                 PART NUMBER                             
003400     03 MOD-KVAVIS           PIC Z(5)9.                                   
003500*                                 AVISERAT ANTAL                          
003600*                                 QUANTITY NOTIFIED                       
003700     03 MOD-BEART            PIC X(25).                                   
003800*                                 ARTIKELBENÄMNING                        
003900*                                 PART DESCRIPTION                        
004000     03 MOD-KDSORT           PIC X(2).                                    
004100*                                 SORT-KOD                                
004200*                                 UNIT OF MEASURE                         
004300     03 MOD-BEFT             PIC Z9.                                      
004400*                                 FÖRPACKNINGSTYP                         
004500*                                 PACKAGING TYPE                          
004600     03 MOD-KVRAPP           PIC Z(5)9.                                   
004700*                                 DELRAPPORTERAT ANTAL                    
004800*                                 PARTIAL REPORTED QUANTITY               
004900     03 MOD-BEFARLIG         PIC X(14).                                   
005000     03 MOD-RADER.                                                        
005100*                                 RADER                                   
005200        05 MOD-INKLATTR      OCCURS 12 TIMES.                             
005300*                                 ATTR + FÄLT                             
005400           07 MOD-ADINLOMR-UPD-ATTR                                       
005500                             PIC X(2).                                    
005600*                                 MFS ATTRIBUTFÄLT                        
005700           07 MOD-ADINLOMR-UPD                                            
005800                             PIC X(4).                                    
005900*                                 INLEVERANSOMRÅDE                        
006000*                                 RECEIVING AREA                          
006100        05 MOD-IDRADNR       OCCURS 12 TIMES                              
006200                             PIC Z(3)9.                                   
006300*                                 RADNUMMER                               
006400*                                 LINE NO                                 
006500        05 MOD-KVINLART      OCCURS 12 TIMES                              
006600                             PIC -(6)9.                                   
006700*                                 ANTAL I PARTIRAD                        
006800*                                 QTY/LINE IN A LOT                       
006900        05 MOD-ADINLOMR      OCCURS 12 TIMES                              
007000                             PIC X(4).                                    
007100*                                 INLEVERANSOMRÅDE                        
007200*                                 RECEIVING AREA                          
007300        05 MOD-KDINLSTA      OCCURS 12 TIMES                              
007400                             PIC X(3).                                    
007500*                                 SYSTEMSTATUS INLEVERANS                 
007600*                                 SYSTEM STATUS RECEIVING                 
007700        05 MOD-IDLEVNR-KOLLI OCCURS 12 TIMES                              
007800                             PIC X(5).                                    
007900*                                 LEVERANTÖRNUMMER KOLLI                  
008000*                                 SUPPLIER NUMBER CASE                    
008100        05 MOD-IDOKOLLI      OCCURS 12 TIMES                              
008200                             PIC Z(8)9.                                   
008300*                                 ODETTE KOLLINUMMER                      
008400*                                 ODETTE CASE NUMBER                      
008500        05 MOD-STATUS-TEXT   OCCURS 12 TIMES                              
008600                             PIC X(25).                                   
008700     03 MOD-IDLEVNR-IN       PIC X(5).                                    
008800*                                 LEVERANTÖRNUMMER                        
008900*                                 SUPPLIER NUMBER (VENDOR NUMBER)         
009000     03 MOD-IDLEVNR-UT       PIC X(5).                                    
009100*                                 LEVERANTÖRNUMMER                        
009200*                                 SUPPLIER NUMBER (VENDOR NUMBER)         
009300     03 MOD-IDFS-IN          PIC X(8).                                    
009400*                                 FÖLJESEDELSNUMMER ENL ODETTE            
009500*                                 ADVICE NOTE NUMBER ODETTE               
009600     03 MOD-IDFS-UT          PIC X(8).                                    
009700*                                 FÖLJESEDELSNUMMER ENL ODETTE            
009800*                                 ADVICE NOTE NUMBER ODETTE               
009900     03 MOD-TIAVIDAT-IN      PIC X(6).                                    
010000*                                 AVISERINGSDATUM (YYMMDD)                
010100*                                 ADVICE NOTE DATE                        
010200     03 MOD-TIAVIDAT-UT      PIC X(6).                                    
010300*                                 AVISERINGSDATUM (YYMMDD)                
010400*                                 ADVICE NOTE DATE                        
010500     03 MOD-ADINLOMR-PRT-IN  PIC X(4).                                    
010600*                                 PRINTERPLACERING                        
010700*                                 PLACE OF A PRINTER                      
010800     03 MOD-ADINLOMR-PRT-UT  PIC X(4).                                    
010900*                                 PRINTERPLACERING                        
011000*                                 PLACE OF A PRINTER                      
011100     03 MOD-KDRT-IN          PIC X(2).                                    
011200*                                 REDOVISNINGSTYP                         
011300*                                 TYPE OF ACCOUNTING                      
011400     03 MOD-KDRT-UT          PIC X(2).                                    
011500*                                 REDOVISNINGSTYP                         
011600*                                 TYPE OF ACCOUNTING                      
011700     03 MOD-IDLBBET-IN       PIC X(12).                                   
011800*                                 LASTBÄRARBETECKNING                     
011900*                                 TRAILER NUMBER                          
012000     03 MOD-IDLBBET-UT       PIC X(12).                                   
012100*                                 LASTBÄRARBETECKNING                     
012200*                                 TRAILER NUMBER                          
012300     03 MOD-FLKLIVIS-IN      PIC X.                                       
012400*                                 JA/NEJ-FLAGGA                           
012500     03 MOD-FLKLIVIS-UT      PIC X.                                       
012600*                                 JA/NEJ-FLAGGA                           
012700     03 MOD-TEMFSINF         PIC X(55).                                   
012800*                                 INFORMATIONSMEDDELANDE                  
012900*                                 INFORMATION MESSAGE                     
013000*** END OF VILMAII-COPY LENGTH= 1022 BYTES                                
