000100 01  W57105.                                                              
000200*                                 UPDATE BUFFER BALANCE                   
000300*                                                                         
000400     03 WDJ701.                                                           
000500*                                 ACS INVENTERINGS REGISTER               
000600*                                 FYSISK NYCKEL: WDJ701KY                 
000700*                                 (IDDC + IDARTNR)                        
000800        05 IDDC              PIC X(2).                                    
000900*                                 IDENTIFIERARE LAGER                     
001000*                                 WAREHOUSE IDENTIFIER                    
001100        05 IDARTNR           PIC S9(9)           COMP-3.                  
001200*                                 ARTIKELNUMMER                           
001300*                                 PART NUMBER                             
001400        05 BEART             PIC X(25).                                   
001500*                                 ARTIKELBENÄMNING                        
001600*                                 PART DESCRIPTION                        
001700        05 ADLAGOMR          PIC 9(2).                                    
001800*                                 LAGEROMRÅDE                             
001900*                                 AREA                                    
002000        05 ADGANG            PIC 9(2).                                    
002100*                                 GÅNG                                    
002200*                                 AISLE                                   
002300        05 ADPLATS           PIC 9(5).                                    
002400*                                 LAGERPLATSNUMMER                        
002500*                                 LOCATION                                
002600        05 KDPRODSL          PIC S9(3)           COMP-3.                  
002700*                                 PRODUKTSLAG                             
002800*                                 PRODUCT GROUP                           
002900        05 KDPSLLOC          PIC 9(2).                                    
003000*                                 PRODUKTSLAG LOKALT                      
003100*                                 PRODUCT GROUP LOCAL                     
003200        05 KVLS              PIC S9(7)           COMP-3.                  
003300*                                 LAGERSALDO                              
003400*                                 STOCK BALANCE                           
003500        05 KVPCOUNT          PIC S9(7)           COMP-3.                  
003600*                                 RÄKNAT   ANTAL                          
003700*                                 QUANTITY COUNTED                        
003800        05 KVRCOUNT          PIC S9(7)           COMP-3.                  
003900*                                 OMRÄKNAT ANTAL                          
004000*                                 QUANTITY RECOUNTED                      
004100        05 KVTCOUNT          PIC S9(7)           COMP-3.                  
004200*                                 OMRÄKNAT ANTAL 3:E GGN                  
004300*                                 QUANTITY RECOUNTED THIRD TIME           
004400        05 FLKLAR            PIC X.                                       
004500*                                 AVSLUTNINGSMARKERING                    
004600*                                 FINISHED FLAG                           
004700        05 IDACSNR-P         PIC S9(7)           COMP-3.                  
004800*                                 ACS LISTNR FÖR RÄKNING INV.             
004900*                                 ACS REPORT NO PRIMECOUNT PARTS          
005000        05 IDACSNR-R         PIC S9(7)           COMP-3.                  
005100*                                 ACS LISTNR FÖR OMRÄKNING INV.           
005200*                                 ACS REPORT NO FOR RECOUNT PARTS         
005300        05 IDACSNR-T         PIC S9(7)           COMP-3.                  
005400*                                 ACS LISTNR FÖR 3E RÄKNING INV.          
005500*                                 ACS REPORTNO FOR 3RD COUNT PART         
005600        05 IDCOUNTER-P       PIC X(20).                                   
005700*                                 RÄKNARE VID INVENTERING                 
005800*                                 PERSON TO COUNT PARTS PRIME             
005900        05 IDCOUNTER-P-REG   PIC X(20).                                   
006000*                                 REG. AV RÄKNING VID INVENTERING         
006100*                                 PERSON TO REG.COUNT PARTS PRIME         
006200        05 IDCOUNTER-R       PIC X(20).                                   
006300*                                 OMRÄKNARE VID INVENTERING               
006400*                                 PERSON TO RECOUNT PARTS PRIME           
006500        05 IDCOUNTER-R-REG   PIC X(20).                                   
006600*                                 REG. AV OMRÄKNING VID INVENT.           
006700*                                 PERSON TO REGISTER RE-COUNT             
006800        05 IDCOUNTER-T       PIC X(20).                                   
006900*                                 3:E RÄKNARE VID INVENTERING             
007000*                                 3RD PERSON TO COUNT PARTS               
007100        05 IDCOUNTER-T-REG   PIC X(20).                                   
007200*                                 REG. AV 3:E RÄKNING VID INVENT.         
007300*                                 PERSON TO REGISTER 3RD COUNT            
007400        05 IDUSER-PCOUNT     PIC X(8).                                    
007500*                                 ANVÄNDARENS SÄKERHETS ID                
007600*                                 USER SECURITY-IDENTITY                  
007700        05 IDUSER-PCOUNT-REG PIC X(8).                                    
007800*                                 ANVÄNDARENS ID REG RÄKNING INV.         
007900*                                 USER SECURITY REG. COUNT PARTS          
008000        05 IDUSER-RCOUNT     PIC X(8).                                    
008100*                                 ANVÄNDARENS SÄKERHETS ID                
008200*                                 USER SECURITY-IDENTITY                  
008300        05 IDUSER-RCOUNT-REG PIC X(8).                                    
008400*                                 ANVÄNDAR ID REG OMRÄKNING INV.          
008500*                                 USER SECURITY REG.RECOUNT PARTS         
008600        05 PRAVCOST          PIC S9(7)V9(2)      COMP-3.                  
008700*                                 MEDELVÄRDESKOSTNAD I UTL.VALUTA         
008800*                                 AVERAGE COST FOREIGN CURRENCY           
008900        05 TIORDREG          PIC S9(7)           COMP-3.                  
009000*                                 ORDERREGISTRERINGSDATUM  ÅÅMMDD         
009100*                                 ORDER REGISTRATION DATE  YYMMDD         
009200        05 TIINVDAT          PIC S9(5)           COMP-3.                  
009300*                                 INVENTERINGSDATUM                       
009400*                                 STOCKTAKING DATE                        
009500        05 TIAVCOST          PIC S9(7)           COMP-3.                  
009600*                                 DATUM DÅ MEDELVÄRDET BERÄKNADES         
009700*                                 AVERAGE COST CALCULATION DATE           
009800        05 TIRETUR-BEORD     PIC S9(7)           COMP-3.                  
009900*                                 DATUM RETUR BEORDRING                   
010000*                                 DATE ISSUE OF RETURN ORDER              
010100        05 TISKROT-BEORD     PIC S9(7)           COMP-3.                  
010200*                                 BEORDRAD SKROTNINGSDATUM                
010300*                                 DATE OF SCRAPPING DECISION              
010400        05 TIREGDAT-PCOUNT   PIC S9(7)           COMP-3.                  
010500*                                 REGISTRERINGSDATUM (ÅÅMMDD)             
010600*                                 REGISTRATION DATE (YYMMDD)              
010700        05 TIREGDAT-PCOUNT-REG                                            
010800                             PIC S9(7)           COMP-3.                  
010900*                                 DATUM FÖR REG AV RÄKNING INV.           
011000*                                 DATE FOR REG. OF COUNT PARTS            
011100        05 TIREGDAT-RCOUNT   PIC S9(7)           COMP-3.                  
011200*                                 REGISTRERINGSDATUM (ÅÅMMDD)             
011300*                                 REGISTRATION DATE (YYMMDD)              
011400        05 TIREGDAT-RCOUNT-REG                                            
011500                             PIC S9(7)           COMP-3.                  
011600*                                 DATUM FÖR REG AV OMRÄKNING INV.         
011700*                                 DATE FOR REG. OF RECOUNT PARTS          
011800        05 TIREGDAT-TCOUNT   PIC S9(7)           COMP-3.                  
011900*                                 DATUM FÖR 3E RÄKNING INVENT.            
012000*                                 DATE FOR 3RD COUNT PARTS                
012100        05 TIREGDAT-TCOUNT-REG                                            
012200                             PIC S9(7)           COMP-3.                  
012300*                                 DATUM FÖR REG AV 3E RÄKNING INV         
012400*                                 DATE REG. OF 3RD COUNT PARTS            
012500        05 TIREGTID-PCOUNT   PIC S9(7)           COMP-3.                  
012600*                                 REGISTRERINGSTID                        
012700*                                 GENERAL REGISTRATION TIME               
012800        05 TIREGTID-PCOUNT-REG                                            
012900                             PIC S9(7)           COMP-3.                  
013000*                                 TID FÖR REG AV RÄKNING INV.             
013100*                                 TIME FOR REG. OF COUNT PARTS            
013200        05 TIREGTID-RCOUNT   PIC S9(7)           COMP-3.                  
013300*                                 REGISTRERINGSTID                        
013400*                                 GENERAL REGISTRATION TIME               
013500        05 TIREGTID-RCOUNT-REG                                            
013600                             PIC S9(7)           COMP-3.                  
013700*                                 TID FÖR REG AV OMRÄKNING INV.           
013800*                                 TIME FOR REG. OF RECOUNT PARTS          
013900        05 TIREGTID-TCOUNT   PIC S9(7)           COMP-3.                  
014000*                                 TID FÖR 3E RÄKNING INV.                 
014100*                                 TIME OF 3RD COUNT PARTS                 
014200        05 TIREGTID-TCOUNT-REG                                            
014300                             PIC S9(7)           COMP-3.                  
014400*                                 TID FÖR REG AV 3E RÄKNING INV.          
014500*                                 TIME REG. OF 3RD COUNT PARTS            
014600     03 KVLS-BUFFERT         PIC S9(7)           COMP-3.                  
014700*                                 LAGERSALDO                              
014800*                                 STOCK BALANCE                           
014900*** END OF VILMAII-COPY LENGTH= 302 BYTES                                 
