000100 01  ACS-WDJ701.                                                          
000200*                                 ACS INVENTERINGS REGISTER               
000300*                                 FYSISK NYCKEL: WDJ701KY                 
000400*                                 (IDDC + IDARTNR)                        
000500     03 ACS-IDDC             PIC X(2).                                    
000600*                                 IDENTIFIERARE LAGER                     
000700*                                 WAREHOUSE IDENTIFIER                    
000800     03 ACS-IDARTNR          PIC S9(9)           COMP-3.                  
000900*                                 ARTIKELNUMMER                           
001000*                                 PART NUMBER                             
001100     03 ACS-BEART            PIC X(25).                                   
001200*                                 ARTIKELBENÄMNING                        
001300*                                 PART DESCRIPTION                        
001400     03 ACS-ADLAGOMR         PIC 9(2).                                    
001500*                                 LAGEROMRÅDE                             
001600*                                 AREA                                    
001700     03 ACS-ADGANG           PIC 9(2).                                    
001800*                                 GÅNG                                    
001900*                                 AISLE                                   
002000     03 ACS-ADPLATS          PIC 9(5).                                    
002100*                                 LAGERPLATSNUMMER                        
002200*                                 LOCATION                                
002300     03 ACS-KDPRODSL         PIC S9(3)           COMP-3.                  
002400*                                 PRODUKTSLAG                             
002500*                                 PRODUCT GROUP                           
002600     03 ACS-KDPSLLOC         PIC 9(2).                                    
002700*                                 PRODUKTSLAG LOKALT                      
002800*                                 PRODUCT GROUP LOCAL                     
002900     03 ACS-KVLS             PIC S9(7)           COMP-3.                  
003000*                                 LAGERSALDO                              
003100*                                 STOCK BALANCE                           
003200     03 ACS-KVPCOUNT         PIC S9(7)           COMP-3.                  
003300*                                 RÄKNAT   ANTAL                          
003400*                                 QUANTITY COUNTED                        
003500     03 ACS-KVRCOUNT         PIC S9(7)           COMP-3.                  
003600*                                 OMRÄKNAT ANTAL                          
003700*                                 QUANTITY RECOUNTED                      
003800     03 ACS-KVTCOUNT         PIC S9(7)           COMP-3.                  
003900*                                 OMRÄKNAT ANTAL 3:E GGN                  
004000*                                 QUANTITY RECOUNTED THIRD TIME           
004100     03 ACS-FLKLAR           PIC X.                                       
004200*                                 AVSLUTNINGSMARKERING                    
004300*                                 FINISHED FLAG                           
004400     03 ACS-IDACSNR-P        PIC S9(7)           COMP-3.                  
004500*                                 ACS LISTNR FÖR RÄKNING INV.             
004600*                                 ACS REPORT NO PRIMECOUNT PARTS          
004700     03 ACS-IDACSNR-R        PIC S9(7)           COMP-3.                  
004800*                                 ACS LISTNR FÖR OMRÄKNING INV.           
004900*                                 ACS REPORT NO FOR RECOUNT PARTS         
005000     03 ACS-IDACSNR-T        PIC S9(7)           COMP-3.                  
005100*                                 ACS LISTNR FÖR 3E RÄKNING INV.          
005200*                                 ACS REPORTNO FOR 3RD COUNT PART         
005300     03 ACS-IDCOUNTER-P      PIC X(20).                                   
005400*                                 RÄKNARE VID INVENTERING                 
005500*                                 PERSON TO COUNT PARTS PRIME             
005600     03 ACS-IDCOUNTER-P-REG  PIC X(20).                                   
005700*                                 REG. AV RÄKNING VID INVENTERING         
005800*                                 PERSON TO REG.COUNT PARTS PRIME         
005900     03 ACS-IDCOUNTER-R      PIC X(20).                                   
006000*                                 OMRÄKNARE VID INVENTERING               
006100*                                 PERSON TO RECOUNT PARTS PRIME           
006200     03 ACS-IDCOUNTER-R-REG  PIC X(20).                                   
006300*                                 REG. AV OMRÄKNING VID INVENT.           
006400*                                 PERSON TO REGISTER RE-COUNT             
006500     03 ACS-IDCOUNTER-T      PIC X(20).                                   
006600*                                 3:E RÄKNARE VID INVENTERING             
006700*                                 3RD PERSON TO COUNT PARTS               
006800     03 ACS-IDCOUNTER-T-REG  PIC X(20).                                   
006900*                                 REG. AV 3:E RÄKNING VID INVENT.         
007000*                                 PERSON TO REGISTER 3RD COUNT            
007100     03 ACS-IDUSER-PCOUNT    PIC X(8).                                    
007200*                                 ANVÄNDARENS SÄKERHETS ID                
007300*                                 USER SECURITY-IDENTITY                  
007400     03 ACS-IDUSER-PCOUNT-REG                                             
007500                             PIC X(8).                                    
007600*                                 ANVÄNDARENS ID REG RÄKNING INV.         
007700*                                 USER SECURITY REG. COUNT PARTS          
007800     03 ACS-IDUSER-RCOUNT    PIC X(8).                                    
007900*                                 ANVÄNDARENS SÄKERHETS ID                
008000*                                 USER SECURITY-IDENTITY                  
008100     03 ACS-IDUSER-RCOUNT-REG                                             
008200                             PIC X(8).                                    
008300*                                 ANVÄNDAR ID REG OMRÄKNING INV.          
008400*                                 USER SECURITY REG.RECOUNT PARTS         
008500     03 ACS-PRAVCOST         PIC S9(7)V9(2)      COMP-3.                  
008600*                                 MEDELVÄRDESKOSTNAD I UTL.VALUTA         
008700*                                 AVERAGE COST FOREIGN CURRENCY           
008800     03 ACS-TIORDREG         PIC S9(7)           COMP-3.                  
008900*                                 ORDERREGISTRERINGSDATUM  ÅÅMMDD         
009000*                                 ORDER REGISTRATION DATE  YYMMDD         
009100     03 ACS-TIINVDAT         PIC S9(5)           COMP-3.                  
009200*                                 INVENTERINGSDATUM                       
009300*                                 STOCKTAKING DATE                        
009400     03 ACS-TIAVCOST         PIC S9(7)           COMP-3.                  
009500*                                 DATUM DÅ MEDELVÄRDET BERÄKNADES         
009600*                                 AVERAGE COST CALCULATION DATE           
009700     03 ACS-TIRETUR-BEORD    PIC S9(7)           COMP-3.                  
009800*                                 DATUM RETUR BEORDRING                   
009900*                                 DATE ISSUE OF RETURN ORDER              
010000     03 ACS-TISKROT-BEORD    PIC S9(7)           COMP-3.                  
010100*                                 BEORDRAD SKROTNINGSDATUM                
010200*                                 DATE OF SCRAPPING DECISION              
010300     03 ACS-TIREGDAT-PCOUNT  PIC S9(7)           COMP-3.                  
010400*                                 REGISTRERINGSDATUM (ÅÅMMDD)             
010500*                                 REGISTRATION DATE (YYMMDD)              
010600     03 ACS-TIREGDAT-PCOUNT-REG                                           
010700                             PIC S9(7)           COMP-3.                  
010800*                                 DATUM FÖR REG AV RÄKNING INV.           
010900*                                 DATE FOR REG. OF COUNT PARTS            
011000     03 ACS-TIREGDAT-RCOUNT  PIC S9(7)           COMP-3.                  
011100*                                 REGISTRERINGSDATUM (ÅÅMMDD)             
011200*                                 REGISTRATION DATE (YYMMDD)              
011300     03 ACS-TIREGDAT-RCOUNT-REG                                           
011400                             PIC S9(7)           COMP-3.                  
011500*                                 DATUM FÖR REG AV OMRÄKNING INV.         
011600*                                 DATE FOR REG. OF RECOUNT PARTS          
011700     03 ACS-TIREGDAT-TCOUNT  PIC S9(7)           COMP-3.                  
011800*                                 DATUM FÖR 3E RÄKNING INVENT.            
011900*                                 DATE FOR 3RD COUNT PARTS                
012000     03 ACS-TIREGDAT-TCOUNT-REG                                           
012100                             PIC S9(7)           COMP-3.                  
012200*                                 DATUM FÖR REG AV 3E RÄKNING INV         
012300*                                 DATE REG. OF 3RD COUNT PARTS            
012400     03 ACS-TIREGTID-PCOUNT  PIC S9(7)           COMP-3.                  
012500*                                 REGISTRERINGSTID                        
012600*                                 GENERAL REGISTRATION TIME               
012700     03 ACS-TIREGTID-PCOUNT-REG                                           
012800                             PIC S9(7)           COMP-3.                  
012900*                                 TID FÖR REG AV RÄKNING INV.             
013000*                                 TIME FOR REG. OF COUNT PARTS            
013100     03 ACS-TIREGTID-RCOUNT  PIC S9(7)           COMP-3.                  
013200*                                 REGISTRERINGSTID                        
013300*                                 GENERAL REGISTRATION TIME               
013400     03 ACS-TIREGTID-RCOUNT-REG                                           
013500                             PIC S9(7)           COMP-3.                  
013600*                                 TID FÖR REG AV OMRÄKNING INV.           
013700*                                 TIME FOR REG. OF RECOUNT PARTS          
013800     03 ACS-TIREGTID-TCOUNT  PIC S9(7)           COMP-3.                  
013900*                                 TID FÖR 3E RÄKNING INV.                 
014000*                                 TIME OF 3RD COUNT PARTS                 
014100     03 ACS-TIREGTID-TCOUNT-REG                                           
014200                             PIC S9(7)           COMP-3.                  
014300*                                 TID FÖR REG AV 3E RÄKNING INV.          
014400*                                 TIME REG. OF 3RD COUNT PARTS            
014500*** END OF VILMAII-COPY LENGTH= 298 BYTES                                 
