000100 01  A4325MR.                                                             
000200*                       *****************************************         
000300*                       ***  POSTTYP 5MR                      ***         
000400*                       ***  MASKINELLA MR FRÅN SYSTEM PÅ     ***         
000500*                       ***  MINIDATORER ALT PC PGA R/3 INST. ***         
000600*                       ***                                   ***         
000700*                       *****************************************         
000800*                                                                         
000900     03  PTYP                    PIC X(3).                                
001000*                        ***  POSTTYP 501                                 
001100     03  MRNR                    PIC X(9).                                
001200*                        ***  IDENTITET PÅ MR                             
001300     03  ARTNR                   PIC X(10).                               
001400*                        ***  ARTIKELNR                                   
001500     03  LEVNR                   PIC X(6).                                
001600*                        ***  LEVERANTÖR                                  
001700     03  BESTNR.                                                          
001800         05  BESTPREF            PIC X(3).                                
001900*                        ***  BESTÄLLNINGSPREFIX = INKÖPARNR              
002000         05  BESTLOPNR           PIC X(6).                                
002100*                        ***  LÖPNUMMER I BESTÄLLNINGEN                   
002200         05  BESTSUFF            PIC X(3).                                
002300*                        ***  BESTÄLLNINGSSUFFIX                          
002400     03  PACKNR                  PIC X(6).                                
002500*                        ***  PACKSEDELNR = FÖLJESEDELNR                  
002600     03  DATUM-AVS               PIC X(8).                                
002700*                        ***  AVSÄNDNINGSDATUM EXTRA FÖR SEKEL            
002800     03  ANTAL                   PIC X(7).                                
002900*                        ***  AVISERAT ANTAL ENLIGT FÖLJESEDEL            
003000     03  SORT1                   PIC X(2).                                
003100*                        ***  SORT                                        
003200     03  PRIS-BEST               PIC S9(9)V9(2).                          
003300     03  PRIS-BEST-X REDEFINES PRIS-BEST                                  
003400                                 PIC X(11).                               
003500*                        ***  BESTÄLLNINGSPRIS I SEK                      
003600     03  URSPPRIS-BEST           PIC S9(8)V9(3).                          
003700     03  URSPPRIS-BEST-X REDEFINES URSPPRIS-BEST                          
003800                                 PIC X(11).                               
003900*                        ***  BESTÄLLNINGSPRIS I URSP VALUTA              
004000     03  ENHET-PRIS              PIC X.                                   
004100*                        ***  ENHET I VILKEN BESTPRIS ÄR ANGIVEN          
004200     03  TIPPAT                  PIC X.                                   
004300*                        ***  KOD FÖR UPPSKATTAT BESTPRIS                 
004400*                        ***  1 = TIPPAT PRIS                             
004500     03  BEL-STD                 PIC S9(9)V9(2).                          
004600     03  BEL-STD-X REDEFINES BEL-STD                                      
004700                                 PIC X(11).                               
004800*                        ***  STANDARD-BELOPP                             
004900     03  BEL-BEST                PIC S9(9)V9(2).                          
005000     03  BEL-BEST-X REDEFINES BEL-BEST                                    
005100                                 PIC X(11).                               
005200*                        ***  BESTÄLLNINGSBELOPP I SEK                    
005300     03  KTO.                                                             
005400         05  HKTO                PIC X(6).                                
005500*                        ***  SEXSTÄLLIGT KONTO PGA R/3                   
005600         05  UKTO                PIC X(4).                                
005700*                        ***  EXTRA FÄLT PGA R/3                          
005800     03  FTAG                    PIC X(2).                                
005900*                        ***  FÖRETAGSKOD                                 
006000     03  PRODKOD                 PIC X(3).                                
006100*                        ***  PRODUKTKOD                                  
006200     03  SYSTKOD                 PIC X(2).                                
006300*                        ***  KOD FÖR AVLÄMNANDE SYSTEM                   
006400*                        ***  10 - 19 TXXX FRED                           
006500*                        ***  20 - 29 RYYY RS                             
006600*                        ***  30 - 39 P784 TOMAT                          
006700*                        ***  40 - 49 UFO                                 
006800*                        ***  91 = MANUELLA AVD 4134                      
006900     03  VAL                     PIC X(3).                                
007000*                        ***  VALUTAKOD                                   
007100     03  TULLKURS                PIC S9(3)V9(2).                          
007200     03  TULLKURS-X REDEFINES TULLKURS                                    
007300                                 PIC X(5).                                
007400*                        ***  TULLKURS                                    
007500     03  TULLFAKT                PIC S9(1)V9(4).                          
007600     03  TULLFAKT-X REDEFINES TULLFAKT                                    
007700                                 PIC X(5).                                
007800*                        ***  TULLFAKTOR                                  
007900     03  RATTKOD                 PIC X(1).                                
008000*                        ***  RÄTTNINGSKOD  1 = RÄTTN PGA VOLVO           
008100*                        ***                2 = RÄTTN PGA LEVERANT        
008200     03  PRCTR                   PIC X(10).                               
008300*------------------------------- PROFIT CENTER                            
008400*                                             SEE SEPARAT DOCUMENT        
008500     03  COSTCTR-FREIGHT         PIC X(12).                               
008600*------------------------------- ORD/COST CENTER FREIGHT COST             
008700*                                             SEE SEPARAT DOCUMENT        
008800     03  FLAGGA1F                PIC X(1).                                
008900*-------------------------------FLAGGA FÖR ORDNO ELLER COST CENTER        
009000*-------------------------------FÖR FREIGHT                               
009100     03  COSTCTR-PRICE-DIFF      PIC X(12).                               
009200*------------------------------- ORD/COST CENTER DEF PRICE DIFF           
009300*                                             SEE SEPARAT DOCUMENT        
009400     03  FLAGGA2D                PIC X(1).                                
009500*-------------------------------FLAGGA FÖR ORDNO ELLER COST CENTER        
009600*-------------------------------FÖR DIFFAR                                
009700     03  COSTCTR-EXCH-DIFF       PIC X(12).                               
009800*-------------------------------ORD/COST CENTER EXCHANGE RATE DIFF        
009900*                                             SEE SEPARAT DOCUMENT        
010000     03  FLAGGA3VD               PIC X(1).                                
010100*-------------------------------FLAGGA FÖR ORDNO ELLER COST CENTER        
010200*-------------------------------FÖR VALUTADMFFAR                          
010300     03  GODSMOT                 PIC X(5).                                
010400*-------------------------------GODSMOTTAGARE                             
010500     03  LOPNRFR1                PIC X(7).                                
010600*-------------------------------GODSMOTTAGARE                             
010700     03  LOPNRTM1                PIC X(7).                                
010800*-------------------------------GODSMOTTAGARE                             
010900     03  LOPNRFR2                PIC X(7).                                
011000*-------------------------------GODSMOTTAGARE                             
011100     03  LOPNRTM2                PIC X(7).                                
011200*-------------------------------GODSMOTTAGARE                             
011300     03  AVVIKELSE               PIC X(1).                                
011400*-------------------------------GODSMOTTAGARE                             
011500     03  PACKNRTOT               PIC X(11).                               
011600*-------------------------------GODSMOTTAGARE                             
011700     03  KREDIT                  PIC X.                                   
011800*-------------------------------KREDIT = '-'                              
011900     03  FILLER                  PIC X(65).                               
012000*** END OF VILMAII-COPY LENGTH= 300 OLD LENGTH= 300                       
