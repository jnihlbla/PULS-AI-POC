000100 01  A432GSDB.                                                            
000200*                       *****************************************         
000300*                       ***  POSTTYP 500                      ***         
000400*                       ***  MASKINELLA MR FRÅN SYSTEM PÅ     ***         
000500*                       ***  STORDATORER EFTER R/3 INSTALLATION**         
000600*                       ***  DSN P78428(MAS)   RC1157(PULS)   ***         
000700*                       *****************************************         
000800*                                                                         
000900     03  PTYP                    PIC X(3).                                
001000*                        ***  POSTTYP 500                                 
001100     03  MRNR                    PIC S9(8)         COMP-3.                
001200*                        ***  IDENTITET PÅ MR                             
001300     03  ARTNR                   PIC S9(10)        COMP-3.                
001400*                        ***  ARTIKELNR                                   
001500     03  GSDB                    PIC X(7).                                
001600*                        ***  LEVERANTÖRNR/GSDB                           
001700     03  BESTNR.                                                          
001800         05  BESTPREF            PIC S9(3)         COMP-3.                
001900*                        ***  BESTÄLLNINGSPREFIX = INKÖPARNR              
002000         05  BESTLOPNR           PIC S9(6)         COMP-3.                
002100*                        ***  LÖPNUMMER I BESTÄLLNINGEN                   
002200         05  BESTSUFF            PIC S9(3)         COMP-3.                
002300*                        ***  BESTÄLLNINGSSUFFIX                          
002400     03  PACKNR                  PIC S9(6)         COMP-3.                
002500*                        ***  PACKSEDELNR = FÖLJESEDELNR                  
002600     03  DATUM-AVS               PIC 9(8).                                
002700*                        ***  AVSÄNDNINGSDATUM (SEKELÄNDRING)             
002800     03  ANTAL                   PIC S9(7)         COMP-3.                
002900*                        ***  AVISERAT ANTAL ENLIGT FÖLJESEDEL            
003000     03  SORT1                   PIC S9(2)         COMP-3.                
003100*                        ***  SORT                                        
003200     03  PRIS-BEST               PIC S9(9)V9(2)    COMP-3.                
003300*                        ***  BESTÄLLNINGSPRIS I SEK                      
003400     03  URSPPRIS-BEST           PIC S9(11)        COMP-3.                
003500*                        ***  BESTÄLLNINGSPRIS I URSP VALUTA              
003600     03  ENHET-PRIS              PIC X.                                   
003700*                        ***  ENHET BESTÄLLNINGSPRIS                      
003800     03  TIPPAT                  PIC X.                                   
003900*                        ***  KOD FÖR UPPSKATTAT BESTPRIS                 
004000*                        ***  KOD = 1 FÖR TIPPAT PRIS                     
004100     03  BEL-STD                 PIC S9(9)V9(2)    COMP-3.                
004200*                        ***  STANDARD-BELOPP                             
004300     03  BEL-BEST                PIC S9(9)V9(2)    COMP-3.                
004400*                        ***                                              
004500     03  KTO.                                                             
004600         05  HKTO                PIC X(6).                                
004700*                        ***  SEXSTÄLLIGT KONTO                           
004800         05  UKTO                PIC X(4).                                
004900*                        ***  EXTRA UTRYMME R/3                           
005000     03  FTAG                    PIC S9(2)         COMP-3.                
005100*                        ***  FÖRETAGSKOD                                 
005200     03  PRODKOD                 PIC S9(4)         COMP-3.                
005300*                        ***  PRODUKTKOD                                  
005400     03  SYSTKOD                 PIC S9(2)         COMP-3.                
005500*                        ***  KOD FÖR AVLÄMNANDE SYSTEM                   
005600*                        ***  30 - 39 P784 TOMAT (VTV)                    
005700*                        ***  40 - 49 UFO                                 
005800*                        ***  50 - 59 VPO                                 
005900*                        ***  60 - 69 VDV                                 
006000*                        ***  70 - 79 P794 KAM (VKAV)                     
006100*                        ***  91 = MANUELLA AVD 4134                      
006200     03  KDVALISO                PIC X(3).                                
006300*                        ***  VALUTA                                      
006400     03  TULLKURS                PIC S9(3)V9(2)    COMP-3.                
006500*                        ***  TULLKURS                                    
006600     03  TULLFAKT                PIC S9(1)V9(4)    COMP-3.                
006700*                        ***  TULLFAKTOR                                  
006800     03  RATTKOD                 PIC X.                                   
006900*                        ***  RÄTTNINGSKOD  1 = RÄTTN PGA VOLVO           
007000*                        ***                2 = RÄTTN PGA LEVERANT        
007100     03  PRCTR                   PIC X(10).                               
007200*------------------------------- PROFIT CENTER                            
007300*                                             SEE SEPARAT DOCUMENT        
007400     03  COSTCTR-FREIGHT         PIC X(12).                               
007500*------------------------------- ORD/COST CENTER FREIGHT COST             
007600*                                             SEE SEPARAT DOCUMENT        
007700     03  FLAGGA1F                PIC X(1).                                
007800*-------------------------------FLAGGA FÖR ORDNO ELLER COST CENTER        
007900*-------------------------------FÖR FREIGHT                               
008000     03  COSTCTR-PRICE-DIFF      PIC X(12).                               
008100*------------------------------- ORD/COST CENTER DEF PRICE DIFF           
008200     03  FLAGGA2D                PIC X(1).                                
008300*-------------------------------FLAGGA FÖR ORDNO ELLER COST CENTER        
008400*-------------------------------FÖR DIFF                                  
008500*                                             SEE SEPARAT DOCUMENT        
008600     03  COSTCTR-EXCH-DIFF       PIC X(12).                               
008700*-------------------------------ORD/COST CENTER EXCHANGE RATE DIFF        
008800*                                             SEE SEPARAT DOCUMENT        
008900     03  FLAGGA3VD               PIC X(1).                                
009000*-------------------------------FLAGGA FÖR ORDNO ELLER COST CENTER        
009100*-------------------------------FÖR VALUTADIFFAR                          
009200     03  GODSMOT                 PIC 9(5).                                
009300*-------------------------------GODSMOTTAGNIG (EX 1003, 1441 ETC)         
009400     03  LOPNRSEKVFROM1          PIC 9(7).                                
009500*-------------------------------SEKVENSLÖPNUMMER FOM   BANA 1             
009600     03  LOPNRSEKVTOM1           PIC 9(7).                                
009700*-------------------------------SEKVENSLÖPNUMMER TOM   BANA 1             
009800     03  LOPNRSEKVFROM2          PIC 9(7).                                
009900*-------------------------------SEKVENSLÖPNUMMER FOM   BANA 2             
010000     03  LOPNRSEKVTOM2           PIC 9(7).                                
010100*-------------------------------SEKVENSLÖPNUMMER TOM   BANA 2             
010200     03  AVVIKELSE               PIC X(1).                                
010300*-------------------------------AVVIKELSE J ELLER N                       
010400     03  PACKNRTOT               PIC S9(11)        COMP-3.                
010500*-------------------------------PACKNR TOTALT                             
010600     03  FILLER                  PIC X(111).                              
010700*-------------------------------EXTRA UTRYMME                             
010800*** END OF VILMAII-COPY LENGTH= 300 OLD LENGTH= 299                       
