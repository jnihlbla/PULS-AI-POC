000100 01  4252-WDGX4252.                                                       
000200*                                 ORDER CONSOLIDATION - DIRLEV            
000300*                                 ORDERRAD                                
000400*                                 FYSISK NYCKEL: KY4252                   
000500*                                 (IDLOPNR + IDARTNR)                     
000600     03 4252-IDLOPNR         PIC S9(3)           COMP-3.                  
000700*                                 LÖPNUMMER                               
000800*                                 SEQUENCE NUMBER                         
000900     03 4252-RADER.                                                       
001000        05 4252-IDARTNR      PIC X(9).                                    
001100*                                 ARTIKELNUMMER                           
001200*                                 PART NUMBER                             
001300        05 4252-REKSIFFR     PIC X.                                       
001400*                                 KONTROLLSIFFRA                          
001500*                                 PART NO CHECK DIGIT                     
001600        05 4252-KVBEART      PIC X(6).                                    
001700*                                 BESTÄLLT ANTAL STYCKEN                  
001800*                                 ORDERED QUANTITY                        
001900        05 4252-PRARTNTO     PIC X(10).                                   
002000*                                 ARTIKELPRIS NETTO                       
002100*                                 NET PRICE EACH   (FOB NET)              
002200        05 4252-TITPO        PIC X(6).                                    
002300*                                 PLANERAD ORDERDATUM                     
002400*                                 PLANNED ORDER DATE                      
002500        05 4252-FLRESTN      PIC X.                                       
002600*                                 RESTNOTERING ?                          
002700*                                 BACKORDERED ?                           
002800        05 4252-KDKVBRYT     PIC X.                                       
002900*                                 KOD OM KVANTFÖRP SKALL BRYTAS           
003000*                                 BREAK BULKPACK CODE                     
003100        05 4252-FLINVEST     PIC X.                                       
003200*                                 BYTES INVENTERINGSFLAGGA                
003300*                                 EXCHANGE INVESTMENT FLAG                
003400        05 4252-KDVRINFO     PIC X.                                       
003500*                                 PÅVERKAN I VR/DSP SYSTEM                
003600*                                 VR/DSP UP-DATE                          
003700        05 4252-IDKONTO      PIC X(10).                                   
003800*                                 KONTO                                   
003900*                                 ACCOUNT                                 
004000        05 4252-IDKST        PIC X(10).                                   
004100*                                 KOSTNADSSTÄLLE                          
004200*                                 COST CENTRE                             
004300        05 4252-BERADREF     PIC X(10).                                   
004400*                                 KUNDENS RADREFERENS                     
004500*                                 CUSTOMERS ITEM REF.                     
004600        05 4252-KDDSP        PIC X.                                       
004700*                                 PÅVERKAN PÅ DSP                         
004800*                                 AFFECT ON DSP                           
004900        05 4252-FLSLATT      PIC X.                                       
005000*                                 FLAGGA SOM ANGER OM KVSLATT SKA         
005100*                                 LL BERÄKNAS ELLER EJ                    
005200*                                 OM FLRESTN = J OCH FLSLATT = J,         
005300*                                  DÅ BERÄKNAS KVSLATT                    
005400        05 4252-FLDIRLEV     PIC X.                                       
005500*                                 DIREKTLEVERANS ?                        
005600*                                 DIRECT DELIVERY ?                       
005700        05 4252-IDBIL.                                                    
005800*                                 BILIDENTITET                            
005900*                                 CAR IDENTITY                            
006000           07 4252-IDBILTYP  PIC X(3).                                    
006100*                                 BILTYP                                  
006200*                                 CAR TYPE                                
006300           07 4252-TIAAAA    PIC X(4).                                    
006400*                                 ÅRTAL (ÅÅÅÅ)                            
006500*                                 YEAR  (YYYY)                            
006600           07 4252-IDCHASSI-PIE                                           
006700                             PIC X(6).                                    
006800*                                 CHASSINUMMER PIE                        
006900*                                 CHASSI NUMBER PIE                       
007000        05 4252-PRARTNTO-LOC PIC X(10).                                   
007100*                                 ARTIKELPRIS NETTO LOKAL VALUTA          
007200*                                 NET PRICE EACH LOCAL CURRENCY           
007300        05 4252-PRARTBTO-LOC PIC X(10).                                   
007400*                                 PRIS I LOKAL VALUTA                     
007500*                                 LOCAL GROSS SALES PRICE                 
007600        05 4252-KDVALISO     PIC X(3).                                    
007700*                                 VALUTAKOD ENLIGT ISO-STANDARD.          
007800*                                 CURRENCY CODE BY ISO-STANDARD.          
007900        05 4252-KDVAT        PIC X(2).                                    
008000*                                 MOMSKOD                                 
008100*                                 VAT CODE                                
008200        05 4252-RERAB        PIC 9(2)V9(1).                               
008300*                                 RABATTSATS (PROCENT)                    
008400        05 4252-KDRAB        PIC X(5).                                    
008500*                                 RABATTKOD                               
008600        05 4252-BEART-VIPS   PIC X(25).                                   
008700*                                 VIPS ARTIKELBENÄMNING                   
008800*                                 PÅ DEALERNS SPRÅK                       
008900        05 4252-ADLAGOMR-CD  PIC 9(2).                                    
009000*                                 LAGEROMRÅDE CROSS DOCKING LAGER         
009100*                                 AREA ADDRESS CROSS DOCKING WARE         
009200*                                 HOUSE                                   
009300        05 4252-ADGANG-CD    PIC 9(2).                                    
009400*                                 GÅNG                                    
009500*                                 AISLE                                   
009600        05 4252-ADPLATS-CD   PIC 9(5).                                    
009700*                                 LAGERPLATSNUMMER                        
009800*                                 LOCATION                                
009900        05 4252-IDKUNDRF-WIP PIC X(10).                                   
010000*                                 REPARATIONS ORDERNR, LDC KUND           
010100*                                 WORK ORDER NUMBER, LDC DEALER           
010200*** END OF VILMAII-COPY LENGTH= 161 BYTES                                 
