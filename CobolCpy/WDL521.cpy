000100 01  FAKL-WDL521.                                                         
000200*                                 INVOICE HISTORY                         
000300*                                 INVOICE LINES                           
000400*                                 FYSISK NYCKEL: WDL521KY                 
000500*                                 (IDARTNR + IDPURAD)                     
000600     03 FAKL-IDARTNR         PIC S9(9)           COMP-3.                  
000700*                                 ARTIKELNUMMER                           
000800*                                 PART NUMBER                             
000900     03 FAKL-IDPURAD         PIC S9(5)           COMP-3.                  
001000*                                 RADNUMMER PÅ PACKUNDERLAG               
001100*                                 LINENO IN PACKINGDOCUMENT               
001200     03 FAKL-BEART           PIC X(25).                                   
001300*                                 ARTIKELBENÄMNING                        
001400*                                 PART DESCRIPTION                        
001500     03 FAKL-IDBORD          PIC X(3).                                    
001600*                                 PACK-BORD                               
001700*                                 PACKING TABLE                           
001800     03 FAKL-IDLEVNR         PIC X(5).                                    
001900*                                 LEVERANTÖRNUMMER                        
002000*                                 SUPPLIER NUMBER (VENDOR NUMBER)         
002100     03 FAKL-IDUSER-OREG     PIC X(8).                                    
002200*                                 ANSVARIGT USERID ORDERREG.              
002300*                                 RESPONSIBLE USERID ORDERREG.            
002400     03 FAKL-IDUSER-PACK     PIC X(8).                                    
002500*                                 ANSVARIGT USERID PACKARE                
002600*                                 RESPONSIBLE USERID PACKER               
002700     03 FAKL-KVAVBART        PIC S9(7)           COMP-3.                  
002800*                                 AVBOKAT ANTAL ARTIKLAR                  
002900*                                 ALLOCATED QUANTITY                      
003000     03 FAKL-KVBEART-Q       PIC S9(7)           COMP-3.                  
003100*                                 BESTÄLLT KVANTANPASSAT ANTAL            
003200*                                 ORDERED QUANTITY ADAPTED                
003300*                                  ITEMS                                  
003400     03 FAKL-KVLEVART        PIC S9(7)           COMP-3.                  
003500*                                 LEVERERAT ANTAL STYCK                   
003600*                                 DELIVERED QUANTITY                      
003700     03 FAKL-KVORDRAD        PIC S9(5)           COMP-3.                  
003800*                                 ANTAL ORDERRADER                        
003900*                                 NUMBER OF ORDER LINES                   
004000     03 FAKL-PRARTNTO        PIC S9(7)V9(2)      COMP-3.                  
004100*                                 ARTIKELPRIS NETTO                       
004200*                                 NET PRICE EACH   (FOB NET)              
004300     03 FAKL-PRAVCOST        PIC S9(7)V9(2)      COMP-3.                  
004400*                                 MEDELVÄRDESKOSTNAD I UTL.VALUTA         
004500*                                 AVERAGE COST FOREIGN CURRENCY           
004600     03 FAKL-KDVALISO-AVC    PIC X(3).                                    
004700*                                 VALUTAKOD ENLIGT ISO-STANDARD.          
004800*                                 CURRENCY CODE BY ISO-STANDARD.          
004900*                                 AVERAGE COST CURRENCY                   
005000     03 FAKL-PRARTNTO-LOC    PIC S9(7)V9(2)      COMP-3.                  
005100*                                 ARTIKELPRIS NETTO LOKAL VALUTA          
005200*                                 NET PRICE EACH LOCAL CURRENCY           
005300     03 FAKL-VKARTNTO        PIC S9(4)V9(3)      COMP-3.                  
005400*                                 ARTIKELVIKT NETTO (KG) MED EMB          
005500*                                 PART NET WEIGHT (KG) W/ PACKAGE         
005600     03 FAKL-VKART-NTO-KG    PIC S9(4)V9(3)      COMP-3.                  
005700*                                 ART. NETTOVIKT I KG UTAN EMB            
005800*                                 PART NET WEIGHT KG NO PACKAGING         
005900     03 FAKL-ADLAGOMR        PIC S9(3)           COMP-3.                  
006000*                                 LAGEROMRÅDE                             
006100*                                 AREA                                    
006200     03 FAKL-FLPCOO          PIC X.                                       
006300*                                 FLAGGA OM FÖRMÅNSAVTAL URS.LAND         
006400*                                 FLAG PREF.AGREM. COUNTRY ORIGIN         
006500     03 FAKL-KDARTURS        PIC X(2).                                    
006600*                                 ARTIKELURSPRUNGSKOD                     
006700*                                 COUNTRY OF ORIGIN                       
006800     03 FAKL-SUBTO-LINE      PIC S9(11)V9(2)     COMP-3.                  
006900*                                 TOTAL SALES AMOUNT PER LINE INC         
007000*                                 L. VAT                                  
007100     03 FAKL-SUNTO-LINE      PIC S9(11)V9(2)     COMP-3.                  
007200*                                 TOTAL SALES AMOUNT PER LINE EXC         
007300*                                 L. VAT                                  
007400     03 FAKL-SUVAT-LINE      PIC S9(11)V9(2)     COMP-3.                  
007500*                                 MOMSVÄRDE PER FAKTURARAD                
007600*                                 VAT VALUE PER INVOICE LINE              
007700     03 FAKL-IDFKNGRP        PIC S9(5)           COMP-3.                  
007800*                                 FUNKTIONSGRUPP                          
007900*                                 FUNCTION GROUP                          
008000     03 FAKL-KDARTRAB        PIC 9(2).                                    
008100*                                 RABATTKOD (ARTIKELPRIS)                 
008200*                                 PURCHASE DISCOUNT CODE                  
008300     03 FAKL-KDPRODSL        PIC S9(3)           COMP-3.                  
008400*                                 PRODUKTSLAG                             
008500*                                 PRODUCT GROUP                           
008600     03 FAKL-PRAVCOST-CORE   PIC S9(7)V9(2)      COMP-3.                  
008700*                                 OBJEKTETS MEDELVÄRDESKOSTNAD I          
008800*                                 UTL.VALUTA                              
008900*                                 AV. COST PRICE OF THE CORE IN F         
009000*                                 OR. CURR.                               
009100     03 FAKL-PRKURS-BET      PIC S9(6)V9(5)      COMP-3.                  
009200*                                 VALUTAKURS                              
009300*                                 CURRENCY EXCHANGE RATE                  
009400     03 FAKL-PRKURS-FAKT     PIC S9(6)V9(5)      COMP-3.                  
009500*                                 VALUTAKURS                              
009600*                                 CURRENCY EXCHANGE RATE                  
009700     03 FAKL-PRKURS-FIKTIV   PIC S9(6)V9(5)      COMP-3.                  
009800*                                 VALUTAKURS                              
009900*                                 CURRENCY EXCHANGE RATE                  
010000     03 FAKL-KDFAKSTA-EXP    PIC X.                                       
010100*                                 DUBBELFAKTURERING STATUS                
010200*                                 STATUS CODE DOUBLE INVOICING            
010300     03 FAKL-KDVALISO-BET    PIC X(3).                                    
010400*                                 VALUTAKOD ENLIGT ISO-STANDARD.          
010500*                                 CURRENCY CODE BY ISO-STANDARD.          
010600     03 FAKL-KDVALISO-NTO    PIC X(3).                                    
010700*                                 VALUTAKOD ENLIGT ISO-STANDARD.          
010800*                                 VALUTAKOD ENLIGT ISO-STANDARD.          
010900*                                 CURRENCY CODE BY ISO-STANDARD.          
011000*** END OF VILMAII-COPY LENGTH= 161 BYTES                                 
