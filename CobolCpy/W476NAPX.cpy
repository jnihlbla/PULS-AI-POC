000100 01  SUBX-W476NAPX.                                                       
000200*                                 COPYTEXT FOR NA PROFORMA                
000300*                                 SUB HEADER, USA->CA DEALER              
000400*                                 RECORD TYPE = X                         
000500     03 SUBX-IDAFPRCD        PIC X(10).                                   
000600*                                 AFP-BLANKETT POSTTYP                    
000700*                                 AFP FORMS RECORD TYPE                   
000800     03 SUBX-TO-IDKUNDNR     PIC Z(5)9.                                   
000900*                                 KUNDNUMMER                              
001000*                                 CUSTOMER NO                             
001100     03 SUBX-TO-BEGMT-RAD1   PIC X(35).                                   
001200*                                 GODSMOTTAGARNAMN RAD 1                  
001300*                                 GOODS RECEIVER NAME LINE 1              
001400     03 SUBX-TO-BEGMT-RAD2   PIC X(35).                                   
001500*                                 GODSMOTTAGARNAMN RAD 2                  
001600*                                 GOODS RECEIVER NAME LINE 2              
001700     03 SUBX-TO-ADGMT-GATA   PIC X(35).                                   
001800*                                 GODSMOTTAGARADRESS GATA                 
001900*                                 GOODS RECEIVER ADDRESS STREET           
002000     03 SUBX-TO-ADGMT-PADR   PIC X(35).                                   
002100*                                 GODSMOTTAGARADRESS POSTADRESS           
002200*                                 GOODS RECEIVER ADDRESS TOWN             
002300     03 SUBX-TO-ADGMT-LAND   PIC X(35).                                   
002400*                                 GODSMOTTAGARADRESS LAND                 
002500*                                 GOODS RECEIVER ADDRESS COUNTRY          
002600     03 SUBX-BELEV-RAD1      PIC X(35).                                   
002700*                                 LEVERANTÖRSNAMN                         
002800*                                 SUPPLIER NAME                           
002900     03 SUBX-BELEV-RAD2      PIC X(35).                                   
003000*                                 LEVERANTÖRSNAMN                         
003100*                                 SUPPLIER NAME                           
003200     03 SUBX-ADLEV-GATA      PIC X(35).                                   
003300*                                 LEVERANTÖRENS GATUADRESS                
003400*                                 SUPPLIER ADDRESS STREET                 
003500     03 SUBX-ADLEV-PADR      PIC X(35).                                   
003600*                                 LEVERANTÖRSADRESS                       
003700*                                 SUPPLIER ADDRESS LINE                   
003800     03 SUBX-ADLEV-LAND      PIC X(35).                                   
003900*                                 LEVERANTÖRSADRESS                       
004000*                                 SUPPLIER ADDRESS LINE                   
004100     03 SUBX-BEBETRAD-1      PIC X(35).                                   
004200*                                 DEL AV BETALNINGSANSVARIGS NAMN         
004300*                                 PART OF FINANCIAL CUSTOMER NAME         
004400     03 SUBX-BEBETRAD-2      PIC X(35).                                   
004500*                                 DEL AV BETALNINGSANSVARIGS NAMN         
004600*                                 PART OF FINANCIAL CUSTOMER NAME         
004700     03 SUBX-ADBETRAD-GATA   PIC X(35).                                   
004800*                                 ADRESSRAD BETALNINGSANSVARIG            
004900*                                 PART OF FINANCIAL CUSTOMER ADDR         
005000*                                 ESS                                     
005100     03 SUBX-ADBETRAD-PADR   PIC X(35).                                   
005200*                                 ADRESSRAD BETALNINGSANSVARIG            
005300*                                 PART OF FINANCIAL CUSTOMER ADDR         
005400*                                 ESS                                     
005500     03 SUBX-ADBETRAD-LAND   PIC X(35).                                   
005600*                                 ADRESSRAD BETALNINGSANSVARIG            
005700*                                 PART OF FINANCIAL CUSTOMER ADDR         
005800*                                 ESS                                     
005900     03 SUBX-KDFRAKT         PIC X(2).                                    
006000*                                 FRAKTSÄTT DC TILL KUND                  
006100*                                 FREIGHT CODE                            
006200     03 SUBX-KDORDKL-MAX     PIC 9.                                       
006300*                                 ORDERKLASS                              
006400*                                 ORDER CLASS                             
006500     03 SUBX-KVKOLLI         PIC Z(3)9.                                   
006600*                                 ANTAL KOLLI                             
006700*                                 NBR OF CASES                            
006800     03 SUBX-VKORDBTO        PIC Z(5)9.9.                                 
006900*                                 ORDERVIKT BRUTTO (KG)                   
007000*                                 GROSS WEIGHT (KG)                       
007100     03 SUBX-KDSORT-VK       PIC X(2).                                    
007200*                                 SORT-KOD                                
007300*                                 UNIT OF MEASURE                         
007400     03 SUBX-VLORDBTO        PIC Z(3)9.9(3).                              
007500*                                 ORDERVOLYM BRUTTO (M3)                  
007600*                                 GROSS VOLUME PER ORDER (M3)             
007700     03 SUBX-KDSORT-VL       PIC X(2).                                    
007800*                                 SORT-KOD                                
007900*                                 UNIT OF MEASURE                         
008000     03 SUBX-PRAVCOST        PIC Z(6)9.9(2).                              
008100*                                 MEDELVÄRDESKOSTNAD I UTL.VALUTA         
008200*                                 AVERAGE COST FOREIGN CURRENCY           
008300     03 SUBX-KDVALISO        PIC X(3).                                    
008400*                                 VALUTAKOD ENLIGT ISO-STANDARD.          
008500*                                 CURRENCY CODE BY ISO-STANDARD.          
008600     03 SUBX-IDFAKT          PIC Z(6)9.                                   
008700*                                 FAKTURANUMMER                           
008800*                                 INVOICE NO.                             
008900*** END OF VILMAII-COPY LENGTH= 588 BYTES                                 
