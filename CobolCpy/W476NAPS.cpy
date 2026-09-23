000100 01  SUBH-W476NAPS.                                                       
000200*                                 COPYTEXT FOR NA PROFORMA                
000300*                                 SUB HEADER (NORMAL)                     
000400*                                 RECORD TYPE = S                         
000500     03 SUBH-IDAFPRCD        PIC X(10).                                   
000600*                                 AFP-BLANKETT POSTTYP                    
000700*                                 AFP FORMS RECORD TYPE                   
000800     03 SUBH-TO-IDDC         PIC X(2).                                    
000900*                                 IDENTIFIERARE LAGER                     
001000*                                 WAREHOUSE IDENTIFIER                    
001100     03 SUBH-BEGMT-RAD1      PIC X(35).                                   
001200*                                 GODSMOTTAGARNAMN RAD 1                  
001300*                                 GOODS RECEIVER NAME LINE 1              
001400     03 SUBH-ADGMT-GATA      PIC X(35).                                   
001500*                                 GODSMOTTAGARADRESS GATA                 
001600*                                 GOODS RECEIVER ADDRESS STREET           
001700     03 SUBH-BEGMT-RAD2      PIC X(35).                                   
001800*                                 GODSMOTTAGARNAMN RAD 2                  
001900*                                 GOODS RECEIVER NAME LINE 2              
002000     03 SUBH-ADGMT-PADR      PIC X(35).                                   
002100*                                 GODSMOTTAGARADRESS POSTADRESS           
002200*                                 GOODS RECEIVER ADDRESS TOWN             
002300     03 SUBH-ADGMT-LAND      PIC X(35).                                   
002400*                                 GODSMOTTAGARADRESS LAND                 
002500*                                 GOODS RECEIVER ADDRESS COUNTRY          
002600     03 SUBH-KDFRAKT         PIC X(2).                                    
002700*                                 FRAKTSÄTT DC TILL KUND                  
002800*                                 FREIGHT CODE                            
002900     03 SUBH-IDFAKT          PIC Z(6)9.                                   
003000*                                 FAKTURANUMMER                           
003100*                                 INVOICE NO.                             
003200     03 SUBH-KDORDKL-MAX     PIC 9.                                       
003300*                                 ORDERKLASS                              
003400*                                 ORDER CLASS                             
003500     03 SUBH-KVKOLLI         PIC Z(3)9.                                   
003600*                                 ANTAL KOLLI                             
003700*                                 NBR OF CASES                            
003800     03 SUBH-VKORDBTO        PIC Z(5)9.9.                                 
003900*                                 ORDERVIKT BRUTTO (KG)                   
004000*                                 GROSS WEIGHT (KG)                       
004100     03 SUBH-KDSORT-VK       PIC X(2).                                    
004200*                                 SORT-KOD                                
004300*                                 UNIT OF MEASURE                         
004400     03 SUBH-VLORDBTO        PIC Z(3)9.9(3).                              
004500*                                 ORDERVOLYM BRUTTO (M3)                  
004600*                                 GROSS VOLUME PER ORDER (M3)             
004700     03 SUBH-KDSORT-VL       PIC X(2).                                    
004800*                                 SORT-KOD                                
004900*                                 UNIT OF MEASURE                         
005000     03 SUBH-PRAVCOST        PIC Z(6)9.9(2).                              
005100*                                 MEDELVÄRDESKOSTNAD I UTL.VALUTA         
005200*                                 AVERAGE COST FOREIGN CURRENCY           
005300     03 SUBH-KDVALISO        PIC X(3).                                    
005400*                                 VALUTAKOD ENLIGT ISO-STANDARD.          
005500*                                 CURRENCY CODE BY ISO-STANDARD.          
005600*** END OF VILMAII-COPY LENGTH= 234 BYTES                                 
