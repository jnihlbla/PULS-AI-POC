000100 01  4008-WDGX4008.                                                       
000200*                                 UTSKRIFT AV PLOCKSATSER                 
000300*                                 PLOCKUNDERLAG                           
000400*                                 FYSISK NYCKEL: KDSEGKEY                 
000500     03 4008-KDSEGKEY        PIC X.                                       
000600*                                 TEKNISK SEGMENT-NYCKEL                  
000700*                                 TECHNICAL SEGMENT KEY                   
000800     03 4008-IDLOPNR-PL      PIC S9(3)           COMP-3.                  
000900*                                 PLOCKSATSENS LÖPNUMMER INOM             
001000*                                 PRC-GRUPP                               
001100*                                 SEQUENCE-NUMBER FOR THE                 
001200*                                 PICKING UNIT WITHIN PRC-GROUP           
001300     03 4008-IDPRC.                                                       
001400*                                 PRODUKTIONSKANAL                        
001500*                                 PRODUCTION CHANNEL                      
001600        05 4008-IDPRCBAS     PIC X(3).                                    
001700*                                 PRC-BAS                                 
001800*                                 PRC-BASIC                               
001900        05 4008-IDPRCVAR     PIC X.                                       
002000*                                 PRC-VARIANT                             
002100*                                 PRC-VARIANT                             
002200     03 4008-IDSID           PIC S9(3)           COMP-3.                  
002300*                                 SIDNUMRERING                            
002400*                                 PAGE NUMBER                             
002500     03 4008-IDDC            PIC X(2).                                    
002600*                                 IDENTIFIERARE LAGER                     
002700*                                 WAREHOUSE IDENTIFIER                    
002800     03 4008-KDPRT-PU        PIC X(3).                                    
002900*                                 PRINTERKOD PACKUNDERLAG                 
003000*                                 PRINTERCODE PACKING UNIT                
003100     03 4008-KVRADER         OCCURS 3 TIMES                               
003200                             PIC S9(5)           COMP-3.                  
003300*                                 ANTAL RADER                             
003400*                                 NUMBER OF LINES                         
003500     03 4008-VKORDNTO        OCCURS 3 TIMES                               
003600                             PIC S9(6)V9(1)      COMP-3.                  
003700*                                 ORDERVIKT NETTO (KG)                    
003800*                                 WEIGHT PER ORDER NETTO (KG)             
003900     03 4008-VLORDNTO        OCCURS 3 TIMES                               
004000                             PIC S9(4)V9(3)      COMP-3.                  
004100*                                 ORDERVOLYM NETTO (M3)                   
004200*                                 NET VOLUME PER ORDER (M3)               
004300     03 4008-NYCKEL-GRP.                                                  
004400        05 4008-IDORDER      PIC S9(7)           COMP-3.                  
004500*                                 VOLVO PARTS ORDERNUMMER                 
004600*                                 VOLVO PARTS ORDER NUMBER                
004700        05 4008-KDPRT        PIC X(3).                                    
004800*                                 PRINTERKOD                              
004900*                                 PRINTERCODE                             
005000        05 4008-KDSS         PIC X.                                       
005100*                                 SIDOSKIPSKOD                            
005200*                                 CODE FOR PAGESKIP                       
005300        05 4008-ADLAGOMR     PIC S9(3)           COMP-3.                  
005400*                                 LAGEROMRÅDE                             
005500*                                 AREA                                    
005600        05 4008-ADGANG       PIC S9(3)           COMP-3.                  
005700*                                 GÅNG                                    
005800*                                 AISLE                                   
005900        05 4008-ADPLATS      PIC S9(5)           COMP-3.                  
006000*                                 LAGERPLATSNUMMER                        
006100*                                 LOCATION                                
006200        05 4008-IDARTNR      PIC S9(9)           COMP-3.                  
006300*                                 ARTIKELNUMMER                           
006400*                                 PART NUMBER                             
006500        05 4008-IDLOPNR      PIC S9(3)           COMP-3.                  
006600*                                 LÖPNUMMER                               
006700*                                 SEQUENCE NUMBER                         
006800     03 4008-KVRADER-GRP     OCCURS 101 TIMES                             
006900                             PIC S9(5)           COMP-3.                  
007000*                                 ANTAL RADER                             
007100*                                 NUMBER OF LINES                         
007200     03 4008-IDMSG3IV        PIC X(30).                                   
007300*                                 3IV MEDDELANDE-ID                       
007400*                                 3IV MESSAGE ID                          
007500     03 4008-IDSNO3IV        PIC X(25).                                   
007600*                                 SERIENR PÅ TERMINAL I 3IV               
007700*                                 SERIAL NO OF TERMINAL IN 3IV            
007800     03 4008-ADDISPXTRA      PIC X(20).                                   
007900*                                 ADDRESSTILLÄGG                          
008000*                                 ADDRESS EXTENTION                       
008100     03 4008-FILLER          PIC X(25).                                   
008200*** END OF VILMAII-COPY LENGTH= 472 BYTES                                 
