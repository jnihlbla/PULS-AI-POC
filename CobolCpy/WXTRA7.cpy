000100 01  WXTRA7.                                                              
000200*                                 INFORMATION, PACKADE RADER              
000300*                                 INFORMATION, PACKED LINES               
000400     03 IDDISTR              PIC 9(4).                                    
000500*                                 DISTRIKTNUMMER                          
000600*                                 DISTRICT NUMBER                         
000700     03 IDKUNDNR             PIC 9(6).                                    
000800*                                 KUNDNUMMER                              
000900*                                 CUSTOMER NO                             
001000     03 IDPRODNR             PIC 9(7).                                    
001100*                                 PRODUKTIONSNUMMER                       
001200*                                 PRODUCTION NUMBER                       
001300     03 IDORDER              PIC 9(7).                                    
001400*                                 VOLVO PARTS ORDERNUMMER                 
001500*                                 VOLVO PARTS ORDER NUMBER                
001600     03 IDDC                 PIC X(2).                                    
001700*                                 IDENTIFIERARE LAGER                     
001800*                                 WAREHOUSE IDENTIFIER                    
001900     03 TIBEGPAC             PIC 9(6).                                    
002000*                                 BEGÄRD PACKNINGSDAG    (ÅÅMMDD)         
002100*                                 REQUESTED PACKING DATE (YYMMDD)         
002200     03 TIBEGTID             PIC 9(4).                                    
002300     03 TIORDREG             PIC 9(6).                                    
002400*                                 ORDERREGISTRERINGSDATUM  ÅÅMMDD         
002500*                                 ORDER REGISTRATION DATE  YYMMDD         
002600     03 IDPLKLST             PIC 9(3).                                    
002700*                                 PLOCKLISTNUMMER                         
002800*                                 PICKING LIST NUMBER                     
002900     03 IDARTNR              PIC 9(8).                                    
003000*                                 ARTIKELNUMMER                           
003100*                                 PART NUMBER                             
003200     03 ADLAGOMR-VERKLIG     PIC 9(2).                                    
003300*                                 LAGEROMRÅDE                             
003400*                                 AREA                                    
003500     03 ADLAGOMR-DLB         PIC 9(2).                                    
003600*                                 LAGEROMRÅDE                             
003700*                                 AREA                                    
003800     03 ADGANG-DLB           PIC 9(2).                                    
003900*                                 GÅNG                                    
004000*                                 AISLE                                   
004100     03 ADPLATS-DLB          PIC 9(5).                                    
004200*                                 LAGERPLATSNUMMER                        
004300*                                 LOCATION                                
004400     03 KDFRAKT              PIC 9(2).                                    
004500*                                 FRAKTSÄTT DC TILL KUND                  
004600*                                 FREIGHT CODE                            
004700     03 KDORDKL              PIC 9.                                       
004800*                                 ORDERKLASS                              
004900*                                 ORDER CLASS                             
005000     03 KVBEART              PIC 9(6).                                    
005100*                                 BESTÄLLT ANTAL STYCKEN                  
005200*                                 ORDERED QUANTITY                        
005300     03 KVLEVART             PIC 9(7).                                    
005400*                                 LEVERERAT ANTAL STYCK                   
005500*                                 DELIVERED QUANTITY                      
005600     03 VKARTNTO             PIC 9(4)V9(3).                               
005700*                                 ARTIKELVIKT NETTO (KG) MED EMB          
005800*                                 PART NET WEIGHT (KG) W/ PACKAGE         
005900     03 VLARTNTO             PIC 9(8)V9(1).                               
006000*                                 ARTIKELVOLYM (CM3)                      
006100*                                 PART VOLUME    (CM3)                    
006200     03 FLDIRLEV             PIC X.                                       
006300*                                 DIREKTLEVERANS ?                        
006400*                                 DIRECT DELIVERY ?                       
006500     03 IDKUNDRF-RO          PIC X(10).                                   
006600*                                 KUND REF PÅ RO                          
006700*                                 CUST REF RO                             
006800     03 IDKOLLI              PIC 9(5).                                    
006900*                                 KOLLINUMMER                             
007000*                                 CASE NUMBER                             
007100     03 IDUSER               PIC X(8).                                    
007200*                                 ANVÄNDARENS SÄKERHETS ID                
007300*                                 USER SECURITY-IDENTITY                  
007400     03 KVLEVART2            PIC 9(7).                                    
007500*                                 FAKTISKT LEVERERAT ANTAL I KOLL         
007600*                                 IT                                      
007700*                                 DELIVERED QUANTITY IN ONE CASE          
007800     03 IDPRC.                                                            
007900*                                 PRODUKTIONSKANAL                        
008000*                                 PRODUCTION CHANNEL                      
008100        05 IDPRCBAS          PIC X(3).                                    
008200*                                 PRC-BAS                                 
008300*                                 PRC-BASIC                               
008400        05 IDPRCVAR          PIC X.                                       
008500*                                 PRC-VARIANT                             
008600*                                 PRC-VARIANT                             
008700     03 IDSKIFT              PIC X.                                       
008800*                                 SHIFT IDENTITET                         
008900*                                 SHIFT IDENTITY                          
009000     03 KDPRODSL             PIC 9(2).                                    
009100*                                 PRODUKTSLAG                             
009200*                                 PRODUCT GROUP                           
009300     03 PRARTSTD             PIC 9(7)V9(2).                               
009400*                                 ARTIKELSTANDARDPRIS                     
009500*                                 STANDARD PRICE                          
009600     03 TIPACKN              PIC 9(6).                                    
009700*                                 PACKNINGSDATUM         (ÅÅMMDD)         
009800*                                 PACKING DATE           (YYMMDD)         
009900     03 IDPLOCK              PIC 9(6).                                    
010000*                                 PLOCKARE                                
010100*                                 PICKER                                  
010200     03 TIREGTID             PIC 9(4).                                    
010300*                                 REGISTRERINGSTID                        
010400*                                 GENERAL REGISTRATION TIME               
010500     03 TIUTSKR              PIC 9(6).                                    
010600*                                 UTSKRIFTDATUM  (ÅÅMMDD)                 
010700*                                 PRINTING DATE  (YYMMDD)                 
010800     03 TIUTSTID             PIC 9(4).                                    
010900*                                 UTSKRIFTSTID (TTMMSS)                   
011000*                                 TIME OF PRINTING (HHMMSS)               
011100     03 TIPACTID             PIC 9(4).                                    
011200*                                 PACKNINGSTID  TTMMSS                    
011300*                                 PACKING TIME  HHMMSS                    
011400     03 KDPRCGRP             PIC X(5).                                    
011500*                                 PRODUKTIONSKANALSGRUPP                  
011600*                                 GROUP OF PRODUCTION CHANNELS            
011700*** END OF VILMAII-COPY LENGTH= 178 BYTES                                 
