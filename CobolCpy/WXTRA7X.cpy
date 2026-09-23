000100 01  WXTRA7X.                                                             
000200*                                 INFORMATION, PACKADE RADER              
000300*                                 INFORMATION, PACKED LINES               
000400     03 IDDISTR              PIC Z(3)9                                    
000500                             VALUE ZEROS.                                 
000600*                                 DISTRIKTNUMMER                          
000700*                                 DISTRICT NUMBER                         
000800     03 IDKUNDNR             PIC Z(5)9                                    
000900                             VALUE ZEROS.                                 
001000*                                 KUNDNUMMER                              
001100*                                 CUSTOMER NO                             
001200     03 IDPRODNR             PIC Z(6)9                                    
001300                             VALUE ZEROS.                                 
001400*                                 PRODUKTIONSNUMMER                       
001500*                                 PRODUCTION NUMBER                       
001600     03 IDORDER              PIC Z(6)9                                    
001700                             VALUE ZEROS.                                 
001800*                                 VOLVO PARTS ORDERNUMMER                 
001900*                                 VOLVO PARTS ORDER NUMBER                
002000     03 IDDC                 PIC X(2)                                     
002100                             VALUE SPACES.                                
002200*                                 IDENTIFIERARE LAGER                     
002300*                                 WAREHOUSE IDENTIFIER                    
002400     03 TIBEGPAC             PIC 9(6)                                     
002500                             VALUE ZEROS.                                 
002600*                                 BEGÄRD PACKNINGSDAG    (ÅÅMMDD)         
002700*                                 REQUESTED PACKING DATE (YYMMDD)         
002800     03 TIBEGTID             PIC 9(4)                                     
002900                             VALUE ZEROS.                                 
003000     03 TIORDREG             PIC 9(6)                                     
003100                             VALUE ZEROS.                                 
003200*                                 ORDERREGISTRERINGSDATUM  ÅÅMMDD         
003300*                                 ORDER REGISTRATION DATE  YYMMDD         
003400     03 IDPLKLST             PIC Z(2)9                                    
003500                             VALUE ZEROS.                                 
003600*                                 PLOCKLISTNUMMER                         
003700*                                 PICKING LIST NUMBER                     
003800     03 IDARTNR              PIC Z(7)9                                    
003900                             VALUE ZEROS.                                 
004000*                                 ARTIKELNUMMER                           
004100*                                 PART NUMBER                             
004200     03 ADLAGOMR-VERKLIG     PIC Z9                                       
004300                             VALUE ZEROS.                                 
004400*                                 LAGEROMRÅDE                             
004500*                                 AREA                                    
004600     03 ADLAGOMR-DLB         PIC Z9                                       
004700                             VALUE ZEROS.                                 
004800*                                 LAGEROMRÅDE                             
004900*                                 AREA                                    
005000     03 ADGANG-DLB           PIC Z9                                       
005100                             VALUE ZEROS.                                 
005200*                                 GÅNG                                    
005300*                                 AISLE                                   
005400     03 ADPLATS-DLB          PIC Z(4)9                                    
005500                             VALUE ZEROS.                                 
005600*                                 LAGERPLATSNUMMER                        
005700*                                 LOCATION                                
005800     03 KDFRAKT              PIC Z9                                       
005900                             VALUE ZEROS.                                 
006000*                                 FRAKTSÄTT DC TILL KUND                  
006100*                                 FREIGHT CODE                            
006200     03 KDORDKL              PIC 9                                        
006300                             VALUE ZERO.                                  
006400*                                 ORDERKLASS                              
006500*                                 ORDER CLASS                             
006600     03 KVBEART              PIC Z(5)9                                    
006700                             VALUE ZEROS.                                 
006800*                                 BESTÄLLT ANTAL STYCKEN                  
006900*                                 ORDERED QUANTITY                        
007000     03 KVLEVART             PIC Z(6)9                                    
007100                             VALUE ZEROS.                                 
007200*                                 LEVERERAT ANTAL STYCK                   
007300*                                 DELIVERED QUANTITY                      
007400     03 VKARTNTO             PIC Z(3)9.9(3)                               
007500                             VALUE ZEROS.                                 
007600*                                 ARTIKELVIKT NETTO (KG) MED EMB          
007700*                                 PART NET WEIGHT (KG) W/ PACKAGE         
007800     03 VLARTNTO             PIC Z(7)9.9                                  
007900                             VALUE ZEROS.                                 
008000*                                 ARTIKELVOLYM (CM3)                      
008100*                                 PART VOLUME    (CM3)                    
008200     03 FLDIRLEV             PIC X                                        
008300                             VALUE SPACE.                                 
008400*                                 DIREKTLEVERANS ?                        
008500*                                 DIRECT DELIVERY ?                       
008600     03 IDKUNDRF-RO          PIC X(10)                                    
008700                             VALUE SPACES.                                
008800*                                 KUND REF PÅ RO                          
008900*                                 CUST REF RO                             
009000     03 IDKOLLI              PIC Z(4)9                                    
009100                             VALUE ZEROS.                                 
009200*                                 KOLLINUMMER                             
009300*                                 CASE NUMBER                             
009400     03 IDUSER               PIC X(8)                                     
009500                             VALUE SPACES.                                
009600*                                 ANVÄNDARENS SÄKERHETS ID                
009700*                                 USER SECURITY-IDENTITY                  
009800     03 KVLEVART2            PIC Z(6)9                                    
009900                             VALUE ZEROS.                                 
010000*                                 FAKTISKT LEVERERAT ANTAL I KOLL         
010100*                                 IT                                      
010200*                                 DELIVERED QUANTITY IN ONE CASE          
010300     03 IDPRC.                                                            
010400*                                 PRODUKTIONSKANAL                        
010500*                                 PRODUCTION CHANNEL                      
010600        05 IDPRCBAS          PIC X(3)                                     
010700                             VALUE SPACES.                                
010800*                                 PRC-BAS                                 
010900*                                 PRC-BASIC                               
011000        05 IDPRCVAR          PIC X                                        
011100                             VALUE SPACE.                                 
011200*                                 PRC-VARIANT                             
011300*                                 PRC-VARIANT                             
011400     03 IDSKIFT              PIC X                                        
011500                             VALUE SPACE.                                 
011600*                                 SHIFT IDENTITET                         
011700*                                 SHIFT IDENTITY                          
011800     03 KDPRODSL             PIC Z9                                       
011900                             VALUE ZEROS.                                 
012000*                                 PRODUKTSLAG                             
012100*                                 PRODUCT GROUP                           
012200     03 PRARTSTD             PIC Z(6)9.9(2)                               
012300                             VALUE ZEROS.                                 
012400*                                 ARTIKELSTANDARDPRIS                     
012500*                                 STANDARD PRICE                          
012600     03 TIPACKN              PIC 9(6)                                     
012700                             VALUE ZEROS.                                 
012800*                                 PACKNINGSDATUM         (ÅÅMMDD)         
012900*                                 PACKING DATE           (YYMMDD)         
013000     03 IDPLOCK              PIC Z(5)9                                    
013100                             VALUE ZEROS.                                 
013200*                                 PLOCKARE                                
013300*                                 PICKER                                  
013400     03 TIREGTID             PIC 9(4)                                     
013500                             VALUE ZEROS.                                 
013600*                                 REGISTRERINGSTID                        
013700*                                 GENERAL REGISTRATION TIME               
013800     03 TIUTSKR              PIC 9(6)                                     
013900                             VALUE ZEROS.                                 
014000*                                 UTSKRIFTDATUM  (ÅÅMMDD)                 
014100*                                 PRINTING DATE  (YYMMDD)                 
014200     03 TIUTSTID             PIC 9(4)                                     
014300                             VALUE ZEROS.                                 
014400*                                 UTSKRIFTSTID (TTMMSS)                   
014500*                                 TIME OF PRINTING (HHMMSS)               
014600     03 TIPACTID             PIC 9(4)                                     
014700                             VALUE ZEROS.                                 
014800*                                 PACKNINGSTID  TTMMSS                    
014900*                                 PACKING TIME  HHMMSS                    
015000     03 KDPRCGRP             PIC X(5)                                     
015100                             VALUE SPACES.                                
015200*                                 PRODUKTIONSKANALSGRUPP                  
015300*                                 GROUP OF PRODUCTION CHANNELS            
015400*** END OF VILMAII-COPY LENGTH= 181 BYTES                                 
