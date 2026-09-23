000100 01  WXTRA0.                                                              
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
002200     03 TIORDREG             PIC 9(6).                                    
002300*                                 ORDERREGISTRERINGSDATUM  ÅÅMMDD         
002400*                                 ORDER REGISTRATION DATE  YYMMDD         
002500     03 IDPLKLST             PIC 9(3).                                    
002600*                                 PLOCKLISTNUMMER                         
002700*                                 PICKING LIST NUMBER                     
002800     03 IDARTNR              PIC 9(8).                                    
002900*                                 ARTIKELNUMMER                           
003000*                                 PART NUMBER                             
003100     03 ADLAGOMR             PIC 9(2).                                    
003200*                                 LAGEROMRÅDE                             
003300*                                 AREA                                    
003400     03 KDFRAKT              PIC 9(2).                                    
003500*                                 FRAKTSÄTT DC TILL KUND                  
003600*                                 FREIGHT CODE                            
003700     03 KDORDKL              PIC 9.                                       
003800*                                 ORDERKLASS                              
003900*                                 ORDER CLASS                             
004000     03 KVBEART              PIC 9(6).                                    
004100*                                 BESTÄLLT ANTAL STYCKEN                  
004200*                                 ORDERED QUANTITY                        
004300     03 KVLEVART             PIC 9(7).                                    
004400*                                 LEVERERAT ANTAL STYCK                   
004500*                                 DELIVERED QUANTITY                      
004600     03 VKARTNTO             PIC 9(4)V9(3).                               
004700*                                 ARTIKELVIKT NETTO (KG)                  
004800*                                 PART NET WEIGHT (KG)                    
004900     03 VLARTNTO             PIC 9(8)V9(1).                               
005000*                                 ARTIKELVOLYM NETTO (CM3)                
005100*                                 PART NET VOLUME    (CM3)                
005200     03 FLDIRLEV             PIC X.                                       
005300*                                 DIREKTLEVERANS ?                        
005400*                                 DIRECT DELIVERY ?                       
005500     03 IDKUNDRF-RO          PIC X(10).                                   
005600*                                 KUND REF PÅ RO                          
005700*                                 CUST REF RO                             
005800     03 IDKOLLI              PIC 9(5).                                    
005900*                                 KOLLINUMMER                             
006000*                                 CASE NUMBER                             
006100     03 IDUSER               PIC X(8).                                    
006200*                                 ANVÄNDARENS SÄKERHETS ID                
006300*                                 USER SECURITY-IDENTITY                  
006400     03 KVLEVART2            PIC 9(7).                                    
006500*                                 FAKTISKT LEVERERAT ANTAL I KOLL         
006600*                                 IT                                      
006700*                                 DELIVERED QUANTITY IN ONE CASE          
006800     03 IDPRC.                                                            
006900*                                 PRODUKTIONSKANAL                        
007000*                                 PRODUCTION CHANNEL                      
007100        05 IDPRCBAS          PIC X(3).                                    
007200*                                 PRC-BAS                                 
007300*                                 PRC-BASIC                               
007400        05 IDPRCVAR          PIC X.                                       
007500*                                 PRC-VARIANT                             
007600*                                 PRC-VARIANT                             
007700     03 IDSKIFT              PIC X.                                       
007800*                                 SHIFT IDENTITET                         
007900*                                 SHIFT IDENTITY                          
008000     03 KDPRODSL             PIC 9(2).                                    
008100*                                 PRODUKTSLAG                             
008200*                                 PRODUCT GROUP                           
008300     03 PRARTSTD             PIC 9(7)V9(2).                               
008400*                                 ARTIKELSTANDARDPRIS                     
008500*                                 STANDARD PRICE                          
008600     03 TIPACKN              PIC 9(6).                                    
008700*                                 PACKNINGSDATUM         (ÅÅMMDD)         
008800*                                 PACKING DATE           (YYMMDD)         
008900     03 IDPLOCK              PIC 9(6).                                    
009000*                                 PLOCKARE                                
009100*                                 PICKER                                  
009200*** END OF VILMAII-COPY LENGTH= 142 BYTES                                 
