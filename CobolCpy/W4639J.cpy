000100 01  W4639J.                                                              
000200*                                 LARM F÷RSENADE DIREKTLEVERANSER         
000300*                                                                         
000400     03 IDPTYP               PIC X(3).                                    
000500*                                 POSTTYP                                 
000600*                                 RECORD TYPE                             
000700     03 IDLEVNR              PIC X(5).                                    
000800*                                 LEVERANT÷RNUMMER                        
000900*                                 SUPPLIER NUMBER (VENDOR NUMBER)         
001000     03 IDPRODNR             PIC 9(7).                                    
001100*                                 PRODUKTIONSNUMMER                       
001200*                                 PRODUCTION NUMBER                       
001300     03 IDPURAD              PIC 9(4).                                    
001400*                                 RADNUMMER P≈ PACKUNDERLAG               
001500*                                 LINENO IN PACKINGDOCUMENT               
001600     03 TIUTSKR              PIC 9(6).                                    
001700*                                 UTSKRIFTDATUM  (≈≈MMDD)                 
001800*                                 PRINTING DATE  (YYMMDD)                 
001900     03 IDARTNR              PIC 9(8).                                    
002000*                                 ARTIKELNUMMER                           
002100*                                 PART NUMBER                             
002200     03 IDDISTR              PIC 9(4).                                    
002300*                                 DISTRIKTNUMMER                          
002400*                                 DISTRICT NUMBER                         
002500     03 IDKUNDNR             PIC 9(6).                                    
002600*                                 KUNDNUMMER                              
002700*                                 CUSTOMER NO                             
002800     03 IDORDNR5             PIC 9(5).                                    
002900*                                 ORDERNUMMER                             
003000*                                 ORDER NUMBER                            
003100     03 KDANNULL             PIC X.                                       
003200*                                 CANCELLATION CODE                       
003300*                                 CANCELLATION CODE                       
003400     03 KVSLULEV             PIC 9(3).                                    
003500*                                 ANTAL LEVERANSF÷RSENINGAR               
003600*                                 HOW MANY DELAYED DELIVERIES             
003700     03 TIANNULL             PIC 9(6).                                    
003800*                                 ANNULLATIONSDATUM (≈≈MMDD)              
003900*                                 CANCELLATION DATE (YYMMDD)              
004000     03 TISKEPPN             PIC 9(6).                                    
004100*                                 SKEPPNINGSDATUM  (≈≈MMDD)               
004200*                                 SHIPPING DATE    (YYMMDD)               
004300     03 TISLULEV             PIC 9(6).                                    
004400*                                 LEVERANSF÷RSENING (≈≈MMDD)              
004500*                                 DELAYED DELIVERY (YYMMDD)               
004600     03 KDOLD                PIC X.                                       
004700*** END OF VILMAII-COPY LENGTH= 71 BYTES                                  
