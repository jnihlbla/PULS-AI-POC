000100 01  W4639A.                                                              
000200*                                 PARTS FOR DIRECT DELIVERIES             
000300*                                                                         
000400     03 IDPTYP               PIC X.                                       
000500*                                 POSTTYP              IDPTYP-001         
000600*                                 RECORD TYPE          IDPTYP-001         
000700     03 IDLEVNR              PIC X(5).                                    
000800*                                 LEVERANT÷RNUMMER                        
000900*                                 SUPPLIER NUMBER (VENDOR NUMBER)         
001000     03 IDLOPNR              PIC 9(5).                                    
001100*                                 L÷PNUMMER          IDLOPNR-002          
001200     03 IDDISTR              PIC 9(4).                                    
001300*                                 DISTRIKTNUMMER                          
001400*                                 DISTRICT NUMBER                         
001500     03 IDKUNDNR             PIC 9(6).                                    
001600*                                 KUNDNUMMER                              
001700*                                 CUSTOMER NO                             
001800     03 IDORDNR7             PIC 9(7).                                    
001900*                                 ORDERNUMMER                             
002000*                                 ORDER NUMBER                            
002100     03 IDARTNR              PIC 9(8).                                    
002200*                                 ARTIKELNUMMER                           
002300*                                 PART NUMBER                             
002400     03 TIUTSKR              PIC 9(6).                                    
002500*                                 UTSKRIFTDATUM  (≈≈MMDD)                 
002600*                                 PRINTING DATE  (YYMMDD)                 
002700     03 TIUTSTID             PIC 9(6).                                    
002800*                                 UTSKRIFTSTID (TTMMSS)                   
002900*                                 TIME OF PRINTING (HHMMSS)               
003000     03 IDPRODNR             PIC 9(7).                                    
003100*                                 PRODUKTIONSNUMMER                       
003200*                                 PRODUCTION NUMBER                       
003300     03 KDORDKL              PIC 9.                                       
003400*                                 ORDERKLASS                              
003500*                                 ORDER CLASS                             
003600     03 KVBEART              PIC 9(6).                                    
003700*                                 BESTƒLLT ANTAL STYCKEN                  
003800*                                 ORDERED QUANTITY                        
003900     03 KVLEVART             PIC 9(7).                                    
004000*                                 LEVERERAT ANTAL STYCK                   
004100*                                 DELIVERED QUANTITY                      
004200     03 IDPURAD              PIC S9(5)           COMP-3.                  
004300*                                 RADNUMMER P≈ PACKUNDERLAG               
004400*                                 LINENO IN PACKINGDOCUMENT               
004500     03 KDANNULL             PIC X.                                       
004600*                                 CANCELLATION CODE                       
004700*                                 CANCELLATION CODE                       
004800     03 DAREGDAT             PIC 9(8).                                    
004900*                                 REGISTRERINGSDATUM (≈≈≈≈MMDD)           
005000*                                 REGISTRATION DATE (YYYYMMDD)            
005100     03 DALEVDAT             PIC 9(8).                                    
005200*                                 F÷RSENAT LEVERANSDATUM                  
005300*                                 DELAYED DELIVERY-DATE                   
005400     03 KVANTAL              PIC 9(6).                                    
005500*                                 ANTAL                                   
005600*                                 NUMBER                                  
005700     03 KDORDBEK             PIC 9(2).                                    
005800*                                 ORDERBEKRƒFTELSEKOD                     
005900*                                 ORDERCONFIMATIONCODE                    
006000     03 TISKEPPN-DDC         PIC 9(6).                                    
006100*                                 SKEPPNINGSDATUM DLEV (≈≈MMDD)           
006200*                                 SHIPPING DATE DIR.LEV. (YYMMDD)         
006300*** END OF VILMAII-COPY LENGTH= 103 BYTES                                 
