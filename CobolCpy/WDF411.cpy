000100 01  DLOR-WDF411.                                                         
000200*                                 ORDERRADER HOS DIREKTLEVERANT÷R         
000300*                                 ORDERRAD INFO                           
000400*                                 FYSISK NYCKEL: WDF411KY                 
000500*                                 (IDPRODNR + IDPURAD + TIUTSKR)          
000600     03 DLOR-IDPRODNR        PIC S9(7)           COMP-3.                  
000700*                                 PRODUKTIONSNUMMER                       
000800*                                 PRODUCTION NUMBER                       
000900     03 DLOR-IDPURAD         PIC S9(5)           COMP-3.                  
001000*                                 RADNUMMER P≈ PACKUNDERLAG               
001100*                                 LINENO IN PACKINGDOCUMENT               
001200     03 DLOR-TIUTSKR         PIC S9(7)           COMP-3.                  
001300*                                 UTSKRIFTDATUM  (≈≈MMDD)                 
001400*                                 PRINTING DATE  (YYMMDD)                 
001500     03 DLOR-IDARTNR         PIC S9(9)           COMP-3.                  
001600*                                 ARTIKELNUMMER                           
001700*                                 PART NUMBER                             
001800     03 DLOR-IDDISTR         PIC S9(5)           COMP-3.                  
001900*                                 DISTRIKTNUMMER                          
002000*                                 DISTRICT NUMBER                         
002100     03 DLOR-IDKUNDNR        PIC S9(7)           COMP-3.                  
002200*                                 KUNDNUMMER                              
002300*                                 CUSTOMER NO                             
002400     03 DLOR-IDORDNR7        PIC 9(7).                                    
002500*                                 ORDERNUMMER                             
002600*                                 ORDER NUMBER                            
002700     03 DLOR-KDANNULL        PIC X.                                       
002800*                                 CANCELLATION CODE                       
002900*                                 CANCELLATION CODE                       
003000     03 DLOR-KVSLULEV        PIC S9(3)           COMP-3.                  
003100*                                 ANTAL LEVERANSF÷RSENINGAR               
003200*                                 HOW MANY DELAYED DELIVERIES             
003300     03 DLOR-TIANNULL        PIC S9(7)           COMP-3.                  
003400*                                 ANNULLATIONSDATUM (≈≈MMDD)              
003500*                                 CANCELLATION DATE (YYMMDD)              
003600     03 DLOR-TISLULEV        PIC S9(7)           COMP-3.                  
003700*                                 LEVERANSF÷RSENING (≈≈MMDD)              
003800*                                 DELAYED DELIVERY (YYMMDD)               
003900     03 DLOR-TISKEPPN        PIC S9(7)           COMP-3.                  
004000*                                 SKEPPNINGSDATUM  (≈≈MMDD)               
004100*                                 SHIPPING DATE    (YYMMDD)               
004200     03 DLOR-TIUTSTID        PIC S9(7)           COMP-3.                  
004300*                                 UTSKRIFTSTID (TTMMSS)                   
004400*                                 TIME OF PRINTING (HHMMSS)               
004500     03 DLOR-TIPACKN         PIC S9(7)           COMP-3.                  
004600*                                 PACKNINGSDATUM         (≈≈MMDD)         
004700*                                 PACKING DATE           (YYMMDD)         
004800     03 DLOR-FILLER          PIC X.                                       
004900*** END OF VILMAII-COPY LENGTH= 54 BYTES                                  
