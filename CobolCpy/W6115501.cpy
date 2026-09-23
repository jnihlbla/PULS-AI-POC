000100 01  W6115501.                                                            
000200*                                 GENOMLOPPSTID PER UPPFÖLJ-              
000300*                                 NINGSSTATUS/ VECKA                      
000400*                                                               .         
000500*                                 WORK FLOW TIME PER FOLLOW-UP            
000600*                                 STATUS AND WEEK                         
000700*                                                               .         
000800     03 IDPTYP               PIC X(3).                                    
000900*                                 POSTTYP                                 
001000*                                 RECORD TYPE                             
001100     03 IDDC                 PIC X(2).                                    
001200*                                 IDENTIFIERARE LAGER                     
001300*                                 WAREHOUSE IDENTIFIER                    
001400     03 KDINLUPF             PIC X(4).                                    
001500*                                 UPPFÖLJNINGSSTATUS INLEVERANS           
001600*                                 FOLLOW-UP STATUS RECEIVING              
001700     03 KVRADER              PIC S9(5)           COMP-3.                  
001800*                                 ANTAL RADER                             
001900*                                 NUMBER OF LINES                         
002000     03 KVRADER-DAG          PIC S9(5)           COMP-3.                  
002100*                                 ANTAL RADER                             
002200*                                 NUMBER OF LINES                         
002300     03 KVRADER-KVALL        PIC S9(5)           COMP-3.                  
002400*                                 ANTAL RADER                             
002500*                                 NUMBER OF LINES                         
002600     03 KVRADER-PRIO         PIC S9(5)           COMP-3.                  
002700*                                 ANTAL RADER                             
002800*                                 NUMBER OF LINES                         
002900     03 KVART                PIC S9(7)           COMP-3.                  
003000*                                 ANTAL ARTNR PER BRYTBEGREPP             
003100*                                 NO OF PARTNOS PER TYPE                  
003200     03 KVART-DAG            PIC S9(7)           COMP-3.                  
003300*                                 ANTAL ARTNR PER BRYTBEGREPP             
003400*                                 NO OF PARTNOS PER TYPE                  
003500     03 KVART-KVALL          PIC S9(7)           COMP-3.                  
003600*                                 ANTAL ARTNR PER BRYTBEGREPP             
003700*                                 NO OF PARTNOS PER TYPE                  
003800     03 SUBEL                PIC S9(9)V9(2).                              
003900*                                 SUMMABELOPP                             
004000*                                 SUM AMOUNT                              
004100     03 SUBEL-PRIO           PIC S9(9)V9(2).                              
004200*                                 SUMMABELOPP                             
004300*                                 SUM AMOUNT                              
004400     03 TIGLT                PIC S9(3)V9(2)      COMP-3.                  
004500*                                 UTFÖRD ARBETSTID (TIMMAR)               
004600*                                 PERFORMED LABOUR TIME (HOURS)           
004700     03 TIGLT-DAG            PIC S9(3)V9(2)      COMP-3.                  
004800*                                 UTFÖRD ARBETSTID (TIMMAR)               
004900*                                 PERFORMED LABOUR TIME (HOURS)           
005000     03 TIGLT-KVALL          PIC S9(3)V9(2)      COMP-3.                  
005100*                                 UTFÖRD ARBETSTID (TIMMAR)               
005200*                                 PERFORMED LABOUR TIME (HOURS)           
005300     03 TIGLT-PRIO           PIC S9(3)V9(2)      COMP-3.                  
005400*                                 UTFÖRD ARBETSTID (TIMMAR)               
005500*                                 PERFORMED LABOUR TIME (HOURS)           
005600     03 TIGLT-PRIO-DAG       PIC S9(3)V9(2)      COMP-3.                  
005700*                                 UTFÖRD ARBETSTID (TIMMAR)               
005800*                                 PERFORMED LABOUR TIME (HOURS)           
005900     03 TIGLT-PRIO-KVALL     PIC S9(3)V9(2)      COMP-3.                  
006000*                                 UTFÖRD ARBETSTID (TIMMAR)               
006100*                                 PERFORMED LABOUR TIME (HOURS)           
006200     03 KVART-MAAL           PIC S9(7)           COMP-3.                  
006300*                                 ANTAL ARTNR PER BRYTBEGREPP             
006400*                                 NO OF PARTNOS PER TYPE                  
006500     03 KVART-MAAL-PRIO      PIC S9(7)           COMP-3.                  
006600*                                 ANTAL ARTNR PER BRYTBEGREPP             
006700*                                 NO OF PARTNOS PER TYPE                  
006800     03 VLARTNTO             PIC S9(8)V9(1)      COMP-3.                  
006900*                                 ARTIKELVOLYM NETTO (CM3)                
007000*                                 PART NET VOLUME    (CM3)                
007100     03 IDLEVNR              PIC X(5).                                    
007200*                                 LEVERANTÖRNUMMER                        
007300*                                 SUPPLIER NUMBER (VENDOR NUMBER)         
007400     03 ADCITY               PIC X(25).                                   
007500*                                 BENÄMNING PÅ STAD                       
007600*                                 CITY                                    
007700*** END OF VILMAII-COPY LENGTH= 116 BYTES                                 
