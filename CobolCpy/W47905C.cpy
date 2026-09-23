000100 01  W47905C.                                                             
000200*                                 EV. ORDER I ARBETE                      
000300     03 IDPTYP               PIC X(3).                                    
000400*                                 POSTTYP                                 
000500     03 IDDISTR              PIC S9(5)           COMP-3.                  
000600*                                 DISTRIKTNUMMER                          
000700     03 IDKUNDNR             PIC S9(7)           COMP-3.                  
000800*                                 KUNDNUMMER                              
000900     03 IDKUNDRF             PIC X(10).                                   
001000*                                 KUNDENS REFERENS (ORDERID)              
001100     03 IDPURAD              PIC S9(5)           COMP-3.                  
001200*                                 RADNUMMER PÅ PACKUNDERLAG               
001300     03 IDDC                 PIC X(2).                                    
001400*                                 IDENTIFIERARE LAGER                     
001500     03 KDFAKTYP             PIC X.                                       
001600*                                 FAKTURATYP                              
001700     03 KDFRAKT              PIC S9(3)           COMP-3.                  
001800*                                 FRAKTSÄTT DC TILL KUND                  
001900     03 KDORDKL              PIC S9              COMP-3.                  
002000*                                 ORDERKLASS                              
002100     03 KVORDRAD-PACK        PIC S9(5)           COMP-3.                  
002200*                                 ANTAL PACKADE ORDERRADER                
002300     03 TIBEGPAC             PIC S9(7)           COMP-3.                  
002400*                                 BEGÄRD PACKNINGSDAG    (ÅÅMMDD)         
002500     03 TIUTSKR              PIC S9(7)           COMP-3.                  
002600*                                 UTSKRIFTDATUM  (ÅÅMMDD)                 
002700     03 TIORDREG             PIC S9(7)           COMP-3.                  
002800*                                 ORDERREGISTRERINGSDATUM  ÅÅMMDD         
002900     03 KDRADSTA             PIC S9              COMP-3.                  
003000*                                 STATUS PÅ ORDERRAD                      
003100     03 IDARTNR              PIC S9(9)           COMP-3.                  
003200*                                 ARTIKELNUMMER                           
003300     03 REKSIFFR             PIC S9              COMP-3.                  
003400*                                 KONTROLLSIFFRA                          
003500     03 IDARTNR-ERS          PIC S9(9)           COMP-3.                  
003600*                                 ERSATT ARTIKELNUMMER                    
003700     03 ADLEVPL              PIC S9(3)           COMP-3.                  
003800*                                 LEVERANSPLATS                           
003900     03 FLDIRLEV             PIC X.                                       
004000*                                 DIREKTLEVERANS ?                        
004100     03 IDPRODNR             PIC S9(7)           COMP-3.                  
004200*                                 PRODUKTIONSNUMMER                       
004300     03 IDKUNDRF-RO          PIC X(10).                                   
004400*                                 KUND REF PÅ RO                          
004500     03 KDORDTYP             PIC S9              COMP-3.                  
004600*                                 ORDERTYP                                
004700     03 KVAVBART             PIC S9(7)           COMP-3.                  
004800*                                 AVBOKAT ANTAL ARTIKLAR                  
004900     03 KVLEVART             PIC S9(7)           COMP-3.                  
005000*                                 LEVERERAT ANTAL STYCK                   
005100     03 KVBEART              PIC S9(7)           COMP-3.                  
005200*                                 BESTÄLLT ANTAL STYCKEN                  
005300     03 TIRODAT              PIC S9(7)           COMP-3.                  
005400*                                 RESTORDERDATUM         (ÅÅMMDD)         
005500     03 KDKVBRYT             PIC S9              COMP-3.                  
005600*                                 KOD OM KVANTFÖRP SKALL BRYTAS           
005700     03 BEVOLREF             PIC X(10).                                   
005800*                                 VOLVO REFERENS                          
005900     03 IDLOPNR              PIC S9(3)           COMP-3.                  
006000*                                 LÖPNUMMER                               
006100     03 FLFYSAVV             PIC X.                                       
006200*                                 FLAGGA FYSISKA AVVIKELSER               
006300*** END OF VILMAII-COPY LENGTH= 104 BYTES                                 
