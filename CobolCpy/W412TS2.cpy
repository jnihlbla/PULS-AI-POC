000100 01  W412TS2-CTX.                                                         
000200*                                 TYP = TS2 TACDIS AO                     
000300*                                 TYP = TS3 TACDIS BO                     
000400*                                 TYP = TS4 TACDIS AO-AKUT                
000500*                                 TYP = TS5 TACDIS BO-AKUT                
000600*                                 TYP = TS6 TACDIS ANNULLATION            
000700*                                                                         
000800     03 IDPTYP               PIC X(3).                                    
000900*                                 POSTTYP                                 
001000     03 IDDISTR              PIC 9(4).                                    
001100*                                 DISTRIKTNUMMER                          
001200     03 IDKUNDNR             PIC 9(6).                                    
001300*                                 KUNDNUMMER                              
001400     03 IDORDNR7             PIC 9(7).                                    
001500*                                 ORDERNUMMER                             
001600     03 KDORDKL              PIC 9.                                       
001700*                                 ORDERKLASS                              
001800     03 BERADREF             PIC X(10).                                   
001900*                                 KUNDENS RADREFERENS                     
002000     03 TIBEGPAC             PIC 9(6).                                    
002100*                                 BEGÄRD PACKNINGSDAG    (ÅÅMMDD)         
002200     03 IDARTPRE             PIC X(3).                                    
002300*                                 IDENTIFIERARE ARTIKELSORTIMENT          
002400     03 IDARTBET             PIC X(17).                                   
002500*                                 ARTIKELBETECKNING EFTERMARKNAD          
002600     03 KVBEART              PIC 9(6).                                    
002700*                                 BESTÄLLT ANTAL STYCKEN                  
002800     03 IDDEPT               PIC 9(2).                                    
002900*                                 AVDELNING I VERKSTAD                    
003000     03 BEGMT-RAD1           PIC X(35).                                   
003100*                                 GODSMOTTAGARNAMN RAD 1                  
003200     03 BEGMT-RAD2           PIC X(35).                                   
003300*                                 GODSMOTTAGARNAMN RAD 2                  
003400     03 ADGMT-GATA           PIC X(35).                                   
003500*                                 GODSMOTTAGARADRESS GATA                 
003600     03 ADGMT-PADR           PIC X(35).                                   
003700*                                 GODSMOTTAGARADRESS POSTADRESS           
003800     03 FLRESTN              PIC X.                                       
003900*                                 RESTNOTERING ?                          
004000     03 KDORDTYP-TACDIS      PIC X(2).                                    
004100*                                 ORDERTYP HOS TACDIS                     
004200     03 IDGROSS              PIC X(3).                                    
004300*                                 GROSSIST KUNDNUMMER FRÅN TACDIS         
004400     03 IDBILREG             PIC X(10).                                   
004500*                                 BILENS REGISTRERINGSNUMMER              
004600     03 IDVIN                PIC X(17).                                   
004700*                                 VIN ID FORDON                           
004800     03 IDCISNR              PIC X(12).                                   
004900*                                 CIS NUMMER                              
005000     03 TETACDBO             PIC X(35).                                   
005100*                                 REFERENS BUTIK ORDER TACDIS             
005200     03 BETELNR-TACD         PIC X(25).                                   
005300*                                 TELEFONNUMMER SMS BUTIKSORDER           
005400     03 BEMEKAN              PIC X(15).                                   
005500*                                 FÖRVALD MEKANIKER/VERKSTAD              
005600     03 FLFPLOCK             PIC X.                                       
005700*                                 FÖRLEVERANSINDIKATOR                    
005800     03 TIHHMM               PIC 9(4).                                    
005900*                                 KLOCKSLAG (TIMMAR OCH MINUTER)          
006000*** END OF VILMAII-COPY LENGTH= 330 BYTES                                 
