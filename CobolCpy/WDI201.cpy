000100 01  TAKF-WDI201.                                                         
000200*                                 TACDIS KOLLIFLAGGAN                     
000300*                                 FYSISK NYCKEL: IDGMTREF                 
000400*                                 (IDDISTR + IDKUNDNR + IDKUNDRF)         
000500*                                                                         
000600     03 TAKF-IDGMTREF.                                                    
000700*                                 GODSMOTTAGAREREFERENS                   
000800*                                 GOODS RECEIVER REFERENS                 
000900        05 TAKF-IDDISTR      PIC S9(5)           COMP-3.                  
001000*                                 DISTRIKTNUMMER                          
001100*                                 DISTRICT NUMBER                         
001200        05 TAKF-IDKUNDNR     PIC S9(7)           COMP-3.                  
001300*                                 KUNDNUMMER                              
001400*                                 CUSTOMER NO                             
001500        05 TAKF-IDKUNDRF-GRP.                                             
001600*                                 KUNDENS REFERENS (ORDERID)              
001700*                                 CUSTOMER REFERENCE (ORDER ID)           
001800           07 TAKF-IDKUNDRF  PIC X(10).                                   
001900*                                 KUNDENS REFERENS (ORDERID)              
002000*                                 CUSTOMER REFERENCE (ORDER ID)           
002100           07 TAKF-IDORDNR5-FILLER REDEFINES TAKF-IDKUNDRF.               
002200              09 TAKF-IDORDNR5                                            
002300                             PIC 9(5).                                    
002400*                                 ORDERNUMMER                             
002500*                                 ORDER NUMBER                            
002600              09 FILLER      PIC X(5).                                    
002700           07 TAKF-IDORDNR7-FILLER REDEFINES TAKF-IDKUNDRF.               
002800              09 TAKF-IDORDNR7                                            
002900                             PIC 9(7).                                    
003000*                                 ORDERNUMMER                             
003100*                                 ORDER NUMBER                            
003200              09 FILLER      PIC X(3).                                    
003300     03 TAKF-BEMEKAN         PIC X(15).                                   
003400*                                 F÷RVALD MEKANIKER/VERKSTAD              
003500*                                 DEFAULT MECHANIC/WORKPLACE              
003600     03 TAKF-BETELNR-TACD    PIC X(25).                                   
003700*                                 TELEFONNUMMER SMS BUTIKSORDER           
003800*                                 TELEPHONE NO SHOP ORDERS                
003900     03 TAKF-FLFPLOCK        PIC X.                                       
004000*                                 F÷RLEVERANSINDIKATOR                    
004100*                                 IND PRE PREPICK                         
004200     03 TAKF-IDBILREG        PIC X(10).                                   
004300*                                 BILENS REGISTRERINGSNUMMER              
004400*                                 CAR REGISTRATION NUMBER                 
004500     03 TAKF-IDGROSS         PIC 9(3).                                    
004600*                                 GROSSIST KUNDNUMMER FR≈N TACDIS         
004700*                                 BRANCH NUMBER FROM TACDIS               
004800     03 TAKF-TETACDBO        PIC X(35).                                   
004900*                                 REFERENS BUTIK ORDER TACDIS             
005000*                                 REFERENCE SHOP ORDERS TACDIS            
005100     03 TAKF-TIREGDAT        PIC S9(7)           COMP-3.                  
005200*                                 REGISTRERINGSDATUM (≈≈MMDD)             
005300*                                 REGISTRATION DATE (YYMMDD)              
005400     03 TAKF-TIHHMM          PIC S9(5)           COMP-3.                  
005500*                                 KLOCKSLAG (TIMMAR OCH MINUTER)          
005600*                                 TIME IN HOUR AND MINUTE                 
005700     03 TAKF-BEGMT-RAD1      PIC X(35).                                   
005800*                                 GODSMOTTAGARNAMN RAD 1                  
005900*                                 GOODS RECEIVER NAME LINE 1              
006000     03 TAKF-BEGMT-RAD2      PIC X(35).                                   
006100*                                 GODSMOTTAGARNAMN RAD 2                  
006200*                                 GOODS RECEIVER NAME LINE 2              
006300     03 TAKF-ADGMT-GATA      PIC X(35).                                   
006400*                                 GODSMOTTAGARADRESS GATA                 
006500*                                 GOODS RECEIVER ADDRESS STREET           
006600     03 TAKF-ADGMT-PADR      PIC X(35).                                   
006700*                                 GODSMOTTAGARADRESS POSTADRESS           
006800*                                 GOODS RECEIVER ADDRESS TOWN             
006900*** END OF VILMAII-COPY LENGTH= 253 BYTES                                 
