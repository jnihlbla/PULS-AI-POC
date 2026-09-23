000100 01  LOGT-WDL301.                                                         
000200*                                 SALDOFÖRÄNDRINGAR LOGGADE               
000300*                                 TRACKING-ID                             
000400*                                 FYSISK NYCKEL: WDL301KY                 
000500*                                 (IDARTNR + DAREGDAT + TIKLOCK +         
000600*                                  IDSEKVNR)                              
000700     03 LOGT-IDARTNR         PIC S9(9)           COMP-3.                  
000800*                                 ARTIKELNUMMER                           
000900*                                 PART NUMBER                             
001000     03 LOGT-DAREGDAT-9KOMPL PIC 9(8).                                    
001100*                                 REGISTRERINGSDATUM (ÅÅÅÅMMDD)           
001200*                                 REGISTRATION DATE (YYYYMMDD)            
001300     03 LOGT-TIKLOCK-9KOMPL  PIC S9(9)           COMP-3.                  
001400*                                 KLOCKSLAG (TTMMSSTH)                    
001500*                                 TIME OF DAY (HHMMSSTH)                  
001600     03 LOGT-IDSEKVNR        PIC S9(3)           COMP-3.                  
001700*                                 GENERELLT SEKVENSNUMMER                 
001800*                                 GENERAL SEQUENCE NUMBER                 
001900     03 LOGT-IDTRACK         PIC X(25).                                   
002000*                                 TRACKING ID FROM CUSTOMS                
002100*                                 CUSTOMS TRACKING ID                     
002200     03 LOGT-IDDC            PIC X(2).                                    
002300*                                 IDENTIFIERARE LAGER                     
002400*                                 WAREHOUSE IDENTIFIER                    
002500     03 LOGT-IDHUVTYP        PIC X(4).                                    
002600*                                 LOGGTYP EKONOMISK HÄNDELSE              
002700*                                 LOGG TYPE ECONOMIC EVENT                
002800     03 LOGT-IDSUBTYP        PIC X(3).                                    
002900*                                 LOGGTYP EKONOMISK HÄNDELSE              
003000*                                 LOGG TYPE ECONOMIC EVENT                
003100     03 LOGT-IDPGM           PIC X(8).                                    
003200*                                 PROGRAM IDENTITET                       
003300*                                 PROGRAM INTENTITY                       
003400     03 LOGT-IDTRANS         PIC X(4).                                    
003500*                                 BILDNUMMER                              
003600*                                 SCREEN NUMBER                           
003700     03 LOGT-IDUSER          PIC X(8).                                    
003800*                                 ANVÄNDARENS SÄKERHETS ID                
003900*                                 USER SECURITY-IDENTITY                  
004000     03 LOGT-REF.                                                         
004100*                                 SALDOREREFERENS                         
004200*                                 BALANCE REFERENCE                       
004300        05 LOGT-UREF1        PIC X(17).                                   
004400        05 LOGT-IDGMTREF REDEFINES LOGT-UREF1.                            
004500*                                 GODSMOTTAGAREREFERENS                   
004600*                                 GOODS RECEIVER REFERENS                 
004700           07 LOGT-IDDISTR   PIC S9(5)           COMP-3.                  
004800*                                 DISTRIKTNUMMER                          
004900*                                 DISTRICT NUMBER                         
005000           07 LOGT-IDKUNDNR  PIC S9(7)           COMP-3.                  
005100*                                 KUNDNUMMER                              
005200*                                 CUSTOMER NO                             
005300           07 LOGT-IDKUNDRF-GRP.                                          
005400*                                 KUNDENS REFERENS (ORDERID)              
005500*                                 CUSTOMER REFERENCE (ORDER ID)           
005600              09 LOGT-IDKUNDRF                                            
005700                             PIC X(10).                                   
005800*                                 KUNDENS REFERENS (ORDERID)              
005900*                                 CUSTOMER REFERENCE (ORDER ID)           
006000              09 LOGT-IDORDNR5-FILLER REDEFINES LOGT-IDKUNDRF.            
006100                 11 LOGT-IDORDNR5                                         
006200                             PIC 9(5).                                    
006300*                                 ORDERNUMMER                             
006400*                                 ORDER NUMBER                            
006500                 11 FILLER   PIC X(5).                                    
006600              09 LOGT-IDORDNR7-FILLER REDEFINES LOGT-IDKUNDRF.            
006700                 11 LOGT-IDORDNR7                                         
006800                             PIC 9(7).                                    
006900*                                 ORDERNUMMER                             
007000*                                 ORDER NUMBER                            
007100                 11 FILLER   PIC X(3).                                    
007200        05 LOGT-IDLEVREF-FILLER REDEFINES LOGT-UREF1.                     
007300           07 LOGT-IDLEVREF.                                              
007400*                                 INLEVERANSREREFERENS                    
007500*                                 INCOMING DELIVERY REFRENCE              
007600              09 LOGT-IDLOPNRM                                            
007700                             PIC S9(9)           COMP-3.                  
007800*                                 LÖPNUMMER MOTTAGNINGSRAPPORT            
007900*                                 (0VVDLLLLK)                             
008000*                                 SERIAL NO RECEIVING REPORT              
008100*                                 (0WWDLLLLC)                             
008200              09 LOGT-IDLEVNR                                             
008300                             PIC X(5).                                    
008400*                                 LEVERANTÖRNUMMER                        
008500*                                 SUPPLIER NUMBER (VENDOR NUMBER)         
008600           07 FILLER         PIC X(7).                                    
008700        05 LOGT-IDPRCREF-FILLER REDEFINES LOGT-UREF1.                     
008800           07 LOGT-IDPRCREF.                                              
008900*                                 PRODUKTIONSKANALREREFERENS              
009000*                                 PRODUCTION CHANNEL REFERENCE            
009100              09 LOGT-IDPRC.                                              
009200*                                 PRODUKTIONSKANAL                        
009300*                                 PRODUCTION CHANNEL                      
009400                 11 LOGT-IDPRCBAS                                         
009500                             PIC X(3).                                    
009600*                                 PRC-BAS                                 
009700*                                 PRC-BASIC                               
009800                 11 LOGT-IDPRCVAR                                         
009900                             PIC X.                                       
010000*                                 PRC-VARIANT                             
010100*                                 PRC-VARIANT                             
010200              09 LOGT-IDUSER-IDPRCREF                                     
010300                             PIC X(8).                                    
010400*                                 ANVÄNDARENS SÄKERHETS ID                
010500*                                 USER SECURITY-IDENTITY                  
010600           07 FILLER         PIC X(5).                                    
010700        05 LOGT-IDLBREF-FILLER REDEFINES LOGT-UREF1.                      
010800           07 LOGT-IDLBREF.                                               
010900*                                 LASTBÄRARREFERENS                       
011000*                                 TRAILER REFERENCE                       
011100              09 LOGT-TIFAKT PIC S9(7)           COMP-3.                  
011200*                                 FAKTURERINGSDATUM (ÅÅMMDD)              
011300*                                 INVOICING DATE   (YYMMDD)               
011400              09 LOGT-IDLBBET                                             
011500                             PIC X(12).                                   
011600*                                 LASTBÄRARBETECKNING                     
011700*                                 TRAILER NUMBER                          
011800           07 FILLER         PIC X.                                       
011900        05 LOGT-IDANSTREF-FILLER REDEFINES LOGT-UREF1.                    
012000           07 LOGT-IDANSTREF.                                             
012100*                                 ANSTÄLLNINGSREFERENS                    
012200*                                 EMPLOYEE REFERENCE                      
012300              09 LOGT-IDANSTNR                                            
012400                             PIC S9(5)           COMP-3.                  
012500*                                 ANSTÄLLNINGSNUMMER                      
012600*                                 IDENTIFICATION NO EMPLOYEE              
012700           07 FILLER         PIC X(14).                                   
012800        05 LOGT-IDDC-SEND-FILLER REDEFINES LOGT-UREF1.                    
012900           07 LOGT-IDDC-SEND PIC X(2).                                    
013000*                                 SÄNDANDE LAGER                          
013100*                                 SENDING WAREHOUSE                       
013200           07 FILLER         PIC X(15).                                   
013300        05 LOGT-UREF2        PIC X(18).                                   
013400        05 LOGT-IDFAKT-FILLER REDEFINES LOGT-UREF2.                       
013500           07 LOGT-IDFAKT    PIC S9(7)           COMP-3.                  
013600*                                 FAKTURANUMMER                           
013700*                                 INVOICE NO.                             
013800           07 FILLER         PIC X(14).                                   
013900        05 LOGT-IDKOLLI-FILLER REDEFINES LOGT-UREF2.                      
014000           07 LOGT-IDKOLLI   PIC S9(5)           COMP-3.                  
014100*                                 KOLLINUMMER                             
014200*                                 CASE NUMBER                             
014300           07 FILLER         PIC X(15).                                   
014400        05 LOGT-IDPRODREF-FILLER REDEFINES LOGT-UREF2.                    
014500           07 LOGT-IDPRODREF.                                             
014600*                                 PRODUKTIONSORDERREFERENS                
014700*                                 PRODUCTION ORDER REFRENCE               
014800              09 LOGT-IDPRODNR                                            
014900                             PIC S9(7)           COMP-3.                  
015000*                                 PRODUKTIONSNUMMER                       
015100*                                 PRODUCTION NUMBER                       
015200              09 LOGT-IDPLKLST                                            
015300                             PIC S9(3)           COMP-3.                  
015400*                                 PLOCKLISTNUMMER                         
015500*                                 PICKING LIST NUMBER                     
015600           07 FILLER         PIC X(12).                                   
015700        05 LOGT-IDFS-FILLER REDEFINES LOGT-UREF2.                         
015800           07 LOGT-IDFS      PIC X(8).                                    
015900*                                 FÖLJESEDELSNUMMER ENL ODETTE            
016000*                                 ADVICE NOTE NUMBER ODETTE               
016100           07 FILLER         PIC X(10).                                   
016200        05 LOGT-IDRAPPNR-FILLER REDEFINES LOGT-UREF2.                     
016300           07 LOGT-IDRAPPNR  PIC 9(7).                                    
016400*                                 RAPPORT NUMMER                          
016500*                                 DISCREPANCY REPORT NUMBER               
016600           07 FILLER         PIC X(11).                                   
016700        05 LOGT-IDAVINR-FILLER REDEFINES LOGT-UREF2.                      
016800           07 LOGT-IDAVINR   PIC S9(7)           COMP-3.                  
016900*                                 AVI-NUMMER                              
017000*                                 ADVICE NOTE NUMBER                      
017100           07 FILLER         PIC X(14).                                   
017200        05 LOGT-IDKR-FILLER REDEFINES LOGT-UREF2.                         
017300           07 LOGT-IDKR      PIC 9(5).                                    
017400*                                 KONTROLLRAPPORT NUMMER                  
017500*                                 INSPECTION REPORT NUMBER                
017600           07 FILLER         PIC X(13).                                   
017700        05 LOGT-IDCLEARREF-FILLER REDEFINES LOGT-UREF2.                   
017800           07 LOGT-IDCLEARREF                                             
017900                             PIC S9(7)           COMP-3.                  
018000*                                 FIKTIV CLEARING REFERENSNR              
018100*                                 FIKTIVE CLEARING REFERENCE NO.          
018200           07 FILLER         PIC X(14).                                   
018300     03 LOGT-KVART-SALDO     PIC S9(7)           COMP-3.                  
018400*                                 ANTAL SALDOFÖRÄNDRADE ARTIKLAR          
018500*                                 QUANTITY PARTS STOCK CHANGED            
018600     03 LOGT-IDTECKEN-KVLS   PIC X.                                       
018700*                                 TECKEN                                  
018800*                                 SIGN                                    
018900     03 LOGT-KVLS            PIC S9(7)           COMP-3.                  
019000*                                 LAGERSALDO                              
019100*                                 STOCK BALANCE                           
019200     03 LOGT-IDTECKEN-KVTRACK-KVAR                                        
019300                             PIC X.                                       
019400*                                 TECKEN                                  
019500*                                 SIGN                                    
019600     03 LOGT-KVTRACK-KVAR    PIC S9(7)           COMP-3.                  
019700*                                 ANTAL KVAR PER TRACKING-ID              
019800*                                 REMAINING QTY OF TRACKING-ID            
019900     03 LOGT-DAREGDAT-LADD   PIC 9(8).                                    
020000*                                 REGISTRERINGSDATUM (ÅÅÅÅMMDD)           
020100*                                 REGISTRATION DATE (YYYYMMDD)            
020200*** END OF VILMAII-COPY LENGTH= 131 BYTES                                 
