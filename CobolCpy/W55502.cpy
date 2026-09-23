000100 01  RENS-W55502.                                                         
000200*                                 SALDOFÖRÄNDRINGAR LOGGADE               
000300*                                 FYSISK NYCKEL: WDL901KY                 
000400*                                 (IDARTNR + DAREGDAT + TIKLOCK +         
000500*                                  IDSEKVNR)                              
000600*                                 SÖKBEGREPP: IDARTNR, DAREGDAT,          
000700*                                 TIKLOCK, IDSEKVNR, IDDC,                
000800*                                 IDHUVTYP, IDSUBTYP, IDPGM,              
000900*                                 IDTRANS                                 
001000     03 RENS-IDARTNR         PIC S9(9)           COMP-3.                  
001100*                                 ARTIKELNUMMER                           
001200*                                 PART NUMBER                             
001300     03 RENS-DAREGDAT-9KOMPL PIC 9(8).                                    
001400*                                 REGISTRERINGSDATUM (ÅÅÅÅMMDD)           
001500*                                 REGISTRATION DATE (YYYYMMDD)            
001600     03 RENS-TIKLOCK-9KOMPL  PIC S9(9)           COMP-3.                  
001700*                                 KLOCKSLAG (TTMMSSTH)                    
001800*                                 TIME OF DAY (HHMMSSTH)                  
001900     03 RENS-IDSEKVNR        PIC S9(3)           COMP-3.                  
002000*                                 GENERELLT SEKVENSNUMMER                 
002100*                                 GENERAL SEQUENCE NUMBER                 
002200     03 RENS-IDDC            PIC X(2).                                    
002300*                                 IDENTIFIERARE LAGER                     
002400*                                 WAREHOUSE IDENTIFIER                    
002500     03 RENS-IDHUVTYP        PIC X(4).                                    
002600*                                 LOGGTYP EKONOMISK HÄNDELSE              
002700*                                 LOGG TYPE ECONOMIC EVENT                
002800     03 RENS-IDSUBTYP        PIC X(3).                                    
002900*                                 LOGGTYP EKONOMISK HÄNDELSE              
003000*                                 LOGG TYPE ECONOMIC EVENT                
003100     03 RENS-IDPGM           PIC X(8).                                    
003200*                                 PROGRAM IDENTITET                       
003300*                                 PROGRAM INTENTITY                       
003400     03 RENS-IDTRANS         PIC X(4).                                    
003500*                                 BILDNUMMER                              
003600*                                 SCREEN NUMBER                           
003700     03 RENS-IDUSER          PIC X(8).                                    
003800*                                 ANVÄNDARENS SÄKERHETS ID                
003900*                                 USER SECURITY-IDENTITY                  
004000     03 RENS-REF.                                                         
004100*                                 SALDOREREFERENS                         
004200*                                 BALANCE REFERENCE                       
004300        05 RENS-UREF1        PIC X(17).                                   
004400        05 RENS-IDGMTREF REDEFINES RENS-UREF1.                            
004500*                                 GODSMOTTAGAREREFERENS                   
004600*                                 GOODS RECEIVER REFERENS                 
004700           07 RENS-IDDISTR   PIC S9(5)           COMP-3.                  
004800*                                 DISTRIKTNUMMER                          
004900*                                 DISTRICT NUMBER                         
005000           07 RENS-IDKUNDNR  PIC S9(7)           COMP-3.                  
005100*                                 KUNDNUMMER                              
005200*                                 CUSTOMER NO                             
005300           07 RENS-IDKUNDRF-GRP.                                          
005400*                                 KUNDENS REFERENS (ORDERID)              
005500*                                 CUSTOMER REFERENCE (ORDER ID)           
005600              09 RENS-IDKUNDRF                                            
005700                             PIC X(10).                                   
005800*                                 KUNDENS REFERENS (ORDERID)              
005900*                                 CUSTOMER REFERENCE (ORDER ID)           
006000              09 RENS-IDORDNR5-FILLER REDEFINES RENS-IDKUNDRF.            
006100                 11 RENS-IDORDNR5                                         
006200                             PIC 9(5).                                    
006300*                                 ORDERNUMMER                             
006400*                                 ORDER NUMBER                            
006500                 11 FILLER   PIC X(5).                                    
006600              09 RENS-IDORDNR7-FILLER REDEFINES RENS-IDKUNDRF.            
006700                 11 RENS-IDORDNR7                                         
006800                             PIC 9(7).                                    
006900*                                 ORDERNUMMER                             
007000*                                 ORDER NUMBER                            
007100                 11 FILLER   PIC X(3).                                    
007200        05 RENS-IDLEVREF-FILLER REDEFINES RENS-UREF1.                     
007300           07 RENS-IDLEVREF.                                              
007400*                                 INLEVERANSREREFERENS                    
007500*                                 INCOMING DELIVERY REFRENCE              
007600              09 RENS-IDLOPNRM                                            
007700                             PIC S9(9)           COMP-3.                  
007800*                                 LÖPNUMMER MOTTAGNINGSRAPPORT            
007900*                                 (0VVDLLLLK)                             
008000*                                 SERIAL NO RECEIVING REPORT              
008100*                                 (0WWDLLLLC)                             
008200              09 RENS-IDLEVNR                                             
008300                             PIC X(5).                                    
008400*                                 LEVERANTÖRNUMMER                        
008500*                                 SUPPLIER NUMBER (VENDOR NUMBER)         
008600           07 FILLER         PIC X(7).                                    
008700        05 RENS-IDPRCREF-FILLER REDEFINES RENS-UREF1.                     
008800           07 RENS-IDPRCREF.                                              
008900*                                 PRODUKTIONSKANALREREFERENS              
009000*                                 PRODUCTION CHANNEL REFERENCE            
009100              09 RENS-IDPRC.                                              
009200*                                 PRODUKTIONSKANAL                        
009300*                                 PRODUCTION CHANNEL                      
009400                 11 RENS-IDPRCBAS                                         
009500                             PIC X(3).                                    
009600*                                 PRC-BAS                                 
009700*                                 PRC-BASIC                               
009800                 11 RENS-IDPRCVAR                                         
009900                             PIC X.                                       
010000*                                 PRC-VARIANT                             
010100*                                 PRC-VARIANT                             
010200              09 RENS-IDUSER-IDPRCREF                                     
010300                             PIC X(8).                                    
010400*                                 ANVÄNDARENS SÄKERHETS ID                
010500*                                 USER SECURITY-IDENTITY                  
010600           07 FILLER         PIC X(5).                                    
010700        05 RENS-IDLBREF-FILLER REDEFINES RENS-UREF1.                      
010800           07 RENS-IDLBREF.                                               
010900*                                 LASTBÄRARREFERENS                       
011000*                                 TRAILER REFERENCE                       
011100              09 RENS-TIFAKT PIC S9(7)           COMP-3.                  
011200*                                 FAKTURERINGSDATUM (ÅÅMMDD)              
011300*                                 INVOICING DATE   (YYMMDD)               
011400              09 RENS-IDLBBET                                             
011500                             PIC X(12).                                   
011600*                                 LASTBÄRARBETECKNING                     
011700*                                 TRAILER NUMBER                          
011800           07 FILLER         PIC X.                                       
011900        05 RENS-IDANSTREF-FILLER REDEFINES RENS-UREF1.                    
012000           07 RENS-IDANSTREF.                                             
012100*                                 ANSTÄLLNINGSREFERENS                    
012200*                                 EMPLOYEE REFERENCE                      
012300              09 RENS-IDANSTNR                                            
012400                             PIC S9(5)           COMP-3.                  
012500*                                 ANSTÄLLNINGSNUMMER                      
012600*                                 IDENTIFICATION NO EMPLOYEE              
012700           07 FILLER         PIC X(14).                                   
012800        05 RENS-IDDC-SEND-FILLER REDEFINES RENS-UREF1.                    
012900           07 RENS-IDDC-SEND PIC X(2).                                    
013000*                                 SÄNDANDE LAGER                          
013100*                                 SENDING WAREHOUSE                       
013200           07 FILLER         PIC X(15).                                   
013300        05 RENS-UREF2        PIC X(18).                                   
013400        05 RENS-IDFAKT-FILLER REDEFINES RENS-UREF2.                       
013500           07 RENS-IDFAKT    PIC S9(7)           COMP-3.                  
013600*                                 FAKTURANUMMER                           
013700*                                 INVOICE NO.                             
013800           07 FILLER         PIC X(14).                                   
013900        05 RENS-IDKOLLI-FILLER REDEFINES RENS-UREF2.                      
014000           07 RENS-IDKOLLI   PIC S9(5)           COMP-3.                  
014100*                                 KOLLINUMMER                             
014200*                                 CASE NUMBER                             
014300           07 FILLER         PIC X(15).                                   
014400        05 RENS-IDPRODREF-FILLER REDEFINES RENS-UREF2.                    
014500           07 RENS-IDPRODREF.                                             
014600*                                 PRODUKTIONSORDERREFERENS                
014700*                                 PRODUCTION ORDER REFRENCE               
014800              09 RENS-IDPRODNR                                            
014900                             PIC S9(7)           COMP-3.                  
015000*                                 PRODUKTIONSNUMMER                       
015100*                                 PRODUCTION NUMBER                       
015200              09 RENS-IDPLKLST                                            
015300                             PIC S9(3)           COMP-3.                  
015400*                                 PLOCKLISTNUMMER                         
015500*                                 PICKING LIST NUMBER                     
015600           07 FILLER         PIC X(12).                                   
015700        05 RENS-IDFS-FILLER REDEFINES RENS-UREF2.                         
015800           07 RENS-IDFS      PIC X(8).                                    
015900*                                 FÖLJESEDELSNUMMER ENL ODETTE            
016000*                                 ADVICE NOTE NUMBER ODETTE               
016100           07 FILLER         PIC X(10).                                   
016200        05 RENS-IDRAPPNR-FILLER REDEFINES RENS-UREF2.                     
016300           07 RENS-IDRAPPNR  PIC 9(7).                                    
016400*                                 RAPPORT NUMMER                          
016500*                                 DISCREPANCY REPORT NUMBER               
016600           07 FILLER         PIC X(11).                                   
016700        05 RENS-IDAVINR-FILLER REDEFINES RENS-UREF2.                      
016800           07 RENS-IDAVINR   PIC S9(7)           COMP-3.                  
016900*                                 AVI-NUMMER                              
017000*                                 ADVICE NOTE NUMBER                      
017100           07 FILLER         PIC X(14).                                   
017200        05 RENS-IDKR-FILLER REDEFINES RENS-UREF2.                         
017300           07 RENS-IDKR      PIC 9(5).                                    
017400*                                 KONTROLLRAPPORT NUMMER                  
017500*                                 INSPECTION REPORT NUMBER                
017600           07 FILLER         PIC X(13).                                   
017700        05 RENS-IDCLEARREF-FILLER REDEFINES RENS-UREF2.                   
017800           07 RENS-IDCLEARREF                                             
017900                             PIC S9(7)           COMP-3.                  
018000*                                 FIKTIV CLEARING REFERENSNR              
018100*                                 FIKTIVE CLEARING REFERENCE NO.          
018200           07 FILLER         PIC X(14).                                   
018300     03 RENS-KVART-SALDO     PIC S9(7)           COMP-3.                  
018400*                                 ANTAL SALDOFÖRÄNDRADE ARTIKLAR          
018500*                                 QUANTITY PARTS STOCK CHANGED            
018600     03 RENS-IDTECKEN-KVAKS  PIC X.                                       
018700*                                 TECKEN                                  
018800*                                 SIGN                                    
018900     03 RENS-KVAKS           PIC S9(7)           COMP-3.                  
019000*                                 ANKOMSTSALDO                            
019100*                                 ADVICED,NOT BINNED,QTY                  
019200     03 RENS-IDTECKEN-KVAKS-PAV                                           
019300                             PIC X.                                       
019400*                                 TECKEN                                  
019500*                                 SIGN                                    
019600     03 RENS-KVAKS-PAV       PIC S9(7)           COMP-3.                  
019700*                                 DEL AV AK PÅ VÄG                        
019800*                                 PART OF AK ON ITS WAY                   
019900     03 RENS-IDTECKEN-KVEFRS PIC X.                                       
020000*                                 TECKEN                                  
020100*                                 SIGN                                    
020200     03 RENS-KVEFRS          PIC S9(7)           COMP-3.                  
020300*                                 EJ FAKTURERAT ANTAL STYCK               
020400*                                 ORDERED NOT INVOICED QTY                
020500     03 RENS-IDTECKEN-KVLS   PIC X.                                       
020600*                                 TECKEN                                  
020700*                                 SIGN                                    
020800     03 RENS-KVLS            PIC S9(7)           COMP-3.                  
020900*                                 LAGERSALDO                              
021000*                                 STOCK BALANCE                           
021100     03 RENS-DAREGDAT-LADD   PIC 9(8).                                    
021200*                                 REGISTRERINGSDATUM (ÅÅÅÅMMDD)           
021300*                                 REGISTRATION DATE (YYYYMMDD)            
021400*** END OF VILMAII-COPY LENGTH= 116 BYTES                                 
