000100 01  SPAR-W55502.                                                         
000200*                                 SALDOFÖRÄNDRINGAR LOGGADE               
000300*                                 FYSISK NYCKEL: WDL901KY                 
000400*                                 (IDARTNR + DAREGDAT + TIKLOCK +         
000500*                                  IDSEKVNR)                              
000600*                                 SÖKBEGREPP: IDARTNR, DAREGDAT,          
000700*                                 TIKLOCK, IDSEKVNR, IDDC,                
000800*                                 IDHUVTYP, IDSUBTYP, IDPGM,              
000900*                                 IDTRANS                                 
001000     03 SPAR-IDARTNR         PIC S9(9)           COMP-3.                  
001100*                                 ARTIKELNUMMER                           
001200*                                 PART NUMBER                             
001300     03 SPAR-DAREGDAT-9KOMPL PIC 9(8).                                    
001400*                                 REGISTRERINGSDATUM (ÅÅÅÅMMDD)           
001500*                                 REGISTRATION DATE (YYYYMMDD)            
001600     03 SPAR-TIKLOCK-9KOMPL  PIC S9(9)           COMP-3.                  
001700*                                 KLOCKSLAG (TTMMSSTH)                    
001800*                                 TIME OF DAY (HHMMSSTH)                  
001900     03 SPAR-IDSEKVNR        PIC S9(3)           COMP-3.                  
002000*                                 GENERELLT SEKVENSNUMMER                 
002100*                                 GENERAL SEQUENCE NUMBER                 
002200     03 SPAR-IDDC            PIC X(2).                                    
002300*                                 IDENTIFIERARE LAGER                     
002400*                                 WAREHOUSE IDENTIFIER                    
002500     03 SPAR-IDHUVTYP        PIC X(4).                                    
002600*                                 LOGGTYP EKONOMISK HÄNDELSE              
002700*                                 LOGG TYPE ECONOMIC EVENT                
002800     03 SPAR-IDSUBTYP        PIC X(3).                                    
002900*                                 LOGGTYP EKONOMISK HÄNDELSE              
003000*                                 LOGG TYPE ECONOMIC EVENT                
003100     03 SPAR-IDPGM           PIC X(8).                                    
003200*                                 PROGRAM IDENTITET                       
003300*                                 PROGRAM INTENTITY                       
003400     03 SPAR-IDTRANS         PIC X(4).                                    
003500*                                 BILDNUMMER                              
003600*                                 SCREEN NUMBER                           
003700     03 SPAR-IDUSER          PIC X(8).                                    
003800*                                 ANVÄNDARENS SÄKERHETS ID                
003900*                                 USER SECURITY-IDENTITY                  
004000     03 SPAR-REF.                                                         
004100*                                 SALDOREREFERENS                         
004200*                                 BALANCE REFERENCE                       
004300        05 SPAR-UREF1        PIC X(17).                                   
004400        05 SPAR-IDGMTREF REDEFINES SPAR-UREF1.                            
004500*                                 GODSMOTTAGAREREFERENS                   
004600*                                 GOODS RECEIVER REFERENS                 
004700           07 SPAR-IDDISTR   PIC S9(5)           COMP-3.                  
004800*                                 DISTRIKTNUMMER                          
004900*                                 DISTRICT NUMBER                         
005000           07 SPAR-IDKUNDNR  PIC S9(7)           COMP-3.                  
005100*                                 KUNDNUMMER                              
005200*                                 CUSTOMER NO                             
005300           07 SPAR-IDKUNDRF-GRP.                                          
005400*                                 KUNDENS REFERENS (ORDERID)              
005500*                                 CUSTOMER REFERENCE (ORDER ID)           
005600              09 SPAR-IDKUNDRF                                            
005700                             PIC X(10).                                   
005800*                                 KUNDENS REFERENS (ORDERID)              
005900*                                 CUSTOMER REFERENCE (ORDER ID)           
006000              09 SPAR-IDORDNR5-FILLER REDEFINES SPAR-IDKUNDRF.            
006100                 11 SPAR-IDORDNR5                                         
006200                             PIC 9(5).                                    
006300*                                 ORDERNUMMER                             
006400*                                 ORDER NUMBER                            
006500                 11 FILLER   PIC X(5).                                    
006600              09 SPAR-IDORDNR7-FILLER REDEFINES SPAR-IDKUNDRF.            
006700                 11 SPAR-IDORDNR7                                         
006800                             PIC 9(7).                                    
006900*                                 ORDERNUMMER                             
007000*                                 ORDER NUMBER                            
007100                 11 FILLER   PIC X(3).                                    
007200        05 SPAR-IDLEVREF-FILLER REDEFINES SPAR-UREF1.                     
007300           07 SPAR-IDLEVREF.                                              
007400*                                 INLEVERANSREREFERENS                    
007500*                                 INCOMING DELIVERY REFRENCE              
007600              09 SPAR-IDLOPNRM                                            
007700                             PIC S9(9)           COMP-3.                  
007800*                                 LÖPNUMMER MOTTAGNINGSRAPPORT            
007900*                                 (0VVDLLLLK)                             
008000*                                 SERIAL NO RECEIVING REPORT              
008100*                                 (0WWDLLLLC)                             
008200              09 SPAR-IDLEVNR                                             
008300                             PIC X(5).                                    
008400*                                 LEVERANTÖRNUMMER                        
008500*                                 SUPPLIER NUMBER (VENDOR NUMBER)         
008600           07 FILLER         PIC X(7).                                    
008700        05 SPAR-IDPRCREF-FILLER REDEFINES SPAR-UREF1.                     
008800           07 SPAR-IDPRCREF.                                              
008900*                                 PRODUKTIONSKANALREREFERENS              
009000*                                 PRODUCTION CHANNEL REFERENCE            
009100              09 SPAR-IDPRC.                                              
009200*                                 PRODUKTIONSKANAL                        
009300*                                 PRODUCTION CHANNEL                      
009400                 11 SPAR-IDPRCBAS                                         
009500                             PIC X(3).                                    
009600*                                 PRC-BAS                                 
009700*                                 PRC-BASIC                               
009800                 11 SPAR-IDPRCVAR                                         
009900                             PIC X.                                       
010000*                                 PRC-VARIANT                             
010100*                                 PRC-VARIANT                             
010200              09 SPAR-IDUSER-IDPRCREF                                     
010300                             PIC X(8).                                    
010400*                                 ANVÄNDARENS SÄKERHETS ID                
010500*                                 USER SECURITY-IDENTITY                  
010600           07 FILLER         PIC X(5).                                    
010700        05 SPAR-IDLBREF-FILLER REDEFINES SPAR-UREF1.                      
010800           07 SPAR-IDLBREF.                                               
010900*                                 LASTBÄRARREFERENS                       
011000*                                 TRAILER REFERENCE                       
011100              09 SPAR-TIFAKT PIC S9(7)           COMP-3.                  
011200*                                 FAKTURERINGSDATUM (ÅÅMMDD)              
011300*                                 INVOICING DATE   (YYMMDD)               
011400              09 SPAR-IDLBBET                                             
011500                             PIC X(12).                                   
011600*                                 LASTBÄRARBETECKNING                     
011700*                                 TRAILER NUMBER                          
011800           07 FILLER         PIC X.                                       
011900        05 SPAR-IDANSTREF-FILLER REDEFINES SPAR-UREF1.                    
012000           07 SPAR-IDANSTREF.                                             
012100*                                 ANSTÄLLNINGSREFERENS                    
012200*                                 EMPLOYEE REFERENCE                      
012300              09 SPAR-IDANSTNR                                            
012400                             PIC S9(5)           COMP-3.                  
012500*                                 ANSTÄLLNINGSNUMMER                      
012600*                                 IDENTIFICATION NO EMPLOYEE              
012700           07 FILLER         PIC X(14).                                   
012800        05 SPAR-IDDC-SEND-FILLER REDEFINES SPAR-UREF1.                    
012900           07 SPAR-IDDC-SEND PIC X(2).                                    
013000*                                 SÄNDANDE LAGER                          
013100*                                 SENDING WAREHOUSE                       
013200           07 FILLER         PIC X(15).                                   
013300        05 SPAR-UREF2        PIC X(18).                                   
013400        05 SPAR-IDFAKT-FILLER REDEFINES SPAR-UREF2.                       
013500           07 SPAR-IDFAKT    PIC S9(7)           COMP-3.                  
013600*                                 FAKTURANUMMER                           
013700*                                 INVOICE NO.                             
013800           07 FILLER         PIC X(14).                                   
013900        05 SPAR-IDKOLLI-FILLER REDEFINES SPAR-UREF2.                      
014000           07 SPAR-IDKOLLI   PIC S9(5)           COMP-3.                  
014100*                                 KOLLINUMMER                             
014200*                                 CASE NUMBER                             
014300           07 FILLER         PIC X(15).                                   
014400        05 SPAR-IDPRODREF-FILLER REDEFINES SPAR-UREF2.                    
014500           07 SPAR-IDPRODREF.                                             
014600*                                 PRODUKTIONSORDERREFERENS                
014700*                                 PRODUCTION ORDER REFRENCE               
014800              09 SPAR-IDPRODNR                                            
014900                             PIC S9(7)           COMP-3.                  
015000*                                 PRODUKTIONSNUMMER                       
015100*                                 PRODUCTION NUMBER                       
015200              09 SPAR-IDPLKLST                                            
015300                             PIC S9(3)           COMP-3.                  
015400*                                 PLOCKLISTNUMMER                         
015500*                                 PICKING LIST NUMBER                     
015600           07 FILLER         PIC X(12).                                   
015700        05 SPAR-IDFS-FILLER REDEFINES SPAR-UREF2.                         
015800           07 SPAR-IDFS      PIC X(8).                                    
015900*                                 FÖLJESEDELSNUMMER ENL ODETTE            
016000*                                 ADVICE NOTE NUMBER ODETTE               
016100           07 FILLER         PIC X(10).                                   
016200        05 SPAR-IDRAPPNR-FILLER REDEFINES SPAR-UREF2.                     
016300           07 SPAR-IDRAPPNR  PIC 9(7).                                    
016400*                                 RAPPORT NUMMER                          
016500*                                 DISCREPANCY REPORT NUMBER               
016600           07 FILLER         PIC X(11).                                   
016700        05 SPAR-IDAVINR-FILLER REDEFINES SPAR-UREF2.                      
016800           07 SPAR-IDAVINR   PIC S9(7)           COMP-3.                  
016900*                                 AVI-NUMMER                              
017000*                                 ADVICE NOTE NUMBER                      
017100           07 FILLER         PIC X(14).                                   
017200        05 SPAR-IDKR-FILLER REDEFINES SPAR-UREF2.                         
017300           07 SPAR-IDKR      PIC 9(5).                                    
017400*                                 KONTROLLRAPPORT NUMMER                  
017500*                                 INSPECTION REPORT NUMBER                
017600           07 FILLER         PIC X(13).                                   
017700        05 SPAR-IDCLEARREF-FILLER REDEFINES SPAR-UREF2.                   
017800           07 SPAR-IDCLEARREF                                             
017900                             PIC S9(7)           COMP-3.                  
018000*                                 FIKTIV CLEARING REFERENSNR              
018100*                                 FIKTIVE CLEARING REFERENCE NO.          
018200           07 FILLER         PIC X(14).                                   
018300     03 SPAR-KVART-SALDO     PIC S9(7)           COMP-3.                  
018400*                                 ANTAL SALDOFÖRÄNDRADE ARTIKLAR          
018500*                                 QUANTITY PARTS STOCK CHANGED            
018600     03 SPAR-IDTECKEN-KVAKS  PIC X.                                       
018700*                                 TECKEN                                  
018800*                                 SIGN                                    
018900     03 SPAR-KVAKS           PIC S9(7)           COMP-3.                  
019000*                                 ANKOMSTSALDO                            
019100*                                 ADVICED,NOT BINNED,QTY                  
019200     03 SPAR-IDTECKEN-KVAKS-PAV                                           
019300                             PIC X.                                       
019400*                                 TECKEN                                  
019500*                                 SIGN                                    
019600     03 SPAR-KVAKS-PAV       PIC S9(7)           COMP-3.                  
019700*                                 DEL AV AK PÅ VÄG                        
019800*                                 PART OF AK ON ITS WAY                   
019900     03 SPAR-IDTECKEN-KVEFRS PIC X.                                       
020000*                                 TECKEN                                  
020100*                                 SIGN                                    
020200     03 SPAR-KVEFRS          PIC S9(7)           COMP-3.                  
020300*                                 EJ FAKTURERAT ANTAL STYCK               
020400*                                 ORDERED NOT INVOICED QTY                
020500     03 SPAR-IDTECKEN-KVLS   PIC X.                                       
020600*                                 TECKEN                                  
020700*                                 SIGN                                    
020800     03 SPAR-KVLS            PIC S9(7)           COMP-3.                  
020900*                                 LAGERSALDO                              
021000*                                 STOCK BALANCE                           
021100     03 SPAR-DAREGDAT-LADD   PIC 9(8).                                    
021200*                                 REGISTRERINGSDATUM (ÅÅÅÅMMDD)           
021300*                                 REGISTRATION DATE (YYYYMMDD)            
021400*** END OF VILMAII-COPY LENGTH= 116 BYTES                                 
