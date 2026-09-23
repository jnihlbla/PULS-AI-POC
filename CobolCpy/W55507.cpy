000100 01  URV-W55507.                                                          
000200*                                 SALDOFÖRÄNDRINGAR LOGGADE               
000300*                                 FYSISK NYCKEL: WDL901KY                 
000400*                                 (IDARTNR + DAREGDAT + TIKLOCK +         
000500*                                  IDSEKVNR)                              
000600*                                 SÖKBEGREPP: IDARTNR, DAREGDAT,          
000700*                                 TIKLOCK, IDSEKVNR, IDDC,                
000800*                                 IDHUVTYP, IDSUBTYP, IDPGM,              
000900*                                 IDTRANS                                 
001000     03 URV-IDARTNR          PIC S9(9)           COMP-3.                  
001100*                                 ARTIKELNUMMER                           
001200*                                 PART NUMBER                             
001300     03 URV-DAREGDAT-9KOMPL  PIC 9(8).                                    
001400*                                 REGISTRERINGSDATUM (ÅÅÅÅMMDD)           
001500*                                 REGISTRATION DATE (YYYYMMDD)            
001600     03 URV-TIKLOCK-9KOMPL   PIC S9(9)           COMP-3.                  
001700*                                 KLOCKSLAG (TTMMSSTH)                    
001800*                                 TIME OF DAY (HHMMSSTH)                  
001900     03 URV-IDSEKVNR         PIC S9(3)           COMP-3.                  
002000*                                 GENERELLT SEKVENSNUMMER                 
002100*                                 GENERAL SEQUENCE NUMBER                 
002200     03 URV-IDDC             PIC X(2).                                    
002300*                                 IDENTIFIERARE LAGER                     
002400*                                 WAREHOUSE IDENTIFIER                    
002500     03 URV-IDHUVTYP         PIC X(4).                                    
002600*                                 LOGGTYP EKONOMISK HÄNDELSE              
002700*                                 LOGG TYPE ECONOMIC EVENT                
002800     03 URV-IDSUBTYP         PIC X(3).                                    
002900*                                 LOGGTYP EKONOMISK HÄNDELSE              
003000*                                 LOGG TYPE ECONOMIC EVENT                
003100     03 URV-IDPGM            PIC X(8).                                    
003200*                                 PROGRAM IDENTITET                       
003300*                                 PROGRAM INTENTITY                       
003400     03 URV-IDTRANS          PIC X(4).                                    
003500*                                 BILDNUMMER                              
003600*                                 SCREEN NUMBER                           
003700     03 URV-IDUSER           PIC X(8).                                    
003800*                                 ANVÄNDARENS SÄKERHETS ID                
003900*                                 USER SECURITY-IDENTITY                  
004000     03 URV-REF.                                                          
004100*                                 SALDOREREFERENS                         
004200*                                 BALANCE REFERENCE                       
004300        05 URV-UREF1         PIC X(17).                                   
004400        05 URV-IDGMTREF REDEFINES URV-UREF1.                              
004500*                                 GODSMOTTAGAREREFERENS                   
004600*                                 GOODS RECEIVER REFERENS                 
004700           07 URV-IDDISTR    PIC S9(5)           COMP-3.                  
004800*                                 DISTRIKTNUMMER                          
004900*                                 DISTRICT NUMBER                         
005000           07 URV-IDKUNDNR   PIC S9(7)           COMP-3.                  
005100*                                 KUNDNUMMER                              
005200*                                 CUSTOMER NO                             
005300           07 URV-IDKUNDRF-GRP.                                           
005400*                                 KUNDENS REFERENS (ORDERID)              
005500*                                 CUSTOMER REFERENCE (ORDER ID)           
005600              09 URV-IDKUNDRF                                             
005700                             PIC X(10).                                   
005800*                                 KUNDENS REFERENS (ORDERID)              
005900*                                 CUSTOMER REFERENCE (ORDER ID)           
006000              09 URV-IDORDNR5-FILLER REDEFINES URV-IDKUNDRF.              
006100                 11 URV-IDORDNR5                                          
006200                             PIC 9(5).                                    
006300*                                 ORDERNUMMER                             
006400*                                 ORDER NUMBER                            
006500                 11 FILLER   PIC X(5).                                    
006600              09 URV-IDORDNR7-FILLER REDEFINES URV-IDKUNDRF.              
006700                 11 URV-IDORDNR7                                          
006800                             PIC 9(7).                                    
006900*                                 ORDERNUMMER                             
007000*                                 ORDER NUMBER                            
007100                 11 FILLER   PIC X(3).                                    
007200        05 URV-IDLEVREF-FILLER REDEFINES URV-UREF1.                       
007300           07 URV-IDLEVREF.                                               
007400*                                 INLEVERANSREREFERENS                    
007500*                                 INCOMING DELIVERY REFRENCE              
007600              09 URV-IDLOPNRM                                             
007700                             PIC S9(9)           COMP-3.                  
007800*                                 LÖPNUMMER MOTTAGNINGSRAPPORT            
007900*                                 (0VVDLLLLK)                             
008000*                                 SERIAL NO RECEIVING REPORT              
008100*                                 (0WWDLLLLC)                             
008200              09 URV-IDLEVNR PIC X(5).                                    
008300*                                 LEVERANTÖRNUMMER                        
008400*                                 SUPPLIER NUMBER (VENDOR NUMBER)         
008500           07 FILLER         PIC X(7).                                    
008600        05 URV-IDPRCREF-FILLER REDEFINES URV-UREF1.                       
008700           07 URV-IDPRCREF.                                               
008800*                                 PRODUKTIONSKANALREREFERENS              
008900*                                 PRODUCTION CHANNEL REFERENCE            
009000              09 URV-IDPRC.                                               
009100*                                 PRODUKTIONSKANAL                        
009200*                                 PRODUCTION CHANNEL                      
009300                 11 URV-IDPRCBAS                                          
009400                             PIC X(3).                                    
009500*                                 PRC-BAS                                 
009600*                                 PRC-BASIC                               
009700                 11 URV-IDPRCVAR                                          
009800                             PIC X.                                       
009900*                                 PRC-VARIANT                             
010000*                                 PRC-VARIANT                             
010100              09 URV-IDUSER-IDPRCREF                                      
010200                             PIC X(8).                                    
010300*                                 ANVÄNDARENS SÄKERHETS ID                
010400*                                 USER SECURITY-IDENTITY                  
010500           07 FILLER         PIC X(5).                                    
010600        05 URV-IDLBREF-FILLER REDEFINES URV-UREF1.                        
010700           07 URV-IDLBREF.                                                
010800*                                 LASTBÄRARREFERENS                       
010900*                                 TRAILER REFERENCE                       
011000              09 URV-TIFAKT  PIC S9(7)           COMP-3.                  
011100*                                 FAKTURERINGSDATUM (ÅÅMMDD)              
011200*                                 INVOICING DATE   (YYMMDD)               
011300              09 URV-IDLBBET PIC X(12).                                   
011400*                                 LASTBÄRARBETECKNING                     
011500*                                 TRAILER NUMBER                          
011600           07 FILLER         PIC X.                                       
011700        05 URV-IDANSTREF-FILLER REDEFINES URV-UREF1.                      
011800           07 URV-IDANSTREF.                                              
011900*                                 ANSTÄLLNINGSREFERENS                    
012000*                                 EMPLOYEE REFERENCE                      
012100              09 URV-IDANSTNR                                             
012200                             PIC S9(5)           COMP-3.                  
012300*                                 ANSTÄLLNINGSNUMMER                      
012400*                                 IDENTIFICATION NO EMPLOYEE              
012500           07 FILLER         PIC X(14).                                   
012600        05 URV-IDDC-SEND-FILLER REDEFINES URV-UREF1.                      
012700           07 URV-IDDC-SEND  PIC X(2).                                    
012800*                                 SÄNDANDE LAGER                          
012900*                                 SENDING WAREHOUSE                       
013000           07 FILLER         PIC X(15).                                   
013100        05 URV-UREF2         PIC X(18).                                   
013200        05 URV-IDFAKT-FILLER REDEFINES URV-UREF2.                         
013300           07 URV-IDFAKT     PIC S9(7)           COMP-3.                  
013400*                                 FAKTURANUMMER                           
013500*                                 INVOICE NO.                             
013600           07 FILLER         PIC X(14).                                   
013700        05 URV-IDKOLLI-FILLER REDEFINES URV-UREF2.                        
013800           07 URV-IDKOLLI    PIC S9(5)           COMP-3.                  
013900*                                 KOLLINUMMER                             
014000*                                 CASE NUMBER                             
014100           07 FILLER         PIC X(15).                                   
014200        05 URV-IDPRODREF-FILLER REDEFINES URV-UREF2.                      
014300           07 URV-IDPRODREF.                                              
014400*                                 PRODUKTIONSORDERREFERENS                
014500*                                 PRODUCTION ORDER REFRENCE               
014600              09 URV-IDPRODNR                                             
014700                             PIC S9(7)           COMP-3.                  
014800*                                 PRODUKTIONSNUMMER                       
014900*                                 PRODUCTION NUMBER                       
015000              09 URV-IDPLKLST                                             
015100                             PIC S9(3)           COMP-3.                  
015200*                                 PLOCKLISTNUMMER                         
015300*                                 PICKING LIST NUMBER                     
015400           07 FILLER         PIC X(12).                                   
015500        05 URV-IDFS-FILLER REDEFINES URV-UREF2.                           
015600           07 URV-IDFS       PIC X(8).                                    
015700*                                 FÖLJESEDELSNUMMER ENL ODETTE            
015800*                                 ADVICE NOTE NUMBER ODETTE               
015900           07 FILLER         PIC X(10).                                   
016000        05 URV-IDRAPPNR-FILLER REDEFINES URV-UREF2.                       
016100           07 URV-IDRAPPNR   PIC 9(7).                                    
016200*                                 RAPPORT NUMMER                          
016300*                                 DISCREPANCY REPORT NUMBER               
016400           07 FILLER         PIC X(11).                                   
016500        05 URV-IDAVINR-FILLER REDEFINES URV-UREF2.                        
016600           07 URV-IDAVINR    PIC S9(7)           COMP-3.                  
016700*                                 AVI-NUMMER                              
016800*                                 ADVICE NOTE NUMBER                      
016900           07 FILLER         PIC X(14).                                   
017000        05 URV-IDKR-FILLER REDEFINES URV-UREF2.                           
017100           07 URV-IDKR       PIC 9(5).                                    
017200*                                 KONTROLLRAPPORT NUMMER                  
017300*                                 INSPECTION REPORT NUMBER                
017400           07 FILLER         PIC X(13).                                   
017500        05 URV-IDCLEARREF-FILLER REDEFINES URV-UREF2.                     
017600           07 URV-IDCLEARREF PIC S9(7)           COMP-3.                  
017700*                                 FIKTIV CLEARING REFERENSNR              
017800*                                 FIKTIVE CLEARING REFERENCE NO.          
017900           07 FILLER         PIC X(14).                                   
018000     03 URV-KVART-SALDO      PIC S9(7)           COMP-3.                  
018100*                                 ANTAL SALDOFÖRÄNDRADE ARTIKLAR          
018200*                                 QUANTITY PARTS STOCK CHANGED            
018300     03 URV-IDTECKEN-KVAKS   PIC X.                                       
018400*                                 TECKEN                                  
018500*                                 SIGN                                    
018600     03 URV-KVAKS            PIC S9(7)           COMP-3.                  
018700*                                 ANKOMSTSALDO                            
018800*                                 ADVICED,NOT BINNED,QTY                  
018900     03 URV-IDTECKEN-KVAKS-PAV                                            
019000                             PIC X.                                       
019100*                                 TECKEN                                  
019200*                                 SIGN                                    
019300     03 URV-KVAKS-PAV        PIC S9(7)           COMP-3.                  
019400*                                 DEL AV AK PÅ VÄG                        
019500*                                 PART OF AK ON ITS WAY                   
019600     03 URV-IDTECKEN-KVEFRS  PIC X.                                       
019700*                                 TECKEN                                  
019800*                                 SIGN                                    
019900     03 URV-KVEFRS           PIC S9(7)           COMP-3.                  
020000*                                 EJ FAKTURERAT ANTAL STYCK               
020100*                                 ORDERED NOT INVOICED QTY                
020200     03 URV-IDTECKEN-KVLS    PIC X.                                       
020300*                                 TECKEN                                  
020400*                                 SIGN                                    
020500     03 URV-KVLS             PIC S9(7)           COMP-3.                  
020600*                                 LAGERSALDO                              
020700*                                 STOCK BALANCE                           
020800     03 URV-DAREGDAT-LADD    PIC 9(8).                                    
020900*                                 REGISTRERINGSDATUM (ÅÅÅÅMMDD)           
021000*                                 REGISTRATION DATE (YYYYMMDD)            
021100*** END OF VILMAII-COPY LENGTH= 116 BYTES                                 
