000100 01  FOOT-WF201103.                                                       
000200*                                 DOCUMENT FOOTER DATA                    
000300     03 FOOT-IDPTYP          PIC X(3).                                    
000400*                                 POSTTYP                                 
000500*                                 RECORD TYPE                             
000600     03 FOOT-DAFINDOC        PIC 9(8).                                    
000700*                                 DOKUMENT DATUM (ÅÅÅÅMMDD)               
000800*                                 INVOICING DATE   (YYYYMMDD)             
000900     03 FOOT-IDFINDOC        PIC S9(9)           COMP-3.                  
001000*                                 FINANSIELLT DOKUMENT ID                 
001100*                                 FINANCIAL DOCUMENT ID                   
001200     03 FOOT-IDLOPNR         PIC S9(5)           COMP-3.                  
001300*                                 LÖPNUMMER          IDLOPNR-002          
001400     03 FOOT-IDLEGSEL        PIC X(4).                                    
001500*                                 FAKTURERANDE FÖRETAG TEX VCCS           
001600*                                 LEGAL SELLER IDENTITY                   
001700     03 FOOT-BEFORMS         PIC X(15).                                   
001800*                                 BENÄMNING PÅ DOKUMENTFORMAT             
001900*                                                                         
002000*                                 DESCRIPTION OF DOCUMENT FORMAT          
002100*                                                                         
002200     03 FOOT-DAEXDAT         PIC 9(8).                                    
002300*                                 EXEKVERINGSDATUM (ÅÅÅÅMMDD)             
002400*                                 EXECUTION DATE (YYYYMMDD)               
002500     03 FOOT-TIEXTID         PIC S9(7)           COMP-3.                  
002600*                                 EXEKVERINGSTIDPUNKT                     
002700*                                 EXECUTION TIME                          
002800     03 FOOT-KDVALISO        PIC X(3).                                    
002900*                                 VALUTAKOD ENLIGT ISO-STANDARD.          
003000*                                 CURRENCY CODE BY ISO-STANDARD.          
003100     03 FOOT-IDLANDX3-SEND   PIC X(3).                                    
003200*                                 LANDKOD SÄNDANDE LAND                   
003300*                                 COUNTRY CODE SENDING COUNTRY            
003400     03 FOOT-IDLEVNR         PIC X(5).                                    
003500*                                 LEVERANTÖRNUMMER                        
003600*                                 SUPPLIER NUMBER (VENDOR NUMBER)         
003700     03 FOOT-IDPARTNR        PIC X(9).                                    
003800*                                 FINANCIELL KUND                         
003900*                                 FINANCIAL CUST                          
004000     03 FOOT-KDFINDOC        PIC X(4).                                    
004100*                                 TYP FINANSIELLT DOKUMENT                
004200*                                 FINANCIAL DOCUMENT TYPE                 
004300     03 FOOT-FLSOFT          PIC X.                                       
004400*                                 FLAGGA SOFTVARA                         
004500*                                 SOFTWARE MARK                           
004600     03 FOOT-FLFREE          PIC X.                                       
004700*                                 GRATISFATURA                            
004800*                                 FREE INVOICE                            
004900     03 FOOT-IDBREAK-1       PIC X(8).                                    
005000*                                 BRYTVÄRDE                               
005100*                                 BREAK VALUE                             
005200     03 FOOT-IDBREAK-2       PIC X(8).                                    
005300*                                 BRYTVÄRDE                               
005400*                                 BREAK VALUE                             
005500     03 FOOT-SUNTO-TOT       PIC S9(11)V9(2)     COMP-3.                  
005600*                                 TOTAL SALES AMOUNT EXCL. VAT            
005700     03 FOOT-SUBTO-TOT       PIC S9(11)V9(2)     COMP-3.                  
005800*                                 TOTAL SALES AMOUNT INCL. VAT            
005900     03 FOOT-SUVAT-BILLIT-TOT                                             
006000                             PIC S9(11)V9(2)     COMP-3.                  
006100*                                 SUMMERAT MOMSVÄRDE                      
006200*                                 TOTAL VAT VALUE                         
006300     03 FOOT-SUNTO-TOT-LOC   PIC S9(11)V9(2)     COMP-3.                  
006400*                                 TOTAL SALES AMOUNT EXCL. VAT            
006500     03 FOOT-SUBTO-TOT-LOC   PIC S9(11)V9(2)     COMP-3.                  
006600*                                 TOTAL SALES AMOUNT INCL. VAT            
006700     03 FOOT-SUVAT-BILLIT-TOT-LOC                                         
006800                             PIC S9(11)V9(2)     COMP-3.                  
006900*                                 SUMMERAT MOMSVÄRDE                      
007000*                                 TOTAL VAT VALUE                         
007100     03 FOOT-KDVALISO-LOC    PIC X(3).                                    
007200*                                 VALUTAKOD ENLIGT ISO-STANDARD.          
007300*                                 CURRENCY CODE BY ISO-STANDARD.          
007400     03 FOOT-BETEXT-1        PIC X(50).                                   
007500     03 FOOT-BETEXT-2        PIC X(50).                                   
007600     03 FOOT-BETEXT-3        PIC X(50).                                   
007700     03 FOOT-BETEXT-4        PIC X(50).                                   
007800     03 FOOT-SUNTO-TOT-SND   PIC S9(11)V9(2)     COMP-3.                  
007900*                                 TOTAL SALES AMOUNT EXCL. VAT            
008000     03 FOOT-SUBTO-TOT-SND   PIC S9(11)V9(2)     COMP-3.                  
008100*                                 TOTAL SALES AMOUNT INCL. VAT            
008200     03 FOOT-SUVAT-BILLIT-TOT-SND                                         
008300                             PIC S9(11)V9(2)     COMP-3.                  
008400*                                 SUMMERAT MOMSVÄRDE                      
008500*                                 TOTAL VAT VALUE                         
008600     03 FOOT-KDVALISO-SND    PIC X(3).                                    
008700*                                 VALUTAKOD ENLIGT ISO-STANDARD.          
008800*                                 CURRENCY CODE BY ISO-STANDARD.          
008900     03 FOOT-PRKURS-SND      PIC S9(6)V9(5)      COMP-3.                  
009000*                                 VALUTAKURS                              
009100*                                 CURRENCY EXCHANGE RATE                  
009200     03 FOOT-REVALUTA-SND    PIC S9(3)           COMP-3.                  
009300*                                 OMRÄKNINGSTAL FÖR VALUTA                
009400*                                 CONVERT VALUE FOR CURRENCY CODE         
009500     03 FOOT-FLCURINF        PIC X.                                       
009600*                                 ANGER OM VALUTA INFO VISAS              
009700*                                 INDICATES IF CURRENCY CONSIDERS         
009800     03 FOOT-FLDECIMAL       PIC X.                                       
009900*                                 ANGER OM DECIMAL ANGES                  
010000*                                 INDICATES IF DECIMALS USED              
010100*** END OF VILMAII-COPY LENGTH= 371 BYTES                                 
