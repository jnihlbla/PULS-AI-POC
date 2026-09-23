000100 01  FOOT-WF2011T3.                                                       
000200*                                 DOCUMENT FOOTER DATA                    
000300     03 FOOT-IDPTYP          OCCURS 100 TIMES                             
000400                             PIC X(3).                                    
000500*                                 POSTTYP                                 
000600*                                 RECORD TYPE                             
000700     03 FOOT-DAFINDOC        OCCURS 100 TIMES                             
000800                             PIC 9(8).                                    
000900*                                 DOKUMENT DATUM (ÅÅÅÅMMDD)               
001000*                                 INVOICING DATE   (YYYYMMDD)             
001100     03 FOOT-IDFINDOC        OCCURS 100 TIMES                             
001200                             PIC S9(9)           COMP-3.                  
001300*                                 FINANSIELLT DOKUMENT ID                 
001400*                                 FINANCIAL DOCUMENT ID                   
001500     03 FOOT-IDLOPNR         OCCURS 100 TIMES                             
001600                             PIC S9(5)           COMP-3.                  
001700*                                 LÖPNUMMER          IDLOPNR-002          
001800     03 FOOT-IDLEGSEL        OCCURS 100 TIMES                             
001900                             PIC X(4).                                    
002000*                                 FAKTURERANDE FÖRETAG TEX VCCS           
002100*                                 LEGAL SELLER IDENTITY                   
002200     03 FOOT-BEFORMS         OCCURS 100 TIMES                             
002300                             PIC X(15).                                   
002400*                                 BENÄMNING PÅ DOKUMENTFORMAT             
002500*                                                                         
002600*                                 DESCRIPTION OF DOCUMENT FORMAT          
002700*                                                                         
002800     03 FOOT-DAEXDAT         OCCURS 100 TIMES                             
002900                             PIC 9(8).                                    
003000*                                 EXEKVERINGSDATUM (ÅÅÅÅMMDD)             
003100*                                 EXECUTION DATE (YYYYMMDD)               
003200     03 FOOT-TIEXTID         OCCURS 100 TIMES                             
003300                             PIC S9(7)           COMP-3.                  
003400*                                 EXEKVERINGSTIDPUNKT                     
003500*                                 EXECUTION TIME                          
003600     03 FOOT-KDVALISO        OCCURS 100 TIMES                             
003700                             PIC X(3).                                    
003800*                                 VALUTAKOD ENLIGT ISO-STANDARD.          
003900*                                 CURRENCY CODE BY ISO-STANDARD.          
004000     03 FOOT-IDLANDX3-SEND   OCCURS 100 TIMES                             
004100                             PIC X(3).                                    
004200*                                 LANDKOD SÄNDANDE LAND                   
004300*                                 COUNTRY CODE SENDING COUNTRY            
004400     03 FOOT-IDLEVNR         OCCURS 100 TIMES                             
004500                             PIC X(5).                                    
004600*                                 LEVERANTÖRNUMMER                        
004700*                                 SUPPLIER NUMBER (VENDOR NUMBER)         
004800     03 FOOT-IDPARTNR        OCCURS 100 TIMES                             
004900                             PIC X(9).                                    
005000*                                 PARTNERNUMMER                           
005100*                                 PARTNER NO                              
005200     03 FOOT-KDFINDOC        OCCURS 100 TIMES                             
005300                             PIC X(4).                                    
005400*                                 TYP FINANSIELLT DOKUMENT                
005500*                                 FINANCIAL DOCUMENT TYPE                 
005600     03 FOOT-FLSOFT          OCCURS 100 TIMES                             
005700                             PIC X.                                       
005800*                                 FLAGGA SOFTVARA                         
005900*                                 SOFTWARE MARK                           
006000     03 FOOT-FLFREE          OCCURS 100 TIMES                             
006100                             PIC X.                                       
006200*                                 GRATISFATURA                            
006300*                                 FREE INVOICE                            
006400     03 FOOT-IDBREAK-1       OCCURS 100 TIMES                             
006500                             PIC X(8).                                    
006600*                                 BRYTVÄRDE                               
006700*                                 BREAK VALUE                             
006800     03 FOOT-IDBREAK-2       OCCURS 100 TIMES                             
006900                             PIC X(8).                                    
007000*                                 BRYTVÄRDE                               
007100*                                 BREAK VALUE                             
007200     03 FOOT-SUNTO-TOT       OCCURS 100 TIMES                             
007300                             PIC S9(11)V9(2)     COMP-3.                  
007400*                                 TOTAL SALES AMOUNT EXCL. VAT            
007500     03 FOOT-SUBTO-TOT       OCCURS 100 TIMES                             
007600                             PIC S9(11)V9(2)     COMP-3.                  
007700*                                 TOTAL SALES AMOUNT INCL. VAT            
007800     03 FOOT-SUVAT-BILLIT-TOT                                             
007900                             OCCURS 100 TIMES                             
008000                             PIC S9(11)V9(2)     COMP-3.                  
008100*                                 SUMMERAT MOMSVÄRDE                      
008200*                                 TOTAL VAT VALUE                         
008300     03 FOOT-SUNTO-TOT-LOC   OCCURS 100 TIMES                             
008400                             PIC S9(11)V9(2)     COMP-3.                  
008500*                                 TOTAL SALES AMOUNT EXCL. VAT            
008600     03 FOOT-SUBTO-TOT-LOC   OCCURS 100 TIMES                             
008700                             PIC S9(11)V9(2)     COMP-3.                  
008800*                                 TOTAL SALES AMOUNT INCL. VAT            
008900     03 FOOT-SUVAT-BILLIT-TOT-LOC                                         
009000                             OCCURS 100 TIMES                             
009100                             PIC S9(11)V9(2)     COMP-3.                  
009200*                                 SUMMERAT MOMSVÄRDE                      
009300*                                 TOTAL VAT VALUE                         
009400     03 FOOT-KDVALISO-LOC    OCCURS 100 TIMES                             
009500                             PIC X(3).                                    
009600*                                 VALUTAKOD ENLIGT ISO-STANDARD.          
009700*                                 CURRENCY CODE BY ISO-STANDARD.          
009800     03 FOOT-BETEXT-1        OCCURS 100 TIMES                             
009900                             PIC X(50).                                   
010000     03 FOOT-BETEXT-2        OCCURS 100 TIMES                             
010100                             PIC X(50).                                   
010200     03 FOOT-BETEXT-3        OCCURS 100 TIMES                             
010300                             PIC X(50).                                   
010400     03 FOOT-BETEXT-4        OCCURS 100 TIMES                             
010500                             PIC X(50).                                   
010600     03 FOOT-SUNTO-TOT-SND   OCCURS 100 TIMES                             
010700                             PIC S9(11)V9(2)     COMP-3.                  
010800*                                 TOTAL SALES AMOUNT EXCL. VAT            
010900     03 FOOT-SUBTO-TOT-SND   OCCURS 100 TIMES                             
011000                             PIC S9(11)V9(2)     COMP-3.                  
011100*                                 TOTAL SALES AMOUNT INCL. VAT            
011200     03 FOOT-SUVAT-BILLIT-TOT-SND                                         
011300                             OCCURS 100 TIMES                             
011400                             PIC S9(11)V9(2)     COMP-3.                  
011500*                                 SUMMERAT MOMSVÄRDE                      
011600*                                 TOTAL VAT VALUE                         
011700     03 FOOT-KDVALISO-SND    OCCURS 100 TIMES                             
011800                             PIC X(3).                                    
011900*                                 VALUTAKOD ENLIGT ISO-STANDARD.          
012000*                                 CURRENCY CODE BY ISO-STANDARD.          
012100     03 FOOT-PRKURS-SND      OCCURS 100 TIMES                             
012200                             PIC S9(6)V9(5)      COMP-3.                  
012300*                                 VALUTAKURS                              
012400*                                 CURRENCY EXCHANGE RATE                  
012500     03 FOOT-REVALUTA-SND    OCCURS 100 TIMES                             
012600                             PIC S9(3)           COMP-3.                  
012700*                                 OMRÄKNINGSTAL FÖR VALUTA                
012800*                                 CONVERT VALUE FOR CURRENCY CODE         
012900*** END OF VILMAII-COPY LENGTH= 36900 BYTES                               
