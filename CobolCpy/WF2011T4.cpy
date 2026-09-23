000100 01  APPX-WF2011T4.                                                       
000200*                                 DOCUMENT APPENDIX DATA                  
000300     03 APPX-IDPTYP          OCCURS 100 TIMES                             
000400                             PIC X(3).                                    
000500*                                 POSTTYP                                 
000600*                                 RECORD TYPE                             
000700     03 APPX-DAFINDOC        OCCURS 100 TIMES                             
000800                             PIC 9(8).                                    
000900*                                 DOKUMENT DATUM (ÅÅÅÅMMDD)               
001000*                                 INVOICING DATE   (YYYYMMDD)             
001100     03 APPX-IDFINDOC        OCCURS 100 TIMES                             
001200                             PIC S9(9)           COMP-3.                  
001300*                                 FINANSIELLT DOKUMENT ID                 
001400*                                 FINANCIAL DOCUMENT ID                   
001500     03 APPX-IDLOPNR         OCCURS 100 TIMES                             
001600                             PIC S9(5)           COMP-3.                  
001700*                                 LÖPNUMMER          IDLOPNR-002          
001800     03 APPX-IDLEGSEL        OCCURS 100 TIMES                             
001900                             PIC X(4).                                    
002000*                                 FAKTURERANDE FÖRETAG TEX VCCS           
002100*                                 LEGAL SELLER IDENTITY                   
002200     03 APPX-BEFORMS         OCCURS 100 TIMES                             
002300                             PIC X(15).                                   
002400*                                 BENÄMNING PÅ DOKUMENTFORMAT             
002500*                                                                         
002600*                                 DESCRIPTION OF DOCUMENT FORMAT          
002700*                                                                         
002800     03 APPX-DAEXDAT         OCCURS 100 TIMES                             
002900                             PIC 9(8).                                    
003000*                                 EXEKVERINGSDATUM (ÅÅÅÅMMDD)             
003100*                                 EXECUTION DATE (YYYYMMDD)               
003200     03 APPX-TIEXTID         OCCURS 100 TIMES                             
003300                             PIC S9(7)           COMP-3.                  
003400*                                 EXEKVERINGSTIDPUNKT                     
003500*                                 EXECUTION TIME                          
003600     03 APPX-KDVALISO        OCCURS 100 TIMES                             
003700                             PIC X(3).                                    
003800*                                 VALUTAKOD ENLIGT ISO-STANDARD.          
003900*                                 CURRENCY CODE BY ISO-STANDARD.          
004000     03 APPX-IDLANDX3-SEND   OCCURS 100 TIMES                             
004100                             PIC X(3).                                    
004200*                                 LANDKOD SÄNDANDE LAND                   
004300*                                 COUNTRY CODE SENDING COUNTRY            
004400     03 APPX-IDLEVNR         OCCURS 100 TIMES                             
004500                             PIC X(5).                                    
004600*                                 LEVERANTÖRNUMMER                        
004700*                                 SUPPLIER NUMBER (VENDOR NUMBER)         
004800     03 APPX-IDPARTNR        OCCURS 100 TIMES                             
004900                             PIC X(9).                                    
005000*                                 PARTNERNUMMER                           
005100*                                 PARTNER NO                              
005200     03 APPX-KDFINDOC        OCCURS 100 TIMES                             
005300                             PIC X(4).                                    
005400*                                 TYP FINANSIELLT DOKUMENT                
005500*                                 FINANCIAL DOCUMENT TYPE                 
005600     03 APPX-FLSOFT          OCCURS 100 TIMES                             
005700                             PIC X.                                       
005800*                                 FLAGGA SOFTVARA                         
005900*                                 SOFTWARE MARK                           
006000     03 APPX-FLFREE          OCCURS 100 TIMES                             
006100                             PIC X.                                       
006200*                                 GRATISFATURA                            
006300*                                 FREE INVOICE                            
006400     03 APPX-IDBREAK-1       OCCURS 100 TIMES                             
006500                             PIC X(8).                                    
006600*                                 BRYTVÄRDE                               
006700*                                 BREAK VALUE                             
006800     03 APPX-IDBREAK-2       OCCURS 100 TIMES                             
006900                             PIC X(8).                                    
007000*                                 BRYTVÄRDE                               
007100*                                 BREAK VALUE                             
007200     03 APPX-KDAPPEND        OCCURS 100 TIMES                             
007300                             PIC X(4).                                    
007400*                                 APPENDIX KOD                            
007500*                                 APPENDIX CODE                           
007600     03 APPX-IDAPPEND        OCCURS 100 TIMES                             
007700                             PIC X(8).                                    
007800*                                 APPENDIXVÄRDE                           
007900*                                 APPENDIX ITEM                           
008000     03 APPX-SUNTO-APP       OCCURS 100 TIMES                             
008100                             PIC S9(11)V9(2)     COMP-3.                  
008200*                                 SALES AMOUNT FOR APPENDIX EXCL.         
008300*                                  VAT                                    
008400     03 APPX-SUBTO-APP       OCCURS 100 TIMES                             
008500                             PIC S9(11)V9(2)     COMP-3.                  
008600*                                 SALES AMOUNT FOR APPENDIX INCL.         
008700*                                  VAT                                    
008800     03 APPX-SUVAT-BILLIT-APP                                             
008900                             OCCURS 100 TIMES                             
009000                             PIC S9(11)V9(2)     COMP-3.                  
009100*                                 MOMSVÄRDE FÖR APPENDIX                  
009200*                                 VAT VALUE FOR APPENDIX                  
009300*** END OF VILMAII-COPY LENGTH= 12500 BYTES                               
