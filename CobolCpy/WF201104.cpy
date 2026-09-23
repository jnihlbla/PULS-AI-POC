000100 01  APPX-WF201104.                                                       
000200*                                 DOCUMENT APPENDIX DATA                  
000300     03 APPX-IDPTYP          PIC X(3).                                    
000400*                                 POSTTYP                                 
000500*                                 RECORD TYPE                             
000600     03 APPX-DAFINDOC        PIC 9(8).                                    
000700*                                 DOKUMENT DATUM (ÅÅÅÅMMDD)               
000800*                                 INVOICING DATE   (YYYYMMDD)             
000900     03 APPX-IDFINDOC        PIC S9(9)           COMP-3.                  
001000*                                 FINANSIELLT DOKUMENT ID                 
001100*                                 FINANCIAL DOCUMENT ID                   
001200     03 APPX-IDLOPNR         PIC S9(5)           COMP-3.                  
001300*                                 LÖPNUMMER          IDLOPNR-002          
001400     03 APPX-IDLEGSEL        PIC X(4).                                    
001500*                                 FAKTURERANDE FÖRETAG TEX VCCS           
001600*                                 LEGAL SELLER IDENTITY                   
001700     03 APPX-BEFORMS         PIC X(15).                                   
001800*                                 BENÄMNING PÅ DOKUMENTFORMAT             
001900*                                                                         
002000*                                 DESCRIPTION OF DOCUMENT FORMAT          
002100*                                                                         
002200     03 APPX-DAEXDAT         PIC 9(8).                                    
002300*                                 EXEKVERINGSDATUM (ÅÅÅÅMMDD)             
002400*                                 EXECUTION DATE (YYYYMMDD)               
002500     03 APPX-TIEXTID         PIC S9(7)           COMP-3.                  
002600*                                 EXEKVERINGSTIDPUNKT                     
002700*                                 EXECUTION TIME                          
002800     03 APPX-KDVALISO        PIC X(3).                                    
002900*                                 VALUTAKOD ENLIGT ISO-STANDARD.          
003000*                                 CURRENCY CODE BY ISO-STANDARD.          
003100     03 APPX-IDLANDX3-SEND   PIC X(3).                                    
003200*                                 LANDKOD SÄNDANDE LAND                   
003300*                                 COUNTRY CODE SENDING COUNTRY            
003400     03 APPX-IDLEVNR         PIC X(5).                                    
003500*                                 LEVERANTÖRNUMMER                        
003600*                                 SUPPLIER NUMBER (VENDOR NUMBER)         
003700     03 APPX-IDPARTNR        PIC X(9).                                    
003800*                                 PARTNERNUMMER                           
003900*                                 PARTNER NO                              
004000     03 APPX-KDFINDOC        PIC X(4).                                    
004100*                                 TYP FINANSIELLT DOKUMENT                
004200*                                 FINANCIAL DOCUMENT TYPE                 
004300     03 APPX-FLSOFT          PIC X.                                       
004400*                                 FLAGGA SOFTVARA                         
004500*                                 SOFTWARE MARK                           
004600     03 APPX-FLFREE          PIC X.                                       
004700*                                 GRATISFATURA                            
004800*                                 FREE INVOICE                            
004900     03 APPX-IDBREAK-1       PIC X(8).                                    
005000*                                 BRYTVÄRDE                               
005100*                                 BREAK VALUE                             
005200     03 APPX-IDBREAK-2       PIC X(8).                                    
005300*                                 BRYTVÄRDE                               
005400*                                 BREAK VALUE                             
005500     03 APPX-KDAPPEND        PIC X(4).                                    
005600*                                 APPENDIX KOD                            
005700*                                 APPENDIX CODE                           
005800     03 APPX-IDAPPEND        PIC X(8).                                    
005900*                                 APPENDIXVÄRDE                           
006000*                                 APPENDIX ITEM                           
006100     03 APPX-SUNTO-APP       PIC S9(11)V9(2)     COMP-3.                  
006200*                                 SALES AMOUNT FOR APPENDIX EXCL.         
006300*                                  VAT                                    
006400     03 APPX-SUBTO-APP       PIC S9(11)V9(2)     COMP-3.                  
006500*                                 SALES AMOUNT FOR APPENDIX INCL.         
006600*                                  VAT                                    
006700     03 APPX-SUVAT-BILLIT-APP                                             
006800                             PIC S9(11)V9(2)     COMP-3.                  
006900*                                 MOMSVÄRDE FÖR APPENDIX                  
007000*                                 VAT VALUE FOR APPENDIX                  
007100*** END OF VILMAII-COPY LENGTH= 125 BYTES                                 
