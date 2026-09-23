000100 01  T01DAPPT.                                                            
000200*                                 MULTIFETCH TABELL TILL T01DAPP          
000300     03 IDLEGSEL             OCCURS 100 TIMES                             
000400                             PIC X(4).                                    
000500*                                 FAKTURERANDE FÖRETAG TEX VCCS           
000600*                                 LEGAL SELLER IDENTITY                   
000700     03 DAEXDAT              OCCURS 100 TIMES                             
000800                             PIC X(8).                                    
000900     03 TIEXTID              OCCURS 100 TIMES                             
001000                             PIC S9(7)           COMP-3.                  
001100*                                 EXEKVERINGSTIDPUNKT                     
001200*                                 EXECUTION TIME                          
001300     03 KDVALISO             OCCURS 100 TIMES                             
001400                             PIC X(3).                                    
001500*                                 VALUTAKOD ENLIGT ISO-STANDARD.          
001600*                                 CURRENCY CODE BY ISO-STANDARD.          
001700     03 IDLANDX3-SEND        OCCURS 100 TIMES                             
001800                             PIC X(3).                                    
001900*                                 LANDKOD SÄNDANDE LAND                   
002000*                                 COUNTRY CODE SENDING COUNTRY            
002100     03 IDLEVNR              OCCURS 100 TIMES                             
002200                             PIC X(5).                                    
002300*                                 LEVERANTÖRNUMMER                        
002400*                                 SUPPLIER NUMBER (VENDOR NUMBER)         
002500     03 IDPARTNR             OCCURS 100 TIMES                             
002600                             PIC X(9).                                    
002700*                                 PARTNERNUMMER                           
002800*                                 PARTNER NO                              
002900     03 KDFINDOC             OCCURS 100 TIMES                             
003000                             PIC X(4).                                    
003100*                                 TYP FINANSIELLT DOKUMENT                
003200*                                 FINANCIAL DOCUMENT TYPE                 
003300     03 FLSOFT               OCCURS 100 TIMES                             
003400                             PIC X.                                       
003500*                                 FLAGGA SOFTVARA                         
003600*                                 SOFTWARE MARK                           
003700     03 FLFREE               OCCURS 100 TIMES                             
003800                             PIC X.                                       
003900*                                 GRATISFATURA                            
004000*                                 FREE INVOICE                            
004100     03 FLPRIV               OCCURS 100 TIMES                             
004200                             PIC X.                                       
004300*                                 KÖPARE ÄR EN PRIVATPERSON               
004400*                                 PURCHASER IS A PRIVATE PERSON           
004500     03 IDBREAK-1            OCCURS 100 TIMES                             
004600                             PIC X(8).                                    
004700*                                 BRYTVÄRDE                               
004800*                                 BREAK VALUE                             
004900     03 IDBREAK-2            OCCURS 100 TIMES                             
005000                             PIC X(8).                                    
005100*                                 BRYTVÄRDE                               
005200*                                 BREAK VALUE                             
005300     03 KDAPPEND             OCCURS 100 TIMES                             
005400                             PIC X(4).                                    
005500*                                 APPENDIX KOD                            
005600*                                 APPENDIX CODE                           
005700     03 IDAPPEND             OCCURS 100 TIMES                             
005800                             PIC X(8).                                    
005900*                                 APPENDIXVÄRDE                           
006000*                                 APPENDIX ITEM                           
006100     03 SUNTO-APP            OCCURS 100 TIMES                             
006200                             PIC S9(11)V9(2)     COMP-3.                  
006300*                                 SALES AMOUNT FOR APPENDIX EXCL.         
006400*                                  VAT                                    
006500     03 SUVAT-BILLIT-APP     OCCURS 100 TIMES                             
006600                             PIC S9(11)V9(2)     COMP-3.                  
006700*                                 MOMSVÄRDE FÖR APPENDIX                  
006800*                                 VAT VALUE FOR APPENDIX                  
006900     03 SUBTO-APP            OCCURS 100 TIMES                             
007000                             PIC S9(11)V9(2)     COMP-3.                  
007100*                                 SALES AMOUNT FOR APPENDIX INCL.         
007200*                                  VAT                                    
007300*** END OF VILMAII-COPY LENGTH= 9200 BYTES                                
