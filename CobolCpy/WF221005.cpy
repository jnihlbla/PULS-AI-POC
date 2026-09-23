000100 01  HEAD-WF221005.                                                       
000200*                                 DOCUMENT HEADER DATA                    
000300     03 HEAD-IDAFPRCD        PIC X(10).                                   
000400*                                 AFP-BLANKETT POSTTYP                    
000500*                                 AFP FORMS RECORD TYPE                   
000600     03 HEAD-IDPARTNR        PIC X(9).                                    
000700*                                 PARTNERNUMMER                           
000800*                                 PARTNER NO                              
000900     03 HEAD-IDLANDX2        PIC X(2).                                    
001000*                                 2-STÄLLIG LANDSBETECKNINGSKOD           
001100*                                 2-LETTER CODE FOR COUNTRY               
001200     03 HEAD-DAREGDAT        PIC 9(8).                                    
001300*                                 REGISTRERINGSDATUM (ÅÅÅÅMMDD)           
001400*                                 REGISTRATION DATE (YYYYMMDD)            
001500     03 HEAD-KDFINDOC        PIC X(4).                                    
001600*                                 TYP FINANSIELLT DOKUMENT                
001700*                                 FINANCIAL DOCUMENT TYPE                 
001800     03 HEAD-IDFINDOC        PIC Z(8)9.                                   
001900*                                 FINANSIELLT DOKUMENT ID                 
002000*                                 FINANCIAL DOCUMENT ID                   
002100     03 HEAD-IDPAYMENT       PIC X(10).                                   
002200     03 HEAD-IJPAYMENT       PIC X(24).                                   
002300     03 HEAD-SUNTO           PIC Z(10)9.9(2).                             
002400*                                 TOTAL SALES AMOUNT EXCL. VAT            
002500     03 HEAD-SUVAT           PIC Z(10)9.9(2).                             
002600*                                 MOMSVÄRDE PER MOMSKOD                   
002700*                                 VAT VALUE PER VAT CODE                  
002800     03 HEAD-SUBTO           PIC Z(10)9.9(2).                             
002900*                                 TOTAL SALES AMOUNT INCL. VAT            
003000     03 HEAD-KDVALISO        PIC X(3).                                    
003100*                                 VALUTAKOD ENLIGT ISO-STANDARD.          
003200*                                 CURRENCY CODE BY ISO-STANDARD.          
003300     03 HEAD-SUREMTO         PIC Z(10)9.9(2).                             
003400*                                 TOTAL SALES AMOUNT EXCL. VAT            
003500*** END OF VILMAII-COPY LENGTH= 135 BYTES                                 
