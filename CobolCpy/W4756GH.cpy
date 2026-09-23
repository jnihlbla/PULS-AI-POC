000100 01  BROKJPH-W4756GH.                                                     
000200*                                 BROKER INFORMATION JAPAN                
000300*                                 HEADER                                  
000400     03 BROKJPH-IDPTYP       PIC X(3).                                    
000500*                                 POSTTYP                                 
000600*                                 RECORD TYPE                             
000700     03 BROKJPH-KDFAKTYP     PIC X.                                       
000800*                                 FAKTURATYP                              
000900*                                 INVOICE TYPE                            
001000     03 BROKJPH-IDFAKT       PIC S9(7)           COMP-3.                  
001100*                                 FAKTURANUMMER                           
001200*                                 INVOICE NO.                             
001300     03 BROKJPH-IDDC         PIC X(2).                                    
001400*                                 IDENTIFIERARE LAGER                     
001500*                                 WAREHOUSE IDENTIFIER                    
001600     03 BROKJPH-DAFAKT       PIC 9(8).                                    
001700*                                 FAKTURERINGSDATUM (ÅÅÅÅMMDD)            
001800*                                 INVOICING DATE   (YYYYMMDD)             
001900     03 BROKJPH-KDVALISO     PIC X(3).                                    
002000*                                 VALUTAKOD ENLIGT ISO-STANDARD.          
002100*                                 CURRENCY CODE BY ISO-STANDARD.          
002200     03 BROKJPH-PRKURS       PIC S9(6)V9(5)      COMP-3.                  
002300*                                 VALUTAKURS                              
002400*                                 CURRENCY EXCHANGE RATE                  
002500     03 BROKJPH-SUORDV-FAKT  PIC S9(9)V9(2)      COMP-3.                  
002600*                                 FAKTURERAT ORDERVÄRDE                   
002700*                                 INVOICED ORDER VALUE                    
002800     03 BROKJPH-PREMBHNT     PIC S9(7)V9(2)      COMP-3.                  
002900*                                 EMBALLAGE O HANTERINGSKOST              
003000*                                 PACKING O HANDL COSTS                   
003100     03 BROKJPH-PRFRAKT      PIC S9(7)V9(2)      COMP-3.                  
003200*                                 FRAKTKOSTNAD                            
003300*                                 FREIGHT COST                            
003400     03 BROKJPH-PRFOERS      PIC S9(7)V9(2)      COMP-3.                  
003500*                                 FÖRSÄKRINGSPREMIE                       
003600*                                 INSURANCE FEE                           
003700     03 BROKJPH-KDFRAKT      PIC S9(3)           COMP-3.                  
003800*                                 FRAKTSÄTT DC TILL KUND                  
003900*                                 FREIGHT CODE                            
004000     03 BROKJPH-SUFKTUTL     PIC S9(11)V9(2)     COMP-3.                  
004100*                                 FAKTURABELOPP I UTLÄNDSK VALUTA         
004200*                                 INVOICE-SUM IN FOREIGN VALUE            
004300*** END OF VILMAII-COPY LENGTH= 57 BYTES                                  
