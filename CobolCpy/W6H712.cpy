000100 01  EK-W6H712.                                                           
000200*                                 KVALITET                                
000300*                                 KONTROLLRAPPORT - EKONOMI               
000400*                                 NYCKEL SAKNAS                           
000500     03 EK-BENAEMN           PIC X(25).                                   
000600*                                 BENÄMNING                               
000700*                                 NAME                                    
000800     03 EK-FLKLAR            PIC X.                                       
000900*                                 AVSLUTNINGSMARKERING                    
001000*                                 FINISHED FLAG                           
001100     03 EK-IDBEMONR          PIC 9(10).                                   
001200*                                 BETALNINGSMOTTAGARNUMMER                
001300*                                 NUMBER OF PAYMENT RECEIVER              
001400     03 EK-IDTFN             PIC X(20).                                   
001500*                                 TELEFONNUMMER EXTERNT                   
001600*                                 TELEPHONE NUMBER  EXTERNAL              
001700     03 EK-IDVERNR           PIC 9(8).                                    
001800*                                 VERIFIKATIONSNUMMER                     
001900*                                 VERIFICATION NUMBER                     
002000     03 EK-KDVALISO          PIC X(3).                                    
002100*                                 VALUTAKOD ENLIGT ISO-STANDARD.          
002200*                                 CURRENCY CODE BY ISO-STANDARD.          
002300     03 EK-KVKRRET           PIC S9(7)           COMP-3.                  
002400*                                 ANTAL ARTIKLAR I RETUR/SKROT            
002500*                                                                         
002600*                                 QUANTITY PARTS RETURNED/SCRAPPE         
002700*                                 D                                       
002800     03 EK-KVFFDAG           PIC 9(3).                                    
002900*                                 ANTAL FÖRFALLODAGAR                     
003000*                                 NUMBER OF DAYS TO MATURITY              
003100     03 EK-PRARTBEL-PR       PIC S9(8)V9(5)      COMP-3.                  
003200*                                 DETTA BESTÄLLNINGSPRIS                  
003300*                                 (I LEVERANTÖRENS VALUTA)                
003400     03 EK-PRKURS            PIC S9(6)V9(5)      COMP-3.                  
003500*                                 VALUTAKURS                              
003600*                                 CURRENCY EXCHANGE RATE                  
003700     03 EK-TEKREATG          PIC X(15).                                   
003800*                                 KONTERING PÅ ANNAT SÄTT                 
003900*                                                                         
004000*                                 ACCOUNTED FOR IN ANOTHER WAY            
004100*                                                                         
004200     03 EK-TEKREKON          OCCURS 2 TIMES                               
004300                             PIC X(70).                                   
004400*                                 NOTERING EKONOMI                        
004500*                                                                         
004600*                                 NOTE ACCOUNTING                         
004700*                                                                         
004800     03 EK-TIFAKT            PIC S9(7)           COMP-3.                  
004900*                                 FAKTURERINGSDATUM (ÅÅMMDD)              
005000*                                 INVOICING DATE   (YYMMDD)               
005100     03 EK-SUARTSTD          PIC S9(9)V9(2)      COMP-3.                  
005200*                                 SUMMA STANDARDPRIS RADVÄRDE             
005300*                                 SUM LINEVALUE STANDARD PRICE            
005400     03 EK-SUKPALAG          PIC S9(7)V9(2)      COMP-3.                  
005500*                                 KALKYLPÅLÄGG                            
005600*                                 MARK-UP                                 
005700     03 EK-SUHEMTAG          PIC S9(7)V9(2)      COMP-3.                  
005800*                                 HEMTAGNINGSKOSTNAD                      
005900*                                 BUY-IN COSTS                            
006000     03 EK-SUBESDIFF         PIC S9(7)V9(2)      COMP-3.                  
006100*                                 PRISDIFFERENS                           
006200*                                 PRICE DIFFERENCE                        
006300     03 EK-SUOMK-EXT         PIC S9(7)           COMP-3.                  
006400*                                 DET EXTERNA BELOPP SOM SKALL            
006500*                                 DEBITERAS KUND                          
006600*                                 EXTERNAL AMOUNT TO BE PAID BY           
006700*                                 CUSTOMER                                
006800     03 EK-SUOMK-INT         PIC S9(7)           COMP-3.                  
006900*                                 DET INTERNA BELOPP SOM SKALL            
007000*                                 DEBITERAS KUND                          
007100*                                 INTERNAL AMOUNT TO BE PAID BY           
007200*                                 CUSTOMER                                
007300     03 EK-SUMAT             PIC S9(7)V9(2)      COMP-3.                  
007400*                                 MATERIALKOSTNAD                         
007500     03 EK-PRFRAKT           PIC S9(7)V9(2)      COMP-3.                  
007600*                                 FRAKTKOSTNAD                            
007700*                                 FREIGHT COST                            
007800     03 EK-PRMOMS            PIC S9(7)V9(2)      COMP-3.                  
007900*                                 MERVÄRDESSKATT                          
008000*                                 VAT                                     
008100     03 EK-FILLER            PIC X(4).                                    
008200*** END OF VILMAII-COPY LENGTH= 294 BYTES                                 
